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
009900150120      *----
010000150120      * nome file PDF copia Ordine (rek.P del VAT)
010100150120     D DVATP         E DS
010200150120     D TXT_Nome_File_PDF...
010300150120     D                 s             70
010400150120      *----
010500090611     D CAG_porto_in_testata...
010600090611     D                 s              1
010700090611     D In_Testata      s              1    INZ('S')
010800090611     D
010900090611     D Mittente        s             35
011000090611     D Mitt_Qualifier  s              4
011100090611     D Id_Messaggio    s             14
011200090209      *---------------------------------------------------------------------*
011300090209     D KPJBA         E DS
011400090209     D FNLV55DS      E DS
011500090209     D TIBS55DS      E DS
011600090209     D TISI95DS      E DS
011700090209     D UTEDSE0F      E DS
011800090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
011900090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012000090209      * Ds scomposzione tipo capoconti
012100090209     D TCUDS           DS
012200090209     D  F34                    3      4
012300090209     D  F56                    5      6
012400090209     D DS1B          E DS
012500090209     D DS15          E DS
012600090209     D DSCV          E DS
012700090213     D TRTC86DS      E DS
012800090209     D EDIDSPT       E DS
012900090209     D EDIDSPU       E DS
013000090209     D EDIDSPV       E DS
013100090211     D EDIDSCL       E DS
013200090216     D EDIDSTC       E DS
013300090209      **
013400090209      *  DS x salvataggio dati impostati in EDiVAB
013500090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
013600090209     D SALVA           DS           545
013700090210      **
013800090209     D WLBDA8          DS
013900090209     D  G02DAT                 1      8  0
014000090209     D  G02INV                 9     16  0
014100090209     D  G02ERR                17     17
014200090209     D  G02TGI                18     22  0
014300090209      *  DS x scomposizione numero spedizione
014400090209     D DSSPE           DS
014500090209     D  SPEAAS                 1      4  0
014600090209     D  SPELNP                 5      7  0
014700090209     D  SPENRS                 8      9  0
014800090209     D  SPENSP                10     16  0
014900090209      *  DS x scomposizione segnacollo
015000090209     D DSSGN           DS
015100090209     D  SGNLNP                 1      3  0
015200090209     D  SGNLNA                 4      6  0
015300090209     D  SGNNRS                 7      8  0
015400090209     D  SGNNSC                 9     15  0
015500090209     D  SGNZNC                16     17  0
015600090209      *  DS x stampa segnacollo
015700090209      *---------------                                                      *
015800090611     D  Tes_VABlnp     s                   LIKE(vablnp)
015900090611     D  Tes_VABnrs     s                   LIKE(vabnrs)
016000090611     D  Tes_VABctm     s                   LIKE(vabctm)
016100090611     C*
016200090603     d tes_VABrmo      s                   LIKE(VABrmo)
016300090603     d tes_VABnmo      s                   LIKE(VABnmo)
016400090603     d tes_VABcmo      s                   LIKE(VABcmo)
016500090209     C*
016600090603     d tes_VABrsd      s                   LIKE(VABrsd)
016700090603     d tes_VABrd2      s                   LIKE(VABrd2)
016800090603     d tes_VABind      s                   LIKE(VABind)
016900090603     d tes_VABlod      s                   LIKE(VABlod)
017000090603     d tes_VABnzd      s                   LIKE(VABnzd)
017100090603     C*
017200090603     d tes_Valuta      s                   LIKE(VABvca)
017300090611     d tes_VABcbo      s                   LIKE(VABcbo)
017400090603     C*
017500090611     d tes_VABnas      s                   LIKE(VABnas)
017600090611     d tes_VABtsp      s                   LIKE(VABtsp)
017700090611     C*
017800090611     d Tes_VABdcr      s                   LIKE(VABdcr)
017900090611     d Tes_VABtcr      s                   LIKE(VABtcr)
018000090611     d Tes_VABhcr      s                   LIKE(VABhcr)
018100090611     C*
018200090611     d Tes_VABdcm      s                   LIKE(VABdcm)
018300090611     C*
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
020700090611     D ISO             S              2    DIM(500)
020800090209     D CPF             S              2  0 DIM(500)
020900090209      * Tabella partner
021000121105     D CPT             S             35    DIM(200)
021100121105     D CPTparz         S             35    DIM(%elem(CPT))
021200121105     D KSC_UNB         S              7    DIM(%elem(CPT))
021300121105     D DPT             S             90    DIM(%elem(CPT))
021400121105     D DPU             S             90    DIM(%elem(CPT))
021500121105     D DPV             S             90    DIM(%elem(CPT))
021600121105     D TRUL0SDS      E DS
021700090209      *
021800121105     d Ptn_parziale    s              1    inz('N')
021900121105     d sav_CPTx        s              3  0 inz(0)
022000090209      * Tabella CL --> codici clienti - tariffa
022100130305     D CCL             S             35    DIM(999)
022200130305     D DCL             S             90    DIM(999)
022300090209      * Schiere x caricamento cod.cliente  <>
022400130305     D KCL             S              7  0 DIM(999)
022500090209      * Termini di consegna
022600090209     D CTB35           S             35    DIM(100)
022700090209     D CTB             S              3    DIM(100)
022800090209     D POR             S              1    DIM(100)
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
025800090209     D kKUT            s                   LIKE(TBLKUT)
025900090209     D kCOD            s                   LIKE(TBLCOD)
026000090209     D kKEY            s                   LIKE(TBLKEY)
026100090209     D kKCC            s                   LIKE(ACOKCC)
026200090209     D kKSC            s                   LIKE(ACOKSC)
026300090209     D kAAA            s                   LIKE(NUFAAA)
026400090209     D kCNU            s                   LIKE(NUFCNU)
026500090209     D kFIL            s                   LIKE(NUFFIL)
026600090209     D kAAS            s                   LIKE(VABAAS)
026700090209     D kLNP            s                   LIKE(VABLNP)
026800090209     D kNRS            s                   LIKE(VABNRS)
026900090209     D kNSP            s                   LIKE(VABNSP)
027000090209     D kCMR            s                   LIKE(VABCMR)
027100090209     D kCCM            s                   LIKE(VABCCM)
027200090209     D kNCD            s                   LIKE(VADNCD)
027300090210     D kCNT            s                   LIKE(VADCNT)
027400090209     D kNMC            s                   LIKE(RDENMC)
027500090209     D kNRP            s                   LIKE(RDENRP)
027600090209     D kLNP1           s                   LIKE(RDELNP)
027700090209      *
027800090209      *  Invio E-mail
027900090209     D Indirizzi       s            253
028000090209     D Oggetto         s             44
028100090209     D Messaggio       s           5000
028200090209      *
028300090209     d   DSmail        ds
028400090209     D Mail_Ind                      44
028500090209     d   IND_char                     1    dim(44)
028600090209      *
028700090209     D Lun_Ind         s              5u 0
028800090209      *
028900090209     D Dac             s              5u 0
029000090209     D Luc             s              5u 0
029100090209      *
029200090209     C*---------------------------------------------------------------*
029300090209     D MsgDSP          S            256                                         Testo generico
029400090209     C*---------------------------------------------------------------*
029500090209     D SndMg01         S             10                                         Message type
029600090209     D                                     INZ('*INFO')
029700090209     D SndMg02         S             10                                         Deluvery mode
029800090209     D                                     INZ('*BREAK')
029900090209     D SndMg03         S            256                                         Message text
030000090209     D SndMg04         S             10I 0                                      Length of text
030100090209     D                                     INZ(%SIZE(SndMg03))
030200090209     D SndMg05         S             10                                         User profile
030300090209     D                                     DIM(299)
030400090209     D SndMg06         S             10I 0                                      Number of user
030500090209     D SndMg07         S             10I 0                                      Message sent indic.
030600090209     D SndMg08         S             10I 0                                      Function requested
030700090209     D SndMg10         S              1                                         Show display
030800090209     D                                     INZ('N')
030900090209     D SndMg11         S             20                                         Qualified MSGQ name
031000090209     D SndMg12         S              4                                         Name type indicator
031100090209     D                                     INZ('*USR')
031200090209      * indice di scihera
031300090209     D Jx              S              5I 0
031400090209      *
031500090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031600090209      *
031700090209     d bart_it         C                   CONST('@Bartolini.it;')
031800090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031900060612      * ?================================================================== */
032000060612      *
032100060612      * ?   * Campi x decodifica * (INPUT  del Record)
032200090130     D  Dati           s           2048
032300060612      * ?   *--------------------------------------------------------------*
032400060612     D  se_errore      s              1    inz(' ')
032500060612      * ?* ------------------------------------------------------ *
032600060710     D Digits          C                   '0123456789'
032700060612      * ?================================================================== */
032800060612      *   Ciclo principale
032900090205      * ?================================================================== */
033000060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
033100060612     C*
033200060612     C                   if        not %open(tivin00r)
033300060612     C                   open      tivin00r
033400060612     C                   endif
033500060612      **
033600060612      * ?------------------------------------------------------------------ */
033700060612     C*? Controllo dati arrivati da DPD
033800060612      * ?------------------------------------------------------------------ */
033900060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
034000060612      * ?- controllando che la trasmissione sia completa.
034100090205      **
034200060612      * ?              /*---------------------- */
034300090205     c                   goto      nonFare_adesso
034400060612     c                   exsr      check_Trasm
034500090205     c     nonFare_adessotag
034600060612      * ?              /*---------------------- */
034700090205      **
034800060613      **  Errore di trasmissione x tutti i records
034900060613      **   --> file in errore
035000060613     c                   if        se_errore = 'S'
035100060612
035200060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
035300090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
035400090205     c                   exsr      Msg_errato
035500060612      **
035600060612     c                   else
035700060614      **
035800060612      * ?------------------------------------------------------------------ */
035900060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
036000060612      * ?------------------------------------------------------------------ */
036100090205      **
036200060612      * ?              /*---------------------- */
036300060612     c                   exsr      Importa_Msg
036400060612      * ?              /*---------------------- */
036500060612
036600060614     c                   end
036700060614
036800060612     C                   if        %open(tivin00r)
036900060612     C                   close     tivin00r
037000060612     C                   endif
037100060612      *
037200060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
037300090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
037400090225     c                             esito ='0'
037500060710     C                   eval      esito ='0'
037600090225      *
037700090225     c                   COMMIT
037800090224      *
037900090213     c                   if        almeno_un_due ='S'
038000090213     C                   eval      esito ='1'
038100060614     C                   endif
038200090213     C                   endif
038300060614      *
038400060612     c                   SETON                                        LR
038500060612      * ?================================================================== */
038600060612      *? Importa i records della tramsissione
038700060612      * ?------------------------------------------------------------------ */
038800060612     c     Importa_Msg   Begsr
038900060612
039000090211     C                   EXSR      AZZMSG
039100060614
039200060612     c     *start        setll     Tivin00r
039300060612     c                   read      Tivin00r
039400060612
039500060612     c                   dow       not %eof(Tivin00r)
039600060614
039700060614      * solo i record sflaggati da rielaborare
039800060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
039900090211      *  Sempre Record OK
040000090211      ** Controlli formali sui campi
040100090211     C                   eval      vinFLG = '1'
040200060612     c                   clear                   se_errore
040300060612
040400060612      ** Decodifica record a campi variabili
040500060612      * ?              /*---------------------- */
040600090205     c                   exsr      Decod_Segmento
040700090209      * ?              /*---------------------- */
040800090209
040900090213      *** ------------------------------------
041000090609      *** A FINE CICLO DI UN DETTAGLIO:   -->"RFF" e alla fine "UNT" x DISPOR DELTAPLUS
041100090213      *** ------------------------------------
041200090213      * ?              /*---------------------- */
041300090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
041400090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
041500090213      * ?              /*---------------------- */
041600090601      ***
041700090601      **
041800090616      *   OGNI DETTAGLIO è determinato dal RFF e alla fine da DOC e per ultimo da UNT.
041900090601      **
042000090603     c                   if           tipo_seg = 'UNT'                 or
042100090616     c                                tipo_seg = 'DOC'                 and
042200090611     c                                  se_trovato_NAD ='S'
042300090213      *
042400090213      *  Deve flaggare il dettaglio con dei problemi
042500090213     C                   if        Err_in_detta = 'S'
042600090213      **?              /*---------------------- */
042700090213     c                   exsr      DETta_Errato
042800090213      **?              /*---------------------- */
042900090213     c                   end
043000090213      *
043100060612      **  Se presente un errore nel record emette una segnalazione msg
043200090213     c                   if        se_errore = 'S'
043300090611      **
043400090611      **
043500090213     c                   else
043600090209      ******
043700090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043800090227     c                   if         se_trovato_NAD ='S'
043900060612      * ?              /*---------------------- */
044000090209     c                   exsr      Scrive_VAB
044100060612      * ?              /*---------------------- */
044200090227     c                   end
044300090227      *
044400090209      *  Problemi nella decodifica dei campi VAB/VAT
044500090213     c                   if        errori_in_Wrt = 'S'
044600090213      *
044700090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
044800090213     c                   exsr      Error_DET
044900090213      *
045000090213     c                   ROLBK
045100090213      *
045200090213     c                   else
045300090601      *
045400090213     c                   COMMIT
045500090601      *
045600090227      * Dopo aver confermato, con COMMIT
045700090227      *  si prepara per una nuova spedizione
045800090227      * ?                         --------------
045900090227     c                   exsr      Nuova_spediz
046000090227      * ?                         --------------
046100090213     c                   end
046200090209
046300090209     c                   end
046400090210      **
046500060620     c                   end
046600060612
046700060621      *  x errore bloccante nella composizione del VAB/VAT
046800060621     c                   if        err_bloccante ='S'
046900060621     C                   eval      vinFLG = '2'
047000090211     c                   eval      esito  = '2'
047100090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
047200090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047300090603     C                             Bolle import in filiale - messaggio -
047400090603     C                             con UNB non presente in tabella PT controlla-
047500090603     C                             re EDI ricevuto su UPLOAD.'
047600090603     c                   exsr      invio_mail_AB
047700060621     c                   end
047800060612
047900100716     c                   update    Tivin000                             99
048000090211
048100090211      *** se non è riconosciuto l'UNB deve uscire direttamente
048200090211     c                   if        se_errore ='S'  and
048300090211     c                             tipo_seg = 'UNB'
048400090213     c                   eval      err_bloccante = 'S'
048500090211     c                   exsr      Msg_errato
048600090211     c                   LEAVE
048700090211     c                   endIF
048800090211      ***
048900060614     c                   endIF
049000060612
049100060612     c                   read      Tivin00r
049200060612     C                   ENDdo
049300060612      **
049400060612     c                   endSR
049500090211     C*----------------------------------------------------------------
049600090211     C*? AZZMSG - Azzera dati messaggio
049700090211     C*----------------------------------------------------------------
049800090211     C     AZZMSG        BEGSR
049900090211     C*
050000090211     C* Pulisco dati di work x foglio viaggio
050100090211     C                   MOVEL     *BLANKS       WNUMFV           35
050200090211     C                   MOVEL     *BLANKS       WNRCMR           35
050300090211     C                   Z-ADD     0             WDATPA            8 0
050400090211      *
050500090211      * pulisce gli RFF+FF di testata
050600090211     C                   clear                   $CLKSC            7 0
050700090211     C                   clear                   $CLTAR            3
050800090211     C                   clear                   $CLLNP            3 0
050900090211     C                   clear                   $CLNRS            2 0
051000090211     C                   clear                   $CLCTM            2
051100090211     C                   clear                   $CLBS1            1
051200090211     C                   clear                   $CLfnsp           1            *blk/S
051300090211      *
051400090211     C*  Inizializzo variabile new documento
051500090211     C                   Z-ADD     0             KCL
051600090211     C                   Z-ADD     0             WW                3 0
051700090211     ** Inizializza campo di work
051800090211     c                   move      'N'           WGID99
051900090211      * per controllare se almeno un record è stato importato sul VAB
052000090211     c                   clear                   Almeno_Uno        1
052100090213     c                   clear                   Almeno_Un_Due     1
052200090211     c                   eval      esito  = '0'
052300090211     C*
052400090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
052500090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
052600090603     c                   eval      NAD_destinatario_in_testata = *blank
052700090603     c                   eval      NAD_mittente_in_testata = *blank
052800090611     c                   eval      In_Testata = 'S'
052900090611     C*
053000090611     C                   clear                   Tes_VABdcm
053100090611     C*
053200090611     c                   clear                   tes_VABrmo
053300090611     c                   clear                   tes_VABnmo
053400090611     c                   clear                   tes_VABcmo
053500090611     C*
053600090611     c                   clear                   tes_VABrsd
053700090611     c                   clear                   tes_VABrd2
053800090611     c                   clear                   tes_VABind
053900090611     c                   clear                   tes_VABlod
054000090611     c                   clear                   tes_VABnzd
054100090611     C*
054200090611     c                   clear                   tes_Valuta
054300090611     c                   clear                   tes_VABcbo
054400090611     C*
054500090611     c                   clear                   Tes_VABdcr
054600090611     c                   clear                   Tes_VABtcr
054700090611     c                   clear                   Tes_VABhcr
054800090611     C*
054900090611     c                   clear                   tes_VABnas
055000090611     c                   clear                   tes_VABtsp
055100090611     C*
055200090611     c                   clear                   unh_VABRMA
055300090609     c                   clear                   unh_VABRMN
055400090611     C*
055500090611     C                   clear                   Tes_VABlnp
055600090611     C                   clear                   Tes_VABnrs
055700090611     C                   clear                   Tes_VABctm
055800090611     C*
055900090611     C                   clear                   VABfnsp_PT
056000090611     C*
056100090611     C                   MOVEA     *HIVAL        SGN
056200090603     C*
056300090211     C                   ENDSR
056400090210     C*----------------------------------------------------------------
056500090210      *? DECODIFICA DATI UNB
056600090210     C*----------------------------------------------------------------
056700090210     C     DATI_UNB      BEGSR
056800090210      *
056900090209      *  INIZIALIZZA  dati fondamentali messaggio
057000090209     C                   clear                   EDIDSPT
057100090209     C                   clear                   EDIDSPU
057200090209     C                   clear                   EDIDSPV
057300090209     C                   clear                   wnrDOC           35
057400090209     C                   move      'N'           PT_ok             1
057500090209      ** ---------------------------------------------------------------
057600090209      * quindi 1° cosa:
057700090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
057800090209      *     senza questo il messaggio NON è trascodificabile
057900090209     C                   MOVEL     unb0004       WCOD             35
058000090209     C                   MOVE      unb000720     WCOD
058100090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
058200090209     C                   Z-ADD     1             XX                3 0
058300090209     C     WCOD          LOOKUP    CPT(XX)                                32
058400090209      *
058500090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
058600090209     C                   If        *IN32 = *off
058700090209      * ******  Errore generale sul Messaggio
058800090209      * Msg di allerta Traduzione
058900090209     C                   EVAL      Indirizzi = CED_Bart
059000090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
059100090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
059200090209     C                             Bolle import in filiale - messaggio -
059300090209     C                             con UNB non presente in tabella PT controlla-
059400090209     C                             re EDI ricevuto su UPLOAD.'
059500090209      *
059600090209     c                   exsr      invio_mail
059700090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
059800090210     C                   eval      vinFLG = '2'
059900090210     C                   eval      se_errore   = 'S'
060000090210     c                   goto      end_UNB
060100090209      *
060200090209     C                   move      'N'           PT_ok             1
060300090209      *
060400090209      *             *------------------------------*
060500090209     C                   eLSe
060600090209      *             *------------------------------*
060700090209      * Se tutto OK su tab.PT:
060800090209     C                   move      'S'           PT_ok             1
060900090209      *
061000090209     C                   MOVEL     DPT(XX)       EDIDSPT
061100090209     C                   MOVEL     DPU(XX)       EDIDSPU
061200090209     C                   MOVEL     DPV(XX)       EDIDSPV
061300090226     C                   eval      Mittente       = unb0004
061400090226     C                   eval      Mitt_qualifier = unb000720
061500090226     C                   eval      Id_Messaggio   = UNB0022
061600090209      *
061700090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
061800090209      *  ricevuti:
061900090209     c                   movel     §PVema        mail_ind         44
062000090209      *
062100090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
062200090209     c                   if        mail_ind <> *blank
062300090209     c                   exsr      imp_ADDR_mail
062400090209     c                   end
062500090209      *
062600090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
062700090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
062800090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
062900090209     C                   if        §PULUN = *blank
063000090209     C                   eval      §PULUN = '00'
063100090209     c                   end
063200090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
063300090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
063400090209      *
063500090209     C                   Z-ADD     §PTKSC        WKSC              7 0
063600090611      *
063700090611      ** salva i dati principali
063800090611     C                   MOVEL     §PTLNP        Tes_VABlnp
063900090611     C                   MOVEL     §PTNRS        Tes_VABnrs
064000090611     C                   MOVEL     §PTCTM        Tes_VABctm
064100090209      *
064200090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
064300090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
064400090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
064500090209      *  i records trasmessi).
064600090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
064700090209      *
064800090209      *  In base al cod.trattamento merce reperisco tipo tratt.
064900090209     C                   CLEAR                   DS1B
065000090209     C                   Z-ADD     1             Y                 3 0
065100090611     C     Tes_VABctm    LOOKUP    CTM(Y)                                 32
065200090209     C     *IN32         IFEQ      '1'
065300090209     C                   MOVEL     DTM(Y)        DS1B
065400090209     C                   MOVEL     DS1B          WSVTRM           38
065500090209     C                   END
065600090209      *
065700090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
065800090209     C                   clear                   ACORAG
065900090209     C                   clear                   INDCAE
066000090209     C                   clear                   INDCAP
066100090209     C                   clear                   INDSTA
066200090209      * Decodifico partner
066300090209     C                   Z-ADD     1             KKUT
066400090209     C                   Z-ADD     KCI           KKCC
066500090209     C                   MOVE      §PTKSC        KKSC
066600090209     C     KACO          CHAIN     CNACO00F                           31
066700090209     C     KIND          CHAIN     CNIND00F                           31
066800090211      *
066900090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
067000090211      *   questa schiera è utilizzata anche per i clienti.
067100090211     C     §ptKSC        LOOKUP    KCL                                    39
067200090211     c                   If        *in39 = *off
067300090211     C                   add       1             WW
067400090211     C                   z-add     §ptKSC        KCL(WW)
067500090211     C                   Endif
067600090209      *
067700090209     C                   MOVEL     unb0017       WAA4              4 0
067800090209     C                   MOVE      WAA4          WAA2              2 0
067900090209     C                   MOVE      unb0017       WGG               2 0
068000090209     C                   MOVE      unb0017       WDTPRE            8 0
068100090611     C                   MOVEl     '20'          WDTPRE
068200090209     C                   MOVE      unb0019       WHMPRE            4 0
068300090209     C                   MOVE      unb0017       WDTMSG            6 0
068400090209     C                   MOVE      WAA2          WDTMSG
068500090209     C                   MOVEL     WGG           WDTMSG
068600090210      * potremmo
068700090210      * Utilizzare l'identificativo della trasmissione come CMR
068800090209     C                   IF        unb0022 <> *blank
068900090209     C                   MOVEL     unb0022       WNRDOC
069000090211     C                   MOVEL     unb0022       WNUMFV
069100090211     C                   MOVEL     unb0022       WNRCMR
069200090209     c                   end
069300090209      *
069400090209     c                   Endif
069500090209      **
069600090609     c     end_UNB       endSR
069700090609      * ?================================================================== */
069800090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
069900090609      * ?================================================================== */
070000090609     C     DATI_UNH      BEGSR
070100090609      *
070200090609      ** Riferimento del cliente
070300090609     C                   MOVEL     unh0062       WPRGSP           35
070400090609     C                   MOVEL     unh0062       WNSP             35
070500090609     C                   MOVEL     unh0062       WNUM             35
070600090609      * ?                                       --------
070700090609     C                   MOVEL     unh0062       unh_VABRMA
070800090609      * ?                                       --------
070900090609     C     ' '           SCAN      unh0062       WLUNGH            3 0
071000090609     C                   SUB       1             WLUNGH
071100090609     C                   EXSR      TSTNUM
071200090609     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
071300090609      * ?                                       --------
071400090609     C                   MOVEL     WNUMOK        unh_VABRMN
071500090609      * ?                                       --------
071600090609     C                   END
071700090609      **
071800090609     c     end_UNH       endSR
071900090609      * ?================================================================== */
072000090609     C*? INZIO del Messaggio
072100090609      * ?================================================================== */
072200090609     C     DATI_BGM      BEGSR
072300090609      *
072400090609      **
072500090609     c     end_BGM       endSR
072600090209      * ?================================================================== */
072700090609     C*? Tipo Servizio
072800090209      * ?================================================================== */
072900090609     C     Dati_TSR      BEGSR
073000090609      **
073100090609      **    Inviano solo spedizioni Espresso e non Priority
073200090611      **        tutte le spedizioni vanno come "Normali"
073300090611     c                   if        tsr4751 = 'D'
073400090611     c                   if        tsr4219 = 1 or tsr4219 = 2
073500090611     c                   eval      VABtsp = 'E'
073600090611     c                   end
073700090611     c                   end
073800090611      **
073900090611      **  se presente in testata se lo memorizza qualora non fosse presente
074000090611      **   nei singoli dettagli
074100090611     c                   if        In_Testata       = 'S'
074200090611     C                   eval       tes_VABtsp = VABtsp
074300090611     c                   end
074400090609      **
074500090609     c     end_TSR       endSR
074600090610      * ?================================================================== */
074700090611     C*? Accordi commerciali
074800090610      * ?================================================================== */
074900090610     C     DATI_CAG      BEGSR
075000090611      ****
075100090611      *  Attenzione questo segmento in alcuni messaggi potrebbe trovarsi solo
075200090611      *  in testata e non in dettaglio.
075300090611      ****
075400090610      *
075500090610      *  imposta il default dalla tabella PT se c'è
075600090610     C     §puPFA        ifNE      *blank
075700090610     c                   SELECT
075800090610      *  Porto Franco
075900090610     c                   WHEN      §puPFA = 'F'
076000090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
076100090611      *  Porto Assegnato
076200090611     C                   WHEN      §puPFA = 'A'
076300090611     C                   movel     '2'           VABCBO                         Senza C/Ass
076400090610     C                   ENDSL
076500090610     c                   end
076600090610      *
076700090610      *   Imposta o il codice o il metodo
076800090610      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
076900090611     c                   if        cag4237  <> *blank
077000090611      *
077100090610     C                   movel     UNB_Mittente  etbUNB
077200090611     C                   movel(p)  'CAG'         etbSGM
077300090611     C                   movel(p)  cag4237       etbVALSGM
077400090610     c     K_TAB1L       chain     edstbl01l
077500090610      * prova quindi senza l'UNB specifico del cliente
077600090610     c                   if        not %Found(edstbl01l)
077700090610     C                   clear                   etbUNB
077800090610     c     K_TAB1L       chain     edstbl01l
077900090610     c                   end
078000090610      *
078100090610     c                   if        %Found(edstbl01l)
078200090610     c                   SELECT
078300090610     c                   WHEN      ETBDATI = 'F'
078400090611     C                   movel     '1'           VABCBO                         Senza C/Ass.
078500090611      *  Porto Assegnato
078600090611     c                   WHEN      VABCAS > *zero
078700090611     C                   movel     '2'           VABCBO                         Senza C/Ass
078800090610     C                   ENDSL
078900090610      *
079000090610     c                   end
079100090610     c                   endIF
079200090610      **
079300090610      **  Il tipo bolla non è presente
079400090611     c                   if         VABCBO = *blank
079500090611     C                   eval      vinMSG = 'CAG non rilevato'
079600090610     c                   exsr      Error_DET
079700090611     c                   goto      end_CAG
079800090610     c                   end
079900090611      **
080000090611      **  se presente in testata se lo memorizza qualora non fosse presente
080100090611      **   nei singoli dettagli
080200090611     c                   if        In_Testata       = 'S'
080300090611     C                   eval       tes_VABcbo = VABcbo                         Senza C/Ass.
080400090611     c                   eval      CAG_porto_in_testata = 'S'
080500090611     c                   end
080600090610      **
080700090610     c     end_CAG       endSR
080800090611      * ?================================================================== */
080900090611     C*?  Anagrafica del mittente e del destinatario
081000090611      * ?================================================================== */
081100090611     C     DATI_NAD      BEGSR
081200090611      *
081300090611      * Dati destinatario
081400090611     C                   IF        nad3035 = 'CN' or
081500090611     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
081600090611      *
081700090611      * Se trovato un indirizzo di destinatario
081800090611      *   abilita la scrittura del VAB e del VAT.
081900090611     c                   eval       se_trovato_NAD ='S'
082000090611      *
082100090611     C* Reperisco nazione corrispondente destinatario
082200090611     C                   Z-ADD     1             YY                3 0
082300090611     c                   eval      *in32 = *off
082400090611     C                   if        nad3207 <> *BLANKS
082500090611     C     nad3207       LOOKUP    ISO(YY)                                32
082600090611     C                   endif
082700090611      *
082800090611     C* Imposto dati destinatario
082900090615     C                   MOVEL     nad30361      VABRSD
083000090615     C                   if        nad30362 <> *LOVAL
083100090615     C                   MOVEL     nad30362      VABRD2
083200090611     C                   endif
083300090611      *
083400090611     C                   MOVEL     nad30421      VABIND
083500090630      *
083600090630     C                   MOVEL     nad3164       VABLOD
083700090630      *
083800090611      *
083900090611     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
084000090630      *   x DELTAPLUS li mettiamo in aggiunta alla 2°ragione sociale  VABRD2
084100090611     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
084200090611      *
084300090611     C* Verifico se c'è dello spazio per nella località
084400090630     C     '    '        SCAN      VABRD2        WI                4 0
084500090611      *
084600090630     C                   MOVEA     VABRD2        LOD
084700090611     C                   ADD       1             WI
084800090611     C                   MOVEA     NAD30422      LOD(WI)
084900090630     C                   MOVEA     LOD           VABRD2
085000090611     C                   ENDif
085100090611      *
085200090611     C                   If        *in32 = *on
085300090611     C                   MOVEL     C15(YY)       VABNZD
085400090611     C                   MOVEL     CPF(YY)       WFLCPF            2 0
085500090611     c                   Else
085600090611     C                   MOVEL     *BLANKS       VABNZD
085700090611     C                   Z-ADD     0             WFLCPF
085800090611     c                   Endif
085900090611      * Converto CAP
086000090611     c                   move      nad3251       cap09a            9
086100090611     C                   EXSR      CNVCAP
086200090611     C* Se in testata
086300090611     c                   if        In_Testata       = 'S'
086400090611     c                   eval       tes_VABrsd = VABrsd
086500090611     c                   eval       tes_VABrd2 = VABrd2
086600090611     c                   eval       tes_VABind = VABind
086700090611     c                   eval       tes_VABlod = VABlod
086800090611     c                   eval       tes_VABnzd = VABnzd
086900090611     c                   eval      NAD_destinatario_in_testata = 'S'
087000090611     c                   end
087100090615      *
087200090615     C* Ricerco CAP
087300090615     C*****WCAP          IFNE      *BLANKS
087400090615      *
087500090615     C     mse6799       IFEQ      'CGW'
087600090615      *
087700090615      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
087800090615     C                   CLEAR                   FNLV55DS
087900090615     C                   MOVEL     'P'           D55TPT
088000090615     C                   MOVEL     WOGGI         D55DRF
088100090615     C                   MOVEL     VABLNP        D55LNP
088200090615     C                   MOVEL     VABLNP        D55LIN
088300090615     C                   CALL      'FNLV55R'
088400090615     C                   PARM                    FNLV55DS
088500090615     C                   MOVE      D55TFP        COMTFP            3 0
088600090615      *
088700090615     C*  PASSO IL TERMINAL PARTENZA
088800090615     C                   CLEAR                   TISI95DS
088900090615     C                   Z-ADD     COMTFP        I95TFP
089000090615     C                   MOVEL     VABTSP        I95TSP
089100090615     C                   MOVEL     'S'           I95FRE
089200090615     C                   MOVEL     '3'           I95TCN
089300090615     C                   MOVEL     WCAP          I95CAP
089400090615     C                   MOVEL     VABNZD        I95NAR
089500090615     C                   MOVEL     VABLOD        I95LOC
089600090615     C                   MOVEL     VABIND        I95IND
089700090615      *
089800090615     C* Se gestita seconda linea di arrivo passo peso - volume
089900090615     C* e numero colli effettivo
090000090615     C     §PULNA        IFEQ      'S'
090100090615     C                   Z-ADD     VABPKB        I95LKG
090200090615     C                   Z-ADD     VABVLB        I95LMC
090300090615     C                   ELSE
090400090615     C                   END
090500090615     C*
090600090615     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
090700090615     C                   SELECT
090800090615     C     VABCBO        WHENEQ    '1 '
090900090615     C                   MOVEL     *BLANKS       I95FCA
091000090615     C                   MOVEL     'F'           I95TPO
091100090615     C     VABCBO        WHENEQ    '2 '
091200090615     C                   MOVEL     *BLANKS       I95FCA
091300090615     C                   MOVEL     'A'           I95TPO
091400090615     C     VABCBO        WHENEQ    '4 '
091500090615     C                   MOVEL     'S'           I95FCA
091600090615     C                   MOVEL     'F'           I95TPO
091700090615     C     VABCBO        WHENEQ    '6 '
091800090615     C                   MOVEL     'S'           I95FCA
091900090615     C                   MOVEL     'A'           I95TPO
092000090615     C                   OTHER
092100090615     C                   MOVEL     'F'           I95TPO
092200090615     C     VABCAS        IFGT      0
092300090615     C                   MOVEL     'S'           I95FCA
092400090615     C                   ELSE
092500090615     C                   MOVEL     *BLANKS       I95FCA
092600090615     C                   END
092700090615     C                   ENDSL
092800090615     C*
092900090615     C                   CALL      'TISI95R'
093000090615     C                   PARM                    TISI95DS
093100090615     C* Reperisco i dati da CAP
093200090615     C                   MOVEL     O95PRV        VABPRD
093300090615     C                   MOVEL     O95LNA        VABLNA
093400090615     C                   MOVEL     O95ZNC        VABZNC
093500090615     C                   MOVEL     WCAP          VABCAD
093600090615     C*
093700090615     C     O95ERR        IFNE      *BLANKS
093800090615     C     O95FIT        ANDEQ     'S'
093900090615     C*  Cap non trovato
094000090615     C                   eval      vinMSG = 'MSE dal peso (CAP) non trovato'
094100090615     c                   exsr      Error_DET
094200090615     C                   goto      end_NAD
094300090615     C                   END
094400090615     C                   END
094500090615      *
094600090615     C*****              END
094700090615      *
094800090615     C     WCAP          IFEQ      *BLANKS
094900090615      *
095000090615     C                   CLEAR                   VABPRD
095100090615     C                   CLEAR                   VABLNA
095200090615     C                   CLEAR                   VABZNC
095300090615     C                   CLEAR                   VABCAD
095400090615     C*  Cap non trovato
095500090615     C*******            eval      vinMSG = 'MSE dal peso (CAP) mancante'
095600090615     c*******            exsr      Error_DET
095700090615     C*******            goto      end_MSE
095800090615     C                   END
095900090611     C*
096000090611     C                   ENDIF
096100090611      *
096200090611      * Dati mittente  ---------------
096300090611     C                   IF        nad3035 = 'CO'
096400090611     C                   movel     nad30361      VABRMO
096500090611      *
096600090611      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
096700090611     C                   movel     §PTNAZ        VABNMO
096800090611      *
096900090611     C                   IF        nad3207 <> *blanks
097000090611     C                   eval      YY = 1
097100090611     C     nad3207       LOOKUP    ISO(YY)                                32
097200090611     C                   if         *in32 = *on
097300090611     C                   MOVEL     C15(YY)       VABNMO
097400090611     C                   endif
097500090611     C                   ENDIF
097600090611      *
097700090611      * Normalizzo CAP mittente originale
097800090611     C                   if        nad3251 <> *blanks
097900090611      *
098000090611     C                   clear                   T
098100090611     C                   clear                   S
098200090611     C                   MOVEA     *BLANKS       CAPM
098300090611     C                   MOVEA     nad3251       CAP9
098400090611     C                   DO        9             T
098500090611     C     CAP9(T)       IFNE      *BLANKS
098600090611     C                   ADD       1             S
098700090611     C                   MOVE      CAP9(T)       CAPM(S)
098800090611     C                   ENDIF
098900090611     C                   ENDDO
099000090611     C*
099100090611      *     Controllo validità CAP
099200090611     C                   MOVEA     CAPM          VABCMO
099300090611      *
099400090611     C                   CLEAR                   TISI95DS
099500090611     C                   MOVEL     VABCMO        I95CAP
099600090611     C                   MOVEL     VABNMO        I95NAR
099700090611     C                   MOVEL     WOGGI         I95DAT
099800090611     C                   MOVEL     '3'           I95TCN
099900090611     C                   CALL      'TISI95R'
100000090611     C                   PARM                    TISI95DS
100100090611      *     Errore
100200090611     C     O95ERR        IFNE      *BLANKS
100300090611     C                   MOVEL     INDCAE        VABCMO
100400090611     C                   MOVEL     §PTNAZ        VABNMO
100500090611     C                   END
100600090611      *
100700090611     c                   endIF
100800090611     C* Se in testata
100900090611     c                   if        In_Testata       = 'S'
101000090611     c                   eval       tes_VABrmo = VABrmo
101100090611     c                   eval       tes_VABnmo = VABnmo
101200090611     c                   eval       tes_VABcmo = VABcmo
101300090611     c                   eval       NAD_mittente_in_testata = 'S'
101400090611     c                   end
101500090611      *
101600090611     C                   END
101700090611      *
101800090611      * NOn c'è l'identificativo bolla del cliente
101900090611     C                   IF        nad3035 = 'CN'
102000090611     C                   if          vabRSD = *blank or
102100090611     C                               vabIND = *blank or
102200090611     C                               vabLOD = *blank
102300090611     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
102400090611     c                   exsr      Error_DET
102500090611     c                   goto      end_NAD
102600090611     C                   ENDIF
102700090611     C                   ENDIF
102800090611      *
102900090611     c     end_NAD       endSR
103000090611      * ?================================================================== */
103100090611     C*? Imposta le date
103200090611      * ?================================================================== */
103300090611     C     DATI_DTM      BEGSR
103400090611     C*
103500090611     C*  Se data documento  memorizzo numero e data x CMR
103600090611     C     dtm2005       IFEQ      'DEP'
103700090611     C                   MOVEL     '20'          W_DATA8           8            Data
103800090615     C                   MOVE      dtm2001       W_DATA8
103900090611     C                   MOVEL     W_DATA8       WDATA            35            Data
104000090611     C                   MOVEL     '102'         WFORM             3            Formato
104100090611     C                   EXSR      CNVDAT
104200090611     C                   MOVEL     WCAMG         Tes_VABdcm                     Data CMR
104300090611     C     WERRO         IFEQ      'S'
104400090611     C                   eval      vinMSG = 'DTM data msg errata'
104500090611     c                   exsr      Error_DET
104600090611     c                   goto      end_DTM
104700090611     C                   END
104800090611     C                   END
104900090611      **
105000090611     C*  Se data consegna richiesta imposto già sui campi del VAB
105100090611     C     dtm2005       IFEQ      'DAD'
105200090611     C                   MOVEL     '20'          W_DATA8           8            Data
105300090615     C                   MOVE      dtm2001       W_DATA8
105400090611     C                   MOVEL     W_DATA8       WDATA            35            Data
105500090611     C                   MOVEL     '102'         WFORM             3            Formato
105600090611     C                   EXSR      CNVDAT
105700090828     C***  In accordo con il cliente NON si prende + la SUA Data cons.rich.
105800090828     C***  poichè il cliente inviava una data uguale a quella della spedizione
105900090828     C***  e non poteva modificare nulla nel suo sistema.
106000090828     C***   autorizzato da Pantalena il  28/08/2009
106100090828     C*******            MOVEL     WCAMG         VABDCR                         Data cons.rich.
106200090828     C*******            MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
106300090828     C*******            MOVEL     WHMS          VABHCR                         Ora  cons.rich.
106400090611     C     WERRO         IFEQ      'S'
106500090611     C                   eval      vinMSG = 'DTM DEP - data errata'
106600090611     c                   exsr      Error_DET
106700090611     c                   goto      end_DTM
106800090615     C                   ELSE
106900090615     C* Se in testata
107000090615     c                   if        In_Testata       = 'S'
107100090828     C*******            eval       Tes_VABdcr = VABDCR                          Data cons.rich.
107200090828     C*******            eval       Tes_VABtcr = VABTCR                          Tipo cons.rich.
107300090828     C*******            eval       Tes_VABhcr = VABHCR                          Ora  cons.rich.
107400090615     c                   endIf
107500090615     C                   END
107600090611     C                   END
107700090611      **
107800090611     c     end_DTM       endSR
107900090611      * ?================================================================== */
108000090611     C*? Descrizione della Merce (NATURA MERCE)
108100090611      * ?================================================================== */
108200090611     C     DATI_GDS      BEGSR
108300090611     C*
108400090611     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
108500090611     c                   if        gds70021 <> *blank
108600090611     C                   MOVEL     gds70021      Tes_VABnas
108700090611     c                   end
108800090611      **
108900090611     c     end_GDS       endSR
109000090609      * ?================================================================== */
109100090611     C*?  Dettagli sul trasporto
109200090609      * ?================================================================== */
109300090611     C     DATI_TDT      BEGSR
109400090609      *
109500090611      **
109600090611     c     end_TDT       endSR
109700090611      * ?================================================================== */
109800090611     C*? Beni Pericolosi
109900090611      * ?================================================================== */
110000090611     C     DATI_DGS      BEGSR
110100090611      **
110200090611      **
110300090611     c     end_DGS       endSR
110400090611      * ?================================================================== */
110500090611     C*? Documentazione
110600090611      * ?================================================================== */
110700090611     C     DATI_DOC      BEGSR
110800090611      **
110900090611      * Numero di ricevuta
111000090611     C                   if        doc1001 = 'WBL' and doc1791 = 'ACG'
111100090611     C                                     and doc1004 <> *blank
111200090611      *
111300090611     C                   movel     doc1004       WCU
111400090611     C                   movel     WCU           WNUM             35
111500090611     C     ' '           scan      WCU           WLUNGH
111600090611     C                   sub       1             WLUNGH
111700090611     C                   EXSR      TSTNUM
111800090611     C                   If          WOKNUM = 'S'                               Ctrl numerico
111900090611     C                   movel     WNUMOK        VABRMN
112000090611     C                   movel     'WBL'         WRMN
112100090611     C                   Endif
112200090611     C                   ENDIF
112300090611      **
112400090611     c     end_DOC       endSR
112500090611      * ?================================================================== */
112600090611     C*? Testata intermedia Inizio e Fine Dettaglio
112700090611      * ?================================================================== */
112800090611     C     DATI_UNS      BEGSR
112900090611      *
113000090615     c                   eval      In_Testata =' '
113100090615      *
113200090611      *   Inizio Dettaglio
113300090611     c                   if         UNS0081  = 'D'
113400090611     c                   end
113500090611      *
113600090611      *   Fine   Dettaglio
113700090611     c                   if         UNS0081  = 'S'
113800090611     c                   end
113900090609      **
114000090609     c     end_UNS       endSR
114100090609      * ?================================================================== */
114200090609     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
114300090609      * ?================================================================== */
114400090609     C     Nuova_spediz  BEGSR
114500090609      **
114600090227      * Pulisco campi del file
114700090227     c                   z-add     vnrec         SAVNUM_RECord
114800090227     C                   CLEAR                   EDiVAB00
114900090227     C                   CLEAR                   EDiVAT00
115000090227     C                   CLEAR                   EDiVAD00
115100090611      *
115200090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
115300090227     C                   MOVEL     'N'           WGID99            1
115400090611      *
115500090227     C                   clear                   XY                5 0
115600090227     C                   clear                   §§                5 0
115700090611      *
115800090227     C                   clear                   WNSP             35
115900090227     C                   MOVEA     *HIVAL        SGN
116000090611      *
116100090227     C*  Azzero codice cliente - tariffa
116200090227     C                   Z-ADD     0             WTOCOC           16 0
116300090227     C                   Z-ADD     0             WTOPLC           16 2          ???
116400090227     C                   Z-ADD     0             WTOPNC           16 2          ???
116500090611      *
116600090611     C                   clear                   QUA
116700090611     C                   clear                   FTX
116800150120     c                   eval        TXT_Nome_File_PDF = *blank
116900090611      *
117000090227     C*  Azzero codice cliente - tariffa
117100090227     C                   clear                   WCLKSC
117200090227     C                   clear                   WCLTAR
117300090227     C                   clear                   WCLLNP
117400090227     C                   clear                   WCLNRS
117500090227     C                   clear                   WCLCTM
117600090227     C                   clear                   WCLBS1
117700090227     C                   clear                   WCLfnsp                        *blk/S
117800090227     C                   clear                   WNOT1            35
117900090227     C                   clear                   WNOT2            35
118000090227      *
118100090227     C* pulisce campi di work
118200090227     c                   clear                   watrde           35
118300090227     c                   clear                   wattel           25
118400090227     C                   clear                   WRMN              3
118500090227      *
118600090227      * Un errore nel dettaglio
118700090227     C                   clear                   Err_in_detta      1
118800090227     c                   clear                   errori_in_Wrt     1
118900090227     c                   clear                   se_trovato_NAD    1
119000090227      **
119100090611      **  imposta i dati x la testata
119200090611     c                   exsr      DATI_TESTATA
119300090611      **
119400090227     c                   endSR
119500090609      * ?================================================================== */
119600090609     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
119700090609      * ?================================================================== */
119800090611     C     DATI_TESTATA  BEGSR
119900090609      *
120000090611      **  IMPOSTA i campi dalla testata
120100090611     C                   eval       VABLNP = Tes_VABlnp
120200090611     C                   eval       VABNRS = Tes_VABnrs
120300090611     C                   eval       VABCTM = Tes_VABctm
120400090611      **
120500090611     C                   eval      VABdcm = Tes_VABdcm
120600090611     C*
120700090611     c                   eval      VABvca = tes_Valuta
120800090611     c                   eval      VABcbo = tes_VABcbo
120900090611     C*
121000090828     C*******   Come da Accordi presi con il cliente la Data consegna richiesta
121100090828     C*******    NON verrà presa in considerazione
121200090828     c*******            eval      VABdcr = Tes_VABdcr
121300090828     c*******            eval      VABtcr = Tes_VABtcr
121400090828     c*******            eval      VABhcr = Tes_VABhcr
121500090611     C*
121600090611     c                   eval      VABnas = tes_VABnas
121700090611     c                   eval      VABtsp = tes_VABtsp
121800090611     C*
121900090611      * Se Mittente o Destinatario presi dalla Testata del messaggio:
122000090611     C* Se in testata
122100090611     c                   if        NAD_destinatario_in_testata = 'S'
122200090611     c                   eval         VABrsd =  tes_VABrsd
122300090611     c                   eval         VABrd2 =  tes_VABrd2
122400090611     c                   eval         VABind =  tes_VABind
122500090611     c                   eval         VABlod =  tes_VABlod
122600090611     c                   eval         VABnzd =  tes_VABnzd
122700090611     c                   end
122800090611     C* Se in testata
122900090611     c                   if        NAD_mittente_in_testata = 'S'
123000090611     c                   eval         VABrmo = tes_VABrmo
123100090611     c                   eval         VABnmo = tes_VABnmo
123200090611     c                   eval         VABcmo = tes_VABcmo
123300090611     c                   end
123400090611      **
123500090611     c                   endSR
123600090611      * ?================================================================== */
123700090611     C*?  Riferimento Spedizione del cliente
123800090611      * ?================================================================== */
123900090611     C     DATI_RFF      BEGSR
124000090611      *
124100090609      *  Prepara una nuova spedizione
124200090609      * ?                         --------------
124300090609     c                   exsr      Nuova_spediz
124400090609      * ?                         --------------
124500090609     C                   clear                   WAGE             35
124600110224     C                   clear                   WFF              35
124700110224     C                   clear                   WCU              35
124800090609      *
124900090609      *  Trascodifico riferimento spedizione
125000090609     c                   if        RFF1153 = 'SEN' and RFF1154 <> *BLANKS
125100090609     C                   movel(p)  RFF1154       WAGE             35
125200090609     c                   end
125300090609      **
125400090609     C                   IF         WAGE <> *BLANKS
125500090609     C                   movel     WAGE          WNSP             35
125600090609     C                   movel     WAGE          WNUM             35
125700090609     C     ' '           scan      WAGE          WLUNGH            3 0
125800090609     C                   sub       1             WLUNGH
125900090609     C                   EXSR      TSTNUM
126000090609     C                   IF         WOKNUM = 'S'                                Ctrl numerico
126100090609     C                   movel     WAGE          VABRMA
126200090609     C                   IF         WRMN = 'SEN' or WRMN = *blanks
126300090609     C                   movel     WNUMOK        VABRMN
126400090609     C                   movel     'SEN'         WRMN
126500090609     C                   ENDIF
126600090609     C                   ENDIF
126700090609     C                   ENDIF
126800090609      *
126900090609      * Codice Cliente di riferimento
127000090609     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
127100090609     C                   movel     RFF1154       WFF
127200090609     c                   END
127300090609      *
127400090609      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
127500090609     C     WFF           IFNE      *BLANKS
127600090609     C                   eval      XX = 1
127700090609     C                   movel     §PTKSC        WCCL             35
127800090609     C                   movel     WFF           WCOM28           28
127900090609     C                   move      WCOM28        WCCL
128000090609     C     WCCL          lookup    CCL(XX)                                34
128100090609     C     *IN34         IFEQ      '1'
128200090609     C                   movel     DCL(XX)       EDIDSCL
128300090611     C* Se in testata
128400090611     c                   if        In_Testata       = 'S'
128500090611      *
128600090611     C                   z-add     §CLKSC        $CLKSC
128700090611     C                   movel     §CLTAR        $CLTAR
128800090611     C                   z-add     §CLLNP        $CLLNP
128900090611     C                   z-add     §CLNRS        $CLNRS
129000090611     C                   movel     §CLCTM        $CLCTM
129100090611     C                   movel     §CLBS1        $CLBS1
129200090611     C                   movel     §CLnsp        $CLfnsp
129300090611      *
129400090611     c                   else
129500090611      *
129600090609     C                   z-add     §CLKSC        WCLKSC            7 0
129700090609     C                   movel     §CLTAR        WCLTAR            3
129800090609     C                   z-add     §CLLNP        WCLLNP            3 0
129900090609     C                   z-add     §CLNRS        WCLNRS            2 0
130000090609     C                   movel     §CLCTM        WCLCTM            2
130100090609     C                   movel     §CLBS1        WCLBS1            1
130200090609     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
130300090611      *
130400090611     c                   end
130500090609      *
130600090609      * imposta la LNP   x il dettaglio specifco
130700090611      * imposta la Serie x il dettaglio specifco
130800090609     c                   eval      VABlnp = §CLlnp
130900090609     c                   eval      VABnrs = §CLnrs
131000090609      *
131100090609      *  In base al cod.trattamento merce reperisco tipo tratt.
131200090609      *    per un dettaglio specifico.
131300090609     C                   CLEAR                   DS1B
131400090609     C                   Z-ADD     1             Y                 3 0
131500090609     C     §CLctm        LOOKUP    CTM(Y)                                 32
131600090609     C     *IN32         IFEQ      '1'
131700090609     C                   MOVEL     DTM(Y)        DS1B
131800090609     C                   END
131900090609      *
132000090609      * Se il cliente non era sulla schiera lo aggiunge
132100090609     C     §CLKSC        lookup    KCL                                    39
132200090609     C                   if        *in39 = *off
132300090609     C                   add       1             WW
132400090609     C                   z-add     §CLKSC        KCL(WW)
132500090609     c                   endif
132600090609      *
132700090609     C                   ENDIF
132800090609     C                   ENDIF
132900090609      *
133000090611      * NON c'è l'identificativo bolla del cliente
133100090611     c                   if        (RFF1153 = 'SEN' and RFF1154 = *BLANKS)
133200090611     C                   eval      vinMSG = 'RFF riferimento cliente assente'
133300090609     c                   exsr      Error_DET
133400090609     c                   goto      end_RFF
133500090609     C                   ENDIF
133600090609      *
133700090609     c     end_RFF       endSR
133800090611      * ?================================================================== */
133900090611     C*?  Dati di dettaglio della spedizione e dei colli
134000090611      * ?================================================================== */
134100090611     C     DATI_GID      BEGSR
134200090611      *
134300090611     C* Il numero dei colli presenti nella spedizione
134400090611     C     GID706520     IFEQ      21
134500090611     C                   MOVEL     'S'           WGID99
134600090611     C                   z-add     GID722420     VABNCL
134700090611     C                   ELSE
134800090611     C     WGID99        IFEQ      'N'
134900090611     C                   z-add     gid722420     WTNCL             5 0
135000090611     C                   ADD       WTNCL         VABNCL
135100090611     C                   END
135200090611     C                   END
135300090611      *
135400090611     C                   ADD       VABNCL        WTOCOC           16 0
135500090611      *
135600090611     c     end_GID       endSR
135700090611      * ?================================================================== */
135800090611     C*?  Dati di dettaglio misure spedizione e colli
135900090611      * ?================================================================== */
136000090611     C     DATI_MSE      BEGSR
136100090611      *
136200090611     C*  Peso
136300090611     C     mse6799       IFEQ      'CGW'
136400090611      *
136500090611     C*  Controllo se ho valore totale o parziale
136600090611      *   sempre il Lordo
136700090611     C     mse6410       IFeq      'KG '
136800090611     C     WGID99        IFEQ      'S'
136900090611     C                   move      mse6314       mse6314_2        18 2          Peso
137000090611     C                   Z-ADD     mse6314_2     VABPKB                         Peso
137100090611     C                   Else
137200090611     C                   move      mse6314       mse6314_2                      Peso
137300090611     C                   ADD       mse6314_2     VABPKB                         Peso
137400090611     C                   End
137500090611     C                   ENDif
137600090615      *
137700090615     C                   END
137800090611      ***
137900090611     c     end_MSE       endSR
138000090209      * ?================================================================== */
138100090213     C*?  C.O.D. Monetary Amount
138200090209      * ?================================================================== */
138300090609     C     DATI_VAL      BEGSR
138400090210      *
138500090216     C* Controllo se campo importo numerico
138600090216      * con 3 decimali
138700090611     C     val5004       IFNE      *zeros
138800090216      *
138900090611     C                   If         val5297 = 'DE '
139000090630     C     val5004       div       100           VABIAS                         IMPORTO
139100090611     C                   MOVEL     val6345       VABVAS                         ASSICURATO
139200090216     C                   END
139300090216      *
139400090611     C                   If         §PUCAS = 'S' and val5297 = 'RE '            C/Assegno
139500090630     C     val5004       div       100           VABCAS
139600090611     C                   MOVEL     val6345       VABVCA
139700141016      *  default EUR
139800141016      *    il cliente aveva smesso di inviare la divisa senza avvisarci
139900141016     c                   if        vabvca = *blank
140000141016     c                   eval      vabVCA ='EUR'
140100141016     c                   end
140200090611      *
140300090216     C                   END
140400090216      *
140500090611     C                   END
140600090213      *
140700090611     c     end_VAL       endSR
140800091104      * ?================================================================== */
140900091104     C*?  Contatto a cui far riferimento
141000091104      * ?================================================================== */
141100091104     C     DATI_CTA      BEGSR
141200091104      *
141300091104      *  memorizza il riferimento di consegna x il destinatario
141400091104      *  memorizza il riferimento di consegna nr.telefonico
141500091104     C     cta3412       ifne      *BLANKS
141600091104     c                   movel     cta3412       watrde
141700091104     c                   end
141800091104      *
141900091104     c                   endSR
142000091104      * ?================================================================== */
142100091104     C*?  Contatto telefonico  a cui far riferimento
142200091104      * ?================================================================== */
142300091104     C     DATI_COM      BEGSR
142400091104      *
142500091104     C     com3148       ifne      *BLANKS
142600091104     c                   movel     com3148       wattel
142700091104     c                   end
142800091104      *
142900091104     C                   ENDSR
143000090213      * ?================================================================== */
143100090213     C*?  FREE TEXT
143200090213      * ?================================================================== */
143300090611     C     DATI_TXT      BEGSR
143400090213      *
143500150120      *
143600150120      * ? se inviano il NOME del PDF per la copia ordine (VAX)
143700150120     C                   if        txt0077 ='P10'  and
143800150120     C                             txt0078 <> *blank
143900150120     c                   eval      TXT_Nome_File_PDF = txt0078
144000150120     c                   goto      end_TXT
144100150120     c                   end
144200150120      *
144300150120      *
144400090611      * ? solo se si tratta di note di consegna
144500100311     C*******            if        txt0077 ='DEL'  and
144600100311     C*******                      txt0078 <> *blank
144700090629      *
144800100311     C                   if        txt0077 ='DEL'
144900100311      *
145000090702     c                   movel     txt0078       tipo_payment      2
145100090629      *
145200090629     c                   If         tipo_payment = 'TM'
145300100311     C                               and  %subst(txt0078:3:1)=' '
145400090629     C                   MOVEL     'TM'          VABTIC
145500090629      *
145600090629     c                   elseIf     tipo_payment = 'BM'
145700100311     C                               and  %subst(txt0078:3:1)=' '
145800090629     C                   MOVEL     'BM'          VABTIC
145900090629      *
146000090629     c                   elseIf     tipo_payment = 'CM'
146100100311     C                               and  %subst(txt0078:3:1)=' '
146200090629     C                   MOVEL     'CM'          VABTIC
146300090629      *
146400090629     c                   elseIf     tipo_payment = '  '
146500100311     C                               and  txt0078 = *BLANK
146600090629     C                   MOVEL     '  '          VABTIC
146700090629      *
146800100311     c                   elseIF     txt0078 <> *BLANK
146900090629      *
147000090611     C     WNOT1         IFEQ      *BLANKS
147100090702     C                   MOVEL     txt0078       WNOT1
147200090702     C                   MOVE      txt0078       WNOT2
147300090611     C                   ELSE
147400090611     C     WNOT2         IFEQ      *BLANKS
147500090702     C                   MOVEL     txt0078       WNOT2
147600090611     C                   END
147700090611     C                   END
147800090629      *
147900090629     c                   endIf
148000090629      *
148100090629     c                   end
148200090213      *
148300090611     c     end_TXT       endSR
148400090210      * ?================================================================== */
148500090210     C*?  Dati di dettaglio Codice a barre Segnacolli
148600090210      * ?================================================================== */
148700090210     C     DATI_PCI      BEGSR
148800090210      *
148900090611      *  Contiene i singoli segnacolli
149000090611     c                   if        PCI4233 ='23 '
149100090611      *
149200090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
149300090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
149400090210     C     §1BF12        IFEQ      'E'
149500090210     C                   EXSR      SEGNEE
149600090210     C                   ELSE
149700090210      *
149800090210      *  La prima volta controllo congruità numero segnacollo
149900090210     C     §PUBS1        IFNE      *BLANKS
150000090210     C     VABnrs        ANDNE     0
150100090210      *  Se il codice trattamento merce prevede che segnacollino i
150200090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
150300090210     C     §1BFG7        IFEQ      'S'
150400090210     C     §1BFG1        OREQ      'S'
150500090210      *  calcolo segnacollo
150600090210     C                   EXSR      SGNELA
150700090210     C                   END
150800090210     C                   END
150900090210      *
151000090210     C                   END
151100090210      *
151200090611     C                   ENDif
151300090611      *
151400090210     c     end_PCI       endSR
151500090211     C*----------------------------------------------------------------
151600090211      *? dati finali del dettaglio    UNT
151700090211     C*----------------------------------------------------------------
151800090211     C     DATI_UNT      BEGSR
151900090211      *
152000090213     c                   eval      NUM_RECord = vnREC
152100090213     c                   z-add     vnrec         last_RECord
152200090211      **
152300090211     c                   endSR
152400090211     C*----------------------------------------------------------------
152500090211      *? dati finali     UNT
152600090211     C*----------------------------------------------------------------
152700090211     C     DATI_UNZ      BEGSR
152800090211      *
152900090211      **
153000090211     c                   endSR
153100090210      *----------------------------------------------------------------
153200090611      *? Input segnacolli su schiera di 10 elementi
153300090210      *----------------------------------------------------------------
153400090210     C     INP_Segnacol  BEGSR
153500090210      *
153600090210     c                   clear                   quanti_PCI        3 0
153700090210     c                   if        §puEL1 = 0
153800090210     c                   z-add     10            quanti_PCI
153900090210     c                   else
154000090210     c                   z-add     §puEL1        quanti_PCI
154100090210     c                   end
154200090210      *
154300090210      *  Pulisce la schiera dei segnacolli da elaborare
154400090210     c                   clear                   x30
154500090210     c                   z-add     0             Nr_x30            3 0
154600090210      *
154700090210      *   riporto tutto sulla schiera generica X30
154800090210      *
154900090210     c                   do        10            limite            3 0
155000090210      * Limita quanti elementi passati in ogni PCI
155100090210      *   devono essere considerati
155200090210     c                   if        limite > quanti_PCI
155300090210     c                   Leave
155400090210     c                   end
155500090210      *
155600090210      *  Carica la schiera generale di tutti i segnacolli
155700090210     c                   if        D30_1(limite) <> *blank
155800090210     c                   add       1             Nr_x30
155900090210     c                   eval      X30(Nr_x30) = D30_1(limite)
156000090210     c                   end
156100090210     c                   enddo
156200090210      *
156300090210     C                   ENDSR
156400090210      *----------------------------------------------------------------
156500090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
156600090210      *----------------------------------------------------------------
156700090210     C     SEGNEE        BEGSR
156800090210      *
156900090210     c                   exsr      INP_Segnacol
157000090210      *
157100090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
157200090210     C                   DO        10            x                              --- 01 ---
157300090210      *
157400090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
157500090210     C     x30(X)        ANDNE     *LOVAL
157600090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
157700090210     C                   ADD       1             §§
157800090210     C                   MOVEL     x30(X)        SGE(§§)
157900090210     C                   END                                                    --- 03 ---
158000090210      *
158100090210     C                   END                                                    --- 02 ---
158200090210      *
158300090210     C                   ENDSR
158400090210     C*----------------------------------------------------------------
158500090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
158600090210     C*----------------------------------------------------------------
158700090210     C     SGNELA        BEGSR
158800090210      *
158900090210     c                   exsr      INP_Segnacol
159000090210     C*
159100090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
159200090210     C*  in base al numero elementi validi carico sgn.
159300090210     C                   DO        10            X                              --- 01 ---
159400090210     C*
159500090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
159600090210     C     x30(X)        ANDNE     *LOVAL
159700090210     C*
159800090210     C                   CLEAR                   DSSGN
159900090210     C*  Controllo se devo ricevere la linea di partenza
160000090210     C                   MOVEL     'N'           WERRO             1
160100090210     C     §PULP1        IFGT      0                                            --- 03 ---
160200090210     C                   EXSR      REPLNP
160300090210     C                   END                                                    --- 03 ---
160400090210     C*  Controllo se devo ricevere la linea di arrivo
160500090210     C     §PULA1        IFGT      0                                            --- 03 ---
160600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
160700090210     C                   EXSR      REPLNA
160800090210     C                   END                                                    --- 03 ---
160900090210     C*  Controllo se devo ricevere il numero di serie
161000090210     C     §PUNR1        IFGT      0                                            --- 03 ---
161100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
161200090210     C                   EXSR      REPNRS
161300090210     C                   END                                                    --- 03 ---
161400090210     C*  Controllo se devo ricevere il numero segnacollo
161500090210     C     §PUSG1        IFGT      0                                            --- 03 ---
161600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
161700090210     C                   EXSR      REPSGN
161800090210     C                   END                                                    --- 03 ---
161900090210     C*  Controllo se devo ricevere la zona di consegna
162000090210     C     §PUZN1        IFGT      0                                            --- 03 ---
162100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
162200090210     C                   EXSR      REPZNC
162300090210     C                   END                                                    --- 03 ---
162400090210     C*  Imposto da VAB i dati a zero se mi è stato passato
162500090210     C*  numero segnacollo valido
162600090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
162700090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
162800090210     C                   Z-ADD     VABLNP        SGNLNP
162900090210     C                   END                                                    --- 04 ---
163000090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
163100090210     C                   Z-ADD     VABLNA        SGNLNA
163200090210     C                   END                                                    --- 04 ---
163300090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
163400090210     C                   Z-ADD     VABNRS        SGNNRS
163500090210     C                   END                                                    --- 04 ---
163600090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
163700090210     C                   Z-ADD     VABZNC        SGNZNC
163800090210     C                   END                                                    --- 04 ---
163900090210      *
164000090210     C*  mi reimposto LNA e zona da segnacollo
164100090210     C                   Z-ADD     SGNLNA        VABLNA
164200090210     C                   Z-ADD     SGNZNC        VABZNC
164300090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
164400090210     C                   ADD       1             XY
164500090210     C                   MOVE      SGNNSC        SGN(XY)
164600090210     C                   END                                                    --- 03 ---
164700090210     C*
164800090210     C                   END                                                    --- 02 ---
164900090210     C*
165000090210     C                   END                                                    --- 01 ---
165100090210      *
165200090210     C                   ENDSR
165300090210     C*----------------------------------------------------------------
165400090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
165500090210     C*----------------------------------------------------------------
165600090210     C     REPLNP        BEGSR
165700090210     C*
165800090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
165900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
166000090210     C*  dal numero segnacollo passatomi dal partner
166100090210     C                   Z-ADD     §PULP1        WP                2 0
166200090210     C     3             SUBST     x30(X):WP     WLNP              3
166300090210     C*
166400090210     C*  controllo che la lnp calcolata sia numerica
166500090210     C                   TESTN                   WLNP                 98
166600090210     C*
166700090210     C*  se è numerica la carico
166800090210     C     *IN98         IFEQ      '1'
166900090210     C                   MOVE      WLNP          SGNLNP
167000090210     C                   ELSE
167100090210     C*  se non è numerica stampo errore
167200090210     C                   MOVEL     'S'           WERRO             1
167300090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
167400090213     c                   exsr      Error_DET
167500090210     C                   goto      end_LNP
167600090210     C                   END
167700090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
167800090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
167900090210     C     SGNLNP        IFNE      VABLNP
168000090210     C                   MOVEL     'S'           WERRO             1
168100090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
168200090210     c                             a della Bolla'
168300090213     c                   exsr      Error_DET
168400090210     C                   goto      end_LNP
168500090210     C                   END
168600090210     C*
168700090210     C     end_LNP       ENDSR
168800090210     C*----------------------------------------------------------------
168900090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
169000090210     C*----------------------------------------------------------------
169100090210     C     REPLNA        BEGSR
169200090210     C*
169300090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
169400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
169500090210     C*  dal numero segnacollo passatomi dal partner
169600090210     C                   Z-ADD     §PULA1        WP                2 0
169700090210     C     3             SUBST     x30(X):WP     WLNA              3
169800090210     C*  controllo che la lna calcolata sia numerica
169900090210     C                   TESTN                   WLNA                 98
170000090210     C*  se è numerica la carico
170100090210     C     *IN98         IFEQ      '1'
170200090210     C                   MOVE      WLNA          SGNLNA
170300090210     C                   ELSE
170400090210     C*  se non è numerica stampo errore
170500090210     C                   MOVEL     'S'           WERRO             1
170600090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
170700090213     c                   exsr      Error_DET
170800090210     C                   goto      end_LNA
170900090210     C                   END
171000090210     C*
171100090210     C     end_LNA       ENDSR
171200090210     C*----------------------------------------------------------------
171300090210     C*? REPNRS - Reperisce numero serie
171400090210     C*----------------------------------------------------------------
171500090210     C     REPNRS        BEGSR
171600090210     C*
171700090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
171800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
171900090210     C*  dal numero segnacollo passatomi dal partner
172000090210     C                   Z-ADD     §PUNR1        WP                2 0
172100090210     C     2             SUBST     x30(X):WP     WNRS              2
172200090210     C*  controllo che il nrs calcolata sia numerico
172300090210     C                   TESTN                   WNRS                 98
172400090210     C*  se è numerica la carico
172500090210     C     *IN98         IFEQ      '1'
172600090210     C                   MOVE      WNRS          SGNNRS
172700090210     C                   ELSE
172800090210     C*  se non è numerico stampo errore
172900090210     C                   MOVEL     'S'           WERRO             1
173000090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
173100090213     c                   exsr      Error_DET
173200090210     C                   goto      end_NRS
173300090210     C                   END
173400090210      *
173500090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
173600090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
173700090210     C     SGNNRS        IFNE      VABNRS
173800090210     C                   MOVEL     'S'           WERRO             1
173900090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
174000090210     c                             o della Bolla'
174100090213     c                   exsr      Error_DET
174200090210     C                   goto      end_NRS
174300090210     C                   END
174400090210     C*
174500090210     C     end_NRS       ENDSR
174600090210     C*----------------------------------------------------------------
174700090210     C*? REPZNC - Reperisce zona consegna
174800090210     C*----------------------------------------------------------------
174900090210     C     REPZNC        BEGSR
175000090210     C*
175100090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
175200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
175300090210     C*  dal numero segnacollo passatomi dal partner
175400090210     C                   Z-ADD     §PUZN1        WP                2 0
175500090210     C     2             SUBST     x30(X):WP     WZNC              2
175600090210     C*  controllo che la zona calcolata sia numerica
175700090210     C                   TESTN                   WZNC                 98
175800090210     C*  se è numerica la carico
175900090210     C     *IN98         IFEQ      '1'
176000090210     C                   MOVE      WZNC          SGNZNC
176100090210     C                   ELSE
176200090210     C*  se non è numerica stampo errore
176300090210     C                   MOVEL     'S'           WERRO             1
176400090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
176500090213     c                   exsr      Error_DET
176600090210     C                   goto      end_ZNC
176700090210     C                   END
176800090210     C*
176900090210     C     end_ZNC       ENDSR
177000090210     C*----------------------------------------------------------------
177100090210     C*? REPSGN - Reperisce numero segnacollo
177200090210     C*----------------------------------------------------------------
177300090210     C     REPSGN        BEGSR
177400090210     C*
177500090210     C*  se viene passata nr.segnacollo estraggo sottostringa
177600090210     C*  lunga 7 chr (a partire dalla posizione iniziale
177700090210     C*  indicata in tabella)
177800090210     C*  dal numero segnacollo passatomi dal partner
177900090210     C                   Z-ADD     §PUSG1        WP                2 0
178000090210     C     7             SUBST     x30(X):WP     WSGN              7
178100090210     C*  controllo che il nr.segnacollo calcolato sia numerico
178200090210     C                   TESTN                   WSGN                 98
178300090210     C*  se è numerico lo carico
178400090210     C     *IN98         IFEQ      '1'
178500090210     C                   MOVE      WSGN          SGNNSC
178600090210     C                   ELSE
178700090210     C*  se non è numerica stampo errore
178800090210     C                   MOVEL     'S'           WERRO             1
178900090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
179000090213     c                   exsr      Error_DET
179100090210     C                   goto      end_SGN
179200090210     C                   END
179300090210     C*
179400090210     C     end_SGN       ENDSR
179500090611     C*----------------------------------------------------------------
179600090611     C*?   Inizio Messaggio
179700090611     C*----------------------------------------------------------------
179800051205     C     *INZSR        BEGSR
179900051205      *
180000991129     c     *ENTRY        PLIST
180100060613     C                   parm                    esito             1
180200971215      *
180300050112     C     KTABel        KLIST
180400050112     C                   KFLD                    TBLKUT
180500050112     C                   KFLD                    TBLCOD
180600050112     C                   KFLD                    TBLKEY
180700090210      *
180800090210     C     K_TAB1L       KLIST
180900090213     C                   KFLD                    etbUNB
181000090213     C                   KFLD                    etbSGM
181100090213     C                   KFLD                    etbVALSGM
181200090209     C*
181300090209     C* Definisco chiavi di accesso
181400090209     C     KTAB1         KLIST
181500090209     C                   KFLD                    KKUT
181600090209     C                   KFLD                    KCOD
181700090209     C*
181800090209     C     KNUF          KLIST
181900090209     C                   KFLD                    KAAA
182000090209     C                   KFLD                    KCNU
182100090209     C                   KFLD                    KFIL
182200090209      *
182300090209     C     KVAB1         KLIST
182400090209     C                   KFLD                    KCCM
182500090209     C                   KFLD                    KCMR
182600090209     C                   KFLD                    KCNT
182700090209     C                   KFLD                    KAAS
182800090209     C                   KFLD                    KLNP
182900090209     C                   KFLD                    KNRS
183000090209     C                   KFLD                    KNSP
183100090209      *
183200090209     C     KVAB2         KLIST
183300090209     C                   KFLD                    KCCM
183400090209     C                   KFLD                    KCMR
183500090209      *
183600090209     C     KRDE          KLIST
183700090209     C                   KFLD                    KAAS
183800090209     C                   KFLD                    KLNP1
183900090209     C                   KFLD                    KNRS
184000090209     C                   KFLD                    KNSP
184100090209     C                   KFLD                    KNMC
184200090209     C                   KFLD                    KNRP
184300090209      *
184400090209     C     KACO          KLIST
184500090209     C                   KFLD                    KKUT
184600090209     C                   KFLD                    KKCC
184700090209     C                   KFLD                    KKSC
184800090209      *
184900090209     C     KIND          KLIST
185000090209     C                   KFLD                    KKUT
185100090209     C                   KFLD                    KKCC
185200090209     C                   KFLD                    KKSC
185300090209      *
185400090209      * DETERMINO SE SONO BARTOLINI O SDI
185500090209     C                   CLEAR                   TIBS55DS
185600090209     C                   MOVEL     'L'           I50TLA
185700090209     C                   CALL      'TIBS55R'
185800090209     C                   PARM                    TIBS55DS
185900090209      *
186000090209     C* RECUPERO DATI DELL'UTENTE
186100090209     C                   Z-ADD     1             CODUT             1 0
186200090209     C                   CALL      'XPARUT'
186300090209     C                   PARM                    UTEDSE0F
186400090209     C                   MOVEL     RAGUT         RSUT             20
186500090209     C*
186600090209     C* RICERCA CAPOCONTI
186700090209     C                   DO        50            X
186800090209     C                   MOVE      TCU(X)        TCUDS
186900090209     C     F56           IFEQ      'CG'
187000090209     C     F34           ANDEQ     '01'
187100090209     C                   Z-ADD     KCU(X)        KCI               4 0
187200090209     C                   END
187300090209     C                   END
187400090209     C*
187500090211     c                   movel(p)  'EDPAB'       User_Msg         10
187600090211     C*
187700090209     C* IMPOSTO A ZERO VARIABILI DI PGM
187800090209     c                   move      *blank        Snd_Msg_alert     1
187900090209     C                   MOVEL     CA(1)         WCA              78
188000090209     C                   MOVEL     CN(1)         WCN              78
188100090209     C*
188200090209     C* RECUPERO DATA E ORA
188300090209     C                   TIME                    WHHDAT           14 0
188400090209     C                   MOVE      WHHDAT        G02DAT
188500090209     C                   MOVE      WHHDAT        WDATA8            8 0
188600090209     C                   MOVE      WDATA8        WAA2              2 0
188700090209     C                   MOVEL     WDATA8        DATA              6 0
188800090209     C                   MOVE      WAA2          DATA
188900090209     C                   MOVEL     *BLANK        G02ERR
189000090209     C                   CALL      'XSRDA8'
189100090209     C                   PARM                    WLBDA8
189200090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
189300090209     C                   TIME                    WORA              6 0
189400090209      * Recupero data e ora
189500090209     C                   TIME                    W0140
189600090209      * UDATE IN GGMMAAAA
189700090209     C                   MOVE      W0140         WDTGIO
189800090209      * UDATE IN AAAAMMGG
189900090209     C     *eur          MOVEL     WDTGIO        DATA_eur
190000090209     C     *iso          MOVEL     DATA_eur      dateu
190100090209      *
190200090209      * ?              /*--------------------------- */
190300090209     c                   exsr      CARICA_TABELLE
190400090209      * ?              /*--------------------------- */
190500090209      *
190600090209     C                   MOVEL     *ALL'-'       CMP132          132
190700991124      *
190800991124     C                   ENDSR
190900060621      * ?------------------------------------------------------------------ */
191000090209      *?    CARICA TABELLE SU SCHIERE
191100060621      * ?------------------------------------------------------------------ */
191200090209     C     CARICA_TABELLEBEGSR
191300090205      *
191400090209     C* Caricamento Tabella 1B
191500090209     C                   Z-ADD     0             X                 4 0
191600090209     C                   Z-ADD     CODUT         KKUT
191700090209     C                   MOVEL     '1B'          KCOD
191800090209     C     KTAB1         CHAIN     TABEL00F                           30
191900090209     C     *IN30         DOWEQ     '0'
192000090209     C     TBLFLG        IFEQ      *BLANKS
192100090209     C                   ADD       1             X
192200090209     C                   MOVEL     TBLKEY        CTM(X)
192300090209     C                   MOVEL     TBLUNI        DTM(X)
192400090209     C                   END
192500090209     C     KTAB1         READE     TABEL00F                               30
192600090209     C                   END
192700090209      *
192800090209     C* Caricamento Tabella 15
192900090209     C                   Z-ADD     0             X                 4 0
193000090209     C                   Z-ADD     CODUT         KKUT
193100090209     C                   MOVEL     '15'          KCOD
193200090209     C     KTAB1         CHAIN     TABEL00F                           30
193300090209     C     *IN30         DOWEQ     '0'
193400090209     C     TBLFLG        IFEQ      *BLANKS
193500090209     C                   ADD       1             X
193600090209     C                   MOVEL     TBLKEY        C15(X)
193700090209     C                   MOVEL     TBLUNI        DS15
193800090611     C                   MOVEL     §15ISO        ISO(X)
193900090209     C                   MOVEL     §15ECF        CPF(X)
194000090209     C                   MOVE      §15PCF        CPF(X)
194100090209     C                   END
194200090209     C     KTAB1         READE     TABEL00F                               30
194300090209     C                   END
194400090209      *
194500090209     C* Caricamento Tabella CV
194600090209     C                   Z-ADD     0             X
194700090209     C                   Z-ADD     CODUT         KKUT
194800090209     C                   MOVEL     'CV'          KCOD
194900090209     C     KTAB1         CHAIN     TABEL00F                           30
195000090209     C     *IN30         DOWEQ     '0'
195100090209     C     TBLFLG        IFEQ      *BLANKS
195200090209     C                   ADD       1             X
195300090209     C                   MOVEL     TBLKEY        CCV(X)
195400090209     C                   MOVEL     TBLUNI        DCV(X)
195500090209     C                   END
195600090209     C     KTAB1         READE     TABEL00F                               30
195700090209     C                   END
195800090216      *
195900090216     C* Caricamento Tabella Tipo Incasso C/Assegno
196000090216     C                   Z-ADD     0             X
196100090216     C                   MOVEL     'TC'          TABCOD
196200090216     C     TABCOD        CHAIN     EDTAB01L                           30
196300090216     C     *IN30         DOWEQ     '0'
196400090216     C     TABFLG        IFEQ      *BLANKS
196500090216     C                   ADD       1             X
196600090216     C                   MOVEL     TABKEY        CTC35(X)
196700090216     C                   MOVEL     TABUNI        TPI(X)
196800090216     C                   END
196900090216     C     TABCOD        READE     EDTAB01L                               30
197000090216     C                   END
197100090209      *
197200090209      * Caricamento Tabella Partner esteri
197300090209     C                   Z-ADD     0             X
197400090209     C                   MOVEL     'PT'          TABCOD
197500090209     C     TABCOD        CHAIN     EDTAB01L                           30
197600090209     C     *IN30         DOWEQ     '0'
197700090209      *
197800090209     C                   SELECT
197900090209     C     TABFLG        WHENNE    *BLANKS
198000090209      *
198100090209     C                   OTHER
198200090209     C                   ADD       1             X
198300090209     C                   MOVEL     TABKEY        CPT(X)
198400090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
198500090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
198600090209     C                   MOVEL     TABUNI        DPT(X)
198700090209     C                   ENDSL
198800090209      *
198900090209     C     TABCOD        READE     EDTAB01L                               30
199000090209     C                   END
199100121105      *
199200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
199300121105     c                   exsr      ChecK_PT
199400121105      *
199500090209      * salva quanti Codici UNB ha caricato
199600090209     c                   z-add     x             sav_CPTx
199700090209      *
199800090209     C                   MOVEL     'PU'          TABCOD
199900090209     C     TABCOD        CHAIN     EDTAB01L                           30
200000090209     C     *IN30         DOWEQ     '0'
200100090209      *
200200090209     C                   SELECT
200300090209     C     TABFLG        WHENNE    *BLANKS
200400090209      *
200500090209     C                   OTHER
200600090209     C                   MOVEL     TABKEY        CODPU            35
200700090209     C                   Z-ADD     1             X
200800090209     C     CODPU         LOOKUP    CPT(X)                                 32
200900090209     C   32              MOVEL     TABUNI        DPU(X)
201000090209     C                   ENDSL
201100090209      *
201200090209     C     TABCOD        READE     EDTAB01L                               30
201300090209     C                   END
201400090209      *
201500090209      * Prosegue tab.PT e PU
201600090209     C                   MOVEL     'PV'          TABCOD
201700090209     C     TABCOD        CHAIN     EDTAB01L                           30
201800090209     C     *IN30         DOWEQ     '0'
201900090209      *
202000090209     C                   SELECT
202100090209     C     TABFLG        WHENNE    *BLANKS
202200090209      *
202300090209     C                   OTHER
202400090209     C                   MOVEL     TABKEY        CODPV            35
202500090209     C                   Z-ADD     1             X
202600090209     C     CODPV         LOOKUP    CPT(X)                                 32
202700090209     C   32              MOVEL     TABUNI        DPV(X)
202800090209     C                   ENDSL
202900090209      *
203000090209     C     TABCOD        READE     EDTAB01L                               30
203100090209     C                   END
203200090209      *
203300090209     C* Caricamento Tabella codici clienti - tariffe
203400090209     C                   Z-ADD     0             X
203500090209     C                   MOVEL     'CL'          TABCOD
203600090209     C     TABCOD        CHAIN     EDTAB01L                           30
203700090209     C     *IN30         DOWEQ     '0'
203800090209     C     TABFLG        IFEQ      *BLANKS
203900090209     C                   ADD       1             X
204000090209     C                   MOVEL     TABKEY        CCL(X)
204100090209     C                   MOVEL     TABUNI        DCL(X)
204200090209     C                   END
204300090209     C     TABCOD        READE     EDTAB01L                               30
204400090209     C                   END
204500090209      *
204600090209     C* Caricamento Serivizi speciali
204700090209     C                   Z-ADD     0             X
204800090209     C                   MOVEL     'SS'          TABCOD
204900090209     C     TABCOD        CHAIN     EDTAB01L                           30
205000090209     C     *IN30         DOWEQ     '0'
205100090209     C     TABFLG        IFEQ      *BLANKS
205200090209     C                   ADD       1             X
205300090209     C                   MOVEL     TABKEY        SS(X)
205400090209     C                   MOVEL     TABKEY        SS35(X)
205500090209     C                   MOVEL     TABUNI        DSS(X)
205600090209     C                   END
205700090209     C     TABCOD        READE     EDTAB01L                               30
205800090209     C                   END
205900090209      *
206000090209     C                   ENDSR
206100090209      * ?------------------------------------------------------------------ */
206200090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
206300090209      * ?------------------------------------------------------------------ */
206400090209     C     MSG_Errato    BEGSR
206500090209      *
206600090205      * ?  Scrive su tutti i records il tipo di errore
206700090205     c     *start        setll     tivin00r
206800090205     c                   read      tivin00r
206900090205     c                   dow       not %eof(tivin00r)
207000090211     c                   if        vinMSG  = *blank
207100090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
207200090210     C                               >> Controllare i DATI su UPLOAD !!'
207300090211     c                   end
207400090205     C                   eval      vinFLG = '2'
207500090213     c                   eval      Almeno_Un_Due ='S'
207600090205     c                   eval      esito  = '2'
207700090209     c                   eval      se_errore ='S'
207800090213     c                   eval      err_bloccante ='S'
207900090205     c                   update    tivin000
208000090205     c                   read      tivin00r
208100090205     c                   endDO
208200090205      *
208300090205     C                   ENDSR
208400090213      * ?------------------------------------------------------------------ */
208500090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
208600090213      * ?------------------------------------------------------------------ */
208700090213     C     DETta_Errato  BEGSR
208800090213      *
208900090213      *  rilascia il record prima di rieseguire il ciclo
209000090213     c                   update    tivin000
209100090213      *
209200090213      * ?  Scrive su tutti i records il tipo di errore
209300090213     c     SavNUM_RECord setll     tivin00r
209400090213     c                   read      tivin00r
209500090213     c                   dow       not %eof(tivin00r)
209600090213      *
209700090213      *
209800090213     c                   if        vinMSG  = *blank
209900090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
210000090213     C                             >> Controllare i DATI di ogni Segmento <<'
210100090213     c                   end
210200090213     c                   exsr      Error_DET
210300090213      *
210400090213     c                   if        VNREC = NUM_RECord
210500090213     c                   leave
210600090213     c                   end
210700090213      *
210800090213     c                   update    tivin000
210900090213     c                   read      tivin00r
211000090213      *
211100090213     c                   endDO
211200090603      *
211300090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
211400090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
211500090609     C                             Bolle DELTA+ non totalmente tradotto. -
211600100716     C                             Controllare UPLOAD del cliente 0055110.'
211700090603     c                   exsr      invio_mail_AB
211800090603      *
211900090213      *
212000090213     C                   ENDSR
212100090205      * ?------------------------------------------------------------------ */
212200090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
212300090205      * ?------------------------------------------------------------------ */
212400090205     C     *pssr         BEGSR
212500090205     C
212600060710     C                   eval      esito = '2'
212700090609     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import DELTA+'
212800090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
212900090603     C                             Bolle import in filiale - messaggio -
213000090603     C                             EDI ricevuto errato su UPLOAD.'
213100090603     c                   exsr      invio_mail_AB
213200060621     C
213300090213     C                   ENDSR
213400090213      * ?------------------------------------------------------------------ */
213500090213      *?      Segnala Errore su TIVIN
213600090213      * ?------------------------------------------------------------------ */
213700090213     C     Error_DET     BEGSR
213800090213     C
213900090213     C                   eval      vinFLG = '2'
214000090213     c                   eval      Almeno_Un_Due ='S'
214100090213     c                   eval      esito  = '1'
214200090213     c                   eval      se_errore ='S'
214300090213     C                   eval      Err_in_detta = 'S'
214400090213     C
214500090213     C                   ENDSR
214600090209      * ?------------------------------------------------------------------ */
214700090209      *? Controlla la trasmissione   se completa
214800090209      * ?------------------------------------------------------------------ */
214900090209     c     Check_Trasm   Begsr
215000090209
215100090209     C                   clear                   se_errore
215200090209     C                   clear                   Riga_iNIZIo       1
215300090209     C                   clear                   Riga_fINe         1
215400090209      ** primo  record
215500090209     c     *start        setll     tivin00r
215600090209      *
215700090209     c     rileggi       tag
215800090209     c                   read      tivin00r
215900090209      *
216000090209     c                   if        not %eof(tivin00r)
216100090209      *
216200090209     c                   movel(p)  VINDTA        dati
216300090209     c                   if        dati = *blank
216400090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
216500090209      * le leggo e le escludo.
216600090209     C                   eval      vinFLG = '1'
216700090209     c                   update    Tivin000
216800090209     c                   goto      rileggi
216900090209     c                   end
217000090209      * ?              /*---------------------- */
217100090210     c                   exsr      Decod_Segmento
217200090209      * ?              /*---------------------- */
217300090209     c                   endif
217400090209      **
217500090209      ** ultimo record
217600090209     c     *hival        setll     tivin00r
217700090209     c     leggiAncora   tag
217800090209     c                   readp     tivin00r
217900090209     c                   if        not %eof(tivin00r)
218000090209      *
218100090209     c                   movel(p)  VINDTA        dati
218200090209     c                   if        dati = *blank
218300090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
218400090209      * le leggo e le escludo.
218500090209     C                   eval      vinFLG = '1'
218600090209     c                   update    Tivin000
218700090209     c                   goto      leggiAncora
218800090209     c                   end
218900090209      * ?              /*---------------------- */
219000090210     c                   exsr      Decod_Segmento
219100090209      * ?              /*---------------------- */
219200090209     c                   endif
219300090209
219400090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
219500090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
219600090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
219700090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
219800090209      * ?-----> Errore
219900090209     C                   eval      se_errore = 'S'
220000090209     c                   end
220100090209
220200090209     c                   endSR
220300090209      * ?------------------------------------------------------------------ */
220400090209      *?    Decodifica il segmento ed importa i campi in formato DS
220500090209      * ?------------------------------------------------------------------ */
220600090209     c     Decod_SegmentoBegsr
220700090209      **
220800090209     c                   eval      tipo_seg = %subst(vinDTA:1:3)
220900090209     c                   eval      segmento = vinDTA
221000090210      **--------
221100100716     C                   eval       keyUNBCLI    = *blank
221200100716     C                   eval       keyTIPOMSG   = 'DISPOR'
221300100716     C                   eval       keyVERSION   = '2'
221400100716     C                   eval       keyRELEASE   = '3'
221500100716     C                   eval       keyAGENCY    = 'GT'
221600100716     C                   eval       keyASSOCIA   = 'GTF'
221700100716      **--------
221800090211     c                   select
221900090609      * --------
222000090609     c                   when      tipo_seg = 'BGM'
222100090609      *
222200090609     c                   call      'TRTCT05BGM'
222300090609     c                   parm                    tipo_seg
222400090609     c                   parm                    segmento
222500100716     C                   parm                    keyUNBCLI
222600100716     C                   parm                    keyTIPOMSG
222700100716     C                   parm                    keyVERSION
222800100716     C                   parm                    keyRELEASE
222900100716     C                   parm                    keyAGENCY
223000100716     C                   parm                    keyASSOCIA
223100090609     c                   parm                    Errore
223200090609     c                   parm                    EDPORBGM
223300090609     c                   parm                    Valori_inErr
223400090609     c                   parm                    Descr_Errore
223500090609      *
223600090609     C                   EXSR      DATI_BGM
223700090609      **--------
223800090609     c                   when      tipo_seg = 'CAG'
223900090609      *
224000090609     c                   call      'TRTCT05CAG'
224100090609     c                   parm                    tipo_seg
224200090609     c                   parm                    segmento
224300100716     C                   parm                    keyUNBCLI
224400100716     C                   parm                    keyTIPOMSG
224500100716     C                   parm                    keyVERSION
224600100716     C                   parm                    keyRELEASE
224700100716     C                   parm                    keyAGENCY
224800100716     C                   parm                    keyASSOCIA
224900090609     c                   parm                    Errore
225000090609     c                   parm                    EDPORCAG
225100090609     c                   parm                    Valori_inErr
225200090609     c                   parm                    Descr_Errore
225300090609      *
225400090609     C                   EXSR      DATI_CAG
225500090609      **--------
225600090609     c                   when      tipo_seg = 'CTA'
225700090609      *
225800090609     c                   call      'TRTCT05CTA'
225900090609     c                   parm                    tipo_seg
226000090609     c                   parm                    segmento
226100100716     C                   parm                    keyUNBCLI
226200100716     C                   parm                    keyTIPOMSG
226300100716     C                   parm                    keyVERSION
226400100716     C                   parm                    keyRELEASE
226500100716     C                   parm                    keyAGENCY
226600100716     C                   parm                    keyASSOCIA
226700090609     c                   parm                    Errore
226800090609     c                   parm                    EDPORCTA
226900090609     c                   parm                    Valori_inErr
227000090609     c                   parm                    Descr_Errore
227100090609      *
227200090609     C                   EXSR      DATI_CTA
227300091104      **--------
227400091104     c                   when      tipo_seg = 'COM'
227500091104      *
227600091104     c                   call      'TRTCT05COM'
227700091104     c                   parm                    tipo_seg
227800091104     c                   parm                    segmento
227900100716     C                   parm                    keyUNBCLI
228000100716     C                   parm                    keyTIPOMSG
228100100716     C                   parm                    keyVERSION
228200100716     C                   parm                    keyRELEASE
228300100716     C                   parm                    keyAGENCY
228400100716     C                   parm                    keyASSOCIA
228500091104     c                   parm                    Errore
228600091104     c                   parm                    EDPORCOM
228700091104     c                   parm                    Valori_inErr
228800091104     c                   parm                    Descr_Errore
228900091104      *
229000091104     C                   EXSR      DATI_COM
229100090609      **--------
229200090609     c                   when      tipo_seg = 'DGS'
229300090609      *
229400090609     c                   call      'TRTCT05DGS'
229500090609     c                   parm                    tipo_seg
229600090609     c                   parm                    segmento
229700100716     C                   parm                    keyUNBCLI
229800100716     C                   parm                    keyTIPOMSG
229900100716     C                   parm                    keyVERSION
230000100716     C                   parm                    keyRELEASE
230100100716     C                   parm                    keyAGENCY
230200100716     C                   parm                    keyASSOCIA
230300090609     c                   parm                    Errore
230400090609     c                   parm                    EDPORDGS
230500090609     c                   parm                    Valori_inErr
230600090609     c                   parm                    Descr_Errore
230700090609      *
230800090609     C                   EXSR      DATI_DGS
230900090609      * --------
231000090609     c                   when      tipo_seg = 'DOC'
231100090609      *
231200090609     c                   call      'TRTCT05DOC'
231300090609     c                   parm                    tipo_seg
231400090609     c                   parm                    segmento
231500100716     C                   parm                    keyUNBCLI
231600100716     C                   parm                    keyTIPOMSG
231700100716     C                   parm                    keyVERSION
231800100716     C                   parm                    keyRELEASE
231900100716     C                   parm                    keyAGENCY
232000100716     C                   parm                    keyASSOCIA
232100090609     c                   parm                    Errore
232200090609     c                   parm                    EDPORDOC
232300090609     c                   parm                    Valori_inErr
232400090609     c                   parm                    Descr_Errore
232500090609      *
232600090609     C                   EXSR      DATI_DOC
232700090609      **--------
232800090609     c                   when      tipo_seg = 'DTM'
232900090609      *
233000090609     c                   call      'TRTCT05DTM'
233100090609     c                   parm                    tipo_seg
233200090609     c                   parm                    segmento
233300100716     C                   parm                    keyUNBCLI
233400100716     C                   parm                    keyTIPOMSG
233500100716     C                   parm                    keyVERSION
233600100716     C                   parm                    keyRELEASE
233700100716     C                   parm                    keyAGENCY
233800100716     C                   parm                    keyASSOCIA
233900090609     c                   parm                    Errore
234000090609     c                   parm                    EDPORDTM
234100090609     c                   parm                    Valori_inErr
234200090609     c                   parm                    Descr_Errore
234300090609      *
234400090609     C                   EXSR      DATI_DTM
234500090609      **--------
234600090609     c                   when      tipo_seg = 'GDS'
234700090609      *
234800090609     c                   call      'TRTCT05GDS'
234900090609     c                   parm                    tipo_seg
235000090609     c                   parm                    segmento
235100100716     C                   parm                    keyUNBCLI
235200100716     C                   parm                    keyTIPOMSG
235300100716     C                   parm                    keyVERSION
235400100716     C                   parm                    keyRELEASE
235500100716     C                   parm                    keyAGENCY
235600100716     C                   parm                    keyASSOCIA
235700090609     c                   parm                    Errore
235800090609     c                   parm                    EDPORGDS
235900090609     c                   parm                    Valori_inErr
236000090609     c                   parm                    Descr_Errore
236100090609      *
236200090609     C                   EXSR      DATI_GDS
236300090609      **--------
236400090609     c                   when      tipo_seg = 'GID'
236500090609      *
236600090609     c                   call      'TRTCT05GID'
236700090609     c                   parm                    tipo_seg
236800090609     c                   parm                    segmento
236900100716     C                   parm                    keyUNBCLI
237000100716     C                   parm                    keyTIPOMSG
237100100716     C                   parm                    keyVERSION
237200100716     C                   parm                    keyRELEASE
237300100716     C                   parm                    keyAGENCY
237400100716     C                   parm                    keyASSOCIA
237500090609     c                   parm                    Errore
237600090609     c                   parm                    EDPORGID
237700090609     c                   parm                    Valori_inErr
237800090609     c                   parm                    Descr_Errore
237900090609      *
238000090609     C                   EXSR      DATI_GID
238100090609      **--------
238200090609     c                   when      tipo_seg = 'MSE'
238300090609      *
238400090609     c                   call      'TRTCT05MSE'
238500090609     c                   parm                    tipo_seg
238600090609     c                   parm                    segmento
238700100716     C                   parm                    keyUNBCLI
238800100716     C                   parm                    keyTIPOMSG
238900100716     C                   parm                    keyVERSION
239000100716     C                   parm                    keyRELEASE
239100100716     C                   parm                    keyAGENCY
239200100716     C                   parm                    keyASSOCIA
239300090609     c                   parm                    Errore
239400090609     c                   parm                    EDPORMSE
239500090609     c                   parm                    Valori_inErr
239600090609     c                   parm                    Descr_Errore
239700090609      *
239800090609     C                   EXSR      DATI_MSE
239900090609      **--------
240000090609     c                   when      tipo_seg = 'NAD'
240100090609      *
240200090609     c                   call      'TRTCT05NAD'
240300090609     c                   parm                    tipo_seg
240400090609     c                   parm                    segmento
240500100716     C                   parm                    keyUNBCLI
240600100716     C                   parm                    keyTIPOMSG
240700100716     C                   parm                    keyVERSION
240800100716     C                   parm                    keyRELEASE
240900100716     C                   parm                    keyAGENCY
241000100716     C                   parm                    keyASSOCIA
241100090609     c                   parm                    Errore
241200090609     c                   parm                    EDPORNAD
241300090609     c                   parm                    Valori_inErr
241400090609     c                   parm                    Descr_Errore
241500090609      *
241600090609     C                   EXSR      DATI_NAD
241700090609      **--------
241800090609     c                   when      tipo_seg = 'PCI'
241900090609      *
242000090609     c                   call      'TRTCT05PCI'
242100090609     c                   parm                    tipo_seg
242200090609     c                   parm                    segmento
242300100716     C                   parm                    keyUNBCLI
242400100716     C                   parm                    keyTIPOMSG
242500100716     C                   parm                    keyVERSION
242600100716     C                   parm                    keyRELEASE
242700100716     C                   parm                    keyAGENCY
242800100716     C                   parm                    keyASSOCIA
242900090609     c                   parm                    Errore
243000090609     c                   parm                    EDPORPCI
243100090609     c                   parm                    Valori_inErr
243200090609     c                   parm                    Descr_Errore
243300090609      *
243400090609     C                   EXSR      DATI_PCI
243500090609      **--------
243600090609     c                   when      tipo_seg = 'RFF'
243700090609      *
243800090609     c                   call      'TRTCT05RFF'
243900090609     c                   parm                    tipo_seg
244000090609     c                   parm                    segmento
244100100716     C                   parm                    keyUNBCLI
244200100716     C                   parm                    keyTIPOMSG
244300100716     C                   parm                    keyVERSION
244400100716     C                   parm                    keyRELEASE
244500100716     C                   parm                    keyAGENCY
244600100716     C                   parm                    keyASSOCIA
244700090609     c                   parm                    Errore
244800090609     c                   parm                    EDPORRFF
244900090609     c                   parm                    Valori_inErr
245000090609     c                   parm                    Descr_Errore
245100090609      *
245200090609     C                   EXSR      DATI_RFF
245300090609      **--------
245400090609     c                   when      tipo_seg = 'TDT'
245500090609      *
245600090609     c                   call      'TRTCT05TDT'
245700090609     c                   parm                    tipo_seg
245800090609     c                   parm                    segmento
245900100716     C                   parm                    keyUNBCLI
246000100716     C                   parm                    keyTIPOMSG
246100100716     C                   parm                    keyVERSION
246200100716     C                   parm                    keyRELEASE
246300100716     C                   parm                    keyAGENCY
246400100716     C                   parm                    keyASSOCIA
246500090609     c                   parm                    Errore
246600090609     c                   parm                    EDPORTDT
246700090609     c                   parm                    Valori_inErr
246800090609     c                   parm                    Descr_Errore
246900090609      *
247000090609     C                   EXSR      DATI_TDT
247100090609      * --------
247200090609     c                   when      tipo_seg = 'TSR'
247300090609      *
247400090609     c                   call      'TRTCT05TSR'
247500090609     c                   parm                    tipo_seg
247600090609     c                   parm                    segmento
247700100716     C                   parm                    keyUNBCLI
247800100716     C                   parm                    keyTIPOMSG
247900100716     C                   parm                    keyVERSION
248000100716     C                   parm                    keyRELEASE
248100100716     C                   parm                    keyAGENCY
248200100716     C                   parm                    keyASSOCIA
248300090609     c                   parm                    Errore
248400090609     c                   parm                    EDPORTSR
248500090609     c                   parm                    Valori_inErr
248600090609     c                   parm                    Descr_Errore
248700090609      *
248800090609     C                   EXSR      DATI_TSR
248900090609      **--------
249000090609     c                   when      tipo_seg = 'TXT'
249100090609      *
249200090609     c                   call      'TRTCT05TXT'
249300090609     c                   parm                    tipo_seg
249400090609     c                   parm                    segmento
249500100716     C                   parm                    keyUNBCLI
249600100716     C                   parm                    keyTIPOMSG
249700100716     C                   parm                    keyVERSION
249800100716     C                   parm                    keyRELEASE
249900100716     C                   parm                    keyAGENCY
250000100716     C                   parm                    keyASSOCIA
250100090609     c                   parm                    Errore
250200090609     c                   parm                    EDPORTXT
250300090609     c                   parm                    Valori_inErr
250400090609     c                   parm                    Descr_Errore
250500090609      *
250600090609     C                   EXSR      DATI_TXT
250700090609      * --------
250800090209     c                   when      tipo_seg = 'UNB'
250900090209      *
251000090609     c                   call      'TRTCT05UNB'
251100090209     c                   parm                    tipo_seg
251200090209     c                   parm                    segmento
251300100716     C                   parm                    keyUNBCLI
251400100716     C                   parm                    keyTIPOMSG
251500100716     C                   parm                    keyVERSION
251600100716     C                   parm                    keyRELEASE
251700100716     C                   parm                    keyAGENCY
251800100716     C                   parm                    keyASSOCIA
251900090209     c                   parm                    Errore
252000090609     c                   parm                    EDPORUNB
252100090209     c                   parm                    Valori_inErr
252200090209     c                   parm                    Descr_Errore
252300090210      *
252400090210     C                   EXSR      DATI_UNB
252500090210      * --------
252600090209     c                   when      tipo_seg = 'UNH'
252700090209      *
252800090609     c                   call      'TRTCT05UNH'
252900090209     c                   parm                    tipo_seg
253000090209     c                   parm                    segmento
253100100716     C                   parm                    keyUNBCLI
253200100716     C                   parm                    keyTIPOMSG
253300100716     C                   parm                    keyVERSION
253400100716     C                   parm                    keyRELEASE
253500100716     C                   parm                    keyAGENCY
253600100716     C                   parm                    keyASSOCIA
253700090209     c                   parm                    Errore
253800090609     c                   parm                    EDPORUNH
253900090209     c                   parm                    Valori_inErr
254000090209     c                   parm                    Descr_Errore
254100090210      *
254200090210     C                   EXSR      DATI_UNH
254300090609      * --------
254400090609     c                   when      tipo_seg = 'UNS'
254500090609      *
254600090609     c                   call      'TRTCT05UNS'
254700090609     c                   parm                    tipo_seg
254800090609     c                   parm                    segmento
254900100716     C                   parm                    keyUNBCLI
255000100716     C                   parm                    keyTIPOMSG
255100100716     C                   parm                    keyVERSION
255200100716     C                   parm                    keyRELEASE
255300100716     C                   parm                    keyAGENCY
255400100716     C                   parm                    keyASSOCIA
255500090609     c                   parm                    Errore
255600090609     c                   parm                    EDPORUNS
255700090609     c                   parm                    Valori_inErr
255800090609     c                   parm                    Descr_Errore
255900090609      *
256000090609     C                   EXSR      DATI_UNS
256100090609      **--------
256200090609     c                   when      tipo_seg = 'UNT'
256300090609      *
256400090616     c                   call      'TRTCT05UNT'
256500090609     c                   parm                    tipo_seg
256600090609     c                   parm                    segmento
256700100716     C                   parm                    keyUNBCLI
256800100716     C                   parm                    keyTIPOMSG
256900100716     C                   parm                    keyVERSION
257000100716     C                   parm                    keyRELEASE
257100100716     C                   parm                    keyAGENCY
257200100716     C                   parm                    keyASSOCIA
257300090609     c                   parm                    Errore
257400090609     c                   parm                    EDPORUNT
257500090609     c                   parm                    Valori_inErr
257600090609     c                   parm                    Descr_Errore
257700090609      *
257800090609     C                   EXSR      DATI_UNT
257900090609      **--------
258000090609     c                   when      tipo_seg = 'UNZ'
258100090609      *
258200090616     c                   call      'TRTCT05UNZ'
258300090609     c                   parm                    tipo_seg
258400090609     c                   parm                    segmento
258500100716     C                   parm                    keyUNBCLI
258600100716     C                   parm                    keyTIPOMSG
258700100716     C                   parm                    keyVERSION
258800100716     C                   parm                    keyRELEASE
258900100716     C                   parm                    keyAGENCY
259000100716     C                   parm                    keyASSOCIA
259100090609     c                   parm                    Errore
259200090609     c                   parm                    EDPORUNZ
259300090609     c                   parm                    Valori_inErr
259400090609     c                   parm                    Descr_Errore
259500090609      *
259600090609     C                   EXSR      DATI_UNZ
259700090210      **--------
259800090609     c                   when      tipo_seg = 'VAL'
259900090213      *
260000090616     c                   call      'TRTCT05VAL'
260100090213     c                   parm                    tipo_seg
260200090213     c                   parm                    segmento
260300100716     C                   parm                    keyUNBCLI
260400100716     C                   parm                    keyTIPOMSG
260500100716     C                   parm                    keyVERSION
260600100716     C                   parm                    keyRELEASE
260700100716     C                   parm                    keyAGENCY
260800100716     C                   parm                    keyASSOCIA
260900090213     c                   parm                    Errore
261000090609     c                   parm                    EDPORVAL
261100090213     c                   parm                    Valori_inErr
261200090213     c                   parm                    Descr_Errore
261300090213      *
261400090609     C                   EXSR      DATI_VAL
261500090210      **--------
261600090209     c                   endSL
261700090209      **
261800090209     c                   endSR
261900090209     c*==================================================================*
262000090209     C*? CNVDAT - DECODIFICA LA DATA
262100090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
262200090209     C*         - WDATA  (DATA ALFANUMERICA)
262300090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
262400090209     C*         - WHMS   (ORA NUMERICA)
262500090209     C*----------------------------------------------------------------
262600090209     C     CNVDAT        BEGSR
262700090209     C*
262800090209     C                   MOVEL     ' '           WERRO             1
262900090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
263000090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
263100090209     C                   Z-ADD     *ZEROS        WHMS              6 0
263200090209     C*
263300090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
263400090209     C                   MOVEL     WDATA         WCOMO8            8
263500090209     C                   TESTN                   WCOMO8               99
263600090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
263700090209     C  N99              MOVEL     'S'           WERRO             1
263800090209     C                   END
263900090209     C*
264000090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
264100090209     C                   MOVEL     WDATA         WCOMO14          14
264200090209     C                   TESTN                   WCOMO14              99
264300090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
264400090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
264500090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
264600090209     C  N99              MOVEL     'S'           WERRO             1
264700090209     C                   END
264800090209     C*
264900090209     C                   ENDSR
265000090210     C*----------------------------------------------------------------
265100090210     C*? TSTNUM - CONTROLLO NUMERICO
265200090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
265300090210     C*         - WLUNGH (Lunghezza campo output)
265400090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
265500090210     C*----------------------------------------------------------------
265600090210     C     TSTNUM        BEGSR
265700090210     C*
265800090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
265900090210     C                   MOVEL     'N'           WOKNUM            1
266000090210     C                   MOVEL     *BLANKS       WNUMOK           15
266100090210     C                   Z-ADD     15            IN                3 0
266200090210     C                   Z-ADD     WLUNGH        IX                3 0
266300090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
266400090210     C                   MOVEA     WNUM          NDA
266500090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
266600090210     C                   MOVEA     *ALL'0'       NDN
266700090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
266800090210     C                   Z-ADD     WLUNGH        I                 3 0
266900090210     C     I             DOWGT     0                                            --- 1 -->
267000090210     C* ESTRAE IL CARATTERE DA ESAMINARE
267100090210     C     1             SUBST     WNUM:I        WTEM01            1
267200090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
267300090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
267400090210     C     WTEM01        SCAN      WCA                                    99
267500090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
267600090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
267700090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
267800090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
267900090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
268000090210     C* DA DESTRA VERSO SINISTRA
268100090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
268200090210     C                   MOVEL     WTEMP1        NDN(IN)
268300090210     C                   SUB       1             IN
268400090210     C                   END                                                    <-- 3 ---
268500090210     C                   END                                                    <-- 2 ---
268600090210     C                   SUB       1             I
268700090210     C                   END                                                    <-- 1 ---
268800090210     C*
268900090210     C                   MOVEA     NDN           WNDN             15
269000090210     C     WNDN          IFNE      *ZEROS
269100090210     C                   MOVEA     NDN           WNUMOK           15
269200090210     C                   MOVEL     'S'           WOKNUM            1
269300090210     C                   END
269400090210     C*
269500090210     C                   ENDSR
269600090210     C*----------------------------------------------------------------
269700090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
269800090210     C*----------------------------------------------------------------
269900090210     C     CNVCAP        BEGSR
270000090210     C*
270100090210     C                   MOVEL     *BLANKS       WCAP              9
270200090609     C     cap09a        IFNE      '0'
270300090609     C     '  '          SCAN      cap09a        LENGHT            2 0
270400090210     C                   SUB       1             LENGHT
270500090210     C     LENGHT        IFLE      5
270600090210     C     LENGHT        ANDGT     0
270700090210     C                   Z-ADD     LENGHT        T                 2 0
270800090210     C                   Z-ADD     5             S                 2 0
270900090609     C                   MOVEA     cap09a        CAP9
271000090210     C                   MOVEL     *ZEROS        CAP5
271100090210     C                   DO        LENGHT
271200090210     C                   MOVE      CAP9(T)       CAP5(S)
271300090210     C                   SUB       1             S
271400090210     C                   SUB       1             T
271500090210     C                   END
271600090210     C                   MOVEA     CAP5          WCAP5             5
271700090210     C     WCAP5         IFEQ      '0'
271800090210     C                   MOVEL     *BLANKS       WCAP
271900090210     C                   ELSE
272000090210     C                   MOVEA     CAP5          WCAP
272100090210     C                   END
272200090210     C*
272300090210     C                   ELSE
272400090609     C                   MOVEL     cap09a        WCAP
272500090210     C                   END
272600090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
272700090210     C     WCAP          IFNE      *BLANKS
272800090210     C     WFLCPF        ANDNE     0
272900090210     C                   MOVEL     WFLCPF        WELE              1 0
273000090210     C                   MOVE      WFLCPF        WPOS              1 0
273100090210     C                   MOVEL     *BLANK        WCPF
273200090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
273300090210     C                   ELSE
273400090210     C                   MOVEL     *BLANKS       WCPF
273500090210     C                   END
273600090210     C                   END
273700090210     C*
273800090210     C                   ENDSR
273900090211      * ?================================================================== */
274000090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
274100090211      * ?================================================================== */
274200090211     C     Scrive_VAB    BEGSR
274300090211      *
274400090211     c                   clear                   err_bloccante     1
274500090211     c                   move      'S'           Almeno_Uno
274600090616     c                   eval       se_trovato_NAD = *blank
274700090211      *
274800090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
274900090211     c                   exsr      UPDVAB
275000090211      *
275100090211     C* >>>>>>>>>>>>
275200090211     C*  Richiamo pgm per scrittura immediata VAB
275300090211     C     §PUWRK        IFEQ      ' '
275400090211      *
275500090211     C*  Richiamo pgm per esecuzione stampa
275600090213     C                   MOVEL     WNRDOC        d86CMR
275700090213     C                   MOVEL     ' '           d86RIE
275800090213     C                   MOVEL     §PUDTA        d86DTA
275900090213     C                   Z-ADD     1             d86CNT
276000090213     C                   Z-ADD     §PTKSC        d86CCM
276100090213     C                   MOVEL     *BLANKS       d86STA
276200090213     C                   MOVEL     'E'           d86MOD
276300090611      *
276400090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
276500090211      * dove poi imposta la DS dei parametri passati
276600090213     C                   MOVEL     'LR'          d86CHI
276700090213     C                   MOVEL     'N'           d86CTL
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
277800090211      *
277900090211     C*  Controllo pgm per scrittura immediata VAB
278000090211     C     WW            IFNE      0
278100090211     C                   DO        WW            WX                3 0
278200090213     C                   Z-ADD     KCL(WX)       d86CCM
278300090211      *
278400090211     c                   move      kpjbu         SvKpjbu
278500090211     c                   clear                   kpjbu
278600090216     C                   movel     *on           Commit_on         1
278700090213     C                   MOVEL     TRTC86ds      KPJBU
278800090213      * ?- Chiamata la TRTC86R2 -*
278900090213     C                   CALL      'TRTC86R2'
279000090211     C                   PARM                    KPJBA
279100090216     C                   PARM                    Commit_on
279200090211     c                   move      Svkpjbu       Kpjbu
279300090211     C                   END
279400090211     C                   END
279500090211      *
279600090211     C                   END
279700090211      * >>>>>>>>>>>>
279800090211     C                   if        se_errore  = 'S'
279900090211     c                   move      'S'           err_bloccante
280000090211     c                   goto      Error_VAB
280100090211     c                   end
280200090211     C*
280300090211     C     Error_VAB     Endsr
280400090211     C*----------------------------------------------------------------
280500090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
280600090211     C*----------------------------------------------------------------
280700090211     C     UPDVAB        BEGSR
280800090211      *
280900090211     C*  Se non è stato impostato il codice bolla lo imposto io
281000090211     C     VABCBO        IFEQ      *BLANKS
281100090630     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
281200090211     C                   END
281300090630      *
281400090630      *  Se c'è il contrassegno
281500090630     C     VABCAS        IFGT      0
281600090630     C     VABCBO        ifeq      '1'
281700090630     C                   MOVEL     '4'           VABCBO                         + C/Ass
281800090630     C                   END
281900090630     C     VABCBO        ifeq      '2'
282000090630     C                   MOVEL     '6'           VABCBO                         + C/Ass
282100090630     C                   END
282200090630     C                   END
282300110922      *
282400110922      * Imposta le NOte
282500110922     C     VABNOT        IFEQ      *BLANKS
282600110922     C                   MOVEL     wnot1         VABNOT
282700110922     C                   MOVEL     wnot2         VABNT2
282800110922     C                   ELSE
282900110922     C                   MOVEL     wnot1         VABNT2
283000110922     C                   END
283100090211      *
283200090211     C*  Attribuisco anno spedizione
283300090611     C     Tes_VABdcm    IFGT      0
283400090611     C                   MOVEL     Tes_VABdcm    VABAAS                         anno sped.
283500090211     C                   END
283600090211      *
283700090211      *  Controllo dettaglio segnacolli
283800090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
283900090211     C                   EXSR      CTRSGE
284000090211     C                   ELSE
284100090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
284200090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
284300090211     C                   EXSR      CTRSGN
284400090211     C                   END
284500090211     C                   END
284600090211      *
284700090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
284800090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
284900090211     c                   clear                   mantiene_nsp      1            *blk/'S'
285000090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
285100090211      *
285200090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
285300090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
285400090211      *
285500090211      *  Imposto codice cliente
285600090211     c                   Select
285700090211      *
285800090611      *  Specificato codice da qualificatore FF
285900090211     C                   When        WCLKSC <> *zeros
286000090211     C                   Z-ADD     WCLKSC        VABCCM
286100090211     C                   MOVEL     WCLTAR        VABCTR
286200090211      * forza LNP/CTM/NRS
286300090211     c                   if        WclLNP <> 0
286400090211     C                   eval      VABLNP = WclLNP
286500090211     c                   end
286600090211     c                   if        WclNRS <> 0
286700090211     C                   eval      VABNRS = WclNRS
286800090211     c                   end
286900090211     c                   if        WclCTM <> *blank
287000090211     C                   eval      VABCTM = WclCTM
287100090211     c                   end
287200090211      *
287300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
287400090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
287500090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
287600090211     c                   end
287700090211      *
287800090611      *  Specificato codice da qualificatore FF
287900090211     C                   When        $CLKSC <> *zeros
288000090211     C                   Z-ADD     $CLKSC        VABCCM
288100090211     C                   MOVEL     $CLTAR        VABCTR
288200090211      * forza LNP/CTM/NRS
288300090211     c                   if        $clLNP <> 0
288400090211     C                   eval      VABLNP = $clLNP
288500090211     c                   end
288600090211     c                   if        $clNRS <> 0
288700090211     C                   eval      VABNRS = $clNRS
288800090211     c                   end
288900090211     c                   if        $clCTM <> *blank
289000090211     C                   eval      VABCTM = $clCTM
289100090211     c                   end
289200090211      *
289300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
289400090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
289500090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
289600090211     c                   end
289700090211      *
289800090211      *  Imposto dati da tabella - PT-
289900090211     C                   Other
290000090211      *
290100090211     C                   MOVEL     §PTKSC        VABCCM
290200090211     C                   MOVEL     §PTCTR        VABCTR
290300090211      *
290400090211     C                   Endsl
290500090211      *
290600090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
290700090211      *    o dalla relativa CL
290800090211     C                   MOVEL     VABlnp        VABfgs
290900090211      *
291000090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
291100090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
291200090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
291300090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
291400090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
291500090211      *
291600100511      * Avendo attivato il VAX si deve recepire lo stesso numero passato nel rif.Alfabetico
291700100511      *
291800100511     c                   if        mantiene_nsp = 'S' and VABRMA <> *blank      *blk/'S' - 1234567
291900100511     c                   movel(p)  vabrma        campo9a           9
292000100511     c                   move      campo9a       VABNSP                         *NUM.SPED.da Cliente
292100100511     c                   else
292200090211      *
292300090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
292400090211     C*  lo attribuisco io
292500090211      **              **------------------**
292600090211     c                   exsr      repnum
292700090211      **              **------------------**
292800090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
292900090211      **              **------------------**
293000090211     c                   end
293100090211      *
293200090211     C*  Altrimenti attribuisco data del giorno
293300090211     C     VABMGS        IFEQ      0
293400090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
293500090211     C                   MOVE      WOGGI         VABMGS                         = oggi
293600090211     C                   END
293700090611      *
293800090211     C*  Eseguo posizionamento su EDiVAB
293900090211     C                   Z-ADD     VABAAS        KAAS
294000090211     C                   Z-ADD     VABlnp        KLNP
294100090211     C                   Z-ADD     VABNRS        KNRS
294200090211     C                   Z-ADD     VABNSP        KNSP
294300090211     C                   MOVEL     SAVBOL        SALVA
294400090211     C     KVAB1         CHAIN     EDiVAB1L                           30
294500090211     C                   MOVEL     SALVA         SAVBOL
294600090611      *
294700090211     C*  Imposto dati CMR
294800090211     C                   Z-ADD     1             VABCNT
294900090211     **
295000090211      *** Attenzione se si deve trattare il VAX con il CMR
295100090211      *** il campo VABCNT deve essere sempre impostato a (1)
295200090211     C     §puWRK        ifEQ      'S'
295300090211     C     §puNSP        andEQ     'S'
295400090211     C                   Z-ADD     1             VABCNT
295500090211     c                   end
295600090211     **
295700090211     C                   Z-ADD     WDTPRE        VABDTS
295800090212     C     WHMPRE        mult      100           VABHMS
295900090611      *
296000090611     C                   Z-ADD     Tes_VABdcm    VABDCM
296100090211     C                   MOVEL     WNRCMR        VABCMR
296200090611      *
296300090211     C*  Se non è indicato il nome del mittente = Partner mittente
296400090211     C     VABRMO        IFEQ      *BLANKS
296500090211     C                   MOVEL     ACORAG        VABRMO
296600090211     C                   MOVEL     INDCAP        VABCMO
296700090211     C     INDCAE        IFNE      *BLANKS
296800090211     C                   MOVEL     INDCAE        VABCMO
296900090211     C                   END
297000090211     C     INDSTA        IFNE      *BLANKS
297100090211     C                   MOVEL     INDSTA        VABNMO
297200090211     C                   END
297300090211     C                   END
297400090611      *
297500090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
297600090211     C     §1BFG1        IFEQ      'N'
297700090211     C     §1BFG7        ANDEQ     'N'
297800090211     C     §1BFG2        ANDEQ     'N'
297900090211     C***                  Z-ADD0         VABNRS
298000090211     C                   END
298100090611      *
298200090211     C*  Imposto segancollo da - a
298300090211     C     §1BFG1        IFEQ      'S'
298400090211     C     §1BFG7        ANDEQ     'N'
298500090211     C     §1BF12        ANDNE     'E'
298600090211      *
298700090211     C     XY            IFEQ      *ZEROS
298800090211     C                   CLEAR                   VABNCD
298900090211     C                   CLEAR                   VABNCA
299000090211     C                   ELSE
299100090211     C                   Z-ADD     SGN(1)        VABNCD
299200090211     C                   Z-ADD     SGN(XY)       VABNCA
299300090211     C                   ENDIF
299400090211     C*
299500090211     C                   END
299600090211     C*
299700090211     C*  Se cap mittente originale a blank abblenco nazione
299800090211     C     VABCMO        IFEQ      *BLANKS
299900090211     C                   MOVEL     *BLANKS       VABNMO
300000090211     C                   END
300100100311     C*
300200100311     C*  aggiungo le NOTE se ci sono
300300100311     C     Wnot1         IFne      *BLANKS
300400100311     c                   movel(p)  Wnot1         vabNOT
300500100311     c                   end
300600100311     C     Wnot2         IFne      *BLANKS
300700100311     c                   movel(p)  Wnot2         vabNT2
300800100311     c                   end
300900100311     C*
301000090603      *
301100090211      *? ------------------------------------------------------------------------
301200090213     C   30              WRITE     EDiVAB00                             99
301300090213     C  N30              UPDATE    EDiVAB00                             99
301400090211      *? ------------------------------------------------------------------------
301500090611     C*
301600090213     c   99              eval      errori_in_Wrt = 'S'
301700090211     C*
301800090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
301900150120      **  il nome del File PDF per la copia d'Ordine (come il VAX ma in PDF)
302000090211     C                   EXSR      WRTVAT_Rif
302100090211     C*
302200090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
302300090211     C     §1BF12        IFEQ      'E'
302400090211     C                   EXSR      WRTVAT
302500090211     C                   ELSE
302600090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
302700090211     C     §1BFG7        IFEQ      'S'
302800090211     C     §1BFG1        OREQ      'S'
302900090211     C                   EXSR      WRTVAD
303000090211     C                   END
303100090211     C                   END
303200090211      *
303300090211     C* Reimposto codice trattamento merce cliente
303400090211     C                   MOVEL     WSVTRM        DS1B
303500090211     C**
303600090211     C* Do errore se previsto CMR ma non c'è
303700090211     C     §PTCMR        IFNE      ' '
303800090211     C                   EXSR      MEMCMR
303900090211     C                   END
304000090611      *
304100090211     C* Do errore se previsto AFB ma non c'è
304200090211     C     §PTNFV        IFNE      ' '
304300090211     C                   EXSR      MEMAFB
304400090211     C                   END
304500090611      *
304600090211     C*  Se il cliente vuole il ritorno delle date di consegna
304700090211     C*  è c'è una squdratura fra i segnacolli e il numero
304800090211     C*  dei colli che ci ha passato scrivo EDSUM
304900090211     C     §PUDTA        IFEQ      'S'
305000090211     C     WSQUAD        ANDEQ     'S'
305100090211     C                   EXSR      WRTSUM
305200090211     C                   END
305300090211     C*
305400090211     C                   ENDSR
305500090211     C*----------------------------------------------------------------
305600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
305700090211     C*----------------------------------------------------------------
305800090211     C     CTRSGN        BEGSR
305900090211     C*
306000090211     C                   MOVEL     'N'           WNSGER            1
306100090211     C*
306200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
306300090211     C     VABNCL        IFNE      XY
306400090211     C                   MOVEL     'S'           WNSGER
306500090211     C                   MOVEL     'S'           WSQUAD
306600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
306700090211     c                              bollettati'
306800090211     C                   GOTO      FINSGN
306900090211     C                   END
307000090211     C*  Riordino i segnacolli
307100090211     C     §1BFG7        IFEQ      'N'
307200090211     C                   SORTA     SGN
307300090211     C*  Controllo se sono sequenziali
307400090211     C                   MOVEL     'N'           WNOSEQ            1
307500090211     C     SGN(1)        ADD       1             WEXSGN            7 0
307600090211     C     2             DO        XY            X
307700090211     C     SGN(X)        IFNE      WEXSGN
307800090211     C                   MOVEL     'S'           WNOSEQ
307900090211     C                   END
308000090211     C     SGN(X)        ADD       1             WEXSGN
308100090211     C                   END
308200090211     C                   END
308300090211     C*
308400090211     C     FINSGN        ENDSR
308500090211     C*----------------------------------------------------------------
308600090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
308700090211     C*----------------------------------------------------------------
308800090211     C     CTRSGE        BEGSR
308900090211     C*
309000090211     C                   MOVEL     'N'           WNSGER            1
309100090211      *
309200090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
309300090211      *
309400090211     C     VABNCL        IFNE      §§
309500090211     C                   MOVEL     'S'           WNSGER
309600090211     C                   MOVEL     'S'           WSQUAD
309700090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
309800090211     c                              bollettati'
309900090211     C                   END
310000090211      *
310100090211     C                   ENDSR
310200090211     C*----------------------------------------------------------------
310300090211     C*? ESEGUO SCRITTURA EDiVAD
310400090211     C*----------------------------------------------------------------
310500090211     C     WRTVAD        BEGSR
310600090211     C*
310700090211     C* Se non seq. scrivo xy record
310800090211     C     §1BFG1        IFEQ      'S'
310900090211     C     XY            ANDNE     *ZEROS
311000090211     C*
311100090211     C     §1BFG7        IFEQ      'S'
311200090211     C                   DO        XY            X
311300090211     C                   Z-ADD     VABCCM        KCCM
311400090211     C                   Z-ADD     SGN(X)        KNCD
311500090211     C                   Z-ADD     VABCNT        VADCNT
311600090211     C                   MOVEL     VABCMR        VADCMR
311700090211     C                   Z-ADD     VABAAS        VADAAS
311800090211     C                   Z-ADD     VABLNP        VADLNP
311900090211     C                   Z-ADD     VABNRS        VADNRS
312000090211     C                   Z-ADD     VABNSP        VADNSP
312100090211     C                   Z-ADD     1             VADNCL
312200090211     C                   MOVEL     SGN(X)        VADCDP
312300090211     C                   Z-ADD     SGN(X)        VADNCD
312400090211     C                   Z-ADD     *ZEROS        VADNCA
312500090211     C                   Z-ADD     VABCCM        VADCCM
312600090211     C                   movel     VABfgs        VADfgs
312700090213     C                   WRITE     EDiVAD00                             99
312800090213     c   99              eval      errori_in_Wrt = 'S'
312900090211     C                   END
313000090211     C* Se sequenziali scrivo un solo record
313100090211     C                   ELSE
313200090211     C                   Z-ADD     VABAAS        VADAAS
313300090211     C                   Z-ADD     VABLNP        VADLNP
313400090211     C                   Z-ADD     VABNRS        VADNRS
313500090211     C                   Z-ADD     VABNSP        VADNSP
313600090211     C                   Z-ADD     VABNCL        VADNCL
313700090211     C                   Z-ADD     SGN(1)        VADNCD
313800090211     C                   Z-ADD     SGN(XY)       VADNCA
313900090211     C                   Z-ADD     VABCCM        VADCCM
314000090211     C                   movel     VABfgs        VADfgs
314100090213     C                   WRITE     EDiVAD00                             99
314200090213     c   99              eval      errori_in_Wrt = 'S'
314300090211     C                   END
314400090211     C*
314500090211     C                   ELSE
314600090211     C                   DO        XY            X
314700090211     C                   Z-ADD     VABCCM        KCCM
314800090211     C                   Z-ADD     SGN(X)        KNCD
314900090211     C                   Z-ADD     VABAAS        VADAAS
315000090211     C                   Z-ADD     VABLNP        VADLNP
315100090211     C                   Z-ADD     VABNRS        VADNRS
315200090211     C                   Z-ADD     VABNSP        VADNSP
315300090211     C                   MOVEL     SGN(X)        VADCDP
315400090211     C                   Z-ADD     1             VADNCL
315500090211     C                   Z-ADD     *ZEROS        VADNCD
315600090211     C                   Z-ADD     *ZEROS        VADNCA
315700090211     C                   Z-ADD     VABCCM        VADCCM
315800090211     C                   movel     VABfgs        VADfgs
315900090213     C                   WRITE     EDiVAD00                             99
316000090213     c   99              eval      errori_in_Wrt = 'S'
316100090211     C                   END
316200090211     C*
316300090211     C                   END
316400090211     C*
316500090211     C                   ENDSR
316600090211     C*----------------------------------------------------------------
316700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
316800090211     C*----------------------------------------------------------------
316900090211     C     WRTVAT_Rif    BEGSR
317000090211      *
317100090211      *  riferimento di consegna destinatario
317200090211     c                   if        WatRDE <> *blank
317300090211     C                   Z-ADD     VABCNT        VATCNT
317400090211     C                   MOVEL     VABCMR        VATCMR
317500090211     C                   Z-ADD     VABCCM        VATCCM
317600090211     C                   movel     VABfgs        VATfgs
317700090211     C                   Z-ADD     VABLNP        VATLNP
317800090211     C                   Z-ADD     VABAAS        VATAAS
317900090211     C                   Z-ADD     VABNRS        VATNRS
318000090211     C                   Z-ADD     VABNSP        VATNSP
318100090211     C                   MOVEL     'A'           VATTRC
318200090211     C                   MOVEL     WatRDE        VATNOT
318300090213     C                   WRITE     EDiVAT00                             99
318400090213     c   99              eval      errori_in_Wrt = 'S'
318500090211     c                   end
318600090211      *
318700090211      *  riferimento di consegna telefonico
318800090211     c                   if        WatTEL <> *blank
318900090211     C                   Z-ADD     VABCNT        VATCNT
319000090211     C                   MOVEL     VABCMR        VATCMR
319100090211     C                   Z-ADD     VABCCM        VATCCM
319200090211     C                   movel     VABfgs        VATfgs
319300090211     C                   Z-ADD     VABLNP        VATLNP
319400090211     C                   Z-ADD     VABAAS        VATAAS
319500090211     C                   Z-ADD     VABNRS        VATNRS
319600090211     C                   Z-ADD     VABNSP        VATNSP
319700090211     C                   MOVEL     'B'           VATTRC
319800090211     C                   MOVEL     WatTEL        VATNOT
319900090213     C                   WRITE     EDiVAT00                             99
320000090213     c   99              eval      errori_in_Wrt = 'S'
320100090211     c                   end
320200090211      *
320300090211      * riferimento del Partner che una volta veniva riportato sul BL4
320400090211     c                   if        Wnsp   <> *blank
320500090211     C                   Z-ADD     VABCNT        VATCNT
320600090211     C                   MOVEL     VABCMR        VATCMR
320700090211     C                   Z-ADD     VABCCM        VATCCM
320800090211     C                   movel     VABfgs        VATfgs
320900090211     C                   Z-ADD     VABLNP        VATLNP
321000090211     C                   Z-ADD     VABAAS        VATAAS
321100090211     C                   Z-ADD     VABNRS        VATNRS
321200090211     C                   Z-ADD     VABNSP        VATNSP
321300090211     C                   MOVEL     'C'           VATTRC
321400090211     C                   MOVEL     Wnsp          VATNOT
321500090213     C                   WRITE     EDiVAT00                             99
321600090213     c   99              eval      errori_in_Wrt = 'S'
321700090211     c                   end
321800150120      *
321900150120      * aggiunto Gen.2015
322000150120      *  copia Ordine in PDF - Nome file tipo record "P"
322100150120     c                   if        TXT_Nome_File_PDF <> *blank
322200150120     C                   Z-ADD     VABCNT        VATCNT
322300150120     C                   MOVEL     VABCMR        VATCMR
322400150120     C                   Z-ADD     VABCCM        VATCCM
322500150120     C                   movel     VABfgs        VATfgs
322600150120     C                   Z-ADD     VABLNP        VATLNP
322700150120     C                   Z-ADD     VABAAS        VATAAS
322800150120     C                   Z-ADD     VABNRS        VATNRS
322900150120     C                   Z-ADD     VABNSP        VATNSP
323000150120     C                   MOVEL     'P'           VATTRC
323100150120     c                   clear                   DVATP
323200150120     c                   eval      §VATPIDZ  = '10'
323300150120     c                   eval      §VATPPDFN = TXT_Nome_File_PDF
323400150120     C                   MOVEL     DVATP         VATNOT
323500150120     C                   WRITE     EDiVAT00                             99
323600150120     c   99              eval      errori_in_Wrt = 'S'
323700150120     c                   end
323800150120      *
323900090211      *
324000090211     C                   ENDSR
324100090211     C*----------------------------------------------------------------
324200090211     C*? ESEGUO SCRITTURA EDiVAT
324300090211     C*----------------------------------------------------------------
324400090211     C     WRTVAT        BEGSR
324500090211      *
324600090211     C                   DO        §§            X
324700090211     C                   Z-ADD     VABCNT        VATCNT
324800090211     C                   MOVEL     VABCMR        VATCMR
324900090211     C                   Z-ADD     VABCCM        VATCCM
325000090211     C                   movel     VABfgs        VATfgs
325100090211     C                   Z-ADD     VABLNP        VATLNP
325200090211     C                   Z-ADD     VABAAS        VATAAS
325300090211     C                   Z-ADD     VABNRS        VATNRS
325400090211     C                   Z-ADD     VABNSP        VATNSP
325500090211     C                   MOVEL     'E'           VATTRC
325600090211      *
325700090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
325800090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
325900090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
326000090211     c                   if        WVATlun > 0
326100090211      *
326200090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
326300090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
326400090211      *
326500090211     c                   else
326600090211      *
326700090211      * Se non è impostato nulla prende il segnacollo passato così com'è
326800090211     C                   MOVEL     SGE(X)        VATNOT
326900090211     c                   end
327000090211      *
327100090213     C                   WRITE     EDiVAT00                             99
327200090213     c   99              eval      errori_in_Wrt = 'S'
327300090211     C                   END
327400090211      *
327500090211     C                   ENDSR
327600090211     C*----------------------------------------------------------------
327700090211     C*? ESEGUO SCRITTURA EDSUM
327800090211     C*----------------------------------------------------------------
327900090211     C     WRTSUM        BEGSR
328000090211     C*
328100090211     C* Se non seq. scrivo xy record
328200090211     C                   DO        XY            X
328300090211     C                   Z-ADD     VABAAS        SUMAAS
328400090211     C                   Z-ADD     VABLNP        SUMFLS
328500090211     C                   MOVEL     WKSC          SUMLNP
328600090211     C                   Z-ADD     VABLNA        SUMLNA
328700090211     C                   Z-ADD     VABZNC        SUMZNC
328800090211     C                   Z-ADD     VABNRS        SUMNRS
328900090211     C                   Z-ADD     VABNSP        SUMNSP
329000090211     C                   Z-ADD     SGN(X)        SUMNSC
329100090211     C                   Z-ADD     WKSC          SUMCCM
329200090211     C                   MOVEL     *BLANKS       SUMSTS
329300090211     C                   MOVEL     *BLANKS       SUMFLG
329400090211     C                   MOVEL     WPRGSP        SUMCNI
329500090211     C                   MOVEL     VABCMR        SUMCMR
329600090211     C                   Z-ADD     0             SUMNFE
329700090211     C                   Z-ADD     VABCCM        VADCCM
329800090213     C                   WRITE     EDSUM000                             99
329900090213     c   99              eval      errori_in_Wrt = 'S'
330000090211     C                   END
330100090211     C*
330200090211     C                   ENDSR
330300090211     c*==================================================================*
330400090211      * Manda un Msg in Posta AS Sede a EDP*
330500090211     c*==================================================================*
330600090211     c     SEND_MSG_EDP  begsr
330700090211      *
330800090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
330900090211      *   Lo manda una sola volta per lavoro
331000090211     c                   IF        Snd_Msg_alert  <> 'N' and
331100090211     c                             User_msg <> *blank
331200090211     c                   z-add     1             Jx
331300090211     C                   exsr      CalQEZSNDMG
331400090211     c                   end
331500090211      *
331600090211     c                   endsr
331700090213     c**********************************************************************
331800090213     c* reperimento numero Spedizione
331900090213     c**********************************************************************
332000090213     C     repnum        BEGSR
332100090213      *
332200090213      *    deve controllare sulla tabella "3C" se il numero spedizione
332300090213      *     deve essere mantenuto oppure no
332400090213     C*
332500090213     C                   clear                   Ds3C
332600090213     C                   Z-ADD     CODUT         TBLKUT
332700090213     C                   MOVEL     '3C'          TBLCOD
332800090213     C                   movel(p)  VABccm        TBLKEY
332900090213     C     KTABel        CHAIN     TABEL00F                           30
333000090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
333100090213     C                   MOVEL     TBLUNI        Ds3C
333200090213     C                   END
333300090213      *
333400090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
333500090213     c                   if        §3CFsp <> 'S'
333600090213      *
333700090213     C* NSP => Stacco un numeratore da AZNUM
333800090213     c                   movel     kpjbu         svkpjbu
333900090213     c                   clear                   kpjbu
334000090213     C                   clear                   TRUL33DS
334100090213     C                   eval      I33OPE = *zeros
334200090213     C                   eval      I33CNU = 302
334300090213     C                   eval      I33NUM = 1
334400090213     C                   movel     TRUL33DS      KPJBU
334500090213     C                   call      'TRUL33R'
334600090213     C                   parm                    KPJBA
334700090213     C                   movel     KPJBU         TRUL33DS
334800090213     c                   movel     svkpjbu       kpjbu
334900090213      ****
335000090213     C                   if        O33ERR = *zeros
335100090213     c                   z-add     o33nrf        NUM_SPED
335200090213     C                   else
335300090213     C                   endif
335400090213      ****
335500090213     c                   ELSE
335600090213      *   se si vuole mantenere il numero spedizione
335700090213      ****
335800090213     C                   MOVEL     WOGGI         WANNO             4 0
335900090213     C                   MOVE      WANNO         KAAA
336000090213     C                   Z-ADD     3             KCNU
336100090213     C                   Z-ADD     VABlnp        KFIL
336200090213     C     KNUF          CHAIN     FLNUF                              9999
336300090213     C     *IN99         IFEQ      '0'
336400090213     C                   ADD       1             NUFNUM
336500090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
336600090213     C                   UPDATE    FLNUF
336700090213     C                   ELSE
336800090213     C                   END
336900090213     C*
337000090213     c                   EndIF
337100090213      **
337200090213     C                   ENDSR
337300090213     C*----------------------------------------------------------------
337400090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
337500090213     C*----------------------------------------------------------------
337600090213     C     MEMCMR        BEGSR
337700090213     C*
337800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
337900090213     C     §PTCM1        IFEQ      'S'
338000090213     C                   CLEAR                   EDRDE000
338100090213     C                   MOVEL     'TD1154'      KNMC
338200090213     C                   Z-ADD     1             KNRP
338300090213     C                   MOVEL     WKSC          KLNP1
338400090213     C     KRDE          CHAIN     EDRDE01L                           31
338500090213     C     *IN31         IFEQ      '1'
338600090213     C                   Z-ADD     VABAAS        RDEAAS
338700090213     C                   Z-ADD     VABLNP        RDELNP
338800090213     C                   Z-ADD     VABNRS        RDENRS
338900090213     C                   Z-ADD     VABNSP        RDENSP
339000090213     C                   MOVEL     'TD1154'      RDENMC
339100090213     C                   Z-ADD     1             RDENRP
339200090213     C                   MOVEL     WNRCMR        RDEVAL
339300090213     C                   WRITE     EDRDE000                             99
339400090213     c   99              eval      errori_in_Wrt = 'S'
339500090213     C                   END
339600090213     C*  Memorizzo qualificatore CMR
339700090213     C                   CLEAR                   EDRDE000
339800090213     C                   MOVEL     'TD1153'      KNMC
339900090213     C                   Z-ADD     1             KNRP
340000090213     C                   MOVEL     WKSC          KLNP1
340100090213     C     KRDE          CHAIN     EDRDE01L                           31
340200090213     C     *IN31         IFEQ      '1'
340300090213     C                   Z-ADD     VABAAS        RDEAAS
340400090213     C                   Z-ADD     VABLNP        RDELNP
340500090213     C                   Z-ADD     VABNRS        RDENRS
340600090213     C                   Z-ADD     VABNSP        RDENSP
340700090213     C                   Z-ADD     1             RDENRP
340800090213     C                   MOVEL     'TD1153'      RDENMC
340900090213     C                   MOVEL     'CMR'         RDEVAL
341000090213     C                   WRITE     EDRDE000                             99
341100090213     c   99              eval      errori_in_Wrt = 'S'
341200090213     C                   END
341300090213     C*  Memorizzo la data
341400090213     C                   CLEAR                   EDRDE000
341500090213     C                   MOVEL     'TD2380'      KNMC
341600090213     C                   Z-ADD     1             KNRP
341700090213     C                   MOVEL     WKSC          KLNP1
341800090213     C     KRDE          CHAIN     EDRDE01L                           31
341900090213     C     *IN31         IFEQ      '1'
342000090213     C                   Z-ADD     VABAAS        RDEAAS
342100090213     C                   Z-ADD     VABLNP        RDELNP
342200090213     C                   Z-ADD     VABNRS        RDENRS
342300090213     C                   Z-ADD     VABNSP        RDENSP
342400090213     C                   Z-ADD     1             RDENRP
342500090213     C                   MOVEL     'TD2380'      RDENMC
342600090611     C                   MOVEL     Tes_VABdcm    RDEVAL
342700090213     C                   WRITE     EDRDE000                             99
342800090213     c   99              eval      errori_in_Wrt = 'S'
342900090213     C                   END
343000090213     C*
343100090213     C                   END                                                    §PTCM1 = 'S'
343200090213     C*
343300090213     C                   ENDSR
343400090213     C*----------------------------------------------------------------
343500090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
343600090213     C*----------------------------------------------------------------
343700090213     C     MEMAFB        BEGSR
343800090213     C*
343900090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
344000090213     C     §PTNF1        IFEQ      'S'
344100090213     C                   CLEAR                   EDRDE000
344200090213     C                   MOVEL     WKSC          KLNP1
344300090213     C                   MOVEL     'TD1154'      KNMC
344400090213     C                   Z-ADD     1             KNRP
344500090213     C     KRDE          CHAIN     EDRDE01L                           31
344600090213     C     *IN31         IFEQ      '1'
344700090213     C                   Z-ADD     VABAAS        RDEAAS
344800090213     C                   Z-ADD     VABLNP        RDELNP
344900090213     C                   Z-ADD     VABNRS        RDENRS
345000090213     C                   Z-ADD     VABNSP        RDENSP
345100090213     C                   MOVEL     'TD1154'      RDENMC
345200090213     C     §PTCM1        IFNE      'S'
345300090213     C     WNRCMR        OREQ      *BLANKS
345400090213     C                   Z-ADD     1             RDENRP
345500090213     C                   ELSE
345600090213     C                   Z-ADD     2             RDENRP
345700090213     C                   END
345800090213     C                   MOVEL     WNUMFV        RDEVAL
345900090213     C                   WRITE     EDRDE000                             99
346000090213     c   99              eval      errori_in_Wrt = 'S'
346100090213     C                   END
346200090213     C*  Memorizzo qualificatore CMR
346300090213     C                   CLEAR                   EDRDE000
346400090213     C                   MOVEL     WKSC          KLNP1
346500090213     C                   MOVEL     'TD1153'      KNMC
346600090213     C                   Z-ADD     1             KNRP
346700090213     C     KRDE          CHAIN     EDRDE01L                           31
346800090213     C     *IN31         IFEQ      '1'
346900090213     C                   Z-ADD     VABAAS        RDEAAS
347000090213     C                   Z-ADD     VABLNP        RDELNP
347100090213     C                   Z-ADD     VABNRS        RDENRS
347200090213     C                   Z-ADD     VABNSP        RDENSP
347300090213     C     §PTCM1        IFNE      'S'
347400090213     C     WNRCMR        OREQ      *BLANKS
347500090213     C                   Z-ADD     1             RDENRP
347600090213     C                   ELSE
347700090213     C                   Z-ADD     2             RDENRP
347800090213     C                   END
347900090213     C                   MOVEL     'TD1153'      RDENMC
348000090213     C                   MOVEL     'AFB'         RDEVAL
348100090213     C                   WRITE     EDRDE000                             99
348200090213     c   99              eval      errori_in_Wrt = 'S'
348300090213     C                   END
348400090213     C*  Memorizzo la data
348500090213     C                   CLEAR                   EDRDE000
348600090213     C                   MOVEL     WKSC          KLNP1
348700090213     C                   MOVEL     'TD2380'      KNMC
348800090213     C                   Z-ADD     1             KNRP
348900090213     C     KRDE          CHAIN     EDRDE01L                           31
349000090213     C     *IN31         IFEQ      '1'
349100090213     C                   Z-ADD     VABAAS        RDEAAS
349200090213     C                   Z-ADD     VABLNP        RDELNP
349300090213     C                   Z-ADD     VABNRS        RDENRS
349400090213     C                   Z-ADD     VABNSP        RDENSP
349500090213     C     §PTCM1        IFNE      'S'
349600090213     C     WNRCMR        OREQ      *BLANKS
349700090213     C                   Z-ADD     1             RDENRP
349800090213     C                   ELSE
349900090213     C                   Z-ADD     2             RDENRP
350000090213     C                   END
350100090213     C                   MOVEL     'TD2380'      RDENMC
350200090611     C                   MOVEL     Tes_VABdcm    RDEVAL
350300090213     C                   WRITE     EDRDE000                             99
350400090213     c   99              eval      errori_in_Wrt = 'S'
350500090213     C                   END
350600090213     C*
350700090213     C                   END                                                    §PTCM1 = 'S'
350800090213     C*
350900090213     C                   ENDSR
351000090213     c*==================================================================*
351100090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
351200090213     c*==================================================================*
351300090213     c     Imp_ADDR_mail begsr
351400090213      *
351500090213     c                   clear                   Indirizzi
351600090213      *
351700090213      *  Lunghezza indirizzi presenti
351800090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
351900090213     c                   z-add     1             al_char           3 0
352000090213     c                   z-add     1             dal_char          3 0
352100090213     c                   z-add     1             tot_char          3 0
352200090213      *
352300090213     c     successivo    tag
352400090213      *
352500090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
352600090213      *   e aggiunge il dominio Bartolini + il (;) separatore
352700090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
352800090213     c                   eval      tot_char = al_char - dal_char
352900090213     c                   z-add     dal_char      Dac
353000090213     c                   z-add     tot_char      Luc
353100090213      *
353200090213     c                   clear                   Indir_Bartol     40
353300090213     c                   clear                   Indir_Parz       20
353400090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
353500090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
353600090213     c                             %Trim(bart_it)
353700090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
353800090213      *
353900090213     c                   if        al_char = Lun_Ind
354000090213     c                   goto      fine_indmail
354100090213     c                   else
354200090213     c                   eval      dal_char = ( al_char + 1 )
354300090213     c                   goto      successivo
354400090213     c                   end
354500090213      *
354600090213     C     fine_indmail  TAG
354700090213      *
354800090213     C                   ENDSR
354900090213     c*==================================================================*
355000090213      * Manda un Msg x E-mail
355100090213     c*==================================================================*
355200090213     c     Invio_Mail    begsr
355300090213      *
355400090213     c                   goto      non_inviare
355500090213      *
355600090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
355700090213     C* Inizializzo variabili
355800090213     C                   movel     *blanks       wrkEml          253
355900090213     C                   movel     *blanks       wrkMsg         5000
356000090213     C                   movel     *blanks       wrkOgg           44
356100090213      *
356200090213     C* Valorizzo i campi della e-m@ail - indirizzo
356300090213     C                   eval      wrkEml = Indirizzi
356400090213     C                   eval      wrkOgg = Oggetto
356500090213     C                   eval      wrkMsg = Messaggio
356600090213
356700090213     C                   call(e)   'TIS701C'
356800090213     C                   parm                    wrkEml
356900090213     C                   parm                    wrkOgg
357000090213     C                   parm                    wrkMsg
357100090213
357200090213     c     non_inviare   tag
357300090213     C*
357400090213     C                   ENDSR
357500090603     c*==================================================================*
357600090603      * Manda un Msg x E-mail a EDPAB
357700090603     c*==================================================================*
357800090603     c     Invio_Mail_AB begsr
357900090603      *
358000090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
358100090603     C* Inizializzo variabili
358200090603     C                   movel     *blanks       wrkEml          253
358300090603     C                   movel     *blanks       wrkMsg         5000
358400090603     C                   movel     *blanks       wrkOgg           44
358500090603      *
358600090603     C* Valorizzo i campi della e-m@ail - indirizzo
358700110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
358800110203     C                   eval      wrkEml = Indirizzi
358900090603     C                   eval      wrkOgg = Oggetto
359000090603     C                   eval      wrkMsg = Messaggio
359100110203      ********
359200110203     C********           call(e)   'TIS701C'
359300110203     C********           parm                    wrkEml
359400110203     C********           parm                    wrkOgg
359500110203     C********           parm                    wrkMsg
359600110203     C*
359700110203      ** *****
359800110203     C                   call(e)   'TRTCT00R2'
359900110203     C                   parm                    wrkEml
360000110203     C                   parm                    wrkOgg
360100110203     C                   parm                    wrkMsg
360200090603     C*
360300090616     c                   if        %error
360400090616     c                   end
360500090616     C*
360600090603     C                   ENDSR
360700090211     ***********************************************************************
360800090211     **
360900090211     ** Send Message (QEZSNDMG) API
361000090211     **
361100090211     ***********************************************************************
361200090211     C     CalQEZSNDMG   BEGSR
361300090211      *
361400090211     ** Invio messaggio all'utente.
361500090211     C                   EVAL      SndMg03 = msgDSP
361600090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
361700090211     C                   EVAL      SndMg05(Jx) = User_msg
361800090211     C                   EVAL      SndMg06 = Jx
361900090211     C                   CLEAR                   QUSEC
362000090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
362100090211
362200090211     C                   CALL      'QEZSNDMG'
362300090211     C                   PARM                    SndMg01
362400090211     C                   PARM                    SndMg02
362500090211     C                   PARM                    SndMg03
362600090211     C                   PARM                    SndMg04
362700090211     C                   PARM                    SndMg05
362800090211     C                   PARM                    SndMg06
362900090211     C                   PARM                    SndMg07
363000090211     C                   PARM                    SndMg08
363100090211     C                   PARM                    Qusec
363200090211     C                   PARM                    SndMg10
363300090211     C                   PARM                    SndMg11
363400090211     C                   PARM                    SndMg12
363500090211      *
363600090211     C                   ENDSR
363700121105      *---------------------------------------------------------------*
363800121105      * CTRL caricamento schiere    PT                               -*
363900121105      *---------------------------------------------------------------*
364000121105     C     Check_PT      begsr
364100121105     C*
364200121105     c* Controllo riempimento schiera
364300121105     c                   clear                   trul0sds
364400121105     c                   eval      i0sski='CPT'
364500121105     c                   eval      i0sele=%elem(CPT)
364600121105     c                   eval      i0spie=x
364700121105     c                   eval      i0sfile='EDTAB00F'
364800121105     c                   eval      i0ssif=knsif
364900121105     c                   eval      i0spgm='TRTCT05R'
365000121113     c                   move      kpjbu         SvKpjbu
365100121113     c                   clear                   kpjbu
365200121105     c                   movel     trul0sds      kpjbu
365300121105     c                   call      'TRUL0SR'
365400121105     c                   parm                    kpjba
365500121113     c                   eval      kpjbu  =  SvKpjbu
365600121105     C*
365700121105     C                   ENDSR
365800090211     O*****************************************************************
365900090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
366000090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
366100090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
366200090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
