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
011300090209      *---------------------------------------------------------------------*
011400090209     D KPJBA         E DS
011500090209     D FNLV55DS      E DS
011600090209     D TIBS55DS      E DS
011700090209     D TISI95DS      E DS
011800090209     D UTEDSE0F      E DS
011900090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012000090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012100090209      * Ds scomposzione tipo capoconti
012200090209     D TCUDS           DS
012300090209     D  F34                    3      4
012400090209     D  F56                    5      6
012500090209     D DS1B          E DS
012600090209     D DS15          E DS
012700090209     D DSCV          E DS
012800090213     D TRTC86DS      E DS
012900090209     D EDIDSPT       E DS
013000090209     D EDIDSPU       E DS
013100090209     D EDIDSPV       E DS
013200090211     D EDIDSCL       E DS
013300090216     D EDIDSTC       E DS
013400090209      **
013500090209      *  DS x salvataggio dati impostati in EDiVAB
013600090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
013700090209     D SALVA           DS           545
013800090210      **
013900090209     D WLBDA8          DS
014000090209     D  G02DAT                 1      8  0
014100090209     D  G02INV                 9     16  0
014200090209     D  G02ERR                17     17
014300090209     D  G02TGI                18     22  0
014400090209      *  DS x scomposizione numero spedizione
014500090209     D DSSPE           DS
014600090209     D  SPEAAS                 1      4  0
014700090209     D  SPELNP                 5      7  0
014800090209     D  SPENRS                 8      9  0
014900090209     D  SPENSP                10     16  0
015000090209      *  DS x scomposizione segnacollo
015100090209     D DSSGN           DS
015200090209     D  SGNLNP                 1      3  0
015300090209     D  SGNLNA                 4      6  0
015400090209     D  SGNNRS                 7      8  0
015500090209     D  SGNNSC                 9     15  0
015600090209     D  SGNZNC                16     17  0
015700090209      *  DS x stampa segnacollo
015800090209      *---------------                                                      *
015900090211     D  vablnp_PT      s                   LIKE(vablnp)
016000090212     D  vabnrs_PT      s                   LIKE(vabnrs)
016100090211     D  vabctm_PT      s                   LIKE(vabctm)
016200090209     D Wvabtic         s                   LIKE(vabtic)
016300090603     d tes_VABrmo      s                   LIKE(VABrmo)
016400090603     d tes_VABnmo      s                   LIKE(VABnmo)
016500090603     d tes_VABcmo      s                   LIKE(VABcmo)
016600090209     C*
016700090603     d tes_VABrsd      s                   LIKE(VABrsd)
016800090603     d tes_VABrd2      s                   LIKE(VABrd2)
016900090603     d tes_VABind      s                   LIKE(VABind)
017000090603     d tes_VABlod      s                   LIKE(VABlod)
017100090603     d tes_VABnzd      s                   LIKE(VABnzd)
017200090603     C*
017300090603     d tes_Valuta      s                   LIKE(VABvca)
017400090603     C*
017500090209      *---------------------------------------------------------------------*
017600090209      * Schiere
017700090209      *---------------------------------------------------------------------*
017800090209      * Schiera per reperimento nr.seganacollo
017900090209     D SGN             S              7  0 DIM(5000)
018000090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
018100090209      * Stringhe per conversione alfanumerico/numerico N.Documento
018200090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
018300090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
018400090209      * Nr. documento alfanumerico da decodificare
018500090209     D NDA             S              1    DIM(35)
018600090209      * Nr. documento alfanumerico da già decodificato
018700090209     D NDN             S              1    DIM(15)
018800090209      * Messaggi di errore
018900090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
019000090209      * Tabella codici trattamento merce (1B)
019100090209     D CTM             S              2    DIM(200)
019200090209     D DTM             S             67    DIM(200)
019300090209      * Tabella codici valuta
019400090209     D CCV             S              3    DIM(200)
019500090209     D DCV             S             89    DIM(200)
019600090209      * Tabella codici nazioni
019700090209     D C15             S              3    DIM(500)
019800090209     D ISO             S              3    DIM(500)
019900090209     D CPF             S              2  0 DIM(500)
020000090209      * Tabella partner
020100121105     D CPT             S             35    DIM(200)
020200121105     D CPTparz         S             35    DIM(%elem(CPT))
020300121105     D KSC_UNB         S              7    DIM(%elem(CPT))
020400121105     D DPT             S             90    DIM(%elem(CPT))
020500121105     D DPU             S             90    DIM(%elem(CPT))
020600121105     D DPV             S             90    DIM(%elem(CPT))
020700121105     D TRUL0SDS      E DS
020800121105      **
020900090209     d Ptn_parziale    s              1    inz('N')
021000090209     d sav_CPTx        s              3  0 inz(0)
021100090209      *
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
025300090209     D pVLB            s                   LIKE(VABVLB)
025400090209     D kKUT            s                   LIKE(TBLKUT)
025500090209     D kCOD            s                   LIKE(TBLCOD)
025600090209     D kKEY            s                   LIKE(TBLKEY)
025700090209     D kKCC            s                   LIKE(ACOKCC)
025800090209     D kKSC            s                   LIKE(ACOKSC)
025900090209     D kAAA            s                   LIKE(NUFAAA)
026000090209     D kCNU            s                   LIKE(NUFCNU)
026100090209     D kFIL            s                   LIKE(NUFFIL)
026200090209     D kAAS            s                   LIKE(VABAAS)
026300090209     D kLNP            s                   LIKE(VABLNP)
026400090209     D kNRS            s                   LIKE(VABNRS)
026500090209     D kNSP            s                   LIKE(VABNSP)
026600090209     D kCMR            s                   LIKE(VABCMR)
026700090209     D kCCM            s                   LIKE(VABCCM)
026800090209     D kNCD            s                   LIKE(VADNCD)
026900090210     D kCNT            s                   LIKE(VADCNT)
027000090209     D kNMC            s                   LIKE(RDENMC)
027100090209     D kNRP            s                   LIKE(RDENRP)
027200090209     D kLNP1           s                   LIKE(RDELNP)
027300090209      *
027400090209      *  Invio E-mail
027500090209     D Indirizzi       s            253
027600090209     D Oggetto         s             44
027700090209     D Messaggio       s           5000
027800090209      *
027900090209     d   DSmail        ds
028000090209     D Mail_Ind                      44
028100090209     d   IND_char                     1    dim(44)
028200090209      *
028300090209     D Lun_Ind         s              5u 0
028400090209      *
028500090209     D Dac             s              5u 0
028600090209     D Luc             s              5u 0
028700090209      *
028800090209     C*---------------------------------------------------------------*
028900090209     D MsgDSP          S            256                                         Testo generico
029000090209     C*---------------------------------------------------------------*
029100090209     D SndMg01         S             10                                         Message type
029200090209     D                                     INZ('*INFO')
029300090209     D SndMg02         S             10                                         Deluvery mode
029400090209     D                                     INZ('*BREAK')
029500090209     D SndMg03         S            256                                         Message text
029600090209     D SndMg04         S             10I 0                                      Length of text
029700090209     D                                     INZ(%SIZE(SndMg03))
029800090209     D SndMg05         S             10                                         User profile
029900090209     D                                     DIM(299)
030000090209     D SndMg06         S             10I 0                                      Number of user
030100090209     D SndMg07         S             10I 0                                      Message sent indic.
030200090209     D SndMg08         S             10I 0                                      Function requested
030300090209     D SndMg10         S              1                                         Show display
030400090209     D                                     INZ('N')
030500090209     D SndMg11         S             20                                         Qualified MSGQ name
030600090209     D SndMg12         S              4                                         Name type indicator
030700090209     D                                     INZ('*USR')
030800090209      * indice di scihera
030900090209     D Jx              S              5I 0
031000090209      *
031100090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031200090209      *
031300090209     d bart_it         C                   CONST('@Bartolini.it;')
031400090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031500060612      * ?================================================================== */
031600060612      *
031700060612      * ?   * Campi x decodifica * (INPUT  del Record)
031800090130     D  Dati           s           2048
031900060612      * ?   *--------------------------------------------------------------*
032000060612     D  se_errore      s              1    inz(' ')
032100060612      * ?* ------------------------------------------------------ *
032200060710     D Digits          C                   '0123456789'
032300091019     D*------------------
032400091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
032500091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
032600060612      * ?================================================================== */
032700060612      *   Ciclo principale
032800090205      * ?================================================================== */
032900130321     c                   clear                   manda_mail        1
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
039400091016     c                   if        %Eof(tivin00r)
039500091016     c                   move      'S'           EoFTivin
039600091016     c                   end
039700060612
039800060612     c                   dow       not %eof(Tivin00r)
039900060614
040000060614      * solo i record sflaggati da rielaborare
040100060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
040200090211      *  Sempre Record OK
040300090211      ** Controlli formali sui campi
040400090211     C                   eval      vinFLG = '1'
040500060612     c                   clear                   se_errore
040600060612
040700060612      ** Decodifica record a campi variabili
040800060612      * ?              /*---------------------- */
040900090205     c                   exsr      Decod_Segmento
041000090209      * ?              /*---------------------- */
041100090209
041200090213      *** ------------------------------------
041300091019????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT" x IFTMIN
041400090213      *** ------------------------------------
041500090213      * ?              /*---------------------- */
041600090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
041700090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
041800090213      * ?              /*---------------------- */
041900090601      ***
042000091019????  **
042100091019????  *   OGNI fine DETTAGLIO è determinato dall'UNT.
042200091019???? c                   if           tipo_seg = 'UNT'
042300091019????  *
042400090213      *  Deve flaggare il dettaglio con dei problemi
042500090213     C                   if        Err_in_detta = 'S'
042600090213      **?              /*---------------------- */
042700090213     c                   exsr      DETta_Errato
042800090213      **?              /*---------------------- */
042900090213     c                   end
043000090213      *
043100060612      **  Se presente un errore nel record emette una segnalazione msg
043200091019???? c                   if        se_errore = 'S'
043300091019???? c                   else
043400091019????  ******
043500090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043600090227     c                   if         se_trovato_NAD ='S'
043700060612      * ?              /*---------------------- */
043800090209     c                   exsr      Scrive_VAB
043900060612      * ?              /*---------------------- */
044000090227     c                   else
044100090227     c                   eval      errori_in_Wrt = 'S'
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
047100091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
047200090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047300090603     C                             Bolle import in filiale - messaggio -
047400090603     C                             con UNB non presente in tabella PT controlla-
047500090603     C                             re EDI ricevuto su UPLOAD.'
047600090603     c                   exsr      invio_mail_AB
047700060621     c                   end
047800060612
047900091016     c                   if        EoFTivin = ' '
048000060612     c                   update    Tivin000
048100091016     c                   end
048200090211
048300090211      *** se non è riconosciuto l'UNB deve uscire direttamente
048400090211     c                   if        se_errore ='S'  and
048500090211     c                             tipo_seg = 'UNB'
048600090213     c                   eval      err_bloccante = 'S'
048700090211     c                   exsr      Msg_errato
048800090211     c                   LEAVE
048900090211     c                   endIF
049000090211      ***
049100060614     c                   endIF
049200060612
049300060612     c                   read      Tivin00r
049400091016     c                   if        %Eof(tivin00r)
049500091016     c                   move      'S'           EoFTivin
049600091016     c                   end
049700060612     C                   ENDdo
049800060612      **
049900060612     c                   endSR
050000100120      * ?------------------------------------------------------------------ */
050100100120      *? Controlla la trasmissione   se completa
050200100120      * ?------------------------------------------------------------------ */
050300100120     c     Check_Trasm   Begsr
050400100120
050500100120     C                   clear                   se_errore
050600100120     C                   clear                   Riga_iNIZIo       1
050700100120     C                   clear                   Riga_fINe         1
050800100120      ** primo  record
050900100120     c     *start        setll     tivin00r
051000100120      *
051100100120     c     rileggi       tag
051200100120     c                   read      tivin00r
051300100120     c                   if        %Eof(tivin00r)
051400100120     c                   move      'S'           EoFTivin
051500100120     c                   end
051600100120      *
051700100120     c                   if        not %eof(tivin00r)
051800100120      *
051900100120     c***********        movel(p)  VINDTA        dati
052000100120     c                   clear                   dati
052100100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
052200100120     c                   if        dati = *blank
052300100120      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
052400100120      * le leggo e le escludo.
052500100120     C                   eval      vinFLG = '1'
052600100120     c                   if        EoFTivin = ' '
052700100120     c                   update    Tivin000
052800100120     c                   end
052900100120     c                   goto      rileggi
053000100120     c                   end
053100100120      * ?              /*---------------------- */
053200100120     c                   exsr      Decod_Segmento
053300100120      * ?              /*---------------------- */
053400100120     c                   endif
053500100120      **
053600100120      ** ultimo record
053700100120     c     *hival        setll     tivin00r
053800100120     c     leggiAncora   tag
053900100120     c                   readp     tivin00r
054000100120     c                   if        %Eof(tivin00r)
054100100120     c                   move      'S'           EoFTivin
054200100120     c                   end
054300100120     c                   if        not %eof(tivin00r)
054400100120      *
054500100120     c***********        movel(p)  VINDTA        dati
054600100120     c                   clear                   dati
054700100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
054800100120     c                   if        dati = *blank
054900100120      * le ultime righe potrebbero essere vuote prima di finire il messaggio
055000100120      * le leggo e le escludo.
055100100120     C                   eval      vinFLG = '1'
055200100120     c                   if        EoFTivin = ' '
055300100120     c                   update    Tivin000
055400100120     c                   end
055500100120     c                   goto      leggiAncora
055600100120     c                   end
055700100120      * ?              /*---------------------- */
055800100120     c                   exsr      Decod_Segmento
055900100120      * ?              /*---------------------- */
056000100120     c                   endif
056100100120
056200100120      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
056300100120      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
056400100120      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
056500100120     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
056600100120      * ?-----> Errore
056700100120     C                   eval      se_errore = 'S'
056800100120     c                   end
056900100120
057000100120     c                   endSR
057100100120      * ?------------------------------------------------------------------ */
057200100120      *?    Decodifica il segmento ed importa i campi in formato DS
057300100120      * ?------------------------------------------------------------------ */
057400100120     c     Decod_SegmentoBegsr
057500100120      **
057600100120     c                   clear                   dati
057700100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
057800100120     c                   eval      tipo_seg = %subst(dati:1:3)
057900100120     c                   eval      segmento = dati
058000100120      *
058100100120      * Imposta il flag di Inizio Dettaglio
058200100120     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
058300100120     c                   eval      Inizio_Dettaglio ='S'
058400100120     c                   end
058500100120      *
058600100120     c                   clear                   seg_imprevisto
058700100720     c                   clear                   trovato_seg
058800100716      *
058900100716     C                   clear                   keyUNBCLI
059000100716     C                   clear                   keyTIPOMSG
059100100716     C                   clear                   keyVERSION
059200100716     C                   clear                   keyRELEASE
059300100716     C                   clear                   keyAGENCY
059400100716     C                   clear                   keyASSOCIA
059500100720      *
059600100720      *  Chiamata generica x decodificare il singolo segmento letto
059700100720     c                   call      'TRTCT00R1'
059800100720      * input
059900100720     c                   parm                    tipo_seg
060000100720     c                   parm                    segmento
060100100720     C                   parm                    keyUNBCLI
060200100720     C                   parm                    keyTIPOMSG
060300100720     C                   parm                    keyVERSION
060400100720     C                   parm                    keyRELEASE
060500100720     C                   parm                    keyAGENCY
060600100720     C                   parm                    keyASSOCIA
060700100720      *output
060800100720     c                   parm                    Errore
060900100720     c                   parm                    DsGenerica
061000100720     c                   parm                    Valori_inErr
061100100720     c                   parm                    Descr_Errore
061200100720     c                   parm                    trovato_seg
061300100720      **--------
061400100720      *  Ritorna la DS GENERICA oppure l'errore
061500100120      **--------
061600100120     c                   select
061700100720      *
061800100720     c                   when      trovato_seg ='N'
061900100720      *
062000100720     c                   move      'S'           seg_imprevisto
062100130321     c                   eval           manda_mail='S'
062200100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
062300100720     c                   exsr      Error_DET
062400100720     c                   goto      end_Decod
062500100720      **--------
062600100120     c                   when      tipo_seg = 'UNA'
062700100720     c                   movel(p)  DsGenerica    EDS00UNA
062800100120      *
062900100720      **--------
063000100120     c                   when      tipo_seg = 'UNB'
063100100720     c                   movel(p)  DsGenerica    EDS00UNB
063200100120     C                   EXSR      DATI_UNB
063300100120      * --------
063400100120     c                   when      tipo_seg = 'UNH'
063500100720     c                   movel(p)  DsGenerica    EDS00UNH
063600100120     C                   EXSR      DATI_UNH
063700100120      * --------
063800100120     c                   when      tipo_seg = 'BGM'
063900100720     c                   movel(p)  DsGenerica    EDS00BGM
064000100120      **--------
064100100120     c                   when      tipo_seg = 'DTM'
064200100720     c                   movel(p)  DsGenerica    EDS00DTM
064300100120     C                   EXSR      DATI_DTM
064400100120      * --------
064500100120     c                   when      tipo_seg = 'TSR'
064600100720     c                   movel(p)  DsGenerica    EDS00TSR
064700100120      **--------
064800100120     c                   when      tipo_seg = 'CNT'
064900100720     c                   movel(p)  DsGenerica    EDS00CNT
065000100120      **--------
065100100120     c                   when      tipo_seg = 'CUX'
065200100720     c                   movel(p)  DsGenerica    EDS00CUX
065300100120      * --------
065400100120     c                   when      tipo_seg = 'TOD'
065500100720     c                   movel(p)  DsGenerica    EDS00TOD
065600100120     C                   EXSR      DATI_TOD
065700100120      **--------
065800100120     c                   when      tipo_seg = 'RFF'
065900100720     c                   movel(p)  DsGenerica    EDS00RFF
066000100120     C                   EXSR      DATI_RFF
066100100120      **--------
066200100120     c                   when      tipo_seg = 'TDT'
066300100720     c                   movel(p)  DsGenerica    EDS00TDT
066400100120      **--------
066500100120     c                   when      tipo_seg = 'MOA'
066600100720     c                   movel(p)  DsGenerica    EDS00MOA
066700100120     C                   EXSR      DATI_MOA
066800100120      **--------
066900100120     c                   when      tipo_seg = 'FTX'
067000100720     c                   movel(p)  DsGenerica    EDS00FTX
067100100120     C                   EXSR      DATI_FTX
067200100120      **--------
067300100120     c                   when      tipo_seg = 'NAD'
067400100720     c                   movel(p)  DsGenerica    EDS00NAD
067500100120     C                   EXSR      DATI_NAD
067600100120      **--------
067700100120     c                   when      tipo_seg = 'CTA'
067800100720     c                   movel(p)  DsGenerica    EDS00CTA
067900100120     C                   EXSR      DATI_CTA
068000100120      **--------
068100100120     c                   when      tipo_seg = 'COM'
068200100720     c                   movel(p)  DsGenerica    EDS00COM
068300100120     C                   EXSR      DATI_COM
068400100120      **--------
068500100120     c                   when      tipo_seg = 'GID'
068600100720     c                   movel(p)  DsGenerica    EDS00GID
068700100120     C                   EXSR      DATI_GID
068800100120      **--------
068900100120     c                   when      tipo_seg = 'MEA'
069000100720     c                   movel(p)  DsGenerica    EDS00MEA
069100100120     C                   EXSR      DATI_MEA
069200100120      **--------
069300100120     c                   when      tipo_seg = 'PCI'
069400100720     c                   movel(p)  DsGenerica    EDS00PCI
069500100120     C                   EXSR      DATI_PCI
069600100120      **--------
069700100120     c                   when      tipo_seg = 'UNT'
069800100720     c                   movel(p)  DsGenerica    EDS00UNT
069900100120     C                   EXSR      DATI_UNT
070000100120      **--------
070100100120     c                   when      tipo_seg = 'UNZ'
070200100720     c                   movel(p)  DsGenerica    EDS00UNZ
070300100120     C                   EXSR      DATI_UNZ
070400100120      **--------
070500100120     c                   OTHER
070600100120      **--------
070700100120     c                   move      'S'           seg_imprevisto
070800100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
070900130321     c                   eval           manda_mail='S'
071000100120     c                   exsr      Error_DET
071100100120     c                   goto      end_Decod
071200100120      **--------
071300100120     c                   endSL
071400100120      **
071500100120     c     end_Decod     endSR
071600090211     C*----------------------------------------------------------------
071700090211     C*? AZZMSG - Azzera dati messaggio
071800090211     C*----------------------------------------------------------------
071900090211     C     AZZMSG        BEGSR
072000090211     C*
072100090211     C* Pulisco dati di work x foglio viaggio
072200090211     C                   MOVEL     *BLANKS       WNUMFV           35
072300090211     C                   MOVEL     *BLANKS       WNRCMR           35
072400090211     C                   Z-ADD     0             WDATPA            8 0
072500090211     C                   Z-ADD     0             WDATFV            8 0
072600090211     C                   Z-ADD     0             WHHNFV            4 0
072700090211     C                   Z-ADD     0             WDTCMR            8 0
072800090211     C                   Z-ADD     0             WHHCMR            4 0
072900090211      *
073000090211      * pulisce gli RFF+FF di testata
073100090211     C                   clear                   $CLKSC            7 0
073200090211     C                   clear                   $CLTAR            3
073300090211     C                   clear                   $CLLNP            3 0
073400090211     C                   clear                   $CLNRS            2 0
073500090211     C                   clear                   $CLCTM            2
073600090211     C                   clear                   $CLBS1            1
073700090211     C                   clear                   $CLfnsp           1            *blk/S
073800090211      *
073900090211     C*  Inizializzo variabile new documento
074000090211     C                   Z-ADD     0             KCL
074100090211     C                   Z-ADD     0             WW                3 0
074200090211     ** Inizializza campo di work
074300090211     c                   move      'N'           WGID99
074400090211      * per controllare se almeno un record è stato importato sul VAB
074500090211     c                   clear                   Almeno_Uno        1
074600090213     c                   clear                   Almeno_Un_Due     1
074700090211     c                   eval      esito  = '0'
074800090211     C*
074900090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
075000090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
075100090603     c                   eval      Dettaglio_NAD_destinatario = *blank
075200090603     c                   eval      NAD_destinatario_in_testata = *blank
075300090603     c                   eval      NAD_mittente_in_testata = *blank
075400090603     c                   eval      Inizio_Dettaglio = *blank
075500090603     C*
075600090603     c                   clear                   tes_VABrmo
075700090603     c                   clear                   tes_VABnmo
075800090603     c                   clear                   tes_VABcmo
075900090603     C*
076000090603     c                   clear                   tes_VABrsd
076100090603     c                   clear                   tes_VABrd2
076200090603     c                   clear                   tes_VABind
076300090603     c                   clear                   tes_VABlod
076400090603     c                   clear                   tes_VABnzd
076500090603     C*
076600090603     c                   clear                   tes_Valuta
076700090603     C*
076800090211     C                   ENDSR
076900090210     C*----------------------------------------------------------------
077000090210      *? DECODIFICA DATI UNB
077100090210     C*----------------------------------------------------------------
077200090210     C     DATI_UNB      BEGSR
077300090210      *
077400090209      *  INIZIALIZZA  dati fondamentali messaggio
077500090209     C                   clear                   EDIDSPT
077600090209     C                   clear                   EDIDSPU
077700090209     C                   clear                   EDIDSPV
077800090209     C                   clear                   wnrDOC           35
077900090209     C                   move      'N'           PT_ok             1
078000090209      ** ---------------------------------------------------------------
078100090209      * quindi 1° cosa:
078200090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
078300090209      *     senza questo il messaggio NON è trascodificabile
078400090209     C                   MOVEL     unb0004       WCOD             35
078500090209     C                   MOVE      unb000720     WCOD
078600090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
078700090209     C                   Z-ADD     1             XX                3 0
078800090209     C     WCOD          LOOKUP    CPT(XX)                                32
078900090209      *
079000090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
079100090209     C                   If        *IN32 = *off
079200090209      * ******  Errore generale sul Messaggio
079300090209      * Msg di allerta Traduzione
079400090209     C                   EVAL      Indirizzi = CED_Bart
079500091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
079600090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
079700090209     C                             Bolle import in filiale - messaggio -
079800090209     C                             con UNB non presente in tabella PT controlla-
079900090209     C                             re EDI ricevuto su UPLOAD.'
080000090209      *
080100130321     c                   eval           manda_mail='S'
080200090209     c                   exsr      invio_mail
080300090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
080400090210     C                   eval      vinFLG = '2'
080500090210     C                   eval      se_errore   = 'S'
080600090210     c                   goto      end_UNB
080700090209      *
080800090209     C                   move      'N'           PT_ok             1
080900090209      *
081000090209      *             *------------------------------*
081100090209     C                   eLSe
081200090209      *             *------------------------------*
081300090209      * Se tutto OK su tab.PT:
081400090209     C                   move      'S'           PT_ok             1
081500090209      *
081600090209     C                   MOVEL     DPT(XX)       EDIDSPT
081700090209     C                   MOVEL     DPU(XX)       EDIDSPU
081800090209     C                   MOVEL     DPV(XX)       EDIDSPV
081900091016     c                   clear                   Mittente         35
082000091016     c                   clear                   Mitt_Qualifier    4
082100091016     c                   clear                   Id_Messaggio     14
082200090226     C                   eval      Mittente       = unb0004
082300090226     C                   eval      Mitt_qualifier = unb000720
082400090226     C                   eval      Id_Messaggio   = UNB0022
082500090209      *
082600090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
082700090209      *  ricevuti:
082800090209     c                   movel     §PVema        mail_ind         44
082900090209      *
083000090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
083100090209     c                   if        mail_ind <> *blank
083200090209     c                   exsr      imp_ADDR_mail
083300090209     c                   end
083400090209      *
083500090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
083600090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
083700090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
083800090209     C                   if        §PULUN = *blank
083900090209     C                   eval      §PULUN = '00'
084000090209     c                   end
084100090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
084200090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
084300090209      *
084400090209     C                   Z-ADD     §PTKSC        WKSC              7 0
084500090211     C                   MOVEL     §PTLNP        VABlnp_PT
084600090212     C                   MOVEL     §PTNRS        VABnrs_PT
084700090211     C                   MOVEL     §PTCTM        VABctm_PT
084800090209      *
084900090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
085000090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
085100090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
085200090209      *  i records trasmessi).
085300090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
085400090209      *
085500090209      *  In base al cod.trattamento merce reperisco tipo tratt.
085600090209     C                   CLEAR                   DS1B
085700090209     C                   Z-ADD     1             Y                 3 0
085800090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
085900090209     C     *IN32         IFEQ      '1'
086000090209     C                   MOVEL     DTM(Y)        DS1B
086100090209     C                   MOVEL     DS1B          WSVTRM           38
086200090209     C                   END
086300090209      *
086400090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
086500090209     C                   clear                   ACORAG
086600090209     C                   clear                   INDCAE
086700090209     C                   clear                   INDCAP
086800090209     C                   clear                   INDSTA
086900090209      * Decodifico partner
087000090209     C                   Z-ADD     1             KKUT
087100090209     C                   Z-ADD     KCI           KKCC
087200090209     C                   MOVE      §PTKSC        KKSC
087300090209     C     KACO          CHAIN     CNACO00F                           31
087400090209     C     KIND          CHAIN     CNIND00F                           31
087500090211      *
087600090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
087700090211      *   questa schiera è utilizzata anche per i clienti.
087800090211     C     §ptKSC        LOOKUP    KCL                                    39
087900090211     c                   If        *in39 = *off
088000090211     C                   add       1             WW
088100090211     C                   z-add     §ptKSC        KCL(WW)
088200090211     C                   Endif
088300090209      *
088400090209     C                   MOVEL     unb0017       WAA4              4 0
088500090209     C                   MOVE      WAA4          WAA2              2 0
088600090209     C                   MOVE      unb0017       WGG               2 0
088700090209     C                   MOVE      unb0017       WDTPRE            8 0
088800090209     C                   MOVE      unb0019       WHMPRE            4 0
088900090209     C                   MOVE      unb0017       WDTMSG            6 0
089000090209     C                   MOVE      WAA2          WDTMSG
089100090209     C                   MOVEL     WGG           WDTMSG
089200090210      * potremmo
089300090210      * Utilizzare l'identificativo della trasmissione come CMR
089400090209     C                   IF        unb0022 <> *blank
089500090209     C                   MOVEL     unb0022       WNRDOC
089600090211     C                   MOVEL     unb0022       WNUMFV
089700090211     C                   MOVEL     unb0022       WNRCMR
089800090209     c                   end
089900090209      *
090000090209     c                   Endif
090100090209      **
090200090210     c     end_UNB       endSR
090300090209      * ?================================================================== */
090400090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
090500090209      * ?================================================================== */
090600090227     C     Nuova_spediz  BEGSR
090700090227      **
090800090227      * Pulisco campi del file
090900090227     c                   z-add     vnrec         SAVNUM_RECord
091000090227     C                   CLEAR                   EDiVAB00
091100090227     C                   CLEAR                   EDiVAT00
091200090227     C                   CLEAR                   EDiVAD00
091300090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
091400090227     C                   MOVEL     'N'           WGID99            1
091500090227     C                   clear                   XY                5 0
091600090227     C                   clear                   §§                5 0
091700090227     C                   clear                   WS1496            5 0
091800090227     C                   clear                   WNSP             35
091900090227     C                   Z-ADD     *ZEROS        PVLB
092000090227     C                   MOVEA     *HIVAL        SGN
092100090227     C*  Azzero codice cliente - tariffa
092200090227     C                   Z-ADD     0             WTOCOC           16 0
092300090227     C                   Z-ADD     0             WTOPLC           16 2          ???
092400090227     C                   Z-ADD     0             WTOPNC           16 2          ???
092500090227     C*  Azzero codice cliente - tariffa
092600090227     C                   clear                   WCLKSC
092700090227     C                   clear                   WCLTAR
092800090227     C                   clear                   WCLLNP
092900090227     C                   clear                   WCLNRS
093000090227     C                   clear                   WCLCTM
093100090227     C                   clear                   WCLBS1
093200090227     C                   clear                   WCLfnsp                        *blk/S
093300090227     C                   clear                   WNOT1            35
093400090227     C                   clear                   WNOT2            35
093500090227     C*  Campi di work
093600090227     c                   clear                   wVabTIC
093700090227     c                   clear                   wri_vabtic        1
093800090227      *
093900090227     C* pulisce campi di work
094000090227     c                   clear                   watrde           35
094100090227     c                   clear                   wattel           25
094200090227     C                   clear                   WRMN              3
094300090227      *
094400090227      * Un errore nel dettaglio
094500090227     C                   clear                   Err_in_detta      1
094600090227     c                   clear                   errori_in_Wrt     1
094700090227     c                   clear                   se_trovato_NAD    1
094800090227      **
094900090227     c                   endSR
095000090227      * ?================================================================== */
095100090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
095200090227      * ?================================================================== */
095300090227     C     DATI_UNH      BEGSR
095400090227      *
095500090227      *  Prepara una nuova spedizione
095600090227      * ?                         --------------
095700090227     c                   exsr      Nuova_spediz
095800090227      * ?                         --------------
095900090211      *
096000090211      ** Riferimento del cliente
096100090211     C                   MOVEL     unh0062       WPRGSP           35
096200090211     C                   MOVEL     unh0062       WNSP             35
096300090211     C                   MOVEL     unh0062       WNUM             35
096400090211      * ?                                       --------
096500090211     C                   MOVEL     unh0062       VABRMA
096600090211      * ?                                       --------
096700090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
096800090211     C                   SUB       1             WLUNGH
096900090211     C                   EXSR      TSTNUM
097000090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
097100090211      * ?                                       --------
097200090211     C                   MOVEL     WNUMOK        VABRMN
097300090211      * ?                                       --------
097400090211     C                   END
097500090210      **
097600090303     C                   MOVEL     VABlnp_PT     VABLNP
097700090303     C                   MOVEL     VABnrs_PT     VABNRS
097800090303     C                   MOVEL     VABctm_PT     VABCTM
097900090303      **
098000090303      **
098100090210     c     end_UNH       endSR
098200090210      * ?================================================================== */
098300090601     C*? Imposta la VALUTA di eventuali C.O.D.
098400090210      * ?================================================================== */
098500090601     C     DATI_CUX      BEGSR
098600090210     C*
098700090603     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
098800090603     c                   if        cux6345 <> *blank
098900090603     C                   MOVEL     cux6345       Tes_Valuta
099000090603     c                   end
099100090601      **
099200090601     c     end_CUX       endSR
099300090601      * ?================================================================== */
099400090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
099500090601      * ?================================================================== */
099600090601     C     DATI_DTM      BEGSR
099700090601     C*
099800090209     C*  Se data documento  memorizzo numero e data x CMR
099900090603     C     dtm2005       IFEQ      '9'
100000090209     C                   MOVEL     dtm2380       WDATA            35            Data
100100090209     C                   MOVEL     dtm2379       WFORM             3            Formato
100200090209     C                   EXSR      CNVDAT
100300090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
100400090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
100500090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
100600090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
100700090209     C     WERRO         IFEQ      'S'
100800090210     C                   eval      vinMSG = 'DTM data errata'
100900090213     c                   exsr      Error_DET
101000090210     c                   goto      end_DTM
101100090209     C                   END
101200090209     C                   END
101300090209      **
101400090209     C*  Se data consegna richiesta imposto già sui campi del VAB
101500090603     C     dtm2005       IFEQ      'X'
101600090209     C                   MOVEL     dtm2380       WDATA            35            Data
101700090209     C                   MOVEL     dtm2379       WFORM             3            Formato
101800090209     C                   EXSR      CNVDAT
101900090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
102000090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
102100090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
102200090209     C     WERRO         IFEQ      'S'
102300090210     C                   eval      vinMSG = 'DTM data errata'
102400090213     c                   exsr      Error_DET
102500090210     c                   goto      end_DTM
102600090209     C                   END
102700090209     C                   END
102800090211      **
102900090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
103000090603     C     dtm2005       IFEQ      '10'
103100090211     C                   MOVEL     dtm2380       WDATA            35            Data
103200090211     C                   MOVEL     dtm2379       WFORM             3            Formato
103300090211     C                   EXSR      CNVDAT
103400090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
103500090211     C     WERRO         IFEQ      'S'
103600090211     C                   eval      vinMSG = 'DTM data errata'
103700090213     c                   exsr      Error_DET
103800090211     c                   goto      end_DTM
103900090211     C                   END
104000090211     C                   END
104100090209      **
104200090210     c     end_DTM       endSR
104300090209      * ?================================================================== */
104400090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
104500090209      * ?================================================================== */
104600090209     C     DATI_TOD      BEGSR
104700090209      *
104800090210      *  Trascodifico tipo trasporto
104900090210     c                   clear                   TOD_char3         3
105000090210      *
105100090210      *  imposta il default dalla tabella PT se c'è
105200090210     C     §puPFA        ifNE      *blank
105300090210     c                   SELECT
105400090210      *  Porto Franco
105500090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
105600090210     C                   movel     '4'           VABCBO                         + C/Ass
105700090210     c                   WHEN      §puPFA = 'F'
105800090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
105900090210      *  Porto Assegnato
106000090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
106100090210     C                   movel     '6'           VABCBO                         + C/Ass
106200090210     C                   WHEN      §puPFA = 'A'
106300090210     C                   movel     '2'           VABCBO                         Senza C/Ass
106400090210     C                   ENDSL
106500090210     c                   end
106600090210      *
106700090210      *   Imposta o il codice o il metodo
106800090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
106900090210     c                   if        tod4053  <> *blank
107000090210     c                   movel(p)  tod4053       TOD_char3
107100090210     c                   else
107200090210     c                   if        tod4215  <> *blank
107300090210     c                   movel(p)  tod4215       TOD_char3
107400090210     c                   end
107500090210     c                   end
107600110328      ****************
107700110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
107800110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
107900110328      ****************
108000110328     c                   eval      Trovata_Tab_TB ='N'
108100110328      *
108200110328     c                   if              TOD_char3 <> *blank
108300110328     c                   movel(p)  TOD_char3     Key_Generica35
108400110328     c                   move      §puTB         Key_Generica35
108500110328     C                   eval      Y = 1
108600110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
108700110328     c   32              eval      Trovata_Tab_TB ='S'
108800110328      *
108900110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
109000110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
109100110328      *
109200110328     c                   if          Trovata_Tab_TB ='N'
109300110328     c                                 and
109400110328     C                               tod4215 <> *blank
109500110328     c                   movel(p)  tod4215       TOD_char3
109600110328     c                   movel(p)  TOD_char3     Key_Generica35
109700110328     c                   move      §puTB         Key_Generica35
109800110328     C                   eval      Y = 1
109900110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
110000110328     c   32              eval      Trovata_Tab_TB ='S'
110100110328     c                   end
110200110328     c                   end
110300110328      *
110400110328      ****************
110500110328      ******* Se vi sono eccezioni sul cliente imposta quelle
110600110328      ****************
110700110328     c                   If          Trovata_Tab_TB ='S'
110800110328      *
110900110328     c                   SELECT
111000110328     c                   WHEN      Decod_Porto(Y) = 'F'
111100110328     c                               and  VABcas > *zeros
111200110328     C                   movel     '4'           VABCBO                         + C/Ass
111300110328     c                   WHEN      Decod_Porto(Y) = 'F'
111400110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
111500110328      *  Porto Assegnato
111600110328     c                   WHEN      VABCAS > *zeros
111700110328     C                   movel     '6'           VABCBO                         + C/Ass
111800110328     C                   OTHER
111900110328     C                   movel     '2'           VABCBO                         Senza C/Ass
112000110328     C                   ENDSL
112100110328      *
112200110328      ****************  altrimenti
112300110328      ******* Se non vi sono eccezioni per il cliente
112400110328      *   Rileva a questo punto lo Standard per tutti
112500110328      ****************
112600110328     c                   elseIf      Trovata_Tab_TB ='N'
112700090210      **
112800090210     c                   if              TOD_char3 <> *blank
112900090210     c                   movel(p)  TOD_char3     fld35a           35
113000090213     C                   movel     UNB_Mittente  etbUNB
113100090213     C                   movel(p)  'TOD'         etbSGM
113200090213     C                   movel(p)  TOD_char3     etbVALSGM
113300090213     c     K_TAB1L       chain     edstbl01l
113400090210      *
113500090210      * prova quindi senza l'UNB specifico del cliente
113600090213     c                   if        not %Found(edstbl01l)
113700090213     C                   clear                   etbUNB
113800090213     c     K_TAB1L       chain     edstbl01l
113900090210     c                   end
114000090210      *
114100090213     c                   if        %Found(edstbl01l)
114200090210     c                   SELECT
114300090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
114400090210     C                   movel     '4'           VABCBO                         + C/Ass
114500090213     c                   WHEN      ETBDATI = 'F'
114600090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
114700090210      *  Porto Assegnato
114800090210     c                   WHEN      VABCAS > *zeros
114900090210     C                   movel     '6'           VABCBO                         + C/Ass
115000090210     C                   OTHER
115100090210     C                   movel     '2'           VABCBO                         Senza C/Ass
115200090210     C                   ENDSL
115300090210      *
115400090210     c                   end
115500090210     c                   endIF
115600090209      **
115700110328     c                   endIF
115800110328      **
115900090210      **  Il tipo bolla non è presente
116000090210     c                   if        VABCBO = *blank
116100090210     C                   eval      vinMSG = 'TOD non rilevato'
116200130321     c**********         exsr      Error_DET
116300130321     c**********         goto      end_TOD
116400090210     c                   end
116500090210      **
116600090210     c     end_TOD       endSR
116700090210      * ?================================================================== */
116800090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
116900090210      * ?================================================================== */
117000090210     C     DATI_RFF      BEGSR
117100090210      *
117200090210     C                   clear                   WAGE             35
117300110224     C                   clear                   WFF              35
117400110224     C                   clear                   WCU              35
117500090210      *
117600090210      *  Trascodifico riferimento spedizione
117700091019     c                   if        RFF1153 = Riferimento_Spedizione  and
117800091019     c                             RFF1154 <> *BLANKS
117900090210     C                   movel(p)  RFF1154       WAGE             35
118000090210     c                   end
118100090210      **
118200090210     C                   IF         WAGE <> *BLANKS
118300090210     C                   movel     WAGE          WNSP             35
118400090210     C                   movel     WAGE          WNUM             35
118500090210     C     ' '           scan      WAGE          WLUNGH            3 0
118600090210     C                   sub       1             WLUNGH
118700090210     C                   EXSR      TSTNUM
118800090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
118900090210     C                   movel     WAGE          VABRMA
119000091019     C                   IF         WRMN = Riferimento_Spedizione  or
119100091019     C                              WRMN = *blanks
119200090210     C                   movel     WNUMOK        VABRMN
119300091019     C                   eval       WRMN = Riferimento_Spedizione
119400090210     C                   ENDIF
119500090210     C                   ENDIF
119600090210     C                   ENDIF
119700090210      *
119800090210      * Codice Cliente di riferimento
119900091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
120000090210     C                   movel     RFF1154       WFF
120100090210     c                   END
120200090211      *
120300090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
120400090211     C     WFF           IFNE      *BLANKS
120500090211     C                   eval      XX = 1
120600090211     C                   movel     §PTKSC        WCCL             35
120700090211     C                   movel     WFF           WCOM28           28
120800090211     C                   move      WCOM28        WCCL
120900090211     C     WCCL          lookup    CCL(XX)                                34
121000090211     C     *IN34         IFEQ      '1'
121100090211     C                   movel     DCL(XX)       EDIDSCL
121200090211     C                   z-add     §CLKSC        WCLKSC            7 0
121300090211     C                   movel     §CLTAR        WCLTAR            3
121400090211     C                   z-add     §CLLNP        WCLLNP            3 0
121500090211     C                   z-add     §CLNRS        WCLNRS            2 0
121600090211     C                   movel     §CLCTM        WCLCTM            2
121700090211     C                   movel     §CLBS1        WCLBS1            1
121800090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
121900090211      * imposta la LNP   x il dettaglio specifco
122000090211     c                   eval      VABlnp = §CLlnp
122100090211      * imposta la Serie x il dettaglio specifco
122200090211     c                   eval      VABnrs = §CLnrs
122300090211      *
122400090211      *  In base al cod.trattamento merce reperisco tipo tratt.
122500090211      *    per un dettaglio specifico.
122600090211     C                   CLEAR                   DS1B
122700090211     C                   Z-ADD     1             Y                 3 0
122800090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
122900090211     C     *IN32         IFEQ      '1'
123000090211     C                   MOVEL     DTM(Y)        DS1B
123100090211     C                   END
123200090211      *
123300090211      * Se il cliente non era sulla schiera lo aggiunge
123400090211     C     §CLKSC        lookup    KCL                                    39
123500090211     C                   if        *in39 = *off
123600090211     C                   add       1             WW
123700090211     C                   z-add     §CLKSC        KCL(WW)
123800090211     c                   endif
123900090211      *
124000090211     C                   ENDIF
124100090211     C                   ENDIF
124200090211     C*----------------------------------------------------------------
124300090210      *
124400090210     C                   if        RFF1153 = 'CU ' and RFF1154 <> *blanks
124500090210     C                   movel     RFF1154       WCU
124600090210     C                   END
124700090210      *
124800090210      * Numero Ordine di vendita
124900090210     C     WCU           IFNE      *BLANKS
125000090210     C                   movel     WCU           WNUM             35
125100090210     C     ' '           scan      WCU           WLUNGH
125200090210     C                   sub       1             WLUNGH
125300090210     C                   EXSR      TSTNUM
125400090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
125500090210     C                   movel     WNUMOK        VABRMN
125600090210     C                   movel     'CU '         WRMN
125700090210     C                   Endif
125800090210     C                   ENDIF
125900090210      *
126000090210      * NOn c'è l'identificativo bolla del cliente
126100091019     c                   if        (RFF1153 = Riferimento_Spedizione and
126200091019     c                              RFF1154 = *BLANKS)
126300091019     c                             or
126400090211     c                             (RFF1153 = 'CU' and RFF1154 = *BLANKS)
126500090210     C                   eval      vinMSG = 'RFF riferimento assente'
126600090213     c                   exsr      Error_DET
126700090210     c                   goto      end_RFF
126800090210     C                   ENDIF
126900090210      *
127000090210     c     end_RFF       endSR
127100090209      * ?================================================================== */
127200090213     C*?  C.O.D. Monetary Amount
127300090209      * ?================================================================== */
127400090213     C     DATI_MOA      BEGSR
127500090210      *
127600090216     C* Controllo se campo importo numerico
127700090216      * con 3 decimali
127800090216     C     moa5004       IFNE      *zeros
127900090216      *
128000090216     C     moa5025       IFEQ      '157'
128100090216     C     moa5004       div       1000          VABIAS                         IMPORTO
128200090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
128300090216     C                   END
128400090216      *
128500090216     C     moa5025       IFEQ      '77 '                                        Valore
128600090216     C     moa5004       div       1000          VABVMD                         MERCE
128700090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
128800090216     C                   END
128900090216      *
129000090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
129100090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
129200120904     C     moa5004       div       100000        VABCAS
129300090216     C                   MOVEL     moa6345       VABVCA
129400090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
129500090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
129600090216     C     §PUTPC        IFEQ      'P'
129700090216     C     moa5004       ANDGT     *ZEROS
129800090216     C                   MOVEL     'OM'          VABTIC
129900090216     C                   MOVEL     'OM'          WVABTIC
130000090216     C                   ENDIF
130100090216     C                   END
130200090216     C                   END
130300090216      *
130400090216     C                   END                                                    Imp.non numer.
130500090213      *
130600090213     c     end_MOA       endSR
130700090213      * ?================================================================== */
130800090213     C*?  FREE TEXT
130900090213      * ?================================================================== */
131000090213     C     DATI_FTX      BEGSR
131100110404      *
131200110404      *
131300110404      * ?Se non è un testo per il Pagamento
131400110404     C     ftx4451       IFne      'PAI'                                        -- 01 --
131500110404      *
131600090216      * ? solo se si tratta di note SIN ICN o PAI non altro
131700090216     C                   DO        4             X
131800090216     C     D05(X)        IFNE      *BLANKS
131900090216     C     WNOT1         IFEQ      *BLANKS
132000090216     C                   MOVEL     D05(X)        WNOT1
132100090216     C                   MOVE      D05(X)        WNOT2
132200090216     C                   ELSE
132300090216     C     WNOT2         IFEQ      *BLANKS
132400090216     C                   MOVEL     D05(X)        WNOT2
132500090216     C                   END
132600110404     C                   ENDif
132700110404     C                   ENDif
132800110404     C                   ENDdo
132900110404      *
133000110404     C                   ENDif
133100090216     C*------>>
133200090216     C*  Decodifico tipo pagamento C/Assegno
133300090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
133400090216      *      (Problema di pulizia in presenza di + righe di testo)
133500090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
133600090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
133700090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
133800090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
133900090216     C                   if        wri_vabtic = *blank
134000090216     C                   MOVEL     *BLANKS       VABTIC
134100090216     c                   end
134200090216      **
134300090216     c                   clear                   trovato_TPI       1
134400090216      *
134500090216      * ?Se c'è il tipo Incasso
134600090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
134700090216      *
134800090216     C                   DO        4             X                              -- 02 --
134900090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
135000130311      **
135100130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
135200130311      **  per gestire Partner come AZKAR che ha codice "AAA"
135300130311     C*******            MOVEL     D05(X)        WCTC              2
135400130311     C                   MOVEL     D05(X)        WCTC             10
135500090216     C                   Z-ADD     1             Y
135600090216     c                   movel(p)  Wctc          fld35a           35
135700090216     c                   move      §puTC         fld35a
135800090216     C     fld35a        LOOKUP    CTC35(Y)                               33
135900090216     C   33              MOVEL     TPI(Y)        VABTIC
136000090216     c   33              move      'S'           trovato_TPI
136100090216     C                   END                                                    -- 03 --
136200090216     C                   ENDdo                                                  -- 02 --
136300090216      *
136400090216     C                   ELSE                                                   -- 01 --
136500090216      *
136600090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
136700090216      *    Deve essere impostato comunque da testata
136800090216     c                   if        vabtic = *blank
136900090216     C                   MOVEL     wVabTIC       VABTIC
137000090216     c                   end
137100090216      *
137200090216     C                   END                                                    -- 01 --
137300090216      *
137400090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
137500090216      *    essere presenti altri record Flat con altre informazioni
137600090216      *    che potrebbero abblancare il Tipo incasso precedentemente
137700090216      *    decodificato
137800090216     c                   if        Trovato_TPI = 'S' and
137900090216     c                                 wri_vabtic = *blank
138000090216     c                   eval      wri_vabtic = 'S'
138100090216     c                   end
138200090213      *
138300090213     c     end_FTX       endSR
138400090213      * ?================================================================== */
138500090213     C*?  Anagrafica del mittente e del destinatario
138600090213      * ?================================================================== */
138700091016     C     DATI_NAD      BEGSR
138800090213      *
138900090210      * Dati destinatario
139000090210     C                   IF        nad3035 = 'CN' or
139100090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
139200090227      *
139300090227      * Se trovato un indirizzo di destinatario
139400090227     c                   eval       se_trovato_NAD ='S'
139500090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
139600090210      *
139700090210     C* Reperisco nazione corrispondente destinatario
139800090210     C                   Z-ADD     1             YY                3 0
139900090210     c                   eval      *in32 = *off
140000090210     C                   if        nad3207 <> *BLANKS
140100090210     C     nad3207       LOOKUP    ISO(YY)                                32
140200090210     C                   endif
140300090210      *
140400090210     C* Imposto dati destinatario
140500090211     C                   MOVEL     nad31241      VABRSD
140600090211     C                   if        nad31242 <> *LOVAL
140700090211     C                   MOVEL     nad31242      VABRD2
140800090210     C                   endif
140900090211      *
141000091019????  * DAL PARTY NAME
141100091019???? C* Imposto dati destinatario
141200091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
141300091019???? C                   MOVEL     nad30361      VABRSD
141400091019???? C                   if        nad30362 <> *LOVAL
141500091019???? C                   MOVEL     nad30362      VABRD2
141600091019???? C                   endif
141700091019???? C                   end
141800090211      *
141900090210     C                   MOVEL     nad30421      VABIND
142000090210     C                   MOVEL     nad3164       VABLOD
142100090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
142200090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
142300090210      *
142400090210     C* Verifico se c'è dello spazio per nella località
142500090210     C     '    '        SCAN      VABLOD        WI                4 0
142600090210      *
142700090210     C                   MOVEA     VABLOD        LOD
142800090210     C                   ADD       1             WI
142900090210     C                   MOVEA     NAD30422      LOD(WI)
143000090210     C                   MOVEA     LOD           VABLOD
143100090210     C                   ENDif
143200090210      *
143300090210     C                   If        *in32 = *on
143400090210     C                   MOVEL     C15(YY)       VABNZD
143500090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
143600090210     c                   Else
143700090210     C                   MOVEL     *BLANKS       VABNZD
143800090210     C                   Z-ADD     0             WFLCPF
143900090210     c                   Endif
144000090210      * Converto CAP
144100090210     C                   EXSR      CNVCAP
144200090603     C*
144300090603     C* Se in testata
144400090603     c                   if        Inizio_dettaglio = *blank
144500090603     c                   eval       tes_VABrsd = VABrsd
144600090603     c                   eval       tes_VABrd2 = VABrd2
144700090603     c                   eval       tes_VABind = VABind
144800090603     c                   eval       tes_VABlod = VABlod
144900090603     c                   eval       tes_VABnzd = VABnzd
145000090603     c                   eval      NAD_destinatario_in_testata = 'S'
145100090603     c                   end
145200090603     C*
145300090210     C                   ENDIF
145400090210      *
145500090210      * Dati mittente
145600091016     C                   IF        nad3035 = 'CZ'
145700090210     C                   movel     nad30361      VABRMO
145800090210      *
145900090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
146000090210     C                   movel     §PTNAZ        VABNMO
146100090210      *
146200090210     C                   IF        nad3207 <> *blanks
146300090210     C                   eval      YY = 1
146400090210     C     nad3207       LOOKUP    ISO(YY)                                32
146500090210     C                   if         *in32 = *on
146600090210     C                   MOVEL     C15(YY)       VABNMO
146700090210     C                   endif
146800090210     C                   ENDIF
146900090210      *
147000090210      * Normalizzo CAP mittente originale
147100090210     C                   clear                   T
147200090210     C                   clear                   S
147300090210     C                   MOVEA     *BLANKS       CAPM
147400090210     C                   MOVEA     nad3251       CAP9
147500090210     C                   DO        9             T
147600090210     C     CAP9(T)       IFNE      *BLANKS
147700090210     C                   ADD       1             S
147800090210     C                   MOVE      CAP9(T)       CAPM(S)
147900090210     C                   ENDIF
148000090210     C                   ENDDO
148100090210     C*
148200090210      *     Controllo validità CAP
148300090210     C                   MOVEA     CAPM          VABCMO
148400090210      *
148500090210     C                   CLEAR                   TISI95DS
148600090210     C                   MOVEL     VABCMO        I95CAP
148700090210     C                   MOVEL     VABNMO        I95NAR
148800090210     C                   MOVEL     WOGGI         I95DAT
148900090210     C                   MOVEL     '3'           I95TCN
149000090210     C                   CALL      'TISI95R'
149100090210     C                   PARM                    TISI95DS
149200090210      *     Errore
149300090210     C     O95ERR        IFNE      *BLANKS
149400090210     C                   MOVEL     INDCAE        VABCMO
149500090210     C                   MOVEL     §PTNAZ        VABNMO
149600090210     C                   END
149700090603      *
149800090603     C* Se in testata
149900090603     c                   if        Inizio_dettaglio = *blank
150000090603     c                   eval      tes_VABrmo = VABrmo
150100090603     c                   eval      tes_VABnmo = VABnmo
150200090603     c                   eval      tes_VABcmo = VABcmo
150300090603     c                   eval      NAD_mittente_in_testata = 'S'
150400090603     c                   end
150500090210      *
150600090210     C                   END
150700090210      *
150800090210      * NOn c'è l'identificativo bolla del cliente
150900090210     C                   if          vabRSD = *blank or
151000090210     C                               vabIND = *blank or
151100090210     C                               vabLOD = *blank
151200090211     C                   IF        nad3035 = 'CN'
151300090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
151400130321     c**********         exsr      Error_DET
151500130321     c**********         goto      end_NAD
151600090210     C                   ENDIF
151700090211     C                   ENDIF
151800090210      *
151900090210     c     end_NAD       endSR
152000090210      * ?================================================================== */
152100090210     C*?  Contatto a cui far riferimento
152200090210      * ?================================================================== */
152300090210     C     DATI_CTA      BEGSR
152400090210      *
152500090210      *  memorizza il riferimento di consegna x il destinatario
152600090210      *  memorizza il riferimento di consegna nr.telefonico
152700090210     C     cta3412       ifne      *BLANKS
152800090210     c                   movel     cta3412       watrde
152900090210     c                   end
153000090210      *
153100090210     c                   endSR
153200090210      * ?================================================================== */
153300090210     C*?  Contatto telefonico  a cui far riferimento
153400090210      * ?================================================================== */
153500090210     C     DATI_COM      BEGSR
153600090210      *
153700090210     C     com3148       ifne      *BLANKS
153800090210     c                   movel     com3148       wattel
153900090210     c                   end
154000090210      *
154100090209     C                   ENDSR
154200090210      * ?================================================================== */
154300090210     C*?  Dati di dettaglio della spedizione e dei colli
154400090210      * ?================================================================== */
154500090210     C     DATI_GID      BEGSR
154600090210      *
154700090210     C* Se ho totali per spedizione metto in campi VAB
154800090210     C     gid1496       IFEQ      99999
154900090210     C     gid1496       orEQ      9999
155000090210     C                   MOVEL     'S'           WGID99
155100090210     C                   z-add     gid722420     VABNCL
155200090210     C                   ELSE
155300090210     C     WGID99        IFEQ      'N'
155400090210     C                   z-add     gid722420     WTNCL             5 0
155500090210     C                   ADD       WTNCL         VABNCL
155600090210     C                   END
155700090210     C                   END
155800090212      *
155900090210     C                   ADD       VABNCL        WTOCOC           16 0
156000090210      *
156100090210     c     end_GID       endSR
156200090210      * ?================================================================== */
156300090210     C*?  Dati di dettaglio misure spedizione e colli
156400090210      * ?================================================================== */
156500090210     C     DATI_MEA      BEGSR
156600090210      *
156700090210     C*  Peso
156800090210     C     mea6311       IFEQ      'WT '
156900090210     C     mea6411       ANDEQ     'KGM'
157000090210      *
157100090210     C*  Controllo se ho valore totale o parziale
157200090210      *   sempre il Lordo
157300091019???? C     mea6313       IFEQ      'G  '
157400090210     C     WS1496        IFEQ      99999
157500090210     C     WS1496        orEQ      9999
157600091019???? C                   move      mea6314       mea6314_2        18 2          Peso
157700091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
157800090210     C                   ELSE
157900091019???? C     WGID99        IFEQ      'N'
158000091019???? C                   move      mea6314       mea6314_2                      Peso
158100091019???? C                   ADD       mea6314_2     VABPKB                         Peso
158200090210     C                   END
158300090210     C                   END
158400090210     C                   END
158500090210      *
158600091019???? C     mea6313       IFEQ      'G  '
158700091019???? C                   move      mea6314       mea6314_2                      Peso
158800091019???? C                   ADD       mea6314_2     WTOPLC
158900090210     C                   END
159000091019???? C     mea6313       IFEQ      'N  '
159100091019???? C                   move      mea6314       mea6314_2                      Peso
159200091019???? C                   ADD       mea6314_2     WTOPNC
159300090210     C                   END
159400090210     C                   END
159500090210      ***
159600090210     C*  Peso in Grammi  se abilitato a utilizzarlo
159700090210      ***
159800090210     C     §puGR         IFeq      'S'
159900090210     C     mea6311       IFEQ      'WT '
160000090210     C     mea6411       ANDEQ     '163'
160100090210      *
160200090210     C*  Controllo se ho valore totale o parziale
160300091019???? C     mea6313       IFEQ      'G  '
160400091019???? C     WS1496        IFEQ      99999
160500091019???? C     WS1496        orEQ      9999
160600091019???? c                   move      mea6314       mea6314_2
160700091019???? C     mea6314_2     div       1000          VABPKB                         Peso
160800090210     C                   ELSE
160900091019???? C     WGID99        IFEQ      'N'
161000091019???? c                   move      mea6314       mea6314_2
161100091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
161200091019???? C                   add       Peso_inKG     VABPKB                         Peso
161300090210     C                   END
161400090210     C                   END
161500090210     C                   END
161600090210      *
161700091019???? C     mea6313       IFEQ      'G  '
161800091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
161900091019???? C                   ADD       Peso_inKG     WTOPLC
162000090210     C                   END
162100091019???? C     mea6313       IFEQ      'N  '
162200091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
162300091019???? C                   ADD       Peso_inKG     WTOPNC
162400090210     C                   END
162500090210     C                   END
162600090210     C                   END
162700090210      *  Volume
162800091019???? C     mea6311       IFEQ      'VOL'
162900091019???? C     mea6411       ANDEQ     'MTQ'
163000090210      *  Controllo se ho valore totale o parziale
163100090210     C     WS1496        IFEQ      99999
163200090210     C     WS1496        orEQ      9999
163300091019???? C                   move      mea6314       mea6314_2        18 2          Volume
163400091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
163500091019???? C                   Z-ADD     mea6314_2     PVLB
163600090210     C                   ELSE
163700091019     C     WGID99        IFEQ      'N'
163800091019???? C                   move      mea6314       mea6314_2        18 2          Volume
163900091019???? C                   ADD       mea6314_2     VABVLB                         Volume
164000091019???? C                   ADD       mea6314_2     PVLB
164100090210     C                   END
164200090210     C                   END
164300090210     C                   END
164400090210      *
164500090210     C* Ricerco CAP
164600090210     C     WCAP          IFNE      *BLANKS
164700090210     C     mea6311       IFEQ      'WT '
164800090210      *
164900090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
165000090210     C                   CLEAR                   FNLV55DS
165100090210     C                   MOVEL     'P'           D55TPT
165200090210     C                   MOVEL     WOGGI         D55DRF
165300090210     C                   MOVEL     VABLNP        D55LNP
165400090210     C                   MOVEL     VABLNP        D55LIN
165500090210     C                   CALL      'FNLV55R'
165600090210     C                   PARM                    FNLV55DS
165700090210     C                   MOVE      D55TFP        COMTFP            3 0
165800090210      *
165900090210     C*  PASSO IL TERMINAL PARTENZA
166000090210     C                   CLEAR                   TISI95DS
166100090210     C                   Z-ADD     COMTFP        I95TFP
166200090210     C                   MOVEL     VABTSP        I95TSP
166300090210     C                   MOVEL     'S'           I95FRE
166400090210     C                   MOVEL     '3'           I95TCN
166500090210     C                   MOVEL     WCAP          I95CAP
166600090210     C                   MOVEL     VABNZD        I95NAR
166700090210     C                   MOVEL     VABLOD        I95LOC
166800090210     C                   MOVEL     VABIND        I95IND
166900090210      *
167000090210     C* Se gestita seconda linea di arrivo passo peso - volume
167100090210     C* e numero colli effettivo
167200090210     C     §PULNA        IFEQ      'S'
167300090210     C                   Z-ADD     VABPKB        I95LKG
167400090210     C                   Z-ADD     VABVLB        I95LMC
167500090210     C                   ELSE
167600090210     C* Altrmenti passo 1 Kg e 0,001
167700090210     C***                  Z-ADD1         I95LKG
167800090210     C***                  Z-ADD0,001     I95LMC
167900090210     C                   END
168000090210     C*
168100090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
168200090210     C                   SELECT
168300090210     C     VABCBO        WHENEQ    '1 '
168400090210     C                   MOVEL     *BLANKS       I95FCA
168500090210     C                   MOVEL     'F'           I95TPO
168600090210     C     VABCBO        WHENEQ    '2 '
168700090210     C                   MOVEL     *BLANKS       I95FCA
168800090210     C                   MOVEL     'A'           I95TPO
168900090210     C     VABCBO        WHENEQ    '4 '
169000090210     C                   MOVEL     'S'           I95FCA
169100090210     C                   MOVEL     'F'           I95TPO
169200090210     C     VABCBO        WHENEQ    '6 '
169300090210     C                   MOVEL     'S'           I95FCA
169400090210     C                   MOVEL     'A'           I95TPO
169500090210     C                   OTHER
169600090210     C                   MOVEL     'F'           I95TPO
169700090210     C     VABCAS        IFGT      0
169800090210     C                   MOVEL     'S'           I95FCA
169900090210     C                   ELSE
170000090210     C                   MOVEL     *BLANKS       I95FCA
170100090210     C                   END
170200090210     C                   ENDSL
170300090210     C*
170400090210     C                   CALL      'TISI95R'
170500090210     C                   PARM                    TISI95DS
170600090210     C* Reperisco i dati da CAP
170700090210     C                   MOVEL     O95PRV        VABPRD
170800090210     C                   MOVEL     O95LNA        VABLNA
170900090210     C                   MOVEL     O95ZNC        VABZNC
171000090210     C                   MOVEL     WCAP          VABCAD
171100090210     C*
171200090210     C     O95ERR        IFNE      *BLANKS
171300090210     C     O95FIT        ANDEQ     'S'
171400090210     C*  Cap non trovato
171500090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
171600130321     c*********          exsr      Error_DET
171700130321     C*********          goto      end_MEA
171800090210     C                   END
171900090210     C                   END
172000090210      *
172100090210     C                   ELSE
172200090210      *
172300090210     C                   CLEAR                   VABPRD
172400090210     C                   CLEAR                   VABLNA
172500090210     C                   CLEAR                   VABZNC
172600090210     C                   CLEAR                   VABCAD
172700090210     C*  Cap non trovato
172800090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
172900130321     c**********         exsr      Error_DET
173000130321     C**********         goto      end_MEA
173100090210     C                   END
173200090210      *
173300090210     c     end_MEA       endSR
173400090210      * ?================================================================== */
173500090210     C*?  Dati di dettaglio Codice a barre Segnacolli
173600090210      * ?================================================================== */
173700090210     C     DATI_PCI      BEGSR
173800090210      *
173900090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
174000090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
174100090210     C     §1BF12        IFEQ      'E'
174200090210     C                   EXSR      SEGNEE
174300090210     C                   ELSE
174400090210      *
174500090210      *  La prima volta controllo congruità numero segnacollo
174600090210     C     §PUBS1        IFNE      *BLANKS
174700090210     C     VABnrs        ANDNE     0
174800090210      *  Se il codice trattamento merce prevede che segnacollino i
174900090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
175000090210     C     §1BFG7        IFEQ      'S'
175100090210     C     §1BFG1        OREQ      'S'
175200090210      *  calcolo segnacollo
175300090210     C                   EXSR      SGNELA
175400090210     C                   END
175500090210     C                   END
175600090210      *
175700090210     C                   END
175800090210      *
175900090210     c     end_PCI       endSR
176000090211     C*----------------------------------------------------------------
176100090211      *? dati finali del dettaglio    UNT
176200090211     C*----------------------------------------------------------------
176300090211     C     DATI_UNT      BEGSR
176400090211      *
176500090213     c                   eval      NUM_RECord = vnREC
176600090213     c                   z-add     vnrec         last_RECord
176700090211      **
176800090211     c                   endSR
176900090211     C*----------------------------------------------------------------
177000090211      *? dati finali     UNT
177100090211     C*----------------------------------------------------------------
177200090211     C     DATI_UNZ      BEGSR
177300090211      *
177400090211      **
177500090211     c                   endSR
177600090210      *----------------------------------------------------------------
177700090210      *? Input segnacolli su schiera di 20 elementi
177800090210      *----------------------------------------------------------------
177900090210     C     INP_Segnacol  BEGSR
178000090210      *
178100090210     c                   clear                   quanti_PCI        3 0
178200090210     c                   if        §puEL1 = 0
178300090210     c                   z-add     10            quanti_PCI
178400090210     c                   else
178500090210     c                   z-add     §puEL1        quanti_PCI
178600090210     c                   end
178700090210      *
178800090210      *  Pulisce la schiera dei segnacolli da elaborare
178900090210     c                   clear                   x30
179000090210     c                   z-add     0             Nr_x30            3 0
179100090210      *
179200090210      *   riporto tutto sulla schiera generica X30
179300090210      *
179400090210     c                   do        10            limite            3 0
179500090210      * Limita quanti elementi passati in ogni PCI
179600090210      *   devono essere considerati
179700090210     c                   if        limite > quanti_PCI
179800090210     c                   Leave
179900090210     c                   end
180000090210      *
180100090210      *  Carica la schiera generale di tutti i segnacolli
180200090210     c                   if        D30_1(limite) <> *blank
180300090210     c                   add       1             Nr_x30
180400090210     c                   eval      X30(Nr_x30) = D30_1(limite)
180500090210     c                   end
180600090210     c                   enddo
180700090210      *
180800090210     C                   ENDSR
180900090210      *----------------------------------------------------------------
181000090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
181100090210      *----------------------------------------------------------------
181200090210     C     SEGNEE        BEGSR
181300090210      *
181400090210     c                   exsr      INP_Segnacol
181500090210      *
181600090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
181700090210     C                   DO        10            x                              --- 01 ---
181800090210      *
181900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
182000090210     C     x30(X)        ANDNE     *LOVAL
182100090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
182200090210     C                   ADD       1             §§
182300090210     C                   MOVEL     x30(X)        SGE(§§)
182400090210     C                   END                                                    --- 03 ---
182500090210      *
182600090210     C                   END                                                    --- 02 ---
182700090210      *
182800090210     C                   ENDSR
182900090210     C*----------------------------------------------------------------
183000090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
183100090210     C*----------------------------------------------------------------
183200090210     C     SGNELA        BEGSR
183300090210      *
183400090210     c                   exsr      INP_Segnacol
183500090210     C*
183600090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
183700090210     C*  in base al numero elementi validi carico sgn.
183800090210     C                   DO        10            X                              --- 01 ---
183900090210     C*
184000090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
184100090210     C     x30(X)        ANDNE     *LOVAL
184200090210     C*
184300090210     C                   CLEAR                   DSSGN
184400090210     C*  Controllo se devo ricevere la linea di partenza
184500090210     C                   MOVEL     'N'           WERRO             1
184600090210     C     §PULP1        IFGT      0                                            --- 03 ---
184700090210     C                   EXSR      REPLNP
184800090210     C                   END                                                    --- 03 ---
184900090210     C*  Controllo se devo ricevere la linea di arrivo
185000090210     C     §PULA1        IFGT      0                                            --- 03 ---
185100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
185200090210     C                   EXSR      REPLNA
185300090210     C                   END                                                    --- 03 ---
185400090210     C*  Controllo se devo ricevere il numero di serie
185500090210     C     §PUNR1        IFGT      0                                            --- 03 ---
185600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
185700090210     C                   EXSR      REPNRS
185800090210     C                   END                                                    --- 03 ---
185900090210     C*  Controllo se devo ricevere il numero segnacollo
186000090210     C     §PUSG1        IFGT      0                                            --- 03 ---
186100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
186200090210     C                   EXSR      REPSGN
186300090210     C                   END                                                    --- 03 ---
186400090210     C*  Controllo se devo ricevere la zona di consegna
186500090210     C     §PUZN1        IFGT      0                                            --- 03 ---
186600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
186700090210     C                   EXSR      REPZNC
186800090210     C                   END                                                    --- 03 ---
186900090210     C*  Imposto da VAB i dati a zero se mi è stato passato
187000090210     C*  numero segnacollo valido
187100090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
187200090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
187300090210     C                   Z-ADD     VABLNP        SGNLNP
187400090210     C                   END                                                    --- 04 ---
187500090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
187600090210     C                   Z-ADD     VABLNA        SGNLNA
187700090210     C                   END                                                    --- 04 ---
187800090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
187900090210     C                   Z-ADD     VABNRS        SGNNRS
188000090210     C                   END                                                    --- 04 ---
188100090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
188200090210     C                   Z-ADD     VABZNC        SGNZNC
188300090210     C                   END                                                    --- 04 ---
188400090210      *
188500090210     C*  mi reimposto LNA e zona da segnacollo
188600090210     C                   Z-ADD     SGNLNA        VABLNA
188700090210     C                   Z-ADD     SGNZNC        VABZNC
188800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
188900090210     C                   ADD       1             XY
189000090210     C                   MOVE      SGNNSC        SGN(XY)
189100090210     C                   END                                                    --- 03 ---
189200090210     C*
189300090210     C                   END                                                    --- 02 ---
189400090210     C*
189500090210     C                   END                                                    --- 01 ---
189600090210      *
189700090210     C                   ENDSR
189800090210     C*----------------------------------------------------------------
189900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
190000090210     C*----------------------------------------------------------------
190100090210     C     REPLNP        BEGSR
190200090210     C*
190300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
190400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
190500090210     C*  dal numero segnacollo passatomi dal partner
190600090210     C                   Z-ADD     §PULP1        WP                2 0
190700090210     C     3             SUBST     x30(X):WP     WLNP              3
190800090210     C*
190900090210     C*  controllo che la lnp calcolata sia numerica
191000090210     C                   TESTN                   WLNP                 98
191100090210     C*
191200090210     C*  se è numerica la carico
191300090210     C     *IN98         IFEQ      '1'
191400090210     C                   MOVE      WLNP          SGNLNP
191500090210     C                   ELSE
191600090210     C*  se non è numerica stampo errore
191700090210     C                   MOVEL     'S'           WERRO             1
191800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
191900090213     c                   exsr      Error_DET
192000090210     C                   goto      end_LNP
192100090210     C                   END
192200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
192300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
192400090210     C     SGNLNP        IFNE      VABLNP
192500090210     C                   MOVEL     'S'           WERRO             1
192600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
192700090210     c                             a della Bolla'
192800090213     c                   exsr      Error_DET
192900090210     C                   goto      end_LNP
193000090210     C                   END
193100090210     C*
193200090210     C     end_LNP       ENDSR
193300090210     C*----------------------------------------------------------------
193400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
193500090210     C*----------------------------------------------------------------
193600090210     C     REPLNA        BEGSR
193700090210     C*
193800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
193900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
194000090210     C*  dal numero segnacollo passatomi dal partner
194100090210     C                   Z-ADD     §PULA1        WP                2 0
194200090210     C     3             SUBST     x30(X):WP     WLNA              3
194300090210     C*  controllo che la lna calcolata sia numerica
194400090210     C                   TESTN                   WLNA                 98
194500090210     C*  se è numerica la carico
194600090210     C     *IN98         IFEQ      '1'
194700090210     C                   MOVE      WLNA          SGNLNA
194800090210     C                   ELSE
194900090210     C*  se non è numerica stampo errore
195000090210     C                   MOVEL     'S'           WERRO             1
195100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
195200090213     c                   exsr      Error_DET
195300090210     C                   goto      end_LNA
195400090210     C                   END
195500090210     C*
195600090210     C     end_LNA       ENDSR
195700090210     C*----------------------------------------------------------------
195800090210     C*? REPNRS - Reperisce numero serie
195900090210     C*----------------------------------------------------------------
196000090210     C     REPNRS        BEGSR
196100090210     C*
196200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
196300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
196400090210     C*  dal numero segnacollo passatomi dal partner
196500090210     C                   Z-ADD     §PUNR1        WP                2 0
196600090210     C     2             SUBST     x30(X):WP     WNRS              2
196700090210     C*  controllo che il nrs calcolata sia numerico
196800090210     C                   TESTN                   WNRS                 98
196900090210     C*  se è numerica la carico
197000090210     C     *IN98         IFEQ      '1'
197100090210     C                   MOVE      WNRS          SGNNRS
197200090210     C                   ELSE
197300090210     C*  se non è numerico stampo errore
197400090210     C                   MOVEL     'S'           WERRO             1
197500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
197600090213     c                   exsr      Error_DET
197700090210     C                   goto      end_NRS
197800090210     C                   END
197900090210      *
198000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
198100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
198200090210     C     SGNNRS        IFNE      VABNRS
198300090210     C                   MOVEL     'S'           WERRO             1
198400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
198500090210     c                             o della Bolla'
198600090213     c                   exsr      Error_DET
198700090210     C                   goto      end_NRS
198800090210     C                   END
198900090210     C*
199000090210     C     end_NRS       ENDSR
199100090210     C*----------------------------------------------------------------
199200090210     C*? REPZNC - Reperisce zona consegna
199300090210     C*----------------------------------------------------------------
199400090210     C     REPZNC        BEGSR
199500090210     C*
199600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
199700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
199800090210     C*  dal numero segnacollo passatomi dal partner
199900090210     C                   Z-ADD     §PUZN1        WP                2 0
200000090210     C     2             SUBST     x30(X):WP     WZNC              2
200100090210     C*  controllo che la zona calcolata sia numerica
200200090210     C                   TESTN                   WZNC                 98
200300090210     C*  se è numerica la carico
200400090210     C     *IN98         IFEQ      '1'
200500090210     C                   MOVE      WZNC          SGNZNC
200600090210     C                   ELSE
200700090210     C*  se non è numerica stampo errore
200800090210     C                   MOVEL     'S'           WERRO             1
200900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
201000090213     c                   exsr      Error_DET
201100090210     C                   goto      end_ZNC
201200090210     C                   END
201300090210     C*
201400090210     C     end_ZNC       ENDSR
201500090210     C*----------------------------------------------------------------
201600090210     C*? REPSGN - Reperisce numero segnacollo
201700090210     C*----------------------------------------------------------------
201800090210     C     REPSGN        BEGSR
201900090210     C*
202000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
202100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
202200090210     C*  indicata in tabella)
202300090210     C*  dal numero segnacollo passatomi dal partner
202400090210     C                   Z-ADD     §PUSG1        WP                2 0
202500090210     C     7             SUBST     x30(X):WP     WSGN              7
202600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
202700090210     C                   TESTN                   WSGN                 98
202800090210     C*  se è numerico lo carico
202900090210     C     *IN98         IFEQ      '1'
203000090210     C                   MOVE      WSGN          SGNNSC
203100090210     C                   ELSE
203200090210     C*  se non è numerica stampo errore
203300090210     C                   MOVEL     'S'           WERRO             1
203400090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
203500090213     c                   exsr      Error_DET
203600090210     C                   goto      end_SGN
203700090210     C                   END
203800090210     C*
203900090210     C     end_SGN       ENDSR
204000051205      *----------------------------------------------------*
204100051205      *   DEFINIZIONE CHIAVI                               *
204200051205      *----------------------------------------------------*
204300051205     C     *INZSR        BEGSR
204400051205      *
204500991129     c     *ENTRY        PLIST
204600060613     C                   parm                    esito             1
204700971215      *
204800050112     C     KTABel        KLIST
204900050112     C                   KFLD                    TBLKUT
205000050112     C                   KFLD                    TBLCOD
205100050112     C                   KFLD                    TBLKEY
205200090210      *
205300090210     C     K_TAB1L       KLIST
205400090213     C                   KFLD                    etbUNB
205500090213     C                   KFLD                    etbSGM
205600090213     C                   KFLD                    etbVALSGM
205700090209     C*
205800090209     C* Definisco chiavi di accesso
205900090209     C     KTAB1         KLIST
206000090209     C                   KFLD                    KKUT
206100090209     C                   KFLD                    KCOD
206200090209     C*
206300090209     C     KNUF          KLIST
206400090209     C                   KFLD                    KAAA
206500090209     C                   KFLD                    KCNU
206600090209     C                   KFLD                    KFIL
206700090209      *
206800090209     C     KVAB1         KLIST
206900090209     C                   KFLD                    KCCM
207000090209     C                   KFLD                    KCMR
207100090209     C                   KFLD                    KCNT
207200090209     C                   KFLD                    KAAS
207300090209     C                   KFLD                    KLNP
207400090209     C                   KFLD                    KNRS
207500090209     C                   KFLD                    KNSP
207600090209      *
207700090209     C     KVAB2         KLIST
207800090209     C                   KFLD                    KCCM
207900090209     C                   KFLD                    KCMR
208000090209      *
208100090209     C     KRDE          KLIST
208200090209     C                   KFLD                    KAAS
208300090209     C                   KFLD                    KLNP1
208400090209     C                   KFLD                    KNRS
208500090209     C                   KFLD                    KNSP
208600090209     C                   KFLD                    KNMC
208700090209     C                   KFLD                    KNRP
208800090209      *
208900090209     C     KACO          KLIST
209000090209     C                   KFLD                    KKUT
209100090209     C                   KFLD                    KKCC
209200090209     C                   KFLD                    KKSC
209300090209      *
209400090209     C     KIND          KLIST
209500090209     C                   KFLD                    KKUT
209600090209     C                   KFLD                    KKCC
209700090209     C                   KFLD                    KKSC
209800090209      *
209900090209      * DETERMINO SE SONO BARTOLINI O SDI
210000090209     C                   CLEAR                   TIBS55DS
210100090209     C                   MOVEL     'L'           I50TLA
210200090209     C                   CALL      'TIBS55R'
210300090209     C                   PARM                    TIBS55DS
210400090209      *
210500090209     C* RECUPERO DATI DELL'UTENTE
210600090209     C                   Z-ADD     1             CODUT             1 0
210700090209     C                   CALL      'XPARUT'
210800090209     C                   PARM                    UTEDSE0F
210900090209     C                   MOVEL     RAGUT         RSUT             20
211000090209     C*
211100090209     C* RICERCA CAPOCONTI
211200090209     C                   DO        50            X
211300090209     C                   MOVE      TCU(X)        TCUDS
211400090209     C     F56           IFEQ      'CG'
211500090209     C     F34           ANDEQ     '01'
211600090209     C                   Z-ADD     KCU(X)        KCI               4 0
211700090209     C                   END
211800090209     C                   END
211900090209     C*
212000090211     c                   movel(p)  'EDPAB'       User_Msg         10
212100090211     C*
212200090209     C* IMPOSTO A ZERO VARIABILI DI PGM
212300090209     c                   move      *blank        Snd_Msg_alert     1
212400090209     C                   MOVEL     CA(1)         WCA              78
212500090209     C                   MOVEL     CN(1)         WCN              78
212600090209     C*
212700090209     C* RECUPERO DATA E ORA
212800090209     C                   TIME                    WHHDAT           14 0
212900090209     C                   MOVE      WHHDAT        G02DAT
213000090209     C                   MOVE      WHHDAT        WDATA8            8 0
213100090209     C                   MOVE      WDATA8        WAA2              2 0
213200090209     C                   MOVEL     WDATA8        DATA              6 0
213300090209     C                   MOVE      WAA2          DATA
213400090209     C                   MOVEL     *BLANK        G02ERR
213500090209     C                   CALL      'XSRDA8'
213600090209     C                   PARM                    WLBDA8
213700090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
213800090209     C                   TIME                    WORA              6 0
213900090209      * Recupero data e ora
214000090209     C                   TIME                    W0140
214100090209      * UDATE IN GGMMAAAA
214200090209     C                   MOVE      W0140         WDTGIO
214300090209      * UDATE IN AAAAMMGG
214400090209     C     *eur          MOVEL     WDTGIO        DATA_eur
214500090209     C     *iso          MOVEL     DATA_eur      dateu
214600090209      *
214700090209      * ?              /*--------------------------- */
214800090209     c                   exsr      CARICA_TABELLE
214900090209      * ?              /*--------------------------- */
215000090209      *
215100090209      *
215200090209     C                   MOVEL     *ALL'-'       CMP132          132
215300090209     C                   CLEAR                   EDiVAB00
215400090211     C                   clear                   vablnp_PT
215500090212     C                   clear                   vabnrs_PT
215600090211     C                   clear                   vabctm_PT
215700090211     C                   clear                   VABfnsp_PT
215800090209     C                   MOVEA     *HIVAL        SGN
215900991124      *
216000050414      *------------------
216100991124     C                   ENDSR
216200060621      * ?------------------------------------------------------------------ */
216300090209      *?    CARICA TABELLE SU SCHIERE
216400060621      * ?------------------------------------------------------------------ */
216500090209     C     CARICA_TABELLEBEGSR
216600090205      *
216700090209     C* Caricamento Tabella 1B
216800090209     C                   Z-ADD     0             X                 4 0
216900090209     C                   Z-ADD     CODUT         KKUT
217000090209     C                   MOVEL     '1B'          KCOD
217100090209     C     KTAB1         CHAIN     TABEL00F                           30
217200090209     C     *IN30         DOWEQ     '0'
217300090209     C     TBLFLG        IFEQ      *BLANKS
217400090209     C                   ADD       1             X
217500090209     C                   MOVEL     TBLKEY        CTM(X)
217600090209     C                   MOVEL     TBLUNI        DTM(X)
217700090209     C                   END
217800090209     C     KTAB1         READE     TABEL00F                               30
217900090209     C                   END
218000090209      *
218100090209     C* Caricamento Tabella 15
218200090209     C                   Z-ADD     0             X                 4 0
218300090209     C                   Z-ADD     CODUT         KKUT
218400090209     C                   MOVEL     '15'          KCOD
218500090209     C     KTAB1         CHAIN     TABEL00F                           30
218600090209     C     *IN30         DOWEQ     '0'
218700090209     C     TBLFLG        IFEQ      *BLANKS
218800090209     C                   ADD       1             X
218900090209     C                   MOVEL     TBLKEY        C15(X)
219000090209     C                   MOVEL     TBLUNI        DS15
219100090209     C                   MOVEL     §15COD        ISO(X)
219200090209     C                   MOVEL     §15ECF        CPF(X)
219300090209     C                   MOVE      §15PCF        CPF(X)
219400090209     C                   END
219500090209     C     KTAB1         READE     TABEL00F                               30
219600090209     C                   END
219700090209      *
219800090209     C* Caricamento Tabella CV
219900090209     C                   Z-ADD     0             X
220000090209     C                   Z-ADD     CODUT         KKUT
220100090209     C                   MOVEL     'CV'          KCOD
220200090209     C     KTAB1         CHAIN     TABEL00F                           30
220300090209     C     *IN30         DOWEQ     '0'
220400090209     C     TBLFLG        IFEQ      *BLANKS
220500090209     C                   ADD       1             X
220600090209     C                   MOVEL     TBLKEY        CCV(X)
220700090209     C                   MOVEL     TBLUNI        DCV(X)
220800090209     C                   END
220900090209     C     KTAB1         READE     TABEL00F                               30
221000090209     C                   END
221100090216      *
221200110328      * Caricamento Tabella termini consegna
221300110328     C                   Z-ADD     0             X
221400110328     C                   MOVEL     'TB'          TABCOD
221500110328     C     TABCOD        CHAIN     EDTAB01L                           30
221600110328     C     *IN30         DOWEQ     '0'
221700110328     C     TABFLG        IFEQ      *BLANKS
221800110328     C                   ADD       1             X
221900110328     C                   eval      K35_TpBolla(X) = TABKEY
222000110328     C                   eval      Cod_TpBolla(X) = TABKEY
222100110328     C                   eval      Decod_Porto(X) = TABUNI
222200110328     C                   END
222300110328     C     TABCOD        READE     EDTAB01L                               30
222400110328     C                   END
222500110328      *
222600090216     C* Caricamento Tabella Tipo Incasso C/Assegno
222700090216     C                   Z-ADD     0             X
222800090216     C                   MOVEL     'TC'          TABCOD
222900090216     C     TABCOD        CHAIN     EDTAB01L                           30
223000090216     C     *IN30         DOWEQ     '0'
223100090216     C     TABFLG        IFEQ      *BLANKS
223200090216     C                   ADD       1             X
223300090216     C                   MOVEL     TABKEY        CTC35(X)
223400090216     C                   MOVEL     TABKEY        CTC(X)
223500090216     C                   MOVEL     TABUNI        TPI(X)
223600090216     C                   END
223700090216     C     TABCOD        READE     EDTAB01L                               30
223800090216     C                   END
223900090209      *
224000090209      * Caricamento Tabella Partner esteri
224100090209     C                   Z-ADD     0             X
224200090209     C                   MOVEL     'PT'          TABCOD
224300090209     C     TABCOD        CHAIN     EDTAB01L                           30
224400090209     C     *IN30         DOWEQ     '0'
224500090209      *
224600090209     C                   SELECT
224700090209     C     TABFLG        WHENNE    *BLANKS
224800090209      *
224900090209     C                   OTHER
225000090209     C                   ADD       1             X
225100090209     C                   MOVEL     TABKEY        CPT(X)
225200090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
225300090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
225400090209     C                   MOVEL     TABUNI        DPT(X)
225500090209     C                   ENDSL
225600090209      *
225700090209     C     TABCOD        READE     EDTAB01L                               30
225800090209     C                   END
225900121105      *
226000121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
226100121105     c                   exsr      ChecK_PT
226200121105     C*
226300090209      * salva quanti Codici UNB ha caricato
226400090209     c                   z-add     x             sav_CPTx
226500090209      *
226600090209     C                   MOVEL     'PU'          TABCOD
226700090209     C     TABCOD        CHAIN     EDTAB01L                           30
226800090209     C     *IN30         DOWEQ     '0'
226900090209      *
227000090209     C                   SELECT
227100090209     C     TABFLG        WHENNE    *BLANKS
227200090209      *
227300090209     C                   OTHER
227400090209     C                   MOVEL     TABKEY        CODPU            35
227500090209     C                   Z-ADD     1             X
227600090209     C     CODPU         LOOKUP    CPT(X)                                 32
227700090209     C   32              MOVEL     TABUNI        DPU(X)
227800090209     C                   ENDSL
227900090209      *
228000090209     C     TABCOD        READE     EDTAB01L                               30
228100090209     C                   END
228200090209      *
228300090209      * Prosegue tab.PT e PU
228400090209     C                   MOVEL     'PV'          TABCOD
228500090209     C     TABCOD        CHAIN     EDTAB01L                           30
228600090209     C     *IN30         DOWEQ     '0'
228700090209      *
228800090209     C                   SELECT
228900090209     C     TABFLG        WHENNE    *BLANKS
229000090209      *
229100090209     C                   OTHER
229200090209     C                   MOVEL     TABKEY        CODPV            35
229300090209     C                   Z-ADD     1             X
229400090209     C     CODPV         LOOKUP    CPT(X)                                 32
229500090209     C   32              MOVEL     TABUNI        DPV(X)
229600090209     C                   ENDSL
229700090209      *
229800090209     C     TABCOD        READE     EDTAB01L                               30
229900090209     C                   END
230000090209      *
230100090209     C* Caricamento Tabella codici clienti - tariffe
230200090209     C                   Z-ADD     0             X
230300090209     C                   MOVEL     'CL'          TABCOD
230400090209     C     TABCOD        CHAIN     EDTAB01L                           30
230500090209     C     *IN30         DOWEQ     '0'
230600090209     C     TABFLG        IFEQ      *BLANKS
230700090209     C                   ADD       1             X
230800090209     C                   MOVEL     TABKEY        CCL(X)
230900090209     C                   MOVEL     TABUNI        DCL(X)
231000090209     C                   END
231100090209     C     TABCOD        READE     EDTAB01L                               30
231200090209     C                   END
231300090209      *
231400090209     C* Caricamento Serivizi speciali
231500090209     C                   Z-ADD     0             X
231600090209     C                   MOVEL     'SS'          TABCOD
231700090209     C     TABCOD        CHAIN     EDTAB01L                           30
231800090209     C     *IN30         DOWEQ     '0'
231900090209     C     TABFLG        IFEQ      *BLANKS
232000090209     C                   ADD       1             X
232100090209     C                   MOVEL     TABKEY        SS(X)
232200090209     C                   MOVEL     TABKEY        SS35(X)
232300090209     C                   MOVEL     TABUNI        DSS(X)
232400090209     C                   END
232500090209     C     TABCOD        READE     EDTAB01L                               30
232600090209     C                   END
232700090209      *
232800090209     C                   ENDSR
232900090209      * ?------------------------------------------------------------------ */
233000090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
233100090209      * ?------------------------------------------------------------------ */
233200090209     C     MSG_Errato    BEGSR
233300090209      *
233400090205      * ?  Scrive su tutti i records il tipo di errore
233500090205     c     *start        setll     tivin00r
233600090205     c                   read      tivin00r
233700091016     c                   if        %Eof(tivin00r)
233800091016     c                   move      'S'           EoFTivin
233900091016     c                   end
234000090205     c                   dow       not %eof(tivin00r)
234100090211     c                   if        vinMSG  = *blank
234200090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
234300090210     C                               >> Controllare i DATI su UPLOAD !!'
234400090211     c                   end
234500090205     C                   eval      vinFLG = '2'
234600090213     c                   eval      Almeno_Un_Due ='S'
234700090205     c                   eval      esito  = '2'
234800090209     c                   eval      se_errore ='S'
234900090213     c                   eval      err_bloccante ='S'
235000091016     c                   if        EoFTivin = ' '
235100090205     c                   update    tivin000
235200091016     c                   end
235300090205     c                   read      tivin00r
235400091016     c                   if        %Eof(tivin00r)
235500091016     c                   move      'S'           EoFTivin
235600091016     c                   end
235700090205     c                   endDO
235800090205      *
235900090205     C                   ENDSR
236000090213      * ?------------------------------------------------------------------ */
236100090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
236200090213      * ?------------------------------------------------------------------ */
236300090213     C     DETta_Errato  BEGSR
236400090213      *
236500090213      *  rilascia il record prima di rieseguire il ciclo
236600090213     c                   update    tivin000
236700090213      *
236800090213      * ?  Scrive su tutti i records il tipo di errore
236900090213     c     SavNUM_RECord setll     tivin00r
237000090213     c                   read      tivin00r
237100091016     c                   if        %Eof(tivin00r)
237200091016     c                   move      'S'           EoFTivin
237300091016     c                   end
237400090213     c                   dow       not %eof(tivin00r)
237500090213      *
237600090213      *
237700090213     c                   if        vinMSG  = *blank
237800090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
237900090213     C                             >> Controllare i DATI di ogni Segmento <<'
238000090213     c                   end
238100090213     c                   exsr      Error_DET
238200090213      *
238300090213     c                   if        VNREC = NUM_RECord
238400090213     c                   leave
238500090213     c                   end
238600090213      *
238700091016     c                   if        EoFTivin = ' '
238800090213     c                   update    tivin000
238900091016     c                   end
239000090213     c                   read      tivin00r
239100091016     c                   if        %Eof(tivin00r)
239200091016     c                   move      'S'           EoFTivin
239300091016     c                   end
239400090213      *
239500090213     c                   endDO
239600090603      *
239700091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
239800090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
239900091019???? C                             Bolle RHENUS non totalmente tradotto. -
240000100603     C                             Controllare UPLOAD del cliente 0895358.'
240100130321     c                   if             manda_mail='S'
240200090603     c                   exsr      invio_mail_AB
240300130321     c                   end
240400090603      *
240500090213      *
240600090213     C                   ENDSR
240700090205      * ?------------------------------------------------------------------ */
240800090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
240900090205      * ?------------------------------------------------------------------ */
241000090205     C     *pssr         BEGSR
241100090205     C
241200060710     C                   eval      esito = '2'
241300091019???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import RHENUS'
241400090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
241500100603     C                             Bolle import in filiale - messaggio EDI -
241600100603     C                             ricevuto errato su UPLOAD cliente 0895358.'
241700090603     c                   exsr      invio_mail_AB
241800060621     C
241900090213     C                   ENDSR
242000090213      * ?------------------------------------------------------------------ */
242100090213      *?      Segnala Errore su TIVIN
242200090213      * ?------------------------------------------------------------------ */
242300090213     C     Error_DET     BEGSR
242400090213     C
242500090213     C                   eval      vinFLG = '2'
242600090213     c                   eval      Almeno_Un_Due ='S'
242700090213     c                   eval      esito  = '1'
242800090213     c                   eval      se_errore ='S'
242900090213     C                   eval      Err_in_detta = 'S'
243000090213     C
243100090213     C                   ENDSR
243200090209     c*==================================================================*
243300090209     C*? CNVDAT - DECODIFICA LA DATA
243400090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
243500090209     C*         - WDATA  (DATA ALFANUMERICA)
243600090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
243700090209     C*         - WHMS   (ORA NUMERICA)
243800090209     C*----------------------------------------------------------------
243900090209     C     CNVDAT        BEGSR
244000090209     C*
244100090209     C                   MOVEL     ' '           WERRO             1
244200090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
244300090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
244400090209     C                   Z-ADD     *ZEROS        WHMS              6 0
244500090209     C*
244600090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
244700090209     C                   MOVEL     WDATA         WCOMO8            8
244800090209     C                   TESTN                   WCOMO8               99
244900090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
245000090209     C  N99              MOVEL     'S'           WERRO             1
245100090209     C                   END
245200090209     C*
245300090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
245400090209     C                   MOVEL     WDATA         WCOMO14          14
245500090209     C                   TESTN                   WCOMO14              99
245600090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
245700090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
245800090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
245900090209     C  N99              MOVEL     'S'           WERRO             1
246000090209     C                   END
246100090209     C*
246200090209     C                   ENDSR
246300090210     C*----------------------------------------------------------------
246400090210     C*? TSTNUM - CONTROLLO NUMERICO
246500090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
246600090210     C*         - WLUNGH (Lunghezza campo output)
246700090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
246800090210     C*----------------------------------------------------------------
246900090210     C     TSTNUM        BEGSR
247000090210     C*
247100090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
247200090210     C                   MOVEL     'N'           WOKNUM            1
247300090210     C                   MOVEL     *BLANKS       WNUMOK           15
247400090210     C                   Z-ADD     15            IN                3 0
247500090210     C                   Z-ADD     WLUNGH        IX                3 0
247600090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
247700090210     C                   MOVEA     WNUM          NDA
247800090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
247900090210     C                   MOVEA     *ALL'0'       NDN
248000090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
248100090210     C                   Z-ADD     WLUNGH        I                 3 0
248200090210     C     I             DOWGT     0                                            --- 1 -->
248300090210     C* ESTRAE IL CARATTERE DA ESAMINARE
248400090210     C     1             SUBST     WNUM:I        WTEM01            1
248500090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
248600090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
248700090210     C     WTEM01        SCAN      WCA                                    99
248800090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
248900090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
249000090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
249100090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
249200090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
249300090210     C* DA DESTRA VERSO SINISTRA
249400090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
249500090210     C                   MOVEL     WTEMP1        NDN(IN)
249600090210     C                   SUB       1             IN
249700090210     C                   END                                                    <-- 3 ---
249800090210     C                   END                                                    <-- 2 ---
249900090210     C                   SUB       1             I
250000090210     C                   END                                                    <-- 1 ---
250100090210     C*
250200090210     C                   MOVEA     NDN           WNDN             15
250300090210     C     WNDN          IFNE      *ZEROS
250400090210     C                   MOVEA     NDN           WNUMOK           15
250500090210     C                   MOVEL     'S'           WOKNUM            1
250600090210     C                   END
250700090210     C*
250800090210     C                   ENDSR
250900090210     C*----------------------------------------------------------------
251000090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
251100090210     C*----------------------------------------------------------------
251200090210     C     CNVCAP        BEGSR
251300090210     C*
251400090210     C                   MOVEL     *BLANKS       WCAP              9
251500090210     C     nad3251       IFNE      '0'
251600090210     C     '  '          SCAN      nad3251       LENGHT            2 0
251700090210     C                   SUB       1             LENGHT
251800090210     C     LENGHT        IFLE      5
251900090210     C     LENGHT        ANDGT     0
252000090210     C                   Z-ADD     LENGHT        T                 2 0
252100090210     C                   Z-ADD     5             S                 2 0
252200090210     C                   MOVEA     nad3251       CAP9
252300090210     C                   MOVEL     *ZEROS        CAP5
252400090210     C                   DO        LENGHT
252500090210     C                   MOVE      CAP9(T)       CAP5(S)
252600090210     C                   SUB       1             S
252700090210     C                   SUB       1             T
252800090210     C                   END
252900090210     C                   MOVEA     CAP5          WCAP5             5
253000090210     C     WCAP5         IFEQ      '0'
253100090210     C                   MOVEL     *BLANKS       WCAP
253200090210     C                   ELSE
253300090210     C                   MOVEA     CAP5          WCAP
253400090210     C                   END
253500090210     C*
253600090210     C                   ELSE
253700090210     C                   MOVEL     nad3251       WCAP
253800090210     C                   END
253900090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
254000090210     C     WCAP          IFNE      *BLANKS
254100090210     C     WFLCPF        ANDNE     0
254200090210     C                   MOVEL     WFLCPF        WELE              1 0
254300090210     C                   MOVE      WFLCPF        WPOS              1 0
254400090210     C                   MOVEL     *BLANK        WCPF
254500090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
254600090210     C                   ELSE
254700090210     C                   MOVEL     *BLANKS       WCPF
254800090210     C                   END
254900090210     C                   END
255000090210     C*
255100090210     C                   ENDSR
255200090211      * ?================================================================== */
255300090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
255400090211      * ?================================================================== */
255500090211     C     Scrive_VAB    BEGSR
255600090211      *
255700090211     c                   clear                   err_bloccante     1
255800090211     c                   move      'S'           Almeno_Uno
255900090211      *
256000090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
256100090211     c                   exsr      UPDVAB
256200090211      *
256300090211     C* >>>>>>>>>>>>
256400090211     C*  Richiamo pgm per scrittura immediata VAB
256500090211     C     §PUWRK        IFEQ      ' '
256600090211      *
256700090211     C*  Richiamo pgm per esecuzione stampa
256800090211     C     §PTCMR        IFEQ      'S'
256900090213     C                   MOVEL     WNRDOC        d86CMR
257000090211     C                   ELSE
257100090213     C                   MOVEL     WNRDOC        d86CMR
257200090211     C                   END
257300090211      *
257400090213     C                   MOVEL     ' '           d86RIE
257500090213     C                   MOVEL     §PUDTA        d86DTA
257600090213     C                   Z-ADD     1             d86CNT
257700090213     C                   Z-ADD     §PTKSC        d86CCM
257800090213     C                   MOVEL     *BLANKS       d86STA
257900090213     C                   MOVEL     'E'           d86MOD
258000090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
258100090211      * dove poi imposta la DS dei parametri passati
258200090213     C                   MOVEL     'LR'          d86CHI
258300090213     C                   MOVEL     'N'           d86CTL
258400090211      *
258500090211     c                   move      kpjbu         SvKpjbu
258600090211     c                   clear                   kpjbu
258700090216     C                   movel     *on           Commit_on         1
258800090213     C                   MOVEL     TRTC86ds      KPJBU
258900090213      * ?- Chiamata la TRTC86R2 -*
259000090213     C                   CALL      'TRTC86R2'
259100090211     C                   PARM                    KPJBA
259200090216     C                   PARM                    Commit_on
259300090211     c                   move      Svkpjbu       Kpjbu
259400090211      *
259500090211     C*  Controllo pgm per scrittura immediata VAB
259600090211     C     WW            IFNE      0
259700090211     C                   DO        WW            WX                3 0
259800090213     C                   Z-ADD     KCL(WX)       d86CCM
259900090211      *
260000090211     c                   move      kpjbu         SvKpjbu
260100090211     c                   clear                   kpjbu
260200090216     C                   movel     *on           Commit_on         1
260300090213     C                   MOVEL     TRTC86ds      KPJBU
260400090213      * ?- Chiamata la TRTC86R2 -*
260500090213     C                   CALL      'TRTC86R2'
260600090211     C                   PARM                    KPJBA
260700090216     C                   PARM                    Commit_on
260800090211     c                   move      Svkpjbu       Kpjbu
260900090211     C                   END
261000090211     C                   END
261100090211      *
261200090211     C                   END
261300090211      * >>>>>>>>>>>>
261400090211     C                   if        se_errore  = 'S'
261500090211     c                   move      'S'           err_bloccante
261600090211     c                   goto      Error_VAB
261700090211     c                   end
261800090211     C*
261900090211     C     Error_VAB     Endsr
262000090211     C*----------------------------------------------------------------
262100090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
262200090211     C*----------------------------------------------------------------
262300090211     C     UPDVAB        BEGSR
262400090211      *
262500090211     C*  Se non è stato impostato il codice bolla lo imposto io
262600090211     C     VABCBO        IFEQ      *BLANKS
262700090211     C     VABCAS        IFGT      0
262800090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
262900090211     C                   ELSE
263000090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
263100090211     C                   END
263200090211     C                   END
263300110922      *
263400110922      * Imposta le NOte
263500110922     C     VABNOT        IFEQ      *BLANKS
263600110922     C                   MOVEL     wnot1         VABNOT
263700110922     C                   MOVEL     wnot2         VABNT2
263800110922     C                   ELSE
263900110922     C                   MOVEL     wnot1         VABNT2
264000110922     C                   END
264100090211      *
264200090211     C*  Attribuisco anno spedizione
264300090211     C     WDATPA        IFGT      0                                            anno sped.
264400090211     C                   MOVEL     WDATPA        VABAAS
264500090211     C                   ELSE
264600090211     C     WDATFV        IFGT      0
264700090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
264800090211     C                   END
264900090211     C                   END
265000090211      *
265100090211      *  Controllo dettaglio segnacolli
265200090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
265300090211     C                   EXSR      CTRSGE
265400090211     C                   ELSE
265500090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
265600090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
265700090211     C                   EXSR      CTRSGN
265800090211     C                   END
265900090211     C                   END
266000090211      *
266100090212      *  Imposto linea partenza e serie
266200090211     C                   eval      VABLNP = VABLNP_PT
266300090212     C                   eval      VABNRS = VABnrs_PT
266400090211      *
266500090211      *  Controllo se devo variare il codice trattamento merce
266600090211     C                   eval      VABCTM = VABctm_PT
266700090211      *
266800090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
266900090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
267000090211     c                   clear                   mantiene_nsp      1            *blk/'S'
267100090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
267200090211      *
267300090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
267400090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
267500090211      *
267600090211      *  Imposto codice cliente
267700090211     c                   Select
267800090211      *
267900090211      *  Specificato codice da qualificatore FF (TD15)
268000090211     C                   When        WCLKSC <> *zeros
268100090211     C                   Z-ADD     WCLKSC        VABCCM
268200090211     C                   MOVEL     WCLTAR        VABCTR
268300090211      * forza LNP/CTM/NRS
268400090211     c                   if        WclLNP <> 0
268500090211     C                   eval      VABLNP = WclLNP
268600090211     c                   end
268700090211     c                   if        WclNRS <> 0
268800090211     C                   eval      VABNRS = WclNRS
268900090211     c                   end
269000090211     c                   if        WclCTM <> *blank
269100090211     C                   eval      VABCTM = WclCTM
269200090211     c                   end
269300090211      *
269400090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
269500090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
269600090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
269700090211     c                   end
269800090211      *
269900090211      *  Specificato codice da qualificatore FF (TT15)
270000090211     C                   When        $CLKSC <> *zeros
270100090211     C                   Z-ADD     $CLKSC        VABCCM
270200090211     C                   MOVEL     $CLTAR        VABCTR
270300090211      * forza LNP/CTM/NRS
270400090211     c                   if        $clLNP <> 0
270500090211     C                   eval      VABLNP = $clLNP
270600090211     c                   end
270700090211     c                   if        $clNRS <> 0
270800090211     C                   eval      VABNRS = $clNRS
270900090211     c                   end
271000090211     c                   if        $clCTM <> *blank
271100090211     C                   eval      VABCTM = $clCTM
271200090211     c                   end
271300090211      *
271400090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
271500090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
271600090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
271700090211     c                   end
271800090211      *
271900090211      *  Imposto dati da tabella - PT-
272000090211     C                   Other
272100090211      *
272200090211     C                   MOVEL     §PTKSC        VABCCM
272300090211     C                   MOVEL     §PTCTR        VABCTR
272400090211      *
272500090211     C                   Endsl
272600090211      *
272700090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
272800090211      *    o dalla relativa CL
272900090211     C                   MOVEL     VABlnp        VABfgs
273000090211      *
273100090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
273200090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
273300090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
273400090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
273500090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
273600090211      *
273700090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
273800090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
273900090211     c                   else
274000090211      *
274100090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
274200090211     C*  lo attribuisco io
274300090211      **              **------------------**
274400090211     c                   exsr      repnum
274500090211      **              **------------------**
274600090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
274700090211      **              **------------------**
274800090211     c                   end
274900090211      *
275000090211     C*  Altrimenti attribuisco data del giorno
275100090211     C     VABMGS        IFEQ      0
275200090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
275300090211     C                   MOVE      WOGGI         VABMGS                         = oggi
275400090211     C                   END
275500090211     C*  Eseguo posizionamento su EDiVAB
275600090211     C                   Z-ADD     VABAAS        KAAS
275700090211     C                   Z-ADD     VABlnp        KLNP
275800090211     C                   Z-ADD     VABNRS        KNRS
275900090211     C                   Z-ADD     VABNSP        KNSP
276000090211     C                   MOVEL     SAVBOL        SALVA
276100090211     C     KVAB1         CHAIN     EDiVAB1L                           30
276200090211     C                   MOVEL     SALVA         SAVBOL
276300090211     C*  Imposto dati CMR
276400090211     C                   Z-ADD     1             VABCNT
276500090211     **
276600090211      *** Attenzione se si deve trattare il VAX con il CMR
276700090211      *** il campo VABCNT deve essere sempre impostato a (1)
276800090211     C     §puWRK        ifEQ      'S'
276900090211     C     §puNSP        andEQ     'S'
277000090211     C                   Z-ADD     1             VABCNT
277100090211     c                   end
277200090211     **
277300090211     C                   Z-ADD     WDTPRE        VABDTS
277400090212     C                   movel     '20'          VABDTS
277500090212     C     WHMPRE        mult      100           VABHMS
277600090211     C     §PTCMR        IFEQ      'S'
277700090211     C                   Z-ADD     WDTCMR        VABDCM
277800090211     C                   MOVEL     WNRCMR        VABCMR
277900090211     C                   ELSE
278000090211     C                   Z-ADD     WDATFV        VABDCM
278100090211     C                   MOVEL     WNUMFV        VABCMR
278200090211     C                   END
278300090211     C*  Se non è indicato il nome del mittente = Partner mittente
278400090211     C     VABRMO        IFEQ      *BLANKS
278500090211     C                   MOVEL     ACORAG        VABRMO
278600090211     C                   MOVEL     INDCAP        VABCMO
278700090211     C     INDCAE        IFNE      *BLANKS
278800090211     C                   MOVEL     INDCAE        VABCMO
278900090211     C                   END
279000090211     C     INDSTA        IFNE      *BLANKS
279100090211     C                   MOVEL     INDSTA        VABNMO
279200090211     C                   END
279300090211     C                   END
279400090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
279500090211     C     §1BFG1        IFEQ      'N'
279600090211     C     §1BFG7        ANDEQ     'N'
279700090211     C     §1BFG2        ANDEQ     'N'
279800090211     C***                  Z-ADD0         VABNRS
279900090211     C                   END
280000090211     C*  Imposto segancollo da - a
280100090211     C     §1BFG1        IFEQ      'S'
280200090211     C     §1BFG7        ANDEQ     'N'
280300090211     C     §1BF12        ANDNE     'E'
280400090211      *
280500090211     C     XY            IFEQ      *ZEROS
280600090211     C                   CLEAR                   VABNCD
280700090211     C                   CLEAR                   VABNCA
280800090211     C                   ELSE
280900090211     C                   Z-ADD     SGN(1)        VABNCD
281000090211     C                   Z-ADD     SGN(XY)       VABNCA
281100090211     C                   ENDIF
281200090211     C*
281300090211     C                   END
281400090211     C*
281500090211     C*  Se cap mittente originale a blank abblenco nazione
281600090211     C     VABCMO        IFEQ      *BLANKS
281700090211     C                   MOVEL     *BLANKS       VABNMO
281800090211     C                   END
281900090603     C*
282000090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
282100090603     C* Se in testata
282200090603     c                   if        NAD_destinatario_in_testata = 'S'
282300090603     c                   eval         VABrsd =  tes_VABrsd
282400090603     c                   eval         VABrd2 =  tes_VABrd2
282500090603     c                   eval         VABind =  tes_VABind
282600090603     c                   eval         VABlod =  tes_VABlod
282700090603     c                   eval         VABnzd =  tes_VABnzd
282800090603     c                   end
282900090603     C* Se in testata
283000090603     c                   if        NAD_mittente_in_testata = 'S'
283100090603     c                   eval         VABrmo = tes_VABrmo
283200090603     c                   eval         VABnmo = tes_VABnmo
283300090603     c                   eval         VABcmo = tes_VABcmo
283400090603     c                   end
283500090603      *
283600090211      *? ------------------------------------------------------------------------
283700090213     C   30              WRITE     EDiVAB00                             99
283800090213     C  N30              UPDATE    EDiVAB00                             99
283900090211      *? ------------------------------------------------------------------------
284000090213     c   99              eval      errori_in_Wrt = 'S'
284100090211     C*
284200090211     c                   clear                   wri_vabtic        1
284300090211     C*
284400090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
284500090211     C                   EXSR      WRTVAT_Rif
284600090211     C*
284700090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
284800090211     C     §1BF12        IFEQ      'E'
284900090211     C                   EXSR      WRTVAT
285000090211     C                   ELSE
285100090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
285200090211     C     §1BFG7        IFEQ      'S'
285300090211     C     §1BFG1        OREQ      'S'
285400090211     C                   EXSR      WRTVAD
285500090211     C                   END
285600090211     C                   END
285700090211      *
285800090211     C* Reimposto codice trattamento merce cliente
285900090211     C                   MOVEL     WSVTRM        DS1B
286000090211     C**
286100090211     C* Do errore se previsto CMR ma non c'è
286200090211     C     §PTCMR        IFNE      ' '
286300090211     C                   EXSR      MEMCMR
286400090211     C                   END
286500090211     C* Do errore se previsto AFB ma non c'è
286600090211     C     §PTNFV        IFNE      ' '
286700090211     C                   EXSR      MEMAFB
286800090211     C                   END
286900090211     C*  Se il cliente vuole il ritorno delle date di consegna
287000090211     C*  è c'è una squdratura fra i segnacolli e il numero
287100090211     C*  dei colli che ci ha passato scrivo EDSUM
287200090211     C     §PUDTA        IFEQ      'S'
287300090211     C     WSQUAD        ANDEQ     'S'
287400090211     C                   EXSR      WRTSUM
287500090211     C                   END
287600090211     C*
287700090211     C                   ENDSR
287800090211     C*----------------------------------------------------------------
287900090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
288000090211     C*----------------------------------------------------------------
288100090211     C     CTRSGN        BEGSR
288200090211     C*
288300090211     C                   MOVEL     'N'           WNSGER            1
288400090211     C*
288500090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
288600090211     C     VABNCL        IFNE      XY
288700090211     C                   MOVEL     'S'           WNSGER
288800090211     C                   MOVEL     'S'           WSQUAD
288900090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
289000090211     c                              bollettati'
289100090211     C                   GOTO      FINSGN
289200090211     C                   END
289300090211     C*  Riordino i segnacolli
289400090211     C     §1BFG7        IFEQ      'N'
289500090211     C                   SORTA     SGN
289600090211     C*  Controllo se sono sequenziali
289700090211     C                   MOVEL     'N'           WNOSEQ            1
289800090211     C     SGN(1)        ADD       1             WEXSGN            7 0
289900090211     C     2             DO        XY            X
290000090211     C     SGN(X)        IFNE      WEXSGN
290100090211     C                   MOVEL     'S'           WNOSEQ
290200090211     C                   END
290300090211     C     SGN(X)        ADD       1             WEXSGN
290400090211     C                   END
290500090211     C                   END
290600090211     C*
290700090211     C     FINSGN        ENDSR
290800090211     C*----------------------------------------------------------------
290900090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
291000090211     C*----------------------------------------------------------------
291100090211     C     CTRSGE        BEGSR
291200090211     C*
291300090211     C                   MOVEL     'N'           WNSGER            1
291400090211      *
291500090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
291600090211      *
291700090211     C     VABNCL        IFNE      §§
291800090211     C                   MOVEL     'S'           WNSGER
291900090211     C                   MOVEL     'S'           WSQUAD
292000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
292100090211     c                              bollettati'
292200090211     C                   END
292300090211      *
292400090211     C                   ENDSR
292500090211     C*----------------------------------------------------------------
292600090211     C*? ESEGUO SCRITTURA EDiVAD
292700090211     C*----------------------------------------------------------------
292800090211     C     WRTVAD        BEGSR
292900090211     C*
293000090211     C* Se non seq. scrivo xy record
293100090211     C     §1BFG1        IFEQ      'S'
293200090211     C     XY            ANDNE     *ZEROS
293300090211     C*
293400090211     C     §1BFG7        IFEQ      'S'
293500090211     C                   DO        XY            X
293600090211     C                   Z-ADD     VABCCM        KCCM
293700090211     C                   Z-ADD     SGN(X)        KNCD
293800090211     C                   Z-ADD     VABCNT        VADCNT
293900090211     C                   MOVEL     VABCMR        VADCMR
294000090211     C                   Z-ADD     VABAAS        VADAAS
294100090211     C                   Z-ADD     VABLNP        VADLNP
294200090211     C                   Z-ADD     VABNRS        VADNRS
294300090211     C                   Z-ADD     VABNSP        VADNSP
294400090211     C                   Z-ADD     1             VADNCL
294500090211     C                   MOVEL     SGN(X)        VADCDP
294600090211     C                   Z-ADD     SGN(X)        VADNCD
294700090211     C                   Z-ADD     *ZEROS        VADNCA
294800090211     C                   Z-ADD     VABCCM        VADCCM
294900090211     C                   movel     VABfgs        VADfgs
295000090213     C                   WRITE     EDiVAD00                             99
295100090213     c   99              eval      errori_in_Wrt = 'S'
295200090211     C                   END
295300090211     C* Se sequenziali scrivo un solo record
295400090211     C                   ELSE
295500090211     C                   Z-ADD     VABAAS        VADAAS
295600090211     C                   Z-ADD     VABLNP        VADLNP
295700090211     C                   Z-ADD     VABNRS        VADNRS
295800090211     C                   Z-ADD     VABNSP        VADNSP
295900090211     C                   Z-ADD     VABNCL        VADNCL
296000090211     C                   Z-ADD     SGN(1)        VADNCD
296100090211     C                   Z-ADD     SGN(XY)       VADNCA
296200090211     C                   Z-ADD     VABCCM        VADCCM
296300090211     C                   movel     VABfgs        VADfgs
296400090213     C                   WRITE     EDiVAD00                             99
296500090213     c   99              eval      errori_in_Wrt = 'S'
296600090211     C                   END
296700090211     C*
296800090211     C                   ELSE
296900090211     C                   DO        XY            X
297000090211     C                   Z-ADD     VABCCM        KCCM
297100090211     C                   Z-ADD     SGN(X)        KNCD
297200090211     C                   Z-ADD     VABAAS        VADAAS
297300090211     C                   Z-ADD     VABLNP        VADLNP
297400090211     C                   Z-ADD     VABNRS        VADNRS
297500090211     C                   Z-ADD     VABNSP        VADNSP
297600090211     C                   MOVEL     SGN(X)        VADCDP
297700090211     C                   Z-ADD     1             VADNCL
297800090211     C                   Z-ADD     *ZEROS        VADNCD
297900090211     C                   Z-ADD     *ZEROS        VADNCA
298000090211     C                   Z-ADD     VABCCM        VADCCM
298100090211     C                   movel     VABfgs        VADfgs
298200090213     C                   WRITE     EDiVAD00                             99
298300090213     c   99              eval      errori_in_Wrt = 'S'
298400090211     C                   END
298500090211     C*
298600090211     C                   END
298700090211     C*
298800090211     C                   ENDSR
298900090211     C*----------------------------------------------------------------
299000090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
299100090211     C*----------------------------------------------------------------
299200090211     C     WRTVAT_Rif    BEGSR
299300090211      *
299400090211      *  riferimento di consegna destinatario
299500090211     c                   if        WatRDE <> *blank
299600090211     C                   Z-ADD     VABCNT        VATCNT
299700090211     C                   MOVEL     VABCMR        VATCMR
299800090211     C                   Z-ADD     VABCCM        VATCCM
299900090211     C                   movel     VABfgs        VATfgs
300000090211     C                   Z-ADD     VABLNP        VATLNP
300100090211     C                   Z-ADD     VABAAS        VATAAS
300200090211     C                   Z-ADD     VABNRS        VATNRS
300300090211     C                   Z-ADD     VABNSP        VATNSP
300400090211     C                   MOVEL     'A'           VATTRC
300500090211     C                   MOVEL     WatRDE        VATNOT
300600090213     C                   WRITE     EDiVAT00                             99
300700090213     c   99              eval      errori_in_Wrt = 'S'
300800090211     c                   end
300900090211      *
301000090211      *  riferimento di consegna telefonico
301100090211     c                   if        WatTEL <> *blank
301200090211     C                   Z-ADD     VABCNT        VATCNT
301300090211     C                   MOVEL     VABCMR        VATCMR
301400090211     C                   Z-ADD     VABCCM        VATCCM
301500090211     C                   movel     VABfgs        VATfgs
301600090211     C                   Z-ADD     VABLNP        VATLNP
301700090211     C                   Z-ADD     VABAAS        VATAAS
301800090211     C                   Z-ADD     VABNRS        VATNRS
301900090211     C                   Z-ADD     VABNSP        VATNSP
302000090211     C                   MOVEL     'B'           VATTRC
302100090211     C                   MOVEL     WatTEL        VATNOT
302200090213     C                   WRITE     EDiVAT00                             99
302300090213     c   99              eval      errori_in_Wrt = 'S'
302400090211     c                   end
302500090211      *
302600090211      * riferimento del Partner che una volta veniva riportato sul BL4
302700090211     c                   if        Wnsp   <> *blank
302800090211     C                   Z-ADD     VABCNT        VATCNT
302900090211     C                   MOVEL     VABCMR        VATCMR
303000090211     C                   Z-ADD     VABCCM        VATCCM
303100090211     C                   movel     VABfgs        VATfgs
303200090211     C                   Z-ADD     VABLNP        VATLNP
303300090211     C                   Z-ADD     VABAAS        VATAAS
303400090211     C                   Z-ADD     VABNRS        VATNRS
303500090211     C                   Z-ADD     VABNSP        VATNSP
303600090211     C                   MOVEL     'C'           VATTRC
303700090211     C                   MOVEL     Wnsp          VATNOT
303800090213     C                   WRITE     EDiVAT00                             99
303900090213     c   99              eval      errori_in_Wrt = 'S'
304000090211     c                   end
304100090211      *
304200090211     C                   ENDSR
304300090211     C*----------------------------------------------------------------
304400090211     C*? ESEGUO SCRITTURA EDiVAT
304500090211     C*----------------------------------------------------------------
304600090211     C     WRTVAT        BEGSR
304700090211      *
304800090211     C                   DO        §§            X
304900090211     C                   Z-ADD     VABCNT        VATCNT
305000090211     C                   MOVEL     VABCMR        VATCMR
305100090211     C                   Z-ADD     VABCCM        VATCCM
305200090211     C                   movel     VABfgs        VATfgs
305300090211     C                   Z-ADD     VABLNP        VATLNP
305400090211     C                   Z-ADD     VABAAS        VATAAS
305500090211     C                   Z-ADD     VABNRS        VATNRS
305600090211     C                   Z-ADD     VABNSP        VATNSP
305700090211     C                   MOVEL     'E'           VATTRC
305800090211      *
305900090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
306000090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
306100090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
306200090211     c                   if        WVATlun > 0
306300090211      *
306400090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
306500090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
306600090211      *
306700090211     c                   else
306800090211      *
306900090211      * Se non è impostato nulla prende il segnacollo passato così com'è
307000090211     C                   MOVEL     SGE(X)        VATNOT
307100090211     c                   end
307200090211      *
307300090213     C                   WRITE     EDiVAT00                             99
307400090213     c   99              eval      errori_in_Wrt = 'S'
307500090211     C                   END
307600090211      *
307700090211     C                   ENDSR
307800090211     C*----------------------------------------------------------------
307900090211     C*? ESEGUO SCRITTURA EDSUM
308000090211     C*----------------------------------------------------------------
308100090211     C     WRTSUM        BEGSR
308200090211     C*
308300090211     C* Se non seq. scrivo xy record
308400090211     C                   DO        XY            X
308500090211     C                   Z-ADD     VABAAS        SUMAAS
308600090211     C                   Z-ADD     VABLNP        SUMFLS
308700090211     C                   MOVEL     WKSC          SUMLNP
308800090211     C                   Z-ADD     VABLNA        SUMLNA
308900090211     C                   Z-ADD     VABZNC        SUMZNC
309000090211     C                   Z-ADD     VABNRS        SUMNRS
309100090211     C                   Z-ADD     VABNSP        SUMNSP
309200090211     C                   Z-ADD     SGN(X)        SUMNSC
309300090211     C                   Z-ADD     WKSC          SUMCCM
309400090211     C                   MOVEL     *BLANKS       SUMSTS
309500090211     C                   MOVEL     *BLANKS       SUMFLG
309600090211     C                   MOVEL     WPRGSP        SUMCNI
309700090211     C                   MOVEL     VABCMR        SUMCMR
309800090211     C                   Z-ADD     0             SUMNFE
309900090211     C                   Z-ADD     VABCCM        VADCCM
310000090213     C                   WRITE     EDSUM000                             99
310100090213     c   99              eval      errori_in_Wrt = 'S'
310200090211     C                   END
310300090211     C*
310400090211     C                   ENDSR
310500090211     c*==================================================================*
310600090211      * Manda un Msg in Posta AS Sede a EDP*
310700090211     c*==================================================================*
310800090211     c     SEND_MSG_EDP  begsr
310900090211      *
311000090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
311100090211      *   Lo manda una sola volta per lavoro
311200090211     c                   IF        Snd_Msg_alert  <> 'N' and
311300090211     c                             User_msg <> *blank
311400090211     c                   z-add     1             Jx
311500090211     C                   exsr      CalQEZSNDMG
311600090211     c                   end
311700090211      *
311800090211     c                   endsr
311900090213     c**********************************************************************
312000090213     c* reperimento numero Spedizione
312100090213     c**********************************************************************
312200090213     C     repnum        BEGSR
312300090213      *
312400090213      *    deve controllare sulla tabella "3C" se il numero spedizione
312500090213      *     deve essere mantenuto oppure no
312600090213     C*
312700090213     C                   clear                   Ds3C
312800090213     C                   Z-ADD     CODUT         TBLKUT
312900090213     C                   MOVEL     '3C'          TBLCOD
313000090213     C                   movel(p)  VABccm        TBLKEY
313100090213     C     KTABel        CHAIN     TABEL00F                           30
313200090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
313300090213     C                   MOVEL     TBLUNI        Ds3C
313400090213     C                   END
313500090213      *
313600090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
313700090213     c                   if        §3CFsp <> 'S'
313800090213      *
313900090213     C* NSP => Stacco un numeratore da AZNUM
314000090213     c                   movel     kpjbu         svkpjbu
314100090213     c                   clear                   kpjbu
314200090213     C                   clear                   TRUL33DS
314300090213     C                   eval      I33OPE = *zeros
314400090213     C                   eval      I33CNU = 302
314500090213     C                   eval      I33NUM = 1
314600090213     C                   movel     TRUL33DS      KPJBU
314700090213     C                   call      'TRUL33R'
314800090213     C                   parm                    KPJBA
314900090213     C                   movel     KPJBU         TRUL33DS
315000090213     c                   movel     svkpjbu       kpjbu
315100090213      ****
315200090213     C                   if        O33ERR = *zeros
315300090213     c                   z-add     o33nrf        NUM_SPED
315400090213     C                   else
315500090213     C                   endif
315600090213      ****
315700090213     c                   ELSE
315800090213      *   se si vuole mantenere il numero spedizione
315900090213      ****
316000090213     C                   MOVEL     WOGGI         WANNO             4 0
316100090213     C                   MOVE      WANNO         KAAA
316200090213     C                   Z-ADD     3             KCNU
316300090213     C                   Z-ADD     VABlnp        KFIL
316400090213     C     KNUF          CHAIN     FLNUF                              9999
316500090213     C     *IN99         IFEQ      '0'
316600090213     C                   ADD       1             NUFNUM
316700090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
316800090213     C                   UPDATE    FLNUF
316900090213     C                   ELSE
317000090213     C                   END
317100090213     C*
317200090213     c                   EndIF
317300090213      **
317400090213     C                   ENDSR
317500090213     C*----------------------------------------------------------------
317600090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
317700090213     C*----------------------------------------------------------------
317800090213     C     MEMCMR        BEGSR
317900090213     C*
318000090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
318100090213     C     §PTCM1        IFEQ      'S'
318200090213     C                   CLEAR                   EDRDE000
318300090213     C                   MOVEL     'TD1154'      KNMC
318400090213     C                   Z-ADD     1             KNRP
318500090213     C                   MOVEL     WKSC          KLNP1
318600090213     C     KRDE          CHAIN     EDRDE01L                           31
318700090213     C     *IN31         IFEQ      '1'
318800090213     C                   Z-ADD     VABAAS        RDEAAS
318900090213     C                   Z-ADD     VABLNP        RDELNP
319000090213     C                   Z-ADD     VABNRS        RDENRS
319100090213     C                   Z-ADD     VABNSP        RDENSP
319200090213     C                   MOVEL     'TD1154'      RDENMC
319300090213     C                   Z-ADD     1             RDENRP
319400090213     C                   MOVEL     WNRCMR        RDEVAL
319500090213     C                   WRITE     EDRDE000                             99
319600090213     c   99              eval      errori_in_Wrt = 'S'
319700090213     C                   END
319800090213     C*  Memorizzo qualificatore CMR
319900090213     C                   CLEAR                   EDRDE000
320000090213     C                   MOVEL     'TD1153'      KNMC
320100090213     C                   Z-ADD     1             KNRP
320200090213     C                   MOVEL     WKSC          KLNP1
320300090213     C     KRDE          CHAIN     EDRDE01L                           31
320400090213     C     *IN31         IFEQ      '1'
320500090213     C                   Z-ADD     VABAAS        RDEAAS
320600090213     C                   Z-ADD     VABLNP        RDELNP
320700090213     C                   Z-ADD     VABNRS        RDENRS
320800090213     C                   Z-ADD     VABNSP        RDENSP
320900090213     C                   Z-ADD     1             RDENRP
321000090213     C                   MOVEL     'TD1153'      RDENMC
321100090213     C                   MOVEL     'CMR'         RDEVAL
321200090213     C                   WRITE     EDRDE000                             99
321300090213     c   99              eval      errori_in_Wrt = 'S'
321400090213     C                   END
321500090213     C*  Memorizzo la data
321600090213     C                   CLEAR                   EDRDE000
321700090213     C                   MOVEL     'TD2380'      KNMC
321800090213     C                   Z-ADD     1             KNRP
321900090213     C                   MOVEL     WKSC          KLNP1
322000090213     C     KRDE          CHAIN     EDRDE01L                           31
322100090213     C     *IN31         IFEQ      '1'
322200090213     C                   Z-ADD     VABAAS        RDEAAS
322300090213     C                   Z-ADD     VABLNP        RDELNP
322400090213     C                   Z-ADD     VABNRS        RDENRS
322500090213     C                   Z-ADD     VABNSP        RDENSP
322600090213     C                   Z-ADD     1             RDENRP
322700090213     C                   MOVEL     'TD2380'      RDENMC
322800090213     C                   MOVEL     WDTCMR        RDEVAL
322900090213     C                   WRITE     EDRDE000                             99
323000090213     c   99              eval      errori_in_Wrt = 'S'
323100090213     C                   END
323200090213     C*
323300090213     C                   END                                                    §PTCM1 = 'S'
323400090213     C*
323500090213     C                   ENDSR
323600090213     C*----------------------------------------------------------------
323700090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
323800090213     C*----------------------------------------------------------------
323900090213     C     MEMAFB        BEGSR
324000090213     C*
324100090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
324200090213     C     §PTNF1        IFEQ      'S'
324300090213     C                   CLEAR                   EDRDE000
324400090213     C                   MOVEL     WKSC          KLNP1
324500090213     C                   MOVEL     'TD1154'      KNMC
324600090213     C                   Z-ADD     1             KNRP
324700090213     C     KRDE          CHAIN     EDRDE01L                           31
324800090213     C     *IN31         IFEQ      '1'
324900090213     C                   Z-ADD     VABAAS        RDEAAS
325000090213     C                   Z-ADD     VABLNP        RDELNP
325100090213     C                   Z-ADD     VABNRS        RDENRS
325200090213     C                   Z-ADD     VABNSP        RDENSP
325300090213     C                   MOVEL     'TD1154'      RDENMC
325400090213     C     §PTCM1        IFNE      'S'
325500090213     C     WNRCMR        OREQ      *BLANKS
325600090213     C                   Z-ADD     1             RDENRP
325700090213     C                   ELSE
325800090213     C                   Z-ADD     2             RDENRP
325900090213     C                   END
326000090213     C                   MOVEL     WNUMFV        RDEVAL
326100090213     C                   WRITE     EDRDE000                             99
326200090213     c   99              eval      errori_in_Wrt = 'S'
326300090213     C                   END
326400090213     C*  Memorizzo qualificatore CMR
326500090213     C                   CLEAR                   EDRDE000
326600090213     C                   MOVEL     WKSC          KLNP1
326700090213     C                   MOVEL     'TD1153'      KNMC
326800090213     C                   Z-ADD     1             KNRP
326900090213     C     KRDE          CHAIN     EDRDE01L                           31
327000090213     C     *IN31         IFEQ      '1'
327100090213     C                   Z-ADD     VABAAS        RDEAAS
327200090213     C                   Z-ADD     VABLNP        RDELNP
327300090213     C                   Z-ADD     VABNRS        RDENRS
327400090213     C                   Z-ADD     VABNSP        RDENSP
327500090213     C     §PTCM1        IFNE      'S'
327600090213     C     WNRCMR        OREQ      *BLANKS
327700090213     C                   Z-ADD     1             RDENRP
327800090213     C                   ELSE
327900090213     C                   Z-ADD     2             RDENRP
328000090213     C                   END
328100090213     C                   MOVEL     'TD1153'      RDENMC
328200090213     C                   MOVEL     'AFB'         RDEVAL
328300090213     C                   WRITE     EDRDE000                             99
328400090213     c   99              eval      errori_in_Wrt = 'S'
328500090213     C                   END
328600090213     C*  Memorizzo la data
328700090213     C                   CLEAR                   EDRDE000
328800090213     C                   MOVEL     WKSC          KLNP1
328900090213     C                   MOVEL     'TD2380'      KNMC
329000090213     C                   Z-ADD     1             KNRP
329100090213     C     KRDE          CHAIN     EDRDE01L                           31
329200090213     C     *IN31         IFEQ      '1'
329300090213     C                   Z-ADD     VABAAS        RDEAAS
329400090213     C                   Z-ADD     VABLNP        RDELNP
329500090213     C                   Z-ADD     VABNRS        RDENRS
329600090213     C                   Z-ADD     VABNSP        RDENSP
329700090213     C     §PTCM1        IFNE      'S'
329800090213     C     WNRCMR        OREQ      *BLANKS
329900090213     C                   Z-ADD     1             RDENRP
330000090213     C                   ELSE
330100090213     C                   Z-ADD     2             RDENRP
330200090213     C                   END
330300090213     C                   MOVEL     'TD2380'      RDENMC
330400090213     C                   MOVEL     WDATFV        RDEVAL
330500090213     C                   WRITE     EDRDE000                             99
330600090213     c   99              eval      errori_in_Wrt = 'S'
330700090213     C                   END
330800090213     C*
330900090213     C                   END                                                    §PTCM1 = 'S'
331000090213     C*
331100090213     C                   ENDSR
331200090213     c*==================================================================*
331300090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
331400090213     c*==================================================================*
331500090213     c     Imp_ADDR_mail begsr
331600090213      *
331700090213     c                   clear                   Indirizzi
331800090213      *
331900090213      *  Lunghezza indirizzi presenti
332000090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
332100090213     c                   z-add     1             al_char           3 0
332200090213     c                   z-add     1             dal_char          3 0
332300090213     c                   z-add     1             tot_char          3 0
332400090213      *
332500090213     c     successivo    tag
332600090213      *
332700090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
332800090213      *   e aggiunge il dominio Bartolini + il (;) separatore
332900090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
333000090213     c                   eval      tot_char = al_char - dal_char
333100090213     c                   z-add     dal_char      Dac
333200090213     c                   z-add     tot_char      Luc
333300090213      *
333400090213     c                   clear                   Indir_Bartol     40
333500090213     c                   clear                   Indir_Parz       20
333600090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
333700090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
333800090213     c                             %Trim(bart_it)
333900090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
334000090213      *
334100090213     c                   if        al_char = Lun_Ind
334200090213     c                   goto      fine_indmail
334300090213     c                   else
334400090213     c                   eval      dal_char = ( al_char + 1 )
334500090213     c                   goto      successivo
334600090213     c                   end
334700090213      *
334800090213     C     fine_indmail  TAG
334900090213      *
335000090213     C                   ENDSR
335100090213     c*==================================================================*
335200090213      * Manda un Msg x E-mail
335300090213     c*==================================================================*
335400090213     c     Invio_Mail    begsr
335500090213      *
335600090213     c                   goto      non_inviare
335700090213      *
335800090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
335900090213     C* Inizializzo variabili
336000090213     C                   movel     *blanks       wrkEml          253
336100090213     C                   movel     *blanks       wrkMsg         5000
336200090213     C                   movel     *blanks       wrkOgg           44
336300090213      *
336400090213     C* Valorizzo i campi della e-m@ail - indirizzo
336500090213     C                   eval      wrkEml = Indirizzi
336600090213     C                   eval      wrkOgg = Oggetto
336700090213     C                   eval      wrkMsg = Messaggio
336800090213
336900090213     C                   call(e)   'TIS701C'
337000090213     C                   parm                    wrkEml
337100090213     C                   parm                    wrkOgg
337200090213     C                   parm                    wrkMsg
337300090213
337400090213     c     non_inviare   tag
337500090213     C*
337600090213     C                   ENDSR
337700090603     c*==================================================================*
337800090603      * Manda un Msg x E-mail a EDPAB
337900090603     c*==================================================================*
338000090603     c     Invio_Mail_AB begsr
338100090603      *
338200090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
338300090603     C* Inizializzo variabili
338400090603     C                   movel     *blanks       wrkEml          253
338500090603     C                   movel     *blanks       wrkMsg         5000
338600090603     C                   movel     *blanks       wrkOgg           44
338700090603      *
338800090603     C* Valorizzo i campi della e-m@ail - indirizzo
338900110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
339000110203     C                   eval      wrkEml = Indirizzi
339100090603     C                   eval      wrkOgg = Oggetto
339200090603     C                   eval      wrkMsg = Messaggio
339300110203      ********
339400110203     C********           call(e)   'TIS701C'
339500110203     C********           parm                    wrkEml
339600110203     C********           parm                    wrkOgg
339700110203     C********           parm                    wrkMsg
339800110203     C*
339900110203      ** *****
340000110203     C                   call(e)   'TRTCT00R2'
340100110203     C                   parm                    wrkEml
340200110203     C                   parm                    wrkOgg
340300110203     C                   parm                    wrkMsg
340400090603     C*
340500090603     C                   ENDSR
340600090211     ***********************************************************************
340700090211     **
340800090211     ** Send Message (QEZSNDMG) API
340900090211     **
341000090211     ***********************************************************************
341100090211     C     CalQEZSNDMG   BEGSR
341200090211      *
341300090211     ** Invio messaggio all'utente.
341400090211     C                   EVAL      SndMg03 = msgDSP
341500090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
341600090211     C                   EVAL      SndMg05(Jx) = User_msg
341700090211     C                   EVAL      SndMg06 = Jx
341800090211     C                   CLEAR                   QUSEC
341900090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
342000090211
342100090211     C                   CALL      'QEZSNDMG'
342200090211     C                   PARM                    SndMg01
342300090211     C                   PARM                    SndMg02
342400090211     C                   PARM                    SndMg03
342500090211     C                   PARM                    SndMg04
342600090211     C                   PARM                    SndMg05
342700090211     C                   PARM                    SndMg06
342800090211     C                   PARM                    SndMg07
342900090211     C                   PARM                    SndMg08
343000090211     C                   PARM                    Qusec
343100090211     C                   PARM                    SndMg10
343200090211     C                   PARM                    SndMg11
343300090211     C                   PARM                    SndMg12
343400090211      *
343500090211     C                   ENDSR
343600121105      *---------------------------------------------------------------*
343700121105      * CTRL caricamento schiere    PT                               -*
343800121105      *---------------------------------------------------------------*
343900121105     C     Check_PT      begsr
344000121105     C*
344100121105     c* Controllo riempimento schiera
344200121105     c                   clear                   trul0sds
344300121105     c                   eval      i0sski='CPT'
344400121105     c                   eval      i0sele=%elem(CPT)
344500121105     c                   eval      i0spie=x
344600121105     c                   eval      i0sfile='EDTAB00F'
344700121105     c                   eval      i0ssif=knsif
344800121105     c                   eval      i0spgm='TRTCT04R'
344900121113     c                   move      kpjbu         SvKpjbu
345000121113     c                   clear                   kpjbu
345100121105     c                   movel     trul0sds      kpjbu
345200121105     c                   call      'TRUL0SR'
345300121105     c                   parm                    kpjba
345400121113     c                   eval      kpjbu  =  SvKpjbu
345500121105     C*
345600121105     C                   ENDSR
345700090211     O*****************************************************************
345800090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
345900090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
346000090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
346100090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
