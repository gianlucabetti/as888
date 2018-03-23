000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600091203????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFTMIN vers D96A  *
000700091203      *                         cliente   YAMAHA         con  IFTMIN vers D96A  *
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
009900140530     D Trovato_UNZ     s              1    inz(' ')
010000091019???? D Dettaglio_NAD_destinatario...
010100091019???? D                 s              1
010200091019???? D NAD_destinatario_in_testata...
010300091019???? D                 s              1
010400091019???? D NAD_mittente_in_testata...
010500091019???? D                 s              1
010600091019???? D Inizio_Dettaglio...
010700091019???? D                 s              1
010800100120???? D seg_imprevisto...
010900100120???? D                 s              1
011000091019???? D Riferimento_Spedizione...
011100100113???? D                 s              3    INZ('SRN')
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
023400090209      * Schiere x località
023500100319     D IND             S              1    DIM(35)
023600060608      **?------------------------------------------------------------------ */
023700090601     C*? Ds Scrittura del VAT "E"
023800060608      **?------------------------------------------------------------------ */
023900060612     D XTOEBCDDS     E DS
024000060620      *
024100060608      **?------------------------------------------------------------------ */
024200050112      * Numeratore Bolle (302)
024300050112     D trul33ds      E DS
024400050112     D Ds3C          E DS
024500090209     C*
024600090209      **?------------------------------------------------------------------ */
024700090209     C* Definizione campi di work
024800090209     D pVLB            s                   LIKE(VABVLB)
024900090209     D kKUT            s                   LIKE(TBLKUT)
025000090209     D kCOD            s                   LIKE(TBLCOD)
025100090209     D kKEY            s                   LIKE(TBLKEY)
025200090209     D kKCC            s                   LIKE(ACOKCC)
025300090209     D kKSC            s                   LIKE(ACOKSC)
025400090209     D kAAA            s                   LIKE(NUFAAA)
025500090209     D kCNU            s                   LIKE(NUFCNU)
025600090209     D kFIL            s                   LIKE(NUFFIL)
025700090209     D kAAS            s                   LIKE(VABAAS)
025800090209     D kLNP            s                   LIKE(VABLNP)
025900090209     D kNRS            s                   LIKE(VABNRS)
026000090209     D kNSP            s                   LIKE(VABNSP)
026100090209     D kCMR            s                   LIKE(VABCMR)
026200090209     D kCCM            s                   LIKE(VABCCM)
026300090209     D kNCD            s                   LIKE(VADNCD)
026400090210     D kCNT            s                   LIKE(VADCNT)
026500090209     D kNMC            s                   LIKE(RDENMC)
026600090209     D kNRP            s                   LIKE(RDENRP)
026700090209     D kLNP1           s                   LIKE(RDELNP)
026800090209      *
026900090209      *  Invio E-mail
027000090209     D Indirizzi       s            253
027100090209     D Oggetto         s             44
027200090209     D Messaggio       s           5000
027300090209      *
027400090209     d   DSmail        ds
027500090209     D Mail_Ind                      44
027600090209     d   IND_char                     1    dim(44)
027700090209      *
027800090209     D Lun_Ind         s              5u 0
027900090209      *
028000090209     D Dac             s              5u 0
028100090209     D Luc             s              5u 0
028200090209      *
028300090209     C*---------------------------------------------------------------*
028400090209     D MsgDSP          S            256                                         Testo generico
028500090209     C*---------------------------------------------------------------*
028600090209     D SndMg01         S             10                                         Message type
028700090209     D                                     INZ('*INFO')
028800090209     D SndMg02         S             10                                         Deluvery mode
028900090209     D                                     INZ('*BREAK')
029000090209     D SndMg03         S            256                                         Message text
029100090209     D SndMg04         S             10I 0                                      Length of text
029200090209     D                                     INZ(%SIZE(SndMg03))
029300090209     D SndMg05         S             10                                         User profile
029400090209     D                                     DIM(299)
029500090209     D SndMg06         S             10I 0                                      Number of user
029600090209     D SndMg07         S             10I 0                                      Message sent indic.
029700090209     D SndMg08         S             10I 0                                      Function requested
029800090209     D SndMg10         S              1                                         Show display
029900090209     D                                     INZ('N')
030000090209     D SndMg11         S             20                                         Qualified MSGQ name
030100090209     D SndMg12         S              4                                         Name type indicator
030200090209     D                                     INZ('*USR')
030300090209      * indice di scihera
030400090209     D Jx              S              5I 0
030500090209      *
030600090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
030700090209      *
030800090209     d bart_it         C                   CONST('@Bartolini.it;')
030900090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
031000060612      * ?================================================================== */
031100060612      *
031200060612      * ?   * Campi x decodifica * (INPUT  del Record)
031300090130     D  Dati           s           2048
031400060612      * ?   *--------------------------------------------------------------*
031500060612     D  se_errore      s              1    inz(' ')
031600060612      * ?* ------------------------------------------------------ *
031700060710     D Digits          C                   '0123456789'
031800091019     D*------------------
031900091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
032000091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
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
038800091016     c                   if        %Eof(tivin00r)
038900091016     c                   move      'S'           EoFTivin
039000091016     c                   end
039100060612
039200060612     c                   dow       not %eof(Tivin00r)
039300060614
039400060614      * solo i record sflaggati da rielaborare
039500060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
039600090211      *  Sempre Record OK
039700090211      ** Controlli formali sui campi
039800090211     C                   eval      vinFLG = '1'
039900060612     c                   clear                   se_errore
040000060612
040100060612      ** Decodifica record a campi variabili
040200060612      * ?              /*---------------------- */
040300090205     c                   exsr      Decod_Segmento
040400090209      * ?              /*---------------------- */
040500090209
040600090213      *** ------------------------------------
040700091019????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT" x IFTMIN
040800090213      *** ------------------------------------
040900090213      * ?              /*---------------------- */
041000090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
041100090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
041200090213      * ?              /*---------------------- */
041300090601      ***
041400091019????  **
041500091019????  *   OGNI fine DETTAGLIO è determinato dall'UNT.
041600091019???? c                   if           tipo_seg = 'UNT'
041700091019????  *
041800090213      *  Deve flaggare il dettaglio con dei problemi
041900090213     C                   if        Err_in_detta = 'S'
042000090213      **?              /*---------------------- */
042100090213     c                   exsr      DETta_Errato
042200090213      **?              /*---------------------- */
042300090213     c                   end
042400090213      *
042500060612      **  Se presente un errore nel record emette una segnalazione msg
042600091019???? c                   if        se_errore = 'S'
042700091019???? c                   else
042800091019????  ******
042900090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
043000090227     c                   if         se_trovato_NAD ='S'
043100060612      * ?              /*---------------------- */
043200090209     c                   exsr      Scrive_VAB
043300060612      * ?              /*---------------------- */
043400090227     c                   else
043500090227     c                   eval      errori_in_Wrt = 'S'
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
046500091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
046600090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
046700090603     C                             Bolle import in filiale - messaggio -
046800090603     C                             con UNB non presente in tabella PT controlla-
046900090603     C                             re EDI ricevuto su UPLOAD.'
047000090603     c                   exsr      invio_mail_AB
047100060621     c                   end
047200060612
047300091016     c                   if        EoFTivin = ' '
047400060612     c                   update    Tivin000
047500091016     c                   end
047600090211
047700090211      *** se non è riconosciuto l'UNB deve uscire direttamente
047800090211     c                   if        se_errore ='S'  and
047900090211     c                             tipo_seg = 'UNB'
048000090213     c                   eval      err_bloccante = 'S'
048100090211     c                   exsr      Msg_errato
048200090211     c                   LEAVE
048300090211     c                   endIF
048400090211      ***
048500060614     c                   endIF
048600060612
048700060612     c                   read      Tivin00r
048800091016     c                   if        %Eof(tivin00r)
048900091016     c                   move      'S'           EoFTivin
049000091016     c                   end
049100060612     C                   ENDdo
049200060612      **
049300060612     c                   endSR
049400100120      * ?------------------------------------------------------------------ */
049500100120      *? Controlla la trasmissione   se completa
049600100120      * ?------------------------------------------------------------------ */
049700100120     c     Check_Trasm   Begsr
049800100120
049900100120     C                   clear                   se_errore
050000100120     C                   clear                   Riga_iNIZIo       1
050100100120     C                   clear                   Riga_fINe         1
050200100120      ** primo  record
050300100120     c     *start        setll     tivin00r
050400100120      *
050500100120     c     rileggi       tag
050600100120     c                   read      tivin00r
050700100120     c                   if        %Eof(tivin00r)
050800100120     c                   move      'S'           EoFTivin
050900100120     c                   end
051000100120      *
051100100120     c                   if        not %eof(tivin00r)
051200100120      *
051300100120     c***********        movel(p)  VINDTA        dati
051400100120     c                   clear                   dati
051500100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
051600100120     c                   if        dati = *blank
051700100120      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
051800100120      * le leggo e le escludo.
051900100120     C                   eval      vinFLG = '1'
052000100120     c                   if        EoFTivin = ' '
052100100120     c                   update    Tivin000
052200100120     c                   end
052300100120     c                   goto      rileggi
052400100120     c                   end
052500100120      * ?              /*---------------------- */
052600100120     c                   exsr      Decod_Segmento
052700100120      * ?              /*---------------------- */
052800100120     c                   endif
052900100120      **
053000100120      ** ultimo record
053100100120     c     *hival        setll     tivin00r
053200100120     c     leggiAncora   tag
053300100120     c                   readp     tivin00r
053400100120     c                   if        %Eof(tivin00r)
053500100120     c                   move      'S'           EoFTivin
053600100120     c                   end
053700100120     c                   if        not %eof(tivin00r)
053800100120      *
053900100120     c***********        movel(p)  VINDTA        dati
054000100120     c                   clear                   dati
054100100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
054200100120     c                   if        dati = *blank
054300100120      * le ultime righe potrebbero essere vuote prima di finire il messaggio
054400100120      * le leggo e le escludo.
054500100120     C                   eval      vinFLG = '1'
054600100120     c                   if        EoFTivin = ' '
054700100120     c                   update    Tivin000
054800100120     c                   end
054900100120     c                   goto      leggiAncora
055000100120     c                   end
055100100120      * ?              /*---------------------- */
055200100120     c                   exsr      Decod_Segmento
055300100120      * ?              /*---------------------- */
055400100120     c                   endif
055500100120
055600100120      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
055700100120      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
055800100120      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
055900100120     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
056000100120      * ?-----> Errore
056100100120     C                   eval      se_errore = 'S'
056200100120     c                   end
056300100120
056400100120     c                   endSR
056500100120      * ?------------------------------------------------------------------ */
056600100120      *?    Decodifica il segmento ed importa i campi in formato DS
056700100120      * ?------------------------------------------------------------------ */
056800100120     c     Decod_SegmentoBegsr
056900100120      **
057000100120     c***********        movel(p)  VINDTA        dati
057100100120     c***********        eval      tipo_seg = %subst(vinDTA:1:3)
057200100120     c***********        eval      segmento = vinDTA
057300100120     c                   clear                   dati
057400100120     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
057500100120     c                   eval      tipo_seg = %subst(dati:1:3)
057600100120     c                   eval      segmento = dati
057700100120      *
057800100120      * Imposta il flag di Inizio Dettaglio
057900100120     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
058000100120     c                   eval      Inizio_Dettaglio ='S'
058100100120     c                   end
058200100120      *
058300140530      *    ci sono righe vuote dopo l'ultimo segmento UNZ
058400140530      *     LE DEVO IGNORARE senza dare ERRORI.
058500140530     c                   if        tipo_seg = *blank and trovato_UNZ='S'
058600140530     c                   goto      end_Decod
058700140530     c                   end
058800140530      *
058900140530      *
059000100120     c                   clear                   seg_imprevisto
059100100720     c                   clear                   trovato_seg
059200100716      *
059300100716     C                   clear                   keyUNBCLI
059400100716     C                   clear                   keyTIPOMSG
059500100716     C                   clear                   keyVERSION
059600100716     C                   clear                   keyRELEASE
059700100716     C                   clear                   keyAGENCY
059800100716     C                   clear                   keyASSOCIA
059900100720      *
060000100720      *  Chiamata generica x decodificare il singolo segmento letto
060100100720     c                   call      'TRTCT00R1'
060200100720      * input
060300100720     c                   parm                    tipo_seg
060400100720     c                   parm                    segmento
060500100720     C                   parm                    keyUNBCLI
060600100720     C                   parm                    keyTIPOMSG
060700100720     C                   parm                    keyVERSION
060800100720     C                   parm                    keyRELEASE
060900100720     C                   parm                    keyAGENCY
061000100720     C                   parm                    keyASSOCIA
061100100720      *output
061200100720     c                   parm                    Errore
061300100720     c                   parm                    DsGenerica
061400100720     c                   parm                    Valori_inErr
061500100720     c                   parm                    Descr_Errore
061600100720     c                   parm                    trovato_seg
061700100720      **--------
061800100720      *  Ritorna la DS GENERICA oppure l'errore
061900100120      **--------
062000100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
062100100120      * ?  Anche se non utilizzati.
062200100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
062300100120     c                   select
062400100720      *
062500100720     c                   when      trovato_seg ='N'
062600100720      *
062700100720     c                   move      'S'           seg_imprevisto
062800100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
062900100720     c                   exsr      Error_DET
063000100720     c                   goto      end_Decod
063100100720      **--------
063200100120      *
063300100120     c                   when      tipo_seg = 'UNA'
063400100720     c                   movel(p)  DsGenerica    EDS00UNA
063500100120      *
063600100120     c                   when      tipo_seg = 'UNB'
063700100720     c                   movel(p)  DsGenerica    EDS00UNB
063800100120     C                   EXSR      DATI_UNB
063900100120      * --------
064000100120     c                   when      tipo_seg = 'UNH'
064100100720     c                   movel(p)  DsGenerica    EDS00UNH
064200100120     C                   EXSR      DATI_UNH
064300100120      * --------
064400100120     c                   when      tipo_seg = 'BGM'
064500100720     c                   movel(p)  DsGenerica    EDS00BGM
064600100120      **--------
064700100120     c                   when      tipo_seg = 'DTM'
064800100720     c                   movel(p)  DsGenerica    EDS00DTM
064900100120     C                   EXSR      DATI_DTM
065000100120      * --------
065100100120     c                   when      tipo_seg = 'TSR'
065200100720     c                   movel(p)  DsGenerica    EDS00TSR
065300100120      **--------
065400100120     c                   when      tipo_seg = 'CNT'
065500100720     c                   movel(p)  DsGenerica    EDS00CNT
065600100120      **--------
065700100120     c                   when      tipo_seg = 'CUX'
065800100720     c                   movel(p)  DsGenerica    EDS00CUX
065900100120      * --------
066000100120     c                   when      tipo_seg = 'TOD'
066100100720     c                   movel(p)  DsGenerica    EDS00TOD
066200100120     C                   EXSR      DATI_TOD
066300100120      **--------
066400100120     c                   when      tipo_seg = 'RFF'
066500100720     c                   movel(p)  DsGenerica    EDS00RFF
066600100120     C                   EXSR      DATI_RFF
066700100120      **--------
066800100120     c                   when      tipo_seg = 'TDT'
066900100720     c                   movel(p)  DsGenerica    EDS00TDT
067000100120      **--------
067100100120     c                   when      tipo_seg = 'MOA'
067200100720     c                   movel(p)  DsGenerica    EDS00MOA
067300100120     C                   EXSR      DATI_MOA
067400100120      **--------
067500100120     c                   when      tipo_seg = 'FTX'
067600100720     c                   movel(p)  DsGenerica    EDS00FTX
067700100120     C                   EXSR      DATI_FTX
067800100120      **--------
067900100120     c                   when      tipo_seg = 'NAD'
068000100720     c                   movel(p)  DsGenerica    EDS00NAD
068100100120     C                   EXSR      DATI_NAD
068200100120      **--------
068300100120     c                   when      tipo_seg = 'CTA'
068400100720     c                   movel(p)  DsGenerica    EDS00CTA
068500100120     C                   EXSR      DATI_CTA
068600100120      **--------
068700100120     c                   when      tipo_seg = 'COM'
068800100720     c                   movel(p)  DsGenerica    EDS00COM
068900100120     C                   EXSR      DATI_COM
069000100120      **--------
069100100120     c                   when      tipo_seg = 'GID'
069200100720     c                   movel(p)  DsGenerica    EDS00GID
069300100120     C                   EXSR      DATI_GID
069400100120      **--------
069500100120     c                   when      tipo_seg = 'MEA'
069600100720     c                   movel(p)  DsGenerica    EDS00MEA
069700100120     C                   EXSR      DATI_MEA
069800100120      **--------
069900100120     c                   when      tipo_seg = 'PCI'
070000100720     c                   movel(p)  DsGenerica    EDS00PCI
070100100120     C                   EXSR      DATI_PCI
070200100120      **--------
070300100120     c                   when      tipo_seg = 'UNT'
070400100720     c                   movel(p)  DsGenerica    EDS00UNT
070500100120     C                   EXSR      DATI_UNT
070600100120      **--------
070700100120     c                   when      tipo_seg = 'UNZ'
070800100720     c                   movel(p)  DsGenerica    EDS00UNZ
070900100120     C                   EXSR      DATI_UNZ
071000100120      **--------
071100100120     c                   OTHER
071200100120      **--------
071300100120     c                   move      'S'           seg_imprevisto
071400100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
071500100120     c                   exsr      Error_DET
071600100120     c                   goto      end_Decod
071700100120      **--------
071800100120     c                   endSL
071900100120      **
072000100120     c     end_Decod     endSR
072100090211     C*----------------------------------------------------------------
072200090211     C*? AZZMSG - Azzera dati messaggio
072300090211     C*----------------------------------------------------------------
072400090211     C     AZZMSG        BEGSR
072500090211     C*
072600090211     C* Pulisco dati di work x foglio viaggio
072700090211     C                   MOVEL     *BLANKS       WNUMFV           35
072800090211     C                   MOVEL     *BLANKS       WNRCMR           35
072900090211     C                   Z-ADD     0             WDATPA            8 0
073000090211     C                   Z-ADD     0             WDATFV            8 0
073100090211     C                   Z-ADD     0             WHHNFV            4 0
073200090211     C                   Z-ADD     0             WDTCMR            8 0
073300090211     C                   Z-ADD     0             WHHCMR            4 0
073400090211      *
073500090211      * pulisce gli RFF+FF di testata
073600090211     C                   clear                   $CLKSC            7 0
073700090211     C                   clear                   $CLTAR            3
073800090211     C                   clear                   $CLLNP            3 0
073900090211     C                   clear                   $CLNRS            2 0
074000090211     C                   clear                   $CLCTM            2
074100090211     C                   clear                   $CLBS1            1
074200090211     C                   clear                   $CLfnsp           1            *blk/S
074300090211      *
074400090211     C*  Inizializzo variabile new documento
074500090211     C                   Z-ADD     0             KCL
074600090211     C                   Z-ADD     0             WW                3 0
074700090211     ** Inizializza campo di work
074800090211     c                   move      'N'           WGID99
074900090211      * per controllare se almeno un record è stato importato sul VAB
075000090211     c                   clear                   Almeno_Uno        1
075100090213     c                   clear                   Almeno_Un_Due     1
075200090211     c                   eval      esito  = '0'
075300090211     C*
075400090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
075500090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
075600090603     c                   eval      Dettaglio_NAD_destinatario = *blank
075700090603     c                   eval      NAD_destinatario_in_testata = *blank
075800090603     c                   eval      NAD_mittente_in_testata = *blank
075900090603     c                   eval      Inizio_Dettaglio = *blank
076000090603     C*
076100090603     c                   clear                   tes_VABrmo
076200090603     c                   clear                   tes_VABnmo
076300090603     c                   clear                   tes_VABcmo
076400090603     C*
076500090603     c                   clear                   tes_VABrsd
076600090603     c                   clear                   tes_VABrd2
076700090603     c                   clear                   tes_VABind
076800090603     c                   clear                   tes_VABlod
076900090603     c                   clear                   tes_VABnzd
077000090603     C*
077100090603     c                   clear                   tes_Valuta
077200090603     C*
077300090211     C                   ENDSR
077400090210     C*----------------------------------------------------------------
077500090210      *? DECODIFICA DATI UNB
077600090210     C*----------------------------------------------------------------
077700090210     C     DATI_UNB      BEGSR
077800090210      *
077900090209      *  INIZIALIZZA  dati fondamentali messaggio
078000090209     C                   clear                   EDIDSPT
078100090209     C                   clear                   EDIDSPU
078200090209     C                   clear                   EDIDSPV
078300090209     C                   clear                   wnrDOC           35
078400090209     C                   move      'N'           PT_ok             1
078500090209      ** ---------------------------------------------------------------
078600090209      * quindi 1° cosa:
078700090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
078800090209      *     senza questo il messaggio NON è trascodificabile
078900090209     C                   MOVEL     unb0004       WCOD             35
079000100114     C                   eval      wcod = %trim(WCOD) + ':' + %trim(unb0010)
079100090209     C                   MOVE      unb000720     WCOD
079200090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
079300090209     C                   Z-ADD     1             XX                3 0
079400090209     C     WCOD          LOOKUP    CPT(XX)                                32
079500090209      *
079600090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
079700090209     C                   If        *IN32 = *off
079800090209      * ******  Errore generale sul Messaggio
079900090209      * Msg di allerta Traduzione
080000090209     C                   EVAL      Indirizzi = CED_Bart
080100091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
080200090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
080300090209     C                             Bolle import in filiale - messaggio -
080400090209     C                             con UNB non presente in tabella PT controlla-
080500090209     C                             re EDI ricevuto su UPLOAD.'
080600090209      *
080700090209     c                   exsr      invio_mail
080800090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
080900090210     C                   eval      vinFLG = '2'
081000090210     C                   eval      se_errore   = 'S'
081100090210     c                   goto      end_UNB
081200090209      *
081300090209     C                   move      'N'           PT_ok             1
081400090209      *
081500090209      *             *------------------------------*
081600090209     C                   eLSe
081700090209      *             *------------------------------*
081800090209      * Se tutto OK su tab.PT:
081900090209     C                   move      'S'           PT_ok             1
082000090209      *
082100090209     C                   MOVEL     DPT(XX)       EDIDSPT
082200090209     C                   MOVEL     DPU(XX)       EDIDSPU
082300090209     C                   MOVEL     DPV(XX)       EDIDSPV
082400091016     c                   clear                   Mittente         35
082500091016     c                   clear                   Mitt_Qualifier    4
082600091016     c                   clear                   Id_Messaggio     14
082700090226     C                   eval      Mittente       = unb0004
082800090226     C                   eval      Mitt_qualifier = unb000720
082900090226     C                   eval      Id_Messaggio   = UNB0022
083000090209      *
083100090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
083200090209      *  ricevuti:
083300090209     c                   movel     §PVema        mail_ind         44
083400090209      *
083500090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
083600090209     c                   if        mail_ind <> *blank
083700090209     c                   exsr      imp_ADDR_mail
083800090209     c                   end
083900090209      *
084000090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
084100090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
084200090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
084300090209     C                   if        §PULUN = *blank
084400090209     C                   eval      §PULUN = '00'
084500090209     c                   end
084600090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
084700090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
084800090209      *
084900090209     C                   Z-ADD     §PTKSC        WKSC              7 0
085000090211     C                   MOVEL     §PTLNP        VABlnp_PT
085100090212     C                   MOVEL     §PTNRS        VABnrs_PT
085200090211     C                   MOVEL     §PTCTM        VABctm_PT
085300090209      *
085400090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
085500090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
085600090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
085700090209      *  i records trasmessi).
085800090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
085900090209      *
086000090209      *  In base al cod.trattamento merce reperisco tipo tratt.
086100090209     C                   CLEAR                   DS1B
086200090209     C                   Z-ADD     1             Y                 3 0
086300090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
086400090209     C     *IN32         IFEQ      '1'
086500090209     C                   MOVEL     DTM(Y)        DS1B
086600090209     C                   MOVEL     DS1B          WSVTRM           38
086700090209     C                   END
086800090209      *
086900090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
087000090209     C                   clear                   ACORAG
087100090209     C                   clear                   INDCAE
087200090209     C                   clear                   INDCAP
087300090209     C                   clear                   INDSTA
087400090209      * Decodifico partner
087500090209     C                   Z-ADD     1             KKUT
087600090209     C                   Z-ADD     KCI           KKCC
087700090209     C                   MOVE      §PTKSC        KKSC
087800090209     C     KACO          CHAIN     CNACO00F                           31
087900090209     C     KIND          CHAIN     CNIND00F                           31
088000090211      *
088100090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
088200090211      *   questa schiera è utilizzata anche per i clienti.
088300090211     C     §ptKSC        LOOKUP    KCL                                    39
088400090211     c                   If        *in39 = *off
088500090211     C                   add       1             WW
088600090211     C                   z-add     §ptKSC        KCL(WW)
088700090211     C                   Endif
088800090209      *
088900090209     C                   MOVEL     unb0017       WAA4              4 0
089000090209     C                   MOVE      WAA4          WAA2              2 0
089100090209     C                   MOVE      unb0017       WGG               2 0
089200090209     C                   MOVE      unb0017       WDTPRE            8 0
089300090209     C                   MOVE      unb0019       WHMPRE            4 0
089400090209     C                   MOVE      unb0017       WDTMSG            6 0
089500090209     C                   MOVE      WAA2          WDTMSG
089600090209     C                   MOVEL     WGG           WDTMSG
089700100113     c****
089800090209     C                   IF        unb0022 <> *blank
089900090209     C                   MOVEL     unb0022       WNRDOC
090000090211     C                   MOVEL     unb0022       WNUMFV
090100090211     C                   MOVEL     unb0022       WNRCMR
090200090209     c                   end
090300090209      *
090400090209     c                   Endif
090500090209      **
090600090210     c     end_UNB       endSR
090700090209      * ?================================================================== */
090800090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
090900090209      * ?================================================================== */
091000090227     C     Nuova_spediz  BEGSR
091100090227      **
091200090227      * Pulisco campi del file
091300090227     c                   z-add     vnrec         SAVNUM_RECord
091400090227     C                   CLEAR                   EDiVAB00
091500090227     C                   CLEAR                   EDiVAT00
091600090227     C                   CLEAR                   EDiVAD00
091700090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
091800090227     C                   MOVEL     'N'           WGID99            1
091900090227     C                   clear                   XY                5 0
092000090227     C                   clear                   §§                5 0
092100090227     C                   clear                   WS1496            5 0
092200090227     C                   clear                   WNSP             35
092300090227     C                   Z-ADD     *ZEROS        PVLB
092400090227     C                   MOVEA     *HIVAL        SGN
092500090227     C*  Azzero codice cliente - tariffa
092600090227     C                   Z-ADD     0             WTOCOC           16 0
092700090227     C                   Z-ADD     0             WTOPLC           16 2          ???
092800090227     C                   Z-ADD     0             WTOPNC           16 2          ???
092900090227     C*  Azzero codice cliente - tariffa
093000090227     C                   clear                   WCLKSC
093100090227     C                   clear                   WCLTAR
093200090227     C                   clear                   WCLLNP
093300090227     C                   clear                   WCLNRS
093400090227     C                   clear                   WCLCTM
093500090227     C                   clear                   WCLBS1
093600090227     C                   clear                   WCLfnsp                        *blk/S
093700090227     C                   clear                   WNOT1            35
093800090227     C                   clear                   WNOT2            35
093900090227     C*  Campi di work
094000090227     c                   clear                   wVabTIC
094100090227     c                   clear                   wri_vabtic        1
094200090227      *
094300090227     C* pulisce campi di work
094400090227     c                   clear                   watrde           35
094500090227     c                   clear                   wattel           25
094600090227     C                   clear                   WRMN              3
094700090227      *
094800090227      * Un errore nel dettaglio
094900090227     C                   clear                   Err_in_detta      1
095000090227     c                   clear                   errori_in_Wrt     1
095100090227     c                   clear                   se_trovato_NAD    1
095200090227      **
095300090227     c                   endSR
095400090227      * ?================================================================== */
095500090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
095600090227      * ?================================================================== */
095700090227     C     DATI_UNH      BEGSR
095800090227      *
095900090227      *  Prepara una nuova spedizione
096000090227      * ?                         --------------
096100090227     c                   exsr      Nuova_spediz
096200090227      * ?                         --------------
096300090211      *
096400090211      ** Riferimento del cliente
096500090211     C                   MOVEL     unh0062       WPRGSP           35
096600090211     C                   MOVEL     unh0062       WNSP             35
096700090211     C                   MOVEL     unh0062       WNUM             35
096800090211      * ?                                       --------
096900090211     C                   MOVEL     unh0062       VABRMA
097000090211      * ?                                       --------
097100090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
097200090211     C                   SUB       1             WLUNGH
097300090211     C                   EXSR      TSTNUM
097400090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
097500090211      * ?                                       --------
097600090211     C                   MOVEL     WNUMOK        VABRMN
097700090211      * ?                                       --------
097800090211     C                   END
097900090210      **
098000090303     C                   MOVEL     VABlnp_PT     VABLNP
098100090303     C                   MOVEL     VABnrs_PT     VABNRS
098200090303     C                   MOVEL     VABctm_PT     VABCTM
098300090303      **
098400090303      **
098500090210     c     end_UNH       endSR
098600090210      * ?================================================================== */
098700090601     C*? Imposta la VALUTA di eventuali C.O.D.
098800090210      * ?================================================================== */
098900090601     C     DATI_CUX      BEGSR
099000090210     C*
099100090603     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
099200090603     c                   if        cux6345 <> *blank
099300090603     C                   MOVEL     cux6345       Tes_Valuta
099400090603     c                   end
099500090601      **
099600090601     c     end_CUX       endSR
099700090601      * ?================================================================== */
099800090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
099900090601      * ?================================================================== */
100000090601     C     DATI_DTM      BEGSR
100100100113     C*
100200090209     C*  Se data documento  memorizzo numero e data x CMR
100300100114     C     dtm2005       IFEQ      '137'
100400090209     C                   MOVEL     dtm2380       WDATA            35            Data
100500090209     C                   MOVEL     dtm2379       WFORM             3            Formato
100600090209     C                   EXSR      CNVDAT
100700100119     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
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
101800100113      **
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
113400091203???? c                   movel(p)  TOD_char3     fld35a           35
113500091203???? C                   movel     UNB_Mittente  etbUNB
113600091203???? C                   movel(p)  'TOD'         etbSGM
113700091203???? C                   movel(p)  TOD_char3     etbVALSGM
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
116100110328      **
116200110328     c                   ENDif
116300090209      **
116400090210      **  Il tipo bolla non è presente
116500090210     c                   if        VABCBO = *blank
116600090210     C                   eval      vinMSG = 'TOD non rilevato'
116700090213     c                   exsr      Error_DET
116800090210     c                   goto      end_TOD
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
124900100113     C                   if        RFF1153 = 'CU ' and RFF1154 <> *blanks
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
126100100113     C                   movel     'CU '         WRMN
126200090210     C                   Endif
126300090210     C                   ENDIF
126400090210      *
126500090210      * NOn c'è l'identificativo bolla del cliente
126600091019     c                   if        (RFF1153 = Riferimento_Spedizione and
126700091019     c                              RFF1154 = *BLANKS)
126800091019     c                             or
126900100113     c                             (RFF1153 = 'CU ' and RFF1154 = *BLANKS)
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
129700090216     C     moa5004       div       1000          VABCAS
129800090216     C                   MOVEL     moa6345       VABVCA
129900100120     c                   if        Tes_valuta <> *blank and VABVCA = *blank
130000100120     C                   MOVEL     Tes_valuta    VABVCA
130100100120     c                   end
130200090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
130300090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
130400090216     C     §PUTPC        IFEQ      'P'
130500090216     C     moa5004       ANDGT     *ZEROS
130600090216     C                   MOVEL     'OM'          VABTIC
130700090216     C                   MOVEL     'OM'          WVABTIC
130800090216     C                   ENDIF
130900090216     C                   END
131000090216     C                   END
131100090216      *
131200090216     C                   END                                                    Imp.non numer.
131300090213      *
131400090213     c     end_MOA       endSR
131500090213      * ?================================================================== */
131600090213     C*?  FREE TEXT
131700090213      * ?================================================================== */
131800090213     C     DATI_FTX      BEGSR
131900090213      *
132000090216      * ? solo se si tratta di note SIN ICN o PAI non altro
132100090216     C                   DO        4             X
132200090216     C     D05(X)        IFNE      *BLANKS
132300090216     C     WNOT1         IFEQ      *BLANKS
132400090216     C                   MOVEL     D05(X)        WNOT1
132500090216     C                   MOVE      D05(X)        WNOT2
132600090216     C                   ELSE
132700090216     C     WNOT2         IFEQ      *BLANKS
132800090216     C                   MOVEL     D05(X)        WNOT2
132900090216     C                   END
133000090216     C                   END
133100090216     C                   END
133200090216     C                   END
133300090216     C*------>>
133400090216     C*  Decodifico tipo pagamento C/Assegno
133500090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
133600090216      *      (Problema di pulizia in presenza di + righe di testo)
133700090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
133800090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
133900090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
134000090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
134100090216     C                   if        wri_vabtic = *blank
134200090216     C                   MOVEL     *BLANKS       VABTIC
134300090216     c                   end
134400090216      **
134500090216     c                   clear                   trovato_TPI       1
134600091203      *
134700091203      * ?Se Natura Merce      .
134800091203     C     ftx4451       IFEQ      'AAA'                                        -- 01 --
134900091203     C                   MOVEL     D05(1)        VABnas
135000091203     c                   End
135100091203      *
135200090216      *
135300090216      * ?Se c'è il tipo Incasso
135400090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
135500090216      *
135600090216     C                   DO        4             X                              -- 02 --
135700090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
135800130311      **
135900130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
136000130311      **  per gestire Partner come AZKAR che ha codice "AAA"
136100130311     C*******            MOVEL     D05(X)        WCTC              2
136200130311     C                   MOVEL     D05(X)        WCTC             10
136300090216     C                   Z-ADD     1             Y
136400090216     c                   movel(p)  Wctc          fld35a           35
136500090216     c                   move      §puTC         fld35a
136600090216     C     fld35a        LOOKUP    CTC35(Y)                               33
136700090216     C   33              MOVEL     TPI(Y)        VABTIC
136800090216     c   33              move      'S'           trovato_TPI
136900090216     C                   END                                                    -- 03 --
137000090216     C                   ENDdo                                                  -- 02 --
137100090216      *
137200090216     C                   ELSE                                                   -- 01 --
137300090216      *
137400090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
137500090216      *    Deve essere impostato comunque da testata
137600090216     c                   if        vabtic = *blank
137700090216     C                   MOVEL     wVabTIC       VABTIC
137800090216     c                   end
137900090216      *
138000090216     C                   END                                                    -- 01 --
138100090216      *
138200090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
138300090216      *    essere presenti altri record Flat con altre informazioni
138400090216      *    che potrebbero abblancare il Tipo incasso precedentemente
138500090216      *    decodificato
138600090216     c                   if        Trovato_TPI = 'S' and
138700090216     c                                 wri_vabtic = *blank
138800090216     c                   eval      wri_vabtic = 'S'
138900090216     c                   end
139000090213      *
139100090213     c     end_FTX       endSR
139200090213      * ?================================================================== */
139300090213     C*?  Anagrafica del mittente e del destinatario
139400090213      * ?================================================================== */
139500091016     C     DATI_NAD      BEGSR
139600090213      *
139700090210      * Dati destinatario
139800100113     C                   IF        nad3035 = 'ST' or
139900090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
140000090227      *
140100090227      * Se trovato un indirizzo di destinatario
140200090227     c                   eval       se_trovato_NAD ='S'
140300090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
140400090210      *
140500090210     C* Reperisco nazione corrispondente destinatario
140600090210     C                   Z-ADD     1             YY                3 0
140700090210     c                   eval      *in32 = *off
140800090210     C                   if        nad3207 <> *BLANKS
140900090210     C     nad3207       LOOKUP    ISO(YY)                                32
141000090210     C                   endif
141100090210      *
141200100318     C* Imposto dati destinatario --> Indirizzo e località
141300100114     C                   MOVEL     nad31241      VABIND
141400100318      *
141500100318     C* Se non è vuoto lo imposto dalla posizione +1 in VABIND o VABLOD.
141600100318     c                   if        NAD31242 <> *blanks and NAD31242 <> *loval
141700100318      *
141800100318     C* Verifico se c'è dello spazio per nella località
141900100318     C     '    '        SCAN      VABIND        WI                4 0    90
142000100318      *  Se c'è posto nell'indirizzo cerca di aggiungerlo lì
142100100318      *   altrimenti prova nel campo località.
142200100319     c                   clear                   IND
142300100318     c                   if        *in90
142400100319     C                   MOVEA     VABIND        IND
142500100318     C                   ADD       1             WI
142600100319     C                   MOVEA     NAD31242      IND(WI)
142700100319     C                   MOVEA     IND           VABIND
142800100318     c                   else
142900100318      *
143000100318     C* Verifico se c'è dello spazio per nella località
143100100318     C     '    '        SCAN      VABLOD        WI                       90
143200100319     c                   clear                   IND
143300100318     c                   if        *in90
143400100319     C                   MOVEA     VABLOD        IND
143500100318     C                   ADD       1             WI
143600100319     C                   MOVEA     NAD31242      IND(WI)
143700100319     C                   MOVEA     IND           VABLOD
143800100318     c                   end
143900100318      *
144000100318     c                   end
144100100318      *
144200100318     C                   ENDif
144300100318      *
144400100318     C                   if        nad31243 <> *LOVAL
144500100318     C                   MOVEL(p)  nad31243      VABLOD
144600100318     C                   endif
144700100318      *
144800091019????  * DAL PARTY NAME
144900091019???? C* Imposto dati destinatario
145000091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
145100091019???? C                   MOVEL     nad30361      VABRSD
145200091019???? C                   if        nad30362 <> *LOVAL
145300091019???? C                   MOVEL     nad30362      VABRD2
145400091019???? C                   endif
145500091019???? C                   end
145600090210      *
145700090210     C                   If        *in32 = *on
145800090210     C                   MOVEL     C15(YY)       VABNZD
145900090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
146000090210     c                   Else
146100090210     C                   MOVEL     *BLANKS       VABNZD
146200090210     C                   Z-ADD     0             WFLCPF
146300090210     c                   Endif
146400090210      * Converto CAP
146500090210     C                   EXSR      CNVCAP
146600090603     C*
146700090603     C* Se in testata
146800090603     c                   if        Inizio_dettaglio = *blank
146900090603     c                   eval       tes_VABrsd = VABrsd
147000090603     c                   eval       tes_VABrd2 = VABrd2
147100090603     c                   eval       tes_VABind = VABind
147200090603     c                   eval       tes_VABlod = VABlod
147300090603     c                   eval       tes_VABnzd = VABnzd
147400090603     c                   eval      NAD_destinatario_in_testata = 'S'
147500090603     c                   end
147600090603     C*
147700090210     C                   ENDIF
147800090210      *
147900090210      * Dati mittente
148000100113     C                   IF        nad3035 = 'SF'
148100090210     C                   movel     nad30361      VABRMO
148200090210      *
148300090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
148400090210     C                   movel     §PTNAZ        VABNMO
148500090210      *
148600090210     C                   IF        nad3207 <> *blanks
148700090210     C                   eval      YY = 1
148800090210     C     nad3207       LOOKUP    ISO(YY)                                32
148900090210     C                   if         *in32 = *on
149000090210     C                   MOVEL     C15(YY)       VABNMO
149100090210     C                   endif
149200090210     C                   ENDIF
149300090210      *
149400090210      * Normalizzo CAP mittente originale
149500090210     C                   clear                   T
149600090210     C                   clear                   S
149700090210     C                   MOVEA     *BLANKS       CAPM
149800090210     C                   MOVEA     nad3251       CAP9
149900090210     C                   DO        9             T
150000090210     C     CAP9(T)       IFNE      *BLANKS
150100090210     C                   ADD       1             S
150200090210     C                   MOVE      CAP9(T)       CAPM(S)
150300090210     C                   ENDIF
150400090210     C                   ENDDO
150500090210     C*
150600090210      *     Controllo validità CAP
150700090210     C                   MOVEA     CAPM          VABCMO
150800090210      *
150900090210     C                   CLEAR                   TISI95DS
151000090210     C                   MOVEL     VABCMO        I95CAP
151100090210     C                   MOVEL     VABNMO        I95NAR
151200090210     C                   MOVEL     WOGGI         I95DAT
151300090210     C                   MOVEL     '3'           I95TCN
151400090210     C                   CALL      'TISI95R'
151500090210     C                   PARM                    TISI95DS
151600090210      *     Errore
151700090210     C     O95ERR        IFNE      *BLANKS
151800090210     C                   MOVEL     INDCAE        VABCMO
151900090210     C                   MOVEL     §PTNAZ        VABNMO
152000090210     C                   END
152100090603      *
152200090603     C* Se in testata
152300090603     c                   if        Inizio_dettaglio = *blank
152400090603     c                   eval      tes_VABrmo = VABrmo
152500090603     c                   eval      tes_VABnmo = VABnmo
152600090603     c                   eval      tes_VABcmo = VABcmo
152700090603     c                   eval      NAD_mittente_in_testata = 'S'
152800090603     c                   end
152900090210      *
153000090210     C                   END
153100090210      *
153200090210      * NOn c'è l'identificativo bolla del cliente
153300090210     C                   if          vabRSD = *blank or
153400090210     C                               vabIND = *blank or
153500090210     C                               vabLOD = *blank
153600100113     C                   IF        nad3035 = 'ST'
153700090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
153800090213     c                   exsr      Error_DET
153900090210     c                   goto      end_NAD
154000090210     C                   ENDIF
154100090211     C                   ENDIF
154200090210      *
154300090210     c     end_NAD       endSR
154400090210      * ?================================================================== */
154500090210     C*?  Contatto a cui far riferimento
154600090210      * ?================================================================== */
154700090210     C     DATI_CTA      BEGSR
154800090210      *
154900090210      *  memorizza il riferimento di consegna x il destinatario
155000090210      *  memorizza il riferimento di consegna nr.telefonico
155100090210     C     cta3412       ifne      *BLANKS
155200090210     c                   movel     cta3412       watrde
155300090210     c                   end
155400090210      *
155500090210     c                   endSR
155600090210      * ?================================================================== */
155700090210     C*?  Contatto telefonico  a cui far riferimento
155800090210      * ?================================================================== */
155900090210     C     DATI_COM      BEGSR
156000090210      *
156100090210     C     com3148       ifne      *BLANKS
156200090210     c                   movel     com3148       wattel
156300090210     c                   end
156400090210      *
156500090209     C                   ENDSR
156600090210      * ?================================================================== */
156700090210     C*?  Dati di dettaglio della spedizione e dei colli
156800090210      * ?================================================================== */
156900090210     C     DATI_GID      BEGSR
157000090210      *
157100090210     C* Se ho totali per spedizione metto in campi VAB
157200090210     C     gid1496       IFEQ      99999
157300090210     C     gid1496       orEQ      9999
157400090210     C                   MOVEL     'S'           WGID99
157500090210     C                   z-add     gid722420     VABNCL
157600090210     C                   ELSE
157700090210     C     WGID99        IFEQ      'N'
157800090210     C                   z-add     gid722420     WTNCL             5 0
157900090210     C                   ADD       WTNCL         VABNCL
158000090210     C                   END
158100090210     C                   END
158200090212      *
158300090210     C                   ADD       VABNCL        WTOCOC           16 0
158400090210      *
158500090210     c     end_GID       endSR
158600090210      * ?================================================================== */
158700090210     C*?  Dati di dettaglio misure spedizione e colli
158800090210      * ?================================================================== */
158900090210     C     DATI_MEA      BEGSR
159000090210      *
159100090210     C*  Peso
159200090210     C     mea6311       IFEQ      'WT '
159300090210     C     mea6411       ANDEQ     'KGM'
159400090210      *
159500090210     C*  Controllo se ho valore totale o parziale
159600090210      *   sempre il Lordo
159700091019???? C     mea6313       IFEQ      'G  '
159800100114???? C     mea6313       oreq      'AAD'
159900090210     C     WS1496        IFEQ      99999
160000090210     C     WS1496        orEQ      9999
160100091019???? C                   move      mea6314       mea6314_2        18 2          Peso
160200091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
160300090210     C                   ELSE
160400091019???? C     WGID99        IFEQ      'N'
160500091019???? C                   move      mea6314       mea6314_2                      Peso
160600091019???? C                   ADD       mea6314_2     VABPKB                         Peso
160700090210     C                   END
160800090210     C                   END
160900091203     C                   END
161000090210      *
161100091019???? C     mea6313       IFEQ      'G  '
161200091204???? C     mea6313       oreq      'AAE'
161300091019???? C                   move      mea6314       mea6314_2                      Peso
161400091019???? C                   ADD       mea6314_2     WTOPLC
161500090210     C                   END
161600091019???? C     mea6313       IFEQ      'N  '
161700091204???? C     mea6313       oreq      'AAF'
161800091019???? C                   move      mea6314       mea6314_2                      Peso
161900091019???? C                   ADD       mea6314_2     WTOPNC
162000090210     C                   END
162100090210     C                   END
162200090210      ***
162300090210     C*  Peso in Grammi  se abilitato a utilizzarlo
162400090210      ***
162500090210     C     §puGR         IFeq      'S'
162600090210     C     mea6311       IFEQ      'WT '
162700090210     C     mea6411       ANDEQ     '163'
162800090210      *
162900090210     C*  Controllo se ho valore totale o parziale
163000091019???? C     mea6313       IFEQ      'G  '
163100091204???? C     mea6313       oreq      'AAE'
163200091019???? C     WS1496        IFEQ      99999
163300091019???? C     WS1496        orEQ      9999
163400091019???? c                   move      mea6314       mea6314_2
163500091019???? C     mea6314_2     div       1000          VABPKB                         Peso
163600090210     C                   ELSE
163700091019???? C     WGID99        IFEQ      'N'
163800091019???? c                   move      mea6314       mea6314_2
163900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
164000091019???? C                   add       Peso_inKG     VABPKB                         Peso
164100090210     C                   END
164200090210     C                   END
164300090210     C                   END
164400090210      *
164500091019???? C     mea6313       IFEQ      'G  '
164600091204???? C     mea6313       oreq      'AAE'
164700091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
164800091019???? C                   ADD       Peso_inKG     WTOPLC
164900090210     C                   END
165000091019???? C     mea6313       IFEQ      'N  '
165100091204???? C     mea6313       oreq      'AAF'
165200091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
165300091019???? C                   ADD       Peso_inKG     WTOPNC
165400090210     C                   END
165500090210     C                   END
165600090210     C                   END
165700090210      *  Volume
165800091019???? C     mea6311       IFEQ      'VOL'
165900091019???? C     mea6411       ANDEQ     'MTQ'
166000090210      *  Controllo se ho valore totale o parziale
166100090210     C     WS1496        IFEQ      99999
166200090210     C     WS1496        orEQ      9999
166300091019???? C                   move      mea6314       mea6314_2        18 2          Volume
166400091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
166500091019???? C                   Z-ADD     mea6314_2     PVLB
166600090210     C                   ELSE
166700091019     C     WGID99        IFEQ      'N'
166800091019???? C                   move      mea6314       mea6314_2        18 2          Volume
166900091019???? C                   ADD       mea6314_2     VABVLB                         Volume
167000091019???? C                   ADD       mea6314_2     PVLB
167100090210     C                   END
167200090210     C                   END
167300090210     C                   END
167400090210      *
167500090210     C* Ricerco CAP
167600090210     C     WCAP          IFNE      *BLANKS
167700090210     C     mea6311       IFEQ      'WT '
167800090210      *
167900090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
168000090210     C                   CLEAR                   FNLV55DS
168100090210     C                   MOVEL     'P'           D55TPT
168200090210     C                   MOVEL     WOGGI         D55DRF
168300090210     C                   MOVEL     VABLNP        D55LNP
168400090210     C                   MOVEL     VABLNP        D55LIN
168500090210     C                   CALL      'FNLV55R'
168600090210     C                   PARM                    FNLV55DS
168700090210     C                   MOVE      D55TFP        COMTFP            3 0
168800090210      *
168900090210     C*  PASSO IL TERMINAL PARTENZA
169000090210     C                   CLEAR                   TISI95DS
169100090210     C                   Z-ADD     COMTFP        I95TFP
169200090210     C                   MOVEL     VABTSP        I95TSP
169300090210     C                   MOVEL     'S'           I95FRE
169400090210     C                   MOVEL     '3'           I95TCN
169500090210     C                   MOVEL     WCAP          I95CAP
169600090210     C                   MOVEL     VABNZD        I95NAR
169700090210     C                   MOVEL     VABLOD        I95LOC
169800090210     C                   MOVEL     VABIND        I95IND
169900090210      *
170000090210     C* Se gestita seconda linea di arrivo passo peso - volume
170100090210     C* e numero colli effettivo
170200090210     C     §PULNA        IFEQ      'S'
170300090210     C                   Z-ADD     VABPKB        I95LKG
170400090210     C                   Z-ADD     VABVLB        I95LMC
170500090210     C                   ELSE
170600090210     C* Altrmenti passo 1 Kg e 0,001
170700090210     C***                  Z-ADD1         I95LKG
170800090210     C***                  Z-ADD0,001     I95LMC
170900090210     C                   END
171000090210     C*
171100090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
171200090210     C                   SELECT
171300090210     C     VABCBO        WHENEQ    '1 '
171400090210     C                   MOVEL     *BLANKS       I95FCA
171500090210     C                   MOVEL     'F'           I95TPO
171600090210     C     VABCBO        WHENEQ    '2 '
171700090210     C                   MOVEL     *BLANKS       I95FCA
171800090210     C                   MOVEL     'A'           I95TPO
171900090210     C     VABCBO        WHENEQ    '4 '
172000090210     C                   MOVEL     'S'           I95FCA
172100090210     C                   MOVEL     'F'           I95TPO
172200090210     C     VABCBO        WHENEQ    '6 '
172300090210     C                   MOVEL     'S'           I95FCA
172400090210     C                   MOVEL     'A'           I95TPO
172500090210     C                   OTHER
172600090210     C                   MOVEL     'F'           I95TPO
172700090210     C     VABCAS        IFGT      0
172800090210     C                   MOVEL     'S'           I95FCA
172900090210     C                   ELSE
173000090210     C                   MOVEL     *BLANKS       I95FCA
173100090210     C                   END
173200090210     C                   ENDSL
173300090210     C*
173400090210     C                   CALL      'TISI95R'
173500090210     C                   PARM                    TISI95DS
173600090210     C* Reperisco i dati da CAP
173700090210     C                   MOVEL     O95PRV        VABPRD
173800100114      ***
173900100114      *** dovendo accorpare le spedizioni da Conferma x CMR
174000100114      ***  non deve impostare l'instradamento.
174100100114     C***                MOVEL     O95LNA        VABLNA
174200100114     C***                MOVEL     O95ZNC        VABZNC
174300090210     C                   MOVEL     WCAP          VABCAD
174400090210     C*
174500090210     C     O95ERR        IFNE      *BLANKS
174600090210     C     O95FIT        ANDEQ     'S'
174700090210     C*  Cap non trovato
174800090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
174900090213     c                   exsr      Error_DET
175000090210     C                   goto      end_MEA
175100090210     C                   END
175200090210     C                   END
175300090210      *
175400090210     C                   ELSE
175500090210      *
175600090210     C                   CLEAR                   VABPRD
175700090210     C                   CLEAR                   VABLNA
175800090210     C                   CLEAR                   VABZNC
175900090210     C                   CLEAR                   VABCAD
176000090210     C*  Cap non trovato
176100090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
176200090213     c                   exsr      Error_DET
176300090210     C                   goto      end_MEA
176400090210     C                   END
176500090210      *
176600090210     c     end_MEA       endSR
176700090210      * ?================================================================== */
176800090210     C*?  Dati di dettaglio Codice a barre Segnacolli
176900090210      * ?================================================================== */
177000090210     C     DATI_PCI      BEGSR
177100090210      *
177200090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
177300090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
177400090210     C     §1BF12        IFEQ      'E'
177500090210     C                   EXSR      SEGNEE
177600090210     C                   ELSE
177700090210      *
177800090210      *  La prima volta controllo congruità numero segnacollo
177900090210     C     §PUBS1        IFNE      *BLANKS
178000090210     C     VABnrs        ANDNE     0
178100090210      *  Se il codice trattamento merce prevede che segnacollino i
178200090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
178300090210     C     §1BFG7        IFEQ      'S'
178400090210     C     §1BFG1        OREQ      'S'
178500090210      *  calcolo segnacollo
178600090210     C                   EXSR      SGNELA
178700090210     C                   END
178800090210     C                   END
178900090210      *
179000090210     C                   END
179100090210      *
179200090210     c     end_PCI       endSR
179300090211     C*----------------------------------------------------------------
179400090211      *? dati finali del dettaglio    UNT
179500090211     C*----------------------------------------------------------------
179600090211     C     DATI_UNT      BEGSR
179700090211      *
179800090213     c                   eval      NUM_RECord = vnREC
179900090213     c                   z-add     vnrec         last_RECord
180000090211      **
180100090211     c                   endSR
180200090211     C*----------------------------------------------------------------
180300090211      *? dati finali     UNT
180400090211     C*----------------------------------------------------------------
180500090211     C     DATI_UNZ      BEGSR
180600090211      *
180700140530     c                   eval      trovato_UNZ = 'S'
180800090211      **
180900090211     c                   endSR
181000090210      *----------------------------------------------------------------
181100090210      *? Input segnacolli su schiera di 20 elementi
181200090210      *----------------------------------------------------------------
181300090210     C     INP_Segnacol  BEGSR
181400090210      *
181500090210     c                   clear                   quanti_PCI        3 0
181600090210     c                   if        §puEL1 = 0
181700090210     c                   z-add     10            quanti_PCI
181800090210     c                   else
181900090210     c                   z-add     §puEL1        quanti_PCI
182000090210     c                   end
182100090210      *
182200090210      *  Pulisce la schiera dei segnacolli da elaborare
182300090210     c                   clear                   x30
182400090210     c                   z-add     0             Nr_x30            3 0
182500090210      *
182600090210      *   riporto tutto sulla schiera generica X30
182700090210      *
182800090210     c                   do        10            limite            3 0
182900090210      * Limita quanti elementi passati in ogni PCI
183000090210      *   devono essere considerati
183100090210     c                   if        limite > quanti_PCI
183200090210     c                   Leave
183300090210     c                   end
183400090210      *
183500090210      *  Carica la schiera generale di tutti i segnacolli
183600090210     c                   if        D30_1(limite) <> *blank
183700090210     c                   add       1             Nr_x30
183800090210     c                   eval      X30(Nr_x30) = D30_1(limite)
183900090210     c                   end
184000090210     c                   enddo
184100090210      *
184200090210     C                   ENDSR
184300090210      *----------------------------------------------------------------
184400090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
184500090210      *----------------------------------------------------------------
184600090210     C     SEGNEE        BEGSR
184700090210      *
184800090210     c                   exsr      INP_Segnacol
184900090210      *
185000090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
185100090210     C                   DO        10            x                              --- 01 ---
185200090210      *
185300090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
185400090210     C     x30(X)        ANDNE     *LOVAL
185500090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
185600090210     C                   ADD       1             §§
185700090210     C                   MOVEL     x30(X)        SGE(§§)
185800090210     C                   END                                                    --- 03 ---
185900090210      *
186000090210     C                   END                                                    --- 02 ---
186100090210      *
186200090210     C                   ENDSR
186300090210     C*----------------------------------------------------------------
186400090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
186500090210     C*----------------------------------------------------------------
186600090210     C     SGNELA        BEGSR
186700090210      *
186800090210     c                   exsr      INP_Segnacol
186900090210     C*
187000090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
187100090210     C*  in base al numero elementi validi carico sgn.
187200090210     C                   DO        10            X                              --- 01 ---
187300090210     C*
187400090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
187500090210     C     x30(X)        ANDNE     *LOVAL
187600090210     C*
187700090210     C                   CLEAR                   DSSGN
187800090210     C*  Controllo se devo ricevere la linea di partenza
187900090210     C                   MOVEL     'N'           WERRO             1
188000090210     C     §PULP1        IFGT      0                                            --- 03 ---
188100090210     C                   EXSR      REPLNP
188200090210     C                   END                                                    --- 03 ---
188300090210     C*  Controllo se devo ricevere la linea di arrivo
188400090210     C     §PULA1        IFGT      0                                            --- 03 ---
188500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
188600090210     C                   EXSR      REPLNA
188700090210     C                   END                                                    --- 03 ---
188800090210     C*  Controllo se devo ricevere il numero di serie
188900090210     C     §PUNR1        IFGT      0                                            --- 03 ---
189000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189100090210     C                   EXSR      REPNRS
189200090210     C                   END                                                    --- 03 ---
189300090210     C*  Controllo se devo ricevere il numero segnacollo
189400090210     C     §PUSG1        IFGT      0                                            --- 03 ---
189500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189600090210     C                   EXSR      REPSGN
189700090210     C                   END                                                    --- 03 ---
189800090210     C*  Controllo se devo ricevere la zona di consegna
189900090210     C     §PUZN1        IFGT      0                                            --- 03 ---
190000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
190100090210     C                   EXSR      REPZNC
190200090210     C                   END                                                    --- 03 ---
190300090210     C*  Imposto da VAB i dati a zero se mi è stato passato
190400090210     C*  numero segnacollo valido
190500090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
190600090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
190700090210     C                   Z-ADD     VABLNP        SGNLNP
190800090210     C                   END                                                    --- 04 ---
190900090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
191000090210     C                   Z-ADD     VABLNA        SGNLNA
191100090210     C                   END                                                    --- 04 ---
191200090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
191300090210     C                   Z-ADD     VABNRS        SGNNRS
191400090210     C                   END                                                    --- 04 ---
191500090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
191600090210     C                   Z-ADD     VABZNC        SGNZNC
191700090210     C                   END                                                    --- 04 ---
191800090210      *
191900090210     C*  mi reimposto LNA e zona da segnacollo
192000100114      ***
192100100114      *** dovendo accorpare le spedizioni da Conferma x CMR
192200100114      ***  non deve impostare l'instradamento.
192300100114     C***                Z-ADD     SGNLNA        VABLNA
192400100114     C***                Z-ADD     SGNZNC        VABZNC
192500100114      ***
192600090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
192700090210     C                   ADD       1             XY
192800090210     C                   MOVE      SGNNSC        SGN(XY)
192900090210     C                   END                                                    --- 03 ---
193000090210     C*
193100090210     C                   END                                                    --- 02 ---
193200090210     C*
193300090210     C                   END                                                    --- 01 ---
193400090210      *
193500090210     C                   ENDSR
193600090210     C*----------------------------------------------------------------
193700090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
193800090210     C*----------------------------------------------------------------
193900090210     C     REPLNP        BEGSR
194000090210     C*
194100090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
194200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
194300090210     C*  dal numero segnacollo passatomi dal partner
194400090210     C                   Z-ADD     §PULP1        WP                2 0
194500090210     C     3             SUBST     x30(X):WP     WLNP              3
194600090210     C*
194700090210     C*  controllo che la lnp calcolata sia numerica
194800090210     C                   TESTN                   WLNP                 98
194900090210     C*
195000090210     C*  se è numerica la carico
195100090210     C     *IN98         IFEQ      '1'
195200090210     C                   MOVE      WLNP          SGNLNP
195300090210     C                   ELSE
195400090210     C*  se non è numerica stampo errore
195500090210     C                   MOVEL     'S'           WERRO             1
195600090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
195700090213     c                   exsr      Error_DET
195800090210     C                   goto      end_LNP
195900090210     C                   END
196000090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
196100090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
196200090210     C     SGNLNP        IFNE      VABLNP
196300090210     C                   MOVEL     'S'           WERRO             1
196400090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
196500090210     c                             a della Bolla'
196600090213     c                   exsr      Error_DET
196700090210     C                   goto      end_LNP
196800090210     C                   END
196900090210     C*
197000090210     C     end_LNP       ENDSR
197100090210     C*----------------------------------------------------------------
197200090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
197300090210     C*----------------------------------------------------------------
197400090210     C     REPLNA        BEGSR
197500090210     C*
197600090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
197700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
197800090210     C*  dal numero segnacollo passatomi dal partner
197900090210     C                   Z-ADD     §PULA1        WP                2 0
198000090210     C     3             SUBST     x30(X):WP     WLNA              3
198100090210     C*  controllo che la lna calcolata sia numerica
198200090210     C                   TESTN                   WLNA                 98
198300090210     C*  se è numerica la carico
198400090210     C     *IN98         IFEQ      '1'
198500090210     C                   MOVE      WLNA          SGNLNA
198600090210     C                   ELSE
198700090210     C*  se non è numerica stampo errore
198800090210     C                   MOVEL     'S'           WERRO             1
198900090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
199000090213     c                   exsr      Error_DET
199100090210     C                   goto      end_LNA
199200090210     C                   END
199300090210     C*
199400090210     C     end_LNA       ENDSR
199500090210     C*----------------------------------------------------------------
199600090210     C*? REPNRS - Reperisce numero serie
199700090210     C*----------------------------------------------------------------
199800090210     C     REPNRS        BEGSR
199900090210     C*
200000090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
200100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
200200090210     C*  dal numero segnacollo passatomi dal partner
200300090210     C                   Z-ADD     §PUNR1        WP                2 0
200400090210     C     2             SUBST     x30(X):WP     WNRS              2
200500090210     C*  controllo che il nrs calcolata sia numerico
200600090210     C                   TESTN                   WNRS                 98
200700090210     C*  se è numerica la carico
200800090210     C     *IN98         IFEQ      '1'
200900090210     C                   MOVE      WNRS          SGNNRS
201000090210     C                   ELSE
201100090210     C*  se non è numerico stampo errore
201200090210     C                   MOVEL     'S'           WERRO             1
201300090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
201400090213     c                   exsr      Error_DET
201500090210     C                   goto      end_NRS
201600090210     C                   END
201700090210      *
201800090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
201900090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
202000090210     C     SGNNRS        IFNE      VABNRS
202100090210     C                   MOVEL     'S'           WERRO             1
202200090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
202300090210     c                             o della Bolla'
202400090213     c                   exsr      Error_DET
202500090210     C                   goto      end_NRS
202600090210     C                   END
202700090210     C*
202800090210     C     end_NRS       ENDSR
202900090210     C*----------------------------------------------------------------
203000090210     C*? REPZNC - Reperisce zona consegna
203100090210     C*----------------------------------------------------------------
203200090210     C     REPZNC        BEGSR
203300090210     C*
203400090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
203500090210     C*  (a partire dalla posizione iniziale indicata in tabella)
203600090210     C*  dal numero segnacollo passatomi dal partner
203700090210     C                   Z-ADD     §PUZN1        WP                2 0
203800090210     C     2             SUBST     x30(X):WP     WZNC              2
203900090210     C*  controllo che la zona calcolata sia numerica
204000090210     C                   TESTN                   WZNC                 98
204100090210     C*  se è numerica la carico
204200090210     C     *IN98         IFEQ      '1'
204300090210     C                   MOVE      WZNC          SGNZNC
204400090210     C                   ELSE
204500090210     C*  se non è numerica stampo errore
204600090210     C                   MOVEL     'S'           WERRO             1
204700090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
204800090213     c                   exsr      Error_DET
204900090210     C                   goto      end_ZNC
205000090210     C                   END
205100090210     C*
205200090210     C     end_ZNC       ENDSR
205300090210     C*----------------------------------------------------------------
205400090210     C*? REPSGN - Reperisce numero segnacollo
205500090210     C*----------------------------------------------------------------
205600090210     C     REPSGN        BEGSR
205700090210     C*
205800090210     C*  se viene passata nr.segnacollo estraggo sottostringa
205900090210     C*  lunga 7 chr (a partire dalla posizione iniziale
206000090210     C*  indicata in tabella)
206100090210     C*  dal numero segnacollo passatomi dal partner
206200090210     C                   Z-ADD     §PUSG1        WP                2 0
206300090210     C     7             SUBST     x30(X):WP     WSGN              7
206400090210     C*  controllo che il nr.segnacollo calcolato sia numerico
206500090210     C                   TESTN                   WSGN                 98
206600090210     C*  se è numerico lo carico
206700090210     C     *IN98         IFEQ      '1'
206800090210     C                   MOVE      WSGN          SGNNSC
206900090210     C                   ELSE
207000090210     C*  se non è numerica stampo errore
207100090210     C                   MOVEL     'S'           WERRO             1
207200090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
207300090213     c                   exsr      Error_DET
207400090210     C                   goto      end_SGN
207500090210     C                   END
207600090210     C*
207700090210     C     end_SGN       ENDSR
207800051205      *----------------------------------------------------*
207900051205      *   DEFINIZIONE CHIAVI                               *
208000051205      *----------------------------------------------------*
208100051205     C     *INZSR        BEGSR
208200051205      *
208300991129     c     *ENTRY        PLIST
208400060613     C                   parm                    esito             1
208500971215      *
208600050112     C     KTABel        KLIST
208700050112     C                   KFLD                    TBLKUT
208800050112     C                   KFLD                    TBLCOD
208900050112     C                   KFLD                    TBLKEY
209000090210      *
209100090210     C     K_TAB1L       KLIST
209200090213     C                   KFLD                    etbUNB
209300090213     C                   KFLD                    etbSGM
209400090213     C                   KFLD                    etbVALSGM
209500090209     C*
209600090209     C* Definisco chiavi di accesso
209700090209     C     KTAB1         KLIST
209800090209     C                   KFLD                    KKUT
209900090209     C                   KFLD                    KCOD
210000090209     C*
210100090209     C     KNUF          KLIST
210200090209     C                   KFLD                    KAAA
210300090209     C                   KFLD                    KCNU
210400090209     C                   KFLD                    KFIL
210500090209      *
210600090209     C     KVAB1         KLIST
210700090209     C                   KFLD                    KCCM
210800090209     C                   KFLD                    KCMR
210900090209     C                   KFLD                    KCNT
211000090209     C                   KFLD                    KAAS
211100090209     C                   KFLD                    KLNP
211200090209     C                   KFLD                    KNRS
211300090209     C                   KFLD                    KNSP
211400090209      *
211500090209     C     KVAB2         KLIST
211600090209     C                   KFLD                    KCCM
211700090209     C                   KFLD                    KCMR
211800090209      *
211900090209     C     KRDE          KLIST
212000090209     C                   KFLD                    KAAS
212100090209     C                   KFLD                    KLNP1
212200090209     C                   KFLD                    KNRS
212300090209     C                   KFLD                    KNSP
212400090209     C                   KFLD                    KNMC
212500090209     C                   KFLD                    KNRP
212600090209      *
212700090209     C     KACO          KLIST
212800090209     C                   KFLD                    KKUT
212900090209     C                   KFLD                    KKCC
213000090209     C                   KFLD                    KKSC
213100090209      *
213200090209     C     KIND          KLIST
213300090209     C                   KFLD                    KKUT
213400090209     C                   KFLD                    KKCC
213500090209     C                   KFLD                    KKSC
213600090209      *
213700090209      * DETERMINO SE SONO BARTOLINI O SDI
213800090209     C                   CLEAR                   TIBS55DS
213900090209     C                   MOVEL     'L'           I50TLA
214000090209     C                   CALL      'TIBS55R'
214100090209     C                   PARM                    TIBS55DS
214200090209      *
214300090209     C* RECUPERO DATI DELL'UTENTE
214400090209     C                   Z-ADD     1             CODUT             1 0
214500090209     C                   CALL      'XPARUT'
214600090209     C                   PARM                    UTEDSE0F
214700090209     C                   MOVEL     RAGUT         RSUT             20
214800090209     C*
214900090209     C* RICERCA CAPOCONTI
215000090209     C                   DO        50            X
215100090209     C                   MOVE      TCU(X)        TCUDS
215200090209     C     F56           IFEQ      'CG'
215300090209     C     F34           ANDEQ     '01'
215400090209     C                   Z-ADD     KCU(X)        KCI               4 0
215500090209     C                   END
215600090209     C                   END
215700090209     C*
215800090211     c                   movel(p)  'EDPAB'       User_Msg         10
215900090211     C*
216000090209     C* IMPOSTO A ZERO VARIABILI DI PGM
216100090209     c                   move      *blank        Snd_Msg_alert     1
216200090209     C                   MOVEL     CA(1)         WCA              78
216300090209     C                   MOVEL     CN(1)         WCN              78
216400090209     C*
216500090209     C* RECUPERO DATA E ORA
216600090209     C                   TIME                    WHHDAT           14 0
216700090209     C                   MOVE      WHHDAT        G02DAT
216800090209     C                   MOVE      WHHDAT        WDATA8            8 0
216900090209     C                   MOVE      WDATA8        WAA2              2 0
217000090209     C                   MOVEL     WDATA8        DATA              6 0
217100090209     C                   MOVE      WAA2          DATA
217200090209     C                   MOVEL     *BLANK        G02ERR
217300090209     C                   CALL      'XSRDA8'
217400090209     C                   PARM                    WLBDA8
217500100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
217600100119     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
217700090209     C                   TIME                    WORA              6 0
217800090209      * Recupero data e ora
217900090209     C                   TIME                    W0140
218000090209      * UDATE IN GGMMAAAA
218100090209     C                   MOVE      W0140         WDTGIO
218200090209      * UDATE IN AAAAMMGG
218300090209     C     *eur          MOVEL     WDTGIO        DATA_eur
218400090209     C     *iso          MOVEL     DATA_eur      dateu
218500090209      *
218600090209      * ?              /*--------------------------- */
218700090209     c                   exsr      CARICA_TABELLE
218800090209      * ?              /*--------------------------- */
218900090209      *
219000090209     C                   MOVEL     *ALL'-'       CMP132          132
219100090209     C                   CLEAR                   EDiVAB00
219200090211     C                   clear                   vablnp_PT
219300090212     C                   clear                   vabnrs_PT
219400090211     C                   clear                   vabctm_PT
219500090211     C                   clear                   VABfnsp_PT
219600090209     C                   MOVEA     *HIVAL        SGN
219700991124      *
219800050414      *------------------
219900991124     C                   ENDSR
220000060621      * ?------------------------------------------------------------------ */
220100090209      *?    CARICA TABELLE SU SCHIERE
220200060621      * ?------------------------------------------------------------------ */
220300090209     C     CARICA_TABELLEBEGSR
220400090205      *
220500090209     C* Caricamento Tabella 1B
220600090209     C                   Z-ADD     0             X                 4 0
220700090209     C                   Z-ADD     CODUT         KKUT
220800090209     C                   MOVEL     '1B'          KCOD
220900090209     C     KTAB1         CHAIN     TABEL00F                           30
221000090209     C     *IN30         DOWEQ     '0'
221100090209     C     TBLFLG        IFEQ      *BLANKS
221200090209     C                   ADD       1             X
221300090209     C                   MOVEL     TBLKEY        CTM(X)
221400090209     C                   MOVEL     TBLUNI        DTM(X)
221500090209     C                   END
221600090209     C     KTAB1         READE     TABEL00F                               30
221700090209     C                   END
221800090209      *
221900090209     C* Caricamento Tabella 15
222000090209     C                   Z-ADD     0             X                 4 0
222100090209     C                   Z-ADD     CODUT         KKUT
222200090209     C                   MOVEL     '15'          KCOD
222300090209     C     KTAB1         CHAIN     TABEL00F                           30
222400090209     C     *IN30         DOWEQ     '0'
222500090209     C     TBLFLG        IFEQ      *BLANKS
222600090209     C                   ADD       1             X
222700090209     C                   MOVEL     TBLKEY        C15(X)
222800090209     C                   MOVEL     TBLUNI        DS15
222900090209     C                   MOVEL     §15COD        ISO(X)
223000090209     C                   MOVEL     §15ECF        CPF(X)
223100090209     C                   MOVE      §15PCF        CPF(X)
223200090209     C                   END
223300090209     C     KTAB1         READE     TABEL00F                               30
223400090209     C                   END
223500090209      *
223600090209     C* Caricamento Tabella CV
223700090209     C                   Z-ADD     0             X
223800090209     C                   Z-ADD     CODUT         KKUT
223900090209     C                   MOVEL     'CV'          KCOD
224000090209     C     KTAB1         CHAIN     TABEL00F                           30
224100090209     C     *IN30         DOWEQ     '0'
224200090209     C     TBLFLG        IFEQ      *BLANKS
224300090209     C                   ADD       1             X
224400090209     C                   MOVEL     TBLKEY        CCV(X)
224500090209     C                   MOVEL     TBLUNI        DCV(X)
224600090209     C                   END
224700090209     C     KTAB1         READE     TABEL00F                               30
224800090209     C                   END
224900090216      *
225000110328      * Caricamento Tabella termini consegna
225100110328     C                   Z-ADD     0             X
225200110328     C                   MOVEL     'TB'          TABCOD
225300110328     C     TABCOD        CHAIN     EDTAB01L                           30
225400110328     C     *IN30         DOWEQ     '0'
225500110328     C     TABFLG        IFEQ      *BLANKS
225600110328     C                   ADD       1             X
225700110328     C                   eval      K35_TpBolla(X) = TABKEY
225800110328     C                   eval      Cod_TpBolla(X) = TABKEY
225900110328     C                   eval      Decod_Porto(X) = TABUNI
226000110328     C                   END
226100110328     C     TABCOD        READE     EDTAB01L                               30
226200110328     C                   END
226300110328      *
226400090216     C* Caricamento Tabella Tipo Incasso C/Assegno
226500090216     C                   Z-ADD     0             X
226600090216     C                   MOVEL     'TC'          TABCOD
226700090216     C     TABCOD        CHAIN     EDTAB01L                           30
226800090216     C     *IN30         DOWEQ     '0'
226900090216     C     TABFLG        IFEQ      *BLANKS
227000090216     C                   ADD       1             X
227100090216     C                   MOVEL     TABKEY        CTC35(X)
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
229900121105      *
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
243400091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
243500090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
243600091203???? C                             Bolle YAMAHA non totalmente tradotto. -
243700100310     C                             Controllare UPLOAD del cliente:'
243800100310     C                   EVAL      Messaggio = %trim(Messaggio) +
243900100310     C                                         %editc(§PTksc:'X')
244000090603     c                   exsr      invio_mail_AB
244100090603      *
244200090213      *
244300090213     C                   ENDSR
244400090205      * ?------------------------------------------------------------------ */
244500090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
244600090205      * ?------------------------------------------------------------------ */
244700090205     C     *pssr         BEGSR
244800090205     C
244900060710     C                   eval      esito = '2'
245000091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
245100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
245200090603     C                             Bolle import in filiale - messaggio -
245300090603     C                             EDI ricevuto errato su UPLOAD.'
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
248200100114     C                   Z-ADD     *ZEROS        WHM               4 0
248300090209     C*
248400100114     C*   CCYYMMDD
248500090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
248600090209     C                   MOVEL     WDATA         WCOMO8            8
248700090209     C                   TESTN                   WCOMO8               99
248800090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
248900090209     C  N99              MOVEL     'S'           WERRO             1
249000090209     C                   END
249100090209     C*
249200100114     C*  CCYYMMDDHHMM
249300090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
249400100120     C                   MOVEL     WDATA         WCOMO12          12
249500100120     C                   TESTN                   WCOMO12              99
249600100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
249700090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
249800100114     C   99              MOVE      WCAMGORA      WHM                            *DATA
249900100114     C   99              MOVEL     WHM           WHMS                           *DATA
250000090209     C  N99              MOVEL     'S'           WERRO             1
250100090209     C                   END
250200090209     C*
250300100114     C*  CCYYMMDDHHMMSS
250400100114     C     WFORM         IFEQ      '204'                                        CCYMMDD
250500100114     C                   MOVEL     WDATA         WCOMO14          14
250600100114     C                   TESTN                   WCOMO14              99
250700100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
250800100114     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
250900100114     C   99              MOVE      WCAMGORA      WHMS                           *DATA
251000100114     C  N99              MOVEL     'S'           WERRO             1
251100100114     C                   END
251200100114     C*
251300090209     C                   ENDSR
251400090210     C*----------------------------------------------------------------
251500090210     C*? TSTNUM - CONTROLLO NUMERICO
251600090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
251700090210     C*         - WLUNGH (Lunghezza campo output)
251800090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
251900090210     C*----------------------------------------------------------------
252000090210     C     TSTNUM        BEGSR
252100090210     C*
252200090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
252300090210     C                   MOVEL     'N'           WOKNUM            1
252400090210     C                   MOVEL     *BLANKS       WNUMOK           15
252500090210     C                   Z-ADD     15            IN                3 0
252600090210     C                   Z-ADD     WLUNGH        IX                3 0
252700090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
252800090210     C                   MOVEA     WNUM          NDA
252900090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
253000090210     C                   MOVEA     *ALL'0'       NDN
253100090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
253200090210     C                   Z-ADD     WLUNGH        I                 3 0
253300090210     C     I             DOWGT     0                                            --- 1 -->
253400090210     C* ESTRAE IL CARATTERE DA ESAMINARE
253500090210     C     1             SUBST     WNUM:I        WTEM01            1
253600090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
253700090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
253800090210     C     WTEM01        SCAN      WCA                                    99
253900090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
254000090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
254100090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
254200090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
254300090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
254400090210     C* DA DESTRA VERSO SINISTRA
254500090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
254600090210     C                   MOVEL     WTEMP1        NDN(IN)
254700090210     C                   SUB       1             IN
254800090210     C                   END                                                    <-- 3 ---
254900090210     C                   END                                                    <-- 2 ---
255000090210     C                   SUB       1             I
255100090210     C                   END                                                    <-- 1 ---
255200090210     C*
255300090210     C                   MOVEA     NDN           WNDN             15
255400090210     C     WNDN          IFNE      *ZEROS
255500090210     C                   MOVEA     NDN           WNUMOK           15
255600090210     C                   MOVEL     'S'           WOKNUM            1
255700090210     C                   END
255800090210     C*
255900090210     C                   ENDSR
256000090210     C*----------------------------------------------------------------
256100090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
256200090210     C*----------------------------------------------------------------
256300090210     C     CNVCAP        BEGSR
256400090210     C*
256500090210     C                   MOVEL     *BLANKS       WCAP              9
256600090210     C     nad3251       IFNE      '0'
256700090210     C     '  '          SCAN      nad3251       LENGHT            2 0
256800090210     C                   SUB       1             LENGHT
256900090210     C     LENGHT        IFLE      5
257000090210     C     LENGHT        ANDGT     0
257100090210     C                   Z-ADD     LENGHT        T                 2 0
257200090210     C                   Z-ADD     5             S                 2 0
257300090210     C                   MOVEA     nad3251       CAP9
257400090210     C                   MOVEL     *ZEROS        CAP5
257500090210     C                   DO        LENGHT
257600090210     C                   MOVE      CAP9(T)       CAP5(S)
257700090210     C                   SUB       1             S
257800090210     C                   SUB       1             T
257900090210     C                   END
258000090210     C                   MOVEA     CAP5          WCAP5             5
258100090210     C     WCAP5         IFEQ      '0'
258200090210     C                   MOVEL     *BLANKS       WCAP
258300090210     C                   ELSE
258400090210     C                   MOVEA     CAP5          WCAP
258500090210     C                   END
258600090210     C*
258700090210     C                   ELSE
258800090210     C                   MOVEL     nad3251       WCAP
258900090210     C                   END
259000090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
259100090210     C     WCAP          IFNE      *BLANKS
259200090210     C     WFLCPF        ANDNE     0
259300090210     C                   MOVEL     WFLCPF        WELE              1 0
259400090210     C                   MOVE      WFLCPF        WPOS              1 0
259500090210     C                   MOVEL     *BLANK        WCPF
259600090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
259700090210     C                   ELSE
259800090210     C                   MOVEL     *BLANKS       WCPF
259900090210     C                   END
260000090210     C                   END
260100090210     C*
260200090210     C                   ENDSR
260300090211      * ?================================================================== */
260400090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
260500090211      * ?================================================================== */
260600090211     C     Scrive_VAB    BEGSR
260700090211      *
260800090211     c                   clear                   err_bloccante     1
260900090211     c                   move      'S'           Almeno_Uno
261000090211      *
261100090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
261200090211     c                   exsr      UPDVAB
261300090211      *
261400090211     C* >>>>>>>>>>>>
261500090211     C*  Richiamo pgm per scrittura immediata VAB
261600090211     C     §PUWRK        IFEQ      ' '
261700090211      *
261800090211     C*  Richiamo pgm per esecuzione stampa
261900090211     C     §PTCMR        IFEQ      'S'
262000090213     C                   MOVEL     WNRDOC        d86CMR
262100090211     C                   ELSE
262200090213     C                   MOVEL     WNRDOC        d86CMR
262300090211     C                   END
262400090211      *
262500090213     C                   MOVEL     ' '           d86RIE
262600090213     C                   MOVEL     §PUDTA        d86DTA
262700090213     C                   Z-ADD     1             d86CNT
262800090213     C                   Z-ADD     §PTKSC        d86CCM
262900090213     C                   MOVEL     *BLANKS       d86STA
263000090213     C                   MOVEL     'E'           d86MOD
263100090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
263200090211      * dove poi imposta la DS dei parametri passati
263300090213     C                   MOVEL     'LR'          d86CHI
263400090213     C                   MOVEL     'N'           d86CTL
263500090211      *
263600090211     c                   move      kpjbu         SvKpjbu
263700090211     c                   clear                   kpjbu
263800090216     C                   movel     *on           Commit_on         1
263900090213     C                   MOVEL     TRTC86ds      KPJBU
264000090213      * ?- Chiamata la TRTC86R2 -*
264100090213     C                   CALL      'TRTC86R2'
264200090211     C                   PARM                    KPJBA
264300090216     C                   PARM                    Commit_on
264400090211     c                   move      Svkpjbu       Kpjbu
264500090211      *
264600090211     C*  Controllo pgm per scrittura immediata VAB
264700090211     C     WW            IFNE      0
264800090211     C                   DO        WW            WX                3 0
264900090213     C                   Z-ADD     KCL(WX)       d86CCM
265000090211      *
265100090211     c                   move      kpjbu         SvKpjbu
265200090211     c                   clear                   kpjbu
265300090216     C                   movel     *on           Commit_on         1
265400090213     C                   MOVEL     TRTC86ds      KPJBU
265500090213      * ?- Chiamata la TRTC86R2 -*
265600090213     C                   CALL      'TRTC86R2'
265700090211     C                   PARM                    KPJBA
265800090216     C                   PARM                    Commit_on
265900090211     c                   move      Svkpjbu       Kpjbu
266000090211     C                   END
266100090211     C                   END
266200090211      *
266300090211     C                   END
266400090211      * >>>>>>>>>>>>
266500090211     C                   if        se_errore  = 'S'
266600090211     c                   move      'S'           err_bloccante
266700090211     c                   goto      Error_VAB
266800090211     c                   end
266900090211     C*
267000090211     C     Error_VAB     Endsr
267100090211     C*----------------------------------------------------------------
267200090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
267300090211     C*----------------------------------------------------------------
267400090211     C     UPDVAB        BEGSR
267500090211      *
267600090211     C*  Se non è stato impostato il codice bolla lo imposto io
267700090211     C     VABCBO        IFEQ      *BLANKS
267800090211     C     VABCAS        IFGT      0
267900090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
268000090211     C                   ELSE
268100090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
268200090211     C                   END
268300090211     C                   END
268400110922      *
268500110922      * Imposta le NOte
268600110922     C     VABNOT        IFEQ      *BLANKS
268700110922     C                   MOVEL     wnot1         VABNOT
268800110922     C                   MOVEL     wnot2         VABNT2
268900110922     C                   ELSE
269000110922     C                   MOVEL     wnot1         VABNT2
269100110922     C                   END
269200090211      *
269300090211     C*  Attribuisco anno spedizione
269400090211     C     WDATPA        IFGT      0                                            anno sped.
269500090211     C                   MOVEL     WDATPA        VABAAS
269600090211     C                   ELSE
269700090211     C     WDATFV        IFGT      0
269800090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
269900090211     C                   END
270000090211     C                   END
270100090211      *
270200090211      *  Controllo dettaglio segnacolli
270300090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
270400090211     C                   EXSR      CTRSGE
270500090211     C                   ELSE
270600090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
270700090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
270800090211     C                   EXSR      CTRSGN
270900090211     C                   END
271000090211     C                   END
271100090211      *
271200090212      *  Imposto linea partenza e serie
271300090211     C                   eval      VABLNP = VABLNP_PT
271400090212     C                   eval      VABNRS = VABnrs_PT
271500090211      *
271600090211      *  Controllo se devo variare il codice trattamento merce
271700090211     C                   eval      VABCTM = VABctm_PT
271800090211      *
271900090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
272000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
272100090211     c                   clear                   mantiene_nsp      1            *blk/'S'
272200090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
272300090211      *
272400090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
272500090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
272600090211      *
272700090211      *  Imposto codice cliente
272800090211     c                   Select
272900090211      *
273000090211      *  Specificato codice da qualificatore FF (TD15)
273100090211     C                   When        WCLKSC <> *zeros
273200090211     C                   Z-ADD     WCLKSC        VABCCM
273300090211     C                   MOVEL     WCLTAR        VABCTR
273400090211      * forza LNP/CTM/NRS
273500090211     c                   if        WclLNP <> 0
273600090211     C                   eval      VABLNP = WclLNP
273700090211     c                   end
273800090211     c                   if        WclNRS <> 0
273900090211     C                   eval      VABNRS = WclNRS
274000090211     c                   end
274100090211     c                   if        WclCTM <> *blank
274200090211     C                   eval      VABCTM = WclCTM
274300090211     c                   end
274400090211      *
274500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
274600090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
274700090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
274800090211     c                   end
274900090211      *
275000090211      *  Specificato codice da qualificatore FF (TT15)
275100090211     C                   When        $CLKSC <> *zeros
275200090211     C                   Z-ADD     $CLKSC        VABCCM
275300090211     C                   MOVEL     $CLTAR        VABCTR
275400090211      * forza LNP/CTM/NRS
275500090211     c                   if        $clLNP <> 0
275600090211     C                   eval      VABLNP = $clLNP
275700090211     c                   end
275800090211     c                   if        $clNRS <> 0
275900090211     C                   eval      VABNRS = $clNRS
276000090211     c                   end
276100090211     c                   if        $clCTM <> *blank
276200090211     C                   eval      VABCTM = $clCTM
276300090211     c                   end
276400090211      *
276500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
276600090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
276700090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
276800090211     c                   end
276900090211      *
277000090211      *  Imposto dati da tabella - PT-
277100090211     C                   Other
277200090211      *
277300090211     C                   MOVEL     §PTKSC        VABCCM
277400090211     C                   MOVEL     §PTCTR        VABCTR
277500090211      *
277600090211     C                   Endsl
277700090211      *
277800090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
277900090211      *    o dalla relativa CL
278000090211     C                   MOVEL     VABlnp        VABfgs
278100090211      *
278200090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
278300090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
278400090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
278500090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
278600090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
278700090211      *
278800090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
278900090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
279000090211     c                   else
279100090211      *
279200090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
279300090211     C*  lo attribuisco io
279400090211      **              **------------------**
279500090211     c                   exsr      repnum
279600090211      **              **------------------**
279700090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
279800090211      **              **------------------**
279900090211     c                   end
280000090211      *
280100090211     C*  Altrimenti attribuisco data del giorno
280200090211     C     VABMGS        IFEQ      0
280300090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
280400090211     C                   MOVE      WOGGI         VABMGS                         = oggi
280500090211     C                   END
280600090211     C*  Eseguo posizionamento su EDiVAB
280700090211     C                   Z-ADD     VABAAS        KAAS
280800090211     C                   Z-ADD     VABlnp        KLNP
280900090211     C                   Z-ADD     VABNRS        KNRS
281000090211     C                   Z-ADD     VABNSP        KNSP
281100090211     C                   MOVEL     SAVBOL        SALVA
281200090211     C     KVAB1         CHAIN     EDiVAB1L                           30
281300090211     C                   MOVEL     SALVA         SAVBOL
281400090211     C*  Imposto dati CMR
281500090211     C                   Z-ADD     1             VABCNT
281600090211     **
281700090211      *** Attenzione se si deve trattare il VAX con il CMR
281800090211      *** il campo VABCNT deve essere sempre impostato a (1)
281900090211     C     §puWRK        ifEQ      'S'
282000090211     C     §puNSP        andEQ     'S'
282100090211     C                   Z-ADD     1             VABCNT
282200090211     c                   end
282300090211     **
282400090211     C                   Z-ADD     WDTPRE        VABDTS
282500090212     C                   movel     '20'          VABDTS
282600100113     ** Forza la Data di Oggi
282700100113     C                   z-add     WOggi         VABDTS
282800100113     **
282900090212     C     WHMPRE        mult      100           VABHMS
283000090211     C     §PTCMR        IFEQ      'S'
283100090211     C                   Z-ADD     WDTCMR        VABDCM
283200090211     C                   MOVEL     WNRCMR        VABCMR
283300090211     C                   ELSE
283400090211     C                   Z-ADD     WDATFV        VABDCM
283500090211     C                   MOVEL     WNUMFV        VABCMR
283600090211     C                   END
283700100120      *
283800100120      * Per YAMAHA occorre fare un unico CMR giornaliero per permettere di
283900100120      * accorpare la bolle ad un unico destinatario da più parti
284000100120      *  YAMAHA quindi deve utilizzare la conferma x CMR.
284100100120     c                   eval      VABCMR  = 'YAMAHA_' + %editc(WOggi_CMR:'X')
284200100120      *
284300090211     C*  Se non è indicato il nome del mittente = Partner mittente
284400090211     C     VABRMO        IFEQ      *BLANKS
284500090211     C                   MOVEL     ACORAG        VABRMO
284600090211     C                   MOVEL     INDCAP        VABCMO
284700090211     C     INDCAE        IFNE      *BLANKS
284800090211     C                   MOVEL     INDCAE        VABCMO
284900090211     C                   END
285000090211     C     INDSTA        IFNE      *BLANKS
285100090211     C                   MOVEL     INDSTA        VABNMO
285200090211     C                   END
285300090211     C                   END
285400090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
285500090211     C     §1BFG1        IFEQ      'N'
285600090211     C     §1BFG7        ANDEQ     'N'
285700090211     C     §1BFG2        ANDEQ     'N'
285800090211     C***                  Z-ADD0         VABNRS
285900090211     C                   END
286000090211     C*  Imposto segancollo da - a
286100090211     C     §1BFG1        IFEQ      'S'
286200090211     C     §1BFG7        ANDEQ     'N'
286300090211     C     §1BF12        ANDNE     'E'
286400090211      *
286500090211     C     XY            IFEQ      *ZEROS
286600090211     C                   CLEAR                   VABNCD
286700090211     C                   CLEAR                   VABNCA
286800090211     C                   ELSE
286900090211     C                   Z-ADD     SGN(1)        VABNCD
287000090211     C                   Z-ADD     SGN(XY)       VABNCA
287100090211     C                   ENDIF
287200090211     C*
287300090211     C                   END
287400090211     C*
287500090211     C*  Se cap mittente originale a blank abblenco nazione
287600090211     C     VABCMO        IFEQ      *BLANKS
287700090211     C                   MOVEL     *BLANKS       VABNMO
287800090211     C                   END
287900090603     C*
288000090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
288100090603     C* Se in testata
288200090603     c                   if        NAD_destinatario_in_testata = 'S'
288300090603     c                   eval         VABrsd =  tes_VABrsd
288400090603     c                   eval         VABrd2 =  tes_VABrd2
288500090603     c                   eval         VABind =  tes_VABind
288600090603     c                   eval         VABlod =  tes_VABlod
288700090603     c                   eval         VABnzd =  tes_VABnzd
288800090603     c                   end
288900090603     C* Se in testata
289000090603     c                   if        NAD_mittente_in_testata = 'S'
289100090603     c                   eval         VABrmo = tes_VABrmo
289200090603     c                   eval         VABnmo = tes_VABnmo
289300090603     c                   eval         VABcmo = tes_VABcmo
289400090603     c                   end
289500090603      *
289600090211      *? ------------------------------------------------------------------------
289700090213     C   30              WRITE     EDiVAB00                             99
289800090213     C  N30              UPDATE    EDiVAB00                             99
289900090211      *? ------------------------------------------------------------------------
290000090213     c   99              eval      errori_in_Wrt = 'S'
290100090211     C*
290200090211     c                   clear                   wri_vabtic        1
290300090211     C*
290400090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
290500090211     C                   EXSR      WRTVAT_Rif
290600090211     C*
290700090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
290800090211     C     §1BF12        IFEQ      'E'
290900090211     C                   EXSR      WRTVAT
291000090211     C                   ELSE
291100090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
291200090211     C     §1BFG7        IFEQ      'S'
291300090211     C     §1BFG1        OREQ      'S'
291400090211     C                   EXSR      WRTVAD
291500090211     C                   END
291600090211     C                   END
291700090211      *
291800090211     C* Reimposto codice trattamento merce cliente
291900090211     C                   MOVEL     WSVTRM        DS1B
292000090211     C**
292100090211     C* Do errore se previsto CMR ma non c'è
292200090211     C     §PTCMR        IFNE      ' '
292300090211     C                   EXSR      MEMCMR
292400090211     C                   END
292500090211     C* Do errore se previsto AFB ma non c'è
292600090211     C     §PTNFV        IFNE      ' '
292700090211     C                   EXSR      MEMAFB
292800090211     C                   END
292900090211     C*  Se il cliente vuole il ritorno delle date di consegna
293000090211     C*  è c'è una squdratura fra i segnacolli e il numero
293100090211     C*  dei colli che ci ha passato scrivo EDSUM
293200090211     C     §PUDTA        IFEQ      'S'
293300090211     C     WSQUAD        ANDEQ     'S'
293400090211     C                   EXSR      WRTSUM
293500090211     C                   END
293600090211     C*
293700090211     C                   ENDSR
293800090211     C*----------------------------------------------------------------
293900090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
294000090211     C*----------------------------------------------------------------
294100090211     C     CTRSGN        BEGSR
294200090211     C*
294300090211     C                   MOVEL     'N'           WNSGER            1
294400090211     C*
294500090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
294600090211     C     VABNCL        IFNE      XY
294700090211     C                   MOVEL     'S'           WNSGER
294800090211     C                   MOVEL     'S'           WSQUAD
294900090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
295000090211     c                              bollettati'
295100090211     C                   GOTO      FINSGN
295200090211     C                   END
295300090211     C*  Riordino i segnacolli
295400090211     C     §1BFG7        IFEQ      'N'
295500090211     C                   SORTA     SGN
295600090211     C*  Controllo se sono sequenziali
295700090211     C                   MOVEL     'N'           WNOSEQ            1
295800090211     C     SGN(1)        ADD       1             WEXSGN            7 0
295900090211     C     2             DO        XY            X
296000090211     C     SGN(X)        IFNE      WEXSGN
296100090211     C                   MOVEL     'S'           WNOSEQ
296200090211     C                   END
296300090211     C     SGN(X)        ADD       1             WEXSGN
296400090211     C                   END
296500090211     C                   END
296600090211     C*
296700090211     C     FINSGN        ENDSR
296800090211     C*----------------------------------------------------------------
296900090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
297000090211     C*----------------------------------------------------------------
297100090211     C     CTRSGE        BEGSR
297200090211     C*
297300090211     C                   MOVEL     'N'           WNSGER            1
297400090211      *
297500090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
297600090211      *
297700090211     C     VABNCL        IFNE      §§
297800090211     C                   MOVEL     'S'           WNSGER
297900090211     C                   MOVEL     'S'           WSQUAD
298000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
298100090211     c                              bollettati'
298200090211     C                   END
298300090211      *
298400090211     C                   ENDSR
298500090211     C*----------------------------------------------------------------
298600090211     C*? ESEGUO SCRITTURA EDiVAD
298700090211     C*----------------------------------------------------------------
298800090211     C     WRTVAD        BEGSR
298900090211     C*
299000090211     C* Se non seq. scrivo xy record
299100090211     C     §1BFG1        IFEQ      'S'
299200090211     C     XY            ANDNE     *ZEROS
299300090211     C*
299400090211     C     §1BFG7        IFEQ      'S'
299500090211     C                   DO        XY            X
299600090211     C                   Z-ADD     VABCCM        KCCM
299700090211     C                   Z-ADD     SGN(X)        KNCD
299800090211     C                   Z-ADD     VABCNT        VADCNT
299900090211     C                   MOVEL     VABCMR        VADCMR
300000090211     C                   Z-ADD     VABAAS        VADAAS
300100090211     C                   Z-ADD     VABLNP        VADLNP
300200090211     C                   Z-ADD     VABNRS        VADNRS
300300090211     C                   Z-ADD     VABNSP        VADNSP
300400090211     C                   Z-ADD     1             VADNCL
300500090211     C                   MOVEL     SGN(X)        VADCDP
300600090211     C                   Z-ADD     SGN(X)        VADNCD
300700090211     C                   Z-ADD     *ZEROS        VADNCA
300800090211     C                   Z-ADD     VABCCM        VADCCM
300900090211     C                   movel     VABfgs        VADfgs
301000090213     C                   WRITE     EDiVAD00                             99
301100090213     c   99              eval      errori_in_Wrt = 'S'
301200090211     C                   END
301300090211     C* Se sequenziali scrivo un solo record
301400090211     C                   ELSE
301500090211     C                   Z-ADD     VABAAS        VADAAS
301600090211     C                   Z-ADD     VABLNP        VADLNP
301700090211     C                   Z-ADD     VABNRS        VADNRS
301800090211     C                   Z-ADD     VABNSP        VADNSP
301900090211     C                   Z-ADD     VABNCL        VADNCL
302000090211     C                   Z-ADD     SGN(1)        VADNCD
302100090211     C                   Z-ADD     SGN(XY)       VADNCA
302200090211     C                   Z-ADD     VABCCM        VADCCM
302300090211     C                   movel     VABfgs        VADfgs
302400090213     C                   WRITE     EDiVAD00                             99
302500090213     c   99              eval      errori_in_Wrt = 'S'
302600090211     C                   END
302700090211     C*
302800090211     C                   ELSE
302900090211     C                   DO        XY            X
303000090211     C                   Z-ADD     VABCCM        KCCM
303100090211     C                   Z-ADD     SGN(X)        KNCD
303200090211     C                   Z-ADD     VABAAS        VADAAS
303300090211     C                   Z-ADD     VABLNP        VADLNP
303400090211     C                   Z-ADD     VABNRS        VADNRS
303500090211     C                   Z-ADD     VABNSP        VADNSP
303600090211     C                   MOVEL     SGN(X)        VADCDP
303700090211     C                   Z-ADD     1             VADNCL
303800090211     C                   Z-ADD     *ZEROS        VADNCD
303900090211     C                   Z-ADD     *ZEROS        VADNCA
304000090211     C                   Z-ADD     VABCCM        VADCCM
304100090211     C                   movel     VABfgs        VADfgs
304200090213     C                   WRITE     EDiVAD00                             99
304300090213     c   99              eval      errori_in_Wrt = 'S'
304400090211     C                   END
304500090211     C*
304600090211     C                   END
304700090211     C*
304800090211     C                   ENDSR
304900090211     C*----------------------------------------------------------------
305000090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
305100090211     C*----------------------------------------------------------------
305200090211     C     WRTVAT_Rif    BEGSR
305300090211      *
305400090211      *  riferimento di consegna destinatario
305500090211     c                   if        WatRDE <> *blank
305600090211     C                   Z-ADD     VABCNT        VATCNT
305700090211     C                   MOVEL     VABCMR        VATCMR
305800090211     C                   Z-ADD     VABCCM        VATCCM
305900090211     C                   movel     VABfgs        VATfgs
306000090211     C                   Z-ADD     VABLNP        VATLNP
306100090211     C                   Z-ADD     VABAAS        VATAAS
306200090211     C                   Z-ADD     VABNRS        VATNRS
306300090211     C                   Z-ADD     VABNSP        VATNSP
306400090211     C                   MOVEL     'A'           VATTRC
306500090211     C                   MOVEL     WatRDE        VATNOT
306600090213     C                   WRITE     EDiVAT00                             99
306700090213     c   99              eval      errori_in_Wrt = 'S'
306800090211     c                   end
306900090211      *
307000090211      *  riferimento di consegna telefonico
307100090211     c                   if        WatTEL <> *blank
307200090211     C                   Z-ADD     VABCNT        VATCNT
307300090211     C                   MOVEL     VABCMR        VATCMR
307400090211     C                   Z-ADD     VABCCM        VATCCM
307500090211     C                   movel     VABfgs        VATfgs
307600090211     C                   Z-ADD     VABLNP        VATLNP
307700090211     C                   Z-ADD     VABAAS        VATAAS
307800090211     C                   Z-ADD     VABNRS        VATNRS
307900090211     C                   Z-ADD     VABNSP        VATNSP
308000090211     C                   MOVEL     'B'           VATTRC
308100090211     C                   MOVEL     WatTEL        VATNOT
308200090213     C                   WRITE     EDiVAT00                             99
308300090213     c   99              eval      errori_in_Wrt = 'S'
308400090211     c                   end
308500090211      *
308600090211      * riferimento del Partner che una volta veniva riportato sul BL4
308700100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
308800100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
308900100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
309000100119      *
309100100119     c                   goto      no_riferimento
309200100119      *
309300090211     c                   if        Wnsp   <> *blank
309400090211     C                   Z-ADD     VABCNT        VATCNT
309500090211     C                   MOVEL     VABCMR        VATCMR
309600090211     C                   Z-ADD     VABCCM        VATCCM
309700090211     C                   movel     VABfgs        VATfgs
309800090211     C                   Z-ADD     VABLNP        VATLNP
309900090211     C                   Z-ADD     VABAAS        VATAAS
310000090211     C                   Z-ADD     VABNRS        VATNRS
310100090211     C                   Z-ADD     VABNSP        VATNSP
310200090211     C                   MOVEL     'C'           VATTRC
310300090211     C                   MOVEL     Wnsp          VATNOT
310400090213     C                   WRITE     EDiVAT00                             99
310500090213     c   99              eval      errori_in_Wrt = 'S'
310600090211     c                   end
310700100119      *
310800100119     c     no_riferimentotag
310900090211      *
311000090211     C                   ENDSR
311100090211     C*----------------------------------------------------------------
311200090211     C*? ESEGUO SCRITTURA EDiVAT
311300090211     C*----------------------------------------------------------------
311400090211     C     WRTVAT        BEGSR
311500090211      *
311600090211     C                   DO        §§            X
311700090211     C                   Z-ADD     VABCNT        VATCNT
311800090211     C                   MOVEL     VABCMR        VATCMR
311900090211     C                   Z-ADD     VABCCM        VATCCM
312000090211     C                   movel     VABfgs        VATfgs
312100090211     C                   Z-ADD     VABLNP        VATLNP
312200090211     C                   Z-ADD     VABAAS        VATAAS
312300090211     C                   Z-ADD     VABNRS        VATNRS
312400090211     C                   Z-ADD     VABNSP        VATNSP
312500090211     C                   MOVEL     'E'           VATTRC
312600090211      *
312700090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
312800090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
312900090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
313000090211     c                   if        WVATlun > 0
313100090211      *
313200090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
313300090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
313400090211      *
313500090211     c                   else
313600090211      *
313700090211      * Se non è impostato nulla prende il segnacollo passato così com'è
313800090211     C                   MOVEL     SGE(X)        VATNOT
313900090211     c                   end
314000090211      *
314100090213     C                   WRITE     EDiVAT00                             99
314200090213     c   99              eval      errori_in_Wrt = 'S'
314300090211     C                   END
314400090211      *
314500090211     C                   ENDSR
314600090211     C*----------------------------------------------------------------
314700090211     C*? ESEGUO SCRITTURA EDSUM
314800090211     C*----------------------------------------------------------------
314900090211     C     WRTSUM        BEGSR
315000090211     C*
315100090211     C* Se non seq. scrivo xy record
315200090211     C                   DO        XY            X
315300090211     C                   Z-ADD     VABAAS        SUMAAS
315400090211     C                   Z-ADD     VABLNP        SUMFLS
315500090211     C                   MOVEL     WKSC          SUMLNP
315600090211     C                   Z-ADD     VABLNA        SUMLNA
315700090211     C                   Z-ADD     VABZNC        SUMZNC
315800090211     C                   Z-ADD     VABNRS        SUMNRS
315900090211     C                   Z-ADD     VABNSP        SUMNSP
316000090211     C                   Z-ADD     SGN(X)        SUMNSC
316100090211     C                   Z-ADD     WKSC          SUMCCM
316200090211     C                   MOVEL     *BLANKS       SUMSTS
316300090211     C                   MOVEL     *BLANKS       SUMFLG
316400090211     C                   MOVEL     WPRGSP        SUMCNI
316500090211     C                   MOVEL     VABCMR        SUMCMR
316600090211     C                   Z-ADD     0             SUMNFE
316700090211     C                   Z-ADD     VABCCM        VADCCM
316800090213     C                   WRITE     EDSUM000                             99
316900090213     c   99              eval      errori_in_Wrt = 'S'
317000090211     C                   END
317100090211     C*
317200090211     C                   ENDSR
317300090211     c*==================================================================*
317400090211      * Manda un Msg in Posta AS Sede a EDP*
317500090211     c*==================================================================*
317600090211     c     SEND_MSG_EDP  begsr
317700090211      *
317800090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
317900090211      *   Lo manda una sola volta per lavoro
318000090211     c                   IF        Snd_Msg_alert  <> 'N' and
318100090211     c                             User_msg <> *blank
318200090211     c                   z-add     1             Jx
318300090211     C                   exsr      CalQEZSNDMG
318400090211     c                   end
318500090211      *
318600090211     c                   endsr
318700090213     c**********************************************************************
318800090213     c* reperimento numero Spedizione
318900090213     c**********************************************************************
319000090213     C     repnum        BEGSR
319100090213      *
319200090213      *    deve controllare sulla tabella "3C" se il numero spedizione
319300090213      *     deve essere mantenuto oppure no
319400090213     C*
319500090213     C                   clear                   Ds3C
319600090213     C                   Z-ADD     CODUT         TBLKUT
319700090213     C                   MOVEL     '3C'          TBLCOD
319800090213     C                   movel(p)  VABccm        TBLKEY
319900090213     C     KTABel        CHAIN     TABEL00F                           30
320000090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
320100090213     C                   MOVEL     TBLUNI        Ds3C
320200090213     C                   END
320300090213      *
320400090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
320500090213     c                   if        §3CFsp <> 'S'
320600090213      *
320700090213     C* NSP => Stacco un numeratore da AZNUM
320800090213     c                   movel     kpjbu         svkpjbu
320900090213     c                   clear                   kpjbu
321000090213     C                   clear                   TRUL33DS
321100090213     C                   eval      I33OPE = *zeros
321200090213     C                   eval      I33CNU = 302
321300090213     C                   eval      I33NUM = 1
321400090213     C                   movel     TRUL33DS      KPJBU
321500090213     C                   call      'TRUL33R'
321600090213     C                   parm                    KPJBA
321700090213     C                   movel     KPJBU         TRUL33DS
321800090213     c                   movel     svkpjbu       kpjbu
321900090213      ****
322000090213     C                   if        O33ERR = *zeros
322100090213     c                   z-add     o33nrf        NUM_SPED
322200090213     C                   else
322300090213     C                   endif
322400090213      ****
322500090213     c                   ELSE
322600090213      *   se si vuole mantenere il numero spedizione
322700090213      ****
322800090213     C                   MOVEL     WOGGI         WANNO             4 0
322900090213     C                   MOVE      WANNO         KAAA
323000090213     C                   Z-ADD     3             KCNU
323100090213     C                   Z-ADD     VABlnp        KFIL
323200090213     C     KNUF          CHAIN     FLNUF                              9999
323300090213     C     *IN99         IFEQ      '0'
323400090213     C                   ADD       1             NUFNUM
323500090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
323600090213     C                   UPDATE    FLNUF
323700090213     C                   ELSE
323800090213     C                   END
323900090213     C*
324000090213     c                   EndIF
324100090213      **
324200090213     C                   ENDSR
324300090213     C*----------------------------------------------------------------
324400090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
324500090213     C*----------------------------------------------------------------
324600090213     C     MEMCMR        BEGSR
324700090213     C*
324800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
324900090213     C     §PTCM1        IFEQ      'S'
325000090213     C                   CLEAR                   EDRDE000
325100090213     C                   MOVEL     'TD1154'      KNMC
325200090213     C                   Z-ADD     1             KNRP
325300090213     C                   MOVEL     WKSC          KLNP1
325400090213     C     KRDE          CHAIN     EDRDE01L                           31
325500090213     C     *IN31         IFEQ      '1'
325600090213     C                   Z-ADD     VABAAS        RDEAAS
325700090213     C                   Z-ADD     VABLNP        RDELNP
325800090213     C                   Z-ADD     VABNRS        RDENRS
325900090213     C                   Z-ADD     VABNSP        RDENSP
326000090213     C                   MOVEL     'TD1154'      RDENMC
326100090213     C                   Z-ADD     1             RDENRP
326200090213     C                   MOVEL     WNRCMR        RDEVAL
326300090213     C                   WRITE     EDRDE000                             99
326400090213     c   99              eval      errori_in_Wrt = 'S'
326500090213     C                   END
326600090213     C*  Memorizzo qualificatore CMR
326700090213     C                   CLEAR                   EDRDE000
326800090213     C                   MOVEL     'TD1153'      KNMC
326900090213     C                   Z-ADD     1             KNRP
327000090213     C                   MOVEL     WKSC          KLNP1
327100090213     C     KRDE          CHAIN     EDRDE01L                           31
327200090213     C     *IN31         IFEQ      '1'
327300090213     C                   Z-ADD     VABAAS        RDEAAS
327400090213     C                   Z-ADD     VABLNP        RDELNP
327500090213     C                   Z-ADD     VABNRS        RDENRS
327600090213     C                   Z-ADD     VABNSP        RDENSP
327700090213     C                   Z-ADD     1             RDENRP
327800090213     C                   MOVEL     'TD1153'      RDENMC
327900090213     C                   MOVEL     'CMR'         RDEVAL
328000090213     C                   WRITE     EDRDE000                             99
328100090213     c   99              eval      errori_in_Wrt = 'S'
328200090213     C                   END
328300090213     C*  Memorizzo la data
328400090213     C                   CLEAR                   EDRDE000
328500090213     C                   MOVEL     'TD2380'      KNMC
328600090213     C                   Z-ADD     1             KNRP
328700090213     C                   MOVEL     WKSC          KLNP1
328800090213     C     KRDE          CHAIN     EDRDE01L                           31
328900090213     C     *IN31         IFEQ      '1'
329000090213     C                   Z-ADD     VABAAS        RDEAAS
329100090213     C                   Z-ADD     VABLNP        RDELNP
329200090213     C                   Z-ADD     VABNRS        RDENRS
329300090213     C                   Z-ADD     VABNSP        RDENSP
329400090213     C                   Z-ADD     1             RDENRP
329500090213     C                   MOVEL     'TD2380'      RDENMC
329600090213     C                   MOVEL     WDTCMR        RDEVAL
329700090213     C                   WRITE     EDRDE000                             99
329800090213     c   99              eval      errori_in_Wrt = 'S'
329900090213     C                   END
330000090213     C*
330100090213     C                   END                                                    §PTCM1 = 'S'
330200090213     C*
330300090213     C                   ENDSR
330400090213     C*----------------------------------------------------------------
330500090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
330600090213     C*----------------------------------------------------------------
330700090213     C     MEMAFB        BEGSR
330800090213     C*
330900090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
331000090213     C     §PTNF1        IFEQ      'S'
331100090213     C                   CLEAR                   EDRDE000
331200090213     C                   MOVEL     WKSC          KLNP1
331300090213     C                   MOVEL     'TD1154'      KNMC
331400090213     C                   Z-ADD     1             KNRP
331500090213     C     KRDE          CHAIN     EDRDE01L                           31
331600090213     C     *IN31         IFEQ      '1'
331700090213     C                   Z-ADD     VABAAS        RDEAAS
331800090213     C                   Z-ADD     VABLNP        RDELNP
331900090213     C                   Z-ADD     VABNRS        RDENRS
332000090213     C                   Z-ADD     VABNSP        RDENSP
332100090213     C                   MOVEL     'TD1154'      RDENMC
332200090213     C     §PTCM1        IFNE      'S'
332300090213     C     WNRCMR        OREQ      *BLANKS
332400090213     C                   Z-ADD     1             RDENRP
332500090213     C                   ELSE
332600090213     C                   Z-ADD     2             RDENRP
332700090213     C                   END
332800090213     C                   MOVEL     WNUMFV        RDEVAL
332900090213     C                   WRITE     EDRDE000                             99
333000090213     c   99              eval      errori_in_Wrt = 'S'
333100090213     C                   END
333200090213     C*  Memorizzo qualificatore CMR
333300090213     C                   CLEAR                   EDRDE000
333400090213     C                   MOVEL     WKSC          KLNP1
333500090213     C                   MOVEL     'TD1153'      KNMC
333600090213     C                   Z-ADD     1             KNRP
333700090213     C     KRDE          CHAIN     EDRDE01L                           31
333800090213     C     *IN31         IFEQ      '1'
333900090213     C                   Z-ADD     VABAAS        RDEAAS
334000090213     C                   Z-ADD     VABLNP        RDELNP
334100090213     C                   Z-ADD     VABNRS        RDENRS
334200090213     C                   Z-ADD     VABNSP        RDENSP
334300090213     C     §PTCM1        IFNE      'S'
334400090213     C     WNRCMR        OREQ      *BLANKS
334500090213     C                   Z-ADD     1             RDENRP
334600090213     C                   ELSE
334700090213     C                   Z-ADD     2             RDENRP
334800090213     C                   END
334900090213     C                   MOVEL     'TD1153'      RDENMC
335000090213     C                   MOVEL     'AFB'         RDEVAL
335100090213     C                   WRITE     EDRDE000                             99
335200090213     c   99              eval      errori_in_Wrt = 'S'
335300090213     C                   END
335400090213     C*  Memorizzo la data
335500090213     C                   CLEAR                   EDRDE000
335600090213     C                   MOVEL     WKSC          KLNP1
335700090213     C                   MOVEL     'TD2380'      KNMC
335800090213     C                   Z-ADD     1             KNRP
335900090213     C     KRDE          CHAIN     EDRDE01L                           31
336000090213     C     *IN31         IFEQ      '1'
336100090213     C                   Z-ADD     VABAAS        RDEAAS
336200090213     C                   Z-ADD     VABLNP        RDELNP
336300090213     C                   Z-ADD     VABNRS        RDENRS
336400090213     C                   Z-ADD     VABNSP        RDENSP
336500090213     C     §PTCM1        IFNE      'S'
336600090213     C     WNRCMR        OREQ      *BLANKS
336700090213     C                   Z-ADD     1             RDENRP
336800090213     C                   ELSE
336900090213     C                   Z-ADD     2             RDENRP
337000090213     C                   END
337100090213     C                   MOVEL     'TD2380'      RDENMC
337200090213     C                   MOVEL     WDATFV        RDEVAL
337300090213     C                   WRITE     EDRDE000                             99
337400090213     c   99              eval      errori_in_Wrt = 'S'
337500090213     C                   END
337600090213     C*
337700090213     C                   END                                                    §PTCM1 = 'S'
337800090213     C*
337900090213     C                   ENDSR
338000090213     c*==================================================================*
338100090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
338200090213     c*==================================================================*
338300090213     c     Imp_ADDR_mail begsr
338400090213      *
338500090213     c                   clear                   Indirizzi
338600090213      *
338700090213      *  Lunghezza indirizzi presenti
338800090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
338900090213     c                   z-add     1             al_char           3 0
339000090213     c                   z-add     1             dal_char          3 0
339100090213     c                   z-add     1             tot_char          3 0
339200090213      *
339300090213     c     successivo    tag
339400090213      *
339500090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
339600090213      *   e aggiunge il dominio Bartolini + il (;) separatore
339700090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
339800090213     c                   eval      tot_char = al_char - dal_char
339900090213     c                   z-add     dal_char      Dac
340000090213     c                   z-add     tot_char      Luc
340100090213      *
340200090213     c                   clear                   Indir_Bartol     40
340300090213     c                   clear                   Indir_Parz       20
340400090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
340500090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
340600090213     c                             %Trim(bart_it)
340700090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
340800090213      *
340900090213     c                   if        al_char = Lun_Ind
341000090213     c                   goto      fine_indmail
341100090213     c                   else
341200090213     c                   eval      dal_char = ( al_char + 1 )
341300090213     c                   goto      successivo
341400090213     c                   end
341500090213      *
341600090213     C     fine_indmail  TAG
341700090213      *
341800090213     C                   ENDSR
341900090213     c*==================================================================*
342000090213      * Manda un Msg x E-mail
342100090213     c*==================================================================*
342200090213     c     Invio_Mail    begsr
342300090213      *
342400090213     c                   goto      non_inviare
342500090213      *
342600090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
342700090213     C* Inizializzo variabili
342800090213     C                   movel     *blanks       wrkEml          253
342900090213     C                   movel     *blanks       wrkMsg         5000
343000090213     C                   movel     *blanks       wrkOgg           44
343100090213      *
343200090213     C* Valorizzo i campi della e-m@ail - indirizzo
343300090213     C                   eval      wrkEml = Indirizzi
343400090213     C                   eval      wrkOgg = Oggetto
343500090213     C                   eval      wrkMsg = Messaggio
343600090213
343700090213     C                   call(e)   'TIS701C'
343800090213     C                   parm                    wrkEml
343900090213     C                   parm                    wrkOgg
344000090213     C                   parm                    wrkMsg
344100090213
344200090213     c     non_inviare   tag
344300090213     C*
344400090213     C                   ENDSR
344500090603     c*==================================================================*
344600090603      * Manda un Msg x E-mail a EDPAB
344700090603     c*==================================================================*
344800090603     c     Invio_Mail_AB begsr
344900090603      *
345000090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
345100090603     C* Inizializzo variabili
345200090603     C                   movel     *blanks       wrkEml          253
345300090603     C                   movel     *blanks       wrkMsg         5000
345400090603     C                   movel     *blanks       wrkOgg           44
345500090603      *
345600090603     C* Valorizzo i campi della e-m@ail - indirizzo
345700110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
345800110203     C                   eval      wrkEml = Indirizzi
345900090603     C                   eval      wrkOgg = Oggetto
346000090603     C                   eval      wrkMsg = Messaggio
346100110203      ********
346200110203     C********           call(e)   'TIS701C'
346300110203     C********           parm                    wrkEml
346400110203     C********           parm                    wrkOgg
346500110203     C********           parm                    wrkMsg
346600110203     C*
346700110203      ** *****
346800110203     C                   call(e)   'TRTCT00R2'
346900110203     C                   parm                    wrkEml
347000110203     C                   parm                    wrkOgg
347100110203     C                   parm                    wrkMsg
347200110203     C*
347300090603     C                   ENDSR
347400090211     ***********************************************************************
347500090211     **
347600090211     ** Send Message (QEZSNDMG) API
347700090211     **
347800090211     ***********************************************************************
347900090211     C     CalQEZSNDMG   BEGSR
348000090211      *
348100090211     ** Invio messaggio all'utente.
348200090211     C                   EVAL      SndMg03 = msgDSP
348300090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
348400090211     C                   EVAL      SndMg05(Jx) = User_msg
348500090211     C                   EVAL      SndMg06 = Jx
348600090211     C                   CLEAR                   QUSEC
348700090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
348800090211
348900090211     C                   CALL      'QEZSNDMG'
349000090211     C                   PARM                    SndMg01
349100090211     C                   PARM                    SndMg02
349200090211     C                   PARM                    SndMg03
349300090211     C                   PARM                    SndMg04
349400090211     C                   PARM                    SndMg05
349500090211     C                   PARM                    SndMg06
349600090211     C                   PARM                    SndMg07
349700090211     C                   PARM                    SndMg08
349800090211     C                   PARM                    Qusec
349900090211     C                   PARM                    SndMg10
350000090211     C                   PARM                    SndMg11
350100090211     C                   PARM                    SndMg12
350200090211      *
350300090211     C                   ENDSR
350400121105      *---------------------------------------------------------------*
350500121105      * CTRL caricamento schiere    PT                               -*
350600121105      *---------------------------------------------------------------*
350700121105     C     Check_PT      begsr
350800121105     C*
350900121105     c* Controllo riempimento schiera
351000121105     c                   clear                   trul0sds
351100121105     c                   eval      i0sski='CPT'
351200121105     c                   eval      i0sele=%elem(CPT)
351300121105     c                   eval      i0spie=x
351400121105     c                   eval      i0sfile='EDTAB00F'
351500121105     c                   eval      i0ssif=knsif
351600121105     c                   eval      i0spgm='TRTCT07R'
351700121113     c                   move      kpjbu         SvKpjbu
351800121113     c                   clear                   kpjbu
351900121105     c                   movel     trul0sds      kpjbu
352000121105     c                   call      'TRUL0SR'
352100121105     c                   parm                    kpjba
352200121113     c                   eval      kpjbu  =  SvKpjbu
352300121105     C*
352400121105     C                   ENDSR
352500090211     O*****************************************************************
352600090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
352700090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
352800090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
352900090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
