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
020800090209      *
020900121105     d Ptn_parziale    s              1    inz('N')
021000121105     d sav_CPTx        s              3  0 inz(0)
021100090209      * Tabella CL --> codici clienti - tariffa
021200130305     D CCL             S             35    DIM(999)
021300130305     D DCL             S             90    DIM(999)
021400090209      * Schiere x caricamento cod.cliente  <>
021500130305     D KCL             S              7  0 DIM(999)
021600090209      * Termini di consegna
021700110328     D K35_TpBolla     S             35    DIM(100)
021800110328     D Cod_TpBolla     S              3    DIM(100)
021900110328     D Decod_Porto     S              1    DIM(100)
022000110328     d Trovata_Tab_TB  S              1
022100110328      *
022200090216      * Tipo pagamento C/Assegno
022300090216     D CTC35           S             35    DIM(100)
022400090216     D CTC             S              2    DIM(100)
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
023500090209     D LOD             S              1    DIM(35)
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
058300100120     c                   clear                   seg_imprevisto
058400100720     c                   clear                   trovato_seg
058500100716      *
058600100716     C                   clear                   keyUNBCLI
058700100716     C                   clear                   keyTIPOMSG
058800100716     C                   clear                   keyVERSION
058900100716     C                   clear                   keyRELEASE
059000100716     C                   clear                   keyAGENCY
059100100716     C                   clear                   keyASSOCIA
059200100716      *
059300100720      *  Chiamata generica x decodificare il singolo segmento letto
059400100720     c                   call      'TRTCT00R1'
059500100720      * input
059600100720     c                   parm                    tipo_seg
059700100720     c                   parm                    segmento
059800100720     C                   parm                    keyUNBCLI
059900100720     C                   parm                    keyTIPOMSG
060000100720     C                   parm                    keyVERSION
060100100720     C                   parm                    keyRELEASE
060200100720     C                   parm                    keyAGENCY
060300100720     C                   parm                    keyASSOCIA
060400100720      *output
060500100720     c                   parm                    Errore
060600100720     c                   parm                    DsGenerica
060700100720     c                   parm                    Valori_inErr
060800100720     c                   parm                    Descr_Errore
060900100720     c                   parm                    trovato_seg
061000100720      **--------
061100100720      *  Ritorna la DS GENERICA oppure l'errore
061200100120      **--------
061300100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
061400100120      * ?  Anche se non utilizzati.
061500100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
061600100120     c                   select
061700100120      *
061800100720     c                   when      trovato_seg ='N'
061900100720      *
062000100720     c                   move      'S'           seg_imprevisto
062100100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
062200100720     c                   exsr      Error_DET
062300100720     c                   goto      end_Decod
062400100720      **--------
062500100120     c                   when      tipo_seg = 'UNA'
062600100720     c                   movel(p)  DsGenerica    EDS00UNA
062700100120      *
062800100120     c                   when      tipo_seg = 'UNB'
062900100720     c                   movel(p)  DsGenerica    EDS00UNB
063000100120     C                   EXSR      DATI_UNB
063100100120      * --------
063200100120     c                   when      tipo_seg = 'UNH'
063300100720     c                   movel(p)  DsGenerica    EDS00UNH
063400100120     C                   EXSR      DATI_UNH
063500100120      * --------
063600100120     c                   when      tipo_seg = 'BGM'
063700100720     c                   movel(p)  DsGenerica    EDS00BGM
063800100120      **--------
063900100120     c                   when      tipo_seg = 'DTM'
064000100720     c                   movel(p)  DsGenerica    EDS00DTM
064100100120     C                   EXSR      DATI_DTM
064200100120      * --------
064300100120     c                   when      tipo_seg = 'TSR'
064400100720     c                   movel(p)  DsGenerica    EDS00TSR
064500100120      **--------
064600100120     c                   when      tipo_seg = 'CNT'
064700100720     c                   movel(p)  DsGenerica    EDS00CNT
064800100120      **--------
064900100120     c                   when      tipo_seg = 'CUX'
065000100720     c                   movel(p)  DsGenerica    EDS00CUX
065100100120      * --------
065200100120     c                   when      tipo_seg = 'TOD'
065300100720     c                   movel(p)  DsGenerica    EDS00TOD
065400100120     C                   EXSR      DATI_TOD
065500100120      **--------
065600100120     c                   when      tipo_seg = 'RFF'
065700100720     c                   movel(p)  DsGenerica    EDS00RFF
065800100120     C                   EXSR      DATI_RFF
065900100120      **--------
066000100120     c                   when      tipo_seg = 'TDT'
066100100720     c                   movel(p)  DsGenerica    EDS00TDT
066200100120      **--------
066300100120     c                   when      tipo_seg = 'MOA'
066400100720     c                   movel(p)  DsGenerica    EDS00MOA
066500100120     C                   EXSR      DATI_MOA
066600100120      **--------
066700100120     c                   when      tipo_seg = 'FTX'
066800100720     c                   movel(p)  DsGenerica    EDS00FTX
066900100120     C                   EXSR      DATI_FTX
067000100120      **--------
067100100120     c                   when      tipo_seg = 'NAD'
067200100720     c                   movel(p)  DsGenerica    EDS00NAD
067300100120     C                   EXSR      DATI_NAD
067400100120      **--------
067500100120     c                   when      tipo_seg = 'CTA'
067600100720     c                   movel(p)  DsGenerica    EDS00CTA
067700100120     C                   EXSR      DATI_CTA
067800100120      **--------
067900100120     c                   when      tipo_seg = 'COM'
068000100720     c                   movel(p)  DsGenerica    EDS00COM
068100100120     C                   EXSR      DATI_COM
068200100120      **--------
068300100120     c                   when      tipo_seg = 'GID'
068400100720     c                   movel(p)  DsGenerica    EDS00GID
068500100120     C                   EXSR      DATI_GID
068600100120      **--------
068700100120     c                   when      tipo_seg = 'MEA'
068800100720     c                   movel(p)  DsGenerica    EDS00MEA
068900100120     C                   EXSR      DATI_MEA
069000100120      **--------
069100100120     c                   when      tipo_seg = 'PCI'
069200100720     c                   movel(p)  DsGenerica    EDS00PCI
069300100120     C                   EXSR      DATI_PCI
069400100120      **--------
069500100120     c                   when      tipo_seg = 'UNT'
069600100720     c                   movel(p)  DsGenerica    EDS00UNT
069700100120     C                   EXSR      DATI_UNT
069800100120      **--------
069900100120     c                   when      tipo_seg = 'UNZ'
070000100720     c                   movel(p)  DsGenerica    EDS00UNZ
070100100120     C                   EXSR      DATI_UNZ
070200100120      **--------
070300100120     c                   OTHER
070400100120      **--------
070500100120     c                   move      'S'           seg_imprevisto
070600100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
070700100120     c                   exsr      Error_DET
070800100120     c                   goto      end_Decod
070900100120      **--------
071000100120     c                   endSL
071100100120      **
071200100120     c     end_Decod     endSR
071300090211     C*----------------------------------------------------------------
071400090211     C*? AZZMSG - Azzera dati messaggio
071500090211     C*----------------------------------------------------------------
071600090211     C     AZZMSG        BEGSR
071700090211     C*
071800090211     C* Pulisco dati di work x foglio viaggio
071900090211     C                   MOVEL     *BLANKS       WNUMFV           35
072000090211     C                   MOVEL     *BLANKS       WNRCMR           35
072100090211     C                   Z-ADD     0             WDATPA            8 0
072200090211     C                   Z-ADD     0             WDATFV            8 0
072300090211     C                   Z-ADD     0             WHHNFV            4 0
072400090211     C                   Z-ADD     0             WDTCMR            8 0
072500090211     C                   Z-ADD     0             WHHCMR            4 0
072600090211      *
072700090211      * pulisce gli RFF+FF di testata
072800090211     C                   clear                   $CLKSC            7 0
072900090211     C                   clear                   $CLTAR            3
073000090211     C                   clear                   $CLLNP            3 0
073100090211     C                   clear                   $CLNRS            2 0
073200090211     C                   clear                   $CLCTM            2
073300090211     C                   clear                   $CLBS1            1
073400090211     C                   clear                   $CLfnsp           1            *blk/S
073500090211      *
073600090211     C*  Inizializzo variabile new documento
073700090211     C                   Z-ADD     0             KCL
073800090211     C                   Z-ADD     0             WW                3 0
073900090211     ** Inizializza campo di work
074000090211     c                   move      'N'           WGID99
074100090211      * per controllare se almeno un record è stato importato sul VAB
074200090211     c                   clear                   Almeno_Uno        1
074300090213     c                   clear                   Almeno_Un_Due     1
074400090211     c                   eval      esito  = '0'
074500090211     C*
074600090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
074700090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
074800090603     c                   eval      Dettaglio_NAD_destinatario = *blank
074900090603     c                   eval      NAD_destinatario_in_testata = *blank
075000090603     c                   eval      NAD_mittente_in_testata = *blank
075100090603     c                   eval      Inizio_Dettaglio = *blank
075200090603     C*
075300090603     c                   clear                   tes_VABrmo
075400090603     c                   clear                   tes_VABnmo
075500090603     c                   clear                   tes_VABcmo
075600090603     C*
075700090603     c                   clear                   tes_VABrsd
075800090603     c                   clear                   tes_VABrd2
075900090603     c                   clear                   tes_VABind
076000090603     c                   clear                   tes_VABlod
076100090603     c                   clear                   tes_VABnzd
076200090603     C*
076300090603     c                   clear                   tes_Valuta
076400090603     C*
076500090211     C                   ENDSR
076600090210     C*----------------------------------------------------------------
076700090210      *? DECODIFICA DATI UNB
076800090210     C*----------------------------------------------------------------
076900090210     C     DATI_UNB      BEGSR
077000090210      *
077100090209      *  INIZIALIZZA  dati fondamentali messaggio
077200090209     C                   clear                   EDIDSPT
077300090209     C                   clear                   EDIDSPU
077400090209     C                   clear                   EDIDSPV
077500090209     C                   clear                   wnrDOC           35
077600090209     C                   move      'N'           PT_ok             1
077700090209      ** ---------------------------------------------------------------
077800090209      * quindi 1° cosa:
077900090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
078000090209      *     senza questo il messaggio NON è trascodificabile
078100090209     C                   MOVEL     unb0004       WCOD             35
078200100114     C                   eval      wcod = %trim(WCOD) + ':' + %trim(unb0010)
078300100114     C                   MOVE      unb000720     WCOD
078400090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
078500090209     C                   Z-ADD     1             XX                3 0
078600090209     C     WCOD          LOOKUP    CPT(XX)                                32
078700090209      *
078800090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
078900090209     C                   If        *IN32 = *off
079000090209      * ******  Errore generale sul Messaggio
079100090209      * Msg di allerta Traduzione
079200090209     C                   EVAL      Indirizzi = CED_Bart
079300091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
079400090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
079500090209     C                             Bolle import in filiale - messaggio -
079600090209     C                             con UNB non presente in tabella PT controlla-
079700090209     C                             re EDI ricevuto su UPLOAD.'
079800090209      *
079900090209     c                   exsr      invio_mail
080000090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
080100090210     C                   eval      vinFLG = '2'
080200090210     C                   eval      se_errore   = 'S'
080300090210     c                   goto      end_UNB
080400090209      *
080500090209     C                   move      'N'           PT_ok             1
080600090209      *
080700090209      *             *------------------------------*
080800090209     C                   eLSe
080900090209      *             *------------------------------*
081000090209      * Se tutto OK su tab.PT:
081100090209     C                   move      'S'           PT_ok             1
081200090209      *
081300090209     C                   MOVEL     DPT(XX)       EDIDSPT
081400090209     C                   MOVEL     DPU(XX)       EDIDSPU
081500090209     C                   MOVEL     DPV(XX)       EDIDSPV
081600091016     c                   clear                   Mittente         35
081700091016     c                   clear                   Mitt_Qualifier    4
081800091016     c                   clear                   Id_Messaggio     14
081900090226     C                   eval      Mittente       = unb0004
082000090226     C                   eval      Mitt_qualifier = unb000720
082100090226     C                   eval      Id_Messaggio   = UNB0022
082200090209      *
082300090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
082400090209      *  ricevuti:
082500090209     c                   movel     §PVema        mail_ind         44
082600090209      *
082700090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
082800090209     c                   if        mail_ind <> *blank
082900090209     c                   exsr      imp_ADDR_mail
083000090209     c                   end
083100090209      *
083200090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
083300090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
083400090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
083500090209     C                   if        §PULUN = *blank
083600090209     C                   eval      §PULUN = '00'
083700090209     c                   end
083800090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
083900090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
084000090209      *
084100090209     C                   Z-ADD     §PTKSC        WKSC              7 0
084200090211     C                   MOVEL     §PTLNP        VABlnp_PT
084300090212     C                   MOVEL     §PTNRS        VABnrs_PT
084400090211     C                   MOVEL     §PTCTM        VABctm_PT
084500090209      *
084600090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
084700090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
084800090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
084900090209      *  i records trasmessi).
085000090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
085100090209      *
085200090209      *  In base al cod.trattamento merce reperisco tipo tratt.
085300090209     C                   CLEAR                   DS1B
085400090209     C                   Z-ADD     1             Y                 3 0
085500090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
085600090209     C     *IN32         IFEQ      '1'
085700090209     C                   MOVEL     DTM(Y)        DS1B
085800090209     C                   MOVEL     DS1B          WSVTRM           38
085900090209     C                   END
086000090209      *
086100090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
086200090209     C                   clear                   ACORAG
086300090209     C                   clear                   INDCAE
086400090209     C                   clear                   INDCAP
086500090209     C                   clear                   INDSTA
086600090209      * Decodifico partner
086700090209     C                   Z-ADD     1             KKUT
086800090209     C                   Z-ADD     KCI           KKCC
086900090209     C                   MOVE      §PTKSC        KKSC
087000090209     C     KACO          CHAIN     CNACO00F                           31
087100090209     C     KIND          CHAIN     CNIND00F                           31
087200090211      *
087300090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
087400090211      *   questa schiera è utilizzata anche per i clienti.
087500090211     C     §ptKSC        LOOKUP    KCL                                    39
087600090211     c                   If        *in39 = *off
087700090211     C                   add       1             WW
087800090211     C                   z-add     §ptKSC        KCL(WW)
087900090211     C                   Endif
088000090209      *
088100090209     C                   MOVEL     unb0017       WAA4              4 0
088200090209     C                   MOVE      WAA4          WAA2              2 0
088300090209     C                   MOVE      unb0017       WGG               2 0
088400090209     C                   MOVE      unb0017       WDTPRE            8 0
088500090209     C                   MOVE      unb0019       WHMPRE            4 0
088600090209     C                   MOVE      unb0017       WDTMSG            6 0
088700090209     C                   MOVE      WAA2          WDTMSG
088800090209     C                   MOVEL     WGG           WDTMSG
088900100113     c****
089000090209     C                   IF        unb0022 <> *blank
089100090209     C                   MOVEL     unb0022       WNRDOC
089200090211     C                   MOVEL     unb0022       WNUMFV
089300090211     C                   MOVEL     unb0022       WNRCMR
089400090209     c                   end
089500090209      *
089600090209     c                   Endif
089700090209      **
089800090210     c     end_UNB       endSR
089900090209      * ?================================================================== */
090000090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
090100090209      * ?================================================================== */
090200090227     C     Nuova_spediz  BEGSR
090300090227      **
090400090227      * Pulisco campi del file
090500090227     c                   z-add     vnrec         SAVNUM_RECord
090600090227     C                   CLEAR                   EDiVAB00
090700090227     C                   CLEAR                   EDiVAT00
090800090227     C                   CLEAR                   EDiVAD00
090900090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
091000090227     C                   MOVEL     'N'           WGID99            1
091100090227     C                   clear                   XY                5 0
091200090227     C                   clear                   §§                5 0
091300090227     C                   clear                   WS1496            5 0
091400090227     C                   clear                   WNSP             35
091500090227     C                   Z-ADD     *ZEROS        PVLB
091600090227     C                   MOVEA     *HIVAL        SGN
091700090227     C*  Azzero codice cliente - tariffa
091800090227     C                   Z-ADD     0             WTOCOC           16 0
091900090227     C                   Z-ADD     0             WTOPLC           16 2          ???
092000090227     C                   Z-ADD     0             WTOPNC           16 2          ???
092100090227     C*  Azzero codice cliente - tariffa
092200090227     C                   clear                   WCLKSC
092300090227     C                   clear                   WCLTAR
092400090227     C                   clear                   WCLLNP
092500090227     C                   clear                   WCLNRS
092600090227     C                   clear                   WCLCTM
092700090227     C                   clear                   WCLBS1
092800090227     C                   clear                   WCLfnsp                        *blk/S
092900090227     C                   clear                   WNOT1            35
093000090227     C                   clear                   WNOT2            35
093100090227     C*  Campi di work
093200090227     c                   clear                   wVabTIC
093300090227     c                   clear                   wri_vabtic        1
093400090227      *
093500090227     C* pulisce campi di work
093600090227     c                   clear                   watrde           35
093700090227     c                   clear                   wattel           25
093800090227     C                   clear                   WRMN              3
093900090227      *
094000090227      * Un errore nel dettaglio
094100090227     C                   clear                   Err_in_detta      1
094200090227     c                   clear                   errori_in_Wrt     1
094300090227     c                   clear                   se_trovato_NAD    1
094400090227      **
094500090227     c                   endSR
094600090227      * ?================================================================== */
094700090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
094800090227      * ?================================================================== */
094900090227     C     DATI_UNH      BEGSR
095000090227      *
095100090227      *  Prepara una nuova spedizione
095200090227      * ?                         --------------
095300090227     c                   exsr      Nuova_spediz
095400090227      * ?                         --------------
095500090211      *
095600090211      ** Riferimento del cliente
095700090211     C                   MOVEL     unh0062       WPRGSP           35
095800090211     C                   MOVEL     unh0062       WNSP             35
095900090211     C                   MOVEL     unh0062       WNUM             35
096000090211      * ?                                       --------
096100090211     C                   MOVEL     unh0062       VABRMA
096200090211      * ?                                       --------
096300090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
096400090211     C                   SUB       1             WLUNGH
096500090211     C                   EXSR      TSTNUM
096600090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
096700090211      * ?                                       --------
096800090211     C                   MOVEL     WNUMOK        VABRMN
096900090211      * ?                                       --------
097000090211     C                   END
097100090210      **
097200090303     C                   MOVEL     VABlnp_PT     VABLNP
097300090303     C                   MOVEL     VABnrs_PT     VABNRS
097400090303     C                   MOVEL     VABctm_PT     VABCTM
097500090303      **
097600090303      **
097700090210     c     end_UNH       endSR
097800090210      * ?================================================================== */
097900090601     C*? Imposta la VALUTA di eventuali C.O.D.
098000090210      * ?================================================================== */
098100090601     C     DATI_CUX      BEGSR
098200090210     C*
098300090603     C*   Valuta in Testata per tutti gli importi presenti sui Dettagli C.O.D.
098400090603     c                   if        cux6345 <> *blank
098500090603     C                   MOVEL     cux6345       Tes_Valuta
098600090603     c                   end
098700090601      **
098800090601     c     end_CUX       endSR
098900090601      * ?================================================================== */
099000090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
099100090601      * ?================================================================== */
099200090601     C     DATI_DTM      BEGSR
099300090601     C*
099400100113     C*
099500090209     C*  Se data documento  memorizzo numero e data x CMR
099600100119     C     dtm2005       IFEQ      '137'
099700090209     C                   MOVEL     dtm2380       WDATA            35            Data
099800090209     C                   MOVEL     dtm2379       WFORM             3            Formato
099900090209     C                   EXSR      CNVDAT
100000100119     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
100100090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
100200090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
100300090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
100400090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
100500090209     C     WERRO         IFEQ      'S'
100600090210     C                   eval      vinMSG = 'DTM data errata'
100700090213     c                   exsr      Error_DET
100800090210     c                   goto      end_DTM
100900090209     C                   END
101000090209     C                   END
101100100113      **
101200090209     C*  Se data consegna richiesta imposto già sui campi del VAB
101300090603     C     dtm2005       IFEQ      'X'
101400090209     C                   MOVEL     dtm2380       WDATA            35            Data
101500090209     C                   MOVEL     dtm2379       WFORM             3            Formato
101600090209     C                   EXSR      CNVDAT
101700090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
101800090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
101900090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
102000090209     C     WERRO         IFEQ      'S'
102100090210     C                   eval      vinMSG = 'DTM data errata'
102200090213     c                   exsr      Error_DET
102300090210     c                   goto      end_DTM
102400090209     C                   END
102500090209     C                   END
102600090211      **
102700090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
102800090603     C     dtm2005       IFEQ      '10'
102900090211     C                   MOVEL     dtm2380       WDATA            35            Data
103000090211     C                   MOVEL     dtm2379       WFORM             3            Formato
103100090211     C                   EXSR      CNVDAT
103200090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
103300090211     C     WERRO         IFEQ      'S'
103400090211     C                   eval      vinMSG = 'DTM data errata'
103500090213     c                   exsr      Error_DET
103600090211     c                   goto      end_DTM
103700090211     C                   END
103800090211     C                   END
103900090209      **
104000090210     c     end_DTM       endSR
104100090209      * ?================================================================== */
104200090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
104300090209      * ?================================================================== */
104400090209     C     DATI_TOD      BEGSR
104500090209      *
104600090210      *  Trascodifico tipo trasporto
104700090210     c                   clear                   TOD_char3         3
104800090210      *
104900090210      *  imposta il default dalla tabella PT se c'è
105000090210     C     §puPFA        ifNE      *blank
105100090210     c                   SELECT
105200090210      *  Porto Franco
105300090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
105400090210     C                   movel     '4'           VABCBO                         + C/Ass
105500090210     c                   WHEN      §puPFA = 'F'
105600090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
105700090210      *  Porto Assegnato
105800090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
105900090210     C                   movel     '6'           VABCBO                         + C/Ass
106000090210     C                   WHEN      §puPFA = 'A'
106100090210     C                   movel     '2'           VABCBO                         Senza C/Ass
106200090210     C                   ENDSL
106300090210     c                   end
106400090210      *
106500090210      *   Imposta o il codice o il metodo
106600090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
106700090210     c                   if        tod4053  <> *blank
106800090210     c                   movel(p)  tod4053       TOD_char3
106900090210     c                   else
107000090210     c                   if        tod4215  <> *blank
107100090210     c                   movel(p)  tod4215       TOD_char3
107200090210     c                   end
107300090210     c                   end
107400110328      ****************
107500110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
107600110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
107700110328      ****************
107800110328     c                   eval      Trovata_Tab_TB ='N'
107900110328      *
108000110328     c                   if              TOD_char3 <> *blank
108100110328     c                   movel(p)  TOD_char3     Key_Generica35
108200110328     c                   move      §puTB         Key_Generica35
108300110328     C                   eval      Y = 1
108400110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
108500110328     c   32              eval      Trovata_Tab_TB ='S'
108600110328      *
108700110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
108800110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
108900110328      *
109000110328     c                   if          Trovata_Tab_TB ='N'
109100110328     c                                 and
109200110328     C                               tod4215 <> *blank
109300110328     c                   movel(p)  tod4215       TOD_char3
109400110328     c                   movel(p)  TOD_char3     Key_Generica35
109500110328     c                   move      §puTB         Key_Generica35
109600110328     C                   eval      Y = 1
109700110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
109800110328     c   32              eval      Trovata_Tab_TB ='S'
109900110328     c                   end
110000110328     c                   end
110100110328      *
110200110328      ****************
110300110328      ******* Se vi sono eccezioni sul cliente imposta quelle
110400110328      ****************
110500110328     c                   If          Trovata_Tab_TB ='S'
110600110328      *
110700110328     c                   SELECT
110800110328     c                   WHEN      Decod_Porto(Y) = 'F'
110900110328     c                               and  VABcas > *zeros
111000110328     C                   movel     '4'           VABCBO                         + C/Ass
111100110328     c                   WHEN      Decod_Porto(Y) = 'F'
111200110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
111300110328      *  Porto Assegnato
111400110328     c                   WHEN      VABCAS > *zeros
111500110328     C                   movel     '6'           VABCBO                         + C/Ass
111600110328     C                   OTHER
111700110328     C                   movel     '2'           VABCBO                         Senza C/Ass
111800110328     C                   ENDSL
111900110328      *
112000110328      ****************  altrimenti
112100110328      ******* Se non vi sono eccezioni per il cliente
112200110328      *   Rileva a questo punto lo Standard per tutti
112300110328      ****************
112400110328     c                   elseIf      Trovata_Tab_TB ='N'
112500090210      **
112600090210     c                   if              TOD_char3 <> *blank
112700091203???? c                   movel(p)  TOD_char3     fld35a           35
112800091203???? C                   movel     UNB_Mittente  etbUNB
112900091203???? C                   movel(p)  'TOD'         etbSGM
113000091203???? C                   movel(p)  TOD_char3     etbVALSGM
113100090213     c     K_TAB1L       chain     edstbl01l
113200090210      *
113300090210      * prova quindi senza l'UNB specifico del cliente
113400090213     c                   if        not %Found(edstbl01l)
113500090213     C                   clear                   etbUNB
113600090213     c     K_TAB1L       chain     edstbl01l
113700090210     c                   end
113800090210      *
113900090213     c                   if        %Found(edstbl01l)
114000090210     c                   SELECT
114100090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
114200090210     C                   movel     '4'           VABCBO                         + C/Ass
114300090213     c                   WHEN      ETBDATI = 'F'
114400090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
114500090210      *  Porto Assegnato
114600090210     c                   WHEN      VABCAS > *zeros
114700090210     C                   movel     '6'           VABCBO                         + C/Ass
114800090210     C                   OTHER
114900090210     C                   movel     '2'           VABCBO                         Senza C/Ass
115000090210     C                   ENDSL
115100090210      *
115200090210     c                   end
115300090210     c                   endIF
115400110328      **
115500110328     c                   ENDif
115600090209      **
115700090210      **  Il tipo bolla non è presente
115800090210     c                   if        VABCBO = *blank
115900090210     C                   eval      vinMSG = 'TOD non rilevato'
116000090213     c                   exsr      Error_DET
116100090210     c                   goto      end_TOD
116200090210     c                   end
116300090210      **
116400090210     c     end_TOD       endSR
116500090210      * ?================================================================== */
116600090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
116700090210      * ?================================================================== */
116800090210     C     DATI_RFF      BEGSR
116900090210      *
117000090210     C                   clear                   WAGE             35
117100110224     C                   clear                   WFF              35
117200110224     C                   clear                   WCU              35
117300090210      *
117400090210      *  Trascodifico riferimento spedizione
117500091019     c                   if        RFF1153 = Riferimento_Spedizione  and
117600091019     c                             RFF1154 <> *BLANKS
117700090210     C                   movel(p)  RFF1154       WAGE             35
117800090210     c                   end
117900090210      **
118000090210     C                   IF         WAGE <> *BLANKS
118100090210     C                   movel     WAGE          WNSP             35
118200090210     C                   movel     WAGE          WNUM             35
118300090210     C     ' '           scan      WAGE          WLUNGH            3 0
118400090210     C                   sub       1             WLUNGH
118500090210     C                   EXSR      TSTNUM
118600090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
118700090210     C                   movel     WAGE          VABRMA
118800091019     C                   IF         WRMN = Riferimento_Spedizione  or
118900091019     C                              WRMN = *blanks
119000090210     C                   movel     WNUMOK        VABRMN
119100091019     C                   eval       WRMN = Riferimento_Spedizione
119200090210     C                   ENDIF
119300090210     C                   ENDIF
119400090210     C                   ENDIF
119500090210      *
119600090210      * Codice Cliente di riferimento
119700091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
119800090210     C                   movel     RFF1154       WFF
119900090210     c                   END
120000090211      *
120100090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
120200090211     C     WFF           IFNE      *BLANKS
120300090211     C                   eval      XX = 1
120400090211     C                   movel     §PTKSC        WCCL             35
120500090211     C                   movel     WFF           WCOM28           28
120600090211     C                   move      WCOM28        WCCL
120700090211     C     WCCL          lookup    CCL(XX)                                34
120800090211     C     *IN34         IFEQ      '1'
120900090211     C                   movel     DCL(XX)       EDIDSCL
121000090211     C                   z-add     §CLKSC        WCLKSC            7 0
121100090211     C                   movel     §CLTAR        WCLTAR            3
121200090211     C                   z-add     §CLLNP        WCLLNP            3 0
121300090211     C                   z-add     §CLNRS        WCLNRS            2 0
121400090211     C                   movel     §CLCTM        WCLCTM            2
121500090211     C                   movel     §CLBS1        WCLBS1            1
121600090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
121700090211      * imposta la LNP   x il dettaglio specifco
121800090211     c                   eval      VABlnp = §CLlnp
121900090211      * imposta la Serie x il dettaglio specifco
122000090211     c                   eval      VABnrs = §CLnrs
122100090211      *
122200090211      *  In base al cod.trattamento merce reperisco tipo tratt.
122300090211      *    per un dettaglio specifico.
122400090211     C                   CLEAR                   DS1B
122500090211     C                   Z-ADD     1             Y                 3 0
122600090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
122700090211     C     *IN32         IFEQ      '1'
122800090211     C                   MOVEL     DTM(Y)        DS1B
122900090211     C                   END
123000090211      *
123100090211      * Se il cliente non era sulla schiera lo aggiunge
123200090211     C     §CLKSC        lookup    KCL                                    39
123300090211     C                   if        *in39 = *off
123400090211     C                   add       1             WW
123500090211     C                   z-add     §CLKSC        KCL(WW)
123600090211     c                   endif
123700090211      *
123800090211     C                   ENDIF
123900090211     C                   ENDIF
124000090211     C*----------------------------------------------------------------
124100090210      *
124200090210     C                   if        RFF1153 = 'CU ' and RFF1154 <> *blanks
124300090210     C                   movel     RFF1154       WCU
124400090210     C                   END
124500090210      *
124600090210      * Numero Ordine di vendita
124700090210     C     WCU           IFNE      *BLANKS
124800090210     C                   movel     WCU           WNUM             35
124900090210     C     ' '           scan      WCU           WLUNGH
125000090210     C                   sub       1             WLUNGH
125100090210     C                   EXSR      TSTNUM
125200090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
125300090210     C                   movel     WNUMOK        VABRMN
125400090210     C                   movel     'CU '         WRMN
125500090210     C                   Endif
125600090210     C                   ENDIF
125700090210      *
125800090210      * NOn c'è l'identificativo bolla del cliente
125900091019     c                   if        (RFF1153 = Riferimento_Spedizione and
126000091019     c                              RFF1154 = *BLANKS)
126100091019     c                             or
126200090211     c                             (RFF1153 = 'CU' and RFF1154 = *BLANKS)
126300090210     C                   eval      vinMSG = 'RFF riferimento assente'
126400090213     c                   exsr      Error_DET
126500090210     c                   goto      end_RFF
126600090210     C                   ENDIF
126700090210      *
126800090210     c     end_RFF       endSR
126900090209      * ?================================================================== */
127000090213     C*?  C.O.D. Monetary Amount
127100090209      * ?================================================================== */
127200090213     C     DATI_MOA      BEGSR
127300090210      *
127400090216     C* Controllo se campo importo numerico
127500090216      * con 3 decimali
127600090216     C     moa5004       IFNE      *zeros
127700090216      *
127800090216     C     moa5025       IFEQ      '157'
127900090216     C     moa5004       div       1000          VABIAS                         IMPORTO
128000090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
128100090216     C                   END
128200090216      *
128300090216     C     moa5025       IFEQ      '77 '                                        Valore
128400090216     C     moa5004       div       1000          VABVMD                         MERCE
128500090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
128600090216     C                   END
128700090216      *
128800090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
128900090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
129000090216     C     moa5004       div       1000          VABCAS
129100090216     C                   MOVEL     moa6345       VABVCA
129200100120     c                   if        Tes_valuta <> *blank and VABVCA = *blank
129300100120     C                   MOVEL     Tes_valuta    VABVCA
129400100120     c                   end
129500090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
129600090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
129700090216     C     §PUTPC        IFEQ      'P'
129800090216     C     moa5004       ANDGT     *ZEROS
129900090216     C                   MOVEL     'OM'          VABTIC
130000090216     C                   MOVEL     'OM'          WVABTIC
130100090216     C                   ENDIF
130200090216     C                   END
130300090216     C                   END
130400090216      *
130500090216     C                   END                                                    Imp.non numer.
130600090213      *
130700090213     c     end_MOA       endSR
130800090213      * ?================================================================== */
130900090213     C*?  FREE TEXT
131000090213      * ?================================================================== */
131100090213     C     DATI_FTX      BEGSR
131200090213      *
131300090216      * ? solo se si tratta di note SIN ICN o PAI non altro
131400090216     C                   DO        4             X
131500090216     C     D05(X)        IFNE      *BLANKS
131600090216     C     WNOT1         IFEQ      *BLANKS
131700090216     C                   MOVEL     D05(X)        WNOT1
131800090216     C                   MOVE      D05(X)        WNOT2
131900090216     C                   ELSE
132000090216     C     WNOT2         IFEQ      *BLANKS
132100090216     C                   MOVEL     D05(X)        WNOT2
132200090216     C                   END
132300090216     C                   END
132400090216     C                   END
132500090216     C                   END
132600090216     C*------>>
132700090216     C*  Decodifico tipo pagamento C/Assegno
132800090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
132900090216      *      (Problema di pulizia in presenza di + righe di testo)
133000090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
133100090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
133200090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
133300090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
133400090216     C                   if        wri_vabtic = *blank
133500090216     C                   MOVEL     *BLANKS       VABTIC
133600090216     c                   end
133700090216      **
133800090216     c                   clear                   trovato_TPI       1
133900091203      *
134000091203      * ?Se Natura Merce      .
134100091203     C     ftx4451       IFEQ      'AAA'                                        -- 01 --
134200091203     C                   MOVEL     D05(1)        VABnas
134300091203     c                   End
134400091203      *
134500090216      *
134600090216      * ?Se c'è il tipo Incasso
134700090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
134800090216      *
134900090216     C                   DO        4             X                              -- 02 --
135000090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
135100090216     C                   MOVEL     D05(X)        WCTC              2
135200090216     C                   Z-ADD     1             Y
135300090216     c                   movel(p)  Wctc          fld35a           35
135400090216     c                   move      §puTC         fld35a
135500090216     C     fld35a        LOOKUP    CTC35(Y)                               33
135600090216     C   33              MOVEL     TPI(Y)        VABTIC
135700090216     c   33              move      'S'           trovato_TPI
135800090216     C                   END                                                    -- 03 --
135900090216     C                   ENDdo                                                  -- 02 --
136000090216      *
136100090216     C                   ELSE                                                   -- 01 --
136200090216      *
136300090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
136400090216      *    Deve essere impostato comunque da testata
136500090216     c                   if        vabtic = *blank
136600090216     C                   MOVEL     wVabTIC       VABTIC
136700090216     c                   end
136800090216      *
136900090216     C                   END                                                    -- 01 --
137000090216      *
137100090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
137200090216      *    essere presenti altri record Flat con altre informazioni
137300090216      *    che potrebbero abblancare il Tipo incasso precedentemente
137400090216      *    decodificato
137500090216     c                   if        Trovato_TPI = 'S' and
137600090216     c                                 wri_vabtic = *blank
137700090216     c                   eval      wri_vabtic = 'S'
137800090216     c                   end
137900090213      *
138000090213     c     end_FTX       endSR
138100090213      * ?================================================================== */
138200090213     C*?  Anagrafica del mittente e del destinatario
138300090213      * ?================================================================== */
138400091016     C     DATI_NAD      BEGSR
138500090213      *
138600090210      * Dati destinatario
138700090210     C                   IF        nad3035 = 'CN' or
138800090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
138900090227      *
139000090227      * Se trovato un indirizzo di destinatario
139100090227     c                   eval       se_trovato_NAD ='S'
139200090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
139300090210      *
139400090210     C* Reperisco nazione corrispondente destinatario
139500090210     C                   Z-ADD     1             YY                3 0
139600090210     c                   eval      *in32 = *off
139700090210     C                   if        nad3207 <> *BLANKS
139800090210     C     nad3207       LOOKUP    ISO(YY)                                32
139900090210     C                   endif
140000090210      *
140100090210     C* Imposto dati destinatario
140200090211     C                   MOVEL     nad31241      VABRSD
140300090211     C                   if        nad31242 <> *LOVAL
140400090211     C                   MOVEL     nad31242      VABRD2
140500090210     C                   endif
140600090211      *
140700091019????  * DAL PARTY NAME
140800091019???? C* Imposto dati destinatario
140900091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
141000091019???? C                   MOVEL     nad30361      VABRSD
141100091019???? C                   if        nad30362 <> *LOVAL
141200091019???? C                   MOVEL     nad30362      VABRD2
141300091019???? C                   endif
141400091019???? C                   end
141500090211      *
141600090210     C                   MOVEL     nad30421      VABIND
141700090210     C                   MOVEL     nad3164       VABLOD
141800090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
141900090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
142000090210      *
142100090210     C* Verifico se c'è dello spazio per nella località
142200090210     C     '    '        SCAN      VABLOD        WI                4 0
142300090210      *
142400090210     C                   MOVEA     VABLOD        LOD
142500090210     C                   ADD       1             WI
142600090210     C                   MOVEA     NAD30422      LOD(WI)
142700090210     C                   MOVEA     LOD           VABLOD
142800090210     C                   ENDif
142900090210      *
143000090210     C                   If        *in32 = *on
143100090210     C                   MOVEL     C15(YY)       VABNZD
143200090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
143300090210     c                   Else
143400090210     C                   MOVEL     *BLANKS       VABNZD
143500090210     C                   Z-ADD     0             WFLCPF
143600090210     c                   Endif
143700090210      * Converto CAP
143800090210     C                   EXSR      CNVCAP
143900090603     C*
144000090603     C* Se in testata
144100090603     c                   if        Inizio_dettaglio = *blank
144200090603     c                   eval       tes_VABrsd = VABrsd
144300090603     c                   eval       tes_VABrd2 = VABrd2
144400090603     c                   eval       tes_VABind = VABind
144500090603     c                   eval       tes_VABlod = VABlod
144600090603     c                   eval       tes_VABnzd = VABnzd
144700090603     c                   eval      NAD_destinatario_in_testata = 'S'
144800090603     c                   end
144900090603     C*
145000090210     C                   ENDIF
145100090210      *
145200090210      * Dati mittente
145300091016     C                   IF        nad3035 = 'CZ'
145400090210     C                   movel     nad30361      VABRMO
145500090210      *
145600090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
145700090210     C                   movel     §PTNAZ        VABNMO
145800090210      *
145900090210     C                   IF        nad3207 <> *blanks
146000090210     C                   eval      YY = 1
146100090210     C     nad3207       LOOKUP    ISO(YY)                                32
146200090210     C                   if         *in32 = *on
146300090210     C                   MOVEL     C15(YY)       VABNMO
146400090210     C                   endif
146500090210     C                   ENDIF
146600090210      *
146700090210      * Normalizzo CAP mittente originale
146800090210     C                   clear                   T
146900090210     C                   clear                   S
147000090210     C                   MOVEA     *BLANKS       CAPM
147100090210     C                   MOVEA     nad3251       CAP9
147200090210     C                   DO        9             T
147300090210     C     CAP9(T)       IFNE      *BLANKS
147400090210     C                   ADD       1             S
147500090210     C                   MOVE      CAP9(T)       CAPM(S)
147600090210     C                   ENDIF
147700090210     C                   ENDDO
147800090210     C*
147900090210      *     Controllo validità CAP
148000090210     C                   MOVEA     CAPM          VABCMO
148100090210      *
148200090210     C                   CLEAR                   TISI95DS
148300090210     C                   MOVEL     VABCMO        I95CAP
148400090210     C                   MOVEL     VABNMO        I95NAR
148500090210     C                   MOVEL     WOGGI         I95DAT
148600090210     C                   MOVEL     '3'           I95TCN
148700090210     C                   CALL      'TISI95R'
148800090210     C                   PARM                    TISI95DS
148900090210      *     Errore
149000090210     C     O95ERR        IFNE      *BLANKS
149100090210     C                   MOVEL     INDCAE        VABCMO
149200090210     C                   MOVEL     §PTNAZ        VABNMO
149300090210     C                   END
149400090603      *
149500090603     C* Se in testata
149600090603     c                   if        Inizio_dettaglio = *blank
149700090603     c                   eval      tes_VABrmo = VABrmo
149800090603     c                   eval      tes_VABnmo = VABnmo
149900090603     c                   eval      tes_VABcmo = VABcmo
150000090603     c                   eval      NAD_mittente_in_testata = 'S'
150100090603     c                   end
150200090210      *
150300090210     C                   END
150400090210      *
150500090210      * NOn c'è l'identificativo bolla del cliente
150600090210     C                   if          vabRSD = *blank or
150700090210     C                               vabIND = *blank or
150800090210     C                               vabLOD = *blank
150900090211     C                   IF        nad3035 = 'CN'
151000090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
151100090213     c                   exsr      Error_DET
151200090210     c                   goto      end_NAD
151300090210     C                   ENDIF
151400090211     C                   ENDIF
151500090210      *
151600090210     c     end_NAD       endSR
151700090210      * ?================================================================== */
151800090210     C*?  Contatto a cui far riferimento
151900090210      * ?================================================================== */
152000090210     C     DATI_CTA      BEGSR
152100090210      *
152200090210      *  memorizza il riferimento di consegna x il destinatario
152300090210      *  memorizza il riferimento di consegna nr.telefonico
152400090210     C     cta3412       ifne      *BLANKS
152500090210     c                   movel     cta3412       watrde
152600090210     c                   end
152700090210      *
152800090210     c                   endSR
152900090210      * ?================================================================== */
153000090210     C*?  Contatto telefonico  a cui far riferimento
153100090210      * ?================================================================== */
153200090210     C     DATI_COM      BEGSR
153300090210      *
153400090210     C     com3148       ifne      *BLANKS
153500090210     c                   movel     com3148       wattel
153600090210     c                   end
153700090210      *
153800090209     C                   ENDSR
153900090210      * ?================================================================== */
154000090210     C*?  Dati di dettaglio della spedizione e dei colli
154100090210      * ?================================================================== */
154200090210     C     DATI_GID      BEGSR
154300090210      *
154400090210     C* Se ho totali per spedizione metto in campi VAB
154500090210     C     gid1496       IFEQ      99999
154600090210     C     gid1496       orEQ      9999
154700090210     C                   MOVEL     'S'           WGID99
154800090210     C                   z-add     gid722420     VABNCL
154900090210     C                   ELSE
155000090210     C     WGID99        IFEQ      'N'
155100090210     C                   z-add     gid722420     WTNCL             5 0
155200090210     C                   ADD       WTNCL         VABNCL
155300090210     C                   END
155400090210     C                   END
155500090212      *
155600090210     C                   ADD       VABNCL        WTOCOC           16 0
155700090210      *
155800090210     c     end_GID       endSR
155900090210      * ?================================================================== */
156000090210     C*?  Dati di dettaglio misure spedizione e colli
156100090210      * ?================================================================== */
156200090210     C     DATI_MEA      BEGSR
156300090210      *
156400090210     C*  Peso
156500090210     C     mea6311       IFEQ      'WT '
156600090210     C     mea6411       ANDEQ     'KGM'
156700090210      *
156800090210     C*  Controllo se ho valore totale o parziale
156900090210      *   sempre il Lordo
157000091019???? C     mea6313       IFEQ      'G  '
157100091204???? C     mea6313       oreq      'AAE'
157200090210     C     WS1496        IFEQ      99999
157300090210     C     WS1496        orEQ      9999
157400091019???? C                   move      mea6314       mea6314_2        18 2          Peso
157500091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
157600090210     C                   ELSE
157700091019???? C     WGID99        IFEQ      'N'
157800091019???? C                   move      mea6314       mea6314_2                      Peso
157900091019???? C                   ADD       mea6314_2     VABPKB                         Peso
158000090210     C                   END
158100090210     C                   END
158200091203     C                   END
158300090210      *
158400091019???? C     mea6313       IFEQ      'G  '
158500091204???? C     mea6313       oreq      'AAE'
158600091019???? C                   move      mea6314       mea6314_2                      Peso
158700091019???? C                   ADD       mea6314_2     WTOPLC
158800090210     C                   END
158900091019???? C     mea6313       IFEQ      'N  '
159000091204???? C     mea6313       oreq      'AAF'
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
160400091204???? C     mea6313       oreq      'AAE'
160500091019???? C     WS1496        IFEQ      99999
160600091019???? C     WS1496        orEQ      9999
160700091019???? c                   move      mea6314       mea6314_2
160800091019???? C     mea6314_2     div       1000          VABPKB                         Peso
160900090210     C                   ELSE
161000091019???? C     WGID99        IFEQ      'N'
161100091019???? c                   move      mea6314       mea6314_2
161200091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
161300091019???? C                   add       Peso_inKG     VABPKB                         Peso
161400090210     C                   END
161500090210     C                   END
161600090210     C                   END
161700090210      *
161800091019???? C     mea6313       IFEQ      'G  '
161900091204???? C     mea6313       oreq      'AAE'
162000091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
162100091019???? C                   ADD       Peso_inKG     WTOPLC
162200090210     C                   END
162300091019???? C     mea6313       IFEQ      'N  '
162400091204???? C     mea6313       oreq      'AAF'
162500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
162600091019???? C                   ADD       Peso_inKG     WTOPNC
162700090210     C                   END
162800090210     C                   END
162900090210     C                   END
163000090210      *  Volume
163100091019???? C     mea6311       IFEQ      'VOL'
163200091019???? C     mea6411       ANDEQ     'MTQ'
163300090210      *  Controllo se ho valore totale o parziale
163400090210     C     WS1496        IFEQ      99999
163500090210     C     WS1496        orEQ      9999
163600091019???? C                   move      mea6314       mea6314_2        18 2          Volume
163700091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
163800091019???? C                   Z-ADD     mea6314_2     PVLB
163900090210     C                   ELSE
164000091019     C     WGID99        IFEQ      'N'
164100091019???? C                   move      mea6314       mea6314_2        18 2          Volume
164200091019???? C                   ADD       mea6314_2     VABVLB                         Volume
164300091019???? C                   ADD       mea6314_2     PVLB
164400090210     C                   END
164500090210     C                   END
164600090210     C                   END
164700090210      *
164800090210     C* Ricerco CAP
164900090210     C     WCAP          IFNE      *BLANKS
165000090210     C     mea6311       IFEQ      'WT '
165100090210      *
165200090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
165300090210     C                   CLEAR                   FNLV55DS
165400090210     C                   MOVEL     'P'           D55TPT
165500090210     C                   MOVEL     WOGGI         D55DRF
165600090210     C                   MOVEL     VABLNP        D55LNP
165700090210     C                   MOVEL     VABLNP        D55LIN
165800090210     C                   CALL      'FNLV55R'
165900090210     C                   PARM                    FNLV55DS
166000090210     C                   MOVE      D55TFP        COMTFP            3 0
166100090210      *
166200090210     C*  PASSO IL TERMINAL PARTENZA
166300090210     C                   CLEAR                   TISI95DS
166400090210     C                   Z-ADD     COMTFP        I95TFP
166500090210     C                   MOVEL     VABTSP        I95TSP
166600090210     C                   MOVEL     'S'           I95FRE
166700090210     C                   MOVEL     '3'           I95TCN
166800090210     C                   MOVEL     WCAP          I95CAP
166900090210     C                   MOVEL     VABNZD        I95NAR
167000090210     C                   MOVEL     VABLOD        I95LOC
167100090210     C                   MOVEL     VABIND        I95IND
167200090210      *
167300090210     C* Se gestita seconda linea di arrivo passo peso - volume
167400090210     C* e numero colli effettivo
167500090210     C     §PULNA        IFEQ      'S'
167600090210     C                   Z-ADD     VABPKB        I95LKG
167700090210     C                   Z-ADD     VABVLB        I95LMC
167800090210     C                   ELSE
167900090210     C* Altrmenti passo 1 Kg e 0,001
168000090210     C***                  Z-ADD1         I95LKG
168100090210     C***                  Z-ADD0,001     I95LMC
168200090210     C                   END
168300090210     C*
168400090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
168500090210     C                   SELECT
168600090210     C     VABCBO        WHENEQ    '1 '
168700090210     C                   MOVEL     *BLANKS       I95FCA
168800090210     C                   MOVEL     'F'           I95TPO
168900090210     C     VABCBO        WHENEQ    '2 '
169000090210     C                   MOVEL     *BLANKS       I95FCA
169100090210     C                   MOVEL     'A'           I95TPO
169200090210     C     VABCBO        WHENEQ    '4 '
169300090210     C                   MOVEL     'S'           I95FCA
169400090210     C                   MOVEL     'F'           I95TPO
169500090210     C     VABCBO        WHENEQ    '6 '
169600090210     C                   MOVEL     'S'           I95FCA
169700090210     C                   MOVEL     'A'           I95TPO
169800090210     C                   OTHER
169900090210     C                   MOVEL     'F'           I95TPO
170000090210     C     VABCAS        IFGT      0
170100090210     C                   MOVEL     'S'           I95FCA
170200090210     C                   ELSE
170300090210     C                   MOVEL     *BLANKS       I95FCA
170400090210     C                   END
170500090210     C                   ENDSL
170600090210     C*
170700090210     C                   CALL      'TISI95R'
170800090210     C                   PARM                    TISI95DS
170900090210     C* Reperisco i dati da CAP
171000090210     C                   MOVEL     O95PRV        VABPRD
171100100114      ***
171200100114      *** dovendo accorpare le spedizioni da Conferma x CMR
171300100114      ***  non deve impostare l'instradamento.
171400100114     C******             MOVEL     O95LNA        VABLNA
171500100114     C******             MOVEL     O95ZNC        VABZNC
171600090210     C                   MOVEL     WCAP          VABCAD
171700090210     C*
171800090210     C     O95ERR        IFNE      *BLANKS
171900090210     C     O95FIT        ANDEQ     'S'
172000090210     C*  Cap non trovato
172100090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
172200090213     c                   exsr      Error_DET
172300090210     C                   goto      end_MEA
172400090210     C                   END
172500090210     C                   END
172600090210      *
172700090210     C                   ELSE
172800090210      *
172900090210     C                   CLEAR                   VABPRD
173000090210     C                   CLEAR                   VABLNA
173100090210     C                   CLEAR                   VABZNC
173200090210     C                   CLEAR                   VABCAD
173300090210     C*  Cap non trovato
173400090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
173500090213     c                   exsr      Error_DET
173600090210     C                   goto      end_MEA
173700090210     C                   END
173800090210      *
173900090210     c     end_MEA       endSR
174000090210      * ?================================================================== */
174100090210     C*?  Dati di dettaglio Codice a barre Segnacolli
174200090210      * ?================================================================== */
174300090210     C     DATI_PCI      BEGSR
174400090210      *
174500090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
174600090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
174700090210     C     §1BF12        IFEQ      'E'
174800090210     C                   EXSR      SEGNEE
174900090210     C                   ELSE
175000090210      *
175100090210      *  La prima volta controllo congruità numero segnacollo
175200090210     C     §PUBS1        IFNE      *BLANKS
175300090210     C     VABnrs        ANDNE     0
175400090210      *  Se il codice trattamento merce prevede che segnacollino i
175500090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
175600090210     C     §1BFG7        IFEQ      'S'
175700090210     C     §1BFG1        OREQ      'S'
175800090210      *  calcolo segnacollo
175900090210     C                   EXSR      SGNELA
176000090210     C                   END
176100090210     C                   END
176200090210      *
176300090210     C                   END
176400090210      *
176500090210     c     end_PCI       endSR
176600090211     C*----------------------------------------------------------------
176700090211      *? dati finali del dettaglio    UNT
176800090211     C*----------------------------------------------------------------
176900090211     C     DATI_UNT      BEGSR
177000090211      *
177100090213     c                   eval      NUM_RECord = vnREC
177200090213     c                   z-add     vnrec         last_RECord
177300090211      **
177400090211     c                   endSR
177500090211     C*----------------------------------------------------------------
177600090211      *? dati finali     UNT
177700090211     C*----------------------------------------------------------------
177800090211     C     DATI_UNZ      BEGSR
177900090211      *
178000090211      **
178100090211     c                   endSR
178200090210      *----------------------------------------------------------------
178300090210      *? Input segnacolli su schiera di 20 elementi
178400090210      *----------------------------------------------------------------
178500090210     C     INP_Segnacol  BEGSR
178600090210      *
178700090210     c                   clear                   quanti_PCI        3 0
178800090210     c                   if        §puEL1 = 0
178900090210     c                   z-add     10            quanti_PCI
179000090210     c                   else
179100090210     c                   z-add     §puEL1        quanti_PCI
179200090210     c                   end
179300090210      *
179400090210      *  Pulisce la schiera dei segnacolli da elaborare
179500090210     c                   clear                   x30
179600090210     c                   z-add     0             Nr_x30            3 0
179700090210      *
179800090210      *   riporto tutto sulla schiera generica X30
179900090210      *
180000090210     c                   do        10            limite            3 0
180100090210      * Limita quanti elementi passati in ogni PCI
180200090210      *   devono essere considerati
180300090210     c                   if        limite > quanti_PCI
180400090210     c                   Leave
180500090210     c                   end
180600090210      *
180700090210      *  Carica la schiera generale di tutti i segnacolli
180800090210     c                   if        D30_1(limite) <> *blank
180900090210     c                   add       1             Nr_x30
181000090210     c                   eval      X30(Nr_x30) = D30_1(limite)
181100090210     c                   end
181200090210     c                   enddo
181300090210      *
181400090210     C                   ENDSR
181500090210      *----------------------------------------------------------------
181600090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
181700090210      *----------------------------------------------------------------
181800090210     C     SEGNEE        BEGSR
181900090210      *
182000090210     c                   exsr      INP_Segnacol
182100090210      *
182200090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
182300090210     C                   DO        10            x                              --- 01 ---
182400090210      *
182500090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
182600090210     C     x30(X)        ANDNE     *LOVAL
182700090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
182800090210     C                   ADD       1             §§
182900090210     C                   MOVEL     x30(X)        SGE(§§)
183000090210     C                   END                                                    --- 03 ---
183100090210      *
183200090210     C                   END                                                    --- 02 ---
183300090210      *
183400090210     C                   ENDSR
183500090210     C*----------------------------------------------------------------
183600090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
183700090210     C*----------------------------------------------------------------
183800090210     C     SGNELA        BEGSR
183900090210      *
184000090210     c                   exsr      INP_Segnacol
184100090210     C*
184200090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
184300090210     C*  in base al numero elementi validi carico sgn.
184400090210     C                   DO        10            X                              --- 01 ---
184500090210     C*
184600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
184700090210     C     x30(X)        ANDNE     *LOVAL
184800090210     C*
184900090210     C                   CLEAR                   DSSGN
185000090210     C*  Controllo se devo ricevere la linea di partenza
185100090210     C                   MOVEL     'N'           WERRO             1
185200090210     C     §PULP1        IFGT      0                                            --- 03 ---
185300090210     C                   EXSR      REPLNP
185400090210     C                   END                                                    --- 03 ---
185500090210     C*  Controllo se devo ricevere la linea di arrivo
185600090210     C     §PULA1        IFGT      0                                            --- 03 ---
185700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
185800090210     C                   EXSR      REPLNA
185900090210     C                   END                                                    --- 03 ---
186000090210     C*  Controllo se devo ricevere il numero di serie
186100090210     C     §PUNR1        IFGT      0                                            --- 03 ---
186200090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
186300090210     C                   EXSR      REPNRS
186400090210     C                   END                                                    --- 03 ---
186500090210     C*  Controllo se devo ricevere il numero segnacollo
186600090210     C     §PUSG1        IFGT      0                                            --- 03 ---
186700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
186800090210     C                   EXSR      REPSGN
186900090210     C                   END                                                    --- 03 ---
187000090210     C*  Controllo se devo ricevere la zona di consegna
187100090210     C     §PUZN1        IFGT      0                                            --- 03 ---
187200090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
187300090210     C                   EXSR      REPZNC
187400090210     C                   END                                                    --- 03 ---
187500090210     C*  Imposto da VAB i dati a zero se mi è stato passato
187600090210     C*  numero segnacollo valido
187700090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
187800090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
187900090210     C                   Z-ADD     VABLNP        SGNLNP
188000090210     C                   END                                                    --- 04 ---
188100090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
188200090210     C                   Z-ADD     VABLNA        SGNLNA
188300090210     C                   END                                                    --- 04 ---
188400090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
188500090210     C                   Z-ADD     VABNRS        SGNNRS
188600090210     C                   END                                                    --- 04 ---
188700090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
188800090210     C                   Z-ADD     VABZNC        SGNZNC
188900090210     C                   END                                                    --- 04 ---
189000090210      *
189100090210     C*  mi reimposto LNA e zona da segnacollo
189200100114      ***
189300100114      *** dovendo accorpare le spedizioni da Conferma x CMR
189400100114      ***  non deve impostare l'instradamento.
189500090210     C                   Z-ADD     SGNLNA        VABLNA
189600090210     C                   Z-ADD     SGNZNC        VABZNC
189700100114      ***
189800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
189900090210     C                   ADD       1             XY
190000090210     C                   MOVE      SGNNSC        SGN(XY)
190100090210     C                   END                                                    --- 03 ---
190200090210     C*
190300090210     C                   END                                                    --- 02 ---
190400090210     C*
190500090210     C                   END                                                    --- 01 ---
190600090210      *
190700090210     C                   ENDSR
190800090210     C*----------------------------------------------------------------
190900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
191000090210     C*----------------------------------------------------------------
191100090210     C     REPLNP        BEGSR
191200090210     C*
191300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
191400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
191500090210     C*  dal numero segnacollo passatomi dal partner
191600090210     C                   Z-ADD     §PULP1        WP                2 0
191700090210     C     3             SUBST     x30(X):WP     WLNP              3
191800090210     C*
191900090210     C*  controllo che la lnp calcolata sia numerica
192000090210     C                   TESTN                   WLNP                 98
192100090210     C*
192200090210     C*  se è numerica la carico
192300090210     C     *IN98         IFEQ      '1'
192400090210     C                   MOVE      WLNP          SGNLNP
192500090210     C                   ELSE
192600090210     C*  se non è numerica stampo errore
192700090210     C                   MOVEL     'S'           WERRO             1
192800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
192900090213     c                   exsr      Error_DET
193000090210     C                   goto      end_LNP
193100090210     C                   END
193200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
193300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
193400090210     C     SGNLNP        IFNE      VABLNP
193500090210     C                   MOVEL     'S'           WERRO             1
193600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
193700090210     c                             a della Bolla'
193800090213     c                   exsr      Error_DET
193900090210     C                   goto      end_LNP
194000090210     C                   END
194100090210     C*
194200090210     C     end_LNP       ENDSR
194300090210     C*----------------------------------------------------------------
194400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
194500090210     C*----------------------------------------------------------------
194600090210     C     REPLNA        BEGSR
194700090210     C*
194800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
194900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
195000090210     C*  dal numero segnacollo passatomi dal partner
195100090210     C                   Z-ADD     §PULA1        WP                2 0
195200090210     C     3             SUBST     x30(X):WP     WLNA              3
195300090210     C*  controllo che la lna calcolata sia numerica
195400090210     C                   TESTN                   WLNA                 98
195500090210     C*  se è numerica la carico
195600090210     C     *IN98         IFEQ      '1'
195700090210     C                   MOVE      WLNA          SGNLNA
195800090210     C                   ELSE
195900090210     C*  se non è numerica stampo errore
196000090210     C                   MOVEL     'S'           WERRO             1
196100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
196200090213     c                   exsr      Error_DET
196300090210     C                   goto      end_LNA
196400090210     C                   END
196500090210     C*
196600090210     C     end_LNA       ENDSR
196700090210     C*----------------------------------------------------------------
196800090210     C*? REPNRS - Reperisce numero serie
196900090210     C*----------------------------------------------------------------
197000090210     C     REPNRS        BEGSR
197100090210     C*
197200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
197300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
197400090210     C*  dal numero segnacollo passatomi dal partner
197500090210     C                   Z-ADD     §PUNR1        WP                2 0
197600090210     C     2             SUBST     x30(X):WP     WNRS              2
197700090210     C*  controllo che il nrs calcolata sia numerico
197800090210     C                   TESTN                   WNRS                 98
197900090210     C*  se è numerica la carico
198000090210     C     *IN98         IFEQ      '1'
198100090210     C                   MOVE      WNRS          SGNNRS
198200090210     C                   ELSE
198300090210     C*  se non è numerico stampo errore
198400090210     C                   MOVEL     'S'           WERRO             1
198500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
198600090213     c                   exsr      Error_DET
198700090210     C                   goto      end_NRS
198800090210     C                   END
198900090210      *
199000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
199100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
199200090210     C     SGNNRS        IFNE      VABNRS
199300090210     C                   MOVEL     'S'           WERRO             1
199400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
199500090210     c                             o della Bolla'
199600090213     c                   exsr      Error_DET
199700090210     C                   goto      end_NRS
199800090210     C                   END
199900090210     C*
200000090210     C     end_NRS       ENDSR
200100090210     C*----------------------------------------------------------------
200200090210     C*? REPZNC - Reperisce zona consegna
200300090210     C*----------------------------------------------------------------
200400090210     C     REPZNC        BEGSR
200500090210     C*
200600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
200700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
200800090210     C*  dal numero segnacollo passatomi dal partner
200900090210     C                   Z-ADD     §PUZN1        WP                2 0
201000090210     C     2             SUBST     x30(X):WP     WZNC              2
201100090210     C*  controllo che la zona calcolata sia numerica
201200090210     C                   TESTN                   WZNC                 98
201300090210     C*  se è numerica la carico
201400090210     C     *IN98         IFEQ      '1'
201500090210     C                   MOVE      WZNC          SGNZNC
201600090210     C                   ELSE
201700090210     C*  se non è numerica stampo errore
201800090210     C                   MOVEL     'S'           WERRO             1
201900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
202000090213     c                   exsr      Error_DET
202100090210     C                   goto      end_ZNC
202200090210     C                   END
202300090210     C*
202400090210     C     end_ZNC       ENDSR
202500090210     C*----------------------------------------------------------------
202600090210     C*? REPSGN - Reperisce numero segnacollo
202700090210     C*----------------------------------------------------------------
202800090210     C     REPSGN        BEGSR
202900090210     C*
203000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
203100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
203200090210     C*  indicata in tabella)
203300090210     C*  dal numero segnacollo passatomi dal partner
203400090210     C                   Z-ADD     §PUSG1        WP                2 0
203500090210     C     7             SUBST     x30(X):WP     WSGN              7
203600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
203700090210     C                   TESTN                   WSGN                 98
203800090210     C*  se è numerico lo carico
203900090210     C     *IN98         IFEQ      '1'
204000090210     C                   MOVE      WSGN          SGNNSC
204100090210     C                   ELSE
204200090210     C*  se non è numerica stampo errore
204300090210     C                   MOVEL     'S'           WERRO             1
204400090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
204500090213     c                   exsr      Error_DET
204600090210     C                   goto      end_SGN
204700090210     C                   END
204800090210     C*
204900090210     C     end_SGN       ENDSR
205000051205      *----------------------------------------------------*
205100051205      *   DEFINIZIONE CHIAVI                               *
205200051205      *----------------------------------------------------*
205300051205     C     *INZSR        BEGSR
205400051205      *
205500991129     c     *ENTRY        PLIST
205600060613     C                   parm                    esito             1
205700971215      *
205800050112     C     KTABel        KLIST
205900050112     C                   KFLD                    TBLKUT
206000050112     C                   KFLD                    TBLCOD
206100050112     C                   KFLD                    TBLKEY
206200090210      *
206300090210     C     K_TAB1L       KLIST
206400090213     C                   KFLD                    etbUNB
206500090213     C                   KFLD                    etbSGM
206600090213     C                   KFLD                    etbVALSGM
206700090209     C*
206800090209     C* Definisco chiavi di accesso
206900090209     C     KTAB1         KLIST
207000090209     C                   KFLD                    KKUT
207100090209     C                   KFLD                    KCOD
207200090209     C*
207300090209     C     KNUF          KLIST
207400090209     C                   KFLD                    KAAA
207500090209     C                   KFLD                    KCNU
207600090209     C                   KFLD                    KFIL
207700090209      *
207800090209     C     KVAB1         KLIST
207900090209     C                   KFLD                    KCCM
208000090209     C                   KFLD                    KCMR
208100090209     C                   KFLD                    KCNT
208200090209     C                   KFLD                    KAAS
208300090209     C                   KFLD                    KLNP
208400090209     C                   KFLD                    KNRS
208500090209     C                   KFLD                    KNSP
208600090209      *
208700090209     C     KVAB2         KLIST
208800090209     C                   KFLD                    KCCM
208900090209     C                   KFLD                    KCMR
209000090209      *
209100090209     C     KRDE          KLIST
209200090209     C                   KFLD                    KAAS
209300090209     C                   KFLD                    KLNP1
209400090209     C                   KFLD                    KNRS
209500090209     C                   KFLD                    KNSP
209600090209     C                   KFLD                    KNMC
209700090209     C                   KFLD                    KNRP
209800090209      *
209900090209     C     KACO          KLIST
210000090209     C                   KFLD                    KKUT
210100090209     C                   KFLD                    KKCC
210200090209     C                   KFLD                    KKSC
210300090209      *
210400090209     C     KIND          KLIST
210500090209     C                   KFLD                    KKUT
210600090209     C                   KFLD                    KKCC
210700090209     C                   KFLD                    KKSC
210800090209      *
210900090209      * DETERMINO SE SONO BARTOLINI O SDI
211000090209     C                   CLEAR                   TIBS55DS
211100090209     C                   MOVEL     'L'           I50TLA
211200090209     C                   CALL      'TIBS55R'
211300090209     C                   PARM                    TIBS55DS
211400090209      *
211500090209     C* RECUPERO DATI DELL'UTENTE
211600090209     C                   Z-ADD     1             CODUT             1 0
211700090209     C                   CALL      'XPARUT'
211800090209     C                   PARM                    UTEDSE0F
211900090209     C                   MOVEL     RAGUT         RSUT             20
212000090209     C*
212100090209     C* RICERCA CAPOCONTI
212200090209     C                   DO        50            X
212300090209     C                   MOVE      TCU(X)        TCUDS
212400090209     C     F56           IFEQ      'CG'
212500090209     C     F34           ANDEQ     '01'
212600090209     C                   Z-ADD     KCU(X)        KCI               4 0
212700090209     C                   END
212800090209     C                   END
212900090209     C*
213000090211     c                   movel(p)  'EDPAB'       User_Msg         10
213100090211     C*
213200090209     C* IMPOSTO A ZERO VARIABILI DI PGM
213300090209     c                   move      *blank        Snd_Msg_alert     1
213400090209     C                   MOVEL     CA(1)         WCA              78
213500090209     C                   MOVEL     CN(1)         WCN              78
213600090209     C*
213700090209     C* RECUPERO DATA E ORA
213800090209     C                   TIME                    WHHDAT           14 0
213900090209     C                   MOVE      WHHDAT        G02DAT
214000090209     C                   MOVE      WHHDAT        WDATA8            8 0
214100090209     C                   MOVE      WDATA8        WAA2              2 0
214200090209     C                   MOVEL     WDATA8        DATA              6 0
214300090209     C                   MOVE      WAA2          DATA
214400090209     C                   MOVEL     *BLANK        G02ERR
214500090209     C                   CALL      'XSRDA8'
214600090209     C                   PARM                    WLBDA8
214700090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
214800100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
214900090209     C                   TIME                    WORA              6 0
215000090209      * Recupero data e ora
215100090209     C                   TIME                    W0140
215200090209      * UDATE IN GGMMAAAA
215300090209     C                   MOVE      W0140         WDTGIO
215400090209      * UDATE IN AAAAMMGG
215500090209     C     *eur          MOVEL     WDTGIO        DATA_eur
215600090209     C     *iso          MOVEL     DATA_eur      dateu
215700090209      *
215800090209      * ?              /*--------------------------- */
215900090209     c                   exsr      CARICA_TABELLE
216000090209      * ?              /*--------------------------- */
216100090209      *
216200090209     C                   MOVEL     *ALL'-'       CMP132          132
216300090209     C                   CLEAR                   EDiVAB00
216400090211     C                   clear                   vablnp_PT
216500090212     C                   clear                   vabnrs_PT
216600090211     C                   clear                   vabctm_PT
216700090211     C                   clear                   VABfnsp_PT
216800090209     C                   MOVEA     *HIVAL        SGN
216900991124      *
217000050414      *------------------
217100991124     C                   ENDSR
217200060621      * ?------------------------------------------------------------------ */
217300090209      *?    CARICA TABELLE SU SCHIERE
217400060621      * ?------------------------------------------------------------------ */
217500090209     C     CARICA_TABELLEBEGSR
217600090205      *
217700090209     C* Caricamento Tabella 1B
217800090209     C                   Z-ADD     0             X                 4 0
217900090209     C                   Z-ADD     CODUT         KKUT
218000090209     C                   MOVEL     '1B'          KCOD
218100090209     C     KTAB1         CHAIN     TABEL00F                           30
218200090209     C     *IN30         DOWEQ     '0'
218300090209     C     TBLFLG        IFEQ      *BLANKS
218400090209     C                   ADD       1             X
218500090209     C                   MOVEL     TBLKEY        CTM(X)
218600090209     C                   MOVEL     TBLUNI        DTM(X)
218700090209     C                   END
218800090209     C     KTAB1         READE     TABEL00F                               30
218900090209     C                   END
219000090209      *
219100090209     C* Caricamento Tabella 15
219200090209     C                   Z-ADD     0             X                 4 0
219300090209     C                   Z-ADD     CODUT         KKUT
219400090209     C                   MOVEL     '15'          KCOD
219500090209     C     KTAB1         CHAIN     TABEL00F                           30
219600090209     C     *IN30         DOWEQ     '0'
219700090209     C     TBLFLG        IFEQ      *BLANKS
219800090209     C                   ADD       1             X
219900090209     C                   MOVEL     TBLKEY        C15(X)
220000090209     C                   MOVEL     TBLUNI        DS15
220100090209     C                   MOVEL     §15COD        ISO(X)
220200090209     C                   MOVEL     §15ECF        CPF(X)
220300090209     C                   MOVE      §15PCF        CPF(X)
220400090209     C                   END
220500090209     C     KTAB1         READE     TABEL00F                               30
220600090209     C                   END
220700090209      *
220800090209     C* Caricamento Tabella CV
220900090209     C                   Z-ADD     0             X
221000090209     C                   Z-ADD     CODUT         KKUT
221100090209     C                   MOVEL     'CV'          KCOD
221200090209     C     KTAB1         CHAIN     TABEL00F                           30
221300090209     C     *IN30         DOWEQ     '0'
221400090209     C     TBLFLG        IFEQ      *BLANKS
221500090209     C                   ADD       1             X
221600090209     C                   MOVEL     TBLKEY        CCV(X)
221700090209     C                   MOVEL     TBLUNI        DCV(X)
221800090209     C                   END
221900090209     C     KTAB1         READE     TABEL00F                               30
222000090209     C                   END
222100090216      *
222200110328      * Caricamento Tabella termini consegna
222300110328     C                   Z-ADD     0             X
222400110328     C                   MOVEL     'TB'          TABCOD
222500110328     C     TABCOD        CHAIN     EDTAB01L                           30
222600110328     C     *IN30         DOWEQ     '0'
222700110328     C     TABFLG        IFEQ      *BLANKS
222800110328     C                   ADD       1             X
222900110328     C                   eval      K35_TpBolla(X) = TABKEY
223000110328     C                   eval      Cod_TpBolla(X) = TABKEY
223100110328     C                   eval      Decod_Porto(X) = TABUNI
223200110328     C                   END
223300110328     C     TABCOD        READE     EDTAB01L                               30
223400110328     C                   END
223500110328      *
223600090216     C* Caricamento Tabella Tipo Incasso C/Assegno
223700090216     C                   Z-ADD     0             X
223800090216     C                   MOVEL     'TC'          TABCOD
223900090216     C     TABCOD        CHAIN     EDTAB01L                           30
224000090216     C     *IN30         DOWEQ     '0'
224100090216     C     TABFLG        IFEQ      *BLANKS
224200090216     C                   ADD       1             X
224300090216     C                   MOVEL     TABKEY        CTC35(X)
224400090216     C                   MOVEL     TABKEY        CTC(X)
224500090216     C                   MOVEL     TABUNI        TPI(X)
224600090216     C                   END
224700090216     C     TABCOD        READE     EDTAB01L                               30
224800090216     C                   END
224900090209      *
225000090209      * Caricamento Tabella Partner esteri
225100090209     C                   Z-ADD     0             X
225200090209     C                   MOVEL     'PT'          TABCOD
225300090209     C     TABCOD        CHAIN     EDTAB01L                           30
225400090209     C     *IN30         DOWEQ     '0'
225500090209      *
225600090209     C                   SELECT
225700090209     C     TABFLG        WHENNE    *BLANKS
225800090209      *
225900090209     C                   OTHER
226000090209     C                   ADD       1             X
226100090209     C                   MOVEL     TABKEY        CPT(X)
226200090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
226300090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
226400090209     C                   MOVEL     TABUNI        DPT(X)
226500090209     C                   ENDSL
226600090209      *
226700090209     C     TABCOD        READE     EDTAB01L                               30
226800090209     C                   END
226900121105      *
227000121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
227100121105     c                   exsr      ChecK_PT
227200121105      *
227300090209      * salva quanti Codici UNB ha caricato
227400090209     c                   z-add     x             sav_CPTx
227500090209      *
227600090209     C                   MOVEL     'PU'          TABCOD
227700090209     C     TABCOD        CHAIN     EDTAB01L                           30
227800090209     C     *IN30         DOWEQ     '0'
227900090209      *
228000090209     C                   SELECT
228100090209     C     TABFLG        WHENNE    *BLANKS
228200090209      *
228300090209     C                   OTHER
228400090209     C                   MOVEL     TABKEY        CODPU            35
228500090209     C                   Z-ADD     1             X
228600090209     C     CODPU         LOOKUP    CPT(X)                                 32
228700090209     C   32              MOVEL     TABUNI        DPU(X)
228800090209     C                   ENDSL
228900090209      *
229000090209     C     TABCOD        READE     EDTAB01L                               30
229100090209     C                   END
229200090209      *
229300090209      * Prosegue tab.PT e PU
229400090209     C                   MOVEL     'PV'          TABCOD
229500090209     C     TABCOD        CHAIN     EDTAB01L                           30
229600090209     C     *IN30         DOWEQ     '0'
229700090209      *
229800090209     C                   SELECT
229900090209     C     TABFLG        WHENNE    *BLANKS
230000090209      *
230100090209     C                   OTHER
230200090209     C                   MOVEL     TABKEY        CODPV            35
230300090209     C                   Z-ADD     1             X
230400090209     C     CODPV         LOOKUP    CPT(X)                                 32
230500090209     C   32              MOVEL     TABUNI        DPV(X)
230600090209     C                   ENDSL
230700090209      *
230800090209     C     TABCOD        READE     EDTAB01L                               30
230900090209     C                   END
231000090209      *
231100090209     C* Caricamento Tabella codici clienti - tariffe
231200090209     C                   Z-ADD     0             X
231300090209     C                   MOVEL     'CL'          TABCOD
231400090209     C     TABCOD        CHAIN     EDTAB01L                           30
231500090209     C     *IN30         DOWEQ     '0'
231600090209     C     TABFLG        IFEQ      *BLANKS
231700090209     C                   ADD       1             X
231800090209     C                   MOVEL     TABKEY        CCL(X)
231900090209     C                   MOVEL     TABUNI        DCL(X)
232000090209     C                   END
232100090209     C     TABCOD        READE     EDTAB01L                               30
232200090209     C                   END
232300090209      *
232400090209     C* Caricamento Serivizi speciali
232500090209     C                   Z-ADD     0             X
232600090209     C                   MOVEL     'SS'          TABCOD
232700090209     C     TABCOD        CHAIN     EDTAB01L                           30
232800090209     C     *IN30         DOWEQ     '0'
232900090209     C     TABFLG        IFEQ      *BLANKS
233000090209     C                   ADD       1             X
233100090209     C                   MOVEL     TABKEY        SS(X)
233200090209     C                   MOVEL     TABKEY        SS35(X)
233300090209     C                   MOVEL     TABUNI        DSS(X)
233400090209     C                   END
233500090209     C     TABCOD        READE     EDTAB01L                               30
233600090209     C                   END
233700090209      *
233800090209     C                   ENDSR
233900090209      * ?------------------------------------------------------------------ */
234000090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
234100090209      * ?------------------------------------------------------------------ */
234200090209     C     MSG_Errato    BEGSR
234300090209      *
234400090205      * ?  Scrive su tutti i records il tipo di errore
234500090205     c     *start        setll     tivin00r
234600090205     c                   read      tivin00r
234700091016     c                   if        %Eof(tivin00r)
234800091016     c                   move      'S'           EoFTivin
234900091016     c                   end
235000090205     c                   dow       not %eof(tivin00r)
235100090211     c                   if        vinMSG  = *blank
235200090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
235300090210     C                               >> Controllare i DATI su UPLOAD !!'
235400090211     c                   end
235500090205     C                   eval      vinFLG = '2'
235600090213     c                   eval      Almeno_Un_Due ='S'
235700090205     c                   eval      esito  = '2'
235800090209     c                   eval      se_errore ='S'
235900090213     c                   eval      err_bloccante ='S'
236000091016     c                   if        EoFTivin = ' '
236100090205     c                   update    tivin000
236200091016     c                   end
236300090205     c                   read      tivin00r
236400091016     c                   if        %Eof(tivin00r)
236500091016     c                   move      'S'           EoFTivin
236600091016     c                   end
236700090205     c                   endDO
236800090205      *
236900090205     C                   ENDSR
237000090213      * ?------------------------------------------------------------------ */
237100090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
237200090213      * ?------------------------------------------------------------------ */
237300090213     C     DETta_Errato  BEGSR
237400090213      *
237500090213      *  rilascia il record prima di rieseguire il ciclo
237600090213     c                   update    tivin000
237700090213      *
237800090213      * ?  Scrive su tutti i records il tipo di errore
237900090213     c     SavNUM_RECord setll     tivin00r
238000090213     c                   read      tivin00r
238100091016     c                   if        %Eof(tivin00r)
238200091016     c                   move      'S'           EoFTivin
238300091016     c                   end
238400090213     c                   dow       not %eof(tivin00r)
238500090213      *
238600090213      *
238700090213     c                   if        vinMSG  = *blank
238800090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
238900090213     C                             >> Controllare i DATI di ogni Segmento <<'
239000090213     c                   end
239100090213     c                   exsr      Error_DET
239200090213      *
239300090213     c                   if        VNREC = NUM_RECord
239400090213     c                   leave
239500090213     c                   end
239600090213      *
239700091016     c                   if        EoFTivin = ' '
239800090213     c                   update    tivin000
239900091016     c                   end
240000090213     c                   read      tivin00r
240100091016     c                   if        %Eof(tivin00r)
240200091016     c                   move      'S'           EoFTivin
240300091016     c                   end
240400090213      *
240500090213     c                   endDO
240600090603      *
240700091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
240800090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
240900091203???? C                             Bolle YAMAHA non totalmente tradotto. -
241000100310     C                             Controllare UPLOAD del cliente:'
241100100310     C                   EVAL      Messaggio = %trim(Messaggio) +
241200100310     C                                         %editc(§PTksc:'X')
241300090603     c                   exsr      invio_mail_AB
241400090603      *
241500090213      *
241600090213     C                   ENDSR
241700090205      * ?------------------------------------------------------------------ */
241800090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
241900090205      * ?------------------------------------------------------------------ */
242000090205     C     *pssr         BEGSR
242100090205     C
242200060710     C                   eval      esito = '2'
242300091203???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import YAMAHA'
242400090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
242500090603     C                             Bolle import in filiale - messaggio -
242600090603     C                             EDI ricevuto errato su UPLOAD.'
242700100324     C                   EVAL      Messaggio = %trim(Messaggio) +
242800100324     C                                         %editc(§PTksc:'X')
242900090603     c                   exsr      invio_mail_AB
243000060621     C
243100090213     C                   ENDSR
243200090213      * ?------------------------------------------------------------------ */
243300090213      *?      Segnala Errore su TIVIN
243400090213      * ?------------------------------------------------------------------ */
243500090213     C     Error_DET     BEGSR
243600090213     C
243700090213     C                   eval      vinFLG = '2'
243800090213     c                   eval      Almeno_Un_Due ='S'
243900090213     c                   eval      esito  = '1'
244000090213     c                   eval      se_errore ='S'
244100090213     C                   eval      Err_in_detta = 'S'
244200090213     C
244300090213     C                   ENDSR
244400090209     c*==================================================================*
244500090209     C*? CNVDAT - DECODIFICA LA DATA
244600090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
244700090209     C*         - WDATA  (DATA ALFANUMERICA)
244800090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
244900090209     C*         - WHMS   (ORA NUMERICA)
245000090209     C*----------------------------------------------------------------
245100090209     C     CNVDAT        BEGSR
245200090209     C*
245300090209     C                   MOVEL     ' '           WERRO             1
245400090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
245500090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
245600100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
245700090209     C                   Z-ADD     *ZEROS        WHMS              6 0
245800090209     C*
245900090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
246000090209     C                   MOVEL     WDATA         WCOMO8            8
246100090209     C                   TESTN                   WCOMO8               99
246200090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
246300090209     C  N99              MOVEL     'S'           WERRO             1
246400090209     C                   END
246500090209     C*
246600090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
246700100120     C                   MOVEL     WDATA         WCOMO12          12
246800100120     C                   TESTN                   WCOMO12              99
246900100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
247000100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
247100100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
247200090209     C  N99              MOVEL     'S'           WERRO             1
247300090209     C                   END
247400100120     C*
247500100120     C     WFORM         IFEQ      '204'                                        CCYMMDD
247600100120     C                   MOVEL     WDATA         WCOMO14          14
247700100120     C                   TESTN                   WCOMO14              99
247800100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
247900100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
248000100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
248100100120     C  N99              MOVEL     'S'           WERRO             1
248200100120     C                   END
248300090209     C*
248400090209     C                   ENDSR
248500090210     C*----------------------------------------------------------------
248600090210     C*? TSTNUM - CONTROLLO NUMERICO
248700090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
248800090210     C*         - WLUNGH (Lunghezza campo output)
248900090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
249000090210     C*----------------------------------------------------------------
249100090210     C     TSTNUM        BEGSR
249200090210     C*
249300090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
249400090210     C                   MOVEL     'N'           WOKNUM            1
249500090210     C                   MOVEL     *BLANKS       WNUMOK           15
249600090210     C                   Z-ADD     15            IN                3 0
249700090210     C                   Z-ADD     WLUNGH        IX                3 0
249800090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
249900090210     C                   MOVEA     WNUM          NDA
250000090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
250100090210     C                   MOVEA     *ALL'0'       NDN
250200090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
250300090210     C                   Z-ADD     WLUNGH        I                 3 0
250400090210     C     I             DOWGT     0                                            --- 1 -->
250500090210     C* ESTRAE IL CARATTERE DA ESAMINARE
250600090210     C     1             SUBST     WNUM:I        WTEM01            1
250700090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
250800090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
250900090210     C     WTEM01        SCAN      WCA                                    99
251000090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
251100090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
251200090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
251300090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
251400090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
251500090210     C* DA DESTRA VERSO SINISTRA
251600090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
251700090210     C                   MOVEL     WTEMP1        NDN(IN)
251800090210     C                   SUB       1             IN
251900090210     C                   END                                                    <-- 3 ---
252000090210     C                   END                                                    <-- 2 ---
252100090210     C                   SUB       1             I
252200090210     C                   END                                                    <-- 1 ---
252300090210     C*
252400090210     C                   MOVEA     NDN           WNDN             15
252500090210     C     WNDN          IFNE      *ZEROS
252600090210     C                   MOVEA     NDN           WNUMOK           15
252700090210     C                   MOVEL     'S'           WOKNUM            1
252800090210     C                   END
252900090210     C*
253000090210     C                   ENDSR
253100090210     C*----------------------------------------------------------------
253200090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
253300090210     C*----------------------------------------------------------------
253400090210     C     CNVCAP        BEGSR
253500090210     C*
253600090210     C                   MOVEL     *BLANKS       WCAP              9
253700090210     C     nad3251       IFNE      '0'
253800090210     C     '  '          SCAN      nad3251       LENGHT            2 0
253900090210     C                   SUB       1             LENGHT
254000090210     C     LENGHT        IFLE      5
254100090210     C     LENGHT        ANDGT     0
254200090210     C                   Z-ADD     LENGHT        T                 2 0
254300090210     C                   Z-ADD     5             S                 2 0
254400090210     C                   MOVEA     nad3251       CAP9
254500090210     C                   MOVEL     *ZEROS        CAP5
254600090210     C                   DO        LENGHT
254700090210     C                   MOVE      CAP9(T)       CAP5(S)
254800090210     C                   SUB       1             S
254900090210     C                   SUB       1             T
255000090210     C                   END
255100090210     C                   MOVEA     CAP5          WCAP5             5
255200090210     C     WCAP5         IFEQ      '0'
255300090210     C                   MOVEL     *BLANKS       WCAP
255400090210     C                   ELSE
255500090210     C                   MOVEA     CAP5          WCAP
255600090210     C                   END
255700090210     C*
255800090210     C                   ELSE
255900090210     C                   MOVEL     nad3251       WCAP
256000090210     C                   END
256100090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
256200090210     C     WCAP          IFNE      *BLANKS
256300090210     C     WFLCPF        ANDNE     0
256400090210     C                   MOVEL     WFLCPF        WELE              1 0
256500090210     C                   MOVE      WFLCPF        WPOS              1 0
256600090210     C                   MOVEL     *BLANK        WCPF
256700090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
256800090210     C                   ELSE
256900090210     C                   MOVEL     *BLANKS       WCPF
257000090210     C                   END
257100090210     C                   END
257200090210     C*
257300090210     C                   ENDSR
257400090211      * ?================================================================== */
257500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
257600090211      * ?================================================================== */
257700090211     C     Scrive_VAB    BEGSR
257800090211      *
257900090211     c                   clear                   err_bloccante     1
258000090211     c                   move      'S'           Almeno_Uno
258100090211      *
258200090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
258300090211     c                   exsr      UPDVAB
258400090211      *
258500090211     C* >>>>>>>>>>>>
258600090211     C*  Richiamo pgm per scrittura immediata VAB
258700090211     C     §PUWRK        IFEQ      ' '
258800090211      *
258900090211     C*  Richiamo pgm per esecuzione stampa
259000090211     C     §PTCMR        IFEQ      'S'
259100090213     C                   MOVEL     WNRDOC        d86CMR
259200090211     C                   ELSE
259300090213     C                   MOVEL     WNRDOC        d86CMR
259400090211     C                   END
259500090211      *
259600090213     C                   MOVEL     ' '           d86RIE
259700090213     C                   MOVEL     §PUDTA        d86DTA
259800090213     C                   Z-ADD     1             d86CNT
259900090213     C                   Z-ADD     §PTKSC        d86CCM
260000090213     C                   MOVEL     *BLANKS       d86STA
260100090213     C                   MOVEL     'E'           d86MOD
260200090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
260300090211      * dove poi imposta la DS dei parametri passati
260400090213     C                   MOVEL     'LR'          d86CHI
260500090213     C                   MOVEL     'N'           d86CTL
260600090211      *
260700090211     c                   move      kpjbu         SvKpjbu
260800090211     c                   clear                   kpjbu
260900090216     C                   movel     *on           Commit_on         1
261000090213     C                   MOVEL     TRTC86ds      KPJBU
261100090213      * ?- Chiamata la TRTC86R2 -*
261200090213     C                   CALL      'TRTC86R2'
261300090211     C                   PARM                    KPJBA
261400090216     C                   PARM                    Commit_on
261500090211     c                   move      Svkpjbu       Kpjbu
261600090211      *
261700090211     C*  Controllo pgm per scrittura immediata VAB
261800090211     C     WW            IFNE      0
261900090211     C                   DO        WW            WX                3 0
262000090213     C                   Z-ADD     KCL(WX)       d86CCM
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
263100090211     C                   END
263200090211     C                   END
263300090211      *
263400090211     C                   END
263500090211      * >>>>>>>>>>>>
263600090211     C                   if        se_errore  = 'S'
263700090211     c                   move      'S'           err_bloccante
263800090211     c                   goto      Error_VAB
263900090211     c                   end
264000090211     C*
264100090211     C     Error_VAB     Endsr
264200090211     C*----------------------------------------------------------------
264300090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
264400090211     C*----------------------------------------------------------------
264500090211     C     UPDVAB        BEGSR
264600090211      *
264700090211     C*  Se non è stato impostato il codice bolla lo imposto io
264800090211     C     VABCBO        IFEQ      *BLANKS
264900090211     C     VABCAS        IFGT      0
265000090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
265100090211     C                   ELSE
265200090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
265300090211     C                   END
265400090211     C                   END
265500110922      *
265600110922      * Imposta le NOte
265700110922     C     VABNOT        IFEQ      *BLANKS
265800110922     C                   MOVEL     wnot1         VABNOT
265900110922     C                   MOVEL     wnot2         VABNT2
266000110922     C                   ELSE
266100110922     C                   MOVEL     wnot1         VABNT2
266200110922     C                   END
266300090211      *
266400090211     C*  Attribuisco anno spedizione
266500090211     C     WDATPA        IFGT      0                                            anno sped.
266600090211     C                   MOVEL     WDATPA        VABAAS
266700090211     C                   ELSE
266800090211     C     WDATFV        IFGT      0
266900090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
267000090211     C                   END
267100090211     C                   END
267200090211      *
267300090211      *  Controllo dettaglio segnacolli
267400090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
267500090211     C                   EXSR      CTRSGE
267600090211     C                   ELSE
267700090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
267800090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
267900090211     C                   EXSR      CTRSGN
268000090211     C                   END
268100090211     C                   END
268200090211      *
268300090212      *  Imposto linea partenza e serie
268400090211     C                   eval      VABLNP = VABLNP_PT
268500090212     C                   eval      VABNRS = VABnrs_PT
268600090211      *
268700090211      *  Controllo se devo variare il codice trattamento merce
268800090211     C                   eval      VABCTM = VABctm_PT
268900090211      *
269000090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
269100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
269200090211     c                   clear                   mantiene_nsp      1            *blk/'S'
269300090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
269400090211      *
269500090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
269600090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
269700090211      *
269800090211      *  Imposto codice cliente
269900090211     c                   Select
270000090211      *
270100090211      *  Specificato codice da qualificatore FF (TD15)
270200090211     C                   When        WCLKSC <> *zeros
270300090211     C                   Z-ADD     WCLKSC        VABCCM
270400090211     C                   MOVEL     WCLTAR        VABCTR
270500090211      * forza LNP/CTM/NRS
270600090211     c                   if        WclLNP <> 0
270700090211     C                   eval      VABLNP = WclLNP
270800090211     c                   end
270900090211     c                   if        WclNRS <> 0
271000090211     C                   eval      VABNRS = WclNRS
271100090211     c                   end
271200090211     c                   if        WclCTM <> *blank
271300090211     C                   eval      VABCTM = WclCTM
271400090211     c                   end
271500090211      *
271600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
271700090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
271800090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
271900090211     c                   end
272000090211      *
272100090211      *  Specificato codice da qualificatore FF (TT15)
272200090211     C                   When        $CLKSC <> *zeros
272300090211     C                   Z-ADD     $CLKSC        VABCCM
272400090211     C                   MOVEL     $CLTAR        VABCTR
272500090211      * forza LNP/CTM/NRS
272600090211     c                   if        $clLNP <> 0
272700090211     C                   eval      VABLNP = $clLNP
272800090211     c                   end
272900090211     c                   if        $clNRS <> 0
273000090211     C                   eval      VABNRS = $clNRS
273100090211     c                   end
273200090211     c                   if        $clCTM <> *blank
273300090211     C                   eval      VABCTM = $clCTM
273400090211     c                   end
273500090211      *
273600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
273700090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
273800090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
273900090211     c                   end
274000090211      *
274100090211      *  Imposto dati da tabella - PT-
274200090211     C                   Other
274300090211      *
274400090211     C                   MOVEL     §PTKSC        VABCCM
274500090211     C                   MOVEL     §PTCTR        VABCTR
274600090211      *
274700090211     C                   Endsl
274800090211      *
274900090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
275000090211      *    o dalla relativa CL
275100090211     C                   MOVEL     VABlnp        VABfgs
275200090211      *
275300090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
275400090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
275500090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
275600090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
275700090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
275800090211      *
275900090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
276000090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
276100090211     c                   else
276200090211      *
276300090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
276400090211     C*  lo attribuisco io
276500090211      **              **------------------**
276600090211     c                   exsr      repnum
276700090211      **              **------------------**
276800090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
276900090211      **              **------------------**
277000090211     c                   end
277100090211      *
277200090211     C*  Altrimenti attribuisco data del giorno
277300090211     C     VABMGS        IFEQ      0
277400090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
277500090211     C                   MOVE      WOGGI         VABMGS                         = oggi
277600090211     C                   END
277700090211     C*  Eseguo posizionamento su EDiVAB
277800090211     C                   Z-ADD     VABAAS        KAAS
277900090211     C                   Z-ADD     VABlnp        KLNP
278000090211     C                   Z-ADD     VABNRS        KNRS
278100090211     C                   Z-ADD     VABNSP        KNSP
278200090211     C                   MOVEL     SAVBOL        SALVA
278300090211     C     KVAB1         CHAIN     EDiVAB1L                           30
278400090211     C                   MOVEL     SALVA         SAVBOL
278500090211     C*  Imposto dati CMR
278600090211     C                   Z-ADD     1             VABCNT
278700090211     **
278800090211      *** Attenzione se si deve trattare il VAX con il CMR
278900090211      *** il campo VABCNT deve essere sempre impostato a (1)
279000090211     C     §puWRK        ifEQ      'S'
279100090211     C     §puNSP        andEQ     'S'
279200090211     C                   Z-ADD     1             VABCNT
279300090211     c                   end
279400090211     **
279500090211     C                   Z-ADD     WDTPRE        VABDTS
279600090212     C                   movel     '20'          VABDTS
279700100113     ** Forza la Data di Oggi
279800100113     C                   z-add     WOggi         VABDTS
279900100113     **
280000090212     C     WHMPRE        mult      100           VABHMS
280100090211     C     §PTCMR        IFEQ      'S'
280200090211     C                   Z-ADD     WDTCMR        VABDCM
280300090211     C                   MOVEL     WNRCMR        VABCMR
280400090211     C                   ELSE
280500090211     C                   Z-ADD     WDATFV        VABDCM
280600090211     C                   MOVEL     WNUMFV        VABCMR
280700090211     C                   END
280800100120      *
280900100120      * Per YAMAHA occorre fare un unico CMR giornaliero per permettere di
281000100120      * accorpare la bolle ad un unico destinatario da più parti
281100100120      *  YAMAHA quindi deve utilizzare la conferma x CMR.
281200100120     c                   eval      VABCMR  = 'YAMAHA_' + %editc(WOggi_CMR:'X')
281300100120      *
281400090211     C*  Se non è indicato il nome del mittente = Partner mittente
281500090211     C     VABRMO        IFEQ      *BLANKS
281600090211     C                   MOVEL     ACORAG        VABRMO
281700090211     C                   MOVEL     INDCAP        VABCMO
281800090211     C     INDCAE        IFNE      *BLANKS
281900090211     C                   MOVEL     INDCAE        VABCMO
282000090211     C                   END
282100090211     C     INDSTA        IFNE      *BLANKS
282200090211     C                   MOVEL     INDSTA        VABNMO
282300090211     C                   END
282400090211     C                   END
282500090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
282600090211     C     §1BFG1        IFEQ      'N'
282700090211     C     §1BFG7        ANDEQ     'N'
282800090211     C     §1BFG2        ANDEQ     'N'
282900090211     C***                  Z-ADD0         VABNRS
283000090211     C                   END
283100090211     C*  Imposto segancollo da - a
283200090211     C     §1BFG1        IFEQ      'S'
283300090211     C     §1BFG7        ANDEQ     'N'
283400090211     C     §1BF12        ANDNE     'E'
283500090211      *
283600090211     C     XY            IFEQ      *ZEROS
283700090211     C                   CLEAR                   VABNCD
283800090211     C                   CLEAR                   VABNCA
283900090211     C                   ELSE
284000090211     C                   Z-ADD     SGN(1)        VABNCD
284100090211     C                   Z-ADD     SGN(XY)       VABNCA
284200090211     C                   ENDIF
284300090211     C*
284400090211     C                   END
284500090211     C*
284600090211     C*  Se cap mittente originale a blank abblenco nazione
284700090211     C     VABCMO        IFEQ      *BLANKS
284800090211     C                   MOVEL     *BLANKS       VABNMO
284900090211     C                   END
285000090603     C*
285100090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
285200090603     C* Se in testata
285300090603     c                   if        NAD_destinatario_in_testata = 'S'
285400090603     c                   eval         VABrsd =  tes_VABrsd
285500090603     c                   eval         VABrd2 =  tes_VABrd2
285600090603     c                   eval         VABind =  tes_VABind
285700090603     c                   eval         VABlod =  tes_VABlod
285800090603     c                   eval         VABnzd =  tes_VABnzd
285900090603     c                   end
286000090603     C* Se in testata
286100090603     c                   if        NAD_mittente_in_testata = 'S'
286200090603     c                   eval         VABrmo = tes_VABrmo
286300090603     c                   eval         VABnmo = tes_VABnmo
286400090603     c                   eval         VABcmo = tes_VABcmo
286500090603     c                   end
286600090603      *
286700090211      *? ------------------------------------------------------------------------
286800090213     C   30              WRITE     EDiVAB00                             99
286900090213     C  N30              UPDATE    EDiVAB00                             99
287000090211      *? ------------------------------------------------------------------------
287100090213     c   99              eval      errori_in_Wrt = 'S'
287200090211     C*
287300090211     c                   clear                   wri_vabtic        1
287400090211     C*
287500090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
287600090211     C                   EXSR      WRTVAT_Rif
287700090211     C*
287800090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
287900090211     C     §1BF12        IFEQ      'E'
288000090211     C                   EXSR      WRTVAT
288100090211     C                   ELSE
288200090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
288300090211     C     §1BFG7        IFEQ      'S'
288400090211     C     §1BFG1        OREQ      'S'
288500090211     C                   EXSR      WRTVAD
288600090211     C                   END
288700090211     C                   END
288800090211      *
288900090211     C* Reimposto codice trattamento merce cliente
289000090211     C                   MOVEL     WSVTRM        DS1B
289100090211     C**
289200090211     C* Do errore se previsto CMR ma non c'è
289300090211     C     §PTCMR        IFNE      ' '
289400090211     C                   EXSR      MEMCMR
289500090211     C                   END
289600090211     C* Do errore se previsto AFB ma non c'è
289700090211     C     §PTNFV        IFNE      ' '
289800090211     C                   EXSR      MEMAFB
289900090211     C                   END
290000090211     C*  Se il cliente vuole il ritorno delle date di consegna
290100090211     C*  è c'è una squdratura fra i segnacolli e il numero
290200090211     C*  dei colli che ci ha passato scrivo EDSUM
290300090211     C     §PUDTA        IFEQ      'S'
290400090211     C     WSQUAD        ANDEQ     'S'
290500090211     C                   EXSR      WRTSUM
290600090211     C                   END
290700090211     C*
290800090211     C                   ENDSR
290900090211     C*----------------------------------------------------------------
291000090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
291100090211     C*----------------------------------------------------------------
291200090211     C     CTRSGN        BEGSR
291300090211     C*
291400090211     C                   MOVEL     'N'           WNSGER            1
291500090211     C*
291600090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
291700090211     C     VABNCL        IFNE      XY
291800090211     C                   MOVEL     'S'           WNSGER
291900090211     C                   MOVEL     'S'           WSQUAD
292000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
292100090211     c                              bollettati'
292200090211     C                   GOTO      FINSGN
292300090211     C                   END
292400090211     C*  Riordino i segnacolli
292500090211     C     §1BFG7        IFEQ      'N'
292600090211     C                   SORTA     SGN
292700090211     C*  Controllo se sono sequenziali
292800090211     C                   MOVEL     'N'           WNOSEQ            1
292900090211     C     SGN(1)        ADD       1             WEXSGN            7 0
293000090211     C     2             DO        XY            X
293100090211     C     SGN(X)        IFNE      WEXSGN
293200090211     C                   MOVEL     'S'           WNOSEQ
293300090211     C                   END
293400090211     C     SGN(X)        ADD       1             WEXSGN
293500090211     C                   END
293600090211     C                   END
293700090211     C*
293800090211     C     FINSGN        ENDSR
293900090211     C*----------------------------------------------------------------
294000090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
294100090211     C*----------------------------------------------------------------
294200090211     C     CTRSGE        BEGSR
294300090211     C*
294400090211     C                   MOVEL     'N'           WNSGER            1
294500090211      *
294600090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
294700090211      *
294800090211     C     VABNCL        IFNE      §§
294900090211     C                   MOVEL     'S'           WNSGER
295000090211     C                   MOVEL     'S'           WSQUAD
295100090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
295200090211     c                              bollettati'
295300090211     C                   END
295400090211      *
295500090211     C                   ENDSR
295600090211     C*----------------------------------------------------------------
295700090211     C*? ESEGUO SCRITTURA EDiVAD
295800090211     C*----------------------------------------------------------------
295900090211     C     WRTVAD        BEGSR
296000090211     C*
296100090211     C* Se non seq. scrivo xy record
296200090211     C     §1BFG1        IFEQ      'S'
296300090211     C     XY            ANDNE     *ZEROS
296400090211     C*
296500090211     C     §1BFG7        IFEQ      'S'
296600090211     C                   DO        XY            X
296700090211     C                   Z-ADD     VABCCM        KCCM
296800090211     C                   Z-ADD     SGN(X)        KNCD
296900090211     C                   Z-ADD     VABCNT        VADCNT
297000090211     C                   MOVEL     VABCMR        VADCMR
297100090211     C                   Z-ADD     VABAAS        VADAAS
297200090211     C                   Z-ADD     VABLNP        VADLNP
297300090211     C                   Z-ADD     VABNRS        VADNRS
297400090211     C                   Z-ADD     VABNSP        VADNSP
297500090211     C                   Z-ADD     1             VADNCL
297600090211     C                   MOVEL     SGN(X)        VADCDP
297700090211     C                   Z-ADD     SGN(X)        VADNCD
297800090211     C                   Z-ADD     *ZEROS        VADNCA
297900090211     C                   Z-ADD     VABCCM        VADCCM
298000090211     C                   movel     VABfgs        VADfgs
298100090213     C                   WRITE     EDiVAD00                             99
298200090213     c   99              eval      errori_in_Wrt = 'S'
298300090211     C                   END
298400090211     C* Se sequenziali scrivo un solo record
298500090211     C                   ELSE
298600090211     C                   Z-ADD     VABAAS        VADAAS
298700090211     C                   Z-ADD     VABLNP        VADLNP
298800090211     C                   Z-ADD     VABNRS        VADNRS
298900090211     C                   Z-ADD     VABNSP        VADNSP
299000090211     C                   Z-ADD     VABNCL        VADNCL
299100090211     C                   Z-ADD     SGN(1)        VADNCD
299200090211     C                   Z-ADD     SGN(XY)       VADNCA
299300090211     C                   Z-ADD     VABCCM        VADCCM
299400090211     C                   movel     VABfgs        VADfgs
299500090213     C                   WRITE     EDiVAD00                             99
299600090213     c   99              eval      errori_in_Wrt = 'S'
299700090211     C                   END
299800090211     C*
299900090211     C                   ELSE
300000090211     C                   DO        XY            X
300100090211     C                   Z-ADD     VABCCM        KCCM
300200090211     C                   Z-ADD     SGN(X)        KNCD
300300090211     C                   Z-ADD     VABAAS        VADAAS
300400090211     C                   Z-ADD     VABLNP        VADLNP
300500090211     C                   Z-ADD     VABNRS        VADNRS
300600090211     C                   Z-ADD     VABNSP        VADNSP
300700090211     C                   MOVEL     SGN(X)        VADCDP
300800090211     C                   Z-ADD     1             VADNCL
300900090211     C                   Z-ADD     *ZEROS        VADNCD
301000090211     C                   Z-ADD     *ZEROS        VADNCA
301100090211     C                   Z-ADD     VABCCM        VADCCM
301200090211     C                   movel     VABfgs        VADfgs
301300090213     C                   WRITE     EDiVAD00                             99
301400090213     c   99              eval      errori_in_Wrt = 'S'
301500090211     C                   END
301600090211     C*
301700090211     C                   END
301800090211     C*
301900090211     C                   ENDSR
302000090211     C*----------------------------------------------------------------
302100090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
302200090211     C*----------------------------------------------------------------
302300090211     C     WRTVAT_Rif    BEGSR
302400090211      *
302500090211      *  riferimento di consegna destinatario
302600090211     c                   if        WatRDE <> *blank
302700090211     C                   Z-ADD     VABCNT        VATCNT
302800090211     C                   MOVEL     VABCMR        VATCMR
302900090211     C                   Z-ADD     VABCCM        VATCCM
303000090211     C                   movel     VABfgs        VATfgs
303100090211     C                   Z-ADD     VABLNP        VATLNP
303200090211     C                   Z-ADD     VABAAS        VATAAS
303300090211     C                   Z-ADD     VABNRS        VATNRS
303400090211     C                   Z-ADD     VABNSP        VATNSP
303500090211     C                   MOVEL     'A'           VATTRC
303600090211     C                   MOVEL     WatRDE        VATNOT
303700090213     C                   WRITE     EDiVAT00                             99
303800090213     c   99              eval      errori_in_Wrt = 'S'
303900090211     c                   end
304000090211      *
304100090211      *  riferimento di consegna telefonico
304200090211     c                   if        WatTEL <> *blank
304300090211     C                   Z-ADD     VABCNT        VATCNT
304400090211     C                   MOVEL     VABCMR        VATCMR
304500090211     C                   Z-ADD     VABCCM        VATCCM
304600090211     C                   movel     VABfgs        VATfgs
304700090211     C                   Z-ADD     VABLNP        VATLNP
304800090211     C                   Z-ADD     VABAAS        VATAAS
304900090211     C                   Z-ADD     VABNRS        VATNRS
305000090211     C                   Z-ADD     VABNSP        VATNSP
305100090211     C                   MOVEL     'B'           VATTRC
305200090211     C                   MOVEL     WatTEL        VATNOT
305300090213     C                   WRITE     EDiVAT00                             99
305400090213     c   99              eval      errori_in_Wrt = 'S'
305500090211     c                   end
305600090211      *
305700090211      * riferimento del Partner che una volta veniva riportato sul BL4
305800100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
305900100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
306000100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
306100100119      *
306200100119     c                   goto      no_riferimento
306300100119      *
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
307800090211      *
307900100119     c     no_riferimentotag
308000100119      *
308100090211     C                   ENDSR
308200090211     C*----------------------------------------------------------------
308300090211     C*? ESEGUO SCRITTURA EDiVAT
308400090211     C*----------------------------------------------------------------
308500090211     C     WRTVAT        BEGSR
308600090211      *
308700090211     C                   DO        §§            X
308800090211     C                   Z-ADD     VABCNT        VATCNT
308900090211     C                   MOVEL     VABCMR        VATCMR
309000090211     C                   Z-ADD     VABCCM        VATCCM
309100090211     C                   movel     VABfgs        VATfgs
309200090211     C                   Z-ADD     VABLNP        VATLNP
309300090211     C                   Z-ADD     VABAAS        VATAAS
309400090211     C                   Z-ADD     VABNRS        VATNRS
309500090211     C                   Z-ADD     VABNSP        VATNSP
309600090211     C                   MOVEL     'E'           VATTRC
309700090211      *
309800090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
309900090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
310000090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
310100090211     c                   if        WVATlun > 0
310200090211      *
310300090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
310400090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
310500090211      *
310600090211     c                   else
310700090211      *
310800090211      * Se non è impostato nulla prende il segnacollo passato così com'è
310900090211     C                   MOVEL     SGE(X)        VATNOT
311000090211     c                   end
311100090211      *
311200090213     C                   WRITE     EDiVAT00                             99
311300090213     c   99              eval      errori_in_Wrt = 'S'
311400090211     C                   END
311500090211      *
311600090211     C                   ENDSR
311700090211     C*----------------------------------------------------------------
311800090211     C*? ESEGUO SCRITTURA EDSUM
311900090211     C*----------------------------------------------------------------
312000090211     C     WRTSUM        BEGSR
312100090211     C*
312200090211     C* Se non seq. scrivo xy record
312300090211     C                   DO        XY            X
312400090211     C                   Z-ADD     VABAAS        SUMAAS
312500090211     C                   Z-ADD     VABLNP        SUMFLS
312600090211     C                   MOVEL     WKSC          SUMLNP
312700090211     C                   Z-ADD     VABLNA        SUMLNA
312800090211     C                   Z-ADD     VABZNC        SUMZNC
312900090211     C                   Z-ADD     VABNRS        SUMNRS
313000090211     C                   Z-ADD     VABNSP        SUMNSP
313100090211     C                   Z-ADD     SGN(X)        SUMNSC
313200090211     C                   Z-ADD     WKSC          SUMCCM
313300090211     C                   MOVEL     *BLANKS       SUMSTS
313400090211     C                   MOVEL     *BLANKS       SUMFLG
313500090211     C                   MOVEL     WPRGSP        SUMCNI
313600090211     C                   MOVEL     VABCMR        SUMCMR
313700090211     C                   Z-ADD     0             SUMNFE
313800090211     C                   Z-ADD     VABCCM        VADCCM
313900090213     C                   WRITE     EDSUM000                             99
314000090213     c   99              eval      errori_in_Wrt = 'S'
314100090211     C                   END
314200090211     C*
314300090211     C                   ENDSR
314400090211     c*==================================================================*
314500090211      * Manda un Msg in Posta AS Sede a EDP*
314600090211     c*==================================================================*
314700090211     c     SEND_MSG_EDP  begsr
314800090211      *
314900090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
315000090211      *   Lo manda una sola volta per lavoro
315100090211     c                   IF        Snd_Msg_alert  <> 'N' and
315200090211     c                             User_msg <> *blank
315300090211     c                   z-add     1             Jx
315400090211     C                   exsr      CalQEZSNDMG
315500090211     c                   end
315600090211      *
315700090211     c                   endsr
315800090213     c**********************************************************************
315900090213     c* reperimento numero Spedizione
316000090213     c**********************************************************************
316100090213     C     repnum        BEGSR
316200090213      *
316300090213      *    deve controllare sulla tabella "3C" se il numero spedizione
316400090213      *     deve essere mantenuto oppure no
316500090213     C*
316600090213     C                   clear                   Ds3C
316700090213     C                   Z-ADD     CODUT         TBLKUT
316800090213     C                   MOVEL     '3C'          TBLCOD
316900090213     C                   movel(p)  VABccm        TBLKEY
317000090213     C     KTABel        CHAIN     TABEL00F                           30
317100090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
317200090213     C                   MOVEL     TBLUNI        Ds3C
317300090213     C                   END
317400090213      *
317500090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
317600090213     c                   if        §3CFsp <> 'S'
317700090213      *
317800090213     C* NSP => Stacco un numeratore da AZNUM
317900090213     c                   movel     kpjbu         svkpjbu
318000090213     c                   clear                   kpjbu
318100090213     C                   clear                   TRUL33DS
318200090213     C                   eval      I33OPE = *zeros
318300090213     C                   eval      I33CNU = 302
318400090213     C                   eval      I33NUM = 1
318500090213     C                   movel     TRUL33DS      KPJBU
318600090213     C                   call      'TRUL33R'
318700090213     C                   parm                    KPJBA
318800090213     C                   movel     KPJBU         TRUL33DS
318900090213     c                   movel     svkpjbu       kpjbu
319000090213      ****
319100090213     C                   if        O33ERR = *zeros
319200090213     c                   z-add     o33nrf        NUM_SPED
319300090213     C                   else
319400090213     C                   endif
319500090213      ****
319600090213     c                   ELSE
319700090213      *   se si vuole mantenere il numero spedizione
319800090213      ****
319900090213     C                   MOVEL     WOGGI         WANNO             4 0
320000090213     C                   MOVE      WANNO         KAAA
320100090213     C                   Z-ADD     3             KCNU
320200090213     C                   Z-ADD     VABlnp        KFIL
320300090213     C     KNUF          CHAIN     FLNUF                              9999
320400090213     C     *IN99         IFEQ      '0'
320500090213     C                   ADD       1             NUFNUM
320600090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
320700090213     C                   UPDATE    FLNUF
320800090213     C                   ELSE
320900090213     C                   END
321000090213     C*
321100090213     c                   EndIF
321200090213      **
321300090213     C                   ENDSR
321400090213     C*----------------------------------------------------------------
321500090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
321600090213     C*----------------------------------------------------------------
321700090213     C     MEMCMR        BEGSR
321800090213     C*
321900090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
322000090213     C     §PTCM1        IFEQ      'S'
322100090213     C                   CLEAR                   EDRDE000
322200090213     C                   MOVEL     'TD1154'      KNMC
322300090213     C                   Z-ADD     1             KNRP
322400090213     C                   MOVEL     WKSC          KLNP1
322500090213     C     KRDE          CHAIN     EDRDE01L                           31
322600090213     C     *IN31         IFEQ      '1'
322700090213     C                   Z-ADD     VABAAS        RDEAAS
322800090213     C                   Z-ADD     VABLNP        RDELNP
322900090213     C                   Z-ADD     VABNRS        RDENRS
323000090213     C                   Z-ADD     VABNSP        RDENSP
323100090213     C                   MOVEL     'TD1154'      RDENMC
323200090213     C                   Z-ADD     1             RDENRP
323300090213     C                   MOVEL     WNRCMR        RDEVAL
323400090213     C                   WRITE     EDRDE000                             99
323500090213     c   99              eval      errori_in_Wrt = 'S'
323600090213     C                   END
323700090213     C*  Memorizzo qualificatore CMR
323800090213     C                   CLEAR                   EDRDE000
323900090213     C                   MOVEL     'TD1153'      KNMC
324000090213     C                   Z-ADD     1             KNRP
324100090213     C                   MOVEL     WKSC          KLNP1
324200090213     C     KRDE          CHAIN     EDRDE01L                           31
324300090213     C     *IN31         IFEQ      '1'
324400090213     C                   Z-ADD     VABAAS        RDEAAS
324500090213     C                   Z-ADD     VABLNP        RDELNP
324600090213     C                   Z-ADD     VABNRS        RDENRS
324700090213     C                   Z-ADD     VABNSP        RDENSP
324800090213     C                   Z-ADD     1             RDENRP
324900090213     C                   MOVEL     'TD1153'      RDENMC
325000090213     C                   MOVEL     'CMR'         RDEVAL
325100090213     C                   WRITE     EDRDE000                             99
325200090213     c   99              eval      errori_in_Wrt = 'S'
325300090213     C                   END
325400090213     C*  Memorizzo la data
325500090213     C                   CLEAR                   EDRDE000
325600090213     C                   MOVEL     'TD2380'      KNMC
325700090213     C                   Z-ADD     1             KNRP
325800090213     C                   MOVEL     WKSC          KLNP1
325900090213     C     KRDE          CHAIN     EDRDE01L                           31
326000090213     C     *IN31         IFEQ      '1'
326100090213     C                   Z-ADD     VABAAS        RDEAAS
326200090213     C                   Z-ADD     VABLNP        RDELNP
326300090213     C                   Z-ADD     VABNRS        RDENRS
326400090213     C                   Z-ADD     VABNSP        RDENSP
326500090213     C                   Z-ADD     1             RDENRP
326600090213     C                   MOVEL     'TD2380'      RDENMC
326700090213     C                   MOVEL     WDTCMR        RDEVAL
326800090213     C                   WRITE     EDRDE000                             99
326900090213     c   99              eval      errori_in_Wrt = 'S'
327000090213     C                   END
327100090213     C*
327200090213     C                   END                                                    §PTCM1 = 'S'
327300090213     C*
327400090213     C                   ENDSR
327500090213     C*----------------------------------------------------------------
327600090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
327700090213     C*----------------------------------------------------------------
327800090213     C     MEMAFB        BEGSR
327900090213     C*
328000090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
328100090213     C     §PTNF1        IFEQ      'S'
328200090213     C                   CLEAR                   EDRDE000
328300090213     C                   MOVEL     WKSC          KLNP1
328400090213     C                   MOVEL     'TD1154'      KNMC
328500090213     C                   Z-ADD     1             KNRP
328600090213     C     KRDE          CHAIN     EDRDE01L                           31
328700090213     C     *IN31         IFEQ      '1'
328800090213     C                   Z-ADD     VABAAS        RDEAAS
328900090213     C                   Z-ADD     VABLNP        RDELNP
329000090213     C                   Z-ADD     VABNRS        RDENRS
329100090213     C                   Z-ADD     VABNSP        RDENSP
329200090213     C                   MOVEL     'TD1154'      RDENMC
329300090213     C     §PTCM1        IFNE      'S'
329400090213     C     WNRCMR        OREQ      *BLANKS
329500090213     C                   Z-ADD     1             RDENRP
329600090213     C                   ELSE
329700090213     C                   Z-ADD     2             RDENRP
329800090213     C                   END
329900090213     C                   MOVEL     WNUMFV        RDEVAL
330000090213     C                   WRITE     EDRDE000                             99
330100090213     c   99              eval      errori_in_Wrt = 'S'
330200090213     C                   END
330300090213     C*  Memorizzo qualificatore CMR
330400090213     C                   CLEAR                   EDRDE000
330500090213     C                   MOVEL     WKSC          KLNP1
330600090213     C                   MOVEL     'TD1153'      KNMC
330700090213     C                   Z-ADD     1             KNRP
330800090213     C     KRDE          CHAIN     EDRDE01L                           31
330900090213     C     *IN31         IFEQ      '1'
331000090213     C                   Z-ADD     VABAAS        RDEAAS
331100090213     C                   Z-ADD     VABLNP        RDELNP
331200090213     C                   Z-ADD     VABNRS        RDENRS
331300090213     C                   Z-ADD     VABNSP        RDENSP
331400090213     C     §PTCM1        IFNE      'S'
331500090213     C     WNRCMR        OREQ      *BLANKS
331600090213     C                   Z-ADD     1             RDENRP
331700090213     C                   ELSE
331800090213     C                   Z-ADD     2             RDENRP
331900090213     C                   END
332000090213     C                   MOVEL     'TD1153'      RDENMC
332100090213     C                   MOVEL     'AFB'         RDEVAL
332200090213     C                   WRITE     EDRDE000                             99
332300090213     c   99              eval      errori_in_Wrt = 'S'
332400090213     C                   END
332500090213     C*  Memorizzo la data
332600090213     C                   CLEAR                   EDRDE000
332700090213     C                   MOVEL     WKSC          KLNP1
332800090213     C                   MOVEL     'TD2380'      KNMC
332900090213     C                   Z-ADD     1             KNRP
333000090213     C     KRDE          CHAIN     EDRDE01L                           31
333100090213     C     *IN31         IFEQ      '1'
333200090213     C                   Z-ADD     VABAAS        RDEAAS
333300090213     C                   Z-ADD     VABLNP        RDELNP
333400090213     C                   Z-ADD     VABNRS        RDENRS
333500090213     C                   Z-ADD     VABNSP        RDENSP
333600090213     C     §PTCM1        IFNE      'S'
333700090213     C     WNRCMR        OREQ      *BLANKS
333800090213     C                   Z-ADD     1             RDENRP
333900090213     C                   ELSE
334000090213     C                   Z-ADD     2             RDENRP
334100090213     C                   END
334200090213     C                   MOVEL     'TD2380'      RDENMC
334300090213     C                   MOVEL     WDATFV        RDEVAL
334400090213     C                   WRITE     EDRDE000                             99
334500090213     c   99              eval      errori_in_Wrt = 'S'
334600090213     C                   END
334700090213     C*
334800090213     C                   END                                                    §PTCM1 = 'S'
334900090213     C*
335000090213     C                   ENDSR
335100090213     c*==================================================================*
335200090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
335300090213     c*==================================================================*
335400090213     c     Imp_ADDR_mail begsr
335500090213      *
335600090213     c                   clear                   Indirizzi
335700090213      *
335800090213      *  Lunghezza indirizzi presenti
335900090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
336000090213     c                   z-add     1             al_char           3 0
336100090213     c                   z-add     1             dal_char          3 0
336200090213     c                   z-add     1             tot_char          3 0
336300090213      *
336400090213     c     successivo    tag
336500090213      *
336600090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
336700090213      *   e aggiunge il dominio Bartolini + il (;) separatore
336800090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
336900090213     c                   eval      tot_char = al_char - dal_char
337000090213     c                   z-add     dal_char      Dac
337100090213     c                   z-add     tot_char      Luc
337200090213      *
337300090213     c                   clear                   Indir_Bartol     40
337400090213     c                   clear                   Indir_Parz       20
337500090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
337600090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
337700090213     c                             %Trim(bart_it)
337800090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
337900090213      *
338000090213     c                   if        al_char = Lun_Ind
338100090213     c                   goto      fine_indmail
338200090213     c                   else
338300090213     c                   eval      dal_char = ( al_char + 1 )
338400090213     c                   goto      successivo
338500090213     c                   end
338600090213      *
338700090213     C     fine_indmail  TAG
338800090213      *
338900090213     C                   ENDSR
339000090213     c*==================================================================*
339100090213      * Manda un Msg x E-mail
339200090213     c*==================================================================*
339300090213     c     Invio_Mail    begsr
339400090213      *
339500090213     c                   goto      non_inviare
339600090213      *
339700090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
339800090213     C* Inizializzo variabili
339900090213     C                   movel     *blanks       wrkEml          253
340000090213     C                   movel     *blanks       wrkMsg         5000
340100090213     C                   movel     *blanks       wrkOgg           44
340200090213      *
340300090213     C* Valorizzo i campi della e-m@ail - indirizzo
340400090213     C                   eval      wrkEml = Indirizzi
340500090213     C                   eval      wrkOgg = Oggetto
340600090213     C                   eval      wrkMsg = Messaggio
340700090213
340800090213     C                   call(e)   'TIS701C'
340900090213     C                   parm                    wrkEml
341000090213     C                   parm                    wrkOgg
341100090213     C                   parm                    wrkMsg
341200090213
341300090213     c     non_inviare   tag
341400090213     C*
341500090213     C                   ENDSR
341600090603     c*==================================================================*
341700090603      * Manda un Msg x E-mail a EDPAB
341800090603     c*==================================================================*
341900090603     c     Invio_Mail_AB begsr
342000090603      *
342100090603     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
342200090603     C* Inizializzo variabili
342300090603     C                   movel     *blanks       wrkEml          253
342400090603     C                   movel     *blanks       wrkMsg         5000
342500090603     C                   movel     *blanks       wrkOgg           44
342600090603      *
342700090603     C* Valorizzo i campi della e-m@ail - indirizzo
342800110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
342900110203     C                   eval      wrkEml = Indirizzi
343000090603     C                   eval      wrkOgg = Oggetto
343100090603     C                   eval      wrkMsg = Messaggio
343200110203      ********
343300110203     C********           call(e)   'TIS701C'
343400110203     C********           parm                    wrkEml
343500110203     C********           parm                    wrkOgg
343600110203     C********           parm                    wrkMsg
343700110203     C*
343800110203      ** *****
343900110203     C                   call(e)   'TRTCT00R2'
344000110203     C                   parm                    wrkEml
344100110203     C                   parm                    wrkOgg
344200110203     C                   parm                    wrkMsg
344300110203     C*
344400090603     C*
344500090603     C                   ENDSR
344600090211     ***********************************************************************
344700090211     **
344800090211     ** Send Message (QEZSNDMG) API
344900090211     **
345000090211     ***********************************************************************
345100090211     C     CalQEZSNDMG   BEGSR
345200090211      *
345300090211     ** Invio messaggio all'utente.
345400090211     C                   EVAL      SndMg03 = msgDSP
345500090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
345600090211     C                   EVAL      SndMg05(Jx) = User_msg
345700090211     C                   EVAL      SndMg06 = Jx
345800090211     C                   CLEAR                   QUSEC
345900090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
346000090211
346100090211     C                   CALL      'QEZSNDMG'
346200090211     C                   PARM                    SndMg01
346300090211     C                   PARM                    SndMg02
346400090211     C                   PARM                    SndMg03
346500090211     C                   PARM                    SndMg04
346600090211     C                   PARM                    SndMg05
346700090211     C                   PARM                    SndMg06
346800090211     C                   PARM                    SndMg07
346900090211     C                   PARM                    SndMg08
347000090211     C                   PARM                    Qusec
347100090211     C                   PARM                    SndMg10
347200090211     C                   PARM                    SndMg11
347300090211     C                   PARM                    SndMg12
347400090211      *
347500090211     C                   ENDSR
347600121105      *---------------------------------------------------------------*
347700121105      * CTRL caricamento schiere    PT                               -*
347800121105      *---------------------------------------------------------------*
347900121105     C     Check_PT      begsr
348000121105     C*
348100121105     c* Controllo riempimento schiera
348200121105     c                   clear                   trul0sds
348300121105     c                   eval      i0sski='CPT'
348400121105     c                   eval      i0sele=%elem(CPT)
348500121105     c                   eval      i0spie=x
348600121105     c                   eval      i0sfile='EDTAB00F'
348700121105     c                   eval      i0ssif=knsif
348800121105     c                   eval      i0spgm='TRTCT06R'
348900121113     c                   move      kpjbu         SvKpjbu
349000121113     c                   clear                   kpjbu
349100121105     c                   movel     trul0sds      kpjbu
349200121105     c                   call      'TRUL0SR'
349300121105     c                   parm                    kpjba
349400121113     c                   eval      kpjbu  =  SvKpjbu
349500121105     C*
349600121105     C                   ENDSR
349700090211     O*****************************************************************
349800090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
349900090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
350000090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
350100090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
