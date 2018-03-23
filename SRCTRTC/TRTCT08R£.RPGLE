000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600090224      *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT       con  IFTMIN        *
000700100623      *                                   NACEX SPAGNA       con  IFTMIN        *
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
003900090205     D edS00BGM      E DS
004000090205     D edS00CNT      E DS
004100090205     D edS00COM      E DS
004200090205     D edS00CTA      E DS
004300090205     D edS00DTM      E DS
004400090205     D edS00GID      E DS
004500090205     D edS00MEA      E DS
004600090205     D edS00NAD      E DS
004700090205     D edS00PCI      E DS
004800090210     D  D30_1                  4    353    DIM(10)
004900090205     D edS00RFF      E DS
005000090205     D edS00TDT      E DS
005100090205     D edS00TOD      E DS
005200090213     D edS00MOA      E DS
005300090213     D edS00FTX      E DS
005400090216     D  D05                   16    365    DIM(5)
005500090205     D edS00TSR      E DS
005600090205     D edS00UNA      E DS
005700090205     D edS00UNB      E DS
005800090205     D edS00UNH      E DS
005900090205     D edS00UNT      E DS
006000090205     D edS00UNZ      E DS
006100090210      **
006200090210     D  X30            S             35    DIM(20)                              generico segnacolli
006300090205      **
006400090209     D Valori_inErr    ds
006500090209     D  SKout_Errori                  1    DIM(50)
006600090209     D Descr_Errore    ds
006700090209     D  SKout_DesErr                 50    DIM(50)
006800090209      *
006900090205      *----------------------------------------------------*
007000090205     D Tipo_seg        S              3
007100100720     D Trovato_seg     S              1
007200100720???? D seg_imprevisto...
007300100720???? D                 s              1
007400100716      *
007500100716     d keyUNBCLI       s             35
007600100716     d keyTIPOMSG      s              6
007700100716     d keyVERSION      s              3
007800100716     d keyRELEASE      s              3
007900100716     d keyAGENCY       s              3
008000100716     d keyASSOCIA      s              6
008100100716      *
008200100720     D DsGenerica      s           2048
008300090209     D segmento        s           2048
008400090209     D Errore          s              1
008500000223     D W0140           S             14  0
008600991129     D WORA            S              6  0
008700991129     D XX              S              3  0 INZ
008800110328     d Key_Generica35  s             35
008900991129     D WDTGIO          S              8  0
009000991129     D DATEU           S              8  0
009100991129     D DATA_eur        S               D   DATFMT(*eur)
009200050112     D NUM_Sped        s                   LIKE(vabnsp)
009300090213     D NUM_RECord      s                   LIKE(vnREC)
009400090213     D SAVNUM_RECord   s                   LIKE(vnREC)
009500090213     D Last_RECord     s                   LIKE(vnREC)
009600050112     D svkpjbu         s                   like(kpjbu)
009700090209      *---------------------------------------------------------------------*
009800090209     D KPJBA         E DS
009900090209     D FNLV55DS      E DS
010000090209     D TIBS55DS      E DS
010100090209     D TISI95DS      E DS
010200090209     D UTEDSE0F      E DS
010300090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
010400090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
010500090209      * Ds scomposzione tipo capoconti
010600090209     D TCUDS           DS
010700090209     D  F34                    3      4
010800090209     D  F56                    5      6
010900090209     D DS1B          E DS
011000090209     D DS15          E DS
011100090209     D DSCV          E DS
011200090213     D TRTC86DS      E DS
011300090209     D EDIDSPT       E DS
011400090209     D EDIDSPU       E DS
011500090209     D EDIDSPV       E DS
011600090211     D EDIDSCL       E DS
011700090216     D EDIDSTC       E DS
011800090209      **
011900090209      *  DS x salvataggio dati impostati in EDiVAB
012000090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
012100090209     D SALVA           DS           545
012200090210      **
012300090209     D WLBDA8          DS
012400090209     D  G02DAT                 1      8  0
012500090209     D  G02INV                 9     16  0
012600090209     D  G02ERR                17     17
012700090209     D  G02TGI                18     22  0
012800090209      *  DS x scomposizione numero spedizione
012900090209     D DSSPE           DS
013000090209     D  SPEAAS                 1      4  0
013100090209     D  SPELNP                 5      7  0
013200090209     D  SPENRS                 8      9  0
013300090209     D  SPENSP                10     16  0
013400090209      *  DS x scomposizione segnacollo
013500090209     D DSSGN           DS
013600090209     D  SGNLNP                 1      3  0
013700090209     D  SGNLNA                 4      6  0
013800090209     D  SGNNRS                 7      8  0
013900090209     D  SGNNSC                 9     15  0
014000090209     D  SGNZNC                16     17  0
014100090209      *  DS x stampa segnacollo
014200090209      *---------------                                                      *
014300090211     D  vablnp_PT      s                   LIKE(vablnp)
014400090212     D  vabnrs_PT      s                   LIKE(vabnrs)
014500090211     D  vabctm_PT      s                   LIKE(vabctm)
014600090209     D Wvabtic         s                   LIKE(vabtic)
014700090209     C*
014800090209      *---------------------------------------------------------------------*
014900090209      * Schiere
015000090209      *---------------------------------------------------------------------*
015100090209      * Schiera per reperimento nr.seganacollo
015200090209     D SGN             S              7  0 DIM(5000)
015300090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
015400090209      * Stringhe per conversione alfanumerico/numerico N.Documento
015500090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
015600090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
015700090209      * Nr. documento alfanumerico da decodificare
015800090209     D NDA             S              1    DIM(35)
015900090209      * Nr. documento alfanumerico da già decodificato
016000090209     D NDN             S              1    DIM(15)
016100090209      * Messaggi di errore
016200090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
016300090209      * Tabella codici trattamento merce (1B)
016400090209     D CTM             S              2    DIM(200)
016500090209     D DTM             S             67    DIM(200)
016600090209      * Tabella codici valuta
016700090209     D CCV             S              3    DIM(200)
016800090209     D DCV             S             89    DIM(200)
016900090209      * Tabella codici nazioni
017000090209     D C15             S              3    DIM(500)
017100090209     D ISO             S              3    DIM(500)
017200090209     D CPF             S              2  0 DIM(500)
017300090209      * Tabella partner
017400121105     D CPT             S             35    DIM(200)
017500121105     D CPTparz         S             35    DIM(%elem(CPT))
017600121105     D KSC_UNB         S              7    DIM(%elem(CPT))
017700121105     D DPT             S             90    DIM(%elem(CPT))
017800121105     D DPU             S             90    DIM(%elem(CPT))
017900121105     D DPV             S             90    DIM(%elem(CPT))
018000121105     D TRUL0SDS      E DS
018100090209      *
018200121105     d Ptn_parziale    s              1    inz('N')
018300121105     d sav_CPTx        s              3  0 inz(0)
018400090209      * Tabella CL --> codici clienti - tariffa
018500130305     D CCL             S             35    DIM(999)
018600130305     D DCL             S             90    DIM(999)
018700090209      * Schiere x caricamento cod.cliente  <>
018800130305     D KCL             S              7  0 DIM(999)
018900090209      * Termini di consegna
019000110328     D K35_TpBolla     S             35    DIM(100)
019100110328     D Cod_TpBolla     S              3    DIM(100)
019200110328     D Decod_Porto     S              1    DIM(100)
019300110328     d Trovata_Tab_TB  S              1
019400110328      *
019500090216      * Tipo pagamento C/Assegno
019600090216     D CTC35           S             35    DIM(100)
019700090216     D TPI             S              2    DIM(100)
019800090209      * Decodifiche servizi speciali
019900090209     D SS35            S             35    DIM(100)
020000090209     D SS              S              3    DIM(100)
020100090209     D DSS             S             90    DIM(100)
020200090209      * Schiere x routine di conversione CAP
020300090209     D CAP5            S              1    DIM(5)
020400090209     D CAP9            S              1    DIM(9)
020500090209     D CAPM            S              1    DIM(9)
020600090209      * Schiera per memorizzazione FTX ricevuti
020700090209     D QUA             S              3    DIM(100)
020800090209     D FTX             S             70    DIM(100)
020900090209      * Tabelle capiconto
021000090209      * Schiere x località
021100090209     D LOD             S              1    DIM(35)
021200060608      **?------------------------------------------------------------------ */
021300060608     C*? Ds Scrittura del VAT "E"  passaggio nuovi campi BIC3
021400060608      **?------------------------------------------------------------------ */
021500060612     D XTOEBCDDS     E DS
021600060620      *
021700060608      **?------------------------------------------------------------------ */
021800050112      * Numeratore Bolle (302)
021900050112     D trul33ds      E DS
022000050112     D Ds3C          E DS
022100090209     C*
022200160524     d FTX_ServizioMail_xDestinatario...
022300160524     d                 s             70A
022400160524     d FTX_MAIL_come_Servizio...
022500160524     d                 s              1A
022600160524     d FTX_ServizioSMS_xDestinatario...
022700160524     d                 s             70A
022800160524     d FTX_SMS_come_Servizio...
022900160524     d                 s              1A
023000090209      **?------------------------------------------------------------------ */
023100090209     C* Definizione campi di work
023200090209     D pVLB            s                   LIKE(VABVLB)
023300090209     D kKUT            s                   LIKE(TBLKUT)
023400090209     D kCOD            s                   LIKE(TBLCOD)
023500090209     D kKEY            s                   LIKE(TBLKEY)
023600090209     D kKCC            s                   LIKE(ACOKCC)
023700090209     D kKSC            s                   LIKE(ACOKSC)
023800090209     D kAAA            s                   LIKE(NUFAAA)
023900090209     D kCNU            s                   LIKE(NUFCNU)
024000090209     D kFIL            s                   LIKE(NUFFIL)
024100090209     D kAAS            s                   LIKE(VABAAS)
024200090209     D kLNP            s                   LIKE(VABLNP)
024300090209     D kNRS            s                   LIKE(VABNRS)
024400090209     D kNSP            s                   LIKE(VABNSP)
024500090209     D kCMR            s                   LIKE(VABCMR)
024600090209     D kCCM            s                   LIKE(VABCCM)
024700090209     D kNCD            s                   LIKE(VADNCD)
024800090210     D kCNT            s                   LIKE(VADCNT)
024900090209     D kNMC            s                   LIKE(RDENMC)
025000090209     D kNRP            s                   LIKE(RDENRP)
025100090209     D kLNP1           s                   LIKE(RDELNP)
025200090209      *
025300090209      *  Invio E-mail
025400090209     D Indirizzi       s            253
025500090209     D Oggetto         s             44
025600090209     D Messaggio       s           5000
025700090209      *
025800090209     d   DSmail        ds
025900090209     D Mail_Ind                      44
026000090209     d   IND_char                     1    dim(44)
026100090209      *
026200090209     D Lun_Ind         s              5u 0
026300090209      *
026400090209     D Dac             s              5u 0
026500090209     D Luc             s              5u 0
026600090209      *
026700090209     C*---------------------------------------------------------------*
026800090209     D MsgDSP          S            256                                         Testo generico
026900090209     C*---------------------------------------------------------------*
027000090209     D SndMg01         S             10                                         Message type
027100090209     D                                     INZ('*INFO')
027200090209     D SndMg02         S             10                                         Deluvery mode
027300090209     D                                     INZ('*BREAK')
027400090209     D SndMg03         S            256                                         Message text
027500090209     D SndMg04         S             10I 0                                      Length of text
027600090209     D                                     INZ(%SIZE(SndMg03))
027700090209     D SndMg05         S             10                                         User profile
027800090209     D                                     DIM(299)
027900090209     D SndMg06         S             10I 0                                      Number of user
028000090209     D SndMg07         S             10I 0                                      Message sent indic.
028100090209     D SndMg08         S             10I 0                                      Function requested
028200090209     D SndMg10         S              1                                         Show display
028300090209     D                                     INZ('N')
028400090209     D SndMg11         S             20                                         Qualified MSGQ name
028500090209     D SndMg12         S              4                                         Name type indicator
028600090209     D                                     INZ('*USR')
028700090209      * indice di scihera
028800090209     D Jx              S              5I 0
028900090209      *
029000090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
029100090209      *
029200090209     d bart_it         C                   CONST('@Bartolini.it;')
029300090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
029400060612      * ?================================================================== */
029500060612      *
029600060612      * ?   * Campi x decodifica * (INPUT  del Record)
029700090130     D  Dati           s           2048
029800060612      * ?   *--------------------------------------------------------------*
029900060612     D  se_errore      s              1    inz(' ')
030000060612      * ?* ------------------------------------------------------ *
030100060710     D Digits          C                   '0123456789'
030200060612      * ?================================================================== */
030300060612      *   Ciclo principale
030400090205      * ?================================================================== */
030500060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
030600060612     C*
030700060612     C                   if        not %open(tivin00r)
030800060612     C                   open      tivin00r
030900060612     C                   endif
031000060612      **
031100060612      * ?------------------------------------------------------------------ */
031200060612     C*? Controllo dati arrivati da DPD
031300060612      * ?------------------------------------------------------------------ */
031400060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
031500060612      * ?- controllando che la trasmissione sia completa.
031600090205      **
031700060612      * ?              /*---------------------- */
031800090205     c                   goto      nonFare_adesso
031900060612     c                   exsr      check_Trasm
032000090205     c     nonFare_adessotag
032100060612      * ?              /*---------------------- */
032200090205      **
032300060613      **  Errore di trasmissione x tutti i records
032400060613      **   --> file in errore
032500060613     c                   if        se_errore = 'S'
032600060612
032700060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
032800090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
032900090205     c                   exsr      Msg_errato
033000060612      **
033100060612     c                   else
033200060614      **
033300060612      * ?------------------------------------------------------------------ */
033400060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
033500060612      * ?------------------------------------------------------------------ */
033600090205      **
033700060612      * ?              /*---------------------- */
033800060612     c                   exsr      Importa_Msg
033900060612      * ?              /*---------------------- */
034000060612
034100060614     c                   end
034200060614
034300060612     C                   if        %open(tivin00r)
034400060612     C                   close     tivin00r
034500060612     C                   endif
034600060612      *
034700060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
034800090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
034900090225     c                             esito ='0'
035000060710     C                   eval      esito ='0'
035100090225      *
035200090225     c                   COMMIT
035300090224      *
035400090213     c                   if        almeno_un_due ='S'
035500090213     C                   eval      esito ='1'
035600060614     C                   endif
035700090213     C                   endif
035800060614      *
035900060612     c                   SETON                                        LR
036000060612      * ?================================================================== */
036100060612      *? Importa i records della tramsissione
036200060612      * ?------------------------------------------------------------------ */
036300060612     c     Importa_Msg   Begsr
036400060612
036500090211     C                   EXSR      AZZMSG
036600060614
036700060612     c     *start        setll     Tivin00r
036800060612     c                   read      Tivin00r
036900060612
037000060612     c                   dow       not %eof(Tivin00r)
037100060614
037200060614      * solo i record sflaggati da rielaborare
037300060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
037400090211      *  Sempre Record OK
037500090211      ** Controlli formali sui campi
037600090211     C                   eval      vinFLG = '1'
037700060612     c                   clear                   se_errore
037800060612
037900060612      ** Decodifica record a campi variabili
038000060612      * ?              /*---------------------- */
038100090205     c                   exsr      Decod_Segmento
038200090209      * ?              /*---------------------- */
038300090209
038400090213      *** ------------------------------------
038500090213      *** A FINE CICLO DI UN DETTAGLIO:   -->"UNT" x IFTMIN
038600090213      *** ------------------------------------
038700090213      * ?              /*---------------------- */
038800090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
038900090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
039000090213      * ?              /*---------------------- */
039100090213     c                   if        tipo_seg = 'UNT'
039200090213      *
039300090213      *  Deve flaggare il dettaglio con dei problemi
039400090213     C                   if        Err_in_detta = 'S'
039500090213      **?              /*---------------------- */
039600090213     c                   exsr      DETta_Errato
039700090213      **?              /*---------------------- */
039800090213     c                   end
039900090213      *
040000060612      **  Se presente un errore nel record emette una segnalazione msg
040100090213     c                   if        se_errore = 'S'
040200090213     c                   else
040300090209      ******
040400090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
040500090227     c                   if         se_trovato_NAD ='S'
040600060612      * ?              /*---------------------- */
040700090209     c                   exsr      Scrive_VAB
040800060612      * ?              /*---------------------- */
040900090227     c                   else
041000090227     c                   eval      errori_in_Wrt = 'S'
041100090227     c                   end
041200090227      *
041300090209      *  Problemi nella decodifica dei campi VAB/VAT
041400090213     c                   if        errori_in_Wrt = 'S'
041500090213      *
041600090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
041700090213     c                   exsr      Error_DET
041800090213      *
041900090213     c                   ROLBK
042000090213      *
042100090213     c                   else
042200090213     c                   COMMIT
042300090227      * Dopo aver confermato, con COMMIT
042400090227      *  si prepara per una nuova spedizione
042500090227      * ?                         --------------
042600090227     c                   exsr      Nuova_spediz
042700090227      * ?                         --------------
042800090213     c                   end
042900090209
043000090209     c                   end
043100090210      **
043200060620     c                   end
043300060612
043400060621      *  x errore bloccante nella composizione del VAB/VAT
043500060621     c                   if        err_bloccante ='S'
043600060621     C                   eval      vinFLG = '2'
043700090211     c                   eval      esito  = '2'
043800100623     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NACEX  '
043900090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
044000090601     C                             Bolle import in filiale - messaggio -
044100090601     C                             con UNB non presente in tabella PT controlla-
044200090601     C                             re EDI ricevuto su UPLOAD.'
044300090601     c                   exsr      invio_mail_AB
044400060621     c                   end
044500060612
044600060612     c                   update    Tivin000
044700090211
044800090211      *** se non è riconosciuto l'UNB deve uscire direttamente
044900090211     c                   if        se_errore ='S'  and
045000090211     c                             tipo_seg = 'UNB'
045100090213     c                   eval      err_bloccante = 'S'
045200090211     c                   exsr      Msg_errato
045300090211     c                   LEAVE
045400090211     c                   endIF
045500090211      ***
045600060614     c                   endIF
045700060612
045800060612     c                   read      Tivin00r
045900060612     C                   ENDdo
046000060612      **
046100060612     c                   endSR
046200090211     C*----------------------------------------------------------------
046300090211     C*? AZZMSG - Azzera dati messaggio
046400090211     C*----------------------------------------------------------------
046500090211     C     AZZMSG        BEGSR
046600090211     C*
046700090211     C* Pulisco dati di work x foglio viaggio
046800100630     C                   MOVEL     *BLANKS       UNB_NFViaggio    35            * WNUMFV
046900100630     C                   MOVEL     *BLANKS       UNB_NrCMR        35            * WNRCMR
047000100630     C                   Z-ADD     0             DTM_DtPartenza    8 0          * WDATPA
047100090211     C                   Z-ADD     0             WDATFV            8 0
047200090211     C                   Z-ADD     0             WHHNFV            4 0
047300090211     C                   Z-ADD     0             WDTCMR            8 0
047400090211     C                   Z-ADD     0             WHHCMR            4 0
047500090211      *
047600090211      * pulisce gli RFF+FF di testata
047700090211     C                   clear                   $CLKSC            7 0
047800090211     C                   clear                   $CLTAR            3
047900090211     C                   clear                   $CLLNP            3 0
048000090211     C                   clear                   $CLNRS            2 0
048100090211     C                   clear                   $CLCTM            2
048200090211     C                   clear                   $CLBS1            1
048300090211     C                   clear                   $CLfnsp           1            *blk/S
048400090211      *
048500090211     C*  Inizializzo variabile new documento
048600090211     C                   Z-ADD     0             KCL
048700090211     C                   Z-ADD     0             WW                3 0
048800090211     ** Inizializza campo di work
048900090211     c                   move      'N'           WGID99
049000090211      * per controllare se almeno un record è stato importato sul VAB
049100090211     c                   clear                   Almeno_Uno        1
049200090213     c                   clear                   Almeno_Un_Due     1
049300090211     c                   eval      esito  = '0'
049400090211     C*
049500090211     C                   ENDSR
049600090210     C*----------------------------------------------------------------
049700090210      *? DECODIFICA DATI UNB
049800090210     C*----------------------------------------------------------------
049900090210     C     DATI_UNB      BEGSR
050000090210      *
050100090209      *  INIZIALIZZA  dati fondamentali messaggio
050200090209     C                   clear                   EDIDSPT
050300090209     C                   clear                   EDIDSPU
050400090209     C                   clear                   EDIDSPV
050500090209     C                   clear                   wnrDOC           35
050600090209     C                   move      'N'           PT_ok             1
050700090209      ** ---------------------------------------------------------------
050800090209      * quindi 1° cosa:
050900090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
051000090209      *     senza questo il messaggio NON è trascodificabile
051100090209     C                   MOVEL     unb0004       WCOD             35
051200090209     C                   MOVE      unb000720     WCOD
051300090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
051400090209     C                   Z-ADD     1             XX                3 0
051500090209     C     WCOD          LOOKUP    CPT(XX)                                32
051600090209      *
051700090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
051800090209     C                   If        *IN32 = *off
051900090209      * ******  Errore generale sul Messaggio
052000090209      * Msg di allerta Traduzione
052100090209     C                   EVAL      Indirizzi = CED_Bart
052200100623     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NACEX  '
052300090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
052400090209     C                             Bolle import in filiale - messaggio -
052500090209     C                             con UNB non presente in tabella PT controlla-
052600090209     C                             re EDI ricevuto su UPLOAD.'
052700090601     c                   exsr      invio_mail_AB
052800090209      *
052900090209     c                   exsr      invio_mail
053000090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
053100090210     C                   eval      vinFLG = '2'
053200090210     C                   eval      se_errore   = 'S'
053300090210     c                   goto      end_UNB
053400090209      *
053500090209     C                   move      'N'           PT_ok             1
053600090209      *
053700090209      *             *------------------------------*
053800090209     C                   eLSe
053900090209      *             *------------------------------*
054000090209      * Se tutto OK su tab.PT:
054100090209     C                   move      'S'           PT_ok             1
054200090209      *
054300090209     C                   MOVEL     DPT(XX)       EDIDSPT
054400090209     C                   MOVEL     DPU(XX)       EDIDSPU
054500090209     C                   MOVEL     DPV(XX)       EDIDSPV
054600100623      *
054700090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
054800090209      *  ricevuti:
054900090209     c                   movel     §PVema        mail_ind         44
055000090209      *
055100090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
055200090209     c                   if        mail_ind <> *blank
055300090209     c                   exsr      imp_ADDR_mail
055400090209     c                   end
055500090209      *
055600090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
055700090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
055800090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
055900090209     C                   if        §PULUN = *blank
056000090209     C                   eval      §PULUN = '00'
056100090209     c                   end
056200090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
056300090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
056400090209      *
056500090209     C                   Z-ADD     §PTKSC        WKSC              7 0
056600090211     C                   MOVEL     §PTLNP        VABlnp_PT
056700090212     C                   MOVEL     §PTNRS        VABnrs_PT
056800090211     C                   MOVEL     §PTCTM        VABctm_PT
056900090209      *
057000090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
057100090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
057200090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
057300090209      *  i records trasmessi).
057400090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
057500090209      *
057600090209      *  In base al cod.trattamento merce reperisco tipo tratt.
057700090209     C                   CLEAR                   DS1B
057800090209     C                   Z-ADD     1             Y                 3 0
057900090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
058000090209     C     *IN32         IFEQ      '1'
058100090209     C                   MOVEL     DTM(Y)        DS1B
058200090209     C                   MOVEL     DS1B          WSVTRM           38
058300090209     C                   END
058400090209      *
058500090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
058600090209     C                   clear                   ACORAG
058700090209     C                   clear                   INDCAE
058800090209     C                   clear                   INDCAP
058900090209     C                   clear                   INDSTA
059000090209      * Decodifico partner
059100090209     C                   Z-ADD     1             KKUT
059200090209     C                   Z-ADD     KCI           KKCC
059300090209     C                   MOVE      §PTKSC        KKSC
059400090209     C     KACO          CHAIN     CNACO00F                           31
059500090209     C     KIND          CHAIN     CNIND00F                           31
059600090211      *
059700090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
059800090211      *   questa schiera è utilizzata anche per i clienti.
059900090211     C     §ptKSC        LOOKUP    KCL                                    39
060000090211     c                   If        *in39 = *off
060100090211     C                   add       1             WW
060200090211     C                   z-add     §ptKSC        KCL(WW)
060300090211     C                   Endif
060400090209      *
060500090209     C                   MOVEL     unb0017       WAA4              4 0
060600090209     C                   MOVE      WAA4          WAA2              2 0
060700090209     C                   MOVE      unb0017       WGG               2 0
060800090209     C                   MOVE      unb0017       WDTPRE            8 0
060900090209     C                   MOVE      unb0019       WHMPRE            4 0
061000090209     C                   MOVE      unb0017       WDTMSG            6 0
061100090209     C                   MOVE      WAA2          WDTMSG
061200090209     C                   MOVEL     WGG           WDTMSG
061300090210      * potremmo
061400090210      * Utilizzare l'identificativo della trasmissione come CMR
061500090209     C                   IF        unb0022 <> *blank
061600090209     C                   MOVEL     unb0022       WNRDOC
061700100630     C                   MOVEL     unb0022       UNB_NFViaggio                  * WNUMFV
061800100630     C                   MOVEL     unb0022       UNB_NrCMR                      * WNRCMR
061900090209     c                   end
062000090209      *
062100090209     c                   Endif
062200090209      **
062300090210     c     end_UNB       endSR
062400090209      * ?================================================================== */
062500090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
062600090209      * ?================================================================== */
062700090227     C     Nuova_spediz  BEGSR
062800090227      **
062900090227      * Pulisco campi del file
063000090227     c                   z-add     vnrec         SAVNUM_RECord
063100090227     C                   CLEAR                   EDiVAB00
063200090227     C                   CLEAR                   EDiVAT00
063300090227     C                   CLEAR                   EDiVAD00
063400090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
063500090227     C                   MOVEL     'N'           WGID99            1
063600090227     C                   clear                   XY                5 0
063700090227     C                   clear                   §§                5 0
063800090227     C                   clear                   WS1496            5 0
063900090227     C                   clear                   WNSP             35
064000090227     C                   Z-ADD     *ZEROS        PVLB
064100090227     C                   MOVEA     *HIVAL        SGN
064200090227     C*  Azzero codice cliente - tariffa
064300090227     C                   Z-ADD     0             WTOCOC           16 0
064400090227     C                   Z-ADD     0             WTOPLC           16 2          ???
064500090227     C                   Z-ADD     0             WTOPNC           16 2          ???
064600090227     C*  Azzero codice cliente - tariffa
064700090227     C                   clear                   WCLKSC
064800090227     C                   clear                   WCLTAR
064900090227     C                   clear                   WCLLNP
065000090227     C                   clear                   WCLNRS
065100090227     C                   clear                   WCLCTM
065200090227     C                   clear                   WCLBS1
065300090227     C                   clear                   WCLfnsp                        *blk/S
065400090227     C                   clear                   WNOT1            35
065500090227     C                   clear                   WNOT2            35
065600160524     C                   clear                   Note_1T          35
065700160524     C                   clear                   Note_2T          35
065800160524     C                   clear                   Note_1sms        35
065900160524     C                   clear                   Note_2sms        35
066000160524      *
066100160524     c                   eval      FTX_ServizioMail_xDestinatario = *blank
066200160524     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
066300160524     c                   eval      FTX_MAIL_come_Servizio = *blank
066400160524     c                   eval      FTX_SMS_come_Servizio = *blank
066500090227     C*  Campi di work
066600090227     c                   clear                   wVabTIC
066700090227     c                   clear                   wri_vabtic        1
066800090227      *
066900090227     C* pulisce campi di work
067000090227     c                   clear                   watrde           35
067100090227     c                   clear                   wattel           25
067200090227     C                   clear                   WRMN              3
067300090227      *
067400090227      * Un errore nel dettaglio
067500090227     C                   clear                   Err_in_detta      1
067600090227     c                   clear                   errori_in_Wrt     1
067700090227     c                   clear                   se_trovato_NAD    1
067800091204     C                   clear                   Rifer_Princip    35
067900090227      **
068000090227     c                   endSR
068100090227      * ?================================================================== */
068200090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
068300090227      * ?================================================================== */
068400090227     C     DATI_UNH      BEGSR
068500090227      *
068600090227      *  Prepara una nuova spedizione
068700090227      * ?                         --------------
068800090227     c                   exsr      Nuova_spediz
068900090227      * ?                         --------------
069000090211      *
069100090211      ** Riferimento del cliente
069200090211     C                   MOVEL     unh0062       WPRGSP           35
069300090211     C                   MOVEL     unh0062       WNSP             35
069400090211     C                   MOVEL     unh0062       WNUM             35
069500090211      * ?                                       --------
069600090211     C                   MOVEL     unh0062       VABRMA
069700090211      * ?                                       --------
069800090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
069900090211     C                   SUB       1             WLUNGH
070000090211     C                   EXSR      TSTNUM
070100090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
070200090211      * ?                                       --------
070300090211     C                   MOVEL     WNUMOK        VABRMN
070400090211      * ?                                       --------
070500090211     C                   END
070600090210      **
070700090303     C                   MOVEL     VABlnp_PT     VABLNP
070800090303     C                   MOVEL     VABnrs_PT     VABNRS
070900090303     C                   MOVEL     VABctm_PT     VABCTM
071000090303      **
071100090303      **
071200090210     c     end_UNH       endSR
071300090210      * ?================================================================== */
071400090210     C*? Impsota i campi del VAB e del VAT da scrivere a rottura dettaglio
071500090210      * ?================================================================== */
071600090210     C     DATI_DTM      BEGSR
071700090210     C*
071800090209     C*  Se data documento  memorizzo numero e data x CMR
071900090209     C     dtm2005       IFEQ      '137'
072000090209     C                   MOVEL     dtm2380       WDATA            35            Data
072100090209     C                   MOVEL     dtm2379       WFORM             3            Formato
072200090209     C                   EXSR      CNVDAT
072300090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
072400090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
072500090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
072600090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
072700090209     C     WERRO         IFEQ      'S'
072800090210     C                   eval      vinMSG = 'DTM data errata'
072900090213     c                   exsr      Error_DET
073000090210     c                   goto      end_DTM
073100090209     C                   END
073200090209     C                   END
073300090209      **
073400090209     C*  Se data consegna richiesta imposto già sui campi del VAB
073500090209     C     dtm2005       IFEQ      '2'
073600090209     C                   MOVEL     dtm2380       WDATA            35            Data
073700090209     C                   MOVEL     dtm2379       WFORM             3            Formato
073800090209     C                   EXSR      CNVDAT
073900090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
074000090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
074100090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
074200090209     C     WERRO         IFEQ      'S'
074300090210     C                   eval      vinMSG = 'DTM data errata'
074400090213     c                   exsr      Error_DET
074500090210     c                   goto      end_DTM
074600090209     C                   END
074700090209     C                   END
074800090211      **
074900090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
075000090211     C     dtm2005       IFEQ      '11'
075100090211     C                   MOVEL     dtm2380       WDATA            35            Data
075200090211     C                   MOVEL     dtm2379       WFORM             3            Formato
075300100630     C                   EXSR      CNVDAT
075400100630     C                   MOVEL     WCAMG         DTM_DtPartenza                 * WDATPA
075500100630     C     WERRO         IFEQ      'S'                                          Data Part.  ch.
075600090211     C                   eval      vinMSG = 'DTM data errata'
075700090213     c                   exsr      Error_DET
075800090211     c                   goto      end_DTM
075900090211     C                   END
076000090211     C                   END
076100090209      **
076200090210     c     end_DTM       endSR
076300090209      * ?================================================================== */
076400090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
076500090209      * ?================================================================== */
076600090209     C     DATI_TOD      BEGSR
076700090209      *
076800090210      *  Trascodifico tipo trasporto
076900090210     c                   clear                   TOD_char3         3
077000090210      *
077100090210      *  imposta il default dalla tabella PT se c'è
077200090210     C     §puPFA        ifNE      *blank
077300090210     c                   SELECT
077400090210      *  Porto Franco
077500090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
077600090210     C                   movel     '4'           VABCBO                         + C/Ass
077700090210     c                   WHEN      §puPFA = 'F'
077800090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
077900090210      *  Porto Assegnato
078000090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
078100090210     C                   movel     '6'           VABCBO                         + C/Ass
078200090210     C                   WHEN      §puPFA = 'A'
078300090210     C                   movel     '2'           VABCBO                         Senza C/Ass
078400090210     C                   ENDSL
078500090210     c                   end
078600090210      *
078700090210      *   Imposta o il codice o il metodo
078800090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
078900090210     c                   if        tod4053  <> *blank
079000090210     c                   movel(p)  tod4053       TOD_char3
079100090210     c                   else
079200090210     c                   if        tod4215  <> *blank
079300090210     c                   movel(p)  tod4215       TOD_char3
079400090210     c                   end
079500090210     c                   end
079600110328      ****************
079700110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
079800110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
079900110328      ****************
080000110328     c                   eval      Trovata_Tab_TB ='N'
080100110328      *
080200110328     c                   if              TOD_char3 <> *blank
080300110328     c                   movel(p)  TOD_char3     Key_Generica35
080400110328     c                   move      §puTB         Key_Generica35
080500110328     C                   eval      Y = 1
080600110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
080700110328     c   32              eval      Trovata_Tab_TB ='S'
080800110328      *
080900110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
081000110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
081100110328      *
081200110328     c                   if          Trovata_Tab_TB ='N'
081300110328     c                                 and
081400110328     C                               tod4215 <> *blank
081500110328     c                   movel(p)  tod4215       TOD_char3
081600110328     c                   movel(p)  TOD_char3     Key_Generica35
081700110328     c                   move      §puTB         Key_Generica35
081800110328     C                   eval      Y = 1
081900110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
082000110328     c   32              eval      Trovata_Tab_TB ='S'
082100110328     c                   end
082200110328     c                   end
082300110328      *
082400110328      ****************
082500110328      ******* Se vi sono eccezioni sul cliente imposta quelle
082600110328      ****************
082700110328     c                   If          Trovata_Tab_TB ='S'
082800110328      *
082900110328     c                   SELECT
083000110328     c                   WHEN      Decod_Porto(Y) = 'F'
083100110328     c                               and  VABcas > *zeros
083200110328     C                   movel     '4'           VABCBO                         + C/Ass
083300110328     c                   WHEN      Decod_Porto(Y) = 'F'
083400110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
083500110328      *  Porto Assegnato
083600110328     c                   WHEN      VABCAS > *zeros
083700110328     C                   movel     '6'           VABCBO                         + C/Ass
083800110328     C                   OTHER
083900110328     C                   movel     '2'           VABCBO                         Senza C/Ass
084000110328     C                   ENDSL
084100110328      *
084200110328      ****************  altrimenti
084300110328      ******* Se non vi sono eccezioni per il cliente
084400110328      *   Rileva a questo punto lo Standard per tutti
084500110328      ****************
084600110328     c                   elseIf      Trovata_Tab_TB ='N'
084700090210      **
084800090210     c                   if              TOD_char3 <> *blank
084900090210     c                   movel(p)  TOD_char3     fld35a           35
085000090213     C                   movel     UNB_Mittente  etbUNB
085100090213     C                   movel(p)  'TOD'         etbSGM
085200090213     C                   movel(p)  TOD_char3     etbVALSGM
085300090213     c     K_TAB1L       chain     edstbl01l
085400090210      *
085500090210      * prova quindi senza l'UNB specifico del cliente
085600090213     c                   if        not %Found(edstbl01l)
085700090213     C                   clear                   etbUNB
085800090213     c     K_TAB1L       chain     edstbl01l
085900090210     c                   end
086000090210      *
086100090213     c                   if        %Found(edstbl01l)
086200090210     c                   SELECT
086300090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
086400090210     C                   movel     '4'           VABCBO                         + C/Ass
086500090213     c                   WHEN      ETBDATI = 'F'
086600090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
086700090210      *  Porto Assegnato
086800090210     c                   WHEN      VABCAS > *zeros
086900090210     C                   movel     '6'           VABCBO                         + C/Ass
087000090210     C                   OTHER
087100090210     C                   movel     '2'           VABCBO                         Senza C/Ass
087200090210     C                   ENDSL
087300090210      *
087400090210     c                   end
087500090210     c                   endIF
087600090209      **
087700110328     c                   endIF
087800110328      **
087900090210      **  Il tipo bolla non è presente
088000090210     c                   if        VABCBO = *blank
088100090210     C                   eval      vinMSG = 'TOD non rilevato'
088200090213     c                   exsr      Error_DET
088300090210     c                   goto      end_TOD
088400090210     c                   end
088500090210      **
088600090210     c     end_TOD       endSR
088700090210      * ?================================================================== */
088800090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
088900090210      * ?================================================================== */
089000090210     C     DATI_RFF      BEGSR
089100090210      *
089200090210     C                   clear                   WAGE             35
089300110224     C                   clear                   WFF              35
089400110224     C                   clear                   WCU              35
089500091204      *
089600091204      *   Se manca sugli altri riporta sempre il principale per non bloccare il messaggio
089700091204     c                   if        RFF1154 = *blank and Rifer_Princip <> *blank
089800091204     c                   eval      RFF1154 = Rifer_Princip
089900091204     c                   end
090000090210      *
090100090210      *  Trascodifico riferimento spedizione
090200100629     c                   if        RFF1153 = 'CU' and RFF1154 <> *BLANKS
090300090210     C                   movel(p)  RFF1154       WAGE             35
090400091204     C                   movel(p)  RFF1154       Rifer_Princip    35
090500090210     c                   end
090600090210      **
090700090210     C                   IF         WAGE <> *BLANKS
090800090210     C                   movel     WAGE          WNSP             35
090900090210     C                   movel     WAGE          WNUM             35
091000090210     C     ' '           scan      WAGE          WLUNGH            3 0
091100090210     C                   sub       1             WLUNGH
091200090210     C                   EXSR      TSTNUM
091300090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
091400090210     C                   movel     WAGE          VABRMA
091500100629     C                   IF         WRMN = 'CU' or WRMN = *blanks
091600090210     C                   movel     WNUMOK        VABRMN
091700100629     C                   movel     'CU'          WRMN
091800090210     C                   ENDIF
091900090210     C                   ENDIF
092000090210     C                   ENDIF
092100090210      *
092200090210      * Codice Cliente di riferimento
092300090210     C                   if        RFF1153 = 'IT ' and RFF1154 <> *BLANKS
092400090210     C                   movel     RFF1154       WFF
092500090210     c                   END
092600090211      *
092700090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
092800090211     C     WFF           IFNE      *BLANKS
092900090211     C                   eval      XX = 1
093000090211     C                   movel     §PTKSC        WCCL             35
093100090211     C                   movel     WFF           WCOM28           28
093200090211     C                   move      WCOM28        WCCL
093300090211     C     WCCL          lookup    CCL(XX)                                34
093400090211     C     *IN34         IFEQ      '1'
093500090211     C                   movel     DCL(XX)       EDIDSCL
093600090211     C                   z-add     §CLKSC        WCLKSC            7 0
093700090211     C                   movel     §CLTAR        WCLTAR            3
093800090211     C                   z-add     §CLLNP        WCLLNP            3 0
093900090211     C                   z-add     §CLNRS        WCLNRS            2 0
094000090211     C                   movel     §CLCTM        WCLCTM            2
094100090211     C                   movel     §CLBS1        WCLBS1            1
094200090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
094300090211      * imposta la LNP   x il dettaglio specifco
094400090211     c                   eval      VABlnp = §CLlnp
094500090211      * imposta la Serie x il dettaglio specifco
094600090211     c                   eval      VABnrs = §CLnrs
094700090211      *
094800090211      *  In base al cod.trattamento merce reperisco tipo tratt.
094900090211      *    per un dettaglio specifico.
095000090211     C                   CLEAR                   DS1B
095100090211     C                   Z-ADD     1             Y                 3 0
095200090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
095300090211     C     *IN32         IFEQ      '1'
095400090211     C                   MOVEL     DTM(Y)        DS1B
095500090211     C                   END
095600090211      *
095700090211      * Se il cliente non era sulla schiera lo aggiunge
095800090211     C     §CLKSC        lookup    KCL                                    39
095900090211     C                   if        *in39 = *off
096000090211     C                   add       1             WW
096100090211     C                   z-add     §CLKSC        KCL(WW)
096200090211     c                   endif
096300090211      *
096400090211     C                   ENDIF
096500090211     C                   ENDIF
096600090211     C*----------------------------------------------------------------
096700090210      *
096800100630     C                   if        RFF1153 = 'AGE' and RFF1154 <> *blanks
096900090210     C                   movel     RFF1154       WCU
097000090210     C                   END
097100090210      *
097200090210      * Numero Ordine di vendita
097300090210     C     WCU           IFNE      *BLANKS
097400090210     C                   movel     WCU           WNUM             35
097500090210     C     ' '           scan      WCU           WLUNGH
097600090210     C                   sub       1             WLUNGH
097700090210     C                   EXSR      TSTNUM
097800090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
097900090210     C                   movel     WNUMOK        VABRMN
098000100630     C                   movel     'AGE'         WRMN
098100090210     C                   Endif
098200090210     C                   ENDIF
098300090210      *
098400090210      * NOn c'è l'identificativo bolla del cliente
098500100629     c                   if        (RFF1153 = 'CU'  and RFF1154 = *BLANKS) or
098600100630     c                             (RFF1153 = 'AGE' and RFF1154 = *BLANKS)
098700090210     C                   eval      vinMSG = 'RFF riferimento assente'
098800090213     c                   exsr      Error_DET
098900090210     c                   goto      end_RFF
099000090210     C                   ENDIF
099100090210      *
099200090210     c     end_RFF       endSR
099300090209      * ?================================================================== */
099400090213     C*?  C.O.D. Monetary Amount
099500090209      * ?================================================================== */
099600090213     C     DATI_MOA      BEGSR
099700090210      *
099800090216     C* Controllo se campo importo numerico
099900090216      * con 3 decimali
100000090216     C     moa5004       IFNE      *zeros
100100090216      *
100200090216     C     moa5025       IFEQ      '157'
100300100630     C     moa5004       div       100           VABIAS                         IMPORTO
100400090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
100500090216     C                   END
100600090216      *
100700090216     C     moa5025       IFEQ      '77 '                                        Valore
100800100630     C     moa5004       div       100           VABVMD                         MERCE
100900090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
101000090216     C                   END
101100090216      *
101200090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
101300090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
101400100630     C     moa5004       div       100           VABCAS
101500090216     C                   MOVEL     moa6345       VABVCA
101600100705      * SE Cash   Forza *blank sempre
101700100630     C     moa5004       ifgt      *ZEROS
101800100705     C                   MOVEL     '  '          VABTIC
101900100705     C                   MOVEL     '  '          WVABTIC
102000090216     C                   ENDIF
102100090216     C                   END
102200090216     C                   END
102300090216      *
102400090216     C                   END                                                    Imp.non numer.
102500090213      *
102600090213     c     end_MOA       endSR
102700090213      * ?================================================================== */
102800090213     C*?  FREE TEXT
102900090213      * ?================================================================== */
103000090213     C     DATI_FTX      BEGSR
103100090213      *
103200160525     c                   if         ftx4451  = 'AAI' or
103300160525     c                              ftx4451  = 'SIN'
103400100630      * ? solo se si tratta di note AAI SIN ICN o PAI non altro
103500090216     C                   DO        4             X
103600090216     C     D05(X)        IFNE      *BLANKS
103700090216     C     WNOT1         IFEQ      *BLANKS
103800090216     C                   MOVEL     D05(X)        WNOT1
103900090216     C                   MOVE      D05(X)        WNOT2
104000090216     C                   ELSE
104100090216     C     WNOT2         IFEQ      *BLANKS
104200090216     C                   MOVEL     D05(X)        WNOT2
104300090216     C                   END
104400090216     C                   END
104500090216     C                   END
104600090216     C                   END
104700160524      ***
104800160525     c                   end
104900160524      ***
105000160524      ***   x Alert EMAIL o SMS
105100160524      ***
105200160524     C*------>>
105300160524      * ? solo se si tratta di note x invio mail di avviso al destinatario
105400160524      *    (nuovo servizio a pagamento attivato nel 2016)
105500160524      *     dovranno essere scritti 2 records nel VAT di tipo I e J
105600160524     c                   if         ftx4451  = 'EML'
105700160524      *
105800160524     c                   if        ftx4453 = 'N'
105900160524     c                   eval      FTX_MAIL_come_servizio = 'N'
106000160524     c                   end
106100160524      *
106200160524     C                   DO        4             X
106300160524      *
106400160524     C     D05(X)        IFNE      *BLANKS
106500160524      *
106600160524     C     Note_1t       IFEQ      *BLANKS
106700160524     C                   MOVEL     D05(X)        Note_1t
106800160524     C                   MOVE      D05(X)        Note_2t
106900160524     C                   ELSE
107000160524     C     Note_2t       IFEQ      *BLANKS
107100160524     C                   MOVEL     D05(X)        Note_2t
107200160524     C                   END
107300160524     C                   END
107400160524      *
107500160524     C                   ENDif
107600160524      *
107700160524     C                   ENDdo
107800160524      *
107900160524      * carica il campo da riportare poi sul VAT
108000160524     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
108100160524     c                             note_1T
108200160524     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
108300160524     c                             note_2T
108400160524     C                   end
108500160524     C*------>>
108600160524      * ? solo se si tratta di note x invio SMS di avviso al destinatario
108700160524      *    (nuovo servizio a pagamento attivato)
108800160524      *     dovrà essere scritto 1 record nel VAT di tipo S
108900160524     c                   if         ftx4451  = 'SMS'
109000160524     c                   if        ftx4453 = 'N'
109100160524     c                   eval      FTX_SMS_come_servizio = 'N'
109200160524     c                   end
109300160524      *
109400160524     C                   DO        4             X
109500160524      *
109600160524     C     D05(X)        IFNE      *BLANKS
109700160524      *
109800160524     C     Note_1sms     IFEQ      *BLANKS
109900160524     C                   MOVEL     D05(X)        Note_1sms
110000160524     C                   MOVE      D05(X)        Note_2sms
110100160524     C                   ELSE
110200160524     C     Note_2sms     IFEQ      *BLANKS
110300160524     C                   MOVEL     D05(X)        Note_2sms
110400160524     C                   END
110500160524     C                   END
110600160524      *
110700160524     C                   ENDif
110800160524      *
110900160524     C                   ENDdo
111000160524      *
111100160524      * carica il campo da riportare poi sul VAT
111200160524     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
111300160524     c                             note_1sms
111400160524     C                   end
111500090216     C*------>>
111600090216     C*  Decodifico tipo pagamento C/Assegno
111700090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
111800090216      *      (Problema di pulizia in presenza di + righe di testo)
111900090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
112000090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
112100090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
112200090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
112300090216     C                   if        wri_vabtic = *blank
112400090216     C                   MOVEL     *BLANKS       VABTIC
112500090216     c                   end
112600090216      **
112700090216     c                   clear                   trovato_TPI       1
112800090216      *
112900090216      * ?Se c'è il tipo Incasso
113000090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
113100090216      *
113200090216     C                   DO        4             X                              -- 02 --
113300090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
113400130311      **
113500130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
113600130311      **  per gestire Partner come AZKAR che ha codice "AAA"
113700130311     C*******            MOVEL     D05(X)        WCTC              2
113800130311     C                   MOVEL     D05(X)        WCTC             10
113900090216     C                   Z-ADD     1             Y
114000090216     c                   movel(p)  Wctc          fld35a           35
114100090216     c                   move      §puTC         fld35a
114200090216     C     fld35a        LOOKUP    CTC35(Y)                               33
114300090216     C   33              MOVEL     TPI(Y)        VABTIC
114400090216     c   33              move      'S'           trovato_TPI
114500090216     C                   END                                                    -- 03 --
114600090216     C                   ENDdo                                                  -- 02 --
114700090216      *
114800090216     C                   ELSE                                                   -- 01 --
114900090216      *
115000090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
115100090216      *    Deve essere impostato comunque da testata
115200090216     c                   if        vabtic = *blank
115300090216     C                   MOVEL     wVabTIC       VABTIC
115400090216     c                   end
115500090216      *
115600090216     C                   END                                                    -- 01 --
115700090216      *
115800090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
115900090216      *    essere presenti altri record Flat con altre informazioni
116000090216      *    che potrebbero abblancare il Tipo incasso precedentemente
116100090216      *    decodificato
116200090216     c                   if        Trovato_TPI = 'S' and
116300090216     c                                 wri_vabtic = *blank
116400090216     c                   eval      wri_vabtic = 'S'
116500090216     c                   end
116600090213      *
116700090213     c     end_FTX       endSR
116800090213      * ?================================================================== */
116900090213     C*?  Anagrafica del mittente e del destinatario
117000090213      * ?================================================================== */
117100090213     C     DATI_NAD      BEGSR
117200090213      *
117300100730     c                   clear                   dopo_NAD_CN       1
117400090210      * Dati destinatario
117500090210     C                   IF        nad3035 = 'CN' or
117600090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
117700100730     c                   move      'S'           dopo_NAD_CN
117800090227      *
117900090227      * Se trovato un indirizzo di destinatario
118000090227     c                   eval       se_trovato_NAD ='S'
118100090210      *
118200090210     C* Reperisco nazione corrispondente destinatario
118300090210     C                   Z-ADD     1             YY                3 0
118400090210     c                   eval      *in32 = *off
118500090210     C                   if        nad3207 <> *BLANKS
118600090210     C     nad3207       LOOKUP    ISO(YY)                                32
118700090210     C                   endif
118800090210      *
118900100629     C* Imposto dati destinatario  dal PARTY NAME
119000100629     C                   MOVEL     nad30361      VABRSD
119100100629     C                   if        nad30362 <> *LOVAL
119200100629     C                   MOVEL     nad30362      VABRD2
119300090210     C                   endif
119400090211      *
119500090211      *
119600090210     C                   MOVEL     nad30421      VABIND
119700090210     C                   MOVEL     nad3164       VABLOD
119800090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
119900090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
120000090210      *
120100090210     C* Verifico se c'è dello spazio per nella località
120200090210     C     '    '        SCAN      VABLOD        WI                4 0
120300090210      *
120400090210     C                   MOVEA     VABLOD        LOD
120500090210     C                   ADD       1             WI
120600090210     C                   MOVEA     NAD30422      LOD(WI)
120700090210     C                   MOVEA     LOD           VABLOD
120800090210     C                   ENDif
120900090210      *
121000090210     C                   If        *in32 = *on
121100090210     C                   MOVEL     C15(YY)       VABNZD
121200090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
121300090210     c                   Else
121400090210     C                   MOVEL     *BLANKS       VABNZD
121500090210     C                   Z-ADD     0             WFLCPF
121600090210     c                   Endif
121700090210      * Converto CAP
121800090210     C                   EXSR      CNVCAP
121900090210     C                   ENDIF
122000090210      *
122100090210      * Dati mittente
122200090210     C                   IF        nad3035 = 'CZ'
122300090210     C                   movel     nad30361      VABRMO
122400090210      *
122500090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
122600090210     C                   movel     §PTNAZ        VABNMO
122700090210      *
122800090210     C                   IF        nad3207 <> *blanks
122900090210     C                   eval      YY = 1
123000090210     C     nad3207       LOOKUP    ISO(YY)                                32
123100090210     C                   if         *in32 = *on
123200090210     C                   MOVEL     C15(YY)       VABNMO
123300090210     C                   endif
123400090210     C                   ENDIF
123500090210      *
123600090210      * Normalizzo CAP mittente originale
123700090210     C                   clear                   T
123800090210     C                   clear                   S
123900090210     C                   MOVEA     *BLANKS       CAPM
124000090210     C                   MOVEA     nad3251       CAP9
124100090210     C                   DO        9             T
124200090210     C     CAP9(T)       IFNE      *BLANKS
124300090210     C                   ADD       1             S
124400090210     C                   MOVE      CAP9(T)       CAPM(S)
124500090210     C                   ENDIF
124600090210     C                   ENDDO
124700090210     C*
124800090210      *     Controllo validità CAP
124900090210     C                   MOVEA     CAPM          VABCMO
125000090210      *
125100090210     C                   CLEAR                   TISI95DS
125200090210     C                   MOVEL     VABCMO        I95CAP
125300090210     C                   MOVEL     VABNMO        I95NAR
125400090210     C                   MOVEL     WOGGI         I95DAT
125500090210     C                   MOVEL     '3'           I95TCN
125600090210     C                   CALL      'TISI95R'
125700090210     C                   PARM                    TISI95DS
125800090210      *     Errore
125900090210     C     O95ERR        IFNE      *BLANKS
126000090210     C                   MOVEL     INDCAE        VABCMO
126100090210     C                   MOVEL     §PTNAZ        VABNMO
126200090210     C                   END
126300090210      *
126400090210     C                   END
126500090210      *
126600090210      * NOn c'è l'identificativo bolla del cliente
126700090210     C                   if          vabRSD = *blank or
126800090210     C                               vabIND = *blank or
126900090210     C                               vabLOD = *blank
127000100629     C                   IF        nad3035 = 'CN'  or
127100100629     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
127200090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
127300141104      * solo segnalazione
127400141104     c************       exsr      Error_DET
127500141104     c************       goto      end_NAD
127600090210     C                   ENDIF
127700090211     C                   ENDIF
127800090210      *
127900090210     c     end_NAD       endSR
128000090210      * ?================================================================== */
128100090210     C*?  Contatto a cui far riferimento
128200090210      * ?================================================================== */
128300090210     C     DATI_CTA      BEGSR
128400090210      *
128500090210      *  memorizza il riferimento di consegna x il destinatario
128600090210      *  memorizza il riferimento di consegna nr.telefonico
128700090210     C     cta3412       ifne      *BLANKS
128800100730     c     dopo_NAD_CN   andeq     'S'
128900090210     c                   movel     cta3412       watrde
129000090210     c                   end
129100090210      *
129200090210     c                   endSR
129300090210      * ?================================================================== */
129400090210     C*?  Contatto telefonico  a cui far riferimento
129500090210      * ?================================================================== */
129600090210     C     DATI_COM      BEGSR
129700090210      *
129800090210     C     com3148       ifne      *BLANKS
129900100730     C     COM3155       andeq     'TE'
130000100730     c     dopo_NAD_CN   andeq     'S'
130100090210     c                   movel     com3148       wattel
130200090210     c                   end
130300090210      *
130400090209     C                   ENDSR
130500090210      * ?================================================================== */
130600090210     C*?  Dati di dettaglio della spedizione e dei colli
130700090210      * ?================================================================== */
130800090210     C     DATI_GID      BEGSR
130900090210      *
131000090210     C* Se ho totali per spedizione metto in campi VAB
131100090210     C     gid1496       IFEQ      99999
131200090210     C     gid1496       orEQ      9999
131300090210     C                   MOVEL     'S'           WGID99
131400090210     C                   z-add     gid722420     VABNCL
131500090210     C                   ELSE
131600090210     C     WGID99        IFEQ      'N'
131700090210     C                   z-add     gid722420     WTNCL             5 0
131800090210     C                   ADD       WTNCL         VABNCL
131900090210     C                   END
132000090210     C                   END
132100090212      *
132200090210     C                   ADD       VABNCL        WTOCOC           16 0
132300090210      *
132400090210     c     end_GID       endSR
132500090210      * ?================================================================== */
132600090210     C*?  Dati di dettaglio misure spedizione e colli
132700090210      * ?================================================================== */
132800090210     C     DATI_MEA      BEGSR
132900090210      *
133000090210     C*  Peso
133100100629     C     mea6311       IFEQ      'AAE'
133200090210     C     mea6411       ANDEQ     'KGM'
133300090210      *
133400090210     C*  Controllo se ho valore totale o parziale
133500090210      *   sempre il Lordo
133600100629     C     mea6313       IFEQ      'G  '
133700090210     C     WS1496        IFEQ      99999
133800090210     C     WS1496        orEQ      9999
133900090211     C                   move      mea6314       mea6314_2        18 2          Peso
134000090211     C                   Z-ADD     mea6314_2     VABPKB                         Peso
134100090210     C                   ELSE
134200090210     C     WGID99        IFEQ      'N'
134300090211     C                   move      mea6314       mea6314_2                      Peso
134400090211     C                   ADD       mea6314_2     VABPKB                         Peso
134500090210     C                   END
134600090210     C                   END
134700090210     C                   END
134800090210      *
134900100629     C     mea6313       IFEQ      'G  '
135000090211     C                   move      mea6314       mea6314_2                      Peso
135100090211     C                   ADD       mea6314_2     WTOPLC
135200090210     C                   END
135300100629     C     mea6313       IFEQ      'N  '
135400090211     C                   move      mea6314       mea6314_2                      Peso
135500090211     C                   ADD       mea6314_2     WTOPNC
135600090210     C                   END
135700090210     C                   END
135800090210      ***
135900090210     C*  Peso in Grammi  se abilitato a utilizzarlo
136000090210      ***
136100090210     C     §puGR         IFeq      'S'
136200100629     C     mea6311       IFEQ      'AAE'
136300090210     C     mea6411       ANDEQ     '163'
136400090210      *
136500090210     C*  Controllo se ho valore totale o parziale
136600100629     C     mea6313       IFEQ      'G  '
136700090210     C     WS1496        IFEQ      99999
136800090210     C     WS1496        orEQ      9999
136900090211     c                   move      mea6314       mea6314_2
137000090211     C     mea6314_2     div       1000          VABPKB                         Peso
137100090210     C                   ELSE
137200090210     C     WGID99        IFEQ      'N'
137300090211     c                   move      mea6314       mea6314_2
137400090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
137500090210     C                   add       Peso_inKG     VABPKB                         Peso
137600090210     C                   END
137700090210     C                   END
137800090210     C                   END
137900090210      *
138000100629     C     mea6313       IFEQ      'G  '
138100090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
138200090210     C                   ADD       Peso_inKG     WTOPLC
138300090210     C                   END
138400100629     C     mea6313       IFEQ      'N  '
138500090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
138600090210     C                   ADD       Peso_inKG     WTOPNC
138700090210     C                   END
138800090210     C                   END
138900090210     C                   END
139000090210      *  Volume
139100100629     C     mea6311       IFEQ      'AAE'
139200090210     C     mea6411       ANDEQ     'MTQ'
139300090210      *  Controllo se ho valore totale o parziale
139400090210     C     WS1496        IFEQ      99999
139500090210     C     WS1496        orEQ      9999
139600091102     C                   move      mea6314       mea6314_2                      Volume
139700091102     C                   Z-ADD     mea6314_2     VABVLB                         Volume
139800091102     C                   Z-ADD     mea6314_2     PVLB
139900090210     C                   ELSE
140000090210     C     WGID99        IFEQ      'N'
140100091102     C                   move      mea6314       mea6314_2                      Volume
140200091102     C                   ADD       mea6314_2     VABVLB                         Volume
140300091102     C                   ADD       mea6314_2     PVLB
140400090210     C                   END
140500090210     C                   END
140600090210     C                   END
140700090210      *
140800090210     C* Ricerco CAP
140900090210     C     WCAP          IFNE      *BLANKS
141000100629     C     mea6311       IFEQ      'AAE'
141100090210      *
141200090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
141300090210     C                   CLEAR                   FNLV55DS
141400090210     C                   MOVEL     'P'           D55TPT
141500090210     C                   MOVEL     WOGGI         D55DRF
141600090210     C                   MOVEL     VABLNP        D55LNP
141700090210     C                   MOVEL     VABLNP        D55LIN
141800090210     C                   CALL      'FNLV55R'
141900090210     C                   PARM                    FNLV55DS
142000090210     C                   MOVE      D55TFP        COMTFP            3 0
142100090210      *
142200090210     C*  PASSO IL TERMINAL PARTENZA
142300090210     C                   CLEAR                   TISI95DS
142400090210     C                   Z-ADD     COMTFP        I95TFP
142500090210     C                   MOVEL     VABTSP        I95TSP
142600090210     C                   MOVEL     'S'           I95FRE
142700090210     C                   MOVEL     '3'           I95TCN
142800090210     C                   MOVEL     WCAP          I95CAP
142900090210     C                   MOVEL     VABNZD        I95NAR
143000090210     C                   MOVEL     VABLOD        I95LOC
143100090210     C                   MOVEL     VABIND        I95IND
143200090210      *
143300090210     C* Se gestita seconda linea di arrivo passo peso - volume
143400090210     C* e numero colli effettivo
143500090210     C     §PULNA        IFEQ      'S'
143600090210     C                   Z-ADD     VABPKB        I95LKG
143700090210     C                   Z-ADD     VABVLB        I95LMC
143800090210     C                   ELSE
143900090210     C* Altrmenti passo 1 Kg e 0,001
144000090210     C***                  Z-ADD1         I95LKG
144100090210     C***                  Z-ADD0,001     I95LMC
144200090210     C                   END
144300090210     C*
144400090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
144500090210     C                   SELECT
144600090210     C     VABCBO        WHENEQ    '1 '
144700090210     C                   MOVEL     *BLANKS       I95FCA
144800090210     C                   MOVEL     'F'           I95TPO
144900090210     C     VABCBO        WHENEQ    '2 '
145000090210     C                   MOVEL     *BLANKS       I95FCA
145100090210     C                   MOVEL     'A'           I95TPO
145200090210     C     VABCBO        WHENEQ    '4 '
145300090210     C                   MOVEL     'S'           I95FCA
145400090210     C                   MOVEL     'F'           I95TPO
145500090210     C     VABCBO        WHENEQ    '6 '
145600090210     C                   MOVEL     'S'           I95FCA
145700090210     C                   MOVEL     'A'           I95TPO
145800090210     C                   OTHER
145900090210     C                   MOVEL     'F'           I95TPO
146000090210     C     VABCAS        IFGT      0
146100090210     C                   MOVEL     'S'           I95FCA
146200090210     C                   ELSE
146300090210     C                   MOVEL     *BLANKS       I95FCA
146400090210     C                   END
146500090210     C                   ENDSL
146600090210     C*
146700090210     C                   CALL      'TISI95R'
146800090210     C                   PARM                    TISI95DS
146900090210     C* Reperisco i dati da CAP
147000090210     C                   MOVEL     O95PRV        VABPRD
147100090210     C                   MOVEL     O95LNA        VABLNA
147200090210     C                   MOVEL     O95ZNC        VABZNC
147300090210     C                   MOVEL     WCAP          VABCAD
147400090210     C*
147500090210     C     O95ERR        IFNE      *BLANKS
147600090210     C     O95FIT        ANDEQ     'S'
147700090210     C*  Cap non trovato
147800110623     C******             eval      vinMSG = 'MEA (CAP) non trovato'
147900110623     c******             exsr      Error_DET
148000110623     C******             goto      end_MEA
148100090210     C                   END
148200090210     C                   END
148300090210      *
148400090210     C                   ELSE
148500090210      *
148600090210     C                   CLEAR                   VABPRD
148700090210     C                   CLEAR                   VABLNA
148800090210     C                   CLEAR                   VABZNC
148900090210     C                   CLEAR                   VABCAD
149000090210     C*  Cap non trovato
149100110623     C******             eval      vinMSG = 'MEA (CAP) mancante'
149200110623     c******             exsr      Error_DET
149300110623     C******             goto      end_MEA
149400090210     C                   END
149500090210      *
149600090210     c     end_MEA       endSR
149700090210      * ?================================================================== */
149800090210     C*?  Dati di dettaglio Codice a barre Segnacolli
149900090210      * ?================================================================== */
150000090210     C     DATI_PCI      BEGSR
150100090210      *
150200090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
150300090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
150400090210     C     §1BF12        IFEQ      'E'
150500090210     C                   EXSR      SEGNEE
150600090210     C                   ELSE
150700090210      *
150800090210      *  La prima volta controllo congruità numero segnacollo
150900090210     C     §PUBS1        IFNE      *BLANKS
151000090210     C     VABnrs        ANDNE     0
151100090210      *  Se il codice trattamento merce prevede che segnacollino i
151200090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
151300090210     C     §1BFG7        IFEQ      'S'
151400090210     C     §1BFG1        OREQ      'S'
151500090210      *  calcolo segnacollo
151600090210     C                   EXSR      SGNELA
151700090210     C                   END
151800090210     C                   END
151900090210      *
152000090210     C                   END
152100090210      *
152200090210     c     end_PCI       endSR
152300090211     C*----------------------------------------------------------------
152400090211      *? dati finali del dettaglio    UNT
152500090211     C*----------------------------------------------------------------
152600090211     C     DATI_UNT      BEGSR
152700090211      *
152800090213     c                   eval      NUM_RECord = vnREC
152900090213     c                   z-add     vnrec         last_RECord
153000090211      **
153100090211     c                   endSR
153200090211     C*----------------------------------------------------------------
153300090211      *? dati finali     UNT
153400090211     C*----------------------------------------------------------------
153500090211     C     DATI_UNZ      BEGSR
153600090211      *
153700090211      **
153800090211     c                   endSR
153900090210      *----------------------------------------------------------------
154000090210      *? Input segnacolli su schiera di 20 elementi
154100090210      *----------------------------------------------------------------
154200090210     C     INP_Segnacol  BEGSR
154300090210      *
154400090210     c                   clear                   quanti_PCI        3 0
154500090210     c                   if        §puEL1 = 0
154600090210     c                   z-add     10            quanti_PCI
154700090210     c                   else
154800090210     c                   z-add     §puEL1        quanti_PCI
154900090210     c                   end
155000090210      *
155100090210      *  Pulisce la schiera dei segnacolli da elaborare
155200090210     c                   clear                   x30
155300090210     c                   z-add     0             Nr_x30            3 0
155400090210      *
155500090210      *   riporto tutto sulla schiera generica X30
155600090210      *
155700090210     c                   do        10            limite            3 0
155800090210      * Limita quanti elementi passati in ogni PCI
155900090210      *   devono essere considerati
156000090210     c                   if        limite > quanti_PCI
156100090210     c                   Leave
156200090210     c                   end
156300090210      *
156400090210      *  Carica la schiera generale di tutti i segnacolli
156500090210     c                   if        D30_1(limite) <> *blank
156600090210     c                   add       1             Nr_x30
156700090210     c                   eval      X30(Nr_x30) = D30_1(limite)
156800090210     c                   end
156900090210     c                   enddo
157000090210      *
157100090210     C                   ENDSR
157200090210      *----------------------------------------------------------------
157300090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
157400090210      *----------------------------------------------------------------
157500090210     C     SEGNEE        BEGSR
157600090210      *
157700090210     c                   exsr      INP_Segnacol
157800090210      *
157900090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
158000090210     C                   DO        10            x                              --- 01 ---
158100090210      *
158200090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
158300090210     C     x30(X)        ANDNE     *LOVAL
158400090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
158500090210     C                   ADD       1             §§
158600160623      *
158700160623      *  se il BARCODE =31  tolgo i primi (5) a sinistra
158800160623     c                   if        %len(%trim(x30(X))) = 31
158900160624     c                   eval      x30(X) = %subst(x30(X):6:26)
159000160623     c                   end
159100160623      *
159200090210     C                   MOVEL     x30(X)        SGE(§§)
159300090210     C                   END                                                    --- 03 ---
159400090210      *
159500090210     C                   END                                                    --- 02 ---
159600090210      *
159700090210     C                   ENDSR
159800090210     C*----------------------------------------------------------------
159900090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
160000090210     C*----------------------------------------------------------------
160100090210     C     SGNELA        BEGSR
160200090210      *
160300090210     c                   exsr      INP_Segnacol
160400090210     C*
160500090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
160600090210     C*  in base al numero elementi validi carico sgn.
160700090210     C                   DO        10            X                              --- 01 ---
160800090210     C*
160900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
161000090210     C     x30(X)        ANDNE     *LOVAL
161100090210     C*
161200090210     C                   CLEAR                   DSSGN
161300090210     C*  Controllo se devo ricevere la linea di partenza
161400090210     C                   MOVEL     'N'           WERRO             1
161500090210     C     §PULP1        IFGT      0                                            --- 03 ---
161600090210     C                   EXSR      REPLNP
161700090210     C                   END                                                    --- 03 ---
161800090210     C*  Controllo se devo ricevere la linea di arrivo
161900090210     C     §PULA1        IFGT      0                                            --- 03 ---
162000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
162100090210     C                   EXSR      REPLNA
162200090210     C                   END                                                    --- 03 ---
162300090210     C*  Controllo se devo ricevere il numero di serie
162400090210     C     §PUNR1        IFGT      0                                            --- 03 ---
162500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
162600090210     C                   EXSR      REPNRS
162700090210     C                   END                                                    --- 03 ---
162800090210     C*  Controllo se devo ricevere il numero segnacollo
162900090210     C     §PUSG1        IFGT      0                                            --- 03 ---
163000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
163100090210     C                   EXSR      REPSGN
163200090210     C                   END                                                    --- 03 ---
163300090210     C*  Controllo se devo ricevere la zona di consegna
163400090210     C     §PUZN1        IFGT      0                                            --- 03 ---
163500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
163600090210     C                   EXSR      REPZNC
163700090210     C                   END                                                    --- 03 ---
163800090210     C*  Imposto da VAB i dati a zero se mi è stato passato
163900090210     C*  numero segnacollo valido
164000090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
164100090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
164200090210     C                   Z-ADD     VABLNP        SGNLNP
164300090210     C                   END                                                    --- 04 ---
164400090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
164500090210     C                   Z-ADD     VABLNA        SGNLNA
164600090210     C                   END                                                    --- 04 ---
164700090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
164800090210     C                   Z-ADD     VABNRS        SGNNRS
164900090210     C                   END                                                    --- 04 ---
165000090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
165100090210     C                   Z-ADD     VABZNC        SGNZNC
165200090210     C                   END                                                    --- 04 ---
165300090210      *
165400090210     C*  mi reimposto LNA e zona da segnacollo
165500090210     C                   Z-ADD     SGNLNA        VABLNA
165600090210     C                   Z-ADD     SGNZNC        VABZNC
165700090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
165800090210     C                   ADD       1             XY
165900090210     C                   MOVE      SGNNSC        SGN(XY)
166000090210     C                   END                                                    --- 03 ---
166100090210     C*
166200090210     C                   END                                                    --- 02 ---
166300090210     C*
166400090210     C                   END                                                    --- 01 ---
166500090210      *
166600090210     C                   ENDSR
166700090210     C*----------------------------------------------------------------
166800090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
166900090210     C*----------------------------------------------------------------
167000090210     C     REPLNP        BEGSR
167100090210     C*
167200090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
167300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
167400090210     C*  dal numero segnacollo passatomi dal partner
167500090210     C                   Z-ADD     §PULP1        WP                2 0
167600090210     C     3             SUBST     x30(X):WP     WLNP              3
167700090210     C*
167800090210     C*  controllo che la lnp calcolata sia numerica
167900090210     C                   TESTN                   WLNP                 98
168000090210     C*
168100090210     C*  se è numerica la carico
168200090210     C     *IN98         IFEQ      '1'
168300090210     C                   MOVE      WLNP          SGNLNP
168400090210     C                   ELSE
168500090210     C*  se non è numerica stampo errore
168600090210     C                   MOVEL     'S'           WERRO             1
168700090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
168800090213     c                   exsr      Error_DET
168900090210     C                   goto      end_LNP
169000090210     C                   END
169100090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
169200090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
169300090210     C     SGNLNP        IFNE      VABLNP
169400090210     C                   MOVEL     'S'           WERRO             1
169500090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
169600090210     c                             a della Bolla'
169700090213     c                   exsr      Error_DET
169800090210     C                   goto      end_LNP
169900090210     C                   END
170000090210     C*
170100090210     C     end_LNP       ENDSR
170200090210     C*----------------------------------------------------------------
170300090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
170400090210     C*----------------------------------------------------------------
170500090210     C     REPLNA        BEGSR
170600090210     C*
170700090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
170800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
170900090210     C*  dal numero segnacollo passatomi dal partner
171000090210     C                   Z-ADD     §PULA1        WP                2 0
171100090210     C     3             SUBST     x30(X):WP     WLNA              3
171200090210     C*  controllo che la lna calcolata sia numerica
171300090210     C                   TESTN                   WLNA                 98
171400090210     C*  se è numerica la carico
171500090210     C     *IN98         IFEQ      '1'
171600090210     C                   MOVE      WLNA          SGNLNA
171700090210     C                   ELSE
171800090210     C*  se non è numerica stampo errore
171900090210     C                   MOVEL     'S'           WERRO             1
172000090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
172100090213     c                   exsr      Error_DET
172200090210     C                   goto      end_LNA
172300090210     C                   END
172400090210     C*
172500090210     C     end_LNA       ENDSR
172600090210     C*----------------------------------------------------------------
172700090210     C*? REPNRS - Reperisce numero serie
172800090210     C*----------------------------------------------------------------
172900090210     C     REPNRS        BEGSR
173000090210     C*
173100090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
173200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
173300090210     C*  dal numero segnacollo passatomi dal partner
173400090210     C                   Z-ADD     §PUNR1        WP                2 0
173500090210     C     2             SUBST     x30(X):WP     WNRS              2
173600090210     C*  controllo che il nrs calcolata sia numerico
173700090210     C                   TESTN                   WNRS                 98
173800090210     C*  se è numerica la carico
173900090210     C     *IN98         IFEQ      '1'
174000090210     C                   MOVE      WNRS          SGNNRS
174100090210     C                   ELSE
174200090210     C*  se non è numerico stampo errore
174300090210     C                   MOVEL     'S'           WERRO             1
174400090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
174500090213     c                   exsr      Error_DET
174600090210     C                   goto      end_NRS
174700090210     C                   END
174800090210      *
174900090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
175000090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
175100090210     C     SGNNRS        IFNE      VABNRS
175200090210     C                   MOVEL     'S'           WERRO             1
175300090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
175400090210     c                             o della Bolla'
175500090213     c                   exsr      Error_DET
175600090210     C                   goto      end_NRS
175700090210     C                   END
175800090210     C*
175900090210     C     end_NRS       ENDSR
176000090210     C*----------------------------------------------------------------
176100090210     C*? REPZNC - Reperisce zona consegna
176200090210     C*----------------------------------------------------------------
176300090210     C     REPZNC        BEGSR
176400090210     C*
176500090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
176600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
176700090210     C*  dal numero segnacollo passatomi dal partner
176800090210     C                   Z-ADD     §PUZN1        WP                2 0
176900090210     C     2             SUBST     x30(X):WP     WZNC              2
177000090210     C*  controllo che la zona calcolata sia numerica
177100090210     C                   TESTN                   WZNC                 98
177200090210     C*  se è numerica la carico
177300090210     C     *IN98         IFEQ      '1'
177400090210     C                   MOVE      WZNC          SGNZNC
177500090210     C                   ELSE
177600090210     C*  se non è numerica stampo errore
177700090210     C                   MOVEL     'S'           WERRO             1
177800090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
177900090213     c                   exsr      Error_DET
178000090210     C                   goto      end_ZNC
178100090210     C                   END
178200090210     C*
178300090210     C     end_ZNC       ENDSR
178400090210     C*----------------------------------------------------------------
178500090210     C*? REPSGN - Reperisce numero segnacollo
178600090210     C*----------------------------------------------------------------
178700090210     C     REPSGN        BEGSR
178800090210     C*
178900090210     C*  se viene passata nr.segnacollo estraggo sottostringa
179000090210     C*  lunga 7 chr (a partire dalla posizione iniziale
179100090210     C*  indicata in tabella)
179200090210     C*  dal numero segnacollo passatomi dal partner
179300090210     C                   Z-ADD     §PUSG1        WP                2 0
179400090210     C     7             SUBST     x30(X):WP     WSGN              7
179500090210     C*  controllo che il nr.segnacollo calcolato sia numerico
179600090210     C                   TESTN                   WSGN                 98
179700090210     C*  se è numerico lo carico
179800090210     C     *IN98         IFEQ      '1'
179900090210     C                   MOVE      WSGN          SGNNSC
180000090210     C                   ELSE
180100090210     C*  se non è numerica stampo errore
180200090210     C                   MOVEL     'S'           WERRO             1
180300090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
180400090213     c                   exsr      Error_DET
180500090210     C                   goto      end_SGN
180600090210     C                   END
180700090210     C*
180800090210     C     end_SGN       ENDSR
180900971215      *----------------------------------------------------------------
181000971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
181100971215      *----------------------------------------------------------------
181200051205      *----------------------------------------------------*
181300051205      *   DEFINIZIONE CHIAVI                               *
181400051205      *----------------------------------------------------*
181500051205     C     *INZSR        BEGSR
181600051205      *
181700991129     c     *ENTRY        PLIST
181800060613     C                   parm                    esito             1
181900971215      *
182000050112     C     KTABel        KLIST
182100050112     C                   KFLD                    TBLKUT
182200050112     C                   KFLD                    TBLCOD
182300050112     C                   KFLD                    TBLKEY
182400090210      *
182500090210     C     K_TAB1L       KLIST
182600090213     C                   KFLD                    etbUNB
182700090213     C                   KFLD                    etbSGM
182800090213     C                   KFLD                    etbVALSGM
182900090209     C*
183000090209     C* Definisco chiavi di accesso
183100090209     C     KTAB1         KLIST
183200090209     C                   KFLD                    KKUT
183300090209     C                   KFLD                    KCOD
183400090209     C*
183500090209     C     KNUF          KLIST
183600090209     C                   KFLD                    KAAA
183700090209     C                   KFLD                    KCNU
183800090209     C                   KFLD                    KFIL
183900090209      *
184000090209     C     KVAB1         KLIST
184100090209     C                   KFLD                    KCCM
184200090209     C                   KFLD                    KCMR
184300090209     C                   KFLD                    KCNT
184400090209     C                   KFLD                    KAAS
184500090209     C                   KFLD                    KLNP
184600090209     C                   KFLD                    KNRS
184700090209     C                   KFLD                    KNSP
184800090209      *
184900090209     C     KVAB2         KLIST
185000090209     C                   KFLD                    KCCM
185100090209     C                   KFLD                    KCMR
185200090209      *
185300090209     C     KRDE          KLIST
185400090209     C                   KFLD                    KAAS
185500090209     C                   KFLD                    KLNP1
185600090209     C                   KFLD                    KNRS
185700090209     C                   KFLD                    KNSP
185800090209     C                   KFLD                    KNMC
185900090209     C                   KFLD                    KNRP
186000090209      *
186100090209     C     KACO          KLIST
186200090209     C                   KFLD                    KKUT
186300090209     C                   KFLD                    KKCC
186400090209     C                   KFLD                    KKSC
186500090209      *
186600090209     C     KIND          KLIST
186700090209     C                   KFLD                    KKUT
186800090209     C                   KFLD                    KKCC
186900090209     C                   KFLD                    KKSC
187000090209      *
187100090209      * DETERMINO SE SONO BARTOLINI O SDI
187200090209     C                   CLEAR                   TIBS55DS
187300090209     C                   MOVEL     'L'           I50TLA
187400090209     C                   CALL      'TIBS55R'
187500090209     C                   PARM                    TIBS55DS
187600090209      *
187700090209     C* RECUPERO DATI DELL'UTENTE
187800090209     C                   Z-ADD     1             CODUT             1 0
187900090209     C                   CALL      'XPARUT'
188000090209     C                   PARM                    UTEDSE0F
188100090209     C                   MOVEL     RAGUT         RSUT             20
188200090209     C*
188300090209     C* RICERCA CAPOCONTI
188400090209     C                   DO        50            X
188500090209     C                   MOVE      TCU(X)        TCUDS
188600090209     C     F56           IFEQ      'CG'
188700090209     C     F34           ANDEQ     '01'
188800090209     C                   Z-ADD     KCU(X)        KCI               4 0
188900090209     C                   END
189000090209     C                   END
189100090209     C*
189200090211     c                   movel(p)  'EDPAB'       User_Msg         10
189300090211     C*
189400090209     C* IMPOSTO A ZERO VARIABILI DI PGM
189500090209     c                   move      *blank        Snd_Msg_alert     1
189600090209     C                   MOVEL     CA(1)         WCA              78
189700090209     C                   MOVEL     CN(1)         WCN              78
189800090209     C*
189900090209     C* RECUPERO DATA E ORA
190000090209     C                   TIME                    WHHDAT           14 0
190100090209     C                   MOVE      WHHDAT        G02DAT
190200090209     C                   MOVE      WHHDAT        WDATA8            8 0
190300090209     C                   MOVE      WDATA8        WAA2              2 0
190400090209     C                   MOVEL     WDATA8        DATA              6 0
190500090209     C                   MOVE      WAA2          DATA
190600090209     C                   MOVEL     *BLANK        G02ERR
190700090209     C                   CALL      'XSRDA8'
190800090209     C                   PARM                    WLBDA8
190900090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
191000090209     C                   TIME                    WORA              6 0
191100090209      * Recupero data e ora
191200090209     C                   TIME                    W0140
191300090209      * UDATE IN GGMMAAAA
191400090209     C                   MOVE      W0140         WDTGIO
191500090209      * UDATE IN AAAAMMGG
191600090209     C     *eur          MOVEL     WDTGIO        DATA_eur
191700090209     C     *iso          MOVEL     DATA_eur      dateu
191800090209      *
191900090209      * ?              /*--------------------------- */
192000090209     c                   exsr      CARICA_TABELLE
192100090209      * ?              /*--------------------------- */
192200090209      *
192300090209      *
192400090209     C                   MOVEL     *ALL'-'       CMP132          132
192500090209     C                   CLEAR                   EDiVAB00
192600090211     C                   clear                   vablnp_PT
192700090212     C                   clear                   vabnrs_PT
192800090211     C                   clear                   vabctm_PT
192900090211     C                   clear                   VABfnsp_PT
193000090209     C                   MOVEA     *HIVAL        SGN
193100991124      *
193200050414      *------------------
193300991124     C                   ENDSR
193400060621      * ?------------------------------------------------------------------ */
193500090209      *?    CARICA TABELLE SU SCHIERE
193600060621      * ?------------------------------------------------------------------ */
193700090209     C     CARICA_TABELLEBEGSR
193800090205      *
193900090209     C* Caricamento Tabella 1B
194000090209     C                   Z-ADD     0             X                 4 0
194100090209     C                   Z-ADD     CODUT         KKUT
194200090209     C                   MOVEL     '1B'          KCOD
194300090209     C     KTAB1         CHAIN     TABEL00F                           30
194400090209     C     *IN30         DOWEQ     '0'
194500090209     C     TBLFLG        IFEQ      *BLANKS
194600090209     C                   ADD       1             X
194700090209     C                   MOVEL     TBLKEY        CTM(X)
194800090209     C                   MOVEL     TBLUNI        DTM(X)
194900090209     C                   END
195000090209     C     KTAB1         READE     TABEL00F                               30
195100090209     C                   END
195200090209      *
195300090209     C* Caricamento Tabella 15
195400090209     C                   Z-ADD     0             X                 4 0
195500090209     C                   Z-ADD     CODUT         KKUT
195600090209     C                   MOVEL     '15'          KCOD
195700090209     C     KTAB1         CHAIN     TABEL00F                           30
195800090209     C     *IN30         DOWEQ     '0'
195900090209     C     TBLFLG        IFEQ      *BLANKS
196000090209     C                   ADD       1             X
196100090209     C                   MOVEL     TBLKEY        C15(X)
196200090209     C                   MOVEL     TBLUNI        DS15
196300090209     C                   MOVEL     §15COD        ISO(X)
196400090209     C                   MOVEL     §15ECF        CPF(X)
196500090209     C                   MOVE      §15PCF        CPF(X)
196600090209     C                   END
196700090209     C     KTAB1         READE     TABEL00F                               30
196800090209     C                   END
196900090209      *
197000090209     C* Caricamento Tabella CV
197100090209     C                   Z-ADD     0             X
197200090209     C                   Z-ADD     CODUT         KKUT
197300090209     C                   MOVEL     'CV'          KCOD
197400090209     C     KTAB1         CHAIN     TABEL00F                           30
197500090209     C     *IN30         DOWEQ     '0'
197600090209     C     TBLFLG        IFEQ      *BLANKS
197700090209     C                   ADD       1             X
197800090209     C                   MOVEL     TBLKEY        CCV(X)
197900090209     C                   MOVEL     TBLUNI        DCV(X)
198000090209     C                   END
198100090209     C     KTAB1         READE     TABEL00F                               30
198200090209     C                   END
198300090216      *
198400110328      * Caricamento Tabella termini consegna
198500110328     C                   Z-ADD     0             X
198600110328     C                   MOVEL     'TB'          TABCOD
198700110328     C     TABCOD        CHAIN     EDTAB01L                           30
198800110328     C     *IN30         DOWEQ     '0'
198900110328     C     TABFLG        IFEQ      *BLANKS
199000110328     C                   ADD       1             X
199100110328     C                   eval      K35_TpBolla(X) = TABKEY
199200110328     C                   eval      Cod_TpBolla(X) = TABKEY
199300110328     C                   eval      Decod_Porto(X) = TABUNI
199400110328     C                   END
199500110328     C     TABCOD        READE     EDTAB01L                               30
199600110328     C                   END
199700110328      *
199800090216     C* Caricamento Tabella Tipo Incasso C/Assegno
199900090216     C                   Z-ADD     0             X
200000090216     C                   MOVEL     'TC'          TABCOD
200100090216     C     TABCOD        CHAIN     EDTAB01L                           30
200200090216     C     *IN30         DOWEQ     '0'
200300090216     C     TABFLG        IFEQ      *BLANKS
200400090216     C                   ADD       1             X
200500090216     C                   MOVEL     TABKEY        CTC35(X)
200600090216     C                   MOVEL     TABUNI        TPI(X)
200700090216     C                   END
200800090216     C     TABCOD        READE     EDTAB01L                               30
200900090216     C                   END
201000090209      *
201100090209      * Caricamento Tabella Partner esteri
201200090209     C                   Z-ADD     0             X
201300090209     C                   MOVEL     'PT'          TABCOD
201400090209     C     TABCOD        CHAIN     EDTAB01L                           30
201500090209     C     *IN30         DOWEQ     '0'
201600090209      *
201700090209     C                   SELECT
201800090209     C     TABFLG        WHENNE    *BLANKS
201900090209      *
202000090209     C                   OTHER
202100090209     C                   ADD       1             X
202200090209     C                   MOVEL     TABKEY        CPT(X)
202300090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
202400090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
202500090209     C                   MOVEL     TABUNI        DPT(X)
202600090209     C                   ENDSL
202700090209      *
202800090209     C     TABCOD        READE     EDTAB01L                               30
202900090209     C                   END
203000121105      *
203100121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
203200121105     c                   exsr      ChecK_PT
203300121105      *
203400090209      * salva quanti Codici UNB ha caricato
203500090209     c                   z-add     x             sav_CPTx
203600090209      *
203700090209     C                   MOVEL     'PU'          TABCOD
203800090209     C     TABCOD        CHAIN     EDTAB01L                           30
203900090209     C     *IN30         DOWEQ     '0'
204000090209      *
204100090209     C                   SELECT
204200090209     C     TABFLG        WHENNE    *BLANKS
204300090209      *
204400090209     C                   OTHER
204500090209     C                   MOVEL     TABKEY        CODPU            35
204600090209     C                   Z-ADD     1             X
204700090209     C     CODPU         LOOKUP    CPT(X)                                 32
204800090209     C   32              MOVEL     TABUNI        DPU(X)
204900090209     C                   ENDSL
205000090209      *
205100090209     C     TABCOD        READE     EDTAB01L                               30
205200090209     C                   END
205300090209      *
205400090209      * Prosegue tab.PT e PU
205500090209     C                   MOVEL     'PV'          TABCOD
205600090209     C     TABCOD        CHAIN     EDTAB01L                           30
205700090209     C     *IN30         DOWEQ     '0'
205800090209      *
205900090209     C                   SELECT
206000090209     C     TABFLG        WHENNE    *BLANKS
206100090209      *
206200090209     C                   OTHER
206300090209     C                   MOVEL     TABKEY        CODPV            35
206400090209     C                   Z-ADD     1             X
206500090209     C     CODPV         LOOKUP    CPT(X)                                 32
206600090209     C   32              MOVEL     TABUNI        DPV(X)
206700090209     C                   ENDSL
206800090209      *
206900090209     C     TABCOD        READE     EDTAB01L                               30
207000090209     C                   END
207100090209      *
207200090209     C* Caricamento Tabella codici clienti - tariffe
207300090209     C                   Z-ADD     0             X
207400090209     C                   MOVEL     'CL'          TABCOD
207500090209     C     TABCOD        CHAIN     EDTAB01L                           30
207600090209     C     *IN30         DOWEQ     '0'
207700090209     C     TABFLG        IFEQ      *BLANKS
207800090209     C                   ADD       1             X
207900090209     C                   MOVEL     TABKEY        CCL(X)
208000090209     C                   MOVEL     TABUNI        DCL(X)
208100090209     C                   END
208200090209     C     TABCOD        READE     EDTAB01L                               30
208300090209     C                   END
208400090209      *
208500090209     C* Caricamento Serivizi speciali
208600090209     C                   Z-ADD     0             X
208700090209     C                   MOVEL     'SS'          TABCOD
208800090209     C     TABCOD        CHAIN     EDTAB01L                           30
208900090209     C     *IN30         DOWEQ     '0'
209000090209     C     TABFLG        IFEQ      *BLANKS
209100090209     C                   ADD       1             X
209200090209     C                   MOVEL     TABKEY        SS(X)
209300090209     C                   MOVEL     TABKEY        SS35(X)
209400090209     C                   MOVEL     TABUNI        DSS(X)
209500090209     C                   END
209600090209     C     TABCOD        READE     EDTAB01L                               30
209700090209     C                   END
209800090209      *
209900090209     C                   ENDSR
210000090209      * ?------------------------------------------------------------------ */
210100090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
210200090209      * ?------------------------------------------------------------------ */
210300090209     C     MSG_Errato    BEGSR
210400090209      *
210500090205      * ?  Scrive su tutti i records il tipo di errore
210600090205     c     *start        setll     tivin00r
210700090205     c                   read      tivin00r
210800090205     c                   dow       not %eof(tivin00r)
210900090211     c                   if        vinMSG  = *blank
211000090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
211100090210     C                               >> Controllare i DATI su UPLOAD !!'
211200090211     c                   end
211300090205     C                   eval      vinFLG = '2'
211400090213     c                   eval      Almeno_Un_Due ='S'
211500090205     c                   eval      esito  = '2'
211600090209     c                   eval      se_errore ='S'
211700090213     c                   eval      err_bloccante ='S'
211800090205     c                   update    tivin000
211900090205     c                   read      tivin00r
212000090205     c                   endDO
212100090205      *
212200090205     C                   ENDSR
212300090213      * ?------------------------------------------------------------------ */
212400090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
212500090213      * ?------------------------------------------------------------------ */
212600090213     C     DETta_Errato  BEGSR
212700090213      *
212800090213      *  rilascia il record prima di rieseguire il ciclo
212900090213     c                   update    tivin000
213000090213      *
213100090213      * ?  Scrive su tutti i records il tipo di errore
213200090213     c     SavNUM_RECord setll     tivin00r
213300090213     c                   read      tivin00r
213400090213     c                   dow       not %eof(tivin00r)
213500090213      *
213600090213      *
213700090213     c                   if        vinMSG  = *blank
213800090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
213900090213     C                             >> Controllare i DATI di ogni Segmento <<'
214000090213     c                   end
214100090213     c                   exsr      Error_DET
214200090213      *
214300090213     c                   if        VNREC = NUM_RECord
214400090213     c                   leave
214500090213     c                   end
214600090213      *
214700090213     c                   update    tivin000
214800090213     c                   read      tivin00r
214900090213      *
215000090213     c                   endDO
215100090601      *
215200100623     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NACEX  '
215300090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
215400100623     C                             Bolle  NACEX  non totalmente tradotto. -
215500100920     C                             Controllare UPLOAD del cliente 0022898.'
215600090601     c                   exsr      invio_mail_AB
215700090213      *
215800090213     C                   ENDSR
215900090205      * ?------------------------------------------------------------------ */
216000090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
216100090205      * ?------------------------------------------------------------------ */
216200090205     C     *pssr         BEGSR
216300090205     C
216400060710     C                   eval      esito = '2'
216500100623     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NACEX  '
216600090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
216700090601     C                             Bolle import in filiale - messaggio -
216800090601     C                             con UNB non presente in tabella PT controlla-
216900090601     C                             re EDI ricevuto su UPLOAD.'
217000090601     c                   exsr      invio_mail_AB
217100060621     C
217200090213     C                   ENDSR
217300090213      * ?------------------------------------------------------------------ */
217400090213      *?      Segnala Errore su TIVIN
217500090213      * ?------------------------------------------------------------------ */
217600090213     C     Error_DET     BEGSR
217700090213     C
217800090213     C                   eval      vinFLG = '2'
217900090213     c                   eval      Almeno_Un_Due ='S'
218000090213     c                   eval      esito  = '1'
218100090213     c                   eval      se_errore ='S'
218200090213     C                   eval      Err_in_detta = 'S'
218300090213     C
218400090213     C                   ENDSR
218500090209      * ?------------------------------------------------------------------ */
218600090209      *? Controlla la trasmissione   se completa
218700090209      * ?------------------------------------------------------------------ */
218800090209     c     Check_Trasm   Begsr
218900090209
219000090209     C                   clear                   se_errore
219100090209     C                   clear                   Riga_iNIZIo       1
219200090209     C                   clear                   Riga_fINe         1
219300090209      ** primo  record
219400090209     c     *start        setll     tivin00r
219500090209      *
219600090209     c     rileggi       tag
219700090209     c                   read      tivin00r
219800090209      *
219900090209     c                   if        not %eof(tivin00r)
220000090209      *
220100090209     c                   movel(p)  VINDTA        dati
220200090209     c                   if        dati = *blank
220300090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
220400090209      * le leggo e le escludo.
220500090209     C                   eval      vinFLG = '1'
220600090209     c                   update    Tivin000
220700090209     c                   goto      rileggi
220800090209     c                   end
220900090209      * ?              /*---------------------- */
221000090210     c                   exsr      Decod_Segmento
221100090209      * ?              /*---------------------- */
221200090209     c                   endif
221300090209      **
221400090209      ** ultimo record
221500090209     c     *hival        setll     tivin00r
221600090209     c     leggiAncora   tag
221700090209     c                   readp     tivin00r
221800090209     c                   if        not %eof(tivin00r)
221900090209      *
222000090209     c                   movel(p)  VINDTA        dati
222100090209     c                   if        dati = *blank
222200090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
222300090209      * le leggo e le escludo.
222400090209     C                   eval      vinFLG = '1'
222500090209     c                   update    Tivin000
222600090209     c                   goto      leggiAncora
222700090209     c                   end
222800090209      * ?              /*---------------------- */
222900090210     c                   exsr      Decod_Segmento
223000090209      * ?              /*---------------------- */
223100090209     c                   endif
223200090209
223300090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
223400090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
223500090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
223600090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
223700090209      * ?-----> Errore
223800090209     C                   eval      se_errore = 'S'
223900090209     c                   end
224000090209
224100090209     c                   endSR
224200090209      * ?------------------------------------------------------------------ */
224300090209      *?    Decodifica il segmento ed importa i campi in formato DS
224400090209      * ?------------------------------------------------------------------ */
224500090209     c     Decod_SegmentoBegsr
224600090209      **
224700090209     c                   eval      tipo_seg = %subst(vinDTA:1:3)
224800090209     c                   eval      segmento = vinDTA
224900100716      *
225000100720     c                   clear                   seg_imprevisto
225100100720     c                   clear                   trovato_seg
225200100720      *
225300100716     C                   clear                   keyUNBCLI
225400100716     C                   clear                   keyTIPOMSG
225500100716     C                   clear                   keyVERSION
225600100716     C                   clear                   keyRELEASE
225700100716     C                   clear                   keyAGENCY
225800100716     C                   clear                   keyASSOCIA
225900090210      **--------
226000100720      *  Chiamata generica x decodificare il singolo segmento letto
226100100720     c                   call      'TRTCT00R1'
226200100720      * input
226300100720     c                   parm                    tipo_seg
226400100720     c                   parm                    segmento
226500100720     C                   parm                    keyUNBCLI
226600100720     C                   parm                    keyTIPOMSG
226700100720     C                   parm                    keyVERSION
226800100720     C                   parm                    keyRELEASE
226900100720     C                   parm                    keyAGENCY
227000100720     C                   parm                    keyASSOCIA
227100100720      *output
227200100720     c                   parm                    Errore
227300100720     c                   parm                    DsGenerica
227400100720     c                   parm                    Valori_inErr
227500100720     c                   parm                    Descr_Errore
227600100720     c                   parm                    trovato_seg
227700100720      **--------
227800090211     c                   select
227900100720      *
228000100720     c                   when      trovato_seg ='N'
228100100720      *
228200100720     c                   move      'S'           seg_imprevisto
228300100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
228400100720     c                   exsr      Error_DET
228500100720     c                   goto      end_Decod
228600100720      * --------
228700100720     c                   when      tipo_seg = 'UNA'
228800100720     c                   movel(p)  DsGenerica    EDS00UNA
228900100720      **--------
229000090209     c                   when      tipo_seg = 'UNB'
229100100720     c                   movel(p)  DsGenerica    EDS00UNB
229200090210     C                   EXSR      DATI_UNB
229300090210      * --------
229400090209     c                   when      tipo_seg = 'UNH'
229500100720     c                   movel(p)  DsGenerica    EDS00UNH
229600090210     C                   EXSR      DATI_UNH
229700090210      * --------
229800090209     c                   when      tipo_seg = 'BGM'
229900100720     c                   movel(p)  DsGenerica    EDS00BGM
230000090210      **--------
230100090209     c                   when      tipo_seg = 'DTM'
230200100720     c                   movel(p)  DsGenerica    EDS00DTM
230300090210     C                   EXSR      DATI_DTM
230400090210      * --------
230500090209     c                   when      tipo_seg = 'TSR'
230600100720     c                   movel(p)  DsGenerica    EDS00TSR
230700090210      **--------
230800090209     c                   when      tipo_seg = 'CNT'
230900100720     c                   movel(p)  DsGenerica    EDS00CNT
231000090210      * --------
231100090209     c                   when      tipo_seg = 'TOD'
231200100720     c                   movel(p)  DsGenerica    EDS00TOD
231300090210     C                   EXSR      DATI_TOD
231400090210      **--------
231500090209     c                   when      tipo_seg = 'RFF'
231600100720     c                   movel(p)  DsGenerica    EDS00RFF
231700090210     C                   EXSR      DATI_RFF
231800090210      **--------
231900090209     c                   when      tipo_seg = 'TDT'
232000100720     c                   movel(p)  DsGenerica    EDS00TDT
232100090210      **--------
232200090213     c                   when      tipo_seg = 'MOA'
232300100720     c                   movel(p)  DsGenerica    EDS00MOA
232400090213     C                   EXSR      DATI_MOA
232500090213      **--------
232600090213     c                   when      tipo_seg = 'FTX'
232700100720     c                   movel(p)  DsGenerica    EDS00FTX
232800090213     C                   EXSR      DATI_FTX
232900090213      **--------
233000090209     c                   when      tipo_seg = 'NAD'
233100100720     c                   movel(p)  DsGenerica    EDS00NAD
233200090210     C                   EXSR      DATI_NAD
233300090210      **--------
233400090209     c                   when      tipo_seg = 'CTA'
233500100720     c                   movel(p)  DsGenerica    EDS00CTA
233600090210     C                   EXSR      DATI_CTA
233700090210      **--------
233800090209     c                   when      tipo_seg = 'COM'
233900100720     c                   movel(p)  DsGenerica    EDS00COM
234000090210     C                   EXSR      DATI_COM
234100090210      **--------
234200090209     c                   when      tipo_seg = 'GID'
234300100720     c                   movel(p)  DsGenerica    EDS00GID
234400090210     C                   EXSR      DATI_GID
234500090210      **--------
234600090209     c                   when      tipo_seg = 'MEA'
234700100720     c                   movel(p)  DsGenerica    EDS00MEA
234800090210     C                   EXSR      DATI_MEA
234900090210      **--------
235000090209     c                   when      tipo_seg = 'PCI'
235100100720     c                   movel(p)  DsGenerica    EDS00PCI
235200090210     C                   EXSR      DATI_PCI
235300090210      **--------
235400090209     c                   when      tipo_seg = 'UNT'
235500100720     c                   movel(p)  DsGenerica    EDS00UNT
235600090210     C                   EXSR      DATI_UNT
235700090210      **--------
235800090209     c                   when      tipo_seg = 'UNZ'
235900100720     c                   movel(p)  DsGenerica    EDS00UNZ
236000090210     C                   EXSR      DATI_UNZ
236100090210      **--------
236200100720     c                   OTHER
236300100720      **--------
236400100720     c                   move      'S'           seg_imprevisto
236500100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
236600100720     c                   exsr      Error_DET
236700100720     c                   goto      end_Decod
236800100720      **--------
236900090209     c                   endSL
237000090209      **
237100100720     c     end_Decod     endSR
237200090209     c*==================================================================*
237300090209     C*? CNVDAT - DECODIFICA LA DATA
237400090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
237500090209     C*         - WDATA  (DATA ALFANUMERICA)
237600090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
237700090209     C*         - WHMS   (ORA NUMERICA)
237800090209     C*----------------------------------------------------------------
237900090209     C     CNVDAT        BEGSR
238000090209     C*
238100090209     C                   MOVEL     ' '           WERRO             1
238200090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
238300090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
238400090209     C                   Z-ADD     *ZEROS        WHMS              6 0
238500090209     C*
238600090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
238700090209     C                   MOVEL     WDATA         WCOMO8            8
238800090209     C                   TESTN                   WCOMO8               99
238900090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
239000090209     C  N99              MOVEL     'S'           WERRO             1
239100090209     C                   END
239200090209     C*
239300090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
239400090209     C                   MOVEL     WDATA         WCOMO14          14
239500091216      *
239600091216     c                   if        %subst(WCOMO14:13:2) = *blank
239700091216     c                   eval      %subst(WCOMO14:13:2) = '00'
239800091216     c                   end
239900091216      *
240000090209     C                   TESTN                   WCOMO14              99
240100090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
240200090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
240300090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
240400090209     C  N99              MOVEL     'S'           WERRO             1
240500090209     C                   END
240600090209     C*
240700090209     C                   ENDSR
240800090210     C*----------------------------------------------------------------
240900090210     C*? TSTNUM - CONTROLLO NUMERICO
241000090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
241100090210     C*         - WLUNGH (Lunghezza campo output)
241200090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
241300090210     C*----------------------------------------------------------------
241400090210     C     TSTNUM        BEGSR
241500090210     C*
241600090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
241700090210     C                   MOVEL     'N'           WOKNUM            1
241800090210     C                   MOVEL     *BLANKS       WNUMOK           15
241900090210     C                   Z-ADD     15            IN                3 0
242000090210     C                   Z-ADD     WLUNGH        IX                3 0
242100090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
242200090210     C                   MOVEA     WNUM          NDA
242300090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
242400090210     C                   MOVEA     *ALL'0'       NDN
242500090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
242600090210     C                   Z-ADD     WLUNGH        I                 3 0
242700090210     C     I             DOWGT     0                                            --- 1 -->
242800090210     C* ESTRAE IL CARATTERE DA ESAMINARE
242900090210     C     1             SUBST     WNUM:I        WTEM01            1
243000090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
243100090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
243200090210     C     WTEM01        SCAN      WCA                                    99
243300090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
243400090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
243500090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
243600090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
243700090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
243800090210     C* DA DESTRA VERSO SINISTRA
243900090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
244000090210     C                   MOVEL     WTEMP1        NDN(IN)
244100090210     C                   SUB       1             IN
244200090210     C                   END                                                    <-- 3 ---
244300090210     C                   END                                                    <-- 2 ---
244400090210     C                   SUB       1             I
244500090210     C                   END                                                    <-- 1 ---
244600090210     C*
244700090210     C                   MOVEA     NDN           WNDN             15
244800090210     C     WNDN          IFNE      *ZEROS
244900090210     C                   MOVEA     NDN           WNUMOK           15
245000090210     C                   MOVEL     'S'           WOKNUM            1
245100090210     C                   END
245200090210     C*
245300090210     C                   ENDSR
245400090210     C*----------------------------------------------------------------
245500090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
245600090210     C*----------------------------------------------------------------
245700090210     C     CNVCAP        BEGSR
245800090210     C*
245900090210     C                   MOVEL     *BLANKS       WCAP              9
246000090210     C     nad3251       IFNE      '0'
246100090210     C     '  '          SCAN      nad3251       LENGHT            2 0
246200090210     C                   SUB       1             LENGHT
246300090210     C     LENGHT        IFLE      5
246400090210     C     LENGHT        ANDGT     0
246500090210     C                   Z-ADD     LENGHT        T                 2 0
246600090210     C                   Z-ADD     5             S                 2 0
246700090210     C                   MOVEA     nad3251       CAP9
246800090210     C                   MOVEL     *ZEROS        CAP5
246900090210     C                   DO        LENGHT
247000090210     C                   MOVE      CAP9(T)       CAP5(S)
247100090210     C                   SUB       1             S
247200090210     C                   SUB       1             T
247300090210     C                   END
247400090210     C                   MOVEA     CAP5          WCAP5             5
247500090210     C     WCAP5         IFEQ      '0'
247600090210     C                   MOVEL     *BLANKS       WCAP
247700090210     C                   ELSE
247800090210     C                   MOVEA     CAP5          WCAP
247900090210     C                   END
248000090210     C*
248100090210     C                   ELSE
248200090210     C                   MOVEL     nad3251       WCAP
248300090210     C                   END
248400090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
248500090210     C     WCAP          IFNE      *BLANKS
248600090210     C     WFLCPF        ANDNE     0
248700090210     C                   MOVEL     WFLCPF        WELE              1 0
248800090210     C                   MOVE      WFLCPF        WPOS              1 0
248900090210     C                   MOVEL     *BLANK        WCPF
249000090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
249100090210     C                   ELSE
249200090210     C                   MOVEL     *BLANKS       WCPF
249300090210     C                   END
249400090210     C                   END
249500090210     C*
249600090210     C                   ENDSR
249700090211      * ?================================================================== */
249800090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
249900090211      * ?================================================================== */
250000090211     C     Scrive_VAB    BEGSR
250100090211      *
250200090211     c                   clear                   err_bloccante     1
250300090211     c                   move      'S'           Almeno_Uno
250400090211      *
250500090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
250600090211     c                   exsr      UPDVAB
250700090211      *
250800090211     C* >>>>>>>>>>>>
250900090211     C*  Richiamo pgm per scrittura immediata VAB
251000090211     C     §PUWRK        IFEQ      ' '
251100090211      *
251200090211     C*  Richiamo pgm per esecuzione stampa
251300090211     C     §PTCMR        IFEQ      'S'
251400090213     C                   MOVEL     WNRDOC        d86CMR
251500090211     C                   ELSE
251600090213     C                   MOVEL     WNRDOC        d86CMR
251700090211     C                   END
251800090211      *
251900090213     C                   MOVEL     ' '           d86RIE
252000090213     C                   MOVEL     §PUDTA        d86DTA
252100090213     C                   Z-ADD     1             d86CNT
252200090213     C                   Z-ADD     §PTKSC        d86CCM
252300090213     C                   MOVEL     *BLANKS       d86STA
252400090213     C                   MOVEL     'E'           d86MOD
252500090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
252600090211      * dove poi imposta la DS dei parametri passati
252700090213     C                   MOVEL     'LR'          d86CHI
252800090213     C                   MOVEL     'N'           d86CTL
252900090211      *
253000090211     c                   move      kpjbu         SvKpjbu
253100090211     c                   clear                   kpjbu
253200090216     C                   movel     *on           Commit_on         1
253300090213     C                   MOVEL     TRTC86ds      KPJBU
253400090213      * ?- Chiamata la TRTC86R2 -*
253500090213     C                   CALL      'TRTC86R2'
253600090211     C                   PARM                    KPJBA
253700090216     C                   PARM                    Commit_on
253800090211     c                   move      Svkpjbu       Kpjbu
253900090211      *
254000090211     C*  Controllo pgm per scrittura immediata VAB
254100090211     C     WW            IFNE      0
254200090211     C                   DO        WW            WX                3 0
254300090213     C                   Z-ADD     KCL(WX)       d86CCM
254400090211      *
254500090211     c                   move      kpjbu         SvKpjbu
254600090211     c                   clear                   kpjbu
254700090216     C                   movel     *on           Commit_on         1
254800090213     C                   MOVEL     TRTC86ds      KPJBU
254900090213      * ?- Chiamata la TRTC86R2 -*
255000090213     C                   CALL      'TRTC86R2'
255100090211     C                   PARM                    KPJBA
255200090216     C                   PARM                    Commit_on
255300090211     c                   move      Svkpjbu       Kpjbu
255400090211     C                   END
255500090211     C                   END
255600090211      *
255700090211     C                   END
255800090211      * >>>>>>>>>>>>
255900090211     C                   if        se_errore  = 'S'
256000090211     c                   move      'S'           err_bloccante
256100090211     c                   goto      Error_VAB
256200090211     c                   end
256300090211     C*
256400090211     C     Error_VAB     Endsr
256500090211     C*----------------------------------------------------------------
256600090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
256700090211     C*----------------------------------------------------------------
256800090211     C     UPDVAB        BEGSR
256900090211      *
257000090211     C*  Se non è stato impostato il codice bolla lo imposto io
257100090211     C     VABCBO        IFEQ      *BLANKS
257200090211     C     VABCAS        IFGT      0
257300090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
257400090211     C                   ELSE
257500090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
257600090211     C                   END
257700090211     C                   END
257800110922      *
257900110922      * Imposta le NOte
258000110922     C     VABNOT        IFEQ      *BLANKS
258100110922     C                   MOVEL     wnot1         VABNOT
258200110922     C                   MOVEL     wnot2         VABNT2
258300110922     C                   ELSE
258400110922     C                   MOVEL     wnot1         VABNT2
258500110922     C                   END
258600090211      *
258700100630     C*  Attribuisco anno spedizione
258800100630     C     DTM_DtPartenzaIFGT      0                                            * WDATPA
258900100630     C                   MOVEL     DTM_DtPartenzaVABAAS                         anno sped.
259000100630     C                   ELSE
259100090211     C     WDATFV        IFGT      0
259200090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
259300090211     C                   END
259400090211     C                   END
259500090211      *
259600090211      *  Controllo dettaglio segnacolli
259700090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
259800090211     C                   EXSR      CTRSGE
259900090211     C                   ELSE
260000090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
260100090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
260200090211     C                   EXSR      CTRSGN
260300090211     C                   END
260400090211     C                   END
260500090211      *
260600090212      *  Imposto linea partenza e serie
260700090211     C                   eval      VABLNP = VABLNP_PT
260800090212     C                   eval      VABNRS = VABnrs_PT
260900090211      *
261000090211      *  Controllo se devo variare il codice trattamento merce
261100090211     C                   eval      VABCTM = VABctm_PT
261200090211      *
261300090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
261400090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
261500090211     c                   clear                   mantiene_nsp      1            *blk/'S'
261600090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
261700090211      *
261800090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
261900090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
262000090211      *
262100090211      *  Imposto codice cliente
262200090211     c                   Select
262300090211      *
262400090211      *  Specificato codice da qualificatore FF (TD15)
262500090211     C                   When        WCLKSC <> *zeros
262600090211     C                   Z-ADD     WCLKSC        VABCCM
262700090211     C                   MOVEL     WCLTAR        VABCTR
262800090211      * forza LNP/CTM/NRS
262900090211     c                   if        WclLNP <> 0
263000090211     C                   eval      VABLNP = WclLNP
263100090211     c                   end
263200090211     c                   if        WclNRS <> 0
263300090211     C                   eval      VABNRS = WclNRS
263400090211     c                   end
263500090211     c                   if        WclCTM <> *blank
263600090211     C                   eval      VABCTM = WclCTM
263700090211     c                   end
263800090211      *
263900090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
264000090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
264100090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
264200090211     c                   end
264300090211      *
264400090211      *  Specificato codice da qualificatore FF (TT15)
264500090211     C                   When        $CLKSC <> *zeros
264600090211     C                   Z-ADD     $CLKSC        VABCCM
264700090211     C                   MOVEL     $CLTAR        VABCTR
264800090211      * forza LNP/CTM/NRS
264900090211     c                   if        $clLNP <> 0
265000090211     C                   eval      VABLNP = $clLNP
265100090211     c                   end
265200090211     c                   if        $clNRS <> 0
265300090211     C                   eval      VABNRS = $clNRS
265400090211     c                   end
265500090211     c                   if        $clCTM <> *blank
265600090211     C                   eval      VABCTM = $clCTM
265700090211     c                   end
265800090211      *
265900090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
266000090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
266100090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
266200090211     c                   end
266300090211      *
266400090211      *  Imposto dati da tabella - PT-
266500090211     C                   Other
266600090211      *
266700090211     C                   MOVEL     §PTKSC        VABCCM
266800090211     C                   MOVEL     §PTCTR        VABCTR
266900090211      *
267000090211     C                   Endsl
267100090211      *
267200090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
267300090211      *    o dalla relativa CL
267400090211     C                   MOVEL     VABlnp        VABfgs
267500090211      *
267600090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
267700090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
267800090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
267900090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
268000090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
268100090211      *
268200090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
268300090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
268400090211     c                   else
268500090211      *
268600090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
268700090211     C*  lo attribuisco io
268800090211      **              **------------------**
268900090211     c                   exsr      repnum
269000090211      **              **------------------**
269100090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
269200090211      **              **------------------**
269300090211     c                   end
269400090211      *
269500090211     C*  Altrimenti attribuisco data del giorno
269600090211     C     VABMGS        IFEQ      0
269700090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
269800090211     C                   MOVE      WOGGI         VABMGS                         = oggi
269900090211     C                   END
270000090211     C*  Eseguo posizionamento su EDiVAB
270100090211     C                   Z-ADD     VABAAS        KAAS
270200090211     C                   Z-ADD     VABlnp        KLNP
270300090211     C                   Z-ADD     VABNRS        KNRS
270400090211     C                   Z-ADD     VABNSP        KNSP
270500090211     C                   MOVEL     SAVBOL        SALVA
270600090211     C     KVAB1         CHAIN     EDiVAB1L                           30
270700090211     C                   MOVEL     SALVA         SAVBOL
270800090211     C*  Imposto dati CMR
270900090211     C                   Z-ADD     1             VABCNT
271000090211     **
271100090211      *** Attenzione se si deve trattare il VAX con il CMR
271200090211      *** il campo VABCNT deve essere sempre impostato a (1)
271300090211     C     §puWRK        ifEQ      'S'
271400090211     C     §puNSP        andEQ     'S'
271500090211     C                   Z-ADD     1             VABCNT
271600090211     c                   end
271700090211     **
271800090211     C                   Z-ADD     WDTPRE        VABDTS
271900090212     C                   movel     '20'          VABDTS
272000090212     C     WHMPRE        mult      100           VABHMS
272100090211     C     §PTCMR        IFEQ      'S'
272200090211     C                   Z-ADD     WDTCMR        VABDCM
272300100630     C                   MOVEL     UNB_NrCMR     VABCMR                         * WNRCMR
272400090211     C                   ELSE
272500090211     C                   Z-ADD     WDATFV        VABDCM
272600100630     C                   MOVEL     UNB_NFViaggio VABCMR                         * WNUMFV
272700090211     C                   END
272800090211     C*  Se non è indicato il nome del mittente = Partner mittente
272900090211     C     VABRMO        IFEQ      *BLANKS
273000090211     C                   MOVEL     ACORAG        VABRMO
273100090211     C                   MOVEL     INDCAP        VABCMO
273200090211     C     INDCAE        IFNE      *BLANKS
273300090211     C                   MOVEL     INDCAE        VABCMO
273400090211     C                   END
273500090211     C     INDSTA        IFNE      *BLANKS
273600090211     C                   MOVEL     INDSTA        VABNMO
273700090211     C                   END
273800090211     C                   END
273900090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
274000090211     C     §1BFG1        IFEQ      'N'
274100090211     C     §1BFG7        ANDEQ     'N'
274200090211     C     §1BFG2        ANDEQ     'N'
274300090211     C***                  Z-ADD0         VABNRS
274400090211     C                   END
274500090211     C*  Imposto segancollo da - a
274600090211     C     §1BFG1        IFEQ      'S'
274700090211     C     §1BFG7        ANDEQ     'N'
274800090211     C     §1BF12        ANDNE     'E'
274900090211      *
275000090211     C     XY            IFEQ      *ZEROS
275100090211     C                   CLEAR                   VABNCD
275200090211     C                   CLEAR                   VABNCA
275300090211     C                   ELSE
275400090211     C                   Z-ADD     SGN(1)        VABNCD
275500090211     C                   Z-ADD     SGN(XY)       VABNCA
275600090211     C                   ENDIF
275700090211     C*
275800090211     C                   END
275900090211     C*
276000090211     C*  Se cap mittente originale a blank abblenco nazione
276100090211     C     VABCMO        IFEQ      *BLANKS
276200090211     C                   MOVEL     *BLANKS       VABNMO
276300090211     C                   END
276400090211     C*
276500090211      *? ------------------------------------------------------------------------
276600090213     C   30              WRITE     EDiVAB00                             99
276700090213     C  N30              UPDATE    EDiVAB00                             99
276800090211      *? ------------------------------------------------------------------------
276900090213     c   99              eval      errori_in_Wrt = 'S'
277000090211     C*
277100090211     c                   clear                   wri_vabtic        1
277200090211     C*
277300090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
277400090211     C                   EXSR      WRTVAT_Rif
277500090211     C*
277600090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
277700090211     C     §1BF12        IFEQ      'E'
277800090211     C                   EXSR      WRTVAT
277900090211     C                   ELSE
278000090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
278100090211     C     §1BFG7        IFEQ      'S'
278200090211     C     §1BFG1        OREQ      'S'
278300090211     C                   EXSR      WRTVAD
278400090211     C                   END
278500090211     C                   END
278600090211      *
278700090211     C* Reimposto codice trattamento merce cliente
278800090211     C                   MOVEL     WSVTRM        DS1B
278900090211     C**
279000090211     C* Do errore se previsto CMR ma non c'è
279100090211     C     §PTCMR        IFNE      ' '
279200090211     C                   EXSR      MEMCMR
279300090211     C                   END
279400090211     C* Do errore se previsto AFB ma non c'è
279500090211     C     §PTNFV        IFNE      ' '
279600090211     C                   EXSR      MEMAFB
279700090211     C                   END
279800090211     C*  Se il cliente vuole il ritorno delle date di consegna
279900090211     C*  è c'è una squdratura fra i segnacolli e il numero
280000090211     C*  dei colli che ci ha passato scrivo EDSUM
280100090211     C     §PUDTA        IFEQ      'S'
280200090211     C     WSQUAD        ANDEQ     'S'
280300090211     C                   EXSR      WRTSUM
280400090211     C                   END
280500090211     C*
280600090211     C                   ENDSR
280700090211     C*----------------------------------------------------------------
280800090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
280900090211     C*----------------------------------------------------------------
281000090211     C     CTRSGN        BEGSR
281100090211     C*
281200090211     C                   MOVEL     'N'           WNSGER            1
281300090211     C*
281400090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
281500090211     C     VABNCL        IFNE      XY
281600090211     C                   MOVEL     'S'           WNSGER
281700090211     C                   MOVEL     'S'           WSQUAD
281800090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
281900090211     c                              bollettati'
282000090211     C                   GOTO      FINSGN
282100090211     C                   END
282200090211     C*  Riordino i segnacolli
282300090211     C     §1BFG7        IFEQ      'N'
282400090211     C                   SORTA     SGN
282500090211     C*  Controllo se sono sequenziali
282600090211     C                   MOVEL     'N'           WNOSEQ            1
282700090211     C     SGN(1)        ADD       1             WEXSGN            7 0
282800090211     C     2             DO        XY            X
282900090211     C     SGN(X)        IFNE      WEXSGN
283000090211     C                   MOVEL     'S'           WNOSEQ
283100090211     C                   END
283200090211     C     SGN(X)        ADD       1             WEXSGN
283300090211     C                   END
283400090211     C                   END
283500090211     C*
283600090211     C     FINSGN        ENDSR
283700090211     C*----------------------------------------------------------------
283800090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
283900090211     C*----------------------------------------------------------------
284000090211     C     CTRSGE        BEGSR
284100090211     C*
284200090211     C                   MOVEL     'N'           WNSGER            1
284300090211      *
284400090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
284500090211      *
284600090211     C     VABNCL        IFNE      §§
284700090211     C                   MOVEL     'S'           WNSGER
284800090211     C                   MOVEL     'S'           WSQUAD
284900090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
285000090211     c                              bollettati'
285100090211     C                   END
285200090211      *
285300090211     C                   ENDSR
285400090211     C*----------------------------------------------------------------
285500090211     C*? ESEGUO SCRITTURA EDiVAD
285600090211     C*----------------------------------------------------------------
285700090211     C     WRTVAD        BEGSR
285800090211     C*
285900090211     C* Se non seq. scrivo xy record
286000090211     C     §1BFG1        IFEQ      'S'
286100090211     C     XY            ANDNE     *ZEROS
286200090211     C*
286300090211     C     §1BFG7        IFEQ      'S'
286400090211     C                   DO        XY            X
286500090211     C                   Z-ADD     VABCCM        KCCM
286600090211     C                   Z-ADD     SGN(X)        KNCD
286700090211     C                   Z-ADD     VABCNT        VADCNT
286800090211     C                   MOVEL     VABCMR        VADCMR
286900090211     C                   Z-ADD     VABAAS        VADAAS
287000090211     C                   Z-ADD     VABLNP        VADLNP
287100090211     C                   Z-ADD     VABNRS        VADNRS
287200090211     C                   Z-ADD     VABNSP        VADNSP
287300090211     C                   Z-ADD     1             VADNCL
287400090211     C                   MOVEL     SGN(X)        VADCDP
287500090211     C                   Z-ADD     SGN(X)        VADNCD
287600090211     C                   Z-ADD     *ZEROS        VADNCA
287700090211     C                   Z-ADD     VABCCM        VADCCM
287800090211     C                   movel     VABfgs        VADfgs
287900090213     C                   WRITE     EDiVAD00                             99
288000090213     c   99              eval      errori_in_Wrt = 'S'
288100090211     C                   END
288200090211     C* Se sequenziali scrivo un solo record
288300090211     C                   ELSE
288400090211     C                   Z-ADD     VABAAS        VADAAS
288500090211     C                   Z-ADD     VABLNP        VADLNP
288600090211     C                   Z-ADD     VABNRS        VADNRS
288700090211     C                   Z-ADD     VABNSP        VADNSP
288800090211     C                   Z-ADD     VABNCL        VADNCL
288900090211     C                   Z-ADD     SGN(1)        VADNCD
289000090211     C                   Z-ADD     SGN(XY)       VADNCA
289100090211     C                   Z-ADD     VABCCM        VADCCM
289200090211     C                   movel     VABfgs        VADfgs
289300090213     C                   WRITE     EDiVAD00                             99
289400090213     c   99              eval      errori_in_Wrt = 'S'
289500090211     C                   END
289600090211     C*
289700090211     C                   ELSE
289800090211     C                   DO        XY            X
289900090211     C                   Z-ADD     VABCCM        KCCM
290000090211     C                   Z-ADD     SGN(X)        KNCD
290100090211     C                   Z-ADD     VABAAS        VADAAS
290200090211     C                   Z-ADD     VABLNP        VADLNP
290300090211     C                   Z-ADD     VABNRS        VADNRS
290400090211     C                   Z-ADD     VABNSP        VADNSP
290500090211     C                   MOVEL     SGN(X)        VADCDP
290600090211     C                   Z-ADD     1             VADNCL
290700090211     C                   Z-ADD     *ZEROS        VADNCD
290800090211     C                   Z-ADD     *ZEROS        VADNCA
290900090211     C                   Z-ADD     VABCCM        VADCCM
291000090211     C                   movel     VABfgs        VADfgs
291100090213     C                   WRITE     EDiVAD00                             99
291200090213     c   99              eval      errori_in_Wrt = 'S'
291300090211     C                   END
291400090211     C*
291500090211     C                   END
291600090211     C*
291700090211     C                   ENDSR
291800090211     C*----------------------------------------------------------------
291900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
292000090211     C*----------------------------------------------------------------
292100090211     C     WRTVAT_Rif    BEGSR
292200090211      *
292300090211      *  riferimento di consegna destinatario
292400090211     c                   if        WatRDE <> *blank
292500090211     C                   Z-ADD     VABCNT        VATCNT
292600090211     C                   MOVEL     VABCMR        VATCMR
292700090211     C                   Z-ADD     VABCCM        VATCCM
292800090211     C                   movel     VABfgs        VATfgs
292900090211     C                   Z-ADD     VABLNP        VATLNP
293000090211     C                   Z-ADD     VABAAS        VATAAS
293100090211     C                   Z-ADD     VABNRS        VATNRS
293200090211     C                   Z-ADD     VABNSP        VATNSP
293300090211     C                   MOVEL     'A'           VATTRC
293400090211     C                   MOVEL     WatRDE        VATNOT
293500090213     C                   WRITE     EDiVAT00                             99
293600090213     c   99              eval      errori_in_Wrt = 'S'
293700090211     c                   end
293800090211      *
293900090211      *  riferimento di consegna telefonico
294000090211     c                   if        WatTEL <> *blank
294100090211     C                   Z-ADD     VABCNT        VATCNT
294200090211     C                   MOVEL     VABCMR        VATCMR
294300090211     C                   Z-ADD     VABCCM        VATCCM
294400090211     C                   movel     VABfgs        VATfgs
294500090211     C                   Z-ADD     VABLNP        VATLNP
294600090211     C                   Z-ADD     VABAAS        VATAAS
294700090211     C                   Z-ADD     VABNRS        VATNRS
294800090211     C                   Z-ADD     VABNSP        VATNSP
294900090211     C                   MOVEL     'B'           VATTRC
295000090211     C                   MOVEL     WatTEL        VATNOT
295100090213     C                   WRITE     EDiVAT00                             99
295200090213     c   99              eval      errori_in_Wrt = 'S'
295300090211     c                   end
295400090211      *
295500090211      * riferimento del Partner che una volta veniva riportato sul BL4
295600090211     c                   if        Wnsp   <> *blank
295700090211     C                   Z-ADD     VABCNT        VATCNT
295800090211     C                   MOVEL     VABCMR        VATCMR
295900090211     C                   Z-ADD     VABCCM        VATCCM
296000090211     C                   movel     VABfgs        VATfgs
296100090211     C                   Z-ADD     VABLNP        VATLNP
296200090211     C                   Z-ADD     VABAAS        VATAAS
296300090211     C                   Z-ADD     VABNRS        VATNRS
296400090211     C                   Z-ADD     VABNSP        VATNSP
296500090211     C                   MOVEL     'C'           VATTRC
296600090211     C                   MOVEL     Wnsp          VATNOT
296700090213     C                   WRITE     EDiVAT00                             99
296800090213     c   99              eval      errori_in_Wrt = 'S'
296900090211     c                   end
297000090211      *
297100160524      *  Aggiunto a maggio 2016
297200160524     c                   exsr      Scrive_Alert
297300160524      *
297400090211     C                   ENDSR
297500160524     C*----------------------------------------------------------------
297600160524     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
297700160524     C*----------------------------------------------------------------
297800160524     C     Scrive_Alert  BEGSR
297900160524      *
298000160524      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
298100160524      *   sulla consegna della sua merce
298200160524      *
298300160524     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
298400160524     c                                  <> *blank
298500160524     C                   Z-ADD     VABCNT        VATCNT
298600160524     C                   MOVEL     VABCMR        VATCMR
298700160524     C                   Z-ADD     VABCCM        VATCCM
298800160524     C                   movel     VABfgs        VATfgs
298900160524     C                   Z-ADD     VABLNP        VATLNP
299000160524     C                   Z-ADD     VABAAS        VATAAS
299100160524     C                   Z-ADD     VABNRS        VATNRS
299200160524     C                   Z-ADD     VABNSP        VATNSP
299300160524     C                   MOVEL     'I'           VATTRC
299400160524     C                   eval       VATNOT =
299500160524     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
299600160524     C                   WRITE     EDiVAT00                             99
299700160524     c   99              eval      errori_in_Wrt = 'S'
299800160524     c                   end
299900160524      *
300000160524     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
300100160524     c                                 <> *blank
300200160524     C                   Z-ADD     VABCNT        VATCNT
300300160524     C                   MOVEL     VABCMR        VATCMR
300400160524     C                   Z-ADD     VABCCM        VATCCM
300500160524     C                   movel     VABfgs        VATfgs
300600160524     C                   Z-ADD     VABLNP        VATLNP
300700160524     C                   Z-ADD     VABAAS        VATAAS
300800160524     C                   Z-ADD     VABNRS        VATNRS
300900160524     C                   Z-ADD     VABNSP        VATNSP
301000160524     C                   MOVEL     'J'           VATTRC
301100160524     C                   eval       VATNOT =
301200160524     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
301300160524     C                   WRITE     EDiVAT00                             99
301400160524     c   99              eval      errori_in_Wrt = 'S'
301500160524     c                   end
301600160524      *
301700160524      *
301800160524      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
301900160524      *   sulla consegna della sua merce
302000160524      *
302100160524     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
302200160524     c                                  <> *blank
302300160524     c                             or FTX_MAIL_come_servizio = 'N'
302400160524      *
302500160524     C                   Z-ADD     VABCNT        VATCNT
302600160524     C                   MOVEL     VABCMR        VATCMR
302700160524     C                   Z-ADD     VABCCM        VATCCM
302800160524     C                   movel     VABfgs        VATfgs
302900160524     C                   Z-ADD     VABLNP        VATLNP
303000160524     C                   Z-ADD     VABAAS        VATAAS
303100160524     C                   Z-ADD     VABNRS        VATNRS
303200160524     C                   Z-ADD     VABNSP        VATNSP
303300160524     C                   MOVEL     'S'           VATTRC
303400160524      *
303500160524      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
303600160524      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
303700160524     C                   eval       VATNOT =
303800160524     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
303900160524      *
304000160524      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
304100160524      * seguenti attivano o meno il servizio x SMS o MAIL
304200160524      *
304300160524      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
304400160524      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
304500160524     C                   eval      %subst(VATNOT:17:1) = 'S'
304600160524     c                   if        FTX_SMS_come_servizio = 'N'
304700160524     c                              or
304800160524     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
304900160524     c                              = *blank
305000160524     C                   eval      %subst(VATNOT:17:1) = 'N'
305100160524     c                   end
305200160524      *
305300160524     C                   eval      %subst(VATNOT:18:1) = 'S'
305400160524     c                   if        FTX_MAIL_come_servizio = 'N'
305500160524     c                              or
305600160524     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
305700160524     c                              = *blank
305800160524     C                   eval      %subst(VATNOT:18:1) = 'N'
305900160524     c                   end
306000160524      *
306100160524     C                   WRITE     EDiVAT00                             99
306200160524     c   99              eval      errori_in_Wrt = 'S'
306300160524     c                   end
306400160524      *
306500160524     C                   ENDSR
306600090211     C*----------------------------------------------------------------
306700090211     C*? ESEGUO SCRITTURA EDiVAT
306800090211     C*----------------------------------------------------------------
306900090211     C     WRTVAT        BEGSR
307000090211      *
307100090211     C                   DO        §§            X
307200090211     C                   Z-ADD     VABCNT        VATCNT
307300090211     C                   MOVEL     VABCMR        VATCMR
307400090211     C                   Z-ADD     VABCCM        VATCCM
307500090211     C                   movel     VABfgs        VATfgs
307600090211     C                   Z-ADD     VABLNP        VATLNP
307700090211     C                   Z-ADD     VABAAS        VATAAS
307800090211     C                   Z-ADD     VABNRS        VATNRS
307900090211     C                   Z-ADD     VABNSP        VATNSP
308000090211     C                   MOVEL     'E'           VATTRC
308100090211      *
308200090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
308300090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
308400090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
308500090211     c                   if        WVATlun > 0
308600090211      *
308700090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
308800090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
308900090211      *
309000090211     c                   else
309100090211      *
309200090211      * Se non è impostato nulla prende il segnacollo passato così com'è
309300090211     C                   MOVEL     SGE(X)        VATNOT
309400090211     c                   end
309500090211      *
309600090213     C                   WRITE     EDiVAT00                             99
309700090213     c   99              eval      errori_in_Wrt = 'S'
309800090211     C                   END
309900090211      *
310000090211     C                   ENDSR
310100090211     C*----------------------------------------------------------------
310200090211     C*? ESEGUO SCRITTURA EDSUM
310300090211     C*----------------------------------------------------------------
310400090211     C     WRTSUM        BEGSR
310500090211     C*
310600090211     C* Se non seq. scrivo xy record
310700090211     C                   DO        XY            X
310800090211     C                   Z-ADD     VABAAS        SUMAAS
310900090211     C                   Z-ADD     VABLNP        SUMFLS
311000090211     C                   MOVEL     WKSC          SUMLNP
311100090211     C                   Z-ADD     VABLNA        SUMLNA
311200090211     C                   Z-ADD     VABZNC        SUMZNC
311300090211     C                   Z-ADD     VABNRS        SUMNRS
311400090211     C                   Z-ADD     VABNSP        SUMNSP
311500090211     C                   Z-ADD     SGN(X)        SUMNSC
311600090211     C                   Z-ADD     WKSC          SUMCCM
311700090211     C                   MOVEL     *BLANKS       SUMSTS
311800090211     C                   MOVEL     *BLANKS       SUMFLG
311900090211     C                   MOVEL     WPRGSP        SUMCNI
312000090211     C                   MOVEL     VABCMR        SUMCMR
312100090211     C                   Z-ADD     0             SUMNFE
312200090211     C                   Z-ADD     VABCCM        VADCCM
312300090213     C                   WRITE     EDSUM000                             99
312400090213     c   99              eval      errori_in_Wrt = 'S'
312500090211     C                   END
312600090211     C*
312700090211     C                   ENDSR
312800090211     c*==================================================================*
312900090211      * Manda un Msg in Posta AS Sede a EDP*
313000090211     c*==================================================================*
313100090211     c     SEND_MSG_EDP  begsr
313200090211      *
313300090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
313400090211      *   Lo manda una sola volta per lavoro
313500090211     c                   IF        Snd_Msg_alert  <> 'N' and
313600090211     c                             User_msg <> *blank
313700090211     c                   z-add     1             Jx
313800090211     C                   exsr      CalQEZSNDMG
313900090211     c                   end
314000090211      *
314100090211     c                   endsr
314200090213     c**********************************************************************
314300090213     c* reperimento numero Spedizione
314400090213     c**********************************************************************
314500090213     C     repnum        BEGSR
314600090213      *
314700090213      *    deve controllare sulla tabella "3C" se il numero spedizione
314800090213      *     deve essere mantenuto oppure no
314900090213     C*
315000090213     C                   clear                   Ds3C
315100090213     C                   Z-ADD     CODUT         TBLKUT
315200090213     C                   MOVEL     '3C'          TBLCOD
315300090213     C                   movel(p)  VABccm        TBLKEY
315400090213     C     KTABel        CHAIN     TABEL00F                           30
315500090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
315600090213     C                   MOVEL     TBLUNI        Ds3C
315700090213     C                   END
315800090213      *
315900090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
316000090213     c                   if        §3CFsp <> 'S'
316100090213      *
316200090213     C* NSP => Stacco un numeratore da AZNUM
316300090213     c                   movel     kpjbu         svkpjbu
316400090213     c                   clear                   kpjbu
316500090213     C                   clear                   TRUL33DS
316600090213     C                   eval      I33OPE = *zeros
316700090213     C                   eval      I33CNU = 302
316800090213     C                   eval      I33NUM = 1
316900090213     C                   movel     TRUL33DS      KPJBU
317000090213     C                   call      'TRUL33R'
317100090213     C                   parm                    KPJBA
317200090213     C                   movel     KPJBU         TRUL33DS
317300090213     c                   movel     svkpjbu       kpjbu
317400090213      ****
317500090213     C                   if        O33ERR = *zeros
317600090213     c                   z-add     o33nrf        NUM_SPED
317700090213     C                   else
317800090213     C                   endif
317900090213      ****
318000090213     c                   ELSE
318100090213      *   se si vuole mantenere il numero spedizione
318200090213      ****
318300090213     C                   MOVEL     WOGGI         WANNO             4 0
318400090213     C                   MOVE      WANNO         KAAA
318500090213     C                   Z-ADD     3             KCNU
318600090213     C                   Z-ADD     VABlnp        KFIL
318700090213     C     KNUF          CHAIN     FLNUF                              9999
318800090213     C     *IN99         IFEQ      '0'
318900090213     C                   ADD       1             NUFNUM
319000090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
319100090213     C                   UPDATE    FLNUF
319200090213     C                   ELSE
319300090213     C                   END
319400090213     C*
319500090213     c                   EndIF
319600090213      **
319700090213     C                   ENDSR
319800090213     C*----------------------------------------------------------------
319900090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
320000090213     C*----------------------------------------------------------------
320100090213     C     MEMCMR        BEGSR
320200090213     C*
320300090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
320400090213     C     §PTCM1        IFEQ      'S'
320500090213     C                   CLEAR                   EDRDE000
320600090213     C                   MOVEL     'TD1154'      KNMC
320700090213     C                   Z-ADD     1             KNRP
320800090213     C                   MOVEL     WKSC          KLNP1
320900090213     C     KRDE          CHAIN     EDRDE01L                           31
321000090213     C     *IN31         IFEQ      '1'
321100090213     C                   Z-ADD     VABAAS        RDEAAS
321200090213     C                   Z-ADD     VABLNP        RDELNP
321300090213     C                   Z-ADD     VABNRS        RDENRS
321400090213     C                   Z-ADD     VABNSP        RDENSP
321500090213     C                   MOVEL     'TD1154'      RDENMC
321600090213     C                   Z-ADD     1             RDENRP
321700100630     C                   MOVEL     UNB_NrCMR     RDEVAL                         * WNRCMR
321800090213     C                   WRITE     EDRDE000                             99
321900090213     c   99              eval      errori_in_Wrt = 'S'
322000090213     C                   END
322100090213     C*  Memorizzo qualificatore CMR
322200090213     C                   CLEAR                   EDRDE000
322300090213     C                   MOVEL     'TD1153'      KNMC
322400090213     C                   Z-ADD     1             KNRP
322500090213     C                   MOVEL     WKSC          KLNP1
322600090213     C     KRDE          CHAIN     EDRDE01L                           31
322700090213     C     *IN31         IFEQ      '1'
322800090213     C                   Z-ADD     VABAAS        RDEAAS
322900090213     C                   Z-ADD     VABLNP        RDELNP
323000090213     C                   Z-ADD     VABNRS        RDENRS
323100090213     C                   Z-ADD     VABNSP        RDENSP
323200090213     C                   Z-ADD     1             RDENRP
323300090213     C                   MOVEL     'TD1153'      RDENMC
323400090213     C                   MOVEL     'CMR'         RDEVAL
323500090213     C                   WRITE     EDRDE000                             99
323600090213     c   99              eval      errori_in_Wrt = 'S'
323700090213     C                   END
323800090213     C*  Memorizzo la data
323900090213     C                   CLEAR                   EDRDE000
324000090213     C                   MOVEL     'TD2380'      KNMC
324100090213     C                   Z-ADD     1             KNRP
324200090213     C                   MOVEL     WKSC          KLNP1
324300090213     C     KRDE          CHAIN     EDRDE01L                           31
324400090213     C     *IN31         IFEQ      '1'
324500090213     C                   Z-ADD     VABAAS        RDEAAS
324600090213     C                   Z-ADD     VABLNP        RDELNP
324700090213     C                   Z-ADD     VABNRS        RDENRS
324800090213     C                   Z-ADD     VABNSP        RDENSP
324900090213     C                   Z-ADD     1             RDENRP
325000090213     C                   MOVEL     'TD2380'      RDENMC
325100090213     C                   MOVEL     WDTCMR        RDEVAL
325200090213     C                   WRITE     EDRDE000                             99
325300090213     c   99              eval      errori_in_Wrt = 'S'
325400090213     C                   END
325500090213     C*
325600090213     C                   END                                                    §PTCM1 = 'S'
325700090213     C*
325800090213     C                   ENDSR
325900090213     C*----------------------------------------------------------------
326000090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
326100090213     C*----------------------------------------------------------------
326200090213     C     MEMAFB        BEGSR
326300090213     C*
326400090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
326500090213     C     §PTNF1        IFEQ      'S'
326600090213     C                   CLEAR                   EDRDE000
326700090213     C                   MOVEL     WKSC          KLNP1
326800090213     C                   MOVEL     'TD1154'      KNMC
326900090213     C                   Z-ADD     1             KNRP
327000090213     C     KRDE          CHAIN     EDRDE01L                           31
327100090213     C     *IN31         IFEQ      '1'
327200090213     C                   Z-ADD     VABAAS        RDEAAS
327300090213     C                   Z-ADD     VABLNP        RDELNP
327400090213     C                   Z-ADD     VABNRS        RDENRS
327500090213     C                   Z-ADD     VABNSP        RDENSP
327600090213     C                   MOVEL     'TD1154'      RDENMC
327700090213     C     §PTCM1        IFNE      'S'
327800100630     C     UNB_NrCMR     OREQ      *BLANKS                                      * WNRCMR
327900090213     C                   Z-ADD     1             RDENRP
328000090213     C                   ELSE
328100090213     C                   Z-ADD     2             RDENRP
328200090213     C                   END
328300100630     C                   MOVEL     UNB_NFViaggio RDEVAL                         * WNUMFV
328400090213     C                   WRITE     EDRDE000                             99
328500090213     c   99              eval      errori_in_Wrt = 'S'
328600090213     C                   END
328700090213     C*  Memorizzo qualificatore CMR
328800090213     C                   CLEAR                   EDRDE000
328900090213     C                   MOVEL     WKSC          KLNP1
329000090213     C                   MOVEL     'TD1153'      KNMC
329100090213     C                   Z-ADD     1             KNRP
329200090213     C     KRDE          CHAIN     EDRDE01L                           31
329300090213     C     *IN31         IFEQ      '1'
329400090213     C                   Z-ADD     VABAAS        RDEAAS
329500090213     C                   Z-ADD     VABLNP        RDELNP
329600090213     C                   Z-ADD     VABNRS        RDENRS
329700090213     C                   Z-ADD     VABNSP        RDENSP
329800090213     C     §PTCM1        IFNE      'S'
329900100630     C     UNB_NrCMR     OREQ      *BLANKS                                      * WNRCMR
330000090213     C                   Z-ADD     1             RDENRP
330100090213     C                   ELSE
330200090213     C                   Z-ADD     2             RDENRP
330300090213     C                   END
330400090213     C                   MOVEL     'TD1153'      RDENMC
330500090213     C                   MOVEL     'AFB'         RDEVAL
330600090213     C                   WRITE     EDRDE000                             99
330700090213     c   99              eval      errori_in_Wrt = 'S'
330800090213     C                   END
330900090213     C*  Memorizzo la data
331000090213     C                   CLEAR                   EDRDE000
331100090213     C                   MOVEL     WKSC          KLNP1
331200090213     C                   MOVEL     'TD2380'      KNMC
331300090213     C                   Z-ADD     1             KNRP
331400090213     C     KRDE          CHAIN     EDRDE01L                           31
331500090213     C     *IN31         IFEQ      '1'
331600090213     C                   Z-ADD     VABAAS        RDEAAS
331700090213     C                   Z-ADD     VABLNP        RDELNP
331800090213     C                   Z-ADD     VABNRS        RDENRS
331900090213     C                   Z-ADD     VABNSP        RDENSP
332000090213     C     §PTCM1        IFNE      'S'
332100100630     C     UNB_NrCMR     OREQ      *BLANKS                                      * WNRCMR
332200090213     C                   Z-ADD     1             RDENRP
332300090213     C                   ELSE
332400090213     C                   Z-ADD     2             RDENRP
332500090213     C                   END
332600090213     C                   MOVEL     'TD2380'      RDENMC
332700090213     C                   MOVEL     WDATFV        RDEVAL
332800090213     C                   WRITE     EDRDE000                             99
332900090213     c   99              eval      errori_in_Wrt = 'S'
333000090213     C                   END
333100090213     C*
333200090213     C                   END                                                    §PTCM1 = 'S'
333300090213     C*
333400090213     C                   ENDSR
333500090213     c*==================================================================*
333600090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
333700090213     c*==================================================================*
333800090213     c     Imp_ADDR_mail begsr
333900090213      *
334000090213     c                   clear                   Indirizzi
334100090213      *
334200090213      *  Lunghezza indirizzi presenti
334300090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
334400090213     c                   z-add     1             al_char           3 0
334500090213     c                   z-add     1             dal_char          3 0
334600090213     c                   z-add     1             tot_char          3 0
334700090213      *
334800090213     c     successivo    tag
334900090213      *
335000090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
335100090213      *   e aggiunge il dominio Bartolini + il (;) separatore
335200090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
335300090213     c                   eval      tot_char = al_char - dal_char
335400090213     c                   z-add     dal_char      Dac
335500090213     c                   z-add     tot_char      Luc
335600090213      *
335700090213     c                   clear                   Indir_Bartol     40
335800090213     c                   clear                   Indir_Parz       20
335900090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
336000090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
336100090213     c                             %Trim(bart_it)
336200090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
336300090213      *
336400090213     c                   if        al_char = Lun_Ind
336500090213     c                   goto      fine_indmail
336600090213     c                   else
336700090213     c                   eval      dal_char = ( al_char + 1 )
336800090213     c                   goto      successivo
336900090213     c                   end
337000090213      *
337100090213     C     fine_indmail  TAG
337200090213      *
337300090213     C                   ENDSR
337400090213     c*==================================================================*
337500090213      * Manda un Msg x E-mail
337600090213     c*==================================================================*
337700090213     c     Invio_Mail    begsr
337800090213      *
337900090213     c                   goto      non_inviare
338000090213      *
338100090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
338200090213     C* Inizializzo variabili
338300090213     C                   movel     *blanks       wrkEml          253
338400090213     C                   movel     *blanks       wrkMsg         5000
338500090213     C                   movel     *blanks       wrkOgg           44
338600090213      *
338700090213     C* Valorizzo i campi della e-m@ail - indirizzo
338800090213     C                   eval      wrkEml = Indirizzi
338900090213     C                   eval      wrkOgg = Oggetto
339000090213     C                   eval      wrkMsg = Messaggio
339100090213
339200090213     C                   call(e)   'TIS701C'
339300090213     C                   parm                    wrkEml
339400090213     C                   parm                    wrkOgg
339500090213     C                   parm                    wrkMsg
339600090213
339700090213     c     non_inviare   tag
339800090213     C*
339900090213     C                   ENDSR
340000090601     c*==================================================================*
340100090601      * Manda un Msg x E-mail a EDPAB
340200090601     c*==================================================================*
340300090601     c     Invio_Mail_AB begsr
340400090601      *
340500090601     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
340600090601     C* Inizializzo variabili
340700090601     C                   movel     *blanks       wrkEml          253
340800090601     C                   movel     *blanks       wrkMsg         5000
340900090601     C                   movel     *blanks       wrkOgg           44
341000090601      *
341100090601     C* Valorizzo i campi della e-m@ail - indirizzo
341200110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
341300110203     C                   eval      wrkEml = Indirizzi
341400090601     C                   eval      wrkOgg = Oggetto
341500090601     C                   eval      wrkMsg = Messaggio
341600090601
341700110203      ********
341800110203     C********           call(e)   'TIS701C'
341900110203     C********           parm                    wrkEml
342000110203     C********           parm                    wrkOgg
342100110203     C********           parm                    wrkMsg
342200110203     C*
342300110203      ** *****
342400110203     C                   call(e)   'TRTCT00R2'
342500110203     C                   parm                    wrkEml
342600110203     C                   parm                    wrkOgg
342700110203     C                   parm                    wrkMsg
342800110203     C*
342900090601     C*
343000090601     C                   ENDSR
343100090211     ***********************************************************************
343200090211     **
343300090211     ** Send Message (QEZSNDMG) API
343400090211     **
343500090211     ***********************************************************************
343600090211     C     CalQEZSNDMG   BEGSR
343700090211      *
343800090211     ** Invio messaggio all'utente.
343900090211     C                   EVAL      SndMg03 = msgDSP
344000090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
344100090211     C                   EVAL      SndMg05(Jx) = User_msg
344200090211     C                   EVAL      SndMg06 = Jx
344300090211     C                   CLEAR                   QUSEC
344400090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
344500090211
344600090211     C                   CALL      'QEZSNDMG'
344700090211     C                   PARM                    SndMg01
344800090211     C                   PARM                    SndMg02
344900090211     C                   PARM                    SndMg03
345000090211     C                   PARM                    SndMg04
345100090211     C                   PARM                    SndMg05
345200090211     C                   PARM                    SndMg06
345300090211     C                   PARM                    SndMg07
345400090211     C                   PARM                    SndMg08
345500090211     C                   PARM                    Qusec
345600090211     C                   PARM                    SndMg10
345700090211     C                   PARM                    SndMg11
345800090211     C                   PARM                    SndMg12
345900090211      *
346000090211     C                   ENDSR
346100121105      *---------------------------------------------------------------*
346200121105      * CTRL caricamento schiere    PT                               -*
346300121105      *---------------------------------------------------------------*
346400121105     C     Check_PT      begsr
346500121105     C*
346600121105     c* Controllo riempimento schiera
346700121105     c                   clear                   trul0sds
346800121105     c                   eval      i0sski='CPT'
346900121105     c                   eval      i0sele=%elem(CPT)
347000121105     c                   eval      i0spie=x
347100121105     c                   eval      i0sfile='EDTAB00F'
347200121105     c                   eval      i0ssif=knsif
347300121105     c                   eval      i0spgm='TRTCT08R'
347400121113     c                   move      kpjbu         SvKpjbu
347500121113     c                   clear                   kpjbu
347600121105     c                   movel     trul0sds      kpjbu
347700121105     c                   call      'TRUL0SR'
347800121105     c                   parm                    kpjba
347900121113     c                   eval      kpjbu  =  SvKpjbu
348000121105     C*
348100121105     C                   ENDSR
348200090211     O*****************************************************************
348300090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
348400090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
348500090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
348600090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
