000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600090224      *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT       con  IFTMIN        *
000700090224      *                   primo cliente NILFISK              con  IFTMIN        *
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
007100100719     D Trovato_seg     S              1
007200100716      *
007300100716     d keyUNBCLI       s             35
007400100716     d keyTIPOMSG      s              6
007500100716     d keyVERSION      s              3
007600100716     d keyRELEASE      s              3
007700100716     d keyAGENCY       s              3
007800100716     d keyASSOCIA      s              6
007900100716      *
008000100719     D DsGenerica      s           2048
008100090209     D segmento        s           2048
008200090209     D Errore          s              1
008300000223     D W0140           S             14  0
008400991129     D WORA            S              6  0
008500991129     D XX              S              3  0 INZ
008600110328     d Key_Generica35  s             35
008700991129     D WDTGIO          S              8  0
008800991129     D DATEU           S              8  0
008900991129     D DATA_eur        S               D   DATFMT(*eur)
009000050112     D NUM_Sped        s                   LIKE(vabnsp)
009100090213     D NUM_RECord      s                   LIKE(vnREC)
009200090213     D SAVNUM_RECord   s                   LIKE(vnREC)
009300090213     D Last_RECord     s                   LIKE(vnREC)
009400050112     D svkpjbu         s                   like(kpjbu)
009500090209      *---------------------------------------------------------------------*
009600090209     D KPJBA         E DS
009700090209     D FNLV55DS      E DS
009800090209     D TIBS55DS      E DS
009900090209     D TISI95DS      E DS
010000090209     D UTEDSE0F      E DS
010100090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
010200090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
010300090209      * Ds scomposzione tipo capoconti
010400090209     D TCUDS           DS
010500090209     D  F34                    3      4
010600090209     D  F56                    5      6
010700090209     D DS1B          E DS
010800090209     D DS15          E DS
010900090209     D DSCV          E DS
011000090213     D TRTC86DS      E DS
011100090209     D EDIDSPT       E DS
011200090209     D EDIDSPU       E DS
011300090209     D EDIDSPV       E DS
011400090211     D EDIDSCL       E DS
011500090216     D EDIDSTC       E DS
011600090209      **
011700090209      *  DS x salvataggio dati impostati in EDiVAB
011800090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
011900090209     D SALVA           DS           545
012000090210      **
012100090209     D WLBDA8          DS
012200090209     D  G02DAT                 1      8  0
012300090209     D  G02INV                 9     16  0
012400090209     D  G02ERR                17     17
012500090209     D  G02TGI                18     22  0
012600090209      *  DS x scomposizione numero spedizione
012700090209     D DSSPE           DS
012800090209     D  SPEAAS                 1      4  0
012900090209     D  SPELNP                 5      7  0
013000090209     D  SPENRS                 8      9  0
013100090209     D  SPENSP                10     16  0
013200090209      *  DS x scomposizione segnacollo
013300090209     D DSSGN           DS
013400090209     D  SGNLNP                 1      3  0
013500090209     D  SGNLNA                 4      6  0
013600090209     D  SGNNRS                 7      8  0
013700090209     D  SGNNSC                 9     15  0
013800090209     D  SGNZNC                16     17  0
013900090209      *  DS x stampa segnacollo
014000090209      *---------------                                                      *
014100090211     D  vablnp_PT      s                   LIKE(vablnp)
014200090212     D  vabnrs_PT      s                   LIKE(vabnrs)
014300090211     D  vabctm_PT      s                   LIKE(vabctm)
014400090209     D Wvabtic         s                   LIKE(vabtic)
014500090209     C*
014600100719???? D seg_imprevisto...
014700100719???? D                 s              1
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
017300121102      ***
017400121102     D TRUL0SDS      E DS
017500090209      * Tabella partner
017600121102     D CPT             S             35    DIM(200)
017700121102     D CPTparz         S             35    DIM(%Elem(CPT))
017800121102     D KSC_UNB         S              7    DIM(%Elem(CPT))
017900121102     D DPT             S             90    DIM(%Elem(CPT))
018000121102     D DPU             S             90    DIM(%Elem(CPT))
018100121102     D DPV             S             90    DIM(%Elem(CPT))
018200121102      *-----
018300121102     d sav_CPTx        s              3  0 inz(0)
018400121102     d Ptn_parziale    s              1    inz('N')
018500090209      *
018600090209      * Tabella CL --> codici clienti - tariffa
018700130305     D CCL             S             35    DIM(999)
018800130305     D DCL             S             90    DIM(999)
018900090209      * Schiere x caricamento cod.cliente  <>
019000130305     D KCL             S              7  0 DIM(999)
019100090209      * Termini di consegna
019200110328     D K35_TpBolla     S             35    DIM(100)
019300110328     D Cod_TpBolla     S              3    DIM(100)
019400110328     D Decod_Porto     S              1    DIM(100)
019500110328     d Trovata_Tab_TB  S              1
019600110328      *
019700090216      * Tipo pagamento C/Assegno
019800090216     D CTC35           S             35    DIM(100)
019900090216     D TPI             S              2    DIM(100)
020000090209      * Decodifiche servizi speciali
020100090209     D SS35            S             35    DIM(100)
020200090209     D SS              S              3    DIM(100)
020300090209     D DSS             S             90    DIM(100)
020400090209      * Schiere x routine di conversione CAP
020500090209     D CAP5            S              1    DIM(5)
020600090209     D CAP9            S              1    DIM(9)
020700090209     D CAPM            S              1    DIM(9)
020800090209      * Schiera per memorizzazione FTX ricevuti
020900090209     D QUA             S              3    DIM(100)
021000090209     D FTX             S             70    DIM(100)
021100090209      * Tabelle capiconto
021200090209      * Schiere x località
021300090209     D LOD             S              1    DIM(35)
021400060608      **?------------------------------------------------------------------ */
021500060608     C*? Ds Scrittura del VAT "E"  passaggio nuovi campi BIC3
021600060608      **?------------------------------------------------------------------ */
021700060612     D XTOEBCDDS     E DS
021800060620      *
021900060608      **?------------------------------------------------------------------ */
022000050112      * Numeratore Bolle (302)
022100050112     D trul33ds      E DS
022200050112     D Ds3C          E DS
022300090209     C*
022400090209      **?------------------------------------------------------------------ */
022500090209     C* Definizione campi di work
022600090209     D pVLB            s                   LIKE(VABVLB)
022700090209     D kKUT            s                   LIKE(TBLKUT)
022800090209     D kCOD            s                   LIKE(TBLCOD)
022900090209     D kKEY            s                   LIKE(TBLKEY)
023000090209     D kKCC            s                   LIKE(ACOKCC)
023100090209     D kKSC            s                   LIKE(ACOKSC)
023200090209     D kAAA            s                   LIKE(NUFAAA)
023300090209     D kCNU            s                   LIKE(NUFCNU)
023400090209     D kFIL            s                   LIKE(NUFFIL)
023500090209     D kAAS            s                   LIKE(VABAAS)
023600090209     D kLNP            s                   LIKE(VABLNP)
023700090209     D kNRS            s                   LIKE(VABNRS)
023800090209     D kNSP            s                   LIKE(VABNSP)
023900090209     D kCMR            s                   LIKE(VABCMR)
024000090209     D kCCM            s                   LIKE(VABCCM)
024100090209     D kNCD            s                   LIKE(VADNCD)
024200090210     D kCNT            s                   LIKE(VADCNT)
024300090209     D kNMC            s                   LIKE(RDENMC)
024400090209     D kNRP            s                   LIKE(RDENRP)
024500090209     D kLNP1           s                   LIKE(RDELNP)
024600090209      *
024700090209      *  Invio E-mail
024800090209     D Indirizzi       s            253
024900090209     D Oggetto         s             44
025000090209     D Messaggio       s           5000
025100090209      *
025200090209     d   DSmail        ds
025300090209     D Mail_Ind                      44
025400090209     d   IND_char                     1    dim(44)
025500090209      *
025600090209     D Lun_Ind         s              5u 0
025700090209      *
025800090209     D Dac             s              5u 0
025900090209     D Luc             s              5u 0
026000090209      *
026100090209     C*---------------------------------------------------------------*
026200090209     D MsgDSP          S            256                                         Testo generico
026300090209     C*---------------------------------------------------------------*
026400090209     D SndMg01         S             10                                         Message type
026500090209     D                                     INZ('*INFO')
026600090209     D SndMg02         S             10                                         Deluvery mode
026700090209     D                                     INZ('*BREAK')
026800090209     D SndMg03         S            256                                         Message text
026900090209     D SndMg04         S             10I 0                                      Length of text
027000090209     D                                     INZ(%SIZE(SndMg03))
027100090209     D SndMg05         S             10                                         User profile
027200090209     D                                     DIM(299)
027300090209     D SndMg06         S             10I 0                                      Number of user
027400090209     D SndMg07         S             10I 0                                      Message sent indic.
027500090209     D SndMg08         S             10I 0                                      Function requested
027600090209     D SndMg10         S              1                                         Show display
027700090209     D                                     INZ('N')
027800090209     D SndMg11         S             20                                         Qualified MSGQ name
027900090209     D SndMg12         S              4                                         Name type indicator
028000090209     D                                     INZ('*USR')
028100090209      * indice di scihera
028200090209     D Jx              S              5I 0
028300090209      *
028400090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
028500090209      *
028600090209     d bart_it         C                   CONST('@Bartolini.it;')
028700090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
028800060612      * ?================================================================== */
028900060612      *
029000060612      * ?   * Campi x decodifica * (INPUT  del Record)
029100090130     D  Dati           s           2048
029200060612      * ?   *--------------------------------------------------------------*
029300060612     D  se_errore      s              1    inz(' ')
029400060612      * ?* ------------------------------------------------------ *
029500060710     D Digits          C                   '0123456789'
029600060612      * ?================================================================== */
029700060612      *   Ciclo principale
029800090205      * ?================================================================== */
029900060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
030000060612     C*
030100060612     C                   if        not %open(tivin00r)
030200060612     C                   open      tivin00r
030300060612     C                   endif
030400060612      **
030500060612      * ?------------------------------------------------------------------ */
030600060612     C*? Controllo dati arrivati da DPD
030700060612      * ?------------------------------------------------------------------ */
030800060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
030900060612      * ?- controllando che la trasmissione sia completa.
031000090205      **
031100060612      * ?              /*---------------------- */
031200090205     c                   goto      nonFare_adesso
031300060612     c                   exsr      check_Trasm
031400090205     c     nonFare_adessotag
031500060612      * ?              /*---------------------- */
031600090205      **
031700060613      **  Errore di trasmissione x tutti i records
031800060613      **   --> file in errore
031900060613     c                   if        se_errore = 'S'
032000060612
032100060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
032200090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
032300090205     c                   exsr      Msg_errato
032400060612      **
032500060612     c                   else
032600060614      **
032700060612      * ?------------------------------------------------------------------ */
032800060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
032900060612      * ?------------------------------------------------------------------ */
033000090205      **
033100060612      * ?              /*---------------------- */
033200060612     c                   exsr      Importa_Msg
033300060612      * ?              /*---------------------- */
033400060612
033500060614     c                   end
033600060614
033700060612     C                   if        %open(tivin00r)
033800060612     C                   close     tivin00r
033900060612     C                   endif
034000060612      *
034100060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
034200090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
034300090225     c                             esito ='0'
034400060710     C                   eval      esito ='0'
034500090224      *
034600090224      *  Il cliente vuole il CHECK di ricevuto della trasmissione
034700090224     c                   call      'TRTCT03R'
034800090224     c                   parm                    Mittente         35
034900090226     c                   parm                    Mitt_Qualifier    4
035000090226     c                   parm                    Id_Messaggio     14
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
043800090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
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
046800090211     C                   MOVEL     *BLANKS       WNUMFV           35
046900090211     C                   MOVEL     *BLANKS       WNRCMR           35
047000090211     C                   Z-ADD     0             WDATPA            8 0
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
052200090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
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
054600090226     C                   eval      Mittente       = unb0004
054700090226     C                   eval      Mitt_qualifier = unb000720
054800100719     C                   eval      Id_Messaggio   = UNB0020
054900090209      *
055000090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
055100090209      *  ricevuti:
055200090209     c                   movel     §PVema        mail_ind         44
055300090209      *
055400090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
055500090209     c                   if        mail_ind <> *blank
055600090209     c                   exsr      imp_ADDR_mail
055700090209     c                   end
055800090209      *
055900090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
056000090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
056100090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
056200090209     C                   if        §PULUN = *blank
056300090209     C                   eval      §PULUN = '00'
056400090209     c                   end
056500090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
056600090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
056700090209      *
056800090209     C                   Z-ADD     §PTKSC        WKSC              7 0
056900090211     C                   MOVEL     §PTLNP        VABlnp_PT
057000090212     C                   MOVEL     §PTNRS        VABnrs_PT
057100090211     C                   MOVEL     §PTCTM        VABctm_PT
057200090209      *
057300090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
057400090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
057500090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
057600090209      *  i records trasmessi).
057700090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
057800090209      *
057900090209      *  In base al cod.trattamento merce reperisco tipo tratt.
058000090209     C                   CLEAR                   DS1B
058100090209     C                   Z-ADD     1             Y                 3 0
058200090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
058300090209     C     *IN32         IFEQ      '1'
058400090209     C                   MOVEL     DTM(Y)        DS1B
058500090209     C                   MOVEL     DS1B          WSVTRM           38
058600090209     C                   END
058700090209      *
058800090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
058900090209     C                   clear                   ACORAG
059000090209     C                   clear                   INDCAE
059100090209     C                   clear                   INDCAP
059200090209     C                   clear                   INDSTA
059300090209      * Decodifico partner
059400090209     C                   Z-ADD     1             KKUT
059500090209     C                   Z-ADD     KCI           KKCC
059600090209     C                   MOVE      §PTKSC        KKSC
059700090209     C     KACO          CHAIN     CNACO00F                           31
059800090209     C     KIND          CHAIN     CNIND00F                           31
059900090211      *
060000090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
060100090211      *   questa schiera è utilizzata anche per i clienti.
060200090211     C     §ptKSC        LOOKUP    KCL                                    39
060300090211     c                   If        *in39 = *off
060400090211     C                   add       1             WW
060500090211     C                   z-add     §ptKSC        KCL(WW)
060600090211     C                   Endif
060700090209      *
060800090209     C                   MOVEL     unb0017       WAA4              4 0
060900090209     C                   MOVE      WAA4          WAA2              2 0
061000090209     C                   MOVE      unb0017       WGG               2 0
061100090209     C                   MOVE      unb0017       WDTPRE            8 0
061200090209     C                   MOVE      unb0019       WHMPRE            4 0
061300090209     C                   MOVE      unb0017       WDTMSG            6 0
061400090209     C                   MOVE      WAA2          WDTMSG
061500090209     C                   MOVEL     WGG           WDTMSG
061600090210      * potremmo
061700090210      * Utilizzare l'identificativo della trasmissione come CMR
061800100719     C                   IF        unb0020 <> *blank
061900100719     C                   MOVEL     unb0020       WNRDOC
062000100719     C                   MOVEL     unb0020       WNUMFV
062100100719     C                   MOVEL     unb0020       WNRCMR
062200090209     c                   end
062300090209      *
062400090209     c                   Endif
062500090209      **
062600090210     c     end_UNB       endSR
062700090209      * ?================================================================== */
062800090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
062900090209      * ?================================================================== */
063000090227     C     Nuova_spediz  BEGSR
063100090227      **
063200090227      * Pulisco campi del file
063300090227     c                   z-add     vnrec         SAVNUM_RECord
063400090227     C                   CLEAR                   EDiVAB00
063500090227     C                   CLEAR                   EDiVAT00
063600090227     C                   CLEAR                   EDiVAD00
063700090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
063800090227     C                   MOVEL     'N'           WGID99            1
063900090227     C                   clear                   XY                5 0
064000090227     C                   clear                   §§                5 0
064100090227     C                   clear                   WS1496            5 0
064200090227     C                   clear                   WNSP             35
064300090227     C                   Z-ADD     *ZEROS        PVLB
064400090227     C                   MOVEA     *HIVAL        SGN
064500090227     C*  Azzero codice cliente - tariffa
064600090227     C                   Z-ADD     0             WTOCOC           16 0
064700090227     C                   Z-ADD     0             WTOPLC           16 2          ???
064800090227     C                   Z-ADD     0             WTOPNC           16 2          ???
064900090227     C*  Azzero codice cliente - tariffa
065000090227     C                   clear                   WCLKSC
065100090227     C                   clear                   WCLTAR
065200090227     C                   clear                   WCLLNP
065300090227     C                   clear                   WCLNRS
065400090227     C                   clear                   WCLCTM
065500090227     C                   clear                   WCLBS1
065600090227     C                   clear                   WCLfnsp                        *blk/S
065700090227     C                   clear                   WNOT1            35
065800090227     C                   clear                   WNOT2            35
065900160405     C                   clear                   WNatMerce        15
066000090227     C*  Campi di work
066100090227     c                   clear                   wVabTIC
066200090227     c                   clear                   wri_vabtic        1
066300090227      *
066400090227     C* pulisce campi di work
066500090227     c                   clear                   watrde           35
066600090227     c                   clear                   wattel           25
066700090227     C                   clear                   WRMN              3
066800090227      *
066900090227      * Un errore nel dettaglio
067000090227     C                   clear                   Err_in_detta      1
067100090227     c                   clear                   errori_in_Wrt     1
067200090227     c                   clear                   se_trovato_NAD    1
067300091204     C                   clear                   Rifer_Princip    35
067400160429     C                   clear                   Contatto_NAD      1
067500160429     C                   clear                   Comunica_NAD      1
067600090227      **
067700090227     c                   endSR
067800090227      * ?================================================================== */
067900090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
068000090227      * ?================================================================== */
068100090227     C     DATI_UNH      BEGSR
068200090227      *
068300090227      *  Prepara una nuova spedizione
068400090227      * ?                         --------------
068500090227     c                   exsr      Nuova_spediz
068600090227      * ?                         --------------
068700090211      *
068800090211      ** Riferimento del cliente
068900090211     C                   MOVEL     unh0062       WPRGSP           35
069000090211     C                   MOVEL     unh0062       WNSP             35
069100090211     C                   MOVEL     unh0062       WNUM             35
069200090211      * ?                                       --------
069300090211     C                   MOVEL     unh0062       VABRMA
069400090211      * ?                                       --------
069500090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
069600090211     C                   SUB       1             WLUNGH
069700090211     C                   EXSR      TSTNUM
069800090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
069900090211      * ?                                       --------
070000090211     C                   MOVEL     WNUMOK        VABRMN
070100090211      * ?                                       --------
070200090211     C                   END
070300090210      **
070400090303     C                   MOVEL     VABlnp_PT     VABLNP
070500090303     C                   MOVEL     VABnrs_PT     VABNRS
070600090303     C                   MOVEL     VABctm_PT     VABCTM
070700090303      **
070800090303      **
070900090210     c     end_UNH       endSR
071000090210      * ?================================================================== */
071100090210     C*? Impsota i campi del VAB e del VAT da scrivere a rottura dettaglio
071200090210      * ?================================================================== */
071300090210     C     DATI_DTM      BEGSR
071400090210     C*
071500090209     C*  Se data documento  memorizzo numero e data x CMR
071600090209     C     dtm2005       IFEQ      '137'
071700090209     C                   MOVEL     dtm2380       WDATA            35            Data
071800090209     C                   MOVEL     dtm2379       WFORM             3            Formato
071900090209     C                   EXSR      CNVDAT
072000090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
072100090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
072200090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
072300090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
072400090209     C     WERRO         IFEQ      'S'
072500090210     C                   eval      vinMSG = 'DTM data errata'
072600090213     c                   exsr      Error_DET
072700090210     c                   goto      end_DTM
072800090209     C                   END
072900090209     C                   END
073000090209      **
073100090209     C*  Se data consegna richiesta imposto già sui campi del VAB
073200160506     C     dtm2005       IFEQ      'Z73'
073300090209     C                   MOVEL     dtm2380       WDATA            35            Data
073400090209     C                   MOVEL     dtm2379       WFORM             3            Formato
073500090209     C                   EXSR      CNVDAT
073600090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
073700090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
073800090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
073900090209     C     WERRO         IFEQ      'S'
074000090210     C                   eval      vinMSG = 'DTM data errata'
074100090213     c                   exsr      Error_DET
074200090210     c                   goto      end_DTM
074300090209     C                   END
074400090209     C                   END
074500090211      **
074600090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
074700090211     C     dtm2005       IFEQ      '11'
074800090211     C                   MOVEL     dtm2380       WDATA            35            Data
074900090211     C                   MOVEL     dtm2379       WFORM             3            Formato
075000090211     C                   EXSR      CNVDAT
075100090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
075200090211     C     WERRO         IFEQ      'S'
075300090211     C                   eval      vinMSG = 'DTM data errata'
075400090213     c                   exsr      Error_DET
075500090211     c                   goto      end_DTM
075600090211     C                   END
075700090211     C                   END
075800090209      **
075900090210     c     end_DTM       endSR
076000090209      * ?================================================================== */
076100090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
076200090209      * ?================================================================== */
076300090209     C     DATI_TOD      BEGSR
076400090209      *
076500090210      *  Trascodifico tipo trasporto
076600090210     c                   clear                   TOD_char3         3
076700090210      *
076800090210      *  imposta il default dalla tabella PT se c'è
076900090210     C     §puPFA        ifNE      *blank
077000090210     c                   SELECT
077100090210      *  Porto Franco
077200090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
077300090210     C                   movel     '4'           VABCBO                         + C/Ass
077400090210     c                   WHEN      §puPFA = 'F'
077500090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
077600090210      *  Porto Assegnato
077700090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
077800090210     C                   movel     '6'           VABCBO                         + C/Ass
077900090210     C                   WHEN      §puPFA = 'A'
078000090210     C                   movel     '2'           VABCBO                         Senza C/Ass
078100090210     C                   ENDSL
078200090210     c                   end
078300090210      *
078400090210      *   Imposta o il codice o il metodo
078500090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
078600090210     c                   if        tod4053  <> *blank
078700090210     c                   movel(p)  tod4053       TOD_char3
078800090210     c                   else
078900090210     c                   if        tod4215  <> *blank
079000090210     c                   movel(p)  tod4215       TOD_char3
079100090210     c                   end
079200090210     c                   end
079300110328      ****************
079400110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
079500110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
079600110328      ****************
079700110328     c                   eval      Trovata_Tab_TB ='N'
079800110328      *
079900110328     c                   if              TOD_char3 <> *blank
080000110328     c                   movel(p)  TOD_char3     Key_Generica35
080100110328     c                   move      §puTB         Key_Generica35
080200110328     C                   eval      Y = 1
080300110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
080400110328     c   32              eval      Trovata_Tab_TB ='S'
080500110328      *
080600110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
080700110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
080800110328      *
080900110328     c                   if          Trovata_Tab_TB ='N'
081000110328     c                                 and
081100110328     C                               tod4215 <> *blank
081200110328     c                   movel(p)  tod4215       TOD_char3
081300110328     c                   movel(p)  TOD_char3     Key_Generica35
081400110328     c                   move      §puTB         Key_Generica35
081500110328     C                   eval      Y = 1
081600110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
081700110328     c   32              eval      Trovata_Tab_TB ='S'
081800110328     c                   end
081900110328     c                   end
082000110328      *
082100110328      ****************
082200110328      ******* Se vi sono eccezioni sul cliente imposta quelle
082300110328      ****************
082400110328     c                   If          Trovata_Tab_TB ='S'
082500110328      *
082600110328     c                   SELECT
082700110328     c                   WHEN      Decod_Porto(Y) = 'F'
082800110328     c                               and  VABcas > *zeros
082900110328     C                   movel     '4'           VABCBO                         + C/Ass
083000110328     c                   WHEN      Decod_Porto(Y) = 'F'
083100110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
083200110328      *  Porto Assegnato
083300110328     c                   WHEN      VABCAS > *zeros
083400110328     C                   movel     '6'           VABCBO                         + C/Ass
083500110328     C                   OTHER
083600110328     C                   movel     '2'           VABCBO                         Senza C/Ass
083700110328     C                   ENDSL
083800110328      *
083900110328      ****************  altrimenti
084000110328      ******* Se non vi sono eccezioni per il cliente
084100110328      *   Rileva a questo punto lo Standard per tutti
084200110328      ****************
084300110328     c                   elseIf      Trovata_Tab_TB ='N'
084400090210      **
084500090210     c                   if              TOD_char3 <> *blank
084600090210     c                   movel(p)  TOD_char3     fld35a           35
084700090213     C                   movel     UNB_Mittente  etbUNB
084800090213     C                   movel(p)  'TOD'         etbSGM
084900090213     C                   movel(p)  TOD_char3     etbVALSGM
085000090213     c     K_TAB1L       chain     edstbl01l
085100090210      *
085200090210      * prova quindi senza l'UNB specifico del cliente
085300090213     c                   if        not %Found(edstbl01l)
085400090213     C                   clear                   etbUNB
085500090213     c     K_TAB1L       chain     edstbl01l
085600090210     c                   end
085700090210      *
085800090213     c                   if        %Found(edstbl01l)
085900090210     c                   SELECT
086000090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
086100090210     C                   movel     '4'           VABCBO                         + C/Ass
086200090213     c                   WHEN      ETBDATI = 'F'
086300090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
086400090210      *  Porto Assegnato
086500090210     c                   WHEN      VABCAS > *zeros
086600090210     C                   movel     '6'           VABCBO                         + C/Ass
086700090210     C                   OTHER
086800090210     C                   movel     '2'           VABCBO                         Senza C/Ass
086900090210     C                   ENDSL
087000090210      *
087100090210     c                   end
087200090210     c                   endIF
087300110328      **
087400110328     c                   ENDif
087500090209      **
087600090210      **  Il tipo bolla non è presente
087700090210     c                   if        VABCBO = *blank
087800160427     C**********         eval      vinMSG = 'TOD non rilevato'
087900160427     c**********         exsr      Error_DET
088000160427     c**********         goto      end_TOD
088100090210     c                   end
088200160427      ** Sempre FRANCHI
088300160427      **
088400090210     c     end_TOD       endSR
088500090210      * ?================================================================== */
088600090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
088700090210      * ?================================================================== */
088800090210     C     DATI_RFF      BEGSR
088900090210      *
089000090210     C                   clear                   WAGE             35
089100110224     C                   clear                   WFF              35
089200110224     C                   clear                   WCU              35
089300091204      *
089400091204      *   Se manca sugli altri riporta sempre il principale per non bloccare il messaggio
089500091204     c                   if        RFF1154 = *blank and Rifer_Princip <> *blank
089600091204     c                   eval      RFF1154 = Rifer_Princip
089700091204     c                   end
089800090210      *
089900090210      *  Trascodifico riferimento spedizione
090000160512     c                   if        RFF1153 = 'ON' and RFF1154 <> *BLANKS
090100090210     C                   movel(p)  RFF1154       WAGE             35
090200091204     C                   movel(p)  RFF1154       Rifer_Princip    35
090300090210     c                   end
090400090210      **
090500090210     C                   IF         WAGE <> *BLANKS
090600090210     C                   movel     WAGE          WNSP             35
090700090210     C                   movel     WAGE          WNUM             35
090800090210     C     ' '           scan      WAGE          WLUNGH            3 0
090900090210     C                   sub       1             WLUNGH
091000090210     C                   EXSR      TSTNUM
091100090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
091200090210     C                   movel     WAGE          VABRMA
091300160512     C                   IF         WRMN = 'ON' or WRMN = *blanks
091400090210     C                   movel     WNUMOK        VABRMN
091500160512     C                   movel     'ON'          WRMN
091600090210     C                   ENDIF
091700090210     C                   ENDIF
091800090210     C                   ENDIF
091900090210      *
092000090210      * Codice Cliente di riferimento
092100090210     C                   if        RFF1153 = 'IT ' and RFF1154 <> *BLANKS
092200090210     C                   movel     RFF1154       WFF
092300090210     c                   END
092400090211      *
092500090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
092600090211     C     WFF           IFNE      *BLANKS
092700090211     C                   eval      XX = 1
092800090211     C                   movel     §PTKSC        WCCL             35
092900090211     C                   movel     WFF           WCOM28           28
093000090211     C                   move      WCOM28        WCCL
093100090211     C     WCCL          lookup    CCL(XX)                                34
093200090211     C     *IN34         IFEQ      '1'
093300090211     C                   movel     DCL(XX)       EDIDSCL
093400090211     C                   z-add     §CLKSC        WCLKSC            7 0
093500090211     C                   movel     §CLTAR        WCLTAR            3
093600090211     C                   z-add     §CLLNP        WCLLNP            3 0
093700090211     C                   z-add     §CLNRS        WCLNRS            2 0
093800090211     C                   movel     §CLCTM        WCLCTM            2
093900090211     C                   movel     §CLBS1        WCLBS1            1
094000090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
094100090211      * imposta la LNP   x il dettaglio specifco
094200090211     c                   eval      VABlnp = §CLlnp
094300090211      * imposta la Serie x il dettaglio specifco
094400090211     c                   eval      VABnrs = §CLnrs
094500090211      *
094600090211      *  In base al cod.trattamento merce reperisco tipo tratt.
094700090211      *    per un dettaglio specifico.
094800090211     C                   CLEAR                   DS1B
094900090211     C                   Z-ADD     1             Y                 3 0
095000090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
095100090211     C     *IN32         IFEQ      '1'
095200090211     C                   MOVEL     DTM(Y)        DS1B
095300090211     C                   END
095400090211      *
095500090211      * Se il cliente non era sulla schiera lo aggiunge
095600090211     C     §CLKSC        lookup    KCL                                    39
095700090211     C                   if        *in39 = *off
095800090211     C                   add       1             WW
095900090211     C                   z-add     §CLKSC        KCL(WW)
096000090211     c                   endif
096100090211      *
096200090211     C                   ENDIF
096300090211     C                   ENDIF
096400090211     C*----------------------------------------------------------------
096500090210      *
096600160512     C                   if        RFF1153 = 'BN ' and RFF1154 <> *blanks
096700090210     C                   movel     RFF1154       WCU
096800090210     C                   END
096900090210      *
097000090210      * Numero Ordine di vendita
097100090210     C     WCU           IFNE      *BLANKS
097200090210     C                   movel     WCU           WNUM             35
097300090210     C     ' '           scan      WCU           WLUNGH
097400090210     C                   sub       1             WLUNGH
097500090210     C                   EXSR      TSTNUM
097600090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
097700090210     C                   movel     WNUMOK        VABRMN
097800160512     C                   movel     'BN '         WRMN
097900090210     C                   Endif
098000090210     C                   ENDIF
098100090210      *
098200090210      * NOn c'è l'identificativo bolla del cliente
098300090211     c                   if        (RFF1153 = 'BN' and RFF1154 = *BLANKS) or
098400160405     c                             (RFF1153 = 'ON' and RFF1154 = *BLANKS)
098500090210     C                   eval      vinMSG = 'RFF riferimento assente'
098600090213     c                   exsr      Error_DET
098700090210     c                   goto      end_RFF
098800090210     C                   ENDIF
098900090210      *
099000090210     c     end_RFF       endSR
099100090209      * ?================================================================== */
099200090213     C*?  C.O.D. Monetary Amount
099300090209      * ?================================================================== */
099400090213     C     DATI_MOA      BEGSR
099500090210      *
099600090216     C* Controllo se campo importo numerico
099700090216      * con 3 decimali
099800090216     C     moa5004       IFNE      *zeros
099900090216      *
100000090216     C     moa5025       IFEQ      '157'
100100090216     C     moa5004       div       1000          VABIAS                         IMPORTO
100200090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
100300090216     C                   END
100400090216      *
100500090216     C     moa5025       IFEQ      '77 '                                        Valore
100600090216     C     moa5004       div       1000          VABVMD                         MERCE
100700090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
100800090216     C                   END
100900090216      *
101000090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
101100090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
101200090216     C     moa5004       div       1000          VABCAS
101300090216     C                   MOVEL     moa6345       VABVCA
101400090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
101500090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
101600090216     C     §PUTPC        IFEQ      'P'
101700090216     C     moa5004       ANDGT     *ZEROS
101800090216     C                   MOVEL     'OM'          VABTIC
101900090216     C                   MOVEL     'OM'          WVABTIC
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
103200090216      * ? solo se si tratta di note SIN ICN o PAI non altro
103300090216     C                   DO        4             X
103400090216     C     D05(X)        IFNE      *BLANKS
103500090216     C     WNOT1         IFEQ      *BLANKS
103600160405     c*
103700160405     c                   if        ftx4451 = 'AAA'
103800160405     C                   MOVEL     D05(X)        WNatMerce
103900160405     c                   end
104000160405     c*
104100090216     C                   MOVEL     D05(X)        WNOT1
104200090216     C                   MOVE      D05(X)        WNOT2
104300160405     c*
104400090216     C                   ELSE
104500090216     C     WNOT2         IFEQ      *BLANKS
104600090216     C                   MOVEL     D05(X)        WNOT2
104700090216     C                   END
104800090216     C                   END
104900090216     C                   END
105000090216     C                   END
105100090216     C*------>>
105200090216     C*  Decodifico tipo pagamento C/Assegno
105300090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
105400090216      *      (Problema di pulizia in presenza di + righe di testo)
105500090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
105600090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
105700090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
105800090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
105900090216     C                   if        wri_vabtic = *blank
106000090216     C                   MOVEL     *BLANKS       VABTIC
106100090216     c                   end
106200090216      **
106300090216     c                   clear                   trovato_TPI       1
106400090216      *
106500090216      * ?Se c'è il tipo Incasso
106600090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
106700090216      *
106800090216     C                   DO        4             X                              -- 02 --
106900090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
107000130311      **
107100130311     C                   MOVEL     D05(X)        WCTC             10
107200090216     C                   Z-ADD     1             Y
107300090216     c                   movel(p)  Wctc          fld35a           35
107400090216     c                   move      §puTC         fld35a
107500090216     C     fld35a        LOOKUP    CTC35(Y)                               33
107600090216     C   33              MOVEL     TPI(Y)        VABTIC
107700090216     c   33              move      'S'           trovato_TPI
107800090216     C                   END                                                    -- 03 --
107900090216     C                   ENDdo                                                  -- 02 --
108000090216      *
108100090216     C                   ELSE                                                   -- 01 --
108200090216      *
108300090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
108400090216      *    Deve essere impostato comunque da testata
108500090216     c                   if        vabtic = *blank
108600090216     C                   MOVEL     wVabTIC       VABTIC
108700090216     c                   end
108800090216      *
108900090216     C                   END                                                    -- 01 --
109000090216      *
109100090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
109200090216      *    essere presenti altri record Flat con altre informazioni
109300090216      *    che potrebbero abblancare il Tipo incasso precedentemente
109400090216      *    decodificato
109500090216     c                   if        Trovato_TPI = 'S' and
109600090216     c                                 wri_vabtic = *blank
109700090216     c                   eval      wri_vabtic = 'S'
109800090216     c                   end
109900090213      *
110000090213     c     end_FTX       endSR
110100090213      * ?================================================================== */
110200090213     C*?  Anagrafica del mittente e del destinatario
110300090213      * ?================================================================== */
110400090213     C     DATI_NAD      BEGSR
110500160429      *
110600090210      * Dati destinatario
110700090210     C                   IF        nad3035 = 'CN' or
110800090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
110900090227      *
111000090227      * Se trovato un indirizzo di destinatario
111100090227     c                   eval       se_trovato_NAD ='S'
111200160429     C                   eval          Contatto_NAD ='S'
111300160429     C                   eval          Comunica_NAD ='S'
111400090210      *
111500090210     C* Reperisco nazione corrispondente destinatario
111600090210     C                   Z-ADD     1             YY                3 0
111700090210     c                   eval      *in32 = *off
111800090210     C                   if        nad3207 <> *BLANKS
111900090210     C     nad3207       LOOKUP    ISO(YY)                                32
112000090210     C                   endif
112100090210      *
112200090210     C* Imposto dati destinatario
112300090211     C                   MOVEL     nad31241      VABRSD
112400090211     C                   if        nad31242 <> *LOVAL
112500090211     C                   MOVEL     nad31242      VABRD2
112600090210     C                   endif
112700090211      *
112800090211      *
112900090210     C                   MOVEL     nad30421      VABIND
113000090210     C                   MOVEL     nad3164       VABLOD
113100090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
113200090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
113300090210      *
113400090210     C* Verifico se c'è dello spazio per nella località
113500090210     C     '    '        SCAN      VABLOD        WI                4 0
113600090210      *
113700090210     C                   MOVEA     VABLOD        LOD
113800090210     C                   ADD       1             WI
113900090210     C                   MOVEA     NAD30422      LOD(WI)
114000090210     C                   MOVEA     LOD           VABLOD
114100090210     C                   ENDif
114200090210      *
114300090210     C                   If        *in32 = *on
114400090210     C                   MOVEL     C15(YY)       VABNZD
114500090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
114600090210     c                   Else
114700090210     C                   MOVEL     *BLANKS       VABNZD
114800090210     C                   Z-ADD     0             WFLCPF
114900090210     c                   Endif
115000090210      * Converto CAP
115100090210     C                   EXSR      CNVCAP
115200090210     C                   ENDIF
115300090210      *
115400090210      * Dati mittente
115500090210     C                   IF        nad3035 = 'CZ'
115600090210     C                   movel     nad30361      VABRMO
115700090210      *
115800090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
115900090210     C                   movel     §PTNAZ        VABNMO
116000090210      *
116100090210     C                   IF        nad3207 <> *blanks
116200090210     C                   eval      YY = 1
116300090210     C     nad3207       LOOKUP    ISO(YY)                                32
116400090210     C                   if         *in32 = *on
116500090210     C                   MOVEL     C15(YY)       VABNMO
116600090210     C                   endif
116700090210     C                   ENDIF
116800090210      *
116900090210      * Normalizzo CAP mittente originale
117000090210     C                   clear                   T
117100090210     C                   clear                   S
117200090210     C                   MOVEA     *BLANKS       CAPM
117300090210     C                   MOVEA     nad3251       CAP9
117400090210     C                   DO        9             T
117500090210     C     CAP9(T)       IFNE      *BLANKS
117600090210     C                   ADD       1             S
117700090210     C                   MOVE      CAP9(T)       CAPM(S)
117800090210     C                   ENDIF
117900090210     C                   ENDDO
118000090210     C*
118100090210      *     Controllo validità CAP
118200090210     C                   MOVEA     CAPM          VABCMO
118300090210      *
118400090210     C                   CLEAR                   TISI95DS
118500090210     C                   MOVEL     VABCMO        I95CAP
118600090210     C                   MOVEL     VABNMO        I95NAR
118700090210     C                   MOVEL     WOGGI         I95DAT
118800090210     C                   MOVEL     '3'           I95TCN
118900090210     C                   CALL      'TISI95R'
119000090210     C                   PARM                    TISI95DS
119100090210      *     Errore
119200090210     C     O95ERR        IFNE      *BLANKS
119300090210     C                   MOVEL     INDCAE        VABCMO
119400090210     C                   MOVEL     §PTNAZ        VABNMO
119500090210     C                   END
119600090210      *
119700090210     C                   END
119800090210      *
119900090210      * NOn c'è l'identificativo bolla del cliente
120000090210     C                   if          vabRSD = *blank or
120100090210     C                               vabIND = *blank or
120200090210     C                               vabLOD = *blank
120300090211     C                   IF        nad3035 = 'CN'
120400090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
120500090213     c                   exsr      Error_DET
120600090210     c                   goto      end_NAD
120700090210     C                   ENDIF
120800090211     C                   ENDIF
120900090210      *
121000090210     c     end_NAD       endSR
121100090210      * ?================================================================== */
121200090210     C*?  Contatto a cui far riferimento
121300090210      * ?================================================================== */
121400090210     C     DATI_CTA      BEGSR
121500090210      *
121600090210      *  memorizza il riferimento di consegna x il destinatario
121700090210      *  memorizza il riferimento di consegna nr.telefonico
121800160429     C                   if        Contatto_NAD ='S'
121900160429     C                   eval      Contatto_NAD = *blank
122000090210     C     cta3412       ifne      *BLANKS
122100090210     c                   movel     cta3412       watrde
122200090210     c                   end
122300160429     c                   end
122400090210      *
122500090210     c                   endSR
122600090210      * ?================================================================== */
122700090210     C*?  Contatto telefonico  a cui far riferimento
122800090210      * ?================================================================== */
122900090210     C     DATI_COM      BEGSR
123000090210      *
123100160429     C                   if        Comunica_NAD ='S'
123200160429     C                   eval      Comunica_NAD = *blank
123300090210     C     com3148       ifne      *BLANKS
123400160429     c                   movel     com3148       wattel
123500090210     c                   end
123600160429     c                   end
123700090210      *
123800090209     C                   ENDSR
123900090210      * ?================================================================== */
124000090210     C*?  Dati di dettaglio della spedizione e dei colli
124100090210      * ?================================================================== */
124200090210     C     DATI_GID      BEGSR
124300090210      *
124400090210     C* Se ho totali per spedizione metto in campi VAB
124500090210     C     gid1496       IFEQ      99999
124600090210     C     gid1496       orEQ      9999
124700090210     C                   MOVEL     'S'           WGID99
124800090210     C                   z-add     gid722420     VABNCL
124900090210     C                   ELSE
125000090210     C     WGID99        IFEQ      'N'
125100090210     C                   z-add     gid722420     WTNCL             5 0
125200090210     C                   ADD       WTNCL         VABNCL
125300090210     C                   END
125400090210     C                   END
125500090212      *
125600090210     C                   ADD       VABNCL        WTOCOC           16 0
125700090210      *
125800090210     c     end_GID       endSR
125900090210      * ?================================================================== */
126000090210     C*?  Dati di dettaglio misure spedizione e colli
126100090210      * ?================================================================== */
126200090210     C     DATI_MEA      BEGSR
126300090210      *
126400090210     C*  Peso
126500090210     C     mea6311       IFEQ      'WT '
126600090210     C     mea6411       ANDEQ     'KGM'
126700090210      *
126800090210     C*  Controllo se ho valore totale o parziale
126900090210      *   sempre il Lordo
127000160405     C     mea6313       IFEQ      'G  '
127100090210     C     WS1496        IFEQ      99999
127200090210     C     WS1496        orEQ      9999
127300090211     C                   move      mea6314       mea6314_2        18 2          Peso
127400090211     C                   Z-ADD     mea6314_2     VABPKB                         Peso
127500160405     c                   if        vabpkb = 0
127600160405     c                   eval      vabpkb = 0,01
127700160405     c                   end
127800090210     C                   ELSE
127900090210     C     WGID99        IFEQ      'N'
128000090211     C                   move      mea6314       mea6314_2                      Peso
128100090211     C                   ADD       mea6314_2     VABPKB                         Peso
128200160405     c                   if        vabpkb = 0
128300160405     c                   eval      vabpkb = 0,01
128400160405     c                   end
128500090210     C                   END
128600090210     C                   END
128700090210     C                   END
128800090210      *
128900160405     C     mea6313       IFEQ      'G  '
129000090211     C                   move      mea6314       mea6314_2                      Peso
129100090211     C                   ADD       mea6314_2     WTOPLC
129200090210     C                   END
129300090210     C     mea6313       IFEQ      'AAC'
129400090211     C                   move      mea6314       mea6314_2                      Peso
129500090211     C                   ADD       mea6314_2     WTOPNC
129600090210     C                   END
129700090210     C                   END
129800090210      ***
129900090210     C*  Peso in Grammi  se abilitato a utilizzarlo
130000090210      ***
130100090210     C     §puGR         IFeq      'S'
130200090210     C     mea6311       IFEQ      'WT '
130300090210     C     mea6411       ANDEQ     '163'
130400090210      *
130500090210     C*  Controllo se ho valore totale o parziale
130600160405     C     mea6313       IFEQ      'G  '
130700090210     C     WS1496        IFEQ      99999
130800090210     C     WS1496        orEQ      9999
130900090211     c                   move      mea6314       mea6314_2
131000090211     C     mea6314_2     div       1000          VABPKB                         Peso
131100160405     c                   if        vabpkb = 0
131200160405     c                   eval      vabpkb = 0,01
131300160405     c                   end
131400090210     C                   ELSE
131500090210     C     WGID99        IFEQ      'N'
131600090211     c                   move      mea6314       mea6314_2
131700090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
131800090210     C                   add       Peso_inKG     VABPKB                         Peso
131900160405     c                   if        vabpkb = 0
132000160405     c                   eval      vabpkb = 0,01
132100160405     c                   end
132200090210     C                   END
132300090210     C                   END
132400090210     C                   END
132500090210      *
132600160405     C     mea6313       IFEQ      'G  '
132700090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
132800090210     C                   ADD       Peso_inKG     WTOPLC
132900090210     C                   END
133000090210     C     mea6313       IFEQ      'AAC'
133100090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
133200090210     C                   ADD       Peso_inKG     WTOPNC
133300090210     C                   END
133400090210     C                   END
133500090210     C                   END
133600090210      *  Volume
133700090210     C     mea6311       IFEQ      'VOL'
133800090210     C     mea6411       ANDEQ     'MTQ'
133900090210      *  Controllo se ho valore totale o parziale
134000090210     C     WS1496        IFEQ      99999
134100090210     C     WS1496        orEQ      9999
134200091102     C                   move      mea6314       mea6314_2                      Volume
134300091102     C                   Z-ADD     mea6314_2     VABVLB                         Volume
134400091102     C                   Z-ADD     mea6314_2     PVLB
134500090210     C                   ELSE
134600090210     C     WGID99        IFEQ      'N'
134700091102     C                   move      mea6314       mea6314_2                      Volume
134800091102     C                   ADD       mea6314_2     VABVLB                         Volume
134900091102     C                   ADD       mea6314_2     PVLB
135000090210     C                   END
135100090210     C                   END
135200090210     C                   END
135300090210      *
135400090210     C* Ricerco CAP
135500090210     C     WCAP          IFNE      *BLANKS
135600090210     C     mea6311       IFEQ      'WT '
135700090210      *
135800090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
135900090210     C                   CLEAR                   FNLV55DS
136000090210     C                   MOVEL     'P'           D55TPT
136100090210     C                   MOVEL     WOGGI         D55DRF
136200090210     C                   MOVEL     VABLNP        D55LNP
136300090210     C                   MOVEL     VABLNP        D55LIN
136400090210     C                   CALL      'FNLV55R'
136500090210     C                   PARM                    FNLV55DS
136600090210     C                   MOVE      D55TFP        COMTFP            3 0
136700090210      *
136800090210     C*  PASSO IL TERMINAL PARTENZA
136900090210     C                   CLEAR                   TISI95DS
137000090210     C                   Z-ADD     COMTFP        I95TFP
137100090210     C                   MOVEL     VABTSP        I95TSP
137200090210     C                   MOVEL     'S'           I95FRE
137300090210     C                   MOVEL     '3'           I95TCN
137400090210     C                   MOVEL     WCAP          I95CAP
137500090210     C                   MOVEL     VABNZD        I95NAR
137600090210     C                   MOVEL     VABLOD        I95LOC
137700090210     C                   MOVEL     VABIND        I95IND
137800090210      *
137900090210     C* Se gestita seconda linea di arrivo passo peso - volume
138000090210     C* e numero colli effettivo
138100090210     C     §PULNA        IFEQ      'S'
138200090210     C                   Z-ADD     VABPKB        I95LKG
138300090210     C                   Z-ADD     VABVLB        I95LMC
138400090210     C                   ELSE
138500090210     C* Altrmenti passo 1 Kg e 0,001
138600090210     C***                  Z-ADD1         I95LKG
138700090210     C***                  Z-ADD0,001     I95LMC
138800090210     C                   END
138900090210     C*
139000090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
139100090210     C                   SELECT
139200090210     C     VABCBO        WHENEQ    '1 '
139300090210     C                   MOVEL     *BLANKS       I95FCA
139400090210     C                   MOVEL     'F'           I95TPO
139500090210     C     VABCBO        WHENEQ    '2 '
139600090210     C                   MOVEL     *BLANKS       I95FCA
139700090210     C                   MOVEL     'A'           I95TPO
139800090210     C     VABCBO        WHENEQ    '4 '
139900090210     C                   MOVEL     'S'           I95FCA
140000090210     C                   MOVEL     'F'           I95TPO
140100090210     C     VABCBO        WHENEQ    '6 '
140200090210     C                   MOVEL     'S'           I95FCA
140300090210     C                   MOVEL     'A'           I95TPO
140400090210     C                   OTHER
140500090210     C                   MOVEL     'F'           I95TPO
140600090210     C     VABCAS        IFGT      0
140700090210     C                   MOVEL     'S'           I95FCA
140800090210     C                   ELSE
140900090210     C                   MOVEL     *BLANKS       I95FCA
141000090210     C                   END
141100090210     C                   ENDSL
141200090210     C*
141300090210     C                   CALL      'TISI95R'
141400090210     C                   PARM                    TISI95DS
141500090210     C* Reperisco i dati da CAP
141600090210     C                   MOVEL     O95PRV        VABPRD
141700090210     C                   MOVEL     O95LNA        VABLNA
141800090210     C                   MOVEL     O95ZNC        VABZNC
141900090210     C                   MOVEL     WCAP          VABCAD
142000090210     C*
142100090210     C     O95ERR        IFNE      *BLANKS
142200090210     C     O95FIT        ANDEQ     'S'
142300090210     C*  Cap non trovato
142400090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
142500090213     c                   exsr      Error_DET
142600090210     C                   goto      end_MEA
142700090210     C                   END
142800090210     C                   END
142900090210      *
143000090210     C                   ELSE
143100090210      *
143200090210     C                   CLEAR                   VABPRD
143300090210     C                   CLEAR                   VABLNA
143400090210     C                   CLEAR                   VABZNC
143500090210     C                   CLEAR                   VABCAD
143600090210     C*  Cap non trovato
143700090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
143800090213     c                   exsr      Error_DET
143900090210     C                   goto      end_MEA
144000090210     C                   END
144100090210      *
144200090210     c     end_MEA       endSR
144300090210      * ?================================================================== */
144400090210     C*?  Dati di dettaglio Codice a barre Segnacolli
144500090210      * ?================================================================== */
144600090210     C     DATI_PCI      BEGSR
144700090210      *
144800090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
144900090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
145000090210     C     §1BF12        IFEQ      'E'
145100090210     C                   EXSR      SEGNEE
145200090210     C                   ELSE
145300090210      *
145400090210      *  La prima volta controllo congruità numero segnacollo
145500090210     C     §PUBS1        IFNE      *BLANKS
145600090210     C     VABnrs        ANDNE     0
145700090210      *  Se il codice trattamento merce prevede che segnacollino i
145800090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
145900090210     C     §1BFG7        IFEQ      'S'
146000090210     C     §1BFG1        OREQ      'S'
146100090210      *  calcolo segnacollo
146200090210     C                   EXSR      SGNELA
146300090210     C                   END
146400090210     C                   END
146500090210      *
146600090210     C                   END
146700090210      *
146800090210     c     end_PCI       endSR
146900090211     C*----------------------------------------------------------------
147000090211      *? dati finali del dettaglio    UNT
147100090211     C*----------------------------------------------------------------
147200090211     C     DATI_UNT      BEGSR
147300090211      *
147400090213     c                   eval      NUM_RECord = vnREC
147500090213     c                   z-add     vnrec         last_RECord
147600090211      **
147700090211     c                   endSR
147800090211     C*----------------------------------------------------------------
147900090211      *? dati finali     UNT
148000090211     C*----------------------------------------------------------------
148100090211     C     DATI_UNZ      BEGSR
148200090211      *
148300100719     C                   eval      Id_Messaggio   = UNZ0020
148400090211      **
148500090211     c                   endSR
148600090210      *----------------------------------------------------------------
148700090210      *? Input segnacolli su schiera di 20 elementi
148800090210      *----------------------------------------------------------------
148900090210     C     INP_Segnacol  BEGSR
149000090210      *
149100090210     c                   clear                   quanti_PCI        3 0
149200090210     c                   if        §puEL1 = 0
149300090210     c                   z-add     10            quanti_PCI
149400090210     c                   else
149500090210     c                   z-add     §puEL1        quanti_PCI
149600090210     c                   end
149700090210      *
149800090210      *  Pulisce la schiera dei segnacolli da elaborare
149900090210     c                   clear                   x30
150000090210     c                   z-add     0             Nr_x30            3 0
150100090210      *
150200090210      *   riporto tutto sulla schiera generica X30
150300090210      *
150400090210     c                   do        10            limite            3 0
150500090210      * Limita quanti elementi passati in ogni PCI
150600090210      *   devono essere considerati
150700090210     c                   if        limite > quanti_PCI
150800090210     c                   Leave
150900090210     c                   end
151000090210      *
151100090210      *  Carica la schiera generale di tutti i segnacolli
151200090210     c                   if        D30_1(limite) <> *blank
151300090210     c                   add       1             Nr_x30
151400090210     c                   eval      X30(Nr_x30) = D30_1(limite)
151500090210     c                   end
151600090210     c                   enddo
151700090210      *
151800090210     C                   ENDSR
151900090210      *----------------------------------------------------------------
152000090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
152100090210      *----------------------------------------------------------------
152200090210     C     SEGNEE        BEGSR
152300090210      *
152400090210     c                   exsr      INP_Segnacol
152500090210      *
152600090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
152700090210     C                   DO        10            x                              --- 01 ---
152800090210      *
152900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
153000090210     C     x30(X)        ANDNE     *LOVAL
153100090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
153200090210     C                   ADD       1             §§
153300090210     C                   MOVEL     x30(X)        SGE(§§)
153400090210     C                   END                                                    --- 03 ---
153500090210      *
153600090210     C                   END                                                    --- 02 ---
153700090210      *
153800090210     C                   ENDSR
153900090210     C*----------------------------------------------------------------
154000090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
154100090210     C*----------------------------------------------------------------
154200090210     C     SGNELA        BEGSR
154300090210      *
154400090210     c                   exsr      INP_Segnacol
154500090210     C*
154600090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
154700090210     C*  in base al numero elementi validi carico sgn.
154800090210     C                   DO        10            X                              --- 01 ---
154900090210     C*
155000090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
155100090210     C     x30(X)        ANDNE     *LOVAL
155200090210     C*
155300090210     C                   CLEAR                   DSSGN
155400090210     C*  Controllo se devo ricevere la linea di partenza
155500090210     C                   MOVEL     'N'           WERRO             1
155600090210     C     §PULP1        IFGT      0                                            --- 03 ---
155700090210     C                   EXSR      REPLNP
155800090210     C                   END                                                    --- 03 ---
155900090210     C*  Controllo se devo ricevere la linea di arrivo
156000090210     C     §PULA1        IFGT      0                                            --- 03 ---
156100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
156200090210     C                   EXSR      REPLNA
156300090210     C                   END                                                    --- 03 ---
156400090210     C*  Controllo se devo ricevere il numero di serie
156500090210     C     §PUNR1        IFGT      0                                            --- 03 ---
156600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
156700090210     C                   EXSR      REPNRS
156800090210     C                   END                                                    --- 03 ---
156900090210     C*  Controllo se devo ricevere il numero segnacollo
157000090210     C     §PUSG1        IFGT      0                                            --- 03 ---
157100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
157200090210     C                   EXSR      REPSGN
157300090210     C                   END                                                    --- 03 ---
157400090210     C*  Controllo se devo ricevere la zona di consegna
157500090210     C     §PUZN1        IFGT      0                                            --- 03 ---
157600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
157700090210     C                   EXSR      REPZNC
157800090210     C                   END                                                    --- 03 ---
157900090210     C*  Imposto da VAB i dati a zero se mi è stato passato
158000090210     C*  numero segnacollo valido
158100090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
158200090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
158300090210     C                   Z-ADD     VABLNP        SGNLNP
158400090210     C                   END                                                    --- 04 ---
158500090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
158600090210     C                   Z-ADD     VABLNA        SGNLNA
158700090210     C                   END                                                    --- 04 ---
158800090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
158900090210     C                   Z-ADD     VABNRS        SGNNRS
159000090210     C                   END                                                    --- 04 ---
159100090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
159200090210     C                   Z-ADD     VABZNC        SGNZNC
159300090210     C                   END                                                    --- 04 ---
159400090210      *
159500090210     C*  mi reimposto LNA e zona da segnacollo
159600090210     C                   Z-ADD     SGNLNA        VABLNA
159700090210     C                   Z-ADD     SGNZNC        VABZNC
159800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
159900090210     C                   ADD       1             XY
160000090210     C                   MOVE      SGNNSC        SGN(XY)
160100090210     C                   END                                                    --- 03 ---
160200090210     C*
160300090210     C                   END                                                    --- 02 ---
160400090210     C*
160500090210     C                   END                                                    --- 01 ---
160600090210      *
160700090210     C                   ENDSR
160800090210     C*----------------------------------------------------------------
160900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
161000090210     C*----------------------------------------------------------------
161100090210     C     REPLNP        BEGSR
161200090210     C*
161300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
161400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
161500090210     C*  dal numero segnacollo passatomi dal partner
161600090210     C                   Z-ADD     §PULP1        WP                2 0
161700090210     C     3             SUBST     x30(X):WP     WLNP              3
161800090210     C*
161900090210     C*  controllo che la lnp calcolata sia numerica
162000090210     C                   TESTN                   WLNP                 98
162100090210     C*
162200090210     C*  se è numerica la carico
162300090210     C     *IN98         IFEQ      '1'
162400090210     C                   MOVE      WLNP          SGNLNP
162500090210     C                   ELSE
162600090210     C*  se non è numerica stampo errore
162700090210     C                   MOVEL     'S'           WERRO             1
162800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
162900090213     c                   exsr      Error_DET
163000090210     C                   goto      end_LNP
163100090210     C                   END
163200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
163300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
163400090210     C     SGNLNP        IFNE      VABLNP
163500090210     C                   MOVEL     'S'           WERRO             1
163600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
163700090210     c                             a della Bolla'
163800090213     c                   exsr      Error_DET
163900090210     C                   goto      end_LNP
164000090210     C                   END
164100090210     C*
164200090210     C     end_LNP       ENDSR
164300090210     C*----------------------------------------------------------------
164400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
164500090210     C*----------------------------------------------------------------
164600090210     C     REPLNA        BEGSR
164700090210     C*
164800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
164900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
165000090210     C*  dal numero segnacollo passatomi dal partner
165100090210     C                   Z-ADD     §PULA1        WP                2 0
165200090210     C     3             SUBST     x30(X):WP     WLNA              3
165300090210     C*  controllo che la lna calcolata sia numerica
165400090210     C                   TESTN                   WLNA                 98
165500090210     C*  se è numerica la carico
165600090210     C     *IN98         IFEQ      '1'
165700090210     C                   MOVE      WLNA          SGNLNA
165800090210     C                   ELSE
165900090210     C*  se non è numerica stampo errore
166000090210     C                   MOVEL     'S'           WERRO             1
166100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
166200090213     c                   exsr      Error_DET
166300090210     C                   goto      end_LNA
166400090210     C                   END
166500090210     C*
166600090210     C     end_LNA       ENDSR
166700090210     C*----------------------------------------------------------------
166800090210     C*? REPNRS - Reperisce numero serie
166900090210     C*----------------------------------------------------------------
167000090210     C     REPNRS        BEGSR
167100090210     C*
167200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
167300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
167400090210     C*  dal numero segnacollo passatomi dal partner
167500090210     C                   Z-ADD     §PUNR1        WP                2 0
167600090210     C     2             SUBST     x30(X):WP     WNRS              2
167700090210     C*  controllo che il nrs calcolata sia numerico
167800090210     C                   TESTN                   WNRS                 98
167900090210     C*  se è numerica la carico
168000090210     C     *IN98         IFEQ      '1'
168100090210     C                   MOVE      WNRS          SGNNRS
168200090210     C                   ELSE
168300090210     C*  se non è numerico stampo errore
168400090210     C                   MOVEL     'S'           WERRO             1
168500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
168600090213     c                   exsr      Error_DET
168700090210     C                   goto      end_NRS
168800090210     C                   END
168900090210      *
169000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
169100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
169200090210     C     SGNNRS        IFNE      VABNRS
169300090210     C                   MOVEL     'S'           WERRO             1
169400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
169500090210     c                             o della Bolla'
169600090213     c                   exsr      Error_DET
169700090210     C                   goto      end_NRS
169800090210     C                   END
169900090210     C*
170000090210     C     end_NRS       ENDSR
170100090210     C*----------------------------------------------------------------
170200090210     C*? REPZNC - Reperisce zona consegna
170300090210     C*----------------------------------------------------------------
170400090210     C     REPZNC        BEGSR
170500090210     C*
170600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
170700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
170800090210     C*  dal numero segnacollo passatomi dal partner
170900090210     C                   Z-ADD     §PUZN1        WP                2 0
171000090210     C     2             SUBST     x30(X):WP     WZNC              2
171100090210     C*  controllo che la zona calcolata sia numerica
171200090210     C                   TESTN                   WZNC                 98
171300090210     C*  se è numerica la carico
171400090210     C     *IN98         IFEQ      '1'
171500090210     C                   MOVE      WZNC          SGNZNC
171600090210     C                   ELSE
171700090210     C*  se non è numerica stampo errore
171800090210     C                   MOVEL     'S'           WERRO             1
171900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
172000090213     c                   exsr      Error_DET
172100090210     C                   goto      end_ZNC
172200090210     C                   END
172300090210     C*
172400090210     C     end_ZNC       ENDSR
172500090210     C*----------------------------------------------------------------
172600090210     C*? REPSGN - Reperisce numero segnacollo
172700090210     C*----------------------------------------------------------------
172800090210     C     REPSGN        BEGSR
172900090210     C*
173000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
173100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
173200090210     C*  indicata in tabella)
173300090210     C*  dal numero segnacollo passatomi dal partner
173400090210     C                   Z-ADD     §PUSG1        WP                2 0
173500090210     C     7             SUBST     x30(X):WP     WSGN              7
173600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
173700090210     C                   TESTN                   WSGN                 98
173800090210     C*  se è numerico lo carico
173900090210     C     *IN98         IFEQ      '1'
174000090210     C                   MOVE      WSGN          SGNNSC
174100090210     C                   ELSE
174200090210     C*  se non è numerica stampo errore
174300090210     C                   MOVEL     'S'           WERRO             1
174400090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
174500090213     c                   exsr      Error_DET
174600090210     C                   goto      end_SGN
174700090210     C                   END
174800090210     C*
174900090210     C     end_SGN       ENDSR
175000971215      *----------------------------------------------------------------
175100971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
175200971215      *----------------------------------------------------------------
175300051205      *----------------------------------------------------*
175400051205      *   DEFINIZIONE CHIAVI                               *
175500051205      *----------------------------------------------------*
175600051205     C     *INZSR        BEGSR
175700051205      *
175800991129     c     *ENTRY        PLIST
175900060613     C                   parm                    esito             1
176000971215      *
176100050112     C     KTABel        KLIST
176200050112     C                   KFLD                    TBLKUT
176300050112     C                   KFLD                    TBLCOD
176400050112     C                   KFLD                    TBLKEY
176500090210      *
176600090210     C     K_TAB1L       KLIST
176700090213     C                   KFLD                    etbUNB
176800090213     C                   KFLD                    etbSGM
176900090213     C                   KFLD                    etbVALSGM
177000090209     C*
177100090209     C* Definisco chiavi di accesso
177200090209     C     KTAB1         KLIST
177300090209     C                   KFLD                    KKUT
177400090209     C                   KFLD                    KCOD
177500090209     C*
177600090209     C     KNUF          KLIST
177700090209     C                   KFLD                    KAAA
177800090209     C                   KFLD                    KCNU
177900090209     C                   KFLD                    KFIL
178000090209      *
178100090209     C     KVAB1         KLIST
178200090209     C                   KFLD                    KCCM
178300090209     C                   KFLD                    KCMR
178400090209     C                   KFLD                    KCNT
178500090209     C                   KFLD                    KAAS
178600090209     C                   KFLD                    KLNP
178700090209     C                   KFLD                    KNRS
178800090209     C                   KFLD                    KNSP
178900090209      *
179000090209     C     KVAB2         KLIST
179100090209     C                   KFLD                    KCCM
179200090209     C                   KFLD                    KCMR
179300090209      *
179400090209     C     KRDE          KLIST
179500090209     C                   KFLD                    KAAS
179600090209     C                   KFLD                    KLNP1
179700090209     C                   KFLD                    KNRS
179800090209     C                   KFLD                    KNSP
179900090209     C                   KFLD                    KNMC
180000090209     C                   KFLD                    KNRP
180100090209      *
180200090209     C     KACO          KLIST
180300090209     C                   KFLD                    KKUT
180400090209     C                   KFLD                    KKCC
180500090209     C                   KFLD                    KKSC
180600090209      *
180700090209     C     KIND          KLIST
180800090209     C                   KFLD                    KKUT
180900090209     C                   KFLD                    KKCC
181000090209     C                   KFLD                    KKSC
181100090209      *
181200090209      * DETERMINO SE SONO BARTOLINI O SDI
181300090209     C                   CLEAR                   TIBS55DS
181400090209     C                   MOVEL     'L'           I50TLA
181500090209     C                   CALL      'TIBS55R'
181600090209     C                   PARM                    TIBS55DS
181700090209      *
181800090209     C* RECUPERO DATI DELL'UTENTE
181900090209     C                   Z-ADD     1             CODUT             1 0
182000090209     C                   CALL      'XPARUT'
182100090209     C                   PARM                    UTEDSE0F
182200090209     C                   MOVEL     RAGUT         RSUT             20
182300090209     C*
182400090209     C* RICERCA CAPOCONTI
182500090209     C                   DO        50            X
182600090209     C                   MOVE      TCU(X)        TCUDS
182700090209     C     F56           IFEQ      'CG'
182800090209     C     F34           ANDEQ     '01'
182900090209     C                   Z-ADD     KCU(X)        KCI               4 0
183000090209     C                   END
183100090209     C                   END
183200090209     C*
183300090211     c                   movel(p)  'EDPAB'       User_Msg         10
183400090211     C*
183500090209     C* IMPOSTO A ZERO VARIABILI DI PGM
183600090209     c                   move      *blank        Snd_Msg_alert     1
183700090209     C                   MOVEL     CA(1)         WCA              78
183800090209     C                   MOVEL     CN(1)         WCN              78
183900090209     C*
184000090209     C* RECUPERO DATA E ORA
184100090209     C                   TIME                    WHHDAT           14 0
184200090209     C                   MOVE      WHHDAT        G02DAT
184300090209     C                   MOVE      WHHDAT        WDATA8            8 0
184400090209     C                   MOVE      WDATA8        WAA2              2 0
184500090209     C                   MOVEL     WDATA8        DATA              6 0
184600090209     C                   MOVE      WAA2          DATA
184700090209     C                   MOVEL     *BLANK        G02ERR
184800090209     C                   CALL      'XSRDA8'
184900090209     C                   PARM                    WLBDA8
185000090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
185100090209     C                   TIME                    WORA              6 0
185200090209      * Recupero data e ora
185300090209     C                   TIME                    W0140
185400090209      * UDATE IN GGMMAAAA
185500090209     C                   MOVE      W0140         WDTGIO
185600090209      * UDATE IN AAAAMMGG
185700090209     C     *eur          MOVEL     WDTGIO        DATA_eur
185800090209     C     *iso          MOVEL     DATA_eur      dateu
185900090209      *
186000090209      * ?              /*--------------------------- */
186100090209     c                   exsr      CARICA_TABELLE
186200090209      * ?              /*--------------------------- */
186300090209      *
186400090209      *
186500090209     C                   MOVEL     *ALL'-'       CMP132          132
186600090209     C                   CLEAR                   EDiVAB00
186700090211     C                   clear                   vablnp_PT
186800090212     C                   clear                   vabnrs_PT
186900090211     C                   clear                   vabctm_PT
187000090211     C                   clear                   VABfnsp_PT
187100090209     C                   MOVEA     *HIVAL        SGN
187200991124      *
187300050414      *------------------
187400991124     C                   ENDSR
187500060621      * ?------------------------------------------------------------------ */
187600090209      *?    CARICA TABELLE SU SCHIERE
187700060621      * ?------------------------------------------------------------------ */
187800090209     C     CARICA_TABELLEBEGSR
187900090205      *
188000090209     C* Caricamento Tabella 1B
188100090209     C                   Z-ADD     0             X                 4 0
188200090209     C                   Z-ADD     CODUT         KKUT
188300090209     C                   MOVEL     '1B'          KCOD
188400090209     C     KTAB1         CHAIN     TABEL00F                           30
188500090209     C     *IN30         DOWEQ     '0'
188600090209     C     TBLFLG        IFEQ      *BLANKS
188700090209     C                   ADD       1             X
188800090209     C                   MOVEL     TBLKEY        CTM(X)
188900090209     C                   MOVEL     TBLUNI        DTM(X)
189000090209     C                   END
189100090209     C     KTAB1         READE     TABEL00F                               30
189200090209     C                   END
189300090209      *
189400090209     C* Caricamento Tabella 15
189500090209     C                   Z-ADD     0             X                 4 0
189600090209     C                   Z-ADD     CODUT         KKUT
189700090209     C                   MOVEL     '15'          KCOD
189800090209     C     KTAB1         CHAIN     TABEL00F                           30
189900090209     C     *IN30         DOWEQ     '0'
190000090209     C     TBLFLG        IFEQ      *BLANKS
190100090209     C                   ADD       1             X
190200090209     C                   MOVEL     TBLKEY        C15(X)
190300090209     C                   MOVEL     TBLUNI        DS15
190400090209     C                   MOVEL     §15COD        ISO(X)
190500090209     C                   MOVEL     §15ECF        CPF(X)
190600090209     C                   MOVE      §15PCF        CPF(X)
190700090209     C                   END
190800090209     C     KTAB1         READE     TABEL00F                               30
190900090209     C                   END
191000090209      *
191100090209     C* Caricamento Tabella CV
191200090209     C                   Z-ADD     0             X
191300090209     C                   Z-ADD     CODUT         KKUT
191400090209     C                   MOVEL     'CV'          KCOD
191500090209     C     KTAB1         CHAIN     TABEL00F                           30
191600090209     C     *IN30         DOWEQ     '0'
191700090209     C     TBLFLG        IFEQ      *BLANKS
191800090209     C                   ADD       1             X
191900090209     C                   MOVEL     TBLKEY        CCV(X)
192000090209     C                   MOVEL     TBLUNI        DCV(X)
192100090209     C                   END
192200090209     C     KTAB1         READE     TABEL00F                               30
192300090209     C                   END
192400090216      *
192500110328      * Caricamento Tabella termini consegna
192600110328     C                   Z-ADD     0             X
192700110328     C                   MOVEL     'TB'          TABCOD
192800110328     C     TABCOD        CHAIN     EDTAB01L                           30
192900110328     C     *IN30         DOWEQ     '0'
193000110328     C     TABFLG        IFEQ      *BLANKS
193100110328     C                   ADD       1             X
193200110328     C                   eval      K35_TpBolla(X) = TABKEY
193300110328     C                   eval      Cod_TpBolla(X) = TABKEY
193400110328     C                   eval      Decod_Porto(X) = TABUNI
193500110328     C                   END
193600110328     C     TABCOD        READE     EDTAB01L                               30
193700110328     C                   END
193800110328      *
193900090216     C* Caricamento Tabella Tipo Incasso C/Assegno
194000090216     C                   Z-ADD     0             X
194100090216     C                   MOVEL     'TC'          TABCOD
194200090216     C     TABCOD        CHAIN     EDTAB01L                           30
194300090216     C     *IN30         DOWEQ     '0'
194400090216     C     TABFLG        IFEQ      *BLANKS
194500090216     C                   ADD       1             X
194600090216     C                   MOVEL     TABKEY        CTC35(X)
194700090216     C                   MOVEL     TABUNI        TPI(X)
194800090216     C                   END
194900090216     C     TABCOD        READE     EDTAB01L                               30
195000090216     C                   END
195100090209      *
195200090209      * Caricamento Tabella Partner esteri
195300090209     C                   Z-ADD     0             X
195400090209     C                   MOVEL     'PT'          TABCOD
195500090209     C     TABCOD        CHAIN     EDTAB01L                           30
195600090209     C     *IN30         DOWEQ     '0'
195700090209      *
195800090209     C                   SELECT
195900090209     C     TABFLG        WHENNE    *BLANKS
196000090209      *
196100090209     C                   OTHER
196200090209     C                   ADD       1             X
196300090209     C                   MOVEL     TABKEY        CPT(X)
196400090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
196500090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
196600090209     C                   MOVEL     TABUNI        DPT(X)
196700090209     C                   ENDSL
196800090209      *
196900090209     C     TABCOD        READE     EDTAB01L                               30
197000090209     C                   END
197100121102      *
197200121102      * Controlla riempimento della PT x limiti di schiera
197300121102      *  se deve inviare la mail di alert x limite quasi raggiunto.
197400121102     c                   exsr      ChecK_PT
197500121102     C*
197600090209      * salva quanti Codici UNB ha caricato
197700090209     c                   z-add     x             sav_CPTx
197800090209      *
197900090209     C                   MOVEL     'PU'          TABCOD
198000090209     C     TABCOD        CHAIN     EDTAB01L                           30
198100090209     C     *IN30         DOWEQ     '0'
198200090209      *
198300090209     C                   SELECT
198400090209     C     TABFLG        WHENNE    *BLANKS
198500090209      *
198600090209     C                   OTHER
198700090209     C                   MOVEL     TABKEY        CODPU            35
198800090209     C                   Z-ADD     1             X
198900090209     C     CODPU         LOOKUP    CPT(X)                                 32
199000090209     C   32              MOVEL     TABUNI        DPU(X)
199100090209     C                   ENDSL
199200090209      *
199300090209     C     TABCOD        READE     EDTAB01L                               30
199400090209     C                   END
199500090209      *
199600090209      * Prosegue tab.PT e PU
199700090209     C                   MOVEL     'PV'          TABCOD
199800090209     C     TABCOD        CHAIN     EDTAB01L                           30
199900090209     C     *IN30         DOWEQ     '0'
200000090209      *
200100090209     C                   SELECT
200200090209     C     TABFLG        WHENNE    *BLANKS
200300090209      *
200400090209     C                   OTHER
200500090209     C                   MOVEL     TABKEY        CODPV            35
200600090209     C                   Z-ADD     1             X
200700090209     C     CODPV         LOOKUP    CPT(X)                                 32
200800090209     C   32              MOVEL     TABUNI        DPV(X)
200900090209     C                   ENDSL
201000090209      *
201100090209     C     TABCOD        READE     EDTAB01L                               30
201200090209     C                   END
201300090209      *
201400090209     C* Caricamento Tabella codici clienti - tariffe
201500090209     C                   Z-ADD     0             X
201600090209     C                   MOVEL     'CL'          TABCOD
201700090209     C     TABCOD        CHAIN     EDTAB01L                           30
201800090209     C     *IN30         DOWEQ     '0'
201900090209     C     TABFLG        IFEQ      *BLANKS
202000090209     C                   ADD       1             X
202100090209     C                   MOVEL     TABKEY        CCL(X)
202200090209     C                   MOVEL     TABUNI        DCL(X)
202300090209     C                   END
202400090209     C     TABCOD        READE     EDTAB01L                               30
202500090209     C                   END
202600090209      *
202700090209     C* Caricamento Serivizi speciali
202800090209     C                   Z-ADD     0             X
202900090209     C                   MOVEL     'SS'          TABCOD
203000090209     C     TABCOD        CHAIN     EDTAB01L                           30
203100090209     C     *IN30         DOWEQ     '0'
203200090209     C     TABFLG        IFEQ      *BLANKS
203300090209     C                   ADD       1             X
203400090209     C                   MOVEL     TABKEY        SS(X)
203500090209     C                   MOVEL     TABKEY        SS35(X)
203600090209     C                   MOVEL     TABUNI        DSS(X)
203700090209     C                   END
203800090209     C     TABCOD        READE     EDTAB01L                               30
203900090209     C                   END
204000090209      *
204100090209     C                   ENDSR
204200090209      * ?------------------------------------------------------------------ */
204300090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
204400090209      * ?------------------------------------------------------------------ */
204500090209     C     MSG_Errato    BEGSR
204600090209      *
204700090205      * ?  Scrive su tutti i records il tipo di errore
204800090205     c     *start        setll     tivin00r
204900090205     c                   read      tivin00r
205000090205     c                   dow       not %eof(tivin00r)
205100090211     c                   if        vinMSG  = *blank
205200090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
205300090210     C                               >> Controllare i DATI su UPLOAD !!'
205400090211     c                   end
205500090205     C                   eval      vinFLG = '2'
205600090213     c                   eval      Almeno_Un_Due ='S'
205700090205     c                   eval      esito  = '2'
205800090209     c                   eval      se_errore ='S'
205900090213     c                   eval      err_bloccante ='S'
206000090205     c                   update    tivin000
206100090205     c                   read      tivin00r
206200090205     c                   endDO
206300090205      *
206400090205     C                   ENDSR
206500090213      * ?------------------------------------------------------------------ */
206600090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
206700090213      * ?------------------------------------------------------------------ */
206800090213     C     DETta_Errato  BEGSR
206900090213      *
207000090213      *  rilascia il record prima di rieseguire il ciclo
207100090213     c                   update    tivin000
207200090213      *
207300090213      * ?  Scrive su tutti i records il tipo di errore
207400090213     c     SavNUM_RECord setll     tivin00r
207500090213     c                   read      tivin00r
207600090213     c                   dow       not %eof(tivin00r)
207700090213      *
207800090213      *
207900090213     c                   if        vinMSG  = *blank
208000090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
208100090213     C                             >> Controllare i DATI di ogni Segmento <<'
208200090213     c                   end
208300090213     c                   exsr      Error_DET
208400090213      *
208500090213     c                   if        VNREC = NUM_RECord
208600090213     c                   leave
208700090213     c                   end
208800090213      *
208900090213     c                   update    tivin000
209000090213     c                   read      tivin00r
209100090213      *
209200090213     c                   endDO
209300090601      *
209400090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
209500090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
209600090601     C                             Bolle NILFISK non totalmente tradotto. -
209700090601     C                             Controllare UPLOAD del cliente 2494114.'
209800090601     c                   exsr      invio_mail_AB
209900090213      *
210000090213     C                   ENDSR
210100090205      * ?------------------------------------------------------------------ */
210200090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
210300090205      * ?------------------------------------------------------------------ */
210400090205     C     *pssr         BEGSR
210500090205     C
210600060710     C                   eval      esito = '2'
210700090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
210800090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
210900090601     C                             Bolle import in filiale - messaggio -
211000090601     C                             con UNB non presente in tabella PT controlla-
211100090601     C                             re EDI ricevuto su UPLOAD.'
211200090601     c                   exsr      invio_mail_AB
211300060621     C
211400090213     C                   ENDSR
211500090213      * ?------------------------------------------------------------------ */
211600090213      *?      Segnala Errore su TIVIN
211700090213      * ?------------------------------------------------------------------ */
211800090213     C     Error_DET     BEGSR
211900090213     C
212000090213     C                   eval      vinFLG = '2'
212100090213     c                   eval      Almeno_Un_Due ='S'
212200090213     c                   eval      esito  = '1'
212300090213     c                   eval      se_errore ='S'
212400090213     C                   eval      Err_in_detta = 'S'
212500090213     C
212600090213     C                   ENDSR
212700090209      * ?------------------------------------------------------------------ */
212800090209      *? Controlla la trasmissione   se completa
212900090209      * ?------------------------------------------------------------------ */
213000090209     c     Check_Trasm   Begsr
213100090209
213200090209     C                   clear                   se_errore
213300090209     C                   clear                   Riga_iNIZIo       1
213400090209     C                   clear                   Riga_fINe         1
213500090209      ** primo  record
213600090209     c     *start        setll     tivin00r
213700090209      *
213800090209     c     rileggi       tag
213900090209     c                   read      tivin00r
214000090209      *
214100090209     c                   if        not %eof(tivin00r)
214200090209      *
214300090209     c                   movel(p)  VINDTA        dati
214400090209     c                   if        dati = *blank
214500090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
214600090209      * le leggo e le escludo.
214700090209     C                   eval      vinFLG = '1'
214800090209     c                   update    Tivin000
214900090209     c                   goto      rileggi
215000090209     c                   end
215100090209      * ?              /*---------------------- */
215200090210     c                   exsr      Decod_Segmento
215300090209      * ?              /*---------------------- */
215400090209     c                   endif
215500090209      **
215600090209      ** ultimo record
215700090209     c     *hival        setll     tivin00r
215800090209     c     leggiAncora   tag
215900090209     c                   readp     tivin00r
216000090209     c                   if        not %eof(tivin00r)
216100090209      *
216200090209     c                   movel(p)  VINDTA        dati
216300090209     c                   if        dati = *blank
216400090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
216500090209      * le leggo e le escludo.
216600090209     C                   eval      vinFLG = '1'
216700090209     c                   update    Tivin000
216800090209     c                   goto      leggiAncora
216900090209     c                   end
217000090209      * ?              /*---------------------- */
217100090210     c                   exsr      Decod_Segmento
217200090209      * ?              /*---------------------- */
217300090209     c                   endif
217400090209
217500090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
217600090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
217700090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
217800090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
217900090209      * ?-----> Errore
218000090209     C                   eval      se_errore = 'S'
218100090209     c                   end
218200090209
218300090209     c                   endSR
218400090209      * ?------------------------------------------------------------------ */
218500090209      *?    Decodifica il segmento ed importa i campi in formato DS
218600090209      * ?------------------------------------------------------------------ */
218700090209     c     Decod_SegmentoBegsr
218800090209      **
218900090209     c                   eval      tipo_seg = %subst(vinDTA:1:3)
219000090209     c                   eval      segmento = vinDTA
219100100719      *
219200100719     c                   clear                   seg_imprevisto
219300100719     c                   clear                   trovato_seg
219400100716      *
219500100716     C                   clear                   keyUNBCLI
219600100716     C                   clear                   keyTIPOMSG
219700100716     C                   clear                   keyVERSION
219800100716     C                   clear                   keyRELEASE
219900100716     C                   clear                   keyAGENCY
220000100716     C                   clear                   keyASSOCIA
220100100716      *
220200100719      *  Chiamata generica x decodificare il singolo segmento letto
220300100719     c                   call      'TRTCT00R1'
220400100719      * input
220500100719     c                   parm                    tipo_seg
220600100719     c                   parm                    segmento
220700100719     C                   parm                    keyUNBCLI
220800100719     C                   parm                    keyTIPOMSG
220900100719     C                   parm                    keyVERSION
221000100719     C                   parm                    keyRELEASE
221100100719     C                   parm                    keyAGENCY
221200100719     C                   parm                    keyASSOCIA
221300100719      *output
221400100719     c                   parm                    Errore
221500100719     c                   parm                    DsGenerica
221600100719     c                   parm                    Valori_inErr
221700100719     c                   parm                    Descr_Errore
221800100719     c                   parm                    trovato_seg
221900100719      **--------
222000100719      *  Ritorna la DS GENERICA oppure l'errore
222100090210      **--------
222200090211     c                   select
222300100719      *
222400100719     c                   when      trovato_seg ='N'
222500100719      *
222600100719     c                   move      'S'           seg_imprevisto
222700100719     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
222800100719     c                   exsr      Error_DET
222900100719     c                   goto      end_Decod
223000100719      **--------
223100100719     c                   when      tipo_seg = 'UNA'
223200100719     c                   movel(p)  DsGenerica    EDS00UNA
223300100719      **--------
223400100719      *
223500100719     c                   when      tipo_seg = 'UNB'
223600100719     c                   movel(p)  DsGenerica    EDS00UNB
223700090210     C                   EXSR      DATI_UNB
223800090210      * --------
223900090209     c                   when      tipo_seg = 'UNH'
224000100719     c                   movel(p)  DsGenerica    EDS00UNH
224100090210     C                   EXSR      DATI_UNH
224200090210      * --------
224300090209     c                   when      tipo_seg = 'BGM'
224400100719     c                   movel(p)  DsGenerica    EDS00BGM
224500090210      **--------
224600090209     c                   when      tipo_seg = 'DTM'
224700100719     c                   movel(p)  DsGenerica    EDS00DTM
224800090210     C                   EXSR      DATI_DTM
224900090210      * --------
225000090209     c                   when      tipo_seg = 'TSR'
225100100719     c                   movel(p)  DsGenerica    EDS00TSR
225200090210      **--------
225300090209     c                   when      tipo_seg = 'CNT'
225400100719     c                   movel(p)  DsGenerica    EDS00CNT
225500090210      * --------
225600090209     c                   when      tipo_seg = 'TOD'
225700100719     c                   movel(p)  DsGenerica    EDS00TOD
225800090210     C                   EXSR      DATI_TOD
225900090210      **--------
226000090209     c                   when      tipo_seg = 'RFF'
226100100719     c                   movel(p)  DsGenerica    EDS00RFF
226200090210     C                   EXSR      DATI_RFF
226300090210      **--------
226400090209     c                   when      tipo_seg = 'TDT'
226500100719     c                   movel(p)  DsGenerica    EDS00TDT
226600090210      **--------
226700090213     c                   when      tipo_seg = 'MOA'
226800100719     c                   movel(p)  DsGenerica    EDS00MOA
226900090213     C                   EXSR      DATI_MOA
227000090213      **--------
227100090213     c                   when      tipo_seg = 'FTX'
227200100719     c                   movel(p)  DsGenerica    EDS00FTX
227300090213     C                   EXSR      DATI_FTX
227400090213      **--------
227500090209     c                   when      tipo_seg = 'NAD'
227600100719     c                   movel(p)  DsGenerica    EDS00NAD
227700090210     C                   EXSR      DATI_NAD
227800090210      **--------
227900090209     c                   when      tipo_seg = 'CTA'
228000100719     c                   movel(p)  DsGenerica    EDS00CTA
228100090210     C                   EXSR      DATI_CTA
228200090210      **--------
228300090209     c                   when      tipo_seg = 'COM'
228400100719     c                   movel(p)  DsGenerica    EDS00COM
228500090210     C                   EXSR      DATI_COM
228600090210      **--------
228700090209     c                   when      tipo_seg = 'GID'
228800100719     c                   movel(p)  DsGenerica    EDS00GID
228900090210     C                   EXSR      DATI_GID
229000090210      **--------
229100090209     c                   when      tipo_seg = 'MEA'
229200100719     c                   movel(p)  DsGenerica    EDS00MEA
229300090210     C                   EXSR      DATI_MEA
229400090210      **--------
229500090209     c                   when      tipo_seg = 'PCI'
229600100719     c                   movel(p)  DsGenerica    EDS00PCI
229700090210     C                   EXSR      DATI_PCI
229800090210      **--------
229900090209     c                   when      tipo_seg = 'UNT'
230000100719     c                   movel(p)  DsGenerica    EDS00UNT
230100090210     C                   EXSR      DATI_UNT
230200090210      **--------
230300090209     c                   when      tipo_seg = 'UNZ'
230400100719     c                   movel(p)  DsGenerica    EDS00UNZ
230500090210     C                   EXSR      DATI_UNZ
230600090210      **--------
230700100719     c                   OTHER
230800100719      **--------
230900100719     c                   move      'S'           seg_imprevisto
231000100719     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
231100100719     c                   exsr      Error_DET
231200100719     c                   goto      end_Decod
231300100719      **--------
231400100719     c                   endSL
231500100719      **
231600100719     c     end_Decod     endSR
231700090209     c*==================================================================*
231800090209     C*? CNVDAT - DECODIFICA LA DATA
231900090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
232000090209     C*         - WDATA  (DATA ALFANUMERICA)
232100090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
232200090209     C*         - WHMS   (ORA NUMERICA)
232300090209     C*----------------------------------------------------------------
232400090209     C     CNVDAT        BEGSR
232500090209     C*
232600090209     C                   MOVEL     ' '           WERRO             1
232700090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
232800090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
232900090209     C                   Z-ADD     *ZEROS        WHMS              6 0
233000090209     C*
233100090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
233200090209     C                   MOVEL     WDATA         WCOMO8            8
233300090209     C                   TESTN                   WCOMO8               99
233400090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
233500090209     C  N99              MOVEL     'S'           WERRO             1
233600090209     C                   END
233700090209     C*
233800090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
233900090209     C                   MOVEL     WDATA         WCOMO14          14
234000091216      *
234100091216     c                   if        %subst(WCOMO14:13:2) = *blank
234200091216     c                   eval      %subst(WCOMO14:13:2) = '00'
234300091216     c                   end
234400091216      *
234500090209     C                   TESTN                   WCOMO14              99
234600090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
234700090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
234800090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
234900090209     C  N99              MOVEL     'S'           WERRO             1
235000090209     C                   END
235100090209     C*
235200090209     C                   ENDSR
235300090210     C*----------------------------------------------------------------
235400090210     C*? TSTNUM - CONTROLLO NUMERICO
235500090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
235600090210     C*         - WLUNGH (Lunghezza campo output)
235700090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
235800090210     C*----------------------------------------------------------------
235900090210     C     TSTNUM        BEGSR
236000090210     C*
236100090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
236200090210     C                   MOVEL     'N'           WOKNUM            1
236300090210     C                   MOVEL     *BLANKS       WNUMOK           15
236400090210     C                   Z-ADD     15            IN                3 0
236500090210     C                   Z-ADD     WLUNGH        IX                3 0
236600090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
236700090210     C                   MOVEA     WNUM          NDA
236800090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
236900090210     C                   MOVEA     *ALL'0'       NDN
237000090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
237100090210     C                   Z-ADD     WLUNGH        I                 3 0
237200090210     C     I             DOWGT     0                                            --- 1 -->
237300090210     C* ESTRAE IL CARATTERE DA ESAMINARE
237400090210     C     1             SUBST     WNUM:I        WTEM01            1
237500090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
237600090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
237700090210     C     WTEM01        SCAN      WCA                                    99
237800090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
237900090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
238000090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
238100090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
238200090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
238300090210     C* DA DESTRA VERSO SINISTRA
238400090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
238500090210     C                   MOVEL     WTEMP1        NDN(IN)
238600090210     C                   SUB       1             IN
238700090210     C                   END                                                    <-- 3 ---
238800090210     C                   END                                                    <-- 2 ---
238900090210     C                   SUB       1             I
239000090210     C                   END                                                    <-- 1 ---
239100090210     C*
239200090210     C                   MOVEA     NDN           WNDN             15
239300090210     C     WNDN          IFNE      *ZEROS
239400090210     C                   MOVEA     NDN           WNUMOK           15
239500090210     C                   MOVEL     'S'           WOKNUM            1
239600090210     C                   END
239700090210     C*
239800090210     C                   ENDSR
239900090210     C*----------------------------------------------------------------
240000090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
240100090210     C*----------------------------------------------------------------
240200090210     C     CNVCAP        BEGSR
240300090210     C*
240400090210     C                   MOVEL     *BLANKS       WCAP              9
240500090210     C     nad3251       IFNE      '0'
240600090210     C     '  '          SCAN      nad3251       LENGHT            2 0
240700090210     C                   SUB       1             LENGHT
240800090210     C     LENGHT        IFLE      5
240900090210     C     LENGHT        ANDGT     0
241000090210     C                   Z-ADD     LENGHT        T                 2 0
241100090210     C                   Z-ADD     5             S                 2 0
241200090210     C                   MOVEA     nad3251       CAP9
241300090210     C                   MOVEL     *ZEROS        CAP5
241400090210     C                   DO        LENGHT
241500090210     C                   MOVE      CAP9(T)       CAP5(S)
241600090210     C                   SUB       1             S
241700090210     C                   SUB       1             T
241800090210     C                   END
241900090210     C                   MOVEA     CAP5          WCAP5             5
242000090210     C     WCAP5         IFEQ      '0'
242100090210     C                   MOVEL     *BLANKS       WCAP
242200090210     C                   ELSE
242300090210     C                   MOVEA     CAP5          WCAP
242400090210     C                   END
242500090210     C*
242600090210     C                   ELSE
242700090210     C                   MOVEL     nad3251       WCAP
242800090210     C                   END
242900090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
243000090210     C     WCAP          IFNE      *BLANKS
243100090210     C     WFLCPF        ANDNE     0
243200090210     C                   MOVEL     WFLCPF        WELE              1 0
243300090210     C                   MOVE      WFLCPF        WPOS              1 0
243400090210     C                   MOVEL     *BLANK        WCPF
243500090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
243600090210     C                   ELSE
243700090210     C                   MOVEL     *BLANKS       WCPF
243800090210     C                   END
243900090210     C                   END
244000090210     C*
244100090210     C                   ENDSR
244200090211      * ?================================================================== */
244300090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
244400090211      * ?================================================================== */
244500090211     C     Scrive_VAB    BEGSR
244600090211      *
244700090211     c                   clear                   err_bloccante     1
244800090211     c                   move      'S'           Almeno_Uno
244900090211      *
245000090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
245100090211     c                   exsr      UPDVAB
245200090211      *
245300090211     C* >>>>>>>>>>>>
245400090211     C*  Richiamo pgm per scrittura immediata VAB
245500090211     C     §PUWRK        IFEQ      ' '
245600090211      *
245700090211     C*  Richiamo pgm per esecuzione stampa
245800090211     C     §PTCMR        IFEQ      'S'
245900090213     C                   MOVEL     WNRDOC        d86CMR
246000090211     C                   ELSE
246100090213     C                   MOVEL     WNRDOC        d86CMR
246200090211     C                   END
246300090211      *
246400090213     C                   MOVEL     ' '           d86RIE
246500090213     C                   MOVEL     §PUDTA        d86DTA
246600090213     C                   Z-ADD     1             d86CNT
246700090213     C                   Z-ADD     §PTKSC        d86CCM
246800090213     C                   MOVEL     *BLANKS       d86STA
246900090213     C                   MOVEL     'E'           d86MOD
247000090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
247100090211      * dove poi imposta la DS dei parametri passati
247200090213     C                   MOVEL     'LR'          d86CHI
247300090213     C                   MOVEL     'N'           d86CTL
247400090211      *
247500090211     c                   move      kpjbu         SvKpjbu
247600090211     c                   clear                   kpjbu
247700090216     C                   movel     *on           Commit_on         1
247800090213     C                   MOVEL     TRTC86ds      KPJBU
247900090213      * ?- Chiamata la TRTC86R2 -*
248000090213     C                   CALL      'TRTC86R2'
248100090211     C                   PARM                    KPJBA
248200090216     C                   PARM                    Commit_on
248300090211     c                   move      Svkpjbu       Kpjbu
248400090211      *
248500090211     C*  Controllo pgm per scrittura immediata VAB
248600090211     C     WW            IFNE      0
248700090211     C                   DO        WW            WX                3 0
248800090213     C                   Z-ADD     KCL(WX)       d86CCM
248900090211      *
249000090211     c                   move      kpjbu         SvKpjbu
249100090211     c                   clear                   kpjbu
249200090216     C                   movel     *on           Commit_on         1
249300090213     C                   MOVEL     TRTC86ds      KPJBU
249400090213      * ?- Chiamata la TRTC86R2 -*
249500090213     C                   CALL      'TRTC86R2'
249600090211     C                   PARM                    KPJBA
249700090216     C                   PARM                    Commit_on
249800090211     c                   move      Svkpjbu       Kpjbu
249900090211     C                   END
250000090211     C                   END
250100090211      *
250200090211     C                   END
250300090211      * >>>>>>>>>>>>
250400090211     C                   if        se_errore  = 'S'
250500090211     c                   move      'S'           err_bloccante
250600090211     c                   goto      Error_VAB
250700090211     c                   end
250800090211     C*
250900090211     C     Error_VAB     Endsr
251000090211     C*----------------------------------------------------------------
251100090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
251200090211     C*----------------------------------------------------------------
251300090211     C     UPDVAB        BEGSR
251400090211      *
251500090211     C*  Se non è stato impostato il codice bolla lo imposto io
251600090211     C     VABCBO        IFEQ      *BLANKS
251700090211     C     VABCAS        IFGT      0
251800090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
251900090211     C                   ELSE
252000090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
252100090211     C                   END
252200090211     C                   END
252300110922      * Imposta le NOte
252400110922     C     VABNOT        IFEQ      *BLANKS
252500110922     C                   MOVEL     wnot1         VABNOT
252600110922     C                   MOVEL     wnot2         VABNT2
252700110922     C                   ELSE
252800110922     C                   MOVEL     wnot1         VABNT2
252900110922     C                   END
253000110922      *
253100160405     C     VABNAS        IFEQ      *BLANKS
253200160405     C                   MOVEL     wnatMerce     VABNAS
253300160405     C                   END
253400090211      *
253500090211     C*  Attribuisco anno spedizione
253600090211     C     WDATPA        IFGT      0                                            anno sped.
253700090211     C                   MOVEL     WDATPA        VABAAS
253800090211     C                   ELSE
253900090211     C     WDATFV        IFGT      0
254000090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
254100090211     C                   END
254200090211     C                   END
254300090211      *
254400090211      *  Controllo dettaglio segnacolli
254500090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
254600090211     C                   EXSR      CTRSGE
254700090211     C                   ELSE
254800090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
254900090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
255000090211     C                   EXSR      CTRSGN
255100090211     C                   END
255200090211     C                   END
255300090211      *
255400090212      *  Imposto linea partenza e serie
255500090211     C                   eval      VABLNP = VABLNP_PT
255600090212     C                   eval      VABNRS = VABnrs_PT
255700090211      *
255800090211      *  Controllo se devo variare il codice trattamento merce
255900090211     C                   eval      VABCTM = VABctm_PT
256000090211      *
256100090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
256200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
256300090211     c                   clear                   mantiene_nsp      1            *blk/'S'
256400090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
256500090211      *
256600090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
256700090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
256800090211      *
256900090211      *  Imposto codice cliente
257000090211     c                   Select
257100090211      *
257200090211      *  Specificato codice da qualificatore FF (TD15)
257300090211     C                   When        WCLKSC <> *zeros
257400090211     C                   Z-ADD     WCLKSC        VABCCM
257500090211     C                   MOVEL     WCLTAR        VABCTR
257600090211      * forza LNP/CTM/NRS
257700090211     c                   if        WclLNP <> 0
257800090211     C                   eval      VABLNP = WclLNP
257900090211     c                   end
258000090211     c                   if        WclNRS <> 0
258100090211     C                   eval      VABNRS = WclNRS
258200090211     c                   end
258300090211     c                   if        WclCTM <> *blank
258400090211     C                   eval      VABCTM = WclCTM
258500090211     c                   end
258600090211      *
258700090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
258800090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
258900090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
259000090211     c                   end
259100090211      *
259200090211      *  Specificato codice da qualificatore FF (TT15)
259300090211     C                   When        $CLKSC <> *zeros
259400090211     C                   Z-ADD     $CLKSC        VABCCM
259500090211     C                   MOVEL     $CLTAR        VABCTR
259600090211      * forza LNP/CTM/NRS
259700090211     c                   if        $clLNP <> 0
259800090211     C                   eval      VABLNP = $clLNP
259900090211     c                   end
260000090211     c                   if        $clNRS <> 0
260100090211     C                   eval      VABNRS = $clNRS
260200090211     c                   end
260300090211     c                   if        $clCTM <> *blank
260400090211     C                   eval      VABCTM = $clCTM
260500090211     c                   end
260600090211      *
260700090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
260800090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
260900090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
261000090211     c                   end
261100090211      *
261200090211      *  Imposto dati da tabella - PT-
261300090211     C                   Other
261400090211      *
261500090211     C                   MOVEL     §PTKSC        VABCCM
261600090211     C                   MOVEL     §PTCTR        VABCTR
261700090211      *
261800090211     C                   Endsl
261900090211      *
262000090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
262100090211      *    o dalla relativa CL
262200090211     C                   MOVEL     VABlnp        VABfgs
262300090211      *
262400090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
262500090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
262600090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
262700090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
262800090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
262900090211      *
263000090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
263100090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
263200090211     c                   else
263300090211      *
263400090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
263500090211     C*  lo attribuisco io
263600090211      **              **------------------**
263700090211     c                   exsr      repnum
263800090211      **              **------------------**
263900090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
264000090211      **              **------------------**
264100090211     c                   end
264200090211      *
264300090211     C*  Altrimenti attribuisco data del giorno
264400090211     C     VABMGS        IFEQ      0
264500090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
264600090211     C                   MOVE      WOGGI         VABMGS                         = oggi
264700090211     C                   END
264800090211     C*  Eseguo posizionamento su EDiVAB
264900090211     C                   Z-ADD     VABAAS        KAAS
265000090211     C                   Z-ADD     VABlnp        KLNP
265100090211     C                   Z-ADD     VABNRS        KNRS
265200090211     C                   Z-ADD     VABNSP        KNSP
265300090211     C                   MOVEL     SAVBOL        SALVA
265400090211     C     KVAB1         CHAIN     EDiVAB1L                           30
265500090211     C                   MOVEL     SALVA         SAVBOL
265600090211     C*  Imposto dati CMR
265700090211     C                   Z-ADD     1             VABCNT
265800090211     **
265900090211      *** Attenzione se si deve trattare il VAX con il CMR
266000090211      *** il campo VABCNT deve essere sempre impostato a (1)
266100090211     C     §puWRK        ifEQ      'S'
266200090211     C     §puNSP        andEQ     'S'
266300090211     C                   Z-ADD     1             VABCNT
266400090211     c                   end
266500090211     **
266600090211     C                   Z-ADD     WDTPRE        VABDTS
266700090212     C                   movel     '20'          VABDTS
266800090212     C     WHMPRE        mult      100           VABHMS
266900090211     C     §PTCMR        IFEQ      'S'
267000090211     C                   Z-ADD     WDTCMR        VABDCM
267100090211     C                   MOVEL     WNRCMR        VABCMR
267200090211     C                   ELSE
267300090211     C                   Z-ADD     WDATFV        VABDCM
267400090211     C                   MOVEL     WNUMFV        VABCMR
267500090211     C                   END
267600090211     C*  Se non è indicato il nome del mittente = Partner mittente
267700090211     C     VABRMO        IFEQ      *BLANKS
267800090211     C                   MOVEL     ACORAG        VABRMO
267900090211     C                   MOVEL     INDCAP        VABCMO
268000090211     C     INDCAE        IFNE      *BLANKS
268100090211     C                   MOVEL     INDCAE        VABCMO
268200090211     C                   END
268300090211     C     INDSTA        IFNE      *BLANKS
268400090211     C                   MOVEL     INDSTA        VABNMO
268500090211     C                   END
268600090211     C                   END
268700090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
268800090211     C     §1BFG1        IFEQ      'N'
268900090211     C     §1BFG7        ANDEQ     'N'
269000090211     C     §1BFG2        ANDEQ     'N'
269100090211     C***                  Z-ADD0         VABNRS
269200090211     C                   END
269300090211     C*  Imposto segancollo da - a
269400090211     C     §1BFG1        IFEQ      'S'
269500090211     C     §1BFG7        ANDEQ     'N'
269600090211     C     §1BF12        ANDNE     'E'
269700090211      *
269800090211     C     XY            IFEQ      *ZEROS
269900090211     C                   CLEAR                   VABNCD
270000090211     C                   CLEAR                   VABNCA
270100090211     C                   ELSE
270200090211     C                   Z-ADD     SGN(1)        VABNCD
270300090211     C                   Z-ADD     SGN(XY)       VABNCA
270400090211     C                   ENDIF
270500090211     C*
270600090211     C                   END
270700090211     C*
270800090211     C*  Se cap mittente originale a blank abblenco nazione
270900090211     C     VABCMO        IFEQ      *BLANKS
271000090211     C                   MOVEL     *BLANKS       VABNMO
271100090211     C                   END
271200090211     C*
271300090211      *? ------------------------------------------------------------------------
271400090213     C   30              WRITE     EDiVAB00                             99
271500090213     C  N30              UPDATE    EDiVAB00                             99
271600090211      *? ------------------------------------------------------------------------
271700090213     c   99              eval      errori_in_Wrt = 'S'
271800090211     C*
271900090211     c                   clear                   wri_vabtic        1
272000090211     C*
272100090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
272200090211     C                   EXSR      WRTVAT_Rif
272300090211     C*
272400090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
272500090211     C     §1BF12        IFEQ      'E'
272600090211     C                   EXSR      WRTVAT
272700090211     C                   ELSE
272800090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
272900090211     C     §1BFG7        IFEQ      'S'
273000090211     C     §1BFG1        OREQ      'S'
273100090211     C                   EXSR      WRTVAD
273200090211     C                   END
273300090211     C                   END
273400090211      *
273500090211     C* Reimposto codice trattamento merce cliente
273600090211     C                   MOVEL     WSVTRM        DS1B
273700090211     C**
273800090211     C* Do errore se previsto CMR ma non c'è
273900090211     C     §PTCMR        IFNE      ' '
274000090211     C                   EXSR      MEMCMR
274100090211     C                   END
274200090211     C* Do errore se previsto AFB ma non c'è
274300090211     C     §PTNFV        IFNE      ' '
274400090211     C                   EXSR      MEMAFB
274500090211     C                   END
274600090211     C*  Se il cliente vuole il ritorno delle date di consegna
274700090211     C*  è c'è una squdratura fra i segnacolli e il numero
274800090211     C*  dei colli che ci ha passato scrivo EDSUM
274900090211     C     §PUDTA        IFEQ      'S'
275000090211     C     WSQUAD        ANDEQ     'S'
275100090211     C                   EXSR      WRTSUM
275200090211     C                   END
275300090211     C*
275400090211     C                   ENDSR
275500090211     C*----------------------------------------------------------------
275600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
275700090211     C*----------------------------------------------------------------
275800090211     C     CTRSGN        BEGSR
275900090211     C*
276000090211     C                   MOVEL     'N'           WNSGER            1
276100090211     C*
276200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
276300090211     C     VABNCL        IFNE      XY
276400090211     C                   MOVEL     'S'           WNSGER
276500090211     C                   MOVEL     'S'           WSQUAD
276600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
276700090211     c                              bollettati'
276800090211     C                   GOTO      FINSGN
276900090211     C                   END
277000090211     C*  Riordino i segnacolli
277100090211     C     §1BFG7        IFEQ      'N'
277200090211     C                   SORTA     SGN
277300090211     C*  Controllo se sono sequenziali
277400090211     C                   MOVEL     'N'           WNOSEQ            1
277500090211     C     SGN(1)        ADD       1             WEXSGN            7 0
277600090211     C     2             DO        XY            X
277700090211     C     SGN(X)        IFNE      WEXSGN
277800090211     C                   MOVEL     'S'           WNOSEQ
277900090211     C                   END
278000090211     C     SGN(X)        ADD       1             WEXSGN
278100090211     C                   END
278200090211     C                   END
278300090211     C*
278400090211     C     FINSGN        ENDSR
278500090211     C*----------------------------------------------------------------
278600090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
278700090211     C*----------------------------------------------------------------
278800090211     C     CTRSGE        BEGSR
278900090211     C*
279000090211     C                   MOVEL     'N'           WNSGER            1
279100090211      *
279200090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
279300090211      *
279400090211     C     VABNCL        IFNE      §§
279500090211     C                   MOVEL     'S'           WNSGER
279600090211     C                   MOVEL     'S'           WSQUAD
279700090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
279800090211     c                              bollettati'
279900090211     C                   END
280000090211      *
280100090211     C                   ENDSR
280200090211     C*----------------------------------------------------------------
280300090211     C*? ESEGUO SCRITTURA EDiVAD
280400090211     C*----------------------------------------------------------------
280500090211     C     WRTVAD        BEGSR
280600090211     C*
280700090211     C* Se non seq. scrivo xy record
280800090211     C     §1BFG1        IFEQ      'S'
280900090211     C     XY            ANDNE     *ZEROS
281000090211     C*
281100090211     C     §1BFG7        IFEQ      'S'
281200090211     C                   DO        XY            X
281300090211     C                   Z-ADD     VABCCM        KCCM
281400090211     C                   Z-ADD     SGN(X)        KNCD
281500090211     C                   Z-ADD     VABCNT        VADCNT
281600090211     C                   MOVEL     VABCMR        VADCMR
281700090211     C                   Z-ADD     VABAAS        VADAAS
281800090211     C                   Z-ADD     VABLNP        VADLNP
281900090211     C                   Z-ADD     VABNRS        VADNRS
282000090211     C                   Z-ADD     VABNSP        VADNSP
282100090211     C                   Z-ADD     1             VADNCL
282200090211     C                   MOVEL     SGN(X)        VADCDP
282300090211     C                   Z-ADD     SGN(X)        VADNCD
282400090211     C                   Z-ADD     *ZEROS        VADNCA
282500090211     C                   Z-ADD     VABCCM        VADCCM
282600090211     C                   movel     VABfgs        VADfgs
282700090213     C                   WRITE     EDiVAD00                             99
282800090213     c   99              eval      errori_in_Wrt = 'S'
282900090211     C                   END
283000090211     C* Se sequenziali scrivo un solo record
283100090211     C                   ELSE
283200090211     C                   Z-ADD     VABAAS        VADAAS
283300090211     C                   Z-ADD     VABLNP        VADLNP
283400090211     C                   Z-ADD     VABNRS        VADNRS
283500090211     C                   Z-ADD     VABNSP        VADNSP
283600090211     C                   Z-ADD     VABNCL        VADNCL
283700090211     C                   Z-ADD     SGN(1)        VADNCD
283800090211     C                   Z-ADD     SGN(XY)       VADNCA
283900090211     C                   Z-ADD     VABCCM        VADCCM
284000090211     C                   movel     VABfgs        VADfgs
284100090213     C                   WRITE     EDiVAD00                             99
284200090213     c   99              eval      errori_in_Wrt = 'S'
284300090211     C                   END
284400090211     C*
284500090211     C                   ELSE
284600090211     C                   DO        XY            X
284700090211     C                   Z-ADD     VABCCM        KCCM
284800090211     C                   Z-ADD     SGN(X)        KNCD
284900090211     C                   Z-ADD     VABAAS        VADAAS
285000090211     C                   Z-ADD     VABLNP        VADLNP
285100090211     C                   Z-ADD     VABNRS        VADNRS
285200090211     C                   Z-ADD     VABNSP        VADNSP
285300090211     C                   MOVEL     SGN(X)        VADCDP
285400090211     C                   Z-ADD     1             VADNCL
285500090211     C                   Z-ADD     *ZEROS        VADNCD
285600090211     C                   Z-ADD     *ZEROS        VADNCA
285700090211     C                   Z-ADD     VABCCM        VADCCM
285800090211     C                   movel     VABfgs        VADfgs
285900090213     C                   WRITE     EDiVAD00                             99
286000090213     c   99              eval      errori_in_Wrt = 'S'
286100090211     C                   END
286200090211     C*
286300090211     C                   END
286400090211     C*
286500090211     C                   ENDSR
286600090211     C*----------------------------------------------------------------
286700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
286800090211     C*----------------------------------------------------------------
286900090211     C     WRTVAT_Rif    BEGSR
287000090211      *
287100090211      *  riferimento di consegna destinatario
287200090211     c                   if        WatRDE <> *blank
287300090211     C                   Z-ADD     VABCNT        VATCNT
287400090211     C                   MOVEL     VABCMR        VATCMR
287500090211     C                   Z-ADD     VABCCM        VATCCM
287600090211     C                   movel     VABfgs        VATfgs
287700090211     C                   Z-ADD     VABLNP        VATLNP
287800090211     C                   Z-ADD     VABAAS        VATAAS
287900090211     C                   Z-ADD     VABNRS        VATNRS
288000090211     C                   Z-ADD     VABNSP        VATNSP
288100090211     C                   MOVEL     'A'           VATTRC
288200090211     C                   MOVEL     WatRDE        VATNOT
288300090213     C                   WRITE     EDiVAT00                             99
288400090213     c   99              eval      errori_in_Wrt = 'S'
288500090211     c                   end
288600090211      *
288700090211      *  riferimento di consegna telefonico
288800090211     c                   if        WatTEL <> *blank
288900090211     C                   Z-ADD     VABCNT        VATCNT
289000090211     C                   MOVEL     VABCMR        VATCMR
289100090211     C                   Z-ADD     VABCCM        VATCCM
289200090211     C                   movel     VABfgs        VATfgs
289300090211     C                   Z-ADD     VABLNP        VATLNP
289400090211     C                   Z-ADD     VABAAS        VATAAS
289500090211     C                   Z-ADD     VABNRS        VATNRS
289600090211     C                   Z-ADD     VABNSP        VATNSP
289700090211     C                   MOVEL     'B'           VATTRC
289800090211     C                   MOVEL     WatTEL        VATNOT
289900090213     C                   WRITE     EDiVAT00                             99
290000090213     c   99              eval      errori_in_Wrt = 'S'
290100090211     c                   end
290200090211      *
290300090211      * riferimento del Partner che una volta veniva riportato sul BL4
290400090211     c                   if        Wnsp   <> *blank
290500090211     C                   Z-ADD     VABCNT        VATCNT
290600090211     C                   MOVEL     VABCMR        VATCMR
290700090211     C                   Z-ADD     VABCCM        VATCCM
290800090211     C                   movel     VABfgs        VATfgs
290900090211     C                   Z-ADD     VABLNP        VATLNP
291000090211     C                   Z-ADD     VABAAS        VATAAS
291100090211     C                   Z-ADD     VABNRS        VATNRS
291200090211     C                   Z-ADD     VABNSP        VATNSP
291300090211     C                   MOVEL     'C'           VATTRC
291400090211     C                   MOVEL     Wnsp          VATNOT
291500090213     C                   WRITE     EDiVAT00                             99
291600090213     c   99              eval      errori_in_Wrt = 'S'
291700090211     c                   end
291800090211      *
291900090211     C                   ENDSR
292000090211     C*----------------------------------------------------------------
292100090211     C*? ESEGUO SCRITTURA EDiVAT
292200090211     C*----------------------------------------------------------------
292300090211     C     WRTVAT        BEGSR
292400090211      *
292500090211     C                   DO        §§            X
292600090211     C                   Z-ADD     VABCNT        VATCNT
292700090211     C                   MOVEL     VABCMR        VATCMR
292800090211     C                   Z-ADD     VABCCM        VATCCM
292900090211     C                   movel     VABfgs        VATfgs
293000090211     C                   Z-ADD     VABLNP        VATLNP
293100090211     C                   Z-ADD     VABAAS        VATAAS
293200090211     C                   Z-ADD     VABNRS        VATNRS
293300090211     C                   Z-ADD     VABNSP        VATNSP
293400090211     C                   MOVEL     'E'           VATTRC
293500090211      *
293600090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
293700090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
293800090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
293900090211     c                   if        WVATlun > 0
294000090211      *
294100090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
294200090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
294300090211      *
294400090211     c                   else
294500090211      *
294600090211      * Se non è impostato nulla prende il segnacollo passato così com'è
294700090211     C                   MOVEL     SGE(X)        VATNOT
294800090211     c                   end
294900090211      *
295000090213     C                   WRITE     EDiVAT00                             99
295100090213     c   99              eval      errori_in_Wrt = 'S'
295200090211     C                   END
295300090211      *
295400090211     C                   ENDSR
295500090211     C*----------------------------------------------------------------
295600090211     C*? ESEGUO SCRITTURA EDSUM
295700090211     C*----------------------------------------------------------------
295800090211     C     WRTSUM        BEGSR
295900090211     C*
296000090211     C* Se non seq. scrivo xy record
296100090211     C                   DO        XY            X
296200090211     C                   Z-ADD     VABAAS        SUMAAS
296300090211     C                   Z-ADD     VABLNP        SUMFLS
296400090211     C                   MOVEL     WKSC          SUMLNP
296500090211     C                   Z-ADD     VABLNA        SUMLNA
296600090211     C                   Z-ADD     VABZNC        SUMZNC
296700090211     C                   Z-ADD     VABNRS        SUMNRS
296800090211     C                   Z-ADD     VABNSP        SUMNSP
296900090211     C                   Z-ADD     SGN(X)        SUMNSC
297000090211     C                   Z-ADD     WKSC          SUMCCM
297100090211     C                   MOVEL     *BLANKS       SUMSTS
297200090211     C                   MOVEL     *BLANKS       SUMFLG
297300090211     C                   MOVEL     WPRGSP        SUMCNI
297400090211     C                   MOVEL     VABCMR        SUMCMR
297500090211     C                   Z-ADD     0             SUMNFE
297600090211     C                   Z-ADD     VABCCM        VADCCM
297700090213     C                   WRITE     EDSUM000                             99
297800090213     c   99              eval      errori_in_Wrt = 'S'
297900090211     C                   END
298000090211     C*
298100090211     C                   ENDSR
298200090211     c*==================================================================*
298300090211      * Manda un Msg in Posta AS Sede a EDP*
298400090211     c*==================================================================*
298500090211     c     SEND_MSG_EDP  begsr
298600090211      *
298700090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
298800090211      *   Lo manda una sola volta per lavoro
298900090211     c                   IF        Snd_Msg_alert  <> 'N' and
299000090211     c                             User_msg <> *blank
299100090211     c                   z-add     1             Jx
299200090211     C                   exsr      CalQEZSNDMG
299300090211     c                   end
299400090211      *
299500090211     c                   endsr
299600090213     c**********************************************************************
299700090213     c* reperimento numero Spedizione
299800090213     c**********************************************************************
299900090213     C     repnum        BEGSR
300000090213      *
300100090213      *    deve controllare sulla tabella "3C" se il numero spedizione
300200090213      *     deve essere mantenuto oppure no
300300090213     C*
300400090213     C                   clear                   Ds3C
300500090213     C                   Z-ADD     CODUT         TBLKUT
300600090213     C                   MOVEL     '3C'          TBLCOD
300700090213     C                   movel(p)  VABccm        TBLKEY
300800090213     C     KTABel        CHAIN     TABEL00F                           30
300900090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
301000090213     C                   MOVEL     TBLUNI        Ds3C
301100090213     C                   END
301200090213      *
301300090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
301400090213     c                   if        §3CFsp <> 'S'
301500090213      *
301600090213     C* NSP => Stacco un numeratore da AZNUM
301700090213     c                   movel     kpjbu         svkpjbu
301800090213     c                   clear                   kpjbu
301900090213     C                   clear                   TRUL33DS
302000090213     C                   eval      I33OPE = *zeros
302100090213     C                   eval      I33CNU = 302
302200090213     C                   eval      I33NUM = 1
302300090213     C                   movel     TRUL33DS      KPJBU
302400090213     C                   call      'TRUL33R'
302500090213     C                   parm                    KPJBA
302600090213     C                   movel     KPJBU         TRUL33DS
302700090213     c                   movel     svkpjbu       kpjbu
302800090213      ****
302900090213     C                   if        O33ERR = *zeros
303000090213     c                   z-add     o33nrf        NUM_SPED
303100090213     C                   else
303200090213     C                   endif
303300090213      ****
303400090213     c                   ELSE
303500090213      *   se si vuole mantenere il numero spedizione
303600090213      ****
303700090213     C                   MOVEL     WOGGI         WANNO             4 0
303800090213     C                   MOVE      WANNO         KAAA
303900090213     C                   Z-ADD     3             KCNU
304000090213     C                   Z-ADD     VABlnp        KFIL
304100090213     C     KNUF          CHAIN     FLNUF                              9999
304200090213     C     *IN99         IFEQ      '0'
304300090213     C                   ADD       1             NUFNUM
304400090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
304500090213     C                   UPDATE    FLNUF
304600090213     C                   ELSE
304700090213     C                   END
304800090213     C*
304900090213     c                   EndIF
305000090213      **
305100090213     C                   ENDSR
305200090213     C*----------------------------------------------------------------
305300090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
305400090213     C*----------------------------------------------------------------
305500090213     C     MEMCMR        BEGSR
305600090213     C*
305700090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
305800090213     C     §PTCM1        IFEQ      'S'
305900090213     C                   CLEAR                   EDRDE000
306000090213     C                   MOVEL     'TD1154'      KNMC
306100090213     C                   Z-ADD     1             KNRP
306200090213     C                   MOVEL     WKSC          KLNP1
306300090213     C     KRDE          CHAIN     EDRDE01L                           31
306400090213     C     *IN31         IFEQ      '1'
306500090213     C                   Z-ADD     VABAAS        RDEAAS
306600090213     C                   Z-ADD     VABLNP        RDELNP
306700090213     C                   Z-ADD     VABNRS        RDENRS
306800090213     C                   Z-ADD     VABNSP        RDENSP
306900090213     C                   MOVEL     'TD1154'      RDENMC
307000090213     C                   Z-ADD     1             RDENRP
307100090213     C                   MOVEL     WNRCMR        RDEVAL
307200090213     C                   WRITE     EDRDE000                             99
307300090213     c   99              eval      errori_in_Wrt = 'S'
307400090213     C                   END
307500090213     C*  Memorizzo qualificatore CMR
307600090213     C                   CLEAR                   EDRDE000
307700090213     C                   MOVEL     'TD1153'      KNMC
307800090213     C                   Z-ADD     1             KNRP
307900090213     C                   MOVEL     WKSC          KLNP1
308000090213     C     KRDE          CHAIN     EDRDE01L                           31
308100090213     C     *IN31         IFEQ      '1'
308200090213     C                   Z-ADD     VABAAS        RDEAAS
308300090213     C                   Z-ADD     VABLNP        RDELNP
308400090213     C                   Z-ADD     VABNRS        RDENRS
308500090213     C                   Z-ADD     VABNSP        RDENSP
308600090213     C                   Z-ADD     1             RDENRP
308700090213     C                   MOVEL     'TD1153'      RDENMC
308800090213     C                   MOVEL     'CMR'         RDEVAL
308900090213     C                   WRITE     EDRDE000                             99
309000090213     c   99              eval      errori_in_Wrt = 'S'
309100090213     C                   END
309200090213     C*  Memorizzo la data
309300090213     C                   CLEAR                   EDRDE000
309400090213     C                   MOVEL     'TD2380'      KNMC
309500090213     C                   Z-ADD     1             KNRP
309600090213     C                   MOVEL     WKSC          KLNP1
309700090213     C     KRDE          CHAIN     EDRDE01L                           31
309800090213     C     *IN31         IFEQ      '1'
309900090213     C                   Z-ADD     VABAAS        RDEAAS
310000090213     C                   Z-ADD     VABLNP        RDELNP
310100090213     C                   Z-ADD     VABNRS        RDENRS
310200090213     C                   Z-ADD     VABNSP        RDENSP
310300090213     C                   Z-ADD     1             RDENRP
310400090213     C                   MOVEL     'TD2380'      RDENMC
310500090213     C                   MOVEL     WDTCMR        RDEVAL
310600090213     C                   WRITE     EDRDE000                             99
310700090213     c   99              eval      errori_in_Wrt = 'S'
310800090213     C                   END
310900090213     C*
311000090213     C                   END                                                    §PTCM1 = 'S'
311100090213     C*
311200090213     C                   ENDSR
311300090213     C*----------------------------------------------------------------
311400090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
311500090213     C*----------------------------------------------------------------
311600090213     C     MEMAFB        BEGSR
311700090213     C*
311800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
311900090213     C     §PTNF1        IFEQ      'S'
312000090213     C                   CLEAR                   EDRDE000
312100090213     C                   MOVEL     WKSC          KLNP1
312200090213     C                   MOVEL     'TD1154'      KNMC
312300090213     C                   Z-ADD     1             KNRP
312400090213     C     KRDE          CHAIN     EDRDE01L                           31
312500090213     C     *IN31         IFEQ      '1'
312600090213     C                   Z-ADD     VABAAS        RDEAAS
312700090213     C                   Z-ADD     VABLNP        RDELNP
312800090213     C                   Z-ADD     VABNRS        RDENRS
312900090213     C                   Z-ADD     VABNSP        RDENSP
313000090213     C                   MOVEL     'TD1154'      RDENMC
313100090213     C     §PTCM1        IFNE      'S'
313200090213     C     WNRCMR        OREQ      *BLANKS
313300090213     C                   Z-ADD     1             RDENRP
313400090213     C                   ELSE
313500090213     C                   Z-ADD     2             RDENRP
313600090213     C                   END
313700090213     C                   MOVEL     WNUMFV        RDEVAL
313800090213     C                   WRITE     EDRDE000                             99
313900090213     c   99              eval      errori_in_Wrt = 'S'
314000090213     C                   END
314100090213     C*  Memorizzo qualificatore CMR
314200090213     C                   CLEAR                   EDRDE000
314300090213     C                   MOVEL     WKSC          KLNP1
314400090213     C                   MOVEL     'TD1153'      KNMC
314500090213     C                   Z-ADD     1             KNRP
314600090213     C     KRDE          CHAIN     EDRDE01L                           31
314700090213     C     *IN31         IFEQ      '1'
314800090213     C                   Z-ADD     VABAAS        RDEAAS
314900090213     C                   Z-ADD     VABLNP        RDELNP
315000090213     C                   Z-ADD     VABNRS        RDENRS
315100090213     C                   Z-ADD     VABNSP        RDENSP
315200090213     C     §PTCM1        IFNE      'S'
315300090213     C     WNRCMR        OREQ      *BLANKS
315400090213     C                   Z-ADD     1             RDENRP
315500090213     C                   ELSE
315600090213     C                   Z-ADD     2             RDENRP
315700090213     C                   END
315800090213     C                   MOVEL     'TD1153'      RDENMC
315900090213     C                   MOVEL     'AFB'         RDEVAL
316000090213     C                   WRITE     EDRDE000                             99
316100090213     c   99              eval      errori_in_Wrt = 'S'
316200090213     C                   END
316300090213     C*  Memorizzo la data
316400090213     C                   CLEAR                   EDRDE000
316500090213     C                   MOVEL     WKSC          KLNP1
316600090213     C                   MOVEL     'TD2380'      KNMC
316700090213     C                   Z-ADD     1             KNRP
316800090213     C     KRDE          CHAIN     EDRDE01L                           31
316900090213     C     *IN31         IFEQ      '1'
317000090213     C                   Z-ADD     VABAAS        RDEAAS
317100090213     C                   Z-ADD     VABLNP        RDELNP
317200090213     C                   Z-ADD     VABNRS        RDENRS
317300090213     C                   Z-ADD     VABNSP        RDENSP
317400090213     C     §PTCM1        IFNE      'S'
317500090213     C     WNRCMR        OREQ      *BLANKS
317600090213     C                   Z-ADD     1             RDENRP
317700090213     C                   ELSE
317800090213     C                   Z-ADD     2             RDENRP
317900090213     C                   END
318000090213     C                   MOVEL     'TD2380'      RDENMC
318100090213     C                   MOVEL     WDATFV        RDEVAL
318200090213     C                   WRITE     EDRDE000                             99
318300090213     c   99              eval      errori_in_Wrt = 'S'
318400090213     C                   END
318500090213     C*
318600090213     C                   END                                                    §PTCM1 = 'S'
318700090213     C*
318800090213     C                   ENDSR
318900090213     c*==================================================================*
319000090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
319100090213     c*==================================================================*
319200090213     c     Imp_ADDR_mail begsr
319300090213      *
319400090213     c                   clear                   Indirizzi
319500090213      *
319600090213      *  Lunghezza indirizzi presenti
319700090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
319800090213     c                   z-add     1             al_char           3 0
319900090213     c                   z-add     1             dal_char          3 0
320000090213     c                   z-add     1             tot_char          3 0
320100090213      *
320200090213     c     successivo    tag
320300090213      *
320400090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
320500090213      *   e aggiunge il dominio Bartolini + il (;) separatore
320600090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
320700090213     c                   eval      tot_char = al_char - dal_char
320800090213     c                   z-add     dal_char      Dac
320900090213     c                   z-add     tot_char      Luc
321000090213      *
321100090213     c                   clear                   Indir_Bartol     40
321200090213     c                   clear                   Indir_Parz       20
321300090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
321400090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
321500090213     c                             %Trim(bart_it)
321600090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
321700090213      *
321800090213     c                   if        al_char = Lun_Ind
321900090213     c                   goto      fine_indmail
322000090213     c                   else
322100090213     c                   eval      dal_char = ( al_char + 1 )
322200090213     c                   goto      successivo
322300090213     c                   end
322400090213      *
322500090213     C     fine_indmail  TAG
322600090213      *
322700090213     C                   ENDSR
322800090213     c*==================================================================*
322900090213      * Manda un Msg x E-mail
323000090213     c*==================================================================*
323100090213     c     Invio_Mail    begsr
323200090213      *
323300090213     c                   goto      non_inviare
323400090213      *
323500090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
323600090213     C* Inizializzo variabili
323700090213     C                   movel     *blanks       wrkEml          253
323800090213     C                   movel     *blanks       wrkMsg         5000
323900090213     C                   movel     *blanks       wrkOgg           44
324000090213      *
324100090213     C* Valorizzo i campi della e-m@ail - indirizzo
324200090213     C                   eval      wrkEml = Indirizzi
324300090213     C                   eval      wrkOgg = Oggetto
324400090213     C                   eval      wrkMsg = Messaggio
324500090213
324600090213     C                   call(e)   'TIS701C'
324700090213     C                   parm                    wrkEml
324800090213     C                   parm                    wrkOgg
324900090213     C                   parm                    wrkMsg
325000090213
325100090213     c     non_inviare   tag
325200090213     C*
325300090213     C                   ENDSR
325400090601     c*==================================================================*
325500090601      * Manda un Msg x E-mail a EDPAB
325600090601     c*==================================================================*
325700090601     c     Invio_Mail_AB begsr
325800090601      *
325900090601     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
326000090601     C* Inizializzo variabili
326100090601     C                   movel     *blanks       wrkEml          253
326200090601     C                   movel     *blanks       wrkMsg         5000
326300090601     C                   movel     *blanks       wrkOgg           44
326400090601      *
326500090601     C* Valorizzo i campi della e-m@ail - indirizzo
326600110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
326700110203     C                   eval      wrkEml = Indirizzi
326800110203     C                   eval      wrkOgg = Oggetto
326900110203     C                   eval      wrkMsg = Messaggio
327000110203      ********
327100110203     C********           call(e)   'TIS701C'
327200110203     C********           parm                    wrkEml
327300110203     C********           parm                    wrkOgg
327400110203     C********           parm                    wrkMsg
327500090601     C*
327600110203      ** *****
327700110203     C                   call(e)   'TRTCT00R2'
327800110203     C                   parm                    wrkEml
327900110203     C                   parm                    wrkOgg
328000110203     C                   parm                    wrkMsg
328100110203     C*
328200090601     C                   ENDSR
328300090211     ***********************************************************************
328400090211     **
328500090211     ** Send Message (QEZSNDMG) API
328600090211     **
328700090211     ***********************************************************************
328800090211     C     CalQEZSNDMG   BEGSR
328900090211      *
329000090211     ** Invio messaggio all'utente.
329100090211     C                   EVAL      SndMg03 = msgDSP
329200090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
329300090211     C                   EVAL      SndMg05(Jx) = User_msg
329400090211     C                   EVAL      SndMg06 = Jx
329500090211     C                   CLEAR                   QUSEC
329600090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
329700090211
329800090211     C                   CALL      'QEZSNDMG'
329900090211     C                   PARM                    SndMg01
330000090211     C                   PARM                    SndMg02
330100090211     C                   PARM                    SndMg03
330200090211     C                   PARM                    SndMg04
330300090211     C                   PARM                    SndMg05
330400090211     C                   PARM                    SndMg06
330500090211     C                   PARM                    SndMg07
330600090211     C                   PARM                    SndMg08
330700090211     C                   PARM                    Qusec
330800090211     C                   PARM                    SndMg10
330900090211     C                   PARM                    SndMg11
331000090211     C                   PARM                    SndMg12
331100090211      *
331200090211     C                   ENDSR
331300121102      *---------------------------------------------------------------*
331400121102      * *cotrollo capienza tabella PT
331500121102      *---------------------------------------------------------------*
331600121102     C     Check_PT      begsr
331700121102     C*
331800121102     c* Controllo riempimento schiera
331900121102     c                   clear                   trul0sds
332000121102     c                   eval      i0sski='CPT'
332100121102     c                   eval      i0sele=%elem(CPT)
332200121102     c                   eval      i0spie=x
332300121102     c                   eval      i0sfile='EDTAB00F'
332400121102     c                   eval      i0ssif=knsif
332500160405     c                   eval      i0spgm='TRTCT12R'
332600121113     c                   move      kpjbu         SvKpjbu
332700121113     c                   clear                   kpjbu
332800121102     c                   movel     trul0sds      kpjbu
332900121102     c                   call      'TRUL0SR'
333000121102     c                   parm                    kpjba
333100121113     c                   eval      kpjbu  =  SvKpjbu
333200121102     C*
333300121102     C                   ENDSR
333400090211     O*****************************************************************
333500090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
333600090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
333700090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
333800090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
