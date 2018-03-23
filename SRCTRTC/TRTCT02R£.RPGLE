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
019900090216     D CTC             S              2    DIM(100)
020000090216     D TPI             S              2    DIM(100)
020100090209      * Decodifiche servizi speciali
020200090209     D SS35            S             35    DIM(100)
020300090209     D SS              S              3    DIM(100)
020400090209     D DSS             S             90    DIM(100)
020500090209      * Schiere x routine di conversione CAP
020600090209     D CAP5            S              1    DIM(5)
020700090209     D CAP9            S              1    DIM(9)
020800090209     D CAPM            S              1    DIM(9)
020900090209      * Schiera per memorizzazione FTX ricevuti
021000090209     D QUA             S              3    DIM(100)
021100090209     D FTX             S             70    DIM(100)
021200090209      * Tabelle capiconto
021300090209      * Schiere x località
021400090209     D LOD             S              1    DIM(35)
021500060608      **?------------------------------------------------------------------ */
021600060608     C*? Ds Scrittura del VAT "E"  passaggio nuovi campi BIC3
021700060608      **?------------------------------------------------------------------ */
021800060612     D XTOEBCDDS     E DS
021900060620      *
022000060608      **?------------------------------------------------------------------ */
022100050112      * Numeratore Bolle (302)
022200050112     D trul33ds      E DS
022300050112     D Ds3C          E DS
022400090209     C*
022500090209      **?------------------------------------------------------------------ */
022600090209     C* Definizione campi di work
022700090209     D pVLB            s                   LIKE(VABVLB)
022800090209     D kKUT            s                   LIKE(TBLKUT)
022900090209     D kCOD            s                   LIKE(TBLCOD)
023000090209     D kKEY            s                   LIKE(TBLKEY)
023100090209     D kKCC            s                   LIKE(ACOKCC)
023200090209     D kKSC            s                   LIKE(ACOKSC)
023300090209     D kAAA            s                   LIKE(NUFAAA)
023400090209     D kCNU            s                   LIKE(NUFCNU)
023500090209     D kFIL            s                   LIKE(NUFFIL)
023600090209     D kAAS            s                   LIKE(VABAAS)
023700090209     D kLNP            s                   LIKE(VABLNP)
023800090209     D kNRS            s                   LIKE(VABNRS)
023900090209     D kNSP            s                   LIKE(VABNSP)
024000090209     D kCMR            s                   LIKE(VABCMR)
024100090209     D kCCM            s                   LIKE(VABCCM)
024200090209     D kNCD            s                   LIKE(VADNCD)
024300090210     D kCNT            s                   LIKE(VADCNT)
024400090209     D kNMC            s                   LIKE(RDENMC)
024500090209     D kNRP            s                   LIKE(RDENRP)
024600090209     D kLNP1           s                   LIKE(RDELNP)
024700090209      *
024800090209      *  Invio E-mail
024900090209     D Indirizzi       s            253
025000090209     D Oggetto         s             44
025100090209     D Messaggio       s           5000
025200090209      *
025300090209     d   DSmail        ds
025400090209     D Mail_Ind                      44
025500090209     d   IND_char                     1    dim(44)
025600090209      *
025700090209     D Lun_Ind         s              5u 0
025800090209      *
025900090209     D Dac             s              5u 0
026000090209     D Luc             s              5u 0
026100090209      *
026200090209     C*---------------------------------------------------------------*
026300090209     D MsgDSP          S            256                                         Testo generico
026400090209     C*---------------------------------------------------------------*
026500090209     D SndMg01         S             10                                         Message type
026600090209     D                                     INZ('*INFO')
026700090209     D SndMg02         S             10                                         Deluvery mode
026800090209     D                                     INZ('*BREAK')
026900090209     D SndMg03         S            256                                         Message text
027000090209     D SndMg04         S             10I 0                                      Length of text
027100090209     D                                     INZ(%SIZE(SndMg03))
027200090209     D SndMg05         S             10                                         User profile
027300090209     D                                     DIM(299)
027400090209     D SndMg06         S             10I 0                                      Number of user
027500090209     D SndMg07         S             10I 0                                      Message sent indic.
027600090209     D SndMg08         S             10I 0                                      Function requested
027700090209     D SndMg10         S              1                                         Show display
027800090209     D                                     INZ('N')
027900090209     D SndMg11         S             20                                         Qualified MSGQ name
028000090209     D SndMg12         S              4                                         Name type indicator
028100090209     D                                     INZ('*USR')
028200090209      * indice di scihera
028300090209     D Jx              S              5I 0
028400090209      *
028500090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
028600090209      *
028700090209     d bart_it         C                   CONST('@Bartolini.it;')
028800090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
028900060612      * ?================================================================== */
029000060612      *
029100060612      * ?   * Campi x decodifica * (INPUT  del Record)
029200090130     D  Dati           s           2048
029300060612      * ?   *--------------------------------------------------------------*
029400060612     D  se_errore      s              1    inz(' ')
029500060612      * ?* ------------------------------------------------------ *
029600060710     D Digits          C                   '0123456789'
029700060612      * ?================================================================== */
029800060612      *   Ciclo principale
029900090205      * ?================================================================== */
030000060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
030100060612     C*
030200060612     C                   if        not %open(tivin00r)
030300060612     C                   open      tivin00r
030400060612     C                   endif
030500060612      **
030600060612      * ?------------------------------------------------------------------ */
030700060612     C*? Controllo dati arrivati da DPD
030800060612      * ?------------------------------------------------------------------ */
030900060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
031000060612      * ?- controllando che la trasmissione sia completa.
031100090205      **
031200060612      * ?              /*---------------------- */
031300090205     c                   goto      nonFare_adesso
031400060612     c                   exsr      check_Trasm
031500090205     c     nonFare_adessotag
031600060612      * ?              /*---------------------- */
031700090205      **
031800060613      **  Errore di trasmissione x tutti i records
031900060613      **   --> file in errore
032000060613     c                   if        se_errore = 'S'
032100060612
032200060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
032300090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
032400090205     c                   exsr      Msg_errato
032500060612      **
032600060612     c                   else
032700060614      **
032800060612      * ?------------------------------------------------------------------ */
032900060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
033000060612      * ?------------------------------------------------------------------ */
033100090205      **
033200060612      * ?              /*---------------------- */
033300060612     c                   exsr      Importa_Msg
033400060612      * ?              /*---------------------- */
033500060612
033600060614     c                   end
033700060614
033800060612     C                   if        %open(tivin00r)
033900060612     C                   close     tivin00r
034000060612     C                   endif
034100060612      *
034200060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
034300090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
034400090225     c                             esito ='0'
034500060710     C                   eval      esito ='0'
034600090224      *
034700090224      *  Il cliente vuole il CHECK di ricevuto della trasmissione
034800090224     c                   call      'TRTCT03R'
034900090224     c                   parm                    Mittente         35
035000090226     c                   parm                    Mitt_Qualifier    4
035100090226     c                   parm                    Id_Messaggio     14
035200090225      *
035300090225     c                   COMMIT
035400090224      *
035500090213     c                   if        almeno_un_due ='S'
035600090213     C                   eval      esito ='1'
035700060614     C                   endif
035800090213     C                   endif
035900060614      *
036000060612     c                   SETON                                        LR
036100060612      * ?================================================================== */
036200060612      *? Importa i records della tramsissione
036300060612      * ?------------------------------------------------------------------ */
036400060612     c     Importa_Msg   Begsr
036500060612
036600090211     C                   EXSR      AZZMSG
036700060614
036800060612     c     *start        setll     Tivin00r
036900060612     c                   read      Tivin00r
037000060612
037100060612     c                   dow       not %eof(Tivin00r)
037200060614
037300060614      * solo i record sflaggati da rielaborare
037400060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
037500090211      *  Sempre Record OK
037600090211      ** Controlli formali sui campi
037700090211     C                   eval      vinFLG = '1'
037800060612     c                   clear                   se_errore
037900060612
038000060612      ** Decodifica record a campi variabili
038100060612      * ?              /*---------------------- */
038200090205     c                   exsr      Decod_Segmento
038300090209      * ?              /*---------------------- */
038400090209
038500090213      *** ------------------------------------
038600090213      *** A FINE CICLO DI UN DETTAGLIO:   -->"UNT" x IFTMIN
038700090213      *** ------------------------------------
038800090213      * ?              /*---------------------- */
038900090213      ******   in base alla tabella PT occorre identificare il tipo di messaggio
039000090213      ******    e cosa chiude un dettaglio per poter scrivere il VAB
039100090213      * ?              /*---------------------- */
039200090213     c                   if        tipo_seg = 'UNT'
039300090213      *
039400090213      *  Deve flaggare il dettaglio con dei problemi
039500090213     C                   if        Err_in_detta = 'S'
039600090213      **?              /*---------------------- */
039700090213     c                   exsr      DETta_Errato
039800090213      **?              /*---------------------- */
039900090213     c                   end
040000090213      *
040100060612      **  Se presente un errore nel record emette una segnalazione msg
040200090213     c                   if        se_errore = 'S'
040300090213     c                   else
040400090209      ******
040500090209      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
040600090227     c                   if         se_trovato_NAD ='S'
040700060612      * ?              /*---------------------- */
040800090209     c                   exsr      Scrive_VAB
040900060612      * ?              /*---------------------- */
041000090227     c                   else
041100090227     c                   eval      errori_in_Wrt = 'S'
041200090227     c                   end
041300090227      *
041400090209      *  Problemi nella decodifica dei campi VAB/VAT
041500090213     c                   if        errori_in_Wrt = 'S'
041600090213      *
041700090213     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
041800090213     c                   exsr      Error_DET
041900090213      *
042000090213     c                   ROLBK
042100090213      *
042200090213     c                   else
042300090213     c                   COMMIT
042400090227      * Dopo aver confermato, con COMMIT
042500090227      *  si prepara per una nuova spedizione
042600090227      * ?                         --------------
042700090227     c                   exsr      Nuova_spediz
042800090227      * ?                         --------------
042900090213     c                   end
043000090209
043100090209     c                   end
043200090210      **
043300060620     c                   end
043400060612
043500060621      *  x errore bloccante nella composizione del VAB/VAT
043600060621     c                   if        err_bloccante ='S'
043700060621     C                   eval      vinFLG = '2'
043800090211     c                   eval      esito  = '2'
043900090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
044000090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
044100090601     C                             Bolle import in filiale - messaggio -
044200090601     C                             con UNB non presente in tabella PT controlla-
044300090601     C                             re EDI ricevuto su UPLOAD.'
044400090601     c                   exsr      invio_mail_AB
044500060621     c                   end
044600060612
044700060612     c                   update    Tivin000
044800090211
044900090211      *** se non è riconosciuto l'UNB deve uscire direttamente
045000090211     c                   if        se_errore ='S'  and
045100090211     c                             tipo_seg = 'UNB'
045200090213     c                   eval      err_bloccante = 'S'
045300090211     c                   exsr      Msg_errato
045400090211     c                   LEAVE
045500090211     c                   endIF
045600090211      ***
045700060614     c                   endIF
045800060612
045900060612     c                   read      Tivin00r
046000060612     C                   ENDdo
046100060612      **
046200060612     c                   endSR
046300090211     C*----------------------------------------------------------------
046400090211     C*? AZZMSG - Azzera dati messaggio
046500090211     C*----------------------------------------------------------------
046600090211     C     AZZMSG        BEGSR
046700090211     C*
046800090211     C* Pulisco dati di work x foglio viaggio
046900090211     C                   MOVEL     *BLANKS       WNUMFV           35
047000090211     C                   MOVEL     *BLANKS       WNRCMR           35
047100090211     C                   Z-ADD     0             WDATPA            8 0
047200090211     C                   Z-ADD     0             WDATFV            8 0
047300090211     C                   Z-ADD     0             WHHNFV            4 0
047400090211     C                   Z-ADD     0             WDTCMR            8 0
047500090211     C                   Z-ADD     0             WHHCMR            4 0
047600090211      *
047700090211      * pulisce gli RFF+FF di testata
047800090211     C                   clear                   $CLKSC            7 0
047900090211     C                   clear                   $CLTAR            3
048000090211     C                   clear                   $CLLNP            3 0
048100090211     C                   clear                   $CLNRS            2 0
048200090211     C                   clear                   $CLCTM            2
048300090211     C                   clear                   $CLBS1            1
048400090211     C                   clear                   $CLfnsp           1            *blk/S
048500090211      *
048600090211     C*  Inizializzo variabile new documento
048700090211     C                   Z-ADD     0             KCL
048800090211     C                   Z-ADD     0             WW                3 0
048900090211     ** Inizializza campo di work
049000090211     c                   move      'N'           WGID99
049100090211      * per controllare se almeno un record è stato importato sul VAB
049200090211     c                   clear                   Almeno_Uno        1
049300090213     c                   clear                   Almeno_Un_Due     1
049400090211     c                   eval      esito  = '0'
049500090211     C*
049600090211     C                   ENDSR
049700090210     C*----------------------------------------------------------------
049800090210      *? DECODIFICA DATI UNB
049900090210     C*----------------------------------------------------------------
050000090210     C     DATI_UNB      BEGSR
050100090210      *
050200090209      *  INIZIALIZZA  dati fondamentali messaggio
050300090209     C                   clear                   EDIDSPT
050400090209     C                   clear                   EDIDSPU
050500090209     C                   clear                   EDIDSPV
050600090209     C                   clear                   wnrDOC           35
050700090209     C                   move      'N'           PT_ok             1
050800090209      ** ---------------------------------------------------------------
050900090209      * quindi 1° cosa:
051000090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
051100090209      *     senza questo il messaggio NON è trascodificabile
051200090209     C                   MOVEL     unb0004       WCOD             35
051300090209     C                   MOVE      unb000720     WCOD
051400090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
051500090209     C                   Z-ADD     1             XX                3 0
051600090209     C     WCOD          LOOKUP    CPT(XX)                                32
051700090209      *
051800090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
051900090209     C                   If        *IN32 = *off
052000090209      * ******  Errore generale sul Messaggio
052100090209      * Msg di allerta Traduzione
052200090209     C                   EVAL      Indirizzi = CED_Bart
052300090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
052400090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
052500090209     C                             Bolle import in filiale - messaggio -
052600090209     C                             con UNB non presente in tabella PT controlla-
052700090209     C                             re EDI ricevuto su UPLOAD.'
052800090601     c                   exsr      invio_mail_AB
052900090209      *
053000090209     c                   exsr      invio_mail
053100090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
053200090210     C                   eval      vinFLG = '2'
053300090210     C                   eval      se_errore   = 'S'
053400090210     c                   goto      end_UNB
053500090209      *
053600090209     C                   move      'N'           PT_ok             1
053700090209      *
053800090209      *             *------------------------------*
053900090209     C                   eLSe
054000090209      *             *------------------------------*
054100090209      * Se tutto OK su tab.PT:
054200090209     C                   move      'S'           PT_ok             1
054300090209      *
054400090209     C                   MOVEL     DPT(XX)       EDIDSPT
054500090209     C                   MOVEL     DPU(XX)       EDIDSPU
054600090209     C                   MOVEL     DPV(XX)       EDIDSPV
054700090226     C                   eval      Mittente       = unb0004
054800090226     C                   eval      Mitt_qualifier = unb000720
054900100719     C                   eval      Id_Messaggio   = UNB0020
055000090209      *
055100090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
055200090209      *  ricevuti:
055300090209     c                   movel     §PVema        mail_ind         44
055400090209      *
055500090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
055600090209     c                   if        mail_ind <> *blank
055700090209     c                   exsr      imp_ADDR_mail
055800090209     c                   end
055900090209      *
056000090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
056100090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
056200090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
056300090209     C                   if        §PULUN = *blank
056400090209     C                   eval      §PULUN = '00'
056500090209     c                   end
056600090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
056700090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
056800090209      *
056900090209     C                   Z-ADD     §PTKSC        WKSC              7 0
057000090211     C                   MOVEL     §PTLNP        VABlnp_PT
057100090212     C                   MOVEL     §PTNRS        VABnrs_PT
057200090211     C                   MOVEL     §PTCTM        VABctm_PT
057300090209      *
057400090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
057500090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
057600090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
057700090209      *  i records trasmessi).
057800090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
057900090209      *
058000090209      *  In base al cod.trattamento merce reperisco tipo tratt.
058100090209     C                   CLEAR                   DS1B
058200090209     C                   Z-ADD     1             Y                 3 0
058300090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
058400090209     C     *IN32         IFEQ      '1'
058500090209     C                   MOVEL     DTM(Y)        DS1B
058600090209     C                   MOVEL     DS1B          WSVTRM           38
058700090209     C                   END
058800090209      *
058900090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
059000090209     C                   clear                   ACORAG
059100090209     C                   clear                   INDCAE
059200090209     C                   clear                   INDCAP
059300090209     C                   clear                   INDSTA
059400090209      * Decodifico partner
059500090209     C                   Z-ADD     1             KKUT
059600090209     C                   Z-ADD     KCI           KKCC
059700090209     C                   MOVE      §PTKSC        KKSC
059800090209     C     KACO          CHAIN     CNACO00F                           31
059900090209     C     KIND          CHAIN     CNIND00F                           31
060000090211      *
060100090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
060200090211      *   questa schiera è utilizzata anche per i clienti.
060300090211     C     §ptKSC        LOOKUP    KCL                                    39
060400090211     c                   If        *in39 = *off
060500090211     C                   add       1             WW
060600090211     C                   z-add     §ptKSC        KCL(WW)
060700090211     C                   Endif
060800090209      *
060900090209     C                   MOVEL     unb0017       WAA4              4 0
061000090209     C                   MOVE      WAA4          WAA2              2 0
061100090209     C                   MOVE      unb0017       WGG               2 0
061200090209     C                   MOVE      unb0017       WDTPRE            8 0
061300090209     C                   MOVE      unb0019       WHMPRE            4 0
061400090209     C                   MOVE      unb0017       WDTMSG            6 0
061500090209     C                   MOVE      WAA2          WDTMSG
061600090209     C                   MOVEL     WGG           WDTMSG
061700090210      * potremmo
061800090210      * Utilizzare l'identificativo della trasmissione come CMR
061900100719     C                   IF        unb0020 <> *blank
062000100719     C                   MOVEL     unb0020       WNRDOC
062100100719     C                   MOVEL     unb0020       WNUMFV
062200100719     C                   MOVEL     unb0020       WNRCMR
062300090209     c                   end
062400090209      *
062500090209     c                   Endif
062600090209      **
062700090210     c     end_UNB       endSR
062800090209      * ?================================================================== */
062900090210     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
063000090209      * ?================================================================== */
063100090227     C     Nuova_spediz  BEGSR
063200090227      **
063300090227      * Pulisco campi del file
063400090227     c                   z-add     vnrec         SAVNUM_RECord
063500090227     C                   CLEAR                   EDiVAB00
063600090227     C                   CLEAR                   EDiVAT00
063700090227     C                   CLEAR                   EDiVAD00
063800090227     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
063900090227     C                   MOVEL     'N'           WGID99            1
064000090227     C                   clear                   XY                5 0
064100090227     C                   clear                   §§                5 0
064200090227     C                   clear                   WS1496            5 0
064300090227     C                   clear                   WNSP             35
064400090227     C                   Z-ADD     *ZEROS        PVLB
064500090227     C                   MOVEA     *HIVAL        SGN
064600090227     C*  Azzero codice cliente - tariffa
064700090227     C                   Z-ADD     0             WTOCOC           16 0
064800090227     C                   Z-ADD     0             WTOPLC           16 2          ???
064900090227     C                   Z-ADD     0             WTOPNC           16 2          ???
065000090227     C*  Azzero codice cliente - tariffa
065100090227     C                   clear                   WCLKSC
065200090227     C                   clear                   WCLTAR
065300090227     C                   clear                   WCLLNP
065400090227     C                   clear                   WCLNRS
065500090227     C                   clear                   WCLCTM
065600090227     C                   clear                   WCLBS1
065700090227     C                   clear                   WCLfnsp                        *blk/S
065800090227     C                   clear                   WNOT1            35
065900090227     C                   clear                   WNOT2            35
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
067400090227      **
067500090227     c                   endSR
067600090227      * ?================================================================== */
067700090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
067800090227      * ?================================================================== */
067900090227     C     DATI_UNH      BEGSR
068000090227      *
068100090227      *  Prepara una nuova spedizione
068200090227      * ?                         --------------
068300090227     c                   exsr      Nuova_spediz
068400090227      * ?                         --------------
068500090211      *
068600090211      ** Riferimento del cliente
068700090211     C                   MOVEL     unh0062       WPRGSP           35
068800090211     C                   MOVEL     unh0062       WNSP             35
068900090211     C                   MOVEL     unh0062       WNUM             35
069000090211      * ?                                       --------
069100090211     C                   MOVEL     unh0062       VABRMA
069200090211      * ?                                       --------
069300090211     C     ' '           SCAN      unh0062       WLUNGH            3 0
069400090211     C                   SUB       1             WLUNGH
069500090211     C                   EXSR      TSTNUM
069600090211     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
069700090211      * ?                                       --------
069800090211     C                   MOVEL     WNUMOK        VABRMN
069900090211      * ?                                       --------
070000090211     C                   END
070100090210      **
070200090303     C                   MOVEL     VABlnp_PT     VABLNP
070300090303     C                   MOVEL     VABnrs_PT     VABNRS
070400090303     C                   MOVEL     VABctm_PT     VABCTM
070500090303      **
070600090303      **
070700090210     c     end_UNH       endSR
070800090210      * ?================================================================== */
070900090210     C*? Impsota i campi del VAB e del VAT da scrivere a rottura dettaglio
071000090210      * ?================================================================== */
071100090210     C     DATI_DTM      BEGSR
071200090210     C*
071300090209     C*  Se data documento  memorizzo numero e data x CMR
071400090209     C     dtm2005       IFEQ      '137'
071500090209     C                   MOVEL     dtm2380       WDATA            35            Data
071600090209     C                   MOVEL     dtm2379       WFORM             3            Formato
071700090209     C                   EXSR      CNVDAT
071800090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
071900090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
072000090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
072100090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
072200090209     C     WERRO         IFEQ      'S'
072300090210     C                   eval      vinMSG = 'DTM data errata'
072400090213     c                   exsr      Error_DET
072500090210     c                   goto      end_DTM
072600090209     C                   END
072700090209     C                   END
072800090209      **
072900090209     C*  Se data consegna richiesta imposto già sui campi del VAB
073000090209     C     dtm2005       IFEQ      '2'
073100090209     C                   MOVEL     dtm2380       WDATA            35            Data
073200090209     C                   MOVEL     dtm2379       WFORM             3            Formato
073300090209     C                   EXSR      CNVDAT
073400090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
073500090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
073600090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
073700090209     C     WERRO         IFEQ      'S'
073800090210     C                   eval      vinMSG = 'DTM data errata'
073900090213     c                   exsr      Error_DET
074000090210     c                   goto      end_DTM
074100090209     C                   END
074200090209     C                   END
074300090211      **
074400090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
074500090211     C     dtm2005       IFEQ      '11'
074600090211     C                   MOVEL     dtm2380       WDATA            35            Data
074700090211     C                   MOVEL     dtm2379       WFORM             3            Formato
074800090211     C                   EXSR      CNVDAT
074900090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
075000090211     C     WERRO         IFEQ      'S'
075100090211     C                   eval      vinMSG = 'DTM data errata'
075200090213     c                   exsr      Error_DET
075300090211     c                   goto      end_DTM
075400090211     C                   END
075500090211     C                   END
075600090209      **
075700090210     c     end_DTM       endSR
075800090209      * ?================================================================== */
075900090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
076000090209      * ?================================================================== */
076100090209     C     DATI_TOD      BEGSR
076200090209      *
076300090210      *  Trascodifico tipo trasporto
076400090210     c                   clear                   TOD_char3         3
076500090210      *
076600090210      *  imposta il default dalla tabella PT se c'è
076700090210     C     §puPFA        ifNE      *blank
076800090210     c                   SELECT
076900090210      *  Porto Franco
077000090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
077100090210     C                   movel     '4'           VABCBO                         + C/Ass
077200090210     c                   WHEN      §puPFA = 'F'
077300090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
077400090210      *  Porto Assegnato
077500090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
077600090210     C                   movel     '6'           VABCBO                         + C/Ass
077700090210     C                   WHEN      §puPFA = 'A'
077800090210     C                   movel     '2'           VABCBO                         Senza C/Ass
077900090210     C                   ENDSL
078000090210     c                   end
078100090210      *
078200090210      *   Imposta o il codice o il metodo
078300090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
078400090210     c                   if        tod4053  <> *blank
078500090210     c                   movel(p)  tod4053       TOD_char3
078600090210     c                   else
078700090210     c                   if        tod4215  <> *blank
078800090210     c                   movel(p)  tod4215       TOD_char3
078900090210     c                   end
079000090210     c                   end
079100110328      ****************
079200110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
079300110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
079400110328      ****************
079500110328     c                   eval      Trovata_Tab_TB ='N'
079600110328      *
079700110328     c                   if              TOD_char3 <> *blank
079800110328     c                   movel(p)  TOD_char3     Key_Generica35
079900110328     c                   move      §puTB         Key_Generica35
080000110328     C                   eval      Y = 1
080100110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
080200110328     c   32              eval      Trovata_Tab_TB ='S'
080300110328      *
080400110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
080500110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
080600110328      *
080700110328     c                   if          Trovata_Tab_TB ='N'
080800110328     c                                 and
080900110328     C                               tod4215 <> *blank
081000110328     c                   movel(p)  tod4215       TOD_char3
081100110328     c                   movel(p)  TOD_char3     Key_Generica35
081200110328     c                   move      §puTB         Key_Generica35
081300110328     C                   eval      Y = 1
081400110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
081500110328     c   32              eval      Trovata_Tab_TB ='S'
081600110328     c                   end
081700110328     c                   end
081800110328      *
081900110328      ****************
082000110328      ******* Se vi sono eccezioni sul cliente imposta quelle
082100110328      ****************
082200110328     c                   If          Trovata_Tab_TB ='S'
082300110328      *
082400110328     c                   SELECT
082500110328     c                   WHEN      Decod_Porto(Y) = 'F'
082600110328     c                               and  VABcas > *zeros
082700110328     C                   movel     '4'           VABCBO                         + C/Ass
082800110328     c                   WHEN      Decod_Porto(Y) = 'F'
082900110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
083000110328      *  Porto Assegnato
083100110328     c                   WHEN      VABCAS > *zeros
083200110328     C                   movel     '6'           VABCBO                         + C/Ass
083300110328     C                   OTHER
083400110328     C                   movel     '2'           VABCBO                         Senza C/Ass
083500110328     C                   ENDSL
083600110328      *
083700110328      ****************  altrimenti
083800110328      ******* Se non vi sono eccezioni per il cliente
083900110328      *   Rileva a questo punto lo Standard per tutti
084000110328      ****************
084100110328     c                   elseIf      Trovata_Tab_TB ='N'
084200090210      **
084300090210     c                   if              TOD_char3 <> *blank
084400090210     c                   movel(p)  TOD_char3     fld35a           35
084500090213     C                   movel     UNB_Mittente  etbUNB
084600090213     C                   movel(p)  'TOD'         etbSGM
084700090213     C                   movel(p)  TOD_char3     etbVALSGM
084800090213     c     K_TAB1L       chain     edstbl01l
084900090210      *
085000090210      * prova quindi senza l'UNB specifico del cliente
085100090213     c                   if        not %Found(edstbl01l)
085200090213     C                   clear                   etbUNB
085300090213     c     K_TAB1L       chain     edstbl01l
085400090210     c                   end
085500090210      *
085600090213     c                   if        %Found(edstbl01l)
085700090210     c                   SELECT
085800090213     c                   WHEN      ETBDATI = 'F'  and  VABcas > *zeros
085900090210     C                   movel     '4'           VABCBO                         + C/Ass
086000090213     c                   WHEN      ETBDATI = 'F'
086100090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
086200090210      *  Porto Assegnato
086300090210     c                   WHEN      VABCAS > *zeros
086400090210     C                   movel     '6'           VABCBO                         + C/Ass
086500090210     C                   OTHER
086600090210     C                   movel     '2'           VABCBO                         Senza C/Ass
086700090210     C                   ENDSL
086800090210      *
086900090210     c                   end
087000090210     c                   endIF
087100110328      **
087200110328     c                   ENDif
087300090209      **
087400090210      **  Il tipo bolla non è presente
087500090210     c                   if        VABCBO = *blank
087600090210     C                   eval      vinMSG = 'TOD non rilevato'
087700090213     c                   exsr      Error_DET
087800090210     c                   goto      end_TOD
087900090210     c                   end
088000090210      **
088100090210     c     end_TOD       endSR
088200090210      * ?================================================================== */
088300090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
088400090210      * ?================================================================== */
088500090210     C     DATI_RFF      BEGSR
088600090210      *
088700090210     C                   clear                   WAGE             35
088800110224     C                   clear                   WFF              35
088900110224     C                   clear                   WCU              35
089000091204      *
089100091204      *   Se manca sugli altri riporta sempre il principale per non bloccare il messaggio
089200091204     c                   if        RFF1154 = *blank and Rifer_Princip <> *blank
089300091204     c                   eval      RFF1154 = Rifer_Princip
089400091204     c                   end
089500090210      *
089600090210      *  Trascodifico riferimento spedizione
089700090210     c                   if        RFF1153 = 'BN' and RFF1154 <> *BLANKS
089800090210     C                   movel(p)  RFF1154       WAGE             35
089900091204     C                   movel(p)  RFF1154       Rifer_Princip    35
090000090210     c                   end
090100090210      **
090200090210     C                   IF         WAGE <> *BLANKS
090300090210     C                   movel     WAGE          WNSP             35
090400090210     C                   movel     WAGE          WNUM             35
090500090210     C     ' '           scan      WAGE          WLUNGH            3 0
090600090210     C                   sub       1             WLUNGH
090700090210     C                   EXSR      TSTNUM
090800090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
090900090210     C                   movel     WAGE          VABRMA
091000090210     C                   IF         WRMN = 'BN' or WRMN = *blanks
091100090210     C                   movel     WNUMOK        VABRMN
091200090210     C                   movel     'BN'          WRMN
091300090210     C                   ENDIF
091400090210     C                   ENDIF
091500090210     C                   ENDIF
091600090210      *
091700090210      * Codice Cliente di riferimento
091800090210     C                   if        RFF1153 = 'IT ' and RFF1154 <> *BLANKS
091900090210     C                   movel     RFF1154       WFF
092000090210     c                   END
092100090211      *
092200090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
092300090211     C     WFF           IFNE      *BLANKS
092400090211     C                   eval      XX = 1
092500090211     C                   movel     §PTKSC        WCCL             35
092600090211     C                   movel     WFF           WCOM28           28
092700090211     C                   move      WCOM28        WCCL
092800090211     C     WCCL          lookup    CCL(XX)                                34
092900090211     C     *IN34         IFEQ      '1'
093000090211     C                   movel     DCL(XX)       EDIDSCL
093100090211     C                   z-add     §CLKSC        WCLKSC            7 0
093200090211     C                   movel     §CLTAR        WCLTAR            3
093300090211     C                   z-add     §CLLNP        WCLLNP            3 0
093400090211     C                   z-add     §CLNRS        WCLNRS            2 0
093500090211     C                   movel     §CLCTM        WCLCTM            2
093600090211     C                   movel     §CLBS1        WCLBS1            1
093700090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
093800090211      * imposta la LNP   x il dettaglio specifco
093900090211     c                   eval      VABlnp = §CLlnp
094000090211      * imposta la Serie x il dettaglio specifco
094100090211     c                   eval      VABnrs = §CLnrs
094200090211      *
094300090211      *  In base al cod.trattamento merce reperisco tipo tratt.
094400090211      *    per un dettaglio specifico.
094500090211     C                   CLEAR                   DS1B
094600090211     C                   Z-ADD     1             Y                 3 0
094700090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
094800090211     C     *IN32         IFEQ      '1'
094900090211     C                   MOVEL     DTM(Y)        DS1B
095000090211     C                   END
095100090211      *
095200090211      * Se il cliente non era sulla schiera lo aggiunge
095300090211     C     §CLKSC        lookup    KCL                                    39
095400090211     C                   if        *in39 = *off
095500090211     C                   add       1             WW
095600090211     C                   z-add     §CLKSC        KCL(WW)
095700090211     c                   endif
095800090211      *
095900090211     C                   ENDIF
096000090211     C                   ENDIF
096100090211     C*----------------------------------------------------------------
096200090210      *
096300090210     C                   if        RFF1153 = 'CU ' and RFF1154 <> *blanks
096400090210     C                   movel     RFF1154       WCU
096500090210     C                   END
096600090210      *
096700090210      * Numero Ordine di vendita
096800090210     C     WCU           IFNE      *BLANKS
096900090210     C                   movel     WCU           WNUM             35
097000090210     C     ' '           scan      WCU           WLUNGH
097100090210     C                   sub       1             WLUNGH
097200090210     C                   EXSR      TSTNUM
097300090210     C                   If          WOKNUM = 'S'                               Ctrl numerico
097400090210     C                   movel     WNUMOK        VABRMN
097500090210     C                   movel     'CU '         WRMN
097600090210     C                   Endif
097700090210     C                   ENDIF
097800090210      *
097900090210      * NOn c'è l'identificativo bolla del cliente
098000090211     c                   if        (RFF1153 = 'BN' and RFF1154 = *BLANKS) or
098100090211     c                             (RFF1153 = 'CU' and RFF1154 = *BLANKS)
098200090210     C                   eval      vinMSG = 'RFF riferimento assente'
098300090213     c                   exsr      Error_DET
098400090210     c                   goto      end_RFF
098500090210     C                   ENDIF
098600090210      *
098700090210     c     end_RFF       endSR
098800090209      * ?================================================================== */
098900090213     C*?  C.O.D. Monetary Amount
099000090209      * ?================================================================== */
099100090213     C     DATI_MOA      BEGSR
099200090210      *
099300090216     C* Controllo se campo importo numerico
099400090216      * con 3 decimali
099500090216     C     moa5004       IFNE      *zeros
099600090216      *
099700090216     C     moa5025       IFEQ      '157'
099800090216     C     moa5004       div       1000          VABIAS                         IMPORTO
099900090216     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
100000090216     C                   END
100100090216      *
100200090216     C     moa5025       IFEQ      '77 '                                        Valore
100300090216     C     moa5004       div       1000          VABVMD                         MERCE
100400090216     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
100500090216     C                   END
100600090216      *
100700090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
100800090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
100900090216     C     moa5004       div       1000          VABCAS
101000090216     C                   MOVEL     moa6345       VABVCA
101100090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
101200090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
101300090216     C     §PUTPC        IFEQ      'P'
101400090216     C     moa5004       ANDGT     *ZEROS
101500090216     C                   MOVEL     'OM'          VABTIC
101600090216     C                   MOVEL     'OM'          WVABTIC
101700090216     C                   ENDIF
101800090216     C                   END
101900090216     C                   END
102000090216      *
102100090216     C                   END                                                    Imp.non numer.
102200090213      *
102300090213     c     end_MOA       endSR
102400090213      * ?================================================================== */
102500090213     C*?  FREE TEXT
102600090213      * ?================================================================== */
102700090213     C     DATI_FTX      BEGSR
102800090213      *
102900090216      * ? solo se si tratta di note SIN ICN o PAI non altro
103000090216     C                   DO        4             X
103100090216     C     D05(X)        IFNE      *BLANKS
103200090216     C     WNOT1         IFEQ      *BLANKS
103300090216     C                   MOVEL     D05(X)        WNOT1
103400090216     C                   MOVE      D05(X)        WNOT2
103500090216     C                   ELSE
103600090216     C     WNOT2         IFEQ      *BLANKS
103700090216     C                   MOVEL     D05(X)        WNOT2
103800090216     C                   END
103900090216     C                   END
104000090216     C                   END
104100090216     C                   END
104200090216     C*------>>
104300090216     C*  Decodifico tipo pagamento C/Assegno
104400090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
104500090216      *      (Problema di pulizia in presenza di + righe di testo)
104600090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
104700090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
104800090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
104900090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
105000090216     C                   if        wri_vabtic = *blank
105100090216     C                   MOVEL     *BLANKS       VABTIC
105200090216     c                   end
105300090216      **
105400090216     c                   clear                   trovato_TPI       1
105500090216      *
105600090216      * ?Se c'è il tipo Incasso
105700090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
105800090216      *
105900090216     C                   DO        4             X                              -- 02 --
106000090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
106100090216     C                   MOVEL     D05(X)        WCTC              2
106200090216     C                   Z-ADD     1             Y
106300090216     c                   movel(p)  Wctc          fld35a           35
106400090216     c                   move      §puTC         fld35a
106500090216     C     fld35a        LOOKUP    CTC35(Y)                               33
106600090216     C   33              MOVEL     TPI(Y)        VABTIC
106700090216     c   33              move      'S'           trovato_TPI
106800090216     C                   END                                                    -- 03 --
106900090216     C                   ENDdo                                                  -- 02 --
107000090216      *
107100090216     C                   ELSE                                                   -- 01 --
107200090216      *
107300090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
107400090216      *    Deve essere impostato comunque da testata
107500090216     c                   if        vabtic = *blank
107600090216     C                   MOVEL     wVabTIC       VABTIC
107700090216     c                   end
107800090216      *
107900090216     C                   END                                                    -- 01 --
108000090216      *
108100090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
108200090216      *    essere presenti altri record Flat con altre informazioni
108300090216      *    che potrebbero abblancare il Tipo incasso precedentemente
108400090216      *    decodificato
108500090216     c                   if        Trovato_TPI = 'S' and
108600090216     c                                 wri_vabtic = *blank
108700090216     c                   eval      wri_vabtic = 'S'
108800090216     c                   end
108900090213      *
109000090213     c     end_FTX       endSR
109100090213      * ?================================================================== */
109200090213     C*?  Anagrafica del mittente e del destinatario
109300090213      * ?================================================================== */
109400090213     C     DATI_NAD      BEGSR
109500090213      *
109600090210      * Dati destinatario
109700090210     C                   IF        nad3035 = 'CN' or
109800090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
109900090227      *
110000090227      * Se trovato un indirizzo di destinatario
110100090227     c                   eval       se_trovato_NAD ='S'
110200090210      *
110300090210     C* Reperisco nazione corrispondente destinatario
110400090210     C                   Z-ADD     1             YY                3 0
110500090210     c                   eval      *in32 = *off
110600090210     C                   if        nad3207 <> *BLANKS
110700090210     C     nad3207       LOOKUP    ISO(YY)                                32
110800090210     C                   endif
110900090210      *
111000090210     C* Imposto dati destinatario
111100090211     C                   MOVEL     nad31241      VABRSD
111200090211     C                   if        nad31242 <> *LOVAL
111300090211     C                   MOVEL     nad31242      VABRD2
111400090210     C                   endif
111500090211      *
111600090211      *
111700090210     C                   MOVEL     nad30421      VABIND
111800090210     C                   MOVEL     nad3164       VABLOD
111900090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
112000090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
112100090210      *
112200090210     C* Verifico se c'è dello spazio per nella località
112300090210     C     '    '        SCAN      VABLOD        WI                4 0
112400090210      *
112500090210     C                   MOVEA     VABLOD        LOD
112600090210     C                   ADD       1             WI
112700090210     C                   MOVEA     NAD30422      LOD(WI)
112800090210     C                   MOVEA     LOD           VABLOD
112900090210     C                   ENDif
113000090210      *
113100090210     C                   If        *in32 = *on
113200090210     C                   MOVEL     C15(YY)       VABNZD
113300090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
113400090210     c                   Else
113500090210     C                   MOVEL     *BLANKS       VABNZD
113600090210     C                   Z-ADD     0             WFLCPF
113700090210     c                   Endif
113800090210      * Converto CAP
113900090210     C                   EXSR      CNVCAP
114000090210     C                   ENDIF
114100090210      *
114200090210      * Dati mittente
114300090210     C                   IF        nad3035 = 'CZ'
114400090210     C                   movel     nad30361      VABRMO
114500090210      *
114600090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
114700090210     C                   movel     §PTNAZ        VABNMO
114800090210      *
114900090210     C                   IF        nad3207 <> *blanks
115000090210     C                   eval      YY = 1
115100090210     C     nad3207       LOOKUP    ISO(YY)                                32
115200090210     C                   if         *in32 = *on
115300090210     C                   MOVEL     C15(YY)       VABNMO
115400090210     C                   endif
115500090210     C                   ENDIF
115600090210      *
115700090210      * Normalizzo CAP mittente originale
115800090210     C                   clear                   T
115900090210     C                   clear                   S
116000090210     C                   MOVEA     *BLANKS       CAPM
116100090210     C                   MOVEA     nad3251       CAP9
116200090210     C                   DO        9             T
116300090210     C     CAP9(T)       IFNE      *BLANKS
116400090210     C                   ADD       1             S
116500090210     C                   MOVE      CAP9(T)       CAPM(S)
116600090210     C                   ENDIF
116700090210     C                   ENDDO
116800090210     C*
116900090210      *     Controllo validità CAP
117000090210     C                   MOVEA     CAPM          VABCMO
117100090210      *
117200090210     C                   CLEAR                   TISI95DS
117300090210     C                   MOVEL     VABCMO        I95CAP
117400090210     C                   MOVEL     VABNMO        I95NAR
117500090210     C                   MOVEL     WOGGI         I95DAT
117600090210     C                   MOVEL     '3'           I95TCN
117700090210     C                   CALL      'TISI95R'
117800090210     C                   PARM                    TISI95DS
117900090210      *     Errore
118000090210     C     O95ERR        IFNE      *BLANKS
118100090210     C                   MOVEL     INDCAE        VABCMO
118200090210     C                   MOVEL     §PTNAZ        VABNMO
118300090210     C                   END
118400090210      *
118500090210     C                   END
118600090210      *
118700090210      * NOn c'è l'identificativo bolla del cliente
118800090210     C                   if          vabRSD = *blank or
118900090210     C                               vabIND = *blank or
119000090210     C                               vabLOD = *blank
119100090211     C                   IF        nad3035 = 'CN'
119200090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
119300090213     c                   exsr      Error_DET
119400090210     c                   goto      end_NAD
119500090210     C                   ENDIF
119600090211     C                   ENDIF
119700090210      *
119800090210     c     end_NAD       endSR
119900090210      * ?================================================================== */
120000090210     C*?  Contatto a cui far riferimento
120100090210      * ?================================================================== */
120200090210     C     DATI_CTA      BEGSR
120300090210      *
120400090210      *  memorizza il riferimento di consegna x il destinatario
120500090210      *  memorizza il riferimento di consegna nr.telefonico
120600090210     C     cta3412       ifne      *BLANKS
120700090210     c                   movel     cta3412       watrde
120800090210     c                   end
120900090210      *
121000090210     c                   endSR
121100090210      * ?================================================================== */
121200090210     C*?  Contatto telefonico  a cui far riferimento
121300090210      * ?================================================================== */
121400090210     C     DATI_COM      BEGSR
121500090210      *
121600090210     C     com3148       ifne      *BLANKS
121700090210     c                   movel     com3148       wattel
121800090210     c                   end
121900090210      *
122000090209     C                   ENDSR
122100090210      * ?================================================================== */
122200090210     C*?  Dati di dettaglio della spedizione e dei colli
122300090210      * ?================================================================== */
122400090210     C     DATI_GID      BEGSR
122500090210      *
122600090210     C* Se ho totali per spedizione metto in campi VAB
122700090210     C     gid1496       IFEQ      99999
122800090210     C     gid1496       orEQ      9999
122900090210     C                   MOVEL     'S'           WGID99
123000090210     C                   z-add     gid722420     VABNCL
123100090210     C                   ELSE
123200090210     C     WGID99        IFEQ      'N'
123300090210     C                   z-add     gid722420     WTNCL             5 0
123400090210     C                   ADD       WTNCL         VABNCL
123500090210     C                   END
123600090210     C                   END
123700090212      *
123800090210     C                   ADD       VABNCL        WTOCOC           16 0
123900090210      *
124000090210     c     end_GID       endSR
124100090210      * ?================================================================== */
124200090210     C*?  Dati di dettaglio misure spedizione e colli
124300090210      * ?================================================================== */
124400090210     C     DATI_MEA      BEGSR
124500090210      *
124600090210     C*  Peso
124700090210     C     mea6311       IFEQ      'WT '
124800090210     C     mea6411       ANDEQ     'KGM'
124900090210      *
125000090210     C*  Controllo se ho valore totale o parziale
125100090210      *   sempre il Lordo
125200090210     C     mea6313       IFEQ      'AAD'
125300090210     C     WS1496        IFEQ      99999
125400090210     C     WS1496        orEQ      9999
125500090211     C                   move      mea6314       mea6314_2        18 2          Peso
125600090211     C                   Z-ADD     mea6314_2     VABPKB                         Peso
125700090210     C                   ELSE
125800090210     C     WGID99        IFEQ      'N'
125900090211     C                   move      mea6314       mea6314_2                      Peso
126000090211     C                   ADD       mea6314_2     VABPKB                         Peso
126100090210     C                   END
126200090210     C                   END
126300090210     C                   END
126400090210      *
126500090210     C     mea6313       IFEQ      'AAD'
126600090211     C                   move      mea6314       mea6314_2                      Peso
126700090211     C                   ADD       mea6314_2     WTOPLC
126800090210     C                   END
126900090210     C     mea6313       IFEQ      'AAC'
127000090211     C                   move      mea6314       mea6314_2                      Peso
127100090211     C                   ADD       mea6314_2     WTOPNC
127200090210     C                   END
127300090210     C                   END
127400090210      ***
127500090210     C*  Peso in Grammi  se abilitato a utilizzarlo
127600090210      ***
127700090210     C     §puGR         IFeq      'S'
127800090210     C     mea6311       IFEQ      'WT '
127900090210     C     mea6411       ANDEQ     '163'
128000090210      *
128100090210     C*  Controllo se ho valore totale o parziale
128200090210     C     mea6313       IFEQ      'AAD'
128300090210     C     WS1496        IFEQ      99999
128400090210     C     WS1496        orEQ      9999
128500090211     c                   move      mea6314       mea6314_2
128600090211     C     mea6314_2     div       1000          VABPKB                         Peso
128700090210     C                   ELSE
128800090210     C     WGID99        IFEQ      'N'
128900090211     c                   move      mea6314       mea6314_2
129000090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
129100090210     C                   add       Peso_inKG     VABPKB                         Peso
129200090210     C                   END
129300090210     C                   END
129400090210     C                   END
129500090210      *
129600090210     C     mea6313       IFEQ      'AAD'
129700090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
129800090210     C                   ADD       Peso_inKG     WTOPLC
129900090210     C                   END
130000090210     C     mea6313       IFEQ      'AAC'
130100090211     C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
130200090210     C                   ADD       Peso_inKG     WTOPNC
130300090210     C                   END
130400090210     C                   END
130500090210     C                   END
130600090210      *  Volume
130700090210     C     mea6311       IFEQ      'VOL'
130800090210     C     mea6411       ANDEQ     'MTQ'
130900090210      *  Controllo se ho valore totale o parziale
131000090210     C     WS1496        IFEQ      99999
131100090210     C     WS1496        orEQ      9999
131200091102     C                   move      mea6314       mea6314_2                      Volume
131300091102     C                   Z-ADD     mea6314_2     VABVLB                         Volume
131400091102     C                   Z-ADD     mea6314_2     PVLB
131500090210     C                   ELSE
131600090210     C     WGID99        IFEQ      'N'
131700091102     C                   move      mea6314       mea6314_2                      Volume
131800091102     C                   ADD       mea6314_2     VABVLB                         Volume
131900091102     C                   ADD       mea6314_2     PVLB
132000090210     C                   END
132100090210     C                   END
132200090210     C                   END
132300090210      *
132400090210     C* Ricerco CAP
132500090210     C     WCAP          IFNE      *BLANKS
132600090210     C     mea6311       IFEQ      'WT '
132700090210      *
132800090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
132900090210     C                   CLEAR                   FNLV55DS
133000090210     C                   MOVEL     'P'           D55TPT
133100090210     C                   MOVEL     WOGGI         D55DRF
133200090210     C                   MOVEL     VABLNP        D55LNP
133300090210     C                   MOVEL     VABLNP        D55LIN
133400090210     C                   CALL      'FNLV55R'
133500090210     C                   PARM                    FNLV55DS
133600090210     C                   MOVE      D55TFP        COMTFP            3 0
133700090210      *
133800090210     C*  PASSO IL TERMINAL PARTENZA
133900090210     C                   CLEAR                   TISI95DS
134000090210     C                   Z-ADD     COMTFP        I95TFP
134100090210     C                   MOVEL     VABTSP        I95TSP
134200090210     C                   MOVEL     'S'           I95FRE
134300090210     C                   MOVEL     '3'           I95TCN
134400090210     C                   MOVEL     WCAP          I95CAP
134500090210     C                   MOVEL     VABNZD        I95NAR
134600090210     C                   MOVEL     VABLOD        I95LOC
134700090210     C                   MOVEL     VABIND        I95IND
134800090210      *
134900090210     C* Se gestita seconda linea di arrivo passo peso - volume
135000090210     C* e numero colli effettivo
135100090210     C     §PULNA        IFEQ      'S'
135200090210     C                   Z-ADD     VABPKB        I95LKG
135300090210     C                   Z-ADD     VABVLB        I95LMC
135400090210     C                   ELSE
135500090210     C* Altrmenti passo 1 Kg e 0,001
135600090210     C***                  Z-ADD1         I95LKG
135700090210     C***                  Z-ADD0,001     I95LMC
135800090210     C                   END
135900090210     C*
136000090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
136100090210     C                   SELECT
136200090210     C     VABCBO        WHENEQ    '1 '
136300090210     C                   MOVEL     *BLANKS       I95FCA
136400090210     C                   MOVEL     'F'           I95TPO
136500090210     C     VABCBO        WHENEQ    '2 '
136600090210     C                   MOVEL     *BLANKS       I95FCA
136700090210     C                   MOVEL     'A'           I95TPO
136800090210     C     VABCBO        WHENEQ    '4 '
136900090210     C                   MOVEL     'S'           I95FCA
137000090210     C                   MOVEL     'F'           I95TPO
137100090210     C     VABCBO        WHENEQ    '6 '
137200090210     C                   MOVEL     'S'           I95FCA
137300090210     C                   MOVEL     'A'           I95TPO
137400090210     C                   OTHER
137500090210     C                   MOVEL     'F'           I95TPO
137600090210     C     VABCAS        IFGT      0
137700090210     C                   MOVEL     'S'           I95FCA
137800090210     C                   ELSE
137900090210     C                   MOVEL     *BLANKS       I95FCA
138000090210     C                   END
138100090210     C                   ENDSL
138200090210     C*
138300090210     C                   CALL      'TISI95R'
138400090210     C                   PARM                    TISI95DS
138500090210     C* Reperisco i dati da CAP
138600090210     C                   MOVEL     O95PRV        VABPRD
138700090210     C                   MOVEL     O95LNA        VABLNA
138800090210     C                   MOVEL     O95ZNC        VABZNC
138900090210     C                   MOVEL     WCAP          VABCAD
139000090210     C*
139100090210     C     O95ERR        IFNE      *BLANKS
139200090210     C     O95FIT        ANDEQ     'S'
139300090210     C*  Cap non trovato
139400090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
139500090213     c                   exsr      Error_DET
139600090210     C                   goto      end_MEA
139700090210     C                   END
139800090210     C                   END
139900090210      *
140000090210     C                   ELSE
140100090210      *
140200090210     C                   CLEAR                   VABPRD
140300090210     C                   CLEAR                   VABLNA
140400090210     C                   CLEAR                   VABZNC
140500090210     C                   CLEAR                   VABCAD
140600090210     C*  Cap non trovato
140700090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
140800090213     c                   exsr      Error_DET
140900090210     C                   goto      end_MEA
141000090210     C                   END
141100090210      *
141200090210     c     end_MEA       endSR
141300090210      * ?================================================================== */
141400090210     C*?  Dati di dettaglio Codice a barre Segnacolli
141500090210      * ?================================================================== */
141600090210     C     DATI_PCI      BEGSR
141700090210      *
141800090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
141900090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
142000090210     C     §1BF12        IFEQ      'E'
142100090210     C                   EXSR      SEGNEE
142200090210     C                   ELSE
142300090210      *
142400090210      *  La prima volta controllo congruità numero segnacollo
142500090210     C     §PUBS1        IFNE      *BLANKS
142600090210     C     VABnrs        ANDNE     0
142700090210      *  Se il codice trattamento merce prevede che segnacollino i
142800090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
142900090210     C     §1BFG7        IFEQ      'S'
143000090210     C     §1BFG1        OREQ      'S'
143100090210      *  calcolo segnacollo
143200090210     C                   EXSR      SGNELA
143300090210     C                   END
143400090210     C                   END
143500090210      *
143600090210     C                   END
143700090210      *
143800090210     c     end_PCI       endSR
143900090211     C*----------------------------------------------------------------
144000090211      *? dati finali del dettaglio    UNT
144100090211     C*----------------------------------------------------------------
144200090211     C     DATI_UNT      BEGSR
144300090211      *
144400090213     c                   eval      NUM_RECord = vnREC
144500090213     c                   z-add     vnrec         last_RECord
144600090211      **
144700090211     c                   endSR
144800090211     C*----------------------------------------------------------------
144900090211      *? dati finali     UNT
145000090211     C*----------------------------------------------------------------
145100090211     C     DATI_UNZ      BEGSR
145200090211      *
145300100719     C                   eval      Id_Messaggio   = UNZ0020
145400090211      **
145500090211     c                   endSR
145600090210      *----------------------------------------------------------------
145700090210      *? Input segnacolli su schiera di 20 elementi
145800090210      *----------------------------------------------------------------
145900090210     C     INP_Segnacol  BEGSR
146000090210      *
146100090210     c                   clear                   quanti_PCI        3 0
146200090210     c                   if        §puEL1 = 0
146300090210     c                   z-add     10            quanti_PCI
146400090210     c                   else
146500090210     c                   z-add     §puEL1        quanti_PCI
146600090210     c                   end
146700090210      *
146800090210      *  Pulisce la schiera dei segnacolli da elaborare
146900090210     c                   clear                   x30
147000090210     c                   z-add     0             Nr_x30            3 0
147100090210      *
147200090210      *   riporto tutto sulla schiera generica X30
147300090210      *
147400090210     c                   do        10            limite            3 0
147500090210      * Limita quanti elementi passati in ogni PCI
147600090210      *   devono essere considerati
147700090210     c                   if        limite > quanti_PCI
147800090210     c                   Leave
147900090210     c                   end
148000090210      *
148100090210      *  Carica la schiera generale di tutti i segnacolli
148200090210     c                   if        D30_1(limite) <> *blank
148300090210     c                   add       1             Nr_x30
148400090210     c                   eval      X30(Nr_x30) = D30_1(limite)
148500090210     c                   end
148600090210     c                   enddo
148700090210      *
148800090210     C                   ENDSR
148900090210      *----------------------------------------------------------------
149000090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
149100090210      *----------------------------------------------------------------
149200090210     C     SEGNEE        BEGSR
149300090210      *
149400090210     c                   exsr      INP_Segnacol
149500090210      *
149600090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
149700090210     C                   DO        10            x                              --- 01 ---
149800090210      *
149900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
150000090210     C     x30(X)        ANDNE     *LOVAL
150100090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
150200090210     C                   ADD       1             §§
150300090210     C                   MOVEL     x30(X)        SGE(§§)
150400090210     C                   END                                                    --- 03 ---
150500090210      *
150600090210     C                   END                                                    --- 02 ---
150700090210      *
150800090210     C                   ENDSR
150900090210     C*----------------------------------------------------------------
151000090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
151100090210     C*----------------------------------------------------------------
151200090210     C     SGNELA        BEGSR
151300090210      *
151400090210     c                   exsr      INP_Segnacol
151500090210     C*
151600090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
151700090210     C*  in base al numero elementi validi carico sgn.
151800090210     C                   DO        10            X                              --- 01 ---
151900090210     C*
152000090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
152100090210     C     x30(X)        ANDNE     *LOVAL
152200090210     C*
152300090210     C                   CLEAR                   DSSGN
152400090210     C*  Controllo se devo ricevere la linea di partenza
152500090210     C                   MOVEL     'N'           WERRO             1
152600090210     C     §PULP1        IFGT      0                                            --- 03 ---
152700090210     C                   EXSR      REPLNP
152800090210     C                   END                                                    --- 03 ---
152900090210     C*  Controllo se devo ricevere la linea di arrivo
153000090210     C     §PULA1        IFGT      0                                            --- 03 ---
153100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
153200090210     C                   EXSR      REPLNA
153300090210     C                   END                                                    --- 03 ---
153400090210     C*  Controllo se devo ricevere il numero di serie
153500090210     C     §PUNR1        IFGT      0                                            --- 03 ---
153600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
153700090210     C                   EXSR      REPNRS
153800090210     C                   END                                                    --- 03 ---
153900090210     C*  Controllo se devo ricevere il numero segnacollo
154000090210     C     §PUSG1        IFGT      0                                            --- 03 ---
154100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
154200090210     C                   EXSR      REPSGN
154300090210     C                   END                                                    --- 03 ---
154400090210     C*  Controllo se devo ricevere la zona di consegna
154500090210     C     §PUZN1        IFGT      0                                            --- 03 ---
154600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
154700090210     C                   EXSR      REPZNC
154800090210     C                   END                                                    --- 03 ---
154900090210     C*  Imposto da VAB i dati a zero se mi è stato passato
155000090210     C*  numero segnacollo valido
155100090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
155200090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
155300090210     C                   Z-ADD     VABLNP        SGNLNP
155400090210     C                   END                                                    --- 04 ---
155500090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
155600090210     C                   Z-ADD     VABLNA        SGNLNA
155700090210     C                   END                                                    --- 04 ---
155800090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
155900090210     C                   Z-ADD     VABNRS        SGNNRS
156000090210     C                   END                                                    --- 04 ---
156100090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
156200090210     C                   Z-ADD     VABZNC        SGNZNC
156300090210     C                   END                                                    --- 04 ---
156400090210      *
156500090210     C*  mi reimposto LNA e zona da segnacollo
156600090210     C                   Z-ADD     SGNLNA        VABLNA
156700090210     C                   Z-ADD     SGNZNC        VABZNC
156800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
156900090210     C                   ADD       1             XY
157000090210     C                   MOVE      SGNNSC        SGN(XY)
157100090210     C                   END                                                    --- 03 ---
157200090210     C*
157300090210     C                   END                                                    --- 02 ---
157400090210     C*
157500090210     C                   END                                                    --- 01 ---
157600090210      *
157700090210     C                   ENDSR
157800090210     C*----------------------------------------------------------------
157900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
158000090210     C*----------------------------------------------------------------
158100090210     C     REPLNP        BEGSR
158200090210     C*
158300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
158400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
158500090210     C*  dal numero segnacollo passatomi dal partner
158600090210     C                   Z-ADD     §PULP1        WP                2 0
158700090210     C     3             SUBST     x30(X):WP     WLNP              3
158800090210     C*
158900090210     C*  controllo che la lnp calcolata sia numerica
159000090210     C                   TESTN                   WLNP                 98
159100090210     C*
159200090210     C*  se è numerica la carico
159300090210     C     *IN98         IFEQ      '1'
159400090210     C                   MOVE      WLNP          SGNLNP
159500090210     C                   ELSE
159600090210     C*  se non è numerica stampo errore
159700090210     C                   MOVEL     'S'           WERRO             1
159800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
159900090213     c                   exsr      Error_DET
160000090210     C                   goto      end_LNP
160100090210     C                   END
160200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
160300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
160400090210     C     SGNLNP        IFNE      VABLNP
160500090210     C                   MOVEL     'S'           WERRO             1
160600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
160700090210     c                             a della Bolla'
160800090213     c                   exsr      Error_DET
160900090210     C                   goto      end_LNP
161000090210     C                   END
161100090210     C*
161200090210     C     end_LNP       ENDSR
161300090210     C*----------------------------------------------------------------
161400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
161500090210     C*----------------------------------------------------------------
161600090210     C     REPLNA        BEGSR
161700090210     C*
161800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
161900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
162000090210     C*  dal numero segnacollo passatomi dal partner
162100090210     C                   Z-ADD     §PULA1        WP                2 0
162200090210     C     3             SUBST     x30(X):WP     WLNA              3
162300090210     C*  controllo che la lna calcolata sia numerica
162400090210     C                   TESTN                   WLNA                 98
162500090210     C*  se è numerica la carico
162600090210     C     *IN98         IFEQ      '1'
162700090210     C                   MOVE      WLNA          SGNLNA
162800090210     C                   ELSE
162900090210     C*  se non è numerica stampo errore
163000090210     C                   MOVEL     'S'           WERRO             1
163100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
163200090213     c                   exsr      Error_DET
163300090210     C                   goto      end_LNA
163400090210     C                   END
163500090210     C*
163600090210     C     end_LNA       ENDSR
163700090210     C*----------------------------------------------------------------
163800090210     C*? REPNRS - Reperisce numero serie
163900090210     C*----------------------------------------------------------------
164000090210     C     REPNRS        BEGSR
164100090210     C*
164200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
164300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
164400090210     C*  dal numero segnacollo passatomi dal partner
164500090210     C                   Z-ADD     §PUNR1        WP                2 0
164600090210     C     2             SUBST     x30(X):WP     WNRS              2
164700090210     C*  controllo che il nrs calcolata sia numerico
164800090210     C                   TESTN                   WNRS                 98
164900090210     C*  se è numerica la carico
165000090210     C     *IN98         IFEQ      '1'
165100090210     C                   MOVE      WNRS          SGNNRS
165200090210     C                   ELSE
165300090210     C*  se non è numerico stampo errore
165400090210     C                   MOVEL     'S'           WERRO             1
165500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
165600090213     c                   exsr      Error_DET
165700090210     C                   goto      end_NRS
165800090210     C                   END
165900090210      *
166000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
166100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
166200090210     C     SGNNRS        IFNE      VABNRS
166300090210     C                   MOVEL     'S'           WERRO             1
166400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
166500090210     c                             o della Bolla'
166600090213     c                   exsr      Error_DET
166700090210     C                   goto      end_NRS
166800090210     C                   END
166900090210     C*
167000090210     C     end_NRS       ENDSR
167100090210     C*----------------------------------------------------------------
167200090210     C*? REPZNC - Reperisce zona consegna
167300090210     C*----------------------------------------------------------------
167400090210     C     REPZNC        BEGSR
167500090210     C*
167600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
167700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
167800090210     C*  dal numero segnacollo passatomi dal partner
167900090210     C                   Z-ADD     §PUZN1        WP                2 0
168000090210     C     2             SUBST     x30(X):WP     WZNC              2
168100090210     C*  controllo che la zona calcolata sia numerica
168200090210     C                   TESTN                   WZNC                 98
168300090210     C*  se è numerica la carico
168400090210     C     *IN98         IFEQ      '1'
168500090210     C                   MOVE      WZNC          SGNZNC
168600090210     C                   ELSE
168700090210     C*  se non è numerica stampo errore
168800090210     C                   MOVEL     'S'           WERRO             1
168900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
169000090213     c                   exsr      Error_DET
169100090210     C                   goto      end_ZNC
169200090210     C                   END
169300090210     C*
169400090210     C     end_ZNC       ENDSR
169500090210     C*----------------------------------------------------------------
169600090210     C*? REPSGN - Reperisce numero segnacollo
169700090210     C*----------------------------------------------------------------
169800090210     C     REPSGN        BEGSR
169900090210     C*
170000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
170100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
170200090210     C*  indicata in tabella)
170300090210     C*  dal numero segnacollo passatomi dal partner
170400090210     C                   Z-ADD     §PUSG1        WP                2 0
170500090210     C     7             SUBST     x30(X):WP     WSGN              7
170600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
170700090210     C                   TESTN                   WSGN                 98
170800090210     C*  se è numerico lo carico
170900090210     C     *IN98         IFEQ      '1'
171000090210     C                   MOVE      WSGN          SGNNSC
171100090210     C                   ELSE
171200090210     C*  se non è numerica stampo errore
171300090210     C                   MOVEL     'S'           WERRO             1
171400090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
171500090213     c                   exsr      Error_DET
171600090210     C                   goto      end_SGN
171700090210     C                   END
171800090210     C*
171900090210     C     end_SGN       ENDSR
172000971215      *----------------------------------------------------------------
172100971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
172200971215      *----------------------------------------------------------------
172300051205      *----------------------------------------------------*
172400051205      *   DEFINIZIONE CHIAVI                               *
172500051205      *----------------------------------------------------*
172600051205     C     *INZSR        BEGSR
172700051205      *
172800991129     c     *ENTRY        PLIST
172900060613     C                   parm                    esito             1
173000971215      *
173100050112     C     KTABel        KLIST
173200050112     C                   KFLD                    TBLKUT
173300050112     C                   KFLD                    TBLCOD
173400050112     C                   KFLD                    TBLKEY
173500090210      *
173600090210     C     K_TAB1L       KLIST
173700090213     C                   KFLD                    etbUNB
173800090213     C                   KFLD                    etbSGM
173900090213     C                   KFLD                    etbVALSGM
174000090209     C*
174100090209     C* Definisco chiavi di accesso
174200090209     C     KTAB1         KLIST
174300090209     C                   KFLD                    KKUT
174400090209     C                   KFLD                    KCOD
174500090209     C*
174600090209     C     KNUF          KLIST
174700090209     C                   KFLD                    KAAA
174800090209     C                   KFLD                    KCNU
174900090209     C                   KFLD                    KFIL
175000090209      *
175100090209     C     KVAB1         KLIST
175200090209     C                   KFLD                    KCCM
175300090209     C                   KFLD                    KCMR
175400090209     C                   KFLD                    KCNT
175500090209     C                   KFLD                    KAAS
175600090209     C                   KFLD                    KLNP
175700090209     C                   KFLD                    KNRS
175800090209     C                   KFLD                    KNSP
175900090209      *
176000090209     C     KVAB2         KLIST
176100090209     C                   KFLD                    KCCM
176200090209     C                   KFLD                    KCMR
176300090209      *
176400090209     C     KRDE          KLIST
176500090209     C                   KFLD                    KAAS
176600090209     C                   KFLD                    KLNP1
176700090209     C                   KFLD                    KNRS
176800090209     C                   KFLD                    KNSP
176900090209     C                   KFLD                    KNMC
177000090209     C                   KFLD                    KNRP
177100090209      *
177200090209     C     KACO          KLIST
177300090209     C                   KFLD                    KKUT
177400090209     C                   KFLD                    KKCC
177500090209     C                   KFLD                    KKSC
177600090209      *
177700090209     C     KIND          KLIST
177800090209     C                   KFLD                    KKUT
177900090209     C                   KFLD                    KKCC
178000090209     C                   KFLD                    KKSC
178100090209      *
178200090209      * DETERMINO SE SONO BARTOLINI O SDI
178300090209     C                   CLEAR                   TIBS55DS
178400090209     C                   MOVEL     'L'           I50TLA
178500090209     C                   CALL      'TIBS55R'
178600090209     C                   PARM                    TIBS55DS
178700090209      *
178800090209     C* RECUPERO DATI DELL'UTENTE
178900090209     C                   Z-ADD     1             CODUT             1 0
179000090209     C                   CALL      'XPARUT'
179100090209     C                   PARM                    UTEDSE0F
179200090209     C                   MOVEL     RAGUT         RSUT             20
179300090209     C*
179400090209     C* RICERCA CAPOCONTI
179500090209     C                   DO        50            X
179600090209     C                   MOVE      TCU(X)        TCUDS
179700090209     C     F56           IFEQ      'CG'
179800090209     C     F34           ANDEQ     '01'
179900090209     C                   Z-ADD     KCU(X)        KCI               4 0
180000090209     C                   END
180100090209     C                   END
180200090209     C*
180300090211     c                   movel(p)  'EDPAB'       User_Msg         10
180400090211     C*
180500090209     C* IMPOSTO A ZERO VARIABILI DI PGM
180600090209     c                   move      *blank        Snd_Msg_alert     1
180700090209     C                   MOVEL     CA(1)         WCA              78
180800090209     C                   MOVEL     CN(1)         WCN              78
180900090209     C*
181000090209     C* RECUPERO DATA E ORA
181100090209     C                   TIME                    WHHDAT           14 0
181200090209     C                   MOVE      WHHDAT        G02DAT
181300090209     C                   MOVE      WHHDAT        WDATA8            8 0
181400090209     C                   MOVE      WDATA8        WAA2              2 0
181500090209     C                   MOVEL     WDATA8        DATA              6 0
181600090209     C                   MOVE      WAA2          DATA
181700090209     C                   MOVEL     *BLANK        G02ERR
181800090209     C                   CALL      'XSRDA8'
181900090209     C                   PARM                    WLBDA8
182000090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
182100090209     C                   TIME                    WORA              6 0
182200090209      * Recupero data e ora
182300090209     C                   TIME                    W0140
182400090209      * UDATE IN GGMMAAAA
182500090209     C                   MOVE      W0140         WDTGIO
182600090209      * UDATE IN AAAAMMGG
182700090209     C     *eur          MOVEL     WDTGIO        DATA_eur
182800090209     C     *iso          MOVEL     DATA_eur      dateu
182900090209      *
183000090209      * ?              /*--------------------------- */
183100090209     c                   exsr      CARICA_TABELLE
183200090209      * ?              /*--------------------------- */
183300090209      *
183400090209      *
183500090209     C                   MOVEL     *ALL'-'       CMP132          132
183600090209     C                   CLEAR                   EDiVAB00
183700090211     C                   clear                   vablnp_PT
183800090212     C                   clear                   vabnrs_PT
183900090211     C                   clear                   vabctm_PT
184000090211     C                   clear                   VABfnsp_PT
184100090209     C                   MOVEA     *HIVAL        SGN
184200991124      *
184300050414      *------------------
184400991124     C                   ENDSR
184500060621      * ?------------------------------------------------------------------ */
184600090209      *?    CARICA TABELLE SU SCHIERE
184700060621      * ?------------------------------------------------------------------ */
184800090209     C     CARICA_TABELLEBEGSR
184900090205      *
185000090209     C* Caricamento Tabella 1B
185100090209     C                   Z-ADD     0             X                 4 0
185200090209     C                   Z-ADD     CODUT         KKUT
185300090209     C                   MOVEL     '1B'          KCOD
185400090209     C     KTAB1         CHAIN     TABEL00F                           30
185500090209     C     *IN30         DOWEQ     '0'
185600090209     C     TBLFLG        IFEQ      *BLANKS
185700090209     C                   ADD       1             X
185800090209     C                   MOVEL     TBLKEY        CTM(X)
185900090209     C                   MOVEL     TBLUNI        DTM(X)
186000090209     C                   END
186100090209     C     KTAB1         READE     TABEL00F                               30
186200090209     C                   END
186300090209      *
186400090209     C* Caricamento Tabella 15
186500090209     C                   Z-ADD     0             X                 4 0
186600090209     C                   Z-ADD     CODUT         KKUT
186700090209     C                   MOVEL     '15'          KCOD
186800090209     C     KTAB1         CHAIN     TABEL00F                           30
186900090209     C     *IN30         DOWEQ     '0'
187000090209     C     TBLFLG        IFEQ      *BLANKS
187100090209     C                   ADD       1             X
187200090209     C                   MOVEL     TBLKEY        C15(X)
187300090209     C                   MOVEL     TBLUNI        DS15
187400090209     C                   MOVEL     §15COD        ISO(X)
187500090209     C                   MOVEL     §15ECF        CPF(X)
187600090209     C                   MOVE      §15PCF        CPF(X)
187700090209     C                   END
187800090209     C     KTAB1         READE     TABEL00F                               30
187900090209     C                   END
188000090209      *
188100090209     C* Caricamento Tabella CV
188200090209     C                   Z-ADD     0             X
188300090209     C                   Z-ADD     CODUT         KKUT
188400090209     C                   MOVEL     'CV'          KCOD
188500090209     C     KTAB1         CHAIN     TABEL00F                           30
188600090209     C     *IN30         DOWEQ     '0'
188700090209     C     TBLFLG        IFEQ      *BLANKS
188800090209     C                   ADD       1             X
188900090209     C                   MOVEL     TBLKEY        CCV(X)
189000090209     C                   MOVEL     TBLUNI        DCV(X)
189100090209     C                   END
189200090209     C     KTAB1         READE     TABEL00F                               30
189300090209     C                   END
189400090216      *
189500110328      * Caricamento Tabella termini consegna
189600110328     C                   Z-ADD     0             X
189700110328     C                   MOVEL     'TB'          TABCOD
189800110328     C     TABCOD        CHAIN     EDTAB01L                           30
189900110328     C     *IN30         DOWEQ     '0'
190000110328     C     TABFLG        IFEQ      *BLANKS
190100110328     C                   ADD       1             X
190200110328     C                   eval      K35_TpBolla(X) = TABKEY
190300110328     C                   eval      Cod_TpBolla(X) = TABKEY
190400110328     C                   eval      Decod_Porto(X) = TABUNI
190500110328     C                   END
190600110328     C     TABCOD        READE     EDTAB01L                               30
190700110328     C                   END
190800110328      *
190900090216     C* Caricamento Tabella Tipo Incasso C/Assegno
191000090216     C                   Z-ADD     0             X
191100090216     C                   MOVEL     'TC'          TABCOD
191200090216     C     TABCOD        CHAIN     EDTAB01L                           30
191300090216     C     *IN30         DOWEQ     '0'
191400090216     C     TABFLG        IFEQ      *BLANKS
191500090216     C                   ADD       1             X
191600090216     C                   MOVEL     TABKEY        CTC35(X)
191700090216     C                   MOVEL     TABKEY        CTC(X)
191800090216     C                   MOVEL     TABUNI        TPI(X)
191900090216     C                   END
192000090216     C     TABCOD        READE     EDTAB01L                               30
192100090216     C                   END
192200090209      *
192300090209      * Caricamento Tabella Partner esteri
192400090209     C                   Z-ADD     0             X
192500090209     C                   MOVEL     'PT'          TABCOD
192600090209     C     TABCOD        CHAIN     EDTAB01L                           30
192700090209     C     *IN30         DOWEQ     '0'
192800090209      *
192900090209     C                   SELECT
193000090209     C     TABFLG        WHENNE    *BLANKS
193100090209      *
193200090209     C                   OTHER
193300090209     C                   ADD       1             X
193400090209     C                   MOVEL     TABKEY        CPT(X)
193500090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
193600090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
193700090209     C                   MOVEL     TABUNI        DPT(X)
193800090209     C                   ENDSL
193900090209      *
194000090209     C     TABCOD        READE     EDTAB01L                               30
194100090209     C                   END
194200121102      *
194300121102      * Controlla riempimento della PT x limiti di schiera
194400121102      *  se deve inviare la mail di alert x limite quasi raggiunto.
194500121102     c                   exsr      ChecK_PT
194600121102     C*
194700090209      * salva quanti Codici UNB ha caricato
194800090209     c                   z-add     x             sav_CPTx
194900090209      *
195000090209     C                   MOVEL     'PU'          TABCOD
195100090209     C     TABCOD        CHAIN     EDTAB01L                           30
195200090209     C     *IN30         DOWEQ     '0'
195300090209      *
195400090209     C                   SELECT
195500090209     C     TABFLG        WHENNE    *BLANKS
195600090209      *
195700090209     C                   OTHER
195800090209     C                   MOVEL     TABKEY        CODPU            35
195900090209     C                   Z-ADD     1             X
196000090209     C     CODPU         LOOKUP    CPT(X)                                 32
196100090209     C   32              MOVEL     TABUNI        DPU(X)
196200090209     C                   ENDSL
196300090209      *
196400090209     C     TABCOD        READE     EDTAB01L                               30
196500090209     C                   END
196600090209      *
196700090209      * Prosegue tab.PT e PU
196800090209     C                   MOVEL     'PV'          TABCOD
196900090209     C     TABCOD        CHAIN     EDTAB01L                           30
197000090209     C     *IN30         DOWEQ     '0'
197100090209      *
197200090209     C                   SELECT
197300090209     C     TABFLG        WHENNE    *BLANKS
197400090209      *
197500090209     C                   OTHER
197600090209     C                   MOVEL     TABKEY        CODPV            35
197700090209     C                   Z-ADD     1             X
197800090209     C     CODPV         LOOKUP    CPT(X)                                 32
197900090209     C   32              MOVEL     TABUNI        DPV(X)
198000090209     C                   ENDSL
198100090209      *
198200090209     C     TABCOD        READE     EDTAB01L                               30
198300090209     C                   END
198400090209      *
198500090209     C* Caricamento Tabella codici clienti - tariffe
198600090209     C                   Z-ADD     0             X
198700090209     C                   MOVEL     'CL'          TABCOD
198800090209     C     TABCOD        CHAIN     EDTAB01L                           30
198900090209     C     *IN30         DOWEQ     '0'
199000090209     C     TABFLG        IFEQ      *BLANKS
199100090209     C                   ADD       1             X
199200090209     C                   MOVEL     TABKEY        CCL(X)
199300090209     C                   MOVEL     TABUNI        DCL(X)
199400090209     C                   END
199500090209     C     TABCOD        READE     EDTAB01L                               30
199600090209     C                   END
199700090209      *
199800090209     C* Caricamento Serivizi speciali
199900090209     C                   Z-ADD     0             X
200000090209     C                   MOVEL     'SS'          TABCOD
200100090209     C     TABCOD        CHAIN     EDTAB01L                           30
200200090209     C     *IN30         DOWEQ     '0'
200300090209     C     TABFLG        IFEQ      *BLANKS
200400090209     C                   ADD       1             X
200500090209     C                   MOVEL     TABKEY        SS(X)
200600090209     C                   MOVEL     TABKEY        SS35(X)
200700090209     C                   MOVEL     TABUNI        DSS(X)
200800090209     C                   END
200900090209     C     TABCOD        READE     EDTAB01L                               30
201000090209     C                   END
201100090209      *
201200090209     C                   ENDSR
201300090209      * ?------------------------------------------------------------------ */
201400090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
201500090209      * ?------------------------------------------------------------------ */
201600090209     C     MSG_Errato    BEGSR
201700090209      *
201800090205      * ?  Scrive su tutti i records il tipo di errore
201900090205     c     *start        setll     tivin00r
202000090205     c                   read      tivin00r
202100090205     c                   dow       not %eof(tivin00r)
202200090211     c                   if        vinMSG  = *blank
202300090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
202400090210     C                               >> Controllare i DATI su UPLOAD !!'
202500090211     c                   end
202600090205     C                   eval      vinFLG = '2'
202700090213     c                   eval      Almeno_Un_Due ='S'
202800090205     c                   eval      esito  = '2'
202900090209     c                   eval      se_errore ='S'
203000090213     c                   eval      err_bloccante ='S'
203100090205     c                   update    tivin000
203200090205     c                   read      tivin00r
203300090205     c                   endDO
203400090205      *
203500090205     C                   ENDSR
203600090213      * ?------------------------------------------------------------------ */
203700090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
203800090213      * ?------------------------------------------------------------------ */
203900090213     C     DETta_Errato  BEGSR
204000090213      *
204100090213      *  rilascia il record prima di rieseguire il ciclo
204200090213     c                   update    tivin000
204300090213      *
204400090213      * ?  Scrive su tutti i records il tipo di errore
204500090213     c     SavNUM_RECord setll     tivin00r
204600090213     c                   read      tivin00r
204700090213     c                   dow       not %eof(tivin00r)
204800090213      *
204900090213      *
205000090213     c                   if        vinMSG  = *blank
205100090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
205200090213     C                             >> Controllare i DATI di ogni Segmento <<'
205300090213     c                   end
205400090213     c                   exsr      Error_DET
205500090213      *
205600090213     c                   if        VNREC = NUM_RECord
205700090213     c                   leave
205800090213     c                   end
205900090213      *
206000090213     c                   update    tivin000
206100090213     c                   read      tivin00r
206200090213      *
206300090213     c                   endDO
206400090601      *
206500090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
206600090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
206700090601     C                             Bolle NILFISK non totalmente tradotto. -
206800090601     C                             Controllare UPLOAD del cliente 2494114.'
206900090601     c                   exsr      invio_mail_AB
207000090213      *
207100090213     C                   ENDSR
207200090205      * ?------------------------------------------------------------------ */
207300090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
207400090205      * ?------------------------------------------------------------------ */
207500090205     C     *pssr         BEGSR
207600090205     C
207700060710     C                   eval      esito = '2'
207800090601     C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import NILFISK'
207900090601     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
208000090601     C                             Bolle import in filiale - messaggio -
208100090601     C                             con UNB non presente in tabella PT controlla-
208200090601     C                             re EDI ricevuto su UPLOAD.'
208300090601     c                   exsr      invio_mail_AB
208400060621     C
208500090213     C                   ENDSR
208600090213      * ?------------------------------------------------------------------ */
208700090213      *?      Segnala Errore su TIVIN
208800090213      * ?------------------------------------------------------------------ */
208900090213     C     Error_DET     BEGSR
209000090213     C
209100090213     C                   eval      vinFLG = '2'
209200090213     c                   eval      Almeno_Un_Due ='S'
209300090213     c                   eval      esito  = '1'
209400090213     c                   eval      se_errore ='S'
209500090213     C                   eval      Err_in_detta = 'S'
209600090213     C
209700090213     C                   ENDSR
209800090209      * ?------------------------------------------------------------------ */
209900090209      *? Controlla la trasmissione   se completa
210000090209      * ?------------------------------------------------------------------ */
210100090209     c     Check_Trasm   Begsr
210200090209
210300090209     C                   clear                   se_errore
210400090209     C                   clear                   Riga_iNIZIo       1
210500090209     C                   clear                   Riga_fINe         1
210600090209      ** primo  record
210700090209     c     *start        setll     tivin00r
210800090209      *
210900090209     c     rileggi       tag
211000090209     c                   read      tivin00r
211100090209      *
211200090209     c                   if        not %eof(tivin00r)
211300090209      *
211400090209     c                   movel(p)  VINDTA        dati
211500090209     c                   if        dati = *blank
211600090209      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
211700090209      * le leggo e le escludo.
211800090209     C                   eval      vinFLG = '1'
211900090209     c                   update    Tivin000
212000090209     c                   goto      rileggi
212100090209     c                   end
212200090209      * ?              /*---------------------- */
212300090210     c                   exsr      Decod_Segmento
212400090209      * ?              /*---------------------- */
212500090209     c                   endif
212600090209      **
212700090209      ** ultimo record
212800090209     c     *hival        setll     tivin00r
212900090209     c     leggiAncora   tag
213000090209     c                   readp     tivin00r
213100090209     c                   if        not %eof(tivin00r)
213200090209      *
213300090209     c                   movel(p)  VINDTA        dati
213400090209     c                   if        dati = *blank
213500090209      * le ultime righe potrebbero essere vuote prima di finire il messaggio
213600090209      * le leggo e le escludo.
213700090209     C                   eval      vinFLG = '1'
213800090209     c                   update    Tivin000
213900090209     c                   goto      leggiAncora
214000090209     c                   end
214100090209      * ?              /*---------------------- */
214200090210     c                   exsr      Decod_Segmento
214300090209      * ?              /*---------------------- */
214400090209     c                   endif
214500090209
214600090209      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
214700090209      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
214800090209      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
214900090209     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
215000090209      * ?-----> Errore
215100090209     C                   eval      se_errore = 'S'
215200090209     c                   end
215300090209
215400090209     c                   endSR
215500090209      * ?------------------------------------------------------------------ */
215600090209      *?    Decodifica il segmento ed importa i campi in formato DS
215700090209      * ?------------------------------------------------------------------ */
215800090209     c     Decod_SegmentoBegsr
215900090209      **
216000090209     c                   eval      tipo_seg = %subst(vinDTA:1:3)
216100090209     c                   eval      segmento = vinDTA
216200100719      *
216300100719     c                   clear                   seg_imprevisto
216400100719     c                   clear                   trovato_seg
216500100716      *
216600100716     C                   clear                   keyUNBCLI
216700100716     C                   clear                   keyTIPOMSG
216800100716     C                   clear                   keyVERSION
216900100716     C                   clear                   keyRELEASE
217000100716     C                   clear                   keyAGENCY
217100100716     C                   clear                   keyASSOCIA
217200100716      *
217300100719      *  Chiamata generica x decodificare il singolo segmento letto
217400100719     c                   call      'TRTCT00R1'
217500100719      * input
217600100719     c                   parm                    tipo_seg
217700100719     c                   parm                    segmento
217800100719     C                   parm                    keyUNBCLI
217900100719     C                   parm                    keyTIPOMSG
218000100719     C                   parm                    keyVERSION
218100100719     C                   parm                    keyRELEASE
218200100719     C                   parm                    keyAGENCY
218300100719     C                   parm                    keyASSOCIA
218400100719      *output
218500100719     c                   parm                    Errore
218600100719     c                   parm                    DsGenerica
218700100719     c                   parm                    Valori_inErr
218800100719     c                   parm                    Descr_Errore
218900100719     c                   parm                    trovato_seg
219000100719      **--------
219100100719      *  Ritorna la DS GENERICA oppure l'errore
219200090210      **--------
219300090211     c                   select
219400100719      *
219500100719     c                   when      trovato_seg ='N'
219600100719      *
219700100719     c                   move      'S'           seg_imprevisto
219800100719     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
219900100719     c                   exsr      Error_DET
220000100719     c                   goto      end_Decod
220100100719      **--------
220200100719     c                   when      tipo_seg = 'UNA'
220300100719     c                   movel(p)  DsGenerica    EDS00UNA
220400100719      **--------
220500100719      *
220600100719     c                   when      tipo_seg = 'UNB'
220700100719     c                   movel(p)  DsGenerica    EDS00UNB
220800090210     C                   EXSR      DATI_UNB
220900090210      * --------
221000090209     c                   when      tipo_seg = 'UNH'
221100100719     c                   movel(p)  DsGenerica    EDS00UNH
221200090210     C                   EXSR      DATI_UNH
221300090210      * --------
221400090209     c                   when      tipo_seg = 'BGM'
221500100719     c                   movel(p)  DsGenerica    EDS00BGM
221600090210      **--------
221700090209     c                   when      tipo_seg = 'DTM'
221800100719     c                   movel(p)  DsGenerica    EDS00DTM
221900090210     C                   EXSR      DATI_DTM
222000090210      * --------
222100090209     c                   when      tipo_seg = 'TSR'
222200100719     c                   movel(p)  DsGenerica    EDS00TSR
222300090210      **--------
222400090209     c                   when      tipo_seg = 'CNT'
222500100719     c                   movel(p)  DsGenerica    EDS00CNT
222600090210      * --------
222700090209     c                   when      tipo_seg = 'TOD'
222800100719     c                   movel(p)  DsGenerica    EDS00TOD
222900090210     C                   EXSR      DATI_TOD
223000090210      **--------
223100090209     c                   when      tipo_seg = 'RFF'
223200100719     c                   movel(p)  DsGenerica    EDS00RFF
223300090210     C                   EXSR      DATI_RFF
223400090210      **--------
223500090209     c                   when      tipo_seg = 'TDT'
223600100719     c                   movel(p)  DsGenerica    EDS00TDT
223700090210      **--------
223800090213     c                   when      tipo_seg = 'MOA'
223900100719     c                   movel(p)  DsGenerica    EDS00MOA
224000090213     C                   EXSR      DATI_MOA
224100090213      **--------
224200090213     c                   when      tipo_seg = 'FTX'
224300100719     c                   movel(p)  DsGenerica    EDS00FTX
224400090213     C                   EXSR      DATI_FTX
224500090213      **--------
224600090209     c                   when      tipo_seg = 'NAD'
224700100719     c                   movel(p)  DsGenerica    EDS00NAD
224800090210     C                   EXSR      DATI_NAD
224900090210      **--------
225000090209     c                   when      tipo_seg = 'CTA'
225100100719     c                   movel(p)  DsGenerica    EDS00CTA
225200090210     C                   EXSR      DATI_CTA
225300090210      **--------
225400090209     c                   when      tipo_seg = 'COM'
225500100719     c                   movel(p)  DsGenerica    EDS00COM
225600090210     C                   EXSR      DATI_COM
225700090210      **--------
225800090209     c                   when      tipo_seg = 'GID'
225900100719     c                   movel(p)  DsGenerica    EDS00GID
226000090210     C                   EXSR      DATI_GID
226100090210      **--------
226200090209     c                   when      tipo_seg = 'MEA'
226300100719     c                   movel(p)  DsGenerica    EDS00MEA
226400090210     C                   EXSR      DATI_MEA
226500090210      **--------
226600090209     c                   when      tipo_seg = 'PCI'
226700100719     c                   movel(p)  DsGenerica    EDS00PCI
226800090210     C                   EXSR      DATI_PCI
226900090210      **--------
227000090209     c                   when      tipo_seg = 'UNT'
227100100719     c                   movel(p)  DsGenerica    EDS00UNT
227200090210     C                   EXSR      DATI_UNT
227300090210      **--------
227400090209     c                   when      tipo_seg = 'UNZ'
227500100719     c                   movel(p)  DsGenerica    EDS00UNZ
227600090210     C                   EXSR      DATI_UNZ
227700090210      **--------
227800100719     c                   OTHER
227900100719      **--------
228000100719     c                   move      'S'           seg_imprevisto
228100100719     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
228200100719     c                   exsr      Error_DET
228300100719     c                   goto      end_Decod
228400100719      **--------
228500100719     c                   endSL
228600100719      **
228700100719     c     end_Decod     endSR
228800090209     c*==================================================================*
228900090209     C*? CNVDAT - DECODIFICA LA DATA
229000090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
229100090209     C*         - WDATA  (DATA ALFANUMERICA)
229200090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
229300090209     C*         - WHMS   (ORA NUMERICA)
229400090209     C*----------------------------------------------------------------
229500090209     C     CNVDAT        BEGSR
229600090209     C*
229700090209     C                   MOVEL     ' '           WERRO             1
229800090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
229900090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
230000090209     C                   Z-ADD     *ZEROS        WHMS              6 0
230100090209     C*
230200090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
230300090209     C                   MOVEL     WDATA         WCOMO8            8
230400090209     C                   TESTN                   WCOMO8               99
230500090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
230600090209     C  N99              MOVEL     'S'           WERRO             1
230700090209     C                   END
230800090209     C*
230900090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
231000090209     C                   MOVEL     WDATA         WCOMO14          14
231100091216      *
231200091216     c                   if        %subst(WCOMO14:13:2) = *blank
231300091216     c                   eval      %subst(WCOMO14:13:2) = '00'
231400091216     c                   end
231500091216      *
231600090209     C                   TESTN                   WCOMO14              99
231700090209     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
231800090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
231900090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
232000090209     C  N99              MOVEL     'S'           WERRO             1
232100090209     C                   END
232200090209     C*
232300090209     C                   ENDSR
232400090210     C*----------------------------------------------------------------
232500090210     C*? TSTNUM - CONTROLLO NUMERICO
232600090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
232700090210     C*         - WLUNGH (Lunghezza campo output)
232800090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
232900090210     C*----------------------------------------------------------------
233000090210     C     TSTNUM        BEGSR
233100090210     C*
233200090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
233300090210     C                   MOVEL     'N'           WOKNUM            1
233400090210     C                   MOVEL     *BLANKS       WNUMOK           15
233500090210     C                   Z-ADD     15            IN                3 0
233600090210     C                   Z-ADD     WLUNGH        IX                3 0
233700090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
233800090210     C                   MOVEA     WNUM          NDA
233900090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
234000090210     C                   MOVEA     *ALL'0'       NDN
234100090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
234200090210     C                   Z-ADD     WLUNGH        I                 3 0
234300090210     C     I             DOWGT     0                                            --- 1 -->
234400090210     C* ESTRAE IL CARATTERE DA ESAMINARE
234500090210     C     1             SUBST     WNUM:I        WTEM01            1
234600090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
234700090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
234800090210     C     WTEM01        SCAN      WCA                                    99
234900090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
235000090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
235100090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
235200090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
235300090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
235400090210     C* DA DESTRA VERSO SINISTRA
235500090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
235600090210     C                   MOVEL     WTEMP1        NDN(IN)
235700090210     C                   SUB       1             IN
235800090210     C                   END                                                    <-- 3 ---
235900090210     C                   END                                                    <-- 2 ---
236000090210     C                   SUB       1             I
236100090210     C                   END                                                    <-- 1 ---
236200090210     C*
236300090210     C                   MOVEA     NDN           WNDN             15
236400090210     C     WNDN          IFNE      *ZEROS
236500090210     C                   MOVEA     NDN           WNUMOK           15
236600090210     C                   MOVEL     'S'           WOKNUM            1
236700090210     C                   END
236800090210     C*
236900090210     C                   ENDSR
237000090210     C*----------------------------------------------------------------
237100090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
237200090210     C*----------------------------------------------------------------
237300090210     C     CNVCAP        BEGSR
237400090210     C*
237500090210     C                   MOVEL     *BLANKS       WCAP              9
237600090210     C     nad3251       IFNE      '0'
237700090210     C     '  '          SCAN      nad3251       LENGHT            2 0
237800090210     C                   SUB       1             LENGHT
237900090210     C     LENGHT        IFLE      5
238000090210     C     LENGHT        ANDGT     0
238100090210     C                   Z-ADD     LENGHT        T                 2 0
238200090210     C                   Z-ADD     5             S                 2 0
238300090210     C                   MOVEA     nad3251       CAP9
238400090210     C                   MOVEL     *ZEROS        CAP5
238500090210     C                   DO        LENGHT
238600090210     C                   MOVE      CAP9(T)       CAP5(S)
238700090210     C                   SUB       1             S
238800090210     C                   SUB       1             T
238900090210     C                   END
239000090210     C                   MOVEA     CAP5          WCAP5             5
239100090210     C     WCAP5         IFEQ      '0'
239200090210     C                   MOVEL     *BLANKS       WCAP
239300090210     C                   ELSE
239400090210     C                   MOVEA     CAP5          WCAP
239500090210     C                   END
239600090210     C*
239700090210     C                   ELSE
239800090210     C                   MOVEL     nad3251       WCAP
239900090210     C                   END
240000090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
240100090210     C     WCAP          IFNE      *BLANKS
240200090210     C     WFLCPF        ANDNE     0
240300090210     C                   MOVEL     WFLCPF        WELE              1 0
240400090210     C                   MOVE      WFLCPF        WPOS              1 0
240500090210     C                   MOVEL     *BLANK        WCPF
240600090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
240700090210     C                   ELSE
240800090210     C                   MOVEL     *BLANKS       WCPF
240900090210     C                   END
241000090210     C                   END
241100090210     C*
241200090210     C                   ENDSR
241300090211      * ?================================================================== */
241400090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
241500090211      * ?================================================================== */
241600090211     C     Scrive_VAB    BEGSR
241700090211      *
241800090211     c                   clear                   err_bloccante     1
241900090211     c                   move      'S'           Almeno_Uno
242000090211      *
242100090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
242200090211     c                   exsr      UPDVAB
242300090211      *
242400090211     C* >>>>>>>>>>>>
242500090211     C*  Richiamo pgm per scrittura immediata VAB
242600090211     C     §PUWRK        IFEQ      ' '
242700090211      *
242800090211     C*  Richiamo pgm per esecuzione stampa
242900090211     C     §PTCMR        IFEQ      'S'
243000090213     C                   MOVEL     WNRDOC        d86CMR
243100090211     C                   ELSE
243200090213     C                   MOVEL     WNRDOC        d86CMR
243300090211     C                   END
243400090211      *
243500090213     C                   MOVEL     ' '           d86RIE
243600090213     C                   MOVEL     §PUDTA        d86DTA
243700090213     C                   Z-ADD     1             d86CNT
243800090213     C                   Z-ADD     §PTKSC        d86CCM
243900090213     C                   MOVEL     *BLANKS       d86STA
244000090213     C                   MOVEL     'E'           d86MOD
244100090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
244200090211      * dove poi imposta la DS dei parametri passati
244300090213     C                   MOVEL     'LR'          d86CHI
244400090213     C                   MOVEL     'N'           d86CTL
244500090211      *
244600090211     c                   move      kpjbu         SvKpjbu
244700090211     c                   clear                   kpjbu
244800090216     C                   movel     *on           Commit_on         1
244900090213     C                   MOVEL     TRTC86ds      KPJBU
245000090213      * ?- Chiamata la TRTC86R2 -*
245100090213     C                   CALL      'TRTC86R2'
245200090211     C                   PARM                    KPJBA
245300090216     C                   PARM                    Commit_on
245400090211     c                   move      Svkpjbu       Kpjbu
245500090211      *
245600090211     C*  Controllo pgm per scrittura immediata VAB
245700090211     C     WW            IFNE      0
245800090211     C                   DO        WW            WX                3 0
245900090213     C                   Z-ADD     KCL(WX)       d86CCM
246000090211      *
246100090211     c                   move      kpjbu         SvKpjbu
246200090211     c                   clear                   kpjbu
246300090216     C                   movel     *on           Commit_on         1
246400090213     C                   MOVEL     TRTC86ds      KPJBU
246500090213      * ?- Chiamata la TRTC86R2 -*
246600090213     C                   CALL      'TRTC86R2'
246700090211     C                   PARM                    KPJBA
246800090216     C                   PARM                    Commit_on
246900090211     c                   move      Svkpjbu       Kpjbu
247000090211     C                   END
247100090211     C                   END
247200090211      *
247300090211     C                   END
247400090211      * >>>>>>>>>>>>
247500090211     C                   if        se_errore  = 'S'
247600090211     c                   move      'S'           err_bloccante
247700090211     c                   goto      Error_VAB
247800090211     c                   end
247900090211     C*
248000090211     C     Error_VAB     Endsr
248100090211     C*----------------------------------------------------------------
248200090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
248300090211     C*----------------------------------------------------------------
248400090211     C     UPDVAB        BEGSR
248500090211      *
248600090211     C*  Se non è stato impostato il codice bolla lo imposto io
248700090211     C     VABCBO        IFEQ      *BLANKS
248800090211     C     VABCAS        IFGT      0
248900090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
249000090211     C                   ELSE
249100090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
249200090211     C                   END
249300090211     C                   END
249400110922      * Imposta le NOte
249500110922     C     VABNOT        IFEQ      *BLANKS
249600110922     C                   MOVEL     wnot1         VABNOT
249700110922     C                   MOVEL     wnot2         VABNT2
249800110922     C                   ELSE
249900110922     C                   MOVEL     wnot1         VABNT2
250000110922     C                   END
250100110922      *
250200090211      *
250300090211     C*  Attribuisco anno spedizione
250400090211     C     WDATPA        IFGT      0                                            anno sped.
250500090211     C                   MOVEL     WDATPA        VABAAS
250600090211     C                   ELSE
250700090211     C     WDATFV        IFGT      0
250800090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
250900090211     C                   END
251000090211     C                   END
251100090211      *
251200090211      *  Controllo dettaglio segnacolli
251300090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
251400090211     C                   EXSR      CTRSGE
251500090211     C                   ELSE
251600090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
251700090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
251800090211     C                   EXSR      CTRSGN
251900090211     C                   END
252000090211     C                   END
252100090211      *
252200090212      *  Imposto linea partenza e serie
252300090211     C                   eval      VABLNP = VABLNP_PT
252400090212     C                   eval      VABNRS = VABnrs_PT
252500090211      *
252600090211      *  Controllo se devo variare il codice trattamento merce
252700090211     C                   eval      VABCTM = VABctm_PT
252800090211      *
252900090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
253000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
253100090211     c                   clear                   mantiene_nsp      1            *blk/'S'
253200090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
253300090211      *
253400090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
253500090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
253600090211      *
253700090211      *  Imposto codice cliente
253800090211     c                   Select
253900090211      *
254000090211      *  Specificato codice da qualificatore FF (TD15)
254100090211     C                   When        WCLKSC <> *zeros
254200090211     C                   Z-ADD     WCLKSC        VABCCM
254300090211     C                   MOVEL     WCLTAR        VABCTR
254400090211      * forza LNP/CTM/NRS
254500090211     c                   if        WclLNP <> 0
254600090211     C                   eval      VABLNP = WclLNP
254700090211     c                   end
254800090211     c                   if        WclNRS <> 0
254900090211     C                   eval      VABNRS = WclNRS
255000090211     c                   end
255100090211     c                   if        WclCTM <> *blank
255200090211     C                   eval      VABCTM = WclCTM
255300090211     c                   end
255400090211      *
255500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
255600090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
255700090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
255800090211     c                   end
255900090211      *
256000090211      *  Specificato codice da qualificatore FF (TT15)
256100090211     C                   When        $CLKSC <> *zeros
256200090211     C                   Z-ADD     $CLKSC        VABCCM
256300090211     C                   MOVEL     $CLTAR        VABCTR
256400090211      * forza LNP/CTM/NRS
256500090211     c                   if        $clLNP <> 0
256600090211     C                   eval      VABLNP = $clLNP
256700090211     c                   end
256800090211     c                   if        $clNRS <> 0
256900090211     C                   eval      VABNRS = $clNRS
257000090211     c                   end
257100090211     c                   if        $clCTM <> *blank
257200090211     C                   eval      VABCTM = $clCTM
257300090211     c                   end
257400090211      *
257500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
257600090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
257700090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
257800090211     c                   end
257900090211      *
258000090211      *  Imposto dati da tabella - PT-
258100090211     C                   Other
258200090211      *
258300090211     C                   MOVEL     §PTKSC        VABCCM
258400090211     C                   MOVEL     §PTCTR        VABCTR
258500090211      *
258600090211     C                   Endsl
258700090211      *
258800090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
258900090211      *    o dalla relativa CL
259000090211     C                   MOVEL     VABlnp        VABfgs
259100090211      *
259200090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
259300090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
259400090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
259500090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
259600090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
259700090211      *
259800090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
259900090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
260000090211     c                   else
260100090211      *
260200090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
260300090211     C*  lo attribuisco io
260400090211      **              **------------------**
260500090211     c                   exsr      repnum
260600090211      **              **------------------**
260700090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
260800090211      **              **------------------**
260900090211     c                   end
261000090211      *
261100090211     C*  Altrimenti attribuisco data del giorno
261200090211     C     VABMGS        IFEQ      0
261300090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
261400090211     C                   MOVE      WOGGI         VABMGS                         = oggi
261500090211     C                   END
261600090211     C*  Eseguo posizionamento su EDiVAB
261700090211     C                   Z-ADD     VABAAS        KAAS
261800090211     C                   Z-ADD     VABlnp        KLNP
261900090211     C                   Z-ADD     VABNRS        KNRS
262000090211     C                   Z-ADD     VABNSP        KNSP
262100090211     C                   MOVEL     SAVBOL        SALVA
262200090211     C     KVAB1         CHAIN     EDiVAB1L                           30
262300090211     C                   MOVEL     SALVA         SAVBOL
262400090211     C*  Imposto dati CMR
262500090211     C                   Z-ADD     1             VABCNT
262600090211     **
262700090211      *** Attenzione se si deve trattare il VAX con il CMR
262800090211      *** il campo VABCNT deve essere sempre impostato a (1)
262900090211     C     §puWRK        ifEQ      'S'
263000090211     C     §puNSP        andEQ     'S'
263100090211     C                   Z-ADD     1             VABCNT
263200090211     c                   end
263300090211     **
263400090211     C                   Z-ADD     WDTPRE        VABDTS
263500090212     C                   movel     '20'          VABDTS
263600090212     C     WHMPRE        mult      100           VABHMS
263700090211     C     §PTCMR        IFEQ      'S'
263800090211     C                   Z-ADD     WDTCMR        VABDCM
263900090211     C                   MOVEL     WNRCMR        VABCMR
264000090211     C                   ELSE
264100090211     C                   Z-ADD     WDATFV        VABDCM
264200090211     C                   MOVEL     WNUMFV        VABCMR
264300090211     C                   END
264400090211     C*  Se non è indicato il nome del mittente = Partner mittente
264500090211     C     VABRMO        IFEQ      *BLANKS
264600090211     C                   MOVEL     ACORAG        VABRMO
264700090211     C                   MOVEL     INDCAP        VABCMO
264800090211     C     INDCAE        IFNE      *BLANKS
264900090211     C                   MOVEL     INDCAE        VABCMO
265000090211     C                   END
265100090211     C     INDSTA        IFNE      *BLANKS
265200090211     C                   MOVEL     INDSTA        VABNMO
265300090211     C                   END
265400090211     C                   END
265500090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
265600090211     C     §1BFG1        IFEQ      'N'
265700090211     C     §1BFG7        ANDEQ     'N'
265800090211     C     §1BFG2        ANDEQ     'N'
265900090211     C***                  Z-ADD0         VABNRS
266000090211     C                   END
266100090211     C*  Imposto segancollo da - a
266200090211     C     §1BFG1        IFEQ      'S'
266300090211     C     §1BFG7        ANDEQ     'N'
266400090211     C     §1BF12        ANDNE     'E'
266500090211      *
266600090211     C     XY            IFEQ      *ZEROS
266700090211     C                   CLEAR                   VABNCD
266800090211     C                   CLEAR                   VABNCA
266900090211     C                   ELSE
267000090211     C                   Z-ADD     SGN(1)        VABNCD
267100090211     C                   Z-ADD     SGN(XY)       VABNCA
267200090211     C                   ENDIF
267300090211     C*
267400090211     C                   END
267500090211     C*
267600090211     C*  Se cap mittente originale a blank abblenco nazione
267700090211     C     VABCMO        IFEQ      *BLANKS
267800090211     C                   MOVEL     *BLANKS       VABNMO
267900090211     C                   END
268000090211     C*
268100090211      *? ------------------------------------------------------------------------
268200090213     C   30              WRITE     EDiVAB00                             99
268300090213     C  N30              UPDATE    EDiVAB00                             99
268400090211      *? ------------------------------------------------------------------------
268500090213     c   99              eval      errori_in_Wrt = 'S'
268600090211     C*
268700090211     c                   clear                   wri_vabtic        1
268800090211     C*
268900090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
269000090211     C                   EXSR      WRTVAT_Rif
269100090211     C*
269200090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
269300090211     C     §1BF12        IFEQ      'E'
269400090211     C                   EXSR      WRTVAT
269500090211     C                   ELSE
269600090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
269700090211     C     §1BFG7        IFEQ      'S'
269800090211     C     §1BFG1        OREQ      'S'
269900090211     C                   EXSR      WRTVAD
270000090211     C                   END
270100090211     C                   END
270200090211      *
270300090211     C* Reimposto codice trattamento merce cliente
270400090211     C                   MOVEL     WSVTRM        DS1B
270500090211     C**
270600090211     C* Do errore se previsto CMR ma non c'è
270700090211     C     §PTCMR        IFNE      ' '
270800090211     C                   EXSR      MEMCMR
270900090211     C                   END
271000090211     C* Do errore se previsto AFB ma non c'è
271100090211     C     §PTNFV        IFNE      ' '
271200090211     C                   EXSR      MEMAFB
271300090211     C                   END
271400090211     C*  Se il cliente vuole il ritorno delle date di consegna
271500090211     C*  è c'è una squdratura fra i segnacolli e il numero
271600090211     C*  dei colli che ci ha passato scrivo EDSUM
271700090211     C     §PUDTA        IFEQ      'S'
271800090211     C     WSQUAD        ANDEQ     'S'
271900090211     C                   EXSR      WRTSUM
272000090211     C                   END
272100090211     C*
272200090211     C                   ENDSR
272300090211     C*----------------------------------------------------------------
272400090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
272500090211     C*----------------------------------------------------------------
272600090211     C     CTRSGN        BEGSR
272700090211     C*
272800090211     C                   MOVEL     'N'           WNSGER            1
272900090211     C*
273000090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
273100090211     C     VABNCL        IFNE      XY
273200090211     C                   MOVEL     'S'           WNSGER
273300090211     C                   MOVEL     'S'           WSQUAD
273400090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
273500090211     c                              bollettati'
273600090211     C                   GOTO      FINSGN
273700090211     C                   END
273800090211     C*  Riordino i segnacolli
273900090211     C     §1BFG7        IFEQ      'N'
274000090211     C                   SORTA     SGN
274100090211     C*  Controllo se sono sequenziali
274200090211     C                   MOVEL     'N'           WNOSEQ            1
274300090211     C     SGN(1)        ADD       1             WEXSGN            7 0
274400090211     C     2             DO        XY            X
274500090211     C     SGN(X)        IFNE      WEXSGN
274600090211     C                   MOVEL     'S'           WNOSEQ
274700090211     C                   END
274800090211     C     SGN(X)        ADD       1             WEXSGN
274900090211     C                   END
275000090211     C                   END
275100090211     C*
275200090211     C     FINSGN        ENDSR
275300090211     C*----------------------------------------------------------------
275400090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
275500090211     C*----------------------------------------------------------------
275600090211     C     CTRSGE        BEGSR
275700090211     C*
275800090211     C                   MOVEL     'N'           WNSGER            1
275900090211      *
276000090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
276100090211      *
276200090211     C     VABNCL        IFNE      §§
276300090211     C                   MOVEL     'S'           WNSGER
276400090211     C                   MOVEL     'S'           WSQUAD
276500090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
276600090211     c                              bollettati'
276700090211     C                   END
276800090211      *
276900090211     C                   ENDSR
277000090211     C*----------------------------------------------------------------
277100090211     C*? ESEGUO SCRITTURA EDiVAD
277200090211     C*----------------------------------------------------------------
277300090211     C     WRTVAD        BEGSR
277400090211     C*
277500090211     C* Se non seq. scrivo xy record
277600090211     C     §1BFG1        IFEQ      'S'
277700090211     C     XY            ANDNE     *ZEROS
277800090211     C*
277900090211     C     §1BFG7        IFEQ      'S'
278000090211     C                   DO        XY            X
278100090211     C                   Z-ADD     VABCCM        KCCM
278200090211     C                   Z-ADD     SGN(X)        KNCD
278300090211     C                   Z-ADD     VABCNT        VADCNT
278400090211     C                   MOVEL     VABCMR        VADCMR
278500090211     C                   Z-ADD     VABAAS        VADAAS
278600090211     C                   Z-ADD     VABLNP        VADLNP
278700090211     C                   Z-ADD     VABNRS        VADNRS
278800090211     C                   Z-ADD     VABNSP        VADNSP
278900090211     C                   Z-ADD     1             VADNCL
279000090211     C                   MOVEL     SGN(X)        VADCDP
279100090211     C                   Z-ADD     SGN(X)        VADNCD
279200090211     C                   Z-ADD     *ZEROS        VADNCA
279300090211     C                   Z-ADD     VABCCM        VADCCM
279400090211     C                   movel     VABfgs        VADfgs
279500090213     C                   WRITE     EDiVAD00                             99
279600090213     c   99              eval      errori_in_Wrt = 'S'
279700090211     C                   END
279800090211     C* Se sequenziali scrivo un solo record
279900090211     C                   ELSE
280000090211     C                   Z-ADD     VABAAS        VADAAS
280100090211     C                   Z-ADD     VABLNP        VADLNP
280200090211     C                   Z-ADD     VABNRS        VADNRS
280300090211     C                   Z-ADD     VABNSP        VADNSP
280400090211     C                   Z-ADD     VABNCL        VADNCL
280500090211     C                   Z-ADD     SGN(1)        VADNCD
280600090211     C                   Z-ADD     SGN(XY)       VADNCA
280700090211     C                   Z-ADD     VABCCM        VADCCM
280800090211     C                   movel     VABfgs        VADfgs
280900090213     C                   WRITE     EDiVAD00                             99
281000090213     c   99              eval      errori_in_Wrt = 'S'
281100090211     C                   END
281200090211     C*
281300090211     C                   ELSE
281400090211     C                   DO        XY            X
281500090211     C                   Z-ADD     VABCCM        KCCM
281600090211     C                   Z-ADD     SGN(X)        KNCD
281700090211     C                   Z-ADD     VABAAS        VADAAS
281800090211     C                   Z-ADD     VABLNP        VADLNP
281900090211     C                   Z-ADD     VABNRS        VADNRS
282000090211     C                   Z-ADD     VABNSP        VADNSP
282100090211     C                   MOVEL     SGN(X)        VADCDP
282200090211     C                   Z-ADD     1             VADNCL
282300090211     C                   Z-ADD     *ZEROS        VADNCD
282400090211     C                   Z-ADD     *ZEROS        VADNCA
282500090211     C                   Z-ADD     VABCCM        VADCCM
282600090211     C                   movel     VABfgs        VADfgs
282700090213     C                   WRITE     EDiVAD00                             99
282800090213     c   99              eval      errori_in_Wrt = 'S'
282900090211     C                   END
283000090211     C*
283100090211     C                   END
283200090211     C*
283300090211     C                   ENDSR
283400090211     C*----------------------------------------------------------------
283500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
283600090211     C*----------------------------------------------------------------
283700090211     C     WRTVAT_Rif    BEGSR
283800090211      *
283900090211      *  riferimento di consegna destinatario
284000090211     c                   if        WatRDE <> *blank
284100090211     C                   Z-ADD     VABCNT        VATCNT
284200090211     C                   MOVEL     VABCMR        VATCMR
284300090211     C                   Z-ADD     VABCCM        VATCCM
284400090211     C                   movel     VABfgs        VATfgs
284500090211     C                   Z-ADD     VABLNP        VATLNP
284600090211     C                   Z-ADD     VABAAS        VATAAS
284700090211     C                   Z-ADD     VABNRS        VATNRS
284800090211     C                   Z-ADD     VABNSP        VATNSP
284900090211     C                   MOVEL     'A'           VATTRC
285000090211     C                   MOVEL     WatRDE        VATNOT
285100090213     C                   WRITE     EDiVAT00                             99
285200090213     c   99              eval      errori_in_Wrt = 'S'
285300090211     c                   end
285400090211      *
285500090211      *  riferimento di consegna telefonico
285600090211     c                   if        WatTEL <> *blank
285700090211     C                   Z-ADD     VABCNT        VATCNT
285800090211     C                   MOVEL     VABCMR        VATCMR
285900090211     C                   Z-ADD     VABCCM        VATCCM
286000090211     C                   movel     VABfgs        VATfgs
286100090211     C                   Z-ADD     VABLNP        VATLNP
286200090211     C                   Z-ADD     VABAAS        VATAAS
286300090211     C                   Z-ADD     VABNRS        VATNRS
286400090211     C                   Z-ADD     VABNSP        VATNSP
286500090211     C                   MOVEL     'B'           VATTRC
286600090211     C                   MOVEL     WatTEL        VATNOT
286700090213     C                   WRITE     EDiVAT00                             99
286800090213     c   99              eval      errori_in_Wrt = 'S'
286900090211     c                   end
287000090211      *
287100090211      * riferimento del Partner che una volta veniva riportato sul BL4
287200090211     c                   if        Wnsp   <> *blank
287300090211     C                   Z-ADD     VABCNT        VATCNT
287400090211     C                   MOVEL     VABCMR        VATCMR
287500090211     C                   Z-ADD     VABCCM        VATCCM
287600090211     C                   movel     VABfgs        VATfgs
287700090211     C                   Z-ADD     VABLNP        VATLNP
287800090211     C                   Z-ADD     VABAAS        VATAAS
287900090211     C                   Z-ADD     VABNRS        VATNRS
288000090211     C                   Z-ADD     VABNSP        VATNSP
288100090211     C                   MOVEL     'C'           VATTRC
288200090211     C                   MOVEL     Wnsp          VATNOT
288300090213     C                   WRITE     EDiVAT00                             99
288400090213     c   99              eval      errori_in_Wrt = 'S'
288500090211     c                   end
288600090211      *
288700090211     C                   ENDSR
288800090211     C*----------------------------------------------------------------
288900090211     C*? ESEGUO SCRITTURA EDiVAT
289000090211     C*----------------------------------------------------------------
289100090211     C     WRTVAT        BEGSR
289200090211      *
289300090211     C                   DO        §§            X
289400090211     C                   Z-ADD     VABCNT        VATCNT
289500090211     C                   MOVEL     VABCMR        VATCMR
289600090211     C                   Z-ADD     VABCCM        VATCCM
289700090211     C                   movel     VABfgs        VATfgs
289800090211     C                   Z-ADD     VABLNP        VATLNP
289900090211     C                   Z-ADD     VABAAS        VATAAS
290000090211     C                   Z-ADD     VABNRS        VATNRS
290100090211     C                   Z-ADD     VABNSP        VATNSP
290200090211     C                   MOVEL     'E'           VATTRC
290300090211      *
290400090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
290500090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
290600090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
290700090211     c                   if        WVATlun > 0
290800090211      *
290900090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
291000090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
291100090211      *
291200090211     c                   else
291300090211      *
291400090211      * Se non è impostato nulla prende il segnacollo passato così com'è
291500090211     C                   MOVEL     SGE(X)        VATNOT
291600090211     c                   end
291700090211      *
291800090213     C                   WRITE     EDiVAT00                             99
291900090213     c   99              eval      errori_in_Wrt = 'S'
292000090211     C                   END
292100090211      *
292200090211     C                   ENDSR
292300090211     C*----------------------------------------------------------------
292400090211     C*? ESEGUO SCRITTURA EDSUM
292500090211     C*----------------------------------------------------------------
292600090211     C     WRTSUM        BEGSR
292700090211     C*
292800090211     C* Se non seq. scrivo xy record
292900090211     C                   DO        XY            X
293000090211     C                   Z-ADD     VABAAS        SUMAAS
293100090211     C                   Z-ADD     VABLNP        SUMFLS
293200090211     C                   MOVEL     WKSC          SUMLNP
293300090211     C                   Z-ADD     VABLNA        SUMLNA
293400090211     C                   Z-ADD     VABZNC        SUMZNC
293500090211     C                   Z-ADD     VABNRS        SUMNRS
293600090211     C                   Z-ADD     VABNSP        SUMNSP
293700090211     C                   Z-ADD     SGN(X)        SUMNSC
293800090211     C                   Z-ADD     WKSC          SUMCCM
293900090211     C                   MOVEL     *BLANKS       SUMSTS
294000090211     C                   MOVEL     *BLANKS       SUMFLG
294100090211     C                   MOVEL     WPRGSP        SUMCNI
294200090211     C                   MOVEL     VABCMR        SUMCMR
294300090211     C                   Z-ADD     0             SUMNFE
294400090211     C                   Z-ADD     VABCCM        VADCCM
294500090213     C                   WRITE     EDSUM000                             99
294600090213     c   99              eval      errori_in_Wrt = 'S'
294700090211     C                   END
294800090211     C*
294900090211     C                   ENDSR
295000090211     c*==================================================================*
295100090211      * Manda un Msg in Posta AS Sede a EDP*
295200090211     c*==================================================================*
295300090211     c     SEND_MSG_EDP  begsr
295400090211      *
295500090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
295600090211      *   Lo manda una sola volta per lavoro
295700090211     c                   IF        Snd_Msg_alert  <> 'N' and
295800090211     c                             User_msg <> *blank
295900090211     c                   z-add     1             Jx
296000090211     C                   exsr      CalQEZSNDMG
296100090211     c                   end
296200090211      *
296300090211     c                   endsr
296400090213     c**********************************************************************
296500090213     c* reperimento numero Spedizione
296600090213     c**********************************************************************
296700090213     C     repnum        BEGSR
296800090213      *
296900090213      *    deve controllare sulla tabella "3C" se il numero spedizione
297000090213      *     deve essere mantenuto oppure no
297100090213     C*
297200090213     C                   clear                   Ds3C
297300090213     C                   Z-ADD     CODUT         TBLKUT
297400090213     C                   MOVEL     '3C'          TBLCOD
297500090213     C                   movel(p)  VABccm        TBLKEY
297600090213     C     KTABel        CHAIN     TABEL00F                           30
297700090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
297800090213     C                   MOVEL     TBLUNI        Ds3C
297900090213     C                   END
298000090213      *
298100090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
298200090213     c                   if        §3CFsp <> 'S'
298300090213      *
298400090213     C* NSP => Stacco un numeratore da AZNUM
298500090213     c                   movel     kpjbu         svkpjbu
298600090213     c                   clear                   kpjbu
298700090213     C                   clear                   TRUL33DS
298800090213     C                   eval      I33OPE = *zeros
298900090213     C                   eval      I33CNU = 302
299000090213     C                   eval      I33NUM = 1
299100090213     C                   movel     TRUL33DS      KPJBU
299200090213     C                   call      'TRUL33R'
299300090213     C                   parm                    KPJBA
299400090213     C                   movel     KPJBU         TRUL33DS
299500090213     c                   movel     svkpjbu       kpjbu
299600090213      ****
299700090213     C                   if        O33ERR = *zeros
299800090213     c                   z-add     o33nrf        NUM_SPED
299900090213     C                   else
300000090213     C                   endif
300100090213      ****
300200090213     c                   ELSE
300300090213      *   se si vuole mantenere il numero spedizione
300400090213      ****
300500090213     C                   MOVEL     WOGGI         WANNO             4 0
300600090213     C                   MOVE      WANNO         KAAA
300700090213     C                   Z-ADD     3             KCNU
300800090213     C                   Z-ADD     VABlnp        KFIL
300900090213     C     KNUF          CHAIN     FLNUF                              9999
301000090213     C     *IN99         IFEQ      '0'
301100090213     C                   ADD       1             NUFNUM
301200090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
301300090213     C                   UPDATE    FLNUF
301400090213     C                   ELSE
301500090213     C                   END
301600090213     C*
301700090213     c                   EndIF
301800090213      **
301900090213     C                   ENDSR
302000090213     C*----------------------------------------------------------------
302100090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
302200090213     C*----------------------------------------------------------------
302300090213     C     MEMCMR        BEGSR
302400090213     C*
302500090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
302600090213     C     §PTCM1        IFEQ      'S'
302700090213     C                   CLEAR                   EDRDE000
302800090213     C                   MOVEL     'TD1154'      KNMC
302900090213     C                   Z-ADD     1             KNRP
303000090213     C                   MOVEL     WKSC          KLNP1
303100090213     C     KRDE          CHAIN     EDRDE01L                           31
303200090213     C     *IN31         IFEQ      '1'
303300090213     C                   Z-ADD     VABAAS        RDEAAS
303400090213     C                   Z-ADD     VABLNP        RDELNP
303500090213     C                   Z-ADD     VABNRS        RDENRS
303600090213     C                   Z-ADD     VABNSP        RDENSP
303700090213     C                   MOVEL     'TD1154'      RDENMC
303800090213     C                   Z-ADD     1             RDENRP
303900090213     C                   MOVEL     WNRCMR        RDEVAL
304000090213     C                   WRITE     EDRDE000                             99
304100090213     c   99              eval      errori_in_Wrt = 'S'
304200090213     C                   END
304300090213     C*  Memorizzo qualificatore CMR
304400090213     C                   CLEAR                   EDRDE000
304500090213     C                   MOVEL     'TD1153'      KNMC
304600090213     C                   Z-ADD     1             KNRP
304700090213     C                   MOVEL     WKSC          KLNP1
304800090213     C     KRDE          CHAIN     EDRDE01L                           31
304900090213     C     *IN31         IFEQ      '1'
305000090213     C                   Z-ADD     VABAAS        RDEAAS
305100090213     C                   Z-ADD     VABLNP        RDELNP
305200090213     C                   Z-ADD     VABNRS        RDENRS
305300090213     C                   Z-ADD     VABNSP        RDENSP
305400090213     C                   Z-ADD     1             RDENRP
305500090213     C                   MOVEL     'TD1153'      RDENMC
305600090213     C                   MOVEL     'CMR'         RDEVAL
305700090213     C                   WRITE     EDRDE000                             99
305800090213     c   99              eval      errori_in_Wrt = 'S'
305900090213     C                   END
306000090213     C*  Memorizzo la data
306100090213     C                   CLEAR                   EDRDE000
306200090213     C                   MOVEL     'TD2380'      KNMC
306300090213     C                   Z-ADD     1             KNRP
306400090213     C                   MOVEL     WKSC          KLNP1
306500090213     C     KRDE          CHAIN     EDRDE01L                           31
306600090213     C     *IN31         IFEQ      '1'
306700090213     C                   Z-ADD     VABAAS        RDEAAS
306800090213     C                   Z-ADD     VABLNP        RDELNP
306900090213     C                   Z-ADD     VABNRS        RDENRS
307000090213     C                   Z-ADD     VABNSP        RDENSP
307100090213     C                   Z-ADD     1             RDENRP
307200090213     C                   MOVEL     'TD2380'      RDENMC
307300090213     C                   MOVEL     WDTCMR        RDEVAL
307400090213     C                   WRITE     EDRDE000                             99
307500090213     c   99              eval      errori_in_Wrt = 'S'
307600090213     C                   END
307700090213     C*
307800090213     C                   END                                                    §PTCM1 = 'S'
307900090213     C*
308000090213     C                   ENDSR
308100090213     C*----------------------------------------------------------------
308200090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
308300090213     C*----------------------------------------------------------------
308400090213     C     MEMAFB        BEGSR
308500090213     C*
308600090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
308700090213     C     §PTNF1        IFEQ      'S'
308800090213     C                   CLEAR                   EDRDE000
308900090213     C                   MOVEL     WKSC          KLNP1
309000090213     C                   MOVEL     'TD1154'      KNMC
309100090213     C                   Z-ADD     1             KNRP
309200090213     C     KRDE          CHAIN     EDRDE01L                           31
309300090213     C     *IN31         IFEQ      '1'
309400090213     C                   Z-ADD     VABAAS        RDEAAS
309500090213     C                   Z-ADD     VABLNP        RDELNP
309600090213     C                   Z-ADD     VABNRS        RDENRS
309700090213     C                   Z-ADD     VABNSP        RDENSP
309800090213     C                   MOVEL     'TD1154'      RDENMC
309900090213     C     §PTCM1        IFNE      'S'
310000090213     C     WNRCMR        OREQ      *BLANKS
310100090213     C                   Z-ADD     1             RDENRP
310200090213     C                   ELSE
310300090213     C                   Z-ADD     2             RDENRP
310400090213     C                   END
310500090213     C                   MOVEL     WNUMFV        RDEVAL
310600090213     C                   WRITE     EDRDE000                             99
310700090213     c   99              eval      errori_in_Wrt = 'S'
310800090213     C                   END
310900090213     C*  Memorizzo qualificatore CMR
311000090213     C                   CLEAR                   EDRDE000
311100090213     C                   MOVEL     WKSC          KLNP1
311200090213     C                   MOVEL     'TD1153'      KNMC
311300090213     C                   Z-ADD     1             KNRP
311400090213     C     KRDE          CHAIN     EDRDE01L                           31
311500090213     C     *IN31         IFEQ      '1'
311600090213     C                   Z-ADD     VABAAS        RDEAAS
311700090213     C                   Z-ADD     VABLNP        RDELNP
311800090213     C                   Z-ADD     VABNRS        RDENRS
311900090213     C                   Z-ADD     VABNSP        RDENSP
312000090213     C     §PTCM1        IFNE      'S'
312100090213     C     WNRCMR        OREQ      *BLANKS
312200090213     C                   Z-ADD     1             RDENRP
312300090213     C                   ELSE
312400090213     C                   Z-ADD     2             RDENRP
312500090213     C                   END
312600090213     C                   MOVEL     'TD1153'      RDENMC
312700090213     C                   MOVEL     'AFB'         RDEVAL
312800090213     C                   WRITE     EDRDE000                             99
312900090213     c   99              eval      errori_in_Wrt = 'S'
313000090213     C                   END
313100090213     C*  Memorizzo la data
313200090213     C                   CLEAR                   EDRDE000
313300090213     C                   MOVEL     WKSC          KLNP1
313400090213     C                   MOVEL     'TD2380'      KNMC
313500090213     C                   Z-ADD     1             KNRP
313600090213     C     KRDE          CHAIN     EDRDE01L                           31
313700090213     C     *IN31         IFEQ      '1'
313800090213     C                   Z-ADD     VABAAS        RDEAAS
313900090213     C                   Z-ADD     VABLNP        RDELNP
314000090213     C                   Z-ADD     VABNRS        RDENRS
314100090213     C                   Z-ADD     VABNSP        RDENSP
314200090213     C     §PTCM1        IFNE      'S'
314300090213     C     WNRCMR        OREQ      *BLANKS
314400090213     C                   Z-ADD     1             RDENRP
314500090213     C                   ELSE
314600090213     C                   Z-ADD     2             RDENRP
314700090213     C                   END
314800090213     C                   MOVEL     'TD2380'      RDENMC
314900090213     C                   MOVEL     WDATFV        RDEVAL
315000090213     C                   WRITE     EDRDE000                             99
315100090213     c   99              eval      errori_in_Wrt = 'S'
315200090213     C                   END
315300090213     C*
315400090213     C                   END                                                    §PTCM1 = 'S'
315500090213     C*
315600090213     C                   ENDSR
315700090213     c*==================================================================*
315800090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
315900090213     c*==================================================================*
316000090213     c     Imp_ADDR_mail begsr
316100090213      *
316200090213     c                   clear                   Indirizzi
316300090213      *
316400090213      *  Lunghezza indirizzi presenti
316500090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
316600090213     c                   z-add     1             al_char           3 0
316700090213     c                   z-add     1             dal_char          3 0
316800090213     c                   z-add     1             tot_char          3 0
316900090213      *
317000090213     c     successivo    tag
317100090213      *
317200090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
317300090213      *   e aggiunge il dominio Bartolini + il (;) separatore
317400090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
317500090213     c                   eval      tot_char = al_char - dal_char
317600090213     c                   z-add     dal_char      Dac
317700090213     c                   z-add     tot_char      Luc
317800090213      *
317900090213     c                   clear                   Indir_Bartol     40
318000090213     c                   clear                   Indir_Parz       20
318100090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
318200090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
318300090213     c                             %Trim(bart_it)
318400090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
318500090213      *
318600090213     c                   if        al_char = Lun_Ind
318700090213     c                   goto      fine_indmail
318800090213     c                   else
318900090213     c                   eval      dal_char = ( al_char + 1 )
319000090213     c                   goto      successivo
319100090213     c                   end
319200090213      *
319300090213     C     fine_indmail  TAG
319400090213      *
319500090213     C                   ENDSR
319600090213     c*==================================================================*
319700090213      * Manda un Msg x E-mail
319800090213     c*==================================================================*
319900090213     c     Invio_Mail    begsr
320000090213      *
320100090213     c                   goto      non_inviare
320200090213      *
320300090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
320400090213     C* Inizializzo variabili
320500090213     C                   movel     *blanks       wrkEml          253
320600090213     C                   movel     *blanks       wrkMsg         5000
320700090213     C                   movel     *blanks       wrkOgg           44
320800090213      *
320900090213     C* Valorizzo i campi della e-m@ail - indirizzo
321000090213     C                   eval      wrkEml = Indirizzi
321100090213     C                   eval      wrkOgg = Oggetto
321200090213     C                   eval      wrkMsg = Messaggio
321300090213
321400090213     C                   call(e)   'TIS701C'
321500090213     C                   parm                    wrkEml
321600090213     C                   parm                    wrkOgg
321700090213     C                   parm                    wrkMsg
321800090213
321900090213     c     non_inviare   tag
322000090213     C*
322100090213     C                   ENDSR
322200090601     c*==================================================================*
322300090601      * Manda un Msg x E-mail a EDPAB
322400090601     c*==================================================================*
322500090601     c     Invio_Mail_AB begsr
322600090601      *
322700090601     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
322800090601     C* Inizializzo variabili
322900090601     C                   movel     *blanks       wrkEml          253
323000090601     C                   movel     *blanks       wrkMsg         5000
323100090601     C                   movel     *blanks       wrkOgg           44
323200090601      *
323300090601     C* Valorizzo i campi della e-m@ail - indirizzo
323400110203     C********           eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
323500110203     C                   eval      wrkEml = Indirizzi
323600110203     C                   eval      wrkOgg = Oggetto
323700110203     C                   eval      wrkMsg = Messaggio
323800110203      ********
323900110203     C********           call(e)   'TIS701C'
324000110203     C********           parm                    wrkEml
324100110203     C********           parm                    wrkOgg
324200110203     C********           parm                    wrkMsg
324300090601     C*
324400110203      ** *****
324500110203     C                   call(e)   'TRTCT00R2'
324600110203     C                   parm                    wrkEml
324700110203     C                   parm                    wrkOgg
324800110203     C                   parm                    wrkMsg
324900110203     C*
325000090601     C                   ENDSR
325100090211     ***********************************************************************
325200090211     **
325300090211     ** Send Message (QEZSNDMG) API
325400090211     **
325500090211     ***********************************************************************
325600090211     C     CalQEZSNDMG   BEGSR
325700090211      *
325800090211     ** Invio messaggio all'utente.
325900090211     C                   EVAL      SndMg03 = msgDSP
326000090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
326100090211     C                   EVAL      SndMg05(Jx) = User_msg
326200090211     C                   EVAL      SndMg06 = Jx
326300090211     C                   CLEAR                   QUSEC
326400090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
326500090211
326600090211     C                   CALL      'QEZSNDMG'
326700090211     C                   PARM                    SndMg01
326800090211     C                   PARM                    SndMg02
326900090211     C                   PARM                    SndMg03
327000090211     C                   PARM                    SndMg04
327100090211     C                   PARM                    SndMg05
327200090211     C                   PARM                    SndMg06
327300090211     C                   PARM                    SndMg07
327400090211     C                   PARM                    SndMg08
327500090211     C                   PARM                    Qusec
327600090211     C                   PARM                    SndMg10
327700090211     C                   PARM                    SndMg11
327800090211     C                   PARM                    SndMg12
327900090211      *
328000090211     C                   ENDSR
328100121102      *---------------------------------------------------------------*
328200121102      * *cotrollo capienza tabella PT
328300121102      *---------------------------------------------------------------*
328400121102     C     Check_PT      begsr
328500121102     C*
328600121102     c* Controllo riempimento schiera
328700121102     c                   clear                   trul0sds
328800121102     c                   eval      i0sski='CPT'
328900121102     c                   eval      i0sele=%elem(CPT)
329000121102     c                   eval      i0spie=x
329100121102     c                   eval      i0sfile='EDTAB00F'
329200121102     c                   eval      i0ssif=knsif
329300121102     c                   eval      i0spgm='TRTCT02R'
329400121113     c                   move      kpjbu         SvKpjbu
329500121113     c                   clear                   kpjbu
329600121102     c                   movel     trul0sds      kpjbu
329700121102     c                   call      'TRUL0SR'
329800121102     c                   parm                    kpjba
329900121113     c                   eval      kpjbu  =  SvKpjbu
330000121102     C*
330100121102     C                   ENDSR
330200090211     O*****************************************************************
330300090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
330400090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
330500090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
330600090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
