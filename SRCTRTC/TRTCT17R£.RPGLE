000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600130930????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFTMIN vers D01A  *
000700130930      *                         cliente     AMAZON       con  IFTMIN vers D01A  *
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
002800140304      *----------------------------------------------------*
002900090213      * numero relativo di record
003000090213     D VINRECDS        DS
003100090213     D  VNREC                397    400B 0
003200090213      *
003300090205      **  Elenco DS di tutti i Segmenti da tradurre
003400091019???? D edS00BGM      E DS
003500091019???? D edS00CNT      E DS
003600091019???? D edS00COM      E DS
003700091019???? D edS00CTA      E DS
003800130930???? D edS00CUX      E DS
003900091019???? D edS00DTM      E DS
004000091019???? D edS00GID      E DS
004100130930???? D edS00LOC      E DS
004200091019???? D edS00MEA      E DS
004300091019???? D edS00NAD      E DS
004400131001      **** PCI - usato NON x il BARCODE ma per altre informazioni
004500091019???? D edS00PCI      E DS
004600091019???? D  D30_1                  4    353    DIM(10)
004700091019???? D edS00RFF      E DS
004800091019???? D edS00TOD      E DS
004900091019???? D edS00MOA      E DS
005000091019???? D edS00FTX      E DS
005100091019???? D  D05                   16    365    DIM(5)
005200130930???? D edS00TSR      E DS
005300091019???? D edS00UNA      E DS
005400131001???? D edS00UNB      E DS
005500091019???? D edS00UNH      E DS
005600091019???? D edS00UNT      E DS
005700091019???? D edS00UNZ      E DS
005800130930      **
005900130930      **  i segmenti TMD e DIM  non sono trascodificati
006000140304      **     perchè NON SERVONO a generare VAB e VAT
006100090205      **
006200090209     D Valori_inErr    ds
006300090209     D  SKout_Errori                  1    DIM(50)
006400090209     D Descr_Errore    ds
006500090209     D  SKout_DesErr                 50    DIM(50)
006600090205      *----------------------------------------------------*
006700091016     D EoFTivin        s              1
006800091016      *----------------------------------------------------*
006900090205     D Tipo_seg        S              3
007000100720     D Trovato_seg     S              1
007100100716      *
007200100716     d keyUNBCLI       s             35
007300100716     d keyTIPOMSG      s              6
007400100716     d keyVERSION      s              3
007500100716     d keyRELEASE      s              3
007600100716     d keyAGENCY       s              3
007700100716     d keyASSOCIA      s              6
007800100716      *
007900100720     D DsGenerica      s           2048
008000090209     D segmento        s           2048
008100090209     D Errore          s              1
008200000223     D W0140           S             14  0
008300991129     D WORA            S              6  0
008400991129     D XX              S              3  0 INZ
008500110328     d Key_Generica35  s             35
008600991129     D WDTGIO          S              8  0
008700991129     D DATEU           S              8  0
008800991129     D DATA_eur        S               D   DATFMT(*eur)
008900050112     D NUM_Sped        s                   LIKE(vabnsp)
009000090213     D NUM_RECord      s                   LIKE(vnREC)
009100090213     D SAVNUM_RECord   s                   LIKE(vnREC)
009200090213     D Last_RECord     s                   LIKE(vnREC)
009300050112     D svkpjbu         s                   like(kpjbu)
009400131022     d BGM_Id_Testata  s             35A   inz(' ')
009500140307     d Work_Data__35   s             35A   inz(' ')
009600091019???? D Dettaglio_NAD_destinatario...
009700091019???? D                 s              1
009800091019???? D NAD_destinatario_in_testata...
009900091019???? D                 s              1
010000091019???? D NAD_mittente_in_testata...
010100091019???? D                 s              1
010200131001???? D siamo_in_Dettaglio...
010300091019???? D                 s              1
010400100120???? D seg_imprevisto...
010500100120???? D                 s              1
010600091019???? D Riferimento_Spedizione...
010700130930???? D                 s              3    INZ('IV ')
010800131002???? D Riferimento_Telefonico...
010900131002???? D                 s              3    INZ('TE ')
011000130930???? D Riferimento_x_Segnacollo...
011100130930???? D                 s              3    INZ('CR ')
011200140304      **?x AMAZON il numerico era identico al segnacollo *  **********
011300140304      **?  diventato "TB" X gestire il segnacollo come   *  **********
011400140304      **? Multicollo (più righe di RFF+CR)               *  **********
011500131002???? D Riferimento_Numerico...
011600140304???? D                 s              3    INZ('TB ')
011700131001???? D Segmento_Inizio_Dettaglio...
011800130930???? D                 s              3    INZ('GID')
011900131001???? D Fine_Ultimo_Dettaglio...
012000131001???? D                 s              3    INZ('UNT')
012100131002???? D in_Kg           s              3    INZ('KG ')
012200131002???? D Peso_in_Kg...
012300131002???? D                 s              3    INZ('WT ')
012400131002???? D Peso_Tassabile...
012500131002???? D                 s              3    INZ('WX ')
012600131002???? D Tipo_Peso_lordo_in_Kg...
012700131002???? D                 s              3    INZ('G  ')
012800131002???? D Tipo_Peso_Tassabile...
012900131002???? D                 s              3    INZ('B  ')
013000140307     D  PU_TipoServ    s              2a
013100140307     D  PU_SpecServ    s              2a
013200090209      *---------------------------------------------------------------------*
013300090209     D KPJBA         E DS
013400090209     D FNLV55DS      E DS
013500090209     D TIBS55DS      E DS
013600090209     D TISI95DS      E DS
013700090209     D UTEDSE0F      E DS
013800090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
013900090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
014000090209      * Ds scomposzione tipo capoconti
014100090209     D TCUDS           DS
014200090209     D  F34                    3      4
014300090209     D  F56                    5      6
014400090209     D DS1B          E DS
014500131002     D SavDS1B         s                   like(ds1b)
014600090209     D DS15          E DS
014700090209     D DSCV          E DS
014800090213     D TRTC86DS      E DS
014900090209     D EDIDSPT       E DS
015000090209     D EDIDSPU       E DS
015100090209     D EDIDSPV       E DS
015200130930      **
015300090211     D EDIDSCL       E DS
015400090216     D EDIDSTC       E DS
015500140307     D EDIDSSS       E DS
015600090209      **
015700090209      *  DS x salvataggio dati impostati in EDiVAB
015800090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
015900090209     D SALVA           DS           545
016000090210      **
016100090209     D WLBDA8          DS
016200090209     D  G02DAT                 1      8  0
016300090209     D  G02INV                 9     16  0
016400090209     D  G02ERR                17     17
016500090209     D  G02TGI                18     22  0
016600090209      *  DS x scomposizione segnacollo
016700090209     D DSSGN           DS
016800090209     D  SGNLNP                 1      3  0
016900090209     D  SGNLNA                 4      6  0
017000090209     D  SGNNRS                 7      8  0
017100090209     D  SGNNSC                 9     15  0
017200090209     D  SGNZNC                16     17  0
017300090209      *  DS x stampa segnacollo
017400090209      *---------------                                                      *
017500090211     D  vablnp_PT      s                   LIKE(vablnp)
017600090212     D  vabnrs_PT      s                   LIKE(vabnrs)
017700090211     D  vabctm_PT      s                   LIKE(vabctm)
017800131001      *---------------                                                      *
017900090209     D Wvabtic         s                   LIKE(vabtic)
018000090603     d tes_VABrmo      s                   LIKE(VABrmo)
018100090603     d tes_VABnmo      s                   LIKE(VABnmo)
018200090603     d tes_VABcmo      s                   LIKE(VABcmo)
018300131001      *
018400090603     d tes_VABrsd      s                   LIKE(VABrsd)
018500090603     d tes_VABrd2      s                   LIKE(VABrd2)
018600090603     d tes_VABind      s                   LIKE(VABind)
018700090603     d tes_VABlod      s                   LIKE(VABlod)
018800090603     d tes_VABnzd      s                   LIKE(VABnzd)
018900131001      *
019000090603     d tes_Valuta      s                   LIKE(VABvca)
019100090209      *---------------------------------------------------------------------*
019200090209      * Schiere
019300090209      *---------------------------------------------------------------------*
019400090209      * Schiera per reperimento nr.seganacollo
019500090209     D SGN             S              7  0 DIM(5000)
019600090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
019700090209      * Stringhe per conversione alfanumerico/numerico N.Documento
019800090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
019900090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
020000090209      * Nr. documento alfanumerico da decodificare
020100131001      * Nr. documento alfanumerico già decodificato
020200090209     D NDA             S              1    DIM(35)
020300090209     D NDN             S              1    DIM(15)
020400090209      * Messaggi di errore
020500090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
020600090209      * Tabella codici trattamento merce (1B)
020700090209     D CTM             S              2    DIM(200)
020800090209     D DTM             S             67    DIM(200)
020900090209      * Tabella codici valuta
021000090209     D CCV             S              3    DIM(200)
021100090209     D DCV             S             89    DIM(200)
021200090209      * Tabella codici nazioni
021300090209     D C15             S              3    DIM(500)
021400090209     D ISO             S              3    DIM(500)
021500090209     D CPF             S              2  0 DIM(500)
021600140307      *
021700090209      * Tabella partner
021800121105     D CPT             S             35    DIM(200)
021900121105     D CPTparz         S             35    DIM(%elem(CPT))
022000121105     D KSC_UNB         S              7    DIM(%elem(CPT))
022100121105     D DPT             S             90    DIM(%elem(CPT))
022200121105     D DPU             S             90    DIM(%elem(CPT))
022300121105     D DPV             S             90    DIM(%elem(CPT))
022400121105     D TRUL0SDS      E DS
022500090209      *
022600090209      * Tabella CL --> codici clienti - tariffa
022700130305     D CCL             S             35    DIM(999)
022800130305     D DCL             S             90    DIM(999)
022900130305     D KCL             S              7  0 DIM(999)
023000090209      * Termini di consegna
023100110328     D K35_TpBolla     S             35    DIM(100)
023200110328     D Cod_TpBolla     S              3    DIM(100)
023300110328     D Decod_Porto     S              1    DIM(100)
023400110328     d Trovata_Tab_TB  S              1
023500140307      *
023600140307      * Priorità trasporto
023700140307     D Key_TipServ     S             35    DIM(100)
023800140307     D TipoServizio    S              1    DIM(100)
023900140307      *
024000140307      * Decodifiche servizi speciali
024100140307     D SpecialServ     S             35    DIM(100)
024200140307     D Key_ServSpec    S              3    DIM(100)
024300140307     D UNI_ServSpec    S             90    DIM(100)
024400140307      *
024500110328      *
024600090216      * Tipo pagamento C/Assegno
024700090216     D CTC35           S             35    DIM(100)
024800090216     D TPI             S              2    DIM(100)
024900090209      * Decodifiche servizi speciali
025000090209     D SS35            S             35    DIM(100)
025100090209     D SS              S              3    DIM(100)
025200090209     D DSS             S             90    DIM(100)
025300090209      * Schiere x routine di conversione CAP
025400090209     D CAP5            S              1    DIM(5)
025500090209     D CAP9            S              1    DIM(9)
025600090209     D CAPM            S              1    DIM(9)
025700090209      * Schiera per memorizzazione FTX ricevuti
025800090209     D QUA             S              3    DIM(100)
025900090209     D FTX             S             70    DIM(100)
026000090209      * Tabelle capiconto
026100060608      **?------------------------------------------------------------------ */
026200050112      * Numeratore Bolle (302)
026300050112     D trul33ds      E DS
026400050112     D Ds3C          E DS
026500090209     C*
026600090209      **?------------------------------------------------------------------ */
026700090209     C* Definizione campi di work
026800090209     D pVLB            s                   LIKE(VABVLB)
026900090209     D kKUT            s                   LIKE(TBLKUT)
027000090209     D kCOD            s                   LIKE(TBLCOD)
027100090209     D kKCC            s                   LIKE(ACOKCC)
027200090209     D kKSC            s                   LIKE(ACOKSC)
027300090209     D kAAA            s                   LIKE(NUFAAA)
027400090209     D kCNU            s                   LIKE(NUFCNU)
027500090209     D kFIL            s                   LIKE(NUFFIL)
027600090209     D kAAS            s                   LIKE(VABAAS)
027700090209     D kLNP            s                   LIKE(VABLNP)
027800090209     D kNRS            s                   LIKE(VABNRS)
027900090209     D kNSP            s                   LIKE(VABNSP)
028000090209     D kCMR            s                   LIKE(VABCMR)
028100090209     D kCCM            s                   LIKE(VABCCM)
028200090209     D kNCD            s                   LIKE(VADNCD)
028300090210     D kCNT            s                   LIKE(VADCNT)
028400090209     D kNMC            s                   LIKE(RDENMC)
028500090209     D kNRP            s                   LIKE(RDENRP)
028600090209     D kLNP1           s                   LIKE(RDELNP)
028700090209      *
028800090209      *  Invio E-mail
028900090209     D Indirizzi       s            253
029000090209     D Oggetto         s             44
029100090209     D Messaggio       s           5000
029200090209      *
029300090209     d   DSmail        ds
029400090209     D Mail_Ind                      44
029500090209     d   IND_char                     1    dim(44)
029600090209      *
029700090209     D Lun_Ind         s              5u 0
029800090209      *
029900090209     D Dac             s              5u 0
030000090209     D Luc             s              5u 0
030100090209     C*---------------------------------------------------------------*
030200090209     D MsgDSP          S            256                                         Testo generico
030300090209     C*---------------------------------------------------------------*
030400090209     D SndMg01         S             10                                         Message type
030500090209     D                                     INZ('*INFO')
030600090209     D SndMg02         S             10                                         Deluvery mode
030700090209     D                                     INZ('*BREAK')
030800090209     D SndMg03         S            256                                         Message text
030900090209     D SndMg04         S             10I 0                                      Length of text
031000090209     D                                     INZ(%SIZE(SndMg03))
031100090209     D SndMg05         S             10                                         User profile
031200090209     D                                     DIM(299)
031300090209     D SndMg06         S             10I 0                                      Number of user
031400090209     D SndMg07         S             10I 0                                      Message sent indic.
031500090209     D SndMg08         S             10I 0                                      Function requested
031600090209     D SndMg10         S              1                                         Show display
031700090209     D                                     INZ('N')
031800090209     D SndMg11         S             20                                         Qualified MSGQ name
031900090209     D SndMg12         S              4                                         Name type indicator
032000090209     D                                     INZ('*USR')
032100090209      * indice di scihera
032200090209     D Jx              S              5I 0
032300110527     D Position        S              5I 0
032400110527     D daPos           S              5I 0
032500090209      *
032600090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
032700090209      *
032800131001     d bart_it         C                   CONST('@Brt.it;')
032900131001     d CED_Bart        C                   CONST('CED@Brt.it;')
033000060612      * ?================================================================== */
033100060612      * ?   * Campi x decodifica * (INPUT  del Record)
033200090130     D  Dati           s           2048
033300060612      * ?   *--------------------------------------------------------------*
033400060612     D  se_errore      s              1    inz(' ')
033500060612      * ?* ------------------------------------------------------ *
033600060710     D Digits          C                   '0123456789'
033700091019     D*------------------
033800091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
033900091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
034000110527     D*------------------
034100110527     D allChars        c                   const('QWERTYUIOPASDFGHJKLZXCVBNMqwe-  caratteri
034200110527     D                                     rtyuiopasdfghjklzxcvbnm0123456789/*--  accettati
034300110527     D                                     +\|!"£$%&/()=''?ì+*èéòçà°ù§-.:_,;<>[-
034400110527     D                                     ]@ ')
034500060612      * ?================================================================== */
034600060612      *   Ciclo principale
034700090205      * ?================================================================== */
034800060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
034900060612     C*
035000060612     C                   if        not %open(tivin00r)
035100060612     C                   open      tivin00r
035200060612     C                   endif
035300130930
035400130930     C                   EXSR      AZZMSG
035500130930
035600130930     c     *start        setll     Tivin00r
035700130930     c                   exsr      Legge_TIVIN
035800130930      *.
035900130930     c                   dow       not %eof(Tivin00r)
036000131001
036100130930      * solo i record sflaggati da rielaborare
036200130930      *  Sempre Record OK  poi  Controlli formali sui campi
036300130930     c                   IF        vinFLG = *blank and vinDTA <> *blank
036400130930      **
036500131001     C                   eval      vinFLG = '1'
036600131001     c                   clear                   se_errore
036700131001
036800130930      **  Normalizza il Segmento eliminando eventuali caratteri strani
036900130930      **   e decodifica di quale Segmento di tratta
037000130930      * ?              /*---------------------- */
037100130930     c                   exsr      Normaliz_Segm
037200130930      * ?              /*---------------------- */
037300130930      **
037400130930      *  per i SEGMENTI non utilizzati e NON presenti sulle MAPPATURE
037500130930      *   evitiamo di farli elaborare inutilmente
037600130930      *
037700130930     c                   if           tipo_seg <>'TMD'
037800130930     c                             and
037900130930     c                                tipo_seg <>'DIM'
038000131001     c                             and
038100131001      * ininfluenti ai fini della traduzione anche se MAPPATI
038200131001     c                                tipo_seg <>'UNA'
038300131001     c                             and
038400131001     c                                tipo_seg <>'CNT'
038500131001     c                             and
038600131001     c                                tipo_seg <>'LOC'
038700130930      *
038800130930      ** Decodifica record a campi variabili
038900130930      * ?              /*---------------------- */
039000130930     c                   exsr      Decod_Segmento
039100130930      * ?              /*---------------------- */
039200131001     c                   end
039300131001
039400131001      *** se non è riconosciuto l'UNB deve uscire IMMEDIATAMENTE
039500131001      *    e tutto il msg è dato x errato
039600131001     c                   if        se_errore ='S'  and
039700131001     c                             tipo_seg = 'UNB'
039800131001     c                   eval      err_bloccante = 'S'
039900131001     c                   exsr      Msg_errato
040000131001     c                   LEAVE
040100131001     c                   endIF
040200131001      **
040300130930     c                   endIF
040400131001      *.
040500131001     c                   exsr      AGGIOR_TIVIN
040600130930     c                   exsr      Legge_TIVIN
040700130930     C                   ENDdo
040800130930      **
040900060612     C                   if        %open(tivin00r)
041000060612     C                   close     tivin00r
041100060612     C                   endif
041200131001
041300131001      *  x errore bloccante nella composizione del VAB/VAT
041400131001     c                   if        err_bloccante ='S'
041500131001     c                   eval      esito  = '2'
041600131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON '
041700131001     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
041800131001     C                             Bolle AMAZON import in filiale - messaggio -
041900131001     C                             con UNB non presente in tabella PT controlla-
042000131001     C                             re EDI ricevuto su UPLOAD (AMAZON)'
042100131001     c                   exsr      invio_mail_AB
042200131001     c                   end
042300060612      *
042400060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
042500090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
042600090225     c                             esito ='0'
042700060710     C                   eval      esito ='0'
042800090225      *
042900090225     c                   COMMIT
043000090224      *
043100090213     c                   if        almeno_un_due ='S'
043200090213     C                   eval      esito ='1'
043300060614     C                   endif
043400090213     C                   endif
043500060614      *
043600060612     c                   SETON                                        LR
043700130930      * ?------------------------------------------------------------------ */
043800130930      *?     LEGGE  il   TIVIN
043900130930      * ?------------------------------------------------------------------ */
044000130930     c     Legge_TIVIN   Begsr
044100130930      *.
044200130930     c                   read      tivin00r
044300130930     c                   if        %Eof(tivin00r)
044400130930     c                   move      'S'           EoFTivin
044500130930     c                   end
044600130930      *.
044700130930     c                   endSR
044800130930      * ?================================================================== */
044900130930      *?     AGGIORNA    TIVIN solo se NON EoF
045000130930      * ?------------------------------------------------------------------ */
045100130930     c     AGGIOR_TIVIN  Begsr
045200130930      *.
045300130930     c                   if        EoFTivin = ' '
045400130930     c                   update    tivin000
045500130930     c                   end
045600130930      *.
045700130930     c                   endSR
045800130930      * ?------------------------------------------------------------------ */
045900130930      *?  Controlla i caratteri sul segmento x evitare caratteri strani
046000130930      *?  Elimina i caratteri che possono provocare problemi alla traduzione
046100130930      * ?------------------------------------------------------------------ */
046200130930     c     Normaliz_Segm Begsr
046300130930      **
046400130930      *  Caratteri non previsti
046500130930     c                   z-add     1             dapos
046600130930     c     Cerca         tag
046700130930     c     allChars      checK     VINdta:daPos  position
046800130930     C                   if        %Found
046900130930     c                   eval      %subst(VinDTA:position:1)=' '
047000130930     c                   eval      dapos = Position +1
047100130930     c                   goto      Cerca
047200130930     c                   endIf
047300130930      *
047400130930     c                   clear                   dati
047500130930     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
047600130930     c                   eval      tipo_seg = %subst(dati:1:3)
047700130930     c                   eval      segmento = dati
047800130930      *
047900131001      *
048000131001      *    Scarica il dettaglio precedente SU VAB E VAT
048100131001      *   e SALTA solo il Primo GIRO seguente la TESTATA
048200131001     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
048300131001     c                               or
048400131001     c                             tipo_seg = Fine_Ultimo_Dettaglio
048500131001      *
048600131001     c                   if        siamo_in_Dettaglio = 'S'
048700131001      *  PRIMA di resettare tutto x un nuovo dettaglio
048800131001      * ?              /*---------------------- */
048900131001     c                   exsr      Importa_SPED
049000131001      * ?              /*---------------------- */
049100131001     C                   else
049200131001      **   Serve per determinare che è finita la TESTATA
049300131001      **   ed inizia il Dettaglio
049400131001     c                   eval        siamo_in_Dettaglio ='S'
049500131001     c                   end
049600131001      *
049700131001     c                   end
049800131001      *
049900130930     c                   endSR
050000130930      * ?------------------------------------------------------------------ */
050100130930      *?    Decodifica il segmento ed importa i campi in formato DS
050200130930      * ?------------------------------------------------------------------ */
050300130930     c     Decod_SegmentoBegsr
050400110503      *
050500100120     c                   clear                   seg_imprevisto
050600100720     c                   clear                   trovato_seg
050700100716      *
050800100716     C                   clear                   keyUNBCLI
050900100716     C                   clear                   keyTIPOMSG
051000100716     C                   clear                   keyVERSION
051100100716     C                   clear                   keyRELEASE
051200100716     C                   clear                   keyAGENCY
051300100716     C                   clear                   keyASSOCIA
051400110503      *
051500100720      *  Chiamata generica x decodificare il singolo segmento letto
051600100720     c                   call      'TRTCT00R1'
051700100720      * input
051800100720     c                   parm                    tipo_seg
051900100720     c                   parm                    segmento
052000100720     C                   parm                    keyUNBCLI
052100100720     C                   parm                    keyTIPOMSG
052200100720     C                   parm                    keyVERSION
052300100720     C                   parm                    keyRELEASE
052400100720     C                   parm                    keyAGENCY
052500100720     C                   parm                    keyASSOCIA
052600100720      *output
052700100720     c                   parm                    Errore
052800100720     c                   parm                    DsGenerica
052900100720     c                   parm                    Valori_inErr
053000100720     c                   parm                    Descr_Errore
053100100720     c                   parm                    trovato_seg
053200100720      **--------
053300100720      *  Ritorna la DS GENERICA oppure l'errore
053400100120      **--------
053500100120     c                   select
053600130930      **--------
053700100720     c                   when      trovato_seg ='N'
053800100720     c                   move      'S'           seg_imprevisto
053900100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
054000100720     c                   exsr      Error_DET
054100131001     c                   LEAVEsr
054200130930      **--------
054300130930     c                   when      tipo_seg = 'UNA'
054400100720     c                   movel(p)  DsGenerica    EDS00UNA
054500100720      **--------
054600100120     c                   when      tipo_seg = 'UNB'
054700100720     c                   movel(p)  DsGenerica    EDS00UNB
054800100120     C                   EXSR      DATI_UNB
054900100120      * --------
055000100120     c                   when      tipo_seg = 'UNH'
055100100720     c                   movel(p)  DsGenerica    EDS00UNH
055200100120      * --------
055300100120     c                   when      tipo_seg = 'BGM'
055400100720     c                   movel(p)  DsGenerica    EDS00BGM
055500130930     C                   EXSR      DATI_BGM
055600130930      **--------
055700130930     c                   when      tipo_seg = 'CNT'
055800130930     c                   movel(p)  DsGenerica    EDS00CNT
055900130930      * --------
056000130930     c                   when      tipo_seg = 'CUX'
056100130930     c                   movel(p)  DsGenerica    EDS00CUX
056200130930     C                   EXSR      DATI_CUX
056300100120      **--------
056400100120     c                   when      tipo_seg = 'DTM'
056500100720     c                   movel(p)  DsGenerica    EDS00DTM
056600100120     C                   EXSR      DATI_DTM
056700130930      * --------
056800130930     c                   when      tipo_seg = 'LOC'
056900130930     c                   movel(p)  DsGenerica    EDS00LOC
057000130930      * --------
057100130930     c                   when      tipo_seg = 'TSR'
057200130930     c                   movel(p)  DsGenerica    EDS00TSR
057300140307     C                   EXSR      DATI_TSR
057400100120      **--------
057500100120     c                   when      tipo_seg = 'TOD'
057600100720     c                   movel(p)  DsGenerica    EDS00TOD
057700100120     C                   EXSR      DATI_TOD
057800100120      **--------
057900100120     c                   when      tipo_seg = 'RFF'
058000100720     c                   movel(p)  DsGenerica    EDS00RFF
058100100120     C                   EXSR      DATI_RFF
058200100120      **--------
058300100120     c                   when      tipo_seg = 'MOA'
058400100720     c                   movel(p)  DsGenerica    EDS00MOA
058500100120     C                   EXSR      DATI_MOA
058600100120      **--------
058700100120     c                   when      tipo_seg = 'FTX'
058800100720     c                   movel(p)  DsGenerica    EDS00FTX
058900100120     C                   EXSR      DATI_FTX
059000100120      **--------
059100100120     c                   when      tipo_seg = 'NAD'
059200100720     c                   movel(p)  DsGenerica    EDS00NAD
059300100120     C                   EXSR      DATI_NAD
059400100120      **--------
059500100120     c                   when      tipo_seg = 'CTA'
059600100720     c                   movel(p)  DsGenerica    EDS00CTA
059700100120     C                   EXSR      DATI_CTA
059800100120      **--------
059900100120     c                   when      tipo_seg = 'COM'
060000100720     c                   movel(p)  DsGenerica    EDS00COM
060100100120     C                   EXSR      DATI_COM
060200100120      **--------
060300100120     c                   when      tipo_seg = 'GID'
060400100720     c                   movel(p)  DsGenerica    EDS00GID
060500100120     C                   EXSR      DATI_GID
060600100120      **--------
060700100120     c                   when      tipo_seg = 'MEA'
060800100720     c                   movel(p)  DsGenerica    EDS00MEA
060900100120     C                   EXSR      DATI_MEA
061000100120      **--------
061100100120     c                   when      tipo_seg = 'PCI'
061200100720     c                   movel(p)  DsGenerica    EDS00PCI
061300100120      **--------
061400100120     c                   when      tipo_seg = 'UNT'
061500100720     c                   movel(p)  DsGenerica    EDS00UNT
061600100120     C                   EXSR      DATI_UNT
061700100120      **--------
061800100120     c                   when      tipo_seg = 'UNZ'
061900100720     c                   movel(p)  DsGenerica    EDS00UNZ
062000100120      **--------
062100100120     c                   OTHER
062200100120      **--------
062300100120     c                   move      'S'           seg_imprevisto
062400100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
062500100120     c                   exsr      Error_DET
062600131001     c                   LEAVEsr
062700100120      **--------
062800100120     c                   endSL
062900100120      **
063000131001     c                   endSR
063100090211     C*----------------------------------------------------------------
063200090211     C*? AZZMSG - Azzera dati messaggio
063300090211     C*----------------------------------------------------------------
063400090211     C     AZZMSG        BEGSR
063500090211     C*
063600090211     C* Pulisco dati di work x foglio viaggio
063700090211     C                   MOVEL     *BLANKS       WNUMFV           35
063800090211     C                   MOVEL     *BLANKS       WNRCMR           35
063900090211     C                   Z-ADD     0             WDATPA            8 0
064000090211     C                   Z-ADD     0             WDATFV            8 0
064100090211     C                   Z-ADD     0             WHHNFV            4 0
064200090211     C                   Z-ADD     0             WDTCMR            8 0
064300090211     C                   Z-ADD     0             WHHCMR            4 0
064400090211      *
064500090211      * pulisce gli RFF+FF di testata
064600090211     C                   clear                   $CLKSC            7 0
064700090211     C                   clear                   $CLTAR            3
064800090211     C                   clear                   $CLLNP            3 0
064900090211     C                   clear                   $CLNRS            2 0
065000090211     C                   clear                   $CLCTM            2
065100090211     C                   clear                   $CLBS1            1
065200090211     C                   clear                   $CLfnsp           1            *blk/S
065300090211      *
065400090211     C*  Inizializzo variabile new documento
065500090211     C                   Z-ADD     0             KCL
065600090211     C                   Z-ADD     0             WW                3 0
065700130930      *
065800090211     ** Inizializza campo di work
065900090211     c                   move      'N'           WGID99
066000130930      *
066100090211      * per controllare se almeno un record è stato importato sul VAB
066200090211     c                   clear                   Almeno_Uno        1
066300090213     c                   clear                   Almeno_Un_Due     1
066400090211     c                   eval      esito  = '0'
066500090211     C*
066600090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
066700090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
066800090603     c                   eval      Dettaglio_NAD_destinatario = *blank
066900090603     c                   eval      NAD_destinatario_in_testata = *blank
067000090603     c                   eval      NAD_mittente_in_testata = *blank
067100131001     c                   eval        siamo_in_Dettaglio = *blank
067200090603     C*
067300090603     c                   clear                   tes_VABrmo
067400090603     c                   clear                   tes_VABnmo
067500090603     c                   clear                   tes_VABcmo
067600090603     C*
067700090603     c                   clear                   tes_VABrsd
067800090603     c                   clear                   tes_VABrd2
067900090603     c                   clear                   tes_VABind
068000090603     c                   clear                   tes_VABlod
068100090603     c                   clear                   tes_VABnzd
068200090603     C*
068300090603     c                   clear                   tes_Valuta
068400131022     c                   clear                   BGM_Id_Testata
068500090603     C*
068600090211     C                   ENDSR
068700090210     C*----------------------------------------------------------------
068800090210      *? DECODIFICA DATI UNB
068900090210     C*----------------------------------------------------------------
069000090210     C     DATI_UNB      BEGSR
069100090210      *
069200090209      *  INIZIALIZZA  dati fondamentali messaggio
069300090209     C                   clear                   EDIDSPT
069400090209     C                   clear                   EDIDSPU
069500090209     C                   clear                   EDIDSPV
069600090209     C                   clear                   wnrDOC           35
069700090209     C                   move      'N'           PT_ok             1
069800090209      ** ---------------------------------------------------------------
069900090209      * quindi 1° cosa:
070000090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
070100090209      *     senza questo il messaggio NON è trascodificabile
070200090209     C                   MOVEL     unb0004       WCOD             35
070300090209     C                   MOVE      unb000720     WCOD
070400090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
070500090209     C                   Z-ADD     1             XX                3 0
070600090209     C     WCOD          LOOKUP    CPT(XX)                                32
070700090209      *
070800090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
070900090209     C                   If        *IN32 = *off
071000090209      * ******  Errore generale sul Messaggio
071100090209      * Msg di allerta Traduzione
071200090209     C                   EVAL      Indirizzi = CED_Bart
071300131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON'
071400090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
071500090209     C                             Bolle import in filiale - messaggio -
071600090209     C                             con UNB non presente in tabella PT controlla-
071700090209     C                             re EDI ricevuto su UPLOAD.'
071800090209      *
071900090209     c                   exsr      invio_mail
072000090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
072100090210     C                   eval      vinFLG = '2'
072200090210     C                   eval      se_errore   = 'S'
072300131001     c                   LEAVEsr
072400090209      *
072500090209     C                   move      'N'           PT_ok             1
072600090209      *
072700090209      *             *------------------------------*
072800090209     C                   eLSe
072900090209      *             *------------------------------*
073000090209      * Se tutto OK su tab.PT:
073100090209     C                   move      'S'           PT_ok             1
073200090209      *
073300090209     C                   MOVEL     DPT(XX)       EDIDSPT
073400090209     C                   MOVEL     DPU(XX)       EDIDSPU
073500090209     C                   MOVEL     DPV(XX)       EDIDSPV
073600091016     c                   clear                   Mittente         35
073700091016     c                   clear                   Mitt_Qualifier    4
073800091016     c                   clear                   Id_Messaggio     14
073900090226     C                   eval      Mittente       = unb0004
074000090226     C                   eval      Mitt_qualifier = unb000720
074100131001     C                   eval      Id_Messaggio   = UNB0020
074200090209      *
074300090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
074400090209      *  ricevuti:
074500090209     c                   movel     §PVema        mail_ind         44
074600090209      *
074700090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
074800090209     c                   if        mail_ind <> *blank
074900090209     c                   exsr      imp_ADDR_mail
075000090209     c                   end
075100090209      *
075200090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
075300090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
075400090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
075500090209     C                   if        §PULUN = *blank
075600090209     C                   eval      §PULUN = '00'
075700090209     c                   end
075800090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
075900090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
076000140307      *   Tipo Servizio
076100140307     C                   move      §puTS         PU_TipoServ
076200140307     C                   move      §puSS         PU_SpecServ
076300090209      *
076400090209     C                   Z-ADD     §PTKSC        WKSC              7 0
076500090211     C                   MOVEL     §PTLNP        VABlnp_PT
076600090212     C                   MOVEL     §PTNRS        VABnrs_PT
076700090211     C                   MOVEL     §PTCTM        VABctm_PT
076800090209      *
076900090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
077000090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
077100090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
077200090209      *  i records trasmessi).
077300090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
077400090209      *
077500090209      *  In base al cod.trattamento merce reperisco tipo tratt.
077600090209     C                   CLEAR                   DS1B
077700090209     C                   Z-ADD     1             Y                 3 0
077800090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
077900090209     C     *IN32         IFEQ      '1'
078000090209     C                   MOVEL     DTM(Y)        DS1B
078100090209     C                   MOVEL     DS1B          WSVTRM           38
078200131002     C                   MOVEL     DS1B          savDS1B
078300090209     C                   END
078400090209      *
078500090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
078600090209     C                   clear                   ACORAG
078700090209     C                   clear                   INDCAE
078800090209     C                   clear                   INDCAP
078900090209     C                   clear                   INDSTA
079000090209      * Decodifico partner
079100090209     C                   Z-ADD     1             KKUT
079200090209     C                   Z-ADD     KCI           KKCC
079300090209     C                   MOVE      §PTKSC        KKSC
079400090209     C     KACO          CHAIN     CNACO00F                           31
079500090209     C     KIND          CHAIN     CNIND00F                           31
079600090211      *
079700090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
079800090211      *   questa schiera è utilizzata anche per i clienti.
079900090211     C     §ptKSC        LOOKUP    KCL                                    39
080000090211     c                   If        *in39 = *off
080100090211     C                   add       1             WW
080200090211     C                   z-add     §ptKSC        KCL(WW)
080300090211     C                   Endif
080400090209      *
080500090209     C                   MOVEL     unb0017       WAA4              4 0
080600090209     C                   MOVE      WAA4          WAA2              2 0
080700090209     C                   MOVE      unb0017       WGG               2 0
080800090209     C                   MOVE      unb0017       WDTPRE            8 0
080900090209     C                   MOVE      unb0019       WHMPRE            4 0
081000090209     C                   MOVE      unb0017       WDTMSG            6 0
081100090209     C                   MOVE      WAA2          WDTMSG
081200090209     C                   MOVEL     WGG           WDTMSG
081300090210      * potremmo
081400090210      * Utilizzare l'identificativo della trasmissione come CMR
081500131001     C                   IF        unb0020 <> *blank
081600131001     C                   MOVEL     unb0020       WNRDOC
081700131001     C                   MOVEL     unb0020       WNUMFV
081800131001     C                   MOVEL     unb0020       WNRCMR
081900090209     c                   end
082000090209      *
082100090209     c                   Endif
082200131001      *
082300090210     c     end_UNB       endSR
082400130930      * ?================================================================== */
082500130930     C*? INIZIO Messaggio
082600130930      * ?================================================================== */
082700130930     C     DATI_BGM      BEGSR
082800130930      *
082900130930      *    Potrebbe essere x AMAZON il Nr.CMR ... per ora lasciamo al UNB
083000130930      *     decodificare il numero di CMR
083100131022      *
083200131022      *  identificativo testata  lo riporteremo x AMAZON
083300131022     c                   if        BGM1004 <> *blank
083400131022     c                   eval          BGM_Id_Testata = BGM1004
083500131022     c                   end
083600130930      **
083700130930     c     end_BGM       endSR
083800130930      * ?================================================================== */
083900130930     C*?   CUX   Currency in testata x eventuali importi sui Dettagli
084000130930      * ?================================================================== */
084100130930     C     DATI_CUX      BEGSR
084200130930      *
084300130930      * se presente la valuta la imposta dalla testata
084400130930     c                   if           CUX6345 <> *blank
084500130930     c                   eval      tes_Valuta = CUX6345
084600130930     c                   end
084700130930      **
084800130930     c     end_CUX       endSR
084900090601      * ?================================================================== */
085000090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
085100090601      * ?================================================================== */
085200090601     C     DATI_DTM      BEGSR
085300090601     C*
085400130930     C*  Se data elaborazione la uso e memorizzo x il CMR
085500130930     C     dtm2005       IFEQ      '9'
085600090209     C                   MOVEL     dtm2380       WDATA            35            Data
085700090209     C                   MOVEL     dtm2379       WFORM             3            Formato
085800090209     C                   EXSR      CNVDAT
085900090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
086000090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
086100090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
086200090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
086300090209     C     WERRO         IFEQ      'S'
086400090210     C                   eval      vinMSG = 'DTM data errata'
086500090213     c                   exsr      Error_DET
086600131001     c                   LEAVEsr
086700090209     C                   END
086800090209     C                   END
086900090209      **
087000090209     C*  Se data consegna richiesta imposto già sui campi del VAB
087100110523     C     dtm2005       IFEQ      '2'
087200090209     C                   MOVEL     dtm2380       WDATA            35            Data
087300090209     C                   MOVEL     dtm2379       WFORM             3            Formato
087400090209     C                   EXSR      CNVDAT
087500090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
087600090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
087700090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
087800090209     C     WERRO         IFEQ      'S'
087900090210     C                   eval      vinMSG = 'DTM data errata'
088000090213     c                   exsr      Error_DET
088100131001     c                   LEAVEsr
088200090209     C                   END
088300090209     C                   END
088400090211      **
088500090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
088600090603     C     dtm2005       IFEQ      '10'
088700090211     C                   MOVEL     dtm2380       WDATA            35            Data
088800090211     C                   MOVEL     dtm2379       WFORM             3            Formato
088900090211     C                   EXSR      CNVDAT
089000090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
089100090211     C     WERRO         IFEQ      'S'
089200090211     C                   eval      vinMSG = 'DTM data errata'
089300090213     c                   exsr      Error_DET
089400131001     c                   LEAVEsr
089500090211     C                   END
089600090211     C                   END
089700090209      **
089800131001     c                   endSR
089900140307      * ?================================================================== */
090000140307     C*?  Tipo Servizio
090100140307      * ?================================================================== */
090200140307     C     DATI_TSR      BEGSR
090300140307      ***
090400140307     C*  Reperisco priorità spedizione:
090500140307     C                   Z-ADD     1             X
090600140307      ***
090700140307      ***    Vecchio modo di dare la priority tramite un flag. in seguito
090800140307      ***     sostituito dal codice di Special Service.
090900140307      ***   (Però nello Standard c'è anche questa possibilità con la sua tabella)
091000140307     C     TSR4219       IFNE      *BLANKS
091100140307     C     TSR4219       ANDNE     *LOVAL
091200140307     c                   movel(p)  TSR4219       Work_Data__35
091300140307     c                   move      PU_TipoServ   Work_Data__35
091400140307     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
091500140307     C                   IF        not *IN32
091600140307      **  Il tipo bolla non è presente
091700140307     C                   eval      vinMSG = 'TSR non in tabella'
091800140307     c*****              exsr      Error_DET
091900140307     c*****              LEAVEsr
092000140307     C                   ELSE
092100140307     C                   eval       VABTSP = TipoServizio(X)
092200140307     C                   END
092300140307     C                   EndIF
092400140307     C*
092500140307     C*  Decodifico servizi speciali
092600140307     C     TSR7085       IFNE      *BLANKS
092700140307     C     TSR7085       ANDNE     *LOVAL
092800140307      *
092900140307     C                   Z-ADD     1             X
093000140307     c                   movel(p)  TSR7085       Work_Data__35
093100140307     c                   move      PU_SpecServ   Work_Data__35
093200140307     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
093300140307      *
093400140307     C                   IF        not *IN32
093500140307      **  Il tipo Serv  non è presente
093600140307     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
093700140307     c*****              exsr      Error_DET
093800140307     c*****              LEAVEsr
093900140307     c                   else
094000140307      *
094100140307     C                   eval       EDIDSSS = UNI_ServSpec(X)
094200140307      *
094300140307     C     §SSNOT        IFEQ      'S'
094400140307     c                   if        vabNOT = *blank
094500140307     C                   MOVEL     §SSDES        VABNOT
094600140307     c                   elseIF    vabNT2 = *blank
094700140307     C                   MOVEL     §SSDES        VABNT2
094800140307     c                   end
094900140307     C                   END
095000140307      *
095100140307      * x Espresso     "Pre-noon"    codice "PRN"
095200140307     C     §SSTPS        IFEQ      'S'
095300140307     C                   MOVEL     'E'           VABTSP
095400140307     C                   END
095500140307      *
095600140307      * x H 10:30      "H 10:30"     codice "H10"
095700140307     C     §SSTPS        IFEQ      'H'
095800140307     C                   MOVEL     'H'           VABTSP
095900140307     C                   END
096000140307      *
096100140307      * x Appuntamento "Booking ins" codice "BKI"
096200140307     c                   IF        §SSTPS ='A' and VABTC1 = *blank
096300140307     C                   MOVEL     'A'           VABTC1
096400140307     c                   elseIF    §SSTPS ='A' and VABTC2 = *blank
096500140307     C                   MOVEL     'A'           VABTC2
096600140307     C                   END
096700140307      *
096800140307      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
096900140307     C     §SSTPS        IFEQ      'F'
097000140307     C                   MOVEL     'S'           VABFFD
097100140307     C                   END
097200140307      *
097300140307     C                   END
097400140307      *
097500140307     C                   END
097600140307      **
097700140307     c     end_TSR       endSR
097800090209      * ?================================================================== */
097900090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
098000090209      * ?================================================================== */
098100090209     C     DATI_TOD      BEGSR
098200131001      *
098300131001      *  in AMAZON è usato solo in testata e non in dettaglio delle spedizioni
098400131001      *     NEL DETTAGGLIO si usa il FTX+AAR <--
098500131001      *
098600090210      *  Trascodifico tipo trasporto
098700090210     c                   clear                   TOD_char3         3
098800090210      *
098900090210      *   Imposta o il codice o il metodo
099000090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
099100131001     c                   if        tod4053  <> *blank
099200131001     c                   movel(p)  tod4053       TOD_char3
099300131001     c                   else
099400131001      *
099500131001      *-  Amazon utilizza --->  solo questo  <---
099600090210     c                   if        tod4215  <> *blank
099700090210     c                   movel(p)  tod4215       TOD_char3
099800090210     c                   end
099900131001      *
100000131001     c                   end
100100110328      ****************
100200110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
100300131001      *****    seguendo gli standard EDIFACT es.: PP (PrePaied) CC (Collect)
100400110328      ****************
100500110328     c                   eval      Trovata_Tab_TB ='N'
100600110328      *
100700110328     c                   if              TOD_char3 <> *blank
100800110328     c                   movel(p)  TOD_char3     Key_Generica35
100900110328     c                   move      §puTB         Key_Generica35
101000110328     C                   eval      Y = 1
101100110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
101200110328     c   32              eval      Trovata_Tab_TB ='S'
101300110328      *
101400131001      * PUO' APPARIRE STRANO fare in seguito questo altro LOOKUP:
101500131001      *   ma, per esperienza, meglio lasciarlo perchè
101600131001      *  Se non ha trovato con il campo 4053 è possibile che abbiano usato
101700110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
101800110328      *
101900110328     c                   if          Trovata_Tab_TB ='N'
102000110328     c                                 and
102100110328     C                               tod4215 <> *blank
102200110328     c                   movel(p)  tod4215       TOD_char3
102300110328     c                   movel(p)  TOD_char3     Key_Generica35
102400110328     c                   move      §puTB         Key_Generica35
102500110328     C                   eval      Y = 1
102600110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
102700110328     c   32              eval      Trovata_Tab_TB ='S'
102800110328     c                   end
102900131001      *
103000110328     c                   end
103100110328      *
103200110328      ****************
103300110328      ******* Se vi sono eccezioni sul cliente imposta quelle
103400110328      ****************
103500110328     c                   If          Trovata_Tab_TB ='S'
103600110328      *
103700110328     c                   SELECT
103800110328     c                   WHEN      Decod_Porto(Y) = 'F'
103900110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
104000110328      *  Porto Assegnato
104100110328     C                   OTHER
104200110328     C                   movel     '2'           VABCBO                         Senza C/Ass
104300110328     C                   ENDSL
104400110328      *
104500110328     c                   elseIf      Trovata_Tab_TB ='N'
104600090209      **
104700131001      **
104800110328     c                   endIF
104900110328      **
105000090210      **  Il tipo bolla non è presente
105100090210     c                   if        VABCBO = *blank
105200090210     C                   eval      vinMSG = 'TOD non rilevato'
105300090213     c                   exsr      Error_DET
105400131001     c                   LEAVEsr
105500090210     c                   end
105600090210      **
105700131001     c                   endSR
105800090210      * ?================================================================== */
105900090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
106000090210      * ?================================================================== */
106100090210     C     DATI_RFF      BEGSR
106200131001      *
106300131001      *  I riferimenti devono essere presi SOLO nei dettagli e non in TESTATA
106400131001     c                   if          siamo_in_Dettaglio = *blank
106500131001     c                   LEAVEsr
106600131001     c                   end
106700131001      *
106800131001      * ?i RIFERIMENTI x AMAZON sono MOLTO STRANI
106900131001      * ?e DEVONO ESSERE PRESI solo in DETTAGLIO.
107000131001      *
107100131001      * ?x AMAZON passano nel riferimento il SEGNACOLLO.
107200131001      * ?   invece di passarlo sul PCI come sarebbe da Standard dei messaggi IFTMIN
107300090210      *
107400090210     C                   clear                   WAGE             35
107500110224     C                   clear                   WFF              35
107600110224     C                   clear                   WCU              35
107700131001     C                   clear                   WBarcode         35
107800090210      *
107900090210      *  Trascodifico riferimento spedizione
108000091019     c                   if        RFF1153 = Riferimento_Spedizione  and
108100091019     c                             RFF1154 <> *BLANKS
108200090210     C                   movel(p)  RFF1154       WAGE             35
108300090210     c                   end
108400090210      **
108500090210     C                   IF         WAGE <> *BLANKS
108600090210     C                   movel     WAGE          WNSP             35
108700090210     C                   movel     WAGE          WNUM             35
108800090210     C     ' '           scan      WAGE          WLUNGH            3 0
108900090210     C                   sub       1             WLUNGH
109000090210     C                   EXSR      TSTNUM
109100090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
109200090210     C                   movel     WAGE          VABRMA
109300091019     C                   IF         WRMN = Riferimento_Spedizione  or
109400091019     C                              WRMN = *blanks
109500090210     C                   movel     WNUMOK        VABRMN
109600091019     C                   eval       WRMN = Riferimento_Spedizione
109700090210     C                   ENDIF
109800090210     C                   ENDIF
109900090210     C                   ENDIF
110000090210      *
110100131001     C*----------------------------------------------------------------
110200131002      * Riferimento TELEFONICO viene fatto con un RFF+TE anzichè il segmento COM
110300131002      *
110400131002     c                   if        RFF1153 = Riferimento_Telefonico  and
110500131001     C                             RFF1154 <> *blanks
110600131002     C                   movel     RFF1154       wattel
110700131001     C                   END
110800131002      *
110900131002      *--------------------------------------------------
111000140304      * il NUMERICO deve essere uguale al SEGNACOLLO
111100140304      * era così all'inizio; cambiato a Marzo 2014 per gestione del segnacollo
111200140304      * multiplo si è quindi deciso di ricevere il numerico come RFF+TB
111300140304      * ed invece tenere RFF+CR con righe in sequenza che contengono i segnacolli.
111400131002      *--------------------------------------------------
111500131002     c                   if        RFF1153 = Riferimento_Numerico    and
111600131002     C                             RFF1154 <> *blanks
111700131002     C                   movel     RFF1154       WCU
111800131002     c                   z-add     0             pos               3 0
111900131002     c                   eval      pos = %Check(digits:%trim(WCU))
112000131002     c                   if        pos > 0
112100131002      *  Va avanti però:
112200131002      *   deve segnalare la presenza di caratteri ALFA
112300131002     C                   eval      vinMSG = 'ATTENZ.:Caratteri ALFA -
112400131002     c                              sul RMN/BARCODE'
112500131002     c                   end
112600131002     C                   END
112700131002      *
112800131002      * Riferimento  solo NUMERICO
112900131002      *  quindi converte le lettere in numeri
113000131001     C     WCU           IFNE      *BLANKS
113100131001     C                   movel     WCU           WNUM             35
113200131001     C     ' '           scan      WCU           WLUNGH
113300131001     C                   sub       1             WLUNGH
113400131001     C                   EXSR      TSTNUM
113500131001     C                   If          WOKNUM = 'S'                               Ctrl numerico
113600131001     C                   movel     WNUMOK        VABRMN
113700131001     c                   eval      WRMN = Riferimento_Numerico
113800131001     C                   Endif
113900131001     C                   ENDIF
114000131001      *
114100131001     C*----------------------------------------------------------------
114200131001      * ? x AMAZON il SEGNACOLLO è passato come RIFERIMANTO
114300140304      * ?   da Marzo 2014 si passa da 1 fisso a Multicollo.
114400131001      *
114500131001     c                   if        RFF1153 = Riferimento_x_Segnacollo and
114600131001     C                             RFF1154 <> *blanks
114700131001     C                   movel     RFF1154       WBarcode
114800140304      *----- gestione Multicollo
114900140304      *-----   qui si calcola anche il Nr.Colli della spedizione invece che dal GID
115000140304     c                   exsr      SEGNACOLLO
115100140304      *-----
115200131001     C                   END
115300131001      *
115400131001     C*----------------------------------------------------------------
115500131001      *
115600131001      * ?NON ATTIVO x AMAZON ma lasciato il meccanismo se servisse in seguito
115700090210      * Codice Cliente di riferimento
115800091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
115900090210     C                   movel     RFF1154       WFF
116000090210     c                   END
116100090211      *
116200090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
116300090211     C     WFF           IFNE      *BLANKS
116400090211     C                   eval      XX = 1
116500090211     C                   movel     §PTKSC        WCCL             35
116600090211     C                   movel     WFF           WCOM28           28
116700090211     C                   move      WCOM28        WCCL
116800090211     C     WCCL          lookup    CCL(XX)                                34
116900090211     C     *IN34         IFEQ      '1'
117000090211     C                   movel     DCL(XX)       EDIDSCL
117100090211     C                   z-add     §CLKSC        WCLKSC            7 0
117200090211     C                   movel     §CLTAR        WCLTAR            3
117300090211     C                   z-add     §CLLNP        WCLLNP            3 0
117400090211     C                   z-add     §CLNRS        WCLNRS            2 0
117500090211     C                   movel     §CLCTM        WCLCTM            2
117600090211     C                   movel     §CLBS1        WCLBS1            1
117700090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
117800090211      * imposta la LNP   x il dettaglio specifco
117900090211     c                   eval      VABlnp = §CLlnp
118000090211      * imposta la Serie x il dettaglio specifco
118100090211     c                   eval      VABnrs = §CLnrs
118200090211      *
118300090211      *  In base al cod.trattamento merce reperisco tipo tratt.
118400090211      *    per un dettaglio specifico.
118500090211     C                   CLEAR                   DS1B
118600090211     C                   Z-ADD     1             Y                 3 0
118700090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
118800090211     C     *IN32         IFEQ      '1'
118900090211     C                   MOVEL     DTM(Y)        DS1B
119000090211     C                   END
119100090211      *
119200090211      * Se il cliente non era sulla schiera lo aggiunge
119300090211     C     §CLKSC        lookup    KCL                                    39
119400090211     C                   if        *in39 = *off
119500090211     C                   add       1             WW
119600090211     C                   z-add     §CLKSC        KCL(WW)
119700090211     c                   endif
119800090211      *
119900090211     C                   ENDIF
120000090211     C                   ENDIF
120100131001     C*----------------------------------------------------------------
120200131001      *
120300131001      * NOn c'è l'identificativo bolla del cliente
120400131001     c                   if        (RFF1153 = Riferimento_Spedizione and
120500131001     c                              RFF1154 = *BLANKS)
120600131001     c                             or
120700131001     c                             (RFF1153 = Riferimento_Numerico and
120800131001     c                              RFF1154 = *BLANKS)
120900131001     C                   eval      vinMSG = 'RFF riferimento assente'
121000131001     c                   exsr      Error_DET
121100131001     c                   LEAVEsr
121200131001     C                   ENDIF
121300131001      *
121400131001     c                   endSR
121500090209      * ?================================================================== */
121600090213     C*?  C.O.D. Monetary Amount
121700090209      * ?================================================================== */
121800090213     C     DATI_MOA      BEGSR
121900131001      *
122000131001      *    ATTENZIONE AL SEPARATORE DEi DECIMALI
122100131001      *
122200131001      *
122300090210      *
122400090216     C* Controllo se campo importo numerico
122500090216      * con 3 decimali
122600090216     C     moa5004       IFNE      *zeros
122700090216      *
122800131001      *  importo da sssicurare è tabella sulla PT
122900131001     c                   if        %subst(§PTxxx:1:1) = 'S'
123000131001     C     moa5025       IFEQ      '141'
123100131001     C     moa5004       div       100           VABIAS                         IMPORTO
123200130930     C                   MOVEL     tes_Valuta    VABVAS                         ASSICURATO
123300090216     C                   END
123400131001     c                   end
123500131001      *
123600131001      * NON è da importare il VALORE MERCE DICHIARATO
123700131001     C                   goto      Non_Fare                                     Valore
123800131001      *
123900131001     C     moa5025       IFEQ      '40 '                                        Valore
124000131001     C     moa5004       div       100           VABVMD                         MERCE
124100130930     C                   MOVEL     tes_Valuta    VABVAD                         DICHIARATO
124200090216     C                   END
124300131001     c     Non_Fare      tag
124400090216      *
124500131001      *  importo da C.O.D. è tabella sulla PT
124600090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
124700090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
124800131001     C     moa5004       div       100           VABCAS
124900130930     C                   MOVEL     tes_Valuta    VABVCA
125000090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
125100090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
125200090216     C     §PUTPC        IFEQ      'P'
125300090216     C     moa5004       ANDGT     *ZEROS
125400090216     C                   MOVEL     'OM'          VABTIC
125500090216     C                   MOVEL     'OM'          WVABTIC
125600090216     C                   ENDIF
125700090216     C                   END
125800090216     C                   END
125900090216      *
126000090216     C                   END                                                    Imp.non numer.
126100090213      *
126200090213     c     end_MOA       endSR
126300090213      * ?================================================================== */
126400090213     C*?  FREE TEXT
126500090213      * ?================================================================== */
126600090213     C     DATI_FTX      BEGSR
126700110404      *
126800131001      * ?x le Delivery Instruction  DIN a livello di TESTATA ????????????
126900131001      * ?   NON VI SONO NOTE IN DETTAGLIO x AMAZON e non verranno generate!!!!
127000130930     C                   IF        ftx4451 = 'DIN'                              -- 01 --
127100090216     C                   DO        4             X
127200090216     C     D05(X)        IFNE      *BLANKS
127300090216     C     WNOT1         IFEQ      *BLANKS
127400090216     C                   MOVEL     D05(X)        WNOT1
127500090216     C                   MOVE      D05(X)        WNOT2
127600090216     C                   ELSE
127700090216     C     WNOT2         IFEQ      *BLANKS
127800090216     C                   MOVEL     D05(X)        WNOT2
127900090216     C                   END
128000110404     C                   ENDif
128100110404     C                   ENDif
128200110404     C                   ENDdo
128300110404     C                   ENDif
128400131001      *
128500131001      * ?x AMAZON i TERMS of DELIVERIES  sul dettaglio sono sul
128600131001      * ?  segmento FTX nel primo segmento.
128700131001     C                   IF        ftx4451 = 'AAR' and D05(1) <> *blank         -- 01 --
128800131001      *
128900131001     C                   MOVEL     D05(1)        TOD_char3
129000131001     c                   movel(p)  TOD_char3     Key_Generica35
129100131001     c                   move      §puTB         Key_Generica35
129200131001     C                   eval      Y = 1
129300131001     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
129400131001     c   32              eval      Trovata_Tab_TB ='S'
129500131001      *
129600131001     c                   If          Trovata_Tab_TB ='S'
129700131001      *
129800131001     c                   SELECT
129900131001     c                   WHEN      Decod_Porto(Y) = 'F'
130000131001     c                               and  VABcas > *zeros
130100131001     C                   movel     '4'           VABCBO                         + C/Ass
130200131001     c                   WHEN      Decod_Porto(Y) = 'F'
130300131001     C                   movel     '1'           VABCBO                         Senza C/Ass.
130400131001      *  Porto Assegnato
130500131001     c                   WHEN      VABCAS > *zeros
130600131001     C                   movel     '6'           VABCBO                         + C/Ass
130700131001     C                   OTHER
130800131001     C                   movel     '2'           VABCBO                         Senza C/Ass
130900131001     C                   ENDSL
131000131001      *
131100131001     c                   elseIf      Trovata_Tab_TB ='N'
131200131001      **
131300131001      **  Il tipo bolla non è presente
131400131001     c                   if        VABCBO = *blank
131500131001     C                   eval      vinMSG = 'TOD non rilevato'
131600131001     c                   exsr      Error_DET
131700131001     c                   LEAVEsr
131800131001     c                   end
131900131001      **
132000131001     c                   endIF
132100131001      **
132200131001     C                   ENDif
132300131001      *_________________________________________________________________________*
132400131001      * ? AMAZON al momento NON fa i C.O.D. viene lasciato il meccanismo
132500131001      * ?    già impostato se un Domani si dovesse attivare C.O.D. e Tipo Incasso
132600131001      *_________________________________________________________________________*
132700131001      *
132800090216     C*------>>
132900131001     C*-    ->>    ATTENZIONE QUESTA é LA DECODIFICA STANDARD DEL TIPO INCASSO
133000131001     C*-    ->>       AMAZON  NON FA i COD.
133100131001     C*------>>
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
134500131001      * ?AMAZON NON FA I C.O.D.    quindi  è lasciata x scrupolo un domani
134600131001      * ?si dovesse attivare  -->  c'è il tipo Incasso da decodificare
134700090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
134800090216      *
134900090216     C                   DO        4             X                              -- 02 --
135000090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
135100130311      **
135200130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
135300130311      **  per gestire Partner come AZKAR che ha codice "AAA"
135400130311     C*******            MOVEL     D05(X)        WCTC              2
135500130311     C                   MOVEL     D05(X)        WCTC             10
135600090216     C                   Z-ADD     1             Y
135700090216     c                   movel(p)  Wctc          fld35a           35
135800090216     c                   move      §puTC         fld35a
135900090216     C     fld35a        LOOKUP    CTC35(Y)                               33
136000090216     C   33              MOVEL     TPI(Y)        VABTIC
136100090216     c   33              move      'S'           trovato_TPI
136200090216     C                   END                                                    -- 03 --
136300090216     C                   ENDdo                                                  -- 02 --
136400090216      *
136500090216     C                   ELSE                                                   -- 01 --
136600090216      *
136700090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
136800090216      *    Deve essere impostato comunque da testata
136900090216     c                   if        vabtic = *blank
137000090216     C                   MOVEL     wVabTIC       VABTIC
137100090216     c                   end
137200090216      *
137300090216     C                   END                                                    -- 01 --
137400090216      *
137500090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
137600090216      *    essere presenti altri record Flat con altre informazioni
137700090216      *    che potrebbero abblancare il Tipo incasso precedentemente
137800090216      *    decodificato
137900090216     c                   if        Trovato_TPI = 'S' and
138000090216     c                                 wri_vabtic = *blank
138100090216     c                   eval      wri_vabtic = 'S'
138200090216     c                   end
138300131001      *_________________________________________________________________________*
138400090213      *
138500090213     c     end_FTX       endSR
138600090213      * ?================================================================== */
138700090213     C*?  Anagrafica del mittente e del destinatario
138800090213      * ?================================================================== */
138900091016     C     DATI_NAD      BEGSR
139000090213      *
139100090210      * Dati destinatario
139200090210     C                   IF        nad3035 = 'CN' or
139300090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
139400090227      *
139500090227      * Se trovato un indirizzo di destinatario
139600090227     c                   eval       se_trovato_NAD ='S'
139700090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
139800090210      *
139900090210     C* Reperisco nazione corrispondente destinatario
140000090210     C                   Z-ADD     1             YY                3 0
140100090210     c                   eval      *in32 = *off
140200090210     C                   if        nad3207 <> *BLANKS
140300090210     C     nad3207       LOOKUP    ISO(YY)                                32
140400090210     C                   endif
140500090210      *
140600090210     C* Imposto dati destinatario
140700090211     C                   MOVEL     nad31241      VABRSD
140800090211     C                   if        nad31242 <> *LOVAL
140900090211     C                   MOVEL     nad31242      VABRD2
141000090210     C                   endif
141100131001????  * DAL PARTY NAME
141200091019???? C* Imposto dati destinatario
141300091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
141400131001???? C                   MOVEL     nad30361      VABRSD
141500131001???? C                   if        nad30362 <> *LOVAL
141600131001???? C                   MOVEL     nad30362      VABRD2
141700091019???? C                   endif
141800091019???? C                   end
141900131002      **
142000131002      **  Nel contatto inseriamo lo stesso nome del DESTINATARIO
142100131002     c                   movel     VABRSD        watrde
142200131002      **
142300131001     C                   MOVEL     nad3164       VABLOD
142400090211      *
142500090210     C                   MOVEL     nad30421      VABIND
142600131001      **
142700090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
142800090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
142900090210      *
143000131112      *  se Mandano un qualcosa d'altro nell'indirizzo sul (2) campo
143100131112      *   prima provo ad accodarlo nella 2°rag.sociale e se fosse già piena
143200131112      *     allora lo accoda ai dati dell'indirizzo fin dove può.
143300131112     c                   if        vabRD2 = *blank
143400131112???? C                   MOVEL     nad30422      VABRD2
143500131112     c                   else
143600131112     c                   eval      VABIND = %Trim(vabIND) + ' ' + NAD30422
143700131112     c                   end
143800131001      *
143900131001     C                   ENDif
144000131001      * ---------
144100090210     C                   If        *in32 = *on
144200090210     C                   MOVEL     C15(YY)       VABNZD
144300090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
144400090210     c                   Else
144500090210     C                   MOVEL     *BLANKS       VABNZD
144600090210     C                   Z-ADD     0             WFLCPF
144700090210     c                   Endif
144800090210      * Converto CAP
144900090210     C                   EXSR      CNVCAP
145000090603     C*
145100090603     C* Se in testata
145200131001     c                   if        siamo_in_Dettaglio = *blank
145300090603     c                   eval       tes_VABrsd = VABrsd
145400090603     c                   eval       tes_VABrd2 = VABrd2
145500090603     c                   eval       tes_VABind = VABind
145600090603     c                   eval       tes_VABlod = VABlod
145700090603     c                   eval       tes_VABnzd = VABnzd
145800090603     c                   eval      NAD_destinatario_in_testata = 'S'
145900090603     c                   end
146000090603     C*
146100090210     C                   ENDIF
146200131001      * __________________________________________________
146300131001      * Dati mittente  (Su AMAZON è AMAZON che spedisce)
146400131001      * __________________________________________________
146500131001     C                   IF        nad3035 = 'SF'
146600131001      *
146700131001      *   aggiungo eventuale rag.soc.aggiuntiva
146800131001     C                   movel     nad31241      VABRMO
146900131001     c                   eval      vabRMO = %Trim(VABRMO) + ' ' +
147000131001     C                                 %Trim(nad31242)
147100131001      ***
147200131001     c                   if        VABRMO =*blank and nad30361 <> *blank
147300131001     C                   movel     nad30361      VABRMO
147400131001     c                   eval      vabRMO = %Trim(VABRMO) + ' ' +
147500131001     C                                 %Trim(nad30362)
147600131001     c                   end
147700131001      *
147800131001      *   default PT
147900090210     C                   movel     §PTNAZ        VABNMO
148000131001      *
148100131001      *  Reperisco nazione mittente
148200090210     C                   IF        nad3207 <> *blanks
148300090210     C                   eval      YY = 1
148400090210     C     nad3207       LOOKUP    ISO(YY)                                32
148500090210     C                   if         *in32 = *on
148600090210     C                   MOVEL     C15(YY)       VABNMO
148700090210     C                   endif
148800090210     C                   ENDIF
148900090210      *
149000090210      * Normalizzo CAP mittente originale
149100090210     C                   clear                   T
149200090210     C                   clear                   S
149300090210     C                   MOVEA     *BLANKS       CAPM
149400090210     C                   MOVEA     nad3251       CAP9
149500090210     C                   DO        9             T
149600090210     C     CAP9(T)       IFNE      *BLANKS
149700090210     C                   ADD       1             S
149800090210     C                   MOVE      CAP9(T)       CAPM(S)
149900090210     C                   ENDIF
150000090210     C                   ENDDO
150100090210     C*
150200090210      *     Controllo validità CAP
150300090210     C                   MOVEA     CAPM          VABCMO
150400090210      *
150500090210     C                   CLEAR                   TISI95DS
150600090210     C                   MOVEL     VABCMO        I95CAP
150700090210     C                   MOVEL     VABNMO        I95NAR
150800090210     C                   MOVEL     WOGGI         I95DAT
150900090210     C                   MOVEL     '3'           I95TCN
151000090210     C                   CALL      'TISI95R'
151100090210     C                   PARM                    TISI95DS
151200090210      *     Errore
151300090210     C     O95ERR        IFNE      *BLANKS
151400090210     C                   MOVEL     INDCAE        VABCMO
151500090210     C                   MOVEL     §PTNAZ        VABNMO
151600090210     C                   END
151700090603     C* Se in testata
151800131001     c                   if        siamo_in_Dettaglio = *blank
151900131001     c                   eval        tes_VABrmo = VABrmo
152000131001     c                   eval        tes_VABnmo = VABnmo
152100131001     c                   eval        tes_VABcmo = VABcmo
152200131001     c                   eval        NAD_mittente_in_testata = 'S'
152300090603     c                   end
152400090210      *
152500090210     C                   END
152600090210      *
152700090210      * NOn c'è l'identificativo bolla del cliente
152800090210     C                   if          vabRSD = *blank or
152900090210     C                               vabIND = *blank or
153000090210     C                               vabLOD = *blank
153100090211     C                   IF        nad3035 = 'CN'
153200090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
153300090213     c                   exsr      Error_DET
153400131001     c                   LEAVEsr
153500090210     C                   ENDIF
153600090211     C                   ENDIF
153700090210      *
153800131001     c                   endSR
153900090210      * ?================================================================== */
154000090210     C*?  Contatto a cui far riferimento
154100090210      * ?================================================================== */
154200090210     C     DATI_CTA      BEGSR
154300090210      *
154400131001      * SEMBRA CHE AMAZON NON ABBIA il CONTATTO
154500090210      *
154600090210     c                   endSR
154700090210      * ?================================================================== */
154800090210     C*?  Contatto telefonico  a cui far riferimento
154900090210      * ?================================================================== */
155000090210     C     DATI_COM      BEGSR
155100131001      *
155200131001      * SEMBRA CHE AMAZON NON ABBIA la COMUNICAZIONE
155300131002      *          ma lo gestisce tramite un RFF quindi decodificato fra _RFF
155400090210      *
155500090209     C                   ENDSR
155600090210      * ?================================================================== */
155700090210     C*?  Dati di dettaglio della spedizione e dei colli
155800090210      * ?================================================================== */
155900090210     C     DATI_GID      BEGSR
156000131001      *
156100131001      * ?  /*----------------------------------------------------------------*/
156200131001      * ?   PER AMAZON QUESTO È IL PRIMO RECORD DI DETTAGLIO DI UNA SPEDIZIONE
156300131001      * ?  /*----------------------------------------------------------------*/
156400130930      *
156500131001      *  Prepara x una nuova spedizione
156600130930      * ?                         --------------
156700130930     c                   exsr      Nuova_spediz
156800130930      * ?                         --------------
156900140304      *
157000140304     C***>>   da Marzo 2014  si gestisce il MULTICOLLO che viene calcolato
157100140304     C***>>   in funzione dei colli presenti sui segmenti RFF+CR
157200140304     C***>>   quindi NON si esegue più nessuna specifica di questa parte di Routine
157300140304     C***>>   e si calcola dinamicamente il totale colli da mettere sul VABNCL in
157400140304     C***>>   seguito.
157500090210      *
157600090210     C* Se ho totali per spedizione metto in campi VAB
157700140304     C     gid1496       IFEQ      99999
157800140304     C     gid1496       orEQ      9999
157900090210     C                   MOVEL     'S'           WGID99
158000140304      *
158100131016     C********   SI VUOLE CHIODATO (1 SOLO COLLO)
158200140304     C***>>         da marzo NON più poichè si passa a spediz. multicollo
158300140304     C***>>              z-add     1             VABNCL
158400131016     C***********        z-add     gid722420     VABNCL
158500140304      *
158600090210     C                   ELSE
158700140304      *
158800090210     C     WGID99        IFEQ      'N'
158900131016     C********   SI VUOLE CHIODATO (1 SOLO COLLO)
159000140304     C***>>         da marzo NON più poichè si passa a spediz. multicollo
159100140304     C***>>              z-add     1             WTNCL             5 0
159200131016     C***********        z-add     gid722420     WTNCL             5 0
159300140304     C***>>              ADD       WTNCL         VABNCL
159400090210     C                   END
159500140304      *
159600090210     C                   END
159700140304      *
159800140304      *  da marzo 2014 VABNCL calcolato alla fine del singolo dettaglio
159900140304      *   NON PRESO DAL segmento GID.
160000140304     C***>>              ADD       VABNCL        WTOCOC           16 0
160100090210      *
160200090210     c     end_GID       endSR
160300130930      * ?================================================================== */
160400130930     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
160500130930      * ?================================================================== */
160600130930     C     Nuova_spediz  BEGSR
160700130930      **
160800130930      * Pulisco campi del file
160900130930     c                   z-add     vnrec         SAVNUM_RECord
161000130930     C                   CLEAR                   EDiVAB00
161100130930     C                   CLEAR                   EDiVAT00
161200130930     C                   CLEAR                   EDiVAD00
161300130930     C                   MOVEL     'N'           WGID99            1
161400130930     C                   clear                   XY                5 0
161500130930     C                   clear                   §§                5 0
161600130930     C                   clear                   WS1496            5 0
161700130930     C                   clear                   WNSP             35
161800130930     C                   Z-ADD     *ZEROS        PVLB
161900130930     C                   MOVEA     *HIVAL        SGN
162000130930     C*  Azzero codice cliente - tariffa
162100130930     C                   Z-ADD     0             WTOCOC           16 0
162200130930     C                   Z-ADD     0             WTOPLC           16 2          ???
162300130930     C                   Z-ADD     0             WTOPNC           16 2          ???
162400130930     C                   Z-ADD     0             Work_KG          16 2          ???
162500130930     C*  Azzero codice cliente - tariffa
162600130930     C                   clear                   WCLKSC
162700130930     C                   clear                   WCLTAR
162800130930     C                   clear                   WCLLNP
162900130930     C                   clear                   WCLNRS
163000130930     C                   clear                   WCLCTM
163100130930     C                   clear                   WCLBS1
163200130930     C                   clear                   WCLfnsp                        *blk/S
163300130930     C                   clear                   WNOT1            35
163400130930     C                   clear                   WNOT2            35
163500130930     C*  Campi di work
163600130930     c                   clear                   wVabTIC
163700130930     c                   clear                   wri_vabtic        1
163800130930      *
163900130930     C* pulisce campi di work
164000130930     c                   clear                   watrde           35
164100130930     c                   clear                   wattel           25
164200130930     C                   clear                   WRMN              3
164300130930      *
164400130930      * Un errore nel dettaglio
164500130930     C                   clear                   Err_in_detta      1
164600130930     c                   clear                   errori_in_Wrt     1
164700130930     c                   clear                   se_trovato_NAD    1
164800131002     C                   eval      DS1B = savDS1B
164900131001      **
165000131001      ** Riferimento del cliente
165100131001     C                   MOVEL     unh0062       WPRGSP           35
165200131001     C                   MOVEL     unh0062       WNSP             35
165300131001     C                   MOVEL     unh0062       WNUM             35
165400131001      * ?                                       --------
165500131001     C                   MOVEL     unh0062       VABRMA
165600131001      * ?                                       --------
165700131001     C     ' '           SCAN      unh0062       WLUNGH            3 0
165800131001     C                   SUB       1             WLUNGH
165900131001     C                   EXSR      TSTNUM
166000131001     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
166100131001      * ?                                       --------
166200131001     C                   MOVEL     WNUMOK        VABRMN
166300131001      * ?                                       --------
166400131001     C                   END
166500131001      **
166600131001      **    imposta dalla PT i valori di default presenti in Tabella
166700131001     C                   MOVEL     VABlnp_PT     VABLNP
166800131001     C                   MOVEL     VABnrs_PT     VABNRS
166900131001     C                   MOVEL     VABctm_PT     VABCTM
167000130930      **
167100130930     c                   endSR
167200090210      * ?================================================================== */
167300090210     C*?  Dati di dettaglio misure spedizione e colli
167400090210      * ?================================================================== */
167500090210     C     DATI_MEA      BEGSR
167600090210      *
167700131001      *    attenzione  ai DECIMALI
167800131001      *
167900090210     C*  Peso
168000131002     C     mea6311       IFEQ      Peso_Tassabile
168100131002     C     mea6411       ANDEQ     in_KG
168200090210      *
168300090210     C*  Controllo se ho valore totale o parziale
168400090210      *   sempre il Lordo
168500131002???? C                   IF        mea6313 = Tipo_Peso_Tassabile
168600090210     C     WS1496        IFEQ      99999
168700090210     C     WS1496        orEQ      9999
168800091019???? C                   move      mea6314       mea6314_2        18 2          Peso
168900091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
169000131001???? C                   ADD       mea6314_2     WTOPLC
169100090210     C                   ELSE
169200091019???? C     WGID99        IFEQ      'N'
169300091019???? C                   move      mea6314       mea6314_2                      Peso
169400110718???? C                   ADD       mea6314_2     Work_KG                        Peso
169500110718     c                   eval         vabPKB = Work_KG
169600131001???? C                   ADD       mea6314_2     WTOPLC
169700090210     C                   END
169800090210     C                   END
169900090210     C                   END
170000090210      *
170100131001     C                   ENDif
170200131001      *
170300090210     C* Ricerco CAP
170400090210     C     WCAP          IFNE      *BLANKS
170500131001      *
170600131002     C     mea6311       IFEQ      Peso_Tassabile
170700090210      *
170800090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
170900090210     C                   CLEAR                   FNLV55DS
171000090210     C                   MOVEL     'P'           D55TPT
171100090210     C                   MOVEL     WOGGI         D55DRF
171200090210     C                   MOVEL     VABLNP        D55LNP
171300090210     C                   MOVEL     VABLNP        D55LIN
171400090210     C                   CALL      'FNLV55R'
171500090210     C                   PARM                    FNLV55DS
171600090210     C                   MOVE      D55TFP        COMTFP            3 0
171700090210      *
171800090210     C*  PASSO IL TERMINAL PARTENZA
171900090210     C                   CLEAR                   TISI95DS
172000090210     C                   Z-ADD     COMTFP        I95TFP
172100090210     C                   MOVEL     VABTSP        I95TSP
172200090210     C                   MOVEL     'S'           I95FRE
172300090210     C                   MOVEL     '3'           I95TCN
172400090210     C                   MOVEL     WCAP          I95CAP
172500090210     C                   MOVEL     VABNZD        I95NAR
172600090210     C                   MOVEL     VABLOD        I95LOC
172700090210     C                   MOVEL     VABIND        I95IND
172800090210      *
172900090210     C* Se gestita seconda linea di arrivo passo peso - volume
173000090210     C* e numero colli effettivo
173100090210     C     §PULNA        IFEQ      'S'
173200090210     C                   Z-ADD     VABPKB        I95LKG
173300090210     C                   Z-ADD     VABVLB        I95LMC
173400090210     C                   END
173500090210     C*
173600090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
173700090210     C                   SELECT
173800090210     C     VABCBO        WHENEQ    '1 '
173900090210     C                   MOVEL     *BLANKS       I95FCA
174000090210     C                   MOVEL     'F'           I95TPO
174100090210     C     VABCBO        WHENEQ    '2 '
174200090210     C                   MOVEL     *BLANKS       I95FCA
174300090210     C                   MOVEL     'A'           I95TPO
174400090210     C     VABCBO        WHENEQ    '4 '
174500090210     C                   MOVEL     'S'           I95FCA
174600090210     C                   MOVEL     'F'           I95TPO
174700090210     C     VABCBO        WHENEQ    '6 '
174800090210     C                   MOVEL     'S'           I95FCA
174900090210     C                   MOVEL     'A'           I95TPO
175000090210     C                   OTHER
175100090210     C                   MOVEL     'F'           I95TPO
175200090210     C     VABCAS        IFGT      0
175300090210     C                   MOVEL     'S'           I95FCA
175400090210     C                   ELSE
175500090210     C                   MOVEL     *BLANKS       I95FCA
175600090210     C                   END
175700090210     C                   ENDSL
175800090210     C*
175900090210     C                   CALL      'TISI95R'
176000090210     C                   PARM                    TISI95DS
176100131001      **
176200090210     C* Reperisco i dati da CAP
176300090210     C                   MOVEL     O95PRV        VABPRD
176400131002     C*******            MOVEL     O95LNA        VABLNA
176500131002     C********           MOVEL     O95ZNC        VABZNC
176600131002     C********
176700131002     C********  NON SI DEVONO CARICARE MAI ci pensa la BOLLETTAZIONE
176800131002     C                   CLEAR                   VABLNA
176900131002     C                   CLEAR                   VABZNC
177000131002     C********
177100090210     C                   MOVEL     WCAP          VABCAD
177200090210     C*
177300090210     C     O95ERR        IFNE      *BLANKS
177400090210     C     O95FIT        ANDEQ     'S'
177500090210     C*  Cap non trovato
177600090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
177700090213     c                   exsr      Error_DET
177800131001     c                   LEAVEsr
177900090210     C                   END
178000131001      *
178100090210     C                   END
178200090210      *
178300090210     C                   ELSE
178400090210      *
178500090210     C                   CLEAR                   VABPRD
178600090210     C                   CLEAR                   VABLNA
178700090210     C                   CLEAR                   VABZNC
178800090210     C                   CLEAR                   VABCAD
178900090210     C*  Cap non trovato
179000090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
179100090213     c                   exsr      Error_DET
179200131001     c                   LEAVEsr
179300131001      *
179400090210     C                   END
179500090210      *
179600131001     c                   endSR
179700090211     C*----------------------------------------------------------------
179800090211      *? dati finali del dettaglio    UNT
179900090211     C*----------------------------------------------------------------
180000090211     C     DATI_UNT      BEGSR
180100090211      *
180200090213     c                   eval      NUM_RECord = vnREC
180300090213     c                   z-add     vnrec         last_RECord
180400090211      **
180500090211     c                   endSR
180600140304      *===============================================================*
180700140304      *  Nel RFF+CR c'è il Segnacollo x AMAZON
180800140304      *===============================================================*
180900140304     C     SEGNACollo    BEGSR
181000140304      *
181100140304      *-----
181200140304      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
181300140304      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
181400140304     C     §1BF12        IFEQ      'E'
181500140304     C                   EXSR      SEGNEE
181600140304     C                   ELSE
181700140304      *
181800140304      *  La prima volta controllo congruità numero segnacollo
181900140304     C     §PUBS1        IFNE      *BLANKS
182000140304     C     VABnrs        ANDNE     0
182100140304      *  Se il codice trattamento merce prevede che segnacollino i
182200140304      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
182300140304     C     §1BFG7        IFEQ      'S'
182400140304     C     §1BFG1        OREQ      'S'
182500140304      *  calcolo segnacollo
182600140304     C                   EXSR      SGNELA
182700140304     C                   END
182800140304     C                   END
182900140304      *
183000140304     C                   END
183100140304      *-----
183200140304     C                   ENDSR
183300090210      *----------------------------------------------------------------
183400090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
183500090210      *----------------------------------------------------------------
183600090210     C     SEGNEE        BEGSR
183700131001      *
183800131001      * ?  /*---------------------------------------------------*/
183900140304      * ?   PER AMAZON un SOLO segnacollo sul RFF+CR:xxxxxxxxxxx'
184000140304      * ?    ed era 1 SOLO per SPEDIZIONE .
184100140304      * ? Da marzo 2014 invece SPEDIZIONI multicollo.
184200131001      * ?  /*---------------------------------------------------*/
184300090210      *
184400090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
184500090210     C                   ADD       1             §§
184600140304     C                   MOVEL     Wbarcode      SGE(§§)
184700140304     c                   z-add     §§            VABNCL
184800140304     C                   z-add     VABNCL        WTOCOC
184900131002      *
185000131002     C                   ENDSR
185100090210     C*----------------------------------------------------------------
185200090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
185300090210     C*----------------------------------------------------------------
185400090210     C     SGNELA        BEGSR
185500131001      *
185600131001      * ?  /*---------------------------------------------------*/
185700131001      * ?   PER AMAZON un SOLO segnacollo sul RFF+CR:xxxxxxxxxxx'
185800131001      * ?    ed è 1 SOLO per SPEDIZIONE .
185900140304      * ? Da marzo 2014 invece SPEDIZIONI multicollo.
186000140304      * ANCHE SE NON sono segnacolli BRT adeguo le specifiche seguenti
186100131001      * ?  /*---------------------------------------------------*/
186200090210     C*
186300090210     C                   CLEAR                   DSSGN
186400140304     C*
186500090210     C*  Controllo se devo ricevere la linea di partenza
186600090210     C                   MOVEL     'N'           WERRO             1
186700090210     C     §PULP1        IFGT      0                                            --- 03 ---
186800090210     C                   EXSR      REPLNP
186900090210     C                   END                                                    --- 03 ---
187000140304     C*
187100090210     C*  Controllo se devo ricevere la linea di arrivo
187200090210     C     §PULA1        IFGT      0                                            --- 03 ---
187300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
187400090210     C                   EXSR      REPLNA
187500090210     C                   END                                                    --- 03 ---
187600140304     C*
187700090210     C*  Controllo se devo ricevere il numero di serie
187800090210     C     §PUNR1        IFGT      0                                            --- 03 ---
187900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
188000090210     C                   EXSR      REPNRS
188100090210     C                   END                                                    --- 03 ---
188200140304     C*
188300090210     C*  Controllo se devo ricevere il numero segnacollo
188400090210     C     §PUSG1        IFGT      0                                            --- 03 ---
188500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
188600090210     C                   EXSR      REPSGN
188700090210     C                   END                                                    --- 03 ---
188800140304     C*
188900090210     C*  Controllo se devo ricevere la zona di consegna
189000090210     C     §PUZN1        IFGT      0                                            --- 03 ---
189100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
189200090210     C                   EXSR      REPZNC
189300090210     C                   END                                                    --- 03 ---
189400140304     C*
189500090210     C*  Imposto da VAB i dati a zero se mi è stato passato
189600090210     C*  numero segnacollo valido
189700090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
189800140304     C*
189900090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
190000090210     C                   Z-ADD     VABLNP        SGNLNP
190100090210     C                   END                                                    --- 04 ---
190200090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
190300090210     C                   Z-ADD     VABLNA        SGNLNA
190400090210     C                   END                                                    --- 04 ---
190500090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
190600090210     C                   Z-ADD     VABNRS        SGNNRS
190700090210     C                   END                                                    --- 04 ---
190800090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
190900090210     C                   Z-ADD     VABZNC        SGNZNC
191000090210     C                   END                                                    --- 04 ---
191100090210      *
191200090210     C*  mi reimposto LNA e zona da segnacollo
191300090210     C                   Z-ADD     SGNLNA        VABLNA
191400090210     C                   Z-ADD     SGNZNC        VABZNC
191500140304      *
191600090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
191700090210     C                   ADD       1             XY
191800090210     C                   MOVE      SGNNSC        SGN(XY)
191900140304     c                   z-add     XY            VABNCL
192000140304     C                   z-add     VABNCL        WTOCOC
192100090210     C                   END                                                    --- 03 ---
192200090210      *
192300090210     C                   ENDSR
192400090210     C*----------------------------------------------------------------
192500090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
192600090210     C*----------------------------------------------------------------
192700090210     C     REPLNP        BEGSR
192800090210     C*
192900090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
193000090210     C*  (a partire dalla posizione iniziale indicata in tabella)
193100090210     C*  dal numero segnacollo passatomi dal partner
193200090210     C                   Z-ADD     §PULP1        WP                2 0
193300140304     C     3             SUBST     Wbarcode:WP   WLNP              3
193400090210     C*
193500090210     C*  controllo che la lnp calcolata sia numerica
193600090210     C                   TESTN                   WLNP                 98
193700090210     C*
193800090210     C*  se è numerica la carico
193900090210     C     *IN98         IFEQ      '1'
194000090210     C                   MOVE      WLNP          SGNLNP
194100090210     C                   ELSE
194200090210     C*  se non è numerica stampo errore
194300090210     C                   MOVEL     'S'           WERRO             1
194400090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
194500090213     c                   exsr      Error_DET
194600131001     c                   LEAVEsr
194700090210     C                   END
194800090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
194900090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
195000090210     C     SGNLNP        IFNE      VABLNP
195100090210     C                   MOVEL     'S'           WERRO             1
195200090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
195300090210     c                             a della Bolla'
195400090213     c                   exsr      Error_DET
195500131001     c                   LEAVEsr
195600090210     C                   END
195700090210     C*
195800131001     C                   ENDSR
195900131001     C*----------------------------------------------------------------
196000090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
196100090210     C*----------------------------------------------------------------
196200090210     C     REPLNA        BEGSR
196300090210     C*
196400090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
196500090210     C*  (a partire dalla posizione iniziale indicata in tabella)
196600090210     C*  dal numero segnacollo passatomi dal partner
196700090210     C                   Z-ADD     §PULA1        WP                2 0
196800140304     C     3             SUBST     Wbarcode:WP   WLNA              3
196900090210     C*  controllo che la lna calcolata sia numerica
197000090210     C                   TESTN                   WLNA                 98
197100090210     C*  se è numerica la carico
197200090210     C     *IN98         IFEQ      '1'
197300090210     C                   MOVE      WLNA          SGNLNA
197400090210     C                   ELSE
197500090210     C*  se non è numerica stampo errore
197600090210     C                   MOVEL     'S'           WERRO             1
197700090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
197800090213     c                   exsr      Error_DET
197900131001     C                   LEAVEsr
198000090210     C                   END
198100090210     C*
198200131001     C                   ENDSR
198300090210     C*----------------------------------------------------------------
198400090210     C*? REPNRS - Reperisce numero serie
198500090210     C*----------------------------------------------------------------
198600090210     C     REPNRS        BEGSR
198700090210     C*
198800090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
198900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
199000090210     C*  dal numero segnacollo passatomi dal partner
199100090210     C                   Z-ADD     §PUNR1        WP                2 0
199200140304     C     2             SUBST     Wbarcode:WP   WNRS              2
199300090210     C*  controllo che il nrs calcolata sia numerico
199400090210     C                   TESTN                   WNRS                 98
199500090210     C*  se è numerica la carico
199600090210     C     *IN98         IFEQ      '1'
199700090210     C                   MOVE      WNRS          SGNNRS
199800090210     C                   ELSE
199900090210     C*  se non è numerico stampo errore
200000090210     C                   MOVEL     'S'           WERRO             1
200100090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
200200090213     c                   exsr      Error_DET
200300131001     C                   LEAVEsr
200400090210     C                   END
200500090210      *
200600090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
200700090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
200800090210     C     SGNNRS        IFNE      VABNRS
200900090210     C                   MOVEL     'S'           WERRO             1
201000090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
201100090210     c                             o della Bolla'
201200090213     c                   exsr      Error_DET
201300131001     C                   LEAVEsr
201400090210     C                   END
201500090210     C*
201600131001     C                   ENDSR
201700090210     C*----------------------------------------------------------------
201800090210     C*? REPZNC - Reperisce zona consegna
201900090210     C*----------------------------------------------------------------
202000090210     C     REPZNC        BEGSR
202100090210     C*
202200090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
202300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
202400090210     C*  dal numero segnacollo passatomi dal partner
202500090210     C                   Z-ADD     §PUZN1        WP                2 0
202600140304     C     2             SUBST     Wbarcode:WP   WZNC              2
202700090210     C*  controllo che la zona calcolata sia numerica
202800090210     C                   TESTN                   WZNC                 98
202900090210     C*  se è numerica la carico
203000090210     C     *IN98         IFEQ      '1'
203100090210     C                   MOVE      WZNC          SGNZNC
203200090210     C                   ELSE
203300090210     C*  se non è numerica stampo errore
203400090210     C                   MOVEL     'S'           WERRO             1
203500090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
203600090213     c                   exsr      Error_DET
203700131001     C                   LEAVEsr
203800090210     C                   END
203900090210     C*
204000131001     C                   ENDSR
204100090210     C*----------------------------------------------------------------
204200090210     C*? REPSGN - Reperisce numero segnacollo
204300090210     C*----------------------------------------------------------------
204400090210     C     REPSGN        BEGSR
204500090210     C*
204600090210     C*  se viene passata nr.segnacollo estraggo sottostringa
204700090210     C*  lunga 7 chr (a partire dalla posizione iniziale
204800090210     C*  indicata in tabella)
204900090210     C*  dal numero segnacollo passatomi dal partner
205000090210     C                   Z-ADD     §PUSG1        WP                2 0
205100140304     C     7             SUBST     Wbarcode:WP   WSGN              7
205200090210     C*  controllo che il nr.segnacollo calcolato sia numerico
205300090210     C                   TESTN                   WSGN                 98
205400090210     C*  se è numerico lo carico
205500090210     C     *IN98         IFEQ      '1'
205600090210     C                   MOVE      WSGN          SGNNSC
205700090210     C                   ELSE
205800090210     C*  se non è numerica stampo errore
205900090210     C                   MOVEL     'S'           WERRO             1
206000090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
206100090213     c                   exsr      Error_DET
206200131001     C                   LEAVEsr
206300090210     C                   END
206400090210     C*
206500131001     C                   ENDSR
206600051205      *----------------------------------------------------*
206700051205      *   DEFINIZIONE CHIAVI                               *
206800051205      *----------------------------------------------------*
206900051205     C     *INZSR        BEGSR
207000051205      *
207100991129     c     *ENTRY        PLIST
207200060613     C                   parm                    esito             1
207300971215      *
207400050112     C     KTABel        KLIST
207500050112     C                   KFLD                    TBLKUT
207600050112     C                   KFLD                    TBLCOD
207700050112     C                   KFLD                    TBLKEY
207800090210      *
207900090210     C     K_TAB1L       KLIST
208000090213     C                   KFLD                    etbUNB
208100090213     C                   KFLD                    etbSGM
208200090213     C                   KFLD                    etbVALSGM
208300090209     C*
208400090209     C* Definisco chiavi di accesso
208500090209     C     KTAB1         KLIST
208600090209     C                   KFLD                    KKUT
208700090209     C                   KFLD                    KCOD
208800090209     C*
208900090209     C     KNUF          KLIST
209000090209     C                   KFLD                    KAAA
209100090209     C                   KFLD                    KCNU
209200090209     C                   KFLD                    KFIL
209300090209      *
209400090209     C     KVAB1         KLIST
209500090209     C                   KFLD                    KCCM
209600090209     C                   KFLD                    KCMR
209700090209     C                   KFLD                    KCNT
209800090209     C                   KFLD                    KAAS
209900090209     C                   KFLD                    KLNP
210000090209     C                   KFLD                    KNRS
210100090209     C                   KFLD                    KNSP
210200090209      *
210300090209     C     KRDE          KLIST
210400090209     C                   KFLD                    KAAS
210500090209     C                   KFLD                    KLNP1
210600090209     C                   KFLD                    KNRS
210700090209     C                   KFLD                    KNSP
210800090209     C                   KFLD                    KNMC
210900090209     C                   KFLD                    KNRP
211000090209      *
211100090209     C     KACO          KLIST
211200090209     C                   KFLD                    KKUT
211300090209     C                   KFLD                    KKCC
211400090209     C                   KFLD                    KKSC
211500090209      *
211600090209     C     KIND          KLIST
211700090209     C                   KFLD                    KKUT
211800090209     C                   KFLD                    KKCC
211900090209     C                   KFLD                    KKSC
212000090209      *
212100090209      * DETERMINO SE SONO BARTOLINI O SDI
212200090209     C                   CLEAR                   TIBS55DS
212300090209     C                   MOVEL     'L'           I50TLA
212400090209     C                   CALL      'TIBS55R'
212500090209     C                   PARM                    TIBS55DS
212600090209      *
212700090209     C* RECUPERO DATI DELL'UTENTE
212800090209     C                   Z-ADD     1             CODUT             1 0
212900090209     C                   CALL      'XPARUT'
213000090209     C                   PARM                    UTEDSE0F
213100090209     C                   MOVEL     RAGUT         RSUT             20
213200090209     C*
213300090209     C* RICERCA CAPOCONTI
213400090209     C                   DO        50            X
213500090209     C                   MOVE      TCU(X)        TCUDS
213600090209     C     F56           IFEQ      'CG'
213700090209     C     F34           ANDEQ     '01'
213800090209     C                   Z-ADD     KCU(X)        KCI               4 0
213900090209     C                   END
214000090209     C                   END
214100090209     C*
214200090211     c                   movel(p)  'EDPAB'       User_Msg         10
214300090211     C*
214400090209     C* IMPOSTO A ZERO VARIABILI DI PGM
214500090209     c                   move      *blank        Snd_Msg_alert     1
214600090209     C                   MOVEL     CA(1)         WCA              78
214700090209     C                   MOVEL     CN(1)         WCN              78
214800090209     C*
214900090209     C* RECUPERO DATA E ORA
215000090209     C                   TIME                    WHHDAT           14 0
215100090209     C                   MOVE      WHHDAT        G02DAT
215200090209     C                   MOVE      WHHDAT        WDATA8            8 0
215300090209     C                   MOVE      WDATA8        WAA2              2 0
215400090209     C                   MOVEL     WDATA8        DATA              6 0
215500090209     C                   MOVE      WAA2          DATA
215600090209     C                   MOVEL     *BLANK        G02ERR
215700090209     C                   CALL      'XSRDA8'
215800090209     C                   PARM                    WLBDA8
215900090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
216000090209     C                   TIME                    WORA              6 0
216100090209      * Recupero data e ora
216200090209     C                   TIME                    W0140
216300090209      * UDATE IN GGMMAAAA
216400090209     C                   MOVE      W0140         WDTGIO
216500090209      * UDATE IN AAAAMMGG
216600090209     C     *eur          MOVEL     WDTGIO        DATA_eur
216700090209     C     *iso          MOVEL     DATA_eur      dateu
216800090209      *
216900090209      * ?              /*--------------------------- */
217000090209     c                   exsr      CARICA_TABELLE
217100090209      * ?              /*--------------------------- */
217200090209      *
217300090209     C                   MOVEL     *ALL'-'       CMP132          132
217400090209     C                   CLEAR                   EDiVAB00
217500090211     C                   clear                   vablnp_PT
217600090212     C                   clear                   vabnrs_PT
217700090211     C                   clear                   vabctm_PT
217800090211     C                   clear                   VABfnsp_PT
217900090209     C                   MOVEA     *HIVAL        SGN
218000991124      *
218100050414      *------------------
218200991124     C                   ENDSR
218300060621      * ?------------------------------------------------------------------ */
218400090209      *?    CARICA TABELLE SU SCHIERE
218500060621      * ?------------------------------------------------------------------ */
218600090209     C     CARICA_TABELLEBEGSR
218700090205      *
218800090209     C* Caricamento Tabella 1B
218900090209     C                   Z-ADD     0             X                 4 0
219000090209     C                   Z-ADD     CODUT         KKUT
219100090209     C                   MOVEL     '1B'          KCOD
219200090209     C     KTAB1         CHAIN     TABEL00F                           30
219300090209     C     *IN30         DOWEQ     '0'
219400090209     C     TBLFLG        IFEQ      *BLANKS
219500090209     C                   ADD       1             X
219600090209     C                   MOVEL     TBLKEY        CTM(X)
219700090209     C                   MOVEL     TBLUNI        DTM(X)
219800090209     C                   END
219900090209     C     KTAB1         READE     TABEL00F                               30
220000090209     C                   END
220100090209      *
220200090209     C* Caricamento Tabella 15
220300090209     C                   Z-ADD     0             X                 4 0
220400090209     C                   Z-ADD     CODUT         KKUT
220500090209     C                   MOVEL     '15'          KCOD
220600090209     C     KTAB1         CHAIN     TABEL00F                           30
220700090209     C     *IN30         DOWEQ     '0'
220800090209     C     TBLFLG        IFEQ      *BLANKS
220900090209     C                   ADD       1             X
221000090209     C                   MOVEL     TBLKEY        C15(X)
221100090209     C                   MOVEL     TBLUNI        DS15
221200090209     C                   MOVEL     §15COD        ISO(X)
221300090209     C                   MOVEL     §15ECF        CPF(X)
221400090209     C                   MOVE      §15PCF        CPF(X)
221500090209     C                   END
221600090209     C     KTAB1         READE     TABEL00F                               30
221700090209     C                   END
221800090209      *
221900090209     C* Caricamento Tabella CV
222000090209     C                   Z-ADD     0             X
222100090209     C                   Z-ADD     CODUT         KKUT
222200090209     C                   MOVEL     'CV'          KCOD
222300090209     C     KTAB1         CHAIN     TABEL00F                           30
222400090209     C     *IN30         DOWEQ     '0'
222500090209     C     TBLFLG        IFEQ      *BLANKS
222600090209     C                   ADD       1             X
222700090209     C                   MOVEL     TBLKEY        CCV(X)
222800090209     C                   MOVEL     TBLUNI        DCV(X)
222900090209     C                   END
223000090209     C     KTAB1         READE     TABEL00F                               30
223100090209     C                   END
223200140307      *
223300140307     C* Caricamento Tabella Priorità trasporto
223400140307     C                   Z-ADD     0             X
223500140307     C                   MOVEL     'TS'          TABCOD
223600140307     C     TABCOD        CHAIN     EDTAB01L                           30
223700140307     C     *IN30         DOWEQ     '0'
223800140307     C     TABFLG        IFEQ      *BLANKS
223900140307     C                   ADD       1             X
224000140307     C                   eval      TipoServizio(X) = TABUNI
224100140307     C                   eval      Key_TipServ(X)  = TABKEY
224200140307     C                   END
224300140307     C     TABCOD        READE     EDTAB01L                               30
224400140307     C                   END
224500090216      *
224600110328      * Caricamento Tabella termini consegna
224700110328     C                   Z-ADD     0             X
224800110328     C                   MOVEL     'TB'          TABCOD
224900110328     C     TABCOD        CHAIN     EDTAB01L                           30
225000110328     C     *IN30         DOWEQ     '0'
225100110328     C     TABFLG        IFEQ      *BLANKS
225200110328     C                   ADD       1             X
225300110328     C                   eval      K35_TpBolla(X) = TABKEY
225400110328     C                   eval      Cod_TpBolla(X) = TABKEY
225500110328     C                   eval      Decod_Porto(X) = TABUNI
225600110328     C                   END
225700110328     C     TABCOD        READE     EDTAB01L                               30
225800110328     C                   END
225900110328      *
226000090216     C* Caricamento Tabella Tipo Incasso C/Assegno
226100090216     C                   Z-ADD     0             X
226200090216     C                   MOVEL     'TC'          TABCOD
226300090216     C     TABCOD        CHAIN     EDTAB01L                           30
226400090216     C     *IN30         DOWEQ     '0'
226500090216     C     TABFLG        IFEQ      *BLANKS
226600090216     C                   ADD       1             X
226700090216     C                   MOVEL     TABKEY        CTC35(X)
226800090216     C                   MOVEL     TABUNI        TPI(X)
226900090216     C                   END
227000090216     C     TABCOD        READE     EDTAB01L                               30
227100090216     C                   END
227200140307      *
227300140307     C* Caricamento Serivizi speciali
227400140307     C                   Z-ADD     0             X
227500140307     C                   MOVEL     'SS'          TABCOD
227600140307     C     TABCOD        CHAIN     EDTAB01L                           30
227700140307     C     *IN30         DOWEQ     '0'
227800140307     C     TABFLG        IFEQ      *BLANKS
227900140307     C                   ADD       1             X
228000140307     C                   eval       Key_ServSpec(X) = TABKEY
228100140307     C                   eval       SpecialServ(X)  = TABKEY
228200140307     C                   eval       UNI_ServSpec(X) = TABUNI
228300140307     C                   END
228400140307     C     TABCOD        READE     EDTAB01L                               30
228500140307     C                   END
228600090209      *
228700090209      * Caricamento Tabella Partner esteri
228800090209     C                   Z-ADD     0             X
228900090209     C                   MOVEL     'PT'          TABCOD
229000090209     C     TABCOD        CHAIN     EDTAB01L                           30
229100090209     C     *IN30         DOWEQ     '0'
229200090209      *
229300090209     C                   SELECT
229400090209     C     TABFLG        WHENNE    *BLANKS
229500090209      *
229600090209     C                   OTHER
229700090209     C                   ADD       1             X
229800090209     C                   MOVEL     TABKEY        CPT(X)
229900090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
230000090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
230100090209     C                   MOVEL     TABUNI        DPT(X)
230200090209     C                   ENDSL
230300090209      *
230400090209     C     TABCOD        READE     EDTAB01L                               30
230500090209     C                   END
230600121105      *
230700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
230800121105     c                   exsr      ChecK_PT
230900121105      *
231000090209      * salva quanti Codici UNB ha caricato
231100090209      *
231200090209     C                   MOVEL     'PU'          TABCOD
231300090209     C     TABCOD        CHAIN     EDTAB01L                           30
231400090209     C     *IN30         DOWEQ     '0'
231500090209      *
231600090209     C                   SELECT
231700090209     C     TABFLG        WHENNE    *BLANKS
231800090209      *
231900090209     C                   OTHER
232000090209     C                   MOVEL     TABKEY        CODPU            35
232100090209     C                   Z-ADD     1             X
232200090209     C     CODPU         LOOKUP    CPT(X)                                 32
232300090209     C   32              MOVEL     TABUNI        DPU(X)
232400090209     C                   ENDSL
232500090209      *
232600090209     C     TABCOD        READE     EDTAB01L                               30
232700090209     C                   END
232800090209      *
232900090209      * Prosegue tab.PT e PU
233000090209     C                   MOVEL     'PV'          TABCOD
233100090209     C     TABCOD        CHAIN     EDTAB01L                           30
233200090209     C     *IN30         DOWEQ     '0'
233300090209      *
233400090209     C                   SELECT
233500090209     C     TABFLG        WHENNE    *BLANKS
233600090209      *
233700090209     C                   OTHER
233800090209     C                   MOVEL     TABKEY        CODPV            35
233900090209     C                   Z-ADD     1             X
234000090209     C     CODPV         LOOKUP    CPT(X)                                 32
234100090209     C   32              MOVEL     TABUNI        DPV(X)
234200090209     C                   ENDSL
234300090209      *
234400090209     C     TABCOD        READE     EDTAB01L                               30
234500090209     C                   END
234600090209      *
234700090209     C* Caricamento Tabella codici clienti - tariffe
234800090209     C                   Z-ADD     0             X
234900090209     C                   MOVEL     'CL'          TABCOD
235000090209     C     TABCOD        CHAIN     EDTAB01L                           30
235100090209     C     *IN30         DOWEQ     '0'
235200090209     C     TABFLG        IFEQ      *BLANKS
235300090209     C                   ADD       1             X
235400090209     C                   MOVEL     TABKEY        CCL(X)
235500090209     C                   MOVEL     TABUNI        DCL(X)
235600090209     C                   END
235700090209     C     TABCOD        READE     EDTAB01L                               30
235800090209     C                   END
235900090209      *
236000090209     C* Caricamento Serivizi speciali
236100090209     C                   Z-ADD     0             X
236200090209     C                   MOVEL     'SS'          TABCOD
236300090209     C     TABCOD        CHAIN     EDTAB01L                           30
236400090209     C     *IN30         DOWEQ     '0'
236500090209     C     TABFLG        IFEQ      *BLANKS
236600090209     C                   ADD       1             X
236700090209     C                   MOVEL     TABKEY        SS(X)
236800090209     C                   MOVEL     TABKEY        SS35(X)
236900090209     C                   MOVEL     TABUNI        DSS(X)
237000090209     C                   END
237100090209     C     TABCOD        READE     EDTAB01L                               30
237200090209     C                   END
237300090209      *
237400090209     C                   ENDSR
237500090209      * ?------------------------------------------------------------------ */
237600090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
237700090209      * ?------------------------------------------------------------------ */
237800090209     C     MSG_Errato    BEGSR
237900090209      *
238000090205      * ?  Scrive su tutti i records il tipo di errore
238100090205     c     *start        setll     tivin00r
238200130930     c                   exsr      Legge_TIVIN
238300130930      *.
238400090205     c                   dow       not %eof(tivin00r)
238500090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
238600131001     C                               >> Controllare UNB e i DATI su UPLOAD !!'
238700090205     C                   eval      vinFLG = '2'
238800130930     c                   exsr      AGGIOR_TIVIN
238900130930     c                   exsr      Legge_TIVIN
239000090205     c                   endDO
239100131001      *
239200131001     c                   eval      Almeno_Un_Due ='S'
239300131001     c                   eval      esito  = '2'
239400131001     c                   eval      se_errore ='S'
239500131001     c                   eval      err_bloccante ='S'
239600090205      *
239700090205     C                   ENDSR
239800090213      * ?------------------------------------------------------------------ */
239900090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
240000090213      * ?------------------------------------------------------------------ */
240100090213     C     DETta_Errato  BEGSR
240200090213      *
240300090213      *  rilascia il record prima di rieseguire il ciclo
240400131001      *.
240500131001     c                   exsr      AGGIOR_TIVIN
240600090213      *
240700090213      * ?  Scrive su tutti i records il tipo di errore
240800090213     c     SavNUM_RECord setll     tivin00r
240900130930      *
241000130930     c                   exsr      Legge_TIVIN
241100130930      *.
241200090213     c                   dow       not %eof(tivin00r)
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
242300130930      *.
242400130930     c                   exsr      AGGIOR_TIVIN
242500131001      *.
242600130930     c                   exsr      Legge_TIVIN
242700090213     c                   endDO
242800090603      *
242900131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON'
243000090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
243100131001???? C                             Bolle AMAZON non totalmente tradotto. -
243200110704     C                             Controllare UPLOAD del cliente :'
243300110704     C                   EVAL      Messaggio = %trim(Messaggio) +
243400110704     C                                         %editc(§PTksc:'X')
243500090603     c                   exsr      invio_mail_AB
243600090603      *
243700090213      *
243800090213     C                   ENDSR
243900090205      * ?------------------------------------------------------------------ */
244000090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
244100090205      * ?------------------------------------------------------------------ */
244200090205     C     *pssr         BEGSR
244300090205     C
244400060710     C                   eval      esito = '2'
244500131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON'
244600090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
244700100603     C                             Bolle import in filiale - messaggio EDI -
244800110704     C                             ricevuto errato su UPLOAD cliente :'
244900110704     C                   EVAL      Messaggio = %trim(Messaggio) +
245000110704     C                                         %editc(§PTksc:'X')
245100090603     c                   exsr      invio_mail_AB
245200060621     C
245300090213     C                   ENDSR
245400090213      * ?------------------------------------------------------------------ */
245500090213      *?      Segnala Errore su TIVIN
245600090213      * ?------------------------------------------------------------------ */
245700090213     C     Error_DET     BEGSR
245800090213     C
245900090213     C                   eval      vinFLG = '2'
246000090213     c                   eval      Almeno_Un_Due ='S'
246100090213     c                   eval      esito  = '1'
246200090213     c                   eval      se_errore ='S'
246300090213     C                   eval      Err_in_detta = 'S'
246400090213     C
246500090213     C                   ENDSR
246600090209     c*==================================================================*
246700090209     C*? CNVDAT - DECODIFICA LA DATA
246800090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
246900090209     C*         - WDATA  (DATA ALFANUMERICA)
247000090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
247100090209     C*         - WHMS   (ORA NUMERICA)
247200090209     C*----------------------------------------------------------------
247300090209     C     CNVDAT        BEGSR
247400090209     C*
247500090209     C                   MOVEL     ' '           WERRO             1
247600090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
247700090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
247800090209     C                   Z-ADD     *ZEROS        WHMS              6 0
247900090209     C*
248000090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
248100090209     C                   MOVEL     WDATA         WCOMO8            8
248200090209     C                   TESTN                   WCOMO8               99
248300090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
248400090209     C  N99              MOVEL     'S'           WERRO             1
248500090209     C                   END
248600090209     C*
248700090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
248800110523     C                   MOVEL     WDATA         WCOMO12          12
248900110523     C                   TESTN                   WCOMO12              99
249000110523     C                   MOVEl     *all'0'       WCAMGORA                       *DATA
249100110523     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
249200090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
249300090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
249400090209     C  N99              MOVEL     'S'           WERRO             1
249500090209     C                   END
249600110523     C*
249700110523     C     WFORM         IFEQ      '204'                                        CCYMMDD
249800110523     C                   MOVEL     WDATA         WCOMO14          14
249900110523     C                   TESTN                   WCOMO14              99
250000110523     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
250100110523     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
250200110523     C   99              MOVE      WCAMGORA      WHMS                           *DATA
250300110523     C  N99              MOVEL     'S'           WERRO             1
250400110523     C                   END
250500090209     C*
250600090209     C                   ENDSR
250700090210     C*----------------------------------------------------------------
250800090210     C*? TSTNUM - CONTROLLO NUMERICO
250900090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
251000090210     C*         - WLUNGH (Lunghezza campo output)
251100090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
251200090210     C*----------------------------------------------------------------
251300090210     C     TSTNUM        BEGSR
251400090210     C*
251500090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
251600090210     C                   MOVEL     'N'           WOKNUM            1
251700090210     C                   MOVEL     *BLANKS       WNUMOK           15
251800090210     C                   Z-ADD     15            IN                3 0
251900090210     C                   Z-ADD     WLUNGH        IX                3 0
252000090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
252100090210     C                   MOVEA     WNUM          NDA
252200090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
252300090210     C                   MOVEA     *ALL'0'       NDN
252400090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
252500090210     C                   Z-ADD     WLUNGH        I                 3 0
252600090210     C     I             DOWGT     0                                            --- 1 -->
252700090210     C* ESTRAE IL CARATTERE DA ESAMINARE
252800090210     C     1             SUBST     WNUM:I        WTEM01            1
252900090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
253000090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
253100090210     C     WTEM01        SCAN      WCA                                    99
253200090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
253300090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
253400090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
253500090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
253600090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
253700090210     C* DA DESTRA VERSO SINISTRA
253800090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
253900090210     C                   MOVEL     WTEMP1        NDN(IN)
254000090210     C                   SUB       1             IN
254100090210     C                   END                                                    <-- 3 ---
254200090210     C                   END                                                    <-- 2 ---
254300090210     C                   SUB       1             I
254400090210     C                   END                                                    <-- 1 ---
254500090210     C*
254600090210     C                   MOVEA     NDN           WNDN             15
254700090210     C     WNDN          IFNE      *ZEROS
254800090210     C                   MOVEA     NDN           WNUMOK           15
254900090210     C                   MOVEL     'S'           WOKNUM            1
255000090210     C                   END
255100090210     C*
255200090210     C                   ENDSR
255300090210     C*----------------------------------------------------------------
255400090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
255500090210     C*----------------------------------------------------------------
255600090210     C     CNVCAP        BEGSR
255700090210     C*
255800090210     C                   MOVEL     *BLANKS       WCAP              9
255900090210     C     nad3251       IFNE      '0'
256000090210     C     '  '          SCAN      nad3251       LENGHT            2 0
256100090210     C                   SUB       1             LENGHT
256200090210     C     LENGHT        IFLE      5
256300090210     C     LENGHT        ANDGT     0
256400090210     C                   Z-ADD     LENGHT        T                 2 0
256500090210     C                   Z-ADD     5             S                 2 0
256600090210     C                   MOVEA     nad3251       CAP9
256700090210     C                   MOVEL     *ZEROS        CAP5
256800090210     C                   DO        LENGHT
256900090210     C                   MOVE      CAP9(T)       CAP5(S)
257000090210     C                   SUB       1             S
257100090210     C                   SUB       1             T
257200090210     C                   END
257300090210     C                   MOVEA     CAP5          WCAP5             5
257400090210     C     WCAP5         IFEQ      '0'
257500090210     C                   MOVEL     *BLANKS       WCAP
257600090210     C                   ELSE
257700090210     C                   MOVEA     CAP5          WCAP
257800090210     C                   END
257900090210     C*
258000090210     C                   ELSE
258100090210     C                   MOVEL     nad3251       WCAP
258200090210     C                   END
258300090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
258400090210     C     WCAP          IFNE      *BLANKS
258500090210     C     WFLCPF        ANDNE     0
258600090210     C                   MOVEL     WFLCPF        WELE              1 0
258700090210     C                   MOVE      WFLCPF        WPOS              1 0
258800090210     C                   MOVEL     *BLANK        WCPF
258900090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
259000090210     C                   ELSE
259100090210     C                   MOVEL     *BLANKS       WCPF
259200090210     C                   END
259300090210     C                   END
259400090210     C*
259500090210     C                   ENDSR
259600090211      * ?================================================================== */
259700130930      *? Importa i records della tramsissione
259800130930      * ?------------------------------------------------------------------ */
259900130930     c     Importa_SPED  Begsr
260000130930
260100130930      *** ------------------------------------
260200131001????  *** A FINE CICLO DI UN DETTAGLIO:   --> prima che ne INIZI un altro
260300130930      *** ------------------------------------
260400130930????  **
260500130930      *  Deve flaggare il dettaglio con dei problemi
260600130930     C                   if        Err_in_detta = 'S'
260700130930      **?              /*---------------------- */
260800130930     c                   exsr      DETta_Errato
260900130930      **?              /*---------------------- */
261000130930     c                   end
261100130930      *
261200130930      **  Se non presenti errori
261300130930      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
261400130930???? c                   if        se_errore <>'S'
261500131001????  **
261600130930     c                   if         se_trovato_NAD ='S'
261700130930      * ?              /*---------------------- */
261800130930     c                   exsr      Scrive_VAB
261900130930      * ?              /*---------------------- */
262000130930     c                   else
262100130930     c                   eval      errori_in_Wrt = 'S'
262200130930     c                   end
262300130930      *
262400130930      *  Problemi nella decodifica dei campi VAB/VAT
262500130930     c                   if        errori_in_Wrt = 'S'
262600130930     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
262700130930     c                   exsr      Error_DET
262800131001     c                   end
262900131001      **
263000131001     c                   end
263100131001      **
263200130930      * Dopo aver confermato, con COMMIT
263300130930      *  si prepara per una nuova spedizione
263400131001     c                   COMMIT
263500130930      ***
263600130930     c                   endSR
263700130930      * ?------------------------------------------------------------------ */
263800090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
263900090211      * ?================================================================== */
264000090211     C     Scrive_VAB    BEGSR
264100090211      *
264200090211     c                   clear                   err_bloccante     1
264300090211     c                   move      'S'           Almeno_Uno
264400090211      *
264500090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
264600090211     c                   exsr      UPDVAB
264700090211      *
264800090211     C* >>>>>>>>>>>>
264900090211     C*  Richiamo pgm per scrittura immediata VAB
265000090211     C     §PUWRK        IFEQ      ' '
265100090211      *
265200090211     C*  Richiamo pgm per esecuzione stampa
265300090211     C     §PTCMR        IFEQ      'S'
265400090213     C                   MOVEL     WNRDOC        d86CMR
265500090211     C                   ELSE
265600090213     C                   MOVEL     WNRDOC        d86CMR
265700090211     C                   END
265800090211      *
265900090213     C                   MOVEL     ' '           d86RIE
266000090213     C                   MOVEL     §PUDTA        d86DTA
266100090213     C                   Z-ADD     1             d86CNT
266200090213     C                   Z-ADD     §PTKSC        d86CCM
266300090213     C                   MOVEL     *BLANKS       d86STA
266400090213     C                   MOVEL     'E'           d86MOD
266500090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
266600090211      * dove poi imposta la DS dei parametri passati
266700090213     C                   MOVEL     'LR'          d86CHI
266800090213     C                   MOVEL     'N'           d86CTL
266900090211      *
267000090211     c                   move      kpjbu         SvKpjbu
267100090211     c                   clear                   kpjbu
267200090216     C                   movel     *on           Commit_on         1
267300090213     C                   MOVEL     TRTC86ds      KPJBU
267400090213      * ?- Chiamata la TRTC86R2 -*
267500090213     C                   CALL      'TRTC86R2'
267600090211     C                   PARM                    KPJBA
267700090216     C                   PARM                    Commit_on
267800090211     c                   move      Svkpjbu       Kpjbu
267900090211      *
268000090211     C*  Controllo pgm per scrittura immediata VAB
268100090211     C     WW            IFNE      0
268200090211     C                   DO        WW            WX                3 0
268300090213     C                   Z-ADD     KCL(WX)       d86CCM
268400090211      *
268500090211     c                   move      kpjbu         SvKpjbu
268600090211     c                   clear                   kpjbu
268700090216     C                   movel     *on           Commit_on         1
268800090213     C                   MOVEL     TRTC86ds      KPJBU
268900090213      * ?- Chiamata la TRTC86R2 -*
269000090213     C                   CALL      'TRTC86R2'
269100090211     C                   PARM                    KPJBA
269200090216     C                   PARM                    Commit_on
269300090211     c                   move      Svkpjbu       Kpjbu
269400090211     C                   END
269500090211     C                   END
269600090211      *
269700090211     C                   END
269800090211      * >>>>>>>>>>>>
269900090211     C                   if        se_errore  = 'S'
270000090211     c                   move      'S'           err_bloccante
270100131001     C                   LEAVEsr
270200090211     c                   end
270300090211     C*
270400131001     C                   Endsr
270500090211     C*----------------------------------------------------------------
270600090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
270700090211     C*----------------------------------------------------------------
270800090211     C     UPDVAB        BEGSR
270900131001      *
271000131022      *   Imposta su NATURA MERCE il contenuto identificativo del documento BGM in testata
271100131022      *    del Messaggio (questo appositamente e solo x Amazon)
271200131022     c                   if        BGM_Id_Testata <> ' '
271300131022     c                   eval      vabNAS = BGM_Id_Testata
271400131022     c                   end
271500131022      *
271600131001      *  imposta il default dalla tabella PT se NON è ancora
271700131001      *   stato impostato il tipo bolla.
271800131001     C     VABCBO        IFEQ      *BLANKS
271900131001     C     §puPFA        ifNE      *blank
272000131001     c                   SELECT
272100131001      *  Porto Franco
272200131001     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
272300131001     C                   movel     '4'           VABCBO                         + C/Ass
272400131001     c                   WHEN      §puPFA = 'F'
272500131001     C                   movel     '1'           VABCBO                         Senza C/Ass.
272600131001      *  Porto Assegnato
272700131001     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
272800131001     C                   movel     '6'           VABCBO                         + C/Ass
272900131001     C                   WHEN      §puPFA = 'A'
273000131001     C                   movel     '2'           VABCBO                         Senza C/Ass
273100131001     C                   ENDSL
273200131001     c                   end
273300131001     C                   END
273400131001      *
273500110523      * Imposta le NOte
273600110523     C     VABNOT        IFEQ      *BLANKS
273700110523     C                   MOVEL     Wnot1         VABNOT
273800110523     C                   MOVEL     Wnot2         VABNT2
273900110523     C                   ELSE
274000110523     C                   MOVEL     Wnot1         VABNT2
274100110523     C                   END
274200090211      *
274300090211     C*  Attribuisco anno spedizione
274400090211     C     WDATPA        IFGT      0                                            anno sped.
274500090211     C                   MOVEL     WDATPA        VABAAS
274600090211     C                   ELSE
274700090211     C     WDATFV        IFGT      0
274800090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
274900090211     C                   END
275000090211     C                   END
275100090211      *
275200090211      *  Controllo dettaglio segnacolli
275300090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
275400090211     C                   ELSE
275500140304      * SOLO se BRT (non è il caso di AMAZON... comunque lascio le specifiche)
275600090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
275700090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
275800090211     C                   EXSR      CTRSGN
275900090211     C                   END
276000090211     C                   END
276100090211      *
276200131001      *  Imposto linea partenza e serie e CTM
276300090211     C                   eval      VABLNP = VABLNP_PT
276400090212     C                   eval      VABNRS = VABnrs_PT
276500090211     C                   eval      VABCTM = VABctm_PT
276600090211      *
276700090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
276800090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
276900090211     c                   clear                   mantiene_nsp      1            *blk/'S'
277000090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
277100090211      *
277200090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
277300090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
277400090211      *
277500090211      *  Imposto codice cliente
277600090211     c                   Select
277700090211      *
277800090211      *  Specificato codice da qualificatore FF (TD15)
277900090211     C                   When        WCLKSC <> *zeros
278000090211     C                   Z-ADD     WCLKSC        VABCCM
278100090211     C                   MOVEL     WCLTAR        VABCTR
278200090211      * forza LNP/CTM/NRS
278300090211     c                   if        WclLNP <> 0
278400090211     C                   eval      VABLNP = WclLNP
278500090211     c                   end
278600090211     c                   if        WclNRS <> 0
278700090211     C                   eval      VABNRS = WclNRS
278800090211     c                   end
278900090211     c                   if        WclCTM <> *blank
279000090211     C                   eval      VABCTM = WclCTM
279100090211     c                   end
279200090211      *
279300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
279400090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
279500090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
279600090211     c                   end
279700090211      *
279800090211      *  Specificato codice da qualificatore FF (TT15)
279900090211     C                   When        $CLKSC <> *zeros
280000090211     C                   Z-ADD     $CLKSC        VABCCM
280100090211     C                   MOVEL     $CLTAR        VABCTR
280200090211      * forza LNP/CTM/NRS
280300090211     c                   if        $clLNP <> 0
280400090211     C                   eval      VABLNP = $clLNP
280500090211     c                   end
280600090211     c                   if        $clNRS <> 0
280700090211     C                   eval      VABNRS = $clNRS
280800090211     c                   end
280900090211     c                   if        $clCTM <> *blank
281000090211     C                   eval      VABCTM = $clCTM
281100090211     c                   end
281200090211      *
281300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
281400090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
281500090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
281600090211     c                   end
281700090211      *
281800090211      *  Imposto dati da tabella - PT-
281900090211     C                   Other
282000090211      *
282100090211     C                   MOVEL     §PTKSC        VABCCM
282200090211     C                   MOVEL     §PTCTR        VABCTR
282300090211      *
282400090211     C                   Endsl
282500090211      *
282600090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
282700090211      *    o dalla relativa CL
282800090211     C                   MOVEL     VABlnp        VABfgs
282900090211      *
283000090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
283100090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
283200090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
283300090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
283400090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
283500090211      *
283600090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
283700090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
283800090211     c                   else
283900090211      *
284000090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
284100090211     C*  lo attribuisco io
284200090211      **              **------------------**
284300090211     c                   exsr      repnum
284400090211      **              **------------------**
284500090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
284600090211      **              **------------------**
284700090211     c                   end
284800090211      *
284900090211     C*  Altrimenti attribuisco data del giorno
285000090211     C     VABMGS        IFEQ      0
285100090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
285200090211     C                   MOVE      WOGGI         VABMGS                         = oggi
285300090211     C                   END
285400090211     C*  Eseguo posizionamento su EDiVAB
285500090211     C                   Z-ADD     VABAAS        KAAS
285600090211     C                   Z-ADD     VABlnp        KLNP
285700090211     C                   Z-ADD     VABNRS        KNRS
285800090211     C                   Z-ADD     VABNSP        KNSP
285900090211     C                   MOVEL     SAVBOL        SALVA
286000090211     C     KVAB1         CHAIN     EDiVAB1L                           30
286100090211     C                   MOVEL     SALVA         SAVBOL
286200090211     C*  Imposto dati CMR
286300090211     C                   Z-ADD     1             VABCNT
286400090211     **
286500090211      *** Attenzione se si deve trattare il VAX con il CMR
286600090211      *** il campo VABCNT deve essere sempre impostato a (1)
286700090211     C     §puWRK        ifEQ      'S'
286800090211     C     §puNSP        andEQ     'S'
286900090211     C                   Z-ADD     1             VABCNT
287000090211     c                   end
287100090211     **
287200090211     C                   Z-ADD     WDTPRE        VABDTS
287300090212     C                   movel     '20'          VABDTS
287400090212     C     WHMPRE        mult      100           VABHMS
287500090211     C     §PTCMR        IFEQ      'S'
287600090211     C                   Z-ADD     WDTCMR        VABDCM
287700090211     C                   MOVEL     WNRCMR        VABCMR
287800090211     C                   ELSE
287900090211     C                   Z-ADD     WDATFV        VABDCM
288000090211     C                   MOVEL     WNUMFV        VABCMR
288100090211     C                   END
288200131001     **
288300090211     C*  Se non è indicato il nome del mittente = Partner mittente
288400090211     C     VABRMO        IFEQ      *BLANKS
288500090211     C                   MOVEL     ACORAG        VABRMO
288600090211     C                   MOVEL     INDCAP        VABCMO
288700090211     C     INDCAE        IFNE      *BLANKS
288800090211     C                   MOVEL     INDCAE        VABCMO
288900090211     C                   END
289000090211     C     INDSTA        IFNE      *BLANKS
289100090211     C                   MOVEL     INDSTA        VABNMO
289200090211     C                   END
289300090211     C                   END
289400131001     **
289500090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
289600090211     C     §1BFG1        IFEQ      'N'
289700090211     C     §1BFG7        ANDEQ     'N'
289800090211     C     §1BFG2        ANDEQ     'N'
289900090211     C***                  Z-ADD0         VABNRS
290000090211     C                   END
290100090211     C*  Imposto segancollo da - a
290200090211     C     §1BFG1        IFEQ      'S'
290300090211     C     §1BFG7        ANDEQ     'N'
290400090211     C     §1BF12        ANDNE     'E'
290500090211      *
290600090211     C     XY            IFEQ      *ZEROS
290700090211     C                   CLEAR                   VABNCD
290800090211     C                   CLEAR                   VABNCA
290900090211     C                   ELSE
291000090211     C                   Z-ADD     SGN(1)        VABNCD
291100090211     C                   Z-ADD     SGN(XY)       VABNCA
291200090211     C                   ENDIF
291300090211     C*
291400090211     C                   END
291500090211     C*
291600090211     C*  Se cap mittente originale a blank abblenco nazione
291700090211     C     VABCMO        IFEQ      *BLANKS
291800090211     C                   MOVEL     *BLANKS       VABNMO
291900090211     C                   END
292000090603     C*
292100090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
292200090603     C* Se in testata
292300090603     c                   if        NAD_destinatario_in_testata = 'S'
292400090603     c                   eval         VABrsd =  tes_VABrsd
292500090603     c                   eval         VABrd2 =  tes_VABrd2
292600090603     c                   eval         VABind =  tes_VABind
292700090603     c                   eval         VABlod =  tes_VABlod
292800090603     c                   eval         VABnzd =  tes_VABnzd
292900090603     c                   end
293000090603     C* Se in testata
293100090603     c                   if        NAD_mittente_in_testata = 'S'
293200090603     c                   eval         VABrmo = tes_VABrmo
293300090603     c                   eval         VABnmo = tes_VABnmo
293400090603     c                   eval         VABcmo = tes_VABcmo
293500090603     c                   end
293600110718      *
293700090211      *? ------------------------------------------------------------------------
293800090213     C   30              WRITE     EDiVAB00                             99
293900090213     C  N30              UPDATE    EDiVAB00                             99
294000090211      *? ------------------------------------------------------------------------
294100090213     c   99              eval      errori_in_Wrt = 'S'
294200090211     C*
294300090211     c                   clear                   wri_vabtic        1
294400090211     C*
294500090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
294600090211     C                   EXSR      WRTVAT_Rif
294700090211     C*
294800090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
294900090211     C     §1BF12        IFEQ      'E'
295000090211     C                   EXSR      WRTVAT
295100090211     C                   ELSE
295200090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
295300090211     C     §1BFG7        IFEQ      'S'
295400090211     C     §1BFG1        OREQ      'S'
295500090211     C                   EXSR      WRTVAD
295600090211     C                   END
295700090211     C                   END
295800090211      *
295900090211     C* Reimposto codice trattamento merce cliente
296000090211     C                   MOVEL     WSVTRM        DS1B
296100090211     C**
296200090211     C* Do errore se previsto CMR ma non c'è
296300090211     C     §PTCMR        IFNE      ' '
296400090211     C                   EXSR      MEMCMR
296500090211     C                   END
296600131001     C**
296700090211     C* Do errore se previsto AFB ma non c'è
296800090211     C     §PTNFV        IFNE      ' '
296900090211     C                   EXSR      MEMAFB
297000090211     C                   END
297100131001     C**
297200090211     C*
297300090211     C                   ENDSR
297400090211     C*----------------------------------------------------------------
297500090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
297600090211     C*----------------------------------------------------------------
297700090211     C     CTRSGN        BEGSR
297800131001      *
297900090211     C*  Riordino i segnacolli
298000090211     C     §1BFG7        IFEQ      'N'
298100090211     C                   SORTA     SGN
298200090211     C*  Controllo se sono sequenziali
298300090211     C                   MOVEL     'N'           WNOSEQ            1
298400090211     C     SGN(1)        ADD       1             WEXSGN            7 0
298500090211     C     2             DO        XY            X
298600090211     C     SGN(X)        IFNE      WEXSGN
298700090211     C                   MOVEL     'S'           WNOSEQ
298800090211     C                   END
298900090211     C     SGN(X)        ADD       1             WEXSGN
299000090211     C                   END
299100090211     C                   END
299200090211     C*
299300090211     C     FINSGN        ENDSR
299400090211     C*----------------------------------------------------------------
299500090211     C*? ESEGUO SCRITTURA EDiVAD
299600090211     C*----------------------------------------------------------------
299700090211     C     WRTVAD        BEGSR
299800090211     C*
299900090211     C* Se non seq. scrivo xy record
300000090211     C     §1BFG1        IFEQ      'S'
300100090211     C     XY            ANDNE     *ZEROS
300200090211     C*
300300090211     C     §1BFG7        IFEQ      'S'
300400090211     C                   DO        XY            X
300500090211     C                   Z-ADD     VABCCM        KCCM
300600090211     C                   Z-ADD     SGN(X)        KNCD
300700090211     C                   Z-ADD     VABCNT        VADCNT
300800090211     C                   MOVEL     VABCMR        VADCMR
300900090211     C                   Z-ADD     VABAAS        VADAAS
301000090211     C                   Z-ADD     VABLNP        VADLNP
301100090211     C                   Z-ADD     VABNRS        VADNRS
301200090211     C                   Z-ADD     VABNSP        VADNSP
301300090211     C                   Z-ADD     1             VADNCL
301400090211     C                   MOVEL     SGN(X)        VADCDP
301500090211     C                   Z-ADD     SGN(X)        VADNCD
301600090211     C                   Z-ADD     *ZEROS        VADNCA
301700090211     C                   Z-ADD     VABCCM        VADCCM
301800090211     C                   movel     VABfgs        VADfgs
301900090213     C                   WRITE     EDiVAD00                             99
302000090213     c   99              eval      errori_in_Wrt = 'S'
302100090211     C                   END
302200090211     C* Se sequenziali scrivo un solo record
302300090211     C                   ELSE
302400090211     C                   Z-ADD     VABAAS        VADAAS
302500090211     C                   Z-ADD     VABLNP        VADLNP
302600090211     C                   Z-ADD     VABNRS        VADNRS
302700090211     C                   Z-ADD     VABNSP        VADNSP
302800090211     C                   Z-ADD     VABNCL        VADNCL
302900090211     C                   Z-ADD     SGN(1)        VADNCD
303000090211     C                   Z-ADD     SGN(XY)       VADNCA
303100090211     C                   Z-ADD     VABCCM        VADCCM
303200090211     C                   movel     VABfgs        VADfgs
303300090213     C                   WRITE     EDiVAD00                             99
303400090213     c   99              eval      errori_in_Wrt = 'S'
303500090211     C                   END
303600090211     C*
303700090211     C                   ELSE
303800090211     C                   DO        XY            X
303900090211     C                   Z-ADD     VABCCM        KCCM
304000090211     C                   Z-ADD     SGN(X)        KNCD
304100090211     C                   Z-ADD     VABAAS        VADAAS
304200090211     C                   Z-ADD     VABLNP        VADLNP
304300090211     C                   Z-ADD     VABNRS        VADNRS
304400090211     C                   Z-ADD     VABNSP        VADNSP
304500090211     C                   MOVEL     SGN(X)        VADCDP
304600090211     C                   Z-ADD     1             VADNCL
304700090211     C                   Z-ADD     *ZEROS        VADNCD
304800090211     C                   Z-ADD     *ZEROS        VADNCA
304900090211     C                   Z-ADD     VABCCM        VADCCM
305000090211     C                   movel     VABfgs        VADfgs
305100090213     C                   WRITE     EDiVAD00                             99
305200090213     c   99              eval      errori_in_Wrt = 'S'
305300090211     C                   END
305400090211     C*
305500090211     C                   END
305600090211     C*
305700090211     C                   ENDSR
305800090211     C*----------------------------------------------------------------
305900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
306000090211     C*----------------------------------------------------------------
306100090211     C     WRTVAT_Rif    BEGSR
306200090211      *
306300090211      *  riferimento di consegna destinatario
306400090211     c                   if        WatRDE <> *blank
306500090211     C                   Z-ADD     VABCNT        VATCNT
306600090211     C                   MOVEL     VABCMR        VATCMR
306700090211     C                   Z-ADD     VABCCM        VATCCM
306800090211     C                   movel     VABfgs        VATfgs
306900090211     C                   Z-ADD     VABLNP        VATLNP
307000090211     C                   Z-ADD     VABAAS        VATAAS
307100090211     C                   Z-ADD     VABNRS        VATNRS
307200090211     C                   Z-ADD     VABNSP        VATNSP
307300090211     C                   MOVEL     'A'           VATTRC
307400131002     C                   MOVEL(p)  WatRDE        VATNOT
307500090213     C                   WRITE     EDiVAT00                             99
307600090213     c   99              eval      errori_in_Wrt = 'S'
307700090211     c                   end
307800090211      *
307900090211      *  riferimento di consegna telefonico
308000090211     c                   if        WatTEL <> *blank
308100090211     C                   Z-ADD     VABCNT        VATCNT
308200090211     C                   MOVEL     VABCMR        VATCMR
308300090211     C                   Z-ADD     VABCCM        VATCCM
308400090211     C                   movel     VABfgs        VATfgs
308500090211     C                   Z-ADD     VABLNP        VATLNP
308600090211     C                   Z-ADD     VABAAS        VATAAS
308700090211     C                   Z-ADD     VABNRS        VATNRS
308800090211     C                   Z-ADD     VABNSP        VATNSP
308900090211     C                   MOVEL     'B'           VATTRC
309000131002     C                   MOVEL(p)  WatTEL        VATNOT
309100090213     C                   WRITE     EDiVAT00                             99
309200090213     c   99              eval      errori_in_Wrt = 'S'
309300090211     c                   end
309400090211      *
309500090211     C                   ENDSR
309600090211     C*----------------------------------------------------------------
309700090211     C*? ESEGUO SCRITTURA EDiVAT
309800090211     C*----------------------------------------------------------------
309900090211     C     WRTVAT        BEGSR
310000090211      *
310100090211     C                   DO        §§            X
310200090211     C                   Z-ADD     VABCNT        VATCNT
310300090211     C                   MOVEL     VABCMR        VATCMR
310400090211     C                   Z-ADD     VABCCM        VATCCM
310500090211     C                   movel     VABfgs        VATfgs
310600090211     C                   Z-ADD     VABLNP        VATLNP
310700090211     C                   Z-ADD     VABAAS        VATAAS
310800090211     C                   Z-ADD     VABNRS        VATNRS
310900090211     C                   Z-ADD     VABNSP        VATNSP
311000090211     C                   MOVEL     'E'           VATTRC
311100090211      *
311200090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
311300090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
311400090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
311500090211     c                   if        WVATlun > 0
311600090211      *
311700090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
311800090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
311900090211      *
312000090211     c                   else
312100090211      *
312200090211      * Se non è impostato nulla prende il segnacollo passato così com'è
312300131002     C                   MOVEL(p)  SGE(X)        VATNOT
312400090211     c                   end
312500090211      *
312600090213     C                   WRITE     EDiVAT00                             99
312700090213     c   99              eval      errori_in_Wrt = 'S'
312800090211     C                   END
312900090211      *
313000090211     C                   ENDSR
313100090213     c**********************************************************************
313200090213     c* reperimento numero Spedizione
313300090213     c**********************************************************************
313400090213     C     repnum        BEGSR
313500090213      *
313600090213      *    deve controllare sulla tabella "3C" se il numero spedizione
313700090213      *     deve essere mantenuto oppure no
313800090213     C*
313900090213     C                   clear                   Ds3C
314000090213     C                   Z-ADD     CODUT         TBLKUT
314100090213     C                   MOVEL     '3C'          TBLCOD
314200090213     C                   movel(p)  VABccm        TBLKEY
314300090213     C     KTABel        CHAIN     TABEL00F                           30
314400090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
314500090213     C                   MOVEL     TBLUNI        Ds3C
314600090213     C                   END
314700090213      *
314800090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
314900090213     c                   if        §3CFsp <> 'S'
315000090213      *
315100090213     C* NSP => Stacco un numeratore da AZNUM
315200090213     c                   movel     kpjbu         svkpjbu
315300090213     c                   clear                   kpjbu
315400090213     C                   clear                   TRUL33DS
315500090213     C                   eval      I33OPE = *zeros
315600090213     C                   eval      I33CNU = 302
315700090213     C                   eval      I33NUM = 1
315800090213     C                   movel     TRUL33DS      KPJBU
315900090213     C                   call      'TRUL33R'
316000090213     C                   parm                    KPJBA
316100090213     C                   movel     KPJBU         TRUL33DS
316200090213     c                   movel     svkpjbu       kpjbu
316300090213      ****
316400090213     C                   if        O33ERR = *zeros
316500090213     c                   z-add     o33nrf        NUM_SPED
316600090213     C                   else
316700090213     C                   endif
316800090213      ****
316900090213     c                   ELSE
317000090213      *   se si vuole mantenere il numero spedizione
317100090213      ****
317200090213     C                   MOVEL     WOGGI         WANNO             4 0
317300090213     C                   MOVE      WANNO         KAAA
317400090213     C                   Z-ADD     3             KCNU
317500090213     C                   Z-ADD     VABlnp        KFIL
317600090213     C     KNUF          CHAIN     FLNUF                              9999
317700090213     C     *IN99         IFEQ      '0'
317800090213     C                   ADD       1             NUFNUM
317900090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
318000090213     C                   UPDATE    FLNUF
318100090213     C                   ELSE
318200090213     C                   END
318300090213     C*
318400090213     c                   EndIF
318500090213      **
318600090213     C                   ENDSR
318700090213     C*----------------------------------------------------------------
318800090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
318900090213     C*----------------------------------------------------------------
319000090213     C     MEMCMR        BEGSR
319100090213     C*
319200090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
319300090213     C     §PTCM1        IFEQ      'S'
319400090213     C                   CLEAR                   EDRDE000
319500090213     C                   MOVEL     'TD1154'      KNMC
319600090213     C                   Z-ADD     1             KNRP
319700090213     C                   MOVEL     WKSC          KLNP1
319800090213     C     KRDE          CHAIN     EDRDE01L                           31
319900090213     C     *IN31         IFEQ      '1'
320000090213     C                   Z-ADD     VABAAS        RDEAAS
320100090213     C                   Z-ADD     VABLNP        RDELNP
320200090213     C                   Z-ADD     VABNRS        RDENRS
320300090213     C                   Z-ADD     VABNSP        RDENSP
320400090213     C                   MOVEL     'TD1154'      RDENMC
320500090213     C                   Z-ADD     1             RDENRP
320600090213     C                   MOVEL     WNRCMR        RDEVAL
320700090213     C                   WRITE     EDRDE000                             99
320800090213     c   99              eval      errori_in_Wrt = 'S'
320900090213     C                   END
321000090213     C*  Memorizzo qualificatore CMR
321100090213     C                   CLEAR                   EDRDE000
321200090213     C                   MOVEL     'TD1153'      KNMC
321300090213     C                   Z-ADD     1             KNRP
321400090213     C                   MOVEL     WKSC          KLNP1
321500090213     C     KRDE          CHAIN     EDRDE01L                           31
321600090213     C     *IN31         IFEQ      '1'
321700090213     C                   Z-ADD     VABAAS        RDEAAS
321800090213     C                   Z-ADD     VABLNP        RDELNP
321900090213     C                   Z-ADD     VABNRS        RDENRS
322000090213     C                   Z-ADD     VABNSP        RDENSP
322100090213     C                   Z-ADD     1             RDENRP
322200090213     C                   MOVEL     'TD1153'      RDENMC
322300090213     C                   MOVEL     'CMR'         RDEVAL
322400090213     C                   WRITE     EDRDE000                             99
322500090213     c   99              eval      errori_in_Wrt = 'S'
322600090213     C                   END
322700090213     C*  Memorizzo la data
322800090213     C                   CLEAR                   EDRDE000
322900090213     C                   MOVEL     'TD2380'      KNMC
323000090213     C                   Z-ADD     1             KNRP
323100090213     C                   MOVEL     WKSC          KLNP1
323200090213     C     KRDE          CHAIN     EDRDE01L                           31
323300090213     C     *IN31         IFEQ      '1'
323400090213     C                   Z-ADD     VABAAS        RDEAAS
323500090213     C                   Z-ADD     VABLNP        RDELNP
323600090213     C                   Z-ADD     VABNRS        RDENRS
323700090213     C                   Z-ADD     VABNSP        RDENSP
323800090213     C                   Z-ADD     1             RDENRP
323900090213     C                   MOVEL     'TD2380'      RDENMC
324000090213     C                   MOVEL     WDTCMR        RDEVAL
324100090213     C                   WRITE     EDRDE000                             99
324200090213     c   99              eval      errori_in_Wrt = 'S'
324300090213     C                   END
324400090213     C*
324500090213     C                   END                                                    §PTCM1 = 'S'
324600090213     C*
324700090213     C                   ENDSR
324800090213     C*----------------------------------------------------------------
324900090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
325000090213     C*----------------------------------------------------------------
325100090213     C     MEMAFB        BEGSR
325200090213     C*
325300090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
325400090213     C     §PTNF1        IFEQ      'S'
325500090213     C                   CLEAR                   EDRDE000
325600090213     C                   MOVEL     WKSC          KLNP1
325700090213     C                   MOVEL     'TD1154'      KNMC
325800090213     C                   Z-ADD     1             KNRP
325900090213     C     KRDE          CHAIN     EDRDE01L                           31
326000090213     C     *IN31         IFEQ      '1'
326100090213     C                   Z-ADD     VABAAS        RDEAAS
326200090213     C                   Z-ADD     VABLNP        RDELNP
326300090213     C                   Z-ADD     VABNRS        RDENRS
326400090213     C                   Z-ADD     VABNSP        RDENSP
326500090213     C                   MOVEL     'TD1154'      RDENMC
326600090213     C     §PTCM1        IFNE      'S'
326700090213     C     WNRCMR        OREQ      *BLANKS
326800090213     C                   Z-ADD     1             RDENRP
326900090213     C                   ELSE
327000090213     C                   Z-ADD     2             RDENRP
327100090213     C                   END
327200090213     C                   MOVEL     WNUMFV        RDEVAL
327300090213     C                   WRITE     EDRDE000                             99
327400090213     c   99              eval      errori_in_Wrt = 'S'
327500090213     C                   END
327600090213     C*  Memorizzo qualificatore CMR
327700090213     C                   CLEAR                   EDRDE000
327800090213     C                   MOVEL     WKSC          KLNP1
327900090213     C                   MOVEL     'TD1153'      KNMC
328000090213     C                   Z-ADD     1             KNRP
328100090213     C     KRDE          CHAIN     EDRDE01L                           31
328200090213     C     *IN31         IFEQ      '1'
328300090213     C                   Z-ADD     VABAAS        RDEAAS
328400090213     C                   Z-ADD     VABLNP        RDELNP
328500090213     C                   Z-ADD     VABNRS        RDENRS
328600090213     C                   Z-ADD     VABNSP        RDENSP
328700090213     C     §PTCM1        IFNE      'S'
328800090213     C     WNRCMR        OREQ      *BLANKS
328900090213     C                   Z-ADD     1             RDENRP
329000090213     C                   ELSE
329100090213     C                   Z-ADD     2             RDENRP
329200090213     C                   END
329300090213     C                   MOVEL     'TD1153'      RDENMC
329400090213     C                   MOVEL     'AFB'         RDEVAL
329500090213     C                   WRITE     EDRDE000                             99
329600090213     c   99              eval      errori_in_Wrt = 'S'
329700090213     C                   END
329800090213     C*  Memorizzo la data
329900090213     C                   CLEAR                   EDRDE000
330000090213     C                   MOVEL     WKSC          KLNP1
330100090213     C                   MOVEL     'TD2380'      KNMC
330200090213     C                   Z-ADD     1             KNRP
330300090213     C     KRDE          CHAIN     EDRDE01L                           31
330400090213     C     *IN31         IFEQ      '1'
330500090213     C                   Z-ADD     VABAAS        RDEAAS
330600090213     C                   Z-ADD     VABLNP        RDELNP
330700090213     C                   Z-ADD     VABNRS        RDENRS
330800090213     C                   Z-ADD     VABNSP        RDENSP
330900090213     C     §PTCM1        IFNE      'S'
331000090213     C     WNRCMR        OREQ      *BLANKS
331100090213     C                   Z-ADD     1             RDENRP
331200090213     C                   ELSE
331300090213     C                   Z-ADD     2             RDENRP
331400090213     C                   END
331500090213     C                   MOVEL     'TD2380'      RDENMC
331600090213     C                   MOVEL     WDATFV        RDEVAL
331700090213     C                   WRITE     EDRDE000                             99
331800090213     c   99              eval      errori_in_Wrt = 'S'
331900090213     C                   END
332000090213     C*
332100090213     C                   END                                                    §PTCM1 = 'S'
332200090213     C*
332300090213     C                   ENDSR
332400090213     c*==================================================================*
332500090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
332600090213     c*==================================================================*
332700090213     c     Imp_ADDR_mail begsr
332800090213      *
332900090213     c                   clear                   Indirizzi
333000090213      *
333100090213      *  Lunghezza indirizzi presenti
333200090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
333300090213     c                   z-add     1             al_char           3 0
333400090213     c                   z-add     1             dal_char          3 0
333500090213     c                   z-add     1             tot_char          3 0
333600090213      *
333700090213     c     successivo    tag
333800090213      *
333900090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
334000090213      *   e aggiunge il dominio Bartolini + il (;) separatore
334100090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
334200090213     c                   eval      tot_char = al_char - dal_char
334300090213     c                   z-add     dal_char      Dac
334400090213     c                   z-add     tot_char      Luc
334500090213      *
334600090213     c                   clear                   Indir_Bartol     40
334700090213     c                   clear                   Indir_Parz       20
334800090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
334900090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
335000090213     c                             %Trim(bart_it)
335100090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
335200090213      *
335300090213     c                   if        al_char = Lun_Ind
335400131001     C                   LEAVEsr
335500090213     c                   else
335600090213     c                   eval      dal_char = ( al_char + 1 )
335700090213     c                   goto      successivo
335800090213     c                   end
335900090213      *
336000090213     C                   ENDSR
336100090213     c*==================================================================*
336200090213      * Manda un Msg x E-mail
336300090213     c*==================================================================*
336400090213     c     Invio_Mail    begsr
336500090213      *
336600131001      *    DA SALTARE
336700131001      *    DA SALTARE
336800131001      *    DA SALTARE
336900131001     C                   LEAVEsr
337000090213      *
337100131001     C*   Alert_mail : invio Mail a CED@Brt.it                  *
337200090213     C* Inizializzo variabili
337300090213     C                   movel     *blanks       wrkEml          253
337400090213     C                   movel     *blanks       wrkMsg         5000
337500090213     C                   movel     *blanks       wrkOgg           44
337600090213      *
337700090213     C* Valorizzo i campi della e-m@ail - indirizzo
337800090213     C                   eval      wrkEml = Indirizzi
337900090213     C                   eval      wrkOgg = Oggetto
338000090213     C                   eval      wrkMsg = Messaggio
338100090213
338200090213     C                   call(e)   'TIS701C'
338300090213     C                   parm                    wrkEml
338400090213     C                   parm                    wrkOgg
338500090213     C                   parm                    wrkMsg
338600090213     C*
338700090213     C                   ENDSR
338800090603     c*==================================================================*
338900090603      * Manda un Msg x E-mail a EDPAB
339000090603     c*==================================================================*
339100090603     c     Invio_Mail_AB begsr
339200090603      *
339300131001     C*   Alert_mail : invio Mail a CEDalert@Brt.it                  *
339400090603     C* Inizializzo variabili
339500090603     C                   movel     *blanks       wrkEml          253
339600090603     C                   movel     *blanks       wrkMsg         5000
339700090603     C                   movel     *blanks       wrkOgg           44
339800090603      *
339900090603     C* Valorizzo i campi della e-m@ail - indirizzo
340000110203     C                   eval      wrkEml = Indirizzi
340100090603     C                   eval      wrkOgg = Oggetto
340200090603     C                   eval      wrkMsg = Messaggio
340300110203     C*
340400110203     C                   call(e)   'TRTCT00R2'
340500110203     C                   parm                    wrkEml
340600110203     C                   parm                    wrkOgg
340700110203     C                   parm                    wrkMsg
340800090603     C*
340900090603     C                   ENDSR
341000121105      *---------------------------------------------------------------*
341100121105      * CTRL caricamento schiere    PT                               -*
341200121105      *---------------------------------------------------------------*
341300121105     C     Check_PT      begsr
341400121105     C*
341500121105     c* Controllo riempimento schiera
341600121105     c                   clear                   trul0sds
341700121105     c                   eval      i0sski='CPT'
341800121105     c                   eval      i0sele=%elem(CPT)
341900121105     c                   eval      i0spie=x
342000121105     c                   eval      i0sfile='EDTAB00F'
342100121105     c                   eval      i0ssif=knsif
342200131001     c                   eval      i0spgm='TRTCT17R'
342300121113     c                   move      kpjbu         SvKpjbu
342400121113     c                   clear                   kpjbu
342500121105     c                   movel     trul0sds      kpjbu
342600121105     c                   call      'TRUL0SR'
342700121105     c                   parm                    kpjba
342800121113     c                   eval      kpjbu  =  SvKpjbu
342900121105     C*
343000121105     C                   ENDSR
343100090211     O*****************************************************************
343200090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
343300090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
343400090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
343500090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
