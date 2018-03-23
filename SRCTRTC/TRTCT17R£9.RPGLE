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
009500091019???? D Dettaglio_NAD_destinatario...
009600091019???? D                 s              1
009700091019???? D NAD_destinatario_in_testata...
009800091019???? D                 s              1
009900091019???? D NAD_mittente_in_testata...
010000091019???? D                 s              1
010100131001???? D siamo_in_Dettaglio...
010200091019???? D                 s              1
010300100120???? D seg_imprevisto...
010400100120???? D                 s              1
010500091019???? D Riferimento_Spedizione...
010600130930???? D                 s              3    INZ('IV ')
010700131002???? D Riferimento_Telefonico...
010800131002???? D                 s              3    INZ('TE ')
010900130930???? D Riferimento_x_Segnacollo...
011000130930???? D                 s              3    INZ('CR ')
011100140304      **?x AMAZON il numerico era identico al segnacollo *  **********
011200140304      **?  diventato "TB" X gestire il segnacollo come   *  **********
011300140304      **? Multicollo (più righe di RFF+CR)               *  **********
011400131002???? D Riferimento_Numerico...
011500140304???? D                 s              3    INZ('TB ')
011600131001???? D Segmento_Inizio_Dettaglio...
011700130930???? D                 s              3    INZ('GID')
011800131001???? D Fine_Ultimo_Dettaglio...
011900131001???? D                 s              3    INZ('UNT')
012000131002???? D in_Kg           s              3    INZ('KG ')
012100131002???? D Peso_in_Kg...
012200131002???? D                 s              3    INZ('WT ')
012300131002???? D Peso_Tassabile...
012400131002???? D                 s              3    INZ('WX ')
012500131002???? D Tipo_Peso_lordo_in_Kg...
012600131002???? D                 s              3    INZ('G  ')
012700131002???? D Tipo_Peso_Tassabile...
012800131002???? D                 s              3    INZ('B  ')
012900090209      *---------------------------------------------------------------------*
013000090209     D KPJBA         E DS
013100090209     D FNLV55DS      E DS
013200090209     D TIBS55DS      E DS
013300090209     D TISI95DS      E DS
013400090209     D UTEDSE0F      E DS
013500090209     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
013600090209     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
013700090209      * Ds scomposzione tipo capoconti
013800090209     D TCUDS           DS
013900090209     D  F34                    3      4
014000090209     D  F56                    5      6
014100090209     D DS1B          E DS
014200131002     D SavDS1B         s                   like(ds1b)
014300090209     D DS15          E DS
014400090209     D DSCV          E DS
014500090213     D TRTC86DS      E DS
014600090209     D EDIDSPT       E DS
014700090209     D EDIDSPU       E DS
014800090209     D EDIDSPV       E DS
014900130930      **
015000090211     D EDIDSCL       E DS
015100090216     D EDIDSTC       E DS
015200090209      **
015300090209      *  DS x salvataggio dati impostati in EDiVAB
015400090209     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
015500090209     D SALVA           DS           545
015600090210      **
015700090209     D WLBDA8          DS
015800090209     D  G02DAT                 1      8  0
015900090209     D  G02INV                 9     16  0
016000090209     D  G02ERR                17     17
016100090209     D  G02TGI                18     22  0
016200090209      *  DS x scomposizione segnacollo
016300090209     D DSSGN           DS
016400090209     D  SGNLNP                 1      3  0
016500090209     D  SGNLNA                 4      6  0
016600090209     D  SGNNRS                 7      8  0
016700090209     D  SGNNSC                 9     15  0
016800090209     D  SGNZNC                16     17  0
016900090209      *  DS x stampa segnacollo
017000090209      *---------------                                                      *
017100090211     D  vablnp_PT      s                   LIKE(vablnp)
017200090212     D  vabnrs_PT      s                   LIKE(vabnrs)
017300090211     D  vabctm_PT      s                   LIKE(vabctm)
017400131001      *---------------                                                      *
017500090209     D Wvabtic         s                   LIKE(vabtic)
017600090603     d tes_VABrmo      s                   LIKE(VABrmo)
017700090603     d tes_VABnmo      s                   LIKE(VABnmo)
017800090603     d tes_VABcmo      s                   LIKE(VABcmo)
017900131001      *
018000090603     d tes_VABrsd      s                   LIKE(VABrsd)
018100090603     d tes_VABrd2      s                   LIKE(VABrd2)
018200090603     d tes_VABind      s                   LIKE(VABind)
018300090603     d tes_VABlod      s                   LIKE(VABlod)
018400090603     d tes_VABnzd      s                   LIKE(VABnzd)
018500131001      *
018600090603     d tes_Valuta      s                   LIKE(VABvca)
018700090209      *---------------------------------------------------------------------*
018800090209      * Schiere
018900090209      *---------------------------------------------------------------------*
019000090209      * Schiera per reperimento nr.seganacollo
019100090209     D SGN             S              7  0 DIM(5000)
019200090209     D SGE             S             35    DIM(5000)                            EUROEXPRESS
019300090209      * Stringhe per conversione alfanumerico/numerico N.Documento
019400090209     D CA              S             78    DIM(1) CTDATA PERRCD(1)
019500090209     D CN              S             78    DIM(1) CTDATA PERRCD(1)
019600090209      * Nr. documento alfanumerico da decodificare
019700131001      * Nr. documento alfanumerico già decodificato
019800090209     D NDA             S              1    DIM(35)
019900090209     D NDN             S              1    DIM(15)
020000090209      * Messaggi di errore
020100090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
020200090209      * Tabella codici trattamento merce (1B)
020300090209     D CTM             S              2    DIM(200)
020400090209     D DTM             S             67    DIM(200)
020500090209      * Tabella codici valuta
020600090209     D CCV             S              3    DIM(200)
020700090209     D DCV             S             89    DIM(200)
020800090209      * Tabella codici nazioni
020900090209     D C15             S              3    DIM(500)
021000090209     D ISO             S              3    DIM(500)
021100090209     D CPF             S              2  0 DIM(500)
021200090209      * Tabella partner
021300121105     D CPT             S             35    DIM(200)
021400121105     D CPTparz         S             35    DIM(%elem(CPT))
021500121105     D KSC_UNB         S              7    DIM(%elem(CPT))
021600121105     D DPT             S             90    DIM(%elem(CPT))
021700121105     D DPU             S             90    DIM(%elem(CPT))
021800121105     D DPV             S             90    DIM(%elem(CPT))
021900121105     D TRUL0SDS      E DS
022000090209      *
022100090209      * Tabella CL --> codici clienti - tariffa
022200130305     D CCL             S             35    DIM(999)
022300130305     D DCL             S             90    DIM(999)
022400130305     D KCL             S              7  0 DIM(999)
022500090209      * Termini di consegna
022600110328     D K35_TpBolla     S             35    DIM(100)
022700110328     D Cod_TpBolla     S              3    DIM(100)
022800110328     D Decod_Porto     S              1    DIM(100)
022900110328     d Trovata_Tab_TB  S              1
023000110328      *
023100090216      * Tipo pagamento C/Assegno
023200090216     D CTC35           S             35    DIM(100)
023300090216     D TPI             S              2    DIM(100)
023400090209      * Decodifiche servizi speciali
023500090209     D SS35            S             35    DIM(100)
023600090209     D SS              S              3    DIM(100)
023700090209     D DSS             S             90    DIM(100)
023800090209      * Schiere x routine di conversione CAP
023900090209     D CAP5            S              1    DIM(5)
024000090209     D CAP9            S              1    DIM(9)
024100090209     D CAPM            S              1    DIM(9)
024200090209      * Schiera per memorizzazione FTX ricevuti
024300090209     D QUA             S              3    DIM(100)
024400090209     D FTX             S             70    DIM(100)
024500090209      * Tabelle capiconto
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
025600090209     D kKCC            s                   LIKE(ACOKCC)
025700090209     D kKSC            s                   LIKE(ACOKSC)
025800090209     D kAAA            s                   LIKE(NUFAAA)
025900090209     D kCNU            s                   LIKE(NUFCNU)
026000090209     D kFIL            s                   LIKE(NUFFIL)
026100090209     D kAAS            s                   LIKE(VABAAS)
026200090209     D kLNP            s                   LIKE(VABLNP)
026300090209     D kNRS            s                   LIKE(VABNRS)
026400090209     D kNSP            s                   LIKE(VABNSP)
026500090209     D kCMR            s                   LIKE(VABCMR)
026600090209     D kCCM            s                   LIKE(VABCCM)
026700090209     D kNCD            s                   LIKE(VADNCD)
026800090210     D kCNT            s                   LIKE(VADCNT)
026900090209     D kNMC            s                   LIKE(RDENMC)
027000090209     D kNRP            s                   LIKE(RDENRP)
027100090209     D kLNP1           s                   LIKE(RDELNP)
027200090209      *
027300090209      *  Invio E-mail
027400090209     D Indirizzi       s            253
027500090209     D Oggetto         s             44
027600090209     D Messaggio       s           5000
027700090209      *
027800090209     d   DSmail        ds
027900090209     D Mail_Ind                      44
028000090209     d   IND_char                     1    dim(44)
028100090209      *
028200090209     D Lun_Ind         s              5u 0
028300090209      *
028400090209     D Dac             s              5u 0
028500090209     D Luc             s              5u 0
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
030800110527     D Position        S              5I 0
030900110527     D daPos           S              5I 0
031000090209      *
031100090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
031200090209      *
031300131001     d bart_it         C                   CONST('@Brt.it;')
031400131001     d CED_Bart        C                   CONST('CED@Brt.it;')
031500060612      * ?================================================================== */
031600060612      * ?   * Campi x decodifica * (INPUT  del Record)
031700090130     D  Dati           s           2048
031800060612      * ?   *--------------------------------------------------------------*
031900060612     D  se_errore      s              1    inz(' ')
032000060612      * ?* ------------------------------------------------------ *
032100060710     D Digits          C                   '0123456789'
032200091019     D*------------------
032300091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
032400091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
032500110527     D*------------------
032600110527     D allChars        c                   const('QWERTYUIOPASDFGHJKLZXCVBNMqwe-  caratteri
032700110527     D                                     rtyuiopasdfghjklzxcvbnm0123456789/*--  accettati
032800110527     D                                     +\|!"£$%&/()=''?ì+*èéòçà°ù§-.:_,;<>[-
032900110527     D                                     ]@ ')
033000060612      * ?================================================================== */
033100060612      *   Ciclo principale
033200090205      * ?================================================================== */
033300060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
033400060612     C*
033500060612     C                   if        not %open(tivin00r)
033600060612     C                   open      tivin00r
033700060612     C                   endif
033800130930
033900130930     C                   EXSR      AZZMSG
034000130930
034100130930     c     *start        setll     Tivin00r
034200130930     c                   exsr      Legge_TIVIN
034300130930      *.
034400130930     c                   dow       not %eof(Tivin00r)
034500131001
034600130930      * solo i record sflaggati da rielaborare
034700130930      *  Sempre Record OK  poi  Controlli formali sui campi
034800130930     c                   IF        vinFLG = *blank and vinDTA <> *blank
034900130930      **
035000131001     C                   eval      vinFLG = '1'
035100131001     c                   clear                   se_errore
035200131001
035300130930      **  Normalizza il Segmento eliminando eventuali caratteri strani
035400130930      **   e decodifica di quale Segmento di tratta
035500130930      * ?              /*---------------------- */
035600130930     c                   exsr      Normaliz_Segm
035700130930      * ?              /*---------------------- */
035800130930      **
035900130930      *  per i SEGMENTI non utilizzati e NON presenti sulle MAPPATURE
036000130930      *   evitiamo di farli elaborare inutilmente
036100130930      *
036200130930     c                   if           tipo_seg <>'TMD'
036300130930     c                             and
036400130930     c                                tipo_seg <>'DIM'
036500131001     c                             and
036600131001      * ininfluenti ai fini della traduzione anche se MAPPATI
036700131001     c                                tipo_seg <>'UNA'
036800131001     c                             and
036900131001     c                                tipo_seg <>'CNT'
037000131001     c                             and
037100131001     c                                tipo_seg <>'LOC'
037200131001     c                             and
037300131001     c                                tipo_seg <>'TSR'
037400130930      *
037500130930      ** Decodifica record a campi variabili
037600130930      * ?              /*---------------------- */
037700130930     c                   exsr      Decod_Segmento
037800130930      * ?              /*---------------------- */
037900131001     c                   end
038000131001
038100131001      *** se non è riconosciuto l'UNB deve uscire IMMEDIATAMENTE
038200131001      *    e tutto il msg è dato x errato
038300131001     c                   if        se_errore ='S'  and
038400131001     c                             tipo_seg = 'UNB'
038500131001     c                   eval      err_bloccante = 'S'
038600131001     c                   exsr      Msg_errato
038700131001     c                   LEAVE
038800131001     c                   endIF
038900131001      **
039000130930     c                   endIF
039100131001      *.
039200131001     c                   exsr      AGGIOR_TIVIN
039300130930     c                   exsr      Legge_TIVIN
039400130930     C                   ENDdo
039500130930      **
039600060612     C                   if        %open(tivin00r)
039700060612     C                   close     tivin00r
039800060612     C                   endif
039900131001
040000131001      *  x errore bloccante nella composizione del VAB/VAT
040100131001     c                   if        err_bloccante ='S'
040200131001     c                   eval      esito  = '2'
040300131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON '
040400131001     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
040500131001     C                             Bolle AMAZON import in filiale - messaggio -
040600131001     C                             con UNB non presente in tabella PT controlla-
040700131001     C                             re EDI ricevuto su UPLOAD (AMAZON)'
040800131001     c                   exsr      invio_mail_AB
040900131001     c                   end
041000060612      *
041100060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
041200090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
041300090225     c                             esito ='0'
041400060710     C                   eval      esito ='0'
041500090225      *
041600090225     c                   COMMIT
041700090224      *
041800090213     c                   if        almeno_un_due ='S'
041900090213     C                   eval      esito ='1'
042000060614     C                   endif
042100090213     C                   endif
042200060614      *
042300060612     c                   SETON                                        LR
042400130930      * ?------------------------------------------------------------------ */
042500130930      *?     LEGGE  il   TIVIN
042600130930      * ?------------------------------------------------------------------ */
042700130930     c     Legge_TIVIN   Begsr
042800130930      *.
042900130930     c                   read      tivin00r
043000130930     c                   if        %Eof(tivin00r)
043100130930     c                   move      'S'           EoFTivin
043200130930     c                   end
043300130930      *.
043400130930     c                   endSR
043500130930      * ?================================================================== */
043600130930      *?     AGGIORNA    TIVIN solo se NON EoF
043700130930      * ?------------------------------------------------------------------ */
043800130930     c     AGGIOR_TIVIN  Begsr
043900130930      *.
044000130930     c                   if        EoFTivin = ' '
044100130930     c                   update    tivin000
044200130930     c                   end
044300130930      *.
044400130930     c                   endSR
044500130930      * ?------------------------------------------------------------------ */
044600130930      *?  Controlla i caratteri sul segmento x evitare caratteri strani
044700130930      *?  Elimina i caratteri che possono provocare problemi alla traduzione
044800130930      * ?------------------------------------------------------------------ */
044900130930     c     Normaliz_Segm Begsr
045000130930      **
045100130930      *  Caratteri non previsti
045200130930     c                   z-add     1             dapos
045300130930     c     Cerca         tag
045400130930     c     allChars      checK     VINdta:daPos  position
045500130930     C                   if        %Found
045600130930     c                   eval      %subst(VinDTA:position:1)=' '
045700130930     c                   eval      dapos = Position +1
045800130930     c                   goto      Cerca
045900130930     c                   endIf
046000130930      *
046100130930     c                   clear                   dati
046200130930     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
046300130930     c                   eval      tipo_seg = %subst(dati:1:3)
046400130930     c                   eval      segmento = dati
046500130930      *
046600131001      *
046700131001      *    Scarica il dettaglio precedente SU VAB E VAT
046800131001      *   e SALTA solo il Primo GIRO seguente la TESTATA
046900131001     c                   if        tipo_seg = Segmento_Inizio_Dettaglio
047000131001     c                               or
047100131001     c                             tipo_seg = Fine_Ultimo_Dettaglio
047200131001      *
047300131001     c                   if        siamo_in_Dettaglio = 'S'
047400131001      *  PRIMA di resettare tutto x un nuovo dettaglio
047500131001      * ?              /*---------------------- */
047600131001     c                   exsr      Importa_SPED
047700131001      * ?              /*---------------------- */
047800131001     C                   else
047900131001      **   Serve per determinare che è finita la TESTATA
048000131001      **   ed inizia il Dettaglio
048100131001     c                   eval        siamo_in_Dettaglio ='S'
048200131001     c                   end
048300131001      *
048400131001     c                   end
048500131001      *
048600130930     c                   endSR
048700130930      * ?------------------------------------------------------------------ */
048800130930      *?    Decodifica il segmento ed importa i campi in formato DS
048900130930      * ?------------------------------------------------------------------ */
049000130930     c     Decod_SegmentoBegsr
049100110503      *
049200100120     c                   clear                   seg_imprevisto
049300100720     c                   clear                   trovato_seg
049400100716      *
049500100716     C                   clear                   keyUNBCLI
049600100716     C                   clear                   keyTIPOMSG
049700100716     C                   clear                   keyVERSION
049800100716     C                   clear                   keyRELEASE
049900100716     C                   clear                   keyAGENCY
050000100716     C                   clear                   keyASSOCIA
050100110503      *
050200100720      *  Chiamata generica x decodificare il singolo segmento letto
050300100720     c                   call      'TRTCT00R1'
050400100720      * input
050500100720     c                   parm                    tipo_seg
050600100720     c                   parm                    segmento
050700100720     C                   parm                    keyUNBCLI
050800100720     C                   parm                    keyTIPOMSG
050900100720     C                   parm                    keyVERSION
051000100720     C                   parm                    keyRELEASE
051100100720     C                   parm                    keyAGENCY
051200100720     C                   parm                    keyASSOCIA
051300100720      *output
051400100720     c                   parm                    Errore
051500100720     c                   parm                    DsGenerica
051600100720     c                   parm                    Valori_inErr
051700100720     c                   parm                    Descr_Errore
051800100720     c                   parm                    trovato_seg
051900100720      **--------
052000100720      *  Ritorna la DS GENERICA oppure l'errore
052100100120      **--------
052200100120     c                   select
052300130930      **--------
052400100720     c                   when      trovato_seg ='N'
052500100720     c                   move      'S'           seg_imprevisto
052600100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
052700100720     c                   exsr      Error_DET
052800131001     c                   LEAVEsr
052900130930      **--------
053000130930     c                   when      tipo_seg = 'UNA'
053100100720     c                   movel(p)  DsGenerica    EDS00UNA
053200100720      **--------
053300100120     c                   when      tipo_seg = 'UNB'
053400100720     c                   movel(p)  DsGenerica    EDS00UNB
053500100120     C                   EXSR      DATI_UNB
053600100120      * --------
053700100120     c                   when      tipo_seg = 'UNH'
053800100720     c                   movel(p)  DsGenerica    EDS00UNH
053900100120      * --------
054000100120     c                   when      tipo_seg = 'BGM'
054100100720     c                   movel(p)  DsGenerica    EDS00BGM
054200130930     C                   EXSR      DATI_BGM
054300130930      **--------
054400130930     c                   when      tipo_seg = 'CNT'
054500130930     c                   movel(p)  DsGenerica    EDS00CNT
054600130930      * --------
054700130930     c                   when      tipo_seg = 'CUX'
054800130930     c                   movel(p)  DsGenerica    EDS00CUX
054900130930     C                   EXSR      DATI_CUX
055000100120      **--------
055100100120     c                   when      tipo_seg = 'DTM'
055200100720     c                   movel(p)  DsGenerica    EDS00DTM
055300100120     C                   EXSR      DATI_DTM
055400130930      * --------
055500130930     c                   when      tipo_seg = 'LOC'
055600130930     c                   movel(p)  DsGenerica    EDS00LOC
055700130930      * --------
055800130930     c                   when      tipo_seg = 'TSR'
055900130930     c                   movel(p)  DsGenerica    EDS00TSR
056000100120      **--------
056100100120     c                   when      tipo_seg = 'TOD'
056200100720     c                   movel(p)  DsGenerica    EDS00TOD
056300100120     C                   EXSR      DATI_TOD
056400100120      **--------
056500100120     c                   when      tipo_seg = 'RFF'
056600100720     c                   movel(p)  DsGenerica    EDS00RFF
056700100120     C                   EXSR      DATI_RFF
056800100120      **--------
056900100120     c                   when      tipo_seg = 'MOA'
057000100720     c                   movel(p)  DsGenerica    EDS00MOA
057100100120     C                   EXSR      DATI_MOA
057200100120      **--------
057300100120     c                   when      tipo_seg = 'FTX'
057400100720     c                   movel(p)  DsGenerica    EDS00FTX
057500100120     C                   EXSR      DATI_FTX
057600100120      **--------
057700100120     c                   when      tipo_seg = 'NAD'
057800100720     c                   movel(p)  DsGenerica    EDS00NAD
057900100120     C                   EXSR      DATI_NAD
058000100120      **--------
058100100120     c                   when      tipo_seg = 'CTA'
058200100720     c                   movel(p)  DsGenerica    EDS00CTA
058300100120     C                   EXSR      DATI_CTA
058400100120      **--------
058500100120     c                   when      tipo_seg = 'COM'
058600100720     c                   movel(p)  DsGenerica    EDS00COM
058700100120     C                   EXSR      DATI_COM
058800100120      **--------
058900100120     c                   when      tipo_seg = 'GID'
059000100720     c                   movel(p)  DsGenerica    EDS00GID
059100100120     C                   EXSR      DATI_GID
059200100120      **--------
059300100120     c                   when      tipo_seg = 'MEA'
059400100720     c                   movel(p)  DsGenerica    EDS00MEA
059500100120     C                   EXSR      DATI_MEA
059600100120      **--------
059700100120     c                   when      tipo_seg = 'PCI'
059800100720     c                   movel(p)  DsGenerica    EDS00PCI
059900100120      **--------
060000100120     c                   when      tipo_seg = 'UNT'
060100100720     c                   movel(p)  DsGenerica    EDS00UNT
060200100120     C                   EXSR      DATI_UNT
060300100120      **--------
060400100120     c                   when      tipo_seg = 'UNZ'
060500100720     c                   movel(p)  DsGenerica    EDS00UNZ
060600100120      **--------
060700100120     c                   OTHER
060800100120      **--------
060900100120     c                   move      'S'           seg_imprevisto
061000100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
061100100120     c                   exsr      Error_DET
061200131001     c                   LEAVEsr
061300100120      **--------
061400100120     c                   endSL
061500100120      **
061600131001     c                   endSR
061700090211     C*----------------------------------------------------------------
061800090211     C*? AZZMSG - Azzera dati messaggio
061900090211     C*----------------------------------------------------------------
062000090211     C     AZZMSG        BEGSR
062100090211     C*
062200090211     C* Pulisco dati di work x foglio viaggio
062300090211     C                   MOVEL     *BLANKS       WNUMFV           35
062400090211     C                   MOVEL     *BLANKS       WNRCMR           35
062500090211     C                   Z-ADD     0             WDATPA            8 0
062600090211     C                   Z-ADD     0             WDATFV            8 0
062700090211     C                   Z-ADD     0             WHHNFV            4 0
062800090211     C                   Z-ADD     0             WDTCMR            8 0
062900090211     C                   Z-ADD     0             WHHCMR            4 0
063000090211      *
063100090211      * pulisce gli RFF+FF di testata
063200090211     C                   clear                   $CLKSC            7 0
063300090211     C                   clear                   $CLTAR            3
063400090211     C                   clear                   $CLLNP            3 0
063500090211     C                   clear                   $CLNRS            2 0
063600090211     C                   clear                   $CLCTM            2
063700090211     C                   clear                   $CLBS1            1
063800090211     C                   clear                   $CLfnsp           1            *blk/S
063900090211      *
064000090211     C*  Inizializzo variabile new documento
064100090211     C                   Z-ADD     0             KCL
064200090211     C                   Z-ADD     0             WW                3 0
064300130930      *
064400090211     ** Inizializza campo di work
064500090211     c                   move      'N'           WGID99
064600130930      *
064700090211      * per controllare se almeno un record è stato importato sul VAB
064800090211     c                   clear                   Almeno_Uno        1
064900090213     c                   clear                   Almeno_Un_Due     1
065000090211     c                   eval      esito  = '0'
065100090211     C*
065200090603     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
065300090603     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
065400090603     c                   eval      Dettaglio_NAD_destinatario = *blank
065500090603     c                   eval      NAD_destinatario_in_testata = *blank
065600090603     c                   eval      NAD_mittente_in_testata = *blank
065700131001     c                   eval        siamo_in_Dettaglio = *blank
065800090603     C*
065900090603     c                   clear                   tes_VABrmo
066000090603     c                   clear                   tes_VABnmo
066100090603     c                   clear                   tes_VABcmo
066200090603     C*
066300090603     c                   clear                   tes_VABrsd
066400090603     c                   clear                   tes_VABrd2
066500090603     c                   clear                   tes_VABind
066600090603     c                   clear                   tes_VABlod
066700090603     c                   clear                   tes_VABnzd
066800090603     C*
066900090603     c                   clear                   tes_Valuta
067000131022     c                   clear                   BGM_Id_Testata
067100090603     C*
067200090211     C                   ENDSR
067300090210     C*----------------------------------------------------------------
067400090210      *? DECODIFICA DATI UNB
067500090210     C*----------------------------------------------------------------
067600090210     C     DATI_UNB      BEGSR
067700090210      *
067800090209      *  INIZIALIZZA  dati fondamentali messaggio
067900090209     C                   clear                   EDIDSPT
068000090209     C                   clear                   EDIDSPU
068100090209     C                   clear                   EDIDSPV
068200090209     C                   clear                   wnrDOC           35
068300090209     C                   move      'N'           PT_ok             1
068400090209      ** ---------------------------------------------------------------
068500090209      * quindi 1° cosa:
068600090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
068700090209      *     senza questo il messaggio NON è trascodificabile
068800090209     C                   MOVEL     unb0004       WCOD             35
068900090209     C                   MOVE      unb000720     WCOD
069000090210     C                   MOVEl(p)  Wcod          UNB_Mittente     35
069100090209     C                   Z-ADD     1             XX                3 0
069200090209     C     WCOD          LOOKUP    CPT(XX)                                32
069300090209      *
069400090209      *  Se non l'ha trovato ...Errore  x messaggio NON riconosciuto
069500090209     C                   If        *IN32 = *off
069600090209      * ******  Errore generale sul Messaggio
069700090209      * Msg di allerta Traduzione
069800090209     C                   EVAL      Indirizzi = CED_Bart
069900131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON'
070000090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
070100090209     C                             Bolle import in filiale - messaggio -
070200090209     C                             con UNB non presente in tabella PT controlla-
070300090209     C                             re EDI ricevuto su UPLOAD.'
070400090209      *
070500090209     c                   exsr      invio_mail
070600090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
070700090210     C                   eval      vinFLG = '2'
070800090210     C                   eval      se_errore   = 'S'
070900131001     c                   LEAVEsr
071000090209      *
071100090209     C                   move      'N'           PT_ok             1
071200090209      *
071300090209      *             *------------------------------*
071400090209     C                   eLSe
071500090209      *             *------------------------------*
071600090209      * Se tutto OK su tab.PT:
071700090209     C                   move      'S'           PT_ok             1
071800090209      *
071900090209     C                   MOVEL     DPT(XX)       EDIDSPT
072000090209     C                   MOVEL     DPU(XX)       EDIDSPU
072100090209     C                   MOVEL     DPV(XX)       EDIDSPV
072200091016     c                   clear                   Mittente         35
072300091016     c                   clear                   Mitt_Qualifier    4
072400091016     c                   clear                   Id_Messaggio     14
072500090226     C                   eval      Mittente       = unb0004
072600090226     C                   eval      Mitt_qualifier = unb000720
072700131001     C                   eval      Id_Messaggio   = UNB0020
072800090209      *
072900090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
073000090209      *  ricevuti:
073100090209     c                   movel     §PVema        mail_ind         44
073200090209      *
073300090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
073400090209     c                   if        mail_ind <> *blank
073500090209     c                   exsr      imp_ADDR_mail
073600090209     c                   end
073700090209      *
073800090209      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
073900090209      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
074000090209      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
074100090209     C                   if        §PULUN = *blank
074200090209     C                   eval      §PULUN = '00'
074300090209     c                   end
074400090209      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
074500090209     C                   move      §puLUN        WVATlun           2 0          *0/>0
074600090209      *
074700090209     C                   Z-ADD     §PTKSC        WKSC              7 0
074800090211     C                   MOVEL     §PTLNP        VABlnp_PT
074900090212     C                   MOVEL     §PTNRS        VABnrs_PT
075000090211     C                   MOVEL     §PTCTM        VABctm_PT
075100090209      *
075200090209      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
075300090209      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
075400090209      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
075500090209      *  i records trasmessi).
075600090211     C                   MOVEL     §puNSP        VABfnsp_PT        1            *blk/S
075700090209      *
075800090209      *  In base al cod.trattamento merce reperisco tipo tratt.
075900090209     C                   CLEAR                   DS1B
076000090209     C                   Z-ADD     1             Y                 3 0
076100090211     C     vabctm_PT     LOOKUP    CTM(Y)                                 32
076200090209     C     *IN32         IFEQ      '1'
076300090209     C                   MOVEL     DTM(Y)        DS1B
076400090209     C                   MOVEL     DS1B          WSVTRM           38
076500131002     C                   MOVEL     DS1B          savDS1B
076600090209     C                   END
076700090209      *
076800090209      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
076900090209     C                   clear                   ACORAG
077000090209     C                   clear                   INDCAE
077100090209     C                   clear                   INDCAP
077200090209     C                   clear                   INDSTA
077300090209      * Decodifico partner
077400090209     C                   Z-ADD     1             KKUT
077500090209     C                   Z-ADD     KCI           KKCC
077600090209     C                   MOVE      §PTKSC        KKSC
077700090209     C     KACO          CHAIN     CNACO00F                           31
077800090209     C     KIND          CHAIN     CNIND00F                           31
077900090211      *
078000090211      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
078100090211      *   questa schiera è utilizzata anche per i clienti.
078200090211     C     §ptKSC        LOOKUP    KCL                                    39
078300090211     c                   If        *in39 = *off
078400090211     C                   add       1             WW
078500090211     C                   z-add     §ptKSC        KCL(WW)
078600090211     C                   Endif
078700090209      *
078800090209     C                   MOVEL     unb0017       WAA4              4 0
078900090209     C                   MOVE      WAA4          WAA2              2 0
079000090209     C                   MOVE      unb0017       WGG               2 0
079100090209     C                   MOVE      unb0017       WDTPRE            8 0
079200090209     C                   MOVE      unb0019       WHMPRE            4 0
079300090209     C                   MOVE      unb0017       WDTMSG            6 0
079400090209     C                   MOVE      WAA2          WDTMSG
079500090209     C                   MOVEL     WGG           WDTMSG
079600090210      * potremmo
079700090210      * Utilizzare l'identificativo della trasmissione come CMR
079800131001     C                   IF        unb0020 <> *blank
079900131001     C                   MOVEL     unb0020       WNRDOC
080000131001     C                   MOVEL     unb0020       WNUMFV
080100131001     C                   MOVEL     unb0020       WNRCMR
080200090209     c                   end
080300090209      *
080400090209     c                   Endif
080500131001      *
080600090210     c     end_UNB       endSR
080700130930      * ?================================================================== */
080800130930     C*? INIZIO Messaggio
080900130930      * ?================================================================== */
081000130930     C     DATI_BGM      BEGSR
081100130930      *
081200130930      *    Potrebbe essere x AMAZON il Nr.CMR ... per ora lasciamo al UNB
081300130930      *     decodificare il numero di CMR
081400131022      *
081500131022      *  identificativo testata  lo riporteremo x AMAZON
081600131022     c                   if        BGM1004 <> *blank
081700131022     c                   eval          BGM_Id_Testata = BGM1004
081800131022     c                   end
081900130930      **
082000130930     c     end_BGM       endSR
082100130930      * ?================================================================== */
082200130930     C*?   CUX   Currency in testata x eventuali importi sui Dettagli
082300130930      * ?================================================================== */
082400130930     C     DATI_CUX      BEGSR
082500130930      *
082600130930      * se presente la valuta la imposta dalla testata
082700130930     c                   if           CUX6345 <> *blank
082800130930     c                   eval      tes_Valuta = CUX6345
082900130930     c                   end
083000130930      **
083100130930     c     end_CUX       endSR
083200090601      * ?================================================================== */
083300090601     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
083400090601      * ?================================================================== */
083500090601     C     DATI_DTM      BEGSR
083600090601     C*
083700130930     C*  Se data elaborazione la uso e memorizzo x il CMR
083800130930     C     dtm2005       IFEQ      '9'
083900090209     C                   MOVEL     dtm2380       WDATA            35            Data
084000090209     C                   MOVEL     dtm2379       WFORM             3            Formato
084100090209     C                   EXSR      CNVDAT
084200090209     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
084300090209     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
084400090211     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
084500090211     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
084600090209     C     WERRO         IFEQ      'S'
084700090210     C                   eval      vinMSG = 'DTM data errata'
084800090213     c                   exsr      Error_DET
084900131001     c                   LEAVEsr
085000090209     C                   END
085100090209     C                   END
085200090209      **
085300090209     C*  Se data consegna richiesta imposto già sui campi del VAB
085400110523     C     dtm2005       IFEQ      '2'
085500090209     C                   MOVEL     dtm2380       WDATA            35            Data
085600090209     C                   MOVEL     dtm2379       WFORM             3            Formato
085700090209     C                   EXSR      CNVDAT
085800090209     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
085900090209     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
086000090209     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
086100090209     C     WERRO         IFEQ      'S'
086200090210     C                   eval      vinMSG = 'DTM data errata'
086300090213     c                   exsr      Error_DET
086400131001     c                   LEAVEsr
086500090209     C                   END
086600090209     C                   END
086700090211      **
086800090211     C*  Se data PARTENZA richiesta imposto già sui campi del VAB
086900090603     C     dtm2005       IFEQ      '10'
087000090211     C                   MOVEL     dtm2380       WDATA            35            Data
087100090211     C                   MOVEL     dtm2379       WFORM             3            Formato
087200090211     C                   EXSR      CNVDAT
087300090211     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.  ch.
087400090211     C     WERRO         IFEQ      'S'
087500090211     C                   eval      vinMSG = 'DTM data errata'
087600090213     c                   exsr      Error_DET
087700131001     c                   LEAVEsr
087800090211     C                   END
087900090211     C                   END
088000090209      **
088100131001     c                   endSR
088200090209      * ?================================================================== */
088300090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
088400090209      * ?================================================================== */
088500090209     C     DATI_TOD      BEGSR
088600131001      *
088700131001      *  in AMAZON è usato solo in testata e non in dettaglio delle spedizioni
088800131001      *     NEL DETTAGGLIO si usa il FTX+AAR <--
088900131001      *
089000090210      *  Trascodifico tipo trasporto
089100090210     c                   clear                   TOD_char3         3
089200090210      *
089300090210      *   Imposta o il codice o il metodo
089400090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
089500131001     c                   if        tod4053  <> *blank
089600131001     c                   movel(p)  tod4053       TOD_char3
089700131001     c                   else
089800131001      *
089900131001      *-  Amazon utilizza --->  solo questo  <---
090000090210     c                   if        tod4215  <> *blank
090100090210     c                   movel(p)  tod4215       TOD_char3
090200090210     c                   end
090300131001      *
090400131001     c                   end
090500110328      ****************
090600110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
090700131001      *****    seguendo gli standard EDIFACT es.: PP (PrePaied) CC (Collect)
090800110328      ****************
090900110328     c                   eval      Trovata_Tab_TB ='N'
091000110328      *
091100110328     c                   if              TOD_char3 <> *blank
091200110328     c                   movel(p)  TOD_char3     Key_Generica35
091300110328     c                   move      §puTB         Key_Generica35
091400110328     C                   eval      Y = 1
091500110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
091600110328     c   32              eval      Trovata_Tab_TB ='S'
091700110328      *
091800131001      * PUO' APPARIRE STRANO fare in seguito questo altro LOOKUP:
091900131001      *   ma, per esperienza, meglio lasciarlo perchè
092000131001      *  Se non ha trovato con il campo 4053 è possibile che abbiano usato
092100110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
092200110328      *
092300110328     c                   if          Trovata_Tab_TB ='N'
092400110328     c                                 and
092500110328     C                               tod4215 <> *blank
092600110328     c                   movel(p)  tod4215       TOD_char3
092700110328     c                   movel(p)  TOD_char3     Key_Generica35
092800110328     c                   move      §puTB         Key_Generica35
092900110328     C                   eval      Y = 1
093000110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
093100110328     c   32              eval      Trovata_Tab_TB ='S'
093200110328     c                   end
093300131001      *
093400110328     c                   end
093500110328      *
093600110328      ****************
093700110328      ******* Se vi sono eccezioni sul cliente imposta quelle
093800110328      ****************
093900110328     c                   If          Trovata_Tab_TB ='S'
094000110328      *
094100110328     c                   SELECT
094200110328     c                   WHEN      Decod_Porto(Y) = 'F'
094300110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
094400110328      *  Porto Assegnato
094500110328     C                   OTHER
094600110328     C                   movel     '2'           VABCBO                         Senza C/Ass
094700110328     C                   ENDSL
094800110328      *
094900110328     c                   elseIf      Trovata_Tab_TB ='N'
095000090209      **
095100131001      **
095200110328     c                   endIF
095300110328      **
095400090210      **  Il tipo bolla non è presente
095500090210     c                   if        VABCBO = *blank
095600090210     C                   eval      vinMSG = 'TOD non rilevato'
095700090213     c                   exsr      Error_DET
095800131001     c                   LEAVEsr
095900090210     c                   end
096000090210      **
096100131001     c                   endSR
096200090210      * ?================================================================== */
096300090210     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
096400090210      * ?================================================================== */
096500090210     C     DATI_RFF      BEGSR
096600131001      *
096700131001      *  I riferimenti devono essere presi SOLO nei dettagli e non in TESTATA
096800131001     c                   if          siamo_in_Dettaglio = *blank
096900131001     c                   LEAVEsr
097000131001     c                   end
097100131001      *
097200131001      * ?i RIFERIMENTI x AMAZON sono MOLTO STRANI
097300131001      * ?e DEVONO ESSERE PRESI solo in DETTAGLIO.
097400131001      *
097500131001      * ?x AMAZON passano nel riferimento il SEGNACOLLO.
097600131001      * ?   invece di passarlo sul PCI come sarebbe da Standard dei messaggi IFTMIN
097700090210      *
097800090210     C                   clear                   WAGE             35
097900110224     C                   clear                   WFF              35
098000110224     C                   clear                   WCU              35
098100131001     C                   clear                   WBarcode         35
098200090210      *
098300090210      *  Trascodifico riferimento spedizione
098400091019     c                   if        RFF1153 = Riferimento_Spedizione  and
098500091019     c                             RFF1154 <> *BLANKS
098600090210     C                   movel(p)  RFF1154       WAGE             35
098700090210     c                   end
098800090210      **
098900090210     C                   IF         WAGE <> *BLANKS
099000090210     C                   movel     WAGE          WNSP             35
099100090210     C                   movel     WAGE          WNUM             35
099200090210     C     ' '           scan      WAGE          WLUNGH            3 0
099300090210     C                   sub       1             WLUNGH
099400090210     C                   EXSR      TSTNUM
099500090210     C                   IF         WOKNUM = 'S'                                Ctrl numerico
099600090210     C                   movel     WAGE          VABRMA
099700091019     C                   IF         WRMN = Riferimento_Spedizione  or
099800091019     C                              WRMN = *blanks
099900090210     C                   movel     WNUMOK        VABRMN
100000091019     C                   eval       WRMN = Riferimento_Spedizione
100100090210     C                   ENDIF
100200090210     C                   ENDIF
100300090210     C                   ENDIF
100400090210      *
100500131001     C*----------------------------------------------------------------
100600131002      * Riferimento TELEFONICO viene fatto con un RFF+TE anzichè il segmento COM
100700131002      *
100800131002     c                   if        RFF1153 = Riferimento_Telefonico  and
100900131001     C                             RFF1154 <> *blanks
101000131002     C                   movel     RFF1154       wattel
101100131001     C                   END
101200131002      *
101300131002      *--------------------------------------------------
101400140304      * il NUMERICO deve essere uguale al SEGNACOLLO
101500140304      * era così all'inizio; cambiato a Marzo 2014 per gestione del segnacollo
101600140304      * multiplo si è quindi deciso di ricevere il numerico come RFF+TB
101700140304      * ed invece tenere RFF+CR con righe in sequenza che contengono i segnacolli.
101800131002      *--------------------------------------------------
101900131002     c                   if        RFF1153 = Riferimento_Numerico    and
102000131002     C                             RFF1154 <> *blanks
102100131002     C                   movel     RFF1154       WCU
102200131002     c                   z-add     0             pos               3 0
102300131002     c                   eval      pos = %Check(digits:%trim(WCU))
102400131002     c                   if        pos > 0
102500131002      *  Va avanti però:
102600131002      *   deve segnalare la presenza di caratteri ALFA
102700131002     C                   eval      vinMSG = 'ATTENZ.:Caratteri ALFA -
102800131002     c                              sul RMN/BARCODE'
102900131002     c                   end
103000131002     C                   END
103100131002      *
103200131002      * Riferimento  solo NUMERICO
103300131002      *  quindi converte le lettere in numeri
103400131001     C     WCU           IFNE      *BLANKS
103500131001     C                   movel     WCU           WNUM             35
103600131001     C     ' '           scan      WCU           WLUNGH
103700131001     C                   sub       1             WLUNGH
103800131001     C                   EXSR      TSTNUM
103900131001     C                   If          WOKNUM = 'S'                               Ctrl numerico
104000131001     C                   movel     WNUMOK        VABRMN
104100131001     c                   eval      WRMN = Riferimento_Numerico
104200131001     C                   Endif
104300131001     C                   ENDIF
104400131001      *
104500131001     C*----------------------------------------------------------------
104600131001      * ? x AMAZON il SEGNACOLLO è passato come RIFERIMANTO
104700140304      * ?   da Marzo 2014 si passa da 1 fisso a Multicollo.
104800131001      *
104900131001     c                   if        RFF1153 = Riferimento_x_Segnacollo and
105000131001     C                             RFF1154 <> *blanks
105100131001     C                   movel     RFF1154       WBarcode
105200140304      *----- gestione Multicollo
105300140304      *-----   qui si calcola anche il Nr.Colli della spedizione invece che dal GID
105400140304     c                   exsr      SEGNACOLLO
105500140304      *-----
105600131001     C                   END
105700131001      *
105800131001     C*----------------------------------------------------------------
105900131001      *
106000131001      * ?NON ATTIVO x AMAZON ma lasciato il meccanismo se servisse in seguito
106100090210      * Codice Cliente di riferimento
106200091016     C                   if        RFF1153 = 'FF ' and RFF1154 <> *BLANKS
106300090210     C                   movel     RFF1154       WFF
106400090210     c                   END
106500090211      *
106600090211      * Se ho FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
106700090211     C     WFF           IFNE      *BLANKS
106800090211     C                   eval      XX = 1
106900090211     C                   movel     §PTKSC        WCCL             35
107000090211     C                   movel     WFF           WCOM28           28
107100090211     C                   move      WCOM28        WCCL
107200090211     C     WCCL          lookup    CCL(XX)                                34
107300090211     C     *IN34         IFEQ      '1'
107400090211     C                   movel     DCL(XX)       EDIDSCL
107500090211     C                   z-add     §CLKSC        WCLKSC            7 0
107600090211     C                   movel     §CLTAR        WCLTAR            3
107700090211     C                   z-add     §CLLNP        WCLLNP            3 0
107800090211     C                   z-add     §CLNRS        WCLNRS            2 0
107900090211     C                   movel     §CLCTM        WCLCTM            2
108000090211     C                   movel     §CLBS1        WCLBS1            1
108100090211     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
108200090211      * imposta la LNP   x il dettaglio specifco
108300090211     c                   eval      VABlnp = §CLlnp
108400090211      * imposta la Serie x il dettaglio specifco
108500090211     c                   eval      VABnrs = §CLnrs
108600090211      *
108700090211      *  In base al cod.trattamento merce reperisco tipo tratt.
108800090211      *    per un dettaglio specifico.
108900090211     C                   CLEAR                   DS1B
109000090211     C                   Z-ADD     1             Y                 3 0
109100090211     C     §CLctm        LOOKUP    CTM(Y)                                 32
109200090211     C     *IN32         IFEQ      '1'
109300090211     C                   MOVEL     DTM(Y)        DS1B
109400090211     C                   END
109500090211      *
109600090211      * Se il cliente non era sulla schiera lo aggiunge
109700090211     C     §CLKSC        lookup    KCL                                    39
109800090211     C                   if        *in39 = *off
109900090211     C                   add       1             WW
110000090211     C                   z-add     §CLKSC        KCL(WW)
110100090211     c                   endif
110200090211      *
110300090211     C                   ENDIF
110400090211     C                   ENDIF
110500131001     C*----------------------------------------------------------------
110600131001      *
110700131001      * NOn c'è l'identificativo bolla del cliente
110800131001     c                   if        (RFF1153 = Riferimento_Spedizione and
110900131001     c                              RFF1154 = *BLANKS)
111000131001     c                             or
111100131001     c                             (RFF1153 = Riferimento_Numerico and
111200131001     c                              RFF1154 = *BLANKS)
111300131001     C                   eval      vinMSG = 'RFF riferimento assente'
111400131001     c                   exsr      Error_DET
111500131001     c                   LEAVEsr
111600131001     C                   ENDIF
111700131001      *
111800131001     c                   endSR
111900090209      * ?================================================================== */
112000090213     C*?  C.O.D. Monetary Amount
112100090209      * ?================================================================== */
112200090213     C     DATI_MOA      BEGSR
112300131001      *
112400131001      *    ATTENZIONE AL SEPARATORE DEi DECIMALI
112500131001      *
112600131001      *
112700090210      *
112800090216     C* Controllo se campo importo numerico
112900090216      * con 3 decimali
113000090216     C     moa5004       IFNE      *zeros
113100090216      *
113200131001      *  importo da sssicurare è tabella sulla PT
113300131001     c                   if        %subst(§PTxxx:1:1) = 'S'
113400131001     C     moa5025       IFEQ      '141'
113500131001     C     moa5004       div       100           VABIAS                         IMPORTO
113600130930     C                   MOVEL     tes_Valuta    VABVAS                         ASSICURATO
113700090216     C                   END
113800131001     c                   end
113900131001      *
114000131001      * NON è da importare il VALORE MERCE DICHIARATO
114100131001     C                   goto      Non_Fare                                     Valore
114200131001      *
114300131001     C     moa5025       IFEQ      '40 '                                        Valore
114400131001     C     moa5004       div       100           VABVMD                         MERCE
114500130930     C                   MOVEL     tes_Valuta    VABVAD                         DICHIARATO
114600090216     C                   END
114700131001     c     Non_Fare      tag
114800090216      *
114900131001      *  importo da C.O.D. è tabella sulla PT
115000090216     C     moa5025       IFEQ      '22 '                                        C/Assegno
115100090216     C     §PUCAS        IFEQ      'S'                                          C/Assegno
115200131001     C     moa5004       div       100           VABCAS
115300130930     C                   MOVEL     tes_Valuta    VABVCA
115400090216      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
115500090216      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
115600090216     C     §PUTPC        IFEQ      'P'
115700090216     C     moa5004       ANDGT     *ZEROS
115800090216     C                   MOVEL     'OM'          VABTIC
115900090216     C                   MOVEL     'OM'          WVABTIC
116000090216     C                   ENDIF
116100090216     C                   END
116200090216     C                   END
116300090216      *
116400090216     C                   END                                                    Imp.non numer.
116500090213      *
116600090213     c     end_MOA       endSR
116700090213      * ?================================================================== */
116800090213     C*?  FREE TEXT
116900090213      * ?================================================================== */
117000090213     C     DATI_FTX      BEGSR
117100110404      *
117200131001      * ?x le Delivery Instruction  DIN a livello di TESTATA ????????????
117300131001      * ?   NON VI SONO NOTE IN DETTAGLIO x AMAZON e non verranno generate!!!!
117400130930     C                   IF        ftx4451 = 'DIN'                              -- 01 --
117500090216     C                   DO        4             X
117600090216     C     D05(X)        IFNE      *BLANKS
117700090216     C     WNOT1         IFEQ      *BLANKS
117800090216     C                   MOVEL     D05(X)        WNOT1
117900090216     C                   MOVE      D05(X)        WNOT2
118000090216     C                   ELSE
118100090216     C     WNOT2         IFEQ      *BLANKS
118200090216     C                   MOVEL     D05(X)        WNOT2
118300090216     C                   END
118400110404     C                   ENDif
118500110404     C                   ENDif
118600110404     C                   ENDdo
118700110404     C                   ENDif
118800131001      *
118900131001      * ?x AMAZON i TERMS of DELIVERIES  sul dettaglio sono sul
119000131001      * ?  segmento FTX nel primo segmento.
119100131001     C                   IF        ftx4451 = 'AAR' and D05(1) <> *blank         -- 01 --
119200131001      *
119300131001     C                   MOVEL     D05(1)        TOD_char3
119400131001     c                   movel(p)  TOD_char3     Key_Generica35
119500131001     c                   move      §puTB         Key_Generica35
119600131001     C                   eval      Y = 1
119700131001     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
119800131001     c   32              eval      Trovata_Tab_TB ='S'
119900131001      *
120000131001     c                   If          Trovata_Tab_TB ='S'
120100131001      *
120200131001     c                   SELECT
120300131001     c                   WHEN      Decod_Porto(Y) = 'F'
120400131001     c                               and  VABcas > *zeros
120500131001     C                   movel     '4'           VABCBO                         + C/Ass
120600131001     c                   WHEN      Decod_Porto(Y) = 'F'
120700131001     C                   movel     '1'           VABCBO                         Senza C/Ass.
120800131001      *  Porto Assegnato
120900131001     c                   WHEN      VABCAS > *zeros
121000131001     C                   movel     '6'           VABCBO                         + C/Ass
121100131001     C                   OTHER
121200131001     C                   movel     '2'           VABCBO                         Senza C/Ass
121300131001     C                   ENDSL
121400131001      *
121500131001     c                   elseIf      Trovata_Tab_TB ='N'
121600131001      **
121700131001      **  Il tipo bolla non è presente
121800131001     c                   if        VABCBO = *blank
121900131001     C                   eval      vinMSG = 'TOD non rilevato'
122000131001     c                   exsr      Error_DET
122100131001     c                   LEAVEsr
122200131001     c                   end
122300131001      **
122400131001     c                   endIF
122500131001      **
122600131001     C                   ENDif
122700131001      *_________________________________________________________________________*
122800131001      * ? AMAZON al momento NON fa i C.O.D. viene lasciato il meccanismo
122900131001      * ?    già impostato se un Domani si dovesse attivare C.O.D. e Tipo Incasso
123000131001      *_________________________________________________________________________*
123100131001      *
123200090216     C*------>>
123300131001     C*-    ->>    ATTENZIONE QUESTA é LA DECODIFICA STANDARD DEL TIPO INCASSO
123400131001     C*-    ->>       AMAZON  NON FA i COD.
123500131001     C*------>>
123600090216     C*  Decodifico tipo pagamento C/Assegno
123700090216      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
123800090216      *      (Problema di pulizia in presenza di + righe di testo)
123900090216      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
124000090216      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
124100090216      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
124200090216      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
124300090216     C                   if        wri_vabtic = *blank
124400090216     C                   MOVEL     *BLANKS       VABTIC
124500090216     c                   end
124600090216      **
124700090216     c                   clear                   trovato_TPI       1
124800090216      *
124900131001      * ?AMAZON NON FA I C.O.D.    quindi  è lasciata x scrupolo un domani
125000131001      * ?si dovesse attivare  -->  c'è il tipo Incasso da decodificare
125100090216     C     ftx4451       IFEQ      'PAI'                                        -- 01 --
125200090216      *
125300090216     C                   DO        4             X                              -- 02 --
125400090216     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
125500130311      **
125600130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
125700130311      **  per gestire Partner come AZKAR che ha codice "AAA"
125800130311     C*******            MOVEL     D05(X)        WCTC              2
125900130311     C                   MOVEL     D05(X)        WCTC             10
126000090216     C                   Z-ADD     1             Y
126100090216     c                   movel(p)  Wctc          fld35a           35
126200090216     c                   move      §puTC         fld35a
126300090216     C     fld35a        LOOKUP    CTC35(Y)                               33
126400090216     C   33              MOVEL     TPI(Y)        VABTIC
126500090216     c   33              move      'S'           trovato_TPI
126600090216     C                   END                                                    -- 03 --
126700090216     C                   ENDdo                                                  -- 02 --
126800090216      *
126900090216     C                   ELSE                                                   -- 01 --
127000090216      *
127100090216      * ?Se NON c'è il tipo Incasso Contrassegno particolare
127200090216      *    Deve essere impostato comunque da testata
127300090216     c                   if        vabtic = *blank
127400090216     C                   MOVEL     wVabTIC       VABTIC
127500090216     c                   end
127600090216      *
127700090216     C                   END                                                    -- 01 --
127800090216      *
127900090216      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
128000090216      *    essere presenti altri record Flat con altre informazioni
128100090216      *    che potrebbero abblancare il Tipo incasso precedentemente
128200090216      *    decodificato
128300090216     c                   if        Trovato_TPI = 'S' and
128400090216     c                                 wri_vabtic = *blank
128500090216     c                   eval      wri_vabtic = 'S'
128600090216     c                   end
128700131001      *_________________________________________________________________________*
128800090213      *
128900090213     c     end_FTX       endSR
129000090213      * ?================================================================== */
129100090213     C*?  Anagrafica del mittente e del destinatario
129200090213      * ?================================================================== */
129300091016     C     DATI_NAD      BEGSR
129400090213      *
129500090210      * Dati destinatario
129600090210     C                   IF        nad3035 = 'CN' or
129700090210     C                             (§PTDES <> *BLANKS  and  §PTDES = nad3035)
129800090227      *
129900090227      * Se trovato un indirizzo di destinatario
130000090227     c                   eval       se_trovato_NAD ='S'
130100090603     c                   eval       Dettaglio_NAD_destinatario = 'S'
130200090210      *
130300090210     C* Reperisco nazione corrispondente destinatario
130400090210     C                   Z-ADD     1             YY                3 0
130500090210     c                   eval      *in32 = *off
130600090210     C                   if        nad3207 <> *BLANKS
130700090210     C     nad3207       LOOKUP    ISO(YY)                                32
130800090210     C                   endif
130900090210      *
131000090210     C* Imposto dati destinatario
131100090211     C                   MOVEL     nad31241      VABRSD
131200090211     C                   if        nad31242 <> *LOVAL
131300090211     C                   MOVEL     nad31242      VABRD2
131400090210     C                   endif
131500131001????  * DAL PARTY NAME
131600091019???? C* Imposto dati destinatario
131700091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
131800131001???? C                   MOVEL     nad30361      VABRSD
131900131001???? C                   if        nad30362 <> *LOVAL
132000131001???? C                   MOVEL     nad30362      VABRD2
132100091019???? C                   endif
132200091019???? C                   end
132300131002      **
132400131002      **  Nel contatto inseriamo lo stesso nome del DESTINATARIO
132500131002     c                   movel     VABRSD        watrde
132600131002      **
132700131001     C                   MOVEL     nad3164       VABLOD
132800090211      *
132900090210     C                   MOVEL     nad30421      VABIND
133000131001      **
133100090210     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
133200090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
133300090210      *
133400131112      *  se Mandano un qualcosa d'altro nell'indirizzo sul (2) campo
133500131112      *   prima provo ad accodarlo nella 2°rag.sociale e se fosse già piena
133600131112      *     allora lo accoda ai dati dell'indirizzo fin dove può.
133700131112     c                   if        vabRD2 = *blank
133800131112???? C                   MOVEL     nad30422      VABRD2
133900131112     c                   else
134000131112     c                   eval      VABIND = %Trim(vabIND) + ' ' + NAD30422
134100131112     c                   end
134200131001      *
134300131001     C                   ENDif
134400131001      * ---------
134500090210     C                   If        *in32 = *on
134600090210     C                   MOVEL     C15(YY)       VABNZD
134700090210     C                   MOVEL     CPF(YY)       WFLCPF            2 0
134800090210     c                   Else
134900090210     C                   MOVEL     *BLANKS       VABNZD
135000090210     C                   Z-ADD     0             WFLCPF
135100090210     c                   Endif
135200090210      * Converto CAP
135300090210     C                   EXSR      CNVCAP
135400090603     C*
135500090603     C* Se in testata
135600131001     c                   if        siamo_in_Dettaglio = *blank
135700090603     c                   eval       tes_VABrsd = VABrsd
135800090603     c                   eval       tes_VABrd2 = VABrd2
135900090603     c                   eval       tes_VABind = VABind
136000090603     c                   eval       tes_VABlod = VABlod
136100090603     c                   eval       tes_VABnzd = VABnzd
136200090603     c                   eval      NAD_destinatario_in_testata = 'S'
136300090603     c                   end
136400090603     C*
136500090210     C                   ENDIF
136600131001      * __________________________________________________
136700131001      * Dati mittente  (Su AMAZON è AMAZON che spedisce)
136800131001      * __________________________________________________
136900131001     C                   IF        nad3035 = 'SF'
137000131001      *
137100131001      *   aggiungo eventuale rag.soc.aggiuntiva
137200131001     C                   movel     nad31241      VABRMO
137300131001     c                   eval      vabRMO = %Trim(VABRMO) + ' ' +
137400131001     C                                 %Trim(nad31242)
137500131001      ***
137600131001     c                   if        VABRMO =*blank and nad30361 <> *blank
137700131001     C                   movel     nad30361      VABRMO
137800131001     c                   eval      vabRMO = %Trim(VABRMO) + ' ' +
137900131001     C                                 %Trim(nad30362)
138000131001     c                   end
138100131001      *
138200131001      *   default PT
138300090210     C                   movel     §PTNAZ        VABNMO
138400131001      *
138500131001      *  Reperisco nazione mittente
138600090210     C                   IF        nad3207 <> *blanks
138700090210     C                   eval      YY = 1
138800090210     C     nad3207       LOOKUP    ISO(YY)                                32
138900090210     C                   if         *in32 = *on
139000090210     C                   MOVEL     C15(YY)       VABNMO
139100090210     C                   endif
139200090210     C                   ENDIF
139300090210      *
139400090210      * Normalizzo CAP mittente originale
139500090210     C                   clear                   T
139600090210     C                   clear                   S
139700090210     C                   MOVEA     *BLANKS       CAPM
139800090210     C                   MOVEA     nad3251       CAP9
139900090210     C                   DO        9             T
140000090210     C     CAP9(T)       IFNE      *BLANKS
140100090210     C                   ADD       1             S
140200090210     C                   MOVE      CAP9(T)       CAPM(S)
140300090210     C                   ENDIF
140400090210     C                   ENDDO
140500090210     C*
140600090210      *     Controllo validità CAP
140700090210     C                   MOVEA     CAPM          VABCMO
140800090210      *
140900090210     C                   CLEAR                   TISI95DS
141000090210     C                   MOVEL     VABCMO        I95CAP
141100090210     C                   MOVEL     VABNMO        I95NAR
141200090210     C                   MOVEL     WOGGI         I95DAT
141300090210     C                   MOVEL     '3'           I95TCN
141400090210     C                   CALL      'TISI95R'
141500090210     C                   PARM                    TISI95DS
141600090210      *     Errore
141700090210     C     O95ERR        IFNE      *BLANKS
141800090210     C                   MOVEL     INDCAE        VABCMO
141900090210     C                   MOVEL     §PTNAZ        VABNMO
142000090210     C                   END
142100090603     C* Se in testata
142200131001     c                   if        siamo_in_Dettaglio = *blank
142300131001     c                   eval        tes_VABrmo = VABrmo
142400131001     c                   eval        tes_VABnmo = VABnmo
142500131001     c                   eval        tes_VABcmo = VABcmo
142600131001     c                   eval        NAD_mittente_in_testata = 'S'
142700090603     c                   end
142800090210      *
142900090210     C                   END
143000090210      *
143100090210      * NOn c'è l'identificativo bolla del cliente
143200090210     C                   if          vabRSD = *blank or
143300090210     C                               vabIND = *blank or
143400090210     C                               vabLOD = *blank
143500090211     C                   IF        nad3035 = 'CN'
143600090211     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
143700090213     c                   exsr      Error_DET
143800131001     c                   LEAVEsr
143900090210     C                   ENDIF
144000090211     C                   ENDIF
144100090210      *
144200131001     c                   endSR
144300090210      * ?================================================================== */
144400090210     C*?  Contatto a cui far riferimento
144500090210      * ?================================================================== */
144600090210     C     DATI_CTA      BEGSR
144700090210      *
144800131001      * SEMBRA CHE AMAZON NON ABBIA il CONTATTO
144900090210      *
145000090210     c                   endSR
145100090210      * ?================================================================== */
145200090210     C*?  Contatto telefonico  a cui far riferimento
145300090210      * ?================================================================== */
145400090210     C     DATI_COM      BEGSR
145500131001      *
145600131001      * SEMBRA CHE AMAZON NON ABBIA la COMUNICAZIONE
145700131002      *          ma lo gestisce tramite un RFF quindi decodificato fra _RFF
145800090210      *
145900090209     C                   ENDSR
146000090210      * ?================================================================== */
146100090210     C*?  Dati di dettaglio della spedizione e dei colli
146200090210      * ?================================================================== */
146300090210     C     DATI_GID      BEGSR
146400131001      *
146500131001      * ?  /*----------------------------------------------------------------*/
146600131001      * ?   PER AMAZON QUESTO È IL PRIMO RECORD DI DETTAGLIO DI UNA SPEDIZIONE
146700131001      * ?  /*----------------------------------------------------------------*/
146800130930      *
146900131001      *  Prepara x una nuova spedizione
147000130930      * ?                         --------------
147100130930     c                   exsr      Nuova_spediz
147200130930      * ?                         --------------
147300140304      *
147400140304     C***>>   da Marzo 2014  si gestisce il MULTICOLLO che viene calcolato
147500140304     C***>>   in funzione dei colli presenti sui segmenti RFF+CR
147600140304     C***>>   quindi NON si esegue più nessuna specifica di questa parte di Routine
147700140304     C***>>   e si calcola dinamicamente il totale colli da mettere sul VABNCL in
147800140304     C***>>   seguito.
147900090210      *
148000090210     C* Se ho totali per spedizione metto in campi VAB
148100140304     C     gid1496       IFEQ      99999
148200140304     C     gid1496       orEQ      9999
148300090210     C                   MOVEL     'S'           WGID99
148400140304      *
148500131016     C********   SI VUOLE CHIODATO (1 SOLO COLLO)
148600140304     C***>>         da marzo NON più poichè si passa a spediz. multicollo
148700140304     C***>>              z-add     1             VABNCL
148800131016     C***********        z-add     gid722420     VABNCL
148900140304      *
149000090210     C                   ELSE
149100140304      *
149200090210     C     WGID99        IFEQ      'N'
149300131016     C********   SI VUOLE CHIODATO (1 SOLO COLLO)
149400140304     C***>>         da marzo NON più poichè si passa a spediz. multicollo
149500140304     C***>>              z-add     1             WTNCL             5 0
149600131016     C***********        z-add     gid722420     WTNCL             5 0
149700140304     C***>>              ADD       WTNCL         VABNCL
149800090210     C                   END
149900140304      *
150000090210     C                   END
150100140304      *
150200140304      *  da marzo 2014 VABNCL calcolato alla fine del singolo dettaglio
150300140304      *   NON PRESO DAL segmento GID.
150400140304     C***>>              ADD       VABNCL        WTOCOC           16 0
150500090210      *
150600090210     c     end_GID       endSR
150700130930      * ?================================================================== */
150800130930     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
150900130930      * ?================================================================== */
151000130930     C     Nuova_spediz  BEGSR
151100130930      **
151200130930      * Pulisco campi del file
151300130930     c                   z-add     vnrec         SAVNUM_RECord
151400130930     C                   CLEAR                   EDiVAB00
151500130930     C                   CLEAR                   EDiVAT00
151600130930     C                   CLEAR                   EDiVAD00
151700130930     C                   MOVEL     'N'           WGID99            1
151800130930     C                   clear                   XY                5 0
151900130930     C                   clear                   §§                5 0
152000130930     C                   clear                   WS1496            5 0
152100130930     C                   clear                   WNSP             35
152200130930     C                   Z-ADD     *ZEROS        PVLB
152300130930     C                   MOVEA     *HIVAL        SGN
152400130930     C*  Azzero codice cliente - tariffa
152500130930     C                   Z-ADD     0             WTOCOC           16 0
152600130930     C                   Z-ADD     0             WTOPLC           16 2          ???
152700130930     C                   Z-ADD     0             WTOPNC           16 2          ???
152800130930     C                   Z-ADD     0             Work_KG          16 2          ???
152900130930     C*  Azzero codice cliente - tariffa
153000130930     C                   clear                   WCLKSC
153100130930     C                   clear                   WCLTAR
153200130930     C                   clear                   WCLLNP
153300130930     C                   clear                   WCLNRS
153400130930     C                   clear                   WCLCTM
153500130930     C                   clear                   WCLBS1
153600130930     C                   clear                   WCLfnsp                        *blk/S
153700130930     C                   clear                   WNOT1            35
153800130930     C                   clear                   WNOT2            35
153900130930     C*  Campi di work
154000130930     c                   clear                   wVabTIC
154100130930     c                   clear                   wri_vabtic        1
154200130930      *
154300130930     C* pulisce campi di work
154400130930     c                   clear                   watrde           35
154500130930     c                   clear                   wattel           25
154600130930     C                   clear                   WRMN              3
154700130930      *
154800130930      * Un errore nel dettaglio
154900130930     C                   clear                   Err_in_detta      1
155000130930     c                   clear                   errori_in_Wrt     1
155100130930     c                   clear                   se_trovato_NAD    1
155200131002     C                   eval      DS1B = savDS1B
155300131001      **
155400131001      ** Riferimento del cliente
155500131001     C                   MOVEL     unh0062       WPRGSP           35
155600131001     C                   MOVEL     unh0062       WNSP             35
155700131001     C                   MOVEL     unh0062       WNUM             35
155800131001      * ?                                       --------
155900131001     C                   MOVEL     unh0062       VABRMA
156000131001      * ?                                       --------
156100131001     C     ' '           SCAN      unh0062       WLUNGH            3 0
156200131001     C                   SUB       1             WLUNGH
156300131001     C                   EXSR      TSTNUM
156400131001     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
156500131001      * ?                                       --------
156600131001     C                   MOVEL     WNUMOK        VABRMN
156700131001      * ?                                       --------
156800131001     C                   END
156900131001      **
157000131001      **    imposta dalla PT i valori di default presenti in Tabella
157100131001     C                   MOVEL     VABlnp_PT     VABLNP
157200131001     C                   MOVEL     VABnrs_PT     VABNRS
157300131001     C                   MOVEL     VABctm_PT     VABCTM
157400130930      **
157500130930     c                   endSR
157600090210      * ?================================================================== */
157700090210     C*?  Dati di dettaglio misure spedizione e colli
157800090210      * ?================================================================== */
157900090210     C     DATI_MEA      BEGSR
158000090210      *
158100131001      *    attenzione  ai DECIMALI
158200131001      *
158300090210     C*  Peso
158400131002     C     mea6311       IFEQ      Peso_Tassabile
158500131002     C     mea6411       ANDEQ     in_KG
158600090210      *
158700090210     C*  Controllo se ho valore totale o parziale
158800090210      *   sempre il Lordo
158900131002???? C                   IF        mea6313 = Tipo_Peso_Tassabile
159000090210     C     WS1496        IFEQ      99999
159100090210     C     WS1496        orEQ      9999
159200091019???? C                   move      mea6314       mea6314_2        18 2          Peso
159300091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
159400131001???? C                   ADD       mea6314_2     WTOPLC
159500090210     C                   ELSE
159600091019???? C     WGID99        IFEQ      'N'
159700091019???? C                   move      mea6314       mea6314_2                      Peso
159800110718???? C                   ADD       mea6314_2     Work_KG                        Peso
159900110718     c                   eval         vabPKB = Work_KG
160000131001???? C                   ADD       mea6314_2     WTOPLC
160100090210     C                   END
160200090210     C                   END
160300090210     C                   END
160400090210      *
160500131001     C                   ENDif
160600131001      *
160700090210     C* Ricerco CAP
160800090210     C     WCAP          IFNE      *BLANKS
160900131001      *
161000131002     C     mea6311       IFEQ      Peso_Tassabile
161100090210      *
161200090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
161300090210     C                   CLEAR                   FNLV55DS
161400090210     C                   MOVEL     'P'           D55TPT
161500090210     C                   MOVEL     WOGGI         D55DRF
161600090210     C                   MOVEL     VABLNP        D55LNP
161700090210     C                   MOVEL     VABLNP        D55LIN
161800090210     C                   CALL      'FNLV55R'
161900090210     C                   PARM                    FNLV55DS
162000090210     C                   MOVE      D55TFP        COMTFP            3 0
162100090210      *
162200090210     C*  PASSO IL TERMINAL PARTENZA
162300090210     C                   CLEAR                   TISI95DS
162400090210     C                   Z-ADD     COMTFP        I95TFP
162500090210     C                   MOVEL     VABTSP        I95TSP
162600090210     C                   MOVEL     'S'           I95FRE
162700090210     C                   MOVEL     '3'           I95TCN
162800090210     C                   MOVEL     WCAP          I95CAP
162900090210     C                   MOVEL     VABNZD        I95NAR
163000090210     C                   MOVEL     VABLOD        I95LOC
163100090210     C                   MOVEL     VABIND        I95IND
163200090210      *
163300090210     C* Se gestita seconda linea di arrivo passo peso - volume
163400090210     C* e numero colli effettivo
163500090210     C     §PULNA        IFEQ      'S'
163600090210     C                   Z-ADD     VABPKB        I95LKG
163700090210     C                   Z-ADD     VABVLB        I95LMC
163800090210     C                   END
163900090210     C*
164000090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
164100090210     C                   SELECT
164200090210     C     VABCBO        WHENEQ    '1 '
164300090210     C                   MOVEL     *BLANKS       I95FCA
164400090210     C                   MOVEL     'F'           I95TPO
164500090210     C     VABCBO        WHENEQ    '2 '
164600090210     C                   MOVEL     *BLANKS       I95FCA
164700090210     C                   MOVEL     'A'           I95TPO
164800090210     C     VABCBO        WHENEQ    '4 '
164900090210     C                   MOVEL     'S'           I95FCA
165000090210     C                   MOVEL     'F'           I95TPO
165100090210     C     VABCBO        WHENEQ    '6 '
165200090210     C                   MOVEL     'S'           I95FCA
165300090210     C                   MOVEL     'A'           I95TPO
165400090210     C                   OTHER
165500090210     C                   MOVEL     'F'           I95TPO
165600090210     C     VABCAS        IFGT      0
165700090210     C                   MOVEL     'S'           I95FCA
165800090210     C                   ELSE
165900090210     C                   MOVEL     *BLANKS       I95FCA
166000090210     C                   END
166100090210     C                   ENDSL
166200090210     C*
166300090210     C                   CALL      'TISI95R'
166400090210     C                   PARM                    TISI95DS
166500131001      **
166600090210     C* Reperisco i dati da CAP
166700090210     C                   MOVEL     O95PRV        VABPRD
166800131002     C*******            MOVEL     O95LNA        VABLNA
166900131002     C********           MOVEL     O95ZNC        VABZNC
167000131002     C********
167100131002     C********  NON SI DEVONO CARICARE MAI ci pensa la BOLLETTAZIONE
167200131002     C                   CLEAR                   VABLNA
167300131002     C                   CLEAR                   VABZNC
167400131002     C********
167500090210     C                   MOVEL     WCAP          VABCAD
167600090210     C*
167700090210     C     O95ERR        IFNE      *BLANKS
167800090210     C     O95FIT        ANDEQ     'S'
167900090210     C*  Cap non trovato
168000090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
168100090213     c                   exsr      Error_DET
168200131001     c                   LEAVEsr
168300090210     C                   END
168400131001      *
168500090210     C                   END
168600090210      *
168700090210     C                   ELSE
168800090210      *
168900090210     C                   CLEAR                   VABPRD
169000090210     C                   CLEAR                   VABLNA
169100090210     C                   CLEAR                   VABZNC
169200090210     C                   CLEAR                   VABCAD
169300090210     C*  Cap non trovato
169400090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
169500090213     c                   exsr      Error_DET
169600131001     c                   LEAVEsr
169700131001      *
169800090210     C                   END
169900090210      *
170000131001     c                   endSR
170100090211     C*----------------------------------------------------------------
170200090211      *? dati finali del dettaglio    UNT
170300090211     C*----------------------------------------------------------------
170400090211     C     DATI_UNT      BEGSR
170500090211      *
170600090213     c                   eval      NUM_RECord = vnREC
170700090213     c                   z-add     vnrec         last_RECord
170800090211      **
170900090211     c                   endSR
171000140304      *===============================================================*
171100140304      *  Nel RFF+CR c'è il Segnacollo x AMAZON
171200140304      *===============================================================*
171300140304     C     SEGNACollo    BEGSR
171400140304      *
171500140304      *-----
171600140304      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
171700140304      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
171800140304     C     §1BF12        IFEQ      'E'
171900140304     C                   EXSR      SEGNEE
172000140304     C                   ELSE
172100140304      *
172200140304      *  La prima volta controllo congruità numero segnacollo
172300140304     C     §PUBS1        IFNE      *BLANKS
172400140304     C     VABnrs        ANDNE     0
172500140304      *  Se il codice trattamento merce prevede che segnacollino i
172600140304      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
172700140304     C     §1BFG7        IFEQ      'S'
172800140304     C     §1BFG1        OREQ      'S'
172900140304      *  calcolo segnacollo
173000140304     C                   EXSR      SGNELA
173100140304     C                   END
173200140304     C                   END
173300140304      *
173400140304     C                   END
173500140304      *-----
173600140304     C                   ENDSR
173700090210      *----------------------------------------------------------------
173800090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
173900090210      *----------------------------------------------------------------
174000090210     C     SEGNEE        BEGSR
174100131001      *
174200131001      * ?  /*---------------------------------------------------*/
174300140304      * ?   PER AMAZON un SOLO segnacollo sul RFF+CR:xxxxxxxxxxx'
174400140304      * ?    ed era 1 SOLO per SPEDIZIONE .
174500140304      * ? Da marzo 2014 invece SPEDIZIONI multicollo.
174600131001      * ?  /*---------------------------------------------------*/
174700090210      *
174800090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
174900090210     C                   ADD       1             §§
175000140304     C                   MOVEL     Wbarcode      SGE(§§)
175100140304     c                   z-add     §§            VABNCL
175200140304     C                   z-add     VABNCL        WTOCOC
175300131002      *
175400131002     C                   ENDSR
175500090210     C*----------------------------------------------------------------
175600090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
175700090210     C*----------------------------------------------------------------
175800090210     C     SGNELA        BEGSR
175900131001      *
176000131001      * ?  /*---------------------------------------------------*/
176100131001      * ?   PER AMAZON un SOLO segnacollo sul RFF+CR:xxxxxxxxxxx'
176200131001      * ?    ed è 1 SOLO per SPEDIZIONE .
176300140304      * ? Da marzo 2014 invece SPEDIZIONI multicollo.
176400140304      * ANCHE SE NON sono segnacolli BRT adeguo le specifiche seguenti
176500131001      * ?  /*---------------------------------------------------*/
176600090210     C*
176700090210     C                   CLEAR                   DSSGN
176800140304     C*
176900090210     C*  Controllo se devo ricevere la linea di partenza
177000090210     C                   MOVEL     'N'           WERRO             1
177100090210     C     §PULP1        IFGT      0                                            --- 03 ---
177200090210     C                   EXSR      REPLNP
177300090210     C                   END                                                    --- 03 ---
177400140304     C*
177500090210     C*  Controllo se devo ricevere la linea di arrivo
177600090210     C     §PULA1        IFGT      0                                            --- 03 ---
177700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
177800090210     C                   EXSR      REPLNA
177900090210     C                   END                                                    --- 03 ---
178000140304     C*
178100090210     C*  Controllo se devo ricevere il numero di serie
178200090210     C     §PUNR1        IFGT      0                                            --- 03 ---
178300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
178400090210     C                   EXSR      REPNRS
178500090210     C                   END                                                    --- 03 ---
178600140304     C*
178700090210     C*  Controllo se devo ricevere il numero segnacollo
178800090210     C     §PUSG1        IFGT      0                                            --- 03 ---
178900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
179000090210     C                   EXSR      REPSGN
179100090210     C                   END                                                    --- 03 ---
179200140304     C*
179300090210     C*  Controllo se devo ricevere la zona di consegna
179400090210     C     §PUZN1        IFGT      0                                            --- 03 ---
179500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
179600090210     C                   EXSR      REPZNC
179700090210     C                   END                                                    --- 03 ---
179800140304     C*
179900090210     C*  Imposto da VAB i dati a zero se mi è stato passato
180000090210     C*  numero segnacollo valido
180100090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
180200140304     C*
180300090210     C     SGNLNP        IFEQ      0                                            --- 04 ---
180400090210     C                   Z-ADD     VABLNP        SGNLNP
180500090210     C                   END                                                    --- 04 ---
180600090210     C     SGNLNA        IFEQ      0                                            --- 04 ---
180700090210     C                   Z-ADD     VABLNA        SGNLNA
180800090210     C                   END                                                    --- 04 ---
180900090210     C     SGNNRS        IFEQ      0                                            --- 04 ---
181000090210     C                   Z-ADD     VABNRS        SGNNRS
181100090210     C                   END                                                    --- 04 ---
181200090210     C     SGNZNC        IFEQ      0                                            --- 04 ---
181300090210     C                   Z-ADD     VABZNC        SGNZNC
181400090210     C                   END                                                    --- 04 ---
181500090210      *
181600090210     C*  mi reimposto LNA e zona da segnacollo
181700090210     C                   Z-ADD     SGNLNA        VABLNA
181800090210     C                   Z-ADD     SGNZNC        VABZNC
181900140304      *
182000090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
182100090210     C                   ADD       1             XY
182200090210     C                   MOVE      SGNNSC        SGN(XY)
182300140304     c                   z-add     XY            VABNCL
182400140304     C                   z-add     VABNCL        WTOCOC
182500090210     C                   END                                                    --- 03 ---
182600090210      *
182700090210     C                   ENDSR
182800090210     C*----------------------------------------------------------------
182900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
183000090210     C*----------------------------------------------------------------
183100090210     C     REPLNP        BEGSR
183200090210     C*
183300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
183400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
183500090210     C*  dal numero segnacollo passatomi dal partner
183600090210     C                   Z-ADD     §PULP1        WP                2 0
183700140304     C     3             SUBST     Wbarcode:WP   WLNP              3
183800090210     C*
183900090210     C*  controllo che la lnp calcolata sia numerica
184000090210     C                   TESTN                   WLNP                 98
184100090210     C*
184200090210     C*  se è numerica la carico
184300090210     C     *IN98         IFEQ      '1'
184400090210     C                   MOVE      WLNP          SGNLNP
184500090210     C                   ELSE
184600090210     C*  se non è numerica stampo errore
184700090210     C                   MOVEL     'S'           WERRO             1
184800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
184900090213     c                   exsr      Error_DET
185000131001     c                   LEAVEsr
185100090210     C                   END
185200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
185300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
185400090210     C     SGNLNP        IFNE      VABLNP
185500090210     C                   MOVEL     'S'           WERRO             1
185600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
185700090210     c                             a della Bolla'
185800090213     c                   exsr      Error_DET
185900131001     c                   LEAVEsr
186000090210     C                   END
186100090210     C*
186200131001     C                   ENDSR
186300131001     C*----------------------------------------------------------------
186400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
186500090210     C*----------------------------------------------------------------
186600090210     C     REPLNA        BEGSR
186700090210     C*
186800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
186900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
187000090210     C*  dal numero segnacollo passatomi dal partner
187100090210     C                   Z-ADD     §PULA1        WP                2 0
187200140304     C     3             SUBST     Wbarcode:WP   WLNA              3
187300090210     C*  controllo che la lna calcolata sia numerica
187400090210     C                   TESTN                   WLNA                 98
187500090210     C*  se è numerica la carico
187600090210     C     *IN98         IFEQ      '1'
187700090210     C                   MOVE      WLNA          SGNLNA
187800090210     C                   ELSE
187900090210     C*  se non è numerica stampo errore
188000090210     C                   MOVEL     'S'           WERRO             1
188100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
188200090213     c                   exsr      Error_DET
188300131001     C                   LEAVEsr
188400090210     C                   END
188500090210     C*
188600131001     C                   ENDSR
188700090210     C*----------------------------------------------------------------
188800090210     C*? REPNRS - Reperisce numero serie
188900090210     C*----------------------------------------------------------------
189000090210     C     REPNRS        BEGSR
189100090210     C*
189200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
189300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
189400090210     C*  dal numero segnacollo passatomi dal partner
189500090210     C                   Z-ADD     §PUNR1        WP                2 0
189600140304     C     2             SUBST     Wbarcode:WP   WNRS              2
189700090210     C*  controllo che il nrs calcolata sia numerico
189800090210     C                   TESTN                   WNRS                 98
189900090210     C*  se è numerica la carico
190000090210     C     *IN98         IFEQ      '1'
190100090210     C                   MOVE      WNRS          SGNNRS
190200090210     C                   ELSE
190300090210     C*  se non è numerico stampo errore
190400090210     C                   MOVEL     'S'           WERRO             1
190500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
190600090213     c                   exsr      Error_DET
190700131001     C                   LEAVEsr
190800090210     C                   END
190900090210      *
191000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
191100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
191200090210     C     SGNNRS        IFNE      VABNRS
191300090210     C                   MOVEL     'S'           WERRO             1
191400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
191500090210     c                             o della Bolla'
191600090213     c                   exsr      Error_DET
191700131001     C                   LEAVEsr
191800090210     C                   END
191900090210     C*
192000131001     C                   ENDSR
192100090210     C*----------------------------------------------------------------
192200090210     C*? REPZNC - Reperisce zona consegna
192300090210     C*----------------------------------------------------------------
192400090210     C     REPZNC        BEGSR
192500090210     C*
192600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
192700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
192800090210     C*  dal numero segnacollo passatomi dal partner
192900090210     C                   Z-ADD     §PUZN1        WP                2 0
193000140304     C     2             SUBST     Wbarcode:WP   WZNC              2
193100090210     C*  controllo che la zona calcolata sia numerica
193200090210     C                   TESTN                   WZNC                 98
193300090210     C*  se è numerica la carico
193400090210     C     *IN98         IFEQ      '1'
193500090210     C                   MOVE      WZNC          SGNZNC
193600090210     C                   ELSE
193700090210     C*  se non è numerica stampo errore
193800090210     C                   MOVEL     'S'           WERRO             1
193900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
194000090213     c                   exsr      Error_DET
194100131001     C                   LEAVEsr
194200090210     C                   END
194300090210     C*
194400131001     C                   ENDSR
194500090210     C*----------------------------------------------------------------
194600090210     C*? REPSGN - Reperisce numero segnacollo
194700090210     C*----------------------------------------------------------------
194800090210     C     REPSGN        BEGSR
194900090210     C*
195000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
195100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
195200090210     C*  indicata in tabella)
195300090210     C*  dal numero segnacollo passatomi dal partner
195400090210     C                   Z-ADD     §PUSG1        WP                2 0
195500140304     C     7             SUBST     Wbarcode:WP   WSGN              7
195600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
195700090210     C                   TESTN                   WSGN                 98
195800090210     C*  se è numerico lo carico
195900090210     C     *IN98         IFEQ      '1'
196000090210     C                   MOVE      WSGN          SGNNSC
196100090210     C                   ELSE
196200090210     C*  se non è numerica stampo errore
196300090210     C                   MOVEL     'S'           WERRO             1
196400090210     C                   eval      vinMSG = 'PCI (SGN) non numerico'
196500090213     c                   exsr      Error_DET
196600131001     C                   LEAVEsr
196700090210     C                   END
196800090210     C*
196900131001     C                   ENDSR
197000051205      *----------------------------------------------------*
197100051205      *   DEFINIZIONE CHIAVI                               *
197200051205      *----------------------------------------------------*
197300051205     C     *INZSR        BEGSR
197400051205      *
197500991129     c     *ENTRY        PLIST
197600060613     C                   parm                    esito             1
197700971215      *
197800050112     C     KTABel        KLIST
197900050112     C                   KFLD                    TBLKUT
198000050112     C                   KFLD                    TBLCOD
198100050112     C                   KFLD                    TBLKEY
198200090210      *
198300090210     C     K_TAB1L       KLIST
198400090213     C                   KFLD                    etbUNB
198500090213     C                   KFLD                    etbSGM
198600090213     C                   KFLD                    etbVALSGM
198700090209     C*
198800090209     C* Definisco chiavi di accesso
198900090209     C     KTAB1         KLIST
199000090209     C                   KFLD                    KKUT
199100090209     C                   KFLD                    KCOD
199200090209     C*
199300090209     C     KNUF          KLIST
199400090209     C                   KFLD                    KAAA
199500090209     C                   KFLD                    KCNU
199600090209     C                   KFLD                    KFIL
199700090209      *
199800090209     C     KVAB1         KLIST
199900090209     C                   KFLD                    KCCM
200000090209     C                   KFLD                    KCMR
200100090209     C                   KFLD                    KCNT
200200090209     C                   KFLD                    KAAS
200300090209     C                   KFLD                    KLNP
200400090209     C                   KFLD                    KNRS
200500090209     C                   KFLD                    KNSP
200600090209      *
200700090209     C     KRDE          KLIST
200800090209     C                   KFLD                    KAAS
200900090209     C                   KFLD                    KLNP1
201000090209     C                   KFLD                    KNRS
201100090209     C                   KFLD                    KNSP
201200090209     C                   KFLD                    KNMC
201300090209     C                   KFLD                    KNRP
201400090209      *
201500090209     C     KACO          KLIST
201600090209     C                   KFLD                    KKUT
201700090209     C                   KFLD                    KKCC
201800090209     C                   KFLD                    KKSC
201900090209      *
202000090209     C     KIND          KLIST
202100090209     C                   KFLD                    KKUT
202200090209     C                   KFLD                    KKCC
202300090209     C                   KFLD                    KKSC
202400090209      *
202500090209      * DETERMINO SE SONO BARTOLINI O SDI
202600090209     C                   CLEAR                   TIBS55DS
202700090209     C                   MOVEL     'L'           I50TLA
202800090209     C                   CALL      'TIBS55R'
202900090209     C                   PARM                    TIBS55DS
203000090209      *
203100090209     C* RECUPERO DATI DELL'UTENTE
203200090209     C                   Z-ADD     1             CODUT             1 0
203300090209     C                   CALL      'XPARUT'
203400090209     C                   PARM                    UTEDSE0F
203500090209     C                   MOVEL     RAGUT         RSUT             20
203600090209     C*
203700090209     C* RICERCA CAPOCONTI
203800090209     C                   DO        50            X
203900090209     C                   MOVE      TCU(X)        TCUDS
204000090209     C     F56           IFEQ      'CG'
204100090209     C     F34           ANDEQ     '01'
204200090209     C                   Z-ADD     KCU(X)        KCI               4 0
204300090209     C                   END
204400090209     C                   END
204500090209     C*
204600090211     c                   movel(p)  'EDPAB'       User_Msg         10
204700090211     C*
204800090209     C* IMPOSTO A ZERO VARIABILI DI PGM
204900090209     c                   move      *blank        Snd_Msg_alert     1
205000090209     C                   MOVEL     CA(1)         WCA              78
205100090209     C                   MOVEL     CN(1)         WCN              78
205200090209     C*
205300090209     C* RECUPERO DATA E ORA
205400090209     C                   TIME                    WHHDAT           14 0
205500090209     C                   MOVE      WHHDAT        G02DAT
205600090209     C                   MOVE      WHHDAT        WDATA8            8 0
205700090209     C                   MOVE      WDATA8        WAA2              2 0
205800090209     C                   MOVEL     WDATA8        DATA              6 0
205900090209     C                   MOVE      WAA2          DATA
206000090209     C                   MOVEL     *BLANK        G02ERR
206100090209     C                   CALL      'XSRDA8'
206200090209     C                   PARM                    WLBDA8
206300090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
206400090209     C                   TIME                    WORA              6 0
206500090209      * Recupero data e ora
206600090209     C                   TIME                    W0140
206700090209      * UDATE IN GGMMAAAA
206800090209     C                   MOVE      W0140         WDTGIO
206900090209      * UDATE IN AAAAMMGG
207000090209     C     *eur          MOVEL     WDTGIO        DATA_eur
207100090209     C     *iso          MOVEL     DATA_eur      dateu
207200090209      *
207300090209      * ?              /*--------------------------- */
207400090209     c                   exsr      CARICA_TABELLE
207500090209      * ?              /*--------------------------- */
207600090209      *
207700090209     C                   MOVEL     *ALL'-'       CMP132          132
207800090209     C                   CLEAR                   EDiVAB00
207900090211     C                   clear                   vablnp_PT
208000090212     C                   clear                   vabnrs_PT
208100090211     C                   clear                   vabctm_PT
208200090211     C                   clear                   VABfnsp_PT
208300090209     C                   MOVEA     *HIVAL        SGN
208400991124      *
208500050414      *------------------
208600991124     C                   ENDSR
208700060621      * ?------------------------------------------------------------------ */
208800090209      *?    CARICA TABELLE SU SCHIERE
208900060621      * ?------------------------------------------------------------------ */
209000090209     C     CARICA_TABELLEBEGSR
209100090205      *
209200090209     C* Caricamento Tabella 1B
209300090209     C                   Z-ADD     0             X                 4 0
209400090209     C                   Z-ADD     CODUT         KKUT
209500090209     C                   MOVEL     '1B'          KCOD
209600090209     C     KTAB1         CHAIN     TABEL00F                           30
209700090209     C     *IN30         DOWEQ     '0'
209800090209     C     TBLFLG        IFEQ      *BLANKS
209900090209     C                   ADD       1             X
210000090209     C                   MOVEL     TBLKEY        CTM(X)
210100090209     C                   MOVEL     TBLUNI        DTM(X)
210200090209     C                   END
210300090209     C     KTAB1         READE     TABEL00F                               30
210400090209     C                   END
210500090209      *
210600090209     C* Caricamento Tabella 15
210700090209     C                   Z-ADD     0             X                 4 0
210800090209     C                   Z-ADD     CODUT         KKUT
210900090209     C                   MOVEL     '15'          KCOD
211000090209     C     KTAB1         CHAIN     TABEL00F                           30
211100090209     C     *IN30         DOWEQ     '0'
211200090209     C     TBLFLG        IFEQ      *BLANKS
211300090209     C                   ADD       1             X
211400090209     C                   MOVEL     TBLKEY        C15(X)
211500090209     C                   MOVEL     TBLUNI        DS15
211600090209     C                   MOVEL     §15COD        ISO(X)
211700090209     C                   MOVEL     §15ECF        CPF(X)
211800090209     C                   MOVE      §15PCF        CPF(X)
211900090209     C                   END
212000090209     C     KTAB1         READE     TABEL00F                               30
212100090209     C                   END
212200090209      *
212300090209     C* Caricamento Tabella CV
212400090209     C                   Z-ADD     0             X
212500090209     C                   Z-ADD     CODUT         KKUT
212600090209     C                   MOVEL     'CV'          KCOD
212700090209     C     KTAB1         CHAIN     TABEL00F                           30
212800090209     C     *IN30         DOWEQ     '0'
212900090209     C     TBLFLG        IFEQ      *BLANKS
213000090209     C                   ADD       1             X
213100090209     C                   MOVEL     TBLKEY        CCV(X)
213200090209     C                   MOVEL     TBLUNI        DCV(X)
213300090209     C                   END
213400090209     C     KTAB1         READE     TABEL00F                               30
213500090209     C                   END
213600090216      *
213700110328      * Caricamento Tabella termini consegna
213800110328     C                   Z-ADD     0             X
213900110328     C                   MOVEL     'TB'          TABCOD
214000110328     C     TABCOD        CHAIN     EDTAB01L                           30
214100110328     C     *IN30         DOWEQ     '0'
214200110328     C     TABFLG        IFEQ      *BLANKS
214300110328     C                   ADD       1             X
214400110328     C                   eval      K35_TpBolla(X) = TABKEY
214500110328     C                   eval      Cod_TpBolla(X) = TABKEY
214600110328     C                   eval      Decod_Porto(X) = TABUNI
214700110328     C                   END
214800110328     C     TABCOD        READE     EDTAB01L                               30
214900110328     C                   END
215000110328      *
215100090216     C* Caricamento Tabella Tipo Incasso C/Assegno
215200090216     C                   Z-ADD     0             X
215300090216     C                   MOVEL     'TC'          TABCOD
215400090216     C     TABCOD        CHAIN     EDTAB01L                           30
215500090216     C     *IN30         DOWEQ     '0'
215600090216     C     TABFLG        IFEQ      *BLANKS
215700090216     C                   ADD       1             X
215800090216     C                   MOVEL     TABKEY        CTC35(X)
215900090216     C                   MOVEL     TABUNI        TPI(X)
216000090216     C                   END
216100090216     C     TABCOD        READE     EDTAB01L                               30
216200090216     C                   END
216300090209      *
216400090209      * Caricamento Tabella Partner esteri
216500090209     C                   Z-ADD     0             X
216600090209     C                   MOVEL     'PT'          TABCOD
216700090209     C     TABCOD        CHAIN     EDTAB01L                           30
216800090209     C     *IN30         DOWEQ     '0'
216900090209      *
217000090209     C                   SELECT
217100090209     C     TABFLG        WHENNE    *BLANKS
217200090209      *
217300090209     C                   OTHER
217400090209     C                   ADD       1             X
217500090209     C                   MOVEL     TABKEY        CPT(X)
217600090209     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
217700090209     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
217800090209     C                   MOVEL     TABUNI        DPT(X)
217900090209     C                   ENDSL
218000090209      *
218100090209     C     TABCOD        READE     EDTAB01L                               30
218200090209     C                   END
218300121105      *
218400121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
218500121105     c                   exsr      ChecK_PT
218600121105      *
218700090209      * salva quanti Codici UNB ha caricato
218800090209      *
218900090209     C                   MOVEL     'PU'          TABCOD
219000090209     C     TABCOD        CHAIN     EDTAB01L                           30
219100090209     C     *IN30         DOWEQ     '0'
219200090209      *
219300090209     C                   SELECT
219400090209     C     TABFLG        WHENNE    *BLANKS
219500090209      *
219600090209     C                   OTHER
219700090209     C                   MOVEL     TABKEY        CODPU            35
219800090209     C                   Z-ADD     1             X
219900090209     C     CODPU         LOOKUP    CPT(X)                                 32
220000090209     C   32              MOVEL     TABUNI        DPU(X)
220100090209     C                   ENDSL
220200090209      *
220300090209     C     TABCOD        READE     EDTAB01L                               30
220400090209     C                   END
220500090209      *
220600090209      * Prosegue tab.PT e PU
220700090209     C                   MOVEL     'PV'          TABCOD
220800090209     C     TABCOD        CHAIN     EDTAB01L                           30
220900090209     C     *IN30         DOWEQ     '0'
221000090209      *
221100090209     C                   SELECT
221200090209     C     TABFLG        WHENNE    *BLANKS
221300090209      *
221400090209     C                   OTHER
221500090209     C                   MOVEL     TABKEY        CODPV            35
221600090209     C                   Z-ADD     1             X
221700090209     C     CODPV         LOOKUP    CPT(X)                                 32
221800090209     C   32              MOVEL     TABUNI        DPV(X)
221900090209     C                   ENDSL
222000090209      *
222100090209     C     TABCOD        READE     EDTAB01L                               30
222200090209     C                   END
222300090209      *
222400090209     C* Caricamento Tabella codici clienti - tariffe
222500090209     C                   Z-ADD     0             X
222600090209     C                   MOVEL     'CL'          TABCOD
222700090209     C     TABCOD        CHAIN     EDTAB01L                           30
222800090209     C     *IN30         DOWEQ     '0'
222900090209     C     TABFLG        IFEQ      *BLANKS
223000090209     C                   ADD       1             X
223100090209     C                   MOVEL     TABKEY        CCL(X)
223200090209     C                   MOVEL     TABUNI        DCL(X)
223300090209     C                   END
223400090209     C     TABCOD        READE     EDTAB01L                               30
223500090209     C                   END
223600090209      *
223700090209     C* Caricamento Serivizi speciali
223800090209     C                   Z-ADD     0             X
223900090209     C                   MOVEL     'SS'          TABCOD
224000090209     C     TABCOD        CHAIN     EDTAB01L                           30
224100090209     C     *IN30         DOWEQ     '0'
224200090209     C     TABFLG        IFEQ      *BLANKS
224300090209     C                   ADD       1             X
224400090209     C                   MOVEL     TABKEY        SS(X)
224500090209     C                   MOVEL     TABKEY        SS35(X)
224600090209     C                   MOVEL     TABUNI        DSS(X)
224700090209     C                   END
224800090209     C     TABCOD        READE     EDTAB01L                               30
224900090209     C                   END
225000090209      *
225100090209     C                   ENDSR
225200090209      * ?------------------------------------------------------------------ */
225300090209      *?    Flagga il TIVIN come messaggio completamente ERRATO
225400090209      * ?------------------------------------------------------------------ */
225500090209     C     MSG_Errato    BEGSR
225600090209      *
225700090205      * ?  Scrive su tutti i records il tipo di errore
225800090205     c     *start        setll     tivin00r
225900130930     c                   exsr      Legge_TIVIN
226000130930      *.
226100090205     c                   dow       not %eof(tivin00r)
226200090213     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
226300131001     C                               >> Controllare UNB e i DATI su UPLOAD !!'
226400090205     C                   eval      vinFLG = '2'
226500130930     c                   exsr      AGGIOR_TIVIN
226600130930     c                   exsr      Legge_TIVIN
226700090205     c                   endDO
226800131001      *
226900131001     c                   eval      Almeno_Un_Due ='S'
227000131001     c                   eval      esito  = '2'
227100131001     c                   eval      se_errore ='S'
227200131001     c                   eval      err_bloccante ='S'
227300090205      *
227400090205     C                   ENDSR
227500090213      * ?------------------------------------------------------------------ */
227600090213      *?    Flagga il TIVIN come messaggio completamente ERRATO
227700090213      * ?------------------------------------------------------------------ */
227800090213     C     DETta_Errato  BEGSR
227900090213      *
228000090213      *  rilascia il record prima di rieseguire il ciclo
228100131001      *.
228200131001     c                   exsr      AGGIOR_TIVIN
228300090213      *
228400090213      * ?  Scrive su tutti i records il tipo di errore
228500090213     c     SavNUM_RECord setll     tivin00r
228600130930      *
228700130930     c                   exsr      Legge_TIVIN
228800130930      *.
228900090213     c                   dow       not %eof(tivin00r)
229000090213      *
229100090213     c                   if        vinMSG  = *blank
229200090213     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
229300090213     C                             >> Controllare i DATI di ogni Segmento <<'
229400090213     c                   end
229500090213     c                   exsr      Error_DET
229600090213      *
229700090213     c                   if        VNREC = NUM_RECord
229800090213     c                   leave
229900090213     c                   end
230000130930      *.
230100130930     c                   exsr      AGGIOR_TIVIN
230200131001      *.
230300130930     c                   exsr      Legge_TIVIN
230400090213     c                   endDO
230500090603      *
230600131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON'
230700090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
230800131001???? C                             Bolle AMAZON non totalmente tradotto. -
230900110704     C                             Controllare UPLOAD del cliente :'
231000110704     C                   EVAL      Messaggio = %trim(Messaggio) +
231100110704     C                                         %editc(§PTksc:'X')
231200090603     c                   exsr      invio_mail_AB
231300090603      *
231400090213      *
231500090213     C                   ENDSR
231600090205      * ?------------------------------------------------------------------ */
231700090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
231800090205      * ?------------------------------------------------------------------ */
231900090205     C     *pssr         BEGSR
232000090205     C
232100060710     C                   eval      esito = '2'
232200131001???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import AMAZON'
232300090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
232400100603     C                             Bolle import in filiale - messaggio EDI -
232500110704     C                             ricevuto errato su UPLOAD cliente :'
232600110704     C                   EVAL      Messaggio = %trim(Messaggio) +
232700110704     C                                         %editc(§PTksc:'X')
232800090603     c                   exsr      invio_mail_AB
232900060621     C
233000090213     C                   ENDSR
233100090213      * ?------------------------------------------------------------------ */
233200090213      *?      Segnala Errore su TIVIN
233300090213      * ?------------------------------------------------------------------ */
233400090213     C     Error_DET     BEGSR
233500090213     C
233600090213     C                   eval      vinFLG = '2'
233700090213     c                   eval      Almeno_Un_Due ='S'
233800090213     c                   eval      esito  = '1'
233900090213     c                   eval      se_errore ='S'
234000090213     C                   eval      Err_in_detta = 'S'
234100090213     C
234200090213     C                   ENDSR
234300090209     c*==================================================================*
234400090209     C*? CNVDAT - DECODIFICA LA DATA
234500090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
234600090209     C*         - WDATA  (DATA ALFANUMERICA)
234700090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
234800090209     C*         - WHMS   (ORA NUMERICA)
234900090209     C*----------------------------------------------------------------
235000090209     C     CNVDAT        BEGSR
235100090209     C*
235200090209     C                   MOVEL     ' '           WERRO             1
235300090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
235400090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
235500090209     C                   Z-ADD     *ZEROS        WHMS              6 0
235600090209     C*
235700090209     C     WFORM         IFEQ      '102'                                        CCYMMDD
235800090209     C                   MOVEL     WDATA         WCOMO8            8
235900090209     C                   TESTN                   WCOMO8               99
236000090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
236100090209     C  N99              MOVEL     'S'           WERRO             1
236200090209     C                   END
236300090209     C*
236400090209     C     WFORM         IFEQ      '203'                                        CCYMMDD
236500110523     C                   MOVEL     WDATA         WCOMO12          12
236600110523     C                   TESTN                   WCOMO12              99
236700110523     C                   MOVEl     *all'0'       WCAMGORA                       *DATA
236800110523     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
236900090209     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
237000090209     C   99              MOVE      WCAMGORA      WHMS                           *DATA
237100090209     C  N99              MOVEL     'S'           WERRO             1
237200090209     C                   END
237300110523     C*
237400110523     C     WFORM         IFEQ      '204'                                        CCYMMDD
237500110523     C                   MOVEL     WDATA         WCOMO14          14
237600110523     C                   TESTN                   WCOMO14              99
237700110523     C   99              MOVE      WCOMO14       WCAMGORA                       *DATA
237800110523     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
237900110523     C   99              MOVE      WCAMGORA      WHMS                           *DATA
238000110523     C  N99              MOVEL     'S'           WERRO             1
238100110523     C                   END
238200090209     C*
238300090209     C                   ENDSR
238400090210     C*----------------------------------------------------------------
238500090210     C*? TSTNUM - CONTROLLO NUMERICO
238600090210     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
238700090210     C*         - WLUNGH (Lunghezza campo output)
238800090210     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
238900090210     C*----------------------------------------------------------------
239000090210     C     TSTNUM        BEGSR
239100090210     C*
239200090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
239300090210     C                   MOVEL     'N'           WOKNUM            1
239400090210     C                   MOVEL     *BLANKS       WNUMOK           15
239500090210     C                   Z-ADD     15            IN                3 0
239600090210     C                   Z-ADD     WLUNGH        IX                3 0
239700090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
239800090210     C                   MOVEA     WNUM          NDA
239900090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
240000090210     C                   MOVEA     *ALL'0'       NDN
240100090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
240200090210     C                   Z-ADD     WLUNGH        I                 3 0
240300090210     C     I             DOWGT     0                                            --- 1 -->
240400090210     C* ESTRAE IL CARATTERE DA ESAMINARE
240500090210     C     1             SUBST     WNUM:I        WTEM01            1
240600090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
240700090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
240800090210     C     WTEM01        SCAN      WCA                                    99
240900090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
241000090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
241100090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
241200090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
241300090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
241400090210     C* DA DESTRA VERSO SINISTRA
241500090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
241600090210     C                   MOVEL     WTEMP1        NDN(IN)
241700090210     C                   SUB       1             IN
241800090210     C                   END                                                    <-- 3 ---
241900090210     C                   END                                                    <-- 2 ---
242000090210     C                   SUB       1             I
242100090210     C                   END                                                    <-- 1 ---
242200090210     C*
242300090210     C                   MOVEA     NDN           WNDN             15
242400090210     C     WNDN          IFNE      *ZEROS
242500090210     C                   MOVEA     NDN           WNUMOK           15
242600090210     C                   MOVEL     'S'           WOKNUM            1
242700090210     C                   END
242800090210     C*
242900090210     C                   ENDSR
243000090210     C*----------------------------------------------------------------
243100090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
243200090210     C*----------------------------------------------------------------
243300090210     C     CNVCAP        BEGSR
243400090210     C*
243500090210     C                   MOVEL     *BLANKS       WCAP              9
243600090210     C     nad3251       IFNE      '0'
243700090210     C     '  '          SCAN      nad3251       LENGHT            2 0
243800090210     C                   SUB       1             LENGHT
243900090210     C     LENGHT        IFLE      5
244000090210     C     LENGHT        ANDGT     0
244100090210     C                   Z-ADD     LENGHT        T                 2 0
244200090210     C                   Z-ADD     5             S                 2 0
244300090210     C                   MOVEA     nad3251       CAP9
244400090210     C                   MOVEL     *ZEROS        CAP5
244500090210     C                   DO        LENGHT
244600090210     C                   MOVE      CAP9(T)       CAP5(S)
244700090210     C                   SUB       1             S
244800090210     C                   SUB       1             T
244900090210     C                   END
245000090210     C                   MOVEA     CAP5          WCAP5             5
245100090210     C     WCAP5         IFEQ      '0'
245200090210     C                   MOVEL     *BLANKS       WCAP
245300090210     C                   ELSE
245400090210     C                   MOVEA     CAP5          WCAP
245500090210     C                   END
245600090210     C*
245700090210     C                   ELSE
245800090210     C                   MOVEL     nad3251       WCAP
245900090210     C                   END
246000090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
246100090210     C     WCAP          IFNE      *BLANKS
246200090210     C     WFLCPF        ANDNE     0
246300090210     C                   MOVEL     WFLCPF        WELE              1 0
246400090210     C                   MOVE      WFLCPF        WPOS              1 0
246500090210     C                   MOVEL     *BLANK        WCPF
246600090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
246700090210     C                   ELSE
246800090210     C                   MOVEL     *BLANKS       WCPF
246900090210     C                   END
247000090210     C                   END
247100090210     C*
247200090210     C                   ENDSR
247300090211      * ?================================================================== */
247400130930      *? Importa i records della tramsissione
247500130930      * ?------------------------------------------------------------------ */
247600130930     c     Importa_SPED  Begsr
247700130930
247800130930      *** ------------------------------------
247900131001????  *** A FINE CICLO DI UN DETTAGLIO:   --> prima che ne INIZI un altro
248000130930      *** ------------------------------------
248100130930????  **
248200130930      *  Deve flaggare il dettaglio con dei problemi
248300130930     C                   if        Err_in_detta = 'S'
248400130930      **?              /*---------------------- */
248500130930     c                   exsr      DETta_Errato
248600130930      **?              /*---------------------- */
248700130930     c                   end
248800130930      *
248900130930      **  Se non presenti errori
249000130930      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
249100130930???? c                   if        se_errore <>'S'
249200131001????  **
249300130930     c                   if         se_trovato_NAD ='S'
249400130930      * ?              /*---------------------- */
249500130930     c                   exsr      Scrive_VAB
249600130930      * ?              /*---------------------- */
249700130930     c                   else
249800130930     c                   eval      errori_in_Wrt = 'S'
249900130930     c                   end
250000130930      *
250100130930      *  Problemi nella decodifica dei campi VAB/VAT
250200130930     c                   if        errori_in_Wrt = 'S'
250300130930     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
250400130930     c                   exsr      Error_DET
250500131001     c                   end
250600131001      **
250700131001     c                   end
250800131001      **
250900130930      * Dopo aver confermato, con COMMIT
251000130930      *  si prepara per una nuova spedizione
251100131001     c                   COMMIT
251200130930      ***
251300130930     c                   endSR
251400130930      * ?------------------------------------------------------------------ */
251500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
251600090211      * ?================================================================== */
251700090211     C     Scrive_VAB    BEGSR
251800090211      *
251900090211     c                   clear                   err_bloccante     1
252000090211     c                   move      'S'           Almeno_Uno
252100090211      *
252200090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
252300090211     c                   exsr      UPDVAB
252400090211      *
252500090211     C* >>>>>>>>>>>>
252600090211     C*  Richiamo pgm per scrittura immediata VAB
252700090211     C     §PUWRK        IFEQ      ' '
252800090211      *
252900090211     C*  Richiamo pgm per esecuzione stampa
253000090211     C     §PTCMR        IFEQ      'S'
253100090213     C                   MOVEL     WNRDOC        d86CMR
253200090211     C                   ELSE
253300090213     C                   MOVEL     WNRDOC        d86CMR
253400090211     C                   END
253500090211      *
253600090213     C                   MOVEL     ' '           d86RIE
253700090213     C                   MOVEL     §PUDTA        d86DTA
253800090213     C                   Z-ADD     1             d86CNT
253900090213     C                   Z-ADD     §PTKSC        d86CCM
254000090213     C                   MOVEL     *BLANKS       d86STA
254100090213     C                   MOVEL     'E'           d86MOD
254200090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
254300090211      * dove poi imposta la DS dei parametri passati
254400090213     C                   MOVEL     'LR'          d86CHI
254500090213     C                   MOVEL     'N'           d86CTL
254600090211      *
254700090211     c                   move      kpjbu         SvKpjbu
254800090211     c                   clear                   kpjbu
254900090216     C                   movel     *on           Commit_on         1
255000090213     C                   MOVEL     TRTC86ds      KPJBU
255100090213      * ?- Chiamata la TRTC86R2 -*
255200090213     C                   CALL      'TRTC86R2'
255300090211     C                   PARM                    KPJBA
255400090216     C                   PARM                    Commit_on
255500090211     c                   move      Svkpjbu       Kpjbu
255600090211      *
255700090211     C*  Controllo pgm per scrittura immediata VAB
255800090211     C     WW            IFNE      0
255900090211     C                   DO        WW            WX                3 0
256000090213     C                   Z-ADD     KCL(WX)       d86CCM
256100090211      *
256200090211     c                   move      kpjbu         SvKpjbu
256300090211     c                   clear                   kpjbu
256400090216     C                   movel     *on           Commit_on         1
256500090213     C                   MOVEL     TRTC86ds      KPJBU
256600090213      * ?- Chiamata la TRTC86R2 -*
256700090213     C                   CALL      'TRTC86R2'
256800090211     C                   PARM                    KPJBA
256900090216     C                   PARM                    Commit_on
257000090211     c                   move      Svkpjbu       Kpjbu
257100090211     C                   END
257200090211     C                   END
257300090211      *
257400090211     C                   END
257500090211      * >>>>>>>>>>>>
257600090211     C                   if        se_errore  = 'S'
257700090211     c                   move      'S'           err_bloccante
257800131001     C                   LEAVEsr
257900090211     c                   end
258000090211     C*
258100131001     C                   Endsr
258200090211     C*----------------------------------------------------------------
258300090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
258400090211     C*----------------------------------------------------------------
258500090211     C     UPDVAB        BEGSR
258600131001      *
258700131022      *   Imposta su NATURA MERCE il contenuto identificativo del documento BGM in testata
258800131022      *    del Messaggio (questo appositamente e solo x Amazon)
258900131022     c                   if        BGM_Id_Testata <> ' '
259000131022     c                   eval      vabNAS = BGM_Id_Testata
259100131022     c                   end
259200131022      *
259300131001      *  imposta il default dalla tabella PT se NON è ancora
259400131001      *   stato impostato il tipo bolla.
259500131001     C     VABCBO        IFEQ      *BLANKS
259600131001     C     §puPFA        ifNE      *blank
259700131001     c                   SELECT
259800131001      *  Porto Franco
259900131001     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
260000131001     C                   movel     '4'           VABCBO                         + C/Ass
260100131001     c                   WHEN      §puPFA = 'F'
260200131001     C                   movel     '1'           VABCBO                         Senza C/Ass.
260300131001      *  Porto Assegnato
260400131001     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
260500131001     C                   movel     '6'           VABCBO                         + C/Ass
260600131001     C                   WHEN      §puPFA = 'A'
260700131001     C                   movel     '2'           VABCBO                         Senza C/Ass
260800131001     C                   ENDSL
260900131001     c                   end
261000131001     C                   END
261100131001      *
261200110523      * Imposta le NOte
261300110523     C     VABNOT        IFEQ      *BLANKS
261400110523     C                   MOVEL     Wnot1         VABNOT
261500110523     C                   MOVEL     Wnot2         VABNT2
261600110523     C                   ELSE
261700110523     C                   MOVEL     Wnot1         VABNT2
261800110523     C                   END
261900090211      *
262000090211     C*  Attribuisco anno spedizione
262100090211     C     WDATPA        IFGT      0                                            anno sped.
262200090211     C                   MOVEL     WDATPA        VABAAS
262300090211     C                   ELSE
262400090211     C     WDATFV        IFGT      0
262500090211     C                   MOVEL     WDATFV        VABAAS                         anno sped.
262600090211     C                   END
262700090211     C                   END
262800090211      *
262900090211      *  Controllo dettaglio segnacolli
263000090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
263100090211     C                   ELSE
263200140304      * SOLO se BRT (non è il caso di AMAZON... comunque lascio le specifiche)
263300090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
263400090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
263500090211     C                   EXSR      CTRSGN
263600090211     C                   END
263700090211     C                   END
263800090211      *
263900131001      *  Imposto linea partenza e serie e CTM
264000090211     C                   eval      VABLNP = VABLNP_PT
264100090212     C                   eval      VABNRS = VABnrs_PT
264200090211     C                   eval      VABCTM = VABctm_PT
264300090211      *
264400090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
264500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
264600090211     c                   clear                   mantiene_nsp      1            *blk/'S'
264700090211     c                   eval      mantiene_nsp = VABfnsp_PT                    *blk/'S'
264800090211      *
264900090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
265000090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
265100090211      *
265200090211      *  Imposto codice cliente
265300090211     c                   Select
265400090211      *
265500090211      *  Specificato codice da qualificatore FF (TD15)
265600090211     C                   When        WCLKSC <> *zeros
265700090211     C                   Z-ADD     WCLKSC        VABCCM
265800090211     C                   MOVEL     WCLTAR        VABCTR
265900090211      * forza LNP/CTM/NRS
266000090211     c                   if        WclLNP <> 0
266100090211     C                   eval      VABLNP = WclLNP
266200090211     c                   end
266300090211     c                   if        WclNRS <> 0
266400090211     C                   eval      VABNRS = WclNRS
266500090211     c                   end
266600090211     c                   if        WclCTM <> *blank
266700090211     C                   eval      VABCTM = WclCTM
266800090211     c                   end
266900090211      *
267000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
267100090211     c                   if        WCLfnsp <> *blank                            *blk/'S'
267200090211     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
267300090211     c                   end
267400090211      *
267500090211      *  Specificato codice da qualificatore FF (TT15)
267600090211     C                   When        $CLKSC <> *zeros
267700090211     C                   Z-ADD     $CLKSC        VABCCM
267800090211     C                   MOVEL     $CLTAR        VABCTR
267900090211      * forza LNP/CTM/NRS
268000090211     c                   if        $clLNP <> 0
268100090211     C                   eval      VABLNP = $clLNP
268200090211     c                   end
268300090211     c                   if        $clNRS <> 0
268400090211     C                   eval      VABNRS = $clNRS
268500090211     c                   end
268600090211     c                   if        $clCTM <> *blank
268700090211     C                   eval      VABCTM = $clCTM
268800090211     c                   end
268900090211      *
269000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
269100090211     c                   if        $CLfnsp <> *blank                            *blk/'S'
269200090211     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
269300090211     c                   end
269400090211      *
269500090211      *  Imposto dati da tabella - PT-
269600090211     C                   Other
269700090211      *
269800090211     C                   MOVEL     §PTKSC        VABCCM
269900090211     C                   MOVEL     §PTCTR        VABCTR
270000090211      *
270100090211     C                   Endsl
270200090211      *
270300090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
270400090211      *    o dalla relativa CL
270500090211     C                   MOVEL     VABlnp        VABfgs
270600090211      *
270700090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
270800090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
270900090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
271000090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
271100090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
271200090211      *
271300090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
271400090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
271500090211     c                   else
271600090211      *
271700090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
271800090211     C*  lo attribuisco io
271900090211      **              **------------------**
272000090211     c                   exsr      repnum
272100090211      **              **------------------**
272200090211     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
272300090211      **              **------------------**
272400090211     c                   end
272500090211      *
272600090211     C*  Altrimenti attribuisco data del giorno
272700090211     C     VABMGS        IFEQ      0
272800090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
272900090211     C                   MOVE      WOGGI         VABMGS                         = oggi
273000090211     C                   END
273100090211     C*  Eseguo posizionamento su EDiVAB
273200090211     C                   Z-ADD     VABAAS        KAAS
273300090211     C                   Z-ADD     VABlnp        KLNP
273400090211     C                   Z-ADD     VABNRS        KNRS
273500090211     C                   Z-ADD     VABNSP        KNSP
273600090211     C                   MOVEL     SAVBOL        SALVA
273700090211     C     KVAB1         CHAIN     EDiVAB1L                           30
273800090211     C                   MOVEL     SALVA         SAVBOL
273900090211     C*  Imposto dati CMR
274000090211     C                   Z-ADD     1             VABCNT
274100090211     **
274200090211      *** Attenzione se si deve trattare il VAX con il CMR
274300090211      *** il campo VABCNT deve essere sempre impostato a (1)
274400090211     C     §puWRK        ifEQ      'S'
274500090211     C     §puNSP        andEQ     'S'
274600090211     C                   Z-ADD     1             VABCNT
274700090211     c                   end
274800090211     **
274900090211     C                   Z-ADD     WDTPRE        VABDTS
275000090212     C                   movel     '20'          VABDTS
275100090212     C     WHMPRE        mult      100           VABHMS
275200090211     C     §PTCMR        IFEQ      'S'
275300090211     C                   Z-ADD     WDTCMR        VABDCM
275400090211     C                   MOVEL     WNRCMR        VABCMR
275500090211     C                   ELSE
275600090211     C                   Z-ADD     WDATFV        VABDCM
275700090211     C                   MOVEL     WNUMFV        VABCMR
275800090211     C                   END
275900131001     **
276000090211     C*  Se non è indicato il nome del mittente = Partner mittente
276100090211     C     VABRMO        IFEQ      *BLANKS
276200090211     C                   MOVEL     ACORAG        VABRMO
276300090211     C                   MOVEL     INDCAP        VABCMO
276400090211     C     INDCAE        IFNE      *BLANKS
276500090211     C                   MOVEL     INDCAE        VABCMO
276600090211     C                   END
276700090211     C     INDSTA        IFNE      *BLANKS
276800090211     C                   MOVEL     INDSTA        VABNMO
276900090211     C                   END
277000090211     C                   END
277100131001     **
277200090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
277300090211     C     §1BFG1        IFEQ      'N'
277400090211     C     §1BFG7        ANDEQ     'N'
277500090211     C     §1BFG2        ANDEQ     'N'
277600090211     C***                  Z-ADD0         VABNRS
277700090211     C                   END
277800090211     C*  Imposto segancollo da - a
277900090211     C     §1BFG1        IFEQ      'S'
278000090211     C     §1BFG7        ANDEQ     'N'
278100090211     C     §1BF12        ANDNE     'E'
278200090211      *
278300090211     C     XY            IFEQ      *ZEROS
278400090211     C                   CLEAR                   VABNCD
278500090211     C                   CLEAR                   VABNCA
278600090211     C                   ELSE
278700090211     C                   Z-ADD     SGN(1)        VABNCD
278800090211     C                   Z-ADD     SGN(XY)       VABNCA
278900090211     C                   ENDIF
279000090211     C*
279100090211     C                   END
279200090211     C*
279300090211     C*  Se cap mittente originale a blank abblenco nazione
279400090211     C     VABCMO        IFEQ      *BLANKS
279500090211     C                   MOVEL     *BLANKS       VABNMO
279600090211     C                   END
279700090603     C*
279800090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
279900090603     C* Se in testata
280000090603     c                   if        NAD_destinatario_in_testata = 'S'
280100090603     c                   eval         VABrsd =  tes_VABrsd
280200090603     c                   eval         VABrd2 =  tes_VABrd2
280300090603     c                   eval         VABind =  tes_VABind
280400090603     c                   eval         VABlod =  tes_VABlod
280500090603     c                   eval         VABnzd =  tes_VABnzd
280600090603     c                   end
280700090603     C* Se in testata
280800090603     c                   if        NAD_mittente_in_testata = 'S'
280900090603     c                   eval         VABrmo = tes_VABrmo
281000090603     c                   eval         VABnmo = tes_VABnmo
281100090603     c                   eval         VABcmo = tes_VABcmo
281200090603     c                   end
281300110718      *
281400090211      *? ------------------------------------------------------------------------
281500090213     C   30              WRITE     EDiVAB00                             99
281600090213     C  N30              UPDATE    EDiVAB00                             99
281700090211      *? ------------------------------------------------------------------------
281800090213     c   99              eval      errori_in_Wrt = 'S'
281900090211     C*
282000090211     c                   clear                   wri_vabtic        1
282100090211     C*
282200090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
282300090211     C                   EXSR      WRTVAT_Rif
282400090211     C*
282500090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
282600090211     C     §1BF12        IFEQ      'E'
282700090211     C                   EXSR      WRTVAT
282800090211     C                   ELSE
282900090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
283000090211     C     §1BFG7        IFEQ      'S'
283100090211     C     §1BFG1        OREQ      'S'
283200090211     C                   EXSR      WRTVAD
283300090211     C                   END
283400090211     C                   END
283500090211      *
283600090211     C* Reimposto codice trattamento merce cliente
283700090211     C                   MOVEL     WSVTRM        DS1B
283800090211     C**
283900090211     C* Do errore se previsto CMR ma non c'è
284000090211     C     §PTCMR        IFNE      ' '
284100090211     C                   EXSR      MEMCMR
284200090211     C                   END
284300131001     C**
284400090211     C* Do errore se previsto AFB ma non c'è
284500090211     C     §PTNFV        IFNE      ' '
284600090211     C                   EXSR      MEMAFB
284700090211     C                   END
284800131001     C**
284900090211     C*
285000090211     C                   ENDSR
285100090211     C*----------------------------------------------------------------
285200090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
285300090211     C*----------------------------------------------------------------
285400090211     C     CTRSGN        BEGSR
285500131001      *
285600090211     C*  Riordino i segnacolli
285700090211     C     §1BFG7        IFEQ      'N'
285800090211     C                   SORTA     SGN
285900090211     C*  Controllo se sono sequenziali
286000090211     C                   MOVEL     'N'           WNOSEQ            1
286100090211     C     SGN(1)        ADD       1             WEXSGN            7 0
286200090211     C     2             DO        XY            X
286300090211     C     SGN(X)        IFNE      WEXSGN
286400090211     C                   MOVEL     'S'           WNOSEQ
286500090211     C                   END
286600090211     C     SGN(X)        ADD       1             WEXSGN
286700090211     C                   END
286800090211     C                   END
286900090211     C*
287000090211     C     FINSGN        ENDSR
287100090211     C*----------------------------------------------------------------
287200090211     C*? ESEGUO SCRITTURA EDiVAD
287300090211     C*----------------------------------------------------------------
287400090211     C     WRTVAD        BEGSR
287500090211     C*
287600090211     C* Se non seq. scrivo xy record
287700090211     C     §1BFG1        IFEQ      'S'
287800090211     C     XY            ANDNE     *ZEROS
287900090211     C*
288000090211     C     §1BFG7        IFEQ      'S'
288100090211     C                   DO        XY            X
288200090211     C                   Z-ADD     VABCCM        KCCM
288300090211     C                   Z-ADD     SGN(X)        KNCD
288400090211     C                   Z-ADD     VABCNT        VADCNT
288500090211     C                   MOVEL     VABCMR        VADCMR
288600090211     C                   Z-ADD     VABAAS        VADAAS
288700090211     C                   Z-ADD     VABLNP        VADLNP
288800090211     C                   Z-ADD     VABNRS        VADNRS
288900090211     C                   Z-ADD     VABNSP        VADNSP
289000090211     C                   Z-ADD     1             VADNCL
289100090211     C                   MOVEL     SGN(X)        VADCDP
289200090211     C                   Z-ADD     SGN(X)        VADNCD
289300090211     C                   Z-ADD     *ZEROS        VADNCA
289400090211     C                   Z-ADD     VABCCM        VADCCM
289500090211     C                   movel     VABfgs        VADfgs
289600090213     C                   WRITE     EDiVAD00                             99
289700090213     c   99              eval      errori_in_Wrt = 'S'
289800090211     C                   END
289900090211     C* Se sequenziali scrivo un solo record
290000090211     C                   ELSE
290100090211     C                   Z-ADD     VABAAS        VADAAS
290200090211     C                   Z-ADD     VABLNP        VADLNP
290300090211     C                   Z-ADD     VABNRS        VADNRS
290400090211     C                   Z-ADD     VABNSP        VADNSP
290500090211     C                   Z-ADD     VABNCL        VADNCL
290600090211     C                   Z-ADD     SGN(1)        VADNCD
290700090211     C                   Z-ADD     SGN(XY)       VADNCA
290800090211     C                   Z-ADD     VABCCM        VADCCM
290900090211     C                   movel     VABfgs        VADfgs
291000090213     C                   WRITE     EDiVAD00                             99
291100090213     c   99              eval      errori_in_Wrt = 'S'
291200090211     C                   END
291300090211     C*
291400090211     C                   ELSE
291500090211     C                   DO        XY            X
291600090211     C                   Z-ADD     VABCCM        KCCM
291700090211     C                   Z-ADD     SGN(X)        KNCD
291800090211     C                   Z-ADD     VABAAS        VADAAS
291900090211     C                   Z-ADD     VABLNP        VADLNP
292000090211     C                   Z-ADD     VABNRS        VADNRS
292100090211     C                   Z-ADD     VABNSP        VADNSP
292200090211     C                   MOVEL     SGN(X)        VADCDP
292300090211     C                   Z-ADD     1             VADNCL
292400090211     C                   Z-ADD     *ZEROS        VADNCD
292500090211     C                   Z-ADD     *ZEROS        VADNCA
292600090211     C                   Z-ADD     VABCCM        VADCCM
292700090211     C                   movel     VABfgs        VADfgs
292800090213     C                   WRITE     EDiVAD00                             99
292900090213     c   99              eval      errori_in_Wrt = 'S'
293000090211     C                   END
293100090211     C*
293200090211     C                   END
293300090211     C*
293400090211     C                   ENDSR
293500090211     C*----------------------------------------------------------------
293600090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
293700090211     C*----------------------------------------------------------------
293800090211     C     WRTVAT_Rif    BEGSR
293900090211      *
294000090211      *  riferimento di consegna destinatario
294100090211     c                   if        WatRDE <> *blank
294200090211     C                   Z-ADD     VABCNT        VATCNT
294300090211     C                   MOVEL     VABCMR        VATCMR
294400090211     C                   Z-ADD     VABCCM        VATCCM
294500090211     C                   movel     VABfgs        VATfgs
294600090211     C                   Z-ADD     VABLNP        VATLNP
294700090211     C                   Z-ADD     VABAAS        VATAAS
294800090211     C                   Z-ADD     VABNRS        VATNRS
294900090211     C                   Z-ADD     VABNSP        VATNSP
295000090211     C                   MOVEL     'A'           VATTRC
295100131002     C                   MOVEL(p)  WatRDE        VATNOT
295200090213     C                   WRITE     EDiVAT00                             99
295300090213     c   99              eval      errori_in_Wrt = 'S'
295400090211     c                   end
295500090211      *
295600090211      *  riferimento di consegna telefonico
295700090211     c                   if        WatTEL <> *blank
295800090211     C                   Z-ADD     VABCNT        VATCNT
295900090211     C                   MOVEL     VABCMR        VATCMR
296000090211     C                   Z-ADD     VABCCM        VATCCM
296100090211     C                   movel     VABfgs        VATfgs
296200090211     C                   Z-ADD     VABLNP        VATLNP
296300090211     C                   Z-ADD     VABAAS        VATAAS
296400090211     C                   Z-ADD     VABNRS        VATNRS
296500090211     C                   Z-ADD     VABNSP        VATNSP
296600090211     C                   MOVEL     'B'           VATTRC
296700131002     C                   MOVEL(p)  WatTEL        VATNOT
296800090213     C                   WRITE     EDiVAT00                             99
296900090213     c   99              eval      errori_in_Wrt = 'S'
297000090211     c                   end
297100090211      *
297200090211     C                   ENDSR
297300090211     C*----------------------------------------------------------------
297400090211     C*? ESEGUO SCRITTURA EDiVAT
297500090211     C*----------------------------------------------------------------
297600090211     C     WRTVAT        BEGSR
297700090211      *
297800090211     C                   DO        §§            X
297900090211     C                   Z-ADD     VABCNT        VATCNT
298000090211     C                   MOVEL     VABCMR        VATCMR
298100090211     C                   Z-ADD     VABCCM        VATCCM
298200090211     C                   movel     VABfgs        VATfgs
298300090211     C                   Z-ADD     VABLNP        VATLNP
298400090211     C                   Z-ADD     VABAAS        VATAAS
298500090211     C                   Z-ADD     VABNRS        VATNRS
298600090211     C                   Z-ADD     VABNSP        VATNSP
298700090211     C                   MOVEL     'E'           VATTRC
298800090211      *
298900090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
299000090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
299100090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
299200090211     c                   if        WVATlun > 0
299300090211      *
299400090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
299500090211     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
299600090211      *
299700090211     c                   else
299800090211      *
299900090211      * Se non è impostato nulla prende il segnacollo passato così com'è
300000131002     C                   MOVEL(p)  SGE(X)        VATNOT
300100090211     c                   end
300200090211      *
300300090213     C                   WRITE     EDiVAT00                             99
300400090213     c   99              eval      errori_in_Wrt = 'S'
300500090211     C                   END
300600090211      *
300700090211     C                   ENDSR
300800090213     c**********************************************************************
300900090213     c* reperimento numero Spedizione
301000090213     c**********************************************************************
301100090213     C     repnum        BEGSR
301200090213      *
301300090213      *    deve controllare sulla tabella "3C" se il numero spedizione
301400090213      *     deve essere mantenuto oppure no
301500090213     C*
301600090213     C                   clear                   Ds3C
301700090213     C                   Z-ADD     CODUT         TBLKUT
301800090213     C                   MOVEL     '3C'          TBLCOD
301900090213     C                   movel(p)  VABccm        TBLKEY
302000090213     C     KTABel        CHAIN     TABEL00F                           30
302100090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
302200090213     C                   MOVEL     TBLUNI        Ds3C
302300090213     C                   END
302400090213      *
302500090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
302600090213     c                   if        §3CFsp <> 'S'
302700090213      *
302800090213     C* NSP => Stacco un numeratore da AZNUM
302900090213     c                   movel     kpjbu         svkpjbu
303000090213     c                   clear                   kpjbu
303100090213     C                   clear                   TRUL33DS
303200090213     C                   eval      I33OPE = *zeros
303300090213     C                   eval      I33CNU = 302
303400090213     C                   eval      I33NUM = 1
303500090213     C                   movel     TRUL33DS      KPJBU
303600090213     C                   call      'TRUL33R'
303700090213     C                   parm                    KPJBA
303800090213     C                   movel     KPJBU         TRUL33DS
303900090213     c                   movel     svkpjbu       kpjbu
304000090213      ****
304100090213     C                   if        O33ERR = *zeros
304200090213     c                   z-add     o33nrf        NUM_SPED
304300090213     C                   else
304400090213     C                   endif
304500090213      ****
304600090213     c                   ELSE
304700090213      *   se si vuole mantenere il numero spedizione
304800090213      ****
304900090213     C                   MOVEL     WOGGI         WANNO             4 0
305000090213     C                   MOVE      WANNO         KAAA
305100090213     C                   Z-ADD     3             KCNU
305200090213     C                   Z-ADD     VABlnp        KFIL
305300090213     C     KNUF          CHAIN     FLNUF                              9999
305400090213     C     *IN99         IFEQ      '0'
305500090213     C                   ADD       1             NUFNUM
305600090213     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
305700090213     C                   UPDATE    FLNUF
305800090213     C                   ELSE
305900090213     C                   END
306000090213     C*
306100090213     c                   EndIF
306200090213      **
306300090213     C                   ENDSR
306400090213     C*----------------------------------------------------------------
306500090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
306600090213     C*----------------------------------------------------------------
306700090213     C     MEMCMR        BEGSR
306800090213     C*
306900090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
307000090213     C     §PTCM1        IFEQ      'S'
307100090213     C                   CLEAR                   EDRDE000
307200090213     C                   MOVEL     'TD1154'      KNMC
307300090213     C                   Z-ADD     1             KNRP
307400090213     C                   MOVEL     WKSC          KLNP1
307500090213     C     KRDE          CHAIN     EDRDE01L                           31
307600090213     C     *IN31         IFEQ      '1'
307700090213     C                   Z-ADD     VABAAS        RDEAAS
307800090213     C                   Z-ADD     VABLNP        RDELNP
307900090213     C                   Z-ADD     VABNRS        RDENRS
308000090213     C                   Z-ADD     VABNSP        RDENSP
308100090213     C                   MOVEL     'TD1154'      RDENMC
308200090213     C                   Z-ADD     1             RDENRP
308300090213     C                   MOVEL     WNRCMR        RDEVAL
308400090213     C                   WRITE     EDRDE000                             99
308500090213     c   99              eval      errori_in_Wrt = 'S'
308600090213     C                   END
308700090213     C*  Memorizzo qualificatore CMR
308800090213     C                   CLEAR                   EDRDE000
308900090213     C                   MOVEL     'TD1153'      KNMC
309000090213     C                   Z-ADD     1             KNRP
309100090213     C                   MOVEL     WKSC          KLNP1
309200090213     C     KRDE          CHAIN     EDRDE01L                           31
309300090213     C     *IN31         IFEQ      '1'
309400090213     C                   Z-ADD     VABAAS        RDEAAS
309500090213     C                   Z-ADD     VABLNP        RDELNP
309600090213     C                   Z-ADD     VABNRS        RDENRS
309700090213     C                   Z-ADD     VABNSP        RDENSP
309800090213     C                   Z-ADD     1             RDENRP
309900090213     C                   MOVEL     'TD1153'      RDENMC
310000090213     C                   MOVEL     'CMR'         RDEVAL
310100090213     C                   WRITE     EDRDE000                             99
310200090213     c   99              eval      errori_in_Wrt = 'S'
310300090213     C                   END
310400090213     C*  Memorizzo la data
310500090213     C                   CLEAR                   EDRDE000
310600090213     C                   MOVEL     'TD2380'      KNMC
310700090213     C                   Z-ADD     1             KNRP
310800090213     C                   MOVEL     WKSC          KLNP1
310900090213     C     KRDE          CHAIN     EDRDE01L                           31
311000090213     C     *IN31         IFEQ      '1'
311100090213     C                   Z-ADD     VABAAS        RDEAAS
311200090213     C                   Z-ADD     VABLNP        RDELNP
311300090213     C                   Z-ADD     VABNRS        RDENRS
311400090213     C                   Z-ADD     VABNSP        RDENSP
311500090213     C                   Z-ADD     1             RDENRP
311600090213     C                   MOVEL     'TD2380'      RDENMC
311700090213     C                   MOVEL     WDTCMR        RDEVAL
311800090213     C                   WRITE     EDRDE000                             99
311900090213     c   99              eval      errori_in_Wrt = 'S'
312000090213     C                   END
312100090213     C*
312200090213     C                   END                                                    §PTCM1 = 'S'
312300090213     C*
312400090213     C                   ENDSR
312500090213     C*----------------------------------------------------------------
312600090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
312700090213     C*----------------------------------------------------------------
312800090213     C     MEMAFB        BEGSR
312900090213     C*
313000090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
313100090213     C     §PTNF1        IFEQ      'S'
313200090213     C                   CLEAR                   EDRDE000
313300090213     C                   MOVEL     WKSC          KLNP1
313400090213     C                   MOVEL     'TD1154'      KNMC
313500090213     C                   Z-ADD     1             KNRP
313600090213     C     KRDE          CHAIN     EDRDE01L                           31
313700090213     C     *IN31         IFEQ      '1'
313800090213     C                   Z-ADD     VABAAS        RDEAAS
313900090213     C                   Z-ADD     VABLNP        RDELNP
314000090213     C                   Z-ADD     VABNRS        RDENRS
314100090213     C                   Z-ADD     VABNSP        RDENSP
314200090213     C                   MOVEL     'TD1154'      RDENMC
314300090213     C     §PTCM1        IFNE      'S'
314400090213     C     WNRCMR        OREQ      *BLANKS
314500090213     C                   Z-ADD     1             RDENRP
314600090213     C                   ELSE
314700090213     C                   Z-ADD     2             RDENRP
314800090213     C                   END
314900090213     C                   MOVEL     WNUMFV        RDEVAL
315000090213     C                   WRITE     EDRDE000                             99
315100090213     c   99              eval      errori_in_Wrt = 'S'
315200090213     C                   END
315300090213     C*  Memorizzo qualificatore CMR
315400090213     C                   CLEAR                   EDRDE000
315500090213     C                   MOVEL     WKSC          KLNP1
315600090213     C                   MOVEL     'TD1153'      KNMC
315700090213     C                   Z-ADD     1             KNRP
315800090213     C     KRDE          CHAIN     EDRDE01L                           31
315900090213     C     *IN31         IFEQ      '1'
316000090213     C                   Z-ADD     VABAAS        RDEAAS
316100090213     C                   Z-ADD     VABLNP        RDELNP
316200090213     C                   Z-ADD     VABNRS        RDENRS
316300090213     C                   Z-ADD     VABNSP        RDENSP
316400090213     C     §PTCM1        IFNE      'S'
316500090213     C     WNRCMR        OREQ      *BLANKS
316600090213     C                   Z-ADD     1             RDENRP
316700090213     C                   ELSE
316800090213     C                   Z-ADD     2             RDENRP
316900090213     C                   END
317000090213     C                   MOVEL     'TD1153'      RDENMC
317100090213     C                   MOVEL     'AFB'         RDEVAL
317200090213     C                   WRITE     EDRDE000                             99
317300090213     c   99              eval      errori_in_Wrt = 'S'
317400090213     C                   END
317500090213     C*  Memorizzo la data
317600090213     C                   CLEAR                   EDRDE000
317700090213     C                   MOVEL     WKSC          KLNP1
317800090213     C                   MOVEL     'TD2380'      KNMC
317900090213     C                   Z-ADD     1             KNRP
318000090213     C     KRDE          CHAIN     EDRDE01L                           31
318100090213     C     *IN31         IFEQ      '1'
318200090213     C                   Z-ADD     VABAAS        RDEAAS
318300090213     C                   Z-ADD     VABLNP        RDELNP
318400090213     C                   Z-ADD     VABNRS        RDENRS
318500090213     C                   Z-ADD     VABNSP        RDENSP
318600090213     C     §PTCM1        IFNE      'S'
318700090213     C     WNRCMR        OREQ      *BLANKS
318800090213     C                   Z-ADD     1             RDENRP
318900090213     C                   ELSE
319000090213     C                   Z-ADD     2             RDENRP
319100090213     C                   END
319200090213     C                   MOVEL     'TD2380'      RDENMC
319300090213     C                   MOVEL     WDATFV        RDEVAL
319400090213     C                   WRITE     EDRDE000                             99
319500090213     c   99              eval      errori_in_Wrt = 'S'
319600090213     C                   END
319700090213     C*
319800090213     C                   END                                                    §PTCM1 = 'S'
319900090213     C*
320000090213     C                   ENDSR
320100090213     c*==================================================================*
320200090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
320300090213     c*==================================================================*
320400090213     c     Imp_ADDR_mail begsr
320500090213      *
320600090213     c                   clear                   Indirizzi
320700090213      *
320800090213      *  Lunghezza indirizzi presenti
320900090213     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
321000090213     c                   z-add     1             al_char           3 0
321100090213     c                   z-add     1             dal_char          3 0
321200090213     c                   z-add     1             tot_char          3 0
321300090213      *
321400090213     c     successivo    tag
321500090213      *
321600090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
321700090213      *   e aggiunge il dominio Bartolini + il (;) separatore
321800090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
321900090213     c                   eval      tot_char = al_char - dal_char
322000090213     c                   z-add     dal_char      Dac
322100090213     c                   z-add     tot_char      Luc
322200090213      *
322300090213     c                   clear                   Indir_Bartol     40
322400090213     c                   clear                   Indir_Parz       20
322500090213     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
322600090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
322700090213     c                             %Trim(bart_it)
322800090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
322900090213      *
323000090213     c                   if        al_char = Lun_Ind
323100131001     C                   LEAVEsr
323200090213     c                   else
323300090213     c                   eval      dal_char = ( al_char + 1 )
323400090213     c                   goto      successivo
323500090213     c                   end
323600090213      *
323700090213     C                   ENDSR
323800090213     c*==================================================================*
323900090213      * Manda un Msg x E-mail
324000090213     c*==================================================================*
324100090213     c     Invio_Mail    begsr
324200090213      *
324300131001      *    DA SALTARE
324400131001      *    DA SALTARE
324500131001      *    DA SALTARE
324600131001     C                   LEAVEsr
324700090213      *
324800131001     C*   Alert_mail : invio Mail a CED@Brt.it                  *
324900090213     C* Inizializzo variabili
325000090213     C                   movel     *blanks       wrkEml          253
325100090213     C                   movel     *blanks       wrkMsg         5000
325200090213     C                   movel     *blanks       wrkOgg           44
325300090213      *
325400090213     C* Valorizzo i campi della e-m@ail - indirizzo
325500090213     C                   eval      wrkEml = Indirizzi
325600090213     C                   eval      wrkOgg = Oggetto
325700090213     C                   eval      wrkMsg = Messaggio
325800090213
325900090213     C                   call(e)   'TIS701C'
326000090213     C                   parm                    wrkEml
326100090213     C                   parm                    wrkOgg
326200090213     C                   parm                    wrkMsg
326300090213     C*
326400090213     C                   ENDSR
326500090603     c*==================================================================*
326600090603      * Manda un Msg x E-mail a EDPAB
326700090603     c*==================================================================*
326800090603     c     Invio_Mail_AB begsr
326900090603      *
327000131001     C*   Alert_mail : invio Mail a CEDalert@Brt.it                  *
327100090603     C* Inizializzo variabili
327200090603     C                   movel     *blanks       wrkEml          253
327300090603     C                   movel     *blanks       wrkMsg         5000
327400090603     C                   movel     *blanks       wrkOgg           44
327500090603      *
327600090603     C* Valorizzo i campi della e-m@ail - indirizzo
327700110203     C                   eval      wrkEml = Indirizzi
327800090603     C                   eval      wrkOgg = Oggetto
327900090603     C                   eval      wrkMsg = Messaggio
328000110203     C*
328100110203     C                   call(e)   'TRTCT00R2'
328200110203     C                   parm                    wrkEml
328300110203     C                   parm                    wrkOgg
328400110203     C                   parm                    wrkMsg
328500090603     C*
328600090603     C                   ENDSR
328700121105      *---------------------------------------------------------------*
328800121105      * CTRL caricamento schiere    PT                               -*
328900121105      *---------------------------------------------------------------*
329000121105     C     Check_PT      begsr
329100121105     C*
329200121105     c* Controllo riempimento schiera
329300121105     c                   clear                   trul0sds
329400121105     c                   eval      i0sski='CPT'
329500121105     c                   eval      i0sele=%elem(CPT)
329600121105     c                   eval      i0spie=x
329700121105     c                   eval      i0sfile='EDTAB00F'
329800121105     c                   eval      i0ssif=knsif
329900131001     c                   eval      i0spgm='TRTCT17R'
330000121113     c                   move      kpjbu         SvKpjbu
330100121113     c                   clear                   kpjbu
330200121105     c                   movel     trul0sds      kpjbu
330300121105     c                   call      'TRUL0SR'
330400121105     c                   parm                    kpjba
330500121113     c                   eval      kpjbu  =  SvKpjbu
330600121105     C*
330700121105     C                   ENDSR
330800090211     O*****************************************************************
330900090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
331000090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
331100090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
331200090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
