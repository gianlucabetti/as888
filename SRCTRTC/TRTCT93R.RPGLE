000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600100316????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFCSUM vers S93A  *
000700060612      **?************************************************************************
000800991124      * Il pgm crea:                                                            *
000900020919      *             EDivab3L file spedizioni da confermare per camion           *
001000020919      *             FiVAB01L file spedizioni da confermare                      *
001100060609      **?************************************************************************
001200090213     Ftivin00r  uF   E             DISK    usropn  INFDS(VINRECDS)
001300991124      *
001400991206     FFLNUF01L  UF   E           K DISK
001500090213     FEDRDE01L  UF A E           K DISK    commit
001600050112     FTABEL00F  IF   E           K DISK
001700090209     FCNACO00F  IF   E           K DISK
001800090209     FCNIND00F  IF   E           K DISK
001900090210     FEDTAB01L  IF   E           K DISK
002000090210      *
002100091019???? Fedstbl01l IF   E           K DISK
002200111027      *
002300111027???? Ftitas30c  IF   E           K DISK
002400090209      *
002500090213     FEDiVAB1L  UF A E           K DISK    commit
002600090213     FEDiVAD0F  O  A E           K DISK    commit
002700090213     FEDiVAT0F  O  A E           K DISK    commit
002800090209      *
002900090213     FEDSUM00F  O  A E           K DISK    commit   INFDS(SRECDS)
003000100701      *----------------------------------------------------*
003100100701      * Stringhe per conversione alfanumerico/numerico N.Documento
003200100701     D CA              S             78    DIM(1) CTDATA PERRCD(1)
003300100701     D CN              S             78    DIM(1) CTDATA PERRCD(1)
003400940321      *----------------------------------------------------*
003500090209      * numero relativo di record
003600090209     D SRECDS          DS
003700090209     D  NRREC                397    400B 0
003800090209      *
003900090213      * numero relativo di record
004000090213     D VINRECDS        DS
004100090213     D  VNREC                397    400B 0
004200100701      * ---------------------------------------------------
004300100701     D KPJBA         E DS
004400100701     D     svkpjbu     s                   like(kpjbu)
004500100701     D FNLV55DS      E DS
004600100701     D TIBS55DS      E DS
004700100701     D TISI95DS      E DS
004800100701     D UTEDSE0F      E DS
004900100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
005000100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
005100100701      * Ds scomposzione tipo capoconti
005200100701     D TCUDS           DS
005300100701     D  F34                    3      4
005400100701     D  F56                    5      6
005500100701      *
005600100701      * Numeratore Bolle (302)
005700100701     D trul33ds      E DS
005800100701     D Ds3C          E DS
005900100701     D DS1B          E DS
006000100701     d sav_DS1B        s                   like(Ds1B)
006100100701     D DS15          E DS
006200100701     D DSCV          E DS
006300100701     D TRTC86DS      E DS
006400100701     D EDIDSPT       E DS
006500100701     D EDIDSPU       E DS
006600160205     D EDIDSPS       E DS
006700100701     D EDIDSPV       E DS
006800120629     D EDIDSPZ       E DS
006900100701     D EDIDSCL       E DS
007000100701     D EDIDSSS       E DS
007100100701     D EDIDSTC       E DS
007200100701      **
007300100701      *  DS x salvataggio dati impostati in EDiVAB
007400100701     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
007500100701     D SALVA           DS           545
007600100701      **
007700100701     D WLBDA8          DS
007800100701     D  G02DAT                 1      8  0
007900100701     D  G02INV                 9     16  0
008000100701     D  G02ERR                17     17
008100100701     D  G02TGI                18     22  0
008200090213      *
008300111027      *----------------------------------------------------*
008400111027      *  DS x scomposizione numero spedizione
008500111027      *----------------------------------------------------*
008600111027     D DSSPE           DS
008700111027     D  SPEAAS                 1      4  0
008800111027     D  SPELNP                 5      7  0
008900111027     D  SPENRS                 8      9  0
009000111027     D  SPENSP                10     16  0
009100111027      **
009200111027     D Sped_RESO       S              1
009300100701      *----------------------------------------------------*
009400090205      **  Elenco DS di tutti i Segmenti da tradurre
009500100701      *----------------------------------------------------*
009600091019???? D edS00BGM      E DS
009700100316???? D edS00CNI      E DS
009800091019???? D edS00CNT      E DS
009900091019???? D edS00COM      E DS
010000091019???? D edS00CTA      E DS
010100100316???? D edS00DGS      E DS
010200091019???? D edS00DTM      E DS
010300100316???? D edS00EQD      E DS
010400100316???? D edS00EQN      E DS
010500100628???? D edS00FTX      E DS
010600100628???? D  D05                   16    365    DIM(5)
010700091019???? D edS00GID      E DS
010800100316???? D edS00LOC      E DS
010900091019???? D edS00MEA      E DS
011000100628???? D edS00MOA      E DS
011100091019???? D edS00NAD      E DS
011200100316???? D edS00PIA      E DS
011300091019???? D edS00PCI      E DS
011400091019???? D  D30_1                  4    353    DIM(10)
011500091019???? D edS00RFF      E DS
011600100316???? D edS00SEL      E DS
011700091019???? D edS00TDT      E DS
011800091019???? D edS00TOD      E DS
011900091019???? D edS00TSR      E DS
012000091019???? D edS00UNA      E DS
012100091019???? D edS00UNB      E DS
012200091019???? D edS00UNH      E DS
012300091019???? D edS00UNT      E DS
012400091019???? D edS00UNZ      E DS
012500100701      *----------------------------------------------------*
012600090210      **
012700090210     D  X30            S             35    DIM(20)                              generico segnacolli
012800090205      **
012900090209     D Valori_inErr    ds
013000090209     D  SKout_Errori                  1    DIM(50)
013100100630      **
013200090209     D Descr_Errore    ds
013300090209     D  SKout_DesErr                 50    DIM(50)
013400090209      *
013500090205      *----------------------------------------------------*
013600091016     D EoFTivin        s              1
013700091016      *----------------------------------------------------*
013800090205     D Tipo_seg        S              3
013900100720     D Trovato_seg     S              1
014000100716      *
014100100716     d keyUNBCLI       s             35
014200100716     d keyTIPOMSG      s              6
014300100716     d keyVERSION      s              3
014400100716     d keyRELEASE      s              3
014500100716     d keyAGENCY       s              3
014600100716     d keyASSOCIA      s              6
014700100716      *
014800100720     D DsGenerica      s           2048
014900090209     D segmento        s           2048
015000090209     D Errore          s              1
015100100701      *
015200000223     D W0140           S             14  0
015300991129     D WORA            S              6  0
015400100701     D DataGGMMAAA     S              8  0
015500100701     D DATEU           S              8  0
015600100701     D DATA_eur        S               D   DATFMT(*eur)
015700100701      *
015800100630     D sk              S              3  0 INZ
015900100630     D sx              S              3  0 INZ
016000100628     D XX              S              3  0 INZ
016100110328     d Key_Generica35  s             35
016200100630     D NUM_Spediz      s                   LIKE(vabnsp)
016300100708     D UNT_record      s                   LIKE(vnREC)
016400100708     D IniDET_RECord   s                   LIKE(vnREC)
016500100708     D FinDET_RECord   s                   LIKE(vnREC)
016600090213     D Last_RECord     s                   LIKE(vnREC)
016700110105     D UNZ_SEGMENTO    s              1
016800100318      *------------------------------------------
016900100318      **  Definizione Qualificatori
017000100318      *------------------------------------------
017100100319      *
017200100628???? D Documento_Manifest...
017300100628???? D                 s              3    INZ('785')
017400100701???? D Totale_Peso_Lordo...
017500100701???? D                 s              3    INZ('7  ')
017600100701???? D Totale_Nr_Spedizioni...
017700100701???? D                 s              3    INZ('10 ')
017800100701???? D Totale_Nr_Colli...
017900100701???? D                 s              3    INZ('11 ')
018000100701???? D Totale_Peso_Netto...
018100100701???? D                 s              3    INZ('ZCW')
018200100628???? D Riferimento_Spedizione...
018300100628???? D                 s              3    INZ('AGE')
018400100708???? D Riferimento_Numerico...
018500100708???? D                 s              3    INZ('CU ')
018600100318???? D Riferimento_Cliente_particolare...
018700100318???? D                 s              3    INZ('FF ')
018800100701???? D X_Cargo_Manifest...
018900100318???? D                 s              3    INZ('AFB')
019000100701???? D X_Numero_CMR...
019100100318???? D                 s              3    INZ('CMR')
019200100318???? D Telephone_Number...
019300100318???? D                 s              3    INZ('TE ')
019400100318???? D TeleFax_Number...
019500100318???? D                 s              3    INZ('TX ')
019600100628???? D Porto_Assegnato...
019700100628???? D                 s              3    INZ('001')
019800100628???? D Porto_Franco...
019900100628???? D                 s              3    INZ('002')
020000100628???? D Porto_Franco_uncleared...
020100100628???? D                 s              3    INZ('003')
020200100628???? D Porto_Franco_domicile...
020300100628???? D                 s              3    INZ('004')
020400100318???? D Volume...
020500100318???? D                 s              3    INZ('VOL')
020600100705???? D Weight...
020700100705???? D                 s              3    INZ('WT ')
020800100319???? D G_Weight...
020900100319???? D                 s              3    INZ('G  ')
021000100318???? D Gross_Weight...
021100100318???? D                 s              3    INZ('AAG')
021200100706???? D Gross_Weight_E...
021300100706???? D                 s              3    INZ('AAE')
021400110512???? D Gross_Weight_D...
021500110512???? D                 s              3    INZ('AAD')
021600100706???? D N_Weight...
021700100706???? D                 s              3    INZ('N  ')
021800100706???? D Net_Weight...
021900100706???? D                 s              3    INZ('AAF')
022000100318???? D Kilograms...
022100100318???? D                 s              3    INZ('KGM')
022200100319???? D Grams...
022300100319???? D                 s              3    INZ('163')
022400100318???? D Meters...
022500100318???? D                 s              3    INZ('MTR')
022600100318???? D Cubic_Meters...
022700100318???? D                 s              3    INZ('MTQ')
022800100318???? D Shipper_assigned...
022900100318???? D                 s              3    INZ('24 ')
023000100705???? D Data_senza_ora...
023100100628???? D                 s              3    INZ('102')
023200100705???? D Data_con_ora...
023300100628???? D                 s              3    INZ('203')
023400100705???? D Data_con_i_secondi...
023500100705???? D                 s              3    INZ('204')
023600100630???? D Squadratura_Colli...
023700100630???? D                 s              1    INZ('N')
023800120613     D controlla_COD_FTX...
023900120613???? D                 s              3
024000100316      *---------------------------------------------------------------------*
024100100316      **  segmenti Identificativi parti specifiche del messaggio
024200100316      *---------------------------------------------------------------------*
024300100701???? D Segm_Inizio_Messaggio...
024400100701???? D                 s              3    INZ('UNB')
024500100701???? D Segm_Testata_Dettagli...
024600100701???? D                 s              3    INZ('UNH')
024700100701???? D Segm_Inizio_Dettaglio...
024800100316???? D                 s              3    INZ('CNI')
024900100701???? D Segm_Fine_Dettagli...
025000100316???? D                 s              3    INZ('UNT')
025100100701???? D Segm_Fine_messaggio...
025200100316???? D                 s              3    INZ('UNZ')
025300100701???? D seg_imprevisto...
025400100701???? D                 s              1
025500100726???? D Forza_Uscita...
025600100726???? D                 s              1
025700100701      *---------------------------------------------------------------------*
025800100701      *
025900100701???? D NAD_destinatario_in_testata...
026000100701???? D                 s              1
026100100701???? D NAD_destinatario_in_dettaglio...
026200100701???? D                 s              1
026300100701???? D NAD_mittente_in_testata...
026400100701???? D                 s              1
026500100701???? D NAD_mittente_in_Dettaglio...
026600100701???? D                 s              1
026700100701     d CTA_destinatario_in_Dettaglio...
026800100701     d                 s             35A
026900100701     d CTA_destinatario_in_testata...
027000100701     d                 s             35A
027100100701     d COM_telefono_in_Dettaglio...
027200100701     d                 s             35A
027300100701     d COM_telefono_in_testata...
027400100701     d                 s             35A
027500110907     d FTX_ServizioMail_xDestinatario...
027600110907     d                 s             70A
027700131025     d FTX_MAIL_come_Servizio...
027800131025     d                 s              1A
027900131025     d FTX_ServizioSMS_xDestinatario...
028000131025     d                 s             70A
028100131025     d FTX_SMS_come_Servizio...
028200131025     d                 s              1A
028300120614     d FTX_ai_piani    s              1A
028400111121???? D NAD_Lunghezza_Indir_Aggiuntivo...
028500111121???? D                 s              3s 0
028600111121???? D NAD_Lunghezza_Indirizzo...
028700111121???? D                 s              3s 0
028800111121???? D NAD_Lunghezza_ragSociale2...
028900111121???? D                 s              3s 0
029000100701      *
029100090209      *---------------------------------------------------------------------*
029200100701???? D Dati_di_Testata...
029300100701???? D                 s              1    INZ('S')
029400100701???? D Contatore_Dettagli...
029500100701???? D                 s              5s 0 INZ(0)
029600100728???? D Segmento_in_errore...
029700100728???? D                 s              5s 0 INZ(0)
029800100701      *---------------------------------------------------------------------*
029900100628      *
030000100701      *---------------------------------------------------------------------*
030100100630     d UNB_diWork      s                   like(UNB0004)
030200100628     d  UNB_Mittente   s                   like(UNB0004)
030300100706     d BGM_Id_Testata  s             35A   inz(' ')
030400100706     d  BGM_NrFviaggio...
030500100628     d                 s             35A
030600100706     d  BGM_nrCMR      s             35A
030700100701     d  RFF_NrFviaggio...
030800100701     d                 s             35A
030900100701     d  RFF_NrCMR      s             35A
031000100701     d  RFF_RifSpediz  s             35A
031100100701     d UNB_parziale    s              1    inz('N')
031200100813     d UNB_Generico    s             31    inz(' ')
031300100701     d UNB_NrTrasmiss  s             14    inz(' ')
031400100701     d UNZ_NrTrasmiss  s             14    inz(' ')
031500100701     d UNH_Rifer_Msg   s             14    inz(' ')
031600100701     d UNH_VABrma      s                   like(VABrma)
031700100701     d UNH_VABrmn      s                   like(VABrmn)
031800120629     d NAD_SF_Cliente  s             35    inz(' ')
031900100701      *
032000100701      * Tabella partner
032100121105     D PT_key          S             35    DIM(200)
032200121105     D PT_Parz         S             35    DIM(%elem(PT_key))
032300121105     D PT_KSC          S              7    DIM(%elem(PT_key))
032400121105     D PT_uni          S             90    DIM(%elem(PT_key))
032500121105     D PU_uni          S             90    DIM(%elem(PT_key))
032600121105     D PV_uni          S             90    DIM(%elem(PT_key))
032700121105     D PZ_uni          S             90    DIM(%elem(PT_key))
032800160205     D PS_uni          S             90    DIM(%elem(PT_key))
032900121105     D TRUL0SDS      E DS
033000100701      *
033100100701     d PT_KeyXsav      s              3  0 inz(0)
033200100701     D PT_trovata      s              1    inz('N')
033300100701     D PU_key          S             35
033400100701     D PV_key          S             35
033500160205     D PS_key          S             35
033600120629     D PZ_key          S             35
033700100701     D PU_HLD_SpedVAX  S              1
033800100813     D PT_con_piu_Nazioni...
033900100813     d                 s              1
034000120629     D PZ_abilita_NAD_SF_come_RFF_FF...
034100120629     d                 s              1
034200100701      *
034300160209     D  PS_vabFGS      s              3A
034400100701     D  PT_vabLNP      s                   LIKE(vablnp)
034500100701     D  PT_vabNRS      s                   LIKE(vabnrs)
034600100701     D  PT_vabCTM      s                   LIKE(vabctm)
034700100701     D  PT_vabCCM      s                   LIKE(vabccm)
034800100701     D  PU_lunVAT      s              2s 0
034900100702     D  PU_TipoServ    s              2a
035000100702     D  PU_SpecServ    s              2a
035100111027     D  PU_in_RMN_BOLLA_EXPORT_x_RESO...
035200111027     d                 s              1
035300111027     D  era_un_export_RESO_dal_PARTNER...
035400111027     d                 s              1
035500140610      *  qui viene riportato il codice della nostra ex bolla export o del ORM
035600140610      *  mandato dal Partner in un particolare segmento concordato
035700140610     D  Cod_RESO_ORM...
035800140610     d                 s             14
035900140623     D  Salva_REF_ORM...
036000140623     d                 s             14
036100100701      *
036200100701     D Nr_PT           S              3  0 INZ
036300100701     D savXX_PT_key    S              3  0 INZ
036400100701      *
036500100630     d  DTM_DtParten   s              8s 0
036600100630     d  DTM_DtFViag    s              8s 0
036700100630     d  DTM_DtCMR      s              8s 0
036800100630     d  DTM_OraFViag   s              4s 0
036900100630     d  DTM_OraCMR     s              4s 0
037000100702     d  DTM_DtArrivo   s              8s 0
037100100702     d  DTM_OraArrivo  s              4s 0
037200100706     d  DTM_OraParten  s              4s 0
037300100702     d  CNI_Identificativo_Bolla...
037400100702     d                 s             35A
037500101027     d  CNI_seNumerico...
037600101027     d                 s                   LIKE(vabRMN)
037700100630      *
037800100701     d  GID_aTotale    s              1A
037900100705     d  GID_ColliInAdd...
038000100705     d                 s                   like(gid722420)
038100100701      **
038200090209      *  DS x scomposizione segnacollo
038300100630     D DS_SegnBART     DS
038400100630     D  SegnBART_LNP           1      3  0
038500100630     D  SegnBART_LNA           4      6  0
038600100630     D  SegnBART_NRS           7      8  0
038700100630     D  SegnBART_NSC           9     15  0
038800100630     D  SegnBART_ZNC          16     17  0
038900100701      **
039000090209      *---------------                                                      *
039100100701     D VABtic_work     s                   LIKE(vabtic)
039200110623     d inviato_tipo_incasso...
039300110623     d                 s              1
039400100630      *
039500090603     d tes_VABrmo      s                   LIKE(VABrmo)
039600090603     d tes_VABnmo      s                   LIKE(VABnmo)
039700090603     d tes_VABcmo      s                   LIKE(VABcmo)
039800090209     C*
039900090603     d tes_VABrsd      s                   LIKE(VABrsd)
040000090603     d tes_VABrd2      s                   LIKE(VABrd2)
040100090603     d tes_VABind      s                   LIKE(VABind)
040200090603     d tes_VABlod      s                   LIKE(VABlod)
040300090603     d tes_VABnzd      s                   LIKE(VABnzd)
040400090603     C*
040500090603     d tes_Valuta      s                   LIKE(VABvca)
040600090603     C*
040700090209      *---------------------------------------------------------------------*
040800090209      * Schiere
040900090209      *---------------------------------------------------------------------*
041000090209      * Schiera per reperimento nr.seganacollo
041100100630     D segn_BAR        S              7  0 DIM(5000)
041200100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
041300100630      *
041400100630      *
041500090209      * Nr. documento alfanumerico da decodificare
041600090209     D NDA             S              1    DIM(35)
041700100630      *
041800090209      * Nr. documento alfanumerico da già decodificato
041900090209     D NDN             S              1    DIM(15)
042000100630      *
042100090209      * Messaggi di errore
042200090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
042300100630      *
042400090209      * Tabella codici trattamento merce (1B)
042500100701     D Cod_Tb_CTM      S              2    DIM(200)
042600100701     D Des_Tb_CTM      S             67    DIM(200)
042700100630      *
042800090209      * Tabella codici valuta
042900090209     D CCV             S              3    DIM(200)
043000090209     D DCV             S             89    DIM(200)
043100100630      *
043200090209      * Tabella codici nazioni
043300090209     D C15             S              3    DIM(500)
043400090209     D ISO             S              3    DIM(500)
043500090209     D CPF             S              2  0 DIM(500)
043600100630      *
043700090209      * Tabella CL --> codici clienti - tariffa
043800130305     D Cod_Tb_CL       S             35    DIM(999)
043900130305     D Des_Tb_CL       S             90    DIM(999)
044000100630      *
044100090209      * Schiere x caricamento cod.cliente  <>
044200130305     D skCLienti       S              7  0 DIM(999)
044300100630      *
044400100318      * Priorità trasporto
044500100705     D Key_TipServ     S             35    DIM(100)
044600100702     D TipoServizio    S              1    DIM(100)
044700100702      *
044800100702      * Decodifiche servizi speciali
044900100702     D SpecialServ     S             35    DIM(100)
045000100702     D Key_ServSpec    S              3    DIM(100)
045100100702     D UNI_ServSpec    S             90    DIM(100)
045200100630      *
045300090209      * Termini di consegna
045400110328     D K35_TpBolla     S             35    DIM(100)
045500110328     D Cod_TpBolla     S              3    DIM(100)
045600110328     D Decod_Porto     S              1    DIM(100)
045700110328     d Trovata_Tab_TB  S              1
045800100630      *
045900090216      * Tipo pagamento C/Assegno
046000100705     D K35_TpIncas     S             35    DIM(100)
046100100705     D TipoIncasso     S              2    DIM(100)
046200100630      *
046300090209      * Schiere x routine di conversione CAP
046400090209     D CAP5            S              1    DIM(5)
046500090209     D CAP9            S              1    DIM(9)
046600090209     D CAPM            S              1    DIM(9)
046700100630      *
046800090209      * Schiera per memorizzazione FTX ricevuti
046900090209     D QUA             S              3    DIM(100)
047000090209     D FTX             S             70    DIM(100)
047100100630      *
047200090209      * Tabelle capiconto
047300090209      * Schiere x località
047400090209     D LOD             S              1    DIM(35)
047500100630      *
047600060608      **?------------------------------------------------------------------ */
047700090601     C*? Ds Scrittura del VAT "E"
047800060608      **?------------------------------------------------------------------ */
047900060612     D XTOEBCDDS     E DS
048000060620      *
048100060608      **?------------------------------------------------------------------ */
048200100701      *
048300090209      **?------------------------------------------------------------------ */
048400090209     C* Definizione campi di work
048500090209     D kKUT            s                   LIKE(TBLKUT)
048600090209     D kCOD            s                   LIKE(TBLCOD)
048700090209     D kKEY            s                   LIKE(TBLKEY)
048800090209     D kKCC            s                   LIKE(ACOKCC)
048900090209     D kKSC            s                   LIKE(ACOKSC)
049000090209     D kAAA            s                   LIKE(NUFAAA)
049100090209     D kCNU            s                   LIKE(NUFCNU)
049200090209     D kFIL            s                   LIKE(NUFFIL)
049300090209     D kAAS            s                   LIKE(VABAAS)
049400090209     D kLNP            s                   LIKE(VABLNP)
049500090209     D kNRS            s                   LIKE(VABNRS)
049600090209     D kNSP            s                   LIKE(VABNSP)
049700090209     D kCMR            s                   LIKE(VABCMR)
049800090209     D kCCM            s                   LIKE(VABCCM)
049900090209     D kNCD            s                   LIKE(VADNCD)
050000090210     D kCNT            s                   LIKE(VADCNT)
050100090209     D kNMC            s                   LIKE(RDENMC)
050200090209     D kNRP            s                   LIKE(RDENRP)
050300090209     D kLNP1           s                   LIKE(RDELNP)
050400090209      *
050500090209      *  Invio E-mail
050600090209     D Indirizzi       s            253
050700090209     D Oggetto         s             44
050800090209     D Messaggio       s           5000
050900090209      *
051000090209     d   DSmail        ds
051100090209     D Mail_Ind                      44
051200090209     d   IND_char                     1    dim(44)
051300090209      *
051400100630     D Lunghezza_Indirizzo...
051500100630     D                 s              5u 0
051600090209      *
051700100630     D dalCarattere    s              5u 0
051800100630     D xTOTcaratteri   s              5u 0
051900090209      *
052000090209     C*---------------------------------------------------------------*
052100090209     D MsgDSP          S            256                                         Testo generico
052200090209     C*---------------------------------------------------------------*
052300090209     D SndMg01         S             10                                         Message type
052400090209     D                                     INZ('*INFO')
052500090209     D SndMg02         S             10                                         Deluvery mode
052600090209     D                                     INZ('*BREAK')
052700090209     D SndMg03         S            256                                         Message text
052800090209     D SndMg04         S             10I 0                                      Length of text
052900090209     D                                     INZ(%SIZE(SndMg03))
053000090209     D SndMg05         S             10                                         User profile
053100090209     D                                     DIM(299)
053200090209     D SndMg06         S             10I 0                                      Number of user
053300090209     D SndMg07         S             10I 0                                      Message sent indic.
053400090209     D SndMg08         S             10I 0                                      Function requested
053500090209     D SndMg10         S              1                                         Show display
053600090209     D                                     INZ('N')
053700090209     D SndMg11         S             20                                         Qualified MSGQ name
053800090209     D SndMg12         S              4                                         Name type indicator
053900090209     D                                     INZ('*USR')
054000090209      * indice di scihera
054100090209     D Jx              S              5I 0
054200090209      *
054300090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
054400090209      *
054500090209     d bart_it         C                   CONST('@Bartolini.it;')
054600090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
054700060612      * ?================================================================== */
054800060612      * ?   * Campi x decodifica * (INPUT  del Record)
054900100319      * ?================================================================== */
055000090130     D  Dati           s           2048
055100060612      * ?   *--------------------------------------------------------------*
055200060612     D  se_errore      s              1    inz(' ')
055300060612      * ?* ------------------------------------------------------ *
055400060710     D Digits          C                   '0123456789'
055500091019     D*------------------
055600091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
055700091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
055800060612      * ?================================================================== */
055900060612      *   Ciclo principale
056000090205      * ?================================================================== */
056100060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
056200060612     C*
056300060612     C                   if        not %open(tivin00r)
056400060612     C                   open      tivin00r
056500060612     C                   endif
056600060612      **
056700060612      * ?------------------------------------------------------------------ */
056800100708     C*  Controllo dati arrivati da DPD
056900060612      * ?------------------------------------------------------------------ */
057000060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
057100060612      * ?- controllando che la trasmissione sia completa.
057200090205      **
057300060612      * ?              /*---------------------- */
057400090205     c                   goto      nonFare_adesso
057500060612     c                   exsr      check_Trasm
057600090205     c     nonFare_adessotag
057700060612      * ?              /*---------------------- */
057800090205      **
057900060613      **  Errore di trasmissione x tutti i records
058000060613      **   --> file in errore
058100060613     c                   if        se_errore = 'S'
058200060612
058300060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
058400090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
058500090205     c                   exsr      Msg_errato
058600060612      **
058700060612     c                   else
058800060614      **
058900060612      * ?------------------------------------------------------------------ */
059000100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
059100060612      * ?------------------------------------------------------------------ */
059200060612      * ?              /*---------------------- */
059300060612     c                   exsr      Importa_Msg
059400060612      * ?              /*---------------------- */
059500060612
059600060614     c                   end
059700060612      *
059800060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
059900090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
060000090225     c                             esito ='0'
060100060710     C                   eval      esito ='0'
060200090225      *
060300090225     c                   COMMIT
060400110208     C                   endif
060500090224      *
060600090213     c                   if        almeno_un_due ='S'
060700110208     C                   eval      esito ='1'
060800060614     C                   endif
060900100708      * chiude TIVIN
061000100708     C                   if        %open(tivin00r)
061100100708     C                   close     tivin00r
061200100708     C                   endif
061300060614      *
061400060612     c                   SETON                                        LR
061500060612      * ?================================================================== */
061600100708      *? Importa i records della trasmissione
061700060612      * ?------------------------------------------------------------------ */
061800060612     c     Importa_Msg   Begsr
061900060614
062000130506      *   Inizia il conteggio totale delle righe
062100130506     c                   z-add     0             total_segmenti    5 0
062200100726     c                   clear                   Forza_Uscita
062300060612     c     *start        setll     Tivin00r
062400100708
062500100708     c                   EXSR      LEGGE_TIVIN_R
062600100708     c                   dow       EoFTivin <> 'S'
062700060614
062800060614      * solo i record sflaggati da rielaborare
062900110107     c                   IF        vinFLG = *blank
063000110107      *** ++++++
063100090211      *  Sempre Record OK
063200090211      ** Controlli formali sui campi
063300090211     C                   eval      vinFLG = '1'
063400060612     c                   clear                   se_errore
063500110107      *
063600110107      *** >>>>>>
063700110107     c                   IF        vinDTA <> *blank
063800110107      *** >>>>>>
063900060612
064000060612      ** Decodifica record a campi variabili
064100060612      * ?              /*---------------------- */
064200090205     c                   exsr      Decod_Segmento
064300090209      * ?              /*---------------------- */
064400060612
064500060621      *  x errore bloccante nella composizione del VAB/VAT
064600060621     c                   if        err_bloccante ='S'
064700060621     C                   eval      vinFLG = '2'
064800110208     c                   eval      Almeno_Un_Due ='S'
064900090211     c                   eval      esito  = '2'
065000100317???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
065100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
065200090603     C                             Bolle import in filiale - messaggio -
065300090603     C                             con UNB non presente in tabella PT controlla-
065400090603     C                             re EDI ricevuto su UPLOAD.'
065500100319     c                   if        §PVinv <> 'N'
065600100319     c                   exsr      invio_mail
065700060621     c                   end
065800100319     c                   end
065900110107      *** >>>>>>
066000110107     c                   endIF
066100110107      *** >>>>>>
066200110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
066300110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
066400110107      *         un erroneo messaggio
066500110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
066600060612     c                   update    Tivin000
066700090211
066800090211      *** se non è riconosciuto l'UNB deve uscire direttamente
066900090211     c                   if        se_errore ='S'  and
067000100708     c                             tipo_seg = Segm_Inizio_Messaggio
067100090213     c                   eval      err_bloccante = 'S'
067200090211     c                   exsr      Msg_errato
067300090211     c                   LEAVE
067400090211     c                   endIF
067500110107      *** ++++++
067600110107     c                   endIF
067700060612
067800100708     c                   EXSR      LEGGE_TIVIN_R
067900060612     C                   ENDdo
068000060612      **
068100060612     c                   endSR
068200100709      * ?------------------------------------------------------------------ */
068300100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
068400100709      * ?------------------------------------------------------------------ */
068500100709     C     MSG_Errato    BEGSR
068600100709      *
068700100709      * ?  Scrive su tutti i records il tipo di errore
068800100709     c     *start        setll     tivin00r
068900100709     c
069000100709     c                   EXSR      LEGGE_TIVIN_R
069100100709     c                   dow       EoFTivin <> 'S'
069200100709     c
069300100709     c                   if        vinMSG  = *blank
069400100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
069500100709     C                               >> Controllare i DATI su UPLOAD !!'
069600100709     c                   end
069700100709     C                   eval      vinFLG = '2'
069800100709     c                   eval      Almeno_Un_Due ='S'
069900100709     c                   eval      esito  = '2'
070000100709     c                   eval      se_errore ='S'
070100100709     c                   eval      err_bloccante ='S'
070200100709      *
070300100709     c                   update    tivin000
070400100709     c                   EXSR      LEGGE_TIVIN_R
070500100709     c                   endDO
070600100709      *
070700100709     C                   ENDSR
070800100709      * ?------------------------------------------------------------------ */
070900100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
071000100709      * ?------------------------------------------------------------------ */
071100100709     C     DETta_Errato  BEGSR
071200100709      *
071300100709     c                   EXSR      LEGGE_TIVIN_R
071400100709     c                   dow       EoFTivin <> 'S'
071500100709      *
071600100709      * Se ha letto il successivo record
071700100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
071800100709     c                   if        VNREC = UNT_record  or
071900100709     c                             VNREC = finDET_RECord
072000100709     c                   leave
072100100709     c                   end
072200100709      *
072300100709     c                   exsr      Error_DET
072400100709      *
072500100709     c                   if        vinMSG  = *blank
072600100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
072700100709     C                             >> Controllare i DATI di ogni Segmento <<'
072800100709     c                   end
072900100709      *
073000100709     c                   update    tivin000
073100100709     c                   EXSR      LEGGE_TIVIN_R
073200100709     c                   endDO
073300100709      * ------
073400100709???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
073500100709     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
073600100709???? C                             Bolle IFCSUM non totalmente tradotto. -
073700100709     C                             Controllare UPLOAD del cliente:'
073800100709     C                   EVAL      Messaggio = %trim(Messaggio) +
073900100709     C                                         %editc(§PTksc:'X')
074000100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
074100100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
074200100709     c                   if        §PVinv <> 'N'
074300100709     c                   exsr      invio_mail
074400100709     c                   end
074500100709      *
074600100709      * ripulisce per un nuovo giro
074700100709     C                   clear                   Err_in_detta
074800100709      *
074900100709     C                   ENDSR
075000100709      * ?------------------------------------------------------------------ */
075100100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
075200100709      * ?------------------------------------------------------------------ */
075300100709     C     LEGGE_TIVIN_R BEGSR
075400100709      *
075500100709     c                   clear                   EoFTivin
075600100709      *  Lettura sequenziale
075700100709     c                   read      Tivin00r
075800100709
075900100709     c                   if        %Eof(tivin00r)
076000100709     c                   move      'S'           EoFTivin
076100100709     c                   else
076200100709      *
076300100709      * ?  imposta il tipo di segmento:
076400100709     c                   clear                   dati
076500100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
076600100728     c                   eval      segmento = %trimL(dati)
076700100728     c                   eval      tipo_seg = %subst(segmento:1:3)
076800100709      **
076900100709     c                   end
077000100709      *
077100100709     C                   ENDSR
077200100120      * ?------------------------------------------------------------------ */
077300100120      *?    Decodifica il segmento ed importa i campi in formato DS
077400100120      * ?------------------------------------------------------------------ */
077500100120     c     Decod_SegmentoBegsr
077600100120      **
077700100706      *** ------------------------------------
077800100706????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT"
077900100706      *** ------------------------------------
078000100706      * OGNI DETTAGLIO inizia dal segmento CNI e per ultimo il msg è chiuso da UNT.
078100100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
078200100706      **
078300100706      **  Scrive alla fine di ogni giro di dettaglio:
078400100706     c                   if           tipo_seg = Segm_Fine_Dettagli
078500100706     c                                   or
078600100706     c                                tipo_seg = Segm_Inizio_Dettaglio  and
078700100706     c                                Contatore_Dettagli > 0
078800100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
078900100708     c                   eval      FinDET_RECord = vnrec
079000100708      **?              /*---------------------- */
079100100708     c                   exsr      Scrive_Dettag
079200100708      **?              /*---------------------- */
079300100813      *
079400100813      *   Ogni nuovo Dettaglio,
079500100813      *    per quei Partner che gestiscono più LInee
079600100813      *    Mediante il Mittente del dettaglio
079700100813     c                   if        UNB_parziale ='S' and
079800100813     c                              PT_con_piu_Nazioni = 'S' and
079900100813     C                                 Naz_salvata = Naz_mittente
080000100813      * ?ReImposta i dati da tabella PT
080100100813      * ?  se fossero stati cambiati dal precedente dettaglio:
080200100813     C                   z-add     savXX_PT_key  Nr_PT
080300100813     c                   exsr      Se_Trovata_PT
080400100813     c                   endIF
080500100813      *
080600100706     c                   endIF
080700100708      *
080800100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
080900100708      **?              /*---------------------- */
081000100708     c                   exsr      Pulizia_Campi
081100100708      **?              /*---------------------- */
081200100813      *
081300100706      *** ------------------------------------
081400100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
081500100120      * ?  Anche se non utilizzati.
081600100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
081700100317     c                   clear                   seg_imprevisto
081800100720     c                   clear                   trovato_seg
081900100319      *
082000100319      *  Contatore delle righe ossia dei segmenti del messaggio
082100100319     c                   add       1             Conta_segmenti
082200130506     c                   add       1             total_segmenti
082300100708      *
082400100716     C                   clear                   keyUNBCLI
082500100716     C                   clear                   keyTIPOMSG
082600100716     C                   clear                   keyVERSION
082700100716     C                   clear                   keyRELEASE
082800100716     C                   clear                   keyAGENCY
082900100716     C                   clear                   keyASSOCIA
083000110530      **--------
083100110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
083200110530      **--------
083300110530     c                   movel     UNB_Mittente  keyUNBCLI
083400100720      **--------
083500100720      *  Ritorna la DS GENERICA oppure l'errore
083600100720      **--------
083700100720      *  Chiamata generica x decodificare il singolo segmento letto
083800100720     c                   call      'TRTCT00R1'
083900100720      * input
084000100720     c                   parm                    tipo_seg
084100100720     c                   parm                    segmento
084200100720     C                   parm                    keyUNBCLI
084300100720     C                   parm                    keyTIPOMSG
084400100720     C                   parm                    keyVERSION
084500100720     C                   parm                    keyRELEASE
084600100720     C                   parm                    keyAGENCY
084700100720     C                   parm                    keyASSOCIA
084800100720      *output
084900100720     c                   parm                    Errore
085000100720     c                   parm                    DsGenerica
085100100720     c                   parm                    Valori_inErr
085200100720     c                   parm                    Descr_Errore
085300100720     c                   parm                    trovato_seg
085400120424      *
085500120424      *>>>--->>>                                                    <<<---<<<
085600120424      *   Se ci sono dei Segmenti Particolari da NON considerare
085700120424     c                   IF        trovato_seg ='N' and unz_segmento <>'S'
085800120424      *
085900120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
086000120424     C                   movel(p)  'SGM'         etbSGM
086100120424     C                   movel(p)  Tipo_Seg      etbVALSGM
086200120424     c                   exsr      In_TAB_EDSTBL
086300120424      *
086400120424     C*  Quindi sono dichiarati nel file delle eccezioni
086500120424     C                   If        Quale_campo ='TIPSGM' and
086600120424     c                                                   trovata_EDSTBL = 'S'
086700120424     c                   eval      trovato_seg ='S'
086800120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
086900120424      * errori sul TIVIN.
087000120424     c                   endIf
087100120424     c                   end
087200120424      *>>>--->>>                                                    <<<---<<<
087300100708      *** ------------------------------------
087400100708      *   Decodifica il singolo segmento LETTO
087500100708      *** ------------------------------------
087600100120     c                   select
087700100120      *
087800110110     c                   when      trovato_seg ='N' and unz_segmento <>'S'
087900100720     c                   move      'S'           seg_imprevisto
088000100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
088100100720     c                   exsr      Error_DET
088200100720     c                   goto      end_Decod
088300100720      **--------
088400100120     c                   when      tipo_seg = 'UNA'
088500100720     c                   movel(p)  DsGenerica    EDS00UNA
088600100120      *
088700100120     c                   when      tipo_seg = 'UNB'
088800100720     c                   movel(p)  DsGenerica    EDS00UNB
088900100120     C                   EXSR      DATI_UNB
089000100120      * --------
089100100120     c                   when      tipo_seg = 'UNH'
089200100720     c                   movel(p)  DsGenerica    EDS00UNH
089300100120     C                   EXSR      DATI_UNH
089400100120      * --------
089500100120     c                   when      tipo_seg = 'BGM'
089600100720     c                   movel(p)  DsGenerica    EDS00BGM
089700100316     C                   EXSR      DATI_BGM
089800100120      **--------
089900100120     c                   when      tipo_seg = 'DTM'
090000100720     c                   movel(p)  DsGenerica    EDS00DTM
090100100120     C                   EXSR      DATI_DTM
090200100120      * --------
090300100120     c                   when      tipo_seg = 'TSR'
090400100720     c                   movel(p)  DsGenerica    EDS00TSR
090500100316     C                   EXSR      DATI_TSR
090600100120      **--------
090700100120     c                   when      tipo_seg = 'CNT'
090800100720     c                   movel(p)  DsGenerica    EDS00CNT
090900100316     C                   EXSR      DATI_CNT
091000100120      **--------
091100100120     c                   when      tipo_seg = 'TOD'
091200100720     c                   movel(p)  DsGenerica    EDS00TOD
091300100120     C                   EXSR      DATI_TOD
091400100120      **--------
091500100120     c                   when      tipo_seg = 'RFF'
091600100720     c                   movel(p)  DsGenerica    EDS00RFF
091700100120     C                   EXSR      DATI_RFF
091800100316      **--------
091900100316     c                   when      tipo_seg = 'SEL'
092000100720     c                   movel(p)  DsGenerica    EDS00SEL
092100100316     C                   EXSR      DATI_SEL
092200100316      **--------
092300100316     c                   when      tipo_seg = 'EQD'
092400100720     c                   movel(p)  DsGenerica    EDS00EQD
092500100316     C                   EXSR      DATI_EQD
092600100316      **--------
092700100316     c                   when      tipo_seg = 'EQN'
092800100720     c                   movel(p)  DsGenerica    EDS00EQN
092900100316     C                   EXSR      DATI_EQN
093000100316      **--------
093100100316     c                   when      tipo_seg = 'PIA'
093200100720     c                   movel(p)  DsGenerica    EDS00PIA
093300100316     C                   EXSR      DATI_PIA
093400100316      **--------
093500100316     c                   when      tipo_seg = 'LOC'
093600100720     c                   movel(p)  DsGenerica    EDS00LOC
093700100316     C                   EXSR      DATI_LOC
093800100316      **--------
093900100316     c                   when      tipo_seg = 'DGS'
094000100720     c                   movel(p)  DsGenerica    EDS00DGS
094100100316     C                   EXSR      DATI_DGS
094200100316      **--------
094300100120     c                   when      tipo_seg = 'TDT'
094400100720     c                   movel(p)  DsGenerica    EDS00TDT
094500100316     C                   EXSR      DATI_TDT
094600100120      **--------
094700100120     c                   when      tipo_seg = 'MOA'
094800100720     c                   movel(p)  DsGenerica    EDS00MOA
094900100120     C                   EXSR      DATI_MOA
095000100120      **--------
095100100120     c                   when      tipo_seg = 'FTX'
095200100720     c                   movel(p)  DsGenerica    EDS00FTX
095300100120     C                   EXSR      DATI_FTX
095400100120      **--------
095500100120     c                   when      tipo_seg = 'NAD'
095600100720     c                   movel(p)  DsGenerica    EDS00NAD
095700100120     C                   EXSR      DATI_NAD
095800100316      **--------
095900100316     c                   when      tipo_seg = 'CNI'
096000100720     c                   movel(p)  DsGenerica    EDS00CNI
096100100316     C                   EXSR      DATI_CNI
096200100120      **--------
096300100120     c                   when      tipo_seg = 'CTA'
096400100720     c                   movel(p)  DsGenerica    EDS00CTA
096500100120     C                   EXSR      DATI_CTA
096600100120      **--------
096700100120     c                   when      tipo_seg = 'COM'
096800100720     c                   movel(p)  DsGenerica    EDS00COM
096900100120     C                   EXSR      DATI_COM
097000100120      **--------
097100100120     c                   when      tipo_seg = 'GID'
097200100720     c                   movel(p)  DsGenerica    EDS00GID
097300100120     C                   EXSR      DATI_GID
097400100120      **--------
097500100120     c                   when      tipo_seg = 'MEA'
097600100720     c                   movel(p)  DsGenerica    EDS00MEA
097700100120     C                   EXSR      DATI_MEA
097800100120      **--------
097900100120     c                   when      tipo_seg = 'PCI'
098000100720     c                   movel(p)  DsGenerica    EDS00PCI
098100100120     C                   EXSR      DATI_PCI
098200100120      **--------
098300100120     c                   when      tipo_seg = 'UNT'
098400100720     c                   movel(p)  DsGenerica    EDS00UNT
098500100120     C                   EXSR      DATI_UNT
098600100120      **--------
098700100120     c                   when      tipo_seg = 'UNZ'
098800100720     c                   movel(p)  DsGenerica    EDS00UNZ
098900100120     C                   EXSR      DATI_UNZ
099000110105     c                   move      'S'           unz_segmento
099100110105      **--------
099200110105     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
099300100120      **--------
099400100120     c                   OTHER
099500120424      **--------
099600120424     c                   if            unz_segmento <>'S'
099700120424     c                                           and trovata_EDSTBL <>'S'
099800100120     c                   move      'S'           seg_imprevisto
099900100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
100000100120     c                   exsr      Error_DET
100100100120     c                   goto      end_Decod
100200110110     c                   end
100300100120      **--------
100400100120     c                   endSL
100500100120      **
100600100120     c     end_Decod     endSR
100700100701      * ?================================================================== */
100800100319     C*? Azzera_MSG   - Azzera dati messaggio
100900100701      * ?================================================================== */
101000100708     C     Scrive_Dettag BEGSR
101100100701      **
101200100708      *  Deve flaggare il dettaglio con dei problemi
101300100708      * Imposta le righe in errore sullo specifico dettaglio
101400100708     C                   if        Err_in_detta = 'S'
101500100708      * prima di rieseguire il ciclo
101600100708      *  rilascia il record
101700100708     c                   update    tivin000
101800100708      * ?  Scrive su tutti i records il tipo di errore
101900100708     c     IniDET_RECord setll     tivin00r
102000100708      **?              /*---------------------- */
102100100708     c                   exsr      DETta_Errato
102200100708      **?              /*---------------------- */
102300100708     c                   end
102400100708      *
102500100708      **  Se presente un errore nel record emette una segnalazione msg
102600100708???? c                   if        se_errore <> 'S'
102700100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
102800100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
102900100708     c                              NAD_Destinatario_in_Testata    ='S'
103000100708      * ?              /*---------------------- */
103100100708     c                   exsr      Scrive_VAB
103200100708      * ?              /*---------------------- */
103300100708     c                   else
103400100708     c                   eval      errori_in_Wrt = 'S'
103500100708     c                   end
103600100708      *
103700100708      *  Problemi nella decodifica dei campi VAB/VAT
103800100708     c                   if        errori_in_Wrt = 'S'
103900100708     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
104000100708     c                   exsr      Error_DET
104100100708     c                   ROLBK
104200100708     c                   else
104300100708     c                   COMMIT
104400100708     c                   end
104500100708      *
104600100708     c                   end
104700100708      **
104800100708     c     end_WRidett   endSR
104900100708      * ?================================================================== */
105000100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
105100100708      * ?================================================================== */
105200100708     C     Pulizia_Campi BEGSR
105300100708      **
105400100708      * inzio messaggio azzera tutte le variabili generali del messaggio
105500100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
105600100708      * ?                         --------------
105700100708     C                   EXSR      Azzera_MSG
105800100708      * ?                         --------------
105900100708      *
106000100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
106100100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
106200100708      * ?                         --------------
106300100708     c                   exsr      Testata_sped
106400100708      * ?                         --------------
106500100708      *
106600100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
106700100708      *  Prepara una nuova spedizione
106800100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
106900100708      * ?                         --------------
107000100708     c                   exsr      Nuova_spediz
107100100708      * ?                         --------------
107200100708     c                   end
107300100708      **
107400100708     c     end_clear     endSR
107500100708      * ?================================================================== */
107600100708     C*? Azzera_MSG   - Azzera dati messaggio
107700100708      * ?================================================================== */
107800100708     C     Azzera_MSG    BEGSR
107900100708      **
108000100701     C*  Inizializza variabili
108100100701     C                   Z-ADD     0             skCLienti
108200100701     C                   Z-ADD     0             sk
108300100701     C*
108400100701      *  INIZIALIZZA  dati fondamentali messaggio
108500100701     C                   clear                   EDIDSPT
108600100701     C                   clear                   EDIDSPU
108700100701     C                   clear                   EDIDSPV
108800120629     C                   clear                   EDIDSPZ
108900160205     C                   clear                   EDIDSPS
109000100701     C*
109100100701     C                   move      'N'           PT_trovata
109200100813     C                   clear                   savXX_PT_key
109300160205     C                   clear                   PS_vabFGS
109400100701     C                   clear                   PT_vabLNP
109500100701     C                   clear                   PT_vabNRS
109600100701     C                   clear                   PT_vabCTM
109700100701     C                   clear                   PT_vabCCM
109800100701     C                   clear                   PU_HLD_SpedVAX
109900111027     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO     = *blank
110000100813     C                   eval        PT_con_piu_Nazioni = *blank
110100100813     c                   move      *blank        Naz_salvata       3
110200090211     C*
110300100701     c                   clear                   UNB_Generico
110400100701     c                   clear                   UNB_NrTrasmiss
110500100701     c                   clear                   UNZ_NrTrasmiss
110600100708      *
110700120629     c                   clear                   NAD_SF_Cliente
110800100630      *
110900100630      * per controllare se almeno un record è stato importato sul VAB
111000100630     c                   clear                   Almeno_Uno        1
111100100630     c                   clear                   Almeno_Un_Due     1
111200100708     c                   clear                   IniDET_RECord
111300100708     c                   clear                   FinDET_RECord
111400100630     c                   eval      esito  = '0'
111500100728     c                   eval      Segmento_in_errore = 0
111600100317     C*
111700090211     C                   ENDSR
111800100701      * ?================================================================== */
111900100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
112000100701      * ?================================================================== */
112100100701     C     Testata_sped  BEGSR
112200100701      *
112300100701      *  imposta il Flag per identificare la testata
112400100701     c                   eval      Dati_di_Testata  ='S'
112500100701     C*
112600100701      *   Contatore dei dettagli
112700100701     c                   eval      Contatore_Dettagli = 0
112800100701      **
112900100701      *   Codici identificativi della Testata del messaggio
113000100701     c                   clear                   UNH_Rifer_Msg
113100100706     C* Pulisco dati di work x foglio viaggio
113200100706     C                   clear                   BGM_nrCMR                      * WNRCMR
113300100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
113400100701     c                   clear                   BGM_Id_Testata
113500100701      **
113600100701     C*  Riferimento Foglio Viaggio o CMR
113700100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
113800100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
113900100701      **   Date generali
114000100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
114100100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
114200100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
114300100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
114400100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
114500100702     C                   Z-ADD     0             DTM_DtArrivo                   *
114600100702     C                   Z-ADD     0             DTM_OraArrivo                  *
114700100701      *
114800100701      * pulisce gli RFF+FF di testata
114900100706     C                   clear                   Tes_RFF_FF_KSC
115000100706     C                   clear                   Tes_RFF_FF_TAR
115100100706     C                   clear                   Tes_RFF_FF_LNP
115200160209     C                   clear                   Tes_RFF_FF_FGS
115300100706     C                   clear                   Tes_RFF_FF_NRS
115400100706     C                   clear                   Tes_RFF_FF_CTM
115500100706     C                   clear                   Tes_RFF_FF_BS1
115600100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
115700100701     C*
115800100701     c                   clear                   tes_VABrmo
115900100701     c                   clear                   tes_VABnmo
116000100701     c                   clear                   tes_VABcmo
116100100701     C*
116200100701     c                   clear                   tes_VABrsd
116300100701     c                   clear                   tes_VABrd2
116400100701     c                   clear                   tes_VABind
116500100701     c                   clear                   tes_VABlod
116600100701     c                   clear                   tes_VABnzd
116700100701     C*
116800100701     c                   clear                   tes_Valuta
116900100702     C*
117000100702     c                   clear                   Tot_Peso_msg     15 2
117100100702     c                   clear                   Tot_Sped_msg     15 2
117200100702     c                   clear                   Tot_Colli_msg    15 2
117300100702     c                   clear                   Tot_PNetto_msg   15 2
117400100701     C*
117500100701     ** Inizializza campo di work
117600100701     c                   eval      GID_aTotale = 'N'
117700100701     C*
117800100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
117900100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
118000100701     c                   eval      NAD_destinatario_in_testata = *blank
118100100701     c                   eval      NAD_mittente_in_testata = *blank
118200100701     c                   eval      CTA_destinatario_in_testata = *blank
118300100701     c                   eval      COM_telefono_in_testata = *blank
118400100701     C*
118500100701      *   Inizia il conteggio delle righe
118600100706     c                   z-add     0             Conta_segmenti    5 0
118700100701     C*
118800100701     C                   ENDSR
118900100701      * ?================================================================== */
119000100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
119100100701      * ?================================================================== */
119200100701     C     Nuova_spediz  BEGSR
119300100701      **
119400100701     C* Da qui inzia il dettaglio perciò lo memorizzo
119500100701      *  imposta il Flag per identificare la testata
119600100701     c                   eval      Dati_di_Testata  ='N'
119700100708      *
119800100708     c                   z-add     vnrec         IniDET_RECord
119900100813      **
120000100813      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
120100100813      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
120200100813      *
120300100813     c                   if        UNB_parziale ='S' and
120400100813     c                              PT_con_piu_Nazioni = 'S'
120500100813      * ?Cerca dalla riga di questo dettaglio in avanti
120600100813      * ?           *------------------------------*
120700100813     c                   exsr      cerca_Mittente
120800100813     c                   exsr      esito_tab_PT
120900100813      * ?           *------------------------------*
121000100813     c                   endIF
121100100813      **
121200100701      *
121300100701      *  Pulizia dei records
121400100701     C                   CLEAR                   EDiVAB00
121500100701     C                   CLEAR                   EDiVAT00
121600100701     C                   CLEAR                   EDiVAD00
121700100701      **
121800100706     C                   z-add     PT_vabLNP     VABLNP
121900100706     C                   z-add     PT_vabNRS     VABNRS
122000100701     C                   MOVEL     PT_vabCTM     VABCTM
122100100701     C                   z-add     PT_vabCCM     VABCCM
122200100701     C*
122300110707     C                   MOVEL(p)  UNH_vabRMA    VABrma
122400100701     C                   z-add     UNH_vabRMN    VABrmn
122500100701      *
122600100701      *  Dettaglio dei segnacolli Bartolini/EEX
122700100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
122800100701     c                   eval      GID_aTotale = 'N'
122900100701      *
123000100701     C                   clear                   Conta_Segn        5 0
123100100701     C                   clear                   totale_PCI        5 0
123200100701      *
123300100701     C                   MOVEA     *HIVAL        segn_BAR
123400100701     C                   MOVEA     *blank        segn_EEX
123500100701      *
123600100701     C*  Azzero codice cliente - tariffa
123700100701     C                   Z-ADD     0             ToT_Colli        16 0
123800100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
123900100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
124000100701      *
124100100701     C*  Azzero codice cliente - tariffa
124200100701     C                   clear                   Det_RFF_FF_ksc
124300100701     C                   clear                   Det_RFF_FF_tar
124400100701     C                   clear                   Det_RFF_FF_lnp
124500160209     C                   clear                   Det_RFF_FF_fgs
124600100701     C                   clear                   Det_RFF_FF_nrs
124700100701     C                   clear                   Det_RFF_FF_ctm
124800100701     C                   clear                   Det_RFF_FF_bs1
124900100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
125000110406     C                   clear                   Det_RFF_FF_tic
125100100701      *
125200100701     C                   clear                   Note_1           35
125300100701     C                   clear                   Note_2           35
125400110907     C                   clear                   Note_1T          35
125500110907     C                   clear                   Note_2T          35
125600131025     C                   clear                   Note_1sms        35
125700131025     C                   clear                   Note_2sms        35
125800100701     C*  Campi di work
125900100701     c                   clear                   VABtic_work
126000100701     c                   clear                   wri_vabtic        1
126100110623     c                   eval      inviato_tipo_incasso ='N'
126200100701      *
126300100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
126400100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
126500100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
126600100701     c                   eval      COM_telefono_in_Dettaglio = *blank
126700100702     c                   eval      CNI_Identificativo_Bolla = *blank
126800110907     c                   eval      FTX_ServizioMail_xDestinatario = *blank
126900131025     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
127000131025     c                   eval      FTX_MAIL_come_Servizio = *blank
127100131025     c                   eval      FTX_SMS_come_Servizio = *blank
127200120613     c                   eval      FTX_ai_piani = *blank
127300101027     c                   clear                   CNI_seNumerico
127400101027     C                   clear                   RFF_RifSpediz
127500100701      *
127600100701     C                   clear                   Rifer_Alfanum    35
127700100701     C                   clear                   Rifer_Cliente    35
127800100701     C                   clear                   Rifer_Numeric    35
127900100701     C                   clear                   Tipo_Rifer        3
128000100701      *
128100100701      * Un errore nel dettaglio
128200100701     C                   clear                   Err_in_detta      1
128300100701     c                   clear                   errori_in_Wrt     1
128400100701      *
128500100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
128600100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
128700140610     C*
128800140610     C*  Resetta l'informazione di spedizione di RESO
128900140610     c                   eval      era_un_export_RESO_dal_PARTNER ='N'
129000140610     c                   eval      Sped_RESO = 'N'
129100140610     c                   eval      Cod_RESO_ORM = *blank
129200140623     c                   eval      Salva_REF_ORM = *blank
129300100701     C*
129400100701     c                   endSR
129500100813     C*----------------------------------------------------------------
129600100813      *? cerca Nazione del primo Mittente nella sequenza dettagli
129700100813     C*----------------------------------------------------------------
129800100813     C     cerca_MittenteBEGSR
129900100813      *
130000100813     c                   z-add     vnrec         rec_posiz         9 0
130100100813     c                   move      *blank        Naz_mittente      3
130200100813     c                   movel(p)  UNB_Generico  PT_Parziale      35
130300100813     c                   move      'N'           OK_Nazione        1
130400100813      *
130500100813      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
130600100813     c                   call      'TRTCT93R1'
130700100813     c                   parm                    rec_posiz
130800100813     c                   parm                    PT_Parziale
130900100813     c                   parm                    Naz_mittente
131000100813     c                   parm                    Ok_Nazione
131100100813     c                   parm                    User_msg
131200100813     C                   parm                    keyUNBCLI
131300100813     C                   parm                    keyTIPOMSG
131400100813     C                   parm                    keyVERSION
131500100813     C                   parm                    keyRELEASE
131600100813     C                   parm                    keyAGENCY
131700100813     C                   parm                    keyASSOCIA
131800100813      *
131900100813      *  Re-inizializza l'indicatore di trovato
132000100813     c                   setoff                                       32
132100100813      *
132200100813      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
132300100813      *   da decodificare come UNB.
132400100813     c                   if        Ok_Nazione = 'S'
132500100813     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
132600100813     C                   eval      Nr_PT = 1
132700100813     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
132800100813     c                   Endif
132900100813      *
133000100813     c                   endSR
133100100813     C*----------------------------------------------------------------
133200100813      *? esito ricerca tabella PT decodificata oppure NO
133300100813     C*----------------------------------------------------------------
133400100813     C     esito_tab_PT  BEGSR
133500100813      *
133600100813     C                   move      'N'           PT_trovata
133700100813     C   32              move      'S'           PT_trovata
133800100813      *
133900100813      * ******  Errore generale sul Messaggio
134000100813      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
134100100813      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
134200100813     C                   If        PT_trovata = 'N'
134300100813      * Msg di allerta Traduzione
134400100813     C                   EVAL      Indirizzi = CED_Bart
134500100813???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
134600100813     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
134700100813     C                             IFCSUM D96A import in filiale - messaggio -
134800100813     C                             con UNB non presente in tabella PT controlla-
134900100813     C                             re EDI ricevuto su UPLOAD.'
135000100813      *
135100100813     c                   exsr      invio_mail
135200100813     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
135300100813     C                   eval      vinFLG = '2'
135400110208     c                   eval      Almeno_Un_Due ='S'
135500100813     C                   eval      se_errore   = 'S'
135600100813      *
135700100813      *             *------------------------------*
135800100813     C                   eLSe
135900100813      *             *------------------------------*
136000100813      *
136100100813     c                   exsr      Se_Trovata_PT
136200100813      *
136300100813     c                   Endif
136400100813      *
136500100813     c                   endSR
136600100813     C*----------------------------------------------------------------
136700100813      *? DECODIFICA DATI UNB
136800100813     C*----------------------------------------------------------------
136900100813     C     Se_Trovata_PT BEGSR
137000100813      *
137100100813      * Se tutto OK su tab.PT:
137200100813      *
137300100813     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
137400100813     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
137500100813     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
137600120629     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
137700160205     C                   MOVEL     PS_uni(Nr_PT) EDIDSPS
137800100813      *
137900100813      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
138000100813      *  ricevuti:
138100100813     c                   movel     §PVema        mail_ind         44
138200100813      *
138300100813      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
138400100813     c                   if        mail_ind <> *blank
138500100813     c                   exsr      imp_ADDR_mail
138600100813     c                   end
138700100813      *
138800100813      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
138900100813      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
139000100813      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
139100100813     C                   if        §PULUN = *blank
139200100813     C                   eval      §PULUN = '00'
139300100813     c                   end
139400100813      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
139500100813     C                   move      §puLUN        PU_lunVAT
139600100813     C                   move      §puTS         PU_TipoServ
139700100813     C                   move      §puSS         PU_SpecServ
139800111027     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO = §puRES
139900120629     C                   eval       PZ_abilita_NAD_SF_come_RFF_FF = §pzNADsf
140000100813      *
140100100813     C                   Z-ADD     §PTKSC        PT_vabCCM
140200100813     C                   z-add     §PTLNP        PT_vabLNP
140300100813     C                   z-add     §PTNRS        PT_vabNRS
140400100813     C                   MOVEL     §PTCTM        PT_vabCTM
140500160205      *
140600160209     C                   move      §PTLNP        PS_vabFGS
140700160205     c                   if         §PSfgs <> *blank and
140800160205     c                              §PSfgs <> *zeros
140900160209     C                   move      §PSfgs        PS_vabFGS
141000160205     c                   end
141100100813      *
141200100813      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
141300100813      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
141400100813      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
141500100813      *  i records trasmessi).
141600100813     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
141700100813      *
141800100813      *  In base al cod.trattamento merce reperisco tipo tratt.
141900100813     C                   CLEAR                   DS1B
142000100813     C                   Z-ADD     1             Y                 3 0
142100100813     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
142200100813     C     *IN32         IFEQ      '1'
142300100813     C                   MOVEL     Des_Tb_CTM(Y) DS1B
142400100813     C                   MOVEL     DS1B          sav_DS1B
142500100813     C                   END
142600100813      *
142700100813      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
142800100813     C                   clear                   ACORAG
142900100813     C                   clear                   INDCAE
143000100813     C                   clear                   INDCAP
143100100813     C                   clear                   INDSTA
143200100813      * Decodifico partner
143300100813     C                   Z-ADD     1             KKUT
143400100813     C                   Z-ADD     KCI           KKCC
143500100813     C                   MOVE      §PTKSC        KKSC
143600100813     C     KACO          CHAIN     CNACO00F                           31
143700100813     C     KIND          CHAIN     CNIND00F                           31
143800100813      *
143900100813      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
144000100813      *   questa schiera è utilizzata anche per i clienti.
144100100813     C     §ptKSC        LOOKUP    skCLienti                              39
144200100813     c                   If        *in39 = *off
144300100813     C                   add       1             sk
144400100813     C                   z-add     §ptKSC        skCLienti(sk)
144500100813     C                   Endif
144600100813      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
144700100813      *
144800100813     c                   endSR
144900090210     C*----------------------------------------------------------------
145000090210      *? DECODIFICA DATI UNB
145100090210     C*----------------------------------------------------------------
145200090210     C     DATI_UNB      BEGSR
145300090210      *
145400100701      * identificativo Messaggio
145500100701     C                   eval      UNB_NrTrasmiss = unb0020
145600100701      *
145700100813     C                   MOVEL     unb0017       Anno2             2 0
145800100813     C                   z-add     2000          Anno4             4 0
145900100813     C                   add       Anno2         Anno4
146000100813     C                   MOVE      unb0017       WGG               2 0
146100100813     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
146200100813     C                   MOVE      Anno2         Data_messag
146300100813     C                   MOVEL     WGG           Data_messag
146400100813     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
146500100813     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
146600100813     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
146700100813     c****
146800100813     c                   clear                   Mittente         35
146900100813     c                   clear                   Mitt_Qualifier    4
147000100813     c                   clear                   Id_Messaggio     14
147100100813     C                   eval      Mittente       = unb0004
147200100813     C                   eval      Mitt_qualifier = unb000720
147300100813     C                   eval      Id_Messaggio   = UNB0022
147400100813      *
147500090209      ** ---------------------------------------------------------------
147600090209      * quindi 1° cosa:
147700090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
147800090209      *     senza questo il messaggio NON è trascodificabile
147900100630     C                   MOVEL     unb0004       UNB_diWork
148000100630     C                   MOVE      unb000720     UNB_diWork
148100100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
148200100630     C                   Z-ADD     1             Nr_PT
148300100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
148400090209      *
148500100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
148600100317      *  la Nazione:
148700100317     C                   If        *IN32 = *off
148800100630     C                   eval      Nr_PT = 1
148900100630     c                   clear                   UNB_diWork
149000100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
149100100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
149200100317      *
149300100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
149400100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
149500100317      *  per cercare di individuare con certezza il codice della tabella PT in base
149600100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
149700100317     C                   If        *IN32 = *on
149800100317      *
149900100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
150000100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
150100100317      * ?  su Partner che hanno + linee con + nazioni.
150200100813      *  se è considerato Parziale
150300100813      *   Salva il codice generico
150400100630     c                   eval      UNB_parziale = 'S'
150500100813     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
150600100813      *             *------------------------------*
150700100813     c                   exsr      cerca_Mittente
150800100813      *             *------------------------------*
150900100813     C                   eval          Naz_salvata = Naz_mittente
151000100813     c                   Endif
151100100813     c                   Endif
151200100813      *
151300100813      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
151400100813      *             *------------------------------*
151500100813     c                   exsr      esito_tab_PT
151600100813      *             *------------------------------*
151700100813      **
151800100813      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
151900100813     C                   If        PT_trovata = 'S'
152000100813      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
152100100813      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
152200100813      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
152300100813      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
152400100813      * ?- Salva l'indice delle schiere per riaggancarla in seguito        ------ */
152500100813     C                   z-add     Nr_PT         savXX_PT_key
152600100813      * ?- Se ha più LINEE da gestire                                      ------ */
152700100813     C                   eval      PT_con_piu_Nazioni = §puPLN
152800100813     c                   end
152900100813      **
153000090209      **
153100090210     c     end_UNB       endSR
153200090227      * ?================================================================== */
153300090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
153400090227      * ?================================================================== */
153500090227     C     DATI_UNH      BEGSR
153600100316      *
153700100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
153800100701      *
153900090211      ** Riferimento del cliente
154000100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
154100100701      * ?                                       ------------
154200110707     C                   MOVEL(p)  UNH_Rifer_Msg UNH_VABrma
154300100701      * ?                                       ------------
154400100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
154500100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
154600100701     C                   SUB       1             lunghezza_num
154700100701     C                   EXSR      Se_Numerico
154800100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
154900090211      * ?                                       --------
155000100701     C                   MOVEL     Numero_OK     UNH_VABrmn
155100090211      * ?                                       --------
155200090211     C                   END
155300090303      **
155400090210     c     end_UNH       endSR
155500100701      * ?================================================================== */
155600100701      *? dati iniziali   BGM
155700100701      * ?================================================================== */
155800100701     C     DATI_BGM      BEGSR
155900100701      *
156000100701      *  identificativo testata
156100100706     c                   eval       BGM_Id_Testata = BGM1004
156200100706     C                   IF           BGM_Id_Testata <> *blank
156300100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
156400100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
156500100706     c                   end
156600100706      *
156700100701      **
156800100701     c                   endSR
156900100701      * ?================================================================== */
157000100701     C*?  Contatori in testata o in Dettaglio
157100100701      * ?================================================================== */
157200100701     C     DATI_CNT      BEGSR
157300100701      **
157400100701     c                   IF        Dati_di_Testata  ='S'
157500100701      **
157600100701      *  TOTALE colli, peso, spedizioni:
157700100701     c                   if        cnt6069 = Totale_Peso_Lordo
157800100701     c                   z-add     cnt6066       Tot_Peso_msg     15 2
157900100701     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
158000100701     c                   z-add     cnt6066       Tot_Sped_msg     15 2
158100100701     c                   elseIf    cnt6069 = Totale_Nr_Colli
158200100701     c                   z-add     cnt6066       Tot_Colli_msg    15 2
158300100701     c                   elseIf    cnt6069 = Totale_Peso_Netto
158400100701     c                   z-add     cnt6066       Tot_PNetto_msg   15 2
158500100701     c                   end
158600100701      **
158700100701     C                   ELSE
158800100701      **
158900100701     C                   END
159000100701      **
159100100701     c     end_CNT       endSR
159200100701      * ?================================================================== */
159300100701     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
159400100701      * ?================================================================== */
159500100701     C     DATI_RFF      BEGSR
159600120424      *
159700120424      *   Cerca la decodifica del dato in TABELLA
159800120424     C                   movel(p)  Tipo_Seg      etbSGM
159900120424     C                   movel(p)  RFF1153       etbVALSGM
160000120424     c                   exsr      In_TAB_EDSTBL
160100100701      **
160200100701      * ?   se in Testata
160300100702      * ?   X_Cargo_Manifest o X_Numero_CMR
160400100702      **
160500100701     c                   IF        Dati_di_Testata  ='S'
160600100701      **
160700100701     C*  Se tipo documento CMR ('CMR')
160800100701     C                   if        RFF1153 =  X_Cargo_Manifest
160900100701     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
161000120424      *
161100100701     C*  Se tipo documento AFB (Foglio viaggio)
161200100701     C                   elseIF    RFF1153 =  X_Numero_CMR
161300100701     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
161400120424      *
161500120424     C*  Se tipo documento  particolare       x CMR/F.Viaggio
161600120424     C                   elseIF    Quale_campo ='RFFTES'
161700120424     c                              and RFF1153 <> *blank
161800120424     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
161900120424     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
162000120424      **
162100100701     C                   END
162200120629      *
162300120629      * ?Codice Cliente di riferimento in Testata
162400120629     C                   clear                   Rifer_Cliente    35
162500120629     C                   if        RFF1153 = Riferimento_Cliente_particolare
162600120629     C                              and RFF1154 <> *BLANKS
162700120629     c                               or
162800120629     C                             Quale_campo ='RFFCLI'  and
162900120629     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
163000120629      *>>>>>>>>
163100120629     C*------------------
163200120629     C                   movel     RFF1154       Rifer_Cliente
163300120629      *
163400120629      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
163500120629     c                   exsr      CL_Particolare
163600120629     C*------------------
163700120629      *>>>>>>>>
163800120629     C                   ENDIF
163900100701      **
164000100701     c                   ELSE
164100100701      *
164200100701      * ?   se in Dettaglio   Riferimento singola spedizione
164300100701      *  Trascodifico riferimento spedizione
164400120424      **  o con altro riferimento diverso dall'AGE che indica la stessa cosa
164500100701     C                   clear                   Rifer_Alfanum    35
164600100701     c                   if        RFF1153 = Riferimento_Spedizione  and
164700100701     c                                RFF1154 <> *BLANKS
164800120424     c                               or
164900120424     C                             Quale_campo ='RFFAGE'  and
165000120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
165100120424      *
165200100701     C                   movel(p)  RFF1154       Rifer_Alfanum
165300120424      *
165400100701     c                   end
165500100701      *
165600100701      ** controlla se contiene solo numeri
165700100701     C                   IF         Rifer_Alfanum <> *BLANKS
165800100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
165900100701     C                   movel     Rifer_Alfanum test_num
166000100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
166100100701     C                   sub       1             lunghezza_num
166200100701     C                   EXSR      Se_Numerico
166300100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
166400110707     C                   movel(p)  Rifer_Alfanum VABRMA
166500100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
166600100701     C                              Tipo_Rifer = *blanks
166700100701     C                   movel     Numero_OK     VABRMN
166800100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
166900111027      ****
167000100701     C                   ENDIF
167100100701     C                   ENDIF
167200100701     C                   ENDIF
167300100701      *
167400100701      * Codice Cliente di riferimento
167500110224     C                   clear                   Rifer_Cliente    35
167600100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
167700100701     C                              and RFF1154 <> *BLANKS
167800120424     c                               or
167900120424     C                             Quale_campo ='RFFCLI'  and
168000120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
168100110224      *>>>>>>>>
168200120629     C*------------------
168300100701     C                   movel     RFF1154       Rifer_Cliente
168400100701      *
168500100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
168600120629     c                   exsr      CL_Particolare
168700120629     C*------------------
168800120629      *>>>>>>>>
168900100702     C                   ENDIF
169000100702     C*------------------
169100100702      * ?Riferimento Numerico
169200100702     C*------------------
169300110224     C                   clear                   Rifer_Numeric    35
169400100701     C                   if        RFF1153 = Riferimento_Numerico
169500100701     C                              and RFF1154 <> *blanks
169600120424     c                               or
169700120424     C                             Quale_campo ='RFFNUM'  and
169800120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
169900120424      *
170000100701     C                   movel     RFF1154       Rifer_Numeric
170100100701     C                   END
170200100701      *
170300100701      * Numero Ordine di vendita
170400100701     C     Rifer_Numeric IFNE      *BLANKS
170500100701     C                   movel     Rifer_Numeric test_num
170600100701     C     ' '           scan      Rifer_Numeric lunghezza_num
170700100701     C                   sub       1             lunghezza_num
170800100701     C                   EXSR      Se_Numerico
170900100702      *
171000100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
171100100701     C                   movel     Numero_OK     VABRMN
171200100701     C                   eval       Tipo_Rifer = Riferimento_Numerico
171300100701     C                   Endif
171400100702      *
171500100701     C                   ENDIF
171600100701      *
171700100701      * NOn c'è l'identificativo bolla del cliente
171800100701     c                   if        (RFF1153 = Riferimento_Spedizione and
171900100701     c                              RFF1154 = *BLANKS)
172000100701     c                             or
172100100701     c                             (RFF1153 = Riferimento_Numerico and
172200100701     c                              RFF1154 = *BLANKS)
172300100701     C                   eval      vinMSG = 'RFF riferimento assente'
172400100701     c                   exsr      Error_DET
172500100701     c                   goto      end_RFF
172600100702     C                   EndIF
172700100701      *
172800100701     C                   ENDIF
172900100701      *
173000140610      * SE IL PARTNER NON VUOLE UTILIZZARE il riferimento NUMERICO e vuole
173100140610      * dare un codice Riferimento PARTICOLARE tabellato su EDSTBL00F x identifare
173200140610      *  un RESO o un ORM
173300140610      *         Memorizzo il codice passato sul campo di 14
173400140610      *  che contiene il nostro identificativo (ex BOLLA Export o ORM export)
173500140610      *
173600140610     C                   if        Quale_campo ='RESORM'
173700140610     c                               and RFF1154 <> *BLANKS
173800140623     C                   movel(p)  RFF1154       salva_REF_ORM
173900140610      *
174000140610      * deve essere ASSOLUTAMENTE un CODICE NUMERICO di 14
174100140919     C                   clear                   TEST_Numerico    35
174200140919     C                   eval       TEST_Numerico = %trim(RFF1154)
174300140610      *
174400140610     C                   movel     TEST_Numerico test_num
174500140610     C     ' '           scan      TEST_Numerico lunghezza_num
174600140610     C                   sub       1             lunghezza_num
174700140610     C                   EXSR      Se_Numerico
174800140610      *
174900140610      * riferimento restituito da Partner  --> lo imposta solo se numerico
175000140610     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
175100140610      * attenzione Numero_OK (lungo 15) quindi "MOVE"!
175200140610     C                   move      Numero_OK     Cod_RESO_ORM
175300140610     C                   Endif
175400140610      *
175500140610     C                   END
175600140610      *
175700100701     c     end_RFF       endSR
175800100701      * ?================================================================== */
175900100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
176000100701      * ?================================================================== */
176100100701     C     DATI_DTM      BEGSR
176200100705      *
176300100705      *   Cerca la decodifica del dato in TABELLA
176400120424     C                   movel(p)  Tipo_Seg      etbSGM
176500100705     C                   movel(p)  dtm2005       etbVALSGM
176600100705     c                   exsr      In_TAB_EDSTBL
176700100705      *
176800100701     C*
176900100701     C*  Se data documento  memorizzo numero e data x CMR
177000100705     C                   If        Quale_campo ='DATDOC'
177100101015     c                              and dtm2380 <> *blank
177200101015      **
177300100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
177400100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
177500100702     C                   EXSR      Converte_Data
177600100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
177700100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
177800100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
177900100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
178000100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
178100100701     C     WERRO         IFEQ      'S'
178200100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
178300100706     C                             dtm2005  +
178400100702     C                              ') data errata'
178500100701     c                   exsr      Error_DET
178600100701     c                   goto      end_DTM
178700100701     C                   END
178800100701     C                   END
178900100702     C*
179000100702     C* Data Partenza del mezzo
179100100705     C                   If        Quale_campo ='DATPAR'
179200101015     c                              and dtm2380 <> *blank
179300101015      **
179400100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
179500100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
179600100702     C                   EXSR      Converte_Data
179700100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
179800100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
179900100702     C     WERRO         IFEQ      'S'
180000100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
180100100706     C                             dtm2005  +
180200100702     C                              ') data errata'
180300100702     c                   exsr      Error_DET
180400100702     c                   goto      end_DTM
180500100702     C                   END
180600100702     C                   END
180700100702     C*
180800100702     C* Data Previsto Arrivo del mezzo
180900100705     C                   If        Quale_campo ='DATARR'
181000101015     c                              and dtm2380 <> *blank
181100101015      **
181200100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
181300100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
181400100702     C                   EXSR      Converte_Data
181500100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
181600100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
181700100702     C     WERRO         IFEQ      'S'
181800100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
181900100706     C                             dtm2005  +
182000100702     C                              ') data errata'
182100100702     c                   exsr      Error_DET
182200100702     c                   goto      end_DTM
182300100702     C                   END
182400100702     C                   END
182500100701      **
182600100701     C*  Se data consegna richiesta imposto già sui campi del VAB
182700101015     C                   If        Quale_campo ='VABDCR'
182800101015     c                              and dtm2380 <> *blank
182900101015      **
183000100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
183100100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
183200100702     C                   EXSR      Converte_Data
183300100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
183400100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
183500100701     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
183600100701     C     WERRO         IFEQ      'S'
183700100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
183800100706     C                             dtm2005  +
183900100702     C                              ') data errata'
184000100701     c                   exsr      Error_DET
184100100701     c                   goto      end_DTM
184200100701     C                   END
184300100702     C                   end
184400100701      **
184500100701     c     end_DTM       endSR
184600100701      * ?================================================================== */
184700100701     C*? Imposta informazioni sul dettaglio del trasporto
184800100701      * ?================================================================== */
184900100701     C     DATI_TDT      BEGSR
185000100701     C*
185100100701      **
185200100701     c     end_TDT       endSR
185300100705      * ?================================================================== */
185400100705     C*? Imposta la Località
185500100705      * ?================================================================== */
185600100705     C     DATI_LOC      BEGSR
185700100705     C*
185800100705      **
185900100705     c     end_LOC       endSR
186000100705      * ?================================================================== */
186100100705     C*? Imposta identificativi delle Merci
186200100705      * ?================================================================== */
186300100705     C     DATI_EQD      BEGSR
186400100705     C*
186500100705      **
186600100705     c     end_EQD       endSR
186700100705      * ?================================================================== */
186800100705     C*? Imposta il numero delle Merci
186900100705      * ?================================================================== */
187000100705     C     DATI_EQN      BEGSR
187100100705     C*
187200100705      **
187300100705     c     end_EQN       endSR
187400100316      * ?================================================================== */
187500100316     C*? Imposta l'inizio del Dettaglio CNI
187600100316      * ?================================================================== */
187700100316     C     DATI_CNI      BEGSR
187800100316     C*
187900100702      ** Inizio Dettaglio riferimento iniziale
188000100702     c                   If        CNI1004 <> *blank
188100100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
188200101027      *
188300101027      *   Nel Riferimento Alfa sempre
188400110707     c                   eval            VABRMA  = CNI_Identificativo_Bolla
188500101027     C                   EVAL      RFF_RifSpediz = CNI_Identificativo_Bolla
188600101027      *
188700101027      *   e nel numerico se di soli numeri e comunque se lo memorizza
188800101027     C                   eval       test_num = CNI1004
188900101027     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
189000101027     C                   SUB       1             lunghezza_num
189100101027     C                   EXSR      Se_Numerico
189200101027     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
189300101027      * ?                                       --------
189400101027     C                   MOVEL     Numero_OK     VABrmn
189500101027      * ?                                       --------
189600101027     C                   z-add     VABrmn        CNI_seNumerico
189700101027      * ?                                       --------
189800101027     C                   END
189900101027      *
190000101027      *
190100100702     c                   end
190200100319      *
190300100316      **
190400100316     c     end_CNI       endSR
190500100316      * ?================================================================== */
190600100316     C*? Imposta le merci pericolose
190700100316      * ?================================================================== */
190800100316     C     DATI_DGS      BEGSR
190900100316     C*
191000100316      **
191100100316     c     end_DGS       endSR
191200100316      * ?================================================================== */
191300100316     C*? Imposta il numero seriale
191400100316      * ?================================================================== */
191500100316     C     DATI_SEL      BEGSR
191600100316     C*
191700100316      **
191800100316     c     end_SEL       endSR
191900100316      * ?================================================================== */
192000100316     C*? Imposta informazioni aggiuntive
192100100316      * ?================================================================== */
192200100316     C     DATI_PIA      BEGSR
192300100316     C*
192400100316      **
192500100316     c     end_PIA       endSR
192600100316      * ?================================================================== */
192700100316     C*?  Tipo Servizio
192800100316      * ?================================================================== */
192900100316     C     DATI_TSR      BEGSR
193000100702      ***
193100100702     C*  Reperisco priorità spedizione:
193200100702     C                   Z-ADD     1             X
193300100702      ***
193400100702      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
193500100702      ***  mentre in tabella EDI EEX è fatto con "1"
193600100702      ***
193700100702     c                   movel(p)  TSR4219       Work_Data__35
193800100702     c                   move      PU_TipoServ   Work_Data__35
193900100705     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
194000100702     C     *IN32         IFEQ      '1'
194100100702     C                   eval       VABTSP = TipoServizio(X)
194200100705     C                   ELSE
194300100705      **  Il tipo bolla non è presente
194400100705     C                   eval      vinMSG = 'TSR non in tabella'
194500100707     c*********          exsr      Error_DET
194600100707     c*********          goto      end_TSR
194700100705     C                   END
194800100702     C*
194900100702     C*  Decodifico servizi speciali
195000100702     C     TSR7085       IFNE      *BLANKS
195100100702     C     TSR7085       ANDNE     *LOVAL
195200100705      *
195300100702     C                   Z-ADD     1             X
195400100702     c                   movel(p)  TSR7085       Work_Data__35
195500100702     c                   move      PU_SpecServ   Work_Data__35
195600100702     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
195700100705     C     *IN32         IFEQ      '1'
195800100702     C                   eval       EDIDSSS = UNI_ServSpec(X)
195900100705      *
196000100702     C     §SSNOT        IFEQ      'S'
196100100702     C                   MOVEL     §SSDES        VABNOT
196200100702     C                   END
196300100702      *
196400120214      * x Espresso     "Pre-noon"    codice "PRN"
196500100702     C     §SSTPS        IFEQ      'S'
196600100702     C                   MOVEL     'E'           VABTSP
196700100702     C                   END
196800110509      *
196900120214      * x H 10:30      "H 10:30"     codice "H10"
197000110509     C     §SSTPS        IFEQ      'H'
197100110509     C                   MOVEL     'H'           VABTSP
197200110509     C                   END
197300100705      *
197400120214      * x Appuntamento "Booking ins" codice "BKI"
197500120403     c                   IF        §SSTPS ='A' and VABTC1 = *blank
197600100702     C                   MOVEL     'A'           VABTC1
197700120403     c                   elseIF    §SSTPS ='A' and VABTC2 = *blank
197800120403     C                   MOVEL     'A'           VABTC2
197900100702     C                   END
198000120403      *
198100120214      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
198200120214     C     §SSTPS        IFEQ      'F'
198300120214     C                   MOVEL     'S'           VABFFD
198400120214     C                   END
198500100705      *
198600100705     C                   ELSE
198700100705      *
198800100705      **  Il tipo bolla non è presente
198900100705     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
199000100707     c*******            exsr      Error_DET
199100100707     c*******            goto      end_TSR
199200100705      *
199300100702     C                   END
199400100705      *
199500100702     C                   END
199600100316      **
199700100316     c     end_TSR       endSR
199800100705      * ?================================================================== */
199900100705     C*?  C.O.D. Monetary Amount
200000100705      * ?================================================================== */
200100100705     C     DATI_MOA      BEGSR
200200100705      *
200300100705      *   Cerca la decodifica del dato in TABELLA
200400120424     C                   movel(p)  Tipo_Seg      etbSGM
200500100705     C                   movel(p)  moa5025       etbVALSGM
200600100705     c                   exsr      In_TAB_EDSTBL
200700100705      *
200800100705     C* Controllo se campo importo numerico con 3 decimali
200900100705     C     moa5004       IFNE      *zeros
201000100705      *
201100100705     C                   If        Quale_campo ='VABIAS'
201200130529      *
201300130529      * il campo è diviso in 2 flag: il primo serve per l'importo da assicurare
201400130529      *    quindi se deve essere considerato è espresso sulla PT.
201500130529     c                   if        %subst(§PTxxx:1:1) = 'S'
201600100707     C     moa5004       div       100           VABIAS                         IMPORTO
201700100705     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
201800130529     c                   end
201900100705      *
202000100705     C                   elseIF    Quale_campo ='VABVMD'
202100100707     C     moa5004       div       100           VABVMD                         MERCE
202200100705     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
202300100705      *
202400100705     C                   elseIF    Quale_campo ='VABCAS'
202500100705     C     §PUCAS        IFEQ      'S'                                          C/Assegno
202600100705      *
202700100707     C     moa5004       div       100           VABCAS
202800100705     C                   MOVEL     moa6345       VABVCA
202900100705     c                   if        Tes_valuta <> *blank and VABVCA = *blank
203000100705     C                   MOVEL     Tes_valuta    VABVCA
203100100705     c                   end
203200110627      **
203300110627      **  se c'è il default ("??")
203400110627     C                   Z-ADD     1             Y
203500110627     c                   movel(p)  '??'          Work_Data__35
203600110627     c                   move      §puTC         Work_Data__35
203700110627     c                   clear                   WData_35_2        2
203800110627     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
203900110627     c                   if        not *in33
204000110627      *
204100100705      *
204200130311      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO ( ma deve leggere il segmento
204300130311      * contenente il Tipo Incasso - sempre successivo al segmento MOA )
204400100705      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
204500130311      *
204600130311      * ATTENZIONE viene impostato di Default ma, in presenza del FTX+PAI,
204700130311      *  viene reimpostato in base a quello sulla tabella "TC".
204800130311      *   Quindi viene modificato da "OM" ....a quello sul FTX+PAI successivo.
204900100705     C     §PUTPC        IFEQ      'P'
205000100705     C                   MOVEL     'OM'          VABTIC
205100100705     C                   MOVEL     'OM'          VABtic_work
205200100705     C                   ENDIF
205300110627      **
205400110627     c                   end
205500100705      *
205600100705     C                   END
205700100705      *
205800100705     C                   EndIF
205900100705     C                   END                                                    Imp.non numer.
206000100705      *
206100100705     c     end_MOA       endSR
206200100705      * ?================================================================== */
206300100705     C*?  FREE TEXT
206400100705      * ?================================================================== */
206500100705     C     DATI_FTX      BEGSR
206600100705      *
206700100705      *   Cerca la decodifica del dato in TABELLA
206800120424     C                   movel(p)  Tipo_Seg      etbSGM
206900100705     C                   movel(p)  ftx4451       etbVALSGM
207000100705     c                   exsr      In_TAB_EDSTBL
207100100705      *
207200100705      * ? solo se si tratta di note descrittive da comunicare in BOLLA
207300100705     c                   if         quale_campo = 'VABNOT'
207400100705     C                   DO        4             X
207500100705      *
207600100705     C     D05(X)        IFNE      *BLANKS
207700100705      *
207800100705     C     Note_1        IFEQ      *BLANKS
207900100705     C                   MOVEL     D05(X)        Note_1
208000100705     C                   MOVE      D05(X)        Note_2
208100100705     C                   ELSE
208200100705     C     Note_2        IFEQ      *BLANKS
208300100705     C                   MOVEL     D05(X)        Note_2
208400100705     C                   END
208500100705     C                   END
208600100705      *
208700100705     C                   ENDif
208800100705      *
208900100705     C                   ENDdo
209000100705     C                   end
209100100705     C*------>>
209200110907      * ? solo se si tratta di note x invio mail di avviso al destinatario
209300110907      *    (nuovo servizio a pagamento attivato nel 2011)
209400110907      *     dovranno essere scritti 2 records nel VAT di tipo I e J
209500110907     c                   if         quale_campo = 'VATNOT'
209600131025     c                                 or
209700131025     c                              quale_campo = 'VATEML'
209800131025      *
209900131025     c                   if        ftx4453 = 'N'
210000131025     c                   eval      FTX_MAIL_come_servizio = 'N'
210100131025     c                   end
210200110907      *
210300110907     C                   DO        4             X
210400110907      *
210500110907     C     D05(X)        IFNE      *BLANKS
210600110907      *
210700110907     C     Note_1t       IFEQ      *BLANKS
210800110907     C                   MOVEL     D05(X)        Note_1t
210900110907     C                   MOVE      D05(X)        Note_2t
211000110907     C                   ELSE
211100110907     C     Note_2t       IFEQ      *BLANKS
211200110907     C                   MOVEL     D05(X)        Note_2t
211300110907     C                   END
211400110907     C                   END
211500110907      *
211600110907     C                   ENDif
211700110907      *
211800110907     C                   ENDdo
211900110907      *
212000110907      * carica il campo da riportare poi sul VAT
212100110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
212200110907     c                             note_1T
212300110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
212400110907     c                             note_2T
212500110907     C                   end
212600131025     C*------>>
212700131025      * ? solo se si tratta di note x invio SMS di avviso al destinatario
212800131025      *    (nuovo servizio a pagamento attivato)
212900131025      *     dovrà essere scritto 1 record nel VAT di tipo S
213000131025     c                   if         quale_campo = 'VATSMS'
213100131025     c                   if        ftx4453 = 'N'
213200131025     c                   eval      FTX_SMS_come_servizio = 'N'
213300131025     c                   end
213400131025      *
213500131025     C                   DO        4             X
213600131025      *
213700131025     C     D05(X)        IFNE      *BLANKS
213800131025      *
213900131025     C     Note_1sms     IFEQ      *BLANKS
214000131025     C                   MOVEL     D05(X)        Note_1sms
214100131025     C                   MOVE      D05(X)        Note_2sms
214200131025     C                   ELSE
214300131025     C     Note_2sms     IFEQ      *BLANKS
214400131025     C                   MOVEL     D05(X)        Note_2sms
214500131025     C                   END
214600131025     C                   END
214700131025      *
214800131025     C                   ENDif
214900131025      *
215000131025     C                   ENDdo
215100131025      *
215200131025      * carica il campo da riportare poi sul VAT
215300131025     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
215400131025     c                             note_1sms
215500131025     C                   end
215600110907     C*------>>
215700100705     C*  Decodifico tipo pagamento C/Assegno
215800100705      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
215900100705      *      (Problema di pulizia in presenza di + righe di testo)
216000100705      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
216100100705      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
216200100705      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
216300100705      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
216400100705     C                   if        wri_vabtic = *blank
216500100705     C                   MOVEL     *BLANKS       VABTIC
216600100705     c                   end
216700100705      **
216800100705     c                   clear                   trovato_TPInc     1            TipoIncasso
216900100705      *
217000100705      * ?Se c'è il tipo Incasso
217100100705     c                   if         quale_campo = 'VABTIC'                      -- 01 --
217200100705      *
217300100705     C                   DO        4             X                              -- 02 --
217400100705     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
217500110623      **
217600130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
217700130311      **  per gestire Partner come AZKAR che ha codice "AAA"
217800130311     C**********         MOVEL     D05(X)        WCTC              2
217900130311     C                   MOVEL     D05(X)        WCTC             10
218000110623      **
218100110623     c                   eval      inviato_tipo_incasso ='S'
218200110623      **
218300110623      **  se non l'avesse trovato e fosse esplicitato un Default ("??")
218400110623      **   come per qualsisi codice venga inviato dal Cliente allora
218500110623      **   deve essere impostato il default (invece di fare un chiodo)
218600110623     C                   Z-ADD     1             Y
218700110623     c                   movel(p)  '??'          Work_Data__35
218800110623     c                   move      §puTC         Work_Data__35
218900110623     c                   clear                   WData_35_2        2
219000110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
219100110623     c                   if            *in33
219200110623     c                   move      'S'           trovato_TPInc
219300110623     C                   MOVEL     TipoIncasso(Y)vabtic
219400110623     c                   movel(p)  '??'          WData_35_2
219500110623     c                   else
219600110623      *
219700130311      *  Se NON HA un default generico definito con "??"
219800130311      *   cerco con il codice arrivato dal Partner/Cliente
219900100705     C                   Z-ADD     1             Y
220000100705     c                   movel(p)  Wctc          Work_Data__35
220100100705     c                   move      §puTC         Work_Data__35
220200100705     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
220300110623     c                   if            *in33
220400110623     c                   move      'S'           trovato_TPInc
220500110623     C                   MOVEL     TipoIncasso(Y)vabtic
220600130326     c                   move      §puTC         WData_35_2        2
220700110623     c                   else
220800110623      *
220900130311      *  Se NON HA TROVATO qualcosa di DEFINITO SU MISURA del Partner/Cliente
221000130311      *   tramite il SUFFISSO presente sulla PT, prova con IL CODICE semplicemente.
221100110623     C                   Z-ADD     1             Y
221200110623     c                   movel(p)  Wctc          Work_Data__35
221300110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
221400110623     c                   if            *in33
221500110623     c                   move      'S'           trovato_TPInc
221600110623     C                   MOVEL     TipoIncasso(Y)vabtic
221700130311     c                   movel     Wctc          WData_35_2        2
221800110623     c                   end
221900110623      *
222000110623     c                   end
222100110623      *
222200110623     c                   end
222300130311      *--------
222400130311      * SOLO SE PARTNER
222500130311      *   IMPOSTO 'OM'    (vecchia regola ormai in disuso se NON siano state
222600130311      *     trovate fra le tabelle dei codici ben precisi x il PARTNER,
222700130311      ***     ossia solo se NON E'STATO PASSATO un codice specifico
222800130311      ** ATTENZIONE NON DEVE ESISTERE IL BLANK in arrivo dal PARTNER ma sempre un CODICE.
222900100705     C     §PUTPC        IFEQ      'P'
223000100705     c     wctc          andne     'TO'
223100110408     c     WData_35_2    andeq     *blank
223200100705     C                   MOVEL     'OM'          VABTIC
223300100705     C                   END                                                    -- 03 --
223400130311      *--------
223500100705     C                   END                                                    -- 03 --
223600100705     C                   ENDdo                                                  -- 02 --
223700100705      *
223800100705     C                   ELSE                                                   -- 01 --
223900130311      *
224000130311      *  PASSA DA QUI se il FTX ha un qualificatore NON RICONOSCIUTO
224100130311      *   e se  aveva impostato blank (nullo) precedentemente....
224200130311      *****
224300100705      * ?Se NON c'è il tipo Incasso Contrassegno particolare
224400100705      *    Deve essere impostato comunque da testata
224500110623     c                   if        vabtic = *blank   and
224600110623     c                             WData_35_2 = *blank
224700100705     C                   MOVEL     VABtic_work   VABTIC
224800100705     c                   end
224900100705      *
225000100705     C                   END                                                    -- 01 --
225100100705      *
225200100705      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
225300100705      *    essere presenti altri record Flat con altre informazioni
225400100705      *    che potrebbero abblancare il Tipo incasso precedentemente
225500100705      *    decodificato
225600100705     c                   if        Trovato_TPInc = 'S' and
225700100705     c                                 wri_vabtic = *blank
225800100705     c                   eval      wri_vabtic = 'S'
225900100705     c                   end
226000100705      *
226100100705      * ?Se Natura Merce      .
226200100705     c                   if         quale_campo = 'VABNAS'                      -- 01 --
226300100705     C                   MOVEL     D05(1)        VABnas
226400100705     c                   End
226500100705      *
226600120613      * ?Consegne Particolari
226700120613     c                   if         quale_campo = 'VABTCX'                      -- 01 --
226800120613      * Imposta il codice di 3 chrs.
226900120613      *   solo i primi 3 caratteri del campo
227000120613     C                   eval      controlla_COD_FTX = D05(1)
227100120613      *
227200120613      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
227300120613     C                   if        controlla_COD_FTX = 'FLR'
227400120613     c                   eval      FTX_ai_piani = 'P'
227500120614     c                   End
227600120613     c                   End
227700120613      *
227800100705     c     end_FTX       endSR
227900090209      * ?================================================================== */
228000090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
228100090209      * ?================================================================== */
228200090209     C     DATI_TOD      BEGSR
228300090209      *
228400090210      *  Trascodifico tipo trasporto
228500090210     c                   clear                   TOD_char3         3
228600100705      *  DEFAULT da PT
228700090210      *  imposta il default dalla tabella PT se c'è
228800090210     C     §puPFA        ifNE      *blank
228900090210     c                   SELECT
229000090210      *  Porto Franco
229100090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
229200090210     C                   movel     '4'           VABCBO                         + C/Ass
229300090210     c                   WHEN      §puPFA = 'F'
229400090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
229500090210      *  Porto Assegnato
229600090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
229700090210     C                   movel     '6'           VABCBO                         + C/Ass
229800090210     C                   WHEN      §puPFA = 'A'
229900090210     C                   movel     '2'           VABCBO                         Senza C/Ass
230000090210     C                   ENDSL
230100090210     c                   end
230200090210      *
230300090210      *   Imposta o il codice o il metodo
230400090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
230500090210     c                   if        tod4053  <> *blank
230600090210     c                   movel(p)  tod4053       TOD_char3
230700090210     c                   else
230800090210     c                   if        tod4215  <> *blank
230900090210     c                   movel(p)  tod4215       TOD_char3
231000090210     c                   end
231100090210     c                   end
231200110328      ****************
231300110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
231400110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
231500110328      ****************
231600110328     c                   eval      Trovata_Tab_TB ='N'
231700110328      *
231800110328     c                   if              TOD_char3 <> *blank
231900110328     c                   movel(p)  TOD_char3     Key_Generica35
232000110328     c                   move      §puTB         Key_Generica35
232100110328     C                   eval      Y = 1
232200110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
232300110328     c   32              eval      Trovata_Tab_TB ='S'
232400110328      *
232500110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
232600110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
232700110328      *
232800110328     c                   if          Trovata_Tab_TB ='N'
232900110328     c                                 and
233000110328     C                               tod4215 <> *blank
233100110328     c                   movel(p)  tod4215       TOD_char3
233200110328     c                   movel(p)  TOD_char3     Key_Generica35
233300110328     c                   move      §puTB         Key_Generica35
233400110328     C                   eval      Y = 1
233500110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
233600110328     c   32              eval      Trovata_Tab_TB ='S'
233700110328     c                   end
233800110328     c                   end
233900110328      *
234000110328      ****************
234100110328      ******* Se vi sono eccezioni sul cliente imposta quelle
234200110328      ****************
234300110328     c                   If          Trovata_Tab_TB ='S'
234400110328      *
234500110328     c                   SELECT
234600110328     c                   WHEN      Decod_Porto(Y) = 'F'
234700110328     c                               and  VABcas > *zeros
234800110328     C                   movel     '4'           VABCBO                         + C/Ass
234900110328     c                   WHEN      Decod_Porto(Y) = 'F'
235000110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
235100110328      *  Porto Assegnato
235200110328     c                   WHEN      VABCAS > *zeros
235300110328     C                   movel     '6'           VABCBO                         + C/Ass
235400110328     C                   OTHER
235500110328     C                   movel     '2'           VABCBO                         Senza C/Ass
235600110328     C                   ENDSL
235700110328      *
235800110328      ****************  altrimenti
235900110328      ******* Se non vi sono eccezioni per il cliente
236000110328      *   Rileva a questo punto lo Standard per tutti
236100110328      ****************
236200110328     c                   elseIf      Trovata_Tab_TB ='N'
236300090210      **
236400090210     c                   if              TOD_char3 <> *blank
236500100705      *   Cerca la decodifica del dato in TABELLA
236600120424     C                   movel(p)  Tipo_Seg      etbSGM
236700100705     C                   movel(p)  TOD_char3     etbVALSGM
236800100705     c                   exsr      In_TAB_EDSTBL
236900090210      *
237000100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
237100100705      *
237200090210     c                   SELECT
237300100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
237400100705     c                               VABcas > *zeros
237500090210     C                   movel     '4'           VABCBO                         + C/Ass
237600100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
237700090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
237800090210      *  Porto Assegnato
237900090210     c                   WHEN      VABCAS > *zeros
238000090210     C                   movel     '6'           VABCBO                         + C/Ass
238100090210     C                   OTHER
238200090210     C                   movel     '2'           VABCBO                         Senza C/Ass
238300090210     C                   ENDSL
238400090210      *
238500090210     c                   end
238600090210     c                   endIF
238700090209      **
238800110328     c                   ENDif
238900110328      **
239000090210      **  Il tipo bolla non è presente
239100090210     c                   if        VABCBO = *blank
239200090210     C                   eval      vinMSG = 'TOD non rilevato'
239300090213     c                   exsr      Error_DET
239400090210     c                   goto      end_TOD
239500090210     c                   end
239600090210      **
239700090210     c     end_TOD       endSR
239800090213      * ?================================================================== */
239900090213     C*?  Anagrafica del mittente e del destinatario
240000090213      * ?================================================================== */
240100091016     C     DATI_NAD      BEGSR
240200100705      *
240300100705      *   Cerca la decodifica del dato in TABELLA
240400120424     C                   movel(p)  Tipo_Seg      etbSGM
240500100705     C                   movel(p)  nad3035       etbVALSGM
240600100705     c                   exsr      In_TAB_EDSTBL
240700100708      *-------
240800120629      * Dato (SHIPPED FROM) usato principalmente x CMR
240900120629      *       ma anche usato al posto di un RFF+FF in testata messaggio
241000120629      *   quindi modificato il nome di riferimento del campo sul EDSTBL
241100120629      *   x renderlo più generico.
241200100708      *-------
241300120629     C********************        IF        Quale_campo ='VABCMR'
241400120629     C                   IF        Quale_campo ='NAD_SF'
241500120629      *-------
241600120629      * Dati di PROVENIENZA MESSAGGIO
241700120629      *-------
241800120629      ***
241900120629      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
242000100708     c                   if        nad30361 <> *blank
242100120629     C                   MOVEL     nad30361      NAD_SF_Cliente
242200100708     c                   elseIf    nad31241 <> *blank
242300120629     C                   MOVEL     nad31241      NAD_SF_Cliente
242400100708     c                   elseIf    nad3039  <> *blank
242500120629     C                   MOVEL     nad3039       NAD_SF_Cliente
242600100708     c                   end
242700120629      ***
242800120629      * ?SOLO SE NON c'è un RFF+FF: di Testata che prevale su tutto.
242900120629      * ?La PLATFORM LIST è usata come il Codice Cliente su RFF+FF:PLATFORMLIST
243000120629      ***
243100120629      * Se partner/cliente è abilitato alla funzione di sostituzione CLIENTE da UNB.
243200120629      *   Il (SHIPPED FROM) in testata funziona come un RFF+FF: a livello di testata.
243300120629      *    per agganciare i dati sulla tab."CL".
243400120629      ***
243500120629     C                   if         PZ_abilita_NAD_SF_come_RFF_FF ='S'
243600120629     c                                 and Tes_RFF_FF_ksc = 0
243700120629      * ?Ma solo se NON c'è un RFF+FF: di Testata che prevale su tutto.
243800120629      *
243900120629      *   imposta la PLATFORM LIST ricevuta del MITTENTE
244000120629      *    questo codice viene poi utilizzato sulla tab.CL (EDTAB) se concordato
244100120629      *    per codificare tutte le spedizioni come un RFF+FF:xxxxxxxxxxxxxxxxx di testata
244200120629     c                   eval      NAD_SF_Cliente = NAD3039
244300120629      *>>>>>>>>
244400120629     C*------------------
244500120629      *    Aggancia il Cliente e tutte le caratteristiche da tab.:"CL"
244600120629      * ?RFF+FF:
244700120629     C                   clear                   Rifer_Cliente    35
244800120629     C                   movel     NAD_SF_ClienteRifer_Cliente
244900120629     c                   exsr      CL_Particolare
245000120629     C*------------------
245100120629      *>>>>>>>>
245200120629     c                   end
245300120629      ***
245400100705      *-------
245500090210      * Dati destinatario
245600100705      *-------
245700100708     C                   elseIF    Quale_campo ='VABRSD'
245800090227      *
245900090227      * Se trovato un indirizzo di destinatario
246000100701     c                   if           Dati_di_Testata = 'N'
246100100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
246200100701     c                   else
246300100701     c                   eval       NAD_destinatario_in_testata = 'S'
246400100701     c                   end
246500090210      *
246600090210     C* Reperisco nazione corrispondente destinatario
246700100705     C                   Z-ADD     1             naz               3 0
246800100705     C                   MOVEL     *BLANKS       VABNZD
246900100705     C                   Z-ADD     0             WFLCPF
247000090210     c                   eval      *in32 = *off
247100100705      ***
247200090210     C                   if        nad3207 <> *BLANKS
247300111028     C     nad3207       LOOKUP    ISO(naz)                               32
247400100705     C                   If        *in32 = *on
247500100705      ***
247600100705      * per reperire il CAP corretto
247700100705     C                   MOVEL     C15(naz)      VABNZD
247800100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
247900100705      ***
248000100705     c                   Endif
248100100705     C                   endif
248200100705      *
248300090210     C* Imposto dati destinatario
248400100705     C                   MOVEL     nad30361      VABRSD
248500100705     C                   if        nad30362 <> *LOVAL
248600100705     C                   MOVEL     nad30362      VABRD2
248700090210     C                   endif
248800100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
248900091019???? C* Imposto dati destinatario
249000091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
249100100705???? C                   MOVEL     nad31241      VABRSD
249200100705???? C                   if        nad31242 <> *LOVAL
249300100705???? C                   MOVEL     nad31242      VABRD2
249400091019???? C                   endif
249500091019???? C                   end
249600100705      *   città/Località
249700090210     C                   MOVEL     nad3164       VABLOD
249800100705      *   Indirizzo
249900100705     C                   MOVEL     nad30421      VABIND
250000111121      *
250100111121     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
250200111121     c                                         %len(%trim(NAD30422))
250300111121???? c                   eval      NAD_Lunghezza_Indirizzo =
250400111121     c                                         %len(%trim(VABIND))
250500111121???? c                   eval      NAD_Lunghezza_ragSociale2 =
250600111121     c                                         %len(%trim(VABRD2))
250700111116      *
250800111121      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
250900111121      *  lo aggiunge nel proseguimento della Ragione Sociale
251000111121     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
251100111121      **
251200111121     c                             and  %len(VABIND) <
251300111121     c                                  NAD_Lunghezza_Indirizzo +
251400111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
251500111121      *
251600111121      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
251700111121      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
251800111121     c                             and  %len(VABRD2) -
251900111121     c                                  NAD_Lunghezza_ragSociale2 -
252000111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
252100111121     c                                  >= 0
252200111121      *
252300111121      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
252400111121     c                             and  NAD_Lunghezza_ragSociale2 <
252500111121     c                                  NAD_Lunghezza_Indirizzo
252600111121      *
252700111121     C     '    '        SCAN      VABRD2        WI
252800111121     c                   if        %Found
252900111121     C                   MOVEA     VABRD2        LOD
253000111121     C                   ADD       1             WI
253100111121     C                   MOVEA     NAD30422      LOD(WI)
253200111121     C                   MOVEA     LOD           VABRD2
253300111121     c                   end
253400111121      *
253500111121      * Altrimenti lo aggiunge direttamente sulla VIA
253600111121     c                   else
253700100705      * Se NAD30422  non è vuoto
253800111116      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
253900111116      *
254000111116     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
254100090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
254200111121      *
254300100705     C* Verifico se nella località c'è dello spazio
254400111116     C     '    '        SCAN      VABIND        WI
254500100705     c                   if        %Found
254600111116     C                   MOVEA     VABIND        LOD
254700090210     C                   ADD       1             WI
254800090210     C                   MOVEA     NAD30422      LOD(WI)
254900111116     C                   MOVEA     LOD           VABIND
255000100705     c                   else
255100111121      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
255200111116     C     '    '        SCAN      VABLOD        WI                4 0
255300100705     c                   if        %Found
255400111116     C                   MOVEA     VABLOD        LOD
255500100705     C                   ADD       1             WI
255600100705     C                   MOVEA     NAD30422      LOD(WI)
255700111116     C                   MOVEA     LOD           VABLOD
255800100705     C                   ENDif
255900090210     C                   ENDif
256000100705      *
256100100705     C                   ENDif
256200111121      *
256300111121     c                   end
256400111121      *
256500111116      **************
256600111116      *** PRIMA provava ad aggiungere prima alla località......
256700111116      **************
256800111116     C*  aggiungo alla LOCALITA' quanto di indirizzo ce n'è in più
256900111116     c*************      if        NAD30422 <> *blanks and NAD30422 <> *loval
257000111116     C* Verifico se nella località c'è dello spazio
257100111116     C*****'    '        SCAN      VABLOD        WI                4 0
257200111116     c*************      if        %Found
257300111116     C*************      MOVEA     VABLOD        LOD
257400111116     C*************      ADD       1             WI
257500111116     C*************      MOVEA     NAD30422      LOD(WI)
257600111116     C*************      MOVEA     LOD           VABLOD
257700111116     c*************      else
257800111116      *  Altrimenti se non c'è spazio dopo la località prova a concatenarlo alla via
257900111116     C*****'    '        SCAN      VABIND        WI
258000111116     c*************      if        %Found
258100111116     C*************      MOVEA     VABIND        LOD
258200111116     C*************      ADD       1             WI
258300111116     C*************      MOVEA     NAD30422      LOD(WI)
258400111116     C*************      MOVEA     LOD           VABIND
258500111116     C*************      ENDif
258600111116     C*************      ENDif
258700111116      *************
258800111116     C*************      ENDif
258900111116      *
259000111116      *
259100111116      *
259200111116      *
259300101027      * RIPORTO LA 2° RAG.SOCIALE NELL'INDIRIZZO.
259400101027     c                   if        vabIND = *blank and VABRD2 <> *BLANK
259500101027      *
259600101027     C                   EVAL      VABIND = VABRD2
259700101027      *
259800101027     C                   ENDif
259900090210      * Converto CAP
260000100705     C                   EXSR      Converte_CAP
260100090603     C*
260200100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
260300100705     C*  record del VAB viene PULITO
260400100316     c                   if        Dati_di_Testata  = 'S'
260500090603     c                   eval       tes_VABrsd = VABrsd
260600090603     c                   eval       tes_VABrd2 = VABrd2
260700090603     c                   eval       tes_VABind = VABind
260800090603     c                   eval       tes_VABlod = VABlod
260900090603     c                   eval       tes_VABnzd = VABnzd
261000090603     c                   end
261100100728      *
261200100728      *   se NON c'è l'identificativo bolla del cliente
261300100728      *    blocca la spedisione e lo segnala.
261400100728     C                   if          vabRSD = *blank
261500100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
261600130315     c******             exsr      Error_DET
261700130315     c******             goto      end_NAD
261800100728     C                   elseIF      vabIND = *blank
261900100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
262000101006     c*********          exsr      Error_DET
262100101006     c*********          goto      end_NAD
262200100728     C                   elseIF      vabLOD = *blank
262300100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
262400101006     c*********          exsr      Error_DET
262500101006     c*********          goto      end_NAD
262600100728     C                   ENDIF
262700100705      *-------
262800090210      * Dati mittente
262900100705      *-------
263000100705     C                   elseIF     Quale_campo ='VABRMO'
263100090210      *
263200100705     C                   movel     nad30361      VABRMO
263300090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
263400090210     C                   movel     §PTNAZ        VABNMO
263500090210      *
263600090210     C                   IF        nad3207 <> *blanks
263700100705     C                   eval      naz = 1
263800100705     C     nad3207       LOOKUP    ISO(naz)                               32
263900090210     C                   if         *in32 = *on
264000100705     C                   MOVEL     C15(naz)      VABNMO
264100090210     C                   endif
264200090210     C                   ENDIF
264300090210      *
264400090210      * Normalizzo CAP mittente originale
264500090210     C                   clear                   T
264600090210     C                   clear                   S
264700090210     C                   MOVEA     *BLANKS       CAPM
264800090210     C                   MOVEA     nad3251       CAP9
264900090210     C                   DO        9             T
265000090210     C     CAP9(T)       IFNE      *BLANKS
265100090210     C                   ADD       1             S
265200090210     C                   MOVE      CAP9(T)       CAPM(S)
265300090210     C                   ENDIF
265400090210     C                   ENDDO
265500090210     C*
265600090210      *     Controllo validità CAP
265700090210     C                   MOVEA     CAPM          VABCMO
265800090210      *
265900090210     C                   CLEAR                   TISI95DS
266000090210     C                   MOVEL     VABCMO        I95CAP
266100090210     C                   MOVEL     VABNMO        I95NAR
266200090210     C                   MOVEL     WOGGI         I95DAT
266300090210     C                   MOVEL     '3'           I95TCN
266400090210     C                   CALL      'TISI95R'
266500090210     C                   PARM                    TISI95DS
266600090210      *     Errore
266700090210     C     O95ERR        IFNE      *BLANKS
266800090210     C                   MOVEL     INDCAE        VABCMO
266900090210     C                   MOVEL     §PTNAZ        VABNMO
267000090210     C                   END
267100090603      *
267200100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
267300100705     C*  record del VAB viene PULITO
267400100316     c                   if        Dati_di_Testata  = 'S'
267500090603     c                   eval      tes_VABrmo = VABrmo
267600090603     c                   eval      tes_VABnmo = VABnmo
267700090603     c                   eval      tes_VABcmo = VABcmo
267800090603     c                   eval      NAD_mittente_in_testata = 'S'
267900100701     c                   else
268000100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
268100090603     c                   end
268200100728      *
268300100728      *   se NON c'è l'identificativo bolla del cliente
268400100728      *    blocca la spedisione e lo segnala.
268500100728     C                   if          vabRMO = *blank
268600100728     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
268700101006     c*********          exsr      Error_DET
268800101006     c*********          goto      end_NAD
268900100728     C                   ENDIF
269000090210      *
269100090210     C                   END
269200090210      *
269300090210     c     end_NAD       endSR
269400090210      * ?================================================================== */
269500090210     C*?  Contatto a cui far riferimento
269600090210      * ?================================================================== */
269700090210     C     DATI_CTA      BEGSR
269800090210      *
269900090210      *  memorizza il riferimento di consegna x il destinatario
270000090210     C     cta3412       ifne      *BLANKS
270100100701     c                   if        Dati_di_Testata  ='S'
270200100701     c                   eval       CTA_Destinatario_in_testata   = cta3412
270300100701     c                   else
270400100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
270500100701     c                   end
270600100701     c                   end
270700090210      *
270800090210     c                   endSR
270900090210      * ?================================================================== */
271000090210     C*?  Contatto telefonico  a cui far riferimento
271100090210      * ?================================================================== */
271200090210     C     DATI_COM      BEGSR
271300090210      *
271400100705      *  memorizza il riferimento di consegna nr.telefonico
271500100705     C                   if        com3148 <> *BLANKS    and
271600100705     C                              com3155 = Telephone_Number
271700100701     c                   if        Dati_di_Testata  ='S'
271800100701     c                   eval      COM_telefono_in_testata   = com3148
271900100701     c                   else
272000100701     c                   eval      COM_telefono_in_dettaglio = com3148
272100090210     c                   end
272200100705     c                   endif
272300090210      *
272400090209     C                   ENDSR
272500090210      * ?================================================================== */
272600090210     C*?  Dati di dettaglio della spedizione e dei colli
272700090210      * ?================================================================== */
272800090210     C     DATI_GID      BEGSR
272900090210      *
273000090210     C* Se ho totali per spedizione metto in campi VAB
273100090210     C     gid1496       IFEQ      99999
273200090210     C     gid1496       orEQ      9999
273300100630     c                   eval      GID_aTotale = 'S'
273400090210     C                   z-add     gid722420     VABNCL
273500090210     C                   ELSE
273600100630     c                   If        GID_aTotale = 'N'
273700100705     C                   z-add     gid722420     GID_ColliInAdd
273800100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
273900090210     C                   END
274000090210     C                   END
274100090212      *
274200100701     C                   ADD       VABNCL        ToT_Colli        16 0
274300090210      *
274400090210     c     end_GID       endSR
274500090210      * ?================================================================== */
274600090210     C*?  Dati di dettaglio misure spedizione e colli
274700090210      * ?================================================================== */
274800090210     C     DATI_MEA      BEGSR
274900090210      *
275000090210     C*  Peso
275100100319     C     mea6311       IFEQ      Weight
275200100319     C     mea6411       ANDEQ     Kilograms
275300090210      *
275400090210     C*  Controllo se ho valore totale o parziale
275500090210      *   sempre il Lordo
275600100319???? C     mea6313       IFEQ      G_Weight
275700100319???? C     mea6313       oreq      Gross_Weight
275800100706???? C     mea6313       oreq      Gross_Weight_E
275900110512???? C     mea6313       oreq      Gross_Weight_D
276000101027???? C     mea6313       oreq      *blank
276100100630     C                   IF        GID_aTotale = 'S'
276200091019???? C                   move      mea6314       mea6314_2        18 2          Peso
276300091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
276400090210     C                   ELSE
276500091019???? C                   move      mea6314       mea6314_2                      Peso
276600091019???? C                   ADD       mea6314_2     VABPKB                         Peso
276700090210     C                   END
276800091203     C                   END
276900090210      *
277000100319???? C     mea6313       IFEQ      G_Weight
277100100319???? C     mea6313       oreq      Gross_Weight
277200100706???? C     mea6313       oreq      Gross_Weight_E
277300110512???? C     mea6313       oreq      Gross_Weight_D
277400101027???? C     mea6313       oreq      *blank
277500091019???? C                   move      mea6314       mea6314_2                      Peso
277600100701???? C                   ADD       mea6314_2     ToT_PesoLordo
277700090210     C                   END
277800100706???? C     mea6313       IFEQ      N_Weight
277900100706???? C     mea6313       oreq      Net_Weight
278000100706???? C     mea6313       oreq      Gross_Weight_E
278100110512???? C     mea6313       oreq      Gross_Weight_D
278200101027???? C     mea6313       oreq      *blank
278300091019???? C                   move      mea6314       mea6314_2                      Peso
278400100701???? C                   ADD       mea6314_2     ToT_PesoNetto
278500090210     C                   END
278600090210     C                   END
278700090210      ***
278800090210     C*  Peso in Grammi  se abilitato a utilizzarlo
278900090210      ***
279000100319     C     mea6311       IFEQ      Weight
279100100319     C     mea6411       ANDEQ     Grams
279200090210      *
279300090210     C*  Controllo se ho valore totale o parziale
279400100319???? C     mea6313       IFEQ      G_Weight
279500100319???? C     mea6313       oreq      Gross_Weight
279600100706???? C     mea6313       oreq      Gross_Weight_E
279700110512???? C     mea6313       oreq      Gross_Weight_D
279800101027???? C     mea6313       oreq      *blank
279900100630     c                   If        GID_aTotale = 'S'
280000091019???? c                   move      mea6314       mea6314_2
280100091019???? C     mea6314_2     div       1000          VABPKB                         Peso
280200090210     C                   ELSE
280300091019???? c                   move      mea6314       mea6314_2
280400091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
280500091019???? C                   add       Peso_inKG     VABPKB                         Peso
280600090210     C                   END
280700090210     C                   END
280800090210      *
280900100319???? C     mea6313       IFEQ      G_Weight
281000100319???? C     mea6313       oreq      Gross_Weight
281100100706???? C     mea6313       oreq      Gross_Weight_E
281200110512???? C     mea6313       oreq      Gross_Weight_D
281300101027???? C     mea6313       oreq      *blank
281400091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
281500100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
281600090210     C                   END
281700100706???? C     mea6313       IFEQ      N_Weight
281800100706???? C     mea6313       oreq      Net_Weight
281900100706???? C     mea6313       oreq      Gross_Weight_E
282000110512???? C     mea6313       oreq      Gross_Weight_D
282100101027???? C     mea6313       oreq      *blank
282200091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
282300100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
282400090210     C                   END
282500090210     C                   END
282600090210      *  Volume
282700100319???? C     mea6311       IFEQ      Volume
282800100319???? C     mea6411       ANDEQ     Cubic_Meters
282900090210      *  Controllo se ho valore totale o parziale
283000100630     c                   If        GID_aTotale = 'S'
283100091019???? C                   move      mea6314       mea6314_2        18 2          Volume
283200091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
283300090210     C                   ELSE
283400091019???? C                   move      mea6314       mea6314_2        18 2          Volume
283500091019???? C                   ADD       mea6314_2     VABVLB                         Volume
283600090210     C                   END
283700090210     C                   END
283800100705      *
283900100705      *
284000100705     C* Ricerco CAP in base alle dimensioni
284100090210     C     WCAP          IFNE      *BLANKS
284200100319     C     mea6311       IFEQ      Weight
284300090210      *
284400090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
284500090210     C                   CLEAR                   FNLV55DS
284600090210     C                   MOVEL     'P'           D55TPT
284700090210     C                   MOVEL     WOGGI         D55DRF
284800090210     C                   MOVEL     VABLNP        D55LNP
284900090210     C                   MOVEL     VABLNP        D55LIN
285000090210     C                   CALL      'FNLV55R'
285100090210     C                   PARM                    FNLV55DS
285200090210     C                   MOVE      D55TFP        COMTFP            3 0
285300090210      *
285400090210     C*  PASSO IL TERMINAL PARTENZA
285500090210     C                   CLEAR                   TISI95DS
285600090210     C                   Z-ADD     COMTFP        I95TFP
285700090210     C                   MOVEL     VABTSP        I95TSP
285800090210     C                   MOVEL     'S'           I95FRE
285900090210     C                   MOVEL     '3'           I95TCN
286000090210     C                   MOVEL     WCAP          I95CAP
286100090210     C                   MOVEL     VABNZD        I95NAR
286200090210     C                   MOVEL     VABLOD        I95LOC
286300090210     C                   MOVEL     VABIND        I95IND
286400090210      *
286500090210     C* Se gestita seconda linea di arrivo passo peso - volume
286600090210     C* e numero colli effettivo
286700090210     C     §PULNA        IFEQ      'S'
286800090210     C                   Z-ADD     VABPKB        I95LKG
286900090210     C                   Z-ADD     VABVLB        I95LMC
287000090210     C                   ELSE
287100100706     C*******
287200090210     C* Altrmenti passo 1 Kg e 0,001
287300100706     C*******              Z-ADD1                  I95LKG
287400100706     C*******              Z-ADD0,001              I95LMC
287500100706     C*******
287600090210     C                   END
287700090210     C*
287800090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
287900090210     C                   SELECT
288000090210     C     VABCBO        WHENEQ    '1 '
288100090210     C                   MOVEL     *BLANKS       I95FCA
288200090210     C                   MOVEL     'F'           I95TPO
288300090210     C     VABCBO        WHENEQ    '2 '
288400090210     C                   MOVEL     *BLANKS       I95FCA
288500090210     C                   MOVEL     'A'           I95TPO
288600090210     C     VABCBO        WHENEQ    '4 '
288700090210     C                   MOVEL     'S'           I95FCA
288800090210     C                   MOVEL     'F'           I95TPO
288900090210     C     VABCBO        WHENEQ    '6 '
289000090210     C                   MOVEL     'S'           I95FCA
289100090210     C                   MOVEL     'A'           I95TPO
289200090210     C                   OTHER
289300090210     C                   MOVEL     'F'           I95TPO
289400090210     C     VABCAS        IFGT      0
289500090210     C                   MOVEL     'S'           I95FCA
289600090210     C                   ELSE
289700090210     C                   MOVEL     *BLANKS       I95FCA
289800090210     C                   END
289900090210     C                   ENDSL
290000090210     C*
290100090210     C                   CALL      'TISI95R'
290200090210     C                   PARM                    TISI95DS
290300090210     C* Reperisco i dati da CAP
290400090210     C                   MOVEL     O95PRV        VABPRD
290500100706     C                   MOVEL     WCAP          VABCAD
290600100114      ***
290700100706     c                   MOVEL     O95LNA        VABLNA
290800100706     C                   MOVEL     O95ZNC        VABZNC
290900090210     C*
291000090210     C     O95ERR        IFNE      *BLANKS
291100090210     C     O95FIT        ANDEQ     'S'
291200090210     C*  Cap non trovato
291300090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
291400101006     c*********          exsr      Error_DET
291500101006     C*********          goto      end_MEA
291600090210     C                   END
291700090210     C                   END
291800090210      *
291900090210     C                   ELSE
292000090210      *
292100090210     C                   CLEAR                   VABPRD
292200090210     C                   CLEAR                   VABLNA
292300090210     C                   CLEAR                   VABZNC
292400090210     C                   CLEAR                   VABCAD
292500090210     C*  Cap non trovato
292600090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
292700101006     c*********          exsr      Error_DET
292800101006     C*********          goto      end_MEA
292900090210     C                   END
293000090210      *
293100090210     c     end_MEA       endSR
293200090210      * ?================================================================== */
293300090210     C*?  Dati di dettaglio Codice a barre Segnacolli
293400090210      * ?================================================================== */
293500090210     C     DATI_PCI      BEGSR
293600090210      *
293700090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
293800090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
293900090210     C     §1BF12        IFEQ      'E'
294000100706     C                   EXSR      SEGNaCollo_EEX
294100090210     C                   ELSE
294200090210      *
294300090210      *  La prima volta controllo congruità numero segnacollo
294400090210     C     §PUBS1        IFNE      *BLANKS
294500090210     C     VABnrs        ANDNE     0
294600090210      *  Se il codice trattamento merce prevede che segnacollino i
294700090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
294800090210     C     §1BFG7        IFEQ      'S'
294900090210     C     §1BFG1        OREQ      'S'
295000090210      *  calcolo segnacollo
295100100706     C                   EXSR      Segn_Bartolini
295200090210     C                   END
295300090210     C                   END
295400090210      *
295500090210     C                   END
295600090210      *
295700090210     c     end_PCI       endSR
295800090211     C*----------------------------------------------------------------
295900090211      *? dati finali del dettaglio    UNT
296000090211     C*----------------------------------------------------------------
296100090211     C     DATI_UNT      BEGSR
296200110324      *
296300110324      * Non deve dare messaggio
296400110324     c                   goto      No_UNTctrl
296500090211      *
296600100708     c                   eval      UNT_record = vnREC
296700090213     c                   z-add     vnrec         last_RECord
296800090211      **
296900100319      **   verifica la quadratura di tutti i segmenti ricevuti
297000100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
297100100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
297200100728     C     UNT0074       IFne      Conta_segmenti
297300100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
297400100319     c                             trollare il Messaggio ricevuto'
297500100319      *
297600100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
297700100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
297800100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
297900100319     C                             Bolle import in filiale - messaggio -
298000100319     C                             con quadratura UNT errata controllare EDI -
298100100319     C                             ricevuto su UPLOAD x il cliente :'
298200100319     C                   EVAL      Messaggio = %trim(Messaggio) +
298300100319     C                                         %editc(§PTksc:'X')
298400100319     C                   EVAL      Messaggio = %trim(Messaggio) +
298500100319     C                             ' i segmenti contati sono :' +
298600100319     C                             %editc(Conta_segmenti:'Z')
298700100319     c                   if        §PVinv <> 'N'
298800100319     c                   exsr      invio_mail
298900100319     c                   end
299000100319     c                   end
299100110321     c     No_UNTctrl    tag
299200100319      **
299300100319     c     End_UNT       endSR
299400100316     C*----------------------------------------------------------------
299500100316      *? dati finali     UNT
299600100316     C*----------------------------------------------------------------
299700100316     C     DATI_UNZ      BEGSR
299800100316      *
299900100726     c                   move      'S'           Forza_Uscita
300000100316      **
300100100316     c                   endSR
300200090210      *----------------------------------------------------------------
300300090210      *? Input segnacolli su schiera di 20 elementi
300400090210      *----------------------------------------------------------------
300500090210     C     INP_Segnacol  BEGSR
300600090210      *
300700090210     c                   clear                   quanti_PCI        3 0
300800090210     c                   if        §puEL1 = 0
300900090210     c                   z-add     10            quanti_PCI
301000090210     c                   else
301100090210     c                   z-add     §puEL1        quanti_PCI
301200090210     c                   end
301300090210      *
301400090210      *  Pulisce la schiera dei segnacolli da elaborare
301500090210     c                   clear                   x30
301600090210     c                   z-add     0             Nr_x30            3 0
301700090210      *
301800090210      *   riporto tutto sulla schiera generica X30
301900090210      *
302000090210     c                   do        10            limite            3 0
302100090210      * Limita quanti elementi passati in ogni PCI
302200090210      *   devono essere considerati
302300090210     c                   if        limite > quanti_PCI
302400090210     c                   Leave
302500090210     c                   end
302600090210      *
302700090210      *  Carica la schiera generale di tutti i segnacolli
302800090210     c                   if        D30_1(limite) <> *blank
302900090210     c                   add       1             Nr_x30
303000090210     c                   eval      X30(Nr_x30) = D30_1(limite)
303100090210     c                   end
303200090210     c                   enddo
303300090210      *
303400090210     C                   ENDSR
303500090210      *----------------------------------------------------------------
303600090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
303700090210      *----------------------------------------------------------------
303800100706     C     SEGNaCollo_EEXBEGSR
303900090210      *
304000090210     c                   exsr      INP_Segnacol
304100090210      *
304200090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
304300090210     C                   DO        10            x                              --- 01 ---
304400090210      *
304500090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
304600090210     C     x30(X)        ANDNE     *LOVAL
304700090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
304800100701     C                   ADD       1             totale_PCI
304900100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
305000090210     C                   END                                                    --- 03 ---
305100090210      *
305200090210     C                   END                                                    --- 02 ---
305300090210      *
305400090210     C                   ENDSR
305500090210     C*----------------------------------------------------------------
305600090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
305700090210     C*----------------------------------------------------------------
305800100706     C     Segn_BartoliniBEGSR
305900090210      *
306000090210     c                   exsr      INP_Segnacol
306100090210     C*
306200090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
306300090210     C*  in base al numero elementi validi carico sgn.
306400090210     C                   DO        10            X                              --- 01 ---
306500090210     C*
306600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
306700090210     C     x30(X)        ANDNE     *LOVAL
306800090210     C*
306900100630     C                   CLEAR                   DS_SegnBART
307000090210     C*  Controllo se devo ricevere la linea di partenza
307100090210     C                   MOVEL     'N'           WERRO             1
307200090210     C     §PULP1        IFGT      0                                            --- 03 ---
307300100706     C                   EXSR      REPerisce_LNP
307400090210     C                   END                                                    --- 03 ---
307500100706      *
307600090210     C*  Controllo se devo ricevere la linea di arrivo
307700090210     C     §PULA1        IFGT      0                                            --- 03 ---
307800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
307900100706     C                   EXSR      REPerisce_LNA
308000090210     C                   END                                                    --- 03 ---
308100100706      *
308200090210     C*  Controllo se devo ricevere il numero di serie
308300090210     C     §PUNR1        IFGT      0                                            --- 03 ---
308400090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
308500100706     C                   EXSR      REPerisce_NRS
308600090210     C                   END                                                    --- 03 ---
308700100706      *
308800090210     C*  Controllo se devo ricevere il numero segnacollo
308900090210     C     §PUSG1        IFGT      0                                            --- 03 ---
309000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
309100100706     C                   EXSR      REPerisce_SGN
309200090210     C                   END                                                    --- 03 ---
309300100706      *
309400090210     C*  Controllo se devo ricevere la zona di consegna
309500090210     C     §PUZN1        IFGT      0                                            --- 03 ---
309600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
309700100706     C                   EXSR      REPerisce_ZNC
309800090210     C                   END                                                    --- 03 ---
309900100706      *
310000090210     C*  Imposto da VAB i dati a zero se mi è stato passato
310100090210     C*  numero segnacollo valido
310200090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
310300100706      *
310400100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
310500100630     C                   Z-ADD     VABLNP        SegnBART_LNP
310600090210     C                   END                                                    --- 04 ---
310700100706      *
310800100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
310900100630     C                   Z-ADD     VABLNA        SegnBART_LNA
311000090210     C                   END                                                    --- 04 ---
311100100706      *
311200100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
311300100630     C                   Z-ADD     VABNRS        SegnBART_NRS
311400090210     C                   END                                                    --- 04 ---
311500100706      *
311600100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
311700100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
311800090210     C                   END                                                    --- 04 ---
311900090210      *
312000090210     C*  mi reimposto LNA e zona da segnacollo
312100100114      ***
312200100114      *** dovendo accorpare le spedizioni da Conferma x CMR
312300100114      ***  non deve impostare l'instradamento.
312400100630     C                   Z-ADD     SegnBART_LNA  VABLNA
312500100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
312600100114      ***
312700090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
312800100701     C                   ADD       1             Conta_Segn
312900100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
313000090210     C                   END                                                    --- 03 ---
313100090210     C*
313200090210     C                   END                                                    --- 02 ---
313300090210     C*
313400090210     C                   END                                                    --- 01 ---
313500090210      *
313600090210     C                   ENDSR
313700090210     C*----------------------------------------------------------------
313800090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
313900090210     C*----------------------------------------------------------------
314000100706     C     REPerisce_LNP BEGSR
314100090210     C*
314200090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
314300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
314400090210     C*  dal numero segnacollo passatomi dal partner
314500090210     C                   Z-ADD     §PULP1        WP                2 0
314600090210     C     3             SUBST     x30(X):WP     WLNP              3
314700090210     C*
314800090210     C*  controllo che la lnp calcolata sia numerica
314900090210     C                   TESTN                   WLNP                 98
315000090210     C*
315100090210     C*  se è numerica la carico
315200090210     C     *IN98         IFEQ      '1'
315300100630     C                   MOVE      WLNP          SegnBART_LNP
315400090210     C                   ELSE
315500090210     C*  se non è numerica stampo errore
315600090210     C                   MOVEL     'S'           WERRO             1
315700090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
315800090213     c                   exsr      Error_DET
315900090210     C                   goto      end_LNP
316000090210     C                   END
316100090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
316200090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
316300100630     C     SegnBART_LNP  IFNE      VABLNP
316400090210     C                   MOVEL     'S'           WERRO             1
316500090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
316600090210     c                             a della Bolla'
316700090213     c                   exsr      Error_DET
316800090210     C                   goto      end_LNP
316900090210     C                   END
317000090210     C*
317100090210     C     end_LNP       ENDSR
317200090210     C*----------------------------------------------------------------
317300090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
317400090210     C*----------------------------------------------------------------
317500100706     C     REPerisce_LNA BEGSR
317600090210     C*
317700090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
317800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
317900090210     C*  dal numero segnacollo passatomi dal partner
318000090210     C                   Z-ADD     §PULA1        WP                2 0
318100090210     C     3             SUBST     x30(X):WP     WLNA              3
318200090210     C*  controllo che la lna calcolata sia numerica
318300090210     C                   TESTN                   WLNA                 98
318400090210     C*  se è numerica la carico
318500090210     C     *IN98         IFEQ      '1'
318600100630     C                   MOVE      WLNA          SegnBART_LNA
318700090210     C                   ELSE
318800090210     C*  se non è numerica stampo errore
318900090210     C                   MOVEL     'S'           WERRO             1
319000090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
319100090213     c                   exsr      Error_DET
319200090210     C                   goto      end_LNA
319300090210     C                   END
319400090210     C*
319500090210     C     end_LNA       ENDSR
319600090210     C*----------------------------------------------------------------
319700090210     C*? REPNRS - Reperisce numero serie
319800090210     C*----------------------------------------------------------------
319900100706     C     REPerisce_NRS BEGSR
320000090210     C*
320100090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
320200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
320300090210     C*  dal numero segnacollo passatomi dal partner
320400090210     C                   Z-ADD     §PUNR1        WP                2 0
320500090210     C     2             SUBST     x30(X):WP     WNRS              2
320600090210     C*  controllo che il nrs calcolata sia numerico
320700090210     C                   TESTN                   WNRS                 98
320800090210     C*  se è numerica la carico
320900090210     C     *IN98         IFEQ      '1'
321000100630     C                   MOVE      WNRS          SegnBART_NRS
321100090210     C                   ELSE
321200090210     C*  se non è numerico stampo errore
321300090210     C                   MOVEL     'S'           WERRO             1
321400090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
321500090213     c                   exsr      Error_DET
321600090210     C                   goto      end_NRS
321700090210     C                   END
321800090210      *
321900090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
322000090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
322100100630     C     SegnBART_NRS  IFNE      VABNRS
322200090210     C                   MOVEL     'S'           WERRO             1
322300090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
322400090210     c                             o della Bolla'
322500090213     c                   exsr      Error_DET
322600090210     C                   goto      end_NRS
322700090210     C                   END
322800090210     C*
322900090210     C     end_NRS       ENDSR
323000090210     C*----------------------------------------------------------------
323100090210     C*? REPZNC - Reperisce zona consegna
323200090210     C*----------------------------------------------------------------
323300100706     C     REPerisce_ZNC BEGSR
323400090210     C*
323500090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
323600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
323700090210     C*  dal numero segnacollo passatomi dal partner
323800090210     C                   Z-ADD     §PUZN1        WP                2 0
323900090210     C     2             SUBST     x30(X):WP     WZNC              2
324000090210     C*  controllo che la zona calcolata sia numerica
324100090210     C                   TESTN                   WZNC                 98
324200090210     C*  se è numerica la carico
324300090210     C     *IN98         IFEQ      '1'
324400100630     C                   MOVE      WZNC          SegnBART_ZNC
324500090210     C                   ELSE
324600090210     C*  se non è numerica stampo errore
324700090210     C                   MOVEL     'S'           WERRO             1
324800090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
324900090213     c                   exsr      Error_DET
325000090210     C                   goto      end_ZNC
325100090210     C                   END
325200090210     C*
325300090210     C     end_ZNC       ENDSR
325400090210     C*----------------------------------------------------------------
325500090210     C*? REPSGN - Reperisce numero segnacollo
325600090210     C*----------------------------------------------------------------
325700100706     C     REPerisce_SGN BEGSR
325800090210     C*
325900090210     C*  se viene passata nr.segnacollo estraggo sottostringa
326000090210     C*  lunga 7 chr (a partire dalla posizione iniziale
326100090210     C*  indicata in tabella)
326200090210     C*  dal numero segnacollo passatomi dal partner
326300090210     C                   Z-ADD     §PUSG1        WP                2 0
326400090210     C     7             SUBST     x30(X):WP     WSGN              7
326500090210     C*  controllo che il nr.segnacollo calcolato sia numerico
326600090210     C                   TESTN                   WSGN                 98
326700090210     C*  se è numerico lo carico
326800090210     C     *IN98         IFEQ      '1'
326900100630     C                   MOVE      WSGN          SegnBART_NSC
327000090210     C                   ELSE
327100090210     C*  se non è numerica stampo errore
327200090210     C                   MOVEL     'S'           WERRO             1
327300100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
327400101006     c*********          exsr      Error_DET
327500101006     C*********          goto      end_SGN
327600090210     C                   END
327700090210     C*
327800090210     C     end_SGN       ENDSR
327900051205      *----------------------------------------------------*
328000051205      *   DEFINIZIONE CHIAVI                               *
328100051205      *----------------------------------------------------*
328200051205     C     *INZSR        BEGSR
328300051205      *
328400991129     c     *ENTRY        PLIST
328500060613     C                   parm                    esito             1
328600971215      *
328700050112     C     KTABel        KLIST
328800050112     C                   KFLD                    TBLKUT
328900050112     C                   KFLD                    TBLCOD
329000050112     C                   KFLD                    TBLKEY
329100090210      *
329200090210     C     K_TAB1L       KLIST
329300090213     C                   KFLD                    etbUNB
329400090213     C                   KFLD                    etbSGM
329500090213     C                   KFLD                    etbVALSGM
329600090209     C*
329700090209     C* Definisco chiavi di accesso
329800090209     C     KTAB1         KLIST
329900090209     C                   KFLD                    KKUT
330000090209     C                   KFLD                    KCOD
330100090209     C*
330200090209     C     KNUF          KLIST
330300090209     C                   KFLD                    KAAA
330400090209     C                   KFLD                    KCNU
330500090209     C                   KFLD                    KFIL
330600090209      *
330700090209     C     KVAB1         KLIST
330800090209     C                   KFLD                    KCCM
330900090209     C                   KFLD                    KCMR
331000090209     C                   KFLD                    KCNT
331100090209     C                   KFLD                    KAAS
331200090209     C                   KFLD                    KLNP
331300090209     C                   KFLD                    KNRS
331400090209     C                   KFLD                    KNSP
331500090209      *
331600090209     C     KVAB2         KLIST
331700090209     C                   KFLD                    KCCM
331800090209     C                   KFLD                    KCMR
331900090209      *
332000090209     C     KRDE          KLIST
332100090209     C                   KFLD                    KAAS
332200090209     C                   KFLD                    KLNP1
332300090209     C                   KFLD                    KNRS
332400090209     C                   KFLD                    KNSP
332500090209     C                   KFLD                    KNMC
332600090209     C                   KFLD                    KNRP
332700090209      *
332800090209     C     KACO          KLIST
332900090209     C                   KFLD                    KKUT
333000090209     C                   KFLD                    KKCC
333100090209     C                   KFLD                    KKSC
333200090209      *
333300090209     C     KIND          KLIST
333400090209     C                   KFLD                    KKUT
333500090209     C                   KFLD                    KKCC
333600090209     C                   KFLD                    KKSC
333700111027      *
333800111027     C     KTAS          KLIST
333900111027     C                   KFLD                    TASAAS
334000111027     C                   KFLD                    TASLNP
334100111027     C                   KFLD                    TASNRS
334200111027     C                   KFLD                    TASNSP
334300090209      *
334400090209      * DETERMINO SE SONO BARTOLINI O SDI
334500090209     C                   CLEAR                   TIBS55DS
334600090209     C                   MOVEL     'L'           I50TLA
334700090209     C                   CALL      'TIBS55R'
334800090209     C                   PARM                    TIBS55DS
334900090209      *
335000090209     C* RECUPERO DATI DELL'UTENTE
335100090209     C                   Z-ADD     1             CODUT             1 0
335200090209     C                   CALL      'XPARUT'
335300090209     C                   PARM                    UTEDSE0F
335400090209     C                   MOVEL     RAGUT         RSUT             20
335500090209     C*
335600090209     C* RICERCA CAPOCONTI
335700090209     C                   DO        50            X
335800090209     C                   MOVE      TCU(X)        TCUDS
335900090209     C     F56           IFEQ      'CG'
336000090209     C     F34           ANDEQ     '01'
336100090209     C                   Z-ADD     KCU(X)        KCI               4 0
336200090209     C                   END
336300090209     C                   END
336400090209     C*
336500090211     c                   movel(p)  'EDPAB'       User_Msg         10
336600090211     C*
336700090209     C* IMPOSTO A ZERO VARIABILI DI PGM
336800090209     c                   move      *blank        Snd_Msg_alert     1
336900090209     C                   MOVEL     CA(1)         WCA              78
337000090209     C                   MOVEL     CN(1)         WCN              78
337100090209     C*
337200090209     C* RECUPERO DATA E ORA
337300100630     C                   TIME                    Ora_Data         14 0
337400100630     C                   MOVE      Ora_Data      G02DAT
337500100630     C                   MOVE      Ora_Data      WDATA8            8 0
337600100630     C                   MOVE      WDATA8        Anno2             2 0
337700090209     C                   MOVEL     WDATA8        DATA              6 0
337800100630     C                   MOVE      Anno2         DATA
337900090209     C                   MOVEL     *BLANK        G02ERR
338000090209     C                   CALL      'XSRDA8'
338100090209     C                   PARM                    WLBDA8
338200090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
338300100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
338400090209     C                   TIME                    WORA              6 0
338500090209      * Recupero data e ora
338600090209     C                   TIME                    W0140
338700090209      * UDATE IN GGMMAAAA
338800100630     C                   MOVE      W0140         DataGGMMAAA
338900090209      * UDATE IN AAAAMMGG
339000100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
339100090209     C     *iso          MOVEL     DATA_eur      dateu
339200090209      *
339300090209      * ?              /*--------------------------- */
339400090209     c                   exsr      CARICA_TABELLE
339500090209      * ?              /*--------------------------- */
339600090209      *
339700090209     C                   MOVEL     *ALL'-'       CMP132          132
339800991124      *
339900050414      *------------------
340000991124     C                   ENDSR
340100060621      * ?------------------------------------------------------------------ */
340200090209      *?    CARICA TABELLE SU SCHIERE
340300060621      * ?------------------------------------------------------------------ */
340400090209     C     CARICA_TABELLEBEGSR
340500090205      *
340600090209     C* Caricamento Tabella 1B
340700090209     C                   Z-ADD     0             X                 4 0
340800090209     C                   Z-ADD     CODUT         KKUT
340900090209     C                   MOVEL     '1B'          KCOD
341000090209     C     KTAB1         CHAIN     TABEL00F                           30
341100090209     C     *IN30         DOWEQ     '0'
341200090209     C     TBLFLG        IFEQ      *BLANKS
341300090209     C                   ADD       1             X
341400100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
341500100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
341600090209     C                   END
341700090209     C     KTAB1         READE     TABEL00F                               30
341800090209     C                   END
341900090209      *
342000090209     C* Caricamento Tabella 15
342100090209     C                   Z-ADD     0             X                 4 0
342200090209     C                   Z-ADD     CODUT         KKUT
342300090209     C                   MOVEL     '15'          KCOD
342400090209     C     KTAB1         CHAIN     TABEL00F                           30
342500090209     C     *IN30         DOWEQ     '0'
342600090209     C     TBLFLG        IFEQ      *BLANKS
342700090209     C                   ADD       1             X
342800090209     C                   MOVEL     TBLKEY        C15(X)
342900090209     C                   MOVEL     TBLUNI        DS15
343000090209     C                   MOVEL     §15COD        ISO(X)
343100090209     C                   MOVEL     §15ECF        CPF(X)
343200090209     C                   MOVE      §15PCF        CPF(X)
343300090209     C                   END
343400090209     C     KTAB1         READE     TABEL00F                               30
343500090209     C                   END
343600090209      *
343700090209     C* Caricamento Tabella CV
343800090209     C                   Z-ADD     0             X
343900090209     C                   Z-ADD     CODUT         KKUT
344000090209     C                   MOVEL     'CV'          KCOD
344100090209     C     KTAB1         CHAIN     TABEL00F                           30
344200090209     C     *IN30         DOWEQ     '0'
344300090209     C     TBLFLG        IFEQ      *BLANKS
344400090209     C                   ADD       1             X
344500090209     C                   MOVEL     TBLKEY        CCV(X)
344600090209     C                   MOVEL     TBLUNI        DCV(X)
344700090209     C                   END
344800090209     C     KTAB1         READE     TABEL00F                               30
344900090209     C                   END
345000090216      *
345100100702     C* Caricamento Tabella Priorità trasporto
345200100702     C                   Z-ADD     0             X
345300100702     C                   MOVEL     'TS'          TABCOD
345400100702     C     TABCOD        CHAIN     EDTAB01L                           30
345500100702     C     *IN30         DOWEQ     '0'
345600100702     C     TABFLG        IFEQ      *BLANKS
345700100702     C                   ADD       1             X
345800100702     C                   eval      TipoServizio(X) = TABUNI
345900100705     C                   eval      Key_TipServ(X)  = TABKEY
346000100702     C                   END
346100100702     C     TABCOD        READE     EDTAB01L                               30
346200100702     C                   END
346300100702      *
346400110328      * Caricamento Tabella termini consegna
346500110328     C                   Z-ADD     0             X
346600110328     C                   MOVEL     'TB'          TABCOD
346700110328     C     TABCOD        CHAIN     EDTAB01L                           30
346800110328     C     *IN30         DOWEQ     '0'
346900110328     C     TABFLG        IFEQ      *BLANKS
347000110328     C                   ADD       1             X
347100110328     C                   eval      K35_TpBolla(X) = TABKEY
347200110328     C                   eval      Cod_TpBolla(X) = TABKEY
347300110328     C                   eval      Decod_Porto(X) = TABUNI
347400110328     C                   END
347500110328     C     TABCOD        READE     EDTAB01L                               30
347600110328     C                   END
347700110328      *
347800090216     C* Caricamento Tabella Tipo Incasso C/Assegno
347900090216     C                   Z-ADD     0             X
348000090216     C                   MOVEL     'TC'          TABCOD
348100090216     C     TABCOD        CHAIN     EDTAB01L                           30
348200090216     C     *IN30         DOWEQ     '0'
348300090216     C     TABFLG        IFEQ      *BLANKS
348400090216     C                   ADD       1             X
348500100705     C                   eval      K35_TpIncas(X) = TABKEY
348600100705     C                   eval      TipoIncasso(X) = TABUNI
348700090216     C                   END
348800090216     C     TABCOD        READE     EDTAB01L                               30
348900090216     C                   END
349000090209      *
349100090209      * Caricamento Tabella Partner esteri
349200090209     C                   Z-ADD     0             X
349300090209     C                   MOVEL     'PT'          TABCOD
349400090209     C     TABCOD        CHAIN     EDTAB01L                           30
349500090209     C     *IN30         DOWEQ     '0'
349600090209      *
349700090209     C                   SELECT
349800090209     C     TABFLG        WHENNE    *BLANKS
349900090209      *
350000090209     C                   OTHER
350100090209     C                   ADD       1             X
350200100630     C                   MOVEL     TABKEY        PT_key(X)
350300100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
350400100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
350500100630     C                   MOVEL     TABUNI        PT_uni(X)
350600090209     C                   ENDSL
350700090209      *
350800090209     C     TABCOD        READE     EDTAB01L                               30
350900090209     C                   END
351000121105      *
351100121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
351200121105     c                   exsr      ChecK_PT
351300121105      *
351400090209      * salva quanti Codici UNB ha caricato
351500100630     c                   z-add     x             PT_KeyXsav
351600090209      *
351700090209     C                   MOVEL     'PU'          TABCOD
351800090209     C     TABCOD        CHAIN     EDTAB01L                           30
351900090209     C     *IN30         DOWEQ     '0'
352000090209      *
352100090209     C                   SELECT
352200090209     C     TABFLG        WHENNE    *BLANKS
352300090209      *
352400090209     C                   OTHER
352500100630     C                   MOVEL     TABKEY        PU_key
352600090209     C                   Z-ADD     1             X
352700100630     C     PU_key        LOOKUP    PT_key(X)                              32
352800100630     C   32              MOVEL     TABUNI        PU_uni(X)
352900090209     C                   ENDSL
353000090209      *
353100090209     C     TABCOD        READE     EDTAB01L                               30
353200090209     C                   END
353300090209      *
353400090209      * Prosegue tab.PT e PU
353500090209     C                   MOVEL     'PV'          TABCOD
353600090209     C     TABCOD        CHAIN     EDTAB01L                           30
353700090209     C     *IN30         DOWEQ     '0'
353800090209      *
353900090209     C                   SELECT
354000090209     C     TABFLG        WHENNE    *BLANKS
354100090209      *
354200090209     C                   OTHER
354300100630     C                   MOVEL     TABKEY        PV_key
354400090209     C                   Z-ADD     1             X
354500100630     C     PV_key        LOOKUP    PT_key(X)                              32
354600100630     C   32              MOVEL     TABUNI        PV_uni(X)
354700090209     C                   ENDSL
354800090209      *
354900090209     C     TABCOD        READE     EDTAB01L                               30
355000090209     C                   END
355100160205      *
355200160205      * Prosegue tab.PT e PU e PV e PS
355300160205     C                   MOVEL     'PS'          TABCOD
355400160205     C     TABCOD        CHAIN     EDTAB01L                           30
355500160205     C     *IN30         DOWEQ     '0'
355600160205      *
355700160205     C                   SELECT
355800160205     C     TABFLG        WHENNE    *BLANKS
355900160205      *
356000160205     C                   OTHER
356100160205     C                   MOVEL     TABKEY        PS_key
356200160205     C                   Z-ADD     1             X
356300160205     C     PS_key        LOOKUP    PT_key(X)                              32
356400160205     C   32              MOVEL     TABUNI        PS_uni(X)
356500160205     C                   ENDSL
356600160205      *
356700160205     C     TABCOD        READE     EDTAB01L                               30
356800160205     C                   END
356900120629      *
357000120629      * Prosegue tab.PT e PU e PV
357100120629     C                   MOVEL     'PZ'          TABCOD
357200120629     C     TABCOD        CHAIN     EDTAB01L                           30
357300120629     C     *IN30         DOWEQ     '0'
357400120629      *
357500120629     C                   SELECT
357600120629     C     TABFLG        WHENNE    *BLANKS
357700120629      *
357800120629     C                   OTHER
357900120629     C                   MOVEL     TABKEY        PZ_key
358000120629     C                   Z-ADD     1             X
358100120629     C     PZ_key        LOOKUP    PT_key(X)                              32
358200120629     C   32              MOVEL     TABUNI        PZ_uni(X)
358300120629     C                   ENDSL
358400120629      *
358500120629     C     TABCOD        READE     EDTAB01L                               30
358600120629     C                   END
358700090209      *
358800090209     C* Caricamento Tabella codici clienti - tariffe
358900090209     C                   Z-ADD     0             X
359000090209     C                   MOVEL     'CL'          TABCOD
359100090209     C     TABCOD        CHAIN     EDTAB01L                           30
359200090209     C     *IN30         DOWEQ     '0'
359300090209     C     TABFLG        IFEQ      *BLANKS
359400090209     C                   ADD       1             X
359500100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
359600100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
359700090209     C                   END
359800090209     C     TABCOD        READE     EDTAB01L                               30
359900090209     C                   END
360000090209      *
360100090209     C* Caricamento Serivizi speciali
360200090209     C                   Z-ADD     0             X
360300090209     C                   MOVEL     'SS'          TABCOD
360400090209     C     TABCOD        CHAIN     EDTAB01L                           30
360500090209     C     *IN30         DOWEQ     '0'
360600090209     C     TABFLG        IFEQ      *BLANKS
360700090209     C                   ADD       1             X
360800100702     C                   eval       Key_ServSpec(X) = TABKEY
360900100702     C                   eval       SpecialServ(X)  = TABKEY
361000100702     C                   eval       UNI_ServSpec(X) = TABUNI
361100090209     C                   END
361200090209     C     TABCOD        READE     EDTAB01L                               30
361300090209     C                   END
361400090209      *
361500090209     C                   ENDSR
361600090205      * ?------------------------------------------------------------------ */
361700090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
361800090205      * ?------------------------------------------------------------------ */
361900090205     C     *pssr         BEGSR
362000090205     C
362100060710     C                   eval      esito = '2'
362200130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 93'
362300130506      *
362400130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
362500130506     C                             IFCSUM S93A EDI ricevuto errato in UPLOAD -
362600130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
362700130506     c                             + ' verificare il '+
362800130506     c                             %editc(total_segmenti:'Z')
362900130506     c                             + ' segmento -> ' + dati
363000130506      *
363100100319     c                   if        §PVinv <> 'N'
363200100319     c                   exsr      invio_mail
363300100319     c                   end
363400060621     C
363500130506     c                   seton                                        LR
363600130506     c                   return
363700130506     C
363800130506     C                   ENDSR
363900090213      * ?------------------------------------------------------------------ */
364000090213      *?      Segnala Errore su TIVIN
364100090213      * ?------------------------------------------------------------------ */
364200090213     C     Error_DET     BEGSR
364300090213     C
364400100728     c                   eval      Segmento_in_errore = Conta_segmenti
364500090213     C                   eval      vinFLG = '2'
364600090213     c                   eval      Almeno_Un_Due ='S'
364700090213     c                   eval      esito  = '1'
364800090213     c                   eval      se_errore ='S'
364900090213     C                   eval      Err_in_detta = 'S'
365000090213     C
365100090213     C                   ENDSR
365200100705      * ?------------------------------------------------------------------ */
365300100705      *?   Controlla e decodifica Valori VALUE dei segmenti
365400100705      * ?------------------------------------------------------------------ */
365500100705     C     In_TAB_EDSTBL BEGSR
365600100705      *
365700100705      *   Cerca la decodifica del dato in TABELLA
365800100705     c                   clear                   trovata_EDSTBL    1
365900100705     c                   clear                   quale_campo       6
366000100705     c                   clear                   campo_dati_70A   70
366100100705     C                   movel     UNB_Mittente  etbUNB
366200100705     c     K_TAB1L       chain     edstbl01l
366300100705      *
366400100705      * prova quindi senza l'UNB specifico del cliente
366500100705     c                   if        not %Found(edstbl01l)
366600100705     C                   clear                   etbUNB
366700100705     c     K_TAB1L       chain     edstbl01l
366800100705     c                   end
366900100705      *
367000100705      *  Traduce i testi descrittivi se PRESENTE in tabella
367100100705      *   il campo di destinazione è definito sulla destra della descrizione
367200100705      *     ed è lungo 6 come i nomi definiti nei VAB.
367300100705     c                   if        %Found(edstbl01l)
367400100705     c                   move(p)   etbDESCRI     quale_campo
367500100705     c                   movel(p)  etbDATI       campo_dati_70A
367600100705     c                   move      'S'           trovata_EDSTBL
367700100705     c                   end
367800100705      *
367900100705      *
368000100705     C                   ENDSR
368100100705     c*==================================================================*
368200090209     C*? CNVDAT - DECODIFICA LA DATA
368300090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
368400090209     C*         - WDATA  (DATA ALFANUMERICA)
368500090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
368600090209     C*         - WHMS   (ORA NUMERICA)
368700090209     C*----------------------------------------------------------------
368800100702     C     Converte_Data BEGSR
368900090209     C*
369000090209     C                   MOVEL     ' '           WERRO             1
369100090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
369200090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
369300100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
369400090209     C                   Z-ADD     *ZEROS        WHMS              6 0
369500090209     C*
369600120905     C     Work_Format_3 IFEQ      *blank
369700120905     c                   if        %Len(%trim(Work_Data__35)) = 8
369800120905     C                   eval         Work_Format_3 = Data_senza_ora
369900120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
370000120905     C                   eval         Work_Format_3 = Data_con_ora
370100120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
370200120905     C                   eval         Work_Format_3 = Data_con_i_secondi
370300120905     c                   end
370400120905     C                   END
370500120905     C*
370600120905     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
370700100702     C                   MOVEL     Work_Data__35 WCOMO8            8
370800090209     C                   TESTN                   WCOMO8               99
370900090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
371000090209     C  N99              MOVEL     'S'           WERRO             1
371100090209     C                   END
371200090209     C*
371300100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
371400100702     C                   MOVEL     Work_Data__35 WCOMO12          12
371500100120     C                   TESTN                   WCOMO12              99
371600100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
371700100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
371800100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
371900090209     C  N99              MOVEL     'S'           WERRO             1
372000090209     C                   END
372100100120     C*
372200100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
372300100702     C                   MOVEL     Work_Data__35 WCOMO14          14
372400100120     C                   TESTN                   WCOMO14              99
372500100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
372600100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
372700100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
372800100120     C  N99              MOVEL     'S'           WERRO             1
372900100120     C                   END
373000090209     C*
373100090209     C                   ENDSR
373200090210     C*----------------------------------------------------------------
373300100701     C*? Se_Numerico       - CONTROLLO NUMERICO
373400100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
373500100701     C*         - lunghezza_num   (Lunghezza campo output)
373600100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
373700090210     C*----------------------------------------------------------------
373800100701     C     Se_Numerico   BEGSR
373900090210     C*
374000090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
374100100701     C                   MOVEL     'N'           Campo_Numerico    1
374200100701     C                   MOVEL     *BLANKS       Numero_OK        15
374300110224     C*
374400090210     C                   Z-ADD     15            IN                3 0
374500100701     C                   Z-ADD     lunghezza_num IX                3 0
374600110224     C*
374700090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
374800100701     C                   MOVEA     test_num      NDA
374900090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
375000090210     C                   MOVEA     *ALL'0'       NDN
375100100630      *
375200090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
375300100701     C                   Z-ADD     lunghezza_num I                 3 0
375400110224     C*
375500090210     C     I             DOWGT     0                                            --- 1 -->
375600110224     C*
375700090210     C* ESTRAE IL CARATTERE DA ESAMINARE
375800100701     C     1             SUBST     test_num:I    WTEM01            1
375900110224     C*
376000090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
376100090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
376200090210     C     WTEM01        SCAN      WCA                                    99
376300110224     C*
376400090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
376500090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
376600110224     C*
376700090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
376800090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
376900110224     C*
377000090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
377100090210     C* DA DESTRA VERSO SINISTRA
377200090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
377300090210     C                   MOVEL     WTEMP1        NDN(IN)
377400090210     C                   SUB       1             IN
377500090210     C                   END                                                    <-- 3 ---
377600110224     C*
377700090210     C                   END                                                    <-- 2 ---
377800090210     C                   SUB       1             I
377900110224     C*
378000090210     C                   END                                                    <-- 1 ---
378100090210     C*
378200090210     C                   MOVEA     NDN           WNDN             15
378300110224     C*
378400090210     C     WNDN          IFNE      *ZEROS
378500100701     C                   MOVEA     NDN           Numero_OK        15
378600100701     C                   MOVEL     'S'           Campo_Numerico    1
378700090210     C                   END
378800090210     C*
378900090210     C                   ENDSR
379000090210     C*----------------------------------------------------------------
379100090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
379200090210     C*----------------------------------------------------------------
379300100705     C     Converte_CAP  BEGSR
379400090210     C*
379500090210     C                   MOVEL     *BLANKS       WCAP              9
379600090210     C     nad3251       IFNE      '0'
379700090210     C     '  '          SCAN      nad3251       LENGHT            2 0
379800090210     C                   SUB       1             LENGHT
379900090210     C     LENGHT        IFLE      5
380000090210     C     LENGHT        ANDGT     0
380100090210     C                   Z-ADD     LENGHT        T                 2 0
380200090210     C                   Z-ADD     5             S                 2 0
380300090210     C                   MOVEA     nad3251       CAP9
380400090210     C                   MOVEL     *ZEROS        CAP5
380500090210     C                   DO        LENGHT
380600090210     C                   MOVE      CAP9(T)       CAP5(S)
380700090210     C                   SUB       1             S
380800090210     C                   SUB       1             T
380900090210     C                   END
381000090210     C                   MOVEA     CAP5          WCAP5             5
381100090210     C     WCAP5         IFEQ      '0'
381200090210     C                   MOVEL     *BLANKS       WCAP
381300090210     C                   ELSE
381400090210     C                   MOVEA     CAP5          WCAP
381500090210     C                   END
381600090210     C*
381700090210     C                   ELSE
381800090210     C                   MOVEL     nad3251       WCAP
381900090210     C                   END
382000090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
382100090210     C     WCAP          IFNE      *BLANKS
382200090210     C     WFLCPF        ANDNE     0
382300090210     C                   MOVEL     WFLCPF        WELE              1 0
382400090210     C                   MOVE      WFLCPF        WPOS              1 0
382500090210     C                   MOVEL     *BLANK        WCPF
382600090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
382700090210     C                   ELSE
382800090210     C                   MOVEL     *BLANKS       WCPF
382900090210     C                   END
383000090210     C                   END
383100090210     C*
383200090210     C                   ENDSR
383300090211      * ?================================================================== */
383400090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
383500090211      * ?================================================================== */
383600090211     C     Scrive_VAB    BEGSR
383700090211      *
383800090211     c                   clear                   err_bloccante     1
383900090211     c                   move      'S'           Almeno_Uno
384000090211      *
384100090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
384200090211     c                   exsr      UPDVAB
384300090211      *
384400090211     C* >>>>>>>>>>>>
384500090211     C*  Richiamo pgm per scrittura immediata VAB
384600090211     C     §PUWRK        IFEQ      ' '
384700110110      ******
384800090211     C*  Richiamo pgm per esecuzione stampa
384900110110     C**** §PTCMR        IFEQ      'S'
385000110110     c******             MOVEL     BGM_nrCMR     d86CMR
385100110110     c******             if            RFF_NrCMR <> *Blank
385200110110     C******             MOVEL     RFF_NrCMR     d86CMR
385300110110     c******             end
385400110110     C******             ELSE
385500110110     C******             MOVEL     BGM_NrFviaggiod86CMR
385600110110     c******             if          RFF_NrFviaggio <> *Blank
385700110110     C******             MOVEL     RFF_NrFviaggiod86CMR
385800110110     c******             end
385900110110     C******             END
386000110110      *
386100110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
386200110110     c                   MOVEL     vabCMR        d86CMR
386300090211      *
386400090213     C                   MOVEL     ' '           d86RIE
386500090213     C                   MOVEL     §PUDTA        d86DTA
386600090213     C                   Z-ADD     1             d86CNT
386700090213     C                   Z-ADD     §PTKSC        d86CCM
386800090213     C                   MOVEL     *BLANKS       d86STA
386900090213     C                   MOVEL     'E'           d86MOD
387000090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
387100090211      * dove poi imposta la DS dei parametri passati
387200090213     C                   MOVEL     'LR'          d86CHI
387300090213     C                   MOVEL     'N'           d86CTL
387400090211      *
387500090211     c                   move      kpjbu         SvKpjbu
387600090211     c                   clear                   kpjbu
387700090216     C                   movel     *on           Commit_on         1
387800090213     C                   MOVEL     TRTC86ds      KPJBU
387900090213      * ?- Chiamata la TRTC86R2 -*
388000090213     C                   CALL      'TRTC86R2'
388100090211     C                   PARM                    KPJBA
388200090216     C                   PARM                    Commit_on
388300090211     c                   move      Svkpjbu       Kpjbu
388400090211      *
388500090211     C*  Controllo pgm per scrittura immediata VAB
388600100630     C     sk            IFNE      0
388700100630     C                   DO        sk            sx
388800100630     C                   Z-ADD     skCLienti(sx) d86CCM
388900090211      *
389000090211     c                   move      kpjbu         SvKpjbu
389100090211     c                   clear                   kpjbu
389200090216     C                   movel     *on           Commit_on         1
389300090213     C                   MOVEL     TRTC86ds      KPJBU
389400090213      * ?- Chiamata la TRTC86R2 -*
389500090213     C                   CALL      'TRTC86R2'
389600090211     C                   PARM                    KPJBA
389700090216     C                   PARM                    Commit_on
389800090211     c                   move      Svkpjbu       Kpjbu
389900090211     C                   END
390000090211     C                   END
390100090211      *
390200090211     C                   END
390300090211      * >>>>>>>>>>>>
390400090211     C                   if        se_errore  = 'S'
390500090211     c                   move      'S'           err_bloccante
390600090211     c                   goto      Error_VAB
390700090211     c                   end
390800090211     C*
390900090211     C     Error_VAB     Endsr
391000090211     C*----------------------------------------------------------------
391100090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
391200090211     C*----------------------------------------------------------------
391300090211     C     UPDVAB        BEGSR
391400090211      *
391500090211     C*  Se non è stato impostato il codice bolla lo imposto io
391600090211     C     VABCBO        IFEQ      *BLANKS
391700090211     C     VABCAS        IFGT      0
391800090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
391900090211     C                   ELSE
392000090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
392100090211     C                   END
392200131025      * se però è stato scritto e non era considerato il Contrassegno
392300131025      *  per motivi di lettura del record MOA successiva al Tipo Bolla
392400131025      *   qui sistema il codice bolla
392500131025     C                   Else
392600131025      *  Porto Franco +COD
392700131025     c                   if        VABCBO = '1' and  VABcas > *zeros
392800131025     C                   movel     '4'           VABCBO                         + C/Ass
392900131025      *  Porto Assegnato +COD
393000131025     c                   elseIF    VABCBO = '2' and  VABcas > *zeros
393100131025     C                   movel     '6'           VABCBO                         + C/Ass
393200131025     C                   ENDif
393300131025     C                   END
393400100701      * Imposta le NOte
393500100701     C     VABNOT        IFEQ      *BLANKS
393600100701     C                   MOVEL     Note_1        VABNOT
393700100701     C                   MOVEL     Note_2        VABNT2
393800100701     C                   ELSE
393900100701     C                   MOVEL     Note_1        VABNT2
394000100701     C                   END
394100090211      *
394200090211     C*  Attribuisco anno spedizione
394300100630     C     DTM_DtParten  IFGT      0                                            anno sped.
394400100630     C                   MOVEL     DTM_DtParten  VABAAS
394500090211     C                   ELSE
394600100630     C     DTM_DtFViag   IFGT      0
394700100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
394800090211     C                   END
394900090211     C                   END
395000090211      *
395100090211      *  Controllo dettaglio segnacolli
395200090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
395300100630     C                   EXSR      CTRsegn_EEX
395400090211     C                   ELSE
395500090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
395600090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
395700100630     C                   EXSR      CTRsegn_BAR
395800090211     C                   END
395900090211     C                   END
396000090211      *
396100090212      *  Imposto linea partenza e serie
396200100701     C                   eval      VABLNP = PT_vabLNP
396300100701     C                   eval      VABNRS = PT_vabNRS
396400160209      *
396500160209      *  e la Filiale di GESTIONE
396600160209     c                   if        PS_vabFGS  <> *blank
396700160209     C                   MOVE      PS_vabFGS     VABfgs
396800160209     c                   else
396900160209     C                   z-add     VABlnp        VABfgs
397000160209     c                   end
397100090211      *
397200090211      *  Controllo se devo variare il codice trattamento merce
397300100701     C                   eval      VABCTM = PT_vabCTM
397400090211      *
397500090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
397600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
397700090211     c                   clear                   mantiene_nsp      1            *blk/'S'
397800100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
397900090211      *
398000090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
398100090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
398200090211      *
398300090211      *  Imposto codice cliente
398400090211     c                   Select
398500090211      *
398600100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
398700100319     C                   When        Det_RFF_FF_ksc  <> *zeros
398800100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
398900100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
399000090211      * forza LNP/CTM/NRS
399100100319     c                   if        Det_RFF_FF_lnp <> 0
399200100319     C                   eval      VABLNP = Det_RFF_FF_lnp
399300090211     c                   end
399400160209     c                   if        Det_RFF_FF_fgs <> *blank
399500160209     C                   move      Det_RFF_FF_fgsVABFGS
399600160209     c                   else
399700160209     c                   if        Det_RFF_FF_lnp <> 0
399800160209     C                   eval      VABfgs = Det_RFF_FF_lnp
399900160209     c                   end
400000160209     c                   end
400100140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
400200140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
400300140901     c*******            if        Det_RFF_FF_nrs <> 0
400400100319     C                   eval      VABNRS = Det_RFF_FF_nrs
400500140901     c*******            end
400600100319     c                   if        Det_RFF_FF_ctm <> *blank
400700100319     C                   eval      VABCTM = Det_RFF_FF_ctm
400800090211     c                   end
400900110408     c                   if        Det_RFF_FF_tic <> *blank and VABCAS <>0
401000110408     C                   eval      VABTIC = Det_RFF_FF_tic
401100110408     c                   end
401200110408      *
401300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
401400100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
401500100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
401600090211     c                   end
401700090211      *
401800100319      *  Specificato codice da qualificatore RFF+FF dalla testata
401900100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
402000100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
402100100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
402200090211      * forza LNP/CTM/NRS
402300100319     c                   if        Tes_RFF_FF_LNP  <> 0
402400100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
402500090211     c                   end
402600160209     c                   if        Tes_RFF_FF_fgs <> *blank
402700160209     C                   move      Tes_RFF_FF_fgsVABFGS
402800160209     c                   else
402900160209     c                   if        Tes_RFF_FF_lnp <> 0
403000160209     C                   eval      VABfgs = Tes_RFF_FF_lnp
403100160209     c                   end
403200160209     c                   end
403300140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
403400140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
403500140901     c*******            if        Tes_RFF_FF_NRS  <> 0
403600100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
403700140901     c*******            end
403800100319     c                   if        Tes_RFF_FF_CTM  <> *blank
403900100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
404000090211     c                   end
404100110408     c                   if        Tes_RFF_FF_tic  <> *blank and VABCAS <>0
404200110408     C                   eval      VABTIC = Tes_RFF_FF_tic
404300110408     c                   end
404400090211      *
404500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
404600100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
404700100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
404800090211     c                   end
404900090211      *
405000090211      *  Imposto dati da tabella - PT-
405100090211     C                   Other
405200090211      *
405300090211     C                   MOVEL     §PTKSC        VABCCM
405400090211     C                   MOVEL     §PTCTR        VABCTR
405500090211      *
405600090211     C                   Endsl
405700090211      *
405800110408      *  attenzione il tipo Contrassegno solo se c'è un valore
405900110408     c                   if        vabCAS = 0
406000110408     c                   clear                   vabTIC
406100110623     c                   else
406200110623      *
406300110623      * Se non è stato passato il Tipo incasso a fronte di particolarità sullo
406400110623      *  specifico cliente/partner
406500110707     c                   if        inviato_tipo_incasso =  'N' and
406600110707     c                             Det_RFF_FF_ksc > *zeros     and
406700110707     c                             Det_RFF_FF_tic = *blank
406800110623      *  e se c'è "??" sul tipo incasso deve Forzare questo tipo di Incasso sulla
406900110623      *  spedizione.
407000110623     c                   z-add     1             Y
407100110623     c                   movel(p)  '??'          Work_Data__35
407200110623     c                   move      §puTC         Work_Data__35
407300130311     c                   movel     Work_Data__35 WData_35_2        2
407400110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
407500110623     c                   if         *in33
407600110623     c                   eval      vabTIC = TipoIncasso(Y)
407700110623     c                   end
407800110623      **
407900110623     c                   end
408000110623      **
408100110623     c                   end
408200090211      *
408300090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
408400090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
408500090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
408600090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
408700090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
408800090211      *
408900090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
409000090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
409100090211     c                   else
409200090211      *
409300090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
409400090211     C*  lo attribuisco io
409500090211      **              **------------------**
409600090211     c                   exsr      repnum
409700090211      **              **------------------**
409800100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
409900090211      **              **------------------**
410000090211     c                   end
410100090211      *
410200090211     C*  Altrimenti attribuisco data del giorno
410300090211     C     VABMGS        IFEQ      0
410400090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
410500090211     C                   MOVE      WOGGI         VABMGS                         = oggi
410600090211     C                   END
410700090211     C*  Eseguo posizionamento su EDiVAB
410800090211     C                   Z-ADD     VABAAS        KAAS
410900090211     C                   Z-ADD     VABlnp        KLNP
411000090211     C                   Z-ADD     VABNRS        KNRS
411100090211     C                   Z-ADD     VABNSP        KNSP
411200090211     C                   MOVEL     SAVBOL        SALVA
411300090211     C     KVAB1         CHAIN     EDiVAB1L                           30
411400090211     C                   MOVEL     SALVA         SAVBOL
411500090211     C*  Imposto dati CMR
411600090211     C                   Z-ADD     1             VABCNT
411700090211     **
411800090211      *** Attenzione se si deve trattare il VAX con il CMR
411900090211      *** il campo VABCNT deve essere sempre impostato a (1)
412000090211     C     §puWRK        ifEQ      'S'
412100090211     C     §puNSP        andEQ     'S'
412200090211     C                   Z-ADD     1             VABCNT
412300090211     c                   end
412400090211     **
412500100630     C                   Z-ADD     Data_prepara  VABDTS
412600090212     C                   movel     '20'          VABDTS
412700100113     ** Forza la Data di Oggi
412800100113     C                   z-add     WOggi         VABDTS
412900100113     **
413000100630     C     Ora__prepara  mult      100           VABHMS
413100090211     C     §PTCMR        IFEQ      'S'
413200100630     C                   Z-ADD     DTM_DtCMR     VABDCM
413300100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
413400100701     c                   if          RFF_NrCMR <> *Blank
413500100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
413600100701     c                   end
413700090211     C                   ELSE
413800100630     C                   Z-ADD     DTM_DtFViag   VABDCM
413900100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
414000100701     c                   if          RFF_NrFviaggio <> *Blank
414100100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
414200100701     c                   end
414300090211     C                   END
414400100120      *
414500100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
414600100120      * accorpare la bolle ad un unico destinatario da più parti
414700100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
414800100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
414900120629     C                   if         NAD_SF_Cliente <> *blank
415000120629     c                   eval      VABCMR = NAD_SF_Cliente
415100100708     c                   else
415200100706     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z') +
415300100706     c                                      '_' +  VABRMO
415400100708     c                   end
415500100706     c                   end
415600100120      *
415700090211     C*  Se non è indicato il nome del mittente = Partner mittente
415800090211     C     VABRMO        IFEQ      *BLANKS
415900090211     C                   MOVEL     ACORAG        VABRMO
416000090211     C                   MOVEL     INDCAP        VABCMO
416100090211     C     INDCAE        IFNE      *BLANKS
416200090211     C                   MOVEL     INDCAE        VABCMO
416300090211     C                   END
416400090211     C     INDSTA        IFNE      *BLANKS
416500090211     C                   MOVEL     INDSTA        VABNMO
416600090211     C                   END
416700090211     C                   END
416800090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
416900090211     C     §1BFG1        IFEQ      'N'
417000090211     C     §1BFG7        ANDEQ     'N'
417100090211     C     §1BFG2        ANDEQ     'N'
417200090211     C***                  Z-ADD0         VABNRS
417300090211     C                   END
417400090211     C*  Imposto segancollo da - a
417500090211     C     §1BFG1        IFEQ      'S'
417600090211     C     §1BFG7        ANDEQ     'N'
417700090211     C     §1BF12        ANDNE     'E'
417800090211      *
417900100701     C     Conta_Segn    IFEQ      *ZEROS
418000090211     C                   CLEAR                   VABNCD
418100090211     C                   CLEAR                   VABNCA
418200090211     C                   ELSE
418300100630     C                   Z-ADD     segn_BAR(1)   VABNCD
418400100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
418500090211     C                   ENDIF
418600090211     C*
418700090211     C                   END
418800101027     C*
418900090211     C*  Se cap mittente originale a blank abblenco nazione
419000090211     C     VABCMO        IFEQ      *BLANKS
419100090211     C                   MOVEL     *BLANKS       VABNMO
419200090211     C                   END
419300100701     C*
419400090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
419500090603     C* Se in testata
419600100701     c                   if        NAD_destinatario_in_testata = 'S' and
419700100701     c                             NAD_destinatario_in_dettaglio <> 'S'
419800090603     c                   eval         VABrsd =  tes_VABrsd
419900090603     c                   eval         VABrd2 =  tes_VABrd2
420000090603     c                   eval         VABind =  tes_VABind
420100090603     c                   eval         VABlod =  tes_VABlod
420200090603     c                   eval         VABnzd =  tes_VABnzd
420300090603     c                   end
420400090603     C* Se in testata
420500100701     c                   if        NAD_mittente_in_testata = 'S' and
420600100701     c                             NAD_mittente_in_dettaglio <> 'S'
420700090603     c                   eval         VABrmo = tes_VABrmo
420800090603     c                   eval         VABnmo = tes_VABnmo
420900090603     c                   eval         VABcmo = tes_VABcmo
421000090603     c                   end
421100100921      *
421200101027      *   Forza il numero passato nel CNI nel riferimento Numerico
421300101027     c                   if        §PVCNIRMN = 'S'
421400101027     c                   z-add     CNI_SeNumericovabRMN
421500101027     c                   end
421600101027      *
421700100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
421800100921     C     §puWRK        ifEQ      'S'
421900100921     C     §puraggDES    andeq     'S'
422000100921     c                   clear                   vabLNA
422100100921     c                   clear                   vabZNC
422200100921     c                   clear                   vabCMR
422300100921      *  il nome identico
422400100921     c                   eval      VABCMR  = §punomeCMR
422500100921      *   e in più la data del giorno
422600100921     C     §puinGIO      ifEQ      'S'
422700100921     c                   eval      VABCMR = %trim(VABCMR)
422800100921     c                                    + %editc(WOggi_CMR:'X')
422900100921     c                   end
423000100921      *
423100100921     c                   end
423200111017      *
423300111017      *? ------------------------------------------------------------------------
423400111017      *?  SE si tratta di un Partner che invia un reso e mette nel RIF.MITT.NUMERICO
423500111017      *?   il nostro codice bolla BRT. esegue un controllo automatico su esistenza
423600111017      *?   della bolla precedentemente inviata come EXPORT....ed ora in RESO.
423700111017      *? ------------------------------------------------------------------------
423800111028      ****
423900111028      **** se nel Riferimento Numerico viene passato il riferimento di una
424000111028      ****  nostra bolla di export precedentemente inviata di cui si sta facendo
424100111028      **** il reso. (Vedi AZKAR)    controlla
424200111028      **
424300111028     c                   if        PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
424400111028      *
424500111028      * controlla che contenga un numero di 14 cifre valide
424600111028      *   quindi la prima a sinistra deve essere (0)
424700111028     c                   movel     vabrmn        uno0              1 0
424800111028     c                   if        uno0 = 0
424900111028      *
425000111028      *   poi la seconda da sinistra deve NON essere (0)
425100111028     c                   movel     vabrmn        due00             2 0
425200111028     c                   move      due00         uno0
425300111028     c                   if        uno0 > 0
425400111028      *
425500140610     C                   move      VABRMN        dsspe
425600111028     c                   exsr      ctrl_seERAexp
425700111028      *
425800111028     c                   if           Sped_RESO = 'S'
425900150330     c                   move      vabrmn        Cod_RESO_ORM
426000111028     c                   exsr      Write_RESO
426100111028     c                   end
426200111028      *
426300111028     c                   end
426400111028     c                   end
426500111028     c                   end
426600110623      *
426700140610      *? ------------------------------------------------------------------------
426800140610      *?  SE però il PARTNER è molto particolare ed utilizza un suo traffico IMPORT
426900140610      *?   come cliente per restituire i RESI o le BOLLE generate da nostri ORM export
427000140610      *?  il caso: FR-EXPRESS - CALBER:ZZ e NON vuole utilizzare il RFF+CU, si passa
427100140610      *?  a questa tecnica intercettando il segmento voulto dal Partner.
427200140610      *? ------------------------------------------------------------------------
427300140610      ****
427400140610      **** se quindi il codice identificativo arriva carico
427500140610      ****  si verifica che sia un RESO oppure un ORM.
427600140610      **
427700140610      **  DEVE comunque essere abilitato a riceverlo mediante la "PT"
427800140623     c                   if        salva_REF_ORM <> *blank  and
427900140610     c                             PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
428000140610      **
428100140919      **  Attenzione il codice può arrivare vuoto per problemi di caratteri
428200140919      **   inviati sposizionati dal Partner.
428300140919     C                   if          Cod_RESO_ORM <> *blank
428400140610     C                   move      Cod_RESO_ORM  dsspe
428500140610     c                   exsr      ctrl_seERAexp
428600140919     c                   end
428700140610      *
428800140610     c                   if           Sped_RESO = 'S'
428900140610     c                   exsr      Write_RESO
429000140610     c                   else
429100140610     c                   exsr      Write_da_ORM
429200140610     c                   end
429300140610      *
429400140610     c                   end
429500140610      *? ------------------------------------------------------------------------
429600120614      *?  PARTICOLARITA'
429700140610      *? ------------------------------------------------------------------------
429800120614      * x Consegna ai PIANI "FLOOR" codice "FLR"
429900120614     c                   if        FTX_ai_piani = 'P'
430000120614     c                   If         VABTC1 = *blank
430100120614     C                   MOVEL     'P'           VABTC1
430200120614     c                   elseIF     VABTC2 = *blank
430300120614     C                   MOVEL     'P'           VABTC2
430400120614     C                   END
430500120614     c                   End
430600100921      *
430700101027      *? ------------------------------------------------------------------------
430800101027      *?  PARTICOLARITA' sui Clienti Partners x scrittura VAB  (C H I O D I)
430900101027      *? ------------------------------------------------------------------------
431000101027     C*
431100101027     c                   exsr      Chiodi_VAB
431200101027     C*
431300090603      *
431400090211      *? ------------------------------------------------------------------------
431500090213     C   30              WRITE     EDiVAB00                             99
431600090213     C  N30              UPDATE    EDiVAB00                             99
431700090211      *? ------------------------------------------------------------------------
431800090213     c   99              eval      errori_in_Wrt = 'S'
431900090211     C*
432000090211     c                   clear                   wri_vabtic        1
432100090211     C*
432200090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
432300100706     C                   EXSR      Scrive_Riferim
432400090211     C*
432500090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
432600090211     C     §1BF12        IFEQ      'E'
432700100706     C                   EXSR      Scrive_VAT
432800090211     C                   ELSE
432900090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
433000090211     C     §1BFG7        IFEQ      'S'
433100090211     C     §1BFG1        OREQ      'S'
433200100706     C                   EXSR      Scrive_VAD
433300090211     C                   END
433400090211     C                   END
433500090211      *
433600090211     C* Reimposto codice trattamento merce cliente
433700100628     C                   MOVEL     sav_DS1B      DS1B
433800090211     C**
433900090211     C* Do errore se previsto CMR ma non c'è
434000090211     C     §PTCMR        IFNE      ' '
434100090211     C                   EXSR      MEMCMR
434200090211     C                   END
434300090211     C* Do errore se previsto AFB ma non c'è
434400090211     C     §PTNFV        IFNE      ' '
434500090211     C                   EXSR      MEMAFB
434600090211     C                   END
434700090211     C*  Se il cliente vuole il ritorno delle date di consegna
434800090211     C*  è c'è una squdratura fra i segnacolli e il numero
434900090211     C*  dei colli che ci ha passato scrivo EDSUM
435000100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
435100090211     C                   EXSR      WRTSUM
435200090211     C                   END
435300090211     C*
435400090211     C                   ENDSR
435500090211     C*----------------------------------------------------------------
435600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
435700090211     C*----------------------------------------------------------------
435800100630     C     CTRsegn_BAR   BEGSR
435900090211     C*
436000090211     C                   MOVEL     'N'           WNSGER            1
436100090211     C*
436200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
436300100701     C     VABNCL        IFNE      Conta_Segn
436400090211     C                   MOVEL     'S'           WNSGER
436500100630     C                   eval       Squadratura_Colli = 'S'
436600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
436700090211     c                              bollettati'
436800090211     C                   GOTO      FINSGN
436900090211     C                   END
437000100706      *
437100090211     C*  Riordino i segnacolli
437200090211     C     §1BFG7        IFEQ      'N'
437300100630     C                   SORTA     segn_BAR
437400090211     C*  Controllo se sono sequenziali
437500090211     C                   MOVEL     'N'           WNOSEQ            1
437600100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
437700100706     C*
437800100701     C     2             DO        Conta_Segn    X
437900100630     C     segn_BAR(X)   IFNE      WEXSGN
438000090211     C                   MOVEL     'S'           WNOSEQ
438100090211     C                   END
438200100630     C     segn_BAR(X)   ADD       1             WEXSGN
438300090211     C                   END
438400100706     C*
438500090211     C                   END
438600090211     C*
438700090211     C     FINSGN        ENDSR
438800090211     C*----------------------------------------------------------------
438900090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
439000090211     C*----------------------------------------------------------------
439100100630     C     CTRsegn_EEX   BEGSR
439200090211     C*
439300090211     C                   MOVEL     'N'           WNSGER            1
439400090211      *
439500090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
439600090211      *
439700100701     C     VABNCL        IFNE      totale_PCI
439800090211     C                   MOVEL     'S'           WNSGER
439900100630     C                   eval       Squadratura_Colli = 'S'
440000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
440100090211     c                              bollettati'
440200090211     C                   END
440300090211      *
440400090211     C                   ENDSR
440500090211     C*----------------------------------------------------------------
440600090211     C*? ESEGUO SCRITTURA EDiVAD
440700090211     C*----------------------------------------------------------------
440800100706     C     Scrive_VAD    BEGSR
440900090211     C*
441000100701     C* Se non seq. scrivo i record
441100090211     C     §1BFG1        IFEQ      'S'
441200100701     C     Conta_Segn    ANDNE     *ZEROS
441300090211     C*
441400090211     C     §1BFG7        IFEQ      'S'
441500100701     C                   DO        Conta_Segn    X
441600090211     C                   Z-ADD     VABCCM        KCCM
441700100630     C                   Z-ADD     segn_BAR(X)   KNCD
441800090211     C                   Z-ADD     VABCNT        VADCNT
441900090211     C                   MOVEL     VABCMR        VADCMR
442000090211     C                   Z-ADD     VABAAS        VADAAS
442100090211     C                   Z-ADD     VABLNP        VADLNP
442200090211     C                   Z-ADD     VABNRS        VADNRS
442300090211     C                   Z-ADD     VABNSP        VADNSP
442400090211     C                   Z-ADD     1             VADNCL
442500100630     C                   MOVEL     segn_BAR(X)   VADCDP
442600100630     C                   Z-ADD     segn_BAR(X)   VADNCD
442700090211     C                   Z-ADD     *ZEROS        VADNCA
442800090211     C                   Z-ADD     VABCCM        VADCCM
442900090211     C                   movel     VABfgs        VADfgs
443000090213     C                   WRITE     EDiVAD00                             99
443100090213     c   99              eval      errori_in_Wrt = 'S'
443200090211     C                   END
443300090211     C* Se sequenziali scrivo un solo record
443400090211     C                   ELSE
443500090211     C                   Z-ADD     VABAAS        VADAAS
443600090211     C                   Z-ADD     VABLNP        VADLNP
443700090211     C                   Z-ADD     VABNRS        VADNRS
443800090211     C                   Z-ADD     VABNSP        VADNSP
443900090211     C                   Z-ADD     VABNCL        VADNCL
444000100630     C                   Z-ADD     segn_BAR(1)   VADNCD
444100100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
444200090211     C                   Z-ADD     VABCCM        VADCCM
444300090211     C                   movel     VABfgs        VADfgs
444400140902     C                   MOVEL     VABCMR        VADCMR
444500090213     C                   WRITE     EDiVAD00                             99
444600090213     c   99              eval      errori_in_Wrt = 'S'
444700090211     C                   END
444800090211     C*
444900090211     C                   ELSE
445000100701     C                   DO        Conta_Segn    X
445100090211     C                   Z-ADD     VABCCM        KCCM
445200100630     C                   Z-ADD     segn_BAR(X)   KNCD
445300090211     C                   Z-ADD     VABAAS        VADAAS
445400090211     C                   Z-ADD     VABLNP        VADLNP
445500090211     C                   Z-ADD     VABNRS        VADNRS
445600090211     C                   Z-ADD     VABNSP        VADNSP
445700100630     C                   MOVEL     segn_BAR(X)   VADCDP
445800090211     C                   Z-ADD     1             VADNCL
445900090211     C                   Z-ADD     *ZEROS        VADNCD
446000090211     C                   Z-ADD     *ZEROS        VADNCA
446100090211     C                   Z-ADD     VABCCM        VADCCM
446200090211     C                   movel     VABfgs        VADfgs
446300140902     C                   MOVEL     VABCMR        VADCMR
446400090213     C                   WRITE     EDiVAD00                             99
446500090213     c   99              eval      errori_in_Wrt = 'S'
446600090211     C                   END
446700090211     C*
446800090211     C                   END
446900090211     C*
447000090211     C                   ENDSR
447100090211     C*----------------------------------------------------------------
447200090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
447300090211     C*----------------------------------------------------------------
447400100706     C     Scrive_RiferimBEGSR
447500090211      *
447600090211      *  riferimento di consegna destinatario
447700100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
447800100701     c                             CTA_destinatario_in_testata   <> *blank
447900090211     C                   Z-ADD     VABCNT        VATCNT
448000090211     C                   MOVEL     VABCMR        VATCMR
448100090211     C                   Z-ADD     VABCCM        VATCCM
448200090211     C                   movel     VABfgs        VATfgs
448300090211     C                   Z-ADD     VABLNP        VATLNP
448400090211     C                   Z-ADD     VABAAS        VATAAS
448500090211     C                   Z-ADD     VABNRS        VATNRS
448600090211     C                   Z-ADD     VABNSP        VATNSP
448700090211     C                   MOVEL     'A'           VATTRC
448800100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
448900100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
449000100701     c                   else
449100100701     C                   eval       VATNOT = CTA_destinatario_in_testata
449200100701     c                   end
449300090213     C                   WRITE     EDiVAT00                             99
449400090213     c   99              eval      errori_in_Wrt = 'S'
449500090211     c                   end
449600090211      *
449700090211      *  riferimento di consegna telefonico
449800100701     c                   if        COM_telefono_in_dettaglio <> *blank or
449900100701     c                             COM_telefono_in_testata   <> *blank
450000090211     C                   Z-ADD     VABCNT        VATCNT
450100090211     C                   MOVEL     VABCMR        VATCMR
450200090211     C                   Z-ADD     VABCCM        VATCCM
450300090211     C                   movel     VABfgs        VATfgs
450400090211     C                   Z-ADD     VABLNP        VATLNP
450500090211     C                   Z-ADD     VABAAS        VATAAS
450600090211     C                   Z-ADD     VABNRS        VATNRS
450700090211     C                   Z-ADD     VABNSP        VATNSP
450800090211     C                   MOVEL     'B'           VATTRC
450900100701     c                   if        COM_telefono_in_dettaglio <> *blank
451000100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
451100100701     c                   else
451200100701     c                   eval      VATNOT = COM_telefono_in_testata
451300100701     c                   end
451400090213     C                   WRITE     EDiVAT00                             99
451500090213     c   99              eval      errori_in_Wrt = 'S'
451600090211     c                   end
451700090211      *
451800090211      * riferimento del Partner che una volta veniva riportato sul BL4
451900100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
452000100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
452100100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
452200100119      *
452300100701     c                   if        RFF_RifSpediz <> *blank
452400090211     C                   Z-ADD     VABCNT        VATCNT
452500090211     C                   MOVEL     VABCMR        VATCMR
452600090211     C                   Z-ADD     VABCCM        VATCCM
452700090211     C                   movel     VABfgs        VATfgs
452800090211     C                   Z-ADD     VABLNP        VATLNP
452900090211     C                   Z-ADD     VABAAS        VATAAS
453000090211     C                   Z-ADD     VABNRS        VATNRS
453100090211     C                   Z-ADD     VABNSP        VATNSP
453200090211     C                   MOVEL     'C'           VATTRC
453300100701     C                   MOVEL     RFF_RifSpediz VATNOT
453400090213     C                   WRITE     EDiVAT00                             99
453500090213     c   99              eval      errori_in_Wrt = 'S'
453600090211     c                   end
453700090211      *
453800110907      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
453900110907      *   sulla consegna della sua merce
454000110907      *
454100110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
454200110907     c                                  <> *blank
454300110907      *
454400110907     C                   Z-ADD     VABCNT        VATCNT
454500110907     C                   MOVEL     VABCMR        VATCMR
454600110907     C                   Z-ADD     VABCCM        VATCCM
454700110907     C                   movel     VABfgs        VATfgs
454800110907     C                   Z-ADD     VABLNP        VATLNP
454900110907     C                   Z-ADD     VABAAS        VATAAS
455000110907     C                   Z-ADD     VABNRS        VATNRS
455100110907     C                   Z-ADD     VABNSP        VATNSP
455200110907     C                   MOVEL     'I'           VATTRC
455300110907     C                   eval       VATNOT =
455400110907     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
455500110907     C                   WRITE     EDiVAT00                             99
455600110907     c   99              eval      errori_in_Wrt = 'S'
455700110907     c                   end
455800110907      *
455900110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
456000110907     c                                 <> *blank
456100110907      *
456200110907     C                   Z-ADD     VABCNT        VATCNT
456300110907     C                   MOVEL     VABCMR        VATCMR
456400110907     C                   Z-ADD     VABCCM        VATCCM
456500110907     C                   movel     VABfgs        VATfgs
456600110907     C                   Z-ADD     VABLNP        VATLNP
456700110907     C                   Z-ADD     VABAAS        VATAAS
456800110907     C                   Z-ADD     VABNRS        VATNRS
456900110907     C                   Z-ADD     VABNSP        VATNSP
457000110907     C                   MOVEL     'J'           VATTRC
457100110907     C                   eval       VATNOT =
457200110907     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
457300110907     C                   WRITE     EDiVAT00                             99
457400110907     c   99              eval      errori_in_Wrt = 'S'
457500110907     c                   end
457600110907      *
457700131025      *
457800131025      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
457900131025      *   sulla consegna della sua merce
458000131025      *
458100131025     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
458200131025     c                                  <> *blank
458300131025     c                             or FTX_MAIL_come_servizio = 'N'
458400131025      *
458500131025     C                   Z-ADD     VABCNT        VATCNT
458600131025     C                   MOVEL     VABCMR        VATCMR
458700131025     C                   Z-ADD     VABCCM        VATCCM
458800131025     C                   movel     VABfgs        VATfgs
458900131025     C                   Z-ADD     VABLNP        VATLNP
459000131025     C                   Z-ADD     VABAAS        VATAAS
459100131025     C                   Z-ADD     VABNRS        VATNRS
459200131025     C                   Z-ADD     VABNSP        VATNSP
459300131025     C                   MOVEL     'S'           VATTRC
459400131025      *
459500131025      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
459600131025      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
459700131025     C                   eval       VATNOT =
459800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
459900131025      *
460000131025      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
460100131025      * seguenti attivano o meno il servizio x SMS o MAIL
460200131025      *
460300131025      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
460400131025      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
460500131025     C                   eval      %subst(VATNOT:17:1) = 'S'
460600131025     c                   if        FTX_SMS_come_servizio = 'N'
460700131025     c                              or
460800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
460900131025     c                              = *blank
461000131025     C                   eval      %subst(VATNOT:17:1) = 'N'
461100131025     c                   end
461200131025      *
461300131025     C                   eval      %subst(VATNOT:18:1) = 'S'
461400131025     c                   if        FTX_MAIL_come_servizio = 'N'
461500131025     c                              or
461600131025     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
461700131025     c                              = *blank
461800131025     C                   eval      %subst(VATNOT:18:1) = 'N'
461900131025     c                   end
462000131025      *
462100131025     C                   WRITE     EDiVAT00                             99
462200131025     c   99              eval      errori_in_Wrt = 'S'
462300131025     c                   end
462400131025      *
462500090211     C                   ENDSR
462600090211     C*----------------------------------------------------------------
462700090211     C*? ESEGUO SCRITTURA EDiVAT
462800090211     C*----------------------------------------------------------------
462900100706     C     Scrive_VAT    BEGSR
463000090211      *
463100100701     C                   DO        totale_PCI    X
463200090211     C                   Z-ADD     VABCNT        VATCNT
463300090211     C                   MOVEL     VABCMR        VATCMR
463400090211     C                   Z-ADD     VABCCM        VATCCM
463500090211     C                   movel     VABfgs        VATfgs
463600090211     C                   Z-ADD     VABLNP        VATLNP
463700090211     C                   Z-ADD     VABAAS        VATAAS
463800090211     C                   Z-ADD     VABNRS        VATNRS
463900090211     C                   Z-ADD     VABNSP        VATNSP
464000090211     C                   MOVEL     'E'           VATTRC
464100090211      *
464200090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
464300090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
464400090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
464500100701     c                   if        PU_lunVAT     > 0
464600090211      *
464700090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
464800100630     C                   eval      VATnot =
464900100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
465000090211      *
465100090211     c                   else
465200090211      *
465300090211      * Se non è impostato nulla prende il segnacollo passato così com'è
465400100630     C                   MOVEL     segn_EEX(X)   VATNOT
465500090211     c                   end
465600090211      *
465700090213     C                   WRITE     EDiVAT00                             99
465800090213     c   99              eval      errori_in_Wrt = 'S'
465900090211     C                   END
466000090211      *
466100090211     C                   ENDSR
466200090211     C*----------------------------------------------------------------
466300090211     C*? ESEGUO SCRITTURA EDSUM
466400090211     C*----------------------------------------------------------------
466500090211     C     WRTSUM        BEGSR
466600090211     C*
466700100701     C* Se non seq. scrivo i   record
466800100701     C                   DO        Conta_Segn    X
466900090211     C                   Z-ADD     VABAAS        SUMAAS
467000090211     C                   Z-ADD     VABLNP        SUMFLS
467100100701     C                   MOVEL     PT_vabCCM     SUMLNP
467200090211     C                   Z-ADD     VABLNA        SUMLNA
467300090211     C                   Z-ADD     VABZNC        SUMZNC
467400090211     C                   Z-ADD     VABNRS        SUMNRS
467500090211     C                   Z-ADD     VABNSP        SUMNSP
467600100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
467700100701     C                   Z-ADD     PT_vabCCM     SUMCCM
467800090211     C                   MOVEL     *BLANKS       SUMSTS
467900090211     C                   MOVEL     *BLANKS       SUMFLG
468000100701     C                   MOVEL     UNH_Rifer_Msg SUMCNI
468100090211     C                   MOVEL     VABCMR        SUMCMR
468200090211     C                   Z-ADD     0             SUMNFE
468300090211     C                   Z-ADD     VABCCM        VADCCM
468400090213     C                   WRITE     EDSUM000                             99
468500090213     c   99              eval      errori_in_Wrt = 'S'
468600090211     C                   END
468700090211     C*
468800090211     C                   ENDSR
468900090211     c*==================================================================*
469000090211      * Manda un Msg in Posta AS Sede a EDP*
469100090211     c*==================================================================*
469200090211     c     SEND_MSG_EDP  begsr
469300090211      *
469400090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
469500090211      *   Lo manda una sola volta per lavoro
469600090211     c                   IF        Snd_Msg_alert  <> 'N' and
469700090211     c                             User_msg <> *blank
469800090211     c                   z-add     1             Jx
469900090211     C                   exsr      CalQEZSNDMG
470000090211     c                   end
470100090211      *
470200090211     c                   endsr
470300090213     c**********************************************************************
470400090213     c* reperimento numero Spedizione
470500090213     c**********************************************************************
470600090213     C     repnum        BEGSR
470700090213      *
470800090213      *    deve controllare sulla tabella "3C" se il numero spedizione
470900090213      *     deve essere mantenuto oppure no
471000090213     C*
471100090213     C                   clear                   Ds3C
471200090213     C                   Z-ADD     CODUT         TBLKUT
471300090213     C                   MOVEL     '3C'          TBLCOD
471400090213     C                   movel(p)  VABccm        TBLKEY
471500090213     C     KTABel        CHAIN     TABEL00F                           30
471600090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
471700090213     C                   MOVEL     TBLUNI        Ds3C
471800090213     C                   END
471900090213      *
472000090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
472100090213     c                   if        §3CFsp <> 'S'
472200090213      *
472300090213     C* NSP => Stacco un numeratore da AZNUM
472400090213     c                   movel     kpjbu         svkpjbu
472500090213     c                   clear                   kpjbu
472600090213     C                   clear                   TRUL33DS
472700090213     C                   eval      I33OPE = *zeros
472800090213     C                   eval      I33CNU = 302
472900090213     C                   eval      I33NUM = 1
473000090213     C                   movel     TRUL33DS      KPJBU
473100090213     C                   call      'TRUL33R'
473200090213     C                   parm                    KPJBA
473300090213     C                   movel     KPJBU         TRUL33DS
473400090213     c                   movel     svkpjbu       kpjbu
473500090213      ****
473600090213     C                   if        O33ERR = *zeros
473700100630     c                   z-add     o33nrf        NUM_Spediz
473800090213     C                   else
473900090213     C                   endif
474000090213      ****
474100090213     c                   ELSE
474200090213      *   se si vuole mantenere il numero spedizione
474300090213      ****
474400090213     C                   MOVEL     WOGGI         WANNO             4 0
474500090213     C                   MOVE      WANNO         KAAA
474600090213     C                   Z-ADD     3             KCNU
474700090213     C                   Z-ADD     VABlnp        KFIL
474800090213     C     KNUF          CHAIN     FLNUF                              9999
474900090213     C     *IN99         IFEQ      '0'
475000090213     C                   ADD       1             NUFNUM
475100100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
475200090213     C                   UPDATE    FLNUF
475300090213     C                   ELSE
475400090213     C                   END
475500090213     C*
475600090213     c                   EndIF
475700090213      **
475800090213     C                   ENDSR
475900090213     C*----------------------------------------------------------------
476000090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
476100090213     C*----------------------------------------------------------------
476200090213     C     MEMCMR        BEGSR
476300090213     C*
476400090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
476500090213     C     §PTCM1        IFEQ      'S'
476600090213     C                   CLEAR                   EDRDE000
476700090213     C                   MOVEL     'TD1154'      KNMC
476800090213     C                   Z-ADD     1             KNRP
476900100701     C                   MOVEL     PT_vabCCM     KLNP1
477000090213     C     KRDE          CHAIN     EDRDE01L                           31
477100090213     C     *IN31         IFEQ      '1'
477200090213     C                   Z-ADD     VABAAS        RDEAAS
477300090213     C                   Z-ADD     VABLNP        RDELNP
477400090213     C                   Z-ADD     VABNRS        RDENRS
477500090213     C                   Z-ADD     VABNSP        RDENSP
477600090213     C                   MOVEL     'TD1154'      RDENMC
477700090213     C                   Z-ADD     1             RDENRP
477800100706     C                   MOVEL     BGM_nrCMR     RDEVAL
477900100701     c                   if          RFF_NrCMR <> *Blank
478000100701     C                   MOVEL     RFF_NrCMR     RDEVAL
478100100701     c                   end
478200090213     C                   WRITE     EDRDE000                             99
478300090213     c   99              eval      errori_in_Wrt = 'S'
478400090213     C                   END
478500090213     C*  Memorizzo qualificatore CMR
478600090213     C                   CLEAR                   EDRDE000
478700090213     C                   MOVEL     'TD1153'      KNMC
478800090213     C                   Z-ADD     1             KNRP
478900100701     C                   MOVEL     PT_vabCCM     KLNP1
479000090213     C     KRDE          CHAIN     EDRDE01L                           31
479100090213     C     *IN31         IFEQ      '1'
479200090213     C                   Z-ADD     VABAAS        RDEAAS
479300090213     C                   Z-ADD     VABLNP        RDELNP
479400090213     C                   Z-ADD     VABNRS        RDENRS
479500090213     C                   Z-ADD     VABNSP        RDENSP
479600090213     C                   Z-ADD     1             RDENRP
479700090213     C                   MOVEL     'TD1153'      RDENMC
479800090213     C                   MOVEL     'CMR'         RDEVAL
479900090213     C                   WRITE     EDRDE000                             99
480000090213     c   99              eval      errori_in_Wrt = 'S'
480100090213     C                   END
480200090213     C*  Memorizzo la data
480300090213     C                   CLEAR                   EDRDE000
480400090213     C                   MOVEL     'TD2380'      KNMC
480500090213     C                   Z-ADD     1             KNRP
480600100701     C                   MOVEL     PT_vabCCM     KLNP1
480700090213     C     KRDE          CHAIN     EDRDE01L                           31
480800090213     C     *IN31         IFEQ      '1'
480900090213     C                   Z-ADD     VABAAS        RDEAAS
481000090213     C                   Z-ADD     VABLNP        RDELNP
481100090213     C                   Z-ADD     VABNRS        RDENRS
481200090213     C                   Z-ADD     VABNSP        RDENSP
481300090213     C                   Z-ADD     1             RDENRP
481400090213     C                   MOVEL     'TD2380'      RDENMC
481500100630     C                   MOVEL     DTM_DtCMR     RDEVAL
481600090213     C                   WRITE     EDRDE000                             99
481700090213     c   99              eval      errori_in_Wrt = 'S'
481800090213     C                   END
481900090213     C*
482000090213     C                   END                                                    §PTCM1 = 'S'
482100090213     C*
482200090213     C                   ENDSR
482300090213     C*----------------------------------------------------------------
482400090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
482500090213     C*----------------------------------------------------------------
482600090213     C     MEMAFB        BEGSR
482700090213     C*
482800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
482900090213     C     §PTNF1        IFEQ      'S'
483000090213     C                   CLEAR                   EDRDE000
483100100701     C                   MOVEL     PT_vabCCM     KLNP1
483200090213     C                   MOVEL     'TD1154'      KNMC
483300090213     C                   Z-ADD     1             KNRP
483400090213     C     KRDE          CHAIN     EDRDE01L                           31
483500090213     C     *IN31         IFEQ      '1'
483600090213     C                   Z-ADD     VABAAS        RDEAAS
483700090213     C                   Z-ADD     VABLNP        RDELNP
483800090213     C                   Z-ADD     VABNRS        RDENRS
483900090213     C                   Z-ADD     VABNSP        RDENSP
484000090213     C                   MOVEL     'TD1154'      RDENMC
484100100701     C                   if        §PTCM1 <> 'S' or
484200100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
484300090213     C                   Z-ADD     1             RDENRP
484400090213     C                   ELSE
484500090213     C                   Z-ADD     2             RDENRP
484600090213     C                   END
484700100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
484800100701     c                   if          RFF_NrFviaggio <> *Blank
484900100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
485000100701     c                   end
485100090213     C                   WRITE     EDRDE000                             99
485200090213     c   99              eval      errori_in_Wrt = 'S'
485300090213     C                   END
485400090213     C*  Memorizzo qualificatore CMR
485500090213     C                   CLEAR                   EDRDE000
485600100701     C                   MOVEL     PT_vabCCM     KLNP1
485700090213     C                   MOVEL     'TD1153'      KNMC
485800090213     C                   Z-ADD     1             KNRP
485900090213     C     KRDE          CHAIN     EDRDE01L                           31
486000090213     C     *IN31         IFEQ      '1'
486100090213     C                   Z-ADD     VABAAS        RDEAAS
486200090213     C                   Z-ADD     VABLNP        RDELNP
486300090213     C                   Z-ADD     VABNRS        RDENRS
486400090213     C                   Z-ADD     VABNSP        RDENSP
486500100701     C                   if        §PTCM1 <> 'S' or
486600100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
486700090213     C                   Z-ADD     1             RDENRP
486800090213     C                   ELSE
486900090213     C                   Z-ADD     2             RDENRP
487000090213     C                   END
487100090213     C                   MOVEL     'TD1153'      RDENMC
487200090213     C                   MOVEL     'AFB'         RDEVAL
487300090213     C                   WRITE     EDRDE000                             99
487400090213     c   99              eval      errori_in_Wrt = 'S'
487500090213     C                   END
487600090213     C*  Memorizzo la data
487700090213     C                   CLEAR                   EDRDE000
487800100701     C                   MOVEL     PT_vabCCM     KLNP1
487900090213     C                   MOVEL     'TD2380'      KNMC
488000090213     C                   Z-ADD     1             KNRP
488100090213     C     KRDE          CHAIN     EDRDE01L                           31
488200090213     C     *IN31         IFEQ      '1'
488300090213     C                   Z-ADD     VABAAS        RDEAAS
488400090213     C                   Z-ADD     VABLNP        RDELNP
488500090213     C                   Z-ADD     VABNRS        RDENRS
488600090213     C                   Z-ADD     VABNSP        RDENSP
488700100701     C                   if        §PTCM1 <> 'S' or
488800100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
488900090213     C                   Z-ADD     1             RDENRP
489000090213     C                   ELSE
489100090213     C                   Z-ADD     2             RDENRP
489200090213     C                   END
489300090213     C                   MOVEL     'TD2380'      RDENMC
489400100630     C                   MOVEL     DTM_DtFViag   RDEVAL
489500090213     C                   WRITE     EDRDE000                             99
489600090213     c   99              eval      errori_in_Wrt = 'S'
489700090213     C                   END
489800090213     C*
489900090213     C                   END                                                    §PTCM1 = 'S'
490000090213     C*
490100090213     C                   ENDSR
490200090213     c*==================================================================*
490300090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
490400090213     c*==================================================================*
490500090213     c     Imp_ADDR_mail begsr
490600090213      *
490700090213     c                   clear                   Indirizzi
490800090213      *
490900090213      *  Lunghezza indirizzi presenti
491000100630     c                   eval      Lunghezza_Indirizzo =
491100100630     c                                       %Len(%TrimR(Mail_Ind))
491200090213     c                   z-add     1             al_char           3 0
491300090213     c                   z-add     1             dal_char          3 0
491400090213     c                   z-add     1             tot_char          3 0
491500090213      *
491600090213     c     successivo    tag
491700090213      *
491800090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
491900090213      *   e aggiunge il dominio Bartolini + il (;) separatore
492000090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
492100090213     c                   eval      tot_char = al_char - dal_char
492200100630     c                   z-add     dal_char      dalCarattere
492300100630     c                   z-add     tot_char      xTOTcaratteri
492400090213      *
492500090213     c                   clear                   Indir_Bartol     40
492600090213     c                   clear                   Indir_Parz       20
492700100630     c                   eval      Indir_Parz =
492800100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
492900100630      *
493000090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
493100090213     c                             %Trim(bart_it)
493200090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
493300090213      *
493400100630     c                   if        al_char = Lunghezza_Indirizzo
493500090213     c                   goto      fine_indmail
493600090213     c                   else
493700090213     c                   eval      dal_char = ( al_char + 1 )
493800090213     c                   goto      successivo
493900090213     c                   end
494000090213      *
494100090213     C     fine_indmail  TAG
494200090213      *
494300090213     C                   ENDSR
494400090213     c*==================================================================*
494500090213      * Manda un Msg x E-mail
494600090213     c*==================================================================*
494700090213     c     Invio_Mail    begsr
494800090213      *
494900090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
495000090213     C* Inizializzo variabili
495100090213     C                   movel     *blanks       wrkEml          253
495200090213     C                   movel     *blanks       wrkMsg         5000
495300090213     C                   movel     *blanks       wrkOgg           44
495400090213      *
495500090213     C* Valorizzo i campi della e-m@ail - indirizzo
495600100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
495700090213     C                   eval      wrkEml = Indirizzi
495800090213     C                   eval      wrkOgg = Oggetto
495900090213     C                   eval      wrkMsg = Messaggio
496000110203      ********
496100110203     C********           call(e)   'TIS701C'
496200110203     C********           parm                    wrkEml
496300110203     C********           parm                    wrkOgg
496400110203     C********           parm                    wrkMsg
496500110203     C*
496600110203      ** *****
496700110203     C                   call(e)   'TRTCT00R2'
496800110203     C                   parm                    wrkEml
496900110203     C                   parm                    wrkOgg
497000110203     C                   parm                    wrkMsg
497100110203     C*
497200090213     C*
497300090213     C                   ENDSR
497400090603      *
497500090211     ***********************************************************************
497600090211     **
497700090211     ** Send Message (QEZSNDMG) API
497800090211     **
497900090211     ***********************************************************************
498000090211     C     CalQEZSNDMG   BEGSR
498100090211      *
498200090211     ** Invio messaggio all'utente.
498300090211     C                   EVAL      SndMg03 = msgDSP
498400090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
498500090211     C                   EVAL      SndMg05(Jx) = User_msg
498600090211     C                   EVAL      SndMg06 = Jx
498700090211     C                   CLEAR                   QUSEC
498800090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
498900090211
499000090211     C                   CALL      'QEZSNDMG'
499100090211     C                   PARM                    SndMg01
499200090211     C                   PARM                    SndMg02
499300090211     C                   PARM                    SndMg03
499400090211     C                   PARM                    SndMg04
499500090211     C                   PARM                    SndMg05
499600090211     C                   PARM                    SndMg06
499700090211     C                   PARM                    SndMg07
499800090211     C                   PARM                    SndMg08
499900090211     C                   PARM                    Qusec
500000090211     C                   PARM                    SndMg10
500100090211     C                   PARM                    SndMg11
500200090211     C                   PARM                    SndMg12
500300090211      *
500400090211     C                   ENDSR
500500100708      * ?------------------------------------------------------------------ */
500600100708      *  Controlla la trasmissione   se completa
500700100708      * ?------------------------------------------------------------------ */
500800100708     c     Check_Trasm   Begsr
500900100708
501000100708     C                   clear                   se_errore
501100100708     C                   clear                   Riga_iNIZIo       1
501200100708     C                   clear                   Riga_fINe         1
501300100708      ** primo  record
501400100708     c     *start        setll     tivin00r
501500100708      *
501600100708     c     rileggi       tag
501700100708     c                   EXSR      LEGGE_TIVIN_R
501800100708      *
501900100708     c                   if        EoFTivin <> 'S'
502000100708      *
502100100708     c                   if        dati = *blank
502200100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
502300100708      * le leggo e le escludo.
502400100708     C                   eval      vinFLG = '1'
502500100708     c                   update    Tivin000
502600100708     c                   goto      rileggi
502700100708     c                   end
502800100708      * ?              /*---------------------- */
502900100708     c                   exsr      Decod_Segmento
503000100708      * ?              /*---------------------- */
503100100708     c                   endif
503200100708      **
503300100708      ** ultimo record
503400100708     c     *hival        setll     tivin00r
503500100708     c     leggiAncora   tag
503600100708     c                   readp     tivin00r
503700100708     c                   if        %Eof(tivin00r)
503800100708     c                   move      'S'           EoFTivin
503900100708     c                   else
504000100708     c                   clear                   dati
504100100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
504200100708      *
504300100708     c                   if        dati = *blank
504400100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
504500100708      * le leggo e le escludo.
504600100708     C                   eval      vinFLG = '1'
504700100708     c                   update    Tivin000
504800100708     c                   goto      leggiAncora
504900100708     c                   end
505000100708      * ?              /*---------------------- */
505100100708     c                   exsr      Decod_Segmento
505200100708      * ?              /*---------------------- */
505300100708     c                   endif
505400100708
505500100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
505600100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
505700100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
505800100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
505900100708      * ?-----> Errore
506000100708     C                   eval      se_errore = 'S'
506100100708     c                   end
506200100708
506300100708     c                   endSR
506400111017      * ?------------------------------------------------------------------ */
506500111017     C*? CHIODI VAB  prima della scrittura del VAB Particolarità su Clienti/Partners
506600111017      * ?------------------------------------------------------------------ */
506700111017     C     Chiodi_VAB    BEGSR
506800140610      *
506900140422      *? ------------------------------------------------------------------------
507000140609      *?   Se arriva da CALBERSON (GEODIS) una spedizione riconosciuta come  RESO
507100140609      *?   o da nostro ORM la spedizione deve prendere il codice FR-EXPRESS come
507200140609      *?   Partner e non come un IMPORT da Cliente. Quindi 3330003 invece di 0493298.
507300141111      *** fino al 16/11/2014
507400141111      *? ------------------------------------------------------------------------
507500141111      *? ------------------------------------------------------------------------
507600141111      *** dal 17/11/2014
507700141111      *?   Partner GEODIS trasmette tutto con CALBER:ZZ dividendo per PLATFORM LIST
507800141111      *?    NAD+SF --> usato come su RFF+FF a livello di testata.
507900141111      *?  Quindi le bolle non sono più 0493298 ma 333 o 334 da Lyone o Parigi
508000141111      *?   vale ancora la regola dei RESI e delle bolle da ORM.
508100140609      *? ------------------------------------------------------------------------
508200141112      *
508300141112      *   RESO o da ORM  -------------------------
508400141112      * vale x 333 e 334
508500141112     c                   IF        cod_RESO_ORM <> *blank
508600141112      *
508700141112      *   LINEA 333 ------------------------------
508800141111      *     LYONE
508900140609     c                   if        vabCCM = 493298
509000141111     c                              or
509100141111     c                             vabCCM = 3330003
509200140609      * MI SERVE cod.cliente e LINEA
509300140610     c                   EVAL      vabCCM = 3330003
509400140610     c                   EVAL      vabLNP = 333
509500140610     c                   EVAL      vabFGS = 333
509600141112     c                   end
509700141112      **
509800141112      *   LINEA 334 ------------------------------
509900141111      *     PARIGI
510000141112     c                   if        vabCCM = 3340001
510100141111      * MI SERVE cod.cliente e LINEA
510200141112     c                   EVAL      vabCCM = 3340001
510300141111     c                   EVAL      vabLNP = 334
510400141112     c                   EVAL      vabFGS = 334
510500141112     c                   end
510600141112      *
510700141112      *   CLIENTE 333 e 334 ----------------------
510800141112     c                   if        vabCCM = 3330003
510900141112     c                              or
511000141112     c                             vabCCM = 3340001
511100141112      *
511200141112      *  Porto Assegnato  (Anche se lo ricevo...NON mi FIDO)
511300141112     c                   IF        VABCAS > *zeros
511400141112     C                   movel     '6'           VABCBO                         + C/Ass
511500141112     C                   ELSE
511600141112     C                   movel     '2'           VABCBO                         Senza C/Ass
511700141112     c                   end
511800141112      *
511900141112      * Se il cliente non era sulla schiera lo aggiunge
512000141112     C     VABCCM        lookup    skCLienti                              39
512100141112     C                   if        *in39 = *off
512200141112     C                   add       1             sk
512300141112     C                   z-add     VABCCM        skCLienti(sk)
512400141112     C                   END
512500141112      *
512600141112     c                   end
512700141112      *   CLIENTE 333 e 334 ----------------------
512800141112      *
512900141112     c                   endIF
513000141112      *   RESO o da ORM  -------------------------
513100141111      *
513200140609      *? ------------------------------------------------------------------------
513300140422      *? ATTENZIONE: (Chiodo PF International da VANDUUREN) su richiesta di TERESA SARACINO
513400140422      *? si deve distinguere il Mittente Originale da altri traffici ricevuti
513500140422      *?  con EDI deve essere scritto come (PF Concept Italia Srl)
513600140422      *? ------------------------------------------------------------------------
513700140422     C     vabCCM        ifeq      897778
513800150126     C     vabCCM        oreq      3250048
513900140422     C                   eval       VABRMO =
514000140422     c                             'PF CONCEPT ITALIA SRL'
514100140422     c                   end
514200140422      *
514300111017      *
514400101027      *? ------------------------------------------------------------------------
514500101027      *? ATTENZIONE: (Chiodo E.Arden)
514600101027      *?  x E.Arden o Unilever           501790 e 501791
514700101027      *? Se è un cliente e non un Partner deve togliere la Ragione Sociale
514800101027      *? originale e mettere un punto al posto della RagSoc poichè non
514900101027      *?  deve essere vuoto il campo.
515000101027      *? ------------------------------------------------------------------------
515100101027     C     VABRMO        IFne      *blank
515200110913     C     §PUTPC        andne     'P'
515300110913     C**** §PUTPC        andeq     'C'
515400101027     C     §PTKSC        ifeq      501790
515500101027     C     §PTKSC        oreq      501791
515600101027     C                   MOVEL     *BLANKS       VABRMO
515700101027     C                   MOVEL     '.'           VABRMO
515800101027     c                   end
515900101027     C                   END
516000101027      *
516100101027      *? ------------------------------------------------------------------------
516200101027      *? Chiodo da togliere a gennaio 2005 :
516300101027      *? attenzione solo x ARVATO non deve essere importato l'IMPORTO ASSICURATO
516400101027      *? come da accordi commerciali anche se lo inviano sul tracciato EDI
516500101027      *? ED ANCHE IL VOLUME dal 1/3/2005
516600101027      *? ------------------------------------------------------------------------
516700101027     C     §PTKSC        ifeq      932337
516800101027     C     VABCCM        oreq      52981
516900101027     C     VABCCM        oreq      932353
517000101027     c     VABCCM        oreq      932461
517100101027     C                   clear                   VABIAS                         IMPORTO
517200101027     C                   clear                   VABVAS                         ASSICURATO
517300101027     C                   clear                   VABVLB                         VOLUME BOLLETTATO
517400101027     c                   end
517500101027     C*
517600101027      *? ------------------------------------------------------------------------
517700101027      *? Chiodo x SFS come da richiesta del 25/5/2007:
517800101027      *? mettere sui 2 campi note:
517900101027      *?      not: DDT SUL COLLO
518000101027      *?      nt2: NON CONSEGNARE IN CASO DI AVARIA
518100101027      *? ------------------------------------------------------------------------
518200101027     C     §PTKSC        ifeq      1160950
518300101027     C     VABNOT        andeq     *blank
518400101027     C     VABNT2        andeq     *blank
518500101027     C                   eval      vabNOT = 'DDT SUL COLLO'
518600101027     C                   eval      vabNT2 = 'NON CONSEGNARE IN CASO DI AVARIA'
518700101027     c                   end
518800101027     C*
518900101027      *? ------------------------------------------------------------------------
519000101027      *? Chiodo x SEUR cliente REBELIO  del 24/9/2007:
519100101027      *?  il cliente se ha il COD vuole sempre in contanti mentre
519200101027      *?   su IFCSUM manda sempre il FTX+PAI+++CO' che noi tradurremmo come OM.
519300101027      *? Chiodo Richiesto dalla Villa con Mail il 24/9/2007
519400101027      *? ------------------------------------------------------------------------
519500101027     C     §PTKSC        ifeq      433048
519600101027     C     VABCCM        andeq     433972
519700101027     C     VABTIC        andne     *blank
519800101027     C                   eval      vabTIC = '  '
519900101027     c                   end
520000101027     C*
520100101027     C*  Per non bloccare la bollettazione
520200101027     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
520300101027     C*  o se il peso non è presente... imposto fisso il minimo 0,1
520400130315     c**********         if        vabpkb = 0
520500130315     c**********         z-add     0,1           vabpkb
520600130315     c**********         end
520700101027     C*
520800101027     C*
520900101027      *? ------------------------------------------------------------------------
521000101027      *? Chiodo x AZKAR BISA richiesta  del 01/4/2009: dalla VILLA con mail di Azkar
521100101027      *?  tutti i contrassegni devono essere intestati come "OM"
521200101027      *? Chiodo Richiesto dalla Villa con Mail il 01/4/2009
521300101027      *? ------------------------------------------------------------------------
521400101027     C     §PTKSC        ifeq      22349
521500101027     C     VABCCM        andeq     22349
521600101027     C     VABTIC        ifne      *blank
521700101027     C     VABCAS        orgt      *zeros
521800101027     C                   eval      vabTIC = 'OM'
521900101027     c                   end
522000101027     c                   end
522100110222     C*
522200110627     C*
522300110222      *? ------------------------------------------------------------------------
522400110222      *? Chiodo x Calberson Maison du Monde codice 0040712 le spedizioni devono
522500110222      *?  essere prese TUTTE CON APPUNTAMENTO - richiesta di Giorgio dal Vivo
522600110222      *? La Marca Alfredo e Carrozzo William x conoscenza del 22/2/2011
522700110222      *? ------------------------------------------------------------------------
522800160830      *? dal 1/9 2016 il cliente NON vuole più la merce su appuntamento (Giorgio dal Vivo)
522900160830     C     VABCCM        ifeq      40712
523000160830     C     Woggi         andlt     20160901
523100160830     c                   if        vabtc1 = *blank
523200110222     c                   eval      vabtc1 = 'A'
523300110222     c                   elseif    vabtc2 = *blank
523400110222     c                   eval      vabtc2 = 'A'
523500110222     c                   end
523600110222     c                   end
523700160830      * riportare il numero di telefono del contatto x fare l'alert con SMS
523800170705     C                   if         (VABCCM = 40712  or
523900170705     C                               VABCCM = 41186) and
524000160830     c                             COM_telefono_in_dettaglio <> *blank and
524100160830     c                             FTX_ServizioSMS_xDestinatario = *blank
524200160830      *
524300160830      *  sposta il numero telefonico x fare l'SMS
524400160830     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:16) =
524500160830     c                             COM_telefono_in_dettaglio
524600160830     c                   end
524700110509     C*
524800160830     C*
524900110509      *? ------------------------------------------------------------------------
525000110509      *? Chiodo x GEODIS MAR x i COD devono essere tutti come "OM" solo per 40566
525100110509      *? ------------------------------------------------------------------------
525200110509     C     VABCCM        ifeq      40566
525300110509     c                   if        vabcas > 0
525400110509     c                   eval      vabtic = 'A'
525500110509     c                   end
525600110509     c                   end
525700110509     C*
525800101027     C                   ENDSR
525900140609      * ?------------------------------------------------------------------ */
526000140609     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
526100140609      * ?------------------------------------------------------------------ */
526200140609     C     Write_RESO    BEGSR
526300140609      *
526400111028      *  Imposta nella descrizione del CMR l'intestazione di RESI
526500111028     c                   movel(p)  vabCMR        campo35          35
526600111028     c                   movel(p)  'RESI__'      vabCMR
526700111028     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
526800111028      *
526900111028      *  e sulla 2° parte della ragione sociale viene apposta la dicitura di RESO
527000111028     c                   move(p)   VABrsd        campo44          44
527100111028     c                   movel     '**RESO** '   campo44
527200111028     C                   movel     campo44       VABrsd
527300111028     C                   move      campo44       campo9            9
527400111028      *
527500111028      * Sposta tutta la ragione sociale di (9 carat.) anche sulla 2°ragione sociale
527600111028     c                   move(p)   VABrd2        campo44          44
527700111028     c                   movel     campo9        campo44
527800111028     C                   movel     campo44       VABrd2
527900140610      *
528000140610      *  e sulle NOTE  viene impostato "Sp.EXP:"
528100140610     c                   clear                   campo57          57
528200140610     c                   eval      campo57 = 'ExSped.' + %trim(cod_RESO_ORM)
528300140610     c                   move      VABnot        campo57
528400140610     C                   movel     campo57       VABnot
528500140610     C                   move      campo57       campo22          22
528600140610      *
528700140610      * Sposta tutta la ragione sociale di (x carat.) anche sulla 2°ragione sociale
528800140610     c                   move(p)   VABnt2        campo57
528900140610     c                   movel     campo22       campo57
529000140610     C                   movel     campo57       VABnt2
529100140610      *
529200111028      *
529300111027     c                   endSR
529400100708      * ?------------------------------------------------------------------ */
529500111027     C*? Controllo se era prima una Bolla di Export che sta rientrando
529600111027      * ?------------------------------------------------------------------ */
529700111027     C     ctrl_seERAexp Begsr
529800111027     C*
529900111027     C*   imposta la chiave per controllare la spedizione
530000111027     C*    in sede
530100111027     C                   movel     '20'          dsspe
530200111027     C*
530300111027     C                   move      SPEAAS        TASAAS
530400111027     C                   move      SPELNP        TASLNP
530500111027     C                   move      SPENRS        TASNRS
530600111027     C                   move      SPENSP        TASNSP
530700111027     C     KTAS          chain     titas30c
530800111027     c                   if        %found(titas30c)
530900111027     c                               and tasLNA >299 and tasLNA <=400
531000111027     c                   eval      era_un_export_RESO_dal_PARTNER = 'S'
531100111027     c                   eval                           Sped_RESO = 'S'
531200111027     c                   end
531300111027     C*
531400111027     C                   ENDSR
531500140610      * ?------------------------------------------------------------------ */
531600140610     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
531700140610      * ?------------------------------------------------------------------ */
531800140610     C     Write_da_ORM  BEGSR
531900140610      *
532000140610      *  Imposta nella descrizione del CMR l'intestazione di RESI
532100140610     c                   movel(p)  vabCMR        campo35          35
532200140610     c                   movel(p)  'DaORM_'      vabCMR
532300140610     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
532400140610      *
532500140623      *  e sulle NOTE  viene impostato da ORM con il codice dell'ORM generante
532600140623     c                   clear                   campo70          70
532700140623     c                   clear                   note_70          70
532800140623     c                   eval      campo70 = 'DaORM:' + %Trim(salva_REF_ORM)
532900140623     c                   eval      note_70 = %trim(VABNOT) +'/'+ %trim(VABNT2)
533000140623     c                   eval      campo70 = %Trim(campo70)+'/'+ %Trim(note_70)
533100140623     c                   eval      VABNOT = %subst(campo70:1:35)
533200140623     c                   eval      VABNT2 = %subst(campo70:36:35)
533300140610      *
533400140610     c                   endSR
533500120629      * ?------------------------------------------------------------------ */
533600120629     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
533700111027      * ?------------------------------------------------------------------ */
533800120629     C     CL_ParticolarebegSR
533900120629      *
534000120629     C     Rifer_Cliente IFNE      *BLANKS
534100120629      * ------>>>
534200120629     C                   eval      XX = 1
534300120629     C                   movel     §PTKSC        Key_CL           35
534400120629     C                   movel     Rifer_Cliente WCOM28           28
534500120629     C                   move      WCOM28        Key_CL
534600120629      *
534700120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
534800120629     C     *IN34         IFEQ      '1'
534900120629     C                   movel     Des_Tb_CL(XX) EDIDSCL
535000120629      *
535100120629      * in testata messaggio
535200120629     c                   if        Dati_di_Testata  ='S'
535300120629     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
535400120629     C                   movel     §CLtar        Tes_RFF_FF_tar    3
535500120629     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
535600160209      *-
535700160209     C                   if        §CLftx   <> *blank
535800160209     C                   move      §CLftx        Tes_RFF_FF_fgs    3
535900160209     c                   else
536000160209     C                   move      §CLlnp        Tes_RFF_FF_fgs
536100160209     c                   end
536200160209      *-
536300120629     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
536400120629     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
536500120629     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
536600120629     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
536700120629     C                   movel     §CLtic        Tes_RFF_FF_tic    2
536800120629     c                   else
536900120629      * in dettaglio Messaggio
537000120629     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
537100120629     C                   movel     §CLtar        Det_RFF_FF_tar    3
537200120629     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
537300160209      *-
537400160209     C                   if        §CLftx   <> *blank
537500160209     C                   move      §CLftx        Det_RFF_FF_fgs    3
537600160209     c                   else
537700160209     C                   move      §CLlnp        Det_RFF_FF_fgs
537800160209     c                   end
537900160209      *-
538000120629     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
538100120629     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
538200120629     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
538300120629     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
538400120629     C                   movel     §CLtic        Det_RFF_FF_tic    2
538500120629     c                   end
538600120629      *
538700120629      * imposta la LNP  e la Serie x il dettaglio specifico
538800120629     c
538900120629      *  In base al cod.trattamento merce reperisco tipo tratt.
539000120629      *    per un dettaglio specifico.
539100120629     C                   CLEAR                   DS1B
539200120629     C                   Z-ADD     1             Y                 3 0
539300120629     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
539400120629     C     *IN32         IFEQ      '1'
539500120629     C                   eval       DS1B = Des_Tb_CTM(Y)
539600120629     C                   END
539700120629      *
539800120629      * Se il cliente non era sulla schiera lo aggiunge
539900120629     C     §CLksc        lookup    skCLienti                              39
540000120629     C                   if        *in39 = *off
540100120629     C                   add       1             sk
540200120629     C                   z-add     §CLksc        skCLienti(sk)
540300120629     c                   endif
540400120629      *
540500120629     C                   ENDIF
540600120629      * ------>>>
540700120629     C                   ENDIF
540800120629      *
540900120629      * in Dettaglio  preimposto i campi rilevati dalle tabelle
541000120629      *   per clienti particolari (CL)
541100120629      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
541200120629      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
541300120629      *   successivi per altre decodifiche.
541400120629     c                   if        Dati_di_Testata <>'S'
541500120629      *
541600120629      *   se aveva un RFF+FF in dettaglio
541700120629     C                   if         Det_RFF_FF_ksc > 0
541800120629      *
541900120629     C                   eval      VABccm = Det_RFF_FF_ksc
542000120629     c                   eval      VABlnp = Det_RFF_FF_lnp
542100120629     c                   eval      VABnrs = Det_RFF_FF_nrs
542200120629     C                   eval      VABctm = Det_RFF_FF_ctm
542300120629     C                   movel     Det_RFF_FF_tarVABctr
542400160209     C                   move      Det_RFF_FF_fgsVABfgs
542500120629      *
542600120629      *   altrimenti se aveva un RFF+FF in Testata
542700120629     c                   elseif     Tes_RFF_FF_ksc > 0
542800120629      *
542900120629     C                   eval      VABccm = Tes_RFF_FF_ksc
543000120629     c                   eval      VABlnp = Tes_RFF_FF_lnp
543100120629     c                   eval      VABnrs = Tes_RFF_FF_nrs
543200120629     C                   eval      VABctm = Tes_RFF_FF_ctm
543300120629     C                   movel     Tes_RFF_FF_tarVABctr
543400160209     C                   move      Tes_RFF_FF_fgsVABfgs
543500120629      *
543600120629     c                   end
543700120629      *>>>>>>>>
543800120629     c                   ENDif
543900120629     C*
544000120629     C                   ENDSR
544100121105      *---------------------------------------------------------------*
544200121105      * CTRL caricamento schiere    PT                               -*
544300121105      *---------------------------------------------------------------*
544400121105     C     Check_PT      begsr
544500121105     C*
544600121105     c* Controllo riempimento schiera
544700121105     c                   clear                   trul0sds
544800121105     c                   eval      i0sski='PT_key'
544900121105     c                   eval      i0sele=%elem(PT_key)
545000121105     c                   eval      i0spie=x
545100121105     c                   eval      i0sfile='EDTAB00F'
545200121105     c                   eval      i0ssif=knsif
545300121105     c                   eval      i0spgm='TRTCT93R'
545400121112     c                   move      kpjbu         SvKpjbu
545500121112     c                   clear                   kpjbu
545600121105     c                   movel     trul0sds      kpjbu
545700121105     c                   call      'TRUL0SR'
545800121105     c                   parm                    kpjba
545900121112     c                   eval      kpjbu = SvKpjbu
546000121105     C*
546100121105     C                   ENDSR
546200120629      * ?------------------------------------------------------------------ */
546300090211     O*****************************************************************
546400090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
546500090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
546600090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
546700090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
