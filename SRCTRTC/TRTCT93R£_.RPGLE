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
006501160205     D EDIDSPS       E DS
006600100701     D EDIDSPV       E DS
006700120629     D EDIDSPZ       E DS
006800100701     D EDIDSCL       E DS
006900100701     D EDIDSSS       E DS
007000100701     D EDIDSTC       E DS
007100100701      **
007200100701      *  DS x salvataggio dati impostati in EDiVAB
007300100701     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
007400100701     D SALVA           DS           545
007500100701      **
007600100701     D WLBDA8          DS
007700100701     D  G02DAT                 1      8  0
007800100701     D  G02INV                 9     16  0
007900100701     D  G02ERR                17     17
008000100701     D  G02TGI                18     22  0
008100090213      *
008200111027      *----------------------------------------------------*
008300111027      *  DS x scomposizione numero spedizione
008400111027      *----------------------------------------------------*
008500111027     D DSSPE           DS
008600111027     D  SPEAAS                 1      4  0
008700111027     D  SPELNP                 5      7  0
008800111027     D  SPENRS                 8      9  0
008900111027     D  SPENSP                10     16  0
009000111027      **
009100111027     D Sped_RESO       S              1
009200100701      *----------------------------------------------------*
009300090205      **  Elenco DS di tutti i Segmenti da tradurre
009400100701      *----------------------------------------------------*
009500091019???? D edS00BGM      E DS
009600100316???? D edS00CNI      E DS
009700091019???? D edS00CNT      E DS
009800091019???? D edS00COM      E DS
009900091019???? D edS00CTA      E DS
010000100316???? D edS00DGS      E DS
010100091019???? D edS00DTM      E DS
010200100316???? D edS00EQD      E DS
010300100316???? D edS00EQN      E DS
010400100628???? D edS00FTX      E DS
010500100628???? D  D05                   16    365    DIM(5)
010600091019???? D edS00GID      E DS
010700100316???? D edS00LOC      E DS
010800091019???? D edS00MEA      E DS
010900100628???? D edS00MOA      E DS
011000091019???? D edS00NAD      E DS
011100100316???? D edS00PIA      E DS
011200091019???? D edS00PCI      E DS
011300091019???? D  D30_1                  4    353    DIM(10)
011400091019???? D edS00RFF      E DS
011500100316???? D edS00SEL      E DS
011600091019???? D edS00TDT      E DS
011700091019???? D edS00TOD      E DS
011800091019???? D edS00TSR      E DS
011900091019???? D edS00UNA      E DS
012000091019???? D edS00UNB      E DS
012100091019???? D edS00UNH      E DS
012200091019???? D edS00UNT      E DS
012300091019???? D edS00UNZ      E DS
012400100701      *----------------------------------------------------*
012500090210      **
012600090210     D  X30            S             35    DIM(20)                              generico segnacolli
012700090205      **
012800090209     D Valori_inErr    ds
012900090209     D  SKout_Errori                  1    DIM(50)
013000100630      **
013100090209     D Descr_Errore    ds
013200090209     D  SKout_DesErr                 50    DIM(50)
013300090209      *
013400090205      *----------------------------------------------------*
013500091016     D EoFTivin        s              1
013600091016      *----------------------------------------------------*
013700090205     D Tipo_seg        S              3
013800100720     D Trovato_seg     S              1
013900100716      *
014000100716     d keyUNBCLI       s             35
014100100716     d keyTIPOMSG      s              6
014200100716     d keyVERSION      s              3
014300100716     d keyRELEASE      s              3
014400100716     d keyAGENCY       s              3
014500100716     d keyASSOCIA      s              6
014600100716      *
014700100720     D DsGenerica      s           2048
014800090209     D segmento        s           2048
014900090209     D Errore          s              1
015000100701      *
015100000223     D W0140           S             14  0
015200991129     D WORA            S              6  0
015300100701     D DataGGMMAAA     S              8  0
015400100701     D DATEU           S              8  0
015500100701     D DATA_eur        S               D   DATFMT(*eur)
015600100701      *
015700100630     D sk              S              3  0 INZ
015800100630     D sx              S              3  0 INZ
015900100628     D XX              S              3  0 INZ
016000110328     d Key_Generica35  s             35
016100100630     D NUM_Spediz      s                   LIKE(vabnsp)
016200100708     D UNT_record      s                   LIKE(vnREC)
016300100708     D IniDET_RECord   s                   LIKE(vnREC)
016400100708     D FinDET_RECord   s                   LIKE(vnREC)
016500090213     D Last_RECord     s                   LIKE(vnREC)
016600110105     D UNZ_SEGMENTO    s              1
016700100318      *------------------------------------------
016800100318      **  Definizione Qualificatori
016900100318      *------------------------------------------
017000100319      *
017100100628???? D Documento_Manifest...
017200100628???? D                 s              3    INZ('785')
017300100701???? D Totale_Peso_Lordo...
017400100701???? D                 s              3    INZ('7  ')
017500100701???? D Totale_Nr_Spedizioni...
017600100701???? D                 s              3    INZ('10 ')
017700100701???? D Totale_Nr_Colli...
017800100701???? D                 s              3    INZ('11 ')
017900100701???? D Totale_Peso_Netto...
018000100701???? D                 s              3    INZ('ZCW')
018100100628???? D Riferimento_Spedizione...
018200100628???? D                 s              3    INZ('AGE')
018300100708???? D Riferimento_Numerico...
018400100708???? D                 s              3    INZ('CU ')
018500100318???? D Riferimento_Cliente_particolare...
018600100318???? D                 s              3    INZ('FF ')
018700100701???? D X_Cargo_Manifest...
018800100318???? D                 s              3    INZ('AFB')
018900100701???? D X_Numero_CMR...
019000100318???? D                 s              3    INZ('CMR')
019100100318???? D Telephone_Number...
019200100318???? D                 s              3    INZ('TE ')
019300100318???? D TeleFax_Number...
019400100318???? D                 s              3    INZ('TX ')
019500100628???? D Porto_Assegnato...
019600100628???? D                 s              3    INZ('001')
019700100628???? D Porto_Franco...
019800100628???? D                 s              3    INZ('002')
019900100628???? D Porto_Franco_uncleared...
020000100628???? D                 s              3    INZ('003')
020100100628???? D Porto_Franco_domicile...
020200100628???? D                 s              3    INZ('004')
020300100318???? D Volume...
020400100318???? D                 s              3    INZ('VOL')
020500100705???? D Weight...
020600100705???? D                 s              3    INZ('WT ')
020700100319???? D G_Weight...
020800100319???? D                 s              3    INZ('G  ')
020900100318???? D Gross_Weight...
021000100318???? D                 s              3    INZ('AAG')
021100100706???? D Gross_Weight_E...
021200100706???? D                 s              3    INZ('AAE')
021300110512???? D Gross_Weight_D...
021400110512???? D                 s              3    INZ('AAD')
021500100706???? D N_Weight...
021600100706???? D                 s              3    INZ('N  ')
021700100706???? D Net_Weight...
021800100706???? D                 s              3    INZ('AAF')
021900100318???? D Kilograms...
022000100318???? D                 s              3    INZ('KGM')
022100100319???? D Grams...
022200100319???? D                 s              3    INZ('163')
022300100318???? D Meters...
022400100318???? D                 s              3    INZ('MTR')
022500100318???? D Cubic_Meters...
022600100318???? D                 s              3    INZ('MTQ')
022700100318???? D Shipper_assigned...
022800100318???? D                 s              3    INZ('24 ')
022900100705???? D Data_senza_ora...
023000100628???? D                 s              3    INZ('102')
023100100705???? D Data_con_ora...
023200100628???? D                 s              3    INZ('203')
023300100705???? D Data_con_i_secondi...
023400100705???? D                 s              3    INZ('204')
023500100630???? D Squadratura_Colli...
023600100630???? D                 s              1    INZ('N')
023700120613     D controlla_COD_FTX...
023800120613???? D                 s              3
023900100316      *---------------------------------------------------------------------*
024000100316      **  segmenti Identificativi parti specifiche del messaggio
024100100316      *---------------------------------------------------------------------*
024200100701???? D Segm_Inizio_Messaggio...
024300100701???? D                 s              3    INZ('UNB')
024400100701???? D Segm_Testata_Dettagli...
024500100701???? D                 s              3    INZ('UNH')
024600100701???? D Segm_Inizio_Dettaglio...
024700100316???? D                 s              3    INZ('CNI')
024800100701???? D Segm_Fine_Dettagli...
024900100316???? D                 s              3    INZ('UNT')
025000100701???? D Segm_Fine_messaggio...
025100100316???? D                 s              3    INZ('UNZ')
025200100701???? D seg_imprevisto...
025300100701???? D                 s              1
025400100726???? D Forza_Uscita...
025500100726???? D                 s              1
025600100701      *---------------------------------------------------------------------*
025700100701      *
025800100701???? D NAD_destinatario_in_testata...
025900100701???? D                 s              1
026000100701???? D NAD_destinatario_in_dettaglio...
026100100701???? D                 s              1
026200100701???? D NAD_mittente_in_testata...
026300100701???? D                 s              1
026400100701???? D NAD_mittente_in_Dettaglio...
026500100701???? D                 s              1
026600100701     d CTA_destinatario_in_Dettaglio...
026700100701     d                 s             35A
026800100701     d CTA_destinatario_in_testata...
026900100701     d                 s             35A
027000100701     d COM_telefono_in_Dettaglio...
027100100701     d                 s             35A
027200100701     d COM_telefono_in_testata...
027300100701     d                 s             35A
027400110907     d FTX_ServizioMail_xDestinatario...
027500110907     d                 s             70A
027600131025     d FTX_MAIL_come_Servizio...
027700131025     d                 s              1A
027800131025     d FTX_ServizioSMS_xDestinatario...
027900131025     d                 s             70A
028000131025     d FTX_SMS_come_Servizio...
028100131025     d                 s              1A
028200120614     d FTX_ai_piani    s              1A
028300111121???? D NAD_Lunghezza_Indir_Aggiuntivo...
028400111121???? D                 s              3s 0
028500111121???? D NAD_Lunghezza_Indirizzo...
028600111121???? D                 s              3s 0
028700111121???? D NAD_Lunghezza_ragSociale2...
028800111121???? D                 s              3s 0
028900100701      *
029000090209      *---------------------------------------------------------------------*
029100100701???? D Dati_di_Testata...
029200100701???? D                 s              1    INZ('S')
029300100701???? D Contatore_Dettagli...
029400100701???? D                 s              5s 0 INZ(0)
029500100728???? D Segmento_in_errore...
029600100728???? D                 s              5s 0 INZ(0)
029700100701      *---------------------------------------------------------------------*
029800100628      *
029900100701      *---------------------------------------------------------------------*
030000100630     d UNB_diWork      s                   like(UNB0004)
030100100628     d  UNB_Mittente   s                   like(UNB0004)
030200100706     d BGM_Id_Testata  s             35A   inz(' ')
030300100706     d  BGM_NrFviaggio...
030400100628     d                 s             35A
030500100706     d  BGM_nrCMR      s             35A
030600100701     d  RFF_NrFviaggio...
030700100701     d                 s             35A
030800100701     d  RFF_NrCMR      s             35A
030900100701     d  RFF_RifSpediz  s             35A
031000100701     d UNB_parziale    s              1    inz('N')
031100100813     d UNB_Generico    s             31    inz(' ')
031200100701     d UNB_NrTrasmiss  s             14    inz(' ')
031300100701     d UNZ_NrTrasmiss  s             14    inz(' ')
031400100701     d UNH_Rifer_Msg   s             14    inz(' ')
031500100701     d UNH_VABrma      s                   like(VABrma)
031600100701     d UNH_VABrmn      s                   like(VABrmn)
031700120629     d NAD_SF_Cliente  s             35    inz(' ')
031800100701      *
031900100701      * Tabella partner
032000121105     D PT_key          S             35    DIM(200)
032100121105     D PT_Parz         S             35    DIM(%elem(PT_key))
032200121105     D PT_KSC          S              7    DIM(%elem(PT_key))
032300121105     D PT_uni          S             90    DIM(%elem(PT_key))
032400121105     D PU_uni          S             90    DIM(%elem(PT_key))
032500121105     D PV_uni          S             90    DIM(%elem(PT_key))
032600121105     D PZ_uni          S             90    DIM(%elem(PT_key))
032601160205     D PS_uni          S             90    DIM(%elem(PT_key))
032700121105     D TRUL0SDS      E DS
032800100701      *
032900100701     d PT_KeyXsav      s              3  0 inz(0)
033000100701     D PT_trovata      s              1    inz('N')
033100100701     D PU_key          S             35
033200100701     D PV_key          S             35
033201160205     D PS_key          S             35
033300120629     D PZ_key          S             35
033400100701     D PU_HLD_SpedVAX  S              1
033500100813     D PT_con_piu_Nazioni...
033600100813     d                 s              1
033700120629     D PZ_abilita_NAD_SF_come_RFF_FF...
033800120629     d                 s              1
033900100701      *
033901160209     D  PS_vabFGS      s              3A
034000100701     D  PT_vabLNP      s                   LIKE(vablnp)
034100100701     D  PT_vabNRS      s                   LIKE(vabnrs)
034200100701     D  PT_vabCTM      s                   LIKE(vabctm)
034300100701     D  PT_vabCCM      s                   LIKE(vabccm)
034400100701     D  PU_lunVAT      s              2s 0
034500100702     D  PU_TipoServ    s              2a
034600100702     D  PU_SpecServ    s              2a
034700111027     D  PU_in_RMN_BOLLA_EXPORT_x_RESO...
034800111027     d                 s              1
034900111027     D  era_un_export_RESO_dal_PARTNER...
035000111027     d                 s              1
035100140610      *  qui viene riportato il codice della nostra ex bolla export o del ORM
035200140610      *  mandato dal Partner in un particolare segmento concordato
035300140610     D  Cod_RESO_ORM...
035400140610     d                 s             14
035500140623     D  Salva_REF_ORM...
035600140623     d                 s             14
035700100701      *
035800100701     D Nr_PT           S              3  0 INZ
035900100701     D savXX_PT_key    S              3  0 INZ
036000100701      *
036100100630     d  DTM_DtParten   s              8s 0
036200100630     d  DTM_DtFViag    s              8s 0
036300100630     d  DTM_DtCMR      s              8s 0
036400100630     d  DTM_OraFViag   s              4s 0
036500100630     d  DTM_OraCMR     s              4s 0
036600100702     d  DTM_DtArrivo   s              8s 0
036700100702     d  DTM_OraArrivo  s              4s 0
036800100706     d  DTM_OraParten  s              4s 0
036900100702     d  CNI_Identificativo_Bolla...
037000100702     d                 s             35A
037100101027     d  CNI_seNumerico...
037200101027     d                 s                   LIKE(vabRMN)
037300100630      *
037400100701     d  GID_aTotale    s              1A
037500100705     d  GID_ColliInAdd...
037600100705     d                 s                   like(gid722420)
037700100701      **
037800090209      *  DS x scomposizione segnacollo
037900100630     D DS_SegnBART     DS
038000100630     D  SegnBART_LNP           1      3  0
038100100630     D  SegnBART_LNA           4      6  0
038200100630     D  SegnBART_NRS           7      8  0
038300100630     D  SegnBART_NSC           9     15  0
038400100630     D  SegnBART_ZNC          16     17  0
038500100701      **
038600090209      *---------------                                                      *
038700100701     D VABtic_work     s                   LIKE(vabtic)
038800110623     d inviato_tipo_incasso...
038900110623     d                 s              1
039000100630      *
039100090603     d tes_VABrmo      s                   LIKE(VABrmo)
039200090603     d tes_VABnmo      s                   LIKE(VABnmo)
039300090603     d tes_VABcmo      s                   LIKE(VABcmo)
039400090209     C*
039500090603     d tes_VABrsd      s                   LIKE(VABrsd)
039600090603     d tes_VABrd2      s                   LIKE(VABrd2)
039700090603     d tes_VABind      s                   LIKE(VABind)
039800090603     d tes_VABlod      s                   LIKE(VABlod)
039900090603     d tes_VABnzd      s                   LIKE(VABnzd)
040000090603     C*
040100090603     d tes_Valuta      s                   LIKE(VABvca)
040200090603     C*
040300090209      *---------------------------------------------------------------------*
040400090209      * Schiere
040500090209      *---------------------------------------------------------------------*
040600090209      * Schiera per reperimento nr.seganacollo
040700100630     D segn_BAR        S              7  0 DIM(5000)
040800100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
040900100630      *
041000100630      *
041100090209      * Nr. documento alfanumerico da decodificare
041200090209     D NDA             S              1    DIM(35)
041300100630      *
041400090209      * Nr. documento alfanumerico da già decodificato
041500090209     D NDN             S              1    DIM(15)
041600100630      *
041700090209      * Messaggi di errore
041800090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
041900100630      *
042000090209      * Tabella codici trattamento merce (1B)
042100100701     D Cod_Tb_CTM      S              2    DIM(200)
042200100701     D Des_Tb_CTM      S             67    DIM(200)
042300100630      *
042400090209      * Tabella codici valuta
042500090209     D CCV             S              3    DIM(200)
042600090209     D DCV             S             89    DIM(200)
042700100630      *
042800090209      * Tabella codici nazioni
042900090209     D C15             S              3    DIM(500)
043000090209     D ISO             S              3    DIM(500)
043100090209     D CPF             S              2  0 DIM(500)
043200100630      *
043300090209      * Tabella CL --> codici clienti - tariffa
043400130305     D Cod_Tb_CL       S             35    DIM(999)
043500130305     D Des_Tb_CL       S             90    DIM(999)
043600100630      *
043700090209      * Schiere x caricamento cod.cliente  <>
043800130305     D skCLienti       S              7  0 DIM(999)
043900100630      *
044000100318      * Priorità trasporto
044100100705     D Key_TipServ     S             35    DIM(100)
044200100702     D TipoServizio    S              1    DIM(100)
044300100702      *
044400100702      * Decodifiche servizi speciali
044500100702     D SpecialServ     S             35    DIM(100)
044600100702     D Key_ServSpec    S              3    DIM(100)
044700100702     D UNI_ServSpec    S             90    DIM(100)
044800100630      *
044900090209      * Termini di consegna
045000110328     D K35_TpBolla     S             35    DIM(100)
045100110328     D Cod_TpBolla     S              3    DIM(100)
045200110328     D Decod_Porto     S              1    DIM(100)
045300110328     d Trovata_Tab_TB  S              1
045400100630      *
045500090216      * Tipo pagamento C/Assegno
045600100705     D K35_TpIncas     S             35    DIM(100)
045700100705     D TipoIncasso     S              2    DIM(100)
045800100630      *
045900090209      * Schiere x routine di conversione CAP
046000090209     D CAP5            S              1    DIM(5)
046100090209     D CAP9            S              1    DIM(9)
046200090209     D CAPM            S              1    DIM(9)
046300100630      *
046400090209      * Schiera per memorizzazione FTX ricevuti
046500090209     D QUA             S              3    DIM(100)
046600090209     D FTX             S             70    DIM(100)
046700100630      *
046800090209      * Tabelle capiconto
046900090209      * Schiere x località
047000090209     D LOD             S              1    DIM(35)
047100100630      *
047200060608      **?------------------------------------------------------------------ */
047300090601     C*? Ds Scrittura del VAT "E"
047400060608      **?------------------------------------------------------------------ */
047500060612     D XTOEBCDDS     E DS
047600060620      *
047700060608      **?------------------------------------------------------------------ */
047800100701      *
047900090209      **?------------------------------------------------------------------ */
048000090209     C* Definizione campi di work
048100090209     D kKUT            s                   LIKE(TBLKUT)
048200090209     D kCOD            s                   LIKE(TBLCOD)
048300090209     D kKEY            s                   LIKE(TBLKEY)
048400090209     D kKCC            s                   LIKE(ACOKCC)
048500090209     D kKSC            s                   LIKE(ACOKSC)
048600090209     D kAAA            s                   LIKE(NUFAAA)
048700090209     D kCNU            s                   LIKE(NUFCNU)
048800090209     D kFIL            s                   LIKE(NUFFIL)
048900090209     D kAAS            s                   LIKE(VABAAS)
049000090209     D kLNP            s                   LIKE(VABLNP)
049100090209     D kNRS            s                   LIKE(VABNRS)
049200090209     D kNSP            s                   LIKE(VABNSP)
049300090209     D kCMR            s                   LIKE(VABCMR)
049400090209     D kCCM            s                   LIKE(VABCCM)
049500090209     D kNCD            s                   LIKE(VADNCD)
049600090210     D kCNT            s                   LIKE(VADCNT)
049700090209     D kNMC            s                   LIKE(RDENMC)
049800090209     D kNRP            s                   LIKE(RDENRP)
049900090209     D kLNP1           s                   LIKE(RDELNP)
050000090209      *
050100090209      *  Invio E-mail
050200090209     D Indirizzi       s            253
050300090209     D Oggetto         s             44
050400090209     D Messaggio       s           5000
050500090209      *
050600090209     d   DSmail        ds
050700090209     D Mail_Ind                      44
050800090209     d   IND_char                     1    dim(44)
050900090209      *
051000100630     D Lunghezza_Indirizzo...
051100100630     D                 s              5u 0
051200090209      *
051300100630     D dalCarattere    s              5u 0
051400100630     D xTOTcaratteri   s              5u 0
051500090209      *
051600090209     C*---------------------------------------------------------------*
051700090209     D MsgDSP          S            256                                         Testo generico
051800090209     C*---------------------------------------------------------------*
051900090209     D SndMg01         S             10                                         Message type
052000090209     D                                     INZ('*INFO')
052100090209     D SndMg02         S             10                                         Deluvery mode
052200090209     D                                     INZ('*BREAK')
052300090209     D SndMg03         S            256                                         Message text
052400090209     D SndMg04         S             10I 0                                      Length of text
052500090209     D                                     INZ(%SIZE(SndMg03))
052600090209     D SndMg05         S             10                                         User profile
052700090209     D                                     DIM(299)
052800090209     D SndMg06         S             10I 0                                      Number of user
052900090209     D SndMg07         S             10I 0                                      Message sent indic.
053000090209     D SndMg08         S             10I 0                                      Function requested
053100090209     D SndMg10         S              1                                         Show display
053200090209     D                                     INZ('N')
053300090209     D SndMg11         S             20                                         Qualified MSGQ name
053400090209     D SndMg12         S              4                                         Name type indicator
053500090209     D                                     INZ('*USR')
053600090209      * indice di scihera
053700090209     D Jx              S              5I 0
053800090209      *
053900090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
054000090209      *
054100090209     d bart_it         C                   CONST('@Bartolini.it;')
054200090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
054300060612      * ?================================================================== */
054400060612      * ?   * Campi x decodifica * (INPUT  del Record)
054500100319      * ?================================================================== */
054600090130     D  Dati           s           2048
054700060612      * ?   *--------------------------------------------------------------*
054800060612     D  se_errore      s              1    inz(' ')
054900060612      * ?* ------------------------------------------------------ *
055000060710     D Digits          C                   '0123456789'
055100091019     D*------------------
055200091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
055300091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
055400060612      * ?================================================================== */
055500060612      *   Ciclo principale
055600090205      * ?================================================================== */
055700060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
055800060612     C*
055900060612     C                   if        not %open(tivin00r)
056000060612     C                   open      tivin00r
056100060612     C                   endif
056200060612      **
056300060612      * ?------------------------------------------------------------------ */
056400100708     C*  Controllo dati arrivati da DPD
056500060612      * ?------------------------------------------------------------------ */
056600060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
056700060612      * ?- controllando che la trasmissione sia completa.
056800090205      **
056900060612      * ?              /*---------------------- */
057000090205     c                   goto      nonFare_adesso
057100060612     c                   exsr      check_Trasm
057200090205     c     nonFare_adessotag
057300060612      * ?              /*---------------------- */
057400090205      **
057500060613      **  Errore di trasmissione x tutti i records
057600060613      **   --> file in errore
057700060613     c                   if        se_errore = 'S'
057800060612
057900060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
058000090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
058100090205     c                   exsr      Msg_errato
058200060612      **
058300060612     c                   else
058400060614      **
058500060612      * ?------------------------------------------------------------------ */
058600100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
058700060612      * ?------------------------------------------------------------------ */
058800060612      * ?              /*---------------------- */
058900060612     c                   exsr      Importa_Msg
059000060612      * ?              /*---------------------- */
059100060612
059200060614     c                   end
059300060612      *
059400060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
059500090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
059600090225     c                             esito ='0'
059700060710     C                   eval      esito ='0'
059800090225      *
059900090225     c                   COMMIT
060000110208     C                   endif
060100090224      *
060200090213     c                   if        almeno_un_due ='S'
060300110208     C                   eval      esito ='1'
060400060614     C                   endif
060500100708      * chiude TIVIN
060600100708     C                   if        %open(tivin00r)
060700100708     C                   close     tivin00r
060800100708     C                   endif
060900060614      *
061000060612     c                   SETON                                        LR
061100060612      * ?================================================================== */
061200100708      *? Importa i records della trasmissione
061300060612      * ?------------------------------------------------------------------ */
061400060612     c     Importa_Msg   Begsr
061500060614
061600130506      *   Inizia il conteggio totale delle righe
061700130506     c                   z-add     0             total_segmenti    5 0
061800100726     c                   clear                   Forza_Uscita
061900060612     c     *start        setll     Tivin00r
062000100708
062100100708     c                   EXSR      LEGGE_TIVIN_R
062200100708     c                   dow       EoFTivin <> 'S'
062300060614
062400060614      * solo i record sflaggati da rielaborare
062500110107     c                   IF        vinFLG = *blank
062600110107      *** ++++++
062700090211      *  Sempre Record OK
062800090211      ** Controlli formali sui campi
062900090211     C                   eval      vinFLG = '1'
063000060612     c                   clear                   se_errore
063100110107      *
063200110107      *** >>>>>>
063300110107     c                   IF        vinDTA <> *blank
063400110107      *** >>>>>>
063500060612
063600060612      ** Decodifica record a campi variabili
063700060612      * ?              /*---------------------- */
063800090205     c                   exsr      Decod_Segmento
063900090209      * ?              /*---------------------- */
064000060612
064100060621      *  x errore bloccante nella composizione del VAB/VAT
064200060621     c                   if        err_bloccante ='S'
064300060621     C                   eval      vinFLG = '2'
064400110208     c                   eval      Almeno_Un_Due ='S'
064500090211     c                   eval      esito  = '2'
064600100317???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
064700090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
064800090603     C                             Bolle import in filiale - messaggio -
064900090603     C                             con UNB non presente in tabella PT controlla-
065000090603     C                             re EDI ricevuto su UPLOAD.'
065100100319     c                   if        §PVinv <> 'N'
065200100319     c                   exsr      invio_mail
065300060621     c                   end
065400100319     c                   end
065500110107      *** >>>>>>
065600110107     c                   endIF
065700110107      *** >>>>>>
065800110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
065900110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
066000110107      *         un erroneo messaggio
066100110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
066200060612     c                   update    Tivin000
066300090211
066400090211      *** se non è riconosciuto l'UNB deve uscire direttamente
066500090211     c                   if        se_errore ='S'  and
066600100708     c                             tipo_seg = Segm_Inizio_Messaggio
066700090213     c                   eval      err_bloccante = 'S'
066800090211     c                   exsr      Msg_errato
066900090211     c                   LEAVE
067000090211     c                   endIF
067100110107      *** ++++++
067200110107     c                   endIF
067300060612
067400100708     c                   EXSR      LEGGE_TIVIN_R
067500060612     C                   ENDdo
067600060612      **
067700060612     c                   endSR
067800100709      * ?------------------------------------------------------------------ */
067900100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
068000100709      * ?------------------------------------------------------------------ */
068100100709     C     MSG_Errato    BEGSR
068200100709      *
068300100709      * ?  Scrive su tutti i records il tipo di errore
068400100709     c     *start        setll     tivin00r
068500100709     c
068600100709     c                   EXSR      LEGGE_TIVIN_R
068700100709     c                   dow       EoFTivin <> 'S'
068800100709     c
068900100709     c                   if        vinMSG  = *blank
069000100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
069100100709     C                               >> Controllare i DATI su UPLOAD !!'
069200100709     c                   end
069300100709     C                   eval      vinFLG = '2'
069400100709     c                   eval      Almeno_Un_Due ='S'
069500100709     c                   eval      esito  = '2'
069600100709     c                   eval      se_errore ='S'
069700100709     c                   eval      err_bloccante ='S'
069800100709      *
069900100709     c                   update    tivin000
070000100709     c                   EXSR      LEGGE_TIVIN_R
070100100709     c                   endDO
070200100709      *
070300100709     C                   ENDSR
070400100709      * ?------------------------------------------------------------------ */
070500100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
070600100709      * ?------------------------------------------------------------------ */
070700100709     C     DETta_Errato  BEGSR
070800100709      *
070900100709     c                   EXSR      LEGGE_TIVIN_R
071000100709     c                   dow       EoFTivin <> 'S'
071100100709      *
071200100709      * Se ha letto il successivo record
071300100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
071400100709     c                   if        VNREC = UNT_record  or
071500100709     c                             VNREC = finDET_RECord
071600100709     c                   leave
071700100709     c                   end
071800100709      *
071900100709     c                   exsr      Error_DET
072000100709      *
072100100709     c                   if        vinMSG  = *blank
072200100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
072300100709     C                             >> Controllare i DATI di ogni Segmento <<'
072400100709     c                   end
072500100709      *
072600100709     c                   update    tivin000
072700100709     c                   EXSR      LEGGE_TIVIN_R
072800100709     c                   endDO
072900100709      * ------
073000100709???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
073100100709     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
073200100709???? C                             Bolle IFCSUM non totalmente tradotto. -
073300100709     C                             Controllare UPLOAD del cliente:'
073400100709     C                   EVAL      Messaggio = %trim(Messaggio) +
073500100709     C                                         %editc(§PTksc:'X')
073600100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
073700100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
073800100709     c                   if        §PVinv <> 'N'
073900100709     c                   exsr      invio_mail
074000100709     c                   end
074100100709      *
074200100709      * ripulisce per un nuovo giro
074300100709     C                   clear                   Err_in_detta
074400100709      *
074500100709     C                   ENDSR
074600100709      * ?------------------------------------------------------------------ */
074700100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
074800100709      * ?------------------------------------------------------------------ */
074900100709     C     LEGGE_TIVIN_R BEGSR
075000100709      *
075100100709     c                   clear                   EoFTivin
075200100709      *  Lettura sequenziale
075300100709     c                   read      Tivin00r
075400100709
075500100709     c                   if        %Eof(tivin00r)
075600100709     c                   move      'S'           EoFTivin
075700100709     c                   else
075800100709      *
075900100709      * ?  imposta il tipo di segmento:
076000100709     c                   clear                   dati
076100100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
076200100728     c                   eval      segmento = %trimL(dati)
076300100728     c                   eval      tipo_seg = %subst(segmento:1:3)
076400100709      **
076500100709     c                   end
076600100709      *
076700100709     C                   ENDSR
076800100120      * ?------------------------------------------------------------------ */
076900100120      *?    Decodifica il segmento ed importa i campi in formato DS
077000100120      * ?------------------------------------------------------------------ */
077100100120     c     Decod_SegmentoBegsr
077200100120      **
077300100706      *** ------------------------------------
077400100706????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT"
077500100706      *** ------------------------------------
077600100706      * OGNI DETTAGLIO inizia dal segmento CNI e per ultimo il msg è chiuso da UNT.
077700100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
077800100706      **
077900100706      **  Scrive alla fine di ogni giro di dettaglio:
078000100706     c                   if           tipo_seg = Segm_Fine_Dettagli
078100100706     c                                   or
078200100706     c                                tipo_seg = Segm_Inizio_Dettaglio  and
078300100706     c                                Contatore_Dettagli > 0
078400100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
078500100708     c                   eval      FinDET_RECord = vnrec
078600100708      **?              /*---------------------- */
078700100708     c                   exsr      Scrive_Dettag
078800100708      **?              /*---------------------- */
078900100813      *
079000100813      *   Ogni nuovo Dettaglio,
079100100813      *    per quei Partner che gestiscono più LInee
079200100813      *    Mediante il Mittente del dettaglio
079300100813     c                   if        UNB_parziale ='S' and
079400100813     c                              PT_con_piu_Nazioni = 'S' and
079500100813     C                                 Naz_salvata = Naz_mittente
079600100813      * ?ReImposta i dati da tabella PT
079700100813      * ?  se fossero stati cambiati dal precedente dettaglio:
079800100813     C                   z-add     savXX_PT_key  Nr_PT
079900100813     c                   exsr      Se_Trovata_PT
080000100813     c                   endIF
080100100813      *
080200100706     c                   endIF
080300100708      *
080400100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
080500100708      **?              /*---------------------- */
080600100708     c                   exsr      Pulizia_Campi
080700100708      **?              /*---------------------- */
080800100813      *
080900100706      *** ------------------------------------
081000100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
081100100120      * ?  Anche se non utilizzati.
081200100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
081300100317     c                   clear                   seg_imprevisto
081400100720     c                   clear                   trovato_seg
081500100319      *
081600100319      *  Contatore delle righe ossia dei segmenti del messaggio
081700100319     c                   add       1             Conta_segmenti
081800130506     c                   add       1             total_segmenti
081900100708      *
082000100716     C                   clear                   keyUNBCLI
082100100716     C                   clear                   keyTIPOMSG
082200100716     C                   clear                   keyVERSION
082300100716     C                   clear                   keyRELEASE
082400100716     C                   clear                   keyAGENCY
082500100716     C                   clear                   keyASSOCIA
082600110530      **--------
082700110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
082800110530      **--------
082900110530     c                   movel     UNB_Mittente  keyUNBCLI
083000100720      **--------
083100100720      *  Ritorna la DS GENERICA oppure l'errore
083200100720      **--------
083300100720      *  Chiamata generica x decodificare il singolo segmento letto
083400100720     c                   call      'TRTCT00R1'
083500100720      * input
083600100720     c                   parm                    tipo_seg
083700100720     c                   parm                    segmento
083800100720     C                   parm                    keyUNBCLI
083900100720     C                   parm                    keyTIPOMSG
084000100720     C                   parm                    keyVERSION
084100100720     C                   parm                    keyRELEASE
084200100720     C                   parm                    keyAGENCY
084300100720     C                   parm                    keyASSOCIA
084400100720      *output
084500100720     c                   parm                    Errore
084600100720     c                   parm                    DsGenerica
084700100720     c                   parm                    Valori_inErr
084800100720     c                   parm                    Descr_Errore
084900100720     c                   parm                    trovato_seg
085000120424      *
085100120424      *>>>--->>>                                                    <<<---<<<
085200120424      *   Se ci sono dei Segmenti Particolari da NON considerare
085300120424     c                   IF        trovato_seg ='N' and unz_segmento <>'S'
085400120424      *
085500120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
085600120424     C                   movel(p)  'SGM'         etbSGM
085700120424     C                   movel(p)  Tipo_Seg      etbVALSGM
085800120424     c                   exsr      In_TAB_EDSTBL
085900120424      *
086000120424     C*  Quindi sono dichiarati nel file delle eccezioni
086100120424     C                   If        Quale_campo ='TIPSGM' and
086200120424     c                                                   trovata_EDSTBL = 'S'
086300120424     c                   eval      trovato_seg ='S'
086400120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
086500120424      * errori sul TIVIN.
086600120424     c                   endIf
086700120424     c                   end
086800120424      *>>>--->>>                                                    <<<---<<<
086900100708      *** ------------------------------------
087000100708      *   Decodifica il singolo segmento LETTO
087100100708      *** ------------------------------------
087200100120     c                   select
087300100120      *
087400110110     c                   when      trovato_seg ='N' and unz_segmento <>'S'
087500100720     c                   move      'S'           seg_imprevisto
087600100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
087700100720     c                   exsr      Error_DET
087800100720     c                   goto      end_Decod
087900100720      **--------
088000100120     c                   when      tipo_seg = 'UNA'
088100100720     c                   movel(p)  DsGenerica    EDS00UNA
088200100120      *
088300100120     c                   when      tipo_seg = 'UNB'
088400100720     c                   movel(p)  DsGenerica    EDS00UNB
088500100120     C                   EXSR      DATI_UNB
088600100120      * --------
088700100120     c                   when      tipo_seg = 'UNH'
088800100720     c                   movel(p)  DsGenerica    EDS00UNH
088900100120     C                   EXSR      DATI_UNH
089000100120      * --------
089100100120     c                   when      tipo_seg = 'BGM'
089200100720     c                   movel(p)  DsGenerica    EDS00BGM
089300100316     C                   EXSR      DATI_BGM
089400100120      **--------
089500100120     c                   when      tipo_seg = 'DTM'
089600100720     c                   movel(p)  DsGenerica    EDS00DTM
089700100120     C                   EXSR      DATI_DTM
089800100120      * --------
089900100120     c                   when      tipo_seg = 'TSR'
090000100720     c                   movel(p)  DsGenerica    EDS00TSR
090100100316     C                   EXSR      DATI_TSR
090200100120      **--------
090300100120     c                   when      tipo_seg = 'CNT'
090400100720     c                   movel(p)  DsGenerica    EDS00CNT
090500100316     C                   EXSR      DATI_CNT
090600100120      **--------
090700100120     c                   when      tipo_seg = 'TOD'
090800100720     c                   movel(p)  DsGenerica    EDS00TOD
090900100120     C                   EXSR      DATI_TOD
091000100120      **--------
091100100120     c                   when      tipo_seg = 'RFF'
091200100720     c                   movel(p)  DsGenerica    EDS00RFF
091300100120     C                   EXSR      DATI_RFF
091400100316      **--------
091500100316     c                   when      tipo_seg = 'SEL'
091600100720     c                   movel(p)  DsGenerica    EDS00SEL
091700100316     C                   EXSR      DATI_SEL
091800100316      **--------
091900100316     c                   when      tipo_seg = 'EQD'
092000100720     c                   movel(p)  DsGenerica    EDS00EQD
092100100316     C                   EXSR      DATI_EQD
092200100316      **--------
092300100316     c                   when      tipo_seg = 'EQN'
092400100720     c                   movel(p)  DsGenerica    EDS00EQN
092500100316     C                   EXSR      DATI_EQN
092600100316      **--------
092700100316     c                   when      tipo_seg = 'PIA'
092800100720     c                   movel(p)  DsGenerica    EDS00PIA
092900100316     C                   EXSR      DATI_PIA
093000100316      **--------
093100100316     c                   when      tipo_seg = 'LOC'
093200100720     c                   movel(p)  DsGenerica    EDS00LOC
093300100316     C                   EXSR      DATI_LOC
093400100316      **--------
093500100316     c                   when      tipo_seg = 'DGS'
093600100720     c                   movel(p)  DsGenerica    EDS00DGS
093700100316     C                   EXSR      DATI_DGS
093800100316      **--------
093900100120     c                   when      tipo_seg = 'TDT'
094000100720     c                   movel(p)  DsGenerica    EDS00TDT
094100100316     C                   EXSR      DATI_TDT
094200100120      **--------
094300100120     c                   when      tipo_seg = 'MOA'
094400100720     c                   movel(p)  DsGenerica    EDS00MOA
094500100120     C                   EXSR      DATI_MOA
094600100120      **--------
094700100120     c                   when      tipo_seg = 'FTX'
094800100720     c                   movel(p)  DsGenerica    EDS00FTX
094900100120     C                   EXSR      DATI_FTX
095000100120      **--------
095100100120     c                   when      tipo_seg = 'NAD'
095200100720     c                   movel(p)  DsGenerica    EDS00NAD
095300100120     C                   EXSR      DATI_NAD
095400100316      **--------
095500100316     c                   when      tipo_seg = 'CNI'
095600100720     c                   movel(p)  DsGenerica    EDS00CNI
095700100316     C                   EXSR      DATI_CNI
095800100120      **--------
095900100120     c                   when      tipo_seg = 'CTA'
096000100720     c                   movel(p)  DsGenerica    EDS00CTA
096100100120     C                   EXSR      DATI_CTA
096200100120      **--------
096300100120     c                   when      tipo_seg = 'COM'
096400100720     c                   movel(p)  DsGenerica    EDS00COM
096500100120     C                   EXSR      DATI_COM
096600100120      **--------
096700100120     c                   when      tipo_seg = 'GID'
096800100720     c                   movel(p)  DsGenerica    EDS00GID
096900100120     C                   EXSR      DATI_GID
097000100120      **--------
097100100120     c                   when      tipo_seg = 'MEA'
097200100720     c                   movel(p)  DsGenerica    EDS00MEA
097300100120     C                   EXSR      DATI_MEA
097400100120      **--------
097500100120     c                   when      tipo_seg = 'PCI'
097600100720     c                   movel(p)  DsGenerica    EDS00PCI
097700100120     C                   EXSR      DATI_PCI
097800100120      **--------
097900100120     c                   when      tipo_seg = 'UNT'
098000100720     c                   movel(p)  DsGenerica    EDS00UNT
098100100120     C                   EXSR      DATI_UNT
098200100120      **--------
098300100120     c                   when      tipo_seg = 'UNZ'
098400100720     c                   movel(p)  DsGenerica    EDS00UNZ
098500100120     C                   EXSR      DATI_UNZ
098600110105     c                   move      'S'           unz_segmento
098700110105      **--------
098800110105     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
098900100120      **--------
099000100120     c                   OTHER
099100120424      **--------
099200120424     c                   if            unz_segmento <>'S'
099300120424     c                                           and trovata_EDSTBL <>'S'
099400100120     c                   move      'S'           seg_imprevisto
099500100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
099600100120     c                   exsr      Error_DET
099700100120     c                   goto      end_Decod
099800110110     c                   end
099900100120      **--------
100000100120     c                   endSL
100100100120      **
100200100120     c     end_Decod     endSR
100300100701      * ?================================================================== */
100400100319     C*? Azzera_MSG   - Azzera dati messaggio
100500100701      * ?================================================================== */
100600100708     C     Scrive_Dettag BEGSR
100700100701      **
100800100708      *  Deve flaggare il dettaglio con dei problemi
100900100708      * Imposta le righe in errore sullo specifico dettaglio
101000100708     C                   if        Err_in_detta = 'S'
101100100708      * prima di rieseguire il ciclo
101200100708      *  rilascia il record
101300100708     c                   update    tivin000
101400100708      * ?  Scrive su tutti i records il tipo di errore
101500100708     c     IniDET_RECord setll     tivin00r
101600100708      **?              /*---------------------- */
101700100708     c                   exsr      DETta_Errato
101800100708      **?              /*---------------------- */
101900100708     c                   end
102000100708      *
102100100708      **  Se presente un errore nel record emette una segnalazione msg
102200100708???? c                   if        se_errore <> 'S'
102300100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
102400100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
102500100708     c                              NAD_Destinatario_in_Testata    ='S'
102600100708      * ?              /*---------------------- */
102700100708     c                   exsr      Scrive_VAB
102800100708      * ?              /*---------------------- */
102900100708     c                   else
103000100708     c                   eval      errori_in_Wrt = 'S'
103100100708     c                   end
103200100708      *
103300100708      *  Problemi nella decodifica dei campi VAB/VAT
103400100708     c                   if        errori_in_Wrt = 'S'
103500100708     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
103600100708     c                   exsr      Error_DET
103700100708     c                   ROLBK
103800100708     c                   else
103900100708     c                   COMMIT
104000100708     c                   end
104100100708      *
104200100708     c                   end
104300100708      **
104400100708     c     end_WRidett   endSR
104500100708      * ?================================================================== */
104600100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
104700100708      * ?================================================================== */
104800100708     C     Pulizia_Campi BEGSR
104900100708      **
105000100708      * inzio messaggio azzera tutte le variabili generali del messaggio
105100100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
105200100708      * ?                         --------------
105300100708     C                   EXSR      Azzera_MSG
105400100708      * ?                         --------------
105500100708      *
105600100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
105700100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
105800100708      * ?                         --------------
105900100708     c                   exsr      Testata_sped
106000100708      * ?                         --------------
106100100708      *
106200100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
106300100708      *  Prepara una nuova spedizione
106400100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
106500100708      * ?                         --------------
106600100708     c                   exsr      Nuova_spediz
106700100708      * ?                         --------------
106800100708     c                   end
106900100708      **
107000100708     c     end_clear     endSR
107100100708      * ?================================================================== */
107200100708     C*? Azzera_MSG   - Azzera dati messaggio
107300100708      * ?================================================================== */
107400100708     C     Azzera_MSG    BEGSR
107500100708      **
107600100701     C*  Inizializza variabili
107700100701     C                   Z-ADD     0             skCLienti
107800100701     C                   Z-ADD     0             sk
107900100701     C*
108000100701      *  INIZIALIZZA  dati fondamentali messaggio
108100100701     C                   clear                   EDIDSPT
108200100701     C                   clear                   EDIDSPU
108300100701     C                   clear                   EDIDSPV
108400120629     C                   clear                   EDIDSPZ
108401160205     C                   clear                   EDIDSPS
108500100701     C*
108600100701     C                   move      'N'           PT_trovata
108700100813     C                   clear                   savXX_PT_key
108701160205     C                   clear                   PS_vabFGS
108800100701     C                   clear                   PT_vabLNP
108900100701     C                   clear                   PT_vabNRS
109000100701     C                   clear                   PT_vabCTM
109100100701     C                   clear                   PT_vabCCM
109200100701     C                   clear                   PU_HLD_SpedVAX
109300111027     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO     = *blank
109400100813     C                   eval        PT_con_piu_Nazioni = *blank
109500100813     c                   move      *blank        Naz_salvata       3
109600090211     C*
109700100701     c                   clear                   UNB_Generico
109800100701     c                   clear                   UNB_NrTrasmiss
109900100701     c                   clear                   UNZ_NrTrasmiss
110000100708      *
110100120629     c                   clear                   NAD_SF_Cliente
110200100630      *
110300100630      * per controllare se almeno un record è stato importato sul VAB
110400100630     c                   clear                   Almeno_Uno        1
110500100630     c                   clear                   Almeno_Un_Due     1
110600100708     c                   clear                   IniDET_RECord
110700100708     c                   clear                   FinDET_RECord
110800100630     c                   eval      esito  = '0'
110900100728     c                   eval      Segmento_in_errore = 0
111000100317     C*
111100090211     C                   ENDSR
111200100701      * ?================================================================== */
111300100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
111400100701      * ?================================================================== */
111500100701     C     Testata_sped  BEGSR
111600100701      *
111700100701      *  imposta il Flag per identificare la testata
111800100701     c                   eval      Dati_di_Testata  ='S'
111900100701     C*
112000100701      *   Contatore dei dettagli
112100100701     c                   eval      Contatore_Dettagli = 0
112200100701      **
112300100701      *   Codici identificativi della Testata del messaggio
112400100701     c                   clear                   UNH_Rifer_Msg
112500100706     C* Pulisco dati di work x foglio viaggio
112600100706     C                   clear                   BGM_nrCMR                      * WNRCMR
112700100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
112800100701     c                   clear                   BGM_Id_Testata
112900100701      **
113000100701     C*  Riferimento Foglio Viaggio o CMR
113100100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
113200100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
113300100701      **   Date generali
113400100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
113500100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
113600100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
113700100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
113800100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
113900100702     C                   Z-ADD     0             DTM_DtArrivo                   *
114000100702     C                   Z-ADD     0             DTM_OraArrivo                  *
114100100701      *
114200100701      * pulisce gli RFF+FF di testata
114300100706     C                   clear                   Tes_RFF_FF_KSC
114400100706     C                   clear                   Tes_RFF_FF_TAR
114500100706     C                   clear                   Tes_RFF_FF_LNP
114501160209     C                   clear                   Tes_RFF_FF_FGS
114600100706     C                   clear                   Tes_RFF_FF_NRS
114700100706     C                   clear                   Tes_RFF_FF_CTM
114800100706     C                   clear                   Tes_RFF_FF_BS1
114900100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
115000100701     C*
115100100701     c                   clear                   tes_VABrmo
115200100701     c                   clear                   tes_VABnmo
115300100701     c                   clear                   tes_VABcmo
115400100701     C*
115500100701     c                   clear                   tes_VABrsd
115600100701     c                   clear                   tes_VABrd2
115700100701     c                   clear                   tes_VABind
115800100701     c                   clear                   tes_VABlod
115900100701     c                   clear                   tes_VABnzd
116000100701     C*
116100100701     c                   clear                   tes_Valuta
116200100702     C*
116300100702     c                   clear                   Tot_Peso_msg     15 2
116400100702     c                   clear                   Tot_Sped_msg     15 2
116500100702     c                   clear                   Tot_Colli_msg    15 2
116600100702     c                   clear                   Tot_PNetto_msg   15 2
116700100701     C*
116800100701     ** Inizializza campo di work
116900100701     c                   eval      GID_aTotale = 'N'
117000100701     C*
117100100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
117200100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
117300100701     c                   eval      NAD_destinatario_in_testata = *blank
117400100701     c                   eval      NAD_mittente_in_testata = *blank
117500100701     c                   eval      CTA_destinatario_in_testata = *blank
117600100701     c                   eval      COM_telefono_in_testata = *blank
117700100701     C*
117800100701      *   Inizia il conteggio delle righe
117900100706     c                   z-add     0             Conta_segmenti    5 0
118000100701     C*
118100100701     C                   ENDSR
118200100701      * ?================================================================== */
118300100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
118400100701      * ?================================================================== */
118500100701     C     Nuova_spediz  BEGSR
118600100701      **
118700100701     C* Da qui inzia il dettaglio perciò lo memorizzo
118800100701      *  imposta il Flag per identificare la testata
118900100701     c                   eval      Dati_di_Testata  ='N'
119000100708      *
119100100708     c                   z-add     vnrec         IniDET_RECord
119200100813      **
119300100813      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
119400100813      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
119500100813      *
119600100813     c                   if        UNB_parziale ='S' and
119700100813     c                              PT_con_piu_Nazioni = 'S'
119800100813      * ?Cerca dalla riga di questo dettaglio in avanti
119900100813      * ?           *------------------------------*
120000100813     c                   exsr      cerca_Mittente
120100100813     c                   exsr      esito_tab_PT
120200100813      * ?           *------------------------------*
120300100813     c                   endIF
120400100813      **
120500100701      *
120600100701      *  Pulizia dei records
120700100701     C                   CLEAR                   EDiVAB00
120800100701     C                   CLEAR                   EDiVAT00
120900100701     C                   CLEAR                   EDiVAD00
121000100701      **
121100100706     C                   z-add     PT_vabLNP     VABLNP
121200100706     C                   z-add     PT_vabNRS     VABNRS
121300100701     C                   MOVEL     PT_vabCTM     VABCTM
121400100701     C                   z-add     PT_vabCCM     VABCCM
121500100701     C*
121600110707     C                   MOVEL(p)  UNH_vabRMA    VABrma
121700100701     C                   z-add     UNH_vabRMN    VABrmn
121800100701      *
121900100701      *  Dettaglio dei segnacolli Bartolini/EEX
122000100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
122100100701     c                   eval      GID_aTotale = 'N'
122200100701      *
122300100701     C                   clear                   Conta_Segn        5 0
122400100701     C                   clear                   totale_PCI        5 0
122500100701      *
122600100701     C                   MOVEA     *HIVAL        segn_BAR
122700100701     C                   MOVEA     *blank        segn_EEX
122800100701      *
122900100701     C*  Azzero codice cliente - tariffa
123000100701     C                   Z-ADD     0             ToT_Colli        16 0
123100100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
123200100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
123300100701      *
123400100701     C*  Azzero codice cliente - tariffa
123500100701     C                   clear                   Det_RFF_FF_ksc
123600100701     C                   clear                   Det_RFF_FF_tar
123700100701     C                   clear                   Det_RFF_FF_lnp
123701160209     C                   clear                   Det_RFF_FF_fgs
123800100701     C                   clear                   Det_RFF_FF_nrs
123900100701     C                   clear                   Det_RFF_FF_ctm
124000100701     C                   clear                   Det_RFF_FF_bs1
124100100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
124200110406     C                   clear                   Det_RFF_FF_tic
124300100701      *
124400100701     C                   clear                   Note_1           35
124500100701     C                   clear                   Note_2           35
124600110907     C                   clear                   Note_1T          35
124700110907     C                   clear                   Note_2T          35
124800131025     C                   clear                   Note_1sms        35
124900131025     C                   clear                   Note_2sms        35
125000100701     C*  Campi di work
125100100701     c                   clear                   VABtic_work
125200100701     c                   clear                   wri_vabtic        1
125300110623     c                   eval      inviato_tipo_incasso ='N'
125400100701      *
125500100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
125600100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
125700100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
125800100701     c                   eval      COM_telefono_in_Dettaglio = *blank
125900100702     c                   eval      CNI_Identificativo_Bolla = *blank
126000110907     c                   eval      FTX_ServizioMail_xDestinatario = *blank
126100131025     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
126200131025     c                   eval      FTX_MAIL_come_Servizio = *blank
126300131025     c                   eval      FTX_SMS_come_Servizio = *blank
126400120613     c                   eval      FTX_ai_piani = *blank
126500101027     c                   clear                   CNI_seNumerico
126600101027     C                   clear                   RFF_RifSpediz
126700100701      *
126800100701     C                   clear                   Rifer_Alfanum    35
126900100701     C                   clear                   Rifer_Cliente    35
127000100701     C                   clear                   Rifer_Numeric    35
127100100701     C                   clear                   Tipo_Rifer        3
127200100701      *
127300100701      * Un errore nel dettaglio
127400100701     C                   clear                   Err_in_detta      1
127500100701     c                   clear                   errori_in_Wrt     1
127600100701      *
127700100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
127800100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
127900140610     C*
128000140610     C*  Resetta l'informazione di spedizione di RESO
128100140610     c                   eval      era_un_export_RESO_dal_PARTNER ='N'
128200140610     c                   eval      Sped_RESO = 'N'
128300140610     c                   eval      Cod_RESO_ORM = *blank
128400140623     c                   eval      Salva_REF_ORM = *blank
128500100701     C*
128600100701     c                   endSR
128700100813     C*----------------------------------------------------------------
128800100813      *? cerca Nazione del primo Mittente nella sequenza dettagli
128900100813     C*----------------------------------------------------------------
129000100813     C     cerca_MittenteBEGSR
129100100813      *
129200100813     c                   z-add     vnrec         rec_posiz         9 0
129300100813     c                   move      *blank        Naz_mittente      3
129400100813     c                   movel(p)  UNB_Generico  PT_Parziale      35
129500100813     c                   move      'N'           OK_Nazione        1
129600100813      *
129700100813      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
129800100813     c                   call      'TRTCT93R1'
129900100813     c                   parm                    rec_posiz
130000100813     c                   parm                    PT_Parziale
130100100813     c                   parm                    Naz_mittente
130200100813     c                   parm                    Ok_Nazione
130300100813     c                   parm                    User_msg
130400100813     C                   parm                    keyUNBCLI
130500100813     C                   parm                    keyTIPOMSG
130600100813     C                   parm                    keyVERSION
130700100813     C                   parm                    keyRELEASE
130800100813     C                   parm                    keyAGENCY
130900100813     C                   parm                    keyASSOCIA
131000100813      *
131100100813      *  Re-inizializza l'indicatore di trovato
131200100813     c                   setoff                                       32
131300100813      *
131400100813      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
131500100813      *   da decodificare come UNB.
131600100813     c                   if        Ok_Nazione = 'S'
131700100813     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
131800100813     C                   eval      Nr_PT = 1
131900100813     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
132000100813     c                   Endif
132100100813      *
132200100813     c                   endSR
132300100813     C*----------------------------------------------------------------
132400100813      *? esito ricerca tabella PT decodificata oppure NO
132500100813     C*----------------------------------------------------------------
132600100813     C     esito_tab_PT  BEGSR
132700100813      *
132800100813     C                   move      'N'           PT_trovata
132900100813     C   32              move      'S'           PT_trovata
133000100813      *
133100100813      * ******  Errore generale sul Messaggio
133200100813      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
133300100813      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
133400100813     C                   If        PT_trovata = 'N'
133500100813      * Msg di allerta Traduzione
133600100813     C                   EVAL      Indirizzi = CED_Bart
133700100813???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
133800100813     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
133900100813     C                             IFCSUM D96A import in filiale - messaggio -
134000100813     C                             con UNB non presente in tabella PT controlla-
134100100813     C                             re EDI ricevuto su UPLOAD.'
134200100813      *
134300100813     c                   exsr      invio_mail
134400100813     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
134500100813     C                   eval      vinFLG = '2'
134600110208     c                   eval      Almeno_Un_Due ='S'
134700100813     C                   eval      se_errore   = 'S'
134800100813      *
134900100813      *             *------------------------------*
135000100813     C                   eLSe
135100100813      *             *------------------------------*
135200100813      *
135300100813     c                   exsr      Se_Trovata_PT
135400100813      *
135500100813     c                   Endif
135600100813      *
135700100813     c                   endSR
135800100813     C*----------------------------------------------------------------
135900100813      *? DECODIFICA DATI UNB
136000100813     C*----------------------------------------------------------------
136100100813     C     Se_Trovata_PT BEGSR
136200100813      *
136300100813      * Se tutto OK su tab.PT:
136400100813      *
136500100813     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
136600100813     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
136700100813     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
136800120629     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
136801160205     C                   MOVEL     PS_uni(Nr_PT) EDIDSPS
136900100813      *
137000100813      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
137100100813      *  ricevuti:
137200100813     c                   movel     §PVema        mail_ind         44
137300100813      *
137400100813      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
137500100813     c                   if        mail_ind <> *blank
137600100813     c                   exsr      imp_ADDR_mail
137700100813     c                   end
137800100813      *
137900100813      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
138000100813      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
138100100813      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
138200100813     C                   if        §PULUN = *blank
138300100813     C                   eval      §PULUN = '00'
138400100813     c                   end
138500100813      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
138600100813     C                   move      §puLUN        PU_lunVAT
138700100813     C                   move      §puTS         PU_TipoServ
138800100813     C                   move      §puSS         PU_SpecServ
138900111027     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO = §puRES
139000120629     C                   eval       PZ_abilita_NAD_SF_come_RFF_FF = §pzNADsf
139100100813      *
139200100813     C                   Z-ADD     §PTKSC        PT_vabCCM
139300100813     C                   z-add     §PTLNP        PT_vabLNP
139400100813     C                   z-add     §PTNRS        PT_vabNRS
139500100813     C                   MOVEL     §PTCTM        PT_vabCTM
139501160205      *
139502160209     C                   move      §PTLNP        PS_vabFGS
139503160205     c                   if         §PSfgs <> *blank and
139504160205     c                              §PSfgs <> *zeros
139505160209     C                   move      §PSfgs        PS_vabFGS
139506160205     c                   end
139600100813      *
139700100813      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
139800100813      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
139900100813      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
140000100813      *  i records trasmessi).
140100100813     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
140200100813      *
140300100813      *  In base al cod.trattamento merce reperisco tipo tratt.
140400100813     C                   CLEAR                   DS1B
140500100813     C                   Z-ADD     1             Y                 3 0
140600100813     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
140700100813     C     *IN32         IFEQ      '1'
140800100813     C                   MOVEL     Des_Tb_CTM(Y) DS1B
140900100813     C                   MOVEL     DS1B          sav_DS1B
141000100813     C                   END
141100100813      *
141200100813      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
141300100813     C                   clear                   ACORAG
141400100813     C                   clear                   INDCAE
141500100813     C                   clear                   INDCAP
141600100813     C                   clear                   INDSTA
141700100813      * Decodifico partner
141800100813     C                   Z-ADD     1             KKUT
141900100813     C                   Z-ADD     KCI           KKCC
142000100813     C                   MOVE      §PTKSC        KKSC
142100100813     C     KACO          CHAIN     CNACO00F                           31
142200100813     C     KIND          CHAIN     CNIND00F                           31
142300100813      *
142400100813      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
142500100813      *   questa schiera è utilizzata anche per i clienti.
142600100813     C     §ptKSC        LOOKUP    skCLienti                              39
142700100813     c                   If        *in39 = *off
142800100813     C                   add       1             sk
142900100813     C                   z-add     §ptKSC        skCLienti(sk)
143000100813     C                   Endif
143100100813      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
143200100813      *
143300100813     c                   endSR
143400090210     C*----------------------------------------------------------------
143500090210      *? DECODIFICA DATI UNB
143600090210     C*----------------------------------------------------------------
143700090210     C     DATI_UNB      BEGSR
143800090210      *
143900100701      * identificativo Messaggio
144000100701     C                   eval      UNB_NrTrasmiss = unb0020
144100100701      *
144200100813     C                   MOVEL     unb0017       Anno2             2 0
144300100813     C                   z-add     2000          Anno4             4 0
144400100813     C                   add       Anno2         Anno4
144500100813     C                   MOVE      unb0017       WGG               2 0
144600100813     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
144700100813     C                   MOVE      Anno2         Data_messag
144800100813     C                   MOVEL     WGG           Data_messag
144900100813     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
145000100813     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
145100100813     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
145200100813     c****
145300100813     c                   clear                   Mittente         35
145400100813     c                   clear                   Mitt_Qualifier    4
145500100813     c                   clear                   Id_Messaggio     14
145600100813     C                   eval      Mittente       = unb0004
145700100813     C                   eval      Mitt_qualifier = unb000720
145800100813     C                   eval      Id_Messaggio   = UNB0022
145900100813      *
146000090209      ** ---------------------------------------------------------------
146100090209      * quindi 1° cosa:
146200090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
146300090209      *     senza questo il messaggio NON è trascodificabile
146400100630     C                   MOVEL     unb0004       UNB_diWork
146500100630     C                   MOVE      unb000720     UNB_diWork
146600100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
146700100630     C                   Z-ADD     1             Nr_PT
146800100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
146900090209      *
147000100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
147100100317      *  la Nazione:
147200100317     C                   If        *IN32 = *off
147300100630     C                   eval      Nr_PT = 1
147400100630     c                   clear                   UNB_diWork
147500100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
147600100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
147700100317      *
147800100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
147900100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
148000100317      *  per cercare di individuare con certezza il codice della tabella PT in base
148100100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
148200100317     C                   If        *IN32 = *on
148300100317      *
148400100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
148500100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
148600100317      * ?  su Partner che hanno + linee con + nazioni.
148700100813      *  se è considerato Parziale
148800100813      *   Salva il codice generico
148900100630     c                   eval      UNB_parziale = 'S'
149000100813     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
149100100813      *             *------------------------------*
149200100813     c                   exsr      cerca_Mittente
149300100813      *             *------------------------------*
149400100813     C                   eval          Naz_salvata = Naz_mittente
149500100813     c                   Endif
149600100813     c                   Endif
149700100813      *
149800100813      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
149900100813      *             *------------------------------*
150000100813     c                   exsr      esito_tab_PT
150100100813      *             *------------------------------*
150200100813      **
150300100813      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
150400100813     C                   If        PT_trovata = 'S'
150500100813      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
150600100813      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
150700100813      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
150800100813      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
150900100813      * ?- Salva l'indice delle schiere per riaggancarla in seguito        ------ */
151000100813     C                   z-add     Nr_PT         savXX_PT_key
151100100813      * ?- Se ha più LINEE da gestire                                      ------ */
151200100813     C                   eval      PT_con_piu_Nazioni = §puPLN
151300100813     c                   end
151400100813      **
151500090209      **
151600090210     c     end_UNB       endSR
151700090227      * ?================================================================== */
151800090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
151900090227      * ?================================================================== */
152000090227     C     DATI_UNH      BEGSR
152100100316      *
152200100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
152300100701      *
152400090211      ** Riferimento del cliente
152500100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
152600100701      * ?                                       ------------
152700110707     C                   MOVEL(p)  UNH_Rifer_Msg UNH_VABrma
152800100701      * ?                                       ------------
152900100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
153000100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
153100100701     C                   SUB       1             lunghezza_num
153200100701     C                   EXSR      Se_Numerico
153300100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
153400090211      * ?                                       --------
153500100701     C                   MOVEL     Numero_OK     UNH_VABrmn
153600090211      * ?                                       --------
153700090211     C                   END
153800090303      **
153900090210     c     end_UNH       endSR
154000100701      * ?================================================================== */
154100100701      *? dati iniziali   BGM
154200100701      * ?================================================================== */
154300100701     C     DATI_BGM      BEGSR
154400100701      *
154500100701      *  identificativo testata
154600100706     c                   eval       BGM_Id_Testata = BGM1004
154700100706     C                   IF           BGM_Id_Testata <> *blank
154800100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
154900100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
155000100706     c                   end
155100100706      *
155200100701      **
155300100701     c                   endSR
155400100701      * ?================================================================== */
155500100701     C*?  Contatori in testata o in Dettaglio
155600100701      * ?================================================================== */
155700100701     C     DATI_CNT      BEGSR
155800100701      **
155900100701     c                   IF        Dati_di_Testata  ='S'
156000100701      **
156100100701      *  TOTALE colli, peso, spedizioni:
156200100701     c                   if        cnt6069 = Totale_Peso_Lordo
156300100701     c                   z-add     cnt6066       Tot_Peso_msg     15 2
156400100701     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
156500100701     c                   z-add     cnt6066       Tot_Sped_msg     15 2
156600100701     c                   elseIf    cnt6069 = Totale_Nr_Colli
156700100701     c                   z-add     cnt6066       Tot_Colli_msg    15 2
156800100701     c                   elseIf    cnt6069 = Totale_Peso_Netto
156900100701     c                   z-add     cnt6066       Tot_PNetto_msg   15 2
157000100701     c                   end
157100100701      **
157200100701     C                   ELSE
157300100701      **
157400100701     C                   END
157500100701      **
157600100701     c     end_CNT       endSR
157700100701      * ?================================================================== */
157800100701     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
157900100701      * ?================================================================== */
158000100701     C     DATI_RFF      BEGSR
158100120424      *
158200120424      *   Cerca la decodifica del dato in TABELLA
158300120424     C                   movel(p)  Tipo_Seg      etbSGM
158400120424     C                   movel(p)  RFF1153       etbVALSGM
158500120424     c                   exsr      In_TAB_EDSTBL
158600100701      **
158700100701      * ?   se in Testata
158800100702      * ?   X_Cargo_Manifest o X_Numero_CMR
158900100702      **
159000100701     c                   IF        Dati_di_Testata  ='S'
159100100701      **
159200100701     C*  Se tipo documento CMR ('CMR')
159300100701     C                   if        RFF1153 =  X_Cargo_Manifest
159400100701     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
159500120424      *
159600100701     C*  Se tipo documento AFB (Foglio viaggio)
159700100701     C                   elseIF    RFF1153 =  X_Numero_CMR
159800100701     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
159900120424      *
160000120424     C*  Se tipo documento  particolare       x CMR/F.Viaggio
160100120424     C                   elseIF    Quale_campo ='RFFTES'
160200120424     c                              and RFF1153 <> *blank
160300120424     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
160400120424     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
160500120424      **
160600100701     C                   END
160700120629      *
160800120629      * ?Codice Cliente di riferimento in Testata
160900120629     C                   clear                   Rifer_Cliente    35
161000120629     C                   if        RFF1153 = Riferimento_Cliente_particolare
161100120629     C                              and RFF1154 <> *BLANKS
161200120629     c                               or
161300120629     C                             Quale_campo ='RFFCLI'  and
161400120629     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
161500120629      *>>>>>>>>
161600120629     C*------------------
161700120629     C                   movel     RFF1154       Rifer_Cliente
161800120629      *
161900120629      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
162000120629     c                   exsr      CL_Particolare
162100120629     C*------------------
162200120629      *>>>>>>>>
162300120629     C                   ENDIF
162400100701      **
162500100701     c                   ELSE
162600100701      *
162700100701      * ?   se in Dettaglio   Riferimento singola spedizione
162800100701      *  Trascodifico riferimento spedizione
162900120424      **  o con altro riferimento diverso dall'AGE che indica la stessa cosa
163000100701     C                   clear                   Rifer_Alfanum    35
163100100701     c                   if        RFF1153 = Riferimento_Spedizione  and
163200100701     c                                RFF1154 <> *BLANKS
163300120424     c                               or
163400120424     C                             Quale_campo ='RFFAGE'  and
163500120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
163600120424      *
163700100701     C                   movel(p)  RFF1154       Rifer_Alfanum
163800120424      *
163900100701     c                   end
164000100701      *
164100100701      ** controlla se contiene solo numeri
164200100701     C                   IF         Rifer_Alfanum <> *BLANKS
164300100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
164400100701     C                   movel     Rifer_Alfanum test_num
164500100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
164600100701     C                   sub       1             lunghezza_num
164700100701     C                   EXSR      Se_Numerico
164800100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
164900110707     C                   movel(p)  Rifer_Alfanum VABRMA
165000100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
165100100701     C                              Tipo_Rifer = *blanks
165200100701     C                   movel     Numero_OK     VABRMN
165300100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
165400111027      ****
165500100701     C                   ENDIF
165600100701     C                   ENDIF
165700100701     C                   ENDIF
165800100701      *
165900100701      * Codice Cliente di riferimento
166000110224     C                   clear                   Rifer_Cliente    35
166100100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
166200100701     C                              and RFF1154 <> *BLANKS
166300120424     c                               or
166400120424     C                             Quale_campo ='RFFCLI'  and
166500120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
166600110224      *>>>>>>>>
166700120629     C*------------------
166800100701     C                   movel     RFF1154       Rifer_Cliente
166900100701      *
167000100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
167100120629     c                   exsr      CL_Particolare
167200120629     C*------------------
167300120629      *>>>>>>>>
167400100702     C                   ENDIF
167500100702     C*------------------
167600100702      * ?Riferimento Numerico
167700100702     C*------------------
167800110224     C                   clear                   Rifer_Numeric    35
167900100701     C                   if        RFF1153 = Riferimento_Numerico
168000100701     C                              and RFF1154 <> *blanks
168100120424     c                               or
168200120424     C                             Quale_campo ='RFFNUM'  and
168300120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
168400120424      *
168500100701     C                   movel     RFF1154       Rifer_Numeric
168600100701     C                   END
168700100701      *
168800100701      * Numero Ordine di vendita
168900100701     C     Rifer_Numeric IFNE      *BLANKS
169000100701     C                   movel     Rifer_Numeric test_num
169100100701     C     ' '           scan      Rifer_Numeric lunghezza_num
169200100701     C                   sub       1             lunghezza_num
169300100701     C                   EXSR      Se_Numerico
169400100702      *
169500100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
169600100701     C                   movel     Numero_OK     VABRMN
169700100701     C                   eval       Tipo_Rifer = Riferimento_Numerico
169800100701     C                   Endif
169900100702      *
170000100701     C                   ENDIF
170100100701      *
170200100701      * NOn c'è l'identificativo bolla del cliente
170300100701     c                   if        (RFF1153 = Riferimento_Spedizione and
170400100701     c                              RFF1154 = *BLANKS)
170500100701     c                             or
170600100701     c                             (RFF1153 = Riferimento_Numerico and
170700100701     c                              RFF1154 = *BLANKS)
170800100701     C                   eval      vinMSG = 'RFF riferimento assente'
170900100701     c                   exsr      Error_DET
171000100701     c                   goto      end_RFF
171100100702     C                   EndIF
171200100701      *
171300100701     C                   ENDIF
171400100701      *
171500140610      * SE IL PARTNER NON VUOLE UTILIZZARE il riferimento NUMERICO e vuole
171600140610      * dare un codice Riferimento PARTICOLARE tabellato su EDSTBL00F x identifare
171700140610      *  un RESO o un ORM
171800140610      *         Memorizzo il codice passato sul campo di 14
171900140610      *  che contiene il nostro identificativo (ex BOLLA Export o ORM export)
172000140610      *
172100140610     C                   if        Quale_campo ='RESORM'
172200140610     c                               and RFF1154 <> *BLANKS
172300140623     C                   movel(p)  RFF1154       salva_REF_ORM
172400140610      *
172500140610      * deve essere ASSOLUTAMENTE un CODICE NUMERICO di 14
172600140919     C                   clear                   TEST_Numerico    35
172700140919     C                   eval       TEST_Numerico = %trim(RFF1154)
172800140610      *
172900140610     C                   movel     TEST_Numerico test_num
173000140610     C     ' '           scan      TEST_Numerico lunghezza_num
173100140610     C                   sub       1             lunghezza_num
173200140610     C                   EXSR      Se_Numerico
173300140610      *
173400140610      * riferimento restituito da Partner  --> lo imposta solo se numerico
173500140610     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
173600140610      * attenzione Numero_OK (lungo 15) quindi "MOVE"!
173700140610     C                   move      Numero_OK     Cod_RESO_ORM
173800140610     C                   Endif
173900140610      *
174000140610     C                   END
174100140610      *
174200100701     c     end_RFF       endSR
174300100701      * ?================================================================== */
174400100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
174500100701      * ?================================================================== */
174600100701     C     DATI_DTM      BEGSR
174700100705      *
174800100705      *   Cerca la decodifica del dato in TABELLA
174900120424     C                   movel(p)  Tipo_Seg      etbSGM
175000100705     C                   movel(p)  dtm2005       etbVALSGM
175100100705     c                   exsr      In_TAB_EDSTBL
175200100705      *
175300100701     C*
175400100701     C*  Se data documento  memorizzo numero e data x CMR
175500100705     C                   If        Quale_campo ='DATDOC'
175600101015     c                              and dtm2380 <> *blank
175700101015      **
175800100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
175900100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
176000100702     C                   EXSR      Converte_Data
176100100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
176200100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
176300100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
176400100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
176500100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
176600100701     C     WERRO         IFEQ      'S'
176700100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
176800100706     C                             dtm2005  +
176900100702     C                              ') data errata'
177000100701     c                   exsr      Error_DET
177100100701     c                   goto      end_DTM
177200100701     C                   END
177300100701     C                   END
177400100702     C*
177500100702     C* Data Partenza del mezzo
177600100705     C                   If        Quale_campo ='DATPAR'
177700101015     c                              and dtm2380 <> *blank
177800101015      **
177900100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
178000100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
178100100702     C                   EXSR      Converte_Data
178200100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
178300100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
178400100702     C     WERRO         IFEQ      'S'
178500100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
178600100706     C                             dtm2005  +
178700100702     C                              ') data errata'
178800100702     c                   exsr      Error_DET
178900100702     c                   goto      end_DTM
179000100702     C                   END
179100100702     C                   END
179200100702     C*
179300100702     C* Data Previsto Arrivo del mezzo
179400100705     C                   If        Quale_campo ='DATARR'
179500101015     c                              and dtm2380 <> *blank
179600101015      **
179700100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
179800100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
179900100702     C                   EXSR      Converte_Data
180000100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
180100100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
180200100702     C     WERRO         IFEQ      'S'
180300100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
180400100706     C                             dtm2005  +
180500100702     C                              ') data errata'
180600100702     c                   exsr      Error_DET
180700100702     c                   goto      end_DTM
180800100702     C                   END
180900100702     C                   END
181000100701      **
181100100701     C*  Se data consegna richiesta imposto già sui campi del VAB
181200101015     C                   If        Quale_campo ='VABDCR'
181300101015     c                              and dtm2380 <> *blank
181400101015      **
181500100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
181600100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
181700100702     C                   EXSR      Converte_Data
181800100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
181900100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
182000100701     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
182100100701     C     WERRO         IFEQ      'S'
182200100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
182300100706     C                             dtm2005  +
182400100702     C                              ') data errata'
182500100701     c                   exsr      Error_DET
182600100701     c                   goto      end_DTM
182700100701     C                   END
182800100702     C                   end
182900100701      **
183000100701     c     end_DTM       endSR
183100100701      * ?================================================================== */
183200100701     C*? Imposta informazioni sul dettaglio del trasporto
183300100701      * ?================================================================== */
183400100701     C     DATI_TDT      BEGSR
183500100701     C*
183600100701      **
183700100701     c     end_TDT       endSR
183800100705      * ?================================================================== */
183900100705     C*? Imposta la Località
184000100705      * ?================================================================== */
184100100705     C     DATI_LOC      BEGSR
184200100705     C*
184300100705      **
184400100705     c     end_LOC       endSR
184500100705      * ?================================================================== */
184600100705     C*? Imposta identificativi delle Merci
184700100705      * ?================================================================== */
184800100705     C     DATI_EQD      BEGSR
184900100705     C*
185000100705      **
185100100705     c     end_EQD       endSR
185200100705      * ?================================================================== */
185300100705     C*? Imposta il numero delle Merci
185400100705      * ?================================================================== */
185500100705     C     DATI_EQN      BEGSR
185600100705     C*
185700100705      **
185800100705     c     end_EQN       endSR
185900100316      * ?================================================================== */
186000100316     C*? Imposta l'inizio del Dettaglio CNI
186100100316      * ?================================================================== */
186200100316     C     DATI_CNI      BEGSR
186300100316     C*
186400100702      ** Inizio Dettaglio riferimento iniziale
186500100702     c                   If        CNI1004 <> *blank
186600100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
186700101027      *
186800101027      *   Nel Riferimento Alfa sempre
186900110707     c                   eval            VABRMA  = CNI_Identificativo_Bolla
187000101027     C                   EVAL      RFF_RifSpediz = CNI_Identificativo_Bolla
187100101027      *
187200101027      *   e nel numerico se di soli numeri e comunque se lo memorizza
187300101027     C                   eval       test_num = CNI1004
187400101027     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
187500101027     C                   SUB       1             lunghezza_num
187600101027     C                   EXSR      Se_Numerico
187700101027     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
187800101027      * ?                                       --------
187900101027     C                   MOVEL     Numero_OK     VABrmn
188000101027      * ?                                       --------
188100101027     C                   z-add     VABrmn        CNI_seNumerico
188200101027      * ?                                       --------
188300101027     C                   END
188400101027      *
188500101027      *
188600100702     c                   end
188700100319      *
188800100316      **
188900100316     c     end_CNI       endSR
189000100316      * ?================================================================== */
189100100316     C*? Imposta le merci pericolose
189200100316      * ?================================================================== */
189300100316     C     DATI_DGS      BEGSR
189400100316     C*
189500100316      **
189600100316     c     end_DGS       endSR
189700100316      * ?================================================================== */
189800100316     C*? Imposta il numero seriale
189900100316      * ?================================================================== */
190000100316     C     DATI_SEL      BEGSR
190100100316     C*
190200100316      **
190300100316     c     end_SEL       endSR
190400100316      * ?================================================================== */
190500100316     C*? Imposta informazioni aggiuntive
190600100316      * ?================================================================== */
190700100316     C     DATI_PIA      BEGSR
190800100316     C*
190900100316      **
191000100316     c     end_PIA       endSR
191100100316      * ?================================================================== */
191200100316     C*?  Tipo Servizio
191300100316      * ?================================================================== */
191400100316     C     DATI_TSR      BEGSR
191500100702      ***
191600100702     C*  Reperisco priorità spedizione:
191700100702     C                   Z-ADD     1             X
191800100702      ***
191900100702      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
192000100702      ***  mentre in tabella EDI EEX è fatto con "1"
192100100702      ***
192200100702     c                   movel(p)  TSR4219       Work_Data__35
192300100702     c                   move      PU_TipoServ   Work_Data__35
192400100705     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
192500100702     C     *IN32         IFEQ      '1'
192600100702     C                   eval       VABTSP = TipoServizio(X)
192700100705     C                   ELSE
192800100705      **  Il tipo bolla non è presente
192900100705     C                   eval      vinMSG = 'TSR non in tabella'
193000100707     c*********          exsr      Error_DET
193100100707     c*********          goto      end_TSR
193200100705     C                   END
193300100702     C*
193400100702     C*  Decodifico servizi speciali
193500100702     C     TSR7085       IFNE      *BLANKS
193600100702     C     TSR7085       ANDNE     *LOVAL
193700100705      *
193800100702     C                   Z-ADD     1             X
193900100702     c                   movel(p)  TSR7085       Work_Data__35
194000100702     c                   move      PU_SpecServ   Work_Data__35
194100100702     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
194200100705     C     *IN32         IFEQ      '1'
194300100702     C                   eval       EDIDSSS = UNI_ServSpec(X)
194400100705      *
194500100702     C     §SSNOT        IFEQ      'S'
194600100702     C                   MOVEL     §SSDES        VABNOT
194700100702     C                   END
194800100702      *
194900120214      * x Espresso     "Pre-noon"    codice "PRN"
195000100702     C     §SSTPS        IFEQ      'S'
195100100702     C                   MOVEL     'E'           VABTSP
195200100702     C                   END
195300110509      *
195400120214      * x H 10:30      "H 10:30"     codice "H10"
195500110509     C     §SSTPS        IFEQ      'H'
195600110509     C                   MOVEL     'H'           VABTSP
195700110509     C                   END
195800100705      *
195900120214      * x Appuntamento "Booking ins" codice "BKI"
196000120403     c                   IF        §SSTPS ='A' and VABTC1 = *blank
196100100702     C                   MOVEL     'A'           VABTC1
196200120403     c                   elseIF    §SSTPS ='A' and VABTC2 = *blank
196300120403     C                   MOVEL     'A'           VABTC2
196400100702     C                   END
196500120403      *
196600120214      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
196700120214     C     §SSTPS        IFEQ      'F'
196800120214     C                   MOVEL     'S'           VABFFD
196900120214     C                   END
197000100705      *
197100100705     C                   ELSE
197200100705      *
197300100705      **  Il tipo bolla non è presente
197400100705     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
197500100707     c*******            exsr      Error_DET
197600100707     c*******            goto      end_TSR
197700100705      *
197800100702     C                   END
197900100705      *
198000100702     C                   END
198100100316      **
198200100316     c     end_TSR       endSR
198300100705      * ?================================================================== */
198400100705     C*?  C.O.D. Monetary Amount
198500100705      * ?================================================================== */
198600100705     C     DATI_MOA      BEGSR
198700100705      *
198800100705      *   Cerca la decodifica del dato in TABELLA
198900120424     C                   movel(p)  Tipo_Seg      etbSGM
199000100705     C                   movel(p)  moa5025       etbVALSGM
199100100705     c                   exsr      In_TAB_EDSTBL
199200100705      *
199300100705     C* Controllo se campo importo numerico con 3 decimali
199400100705     C     moa5004       IFNE      *zeros
199500100705      *
199600100705     C                   If        Quale_campo ='VABIAS'
199700130529      *
199800130529      * il campo è diviso in 2 flag: il primo serve per l'importo da assicurare
199900130529      *    quindi se deve essere considerato è espresso sulla PT.
200000130529     c                   if        %subst(§PTxxx:1:1) = 'S'
200100100707     C     moa5004       div       100           VABIAS                         IMPORTO
200200100705     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
200300130529     c                   end
200400100705      *
200500100705     C                   elseIF    Quale_campo ='VABVMD'
200600100707     C     moa5004       div       100           VABVMD                         MERCE
200700100705     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
200800100705      *
200900100705     C                   elseIF    Quale_campo ='VABCAS'
201000100705     C     §PUCAS        IFEQ      'S'                                          C/Assegno
201100100705      *
201200100707     C     moa5004       div       100           VABCAS
201300100705     C                   MOVEL     moa6345       VABVCA
201400100705     c                   if        Tes_valuta <> *blank and VABVCA = *blank
201500100705     C                   MOVEL     Tes_valuta    VABVCA
201600100705     c                   end
201700110627      **
201800110627      **  se c'è il default ("??")
201900110627     C                   Z-ADD     1             Y
202000110627     c                   movel(p)  '??'          Work_Data__35
202100110627     c                   move      §puTC         Work_Data__35
202200110627     c                   clear                   WData_35_2        2
202300110627     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
202400110627     c                   if        not *in33
202500110627      *
202600100705      *
202700130311      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO ( ma deve leggere il segmento
202800130311      * contenente il Tipo Incasso - sempre successivo al segmento MOA )
202900100705      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
203000130311      *
203100130311      * ATTENZIONE viene impostato di Default ma, in presenza del FTX+PAI,
203200130311      *  viene reimpostato in base a quello sulla tabella "TC".
203300130311      *   Quindi viene modificato da "OM" ....a quello sul FTX+PAI successivo.
203400100705     C     §PUTPC        IFEQ      'P'
203500100705     C                   MOVEL     'OM'          VABTIC
203600100705     C                   MOVEL     'OM'          VABtic_work
203700100705     C                   ENDIF
203800110627      **
203900110627     c                   end
204000100705      *
204100100705     C                   END
204200100705      *
204300100705     C                   EndIF
204400100705     C                   END                                                    Imp.non numer.
204500100705      *
204600100705     c     end_MOA       endSR
204700100705      * ?================================================================== */
204800100705     C*?  FREE TEXT
204900100705      * ?================================================================== */
205000100705     C     DATI_FTX      BEGSR
205100100705      *
205200100705      *   Cerca la decodifica del dato in TABELLA
205300120424     C                   movel(p)  Tipo_Seg      etbSGM
205400100705     C                   movel(p)  ftx4451       etbVALSGM
205500100705     c                   exsr      In_TAB_EDSTBL
205600100705      *
205700100705      * ? solo se si tratta di note descrittive da comunicare in BOLLA
205800100705     c                   if         quale_campo = 'VABNOT'
205900100705     C                   DO        4             X
206000100705      *
206100100705     C     D05(X)        IFNE      *BLANKS
206200100705      *
206300100705     C     Note_1        IFEQ      *BLANKS
206400100705     C                   MOVEL     D05(X)        Note_1
206500100705     C                   MOVE      D05(X)        Note_2
206600100705     C                   ELSE
206700100705     C     Note_2        IFEQ      *BLANKS
206800100705     C                   MOVEL     D05(X)        Note_2
206900100705     C                   END
207000100705     C                   END
207100100705      *
207200100705     C                   ENDif
207300100705      *
207400100705     C                   ENDdo
207500100705     C                   end
207600100705     C*------>>
207700110907      * ? solo se si tratta di note x invio mail di avviso al destinatario
207800110907      *    (nuovo servizio a pagamento attivato nel 2011)
207900110907      *     dovranno essere scritti 2 records nel VAT di tipo I e J
208000110907     c                   if         quale_campo = 'VATNOT'
208100131025     c                                 or
208200131025     c                              quale_campo = 'VATEML'
208300131025      *
208400131025     c                   if        ftx4453 = 'N'
208500131025     c                   eval      FTX_MAIL_come_servizio = 'N'
208600131025     c                   end
208700110907      *
208800110907     C                   DO        4             X
208900110907      *
209000110907     C     D05(X)        IFNE      *BLANKS
209100110907      *
209200110907     C     Note_1t       IFEQ      *BLANKS
209300110907     C                   MOVEL     D05(X)        Note_1t
209400110907     C                   MOVE      D05(X)        Note_2t
209500110907     C                   ELSE
209600110907     C     Note_2t       IFEQ      *BLANKS
209700110907     C                   MOVEL     D05(X)        Note_2t
209800110907     C                   END
209900110907     C                   END
210000110907      *
210100110907     C                   ENDif
210200110907      *
210300110907     C                   ENDdo
210400110907      *
210500110907      * carica il campo da riportare poi sul VAT
210600110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
210700110907     c                             note_1T
210800110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
210900110907     c                             note_2T
211000110907     C                   end
211100131025     C*------>>
211200131025      * ? solo se si tratta di note x invio SMS di avviso al destinatario
211300131025      *    (nuovo servizio a pagamento attivato)
211400131025      *     dovrà essere scritto 1 record nel VAT di tipo S
211500131025     c                   if         quale_campo = 'VATSMS'
211600131025     c                   if        ftx4453 = 'N'
211700131025     c                   eval      FTX_SMS_come_servizio = 'N'
211800131025     c                   end
211900131025      *
212000131025     C                   DO        4             X
212100131025      *
212200131025     C     D05(X)        IFNE      *BLANKS
212300131025      *
212400131025     C     Note_1sms     IFEQ      *BLANKS
212500131025     C                   MOVEL     D05(X)        Note_1sms
212600131025     C                   MOVE      D05(X)        Note_2sms
212700131025     C                   ELSE
212800131025     C     Note_2sms     IFEQ      *BLANKS
212900131025     C                   MOVEL     D05(X)        Note_2sms
213000131025     C                   END
213100131025     C                   END
213200131025      *
213300131025     C                   ENDif
213400131025      *
213500131025     C                   ENDdo
213600131025      *
213700131025      * carica il campo da riportare poi sul VAT
213800131025     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
213900131025     c                             note_1sms
214000131025     C                   end
214100110907     C*------>>
214200100705     C*  Decodifico tipo pagamento C/Assegno
214300100705      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
214400100705      *      (Problema di pulizia in presenza di + righe di testo)
214500100705      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
214600100705      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
214700100705      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
214800100705      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
214900100705     C                   if        wri_vabtic = *blank
215000100705     C                   MOVEL     *BLANKS       VABTIC
215100100705     c                   end
215200100705      **
215300100705     c                   clear                   trovato_TPInc     1            TipoIncasso
215400100705      *
215500100705      * ?Se c'è il tipo Incasso
215600100705     c                   if         quale_campo = 'VABTIC'                      -- 01 --
215700100705      *
215800100705     C                   DO        4             X                              -- 02 --
215900100705     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
216000110623      **
216100130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
216200130311      **  per gestire Partner come AZKAR che ha codice "AAA"
216300130311     C**********         MOVEL     D05(X)        WCTC              2
216400130311     C                   MOVEL     D05(X)        WCTC             10
216500110623      **
216600110623     c                   eval      inviato_tipo_incasso ='S'
216700110623      **
216800110623      **  se non l'avesse trovato e fosse esplicitato un Default ("??")
216900110623      **   come per qualsisi codice venga inviato dal Cliente allora
217000110623      **   deve essere impostato il default (invece di fare un chiodo)
217100110623     C                   Z-ADD     1             Y
217200110623     c                   movel(p)  '??'          Work_Data__35
217300110623     c                   move      §puTC         Work_Data__35
217400110623     c                   clear                   WData_35_2        2
217500110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
217600110623     c                   if            *in33
217700110623     c                   move      'S'           trovato_TPInc
217800110623     C                   MOVEL     TipoIncasso(Y)vabtic
217900110623     c                   movel(p)  '??'          WData_35_2
218000110623     c                   else
218100110623      *
218200130311      *  Se NON HA un default generico definito con "??"
218300130311      *   cerco con il codice arrivato dal Partner/Cliente
218400100705     C                   Z-ADD     1             Y
218500100705     c                   movel(p)  Wctc          Work_Data__35
218600100705     c                   move      §puTC         Work_Data__35
218700100705     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
218800110623     c                   if            *in33
218900110623     c                   move      'S'           trovato_TPInc
219000110623     C                   MOVEL     TipoIncasso(Y)vabtic
219100130326     c                   move      §puTC         WData_35_2        2
219200110623     c                   else
219300110623      *
219400130311      *  Se NON HA TROVATO qualcosa di DEFINITO SU MISURA del Partner/Cliente
219500130311      *   tramite il SUFFISSO presente sulla PT, prova con IL CODICE semplicemente.
219600110623     C                   Z-ADD     1             Y
219700110623     c                   movel(p)  Wctc          Work_Data__35
219800110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
219900110623     c                   if            *in33
220000110623     c                   move      'S'           trovato_TPInc
220100110623     C                   MOVEL     TipoIncasso(Y)vabtic
220200130311     c                   movel     Wctc          WData_35_2        2
220300110623     c                   end
220400110623      *
220500110623     c                   end
220600110623      *
220700110623     c                   end
220800130311      *--------
220900130311      * SOLO SE PARTNER
221000130311      *   IMPOSTO 'OM'    (vecchia regola ormai in disuso se NON siano state
221100130311      *     trovate fra le tabelle dei codici ben precisi x il PARTNER,
221200130311      ***     ossia solo se NON E'STATO PASSATO un codice specifico
221300130311      ** ATTENZIONE NON DEVE ESISTERE IL BLANK in arrivo dal PARTNER ma sempre un CODICE.
221400100705     C     §PUTPC        IFEQ      'P'
221500100705     c     wctc          andne     'TO'
221600110408     c     WData_35_2    andeq     *blank
221700100705     C                   MOVEL     'OM'          VABTIC
221800100705     C                   END                                                    -- 03 --
221900130311      *--------
222000100705     C                   END                                                    -- 03 --
222100100705     C                   ENDdo                                                  -- 02 --
222200100705      *
222300100705     C                   ELSE                                                   -- 01 --
222400130311      *
222500130311      *  PASSA DA QUI se il FTX ha un qualificatore NON RICONOSCIUTO
222600130311      *   e se  aveva impostato blank (nullo) precedentemente....
222700130311      *****
222800100705      * ?Se NON c'è il tipo Incasso Contrassegno particolare
222900100705      *    Deve essere impostato comunque da testata
223000110623     c                   if        vabtic = *blank   and
223100110623     c                             WData_35_2 = *blank
223200100705     C                   MOVEL     VABtic_work   VABTIC
223300100705     c                   end
223400100705      *
223500100705     C                   END                                                    -- 01 --
223600100705      *
223700100705      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
223800100705      *    essere presenti altri record Flat con altre informazioni
223900100705      *    che potrebbero abblancare il Tipo incasso precedentemente
224000100705      *    decodificato
224100100705     c                   if        Trovato_TPInc = 'S' and
224200100705     c                                 wri_vabtic = *blank
224300100705     c                   eval      wri_vabtic = 'S'
224400100705     c                   end
224500100705      *
224600100705      * ?Se Natura Merce      .
224700100705     c                   if         quale_campo = 'VABNAS'                      -- 01 --
224800100705     C                   MOVEL     D05(1)        VABnas
224900100705     c                   End
225000100705      *
225100120613      * ?Consegne Particolari
225200120613     c                   if         quale_campo = 'VABTCX'                      -- 01 --
225300120613      * Imposta il codice di 3 chrs.
225400120613      *   solo i primi 3 caratteri del campo
225500120613     C                   eval      controlla_COD_FTX = D05(1)
225600120613      *
225700120613      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
225800120613     C                   if        controlla_COD_FTX = 'FLR'
225900120613     c                   eval      FTX_ai_piani = 'P'
226000120614     c                   End
226100120613     c                   End
226200120613      *
226300100705     c     end_FTX       endSR
226400090209      * ?================================================================== */
226500090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
226600090209      * ?================================================================== */
226700090209     C     DATI_TOD      BEGSR
226800090209      *
226900090210      *  Trascodifico tipo trasporto
227000090210     c                   clear                   TOD_char3         3
227100100705      *  DEFAULT da PT
227200090210      *  imposta il default dalla tabella PT se c'è
227300090210     C     §puPFA        ifNE      *blank
227400090210     c                   SELECT
227500090210      *  Porto Franco
227600090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
227700090210     C                   movel     '4'           VABCBO                         + C/Ass
227800090210     c                   WHEN      §puPFA = 'F'
227900090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
228000090210      *  Porto Assegnato
228100090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
228200090210     C                   movel     '6'           VABCBO                         + C/Ass
228300090210     C                   WHEN      §puPFA = 'A'
228400090210     C                   movel     '2'           VABCBO                         Senza C/Ass
228500090210     C                   ENDSL
228600090210     c                   end
228700090210      *
228800090210      *   Imposta o il codice o il metodo
228900090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
229000090210     c                   if        tod4053  <> *blank
229100090210     c                   movel(p)  tod4053       TOD_char3
229200090210     c                   else
229300090210     c                   if        tod4215  <> *blank
229400090210     c                   movel(p)  tod4215       TOD_char3
229500090210     c                   end
229600090210     c                   end
229700110328      ****************
229800110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
229900110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
230000110328      ****************
230100110328     c                   eval      Trovata_Tab_TB ='N'
230200110328      *
230300110328     c                   if              TOD_char3 <> *blank
230400110328     c                   movel(p)  TOD_char3     Key_Generica35
230500110328     c                   move      §puTB         Key_Generica35
230600110328     C                   eval      Y = 1
230700110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
230800110328     c   32              eval      Trovata_Tab_TB ='S'
230900110328      *
231000110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
231100110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
231200110328      *
231300110328     c                   if          Trovata_Tab_TB ='N'
231400110328     c                                 and
231500110328     C                               tod4215 <> *blank
231600110328     c                   movel(p)  tod4215       TOD_char3
231700110328     c                   movel(p)  TOD_char3     Key_Generica35
231800110328     c                   move      §puTB         Key_Generica35
231900110328     C                   eval      Y = 1
232000110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
232100110328     c   32              eval      Trovata_Tab_TB ='S'
232200110328     c                   end
232300110328     c                   end
232400110328      *
232500110328      ****************
232600110328      ******* Se vi sono eccezioni sul cliente imposta quelle
232700110328      ****************
232800110328     c                   If          Trovata_Tab_TB ='S'
232900110328      *
233000110328     c                   SELECT
233100110328     c                   WHEN      Decod_Porto(Y) = 'F'
233200110328     c                               and  VABcas > *zeros
233300110328     C                   movel     '4'           VABCBO                         + C/Ass
233400110328     c                   WHEN      Decod_Porto(Y) = 'F'
233500110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
233600110328      *  Porto Assegnato
233700110328     c                   WHEN      VABCAS > *zeros
233800110328     C                   movel     '6'           VABCBO                         + C/Ass
233900110328     C                   OTHER
234000110328     C                   movel     '2'           VABCBO                         Senza C/Ass
234100110328     C                   ENDSL
234200110328      *
234300110328      ****************  altrimenti
234400110328      ******* Se non vi sono eccezioni per il cliente
234500110328      *   Rileva a questo punto lo Standard per tutti
234600110328      ****************
234700110328     c                   elseIf      Trovata_Tab_TB ='N'
234800090210      **
234900090210     c                   if              TOD_char3 <> *blank
235000100705      *   Cerca la decodifica del dato in TABELLA
235100120424     C                   movel(p)  Tipo_Seg      etbSGM
235200100705     C                   movel(p)  TOD_char3     etbVALSGM
235300100705     c                   exsr      In_TAB_EDSTBL
235400090210      *
235500100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
235600100705      *
235700090210     c                   SELECT
235800100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
235900100705     c                               VABcas > *zeros
236000090210     C                   movel     '4'           VABCBO                         + C/Ass
236100100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
236200090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
236300090210      *  Porto Assegnato
236400090210     c                   WHEN      VABCAS > *zeros
236500090210     C                   movel     '6'           VABCBO                         + C/Ass
236600090210     C                   OTHER
236700090210     C                   movel     '2'           VABCBO                         Senza C/Ass
236800090210     C                   ENDSL
236900090210      *
237000090210     c                   end
237100090210     c                   endIF
237200090209      **
237300110328     c                   ENDif
237400110328      **
237500090210      **  Il tipo bolla non è presente
237600090210     c                   if        VABCBO = *blank
237700090210     C                   eval      vinMSG = 'TOD non rilevato'
237800090213     c                   exsr      Error_DET
237900090210     c                   goto      end_TOD
238000090210     c                   end
238100090210      **
238200090210     c     end_TOD       endSR
238300090213      * ?================================================================== */
238400090213     C*?  Anagrafica del mittente e del destinatario
238500090213      * ?================================================================== */
238600091016     C     DATI_NAD      BEGSR
238700100705      *
238800100705      *   Cerca la decodifica del dato in TABELLA
238900120424     C                   movel(p)  Tipo_Seg      etbSGM
239000100705     C                   movel(p)  nad3035       etbVALSGM
239100100705     c                   exsr      In_TAB_EDSTBL
239200100708      *-------
239300120629      * Dato (SHIPPED FROM) usato principalmente x CMR
239400120629      *       ma anche usato al posto di un RFF+FF in testata messaggio
239500120629      *   quindi modificato il nome di riferimento del campo sul EDSTBL
239600120629      *   x renderlo più generico.
239700100708      *-------
239800120629     C********************        IF        Quale_campo ='VABCMR'
239900120629     C                   IF        Quale_campo ='NAD_SF'
240000120629      *-------
240100120629      * Dati di PROVENIENZA MESSAGGIO
240200120629      *-------
240300120629      ***
240400120629      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
240500100708     c                   if        nad30361 <> *blank
240600120629     C                   MOVEL     nad30361      NAD_SF_Cliente
240700100708     c                   elseIf    nad31241 <> *blank
240800120629     C                   MOVEL     nad31241      NAD_SF_Cliente
240900100708     c                   elseIf    nad3039  <> *blank
241000120629     C                   MOVEL     nad3039       NAD_SF_Cliente
241100100708     c                   end
241200120629      ***
241300120629      * ?SOLO SE NON c'è un RFF+FF: di Testata che prevale su tutto.
241400120629      * ?La PLATFORM LIST è usata come il Codice Cliente su RFF+FF:PLATFORMLIST
241500120629      ***
241600120629      * Se partner/cliente è abilitato alla funzione di sostituzione CLIENTE da UNB.
241700120629      *   Il (SHIPPED FROM) in testata funziona come un RFF+FF: a livello di testata.
241800120629      *    per agganciare i dati sulla tab."CL".
241900120629      ***
242000120629     C                   if         PZ_abilita_NAD_SF_come_RFF_FF ='S'
242100120629     c                                 and Tes_RFF_FF_ksc = 0
242200120629      * ?Ma solo se NON c'è un RFF+FF: di Testata che prevale su tutto.
242300120629      *
242400120629      *   imposta la PLATFORM LIST ricevuta del MITTENTE
242500120629      *    questo codice viene poi utilizzato sulla tab.CL (EDTAB) se concordato
242600120629      *    per codificare tutte le spedizioni come un RFF+FF:xxxxxxxxxxxxxxxxx di testata
242700120629     c                   eval      NAD_SF_Cliente = NAD3039
242800120629      *>>>>>>>>
242900120629     C*------------------
243000120629      *    Aggancia il Cliente e tutte le caratteristiche da tab.:"CL"
243100120629      * ?RFF+FF:
243200120629     C                   clear                   Rifer_Cliente    35
243300120629     C                   movel     NAD_SF_ClienteRifer_Cliente
243400120629     c                   exsr      CL_Particolare
243500120629     C*------------------
243600120629      *>>>>>>>>
243700120629     c                   end
243800120629      ***
243900100705      *-------
244000090210      * Dati destinatario
244100100705      *-------
244200100708     C                   elseIF    Quale_campo ='VABRSD'
244300090227      *
244400090227      * Se trovato un indirizzo di destinatario
244500100701     c                   if           Dati_di_Testata = 'N'
244600100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
244700100701     c                   else
244800100701     c                   eval       NAD_destinatario_in_testata = 'S'
244900100701     c                   end
245000090210      *
245100090210     C* Reperisco nazione corrispondente destinatario
245200100705     C                   Z-ADD     1             naz               3 0
245300100705     C                   MOVEL     *BLANKS       VABNZD
245400100705     C                   Z-ADD     0             WFLCPF
245500090210     c                   eval      *in32 = *off
245600100705      ***
245700090210     C                   if        nad3207 <> *BLANKS
245800111028     C     nad3207       LOOKUP    ISO(naz)                               32
245900100705     C                   If        *in32 = *on
246000100705      ***
246100100705      * per reperire il CAP corretto
246200100705     C                   MOVEL     C15(naz)      VABNZD
246300100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
246400100705      ***
246500100705     c                   Endif
246600100705     C                   endif
246700100705      *
246800090210     C* Imposto dati destinatario
246900100705     C                   MOVEL     nad30361      VABRSD
247000100705     C                   if        nad30362 <> *LOVAL
247100100705     C                   MOVEL     nad30362      VABRD2
247200090210     C                   endif
247300100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
247400091019???? C* Imposto dati destinatario
247500091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
247600100705???? C                   MOVEL     nad31241      VABRSD
247700100705???? C                   if        nad31242 <> *LOVAL
247800100705???? C                   MOVEL     nad31242      VABRD2
247900091019???? C                   endif
248000091019???? C                   end
248100100705      *   città/Località
248200090210     C                   MOVEL     nad3164       VABLOD
248300100705      *   Indirizzo
248400100705     C                   MOVEL     nad30421      VABIND
248500111121      *
248600111121     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
248700111121     c                                         %len(%trim(NAD30422))
248800111121???? c                   eval      NAD_Lunghezza_Indirizzo =
248900111121     c                                         %len(%trim(VABIND))
249000111121???? c                   eval      NAD_Lunghezza_ragSociale2 =
249100111121     c                                         %len(%trim(VABRD2))
249200111116      *
249300111121      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
249400111121      *  lo aggiunge nel proseguimento della Ragione Sociale
249500111121     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
249600111121      **
249700111121     c                             and  %len(VABIND) <
249800111121     c                                  NAD_Lunghezza_Indirizzo +
249900111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
250000111121      *
250100111121      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
250200111121      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
250300111121     c                             and  %len(VABRD2) -
250400111121     c                                  NAD_Lunghezza_ragSociale2 -
250500111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
250600111121     c                                  >= 0
250700111121      *
250800111121      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
250900111121     c                             and  NAD_Lunghezza_ragSociale2 <
251000111121     c                                  NAD_Lunghezza_Indirizzo
251100111121      *
251200111121     C     '    '        SCAN      VABRD2        WI
251300111121     c                   if        %Found
251400111121     C                   MOVEA     VABRD2        LOD
251500111121     C                   ADD       1             WI
251600111121     C                   MOVEA     NAD30422      LOD(WI)
251700111121     C                   MOVEA     LOD           VABRD2
251800111121     c                   end
251900111121      *
252000111121      * Altrimenti lo aggiunge direttamente sulla VIA
252100111121     c                   else
252200100705      * Se NAD30422  non è vuoto
252300111116      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
252400111116      *
252500111116     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
252600090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
252700111121      *
252800100705     C* Verifico se nella località c'è dello spazio
252900111116     C     '    '        SCAN      VABIND        WI
253000100705     c                   if        %Found
253100111116     C                   MOVEA     VABIND        LOD
253200090210     C                   ADD       1             WI
253300090210     C                   MOVEA     NAD30422      LOD(WI)
253400111116     C                   MOVEA     LOD           VABIND
253500100705     c                   else
253600111121      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
253700111116     C     '    '        SCAN      VABLOD        WI                4 0
253800100705     c                   if        %Found
253900111116     C                   MOVEA     VABLOD        LOD
254000100705     C                   ADD       1             WI
254100100705     C                   MOVEA     NAD30422      LOD(WI)
254200111116     C                   MOVEA     LOD           VABLOD
254300100705     C                   ENDif
254400090210     C                   ENDif
254500100705      *
254600100705     C                   ENDif
254700111121      *
254800111121     c                   end
254900111121      *
255000111116      **************
255100111116      *** PRIMA provava ad aggiungere prima alla località......
255200111116      **************
255300111116     C*  aggiungo alla LOCALITA' quanto di indirizzo ce n'è in più
255400111116     c*************      if        NAD30422 <> *blanks and NAD30422 <> *loval
255500111116     C* Verifico se nella località c'è dello spazio
255600111116     C*****'    '        SCAN      VABLOD        WI                4 0
255700111116     c*************      if        %Found
255800111116     C*************      MOVEA     VABLOD        LOD
255900111116     C*************      ADD       1             WI
256000111116     C*************      MOVEA     NAD30422      LOD(WI)
256100111116     C*************      MOVEA     LOD           VABLOD
256200111116     c*************      else
256300111116      *  Altrimenti se non c'è spazio dopo la località prova a concatenarlo alla via
256400111116     C*****'    '        SCAN      VABIND        WI
256500111116     c*************      if        %Found
256600111116     C*************      MOVEA     VABIND        LOD
256700111116     C*************      ADD       1             WI
256800111116     C*************      MOVEA     NAD30422      LOD(WI)
256900111116     C*************      MOVEA     LOD           VABIND
257000111116     C*************      ENDif
257100111116     C*************      ENDif
257200111116      *************
257300111116     C*************      ENDif
257400111116      *
257500111116      *
257600111116      *
257700111116      *
257800101027      * RIPORTO LA 2° RAG.SOCIALE NELL'INDIRIZZO.
257900101027     c                   if        vabIND = *blank and VABRD2 <> *BLANK
258000101027      *
258100101027     C                   EVAL      VABIND = VABRD2
258200101027      *
258300101027     C                   ENDif
258400090210      * Converto CAP
258500100705     C                   EXSR      Converte_CAP
258600090603     C*
258700100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
258800100705     C*  record del VAB viene PULITO
258900100316     c                   if        Dati_di_Testata  = 'S'
259000090603     c                   eval       tes_VABrsd = VABrsd
259100090603     c                   eval       tes_VABrd2 = VABrd2
259200090603     c                   eval       tes_VABind = VABind
259300090603     c                   eval       tes_VABlod = VABlod
259400090603     c                   eval       tes_VABnzd = VABnzd
259500090603     c                   end
259600100728      *
259700100728      *   se NON c'è l'identificativo bolla del cliente
259800100728      *    blocca la spedisione e lo segnala.
259900100728     C                   if          vabRSD = *blank
260000100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
260100130315     c******             exsr      Error_DET
260200130315     c******             goto      end_NAD
260300100728     C                   elseIF      vabIND = *blank
260400100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
260500101006     c*********          exsr      Error_DET
260600101006     c*********          goto      end_NAD
260700100728     C                   elseIF      vabLOD = *blank
260800100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
260900101006     c*********          exsr      Error_DET
261000101006     c*********          goto      end_NAD
261100100728     C                   ENDIF
261200100705      *-------
261300090210      * Dati mittente
261400100705      *-------
261500100705     C                   elseIF     Quale_campo ='VABRMO'
261600090210      *
261700100705     C                   movel     nad30361      VABRMO
261800090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
261900090210     C                   movel     §PTNAZ        VABNMO
262000090210      *
262100090210     C                   IF        nad3207 <> *blanks
262200100705     C                   eval      naz = 1
262300100705     C     nad3207       LOOKUP    ISO(naz)                               32
262400090210     C                   if         *in32 = *on
262500100705     C                   MOVEL     C15(naz)      VABNMO
262600090210     C                   endif
262700090210     C                   ENDIF
262800090210      *
262900090210      * Normalizzo CAP mittente originale
263000090210     C                   clear                   T
263100090210     C                   clear                   S
263200090210     C                   MOVEA     *BLANKS       CAPM
263300090210     C                   MOVEA     nad3251       CAP9
263400090210     C                   DO        9             T
263500090210     C     CAP9(T)       IFNE      *BLANKS
263600090210     C                   ADD       1             S
263700090210     C                   MOVE      CAP9(T)       CAPM(S)
263800090210     C                   ENDIF
263900090210     C                   ENDDO
264000090210     C*
264100090210      *     Controllo validità CAP
264200090210     C                   MOVEA     CAPM          VABCMO
264300090210      *
264400090210     C                   CLEAR                   TISI95DS
264500090210     C                   MOVEL     VABCMO        I95CAP
264600090210     C                   MOVEL     VABNMO        I95NAR
264700090210     C                   MOVEL     WOGGI         I95DAT
264800090210     C                   MOVEL     '3'           I95TCN
264900090210     C                   CALL      'TISI95R'
265000090210     C                   PARM                    TISI95DS
265100090210      *     Errore
265200090210     C     O95ERR        IFNE      *BLANKS
265300090210     C                   MOVEL     INDCAE        VABCMO
265400090210     C                   MOVEL     §PTNAZ        VABNMO
265500090210     C                   END
265600090603      *
265700100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
265800100705     C*  record del VAB viene PULITO
265900100316     c                   if        Dati_di_Testata  = 'S'
266000090603     c                   eval      tes_VABrmo = VABrmo
266100090603     c                   eval      tes_VABnmo = VABnmo
266200090603     c                   eval      tes_VABcmo = VABcmo
266300090603     c                   eval      NAD_mittente_in_testata = 'S'
266400100701     c                   else
266500100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
266600090603     c                   end
266700100728      *
266800100728      *   se NON c'è l'identificativo bolla del cliente
266900100728      *    blocca la spedisione e lo segnala.
267000100728     C                   if          vabRMO = *blank
267100100728     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
267200101006     c*********          exsr      Error_DET
267300101006     c*********          goto      end_NAD
267400100728     C                   ENDIF
267500090210      *
267600090210     C                   END
267700090210      *
267800090210     c     end_NAD       endSR
267900090210      * ?================================================================== */
268000090210     C*?  Contatto a cui far riferimento
268100090210      * ?================================================================== */
268200090210     C     DATI_CTA      BEGSR
268300090210      *
268400090210      *  memorizza il riferimento di consegna x il destinatario
268500090210     C     cta3412       ifne      *BLANKS
268600100701     c                   if        Dati_di_Testata  ='S'
268700100701     c                   eval       CTA_Destinatario_in_testata   = cta3412
268800100701     c                   else
268900100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
269000100701     c                   end
269100100701     c                   end
269200090210      *
269300090210     c                   endSR
269400090210      * ?================================================================== */
269500090210     C*?  Contatto telefonico  a cui far riferimento
269600090210      * ?================================================================== */
269700090210     C     DATI_COM      BEGSR
269800090210      *
269900100705      *  memorizza il riferimento di consegna nr.telefonico
270000100705     C                   if        com3148 <> *BLANKS    and
270100100705     C                              com3155 = Telephone_Number
270200100701     c                   if        Dati_di_Testata  ='S'
270300100701     c                   eval      COM_telefono_in_testata   = com3148
270400100701     c                   else
270500100701     c                   eval      COM_telefono_in_dettaglio = com3148
270600090210     c                   end
270700100705     c                   endif
270800090210      *
270900090209     C                   ENDSR
271000090210      * ?================================================================== */
271100090210     C*?  Dati di dettaglio della spedizione e dei colli
271200090210      * ?================================================================== */
271300090210     C     DATI_GID      BEGSR
271400090210      *
271500090210     C* Se ho totali per spedizione metto in campi VAB
271600090210     C     gid1496       IFEQ      99999
271700090210     C     gid1496       orEQ      9999
271800100630     c                   eval      GID_aTotale = 'S'
271900090210     C                   z-add     gid722420     VABNCL
272000090210     C                   ELSE
272100100630     c                   If        GID_aTotale = 'N'
272200100705     C                   z-add     gid722420     GID_ColliInAdd
272300100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
272400090210     C                   END
272500090210     C                   END
272600090212      *
272700100701     C                   ADD       VABNCL        ToT_Colli        16 0
272800090210      *
272900090210     c     end_GID       endSR
273000090210      * ?================================================================== */
273100090210     C*?  Dati di dettaglio misure spedizione e colli
273200090210      * ?================================================================== */
273300090210     C     DATI_MEA      BEGSR
273400090210      *
273500090210     C*  Peso
273600100319     C     mea6311       IFEQ      Weight
273700100319     C     mea6411       ANDEQ     Kilograms
273800090210      *
273900090210     C*  Controllo se ho valore totale o parziale
274000090210      *   sempre il Lordo
274100100319???? C     mea6313       IFEQ      G_Weight
274200100319???? C     mea6313       oreq      Gross_Weight
274300100706???? C     mea6313       oreq      Gross_Weight_E
274400110512???? C     mea6313       oreq      Gross_Weight_D
274500101027???? C     mea6313       oreq      *blank
274600100630     C                   IF        GID_aTotale = 'S'
274700091019???? C                   move      mea6314       mea6314_2        18 2          Peso
274800091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
274900090210     C                   ELSE
275000091019???? C                   move      mea6314       mea6314_2                      Peso
275100091019???? C                   ADD       mea6314_2     VABPKB                         Peso
275200090210     C                   END
275300091203     C                   END
275400090210      *
275500100319???? C     mea6313       IFEQ      G_Weight
275600100319???? C     mea6313       oreq      Gross_Weight
275700100706???? C     mea6313       oreq      Gross_Weight_E
275800110512???? C     mea6313       oreq      Gross_Weight_D
275900101027???? C     mea6313       oreq      *blank
276000091019???? C                   move      mea6314       mea6314_2                      Peso
276100100701???? C                   ADD       mea6314_2     ToT_PesoLordo
276200090210     C                   END
276300100706???? C     mea6313       IFEQ      N_Weight
276400100706???? C     mea6313       oreq      Net_Weight
276500100706???? C     mea6313       oreq      Gross_Weight_E
276600110512???? C     mea6313       oreq      Gross_Weight_D
276700101027???? C     mea6313       oreq      *blank
276800091019???? C                   move      mea6314       mea6314_2                      Peso
276900100701???? C                   ADD       mea6314_2     ToT_PesoNetto
277000090210     C                   END
277100090210     C                   END
277200090210      ***
277300090210     C*  Peso in Grammi  se abilitato a utilizzarlo
277400090210      ***
277500100319     C     mea6311       IFEQ      Weight
277600100319     C     mea6411       ANDEQ     Grams
277700090210      *
277800090210     C*  Controllo se ho valore totale o parziale
277900100319???? C     mea6313       IFEQ      G_Weight
278000100319???? C     mea6313       oreq      Gross_Weight
278100100706???? C     mea6313       oreq      Gross_Weight_E
278200110512???? C     mea6313       oreq      Gross_Weight_D
278300101027???? C     mea6313       oreq      *blank
278400100630     c                   If        GID_aTotale = 'S'
278500091019???? c                   move      mea6314       mea6314_2
278600091019???? C     mea6314_2     div       1000          VABPKB                         Peso
278700090210     C                   ELSE
278800091019???? c                   move      mea6314       mea6314_2
278900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
279000091019???? C                   add       Peso_inKG     VABPKB                         Peso
279100090210     C                   END
279200090210     C                   END
279300090210      *
279400100319???? C     mea6313       IFEQ      G_Weight
279500100319???? C     mea6313       oreq      Gross_Weight
279600100706???? C     mea6313       oreq      Gross_Weight_E
279700110512???? C     mea6313       oreq      Gross_Weight_D
279800101027???? C     mea6313       oreq      *blank
279900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
280000100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
280100090210     C                   END
280200100706???? C     mea6313       IFEQ      N_Weight
280300100706???? C     mea6313       oreq      Net_Weight
280400100706???? C     mea6313       oreq      Gross_Weight_E
280500110512???? C     mea6313       oreq      Gross_Weight_D
280600101027???? C     mea6313       oreq      *blank
280700091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
280800100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
280900090210     C                   END
281000090210     C                   END
281100090210      *  Volume
281200100319???? C     mea6311       IFEQ      Volume
281300100319???? C     mea6411       ANDEQ     Cubic_Meters
281400090210      *  Controllo se ho valore totale o parziale
281500100630     c                   If        GID_aTotale = 'S'
281600091019???? C                   move      mea6314       mea6314_2        18 2          Volume
281700091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
281800090210     C                   ELSE
281900091019???? C                   move      mea6314       mea6314_2        18 2          Volume
282000091019???? C                   ADD       mea6314_2     VABVLB                         Volume
282100090210     C                   END
282200090210     C                   END
282300100705      *
282400100705      *
282500100705     C* Ricerco CAP in base alle dimensioni
282600090210     C     WCAP          IFNE      *BLANKS
282700100319     C     mea6311       IFEQ      Weight
282800090210      *
282900090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
283000090210     C                   CLEAR                   FNLV55DS
283100090210     C                   MOVEL     'P'           D55TPT
283200090210     C                   MOVEL     WOGGI         D55DRF
283300090210     C                   MOVEL     VABLNP        D55LNP
283400090210     C                   MOVEL     VABLNP        D55LIN
283500090210     C                   CALL      'FNLV55R'
283600090210     C                   PARM                    FNLV55DS
283700090210     C                   MOVE      D55TFP        COMTFP            3 0
283800090210      *
283900090210     C*  PASSO IL TERMINAL PARTENZA
284000090210     C                   CLEAR                   TISI95DS
284100090210     C                   Z-ADD     COMTFP        I95TFP
284200090210     C                   MOVEL     VABTSP        I95TSP
284300090210     C                   MOVEL     'S'           I95FRE
284400090210     C                   MOVEL     '3'           I95TCN
284500090210     C                   MOVEL     WCAP          I95CAP
284600090210     C                   MOVEL     VABNZD        I95NAR
284700090210     C                   MOVEL     VABLOD        I95LOC
284800090210     C                   MOVEL     VABIND        I95IND
284900090210      *
285000090210     C* Se gestita seconda linea di arrivo passo peso - volume
285100090210     C* e numero colli effettivo
285200090210     C     §PULNA        IFEQ      'S'
285300090210     C                   Z-ADD     VABPKB        I95LKG
285400090210     C                   Z-ADD     VABVLB        I95LMC
285500090210     C                   ELSE
285600100706     C*******
285700090210     C* Altrmenti passo 1 Kg e 0,001
285800100706     C*******              Z-ADD1                  I95LKG
285900100706     C*******              Z-ADD0,001              I95LMC
286000100706     C*******
286100090210     C                   END
286200090210     C*
286300090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
286400090210     C                   SELECT
286500090210     C     VABCBO        WHENEQ    '1 '
286600090210     C                   MOVEL     *BLANKS       I95FCA
286700090210     C                   MOVEL     'F'           I95TPO
286800090210     C     VABCBO        WHENEQ    '2 '
286900090210     C                   MOVEL     *BLANKS       I95FCA
287000090210     C                   MOVEL     'A'           I95TPO
287100090210     C     VABCBO        WHENEQ    '4 '
287200090210     C                   MOVEL     'S'           I95FCA
287300090210     C                   MOVEL     'F'           I95TPO
287400090210     C     VABCBO        WHENEQ    '6 '
287500090210     C                   MOVEL     'S'           I95FCA
287600090210     C                   MOVEL     'A'           I95TPO
287700090210     C                   OTHER
287800090210     C                   MOVEL     'F'           I95TPO
287900090210     C     VABCAS        IFGT      0
288000090210     C                   MOVEL     'S'           I95FCA
288100090210     C                   ELSE
288200090210     C                   MOVEL     *BLANKS       I95FCA
288300090210     C                   END
288400090210     C                   ENDSL
288500090210     C*
288600090210     C                   CALL      'TISI95R'
288700090210     C                   PARM                    TISI95DS
288800090210     C* Reperisco i dati da CAP
288900090210     C                   MOVEL     O95PRV        VABPRD
289000100706     C                   MOVEL     WCAP          VABCAD
289100100114      ***
289200100706     c                   MOVEL     O95LNA        VABLNA
289300100706     C                   MOVEL     O95ZNC        VABZNC
289400090210     C*
289500090210     C     O95ERR        IFNE      *BLANKS
289600090210     C     O95FIT        ANDEQ     'S'
289700090210     C*  Cap non trovato
289800090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
289900101006     c*********          exsr      Error_DET
290000101006     C*********          goto      end_MEA
290100090210     C                   END
290200090210     C                   END
290300090210      *
290400090210     C                   ELSE
290500090210      *
290600090210     C                   CLEAR                   VABPRD
290700090210     C                   CLEAR                   VABLNA
290800090210     C                   CLEAR                   VABZNC
290900090210     C                   CLEAR                   VABCAD
291000090210     C*  Cap non trovato
291100090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
291200101006     c*********          exsr      Error_DET
291300101006     C*********          goto      end_MEA
291400090210     C                   END
291500090210      *
291600090210     c     end_MEA       endSR
291700090210      * ?================================================================== */
291800090210     C*?  Dati di dettaglio Codice a barre Segnacolli
291900090210      * ?================================================================== */
292000090210     C     DATI_PCI      BEGSR
292100090210      *
292200090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
292300090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
292400090210     C     §1BF12        IFEQ      'E'
292500100706     C                   EXSR      SEGNaCollo_EEX
292600090210     C                   ELSE
292700090210      *
292800090210      *  La prima volta controllo congruità numero segnacollo
292900090210     C     §PUBS1        IFNE      *BLANKS
293000090210     C     VABnrs        ANDNE     0
293100090210      *  Se il codice trattamento merce prevede che segnacollino i
293200090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
293300090210     C     §1BFG7        IFEQ      'S'
293400090210     C     §1BFG1        OREQ      'S'
293500090210      *  calcolo segnacollo
293600100706     C                   EXSR      Segn_Bartolini
293700090210     C                   END
293800090210     C                   END
293900090210      *
294000090210     C                   END
294100090210      *
294200090210     c     end_PCI       endSR
294300090211     C*----------------------------------------------------------------
294400090211      *? dati finali del dettaglio    UNT
294500090211     C*----------------------------------------------------------------
294600090211     C     DATI_UNT      BEGSR
294700110324      *
294800110324      * Non deve dare messaggio
294900110324     c                   goto      No_UNTctrl
295000090211      *
295100100708     c                   eval      UNT_record = vnREC
295200090213     c                   z-add     vnrec         last_RECord
295300090211      **
295400100319      **   verifica la quadratura di tutti i segmenti ricevuti
295500100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
295600100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
295700100728     C     UNT0074       IFne      Conta_segmenti
295800100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
295900100319     c                             trollare il Messaggio ricevuto'
296000100319      *
296100100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
296200100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
296300100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
296400100319     C                             Bolle import in filiale - messaggio -
296500100319     C                             con quadratura UNT errata controllare EDI -
296600100319     C                             ricevuto su UPLOAD x il cliente :'
296700100319     C                   EVAL      Messaggio = %trim(Messaggio) +
296800100319     C                                         %editc(§PTksc:'X')
296900100319     C                   EVAL      Messaggio = %trim(Messaggio) +
297000100319     C                             ' i segmenti contati sono :' +
297100100319     C                             %editc(Conta_segmenti:'Z')
297200100319     c                   if        §PVinv <> 'N'
297300100319     c                   exsr      invio_mail
297400100319     c                   end
297500100319     c                   end
297600110321     c     No_UNTctrl    tag
297700100319      **
297800100319     c     End_UNT       endSR
297900100316     C*----------------------------------------------------------------
298000100316      *? dati finali     UNT
298100100316     C*----------------------------------------------------------------
298200100316     C     DATI_UNZ      BEGSR
298300100316      *
298400100726     c                   move      'S'           Forza_Uscita
298500100316      **
298600100316     c                   endSR
298700090210      *----------------------------------------------------------------
298800090210      *? Input segnacolli su schiera di 20 elementi
298900090210      *----------------------------------------------------------------
299000090210     C     INP_Segnacol  BEGSR
299100090210      *
299200090210     c                   clear                   quanti_PCI        3 0
299300090210     c                   if        §puEL1 = 0
299400090210     c                   z-add     10            quanti_PCI
299500090210     c                   else
299600090210     c                   z-add     §puEL1        quanti_PCI
299700090210     c                   end
299800090210      *
299900090210      *  Pulisce la schiera dei segnacolli da elaborare
300000090210     c                   clear                   x30
300100090210     c                   z-add     0             Nr_x30            3 0
300200090210      *
300300090210      *   riporto tutto sulla schiera generica X30
300400090210      *
300500090210     c                   do        10            limite            3 0
300600090210      * Limita quanti elementi passati in ogni PCI
300700090210      *   devono essere considerati
300800090210     c                   if        limite > quanti_PCI
300900090210     c                   Leave
301000090210     c                   end
301100090210      *
301200090210      *  Carica la schiera generale di tutti i segnacolli
301300090210     c                   if        D30_1(limite) <> *blank
301400090210     c                   add       1             Nr_x30
301500090210     c                   eval      X30(Nr_x30) = D30_1(limite)
301600090210     c                   end
301700090210     c                   enddo
301800090210      *
301900090210     C                   ENDSR
302000090210      *----------------------------------------------------------------
302100090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
302200090210      *----------------------------------------------------------------
302300100706     C     SEGNaCollo_EEXBEGSR
302400090210      *
302500090210     c                   exsr      INP_Segnacol
302600090210      *
302700090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
302800090210     C                   DO        10            x                              --- 01 ---
302900090210      *
303000090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
303100090210     C     x30(X)        ANDNE     *LOVAL
303200090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
303300100701     C                   ADD       1             totale_PCI
303400100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
303500090210     C                   END                                                    --- 03 ---
303600090210      *
303700090210     C                   END                                                    --- 02 ---
303800090210      *
303900090210     C                   ENDSR
304000090210     C*----------------------------------------------------------------
304100090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
304200090210     C*----------------------------------------------------------------
304300100706     C     Segn_BartoliniBEGSR
304400090210      *
304500090210     c                   exsr      INP_Segnacol
304600090210     C*
304700090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
304800090210     C*  in base al numero elementi validi carico sgn.
304900090210     C                   DO        10            X                              --- 01 ---
305000090210     C*
305100090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
305200090210     C     x30(X)        ANDNE     *LOVAL
305300090210     C*
305400100630     C                   CLEAR                   DS_SegnBART
305500090210     C*  Controllo se devo ricevere la linea di partenza
305600090210     C                   MOVEL     'N'           WERRO             1
305700090210     C     §PULP1        IFGT      0                                            --- 03 ---
305800100706     C                   EXSR      REPerisce_LNP
305900090210     C                   END                                                    --- 03 ---
306000100706      *
306100090210     C*  Controllo se devo ricevere la linea di arrivo
306200090210     C     §PULA1        IFGT      0                                            --- 03 ---
306300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
306400100706     C                   EXSR      REPerisce_LNA
306500090210     C                   END                                                    --- 03 ---
306600100706      *
306700090210     C*  Controllo se devo ricevere il numero di serie
306800090210     C     §PUNR1        IFGT      0                                            --- 03 ---
306900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
307000100706     C                   EXSR      REPerisce_NRS
307100090210     C                   END                                                    --- 03 ---
307200100706      *
307300090210     C*  Controllo se devo ricevere il numero segnacollo
307400090210     C     §PUSG1        IFGT      0                                            --- 03 ---
307500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
307600100706     C                   EXSR      REPerisce_SGN
307700090210     C                   END                                                    --- 03 ---
307800100706      *
307900090210     C*  Controllo se devo ricevere la zona di consegna
308000090210     C     §PUZN1        IFGT      0                                            --- 03 ---
308100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
308200100706     C                   EXSR      REPerisce_ZNC
308300090210     C                   END                                                    --- 03 ---
308400100706      *
308500090210     C*  Imposto da VAB i dati a zero se mi è stato passato
308600090210     C*  numero segnacollo valido
308700090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
308800100706      *
308900100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
309000100630     C                   Z-ADD     VABLNP        SegnBART_LNP
309100090210     C                   END                                                    --- 04 ---
309200100706      *
309300100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
309400100630     C                   Z-ADD     VABLNA        SegnBART_LNA
309500090210     C                   END                                                    --- 04 ---
309600100706      *
309700100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
309800100630     C                   Z-ADD     VABNRS        SegnBART_NRS
309900090210     C                   END                                                    --- 04 ---
310000100706      *
310100100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
310200100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
310300090210     C                   END                                                    --- 04 ---
310400090210      *
310500090210     C*  mi reimposto LNA e zona da segnacollo
310600100114      ***
310700100114      *** dovendo accorpare le spedizioni da Conferma x CMR
310800100114      ***  non deve impostare l'instradamento.
310900100630     C                   Z-ADD     SegnBART_LNA  VABLNA
311000100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
311100100114      ***
311200090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
311300100701     C                   ADD       1             Conta_Segn
311400100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
311500090210     C                   END                                                    --- 03 ---
311600090210     C*
311700090210     C                   END                                                    --- 02 ---
311800090210     C*
311900090210     C                   END                                                    --- 01 ---
312000090210      *
312100090210     C                   ENDSR
312200090210     C*----------------------------------------------------------------
312300090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
312400090210     C*----------------------------------------------------------------
312500100706     C     REPerisce_LNP BEGSR
312600090210     C*
312700090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
312800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
312900090210     C*  dal numero segnacollo passatomi dal partner
313000090210     C                   Z-ADD     §PULP1        WP                2 0
313100090210     C     3             SUBST     x30(X):WP     WLNP              3
313200090210     C*
313300090210     C*  controllo che la lnp calcolata sia numerica
313400090210     C                   TESTN                   WLNP                 98
313500090210     C*
313600090210     C*  se è numerica la carico
313700090210     C     *IN98         IFEQ      '1'
313800100630     C                   MOVE      WLNP          SegnBART_LNP
313900090210     C                   ELSE
314000090210     C*  se non è numerica stampo errore
314100090210     C                   MOVEL     'S'           WERRO             1
314200090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
314300090213     c                   exsr      Error_DET
314400090210     C                   goto      end_LNP
314500090210     C                   END
314600090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
314700090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
314800100630     C     SegnBART_LNP  IFNE      VABLNP
314900090210     C                   MOVEL     'S'           WERRO             1
315000090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
315100090210     c                             a della Bolla'
315200090213     c                   exsr      Error_DET
315300090210     C                   goto      end_LNP
315400090210     C                   END
315500090210     C*
315600090210     C     end_LNP       ENDSR
315700090210     C*----------------------------------------------------------------
315800090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
315900090210     C*----------------------------------------------------------------
316000100706     C     REPerisce_LNA BEGSR
316100090210     C*
316200090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
316300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
316400090210     C*  dal numero segnacollo passatomi dal partner
316500090210     C                   Z-ADD     §PULA1        WP                2 0
316600090210     C     3             SUBST     x30(X):WP     WLNA              3
316700090210     C*  controllo che la lna calcolata sia numerica
316800090210     C                   TESTN                   WLNA                 98
316900090210     C*  se è numerica la carico
317000090210     C     *IN98         IFEQ      '1'
317100100630     C                   MOVE      WLNA          SegnBART_LNA
317200090210     C                   ELSE
317300090210     C*  se non è numerica stampo errore
317400090210     C                   MOVEL     'S'           WERRO             1
317500090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
317600090213     c                   exsr      Error_DET
317700090210     C                   goto      end_LNA
317800090210     C                   END
317900090210     C*
318000090210     C     end_LNA       ENDSR
318100090210     C*----------------------------------------------------------------
318200090210     C*? REPNRS - Reperisce numero serie
318300090210     C*----------------------------------------------------------------
318400100706     C     REPerisce_NRS BEGSR
318500090210     C*
318600090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
318700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
318800090210     C*  dal numero segnacollo passatomi dal partner
318900090210     C                   Z-ADD     §PUNR1        WP                2 0
319000090210     C     2             SUBST     x30(X):WP     WNRS              2
319100090210     C*  controllo che il nrs calcolata sia numerico
319200090210     C                   TESTN                   WNRS                 98
319300090210     C*  se è numerica la carico
319400090210     C     *IN98         IFEQ      '1'
319500100630     C                   MOVE      WNRS          SegnBART_NRS
319600090210     C                   ELSE
319700090210     C*  se non è numerico stampo errore
319800090210     C                   MOVEL     'S'           WERRO             1
319900090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
320000090213     c                   exsr      Error_DET
320100090210     C                   goto      end_NRS
320200090210     C                   END
320300090210      *
320400090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
320500090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
320600100630     C     SegnBART_NRS  IFNE      VABNRS
320700090210     C                   MOVEL     'S'           WERRO             1
320800090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
320900090210     c                             o della Bolla'
321000090213     c                   exsr      Error_DET
321100090210     C                   goto      end_NRS
321200090210     C                   END
321300090210     C*
321400090210     C     end_NRS       ENDSR
321500090210     C*----------------------------------------------------------------
321600090210     C*? REPZNC - Reperisce zona consegna
321700090210     C*----------------------------------------------------------------
321800100706     C     REPerisce_ZNC BEGSR
321900090210     C*
322000090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
322100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
322200090210     C*  dal numero segnacollo passatomi dal partner
322300090210     C                   Z-ADD     §PUZN1        WP                2 0
322400090210     C     2             SUBST     x30(X):WP     WZNC              2
322500090210     C*  controllo che la zona calcolata sia numerica
322600090210     C                   TESTN                   WZNC                 98
322700090210     C*  se è numerica la carico
322800090210     C     *IN98         IFEQ      '1'
322900100630     C                   MOVE      WZNC          SegnBART_ZNC
323000090210     C                   ELSE
323100090210     C*  se non è numerica stampo errore
323200090210     C                   MOVEL     'S'           WERRO             1
323300090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
323400090213     c                   exsr      Error_DET
323500090210     C                   goto      end_ZNC
323600090210     C                   END
323700090210     C*
323800090210     C     end_ZNC       ENDSR
323900090210     C*----------------------------------------------------------------
324000090210     C*? REPSGN - Reperisce numero segnacollo
324100090210     C*----------------------------------------------------------------
324200100706     C     REPerisce_SGN BEGSR
324300090210     C*
324400090210     C*  se viene passata nr.segnacollo estraggo sottostringa
324500090210     C*  lunga 7 chr (a partire dalla posizione iniziale
324600090210     C*  indicata in tabella)
324700090210     C*  dal numero segnacollo passatomi dal partner
324800090210     C                   Z-ADD     §PUSG1        WP                2 0
324900090210     C     7             SUBST     x30(X):WP     WSGN              7
325000090210     C*  controllo che il nr.segnacollo calcolato sia numerico
325100090210     C                   TESTN                   WSGN                 98
325200090210     C*  se è numerico lo carico
325300090210     C     *IN98         IFEQ      '1'
325400100630     C                   MOVE      WSGN          SegnBART_NSC
325500090210     C                   ELSE
325600090210     C*  se non è numerica stampo errore
325700090210     C                   MOVEL     'S'           WERRO             1
325800100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
325900101006     c*********          exsr      Error_DET
326000101006     C*********          goto      end_SGN
326100090210     C                   END
326200090210     C*
326300090210     C     end_SGN       ENDSR
326400051205      *----------------------------------------------------*
326500051205      *   DEFINIZIONE CHIAVI                               *
326600051205      *----------------------------------------------------*
326700051205     C     *INZSR        BEGSR
326800051205      *
326900991129     c     *ENTRY        PLIST
327000060613     C                   parm                    esito             1
327100971215      *
327200050112     C     KTABel        KLIST
327300050112     C                   KFLD                    TBLKUT
327400050112     C                   KFLD                    TBLCOD
327500050112     C                   KFLD                    TBLKEY
327600090210      *
327700090210     C     K_TAB1L       KLIST
327800090213     C                   KFLD                    etbUNB
327900090213     C                   KFLD                    etbSGM
328000090213     C                   KFLD                    etbVALSGM
328100090209     C*
328200090209     C* Definisco chiavi di accesso
328300090209     C     KTAB1         KLIST
328400090209     C                   KFLD                    KKUT
328500090209     C                   KFLD                    KCOD
328600090209     C*
328700090209     C     KNUF          KLIST
328800090209     C                   KFLD                    KAAA
328900090209     C                   KFLD                    KCNU
329000090209     C                   KFLD                    KFIL
329100090209      *
329200090209     C     KVAB1         KLIST
329300090209     C                   KFLD                    KCCM
329400090209     C                   KFLD                    KCMR
329500090209     C                   KFLD                    KCNT
329600090209     C                   KFLD                    KAAS
329700090209     C                   KFLD                    KLNP
329800090209     C                   KFLD                    KNRS
329900090209     C                   KFLD                    KNSP
330000090209      *
330100090209     C     KVAB2         KLIST
330200090209     C                   KFLD                    KCCM
330300090209     C                   KFLD                    KCMR
330400090209      *
330500090209     C     KRDE          KLIST
330600090209     C                   KFLD                    KAAS
330700090209     C                   KFLD                    KLNP1
330800090209     C                   KFLD                    KNRS
330900090209     C                   KFLD                    KNSP
331000090209     C                   KFLD                    KNMC
331100090209     C                   KFLD                    KNRP
331200090209      *
331300090209     C     KACO          KLIST
331400090209     C                   KFLD                    KKUT
331500090209     C                   KFLD                    KKCC
331600090209     C                   KFLD                    KKSC
331700090209      *
331800090209     C     KIND          KLIST
331900090209     C                   KFLD                    KKUT
332000090209     C                   KFLD                    KKCC
332100090209     C                   KFLD                    KKSC
332200111027      *
332300111027     C     KTAS          KLIST
332400111027     C                   KFLD                    TASAAS
332500111027     C                   KFLD                    TASLNP
332600111027     C                   KFLD                    TASNRS
332700111027     C                   KFLD                    TASNSP
332800090209      *
332900090209      * DETERMINO SE SONO BARTOLINI O SDI
333000090209     C                   CLEAR                   TIBS55DS
333100090209     C                   MOVEL     'L'           I50TLA
333200090209     C                   CALL      'TIBS55R'
333300090209     C                   PARM                    TIBS55DS
333400090209      *
333500090209     C* RECUPERO DATI DELL'UTENTE
333600090209     C                   Z-ADD     1             CODUT             1 0
333700090209     C                   CALL      'XPARUT'
333800090209     C                   PARM                    UTEDSE0F
333900090209     C                   MOVEL     RAGUT         RSUT             20
334000090209     C*
334100090209     C* RICERCA CAPOCONTI
334200090209     C                   DO        50            X
334300090209     C                   MOVE      TCU(X)        TCUDS
334400090209     C     F56           IFEQ      'CG'
334500090209     C     F34           ANDEQ     '01'
334600090209     C                   Z-ADD     KCU(X)        KCI               4 0
334700090209     C                   END
334800090209     C                   END
334900090209     C*
335000090211     c                   movel(p)  'EDPAB'       User_Msg         10
335100090211     C*
335200090209     C* IMPOSTO A ZERO VARIABILI DI PGM
335300090209     c                   move      *blank        Snd_Msg_alert     1
335400090209     C                   MOVEL     CA(1)         WCA              78
335500090209     C                   MOVEL     CN(1)         WCN              78
335600090209     C*
335700090209     C* RECUPERO DATA E ORA
335800100630     C                   TIME                    Ora_Data         14 0
335900100630     C                   MOVE      Ora_Data      G02DAT
336000100630     C                   MOVE      Ora_Data      WDATA8            8 0
336100100630     C                   MOVE      WDATA8        Anno2             2 0
336200090209     C                   MOVEL     WDATA8        DATA              6 0
336300100630     C                   MOVE      Anno2         DATA
336400090209     C                   MOVEL     *BLANK        G02ERR
336500090209     C                   CALL      'XSRDA8'
336600090209     C                   PARM                    WLBDA8
336700090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
336800100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
336900090209     C                   TIME                    WORA              6 0
337000090209      * Recupero data e ora
337100090209     C                   TIME                    W0140
337200090209      * UDATE IN GGMMAAAA
337300100630     C                   MOVE      W0140         DataGGMMAAA
337400090209      * UDATE IN AAAAMMGG
337500100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
337600090209     C     *iso          MOVEL     DATA_eur      dateu
337700090209      *
337800090209      * ?              /*--------------------------- */
337900090209     c                   exsr      CARICA_TABELLE
338000090209      * ?              /*--------------------------- */
338100090209      *
338200090209     C                   MOVEL     *ALL'-'       CMP132          132
338300991124      *
338400050414      *------------------
338500991124     C                   ENDSR
338600060621      * ?------------------------------------------------------------------ */
338700090209      *?    CARICA TABELLE SU SCHIERE
338800060621      * ?------------------------------------------------------------------ */
338900090209     C     CARICA_TABELLEBEGSR
339000090205      *
339100090209     C* Caricamento Tabella 1B
339200090209     C                   Z-ADD     0             X                 4 0
339300090209     C                   Z-ADD     CODUT         KKUT
339400090209     C                   MOVEL     '1B'          KCOD
339500090209     C     KTAB1         CHAIN     TABEL00F                           30
339600090209     C     *IN30         DOWEQ     '0'
339700090209     C     TBLFLG        IFEQ      *BLANKS
339800090209     C                   ADD       1             X
339900100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
340000100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
340100090209     C                   END
340200090209     C     KTAB1         READE     TABEL00F                               30
340300090209     C                   END
340400090209      *
340500090209     C* Caricamento Tabella 15
340600090209     C                   Z-ADD     0             X                 4 0
340700090209     C                   Z-ADD     CODUT         KKUT
340800090209     C                   MOVEL     '15'          KCOD
340900090209     C     KTAB1         CHAIN     TABEL00F                           30
341000090209     C     *IN30         DOWEQ     '0'
341100090209     C     TBLFLG        IFEQ      *BLANKS
341200090209     C                   ADD       1             X
341300090209     C                   MOVEL     TBLKEY        C15(X)
341400090209     C                   MOVEL     TBLUNI        DS15
341500090209     C                   MOVEL     §15COD        ISO(X)
341600090209     C                   MOVEL     §15ECF        CPF(X)
341700090209     C                   MOVE      §15PCF        CPF(X)
341800090209     C                   END
341900090209     C     KTAB1         READE     TABEL00F                               30
342000090209     C                   END
342100090209      *
342200090209     C* Caricamento Tabella CV
342300090209     C                   Z-ADD     0             X
342400090209     C                   Z-ADD     CODUT         KKUT
342500090209     C                   MOVEL     'CV'          KCOD
342600090209     C     KTAB1         CHAIN     TABEL00F                           30
342700090209     C     *IN30         DOWEQ     '0'
342800090209     C     TBLFLG        IFEQ      *BLANKS
342900090209     C                   ADD       1             X
343000090209     C                   MOVEL     TBLKEY        CCV(X)
343100090209     C                   MOVEL     TBLUNI        DCV(X)
343200090209     C                   END
343300090209     C     KTAB1         READE     TABEL00F                               30
343400090209     C                   END
343500090216      *
343600100702     C* Caricamento Tabella Priorità trasporto
343700100702     C                   Z-ADD     0             X
343800100702     C                   MOVEL     'TS'          TABCOD
343900100702     C     TABCOD        CHAIN     EDTAB01L                           30
344000100702     C     *IN30         DOWEQ     '0'
344100100702     C     TABFLG        IFEQ      *BLANKS
344200100702     C                   ADD       1             X
344300100702     C                   eval      TipoServizio(X) = TABUNI
344400100705     C                   eval      Key_TipServ(X)  = TABKEY
344500100702     C                   END
344600100702     C     TABCOD        READE     EDTAB01L                               30
344700100702     C                   END
344800100702      *
344900110328      * Caricamento Tabella termini consegna
345000110328     C                   Z-ADD     0             X
345100110328     C                   MOVEL     'TB'          TABCOD
345200110328     C     TABCOD        CHAIN     EDTAB01L                           30
345300110328     C     *IN30         DOWEQ     '0'
345400110328     C     TABFLG        IFEQ      *BLANKS
345500110328     C                   ADD       1             X
345600110328     C                   eval      K35_TpBolla(X) = TABKEY
345700110328     C                   eval      Cod_TpBolla(X) = TABKEY
345800110328     C                   eval      Decod_Porto(X) = TABUNI
345900110328     C                   END
346000110328     C     TABCOD        READE     EDTAB01L                               30
346100110328     C                   END
346200110328      *
346300090216     C* Caricamento Tabella Tipo Incasso C/Assegno
346400090216     C                   Z-ADD     0             X
346500090216     C                   MOVEL     'TC'          TABCOD
346600090216     C     TABCOD        CHAIN     EDTAB01L                           30
346700090216     C     *IN30         DOWEQ     '0'
346800090216     C     TABFLG        IFEQ      *BLANKS
346900090216     C                   ADD       1             X
347000100705     C                   eval      K35_TpIncas(X) = TABKEY
347100100705     C                   eval      TipoIncasso(X) = TABUNI
347200090216     C                   END
347300090216     C     TABCOD        READE     EDTAB01L                               30
347400090216     C                   END
347500090209      *
347600090209      * Caricamento Tabella Partner esteri
347700090209     C                   Z-ADD     0             X
347800090209     C                   MOVEL     'PT'          TABCOD
347900090209     C     TABCOD        CHAIN     EDTAB01L                           30
348000090209     C     *IN30         DOWEQ     '0'
348100090209      *
348200090209     C                   SELECT
348300090209     C     TABFLG        WHENNE    *BLANKS
348400090209      *
348500090209     C                   OTHER
348600090209     C                   ADD       1             X
348700100630     C                   MOVEL     TABKEY        PT_key(X)
348800100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
348900100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
349000100630     C                   MOVEL     TABUNI        PT_uni(X)
349100090209     C                   ENDSL
349200090209      *
349300090209     C     TABCOD        READE     EDTAB01L                               30
349400090209     C                   END
349500121105      *
349600121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
349700121105     c                   exsr      ChecK_PT
349800121105      *
349900090209      * salva quanti Codici UNB ha caricato
350000100630     c                   z-add     x             PT_KeyXsav
350100090209      *
350200090209     C                   MOVEL     'PU'          TABCOD
350300090209     C     TABCOD        CHAIN     EDTAB01L                           30
350400090209     C     *IN30         DOWEQ     '0'
350500090209      *
350600090209     C                   SELECT
350700090209     C     TABFLG        WHENNE    *BLANKS
350800090209      *
350900090209     C                   OTHER
351000100630     C                   MOVEL     TABKEY        PU_key
351100090209     C                   Z-ADD     1             X
351200100630     C     PU_key        LOOKUP    PT_key(X)                              32
351300100630     C   32              MOVEL     TABUNI        PU_uni(X)
351400090209     C                   ENDSL
351500090209      *
351600090209     C     TABCOD        READE     EDTAB01L                               30
351700090209     C                   END
351800090209      *
351900090209      * Prosegue tab.PT e PU
352000090209     C                   MOVEL     'PV'          TABCOD
352100090209     C     TABCOD        CHAIN     EDTAB01L                           30
352200090209     C     *IN30         DOWEQ     '0'
352300090209      *
352400090209     C                   SELECT
352500090209     C     TABFLG        WHENNE    *BLANKS
352600090209      *
352700090209     C                   OTHER
352800100630     C                   MOVEL     TABKEY        PV_key
352900090209     C                   Z-ADD     1             X
353000100630     C     PV_key        LOOKUP    PT_key(X)                              32
353100100630     C   32              MOVEL     TABUNI        PV_uni(X)
353200090209     C                   ENDSL
353300090209      *
353400090209     C     TABCOD        READE     EDTAB01L                               30
353500090209     C                   END
353501160205      *
353502160205      * Prosegue tab.PT e PU e PV e PS
353503160205     C                   MOVEL     'PS'          TABCOD
353504160205     C     TABCOD        CHAIN     EDTAB01L                           30
353505160205     C     *IN30         DOWEQ     '0'
353506160205      *
353507160205     C                   SELECT
353508160205     C     TABFLG        WHENNE    *BLANKS
353509160205      *
353510160205     C                   OTHER
353511160205     C                   MOVEL     TABKEY        PS_key
353512160205     C                   Z-ADD     1             X
353513160205     C     PS_key        LOOKUP    PT_key(X)                              32
353514160205     C   32              MOVEL     TABUNI        PS_uni(X)
353515160205     C                   ENDSL
353516160205      *
353517160205     C     TABCOD        READE     EDTAB01L                               30
353518160205     C                   END
353600120629      *
353700120629      * Prosegue tab.PT e PU e PV
353800120629     C                   MOVEL     'PZ'          TABCOD
353900120629     C     TABCOD        CHAIN     EDTAB01L                           30
354000120629     C     *IN30         DOWEQ     '0'
354100120629      *
354200120629     C                   SELECT
354300120629     C     TABFLG        WHENNE    *BLANKS
354400120629      *
354500120629     C                   OTHER
354600120629     C                   MOVEL     TABKEY        PZ_key
354700120629     C                   Z-ADD     1             X
354800120629     C     PZ_key        LOOKUP    PT_key(X)                              32
354900120629     C   32              MOVEL     TABUNI        PZ_uni(X)
355000120629     C                   ENDSL
355100120629      *
355200120629     C     TABCOD        READE     EDTAB01L                               30
355300120629     C                   END
355400090209      *
355500090209     C* Caricamento Tabella codici clienti - tariffe
355600090209     C                   Z-ADD     0             X
355700090209     C                   MOVEL     'CL'          TABCOD
355800090209     C     TABCOD        CHAIN     EDTAB01L                           30
355900090209     C     *IN30         DOWEQ     '0'
356000090209     C     TABFLG        IFEQ      *BLANKS
356100090209     C                   ADD       1             X
356200100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
356300100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
356400090209     C                   END
356500090209     C     TABCOD        READE     EDTAB01L                               30
356600090209     C                   END
356700090209      *
356800090209     C* Caricamento Serivizi speciali
356900090209     C                   Z-ADD     0             X
357000090209     C                   MOVEL     'SS'          TABCOD
357100090209     C     TABCOD        CHAIN     EDTAB01L                           30
357200090209     C     *IN30         DOWEQ     '0'
357300090209     C     TABFLG        IFEQ      *BLANKS
357400090209     C                   ADD       1             X
357500100702     C                   eval       Key_ServSpec(X) = TABKEY
357600100702     C                   eval       SpecialServ(X)  = TABKEY
357700100702     C                   eval       UNI_ServSpec(X) = TABUNI
357800090209     C                   END
357900090209     C     TABCOD        READE     EDTAB01L                               30
358000090209     C                   END
358100090209      *
358200090209     C                   ENDSR
358300090205      * ?------------------------------------------------------------------ */
358400090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
358500090205      * ?------------------------------------------------------------------ */
358600090205     C     *pssr         BEGSR
358700090205     C
358800060710     C                   eval      esito = '2'
358900130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 93'
359000130506      *
359100130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
359200130506     C                             IFCSUM S93A EDI ricevuto errato in UPLOAD -
359300130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
359400130506     c                             + ' verificare il '+
359500130506     c                             %editc(total_segmenti:'Z')
359600130506     c                             + ' segmento -> ' + dati
359700130506      *
359800100319     c                   if        §PVinv <> 'N'
359900100319     c                   exsr      invio_mail
360000100319     c                   end
360100060621     C
360200130506     c                   seton                                        LR
360300130506     c                   return
360400130506     C
360500130506     C                   ENDSR
360600090213      * ?------------------------------------------------------------------ */
360700090213      *?      Segnala Errore su TIVIN
360800090213      * ?------------------------------------------------------------------ */
360900090213     C     Error_DET     BEGSR
361000090213     C
361100100728     c                   eval      Segmento_in_errore = Conta_segmenti
361200090213     C                   eval      vinFLG = '2'
361300090213     c                   eval      Almeno_Un_Due ='S'
361400090213     c                   eval      esito  = '1'
361500090213     c                   eval      se_errore ='S'
361600090213     C                   eval      Err_in_detta = 'S'
361700090213     C
361800090213     C                   ENDSR
361900100705      * ?------------------------------------------------------------------ */
362000100705      *?   Controlla e decodifica Valori VALUE dei segmenti
362100100705      * ?------------------------------------------------------------------ */
362200100705     C     In_TAB_EDSTBL BEGSR
362300100705      *
362400100705      *   Cerca la decodifica del dato in TABELLA
362500100705     c                   clear                   trovata_EDSTBL    1
362600100705     c                   clear                   quale_campo       6
362700100705     c                   clear                   campo_dati_70A   70
362800100705     C                   movel     UNB_Mittente  etbUNB
362900100705     c     K_TAB1L       chain     edstbl01l
363000100705      *
363100100705      * prova quindi senza l'UNB specifico del cliente
363200100705     c                   if        not %Found(edstbl01l)
363300100705     C                   clear                   etbUNB
363400100705     c     K_TAB1L       chain     edstbl01l
363500100705     c                   end
363600100705      *
363700100705      *  Traduce i testi descrittivi se PRESENTE in tabella
363800100705      *   il campo di destinazione è definito sulla destra della descrizione
363900100705      *     ed è lungo 6 come i nomi definiti nei VAB.
364000100705     c                   if        %Found(edstbl01l)
364100100705     c                   move(p)   etbDESCRI     quale_campo
364200100705     c                   movel(p)  etbDATI       campo_dati_70A
364300100705     c                   move      'S'           trovata_EDSTBL
364400100705     c                   end
364500100705      *
364600100705      *
364700100705     C                   ENDSR
364800100705     c*==================================================================*
364900090209     C*? CNVDAT - DECODIFICA LA DATA
365000090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
365100090209     C*         - WDATA  (DATA ALFANUMERICA)
365200090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
365300090209     C*         - WHMS   (ORA NUMERICA)
365400090209     C*----------------------------------------------------------------
365500100702     C     Converte_Data BEGSR
365600090209     C*
365700090209     C                   MOVEL     ' '           WERRO             1
365800090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
365900090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
366000100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
366100090209     C                   Z-ADD     *ZEROS        WHMS              6 0
366200090209     C*
366300120905     C     Work_Format_3 IFEQ      *blank
366400120905     c                   if        %Len(%trim(Work_Data__35)) = 8
366500120905     C                   eval         Work_Format_3 = Data_senza_ora
366600120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
366700120905     C                   eval         Work_Format_3 = Data_con_ora
366800120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
366900120905     C                   eval         Work_Format_3 = Data_con_i_secondi
367000120905     c                   end
367100120905     C                   END
367200120905     C*
367300120905     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
367400100702     C                   MOVEL     Work_Data__35 WCOMO8            8
367500090209     C                   TESTN                   WCOMO8               99
367600090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
367700090209     C  N99              MOVEL     'S'           WERRO             1
367800090209     C                   END
367900090209     C*
368000100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
368100100702     C                   MOVEL     Work_Data__35 WCOMO12          12
368200100120     C                   TESTN                   WCOMO12              99
368300100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
368400100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
368500100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
368600090209     C  N99              MOVEL     'S'           WERRO             1
368700090209     C                   END
368800100120     C*
368900100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
369000100702     C                   MOVEL     Work_Data__35 WCOMO14          14
369100100120     C                   TESTN                   WCOMO14              99
369200100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
369300100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
369400100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
369500100120     C  N99              MOVEL     'S'           WERRO             1
369600100120     C                   END
369700090209     C*
369800090209     C                   ENDSR
369900090210     C*----------------------------------------------------------------
370000100701     C*? Se_Numerico       - CONTROLLO NUMERICO
370100100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
370200100701     C*         - lunghezza_num   (Lunghezza campo output)
370300100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
370400090210     C*----------------------------------------------------------------
370500100701     C     Se_Numerico   BEGSR
370600090210     C*
370700090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
370800100701     C                   MOVEL     'N'           Campo_Numerico    1
370900100701     C                   MOVEL     *BLANKS       Numero_OK        15
371000110224     C*
371100090210     C                   Z-ADD     15            IN                3 0
371200100701     C                   Z-ADD     lunghezza_num IX                3 0
371300110224     C*
371400090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
371500100701     C                   MOVEA     test_num      NDA
371600090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
371700090210     C                   MOVEA     *ALL'0'       NDN
371800100630      *
371900090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
372000100701     C                   Z-ADD     lunghezza_num I                 3 0
372100110224     C*
372200090210     C     I             DOWGT     0                                            --- 1 -->
372300110224     C*
372400090210     C* ESTRAE IL CARATTERE DA ESAMINARE
372500100701     C     1             SUBST     test_num:I    WTEM01            1
372600110224     C*
372700090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
372800090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
372900090210     C     WTEM01        SCAN      WCA                                    99
373000110224     C*
373100090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
373200090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
373300110224     C*
373400090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
373500090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
373600110224     C*
373700090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
373800090210     C* DA DESTRA VERSO SINISTRA
373900090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
374000090210     C                   MOVEL     WTEMP1        NDN(IN)
374100090210     C                   SUB       1             IN
374200090210     C                   END                                                    <-- 3 ---
374300110224     C*
374400090210     C                   END                                                    <-- 2 ---
374500090210     C                   SUB       1             I
374600110224     C*
374700090210     C                   END                                                    <-- 1 ---
374800090210     C*
374900090210     C                   MOVEA     NDN           WNDN             15
375000110224     C*
375100090210     C     WNDN          IFNE      *ZEROS
375200100701     C                   MOVEA     NDN           Numero_OK        15
375300100701     C                   MOVEL     'S'           Campo_Numerico    1
375400090210     C                   END
375500090210     C*
375600090210     C                   ENDSR
375700090210     C*----------------------------------------------------------------
375800090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
375900090210     C*----------------------------------------------------------------
376000100705     C     Converte_CAP  BEGSR
376100090210     C*
376200090210     C                   MOVEL     *BLANKS       WCAP              9
376300090210     C     nad3251       IFNE      '0'
376400090210     C     '  '          SCAN      nad3251       LENGHT            2 0
376500090210     C                   SUB       1             LENGHT
376600090210     C     LENGHT        IFLE      5
376700090210     C     LENGHT        ANDGT     0
376800090210     C                   Z-ADD     LENGHT        T                 2 0
376900090210     C                   Z-ADD     5             S                 2 0
377000090210     C                   MOVEA     nad3251       CAP9
377100090210     C                   MOVEL     *ZEROS        CAP5
377200090210     C                   DO        LENGHT
377300090210     C                   MOVE      CAP9(T)       CAP5(S)
377400090210     C                   SUB       1             S
377500090210     C                   SUB       1             T
377600090210     C                   END
377700090210     C                   MOVEA     CAP5          WCAP5             5
377800090210     C     WCAP5         IFEQ      '0'
377900090210     C                   MOVEL     *BLANKS       WCAP
378000090210     C                   ELSE
378100090210     C                   MOVEA     CAP5          WCAP
378200090210     C                   END
378300090210     C*
378400090210     C                   ELSE
378500090210     C                   MOVEL     nad3251       WCAP
378600090210     C                   END
378700090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
378800090210     C     WCAP          IFNE      *BLANKS
378900090210     C     WFLCPF        ANDNE     0
379000090210     C                   MOVEL     WFLCPF        WELE              1 0
379100090210     C                   MOVE      WFLCPF        WPOS              1 0
379200090210     C                   MOVEL     *BLANK        WCPF
379300090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
379400090210     C                   ELSE
379500090210     C                   MOVEL     *BLANKS       WCPF
379600090210     C                   END
379700090210     C                   END
379800090210     C*
379900090210     C                   ENDSR
380000090211      * ?================================================================== */
380100090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
380200090211      * ?================================================================== */
380300090211     C     Scrive_VAB    BEGSR
380400090211      *
380500090211     c                   clear                   err_bloccante     1
380600090211     c                   move      'S'           Almeno_Uno
380700090211      *
380800090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
380900090211     c                   exsr      UPDVAB
381000090211      *
381100090211     C* >>>>>>>>>>>>
381200090211     C*  Richiamo pgm per scrittura immediata VAB
381300090211     C     §PUWRK        IFEQ      ' '
381400110110      ******
381500090211     C*  Richiamo pgm per esecuzione stampa
381600110110     C**** §PTCMR        IFEQ      'S'
381700110110     c******             MOVEL     BGM_nrCMR     d86CMR
381800110110     c******             if            RFF_NrCMR <> *Blank
381900110110     C******             MOVEL     RFF_NrCMR     d86CMR
382000110110     c******             end
382100110110     C******             ELSE
382200110110     C******             MOVEL     BGM_NrFviaggiod86CMR
382300110110     c******             if          RFF_NrFviaggio <> *Blank
382400110110     C******             MOVEL     RFF_NrFviaggiod86CMR
382500110110     c******             end
382600110110     C******             END
382700110110      *
382800110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
382900110110     c                   MOVEL     vabCMR        d86CMR
383000090211      *
383100090213     C                   MOVEL     ' '           d86RIE
383200090213     C                   MOVEL     §PUDTA        d86DTA
383300090213     C                   Z-ADD     1             d86CNT
383400090213     C                   Z-ADD     §PTKSC        d86CCM
383500090213     C                   MOVEL     *BLANKS       d86STA
383600090213     C                   MOVEL     'E'           d86MOD
383700090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
383800090211      * dove poi imposta la DS dei parametri passati
383900090213     C                   MOVEL     'LR'          d86CHI
384000090213     C                   MOVEL     'N'           d86CTL
384100090211      *
384200090211     c                   move      kpjbu         SvKpjbu
384300090211     c                   clear                   kpjbu
384400090216     C                   movel     *on           Commit_on         1
384500090213     C                   MOVEL     TRTC86ds      KPJBU
384600090213      * ?- Chiamata la TRTC86R2 -*
384700090213     C                   CALL      'TRTC86R2'
384800090211     C                   PARM                    KPJBA
384900090216     C                   PARM                    Commit_on
385000090211     c                   move      Svkpjbu       Kpjbu
385100090211      *
385200090211     C*  Controllo pgm per scrittura immediata VAB
385300100630     C     sk            IFNE      0
385400100630     C                   DO        sk            sx
385500100630     C                   Z-ADD     skCLienti(sx) d86CCM
385600090211      *
385700090211     c                   move      kpjbu         SvKpjbu
385800090211     c                   clear                   kpjbu
385900090216     C                   movel     *on           Commit_on         1
386000090213     C                   MOVEL     TRTC86ds      KPJBU
386100090213      * ?- Chiamata la TRTC86R2 -*
386200090213     C                   CALL      'TRTC86R2'
386300090211     C                   PARM                    KPJBA
386400090216     C                   PARM                    Commit_on
386500090211     c                   move      Svkpjbu       Kpjbu
386600090211     C                   END
386700090211     C                   END
386800090211      *
386900090211     C                   END
387000090211      * >>>>>>>>>>>>
387100090211     C                   if        se_errore  = 'S'
387200090211     c                   move      'S'           err_bloccante
387300090211     c                   goto      Error_VAB
387400090211     c                   end
387500090211     C*
387600090211     C     Error_VAB     Endsr
387700090211     C*----------------------------------------------------------------
387800090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
387900090211     C*----------------------------------------------------------------
388000090211     C     UPDVAB        BEGSR
388100090211      *
388200090211     C*  Se non è stato impostato il codice bolla lo imposto io
388300090211     C     VABCBO        IFEQ      *BLANKS
388400090211     C     VABCAS        IFGT      0
388500090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
388600090211     C                   ELSE
388700090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
388800090211     C                   END
388900131025      * se però è stato scritto e non era considerato il Contrassegno
389000131025      *  per motivi di lettura del record MOA successiva al Tipo Bolla
389100131025      *   qui sistema il codice bolla
389200131025     C                   Else
389300131025      *  Porto Franco +COD
389400131025     c                   if        VABCBO = '1' and  VABcas > *zeros
389500131025     C                   movel     '4'           VABCBO                         + C/Ass
389600131025      *  Porto Assegnato +COD
389700131025     c                   elseIF    VABCBO = '2' and  VABcas > *zeros
389800131025     C                   movel     '6'           VABCBO                         + C/Ass
389900131025     C                   ENDif
390000131025     C                   END
390100100701      * Imposta le NOte
390200100701     C     VABNOT        IFEQ      *BLANKS
390300100701     C                   MOVEL     Note_1        VABNOT
390400100701     C                   MOVEL     Note_2        VABNT2
390500100701     C                   ELSE
390600100701     C                   MOVEL     Note_1        VABNT2
390700100701     C                   END
390800090211      *
390900090211     C*  Attribuisco anno spedizione
391000100630     C     DTM_DtParten  IFGT      0                                            anno sped.
391100100630     C                   MOVEL     DTM_DtParten  VABAAS
391200090211     C                   ELSE
391300100630     C     DTM_DtFViag   IFGT      0
391400100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
391500090211     C                   END
391600090211     C                   END
391700090211      *
391800090211      *  Controllo dettaglio segnacolli
391900090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
392000100630     C                   EXSR      CTRsegn_EEX
392100090211     C                   ELSE
392200090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
392300090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
392400100630     C                   EXSR      CTRsegn_BAR
392500090211     C                   END
392600090211     C                   END
392700090211      *
392800090212      *  Imposto linea partenza e serie
392900100701     C                   eval      VABLNP = PT_vabLNP
393000100701     C                   eval      VABNRS = PT_vabNRS
393001160209      *
393002160209      *  e la Filiale di GESTIONE
393003160209     c                   if        PS_vabFGS  <> *blank
393005160209     C                   MOVE      PS_vabFGS     VABfgs
393006160209     c                   else
393007160209     C                   z-add     VABlnp        VABfgs
393008160209     c                   end
393100090211      *
393200090211      *  Controllo se devo variare il codice trattamento merce
393300100701     C                   eval      VABCTM = PT_vabCTM
393400090211      *
393500090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
393600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
393700090211     c                   clear                   mantiene_nsp      1            *blk/'S'
393800100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
393900090211      *
394000090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
394100090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
394200090211      *
394300090211      *  Imposto codice cliente
394400090211     c                   Select
394500090211      *
394600100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
394700100319     C                   When        Det_RFF_FF_ksc  <> *zeros
394800100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
394900100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
395000090211      * forza LNP/CTM/NRS
395100100319     c                   if        Det_RFF_FF_lnp <> 0
395200100319     C                   eval      VABLNP = Det_RFF_FF_lnp
395300090211     c                   end
395301160209     c                   if        Det_RFF_FF_fgs <> *blank
395302160209     C                   move      Det_RFF_FF_fgsVABFGS
395303160209     c                   else
395304160209     c                   if        Det_RFF_FF_lnp <> 0
395306160209     C                   eval      VABfgs = Det_RFF_FF_lnp
395307160209     c                   end
395308160209     c                   end
395400140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
395500140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
395600140901     c*******            if        Det_RFF_FF_nrs <> 0
395700100319     C                   eval      VABNRS = Det_RFF_FF_nrs
395800140901     c*******            end
395900100319     c                   if        Det_RFF_FF_ctm <> *blank
396000100319     C                   eval      VABCTM = Det_RFF_FF_ctm
396100090211     c                   end
396200110408     c                   if        Det_RFF_FF_tic <> *blank and VABCAS <>0
396300110408     C                   eval      VABTIC = Det_RFF_FF_tic
396400110408     c                   end
396500110408      *
396600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
396700100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
396800100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
396900090211     c                   end
397000090211      *
397100100319      *  Specificato codice da qualificatore RFF+FF dalla testata
397200100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
397300100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
397400100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
397500090211      * forza LNP/CTM/NRS
397600100319     c                   if        Tes_RFF_FF_LNP  <> 0
397700100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
397800090211     c                   end
397801160209     c                   if        Tes_RFF_FF_fgs <> *blank
397802160209     C                   move      Tes_RFF_FF_fgsVABFGS
397803160209     c                   else
397804160209     c                   if        Tes_RFF_FF_lnp <> 0
397805160209     C                   eval      VABfgs = Tes_RFF_FF_lnp
397806160209     c                   end
397807160209     c                   end
397900140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
398000140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
398100140901     c*******            if        Tes_RFF_FF_NRS  <> 0
398200100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
398300140901     c*******            end
398400100319     c                   if        Tes_RFF_FF_CTM  <> *blank
398500100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
398600090211     c                   end
398700110408     c                   if        Tes_RFF_FF_tic  <> *blank and VABCAS <>0
398800110408     C                   eval      VABTIC = Tes_RFF_FF_tic
398900110408     c                   end
399000090211      *
399100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
399200100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
399300100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
399400090211     c                   end
399500090211      *
399600090211      *  Imposto dati da tabella - PT-
399700090211     C                   Other
399800090211      *
399900090211     C                   MOVEL     §PTKSC        VABCCM
400000090211     C                   MOVEL     §PTCTR        VABCTR
400100090211      *
400200090211     C                   Endsl
400300090211      *
400400110408      *  attenzione il tipo Contrassegno solo se c'è un valore
400500110408     c                   if        vabCAS = 0
400600110408     c                   clear                   vabTIC
400700110623     c                   else
400800110623      *
400900110623      * Se non è stato passato il Tipo incasso a fronte di particolarità sullo
401000110623      *  specifico cliente/partner
401100110707     c                   if        inviato_tipo_incasso =  'N' and
401200110707     c                             Det_RFF_FF_ksc > *zeros     and
401300110707     c                             Det_RFF_FF_tic = *blank
401400110623      *  e se c'è "??" sul tipo incasso deve Forzare questo tipo di Incasso sulla
401500110623      *  spedizione.
401600110623     c                   z-add     1             Y
401700110623     c                   movel(p)  '??'          Work_Data__35
401800110623     c                   move      §puTC         Work_Data__35
401900130311     c                   movel     Work_Data__35 WData_35_2        2
402000110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
402100110623     c                   if         *in33
402200110623     c                   eval      vabTIC = TipoIncasso(Y)
402300110623     c                   end
402400110623      **
402500110623     c                   end
402600110623      **
402700110623     c                   end
403200090211      *
403300090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
403400090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
403500090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
403600090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
403700090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
403800090211      *
403900090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
404000090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
404100090211     c                   else
404200090211      *
404300090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
404400090211     C*  lo attribuisco io
404500090211      **              **------------------**
404600090211     c                   exsr      repnum
404700090211      **              **------------------**
404800100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
404900090211      **              **------------------**
405000090211     c                   end
405100090211      *
405200090211     C*  Altrimenti attribuisco data del giorno
405300090211     C     VABMGS        IFEQ      0
405400090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
405500090211     C                   MOVE      WOGGI         VABMGS                         = oggi
405600090211     C                   END
405700090211     C*  Eseguo posizionamento su EDiVAB
405800090211     C                   Z-ADD     VABAAS        KAAS
405900090211     C                   Z-ADD     VABlnp        KLNP
406000090211     C                   Z-ADD     VABNRS        KNRS
406100090211     C                   Z-ADD     VABNSP        KNSP
406200090211     C                   MOVEL     SAVBOL        SALVA
406300090211     C     KVAB1         CHAIN     EDiVAB1L                           30
406400090211     C                   MOVEL     SALVA         SAVBOL
406500090211     C*  Imposto dati CMR
406600090211     C                   Z-ADD     1             VABCNT
406700090211     **
406800090211      *** Attenzione se si deve trattare il VAX con il CMR
406900090211      *** il campo VABCNT deve essere sempre impostato a (1)
407000090211     C     §puWRK        ifEQ      'S'
407100090211     C     §puNSP        andEQ     'S'
407200090211     C                   Z-ADD     1             VABCNT
407300090211     c                   end
407400090211     **
407500100630     C                   Z-ADD     Data_prepara  VABDTS
407600090212     C                   movel     '20'          VABDTS
407700100113     ** Forza la Data di Oggi
407800100113     C                   z-add     WOggi         VABDTS
407900100113     **
408000100630     C     Ora__prepara  mult      100           VABHMS
408100090211     C     §PTCMR        IFEQ      'S'
408200100630     C                   Z-ADD     DTM_DtCMR     VABDCM
408300100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
408400100701     c                   if          RFF_NrCMR <> *Blank
408500100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
408600100701     c                   end
408700090211     C                   ELSE
408800100630     C                   Z-ADD     DTM_DtFViag   VABDCM
408900100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
409000100701     c                   if          RFF_NrFviaggio <> *Blank
409100100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
409200100701     c                   end
409300090211     C                   END
409400100120      *
409500100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
409600100120      * accorpare la bolle ad un unico destinatario da più parti
409700100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
409800100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
409900120629     C                   if         NAD_SF_Cliente <> *blank
410000120629     c                   eval      VABCMR = NAD_SF_Cliente
410100100708     c                   else
410200100706     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z') +
410300100706     c                                      '_' +  VABRMO
410400100708     c                   end
410500100706     c                   end
410600100120      *
410700090211     C*  Se non è indicato il nome del mittente = Partner mittente
410800090211     C     VABRMO        IFEQ      *BLANKS
410900090211     C                   MOVEL     ACORAG        VABRMO
411000090211     C                   MOVEL     INDCAP        VABCMO
411100090211     C     INDCAE        IFNE      *BLANKS
411200090211     C                   MOVEL     INDCAE        VABCMO
411300090211     C                   END
411400090211     C     INDSTA        IFNE      *BLANKS
411500090211     C                   MOVEL     INDSTA        VABNMO
411600090211     C                   END
411700090211     C                   END
411800090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
411900090211     C     §1BFG1        IFEQ      'N'
412000090211     C     §1BFG7        ANDEQ     'N'
412100090211     C     §1BFG2        ANDEQ     'N'
412200090211     C***                  Z-ADD0         VABNRS
412300090211     C                   END
412400090211     C*  Imposto segancollo da - a
412500090211     C     §1BFG1        IFEQ      'S'
412600090211     C     §1BFG7        ANDEQ     'N'
412700090211     C     §1BF12        ANDNE     'E'
412800090211      *
412900100701     C     Conta_Segn    IFEQ      *ZEROS
413000090211     C                   CLEAR                   VABNCD
413100090211     C                   CLEAR                   VABNCA
413200090211     C                   ELSE
413300100630     C                   Z-ADD     segn_BAR(1)   VABNCD
413400100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
413500090211     C                   ENDIF
413600090211     C*
413700090211     C                   END
413800101027     C*
413900090211     C*  Se cap mittente originale a blank abblenco nazione
414000090211     C     VABCMO        IFEQ      *BLANKS
414100090211     C                   MOVEL     *BLANKS       VABNMO
414200090211     C                   END
414300100701     C*
414400090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
414500090603     C* Se in testata
414600100701     c                   if        NAD_destinatario_in_testata = 'S' and
414700100701     c                             NAD_destinatario_in_dettaglio <> 'S'
414800090603     c                   eval         VABrsd =  tes_VABrsd
414900090603     c                   eval         VABrd2 =  tes_VABrd2
415000090603     c                   eval         VABind =  tes_VABind
415100090603     c                   eval         VABlod =  tes_VABlod
415200090603     c                   eval         VABnzd =  tes_VABnzd
415300090603     c                   end
415400090603     C* Se in testata
415500100701     c                   if        NAD_mittente_in_testata = 'S' and
415600100701     c                             NAD_mittente_in_dettaglio <> 'S'
415700090603     c                   eval         VABrmo = tes_VABrmo
415800090603     c                   eval         VABnmo = tes_VABnmo
415900090603     c                   eval         VABcmo = tes_VABcmo
416000090603     c                   end
416100100921      *
416200101027      *   Forza il numero passato nel CNI nel riferimento Numerico
416300101027     c                   if        §PVCNIRMN = 'S'
416400101027     c                   z-add     CNI_SeNumericovabRMN
416500101027     c                   end
416600101027      *
416700100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
416800100921     C     §puWRK        ifEQ      'S'
416900100921     C     §puraggDES    andeq     'S'
417000100921     c                   clear                   vabLNA
417100100921     c                   clear                   vabZNC
417200100921     c                   clear                   vabCMR
417300100921      *  il nome identico
417400100921     c                   eval      VABCMR  = §punomeCMR
417500100921      *   e in più la data del giorno
417600100921     C     §puinGIO      ifEQ      'S'
417700100921     c                   eval      VABCMR = %trim(VABCMR)
417800100921     c                                    + %editc(WOggi_CMR:'X')
417900100921     c                   end
418000100921      *
418100100921     c                   end
418200111017      *
418300111017      *? ------------------------------------------------------------------------
418400111017      *?  SE si tratta di un Partner che invia un reso e mette nel RIF.MITT.NUMERICO
418500111017      *?   il nostro codice bolla BRT. esegue un controllo automatico su esistenza
418600111017      *?   della bolla precedentemente inviata come EXPORT....ed ora in RESO.
418700111017      *? ------------------------------------------------------------------------
418800111028      ****
418900111028      **** se nel Riferimento Numerico viene passato il riferimento di una
419000111028      ****  nostra bolla di export precedentemente inviata di cui si sta facendo
419100111028      **** il reso. (Vedi AZKAR)    controlla
419200111028      **
419300111028     c                   if        PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
419400111028      *
419500111028      * controlla che contenga un numero di 14 cifre valide
419600111028      *   quindi la prima a sinistra deve essere (0)
419700111028     c                   movel     vabrmn        uno0              1 0
419800111028     c                   if        uno0 = 0
419900111028      *
420000111028      *   poi la seconda da sinistra deve NON essere (0)
420100111028     c                   movel     vabrmn        due00             2 0
420200111028     c                   move      due00         uno0
420300111028     c                   if        uno0 > 0
420400111028      *
420500140610     C                   move      VABRMN        dsspe
420600111028     c                   exsr      ctrl_seERAexp
420700111028      *
420800111028     c                   if           Sped_RESO = 'S'
420900150330     c                   move      vabrmn        Cod_RESO_ORM
421000111028     c                   exsr      Write_RESO
421100111028     c                   end
421200111028      *
421300111028     c                   end
421400111028     c                   end
421500111028     c                   end
421600110623      *
421700140610      *? ------------------------------------------------------------------------
421800140610      *?  SE però il PARTNER è molto particolare ed utilizza un suo traffico IMPORT
421900140610      *?   come cliente per restituire i RESI o le BOLLE generate da nostri ORM export
422000140610      *?  il caso: FR-EXPRESS - CALBER:ZZ e NON vuole utilizzare il RFF+CU, si passa
422100140610      *?  a questa tecnica intercettando il segmento voulto dal Partner.
422200140610      *? ------------------------------------------------------------------------
422300140610      ****
422400140610      **** se quindi il codice identificativo arriva carico
422500140610      ****  si verifica che sia un RESO oppure un ORM.
422600140610      **
422700140610      **  DEVE comunque essere abilitato a riceverlo mediante la "PT"
422800140623     c                   if        salva_REF_ORM <> *blank  and
422900140610     c                             PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
423000140610      **
423100140919      **  Attenzione il codice può arrivare vuoto per problemi di caratteri
423200140919      **   inviati sposizionati dal Partner.
423300140919     C                   if          Cod_RESO_ORM <> *blank
423400140610     C                   move      Cod_RESO_ORM  dsspe
423500140610     c                   exsr      ctrl_seERAexp
423600140919     c                   end
423700140610      *
423800140610     c                   if           Sped_RESO = 'S'
423900140610     c                   exsr      Write_RESO
424000140610     c                   else
424100140610     c                   exsr      Write_da_ORM
424200140610     c                   end
424300140610      *
424400140610     c                   end
424500140610      *? ------------------------------------------------------------------------
424600120614      *?  PARTICOLARITA'
424700140610      *? ------------------------------------------------------------------------
424800120614      * x Consegna ai PIANI "FLOOR" codice "FLR"
424900120614     c                   if        FTX_ai_piani = 'P'
425000120614     c                   If         VABTC1 = *blank
425100120614     C                   MOVEL     'P'           VABTC1
425200120614     c                   elseIF     VABTC2 = *blank
425300120614     C                   MOVEL     'P'           VABTC2
425400120614     C                   END
425500120614     c                   End
425600100921      *
425700101027      *? ------------------------------------------------------------------------
425800101027      *?  PARTICOLARITA' sui Clienti Partners x scrittura VAB  (C H I O D I)
425900101027      *? ------------------------------------------------------------------------
426000101027     C*
426100101027     c                   exsr      Chiodi_VAB
426200101027     C*
426300090603      *
426400090211      *? ------------------------------------------------------------------------
426500090213     C   30              WRITE     EDiVAB00                             99
426600090213     C  N30              UPDATE    EDiVAB00                             99
426700090211      *? ------------------------------------------------------------------------
426800090213     c   99              eval      errori_in_Wrt = 'S'
426900090211     C*
427000090211     c                   clear                   wri_vabtic        1
427100090211     C*
427200090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
427300100706     C                   EXSR      Scrive_Riferim
427400090211     C*
427500090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
427600090211     C     §1BF12        IFEQ      'E'
427700100706     C                   EXSR      Scrive_VAT
427800090211     C                   ELSE
427900090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
428000090211     C     §1BFG7        IFEQ      'S'
428100090211     C     §1BFG1        OREQ      'S'
428200100706     C                   EXSR      Scrive_VAD
428300090211     C                   END
428400090211     C                   END
428500090211      *
428600090211     C* Reimposto codice trattamento merce cliente
428700100628     C                   MOVEL     sav_DS1B      DS1B
428800090211     C**
428900090211     C* Do errore se previsto CMR ma non c'è
429000090211     C     §PTCMR        IFNE      ' '
429100090211     C                   EXSR      MEMCMR
429200090211     C                   END
429300090211     C* Do errore se previsto AFB ma non c'è
429400090211     C     §PTNFV        IFNE      ' '
429500090211     C                   EXSR      MEMAFB
429600090211     C                   END
429700090211     C*  Se il cliente vuole il ritorno delle date di consegna
429800090211     C*  è c'è una squdratura fra i segnacolli e il numero
429900090211     C*  dei colli che ci ha passato scrivo EDSUM
430000100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
430100090211     C                   EXSR      WRTSUM
430200090211     C                   END
430300090211     C*
430400090211     C                   ENDSR
430500090211     C*----------------------------------------------------------------
430600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
430700090211     C*----------------------------------------------------------------
430800100630     C     CTRsegn_BAR   BEGSR
430900090211     C*
431000090211     C                   MOVEL     'N'           WNSGER            1
431100090211     C*
431200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
431300100701     C     VABNCL        IFNE      Conta_Segn
431400090211     C                   MOVEL     'S'           WNSGER
431500100630     C                   eval       Squadratura_Colli = 'S'
431600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
431700090211     c                              bollettati'
431800090211     C                   GOTO      FINSGN
431900090211     C                   END
432000100706      *
432100090211     C*  Riordino i segnacolli
432200090211     C     §1BFG7        IFEQ      'N'
432300100630     C                   SORTA     segn_BAR
432400090211     C*  Controllo se sono sequenziali
432500090211     C                   MOVEL     'N'           WNOSEQ            1
432600100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
432700100706     C*
432800100701     C     2             DO        Conta_Segn    X
432900100630     C     segn_BAR(X)   IFNE      WEXSGN
433000090211     C                   MOVEL     'S'           WNOSEQ
433100090211     C                   END
433200100630     C     segn_BAR(X)   ADD       1             WEXSGN
433300090211     C                   END
433400100706     C*
433500090211     C                   END
433600090211     C*
433700090211     C     FINSGN        ENDSR
433800090211     C*----------------------------------------------------------------
433900090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
434000090211     C*----------------------------------------------------------------
434100100630     C     CTRsegn_EEX   BEGSR
434200090211     C*
434300090211     C                   MOVEL     'N'           WNSGER            1
434400090211      *
434500090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
434600090211      *
434700100701     C     VABNCL        IFNE      totale_PCI
434800090211     C                   MOVEL     'S'           WNSGER
434900100630     C                   eval       Squadratura_Colli = 'S'
435000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
435100090211     c                              bollettati'
435200090211     C                   END
435300090211      *
435400090211     C                   ENDSR
435500090211     C*----------------------------------------------------------------
435600090211     C*? ESEGUO SCRITTURA EDiVAD
435700090211     C*----------------------------------------------------------------
435800100706     C     Scrive_VAD    BEGSR
435900090211     C*
436000100701     C* Se non seq. scrivo i record
436100090211     C     §1BFG1        IFEQ      'S'
436200100701     C     Conta_Segn    ANDNE     *ZEROS
436300090211     C*
436400090211     C     §1BFG7        IFEQ      'S'
436500100701     C                   DO        Conta_Segn    X
436600090211     C                   Z-ADD     VABCCM        KCCM
436700100630     C                   Z-ADD     segn_BAR(X)   KNCD
436800090211     C                   Z-ADD     VABCNT        VADCNT
436900090211     C                   MOVEL     VABCMR        VADCMR
437000090211     C                   Z-ADD     VABAAS        VADAAS
437100090211     C                   Z-ADD     VABLNP        VADLNP
437200090211     C                   Z-ADD     VABNRS        VADNRS
437300090211     C                   Z-ADD     VABNSP        VADNSP
437400090211     C                   Z-ADD     1             VADNCL
437500100630     C                   MOVEL     segn_BAR(X)   VADCDP
437600100630     C                   Z-ADD     segn_BAR(X)   VADNCD
437700090211     C                   Z-ADD     *ZEROS        VADNCA
437800090211     C                   Z-ADD     VABCCM        VADCCM
437900090211     C                   movel     VABfgs        VADfgs
438000090213     C                   WRITE     EDiVAD00                             99
438100090213     c   99              eval      errori_in_Wrt = 'S'
438200090211     C                   END
438300090211     C* Se sequenziali scrivo un solo record
438400090211     C                   ELSE
438500090211     C                   Z-ADD     VABAAS        VADAAS
438600090211     C                   Z-ADD     VABLNP        VADLNP
438700090211     C                   Z-ADD     VABNRS        VADNRS
438800090211     C                   Z-ADD     VABNSP        VADNSP
438900090211     C                   Z-ADD     VABNCL        VADNCL
439000100630     C                   Z-ADD     segn_BAR(1)   VADNCD
439100100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
439200090211     C                   Z-ADD     VABCCM        VADCCM
439300090211     C                   movel     VABfgs        VADfgs
439400140902     C                   MOVEL     VABCMR        VADCMR
439500090213     C                   WRITE     EDiVAD00                             99
439600090213     c   99              eval      errori_in_Wrt = 'S'
439700090211     C                   END
439800090211     C*
439900090211     C                   ELSE
440000100701     C                   DO        Conta_Segn    X
440100090211     C                   Z-ADD     VABCCM        KCCM
440200100630     C                   Z-ADD     segn_BAR(X)   KNCD
440300090211     C                   Z-ADD     VABAAS        VADAAS
440400090211     C                   Z-ADD     VABLNP        VADLNP
440500090211     C                   Z-ADD     VABNRS        VADNRS
440600090211     C                   Z-ADD     VABNSP        VADNSP
440700100630     C                   MOVEL     segn_BAR(X)   VADCDP
440800090211     C                   Z-ADD     1             VADNCL
440900090211     C                   Z-ADD     *ZEROS        VADNCD
441000090211     C                   Z-ADD     *ZEROS        VADNCA
441100090211     C                   Z-ADD     VABCCM        VADCCM
441200090211     C                   movel     VABfgs        VADfgs
441300140902     C                   MOVEL     VABCMR        VADCMR
441400090213     C                   WRITE     EDiVAD00                             99
441500090213     c   99              eval      errori_in_Wrt = 'S'
441600090211     C                   END
441700090211     C*
441800090211     C                   END
441900090211     C*
442000090211     C                   ENDSR
442100090211     C*----------------------------------------------------------------
442200090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
442300090211     C*----------------------------------------------------------------
442400100706     C     Scrive_RiferimBEGSR
442500090211      *
442600090211      *  riferimento di consegna destinatario
442700100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
442800100701     c                             CTA_destinatario_in_testata   <> *blank
442900090211     C                   Z-ADD     VABCNT        VATCNT
443000090211     C                   MOVEL     VABCMR        VATCMR
443100090211     C                   Z-ADD     VABCCM        VATCCM
443200090211     C                   movel     VABfgs        VATfgs
443300090211     C                   Z-ADD     VABLNP        VATLNP
443400090211     C                   Z-ADD     VABAAS        VATAAS
443500090211     C                   Z-ADD     VABNRS        VATNRS
443600090211     C                   Z-ADD     VABNSP        VATNSP
443700090211     C                   MOVEL     'A'           VATTRC
443800100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
443900100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
444000100701     c                   else
444100100701     C                   eval       VATNOT = CTA_destinatario_in_testata
444200100701     c                   end
444300090213     C                   WRITE     EDiVAT00                             99
444400090213     c   99              eval      errori_in_Wrt = 'S'
444500090211     c                   end
444600090211      *
444700090211      *  riferimento di consegna telefonico
444800100701     c                   if        COM_telefono_in_dettaglio <> *blank or
444900100701     c                             COM_telefono_in_testata   <> *blank
445000090211     C                   Z-ADD     VABCNT        VATCNT
445100090211     C                   MOVEL     VABCMR        VATCMR
445200090211     C                   Z-ADD     VABCCM        VATCCM
445300090211     C                   movel     VABfgs        VATfgs
445400090211     C                   Z-ADD     VABLNP        VATLNP
445500090211     C                   Z-ADD     VABAAS        VATAAS
445600090211     C                   Z-ADD     VABNRS        VATNRS
445700090211     C                   Z-ADD     VABNSP        VATNSP
445800090211     C                   MOVEL     'B'           VATTRC
445900100701     c                   if        COM_telefono_in_dettaglio <> *blank
446000100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
446100100701     c                   else
446200100701     c                   eval      VATNOT = COM_telefono_in_testata
446300100701     c                   end
446400090213     C                   WRITE     EDiVAT00                             99
446500090213     c   99              eval      errori_in_Wrt = 'S'
446600090211     c                   end
446700090211      *
446800090211      * riferimento del Partner che una volta veniva riportato sul BL4
446900100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
447000100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
447100100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
447200100119      *
447300100701     c                   if        RFF_RifSpediz <> *blank
447400090211     C                   Z-ADD     VABCNT        VATCNT
447500090211     C                   MOVEL     VABCMR        VATCMR
447600090211     C                   Z-ADD     VABCCM        VATCCM
447700090211     C                   movel     VABfgs        VATfgs
447800090211     C                   Z-ADD     VABLNP        VATLNP
447900090211     C                   Z-ADD     VABAAS        VATAAS
448000090211     C                   Z-ADD     VABNRS        VATNRS
448100090211     C                   Z-ADD     VABNSP        VATNSP
448200090211     C                   MOVEL     'C'           VATTRC
448300100701     C                   MOVEL     RFF_RifSpediz VATNOT
448400090213     C                   WRITE     EDiVAT00                             99
448500090213     c   99              eval      errori_in_Wrt = 'S'
448600090211     c                   end
448700090211      *
448800110907      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
448900110907      *   sulla consegna della sua merce
449000110907      *
449100110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
449200110907     c                                  <> *blank
449300110907      *
449400110907     C                   Z-ADD     VABCNT        VATCNT
449500110907     C                   MOVEL     VABCMR        VATCMR
449600110907     C                   Z-ADD     VABCCM        VATCCM
449700110907     C                   movel     VABfgs        VATfgs
449800110907     C                   Z-ADD     VABLNP        VATLNP
449900110907     C                   Z-ADD     VABAAS        VATAAS
450000110907     C                   Z-ADD     VABNRS        VATNRS
450100110907     C                   Z-ADD     VABNSP        VATNSP
450200110907     C                   MOVEL     'I'           VATTRC
450300110907     C                   eval       VATNOT =
450400110907     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
450500110907     C                   WRITE     EDiVAT00                             99
450600110907     c   99              eval      errori_in_Wrt = 'S'
450700110907     c                   end
450800110907      *
450900110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
451000110907     c                                 <> *blank
451100110907      *
451200110907     C                   Z-ADD     VABCNT        VATCNT
451300110907     C                   MOVEL     VABCMR        VATCMR
451400110907     C                   Z-ADD     VABCCM        VATCCM
451500110907     C                   movel     VABfgs        VATfgs
451600110907     C                   Z-ADD     VABLNP        VATLNP
451700110907     C                   Z-ADD     VABAAS        VATAAS
451800110907     C                   Z-ADD     VABNRS        VATNRS
451900110907     C                   Z-ADD     VABNSP        VATNSP
452000110907     C                   MOVEL     'J'           VATTRC
452100110907     C                   eval       VATNOT =
452200110907     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
452300110907     C                   WRITE     EDiVAT00                             99
452400110907     c   99              eval      errori_in_Wrt = 'S'
452500110907     c                   end
452600110907      *
452700131025      *
452800131025      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
452900131025      *   sulla consegna della sua merce
453000131025      *
453100131025     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
453200131025     c                                  <> *blank
453300131025     c                             or FTX_MAIL_come_servizio = 'N'
453400131025      *
453500131025     C                   Z-ADD     VABCNT        VATCNT
453600131025     C                   MOVEL     VABCMR        VATCMR
453700131025     C                   Z-ADD     VABCCM        VATCCM
453800131025     C                   movel     VABfgs        VATfgs
453900131025     C                   Z-ADD     VABLNP        VATLNP
454000131025     C                   Z-ADD     VABAAS        VATAAS
454100131025     C                   Z-ADD     VABNRS        VATNRS
454200131025     C                   Z-ADD     VABNSP        VATNSP
454300131025     C                   MOVEL     'S'           VATTRC
454400131025      *
454500131025      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
454600131025      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
454700131025     C                   eval       VATNOT =
454800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
454900131025      *
455000131025      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
455100131025      * seguenti attivano o meno il servizio x SMS o MAIL
455200131025      *
455300131025      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
455400131025      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
455500131025     C                   eval      %subst(VATNOT:17:1) = 'S'
455600131025     c                   if        FTX_SMS_come_servizio = 'N'
455700131025     c                              or
455800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
455900131025     c                              = *blank
456000131025     C                   eval      %subst(VATNOT:17:1) = 'N'
456100131025     c                   end
456200131025      *
456300131025     C                   eval      %subst(VATNOT:18:1) = 'S'
456400131025     c                   if        FTX_MAIL_come_servizio = 'N'
456500131025     c                              or
456600131025     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
456700131025     c                              = *blank
456800131025     C                   eval      %subst(VATNOT:18:1) = 'N'
456900131025     c                   end
457000131025      *
457100131025     C                   WRITE     EDiVAT00                             99
457200131025     c   99              eval      errori_in_Wrt = 'S'
457300131025     c                   end
457400131025      *
457500090211     C                   ENDSR
457600090211     C*----------------------------------------------------------------
457700090211     C*? ESEGUO SCRITTURA EDiVAT
457800090211     C*----------------------------------------------------------------
457900100706     C     Scrive_VAT    BEGSR
458000090211      *
458100100701     C                   DO        totale_PCI    X
458200090211     C                   Z-ADD     VABCNT        VATCNT
458300090211     C                   MOVEL     VABCMR        VATCMR
458400090211     C                   Z-ADD     VABCCM        VATCCM
458500090211     C                   movel     VABfgs        VATfgs
458600090211     C                   Z-ADD     VABLNP        VATLNP
458700090211     C                   Z-ADD     VABAAS        VATAAS
458800090211     C                   Z-ADD     VABNRS        VATNRS
458900090211     C                   Z-ADD     VABNSP        VATNSP
459000090211     C                   MOVEL     'E'           VATTRC
459100090211      *
459200090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
459300090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
459400090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
459500100701     c                   if        PU_lunVAT     > 0
459600090211      *
459700090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
459800100630     C                   eval      VATnot =
459900100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
460000090211      *
460100090211     c                   else
460200090211      *
460300090211      * Se non è impostato nulla prende il segnacollo passato così com'è
460400100630     C                   MOVEL     segn_EEX(X)   VATNOT
460500090211     c                   end
460600090211      *
460700090213     C                   WRITE     EDiVAT00                             99
460800090213     c   99              eval      errori_in_Wrt = 'S'
460900090211     C                   END
461000090211      *
461100090211     C                   ENDSR
461200090211     C*----------------------------------------------------------------
461300090211     C*? ESEGUO SCRITTURA EDSUM
461400090211     C*----------------------------------------------------------------
461500090211     C     WRTSUM        BEGSR
461600090211     C*
461700100701     C* Se non seq. scrivo i   record
461800100701     C                   DO        Conta_Segn    X
461900090211     C                   Z-ADD     VABAAS        SUMAAS
462000090211     C                   Z-ADD     VABLNP        SUMFLS
462100100701     C                   MOVEL     PT_vabCCM     SUMLNP
462200090211     C                   Z-ADD     VABLNA        SUMLNA
462300090211     C                   Z-ADD     VABZNC        SUMZNC
462400090211     C                   Z-ADD     VABNRS        SUMNRS
462500090211     C                   Z-ADD     VABNSP        SUMNSP
462600100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
462700100701     C                   Z-ADD     PT_vabCCM     SUMCCM
462800090211     C                   MOVEL     *BLANKS       SUMSTS
462900090211     C                   MOVEL     *BLANKS       SUMFLG
463000100701     C                   MOVEL     UNH_Rifer_Msg SUMCNI
463100090211     C                   MOVEL     VABCMR        SUMCMR
463200090211     C                   Z-ADD     0             SUMNFE
463300090211     C                   Z-ADD     VABCCM        VADCCM
463400090213     C                   WRITE     EDSUM000                             99
463500090213     c   99              eval      errori_in_Wrt = 'S'
463600090211     C                   END
463700090211     C*
463800090211     C                   ENDSR
463900090211     c*==================================================================*
464000090211      * Manda un Msg in Posta AS Sede a EDP*
464100090211     c*==================================================================*
464200090211     c     SEND_MSG_EDP  begsr
464300090211      *
464400090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
464500090211      *   Lo manda una sola volta per lavoro
464600090211     c                   IF        Snd_Msg_alert  <> 'N' and
464700090211     c                             User_msg <> *blank
464800090211     c                   z-add     1             Jx
464900090211     C                   exsr      CalQEZSNDMG
465000090211     c                   end
465100090211      *
465200090211     c                   endsr
465300090213     c**********************************************************************
465400090213     c* reperimento numero Spedizione
465500090213     c**********************************************************************
465600090213     C     repnum        BEGSR
465700090213      *
465800090213      *    deve controllare sulla tabella "3C" se il numero spedizione
465900090213      *     deve essere mantenuto oppure no
466000090213     C*
466100090213     C                   clear                   Ds3C
466200090213     C                   Z-ADD     CODUT         TBLKUT
466300090213     C                   MOVEL     '3C'          TBLCOD
466400090213     C                   movel(p)  VABccm        TBLKEY
466500090213     C     KTABel        CHAIN     TABEL00F                           30
466600090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
466700090213     C                   MOVEL     TBLUNI        Ds3C
466800090213     C                   END
466900090213      *
467000090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
467100090213     c                   if        §3CFsp <> 'S'
467200090213      *
467300090213     C* NSP => Stacco un numeratore da AZNUM
467400090213     c                   movel     kpjbu         svkpjbu
467500090213     c                   clear                   kpjbu
467600090213     C                   clear                   TRUL33DS
467700090213     C                   eval      I33OPE = *zeros
467800090213     C                   eval      I33CNU = 302
467900090213     C                   eval      I33NUM = 1
468000090213     C                   movel     TRUL33DS      KPJBU
468100090213     C                   call      'TRUL33R'
468200090213     C                   parm                    KPJBA
468300090213     C                   movel     KPJBU         TRUL33DS
468400090213     c                   movel     svkpjbu       kpjbu
468500090213      ****
468600090213     C                   if        O33ERR = *zeros
468700100630     c                   z-add     o33nrf        NUM_Spediz
468800090213     C                   else
468900090213     C                   endif
469000090213      ****
469100090213     c                   ELSE
469200090213      *   se si vuole mantenere il numero spedizione
469300090213      ****
469400090213     C                   MOVEL     WOGGI         WANNO             4 0
469500090213     C                   MOVE      WANNO         KAAA
469600090213     C                   Z-ADD     3             KCNU
469700090213     C                   Z-ADD     VABlnp        KFIL
469800090213     C     KNUF          CHAIN     FLNUF                              9999
469900090213     C     *IN99         IFEQ      '0'
470000090213     C                   ADD       1             NUFNUM
470100100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
470200090213     C                   UPDATE    FLNUF
470300090213     C                   ELSE
470400090213     C                   END
470500090213     C*
470600090213     c                   EndIF
470700090213      **
470800090213     C                   ENDSR
470900090213     C*----------------------------------------------------------------
471000090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
471100090213     C*----------------------------------------------------------------
471200090213     C     MEMCMR        BEGSR
471300090213     C*
471400090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
471500090213     C     §PTCM1        IFEQ      'S'
471600090213     C                   CLEAR                   EDRDE000
471700090213     C                   MOVEL     'TD1154'      KNMC
471800090213     C                   Z-ADD     1             KNRP
471900100701     C                   MOVEL     PT_vabCCM     KLNP1
472000090213     C     KRDE          CHAIN     EDRDE01L                           31
472100090213     C     *IN31         IFEQ      '1'
472200090213     C                   Z-ADD     VABAAS        RDEAAS
472300090213     C                   Z-ADD     VABLNP        RDELNP
472400090213     C                   Z-ADD     VABNRS        RDENRS
472500090213     C                   Z-ADD     VABNSP        RDENSP
472600090213     C                   MOVEL     'TD1154'      RDENMC
472700090213     C                   Z-ADD     1             RDENRP
472800100706     C                   MOVEL     BGM_nrCMR     RDEVAL
472900100701     c                   if          RFF_NrCMR <> *Blank
473000100701     C                   MOVEL     RFF_NrCMR     RDEVAL
473100100701     c                   end
473200090213     C                   WRITE     EDRDE000                             99
473300090213     c   99              eval      errori_in_Wrt = 'S'
473400090213     C                   END
473500090213     C*  Memorizzo qualificatore CMR
473600090213     C                   CLEAR                   EDRDE000
473700090213     C                   MOVEL     'TD1153'      KNMC
473800090213     C                   Z-ADD     1             KNRP
473900100701     C                   MOVEL     PT_vabCCM     KLNP1
474000090213     C     KRDE          CHAIN     EDRDE01L                           31
474100090213     C     *IN31         IFEQ      '1'
474200090213     C                   Z-ADD     VABAAS        RDEAAS
474300090213     C                   Z-ADD     VABLNP        RDELNP
474400090213     C                   Z-ADD     VABNRS        RDENRS
474500090213     C                   Z-ADD     VABNSP        RDENSP
474600090213     C                   Z-ADD     1             RDENRP
474700090213     C                   MOVEL     'TD1153'      RDENMC
474800090213     C                   MOVEL     'CMR'         RDEVAL
474900090213     C                   WRITE     EDRDE000                             99
475000090213     c   99              eval      errori_in_Wrt = 'S'
475100090213     C                   END
475200090213     C*  Memorizzo la data
475300090213     C                   CLEAR                   EDRDE000
475400090213     C                   MOVEL     'TD2380'      KNMC
475500090213     C                   Z-ADD     1             KNRP
475600100701     C                   MOVEL     PT_vabCCM     KLNP1
475700090213     C     KRDE          CHAIN     EDRDE01L                           31
475800090213     C     *IN31         IFEQ      '1'
475900090213     C                   Z-ADD     VABAAS        RDEAAS
476000090213     C                   Z-ADD     VABLNP        RDELNP
476100090213     C                   Z-ADD     VABNRS        RDENRS
476200090213     C                   Z-ADD     VABNSP        RDENSP
476300090213     C                   Z-ADD     1             RDENRP
476400090213     C                   MOVEL     'TD2380'      RDENMC
476500100630     C                   MOVEL     DTM_DtCMR     RDEVAL
476600090213     C                   WRITE     EDRDE000                             99
476700090213     c   99              eval      errori_in_Wrt = 'S'
476800090213     C                   END
476900090213     C*
477000090213     C                   END                                                    §PTCM1 = 'S'
477100090213     C*
477200090213     C                   ENDSR
477300090213     C*----------------------------------------------------------------
477400090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
477500090213     C*----------------------------------------------------------------
477600090213     C     MEMAFB        BEGSR
477700090213     C*
477800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
477900090213     C     §PTNF1        IFEQ      'S'
478000090213     C                   CLEAR                   EDRDE000
478100100701     C                   MOVEL     PT_vabCCM     KLNP1
478200090213     C                   MOVEL     'TD1154'      KNMC
478300090213     C                   Z-ADD     1             KNRP
478400090213     C     KRDE          CHAIN     EDRDE01L                           31
478500090213     C     *IN31         IFEQ      '1'
478600090213     C                   Z-ADD     VABAAS        RDEAAS
478700090213     C                   Z-ADD     VABLNP        RDELNP
478800090213     C                   Z-ADD     VABNRS        RDENRS
478900090213     C                   Z-ADD     VABNSP        RDENSP
479000090213     C                   MOVEL     'TD1154'      RDENMC
479100100701     C                   if        §PTCM1 <> 'S' or
479200100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
479300090213     C                   Z-ADD     1             RDENRP
479400090213     C                   ELSE
479500090213     C                   Z-ADD     2             RDENRP
479600090213     C                   END
479700100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
479800100701     c                   if          RFF_NrFviaggio <> *Blank
479900100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
480000100701     c                   end
480100090213     C                   WRITE     EDRDE000                             99
480200090213     c   99              eval      errori_in_Wrt = 'S'
480300090213     C                   END
480400090213     C*  Memorizzo qualificatore CMR
480500090213     C                   CLEAR                   EDRDE000
480600100701     C                   MOVEL     PT_vabCCM     KLNP1
480700090213     C                   MOVEL     'TD1153'      KNMC
480800090213     C                   Z-ADD     1             KNRP
480900090213     C     KRDE          CHAIN     EDRDE01L                           31
481000090213     C     *IN31         IFEQ      '1'
481100090213     C                   Z-ADD     VABAAS        RDEAAS
481200090213     C                   Z-ADD     VABLNP        RDELNP
481300090213     C                   Z-ADD     VABNRS        RDENRS
481400090213     C                   Z-ADD     VABNSP        RDENSP
481500100701     C                   if        §PTCM1 <> 'S' or
481600100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
481700090213     C                   Z-ADD     1             RDENRP
481800090213     C                   ELSE
481900090213     C                   Z-ADD     2             RDENRP
482000090213     C                   END
482100090213     C                   MOVEL     'TD1153'      RDENMC
482200090213     C                   MOVEL     'AFB'         RDEVAL
482300090213     C                   WRITE     EDRDE000                             99
482400090213     c   99              eval      errori_in_Wrt = 'S'
482500090213     C                   END
482600090213     C*  Memorizzo la data
482700090213     C                   CLEAR                   EDRDE000
482800100701     C                   MOVEL     PT_vabCCM     KLNP1
482900090213     C                   MOVEL     'TD2380'      KNMC
483000090213     C                   Z-ADD     1             KNRP
483100090213     C     KRDE          CHAIN     EDRDE01L                           31
483200090213     C     *IN31         IFEQ      '1'
483300090213     C                   Z-ADD     VABAAS        RDEAAS
483400090213     C                   Z-ADD     VABLNP        RDELNP
483500090213     C                   Z-ADD     VABNRS        RDENRS
483600090213     C                   Z-ADD     VABNSP        RDENSP
483700100701     C                   if        §PTCM1 <> 'S' or
483800100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
483900090213     C                   Z-ADD     1             RDENRP
484000090213     C                   ELSE
484100090213     C                   Z-ADD     2             RDENRP
484200090213     C                   END
484300090213     C                   MOVEL     'TD2380'      RDENMC
484400100630     C                   MOVEL     DTM_DtFViag   RDEVAL
484500090213     C                   WRITE     EDRDE000                             99
484600090213     c   99              eval      errori_in_Wrt = 'S'
484700090213     C                   END
484800090213     C*
484900090213     C                   END                                                    §PTCM1 = 'S'
485000090213     C*
485100090213     C                   ENDSR
485200090213     c*==================================================================*
485300090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
485400090213     c*==================================================================*
485500090213     c     Imp_ADDR_mail begsr
485600090213      *
485700090213     c                   clear                   Indirizzi
485800090213      *
485900090213      *  Lunghezza indirizzi presenti
486000100630     c                   eval      Lunghezza_Indirizzo =
486100100630     c                                       %Len(%TrimR(Mail_Ind))
486200090213     c                   z-add     1             al_char           3 0
486300090213     c                   z-add     1             dal_char          3 0
486400090213     c                   z-add     1             tot_char          3 0
486500090213      *
486600090213     c     successivo    tag
486700090213      *
486800090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
486900090213      *   e aggiunge il dominio Bartolini + il (;) separatore
487000090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
487100090213     c                   eval      tot_char = al_char - dal_char
487200100630     c                   z-add     dal_char      dalCarattere
487300100630     c                   z-add     tot_char      xTOTcaratteri
487400090213      *
487500090213     c                   clear                   Indir_Bartol     40
487600090213     c                   clear                   Indir_Parz       20
487700100630     c                   eval      Indir_Parz =
487800100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
487900100630      *
488000090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
488100090213     c                             %Trim(bart_it)
488200090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
488300090213      *
488400100630     c                   if        al_char = Lunghezza_Indirizzo
488500090213     c                   goto      fine_indmail
488600090213     c                   else
488700090213     c                   eval      dal_char = ( al_char + 1 )
488800090213     c                   goto      successivo
488900090213     c                   end
489000090213      *
489100090213     C     fine_indmail  TAG
489200090213      *
489300090213     C                   ENDSR
489400090213     c*==================================================================*
489500090213      * Manda un Msg x E-mail
489600090213     c*==================================================================*
489700090213     c     Invio_Mail    begsr
489800090213      *
489900090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
490000090213     C* Inizializzo variabili
490100090213     C                   movel     *blanks       wrkEml          253
490200090213     C                   movel     *blanks       wrkMsg         5000
490300090213     C                   movel     *blanks       wrkOgg           44
490400090213      *
490500090213     C* Valorizzo i campi della e-m@ail - indirizzo
490600100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
490700090213     C                   eval      wrkEml = Indirizzi
490800090213     C                   eval      wrkOgg = Oggetto
490900090213     C                   eval      wrkMsg = Messaggio
491000110203      ********
491100110203     C********           call(e)   'TIS701C'
491200110203     C********           parm                    wrkEml
491300110203     C********           parm                    wrkOgg
491400110203     C********           parm                    wrkMsg
491500110203     C*
491600110203      ** *****
491700110203     C                   call(e)   'TRTCT00R2'
491800110203     C                   parm                    wrkEml
491900110203     C                   parm                    wrkOgg
492000110203     C                   parm                    wrkMsg
492100110203     C*
492200090213     C*
492300090213     C                   ENDSR
492400090603      *
492500090211     ***********************************************************************
492600090211     **
492700090211     ** Send Message (QEZSNDMG) API
492800090211     **
492900090211     ***********************************************************************
493000090211     C     CalQEZSNDMG   BEGSR
493100090211      *
493200090211     ** Invio messaggio all'utente.
493300090211     C                   EVAL      SndMg03 = msgDSP
493400090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
493500090211     C                   EVAL      SndMg05(Jx) = User_msg
493600090211     C                   EVAL      SndMg06 = Jx
493700090211     C                   CLEAR                   QUSEC
493800090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
493900090211
494000090211     C                   CALL      'QEZSNDMG'
494100090211     C                   PARM                    SndMg01
494200090211     C                   PARM                    SndMg02
494300090211     C                   PARM                    SndMg03
494400090211     C                   PARM                    SndMg04
494500090211     C                   PARM                    SndMg05
494600090211     C                   PARM                    SndMg06
494700090211     C                   PARM                    SndMg07
494800090211     C                   PARM                    SndMg08
494900090211     C                   PARM                    Qusec
495000090211     C                   PARM                    SndMg10
495100090211     C                   PARM                    SndMg11
495200090211     C                   PARM                    SndMg12
495300090211      *
495400090211     C                   ENDSR
495500100708      * ?------------------------------------------------------------------ */
495600100708      *  Controlla la trasmissione   se completa
495700100708      * ?------------------------------------------------------------------ */
495800100708     c     Check_Trasm   Begsr
495900100708
496000100708     C                   clear                   se_errore
496100100708     C                   clear                   Riga_iNIZIo       1
496200100708     C                   clear                   Riga_fINe         1
496300100708      ** primo  record
496400100708     c     *start        setll     tivin00r
496500100708      *
496600100708     c     rileggi       tag
496700100708     c                   EXSR      LEGGE_TIVIN_R
496800100708      *
496900100708     c                   if        EoFTivin <> 'S'
497000100708      *
497100100708     c                   if        dati = *blank
497200100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
497300100708      * le leggo e le escludo.
497400100708     C                   eval      vinFLG = '1'
497500100708     c                   update    Tivin000
497600100708     c                   goto      rileggi
497700100708     c                   end
497800100708      * ?              /*---------------------- */
497900100708     c                   exsr      Decod_Segmento
498000100708      * ?              /*---------------------- */
498100100708     c                   endif
498200100708      **
498300100708      ** ultimo record
498400100708     c     *hival        setll     tivin00r
498500100708     c     leggiAncora   tag
498600100708     c                   readp     tivin00r
498700100708     c                   if        %Eof(tivin00r)
498800100708     c                   move      'S'           EoFTivin
498900100708     c                   else
499000100708     c                   clear                   dati
499100100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
499200100708      *
499300100708     c                   if        dati = *blank
499400100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
499500100708      * le leggo e le escludo.
499600100708     C                   eval      vinFLG = '1'
499700100708     c                   update    Tivin000
499800100708     c                   goto      leggiAncora
499900100708     c                   end
500000100708      * ?              /*---------------------- */
500100100708     c                   exsr      Decod_Segmento
500200100708      * ?              /*---------------------- */
500300100708     c                   endif
500400100708
500500100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
500600100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
500700100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
500800100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
500900100708      * ?-----> Errore
501000100708     C                   eval      se_errore = 'S'
501100100708     c                   end
501200100708
501300100708     c                   endSR
501400111017      * ?------------------------------------------------------------------ */
501500111017     C*? CHIODI VAB  prima della scrittura del VAB Particolarità su Clienti/Partners
501600111017      * ?------------------------------------------------------------------ */
501700111017     C     Chiodi_VAB    BEGSR
501800140610      *
501900140422      *? ------------------------------------------------------------------------
502000140609      *?   Se arriva da CALBERSON (GEODIS) una spedizione riconosciuta come  RESO
502100140609      *?   o da nostro ORM la spedizione deve prendere il codice FR-EXPRESS come
502200140609      *?   Partner e non come un IMPORT da Cliente. Quindi 3330003 invece di 0493298.
502300141111      *** fino al 16/11/2014
502400141111      *? ------------------------------------------------------------------------
502500141111      *? ------------------------------------------------------------------------
502600141111      *** dal 17/11/2014
502700141111      *?   Partner GEODIS trasmette tutto con CALBER:ZZ dividendo per PLATFORM LIST
502800141111      *?    NAD+SF --> usato come su RFF+FF a livello di testata.
502900141111      *?  Quindi le bolle non sono più 0493298 ma 333 o 334 da Lyone o Parigi
503000141111      *?   vale ancora la regola dei RESI e delle bolle da ORM.
503100140609      *? ------------------------------------------------------------------------
503200141112      *
503300141112      *   RESO o da ORM  -------------------------
503400141112      * vale x 333 e 334
503500141112     c                   IF        cod_RESO_ORM <> *blank
503600141112      *
503700141112      *   LINEA 333 ------------------------------
503800141111      *     LYONE
503900140609     c                   if        vabCCM = 493298
504000141111     c                              or
504100141111     c                             vabCCM = 3330003
504200140609      * MI SERVE cod.cliente e LINEA
504300140610     c                   EVAL      vabCCM = 3330003
504400140610     c                   EVAL      vabLNP = 333
504500140610     c                   EVAL      vabFGS = 333
504600141112     c                   end
504700141112      **
504800141112      *   LINEA 334 ------------------------------
504900141111      *     PARIGI
505000141112     c                   if        vabCCM = 3340001
505100141111      * MI SERVE cod.cliente e LINEA
505200141112     c                   EVAL      vabCCM = 3340001
505300141111     c                   EVAL      vabLNP = 334
505400141112     c                   EVAL      vabFGS = 334
505500141112     c                   end
505600141112      *
505700141112      *   CLIENTE 333 e 334 ----------------------
505800141112     c                   if        vabCCM = 3330003
505900141112     c                              or
506000141112     c                             vabCCM = 3340001
506100141112      *
506200141112      *  Porto Assegnato  (Anche se lo ricevo...NON mi FIDO)
506300141112     c                   IF        VABCAS > *zeros
506400141112     C                   movel     '6'           VABCBO                         + C/Ass
506500141112     C                   ELSE
506600141112     C                   movel     '2'           VABCBO                         Senza C/Ass
506700141112     c                   end
506800141112      *
506900141112      * Se il cliente non era sulla schiera lo aggiunge
507000141112     C     VABCCM        lookup    skCLienti                              39
507100141112     C                   if        *in39 = *off
507200141112     C                   add       1             sk
507300141112     C                   z-add     VABCCM        skCLienti(sk)
507400141112     C                   END
507500141112      *
507600141112     c                   end
507700141112      *   CLIENTE 333 e 334 ----------------------
507800141112      *
507900141112     c                   endIF
508000141112      *   RESO o da ORM  -------------------------
508100141111      *
508200140609      *? ------------------------------------------------------------------------
508300140422      *? ATTENZIONE: (Chiodo PF International da VANDUUREN) su richiesta di TERESA SARACINO
508400140422      *? si deve distinguere il Mittente Originale da altri traffici ricevuti
508500140422      *?  con EDI deve essere scritto come (PF Concept Italia Srl)
508600140422      *? ------------------------------------------------------------------------
508700140422     C     vabCCM        ifeq      897778
508800150126     C     vabCCM        oreq      3250048
508900140422     C                   eval       VABRMO =
509000140422     c                             'PF CONCEPT ITALIA SRL'
509100140422     c                   end
509200140422      *
509300111017      *
509400101027      *? ------------------------------------------------------------------------
509500101027      *? ATTENZIONE: (Chiodo E.Arden)
509600101027      *?  x E.Arden o Unilever           501790 e 501791
509700101027      *? Se è un cliente e non un Partner deve togliere la Ragione Sociale
509800101027      *? originale e mettere un punto al posto della RagSoc poichè non
509900101027      *?  deve essere vuoto il campo.
510000101027      *? ------------------------------------------------------------------------
510100101027     C     VABRMO        IFne      *blank
510200110913     C     §PUTPC        andne     'P'
510300110913     C**** §PUTPC        andeq     'C'
510400101027     C     §PTKSC        ifeq      501790
510500101027     C     §PTKSC        oreq      501791
510600101027     C                   MOVEL     *BLANKS       VABRMO
510700101027     C                   MOVEL     '.'           VABRMO
510800101027     c                   end
510900101027     C                   END
511000101027      *
511100101027      *? ------------------------------------------------------------------------
511200101027      *? Chiodo da togliere a gennaio 2005 :
511300101027      *? attenzione solo x ARVATO non deve essere importato l'IMPORTO ASSICURATO
511400101027      *? come da accordi commerciali anche se lo inviano sul tracciato EDI
511500101027      *? ED ANCHE IL VOLUME dal 1/3/2005
511600101027      *? ------------------------------------------------------------------------
511700101027     C     §PTKSC        ifeq      932337
511800101027     C     VABCCM        oreq      52981
511900101027     C     VABCCM        oreq      932353
512000101027     c     VABCCM        oreq      932461
512100101027     C                   clear                   VABIAS                         IMPORTO
512200101027     C                   clear                   VABVAS                         ASSICURATO
512300101027     C                   clear                   VABVLB                         VOLUME BOLLETTATO
512400101027     c                   end
512500101027     C*
512600101027      *? ------------------------------------------------------------------------
512700101027      *? Chiodo x SFS come da richiesta del 25/5/2007:
512800101027      *? mettere sui 2 campi note:
512900101027      *?      not: DDT SUL COLLO
513000101027      *?      nt2: NON CONSEGNARE IN CASO DI AVARIA
513100101027      *? ------------------------------------------------------------------------
513200101027     C     §PTKSC        ifeq      1160950
513300101027     C     VABNOT        andeq     *blank
513400101027     C     VABNT2        andeq     *blank
513500101027     C                   eval      vabNOT = 'DDT SUL COLLO'
513600101027     C                   eval      vabNT2 = 'NON CONSEGNARE IN CASO DI AVARIA'
513700101027     c                   end
513800101027     C*
513900101027      *? ------------------------------------------------------------------------
514000101027      *? Chiodo x SEUR cliente REBELIO  del 24/9/2007:
514100101027      *?  il cliente se ha il COD vuole sempre in contanti mentre
514200101027      *?   su IFCSUM manda sempre il FTX+PAI+++CO' che noi tradurremmo come OM.
514300101027      *? Chiodo Richiesto dalla Villa con Mail il 24/9/2007
514400101027      *? ------------------------------------------------------------------------
514500101027     C     §PTKSC        ifeq      433048
514600101027     C     VABCCM        andeq     433972
514700101027     C     VABTIC        andne     *blank
514800101027     C                   eval      vabTIC = '  '
514900101027     c                   end
515000101027     C*
515100101027     C*  Per non bloccare la bollettazione
515200101027     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
515300101027     C*  o se il peso non è presente... imposto fisso il minimo 0,1
515400130315     c**********         if        vabpkb = 0
515500130315     c**********         z-add     0,1           vabpkb
515600130315     c**********         end
515700101027     C*
515800101027     C*
515900101027      *? ------------------------------------------------------------------------
516000101027      *? Chiodo x AZKAR BISA richiesta  del 01/4/2009: dalla VILLA con mail di Azkar
516100101027      *?  tutti i contrassegni devono essere intestati come "OM"
516200101027      *? Chiodo Richiesto dalla Villa con Mail il 01/4/2009
516300101027      *? ------------------------------------------------------------------------
516400101027     C     §PTKSC        ifeq      22349
516500101027     C     VABCCM        andeq     22349
516600101027     C     VABTIC        ifne      *blank
516700101027     C     VABCAS        orgt      *zeros
516800101027     C                   eval      vabTIC = 'OM'
516900101027     c                   end
517000101027     c                   end
517100110222     C*
517200110627     C*
517300110222      *? ------------------------------------------------------------------------
517400110222      *? Chiodo x Calberson Maison du Monde codice 0040712 le spedizioni devono
517500110222      *?  essere prese TUTTE CON APPUNTAMENTO - richiesta di Giorgio dal Vivo
517600110222      *? La Marca Alfredo e Carrozzo William x conoscenza del 22/2/2011
517700110222      *? ------------------------------------------------------------------------
517800110222     C     VABCCM        ifeq      40712
517900110222     c                   if        vabtc1 = *blank
518000110222     c                   eval      vabtc1 = 'A'
518100110222     c                   elseif    vabtc2 = *blank
518200110222     c                   eval      vabtc2 = 'A'
518300110222     c                   end
518400110222     c                   end
518500101027     C*
518600110509     C*
518700110509      *? ------------------------------------------------------------------------
518800110509      *? Chiodo x GEODIS MAR x i COD devono essere tutti come "OM" solo per 40566
518900110509      *? ------------------------------------------------------------------------
519000110509     C     VABCCM        ifeq      40566
519100110509     c                   if        vabcas > 0
519200110509     c                   eval      vabtic = 'A'
519300110509     c                   end
519400110509     c                   end
519500110509     C*
519600101027     C                   ENDSR
519700140609      * ?------------------------------------------------------------------ */
519800140609     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
519900140609      * ?------------------------------------------------------------------ */
520000140609     C     Write_RESO    BEGSR
520100140609      *
520200111028      *  Imposta nella descrizione del CMR l'intestazione di RESI
520300111028     c                   movel(p)  vabCMR        campo35          35
520400111028     c                   movel(p)  'RESI__'      vabCMR
520500111028     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
520600111028      *
520700111028      *  e sulla 2° parte della ragione sociale viene apposta la dicitura di RESO
520800111028     c                   move(p)   VABrsd        campo44          44
520900111028     c                   movel     '**RESO** '   campo44
521000111028     C                   movel     campo44       VABrsd
521100111028     C                   move      campo44       campo9            9
521200111028      *
521300111028      * Sposta tutta la ragione sociale di (9 carat.) anche sulla 2°ragione sociale
521400111028     c                   move(p)   VABrd2        campo44          44
521500111028     c                   movel     campo9        campo44
521600111028     C                   movel     campo44       VABrd2
521700140610      *
521800140610      *  e sulle NOTE  viene impostato "Sp.EXP:"
521900140610     c                   clear                   campo57          57
522000140610     c                   eval      campo57 = 'ExSped.' + %trim(cod_RESO_ORM)
522100140610     c                   move      VABnot        campo57
522200140610     C                   movel     campo57       VABnot
522300140610     C                   move      campo57       campo22          22
522400140610      *
522500140610      * Sposta tutta la ragione sociale di (x carat.) anche sulla 2°ragione sociale
522600140610     c                   move(p)   VABnt2        campo57
522700140610     c                   movel     campo22       campo57
522800140610     C                   movel     campo57       VABnt2
522900140610      *
523000111028      *
523100111027     c                   endSR
523200100708      * ?------------------------------------------------------------------ */
523300111027     C*? Controllo se era prima una Bolla di Export che sta rientrando
523400111027      * ?------------------------------------------------------------------ */
523500111027     C     ctrl_seERAexp Begsr
523600111027     C*
523700111027     C*   imposta la chiave per controllare la spedizione
523800111027     C*    in sede
523900111027     C                   movel     '20'          dsspe
524000111027     C*
524100111027     C                   move      SPEAAS        TASAAS
524200111027     C                   move      SPELNP        TASLNP
524300111027     C                   move      SPENRS        TASNRS
524400111027     C                   move      SPENSP        TASNSP
524500111027     C     KTAS          chain     titas30c
524600111027     c                   if        %found(titas30c)
524700111027     c                               and tasLNA >299 and tasLNA <=400
524800111027     c                   eval      era_un_export_RESO_dal_PARTNER = 'S'
524900111027     c                   eval                           Sped_RESO = 'S'
525000111027     c                   end
525100111027     C*
525200111027     C                   ENDSR
525300140610      * ?------------------------------------------------------------------ */
525400140610     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
525500140610      * ?------------------------------------------------------------------ */
525600140610     C     Write_da_ORM  BEGSR
525700140610      *
525800140610      *  Imposta nella descrizione del CMR l'intestazione di RESI
525900140610     c                   movel(p)  vabCMR        campo35          35
526000140610     c                   movel(p)  'DaORM_'      vabCMR
526100140610     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
526200140610      *
526300140623      *  e sulle NOTE  viene impostato da ORM con il codice dell'ORM generante
526400140623     c                   clear                   campo70          70
526500140623     c                   clear                   note_70          70
526600140623     c                   eval      campo70 = 'DaORM:' + %Trim(salva_REF_ORM)
526700140623     c                   eval      note_70 = %trim(VABNOT) +'/'+ %trim(VABNT2)
526800140623     c                   eval      campo70 = %Trim(campo70)+'/'+ %Trim(note_70)
526900140623     c                   eval      VABNOT = %subst(campo70:1:35)
527000140623     c                   eval      VABNT2 = %subst(campo70:36:35)
527100140610      *
527200140610     c                   endSR
527300120629      * ?------------------------------------------------------------------ */
527400120629     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
527500111027      * ?------------------------------------------------------------------ */
527600120629     C     CL_ParticolarebegSR
527700120629      *
527800120629     C     Rifer_Cliente IFNE      *BLANKS
527900120629      * ------>>>
528000120629     C                   eval      XX = 1
528100120629     C                   movel     §PTKSC        Key_CL           35
528200120629     C                   movel     Rifer_Cliente WCOM28           28
528300120629     C                   move      WCOM28        Key_CL
528400120629      *
528500120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
528600120629     C     *IN34         IFEQ      '1'
528700120629     C                   movel     Des_Tb_CL(XX) EDIDSCL
528800120629      *
528900120629      * in testata messaggio
529000120629     c                   if        Dati_di_Testata  ='S'
529100120629     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
529200120629     C                   movel     §CLtar        Tes_RFF_FF_tar    3
529300120629     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
529301160209      *-
529302160209     C                   if        §CLftx   <> *blank
529303160209     C                   move      §CLftx        Tes_RFF_FF_fgs    3
529304160209     c                   else
529305160209     C                   move      §CLlnp        Tes_RFF_FF_fgs
529306160209     c                   end
529307160209      *-
529400120629     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
529500120629     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
529600120629     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
529700120629     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
529800120629     C                   movel     §CLtic        Tes_RFF_FF_tic    2
529900120629     c                   else
530000120629      * in dettaglio Messaggio
530100120629     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
530200120629     C                   movel     §CLtar        Det_RFF_FF_tar    3
530300120629     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
530301160209      *-
530302160209     C                   if        §CLftx   <> *blank
530303160209     C                   move      §CLftx        Det_RFF_FF_fgs    3
530304160209     c                   else
530305160209     C                   move      §CLlnp        Det_RFF_FF_fgs
530306160209     c                   end
530307160209      *-
530400120629     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
530500120629     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
530600120629     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
530700120629     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
530800120629     C                   movel     §CLtic        Det_RFF_FF_tic    2
530900120629     c                   end
531000120629      *
531100120629      * imposta la LNP  e la Serie x il dettaglio specifico
531200120629     c
531300120629      *  In base al cod.trattamento merce reperisco tipo tratt.
531400120629      *    per un dettaglio specifico.
531500120629     C                   CLEAR                   DS1B
531600120629     C                   Z-ADD     1             Y                 3 0
531700120629     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
531800120629     C     *IN32         IFEQ      '1'
531900120629     C                   eval       DS1B = Des_Tb_CTM(Y)
532000120629     C                   END
532100120629      *
532200120629      * Se il cliente non era sulla schiera lo aggiunge
532300120629     C     §CLksc        lookup    skCLienti                              39
532400120629     C                   if        *in39 = *off
532500120629     C                   add       1             sk
532600120629     C                   z-add     §CLksc        skCLienti(sk)
532700120629     c                   endif
532800120629      *
532900120629     C                   ENDIF
533000120629      * ------>>>
533100120629     C                   ENDIF
533200120629      *
533300120629      * in Dettaglio  preimposto i campi rilevati dalle tabelle
533400120629      *   per clienti particolari (CL)
533500120629      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
533600120629      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
533700120629      *   successivi per altre decodifiche.
533800120629     c                   if        Dati_di_Testata <>'S'
533900120629      *
534000120629      *   se aveva un RFF+FF in dettaglio
534100120629     C                   if         Det_RFF_FF_ksc > 0
534200120629      *
534300120629     C                   eval      VABccm = Det_RFF_FF_ksc
534400120629     c                   eval      VABlnp = Det_RFF_FF_lnp
534500120629     c                   eval      VABnrs = Det_RFF_FF_nrs
534600120629     C                   eval      VABctm = Det_RFF_FF_ctm
534700120629     C                   movel     Det_RFF_FF_tarVABctr
534701160209     C                   move      Det_RFF_FF_fgsVABfgs
534800120629      *
534900120629      *   altrimenti se aveva un RFF+FF in Testata
535000120629     c                   elseif     Tes_RFF_FF_ksc > 0
535100120629      *
535200120629     C                   eval      VABccm = Tes_RFF_FF_ksc
535300120629     c                   eval      VABlnp = Tes_RFF_FF_lnp
535400120629     c                   eval      VABnrs = Tes_RFF_FF_nrs
535500120629     C                   eval      VABctm = Tes_RFF_FF_ctm
535600120629     C                   movel     Tes_RFF_FF_tarVABctr
535601160209     C                   move      Tes_RFF_FF_fgsVABfgs
535700120629      *
535800120629     c                   end
535900120629      *>>>>>>>>
536000120629     c                   ENDif
536100120629     C*
536200120629     C                   ENDSR
536300121105      *---------------------------------------------------------------*
536400121105      * CTRL caricamento schiere    PT                               -*
536500121105      *---------------------------------------------------------------*
536600121105     C     Check_PT      begsr
536700121105     C*
536800121105     c* Controllo riempimento schiera
536900121105     c                   clear                   trul0sds
537000121105     c                   eval      i0sski='PT_key'
537100121105     c                   eval      i0sele=%elem(PT_key)
537200121105     c                   eval      i0spie=x
537300121105     c                   eval      i0sfile='EDTAB00F'
537400121105     c                   eval      i0ssif=knsif
537500121105     c                   eval      i0spgm='TRTCT93R'
537600121112     c                   move      kpjbu         SvKpjbu
537700121112     c                   clear                   kpjbu
537800121105     c                   movel     trul0sds      kpjbu
537900121105     c                   call      'TRUL0SR'
538000121105     c                   parm                    kpjba
538100121112     c                   eval      kpjbu = SvKpjbu
538200121105     C*
538300121105     C                   ENDSR
538400120629      * ?------------------------------------------------------------------ */
538500090211     O*****************************************************************
538600090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
538700090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
538800090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
538900090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
