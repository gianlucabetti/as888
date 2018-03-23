000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500140212      *  Da UPLOAD          H I L T I     identico al TRTCT93R                  *
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
006501160209     D EDIDSPS       E DS
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
009200111027      **
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
032701160209     D PS_uni          S             90    DIM(%elem(PT_key))
032800121105     D TRUL0SDS      E DS
032900100701      *
033000100701     d PT_KeyXsav      s              3  0 inz(0)
033100100701     D PT_trovata      s              1    inz('N')
033200100701     D PU_key          S             35
033300100701     D PV_key          S             35
033301160209     D PS_key          S             35
033400120629     D PZ_key          S             35
033500100701     D PU_HLD_SpedVAX  S              1
033600100813     D PT_con_piu_Nazioni...
033700100813     d                 s              1
033800120629     D PZ_abilita_NAD_SF_come_RFF_FF...
033900120629     d                 s              1
034000100701      *
034001160209     D  PS_vabFGS      s              3A
034100100701     D  PT_vabLNP      s                   LIKE(vablnp)
034200100701     D  PT_vabNRS      s                   LIKE(vabnrs)
034300100701     D  PT_vabCTM      s                   LIKE(vabctm)
034400100701     D  PT_vabCCM      s                   LIKE(vabccm)
034500100701     D  PU_lunVAT      s              2s 0
034600100702     D  PU_TipoServ    s              2a
034700100702     D  PU_SpecServ    s              2a
034800111027     D  PU_in_RMN_BOLLA_EXPORT_x_RESO...
034900111027     d                 s              1
035000111027     D  era_un_export_RESO_dal_PARTNER...
035100111027     d                 s              1
035200100701      *
035300100701     D Nr_PT           S              3  0 INZ
035400100701     D savXX_PT_key    S              3  0 INZ
035500100701      *
035600100630     d  DTM_DtParten   s              8s 0
035700100630     d  DTM_DtFViag    s              8s 0
035800100630     d  DTM_DtCMR      s              8s 0
035900100630     d  DTM_OraFViag   s              4s 0
036000100630     d  DTM_OraCMR     s              4s 0
036100100702     d  DTM_DtArrivo   s              8s 0
036200100702     d  DTM_OraArrivo  s              4s 0
036300100706     d  DTM_OraParten  s              4s 0
036400100702     d  CNI_Identificativo_Bolla...
036500100702     d                 s             35A
036600101027     d  CNI_seNumerico...
036700101027     d                 s                   LIKE(vabRMN)
036800100630      *
036900100701     d  GID_aTotale    s              1A
037000100705     d  GID_ColliInAdd...
037100100705     d                 s                   like(gid722420)
037200100701      **
037300090209      *  DS x scomposizione segnacollo
037400100630     D DS_SegnBART     DS
037500100630     D  SegnBART_LNP           1      3  0
037600100630     D  SegnBART_LNA           4      6  0
037700100630     D  SegnBART_NRS           7      8  0
037800100630     D  SegnBART_NSC           9     15  0
037900100630     D  SegnBART_ZNC          16     17  0
038000100701      **
038100090209      *---------------                                                      *
038200100701     D VABtic_work     s                   LIKE(vabtic)
038300110623     d inviato_tipo_incasso...
038400110623     d                 s              1
038500100630      *
038600090603     d tes_VABrmo      s                   LIKE(VABrmo)
038700090603     d tes_VABnmo      s                   LIKE(VABnmo)
038800090603     d tes_VABcmo      s                   LIKE(VABcmo)
038900090209     C*
039000090603     d tes_VABrsd      s                   LIKE(VABrsd)
039100090603     d tes_VABrd2      s                   LIKE(VABrd2)
039200090603     d tes_VABind      s                   LIKE(VABind)
039300090603     d tes_VABlod      s                   LIKE(VABlod)
039400090603     d tes_VABnzd      s                   LIKE(VABnzd)
039500090603     C*
039600090603     d tes_Valuta      s                   LIKE(VABvca)
039700090603     C*
039800090209      *---------------------------------------------------------------------*
039900090209      * Schiere
040000090209      *---------------------------------------------------------------------*
040100090209      * Schiera per reperimento nr.seganacollo
040200100630     D segn_BAR        S              7  0 DIM(5000)
040300100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
040400100630      *
040500100630      *
040600090209      * Nr. documento alfanumerico da decodificare
040700090209     D NDA             S              1    DIM(35)
040800100630      *
040900090209      * Nr. documento alfanumerico da già decodificato
041000090209     D NDN             S              1    DIM(15)
041100100630      *
041200090209      * Messaggi di errore
041300090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
041400100630      *
041500090209      * Tabella codici trattamento merce (1B)
041600100701     D Cod_Tb_CTM      S              2    DIM(200)
041700100701     D Des_Tb_CTM      S             67    DIM(200)
041800100630      *
041900090209      * Tabella codici valuta
042000090209     D CCV             S              3    DIM(200)
042100090209     D DCV             S             89    DIM(200)
042200100630      *
042300090209      * Tabella codici nazioni
042400090209     D C15             S              3    DIM(500)
042500090209     D ISO             S              3    DIM(500)
042600090209     D CPF             S              2  0 DIM(500)
042700100630      *
042800090209      * Tabella CL --> codici clienti - tariffa
042900130305     D Cod_Tb_CL       S             35    DIM(999)
043000130305     D Des_Tb_CL       S             90    DIM(999)
043100100630      *
043200090209      * Schiere x caricamento cod.cliente  <>
043300130305     D skCLienti       S              7  0 DIM(999)
043400100630      *
043500100318      * Priorità trasporto
043600100705     D Key_TipServ     S             35    DIM(100)
043700100702     D TipoServizio    S              1    DIM(100)
043800100702      *
043900100702      * Decodifiche servizi speciali
044000100702     D SpecialServ     S             35    DIM(100)
044100100702     D Key_ServSpec    S              3    DIM(100)
044200100702     D UNI_ServSpec    S             90    DIM(100)
044300100630      *
044400090209      * Termini di consegna
044500110328     D K35_TpBolla     S             35    DIM(100)
044600110328     D Cod_TpBolla     S              3    DIM(100)
044700110328     D Decod_Porto     S              1    DIM(100)
044800110328     d Trovata_Tab_TB  S              1
044900100630      *
045000090216      * Tipo pagamento C/Assegno
045100100705     D K35_TpIncas     S             35    DIM(100)
045200100705     D TipoIncasso     S              2    DIM(100)
045300100630      *
045400090209      * Schiere x routine di conversione CAP
045500090209     D CAP5            S              1    DIM(5)
045600090209     D CAP9            S              1    DIM(9)
045700090209     D CAPM            S              1    DIM(9)
045800100630      *
045900090209      * Schiera per memorizzazione FTX ricevuti
046000090209     D QUA             S              3    DIM(100)
046100090209     D FTX             S             70    DIM(100)
046200100630      *
046300090209      * Tabelle capiconto
046400090209      * Schiere x località
046500090209     D LOD             S              1    DIM(35)
046600100630      *
046700060608      **?------------------------------------------------------------------ */
046800090601     C*? Ds Scrittura del VAT "E"
046900060608      **?------------------------------------------------------------------ */
047000060612     D XTOEBCDDS     E DS
047100060620      *
047200060608      **?------------------------------------------------------------------ */
047300100701      *
047400090209      **?------------------------------------------------------------------ */
047500090209     C* Definizione campi di work
047600090209     D kKUT            s                   LIKE(TBLKUT)
047700090209     D kCOD            s                   LIKE(TBLCOD)
047800090209     D kKEY            s                   LIKE(TBLKEY)
047900090209     D kKCC            s                   LIKE(ACOKCC)
048000090209     D kKSC            s                   LIKE(ACOKSC)
048100090209     D kAAA            s                   LIKE(NUFAAA)
048200090209     D kCNU            s                   LIKE(NUFCNU)
048300090209     D kFIL            s                   LIKE(NUFFIL)
048400090209     D kAAS            s                   LIKE(VABAAS)
048500090209     D kLNP            s                   LIKE(VABLNP)
048600090209     D kNRS            s                   LIKE(VABNRS)
048700090209     D kNSP            s                   LIKE(VABNSP)
048800090209     D kCMR            s                   LIKE(VABCMR)
048900090209     D kCCM            s                   LIKE(VABCCM)
049000090209     D kNCD            s                   LIKE(VADNCD)
049100090210     D kCNT            s                   LIKE(VADCNT)
049200090209     D kNMC            s                   LIKE(RDENMC)
049300090209     D kNRP            s                   LIKE(RDENRP)
049400090209     D kLNP1           s                   LIKE(RDELNP)
049500090209      *
049600090209      *  Invio E-mail
049700090209     D Indirizzi       s            253
049800090209     D Oggetto         s             44
049900090209     D Messaggio       s           5000
050000090209      *
050100090209     d   DSmail        ds
050200090209     D Mail_Ind                      44
050300090209     d   IND_char                     1    dim(44)
050400090209      *
050500100630     D Lunghezza_Indirizzo...
050600100630     D                 s              5u 0
050700090209      *
050800100630     D dalCarattere    s              5u 0
050900100630     D xTOTcaratteri   s              5u 0
051000090209      *
051100090209     C*---------------------------------------------------------------*
051200090209     D MsgDSP          S            256                                         Testo generico
051300090209     C*---------------------------------------------------------------*
051400090209     D SndMg01         S             10                                         Message type
051500090209     D                                     INZ('*INFO')
051600090209     D SndMg02         S             10                                         Deluvery mode
051700090209     D                                     INZ('*BREAK')
051800090209     D SndMg03         S            256                                         Message text
051900090209     D SndMg04         S             10I 0                                      Length of text
052000090209     D                                     INZ(%SIZE(SndMg03))
052100090209     D SndMg05         S             10                                         User profile
052200090209     D                                     DIM(299)
052300090209     D SndMg06         S             10I 0                                      Number of user
052400090209     D SndMg07         S             10I 0                                      Message sent indic.
052500090209     D SndMg08         S             10I 0                                      Function requested
052600090209     D SndMg10         S              1                                         Show display
052700090209     D                                     INZ('N')
052800090209     D SndMg11         S             20                                         Qualified MSGQ name
052900090209     D SndMg12         S              4                                         Name type indicator
053000090209     D                                     INZ('*USR')
053100090209      * indice di scihera
053200090209     D Jx              S              5I 0
053300090209      *
053400090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
053500090209      *
053600090209     d bart_it         C                   CONST('@Bartolini.it;')
053700090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
053800060612      * ?================================================================== */
053900060612      * ?   * Campi x decodifica * (INPUT  del Record)
054000100319      * ?================================================================== */
054100090130     D  Dati           s           2048
054200060612      * ?   *--------------------------------------------------------------*
054300060612     D  se_errore      s              1    inz(' ')
054400060612      * ?* ------------------------------------------------------ *
054500060710     D Digits          C                   '0123456789'
054600091019     D*------------------
054700091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
054800091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
054900060612      * ?================================================================== */
055000060612      *   Ciclo principale
055100090205      * ?================================================================== */
055200060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
055300060612     C*
055400060612     C                   if        not %open(tivin00r)
055500060612     C                   open      tivin00r
055600060612     C                   endif
055700060612      **
055800060612      * ?------------------------------------------------------------------ */
055900100708     C*  Controllo dati arrivati da DPD
056000060612      * ?------------------------------------------------------------------ */
056100060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
056200060612      * ?- controllando che la trasmissione sia completa.
056300090205      **
056400060612      * ?              /*---------------------- */
056500090205     c                   goto      nonFare_adesso
056600060612     c                   exsr      check_Trasm
056700090205     c     nonFare_adessotag
056800060612      * ?              /*---------------------- */
056900090205      **
057000060613      **  Errore di trasmissione x tutti i records
057100060613      **   --> file in errore
057200060613     c                   if        se_errore = 'S'
057300060612
057400060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
057500090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
057600090205     c                   exsr      Msg_errato
057700060612      **
057800060612     c                   else
057900060614      **
058000060612      * ?------------------------------------------------------------------ */
058100100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
058200060612      * ?------------------------------------------------------------------ */
058300060612      * ?              /*---------------------- */
058400060612     c                   exsr      Importa_Msg
058500060612      * ?              /*---------------------- */
058600060612
058700060614     c                   end
058800060612      *
058900060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
059000090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
059100090225     c                             esito ='0'
059200060710     C                   eval      esito ='0'
059300090225      *
059400090225     c                   COMMIT
059500110208     C                   endif
059600090224      *
059700090213     c                   if        almeno_un_due ='S'
059800110208     C                   eval      esito ='1'
059900060614     C                   endif
060000100708      * chiude TIVIN
060100100708     C                   if        %open(tivin00r)
060200100708     C                   close     tivin00r
060300100708     C                   endif
060400060614      *
060500060612     c                   SETON                                        LR
060600060612      * ?================================================================== */
060700100708      *? Importa i records della trasmissione
060800060612      * ?------------------------------------------------------------------ */
060900060612     c     Importa_Msg   Begsr
061000060614
061100130506      *   Inizia il conteggio totale delle righe
061200130506     c                   z-add     0             total_segmenti    5 0
061300100726     c                   clear                   Forza_Uscita
061400060612     c     *start        setll     Tivin00r
061500100708
061600100708     c                   EXSR      LEGGE_TIVIN_R
061700100708     c                   dow       EoFTivin <> 'S'
061800060614
061900060614      * solo i record sflaggati da rielaborare
062000110107     c                   IF        vinFLG = *blank
062100110107      *** ++++++
062200090211      *  Sempre Record OK
062300090211      ** Controlli formali sui campi
062400090211     C                   eval      vinFLG = '1'
062500060612     c                   clear                   se_errore
062600110107      *
062700110107      *** >>>>>>
062800110107     c                   IF        vinDTA <> *blank
062900110107      *** >>>>>>
063000060612
063100060612      ** Decodifica record a campi variabili
063200060612      * ?              /*---------------------- */
063300090205     c                   exsr      Decod_Segmento
063400090209      * ?              /*---------------------- */
063500060612
063600060621      *  x errore bloccante nella composizione del VAB/VAT
063700060621     c                   if        err_bloccante ='S'
063800060621     C                   eval      vinFLG = '2'
063900110208     c                   eval      Almeno_Un_Due ='S'
064000090211     c                   eval      esito  = '2'
064100100317???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
064200090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
064300090603     C                             Bolle import in filiale - messaggio -
064400090603     C                             con UNB non presente in tabella PT controlla-
064500090603     C                             re EDI ricevuto su UPLOAD.'
064600100319     c                   if        §PVinv <> 'N'
064700100319     c                   exsr      invio_mail
064800060621     c                   end
064900100319     c                   end
065000110107      *** >>>>>>
065100110107     c                   endIF
065200110107      *** >>>>>>
065300110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
065400110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
065500110107      *         un erroneo messaggio
065600110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
065700060612     c                   update    Tivin000
065800090211
065900090211      *** se non è riconosciuto l'UNB deve uscire direttamente
066000090211     c                   if        se_errore ='S'  and
066100100708     c                             tipo_seg = Segm_Inizio_Messaggio
066200090213     c                   eval      err_bloccante = 'S'
066300090211     c                   exsr      Msg_errato
066400090211     c                   LEAVE
066500090211     c                   endIF
066600110107      *** ++++++
066700110107     c                   endIF
066800060612
066900100708     c                   EXSR      LEGGE_TIVIN_R
067000060612     C                   ENDdo
067100060612      **
067200060612     c                   endSR
067300100709      * ?------------------------------------------------------------------ */
067400100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
067500100709      * ?------------------------------------------------------------------ */
067600100709     C     MSG_Errato    BEGSR
067700100709      *
067800100709      * ?  Scrive su tutti i records il tipo di errore
067900100709     c     *start        setll     tivin00r
068000100709     c
068100100709     c                   EXSR      LEGGE_TIVIN_R
068200100709     c                   dow       EoFTivin <> 'S'
068300100709     c
068400100709     c                   if        vinMSG  = *blank
068500100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
068600100709     C                               >> Controllare i DATI su UPLOAD !!'
068700100709     c                   end
068800100709     C                   eval      vinFLG = '2'
068900100709     c                   eval      Almeno_Un_Due ='S'
069000100709     c                   eval      esito  = '2'
069100100709     c                   eval      se_errore ='S'
069200100709     c                   eval      err_bloccante ='S'
069300100709      *
069400100709     c                   update    tivin000
069500100709     c                   EXSR      LEGGE_TIVIN_R
069600100709     c                   endDO
069700100709      *
069800100709     C                   ENDSR
069900100709      * ?------------------------------------------------------------------ */
070000100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
070100100709      * ?------------------------------------------------------------------ */
070200100709     C     DETta_Errato  BEGSR
070300100709      *
070400100709     c                   EXSR      LEGGE_TIVIN_R
070500100709     c                   dow       EoFTivin <> 'S'
070600100709      *
070700100709      * Se ha letto il successivo record
070800100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
070900100709     c                   if        VNREC = UNT_record  or
071000100709     c                             VNREC = finDET_RECord
071100100709     c                   leave
071200100709     c                   end
071300100709      *
071400100709     c                   exsr      Error_DET
071500100709      *
071600100709     c                   if        vinMSG  = *blank
071700100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
071800100709     C                             >> Controllare i DATI di ogni Segmento <<'
071900100709     c                   end
072000100709      *
072100100709     c                   update    tivin000
072200100709     c                   EXSR      LEGGE_TIVIN_R
072300100709     c                   endDO
072400100709      * ------
072500100709???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
072600100709     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
072700100709???? C                             Bolle IFCSUM non totalmente tradotto. -
072800100709     C                             Controllare UPLOAD del cliente:'
072900100709     C                   EVAL      Messaggio = %trim(Messaggio) +
073000100709     C                                         %editc(§PTksc:'X')
073100100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
073200100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
073300100709     c                   if        §PVinv <> 'N'
073400100709     c                   exsr      invio_mail
073500100709     c                   end
073600100709      *
073700100709      * ripulisce per un nuovo giro
073800100709     C                   clear                   Err_in_detta
073900100709      *
074000100709     C                   ENDSR
074100100709      * ?------------------------------------------------------------------ */
074200100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
074300100709      * ?------------------------------------------------------------------ */
074400100709     C     LEGGE_TIVIN_R BEGSR
074500100709      *
074600100709     c                   clear                   EoFTivin
074700100709      *  Lettura sequenziale
074800100709     c                   read      Tivin00r
074900100709
075000100709     c                   if        %Eof(tivin00r)
075100100709     c                   move      'S'           EoFTivin
075200100709     c                   else
075300100709      *
075400100709      * ?  imposta il tipo di segmento:
075500100709     c                   clear                   dati
075600100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
075700100728     c                   eval      segmento = %trimL(dati)
075800100728     c                   eval      tipo_seg = %subst(segmento:1:3)
075900100709      **
076000100709     c                   end
076100100709      *
076200100709     C                   ENDSR
076300100120      * ?------------------------------------------------------------------ */
076400100120      *?    Decodifica il segmento ed importa i campi in formato DS
076500100120      * ?------------------------------------------------------------------ */
076600100120     c     Decod_SegmentoBegsr
076700100120      **
076800100706      *** ------------------------------------
076900100706????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT"
077000100706      *** ------------------------------------
077100100706      * OGNI DETTAGLIO inizia dal segmento CNI e per ultimo il msg è chiuso da UNT.
077200100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
077300100706      **
077400100706      **  Scrive alla fine di ogni giro di dettaglio:
077500100706     c                   if           tipo_seg = Segm_Fine_Dettagli
077600100706     c                                   or
077700100706     c                                tipo_seg = Segm_Inizio_Dettaglio  and
077800100706     c                                Contatore_Dettagli > 0
077900100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
078000100708     c                   eval      FinDET_RECord = vnrec
078100100708      **?              /*---------------------- */
078200100708     c                   exsr      Scrive_Dettag
078300100708      **?              /*---------------------- */
078400100813      *
078500100813      *   Ogni nuovo Dettaglio,
078600100813      *    per quei Partner che gestiscono più LInee
078700100813      *    Mediante il Mittente del dettaglio
078800100813     c                   if        UNB_parziale ='S' and
078900100813     c                              PT_con_piu_Nazioni = 'S' and
079000100813     C                                 Naz_salvata = Naz_mittente
079100100813      * ?ReImposta i dati da tabella PT
079200100813      * ?  se fossero stati cambiati dal precedente dettaglio:
079300100813     C                   z-add     savXX_PT_key  Nr_PT
079400100813     c                   exsr      Se_Trovata_PT
079500100813     c                   endIF
079600100813      *
079700100706     c                   endIF
079800100708      *
079900100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
080000100708      **?              /*---------------------- */
080100100708     c                   exsr      Pulizia_Campi
080200100708      **?              /*---------------------- */
080300100813      *
080400100706      *** ------------------------------------
080500100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
080600100120      * ?  Anche se non utilizzati.
080700100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
080800100317     c                   clear                   seg_imprevisto
080900100720     c                   clear                   trovato_seg
081000100319      *
081100100319      *  Contatore delle righe ossia dei segmenti del messaggio
081200100319     c                   add       1             Conta_segmenti
081300130506     c                   add       1             total_segmenti
081400100708      *
081500100716     C                   clear                   keyUNBCLI
081600100716     C                   clear                   keyTIPOMSG
081700100716     C                   clear                   keyVERSION
081800100716     C                   clear                   keyRELEASE
081900100716     C                   clear                   keyAGENCY
082000100716     C                   clear                   keyASSOCIA
082100110530      **--------
082200110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
082300110530      **--------
082400110530     c                   movel     UNB_Mittente  keyUNBCLI
082500100720      **--------
082600100720      *  Ritorna la DS GENERICA oppure l'errore
082700100720      **--------
082800100720      *  Chiamata generica x decodificare il singolo segmento letto
082900100720     c                   call      'TRTCT00R1'
083000100720      * input
083100100720     c                   parm                    tipo_seg
083200100720     c                   parm                    segmento
083300100720     C                   parm                    keyUNBCLI
083400100720     C                   parm                    keyTIPOMSG
083500100720     C                   parm                    keyVERSION
083600100720     C                   parm                    keyRELEASE
083700100720     C                   parm                    keyAGENCY
083800100720     C                   parm                    keyASSOCIA
083900100720      *output
084000100720     c                   parm                    Errore
084100100720     c                   parm                    DsGenerica
084200100720     c                   parm                    Valori_inErr
084300100720     c                   parm                    Descr_Errore
084400100720     c                   parm                    trovato_seg
084500120424      *
084600120424      *>>>--->>>                                                    <<<---<<<
084700120424      *   Se ci sono dei Segmenti Particolari da NON considerare
084800120424     c                   IF        trovato_seg ='N' and unz_segmento <>'S'
084900120424      *
085000120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
085100120424     C                   movel(p)  'SGM'         etbSGM
085200120424     C                   movel(p)  Tipo_Seg      etbVALSGM
085300120424     c                   exsr      In_TAB_EDSTBL
085400120424      *
085500120424     C*  Quindi sono dichiarati nel file delle eccezioni
085600120424     C                   If        Quale_campo ='TIPSGM' and
085700120424     c                                                   trovata_EDSTBL = 'S'
085800120424     c                   eval      trovato_seg ='S'
085900120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
086000120424      * errori sul TIVIN.
086100120424     c                   endIf
086200120424     c                   end
086300120424      *>>>--->>>                                                    <<<---<<<
086400100708      *** ------------------------------------
086500100708      *   Decodifica il singolo segmento LETTO
086600100708      *** ------------------------------------
086700100120     c                   select
086800100120      *
086900110110     c                   when      trovato_seg ='N' and unz_segmento <>'S'
087000100720     c                   move      'S'           seg_imprevisto
087100100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
087200100720     c                   exsr      Error_DET
087300100720     c                   goto      end_Decod
087400100720      **--------
087500100120     c                   when      tipo_seg = 'UNA'
087600100720     c                   movel(p)  DsGenerica    EDS00UNA
087700100120      *
087800100120     c                   when      tipo_seg = 'UNB'
087900100720     c                   movel(p)  DsGenerica    EDS00UNB
088000100120     C                   EXSR      DATI_UNB
088100100120      * --------
088200100120     c                   when      tipo_seg = 'UNH'
088300100720     c                   movel(p)  DsGenerica    EDS00UNH
088400100120     C                   EXSR      DATI_UNH
088500100120      * --------
088600100120     c                   when      tipo_seg = 'BGM'
088700100720     c                   movel(p)  DsGenerica    EDS00BGM
088800100316     C                   EXSR      DATI_BGM
088900100120      **--------
089000100120     c                   when      tipo_seg = 'DTM'
089100100720     c                   movel(p)  DsGenerica    EDS00DTM
089200100120     C                   EXSR      DATI_DTM
089300100120      * --------
089400100120     c                   when      tipo_seg = 'TSR'
089500100720     c                   movel(p)  DsGenerica    EDS00TSR
089600100316     C                   EXSR      DATI_TSR
089700100120      **--------
089800100120     c                   when      tipo_seg = 'CNT'
089900100720     c                   movel(p)  DsGenerica    EDS00CNT
090000100316     C                   EXSR      DATI_CNT
090100100120      **--------
090200100120     c                   when      tipo_seg = 'TOD'
090300100720     c                   movel(p)  DsGenerica    EDS00TOD
090400100120     C                   EXSR      DATI_TOD
090500100120      **--------
090600100120     c                   when      tipo_seg = 'RFF'
090700100720     c                   movel(p)  DsGenerica    EDS00RFF
090800100120     C                   EXSR      DATI_RFF
090900100316      **--------
091000100316     c                   when      tipo_seg = 'SEL'
091100100720     c                   movel(p)  DsGenerica    EDS00SEL
091200100316     C                   EXSR      DATI_SEL
091300100316      **--------
091400100316     c                   when      tipo_seg = 'EQD'
091500100720     c                   movel(p)  DsGenerica    EDS00EQD
091600100316     C                   EXSR      DATI_EQD
091700100316      **--------
091800100316     c                   when      tipo_seg = 'EQN'
091900100720     c                   movel(p)  DsGenerica    EDS00EQN
092000100316     C                   EXSR      DATI_EQN
092100100316      **--------
092200100316     c                   when      tipo_seg = 'PIA'
092300100720     c                   movel(p)  DsGenerica    EDS00PIA
092400100316     C                   EXSR      DATI_PIA
092500100316      **--------
092600100316     c                   when      tipo_seg = 'LOC'
092700100720     c                   movel(p)  DsGenerica    EDS00LOC
092800100316     C                   EXSR      DATI_LOC
092900100316      **--------
093000100316     c                   when      tipo_seg = 'DGS'
093100100720     c                   movel(p)  DsGenerica    EDS00DGS
093200100316     C                   EXSR      DATI_DGS
093300100316      **--------
093400100120     c                   when      tipo_seg = 'TDT'
093500100720     c                   movel(p)  DsGenerica    EDS00TDT
093600100316     C                   EXSR      DATI_TDT
093700100120      **--------
093800100120     c                   when      tipo_seg = 'MOA'
093900100720     c                   movel(p)  DsGenerica    EDS00MOA
094000100120     C                   EXSR      DATI_MOA
094100100120      **--------
094200100120     c                   when      tipo_seg = 'FTX'
094300100720     c                   movel(p)  DsGenerica    EDS00FTX
094400100120     C                   EXSR      DATI_FTX
094500100120      **--------
094600100120     c                   when      tipo_seg = 'NAD'
094700100720     c                   movel(p)  DsGenerica    EDS00NAD
094800100120     C                   EXSR      DATI_NAD
094900100316      **--------
095000100316     c                   when      tipo_seg = 'CNI'
095100100720     c                   movel(p)  DsGenerica    EDS00CNI
095200100316     C                   EXSR      DATI_CNI
095300100120      **--------
095400100120     c                   when      tipo_seg = 'CTA'
095500100720     c                   movel(p)  DsGenerica    EDS00CTA
095600100120     C                   EXSR      DATI_CTA
095700100120      **--------
095800100120     c                   when      tipo_seg = 'COM'
095900100720     c                   movel(p)  DsGenerica    EDS00COM
096000100120     C                   EXSR      DATI_COM
096100100120      **--------
096200100120     c                   when      tipo_seg = 'GID'
096300100720     c                   movel(p)  DsGenerica    EDS00GID
096400100120     C                   EXSR      DATI_GID
096500100120      **--------
096600100120     c                   when      tipo_seg = 'MEA'
096700100720     c                   movel(p)  DsGenerica    EDS00MEA
096800100120     C                   EXSR      DATI_MEA
096900100120      **--------
097000100120     c                   when      tipo_seg = 'PCI'
097100100720     c                   movel(p)  DsGenerica    EDS00PCI
097200100120     C                   EXSR      DATI_PCI
097300100120      **--------
097400100120     c                   when      tipo_seg = 'UNT'
097500100720     c                   movel(p)  DsGenerica    EDS00UNT
097600100120     C                   EXSR      DATI_UNT
097700100120      **--------
097800100120     c                   when      tipo_seg = 'UNZ'
097900100720     c                   movel(p)  DsGenerica    EDS00UNZ
098000100120     C                   EXSR      DATI_UNZ
098100110105     c                   move      'S'           unz_segmento
098200110105      **--------
098300110105     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
098400100120      **--------
098500100120     c                   OTHER
098600120424      **--------
098700120424     c                   if            unz_segmento <>'S'
098800120424     c                                           and trovata_EDSTBL <>'S'
098900100120     c                   move      'S'           seg_imprevisto
099000100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
099100100120     c                   exsr      Error_DET
099200100120     c                   goto      end_Decod
099300110110     c                   end
099400100120      **--------
099500100120     c                   endSL
099600100120      **
099700100120     c     end_Decod     endSR
099800100701      * ?================================================================== */
099900100319     C*? Azzera_MSG   - Azzera dati messaggio
100000100701      * ?================================================================== */
100100100708     C     Scrive_Dettag BEGSR
100200100701      **
100300100708      *  Deve flaggare il dettaglio con dei problemi
100400100708      * Imposta le righe in errore sullo specifico dettaglio
100500100708     C                   if        Err_in_detta = 'S'
100600100708      * prima di rieseguire il ciclo
100700100708      *  rilascia il record
100800100708     c                   update    tivin000
100900100708      * ?  Scrive su tutti i records il tipo di errore
101000100708     c     IniDET_RECord setll     tivin00r
101100100708      **?              /*---------------------- */
101200100708     c                   exsr      DETta_Errato
101300100708      **?              /*---------------------- */
101400100708     c                   end
101500100708      *
101600100708      **  Se presente un errore nel record emette una segnalazione msg
101700100708???? c                   if        se_errore <> 'S'
101800100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
101900100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
102000100708     c                              NAD_Destinatario_in_Testata    ='S'
102100100708      * ?              /*---------------------- */
102200100708     c                   exsr      Scrive_VAB
102300100708      * ?              /*---------------------- */
102400100708     c                   else
102500100708     c                   eval      errori_in_Wrt = 'S'
102600100708     c                   end
102700100708      *
102800100708      *  Problemi nella decodifica dei campi VAB/VAT
102900100708     c                   if        errori_in_Wrt = 'S'
103000100708     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
103100100708     c                   exsr      Error_DET
103200100708     c                   ROLBK
103300100708     c                   else
103400100708     c                   COMMIT
103500100708     c                   end
103600100708      *
103700100708     c                   end
103800100708      **
103900100708     c     end_WRidett   endSR
104000100708      * ?================================================================== */
104100100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
104200100708      * ?================================================================== */
104300100708     C     Pulizia_Campi BEGSR
104400100708      **
104500100708      * inzio messaggio azzera tutte le variabili generali del messaggio
104600100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
104700100708      * ?                         --------------
104800100708     C                   EXSR      Azzera_MSG
104900100708      * ?                         --------------
105000100708      *
105100100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
105200100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
105300100708      * ?                         --------------
105400100708     c                   exsr      Testata_sped
105500100708      * ?                         --------------
105600100708      *
105700100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
105800100708      *  Prepara una nuova spedizione
105900100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
106000100708      * ?                         --------------
106100100708     c                   exsr      Nuova_spediz
106200100708      * ?                         --------------
106300100708     c                   end
106400100708      **
106500100708     c     end_clear     endSR
106600100708      * ?================================================================== */
106700100708     C*? Azzera_MSG   - Azzera dati messaggio
106800100708      * ?================================================================== */
106900100708     C     Azzera_MSG    BEGSR
107000100708      **
107100100701     C*  Inizializza variabili
107200100701     C                   Z-ADD     0             skCLienti
107300100701     C                   Z-ADD     0             sk
107400100701     C*
107500100701      *  INIZIALIZZA  dati fondamentali messaggio
107600100701     C                   clear                   EDIDSPT
107700100701     C                   clear                   EDIDSPU
107800100701     C                   clear                   EDIDSPV
107900120629     C                   clear                   EDIDSPZ
107901160209     C                   clear                   EDIDSPS
108000100701     C*
108100100701     C                   move      'N'           PT_trovata
108200100813     C                   clear                   savXX_PT_key
108201160209     C                   clear                   PS_vabFGS
108300100701     C                   clear                   PT_vabLNP
108400100701     C                   clear                   PT_vabNRS
108500100701     C                   clear                   PT_vabCTM
108600100701     C                   clear                   PT_vabCCM
108700100701     C                   clear                   PU_HLD_SpedVAX
108800111027     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO     = *blank
108900100813     C                   eval        PT_con_piu_Nazioni = *blank
109000100813     c                   move      *blank        Naz_salvata       3
109100090211     C*
109200100701     c                   clear                   UNB_Generico
109300100701     c                   clear                   UNB_NrTrasmiss
109400100701     c                   clear                   UNZ_NrTrasmiss
109500100708      *
109600120629     c                   clear                   NAD_SF_Cliente
109700100630      *
109800100630      * per controllare se almeno un record è stato importato sul VAB
109900100630     c                   clear                   Almeno_Uno        1
110000100630     c                   clear                   Almeno_Un_Due     1
110100100708     c                   clear                   IniDET_RECord
110200100708     c                   clear                   FinDET_RECord
110300100630     c                   eval      esito  = '0'
110400100728     c                   eval      Segmento_in_errore = 0
110500100317     C*
110600090211     C                   ENDSR
110700100701      * ?================================================================== */
110800100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
110900100701      * ?================================================================== */
111000100701     C     Testata_sped  BEGSR
111100100701      *
111200100701      *  imposta il Flag per identificare la testata
111300100701     c                   eval      Dati_di_Testata  ='S'
111400100701     C*
111500100701      *   Contatore dei dettagli
111600100701     c                   eval      Contatore_Dettagli = 0
111700100701      **
111800100701      *   Codici identificativi della Testata del messaggio
111900100701     c                   clear                   UNH_Rifer_Msg
112000100706     C* Pulisco dati di work x foglio viaggio
112100100706     C                   clear                   BGM_nrCMR                      * WNRCMR
112200100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
112300100701     c                   clear                   BGM_Id_Testata
112400100701      **
112500100701     C*  Riferimento Foglio Viaggio o CMR
112600100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
112700100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
112800100701      **   Date generali
112900100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
113000100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
113100100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
113200100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
113300100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
113400100702     C                   Z-ADD     0             DTM_DtArrivo                   *
113500100702     C                   Z-ADD     0             DTM_OraArrivo                  *
113600100701      *
113700100701      * pulisce gli RFF+FF di testata
113800100706     C                   clear                   Tes_RFF_FF_KSC
113900100706     C                   clear                   Tes_RFF_FF_TAR
114000100706     C                   clear                   Tes_RFF_FF_LNP
114001160209     C                   clear                   Tes_RFF_FF_FGS
114100100706     C                   clear                   Tes_RFF_FF_NRS
114200100706     C                   clear                   Tes_RFF_FF_CTM
114300100706     C                   clear                   Tes_RFF_FF_BS1
114400100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
114500100701     C*
114600100701     c                   clear                   tes_VABrmo
114700100701     c                   clear                   tes_VABnmo
114800100701     c                   clear                   tes_VABcmo
114900100701     C*
115000100701     c                   clear                   tes_VABrsd
115100100701     c                   clear                   tes_VABrd2
115200100701     c                   clear                   tes_VABind
115300100701     c                   clear                   tes_VABlod
115400100701     c                   clear                   tes_VABnzd
115500100701     C*
115600100701     c                   clear                   tes_Valuta
115700100702     C*
115800100702     c                   clear                   Tot_Peso_msg     15 2
115900100702     c                   clear                   Tot_Sped_msg     15 2
116000100702     c                   clear                   Tot_Colli_msg    15 2
116100100702     c                   clear                   Tot_PNetto_msg   15 2
116200100701     C*
116300100701     ** Inizializza campo di work
116400100701     c                   eval      GID_aTotale = 'N'
116500100701     C*
116600100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
116700100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
116800100701     c                   eval      NAD_destinatario_in_testata = *blank
116900100701     c                   eval      NAD_mittente_in_testata = *blank
117000100701     c                   eval      CTA_destinatario_in_testata = *blank
117100100701     c                   eval      COM_telefono_in_testata = *blank
117200100701     C*
117300100701      *   Inizia il conteggio delle righe
117400100706     c                   z-add     0             Conta_segmenti    5 0
117500111027     C*
117600111027     C*  Resetta l'informazione di spedizione di RESO
117700111027     c                   eval      era_un_export_RESO_dal_PARTNER ='N'
117800111027     c                   eval      Sped_RESO = 'N'
117900100701     C*
118000100701     C                   ENDSR
118100100701      * ?================================================================== */
118200100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
118300100701      * ?================================================================== */
118400100701     C     Nuova_spediz  BEGSR
118500100701      **
118600100701     C* Da qui inzia il dettaglio perciò lo memorizzo
118700100701      *  imposta il Flag per identificare la testata
118800100701     c                   eval      Dati_di_Testata  ='N'
118900100708      *
119000100708     c                   z-add     vnrec         IniDET_RECord
119100100813      **
119200100813      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
119300100813      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
119400100813      *
119500100813     c                   if        UNB_parziale ='S' and
119600100813     c                              PT_con_piu_Nazioni = 'S'
119700100813      * ?Cerca dalla riga di questo dettaglio in avanti
119800100813      * ?           *------------------------------*
119900100813     c                   exsr      cerca_Mittente
120000100813     c                   exsr      esito_tab_PT
120100100813      * ?           *------------------------------*
120200100813     c                   endIF
120300100813      **
120400100701      *
120500100701      *  Pulizia dei records
120600100701     C                   CLEAR                   EDiVAB00
120700100701     C                   CLEAR                   EDiVAT00
120800100701     C                   CLEAR                   EDiVAD00
120900100701      **
121000100706     C                   z-add     PT_vabLNP     VABLNP
121100100706     C                   z-add     PT_vabNRS     VABNRS
121200100701     C                   MOVEL     PT_vabCTM     VABCTM
121300100701     C                   z-add     PT_vabCCM     VABCCM
121400100701     C*
121500110707     C                   MOVEL(p)  UNH_vabRMA    VABrma
121600100701     C                   z-add     UNH_vabRMN    VABrmn
121700100701      *
121800100701      *  Dettaglio dei segnacolli Bartolini/EEX
121900100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
122000100701     c                   eval      GID_aTotale = 'N'
122100100701      *
122200100701     C                   clear                   Conta_Segn        5 0
122300100701     C                   clear                   totale_PCI        5 0
122400100701      *
122500100701     C                   MOVEA     *HIVAL        segn_BAR
122600100701     C                   MOVEA     *blank        segn_EEX
122700100701      *
122800100701     C*  Azzero codice cliente - tariffa
122900100701     C                   Z-ADD     0             ToT_Colli        16 0
123000100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
123100100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
123200100701      *
123300100701     C*  Azzero codice cliente - tariffa
123400100701     C                   clear                   Det_RFF_FF_ksc
123500100701     C                   clear                   Det_RFF_FF_tar
123600100701     C                   clear                   Det_RFF_FF_lnp
123601160209     C                   clear                   Det_RFF_FF_fgs
123700100701     C                   clear                   Det_RFF_FF_nrs
123800100701     C                   clear                   Det_RFF_FF_ctm
123900100701     C                   clear                   Det_RFF_FF_bs1
124000100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
124100110406     C                   clear                   Det_RFF_FF_tic
124200100701      *
124300100701     C                   clear                   Note_1           35
124400100701     C                   clear                   Note_2           35
124500110907     C                   clear                   Note_1T          35
124600110907     C                   clear                   Note_2T          35
124700131025     C                   clear                   Note_1sms        35
124800131025     C                   clear                   Note_2sms        35
124900100701     C*  Campi di work
125000100701     c                   clear                   VABtic_work
125100100701     c                   clear                   wri_vabtic        1
125200110623     c                   eval      inviato_tipo_incasso ='N'
125300100701      *
125400100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
125500100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
125600100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
125700100701     c                   eval      COM_telefono_in_Dettaglio = *blank
125800100702     c                   eval      CNI_Identificativo_Bolla = *blank
125900110907     c                   eval      FTX_ServizioMail_xDestinatario = *blank
126000131025     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
126100131025     c                   eval      FTX_MAIL_come_Servizio = *blank
126200131025     c                   eval      FTX_SMS_come_Servizio = *blank
126300120613     c                   eval      FTX_ai_piani = *blank
126400101027     c                   clear                   CNI_seNumerico
126500101027     C                   clear                   RFF_RifSpediz
126600100701      *
126700100701     C                   clear                   Rifer_Alfanum    35
126800100701     C                   clear                   Rifer_Cliente    35
126900100701     C                   clear                   Rifer_Numeric    35
127000100701     C                   clear                   Tipo_Rifer        3
127100100701      *
127200100701      * Un errore nel dettaglio
127300100701     C                   clear                   Err_in_detta      1
127400100701     c                   clear                   errori_in_Wrt     1
127500100701      *
127600100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
127700100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
127800100701     C*
127900100701     c                   endSR
128000100813     C*----------------------------------------------------------------
128100100813      *? cerca Nazione del primo Mittente nella sequenza dettagli
128200100813     C*----------------------------------------------------------------
128300100813     C     cerca_MittenteBEGSR
128400100813      *
128500100813     c                   z-add     vnrec         rec_posiz         9 0
128600100813     c                   move      *blank        Naz_mittente      3
128700100813     c                   movel(p)  UNB_Generico  PT_Parziale      35
128800100813     c                   move      'N'           OK_Nazione        1
128900100813      *
129000100813      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
129100100813     c                   call      'TRTCT93R1'
129200100813     c                   parm                    rec_posiz
129300100813     c                   parm                    PT_Parziale
129400100813     c                   parm                    Naz_mittente
129500100813     c                   parm                    Ok_Nazione
129600100813     c                   parm                    User_msg
129700100813     C                   parm                    keyUNBCLI
129800100813     C                   parm                    keyTIPOMSG
129900100813     C                   parm                    keyVERSION
130000100813     C                   parm                    keyRELEASE
130100100813     C                   parm                    keyAGENCY
130200100813     C                   parm                    keyASSOCIA
130300100813      *
130400100813      *  Re-inizializza l'indicatore di trovato
130500100813     c                   setoff                                       32
130600100813      *
130700100813      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
130800100813      *   da decodificare come UNB.
130900100813     c                   if        Ok_Nazione = 'S'
131000100813     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
131100100813     C                   eval      Nr_PT = 1
131200100813     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
131300100813     c                   Endif
131400100813      *
131500100813     c                   endSR
131600100813     C*----------------------------------------------------------------
131700100813      *? esito ricerca tabella PT decodificata oppure NO
131800100813     C*----------------------------------------------------------------
131900100813     C     esito_tab_PT  BEGSR
132000100813      *
132100100813     C                   move      'N'           PT_trovata
132200100813     C   32              move      'S'           PT_trovata
132300100813      *
132400100813      * ******  Errore generale sul Messaggio
132500100813      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
132600100813      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
132700100813     C                   If        PT_trovata = 'N'
132800100813      * Msg di allerta Traduzione
132900100813     C                   EVAL      Indirizzi = CED_Bart
133000100813???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
133100100813     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
133200100813     C                             IFCSUM D96A import in filiale - messaggio -
133300100813     C                             con UNB non presente in tabella PT controlla-
133400100813     C                             re EDI ricevuto su UPLOAD.'
133500100813      *
133600100813     c                   exsr      invio_mail
133700100813     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
133800100813     C                   eval      vinFLG = '2'
133900110208     c                   eval      Almeno_Un_Due ='S'
134000100813     C                   eval      se_errore   = 'S'
134100100813      *
134200100813      *             *------------------------------*
134300100813     C                   eLSe
134400100813      *             *------------------------------*
134500100813      *
134600100813     c                   exsr      Se_Trovata_PT
134700100813      *
134800100813     c                   Endif
134900100813      *
135000100813     c                   endSR
135100100813     C*----------------------------------------------------------------
135200100813      *? DECODIFICA DATI UNB
135300100813     C*----------------------------------------------------------------
135400100813     C     Se_Trovata_PT BEGSR
135500100813      *
135600100813      * Se tutto OK su tab.PT:
135700100813      *
135800100813     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
135900100813     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
136000100813     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
136100120629     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
136101160209     C                   MOVEL     PS_uni(Nr_PT) EDIDSPS
136200100813      *
136300100813      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
136400100813      *  ricevuti:
136500100813     c                   movel     §PVema        mail_ind         44
136600100813      *
136700100813      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
136800100813     c                   if        mail_ind <> *blank
136900100813     c                   exsr      imp_ADDR_mail
137000100813     c                   end
137100100813      *
137200100813      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
137300100813      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
137400100813      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
137500100813     C                   if        §PULUN = *blank
137600100813     C                   eval      §PULUN = '00'
137700100813     c                   end
137800100813      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
137900100813     C                   move      §puLUN        PU_lunVAT
138000100813     C                   move      §puTS         PU_TipoServ
138100100813     C                   move      §puSS         PU_SpecServ
138200111027     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO = §puRES
138300120629     C                   eval       PZ_abilita_NAD_SF_come_RFF_FF = §pzNADsf
138400100813      *
138500100813     C                   Z-ADD     §PTKSC        PT_vabCCM
138600100813     C                   z-add     §PTLNP        PT_vabLNP
138700100813     C                   z-add     §PTNRS        PT_vabNRS
138800100813     C                   MOVEL     §PTCTM        PT_vabCTM
138801160209      *
138802160209     C                   move      §PTLNP        PS_vabFGS
138803160209     c                   if         §PSfgs <> *blank and
138804160209     c                              §PSfgs <> *zeros
138805160209     C                   MOVE      §PSfgs        PS_vabFGS
138806160209     c                   end
138900100813      *
139000100813      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
139100100813      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
139200100813      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
139300100813      *  i records trasmessi).
139400100813     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
139500100813      *
139600100813      *  In base al cod.trattamento merce reperisco tipo tratt.
139700100813     C                   CLEAR                   DS1B
139800100813     C                   Z-ADD     1             Y                 3 0
139900100813     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
140000100813     C     *IN32         IFEQ      '1'
140100100813     C                   MOVEL     Des_Tb_CTM(Y) DS1B
140200100813     C                   MOVEL     DS1B          sav_DS1B
140300100813     C                   END
140400100813      *
140500100813      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
140600100813     C                   clear                   ACORAG
140700100813     C                   clear                   INDCAE
140800100813     C                   clear                   INDCAP
140900100813     C                   clear                   INDSTA
141000100813      * Decodifico partner
141100100813     C                   Z-ADD     1             KKUT
141200100813     C                   Z-ADD     KCI           KKCC
141300100813     C                   MOVE      §PTKSC        KKSC
141400100813     C     KACO          CHAIN     CNACO00F                           31
141500100813     C     KIND          CHAIN     CNIND00F                           31
141600100813      *
141700100813      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
141800100813      *   questa schiera è utilizzata anche per i clienti.
141900100813     C     §ptKSC        LOOKUP    skCLienti                              39
142000100813     c                   If        *in39 = *off
142100100813     C                   add       1             sk
142200100813     C                   z-add     §ptKSC        skCLienti(sk)
142300100813     C                   Endif
142400100813      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
142500100813      *
142600100813     c                   endSR
142700090210     C*----------------------------------------------------------------
142800090210      *? DECODIFICA DATI UNB
142900090210     C*----------------------------------------------------------------
143000090210     C     DATI_UNB      BEGSR
143100090210      *
143200100701      * identificativo Messaggio
143300100701     C                   eval      UNB_NrTrasmiss = unb0020
143400100701      *
143500100813     C                   MOVEL     unb0017       Anno2             2 0
143600100813     C                   z-add     2000          Anno4             4 0
143700100813     C                   add       Anno2         Anno4
143800100813     C                   MOVE      unb0017       WGG               2 0
143900100813     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
144000100813     C                   MOVE      Anno2         Data_messag
144100100813     C                   MOVEL     WGG           Data_messag
144200100813     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
144300100813     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
144400100813     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
144500100813     c****
144600100813     c                   clear                   Mittente         35
144700100813     c                   clear                   Mitt_Qualifier    4
144800100813     c                   clear                   Id_Messaggio     14
144900100813     C                   eval      Mittente       = unb0004
145000100813     C                   eval      Mitt_qualifier = unb000720
145100100813     C                   eval      Id_Messaggio   = UNB0022
145200100813      *
145300090209      ** ---------------------------------------------------------------
145400090209      * quindi 1° cosa:
145500090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
145600090209      *     senza questo il messaggio NON è trascodificabile
145700100630     C                   MOVEL     unb0004       UNB_diWork
145800100630     C                   MOVE      unb000720     UNB_diWork
145900100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
146000100630     C                   Z-ADD     1             Nr_PT
146100100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
146200090209      *
146300100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
146400100317      *  la Nazione:
146500100317     C                   If        *IN32 = *off
146600100630     C                   eval      Nr_PT = 1
146700100630     c                   clear                   UNB_diWork
146800100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
146900100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
147000100317      *
147100100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
147200100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
147300100317      *  per cercare di individuare con certezza il codice della tabella PT in base
147400100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
147500100317     C                   If        *IN32 = *on
147600100317      *
147700100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
147800100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
147900100317      * ?  su Partner che hanno + linee con + nazioni.
148000100813      *  se è considerato Parziale
148100100813      *   Salva il codice generico
148200100630     c                   eval      UNB_parziale = 'S'
148300100813     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
148400100813      *             *------------------------------*
148500100813     c                   exsr      cerca_Mittente
148600100813      *             *------------------------------*
148700100813     C                   eval          Naz_salvata = Naz_mittente
148800100813     c                   Endif
148900100813     c                   Endif
149000100813      *
149100100813      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
149200100813      *             *------------------------------*
149300100813     c                   exsr      esito_tab_PT
149400100813      *             *------------------------------*
149500100813      **
149600100813      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
149700100813     C                   If        PT_trovata = 'S'
149800100813      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
149900100813      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
150000100813      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
150100100813      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
150200100813      * ?- Salva l'indice delle schiere per riaggancarla in seguito        ------ */
150300100813     C                   z-add     Nr_PT         savXX_PT_key
150400100813      * ?- Se ha più LINEE da gestire                                      ------ */
150500100813     C                   eval      PT_con_piu_Nazioni = §puPLN
150600100813     c                   end
150700100813      **
150800090209      **
150900090210     c     end_UNB       endSR
151000090227      * ?================================================================== */
151100090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
151200090227      * ?================================================================== */
151300090227     C     DATI_UNH      BEGSR
151400100316      *
151500100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
151600100701      *
151700090211      ** Riferimento del cliente
151800100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
151900100701      * ?                                       ------------
152000110707     C                   MOVEL(p)  UNH_Rifer_Msg UNH_VABrma
152100100701      * ?                                       ------------
152200100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
152300100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
152400100701     C                   SUB       1             lunghezza_num
152500100701     C                   EXSR      Se_Numerico
152600100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
152700090211      * ?                                       --------
152800100701     C                   MOVEL     Numero_OK     UNH_VABrmn
152900090211      * ?                                       --------
153000090211     C                   END
153100090303      **
153200090210     c     end_UNH       endSR
153300100701      * ?================================================================== */
153400100701      *? dati iniziali   BGM
153500100701      * ?================================================================== */
153600100701     C     DATI_BGM      BEGSR
153700100701      *
153800100701      *  identificativo testata
153900100706     c                   eval       BGM_Id_Testata = BGM1004
154000100706     C                   IF           BGM_Id_Testata <> *blank
154100100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
154200100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
154300100706     c                   end
154400100706      *
154500100701      **
154600100701     c                   endSR
154700100701      * ?================================================================== */
154800100701     C*?  Contatori in testata o in Dettaglio
154900100701      * ?================================================================== */
155000100701     C     DATI_CNT      BEGSR
155100100701      **
155200100701     c                   IF        Dati_di_Testata  ='S'
155300100701      **
155400100701      *  TOTALE colli, peso, spedizioni:
155500100701     c                   if        cnt6069 = Totale_Peso_Lordo
155600100701     c                   z-add     cnt6066       Tot_Peso_msg     15 2
155700100701     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
155800100701     c                   z-add     cnt6066       Tot_Sped_msg     15 2
155900100701     c                   elseIf    cnt6069 = Totale_Nr_Colli
156000100701     c                   z-add     cnt6066       Tot_Colli_msg    15 2
156100100701     c                   elseIf    cnt6069 = Totale_Peso_Netto
156200100701     c                   z-add     cnt6066       Tot_PNetto_msg   15 2
156300100701     c                   end
156400100701      **
156500100701     C                   ELSE
156600100701      **
156700100701     C                   END
156800100701      **
156900100701     c     end_CNT       endSR
157000100701      * ?================================================================== */
157100100701     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
157200100701      * ?================================================================== */
157300100701     C     DATI_RFF      BEGSR
157400120424      *
157500120424      *   Cerca la decodifica del dato in TABELLA
157600120424     C                   movel(p)  Tipo_Seg      etbSGM
157700120424     C                   movel(p)  RFF1153       etbVALSGM
157800120424     c                   exsr      In_TAB_EDSTBL
157900100701      **
158000100701      * ?   se in Testata
158100100702      * ?   X_Cargo_Manifest o X_Numero_CMR
158200100702      **
158300100701     c                   IF        Dati_di_Testata  ='S'
158400100701      **
158500100701     C*  Se tipo documento CMR ('CMR')
158600100701     C                   if        RFF1153 =  X_Cargo_Manifest
158700100701     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
158800120424      *
158900100701     C*  Se tipo documento AFB (Foglio viaggio)
159000100701     C                   elseIF    RFF1153 =  X_Numero_CMR
159100100701     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
159200120424      *
159300120424     C*  Se tipo documento  particolare       x CMR/F.Viaggio
159400120424     C                   elseIF    Quale_campo ='RFFTES'
159500120424     c                              and RFF1153 <> *blank
159600120424     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
159700120424     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
159800120424      **
159900100701     C                   END
160000120629      *
160100120629      * ?Codice Cliente di riferimento in Testata
160200120629     C                   clear                   Rifer_Cliente    35
160300120629     C                   if        RFF1153 = Riferimento_Cliente_particolare
160400120629     C                              and RFF1154 <> *BLANKS
160500120629     c                               or
160600120629     C                             Quale_campo ='RFFCLI'  and
160700120629     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
160800120629      *>>>>>>>>
160900120629     C*------------------
161000120629     C                   movel     RFF1154       Rifer_Cliente
161100120629      *
161200120629      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
161300120629     c                   exsr      CL_Particolare
161400120629     C*------------------
161500120629      *>>>>>>>>
161600120629     C                   ENDIF
161700100701      **
161800100701     c                   ELSE
161900100701      *
162000100701      * ?   se in Dettaglio   Riferimento singola spedizione
162100100701      *  Trascodifico riferimento spedizione
162200120424      **  o con altro riferimento diverso dall'AGE che indica la stessa cosa
162300100701     C                   clear                   Rifer_Alfanum    35
162400100701     c                   if        RFF1153 = Riferimento_Spedizione  and
162500100701     c                                RFF1154 <> *BLANKS
162600120424     c                               or
162700120424     C                             Quale_campo ='RFFAGE'  and
162800120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
162900120424      *
163000100701     C                   movel(p)  RFF1154       Rifer_Alfanum
163100120424      *
163200100701     c                   end
163300100701      *
163400100701      ** controlla se contiene solo numeri
163500100701     C                   IF         Rifer_Alfanum <> *BLANKS
163600100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
163700100701     C                   movel     Rifer_Alfanum test_num
163800100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
163900100701     C                   sub       1             lunghezza_num
164000100701     C                   EXSR      Se_Numerico
164100100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
164200110707     C                   movel(p)  Rifer_Alfanum VABRMA
164300100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
164400100701     C                              Tipo_Rifer = *blanks
164500100701     C                   movel     Numero_OK     VABRMN
164600100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
164700111027      ****
164800100701     C                   ENDIF
164900100701     C                   ENDIF
165000100701     C                   ENDIF
165100100701      *
165200100701      * Codice Cliente di riferimento
165300110224     C                   clear                   Rifer_Cliente    35
165400100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
165500100701     C                              and RFF1154 <> *BLANKS
165600120424     c                               or
165700120424     C                             Quale_campo ='RFFCLI'  and
165800120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
165900110224      *>>>>>>>>
166000120629     C*------------------
166100100701     C                   movel     RFF1154       Rifer_Cliente
166200100701      *
166300100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
166400120629     c                   exsr      CL_Particolare
166500120629     C*------------------
166600120629      *>>>>>>>>
166700100702     C                   ENDIF
166800100702     C*------------------
166900100702      * ?Riferimento Numerico
167000100702     C*------------------
167100110224     C                   clear                   Rifer_Numeric    35
167200100701     C                   if        RFF1153 = Riferimento_Numerico
167300100701     C                              and RFF1154 <> *blanks
167400120424     c                               or
167500120424     C                             Quale_campo ='RFFNUM'  and
167600120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
167700120424      *
167800100701     C                   movel     RFF1154       Rifer_Numeric
167900100701     C                   END
168000100701      *
168100100701      * Numero Ordine di vendita
168200100701     C     Rifer_Numeric IFNE      *BLANKS
168300100701     C                   movel     Rifer_Numeric test_num
168400100701     C     ' '           scan      Rifer_Numeric lunghezza_num
168500100701     C                   sub       1             lunghezza_num
168600100701     C                   EXSR      Se_Numerico
168700100702      *
168800100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
168900100701     C                   movel     Numero_OK     VABRMN
169000100701     C                   eval       Tipo_Rifer = Riferimento_Numerico
169100100701     C                   Endif
169200100702      *
169300100701     C                   ENDIF
169400100701      *
169500100701      * NOn c'è l'identificativo bolla del cliente
169600100701     c                   if        (RFF1153 = Riferimento_Spedizione and
169700100701     c                              RFF1154 = *BLANKS)
169800100701     c                             or
169900100701     c                             (RFF1153 = Riferimento_Numerico and
170000100701     c                              RFF1154 = *BLANKS)
170100100701     C                   eval      vinMSG = 'RFF riferimento assente'
170200100701     c                   exsr      Error_DET
170300100701     c                   goto      end_RFF
170400100702     C                   EndIF
170500100701      *
170600100701     C                   ENDIF
170700100701      *
170800100701     c     end_RFF       endSR
170900100701      * ?================================================================== */
171000100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
171100100701      * ?================================================================== */
171200100701     C     DATI_DTM      BEGSR
171300100705      *
171400100705      *   Cerca la decodifica del dato in TABELLA
171500120424     C                   movel(p)  Tipo_Seg      etbSGM
171600100705     C                   movel(p)  dtm2005       etbVALSGM
171700100705     c                   exsr      In_TAB_EDSTBL
171800100705      *
171900100701     C*
172000100701     C*  Se data documento  memorizzo numero e data x CMR
172100100705     C                   If        Quale_campo ='DATDOC'
172200101015     c                              and dtm2380 <> *blank
172300101015      **
172400100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
172500100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
172600100702     C                   EXSR      Converte_Data
172700100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
172800100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
172900100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
173000100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
173100100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
173200100701     C     WERRO         IFEQ      'S'
173300100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
173400100706     C                             dtm2005  +
173500100702     C                              ') data errata'
173600100701     c                   exsr      Error_DET
173700100701     c                   goto      end_DTM
173800100701     C                   END
173900100701     C                   END
174000100702     C*
174100100702     C* Data Partenza del mezzo
174200100705     C                   If        Quale_campo ='DATPAR'
174300101015     c                              and dtm2380 <> *blank
174400101015      **
174500100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
174600100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
174700100702     C                   EXSR      Converte_Data
174800100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
174900100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
175000100702     C     WERRO         IFEQ      'S'
175100100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
175200100706     C                             dtm2005  +
175300100702     C                              ') data errata'
175400100702     c                   exsr      Error_DET
175500100702     c                   goto      end_DTM
175600100702     C                   END
175700100702     C                   END
175800100702     C*
175900100702     C* Data Previsto Arrivo del mezzo
176000100705     C                   If        Quale_campo ='DATARR'
176100101015     c                              and dtm2380 <> *blank
176200101015      **
176300100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
176400100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
176500100702     C                   EXSR      Converte_Data
176600100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
176700100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
176800100702     C     WERRO         IFEQ      'S'
176900100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
177000100706     C                             dtm2005  +
177100100702     C                              ') data errata'
177200100702     c                   exsr      Error_DET
177300100702     c                   goto      end_DTM
177400100702     C                   END
177500100702     C                   END
177600100701      **
177700140212      **    PER HILTI non deve essere eseguita la VABDCR
177800140212      **       causa un errore del loro messaggio
177900140212      **
178000140212      **               =======================
178100140212     C                   goto      NO_VABDCR
178200140212      **               =======================
178300140212      **
178400100701     C*  Se data consegna richiesta imposto già sui campi del VAB
178500101015     C                   If        Quale_campo ='VABDCR'
178600101015     c                              and dtm2380 <> *blank
178700101015      **
178800100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
178900100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
179000100702     C                   EXSR      Converte_Data
179100100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
179200100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
179300100701     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
179400100701     C     WERRO         IFEQ      'S'
179500100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
179600100706     C                             dtm2005  +
179700100702     C                              ') data errata'
179800100701     c                   exsr      Error_DET
179900100701     c                   goto      end_DTM
180000100701     C                   END
180100100702     C                   end
180200140212      **
180300140212      ** =======================
180400140212     c     NO_VABDCR     tag
180500140212      ** =======================
180600140212      **
180700100701      **
180800100701     c     end_DTM       endSR
180900100701      * ?================================================================== */
181000100701     C*? Imposta informazioni sul dettaglio del trasporto
181100100701      * ?================================================================== */
181200100701     C     DATI_TDT      BEGSR
181300100701     C*
181400100701      **
181500100701     c     end_TDT       endSR
181600100705      * ?================================================================== */
181700100705     C*? Imposta la Località
181800100705      * ?================================================================== */
181900100705     C     DATI_LOC      BEGSR
182000100705     C*
182100100705      **
182200100705     c     end_LOC       endSR
182300100705      * ?================================================================== */
182400100705     C*? Imposta identificativi delle Merci
182500100705      * ?================================================================== */
182600100705     C     DATI_EQD      BEGSR
182700100705     C*
182800100705      **
182900100705     c     end_EQD       endSR
183000100705      * ?================================================================== */
183100100705     C*? Imposta il numero delle Merci
183200100705      * ?================================================================== */
183300100705     C     DATI_EQN      BEGSR
183400100705     C*
183500100705      **
183600100705     c     end_EQN       endSR
183700100316      * ?================================================================== */
183800100316     C*? Imposta l'inizio del Dettaglio CNI
183900100316      * ?================================================================== */
184000100316     C     DATI_CNI      BEGSR
184100100316     C*
184200100702      ** Inizio Dettaglio riferimento iniziale
184300100702     c                   If        CNI1004 <> *blank
184400100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
184500101027      *
184600101027      *   Nel Riferimento Alfa sempre
184700110707     c                   eval            VABRMA  = CNI_Identificativo_Bolla
184800101027     C                   EVAL      RFF_RifSpediz = CNI_Identificativo_Bolla
184900101027      *
185000101027      *   e nel numerico se di soli numeri e comunque se lo memorizza
185100101027     C                   eval       test_num = CNI1004
185200101027     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
185300101027     C                   SUB       1             lunghezza_num
185400101027     C                   EXSR      Se_Numerico
185500101027     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
185600101027      * ?                                       --------
185700101027     C                   MOVEL     Numero_OK     VABrmn
185800101027      * ?                                       --------
185900101027     C                   z-add     VABrmn        CNI_seNumerico
186000101027      * ?                                       --------
186100101027     C                   END
186200101027      *
186300101027      *
186400100702     c                   end
186500100319      *
186600100316      **
186700100316     c     end_CNI       endSR
186800100316      * ?================================================================== */
186900100316     C*? Imposta le merci pericolose
187000100316      * ?================================================================== */
187100100316     C     DATI_DGS      BEGSR
187200100316     C*
187300100316      **
187400100316     c     end_DGS       endSR
187500100316      * ?================================================================== */
187600100316     C*? Imposta il numero seriale
187700100316      * ?================================================================== */
187800100316     C     DATI_SEL      BEGSR
187900100316     C*
188000100316      **
188100100316     c     end_SEL       endSR
188200100316      * ?================================================================== */
188300100316     C*? Imposta informazioni aggiuntive
188400100316      * ?================================================================== */
188500100316     C     DATI_PIA      BEGSR
188600100316     C*
188700100316      **
188800100316     c     end_PIA       endSR
188900100316      * ?================================================================== */
189000100316     C*?  Tipo Servizio
189100100316      * ?================================================================== */
189200100316     C     DATI_TSR      BEGSR
189300100702      ***
189400100702     C*  Reperisco priorità spedizione:
189500100702     C                   Z-ADD     1             X
189600100702      ***
189700100702      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
189800100702      ***  mentre in tabella EDI EEX è fatto con "1"
189900100702      ***
190000100702     c                   movel(p)  TSR4219       Work_Data__35
190100100702     c                   move      PU_TipoServ   Work_Data__35
190200100705     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
190300100702     C     *IN32         IFEQ      '1'
190400100702     C                   eval       VABTSP = TipoServizio(X)
190500100705     C                   ELSE
190600100705      **  Il tipo bolla non è presente
190700100705     C                   eval      vinMSG = 'TSR non in tabella'
190800100707     c*********          exsr      Error_DET
190900100707     c*********          goto      end_TSR
191000100705     C                   END
191100100702     C*
191200100702     C*  Decodifico servizi speciali
191300100702     C     TSR7085       IFNE      *BLANKS
191400100702     C     TSR7085       ANDNE     *LOVAL
191500100705      *
191600100702     C                   Z-ADD     1             X
191700100702     c                   movel(p)  TSR7085       Work_Data__35
191800100702     c                   move      PU_SpecServ   Work_Data__35
191900100702     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
192000100705     C     *IN32         IFEQ      '1'
192100100702     C                   eval       EDIDSSS = UNI_ServSpec(X)
192200100705      *
192300100702     C     §SSNOT        IFEQ      'S'
192400100702     C                   MOVEL     §SSDES        VABNOT
192500100702     C                   END
192600100702      *
192700120214      * x Espresso     "Pre-noon"    codice "PRN"
192800100702     C     §SSTPS        IFEQ      'S'
192900100702     C                   MOVEL     'E'           VABTSP
193000100702     C                   END
193100110509      *
193200120214      * x H 10:30      "H 10:30"     codice "H10"
193300110509     C     §SSTPS        IFEQ      'H'
193400110509     C                   MOVEL     'H'           VABTSP
193500110509     C                   END
193600100705      *
193700120214      * x Appuntamento "Booking ins" codice "BKI"
193800120403     c                   IF        §SSTPS ='A' and VABTC1 = *blank
193900100702     C                   MOVEL     'A'           VABTC1
194000120403     c                   elseIF    §SSTPS ='A' and VABTC2 = *blank
194100120403     C                   MOVEL     'A'           VABTC2
194200100702     C                   END
194300120403      *
194400120214      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
194500120214     C     §SSTPS        IFEQ      'F'
194600120214     C                   MOVEL     'S'           VABFFD
194700120214     C                   END
194800100705      *
194900100705     C                   ELSE
195000100705      *
195100100705      **  Il tipo bolla non è presente
195200100705     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
195300100707     c*******            exsr      Error_DET
195400100707     c*******            goto      end_TSR
195500100705      *
195600100702     C                   END
195700100705      *
195800100702     C                   END
195900100316      **
196000100316     c     end_TSR       endSR
196100100705      * ?================================================================== */
196200100705     C*?  C.O.D. Monetary Amount
196300100705      * ?================================================================== */
196400100705     C     DATI_MOA      BEGSR
196500100705      *
196600100705      *   Cerca la decodifica del dato in TABELLA
196700120424     C                   movel(p)  Tipo_Seg      etbSGM
196800100705     C                   movel(p)  moa5025       etbVALSGM
196900100705     c                   exsr      In_TAB_EDSTBL
197000100705      *
197100100705     C* Controllo se campo importo numerico con 3 decimali
197200100705     C     moa5004       IFNE      *zeros
197300100705      *
197400100705     C                   If        Quale_campo ='VABIAS'
197500130529      *
197600130529      * il campo è diviso in 2 flag: il primo serve per l'importo da assicurare
197700130529      *    quindi se deve essere considerato è espresso sulla PT.
197800130529     c                   if        %subst(§PTxxx:1:1) = 'S'
197900100707     C     moa5004       div       100           VABIAS                         IMPORTO
198000100705     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
198100130529     c                   end
198200100705      *
198300100705     C                   elseIF    Quale_campo ='VABVMD'
198400100707     C     moa5004       div       100           VABVMD                         MERCE
198500100705     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
198600100705      *
198700100705     C                   elseIF    Quale_campo ='VABCAS'
198800100705     C     §PUCAS        IFEQ      'S'                                          C/Assegno
198900100705      *
199000100707     C     moa5004       div       100           VABCAS
199100100705     C                   MOVEL     moa6345       VABVCA
199200100705     c                   if        Tes_valuta <> *blank and VABVCA = *blank
199300100705     C                   MOVEL     Tes_valuta    VABVCA
199400100705     c                   end
199500110627      **
199600110627      **  se c'è il default ("??")
199700110627     C                   Z-ADD     1             Y
199800110627     c                   movel(p)  '??'          Work_Data__35
199900110627     c                   move      §puTC         Work_Data__35
200000110627     c                   clear                   WData_35_2        2
200100110627     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
200200110627     c                   if        not *in33
200300110627      *
200400100705      *
200500130311      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO ( ma deve leggere il segmento
200600130311      * contenente il Tipo Incasso - sempre successivo al segmento MOA )
200700100705      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
200800130311      *
200900130311      * ATTENZIONE viene impostato di Default ma, in presenza del FTX+PAI,
201000130311      *  viene reimpostato in base a quello sulla tabella "TC".
201100130311      *   Quindi viene modificato da "OM" ....a quello sul FTX+PAI successivo.
201200100705     C     §PUTPC        IFEQ      'P'
201300100705     C                   MOVEL     'OM'          VABTIC
201400100705     C                   MOVEL     'OM'          VABtic_work
201500100705     C                   ENDIF
201600110627      **
201700110627     c                   end
201800100705      *
201900100705     C                   END
202000100705      *
202100100705     C                   EndIF
202200100705     C                   END                                                    Imp.non numer.
202300100705      *
202400100705     c     end_MOA       endSR
202500100705      * ?================================================================== */
202600100705     C*?  FREE TEXT
202700100705      * ?================================================================== */
202800100705     C     DATI_FTX      BEGSR
202900100705      *
203000100705      *   Cerca la decodifica del dato in TABELLA
203100120424     C                   movel(p)  Tipo_Seg      etbSGM
203200100705     C                   movel(p)  ftx4451       etbVALSGM
203300100705     c                   exsr      In_TAB_EDSTBL
203400100705      *
203500100705      * ? solo se si tratta di note descrittive da comunicare in BOLLA
203600100705     c                   if         quale_campo = 'VABNOT'
203700100705     C                   DO        4             X
203800100705      *
203900100705     C     D05(X)        IFNE      *BLANKS
204000100705      *
204100100705     C     Note_1        IFEQ      *BLANKS
204200100705     C                   MOVEL     D05(X)        Note_1
204300100705     C                   MOVE      D05(X)        Note_2
204400100705     C                   ELSE
204500100705     C     Note_2        IFEQ      *BLANKS
204600100705     C                   MOVEL     D05(X)        Note_2
204700100705     C                   END
204800100705     C                   END
204900100705      *
205000100705     C                   ENDif
205100100705      *
205200100705     C                   ENDdo
205300100705     C                   end
205400100705     C*------>>
205500110907      * ? solo se si tratta di note x invio mail di avviso al destinatario
205600110907      *    (nuovo servizio a pagamento attivato nel 2011)
205700110907      *     dovranno essere scritti 2 records nel VAT di tipo I e J
205800110907     c                   if         quale_campo = 'VATNOT'
205900131025     c                                 or
206000131025     c                              quale_campo = 'VATEML'
206100131025      *
206200131025     c                   if        ftx4453 = 'N'
206300131025     c                   eval      FTX_MAIL_come_servizio = 'N'
206400131025     c                   end
206500110907      *
206600110907     C                   DO        4             X
206700110907      *
206800110907     C     D05(X)        IFNE      *BLANKS
206900110907      *
207000110907     C     Note_1t       IFEQ      *BLANKS
207100110907     C                   MOVEL     D05(X)        Note_1t
207200110907     C                   MOVE      D05(X)        Note_2t
207300110907     C                   ELSE
207400110907     C     Note_2t       IFEQ      *BLANKS
207500110907     C                   MOVEL     D05(X)        Note_2t
207600110907     C                   END
207700110907     C                   END
207800110907      *
207900110907     C                   ENDif
208000110907      *
208100110907     C                   ENDdo
208200110907      *
208300110907      * carica il campo da riportare poi sul VAT
208400110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
208500110907     c                             note_1T
208600110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
208700110907     c                             note_2T
208800110907     C                   end
208900131025     C*------>>
209000131025      * ? solo se si tratta di note x invio SMS di avviso al destinatario
209100131025      *    (nuovo servizio a pagamento attivato)
209200131025      *     dovrà essere scritto 1 record nel VAT di tipo S
209300131025     c                   if         quale_campo = 'VATSMS'
209400131025     c                   if        ftx4453 = 'N'
209500131025     c                   eval      FTX_SMS_come_servizio = 'N'
209600131025     c                   end
209700131025      *
209800131025     C                   DO        4             X
209900131025      *
210000131025     C     D05(X)        IFNE      *BLANKS
210100131025      *
210200131025     C     Note_1sms     IFEQ      *BLANKS
210300131025     C                   MOVEL     D05(X)        Note_1sms
210400131025     C                   MOVE      D05(X)        Note_2sms
210500131025     C                   ELSE
210600131025     C     Note_2sms     IFEQ      *BLANKS
210700131025     C                   MOVEL     D05(X)        Note_2sms
210800131025     C                   END
210900131025     C                   END
211000131025      *
211100131025     C                   ENDif
211200131025      *
211300131025     C                   ENDdo
211400131025      *
211500131025      * carica il campo da riportare poi sul VAT
211600131025     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
211700131025     c                             note_1sms
211800131025     C                   end
211900110907     C*------>>
212000100705     C*  Decodifico tipo pagamento C/Assegno
212100100705      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
212200100705      *      (Problema di pulizia in presenza di + righe di testo)
212300100705      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
212400100705      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
212500100705      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
212600100705      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
212700100705     C                   if        wri_vabtic = *blank
212800100705     C                   MOVEL     *BLANKS       VABTIC
212900100705     c                   end
213000100705      **
213100100705     c                   clear                   trovato_TPInc     1            TipoIncasso
213200100705      *
213300100705      * ?Se c'è il tipo Incasso
213400100705     c                   if         quale_campo = 'VABTIC'                      -- 01 --
213500100705      *
213600100705     C                   DO        4             X                              -- 02 --
213700100705     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
213800110623      **
213900130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
214000130311      **  per gestire Partner come AZKAR che ha codice "AAA"
214100130311     C**********         MOVEL     D05(X)        WCTC              2
214200130311     C                   MOVEL     D05(X)        WCTC             10
214300110623      **
214400110623     c                   eval      inviato_tipo_incasso ='S'
214500110623      **
214600110623      **  se non l'avesse trovato e fosse esplicitato un Default ("??")
214700110623      **   come per qualsisi codice venga inviato dal Cliente allora
214800110623      **   deve essere impostato il default (invece di fare un chiodo)
214900110623     C                   Z-ADD     1             Y
215000110623     c                   movel(p)  '??'          Work_Data__35
215100110623     c                   move      §puTC         Work_Data__35
215200110623     c                   clear                   WData_35_2        2
215300110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
215400110623     c                   if            *in33
215500110623     c                   move      'S'           trovato_TPInc
215600110623     C                   MOVEL     TipoIncasso(Y)vabtic
215700110623     c                   movel(p)  '??'          WData_35_2
215800110623     c                   else
215900110623      *
216000130311      *  Se NON HA un default generico definito con "??"
216100130311      *   cerco con il codice arrivato dal Partner/Cliente
216200100705     C                   Z-ADD     1             Y
216300100705     c                   movel(p)  Wctc          Work_Data__35
216400100705     c                   move      §puTC         Work_Data__35
216500100705     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
216600110623     c                   if            *in33
216700110623     c                   move      'S'           trovato_TPInc
216800110623     C                   MOVEL     TipoIncasso(Y)vabtic
216900130326     c                   move      §puTC         WData_35_2        2
217000110623     c                   else
217100110623      *
217200130311      *  Se NON HA TROVATO qualcosa di DEFINITO SU MISURA del Partner/Cliente
217300130311      *   tramite il SUFFISSO presente sulla PT, prova con IL CODICE semplicemente.
217400110623     C                   Z-ADD     1             Y
217500110623     c                   movel(p)  Wctc          Work_Data__35
217600110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
217700110623     c                   if            *in33
217800110623     c                   move      'S'           trovato_TPInc
217900110623     C                   MOVEL     TipoIncasso(Y)vabtic
218000130311     c                   movel     Wctc          WData_35_2        2
218100110623     c                   end
218200110623      *
218300110623     c                   end
218400110623      *
218500110623     c                   end
218600130311      *--------
218700130311      * SOLO SE PARTNER
218800130311      *   IMPOSTO 'OM'    (vecchia regola ormai in disuso se NON siano state
218900130311      *     trovate fra le tabelle dei codici ben precisi x il PARTNER,
219000130311      ***     ossia solo se NON E'STATO PASSATO un codice specifico
219100130311      ** ATTENZIONE NON DEVE ESISTERE IL BLANK in arrivo dal PARTNER ma sempre un CODICE.
219200100705     C     §PUTPC        IFEQ      'P'
219300100705     c     wctc          andne     'TO'
219400110408     c     WData_35_2    andeq     *blank
219500100705     C                   MOVEL     'OM'          VABTIC
219600100705     C                   END                                                    -- 03 --
219700130311      *--------
219800100705     C                   END                                                    -- 03 --
219900100705     C                   ENDdo                                                  -- 02 --
220000100705      *
220100100705     C                   ELSE                                                   -- 01 --
220200130311      *
220300130311      *  PASSA DA QUI se il FTX ha un qualificatore NON RICONOSCIUTO
220400130311      *   e se  aveva impostato blank (nullo) precedentemente....
220500130311      *****
220600100705      * ?Se NON c'è il tipo Incasso Contrassegno particolare
220700100705      *    Deve essere impostato comunque da testata
220800110623     c                   if        vabtic = *blank   and
220900110623     c                             WData_35_2 = *blank
221000100705     C                   MOVEL     VABtic_work   VABTIC
221100100705     c                   end
221200100705      *
221300100705     C                   END                                                    -- 01 --
221400100705      *
221500100705      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
221600100705      *    essere presenti altri record Flat con altre informazioni
221700100705      *    che potrebbero abblancare il Tipo incasso precedentemente
221800100705      *    decodificato
221900100705     c                   if        Trovato_TPInc = 'S' and
222000100705     c                                 wri_vabtic = *blank
222100100705     c                   eval      wri_vabtic = 'S'
222200100705     c                   end
222300100705      *
222400100705      * ?Se Natura Merce      .
222500100705     c                   if         quale_campo = 'VABNAS'                      -- 01 --
222600100705     C                   MOVEL     D05(1)        VABnas
222700100705     c                   End
222800100705      *
222900120613      * ?Consegne Particolari
223000120613     c                   if         quale_campo = 'VABTCX'                      -- 01 --
223100120613      * Imposta il codice di 3 chrs.
223200120613      *   solo i primi 3 caratteri del campo
223300120613     C                   eval      controlla_COD_FTX = D05(1)
223400120613      *
223500120613      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
223600120613     C                   if        controlla_COD_FTX = 'FLR'
223700120613     c                   eval      FTX_ai_piani = 'P'
223800120614     c                   End
223900120613     c                   End
224000120613      *
224100100705     c     end_FTX       endSR
224200090209      * ?================================================================== */
224300090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
224400090209      * ?================================================================== */
224500090209     C     DATI_TOD      BEGSR
224600090209      *
224700090210      *  Trascodifico tipo trasporto
224800090210     c                   clear                   TOD_char3         3
224900100705      *  DEFAULT da PT
225000090210      *  imposta il default dalla tabella PT se c'è
225100090210     C     §puPFA        ifNE      *blank
225200090210     c                   SELECT
225300090210      *  Porto Franco
225400090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
225500090210     C                   movel     '4'           VABCBO                         + C/Ass
225600090210     c                   WHEN      §puPFA = 'F'
225700090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
225800090210      *  Porto Assegnato
225900090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
226000090210     C                   movel     '6'           VABCBO                         + C/Ass
226100090210     C                   WHEN      §puPFA = 'A'
226200090210     C                   movel     '2'           VABCBO                         Senza C/Ass
226300090210     C                   ENDSL
226400090210     c                   end
226500090210      *
226600090210      *   Imposta o il codice o il metodo
226700090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
226800090210     c                   if        tod4053  <> *blank
226900090210     c                   movel(p)  tod4053       TOD_char3
227000090210     c                   else
227100090210     c                   if        tod4215  <> *blank
227200090210     c                   movel(p)  tod4215       TOD_char3
227300090210     c                   end
227400090210     c                   end
227500110328      ****************
227600110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
227700110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
227800110328      ****************
227900110328     c                   eval      Trovata_Tab_TB ='N'
228000110328      *
228100110328     c                   if              TOD_char3 <> *blank
228200110328     c                   movel(p)  TOD_char3     Key_Generica35
228300110328     c                   move      §puTB         Key_Generica35
228400110328     C                   eval      Y = 1
228500110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
228600110328     c   32              eval      Trovata_Tab_TB ='S'
228700110328      *
228800110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
228900110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
229000110328      *
229100110328     c                   if          Trovata_Tab_TB ='N'
229200110328     c                                 and
229300110328     C                               tod4215 <> *blank
229400110328     c                   movel(p)  tod4215       TOD_char3
229500110328     c                   movel(p)  TOD_char3     Key_Generica35
229600110328     c                   move      §puTB         Key_Generica35
229700110328     C                   eval      Y = 1
229800110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
229900110328     c   32              eval      Trovata_Tab_TB ='S'
230000110328     c                   end
230100110328     c                   end
230200110328      *
230300110328      ****************
230400110328      ******* Se vi sono eccezioni sul cliente imposta quelle
230500110328      ****************
230600110328     c                   If          Trovata_Tab_TB ='S'
230700110328      *
230800110328     c                   SELECT
230900110328     c                   WHEN      Decod_Porto(Y) = 'F'
231000110328     c                               and  VABcas > *zeros
231100110328     C                   movel     '4'           VABCBO                         + C/Ass
231200110328     c                   WHEN      Decod_Porto(Y) = 'F'
231300110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
231400110328      *  Porto Assegnato
231500110328     c                   WHEN      VABCAS > *zeros
231600110328     C                   movel     '6'           VABCBO                         + C/Ass
231700110328     C                   OTHER
231800110328     C                   movel     '2'           VABCBO                         Senza C/Ass
231900110328     C                   ENDSL
232000110328      *
232100110328      ****************  altrimenti
232200110328      ******* Se non vi sono eccezioni per il cliente
232300110328      *   Rileva a questo punto lo Standard per tutti
232400110328      ****************
232500110328     c                   elseIf      Trovata_Tab_TB ='N'
232600090210      **
232700090210     c                   if              TOD_char3 <> *blank
232800100705      *   Cerca la decodifica del dato in TABELLA
232900120424     C                   movel(p)  Tipo_Seg      etbSGM
233000100705     C                   movel(p)  TOD_char3     etbVALSGM
233100100705     c                   exsr      In_TAB_EDSTBL
233200090210      *
233300100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
233400100705      *
233500090210     c                   SELECT
233600100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
233700100705     c                               VABcas > *zeros
233800090210     C                   movel     '4'           VABCBO                         + C/Ass
233900100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
234000090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
234100090210      *  Porto Assegnato
234200090210     c                   WHEN      VABCAS > *zeros
234300090210     C                   movel     '6'           VABCBO                         + C/Ass
234400090210     C                   OTHER
234500090210     C                   movel     '2'           VABCBO                         Senza C/Ass
234600090210     C                   ENDSL
234700090210      *
234800090210     c                   end
234900090210     c                   endIF
235000090209      **
235100110328     c                   ENDif
235200110328      **
235300090210      **  Il tipo bolla non è presente
235400090210     c                   if        VABCBO = *blank
235500090210     C                   eval      vinMSG = 'TOD non rilevato'
235600090213     c                   exsr      Error_DET
235700090210     c                   goto      end_TOD
235800090210     c                   end
235900090210      **
236000090210     c     end_TOD       endSR
236100090213      * ?================================================================== */
236200090213     C*?  Anagrafica del mittente e del destinatario
236300090213      * ?================================================================== */
236400091016     C     DATI_NAD      BEGSR
236500100705      *
236600100705      *   Cerca la decodifica del dato in TABELLA
236700120424     C                   movel(p)  Tipo_Seg      etbSGM
236800100705     C                   movel(p)  nad3035       etbVALSGM
236900100705     c                   exsr      In_TAB_EDSTBL
237000100708      *-------
237100120629      * Dato (SHIPPED FROM) usato principalmente x CMR
237200120629      *       ma anche usato al posto di un RFF+FF in testata messaggio
237300120629      *   quindi modificato il nome di riferimento del campo sul EDSTBL
237400120629      *   x renderlo più generico.
237500100708      *-------
237600120629     C********************        IF        Quale_campo ='VABCMR'
237700120629     C                   IF        Quale_campo ='NAD_SF'
237800120629      *-------
237900120629      * Dati di PROVENIENZA MESSAGGIO
238000120629      *-------
238100120629      ***
238200120629      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
238300100708     c                   if        nad30361 <> *blank
238400120629     C                   MOVEL     nad30361      NAD_SF_Cliente
238500100708     c                   elseIf    nad31241 <> *blank
238600120629     C                   MOVEL     nad31241      NAD_SF_Cliente
238700100708     c                   elseIf    nad3039  <> *blank
238800120629     C                   MOVEL     nad3039       NAD_SF_Cliente
238900100708     c                   end
239000120629      ***
239100120629      * ?SOLO SE NON c'è un RFF+FF: di Testata che prevale su tutto.
239200120629      * ?La PLATFORM LIST è usata come il Codice Cliente su RFF+FF:PLATFORMLIST
239300120629      ***
239400120629      * Se partner/cliente è abilitato alla funzione di sostituzione CLIENTE da UNB.
239500120629      *   Il (SHIPPED FROM) in testata funziona come un RFF+FF: a livello di testata.
239600120629      *    per agganciare i dati sulla tab."CL".
239700120629      ***
239800120629     C                   if         PZ_abilita_NAD_SF_come_RFF_FF ='S'
239900120629     c                                 and Tes_RFF_FF_ksc = 0
240000120629      * ?Ma solo se NON c'è un RFF+FF: di Testata che prevale su tutto.
240100120629      *
240200120629      *   imposta la PLATFORM LIST ricevuta del MITTENTE
240300120629      *    questo codice viene poi utilizzato sulla tab.CL (EDTAB) se concordato
240400120629      *    per codificare tutte le spedizioni come un RFF+FF:xxxxxxxxxxxxxxxxx di testata
240500120629     c                   eval      NAD_SF_Cliente = NAD3039
240600120629      *>>>>>>>>
240700120629     C*------------------
240800120629      *    Aggancia il Cliente e tutte le caratteristiche da tab.:"CL"
240900120629      * ?RFF+FF:
241000120629     C                   clear                   Rifer_Cliente    35
241100120629     C                   movel     NAD_SF_ClienteRifer_Cliente
241200120629     c                   exsr      CL_Particolare
241300120629     C*------------------
241400120629      *>>>>>>>>
241500120629     c                   end
241600120629      ***
241700100705      *-------
241800090210      * Dati destinatario
241900100705      *-------
242000100708     C                   elseIF    Quale_campo ='VABRSD'
242100090227      *
242200090227      * Se trovato un indirizzo di destinatario
242300100701     c                   if           Dati_di_Testata = 'N'
242400100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
242500100701     c                   else
242600100701     c                   eval       NAD_destinatario_in_testata = 'S'
242700100701     c                   end
242800090210      *
242900090210     C* Reperisco nazione corrispondente destinatario
243000100705     C                   Z-ADD     1             naz               3 0
243100100705     C                   MOVEL     *BLANKS       VABNZD
243200100705     C                   Z-ADD     0             WFLCPF
243300090210     c                   eval      *in32 = *off
243400100705      ***
243500090210     C                   if        nad3207 <> *BLANKS
243600111028     C     nad3207       LOOKUP    ISO(naz)                               32
243700100705     C                   If        *in32 = *on
243800100705      ***
243900100705      * per reperire il CAP corretto
244000100705     C                   MOVEL     C15(naz)      VABNZD
244100100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
244200100705      ***
244300100705     c                   Endif
244400100705     C                   endif
244500100705      *
244600090210     C* Imposto dati destinatario
244700100705     C                   MOVEL     nad30361      VABRSD
244800100705     C                   if        nad30362 <> *LOVAL
244900100705     C                   MOVEL     nad30362      VABRD2
245000090210     C                   endif
245100100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
245200091019???? C* Imposto dati destinatario
245300091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
245400100705???? C                   MOVEL     nad31241      VABRSD
245500100705???? C                   if        nad31242 <> *LOVAL
245600100705???? C                   MOVEL     nad31242      VABRD2
245700091019???? C                   endif
245800091019???? C                   end
245900100705      *   città/Località
246000090210     C                   MOVEL     nad3164       VABLOD
246100100705      *   Indirizzo
246200100705     C                   MOVEL     nad30421      VABIND
246300111121      *
246400111121     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
246500111121     c                                         %len(%trim(NAD30422))
246600111121???? c                   eval      NAD_Lunghezza_Indirizzo =
246700111121     c                                         %len(%trim(VABIND))
246800111121???? c                   eval      NAD_Lunghezza_ragSociale2 =
246900111121     c                                         %len(%trim(VABRD2))
247000111116      *
247100111121      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
247200111121      *  lo aggiunge nel proseguimento della Ragione Sociale
247300111121     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
247400111121      **
247500111121     c                             and  %len(VABIND) <
247600111121     c                                  NAD_Lunghezza_Indirizzo +
247700111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
247800111121      *
247900111121      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
248000111121      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
248100111121     c                             and  %len(VABRD2) -
248200111121     c                                  NAD_Lunghezza_ragSociale2 -
248300111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
248400111121     c                                  >= 0
248500111121      *
248600111121      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
248700111121     c                             and  NAD_Lunghezza_ragSociale2 <
248800111121     c                                  NAD_Lunghezza_Indirizzo
248900111121      *
249000111121     C     '    '        SCAN      VABRD2        WI
249100111121     c                   if        %Found
249200111121     C                   MOVEA     VABRD2        LOD
249300111121     C                   ADD       1             WI
249400111121     C                   MOVEA     NAD30422      LOD(WI)
249500111121     C                   MOVEA     LOD           VABRD2
249600111121     c                   end
249700111121      *
249800111121      * Altrimenti lo aggiunge direttamente sulla VIA
249900111121     c                   else
250000100705      * Se NAD30422  non è vuoto
250100111116      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
250200111116      *
250300111116     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
250400090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
250500111121      *
250600100705     C* Verifico se nella località c'è dello spazio
250700111116     C     '    '        SCAN      VABIND        WI
250800100705     c                   if        %Found
250900111116     C                   MOVEA     VABIND        LOD
251000090210     C                   ADD       1             WI
251100090210     C                   MOVEA     NAD30422      LOD(WI)
251200111116     C                   MOVEA     LOD           VABIND
251300100705     c                   else
251400111121      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
251500111116     C     '    '        SCAN      VABLOD        WI                4 0
251600100705     c                   if        %Found
251700111116     C                   MOVEA     VABLOD        LOD
251800100705     C                   ADD       1             WI
251900100705     C                   MOVEA     NAD30422      LOD(WI)
252000111116     C                   MOVEA     LOD           VABLOD
252100100705     C                   ENDif
252200090210     C                   ENDif
252300100705      *
252400100705     C                   ENDif
252500111121      *
252600111121     c                   end
252700111121      *
252800111116      **************
252900111116      *** PRIMA provava ad aggiungere prima alla località......
253000111116      **************
253100111116     C*  aggiungo alla LOCALITA' quanto di indirizzo ce n'è in più
253200111116     c*************      if        NAD30422 <> *blanks and NAD30422 <> *loval
253300111116     C* Verifico se nella località c'è dello spazio
253400111116     C*****'    '        SCAN      VABLOD        WI                4 0
253500111116     c*************      if        %Found
253600111116     C*************      MOVEA     VABLOD        LOD
253700111116     C*************      ADD       1             WI
253800111116     C*************      MOVEA     NAD30422      LOD(WI)
253900111116     C*************      MOVEA     LOD           VABLOD
254000111116     c*************      else
254100111116      *  Altrimenti se non c'è spazio dopo la località prova a concatenarlo alla via
254200111116     C*****'    '        SCAN      VABIND        WI
254300111116     c*************      if        %Found
254400111116     C*************      MOVEA     VABIND        LOD
254500111116     C*************      ADD       1             WI
254600111116     C*************      MOVEA     NAD30422      LOD(WI)
254700111116     C*************      MOVEA     LOD           VABIND
254800111116     C*************      ENDif
254900111116     C*************      ENDif
255000111116      *************
255100111116     C*************      ENDif
255200111116      *
255300111116      *
255400111116      *
255500111116      *
255600101027      * RIPORTO LA 2° RAG.SOCIALE NELL'INDIRIZZO.
255700101027     c                   if        vabIND = *blank and VABRD2 <> *BLANK
255800101027      *
255900101027     C                   EVAL      VABIND = VABRD2
256000101027      *
256100101027     C                   ENDif
256200090210      * Converto CAP
256300100705     C                   EXSR      Converte_CAP
256400090603     C*
256500100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
256600100705     C*  record del VAB viene PULITO
256700100316     c                   if        Dati_di_Testata  = 'S'
256800090603     c                   eval       tes_VABrsd = VABrsd
256900090603     c                   eval       tes_VABrd2 = VABrd2
257000090603     c                   eval       tes_VABind = VABind
257100090603     c                   eval       tes_VABlod = VABlod
257200090603     c                   eval       tes_VABnzd = VABnzd
257300090603     c                   end
257400100728      *
257500100728      *   se NON c'è l'identificativo bolla del cliente
257600100728      *    blocca la spedisione e lo segnala.
257700100728     C                   if          vabRSD = *blank
257800100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
257900130315     c******             exsr      Error_DET
258000130315     c******             goto      end_NAD
258100100728     C                   elseIF      vabIND = *blank
258200100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
258300101006     c*********          exsr      Error_DET
258400101006     c*********          goto      end_NAD
258500100728     C                   elseIF      vabLOD = *blank
258600100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
258700101006     c*********          exsr      Error_DET
258800101006     c*********          goto      end_NAD
258900100728     C                   ENDIF
259000100705      *-------
259100090210      * Dati mittente
259200100705      *-------
259300100705     C                   elseIF     Quale_campo ='VABRMO'
259400090210      *
259500100705     C                   movel     nad30361      VABRMO
259600090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
259700090210     C                   movel     §PTNAZ        VABNMO
259800090210      *
259900090210     C                   IF        nad3207 <> *blanks
260000100705     C                   eval      naz = 1
260100100705     C     nad3207       LOOKUP    ISO(naz)                               32
260200090210     C                   if         *in32 = *on
260300100705     C                   MOVEL     C15(naz)      VABNMO
260400090210     C                   endif
260500090210     C                   ENDIF
260600090210      *
260700090210      * Normalizzo CAP mittente originale
260800090210     C                   clear                   T
260900090210     C                   clear                   S
261000090210     C                   MOVEA     *BLANKS       CAPM
261100090210     C                   MOVEA     nad3251       CAP9
261200090210     C                   DO        9             T
261300090210     C     CAP9(T)       IFNE      *BLANKS
261400090210     C                   ADD       1             S
261500090210     C                   MOVE      CAP9(T)       CAPM(S)
261600090210     C                   ENDIF
261700090210     C                   ENDDO
261800090210     C*
261900090210      *     Controllo validità CAP
262000090210     C                   MOVEA     CAPM          VABCMO
262100090210      *
262200090210     C                   CLEAR                   TISI95DS
262300090210     C                   MOVEL     VABCMO        I95CAP
262400090210     C                   MOVEL     VABNMO        I95NAR
262500090210     C                   MOVEL     WOGGI         I95DAT
262600090210     C                   MOVEL     '3'           I95TCN
262700090210     C                   CALL      'TISI95R'
262800090210     C                   PARM                    TISI95DS
262900090210      *     Errore
263000090210     C     O95ERR        IFNE      *BLANKS
263100090210     C                   MOVEL     INDCAE        VABCMO
263200090210     C                   MOVEL     §PTNAZ        VABNMO
263300090210     C                   END
263400090603      *
263500100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
263600100705     C*  record del VAB viene PULITO
263700100316     c                   if        Dati_di_Testata  = 'S'
263800090603     c                   eval      tes_VABrmo = VABrmo
263900090603     c                   eval      tes_VABnmo = VABnmo
264000090603     c                   eval      tes_VABcmo = VABcmo
264100090603     c                   eval      NAD_mittente_in_testata = 'S'
264200100701     c                   else
264300100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
264400090603     c                   end
264500100728      *
264600100728      *   se NON c'è l'identificativo bolla del cliente
264700100728      *    blocca la spedisione e lo segnala.
264800100728     C                   if          vabRMO = *blank
264900100728     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
265000101006     c*********          exsr      Error_DET
265100101006     c*********          goto      end_NAD
265200100728     C                   ENDIF
265300090210      *
265400090210     C                   END
265500090210      *
265600090210     c     end_NAD       endSR
265700090210      * ?================================================================== */
265800090210     C*?  Contatto a cui far riferimento
265900090210      * ?================================================================== */
266000090210     C     DATI_CTA      BEGSR
266100090210      *
266200090210      *  memorizza il riferimento di consegna x il destinatario
266300090210     C     cta3412       ifne      *BLANKS
266400100701     c                   if        Dati_di_Testata  ='S'
266500100701     c                   eval       CTA_Destinatario_in_testata   = cta3412
266600100701     c                   else
266700100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
266800100701     c                   end
266900100701     c                   end
267000090210      *
267100090210     c                   endSR
267200090210      * ?================================================================== */
267300090210     C*?  Contatto telefonico  a cui far riferimento
267400090210      * ?================================================================== */
267500090210     C     DATI_COM      BEGSR
267600090210      *
267700100705      *  memorizza il riferimento di consegna nr.telefonico
267800100705     C                   if        com3148 <> *BLANKS    and
267900100705     C                              com3155 = Telephone_Number
268000100701     c                   if        Dati_di_Testata  ='S'
268100100701     c                   eval      COM_telefono_in_testata   = com3148
268200100701     c                   else
268300100701     c                   eval      COM_telefono_in_dettaglio = com3148
268400090210     c                   end
268500100705     c                   endif
268600090210      *
268700090209     C                   ENDSR
268800090210      * ?================================================================== */
268900090210     C*?  Dati di dettaglio della spedizione e dei colli
269000090210      * ?================================================================== */
269100090210     C     DATI_GID      BEGSR
269200090210      *
269300090210     C* Se ho totali per spedizione metto in campi VAB
269400090210     C     gid1496       IFEQ      99999
269500090210     C     gid1496       orEQ      9999
269600100630     c                   eval      GID_aTotale = 'S'
269700090210     C                   z-add     gid722420     VABNCL
269800090210     C                   ELSE
269900100630     c                   If        GID_aTotale = 'N'
270000100705     C                   z-add     gid722420     GID_ColliInAdd
270100100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
270200090210     C                   END
270300090210     C                   END
270400090212      *
270500100701     C                   ADD       VABNCL        ToT_Colli        16 0
270600090210      *
270700090210     c     end_GID       endSR
270800090210      * ?================================================================== */
270900090210     C*?  Dati di dettaglio misure spedizione e colli
271000090210      * ?================================================================== */
271100090210     C     DATI_MEA      BEGSR
271200090210      *
271300090210     C*  Peso
271400100319     C     mea6311       IFEQ      Weight
271500100319     C     mea6411       ANDEQ     Kilograms
271600090210      *
271700090210     C*  Controllo se ho valore totale o parziale
271800090210      *   sempre il Lordo
271900100319???? C     mea6313       IFEQ      G_Weight
272000100319???? C     mea6313       oreq      Gross_Weight
272100100706???? C     mea6313       oreq      Gross_Weight_E
272200110512???? C     mea6313       oreq      Gross_Weight_D
272300101027???? C     mea6313       oreq      *blank
272400100630     C                   IF        GID_aTotale = 'S'
272500091019???? C                   move      mea6314       mea6314_2        18 2          Peso
272600091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
272700090210     C                   ELSE
272800091019???? C                   move      mea6314       mea6314_2                      Peso
272900091019???? C                   ADD       mea6314_2     VABPKB                         Peso
273000090210     C                   END
273100091203     C                   END
273200090210      *
273300100319???? C     mea6313       IFEQ      G_Weight
273400100319???? C     mea6313       oreq      Gross_Weight
273500100706???? C     mea6313       oreq      Gross_Weight_E
273600110512???? C     mea6313       oreq      Gross_Weight_D
273700101027???? C     mea6313       oreq      *blank
273800091019???? C                   move      mea6314       mea6314_2                      Peso
273900100701???? C                   ADD       mea6314_2     ToT_PesoLordo
274000090210     C                   END
274100100706???? C     mea6313       IFEQ      N_Weight
274200100706???? C     mea6313       oreq      Net_Weight
274300100706???? C     mea6313       oreq      Gross_Weight_E
274400110512???? C     mea6313       oreq      Gross_Weight_D
274500101027???? C     mea6313       oreq      *blank
274600091019???? C                   move      mea6314       mea6314_2                      Peso
274700100701???? C                   ADD       mea6314_2     ToT_PesoNetto
274800090210     C                   END
274900090210     C                   END
275000090210      ***
275100090210     C*  Peso in Grammi  se abilitato a utilizzarlo
275200090210      ***
275300100319     C     mea6311       IFEQ      Weight
275400100319     C     mea6411       ANDEQ     Grams
275500090210      *
275600090210     C*  Controllo se ho valore totale o parziale
275700100319???? C     mea6313       IFEQ      G_Weight
275800100319???? C     mea6313       oreq      Gross_Weight
275900100706???? C     mea6313       oreq      Gross_Weight_E
276000110512???? C     mea6313       oreq      Gross_Weight_D
276100101027???? C     mea6313       oreq      *blank
276200100630     c                   If        GID_aTotale = 'S'
276300091019???? c                   move      mea6314       mea6314_2
276400091019???? C     mea6314_2     div       1000          VABPKB                         Peso
276500090210     C                   ELSE
276600091019???? c                   move      mea6314       mea6314_2
276700091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
276800091019???? C                   add       Peso_inKG     VABPKB                         Peso
276900090210     C                   END
277000090210     C                   END
277100090210      *
277200100319???? C     mea6313       IFEQ      G_Weight
277300100319???? C     mea6313       oreq      Gross_Weight
277400100706???? C     mea6313       oreq      Gross_Weight_E
277500110512???? C     mea6313       oreq      Gross_Weight_D
277600101027???? C     mea6313       oreq      *blank
277700091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
277800100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
277900090210     C                   END
278000100706???? C     mea6313       IFEQ      N_Weight
278100100706???? C     mea6313       oreq      Net_Weight
278200100706???? C     mea6313       oreq      Gross_Weight_E
278300110512???? C     mea6313       oreq      Gross_Weight_D
278400101027???? C     mea6313       oreq      *blank
278500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
278600100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
278700090210     C                   END
278800090210     C                   END
278900090210      *  Volume
279000100319???? C     mea6311       IFEQ      Volume
279100100319???? C     mea6411       ANDEQ     Cubic_Meters
279200090210      *  Controllo se ho valore totale o parziale
279300100630     c                   If        GID_aTotale = 'S'
279400091019???? C                   move      mea6314       mea6314_2        18 2          Volume
279500091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
279600090210     C                   ELSE
279700091019???? C                   move      mea6314       mea6314_2        18 2          Volume
279800091019???? C                   ADD       mea6314_2     VABVLB                         Volume
279900090210     C                   END
280000090210     C                   END
280100100705      *
280200100705      *
280300100705     C* Ricerco CAP in base alle dimensioni
280400090210     C     WCAP          IFNE      *BLANKS
280500100319     C     mea6311       IFEQ      Weight
280600090210      *
280700090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
280800090210     C                   CLEAR                   FNLV55DS
280900090210     C                   MOVEL     'P'           D55TPT
281000090210     C                   MOVEL     WOGGI         D55DRF
281100090210     C                   MOVEL     VABLNP        D55LNP
281200090210     C                   MOVEL     VABLNP        D55LIN
281300090210     C                   CALL      'FNLV55R'
281400090210     C                   PARM                    FNLV55DS
281500090210     C                   MOVE      D55TFP        COMTFP            3 0
281600090210      *
281700090210     C*  PASSO IL TERMINAL PARTENZA
281800090210     C                   CLEAR                   TISI95DS
281900090210     C                   Z-ADD     COMTFP        I95TFP
282000090210     C                   MOVEL     VABTSP        I95TSP
282100090210     C                   MOVEL     'S'           I95FRE
282200090210     C                   MOVEL     '3'           I95TCN
282300090210     C                   MOVEL     WCAP          I95CAP
282400090210     C                   MOVEL     VABNZD        I95NAR
282500090210     C                   MOVEL     VABLOD        I95LOC
282600090210     C                   MOVEL     VABIND        I95IND
282700090210      *
282800090210     C* Se gestita seconda linea di arrivo passo peso - volume
282900090210     C* e numero colli effettivo
283000090210     C     §PULNA        IFEQ      'S'
283100090210     C                   Z-ADD     VABPKB        I95LKG
283200090210     C                   Z-ADD     VABVLB        I95LMC
283300090210     C                   ELSE
283400100706     C*******
283500090210     C* Altrmenti passo 1 Kg e 0,001
283600100706     C*******              Z-ADD1                  I95LKG
283700100706     C*******              Z-ADD0,001              I95LMC
283800100706     C*******
283900090210     C                   END
284000090210     C*
284100090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
284200090210     C                   SELECT
284300090210     C     VABCBO        WHENEQ    '1 '
284400090210     C                   MOVEL     *BLANKS       I95FCA
284500090210     C                   MOVEL     'F'           I95TPO
284600090210     C     VABCBO        WHENEQ    '2 '
284700090210     C                   MOVEL     *BLANKS       I95FCA
284800090210     C                   MOVEL     'A'           I95TPO
284900090210     C     VABCBO        WHENEQ    '4 '
285000090210     C                   MOVEL     'S'           I95FCA
285100090210     C                   MOVEL     'F'           I95TPO
285200090210     C     VABCBO        WHENEQ    '6 '
285300090210     C                   MOVEL     'S'           I95FCA
285400090210     C                   MOVEL     'A'           I95TPO
285500090210     C                   OTHER
285600090210     C                   MOVEL     'F'           I95TPO
285700090210     C     VABCAS        IFGT      0
285800090210     C                   MOVEL     'S'           I95FCA
285900090210     C                   ELSE
286000090210     C                   MOVEL     *BLANKS       I95FCA
286100090210     C                   END
286200090210     C                   ENDSL
286300090210     C*
286400090210     C                   CALL      'TISI95R'
286500090210     C                   PARM                    TISI95DS
286600090210     C* Reperisco i dati da CAP
286700090210     C                   MOVEL     O95PRV        VABPRD
286800100706     C                   MOVEL     WCAP          VABCAD
286900100114      ***
287000100706     c                   MOVEL     O95LNA        VABLNA
287100100706     C                   MOVEL     O95ZNC        VABZNC
287200090210     C*
287300090210     C     O95ERR        IFNE      *BLANKS
287400090210     C     O95FIT        ANDEQ     'S'
287500090210     C*  Cap non trovato
287600090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
287700101006     c*********          exsr      Error_DET
287800101006     C*********          goto      end_MEA
287900090210     C                   END
288000090210     C                   END
288100090210      *
288200090210     C                   ELSE
288300090210      *
288400090210     C                   CLEAR                   VABPRD
288500090210     C                   CLEAR                   VABLNA
288600090210     C                   CLEAR                   VABZNC
288700090210     C                   CLEAR                   VABCAD
288800090210     C*  Cap non trovato
288900090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
289000101006     c*********          exsr      Error_DET
289100101006     C*********          goto      end_MEA
289200090210     C                   END
289300090210      *
289400090210     c     end_MEA       endSR
289500090210      * ?================================================================== */
289600090210     C*?  Dati di dettaglio Codice a barre Segnacolli
289700090210      * ?================================================================== */
289800090210     C     DATI_PCI      BEGSR
289900090210      *
290000090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
290100090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
290200090210     C     §1BF12        IFEQ      'E'
290300100706     C                   EXSR      SEGNaCollo_EEX
290400090210     C                   ELSE
290500090210      *
290600090210      *  La prima volta controllo congruità numero segnacollo
290700090210     C     §PUBS1        IFNE      *BLANKS
290800090210     C     VABnrs        ANDNE     0
290900090210      *  Se il codice trattamento merce prevede che segnacollino i
291000090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
291100090210     C     §1BFG7        IFEQ      'S'
291200090210     C     §1BFG1        OREQ      'S'
291300090210      *  calcolo segnacollo
291400100706     C                   EXSR      Segn_Bartolini
291500090210     C                   END
291600090210     C                   END
291700090210      *
291800090210     C                   END
291900090210      *
292000090210     c     end_PCI       endSR
292100090211     C*----------------------------------------------------------------
292200090211      *? dati finali del dettaglio    UNT
292300090211     C*----------------------------------------------------------------
292400090211     C     DATI_UNT      BEGSR
292500110324      *
292600110324      * Non deve dare messaggio
292700110324     c                   goto      No_UNTctrl
292800090211      *
292900100708     c                   eval      UNT_record = vnREC
293000090213     c                   z-add     vnrec         last_RECord
293100090211      **
293200100319      **   verifica la quadratura di tutti i segmenti ricevuti
293300100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
293400100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
293500100728     C     UNT0074       IFne      Conta_segmenti
293600100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
293700100319     c                             trollare il Messaggio ricevuto'
293800100319      *
293900100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
294000100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
294100100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
294200100319     C                             Bolle import in filiale - messaggio -
294300100319     C                             con quadratura UNT errata controllare EDI -
294400100319     C                             ricevuto su UPLOAD x il cliente :'
294500100319     C                   EVAL      Messaggio = %trim(Messaggio) +
294600100319     C                                         %editc(§PTksc:'X')
294700100319     C                   EVAL      Messaggio = %trim(Messaggio) +
294800100319     C                             ' i segmenti contati sono :' +
294900100319     C                             %editc(Conta_segmenti:'Z')
295000100319     c                   if        §PVinv <> 'N'
295100100319     c                   exsr      invio_mail
295200100319     c                   end
295300100319     c                   end
295400110321     c     No_UNTctrl    tag
295500100319      **
295600100319     c     End_UNT       endSR
295700100316     C*----------------------------------------------------------------
295800100316      *? dati finali     UNT
295900100316     C*----------------------------------------------------------------
296000100316     C     DATI_UNZ      BEGSR
296100100316      *
296200100726     c                   move      'S'           Forza_Uscita
296300100316      **
296400100316     c                   endSR
296500090210      *----------------------------------------------------------------
296600090210      *? Input segnacolli su schiera di 20 elementi
296700090210      *----------------------------------------------------------------
296800090210     C     INP_Segnacol  BEGSR
296900090210      *
297000090210     c                   clear                   quanti_PCI        3 0
297100090210     c                   if        §puEL1 = 0
297200090210     c                   z-add     10            quanti_PCI
297300090210     c                   else
297400090210     c                   z-add     §puEL1        quanti_PCI
297500090210     c                   end
297600090210      *
297700090210      *  Pulisce la schiera dei segnacolli da elaborare
297800090210     c                   clear                   x30
297900090210     c                   z-add     0             Nr_x30            3 0
298000090210      *
298100090210      *   riporto tutto sulla schiera generica X30
298200090210      *
298300090210     c                   do        10            limite            3 0
298400090210      * Limita quanti elementi passati in ogni PCI
298500090210      *   devono essere considerati
298600090210     c                   if        limite > quanti_PCI
298700090210     c                   Leave
298800090210     c                   end
298900090210      *
299000090210      *  Carica la schiera generale di tutti i segnacolli
299100090210     c                   if        D30_1(limite) <> *blank
299200090210     c                   add       1             Nr_x30
299300090210     c                   eval      X30(Nr_x30) = D30_1(limite)
299400090210     c                   end
299500090210     c                   enddo
299600090210      *
299700090210     C                   ENDSR
299800090210      *----------------------------------------------------------------
299900090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
300000090210      *----------------------------------------------------------------
300100100706     C     SEGNaCollo_EEXBEGSR
300200090210      *
300300090210     c                   exsr      INP_Segnacol
300400090210      *
300500090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
300600090210     C                   DO        10            x                              --- 01 ---
300700090210      *
300800090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
300900090210     C     x30(X)        ANDNE     *LOVAL
301000090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
301100100701     C                   ADD       1             totale_PCI
301200100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
301300090210     C                   END                                                    --- 03 ---
301400090210      *
301500090210     C                   END                                                    --- 02 ---
301600090210      *
301700090210     C                   ENDSR
301800090210     C*----------------------------------------------------------------
301900090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
302000090210     C*----------------------------------------------------------------
302100100706     C     Segn_BartoliniBEGSR
302200090210      *
302300090210     c                   exsr      INP_Segnacol
302400090210     C*
302500090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
302600090210     C*  in base al numero elementi validi carico sgn.
302700090210     C                   DO        10            X                              --- 01 ---
302800090210     C*
302900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
303000090210     C     x30(X)        ANDNE     *LOVAL
303100090210     C*
303200100630     C                   CLEAR                   DS_SegnBART
303300090210     C*  Controllo se devo ricevere la linea di partenza
303400090210     C                   MOVEL     'N'           WERRO             1
303500090210     C     §PULP1        IFGT      0                                            --- 03 ---
303600100706     C                   EXSR      REPerisce_LNP
303700090210     C                   END                                                    --- 03 ---
303800100706      *
303900090210     C*  Controllo se devo ricevere la linea di arrivo
304000090210     C     §PULA1        IFGT      0                                            --- 03 ---
304100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
304200100706     C                   EXSR      REPerisce_LNA
304300090210     C                   END                                                    --- 03 ---
304400100706      *
304500090210     C*  Controllo se devo ricevere il numero di serie
304600090210     C     §PUNR1        IFGT      0                                            --- 03 ---
304700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
304800100706     C                   EXSR      REPerisce_NRS
304900090210     C                   END                                                    --- 03 ---
305000100706      *
305100090210     C*  Controllo se devo ricevere il numero segnacollo
305200090210     C     §PUSG1        IFGT      0                                            --- 03 ---
305300090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
305400100706     C                   EXSR      REPerisce_SGN
305500090210     C                   END                                                    --- 03 ---
305600100706      *
305700090210     C*  Controllo se devo ricevere la zona di consegna
305800090210     C     §PUZN1        IFGT      0                                            --- 03 ---
305900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
306000100706     C                   EXSR      REPerisce_ZNC
306100090210     C                   END                                                    --- 03 ---
306200100706      *
306300090210     C*  Imposto da VAB i dati a zero se mi è stato passato
306400090210     C*  numero segnacollo valido
306500090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
306600100706      *
306700100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
306800100630     C                   Z-ADD     VABLNP        SegnBART_LNP
306900090210     C                   END                                                    --- 04 ---
307000100706      *
307100100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
307200100630     C                   Z-ADD     VABLNA        SegnBART_LNA
307300090210     C                   END                                                    --- 04 ---
307400100706      *
307500100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
307600100630     C                   Z-ADD     VABNRS        SegnBART_NRS
307700090210     C                   END                                                    --- 04 ---
307800100706      *
307900100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
308000100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
308100090210     C                   END                                                    --- 04 ---
308200090210      *
308300090210     C*  mi reimposto LNA e zona da segnacollo
308400100114      ***
308500100114      *** dovendo accorpare le spedizioni da Conferma x CMR
308600100114      ***  non deve impostare l'instradamento.
308700100630     C                   Z-ADD     SegnBART_LNA  VABLNA
308800100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
308900100114      ***
309000090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
309100100701     C                   ADD       1             Conta_Segn
309200100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
309300090210     C                   END                                                    --- 03 ---
309400090210     C*
309500090210     C                   END                                                    --- 02 ---
309600090210     C*
309700090210     C                   END                                                    --- 01 ---
309800090210      *
309900090210     C                   ENDSR
310000090210     C*----------------------------------------------------------------
310100090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
310200090210     C*----------------------------------------------------------------
310300100706     C     REPerisce_LNP BEGSR
310400090210     C*
310500090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
310600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
310700090210     C*  dal numero segnacollo passatomi dal partner
310800090210     C                   Z-ADD     §PULP1        WP                2 0
310900090210     C     3             SUBST     x30(X):WP     WLNP              3
311000090210     C*
311100090210     C*  controllo che la lnp calcolata sia numerica
311200090210     C                   TESTN                   WLNP                 98
311300090210     C*
311400090210     C*  se è numerica la carico
311500090210     C     *IN98         IFEQ      '1'
311600100630     C                   MOVE      WLNP          SegnBART_LNP
311700090210     C                   ELSE
311800090210     C*  se non è numerica stampo errore
311900090210     C                   MOVEL     'S'           WERRO             1
312000090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
312100090213     c                   exsr      Error_DET
312200090210     C                   goto      end_LNP
312300090210     C                   END
312400090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
312500090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
312600100630     C     SegnBART_LNP  IFNE      VABLNP
312700090210     C                   MOVEL     'S'           WERRO             1
312800090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
312900090210     c                             a della Bolla'
313000090213     c                   exsr      Error_DET
313100090210     C                   goto      end_LNP
313200090210     C                   END
313300090210     C*
313400090210     C     end_LNP       ENDSR
313500090210     C*----------------------------------------------------------------
313600090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
313700090210     C*----------------------------------------------------------------
313800100706     C     REPerisce_LNA BEGSR
313900090210     C*
314000090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
314100090210     C*  (a partire dalla posizione iniziale indicata in tabella)
314200090210     C*  dal numero segnacollo passatomi dal partner
314300090210     C                   Z-ADD     §PULA1        WP                2 0
314400090210     C     3             SUBST     x30(X):WP     WLNA              3
314500090210     C*  controllo che la lna calcolata sia numerica
314600090210     C                   TESTN                   WLNA                 98
314700090210     C*  se è numerica la carico
314800090210     C     *IN98         IFEQ      '1'
314900100630     C                   MOVE      WLNA          SegnBART_LNA
315000090210     C                   ELSE
315100090210     C*  se non è numerica stampo errore
315200090210     C                   MOVEL     'S'           WERRO             1
315300090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
315400090213     c                   exsr      Error_DET
315500090210     C                   goto      end_LNA
315600090210     C                   END
315700090210     C*
315800090210     C     end_LNA       ENDSR
315900090210     C*----------------------------------------------------------------
316000090210     C*? REPNRS - Reperisce numero serie
316100090210     C*----------------------------------------------------------------
316200100706     C     REPerisce_NRS BEGSR
316300090210     C*
316400090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
316500090210     C*  (a partire dalla posizione iniziale indicata in tabella)
316600090210     C*  dal numero segnacollo passatomi dal partner
316700090210     C                   Z-ADD     §PUNR1        WP                2 0
316800090210     C     2             SUBST     x30(X):WP     WNRS              2
316900090210     C*  controllo che il nrs calcolata sia numerico
317000090210     C                   TESTN                   WNRS                 98
317100090210     C*  se è numerica la carico
317200090210     C     *IN98         IFEQ      '1'
317300100630     C                   MOVE      WNRS          SegnBART_NRS
317400090210     C                   ELSE
317500090210     C*  se non è numerico stampo errore
317600090210     C                   MOVEL     'S'           WERRO             1
317700090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
317800090213     c                   exsr      Error_DET
317900090210     C                   goto      end_NRS
318000090210     C                   END
318100090210      *
318200090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
318300090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
318400100630     C     SegnBART_NRS  IFNE      VABNRS
318500090210     C                   MOVEL     'S'           WERRO             1
318600090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
318700090210     c                             o della Bolla'
318800090213     c                   exsr      Error_DET
318900090210     C                   goto      end_NRS
319000090210     C                   END
319100090210     C*
319200090210     C     end_NRS       ENDSR
319300090210     C*----------------------------------------------------------------
319400090210     C*? REPZNC - Reperisce zona consegna
319500090210     C*----------------------------------------------------------------
319600100706     C     REPerisce_ZNC BEGSR
319700090210     C*
319800090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
319900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
320000090210     C*  dal numero segnacollo passatomi dal partner
320100090210     C                   Z-ADD     §PUZN1        WP                2 0
320200090210     C     2             SUBST     x30(X):WP     WZNC              2
320300090210     C*  controllo che la zona calcolata sia numerica
320400090210     C                   TESTN                   WZNC                 98
320500090210     C*  se è numerica la carico
320600090210     C     *IN98         IFEQ      '1'
320700100630     C                   MOVE      WZNC          SegnBART_ZNC
320800090210     C                   ELSE
320900090210     C*  se non è numerica stampo errore
321000090210     C                   MOVEL     'S'           WERRO             1
321100090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
321200090213     c                   exsr      Error_DET
321300090210     C                   goto      end_ZNC
321400090210     C                   END
321500090210     C*
321600090210     C     end_ZNC       ENDSR
321700090210     C*----------------------------------------------------------------
321800090210     C*? REPSGN - Reperisce numero segnacollo
321900090210     C*----------------------------------------------------------------
322000100706     C     REPerisce_SGN BEGSR
322100090210     C*
322200090210     C*  se viene passata nr.segnacollo estraggo sottostringa
322300090210     C*  lunga 7 chr (a partire dalla posizione iniziale
322400090210     C*  indicata in tabella)
322500090210     C*  dal numero segnacollo passatomi dal partner
322600090210     C                   Z-ADD     §PUSG1        WP                2 0
322700090210     C     7             SUBST     x30(X):WP     WSGN              7
322800090210     C*  controllo che il nr.segnacollo calcolato sia numerico
322900090210     C                   TESTN                   WSGN                 98
323000090210     C*  se è numerico lo carico
323100090210     C     *IN98         IFEQ      '1'
323200100630     C                   MOVE      WSGN          SegnBART_NSC
323300090210     C                   ELSE
323400090210     C*  se non è numerica stampo errore
323500090210     C                   MOVEL     'S'           WERRO             1
323600100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
323700101006     c*********          exsr      Error_DET
323800101006     C*********          goto      end_SGN
323900090210     C                   END
324000090210     C*
324100090210     C     end_SGN       ENDSR
324200051205      *----------------------------------------------------*
324300051205      *   DEFINIZIONE CHIAVI                               *
324400051205      *----------------------------------------------------*
324500051205     C     *INZSR        BEGSR
324600051205      *
324700991129     c     *ENTRY        PLIST
324800060613     C                   parm                    esito             1
324900971215      *
325000050112     C     KTABel        KLIST
325100050112     C                   KFLD                    TBLKUT
325200050112     C                   KFLD                    TBLCOD
325300050112     C                   KFLD                    TBLKEY
325400090210      *
325500090210     C     K_TAB1L       KLIST
325600090213     C                   KFLD                    etbUNB
325700090213     C                   KFLD                    etbSGM
325800090213     C                   KFLD                    etbVALSGM
325900090209     C*
326000090209     C* Definisco chiavi di accesso
326100090209     C     KTAB1         KLIST
326200090209     C                   KFLD                    KKUT
326300090209     C                   KFLD                    KCOD
326400090209     C*
326500090209     C     KNUF          KLIST
326600090209     C                   KFLD                    KAAA
326700090209     C                   KFLD                    KCNU
326800090209     C                   KFLD                    KFIL
326900090209      *
327000090209     C     KVAB1         KLIST
327100090209     C                   KFLD                    KCCM
327200090209     C                   KFLD                    KCMR
327300090209     C                   KFLD                    KCNT
327400090209     C                   KFLD                    KAAS
327500090209     C                   KFLD                    KLNP
327600090209     C                   KFLD                    KNRS
327700090209     C                   KFLD                    KNSP
327800090209      *
327900090209     C     KVAB2         KLIST
328000090209     C                   KFLD                    KCCM
328100090209     C                   KFLD                    KCMR
328200090209      *
328300090209     C     KRDE          KLIST
328400090209     C                   KFLD                    KAAS
328500090209     C                   KFLD                    KLNP1
328600090209     C                   KFLD                    KNRS
328700090209     C                   KFLD                    KNSP
328800090209     C                   KFLD                    KNMC
328900090209     C                   KFLD                    KNRP
329000090209      *
329100090209     C     KACO          KLIST
329200090209     C                   KFLD                    KKUT
329300090209     C                   KFLD                    KKCC
329400090209     C                   KFLD                    KKSC
329500090209      *
329600090209     C     KIND          KLIST
329700090209     C                   KFLD                    KKUT
329800090209     C                   KFLD                    KKCC
329900090209     C                   KFLD                    KKSC
330000111027      *
330100111027     C     KTAS          KLIST
330200111027     C                   KFLD                    TASAAS
330300111027     C                   KFLD                    TASLNP
330400111027     C                   KFLD                    TASNRS
330500111027     C                   KFLD                    TASNSP
330600090209      *
330700090209      * DETERMINO SE SONO BARTOLINI O SDI
330800090209     C                   CLEAR                   TIBS55DS
330900090209     C                   MOVEL     'L'           I50TLA
331000090209     C                   CALL      'TIBS55R'
331100090209     C                   PARM                    TIBS55DS
331200090209      *
331300090209     C* RECUPERO DATI DELL'UTENTE
331400090209     C                   Z-ADD     1             CODUT             1 0
331500090209     C                   CALL      'XPARUT'
331600090209     C                   PARM                    UTEDSE0F
331700090209     C                   MOVEL     RAGUT         RSUT             20
331800090209     C*
331900090209     C* RICERCA CAPOCONTI
332000090209     C                   DO        50            X
332100090209     C                   MOVE      TCU(X)        TCUDS
332200090209     C     F56           IFEQ      'CG'
332300090209     C     F34           ANDEQ     '01'
332400090209     C                   Z-ADD     KCU(X)        KCI               4 0
332500090209     C                   END
332600090209     C                   END
332700090209     C*
332800090211     c                   movel(p)  'EDPAB'       User_Msg         10
332900090211     C*
333000090209     C* IMPOSTO A ZERO VARIABILI DI PGM
333100090209     c                   move      *blank        Snd_Msg_alert     1
333200090209     C                   MOVEL     CA(1)         WCA              78
333300090209     C                   MOVEL     CN(1)         WCN              78
333400090209     C*
333500090209     C* RECUPERO DATA E ORA
333600100630     C                   TIME                    Ora_Data         14 0
333700100630     C                   MOVE      Ora_Data      G02DAT
333800100630     C                   MOVE      Ora_Data      WDATA8            8 0
333900100630     C                   MOVE      WDATA8        Anno2             2 0
334000090209     C                   MOVEL     WDATA8        DATA              6 0
334100100630     C                   MOVE      Anno2         DATA
334200090209     C                   MOVEL     *BLANK        G02ERR
334300090209     C                   CALL      'XSRDA8'
334400090209     C                   PARM                    WLBDA8
334500090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
334600100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
334700090209     C                   TIME                    WORA              6 0
334800090209      * Recupero data e ora
334900090209     C                   TIME                    W0140
335000090209      * UDATE IN GGMMAAAA
335100100630     C                   MOVE      W0140         DataGGMMAAA
335200090209      * UDATE IN AAAAMMGG
335300100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
335400090209     C     *iso          MOVEL     DATA_eur      dateu
335500090209      *
335600090209      * ?              /*--------------------------- */
335700090209     c                   exsr      CARICA_TABELLE
335800090209      * ?              /*--------------------------- */
335900090209      *
336000090209     C                   MOVEL     *ALL'-'       CMP132          132
336100991124      *
336200050414      *------------------
336300991124     C                   ENDSR
336400060621      * ?------------------------------------------------------------------ */
336500090209      *?    CARICA TABELLE SU SCHIERE
336600060621      * ?------------------------------------------------------------------ */
336700090209     C     CARICA_TABELLEBEGSR
336800090205      *
336900090209     C* Caricamento Tabella 1B
337000090209     C                   Z-ADD     0             X                 4 0
337100090209     C                   Z-ADD     CODUT         KKUT
337200090209     C                   MOVEL     '1B'          KCOD
337300090209     C     KTAB1         CHAIN     TABEL00F                           30
337400090209     C     *IN30         DOWEQ     '0'
337500090209     C     TBLFLG        IFEQ      *BLANKS
337600090209     C                   ADD       1             X
337700100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
337800100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
337900090209     C                   END
338000090209     C     KTAB1         READE     TABEL00F                               30
338100090209     C                   END
338200090209      *
338300090209     C* Caricamento Tabella 15
338400090209     C                   Z-ADD     0             X                 4 0
338500090209     C                   Z-ADD     CODUT         KKUT
338600090209     C                   MOVEL     '15'          KCOD
338700090209     C     KTAB1         CHAIN     TABEL00F                           30
338800090209     C     *IN30         DOWEQ     '0'
338900090209     C     TBLFLG        IFEQ      *BLANKS
339000090209     C                   ADD       1             X
339100090209     C                   MOVEL     TBLKEY        C15(X)
339200090209     C                   MOVEL     TBLUNI        DS15
339300090209     C                   MOVEL     §15COD        ISO(X)
339400090209     C                   MOVEL     §15ECF        CPF(X)
339500090209     C                   MOVE      §15PCF        CPF(X)
339600090209     C                   END
339700090209     C     KTAB1         READE     TABEL00F                               30
339800090209     C                   END
339900090209      *
340000090209     C* Caricamento Tabella CV
340100090209     C                   Z-ADD     0             X
340200090209     C                   Z-ADD     CODUT         KKUT
340300090209     C                   MOVEL     'CV'          KCOD
340400090209     C     KTAB1         CHAIN     TABEL00F                           30
340500090209     C     *IN30         DOWEQ     '0'
340600090209     C     TBLFLG        IFEQ      *BLANKS
340700090209     C                   ADD       1             X
340800090209     C                   MOVEL     TBLKEY        CCV(X)
340900090209     C                   MOVEL     TBLUNI        DCV(X)
341000090209     C                   END
341100090209     C     KTAB1         READE     TABEL00F                               30
341200090209     C                   END
341300090216      *
341400100702     C* Caricamento Tabella Priorità trasporto
341500100702     C                   Z-ADD     0             X
341600100702     C                   MOVEL     'TS'          TABCOD
341700100702     C     TABCOD        CHAIN     EDTAB01L                           30
341800100702     C     *IN30         DOWEQ     '0'
341900100702     C     TABFLG        IFEQ      *BLANKS
342000100702     C                   ADD       1             X
342100100702     C                   eval      TipoServizio(X) = TABUNI
342200100705     C                   eval      Key_TipServ(X)  = TABKEY
342300100702     C                   END
342400100702     C     TABCOD        READE     EDTAB01L                               30
342500100702     C                   END
342600100702      *
342700110328      * Caricamento Tabella termini consegna
342800110328     C                   Z-ADD     0             X
342900110328     C                   MOVEL     'TB'          TABCOD
343000110328     C     TABCOD        CHAIN     EDTAB01L                           30
343100110328     C     *IN30         DOWEQ     '0'
343200110328     C     TABFLG        IFEQ      *BLANKS
343300110328     C                   ADD       1             X
343400110328     C                   eval      K35_TpBolla(X) = TABKEY
343500110328     C                   eval      Cod_TpBolla(X) = TABKEY
343600110328     C                   eval      Decod_Porto(X) = TABUNI
343700110328     C                   END
343800110328     C     TABCOD        READE     EDTAB01L                               30
343900110328     C                   END
344000110328      *
344100090216     C* Caricamento Tabella Tipo Incasso C/Assegno
344200090216     C                   Z-ADD     0             X
344300090216     C                   MOVEL     'TC'          TABCOD
344400090216     C     TABCOD        CHAIN     EDTAB01L                           30
344500090216     C     *IN30         DOWEQ     '0'
344600090216     C     TABFLG        IFEQ      *BLANKS
344700090216     C                   ADD       1             X
344800100705     C                   eval      K35_TpIncas(X) = TABKEY
344900100705     C                   eval      TipoIncasso(X) = TABUNI
345000090216     C                   END
345100090216     C     TABCOD        READE     EDTAB01L                               30
345200090216     C                   END
345300090209      *
345400090209      * Caricamento Tabella Partner esteri
345500090209     C                   Z-ADD     0             X
345600090209     C                   MOVEL     'PT'          TABCOD
345700090209     C     TABCOD        CHAIN     EDTAB01L                           30
345800090209     C     *IN30         DOWEQ     '0'
345900090209      *
346000090209     C                   SELECT
346100090209     C     TABFLG        WHENNE    *BLANKS
346200090209      *
346300090209     C                   OTHER
346400090209     C                   ADD       1             X
346500100630     C                   MOVEL     TABKEY        PT_key(X)
346600100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
346700100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
346800100630     C                   MOVEL     TABUNI        PT_uni(X)
346900090209     C                   ENDSL
347000090209      *
347100090209     C     TABCOD        READE     EDTAB01L                               30
347200090209     C                   END
347300121105      *
347400121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
347500121105     c                   exsr      ChecK_PT
347600121105      *
347700090209      * salva quanti Codici UNB ha caricato
347800100630     c                   z-add     x             PT_KeyXsav
347900090209      *
348000090209     C                   MOVEL     'PU'          TABCOD
348100090209     C     TABCOD        CHAIN     EDTAB01L                           30
348200090209     C     *IN30         DOWEQ     '0'
348300090209      *
348400090209     C                   SELECT
348500090209     C     TABFLG        WHENNE    *BLANKS
348600090209      *
348700090209     C                   OTHER
348800100630     C                   MOVEL     TABKEY        PU_key
348900090209     C                   Z-ADD     1             X
349000100630     C     PU_key        LOOKUP    PT_key(X)                              32
349100100630     C   32              MOVEL     TABUNI        PU_uni(X)
349200090209     C                   ENDSL
349300090209      *
349400090209     C     TABCOD        READE     EDTAB01L                               30
349500090209     C                   END
349600090209      *
349700090209      * Prosegue tab.PT e PU
349800090209     C                   MOVEL     'PV'          TABCOD
349900090209     C     TABCOD        CHAIN     EDTAB01L                           30
350000090209     C     *IN30         DOWEQ     '0'
350100090209      *
350200090209     C                   SELECT
350300090209     C     TABFLG        WHENNE    *BLANKS
350400090209      *
350500090209     C                   OTHER
350600100630     C                   MOVEL     TABKEY        PV_key
350700090209     C                   Z-ADD     1             X
350800100630     C     PV_key        LOOKUP    PT_key(X)                              32
350900100630     C   32              MOVEL     TABUNI        PV_uni(X)
351000090209     C                   ENDSL
351100090209      *
351200090209     C     TABCOD        READE     EDTAB01L                               30
351300090209     C                   END
351301160209      *
351302160209      * Prosegue tab.PT e PU e PV e PS
351303160209     C                   MOVEL     'PS'          TABCOD
351304160209     C     TABCOD        CHAIN     EDTAB01L                           30
351305160209     C     *IN30         DOWEQ     '0'
351306160209      *
351307160209     C                   SELECT
351308160209     C     TABFLG        WHENNE    *BLANKS
351309160209      *
351310160209     C                   OTHER
351311160209     C                   MOVEL     TABKEY        PS_key
351312160209     C                   Z-ADD     1             X
351313160209     C     PS_key        LOOKUP    PT_key(X)                              32
351314160209     C   32              MOVEL     TABUNI        PS_uni(X)
351315160209     C                   ENDSL
351316160209      *
351317160209     C     TABCOD        READE     EDTAB01L                               30
351318160209     C                   END
351400120629      *
351500120629      * Prosegue tab.PT e PU e PV
351600120629     C                   MOVEL     'PZ'          TABCOD
351700120629     C     TABCOD        CHAIN     EDTAB01L                           30
351800120629     C     *IN30         DOWEQ     '0'
351900120629      *
352000120629     C                   SELECT
352100120629     C     TABFLG        WHENNE    *BLANKS
352200120629      *
352300120629     C                   OTHER
352400120629     C                   MOVEL     TABKEY        PZ_key
352500120629     C                   Z-ADD     1             X
352600120629     C     PZ_key        LOOKUP    PT_key(X)                              32
352700120629     C   32              MOVEL     TABUNI        PZ_uni(X)
352800120629     C                   ENDSL
352900120629      *
353000120629     C     TABCOD        READE     EDTAB01L                               30
353100120629     C                   END
353200090209      *
353300090209     C* Caricamento Tabella codici clienti - tariffe
353400090209     C                   Z-ADD     0             X
353500090209     C                   MOVEL     'CL'          TABCOD
353600090209     C     TABCOD        CHAIN     EDTAB01L                           30
353700090209     C     *IN30         DOWEQ     '0'
353800090209     C     TABFLG        IFEQ      *BLANKS
353900090209     C                   ADD       1             X
354000100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
354100100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
354200090209     C                   END
354300090209     C     TABCOD        READE     EDTAB01L                               30
354400090209     C                   END
354500090209      *
354600090209     C* Caricamento Serivizi speciali
354700090209     C                   Z-ADD     0             X
354800090209     C                   MOVEL     'SS'          TABCOD
354900090209     C     TABCOD        CHAIN     EDTAB01L                           30
355000090209     C     *IN30         DOWEQ     '0'
355100090209     C     TABFLG        IFEQ      *BLANKS
355200090209     C                   ADD       1             X
355300100702     C                   eval       Key_ServSpec(X) = TABKEY
355400100702     C                   eval       SpecialServ(X)  = TABKEY
355500100702     C                   eval       UNI_ServSpec(X) = TABUNI
355600090209     C                   END
355700090209     C     TABCOD        READE     EDTAB01L                               30
355800090209     C                   END
355900090209      *
356000090209     C                   ENDSR
356100090205      * ?------------------------------------------------------------------ */
356200090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
356300090205      * ?------------------------------------------------------------------ */
356400090205     C     *pssr         BEGSR
356500090205     C
356600060710     C                   eval      esito = '2'
356700130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 93'
356800130506      *
356900130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
357000130506     C                             IFCSUM S93A EDI ricevuto errato in UPLOAD -
357100130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
357200130506     c                             + ' verificare il '+
357300130506     c                             %editc(total_segmenti:'Z')
357400130506     c                             + ' segmento -> ' + dati
357500130506      *
357600100319     c                   if        §PVinv <> 'N'
357700100319     c                   exsr      invio_mail
357800100319     c                   end
357900060621     C
358000130506     c                   seton                                        LR
358100130506     c                   return
358200130506     C
358300130506     C                   ENDSR
358400090213      * ?------------------------------------------------------------------ */
358500090213      *?      Segnala Errore su TIVIN
358600090213      * ?------------------------------------------------------------------ */
358700090213     C     Error_DET     BEGSR
358800090213     C
358900100728     c                   eval      Segmento_in_errore = Conta_segmenti
359000090213     C                   eval      vinFLG = '2'
359100090213     c                   eval      Almeno_Un_Due ='S'
359200090213     c                   eval      esito  = '1'
359300090213     c                   eval      se_errore ='S'
359400090213     C                   eval      Err_in_detta = 'S'
359500090213     C
359600090213     C                   ENDSR
359700100705      * ?------------------------------------------------------------------ */
359800100705      *?   Controlla e decodifica Valori VALUE dei segmenti
359900100705      * ?------------------------------------------------------------------ */
360000100705     C     In_TAB_EDSTBL BEGSR
360100100705      *
360200100705      *   Cerca la decodifica del dato in TABELLA
360300100705     c                   clear                   trovata_EDSTBL    1
360400100705     c                   clear                   quale_campo       6
360500100705     c                   clear                   campo_dati_70A   70
360600100705     C                   movel     UNB_Mittente  etbUNB
360700100705     c     K_TAB1L       chain     edstbl01l
360800100705      *
360900100705      * prova quindi senza l'UNB specifico del cliente
361000100705     c                   if        not %Found(edstbl01l)
361100100705     C                   clear                   etbUNB
361200100705     c     K_TAB1L       chain     edstbl01l
361300100705     c                   end
361400100705      *
361500100705      *  Traduce i testi descrittivi se PRESENTE in tabella
361600100705      *   il campo di destinazione è definito sulla destra della descrizione
361700100705      *     ed è lungo 6 come i nomi definiti nei VAB.
361800100705     c                   if        %Found(edstbl01l)
361900100705     c                   move(p)   etbDESCRI     quale_campo
362000100705     c                   movel(p)  etbDATI       campo_dati_70A
362100100705     c                   move      'S'           trovata_EDSTBL
362200100705     c                   end
362300100705      *
362400100705      *
362500100705     C                   ENDSR
362600100705     c*==================================================================*
362700090209     C*? CNVDAT - DECODIFICA LA DATA
362800090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
362900090209     C*         - WDATA  (DATA ALFANUMERICA)
363000090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
363100090209     C*         - WHMS   (ORA NUMERICA)
363200090209     C*----------------------------------------------------------------
363300100702     C     Converte_Data BEGSR
363400090209     C*
363500090209     C                   MOVEL     ' '           WERRO             1
363600090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
363700090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
363800100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
363900090209     C                   Z-ADD     *ZEROS        WHMS              6 0
364000090209     C*
364100120905     C     Work_Format_3 IFEQ      *blank
364200120905     c                   if        %Len(%trim(Work_Data__35)) = 8
364300120905     C                   eval         Work_Format_3 = Data_senza_ora
364400120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
364500120905     C                   eval         Work_Format_3 = Data_con_ora
364600120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
364700120905     C                   eval         Work_Format_3 = Data_con_i_secondi
364800120905     c                   end
364900120905     C                   END
365000120905     C*
365100120905     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
365200100702     C                   MOVEL     Work_Data__35 WCOMO8            8
365300090209     C                   TESTN                   WCOMO8               99
365400090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
365500090209     C  N99              MOVEL     'S'           WERRO             1
365600090209     C                   END
365700090209     C*
365800100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
365900100702     C                   MOVEL     Work_Data__35 WCOMO12          12
366000100120     C                   TESTN                   WCOMO12              99
366100100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
366200100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
366300100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
366400090209     C  N99              MOVEL     'S'           WERRO             1
366500090209     C                   END
366600100120     C*
366700100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
366800100702     C                   MOVEL     Work_Data__35 WCOMO14          14
366900100120     C                   TESTN                   WCOMO14              99
367000100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
367100100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
367200100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
367300100120     C  N99              MOVEL     'S'           WERRO             1
367400100120     C                   END
367500090209     C*
367600090209     C                   ENDSR
367700090210     C*----------------------------------------------------------------
367800100701     C*? Se_Numerico       - CONTROLLO NUMERICO
367900100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
368000100701     C*         - lunghezza_num   (Lunghezza campo output)
368100100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
368200090210     C*----------------------------------------------------------------
368300100701     C     Se_Numerico   BEGSR
368400090210     C*
368500090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
368600100701     C                   MOVEL     'N'           Campo_Numerico    1
368700100701     C                   MOVEL     *BLANKS       Numero_OK        15
368800110224     C*
368900090210     C                   Z-ADD     15            IN                3 0
369000100701     C                   Z-ADD     lunghezza_num IX                3 0
369100110224     C*
369200090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
369300100701     C                   MOVEA     test_num      NDA
369400090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
369500090210     C                   MOVEA     *ALL'0'       NDN
369600100630      *
369700090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
369800100701     C                   Z-ADD     lunghezza_num I                 3 0
369900110224     C*
370000090210     C     I             DOWGT     0                                            --- 1 -->
370100110224     C*
370200090210     C* ESTRAE IL CARATTERE DA ESAMINARE
370300100701     C     1             SUBST     test_num:I    WTEM01            1
370400110224     C*
370500090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
370600090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
370700090210     C     WTEM01        SCAN      WCA                                    99
370800110224     C*
370900090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
371000090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
371100110224     C*
371200090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
371300090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
371400110224     C*
371500090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
371600090210     C* DA DESTRA VERSO SINISTRA
371700090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
371800090210     C                   MOVEL     WTEMP1        NDN(IN)
371900090210     C                   SUB       1             IN
372000090210     C                   END                                                    <-- 3 ---
372100110224     C*
372200090210     C                   END                                                    <-- 2 ---
372300090210     C                   SUB       1             I
372400110224     C*
372500090210     C                   END                                                    <-- 1 ---
372600090210     C*
372700090210     C                   MOVEA     NDN           WNDN             15
372800110224     C*
372900090210     C     WNDN          IFNE      *ZEROS
373000100701     C                   MOVEA     NDN           Numero_OK        15
373100100701     C                   MOVEL     'S'           Campo_Numerico    1
373200090210     C                   END
373300090210     C*
373400090210     C                   ENDSR
373500090210     C*----------------------------------------------------------------
373600090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
373700090210     C*----------------------------------------------------------------
373800100705     C     Converte_CAP  BEGSR
373900090210     C*
374000090210     C                   MOVEL     *BLANKS       WCAP              9
374100090210     C     nad3251       IFNE      '0'
374200090210     C     '  '          SCAN      nad3251       LENGHT            2 0
374300090210     C                   SUB       1             LENGHT
374400090210     C     LENGHT        IFLE      5
374500090210     C     LENGHT        ANDGT     0
374600090210     C                   Z-ADD     LENGHT        T                 2 0
374700090210     C                   Z-ADD     5             S                 2 0
374800090210     C                   MOVEA     nad3251       CAP9
374900090210     C                   MOVEL     *ZEROS        CAP5
375000090210     C                   DO        LENGHT
375100090210     C                   MOVE      CAP9(T)       CAP5(S)
375200090210     C                   SUB       1             S
375300090210     C                   SUB       1             T
375400090210     C                   END
375500090210     C                   MOVEA     CAP5          WCAP5             5
375600090210     C     WCAP5         IFEQ      '0'
375700090210     C                   MOVEL     *BLANKS       WCAP
375800090210     C                   ELSE
375900090210     C                   MOVEA     CAP5          WCAP
376000090210     C                   END
376100090210     C*
376200090210     C                   ELSE
376300090210     C                   MOVEL     nad3251       WCAP
376400090210     C                   END
376500090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
376600090210     C     WCAP          IFNE      *BLANKS
376700090210     C     WFLCPF        ANDNE     0
376800090210     C                   MOVEL     WFLCPF        WELE              1 0
376900090210     C                   MOVE      WFLCPF        WPOS              1 0
377000090210     C                   MOVEL     *BLANK        WCPF
377100090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
377200090210     C                   ELSE
377300090210     C                   MOVEL     *BLANKS       WCPF
377400090210     C                   END
377500090210     C                   END
377600090210     C*
377700090210     C                   ENDSR
377800090211      * ?================================================================== */
377900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
378000090211      * ?================================================================== */
378100090211     C     Scrive_VAB    BEGSR
378200090211      *
378300090211     c                   clear                   err_bloccante     1
378400090211     c                   move      'S'           Almeno_Uno
378500090211      *
378600090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
378700090211     c                   exsr      UPDVAB
378800090211      *
378900090211     C* >>>>>>>>>>>>
379000090211     C*  Richiamo pgm per scrittura immediata VAB
379100090211     C     §PUWRK        IFEQ      ' '
379200110110      ******
379300090211     C*  Richiamo pgm per esecuzione stampa
379400110110     C**** §PTCMR        IFEQ      'S'
379500110110     c******             MOVEL     BGM_nrCMR     d86CMR
379600110110     c******             if            RFF_NrCMR <> *Blank
379700110110     C******             MOVEL     RFF_NrCMR     d86CMR
379800110110     c******             end
379900110110     C******             ELSE
380000110110     C******             MOVEL     BGM_NrFviaggiod86CMR
380100110110     c******             if          RFF_NrFviaggio <> *Blank
380200110110     C******             MOVEL     RFF_NrFviaggiod86CMR
380300110110     c******             end
380400110110     C******             END
380500110110      *
380600110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
380700110110     c                   MOVEL     vabCMR        d86CMR
380800090211      *
380900090213     C                   MOVEL     ' '           d86RIE
381000090213     C                   MOVEL     §PUDTA        d86DTA
381100090213     C                   Z-ADD     1             d86CNT
381200090213     C                   Z-ADD     §PTKSC        d86CCM
381300090213     C                   MOVEL     *BLANKS       d86STA
381400090213     C                   MOVEL     'E'           d86MOD
381500090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
381600090211      * dove poi imposta la DS dei parametri passati
381700090213     C                   MOVEL     'LR'          d86CHI
381800090213     C                   MOVEL     'N'           d86CTL
381900090211      *
382000090211     c                   move      kpjbu         SvKpjbu
382100090211     c                   clear                   kpjbu
382200090216     C                   movel     *on           Commit_on         1
382300090213     C                   MOVEL     TRTC86ds      KPJBU
382400090213      * ?- Chiamata la TRTC86R2 -*
382500090213     C                   CALL      'TRTC86R2'
382600090211     C                   PARM                    KPJBA
382700090216     C                   PARM                    Commit_on
382800090211     c                   move      Svkpjbu       Kpjbu
382900090211      *
383000090211     C*  Controllo pgm per scrittura immediata VAB
383100100630     C     sk            IFNE      0
383200100630     C                   DO        sk            sx
383300100630     C                   Z-ADD     skCLienti(sx) d86CCM
383400090211      *
383500090211     c                   move      kpjbu         SvKpjbu
383600090211     c                   clear                   kpjbu
383700090216     C                   movel     *on           Commit_on         1
383800090213     C                   MOVEL     TRTC86ds      KPJBU
383900090213      * ?- Chiamata la TRTC86R2 -*
384000090213     C                   CALL      'TRTC86R2'
384100090211     C                   PARM                    KPJBA
384200090216     C                   PARM                    Commit_on
384300090211     c                   move      Svkpjbu       Kpjbu
384400090211     C                   END
384500090211     C                   END
384600090211      *
384700090211     C                   END
384800090211      * >>>>>>>>>>>>
384900090211     C                   if        se_errore  = 'S'
385000090211     c                   move      'S'           err_bloccante
385100090211     c                   goto      Error_VAB
385200090211     c                   end
385300090211     C*
385400090211     C     Error_VAB     Endsr
385500090211     C*----------------------------------------------------------------
385600090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
385700090211     C*----------------------------------------------------------------
385800090211     C     UPDVAB        BEGSR
385900090211      *
386000090211     C*  Se non è stato impostato il codice bolla lo imposto io
386100090211     C     VABCBO        IFEQ      *BLANKS
386200090211     C     VABCAS        IFGT      0
386300090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
386400090211     C                   ELSE
386500090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
386600090211     C                   END
386700131025      * se però è stato scritto e non era considerato il Contrassegno
386800131025      *  per motivi di lettura del record MOA successiva al Tipo Bolla
386900131025      *   qui sistema il codice bolla
387000131025     C                   Else
387100131025      *  Porto Franco +COD
387200131025     c                   if        VABCBO = '1' and  VABcas > *zeros
387300131025     C                   movel     '4'           VABCBO                         + C/Ass
387400131025      *  Porto Assegnato +COD
387500131025     c                   elseIF    VABCBO = '2' and  VABcas > *zeros
387600131025     C                   movel     '6'           VABCBO                         + C/Ass
387700131025     C                   ENDif
387800131025     C                   END
387900100701      * Imposta le NOte
388000100701     C     VABNOT        IFEQ      *BLANKS
388100100701     C                   MOVEL     Note_1        VABNOT
388200100701     C                   MOVEL     Note_2        VABNT2
388300100701     C                   ELSE
388400100701     C                   MOVEL     Note_1        VABNT2
388500100701     C                   END
388600090211      *
388700090211     C*  Attribuisco anno spedizione
388800100630     C     DTM_DtParten  IFGT      0                                            anno sped.
388900100630     C                   MOVEL     DTM_DtParten  VABAAS
389000090211     C                   ELSE
389100100630     C     DTM_DtFViag   IFGT      0
389200100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
389300090211     C                   END
389400090211     C                   END
389500090211      *
389600090211      *  Controllo dettaglio segnacolli
389700090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
389800100630     C                   EXSR      CTRsegn_EEX
389900090211     C                   ELSE
390000090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
390100090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
390200100630     C                   EXSR      CTRsegn_BAR
390300090211     C                   END
390400090211     C                   END
390500090211      *
390600090212      *  Imposto linea partenza e serie
390700100701     C                   eval      VABLNP = PT_vabLNP
390800100701     C                   eval      VABNRS = PT_vabNRS
390801160209      *
390802160209      *  e la Filiale di GESTIONE
390803160209     c                   if        PS_vabFGS  <> *blank
390804160209     C                   MOVE      PS_vabFGS     VABfgs
390805160209     c                   else
390806160209     C                   z-add     VABlnp        VABfgs
390807160209     c                   end
390900090211      *
391000090211      *  Controllo se devo variare il codice trattamento merce
391100100701     C                   eval      VABCTM = PT_vabCTM
391200090211      *
391300090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
391400090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
391500090211     c                   clear                   mantiene_nsp      1            *blk/'S'
391600100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
391700090211      *
391800090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
391900090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
392000090211      *
392100090211      *  Imposto codice cliente
392200090211     c                   Select
392300090211      *
392400100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
392500100319     C                   When        Det_RFF_FF_ksc  <> *zeros
392600100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
392700100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
392800090211      * forza LNP/CTM/NRS
392900100319     c                   if        Det_RFF_FF_lnp <> 0
393000100319     C                   eval      VABLNP = Det_RFF_FF_lnp
393100090211     c                   end
393101160209     c                   if        Det_RFF_FF_fgs <> *blank
393102160209     C                   move      Det_RFF_FF_fgsVABFGS
393103160209     c                   else
393104160209     c                   if        Det_RFF_FF_lnp <> 0
393105160209     C                   eval      VABfgs = Det_RFF_FF_lnp
393106160209     c                   end
393107160209     c                   end
393200100319     c                   if        Det_RFF_FF_nrs <> 0
393300100319     C                   eval      VABNRS = Det_RFF_FF_nrs
393400090211     c                   end
393500100319     c                   if        Det_RFF_FF_ctm <> *blank
393600100319     C                   eval      VABCTM = Det_RFF_FF_ctm
393700090211     c                   end
393800110408     c                   if        Det_RFF_FF_tic <> *blank and VABCAS <>0
393900110408     C                   eval      VABTIC = Det_RFF_FF_tic
394000110408     c                   end
394100110408      *
394200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
394300100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
394400100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
394500090211     c                   end
394600090211      *
394700100319      *  Specificato codice da qualificatore RFF+FF dalla testata
394800100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
394900100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
395000100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
395100090211      * forza LNP/CTM/NRS
395200100319     c                   if        Tes_RFF_FF_LNP  <> 0
395300100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
395400090211     c                   end
395401160209     c                   if        Tes_RFF_FF_fgs <> *blank
395402160209     C                   move      Tes_RFF_FF_fgsVABFGS
395403160209     c                   else
395404160209     c                   if        Tes_RFF_FF_lnp <> 0
395405160209     C                   eval      VABfgs = Tes_RFF_FF_lnp
395406160209     c                   end
395407160209     c                   end
395500100319     c                   if        Tes_RFF_FF_NRS  <> 0
395600100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
395700090211     c                   end
395800100319     c                   if        Tes_RFF_FF_CTM  <> *blank
395900100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
396000090211     c                   end
396100110408     c                   if        Tes_RFF_FF_tic  <> *blank and VABCAS <>0
396200110408     C                   eval      VABTIC = Tes_RFF_FF_tic
396300110408     c                   end
396400090211      *
396500090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
396600100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
396700100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
396800090211     c                   end
396900090211      *
397000090211      *  Imposto dati da tabella - PT-
397100090211     C                   Other
397200090211      *
397300090211     C                   MOVEL     §PTKSC        VABCCM
397400090211     C                   MOVEL     §PTCTR        VABCTR
397500090211      *
397600090211     C                   Endsl
397700090211      *
397800110408      *  attenzione il tipo Contrassegno solo se c'è un valore
397900110408     c                   if        vabCAS = 0
398000110408     c                   clear                   vabTIC
398100110623     c                   else
398200110623      *
398300110623      * Se non è stato passato il Tipo incasso a fronte di particolarità sullo
398400110623      *  specifico cliente/partner
398500110707     c                   if        inviato_tipo_incasso =  'N' and
398600110707     c                             Det_RFF_FF_ksc > *zeros     and
398700110707     c                             Det_RFF_FF_tic = *blank
398800110623      *  e se c'è "??" sul tipo incasso deve Forzare questo tipo di Incasso sulla
398900110623      *  spedizione.
399000110623     c                   z-add     1             Y
399100110623     c                   movel(p)  '??'          Work_Data__35
399200110623     c                   move      §puTC         Work_Data__35
399300130311     c                   movel     Work_Data__35 WData_35_2        2
399400110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
399500110623     c                   if         *in33
399600110623     c                   eval      vabTIC = TipoIncasso(Y)
399700110623     c                   end
399800110623      **
399900110623     c                   end
400000110623      **
400100110623     c                   end
400600090211      *
400700090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
400800090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
400900090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
401000090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
401100090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
401200090211      *
401300090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
401400090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
401500090211     c                   else
401600090211      *
401700090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
401800090211     C*  lo attribuisco io
401900090211      **              **------------------**
402000090211     c                   exsr      repnum
402100090211      **              **------------------**
402200100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
402300090211      **              **------------------**
402400090211     c                   end
402500090211      *
402600090211     C*  Altrimenti attribuisco data del giorno
402700090211     C     VABMGS        IFEQ      0
402800090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
402900090211     C                   MOVE      WOGGI         VABMGS                         = oggi
403000090211     C                   END
403100090211     C*  Eseguo posizionamento su EDiVAB
403200090211     C                   Z-ADD     VABAAS        KAAS
403300090211     C                   Z-ADD     VABlnp        KLNP
403400090211     C                   Z-ADD     VABNRS        KNRS
403500090211     C                   Z-ADD     VABNSP        KNSP
403600090211     C                   MOVEL     SAVBOL        SALVA
403700090211     C     KVAB1         CHAIN     EDiVAB1L                           30
403800090211     C                   MOVEL     SALVA         SAVBOL
403900090211     C*  Imposto dati CMR
404000090211     C                   Z-ADD     1             VABCNT
404100090211     **
404200090211      *** Attenzione se si deve trattare il VAX con il CMR
404300090211      *** il campo VABCNT deve essere sempre impostato a (1)
404400090211     C     §puWRK        ifEQ      'S'
404500090211     C     §puNSP        andEQ     'S'
404600090211     C                   Z-ADD     1             VABCNT
404700090211     c                   end
404800090211     **
404900100630     C                   Z-ADD     Data_prepara  VABDTS
405000090212     C                   movel     '20'          VABDTS
405100100113     ** Forza la Data di Oggi
405200100113     C                   z-add     WOggi         VABDTS
405300100113     **
405400100630     C     Ora__prepara  mult      100           VABHMS
405500090211     C     §PTCMR        IFEQ      'S'
405600100630     C                   Z-ADD     DTM_DtCMR     VABDCM
405700100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
405800100701     c                   if          RFF_NrCMR <> *Blank
405900100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
406000100701     c                   end
406100090211     C                   ELSE
406200100630     C                   Z-ADD     DTM_DtFViag   VABDCM
406300100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
406400100701     c                   if          RFF_NrFviaggio <> *Blank
406500100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
406600100701     c                   end
406700090211     C                   END
406800100120      *
406900100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
407000100120      * accorpare la bolle ad un unico destinatario da più parti
407100100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
407200100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
407300120629     C                   if         NAD_SF_Cliente <> *blank
407400120629     c                   eval      VABCMR = NAD_SF_Cliente
407500100708     c                   else
407600100706     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z') +
407700100706     c                                      '_' +  VABRMO
407800100708     c                   end
407900100706     c                   end
408000100120      *
408100090211     C*  Se non è indicato il nome del mittente = Partner mittente
408200090211     C     VABRMO        IFEQ      *BLANKS
408300090211     C                   MOVEL     ACORAG        VABRMO
408400090211     C                   MOVEL     INDCAP        VABCMO
408500090211     C     INDCAE        IFNE      *BLANKS
408600090211     C                   MOVEL     INDCAE        VABCMO
408700090211     C                   END
408800090211     C     INDSTA        IFNE      *BLANKS
408900090211     C                   MOVEL     INDSTA        VABNMO
409000090211     C                   END
409100090211     C                   END
409200090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
409300090211     C     §1BFG1        IFEQ      'N'
409400090211     C     §1BFG7        ANDEQ     'N'
409500090211     C     §1BFG2        ANDEQ     'N'
409600090211     C***                  Z-ADD0         VABNRS
409700090211     C                   END
409800090211     C*  Imposto segancollo da - a
409900090211     C     §1BFG1        IFEQ      'S'
410000090211     C     §1BFG7        ANDEQ     'N'
410100090211     C     §1BF12        ANDNE     'E'
410200090211      *
410300100701     C     Conta_Segn    IFEQ      *ZEROS
410400090211     C                   CLEAR                   VABNCD
410500090211     C                   CLEAR                   VABNCA
410600090211     C                   ELSE
410700100630     C                   Z-ADD     segn_BAR(1)   VABNCD
410800100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
410900090211     C                   ENDIF
411000090211     C*
411100090211     C                   END
411200101027     C*
411300090211     C*  Se cap mittente originale a blank abblenco nazione
411400090211     C     VABCMO        IFEQ      *BLANKS
411500090211     C                   MOVEL     *BLANKS       VABNMO
411600090211     C                   END
411700100701     C*
411800090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
411900090603     C* Se in testata
412000100701     c                   if        NAD_destinatario_in_testata = 'S' and
412100100701     c                             NAD_destinatario_in_dettaglio <> 'S'
412200090603     c                   eval         VABrsd =  tes_VABrsd
412300090603     c                   eval         VABrd2 =  tes_VABrd2
412400090603     c                   eval         VABind =  tes_VABind
412500090603     c                   eval         VABlod =  tes_VABlod
412600090603     c                   eval         VABnzd =  tes_VABnzd
412700090603     c                   end
412800090603     C* Se in testata
412900100701     c                   if        NAD_mittente_in_testata = 'S' and
413000100701     c                             NAD_mittente_in_dettaglio <> 'S'
413100090603     c                   eval         VABrmo = tes_VABrmo
413200090603     c                   eval         VABnmo = tes_VABnmo
413300090603     c                   eval         VABcmo = tes_VABcmo
413400090603     c                   end
413500100921      *
413600101027      *   Forza il numero passato nel CNI nel riferimento Numerico
413700101027     c                   if        §PVCNIRMN = 'S'
413800101027     c                   z-add     CNI_SeNumericovabRMN
413900101027     c                   end
414000101027      *
414100100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
414200100921     C     §puWRK        ifEQ      'S'
414300100921     C     §puraggDES    andeq     'S'
414400100921     c                   clear                   vabLNA
414500100921     c                   clear                   vabZNC
414600100921     c                   clear                   vabCMR
414700100921      *  il nome identico
414800100921     c                   eval      VABCMR  = §punomeCMR
414900100921      *   e in più la data del giorno
415000100921     C     §puinGIO      ifEQ      'S'
415100100921     c                   eval      VABCMR = %trim(VABCMR)
415200100921     c                                    + %editc(WOggi_CMR:'X')
415300100921     c                   end
415400100921      *
415500100921     c                   end
415600111017      *
415700111017      *? ------------------------------------------------------------------------
415800111017      *?  SE si tratta di un Partner che invia un reso e mette nel RIF.MITT.NUMERICO
415900111017      *?   il nostro codice bolla BRT. esegue un controllo automatico su esistenza
416000111017      *?   della bolla precedentemente inviata come EXPORT....ed ora in RESO.
416100111017      *? ------------------------------------------------------------------------
416200111028      ****
416300111028      **** se nel Riferimento Numerico viene passato il riferimento di una
416400111028      ****  nostra bolla di export precedentemente inviata di cui si sta facendo
416500111028      **** il reso. (Vedi AZKAR)    controlla
416600111028      **
416700111028     c                   if        PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
416800111028      *
416900111028      * controlla che contenga un numero di 14 cifre valide
417000111028      *   quindi la prima a sinistra deve essere (0)
417100111028     c                   movel     vabrmn        uno0              1 0
417200111028     c                   if        uno0 = 0
417300111028      *
417400111028      *   poi la seconda da sinistra deve NON essere (0)
417500111028     c                   movel     vabrmn        due00             2 0
417600111028     c                   move      due00         uno0
417700111028     c                   if        uno0 > 0
417800111028      *
417900111028     c                   exsr      ctrl_seERAexp
418000111028      *
418100111028     c                   if           Sped_RESO = 'S'
418200111028     c                   exsr      Write_RESO
418300111028     c                   end
418400111028      *
418500111028     c                   end
418600111028     c                   end
418700111028     c                   end
418800110623      *
418900120614      *?  PARTICOLARITA'
419000120614      * x Consegna ai PIANI "FLOOR" codice "FLR"
419100120614     c                   if        FTX_ai_piani = 'P'
419200120614     c                   If         VABTC1 = *blank
419300120614     C                   MOVEL     'P'           VABTC1
419400120614     c                   elseIF     VABTC2 = *blank
419500120614     C                   MOVEL     'P'           VABTC2
419600120614     C                   END
419700120614     c                   End
419800100921      *
419900101027      *? ------------------------------------------------------------------------
420000101027      *?  PARTICOLARITA' sui Clienti Partners x scrittura VAB  (C H I O D I)
420100101027      *? ------------------------------------------------------------------------
420200101027     C*
420300101027     c                   exsr      Chiodi_VAB
420400101027     C*
420500090603      *
420600090211      *? ------------------------------------------------------------------------
420700090213     C   30              WRITE     EDiVAB00                             99
420800090213     C  N30              UPDATE    EDiVAB00                             99
420900090211      *? ------------------------------------------------------------------------
421000090213     c   99              eval      errori_in_Wrt = 'S'
421100090211     C*
421200090211     c                   clear                   wri_vabtic        1
421300090211     C*
421400090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
421500100706     C                   EXSR      Scrive_Riferim
421600090211     C*
421700090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
421800090211     C     §1BF12        IFEQ      'E'
421900100706     C                   EXSR      Scrive_VAT
422000090211     C                   ELSE
422100090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
422200090211     C     §1BFG7        IFEQ      'S'
422300090211     C     §1BFG1        OREQ      'S'
422400100706     C                   EXSR      Scrive_VAD
422500090211     C                   END
422600090211     C                   END
422700090211      *
422800090211     C* Reimposto codice trattamento merce cliente
422900100628     C                   MOVEL     sav_DS1B      DS1B
423000090211     C**
423100090211     C* Do errore se previsto CMR ma non c'è
423200090211     C     §PTCMR        IFNE      ' '
423300090211     C                   EXSR      MEMCMR
423400090211     C                   END
423500090211     C* Do errore se previsto AFB ma non c'è
423600090211     C     §PTNFV        IFNE      ' '
423700090211     C                   EXSR      MEMAFB
423800090211     C                   END
423900090211     C*  Se il cliente vuole il ritorno delle date di consegna
424000090211     C*  è c'è una squdratura fra i segnacolli e il numero
424100090211     C*  dei colli che ci ha passato scrivo EDSUM
424200100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
424300090211     C                   EXSR      WRTSUM
424400090211     C                   END
424500090211     C*
424600090211     C                   ENDSR
424700090211     C*----------------------------------------------------------------
424800090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
424900090211     C*----------------------------------------------------------------
425000100630     C     CTRsegn_BAR   BEGSR
425100090211     C*
425200090211     C                   MOVEL     'N'           WNSGER            1
425300090211     C*
425400090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
425500100701     C     VABNCL        IFNE      Conta_Segn
425600090211     C                   MOVEL     'S'           WNSGER
425700100630     C                   eval       Squadratura_Colli = 'S'
425800090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
425900090211     c                              bollettati'
426000090211     C                   GOTO      FINSGN
426100090211     C                   END
426200100706      *
426300090211     C*  Riordino i segnacolli
426400090211     C     §1BFG7        IFEQ      'N'
426500100630     C                   SORTA     segn_BAR
426600090211     C*  Controllo se sono sequenziali
426700090211     C                   MOVEL     'N'           WNOSEQ            1
426800100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
426900100706     C*
427000100701     C     2             DO        Conta_Segn    X
427100100630     C     segn_BAR(X)   IFNE      WEXSGN
427200090211     C                   MOVEL     'S'           WNOSEQ
427300090211     C                   END
427400100630     C     segn_BAR(X)   ADD       1             WEXSGN
427500090211     C                   END
427600100706     C*
427700090211     C                   END
427800090211     C*
427900090211     C     FINSGN        ENDSR
428000090211     C*----------------------------------------------------------------
428100090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
428200090211     C*----------------------------------------------------------------
428300100630     C     CTRsegn_EEX   BEGSR
428400090211     C*
428500090211     C                   MOVEL     'N'           WNSGER            1
428600090211      *
428700090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
428800090211      *
428900100701     C     VABNCL        IFNE      totale_PCI
429000090211     C                   MOVEL     'S'           WNSGER
429100100630     C                   eval       Squadratura_Colli = 'S'
429200090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
429300090211     c                              bollettati'
429400090211     C                   END
429500090211      *
429600090211     C                   ENDSR
429700090211     C*----------------------------------------------------------------
429800090211     C*? ESEGUO SCRITTURA EDiVAD
429900090211     C*----------------------------------------------------------------
430000100706     C     Scrive_VAD    BEGSR
430100090211     C*
430200100701     C* Se non seq. scrivo i record
430300090211     C     §1BFG1        IFEQ      'S'
430400100701     C     Conta_Segn    ANDNE     *ZEROS
430500090211     C*
430600090211     C     §1BFG7        IFEQ      'S'
430700100701     C                   DO        Conta_Segn    X
430800090211     C                   Z-ADD     VABCCM        KCCM
430900100630     C                   Z-ADD     segn_BAR(X)   KNCD
431000090211     C                   Z-ADD     VABCNT        VADCNT
431100090211     C                   MOVEL     VABCMR        VADCMR
431200090211     C                   Z-ADD     VABAAS        VADAAS
431300090211     C                   Z-ADD     VABLNP        VADLNP
431400090211     C                   Z-ADD     VABNRS        VADNRS
431500090211     C                   Z-ADD     VABNSP        VADNSP
431600090211     C                   Z-ADD     1             VADNCL
431700100630     C                   MOVEL     segn_BAR(X)   VADCDP
431800100630     C                   Z-ADD     segn_BAR(X)   VADNCD
431900090211     C                   Z-ADD     *ZEROS        VADNCA
432000090211     C                   Z-ADD     VABCCM        VADCCM
432100090211     C                   movel     VABfgs        VADfgs
432200090213     C                   WRITE     EDiVAD00                             99
432300090213     c   99              eval      errori_in_Wrt = 'S'
432400090211     C                   END
432500090211     C* Se sequenziali scrivo un solo record
432600090211     C                   ELSE
432700090211     C                   Z-ADD     VABAAS        VADAAS
432800090211     C                   Z-ADD     VABLNP        VADLNP
432900090211     C                   Z-ADD     VABNRS        VADNRS
433000090211     C                   Z-ADD     VABNSP        VADNSP
433100090211     C                   Z-ADD     VABNCL        VADNCL
433200100630     C                   Z-ADD     segn_BAR(1)   VADNCD
433300100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
433400090211     C                   Z-ADD     VABCCM        VADCCM
433500090211     C                   movel     VABfgs        VADfgs
433600090213     C                   WRITE     EDiVAD00                             99
433700090213     c   99              eval      errori_in_Wrt = 'S'
433800090211     C                   END
433900090211     C*
434000090211     C                   ELSE
434100100701     C                   DO        Conta_Segn    X
434200090211     C                   Z-ADD     VABCCM        KCCM
434300100630     C                   Z-ADD     segn_BAR(X)   KNCD
434400090211     C                   Z-ADD     VABAAS        VADAAS
434500090211     C                   Z-ADD     VABLNP        VADLNP
434600090211     C                   Z-ADD     VABNRS        VADNRS
434700090211     C                   Z-ADD     VABNSP        VADNSP
434800100630     C                   MOVEL     segn_BAR(X)   VADCDP
434900090211     C                   Z-ADD     1             VADNCL
435000090211     C                   Z-ADD     *ZEROS        VADNCD
435100090211     C                   Z-ADD     *ZEROS        VADNCA
435200090211     C                   Z-ADD     VABCCM        VADCCM
435300090211     C                   movel     VABfgs        VADfgs
435400090213     C                   WRITE     EDiVAD00                             99
435500090213     c   99              eval      errori_in_Wrt = 'S'
435600090211     C                   END
435700090211     C*
435800090211     C                   END
435900090211     C*
436000090211     C                   ENDSR
436100090211     C*----------------------------------------------------------------
436200090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
436300090211     C*----------------------------------------------------------------
436400100706     C     Scrive_RiferimBEGSR
436500090211      *
436600090211      *  riferimento di consegna destinatario
436700100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
436800100701     c                             CTA_destinatario_in_testata   <> *blank
436900090211     C                   Z-ADD     VABCNT        VATCNT
437000090211     C                   MOVEL     VABCMR        VATCMR
437100090211     C                   Z-ADD     VABCCM        VATCCM
437200090211     C                   movel     VABfgs        VATfgs
437300090211     C                   Z-ADD     VABLNP        VATLNP
437400090211     C                   Z-ADD     VABAAS        VATAAS
437500090211     C                   Z-ADD     VABNRS        VATNRS
437600090211     C                   Z-ADD     VABNSP        VATNSP
437700090211     C                   MOVEL     'A'           VATTRC
437800100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
437900100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
438000100701     c                   else
438100100701     C                   eval       VATNOT = CTA_destinatario_in_testata
438200100701     c                   end
438300090213     C                   WRITE     EDiVAT00                             99
438400090213     c   99              eval      errori_in_Wrt = 'S'
438500090211     c                   end
438600090211      *
438700090211      *  riferimento di consegna telefonico
438800100701     c                   if        COM_telefono_in_dettaglio <> *blank or
438900100701     c                             COM_telefono_in_testata   <> *blank
439000090211     C                   Z-ADD     VABCNT        VATCNT
439100090211     C                   MOVEL     VABCMR        VATCMR
439200090211     C                   Z-ADD     VABCCM        VATCCM
439300090211     C                   movel     VABfgs        VATfgs
439400090211     C                   Z-ADD     VABLNP        VATLNP
439500090211     C                   Z-ADD     VABAAS        VATAAS
439600090211     C                   Z-ADD     VABNRS        VATNRS
439700090211     C                   Z-ADD     VABNSP        VATNSP
439800090211     C                   MOVEL     'B'           VATTRC
439900100701     c                   if        COM_telefono_in_dettaglio <> *blank
440000100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
440100100701     c                   else
440200100701     c                   eval      VATNOT = COM_telefono_in_testata
440300100701     c                   end
440400090213     C                   WRITE     EDiVAT00                             99
440500090213     c   99              eval      errori_in_Wrt = 'S'
440600090211     c                   end
440700090211      *
440800090211      * riferimento del Partner che una volta veniva riportato sul BL4
440900100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
441000100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
441100100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
441200100119      *
441300100701     c                   if        RFF_RifSpediz <> *blank
441400090211     C                   Z-ADD     VABCNT        VATCNT
441500090211     C                   MOVEL     VABCMR        VATCMR
441600090211     C                   Z-ADD     VABCCM        VATCCM
441700090211     C                   movel     VABfgs        VATfgs
441800090211     C                   Z-ADD     VABLNP        VATLNP
441900090211     C                   Z-ADD     VABAAS        VATAAS
442000090211     C                   Z-ADD     VABNRS        VATNRS
442100090211     C                   Z-ADD     VABNSP        VATNSP
442200090211     C                   MOVEL     'C'           VATTRC
442300100701     C                   MOVEL     RFF_RifSpediz VATNOT
442400090213     C                   WRITE     EDiVAT00                             99
442500090213     c   99              eval      errori_in_Wrt = 'S'
442600090211     c                   end
442700090211      *
442800110907      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
442900110907      *   sulla consegna della sua merce
443000110907      *
443100110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
443200110907     c                                  <> *blank
443300110907      *
443400110907     C                   Z-ADD     VABCNT        VATCNT
443500110907     C                   MOVEL     VABCMR        VATCMR
443600110907     C                   Z-ADD     VABCCM        VATCCM
443700110907     C                   movel     VABfgs        VATfgs
443800110907     C                   Z-ADD     VABLNP        VATLNP
443900110907     C                   Z-ADD     VABAAS        VATAAS
444000110907     C                   Z-ADD     VABNRS        VATNRS
444100110907     C                   Z-ADD     VABNSP        VATNSP
444200110907     C                   MOVEL     'I'           VATTRC
444300110907     C                   eval       VATNOT =
444400110907     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
444500110907     C                   WRITE     EDiVAT00                             99
444600110907     c   99              eval      errori_in_Wrt = 'S'
444700110907     c                   end
444800110907      *
444900110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
445000110907     c                                 <> *blank
445100110907      *
445200110907     C                   Z-ADD     VABCNT        VATCNT
445300110907     C                   MOVEL     VABCMR        VATCMR
445400110907     C                   Z-ADD     VABCCM        VATCCM
445500110907     C                   movel     VABfgs        VATfgs
445600110907     C                   Z-ADD     VABLNP        VATLNP
445700110907     C                   Z-ADD     VABAAS        VATAAS
445800110907     C                   Z-ADD     VABNRS        VATNRS
445900110907     C                   Z-ADD     VABNSP        VATNSP
446000110907     C                   MOVEL     'J'           VATTRC
446100110907     C                   eval       VATNOT =
446200110907     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
446300110907     C                   WRITE     EDiVAT00                             99
446400110907     c   99              eval      errori_in_Wrt = 'S'
446500110907     c                   end
446600110907      *
446700131025      *
446800131025      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
446900131025      *   sulla consegna della sua merce
447000131025      *
447100131025     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
447200131025     c                                  <> *blank
447300131025     c                             or FTX_MAIL_come_servizio = 'N'
447400131025      *
447500131025     C                   Z-ADD     VABCNT        VATCNT
447600131025     C                   MOVEL     VABCMR        VATCMR
447700131025     C                   Z-ADD     VABCCM        VATCCM
447800131025     C                   movel     VABfgs        VATfgs
447900131025     C                   Z-ADD     VABLNP        VATLNP
448000131025     C                   Z-ADD     VABAAS        VATAAS
448100131025     C                   Z-ADD     VABNRS        VATNRS
448200131025     C                   Z-ADD     VABNSP        VATNSP
448300131025     C                   MOVEL     'S'           VATTRC
448400131025      *
448500131025      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
448600131025      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
448700131025     C                   eval       VATNOT =
448800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
448900131025      *
449000131025      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
449100131025      * seguenti attivano o meno il servizio x SMS o MAIL
449200131025      *
449300131025      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
449400131025      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
449500131025     C                   eval      %subst(VATNOT:17:1) = 'S'
449600131025     c                   if        FTX_SMS_come_servizio = 'N'
449700131025     c                              or
449800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
449900131025     c                              = *blank
450000131025     C                   eval      %subst(VATNOT:17:1) = 'N'
450100131025     c                   end
450200131025      *
450300131025     C                   eval      %subst(VATNOT:18:1) = 'S'
450400131025     c                   if        FTX_MAIL_come_servizio = 'N'
450500131025     c                              or
450600131025     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
450700131025     c                              = *blank
450800131025     C                   eval      %subst(VATNOT:18:1) = 'N'
450900131025     c                   end
451000131025      *
451100131025     C                   WRITE     EDiVAT00                             99
451200131025     c   99              eval      errori_in_Wrt = 'S'
451300131025     c                   end
451400131025      *
451500090211     C                   ENDSR
451600090211     C*----------------------------------------------------------------
451700090211     C*? ESEGUO SCRITTURA EDiVAT
451800090211     C*----------------------------------------------------------------
451900100706     C     Scrive_VAT    BEGSR
452000090211      *
452100100701     C                   DO        totale_PCI    X
452200090211     C                   Z-ADD     VABCNT        VATCNT
452300090211     C                   MOVEL     VABCMR        VATCMR
452400090211     C                   Z-ADD     VABCCM        VATCCM
452500090211     C                   movel     VABfgs        VATfgs
452600090211     C                   Z-ADD     VABLNP        VATLNP
452700090211     C                   Z-ADD     VABAAS        VATAAS
452800090211     C                   Z-ADD     VABNRS        VATNRS
452900090211     C                   Z-ADD     VABNSP        VATNSP
453000090211     C                   MOVEL     'E'           VATTRC
453100090211      *
453200090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
453300090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
453400090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
453500100701     c                   if        PU_lunVAT     > 0
453600090211      *
453700090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
453800100630     C                   eval      VATnot =
453900100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
454000090211      *
454100090211     c                   else
454200090211      *
454300090211      * Se non è impostato nulla prende il segnacollo passato così com'è
454400100630     C                   MOVEL     segn_EEX(X)   VATNOT
454500090211     c                   end
454600090211      *
454700090213     C                   WRITE     EDiVAT00                             99
454800090213     c   99              eval      errori_in_Wrt = 'S'
454900090211     C                   END
455000090211      *
455100090211     C                   ENDSR
455200090211     C*----------------------------------------------------------------
455300090211     C*? ESEGUO SCRITTURA EDSUM
455400090211     C*----------------------------------------------------------------
455500090211     C     WRTSUM        BEGSR
455600090211     C*
455700100701     C* Se non seq. scrivo i   record
455800100701     C                   DO        Conta_Segn    X
455900090211     C                   Z-ADD     VABAAS        SUMAAS
456000090211     C                   Z-ADD     VABLNP        SUMFLS
456100100701     C                   MOVEL     PT_vabCCM     SUMLNP
456200090211     C                   Z-ADD     VABLNA        SUMLNA
456300090211     C                   Z-ADD     VABZNC        SUMZNC
456400090211     C                   Z-ADD     VABNRS        SUMNRS
456500090211     C                   Z-ADD     VABNSP        SUMNSP
456600100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
456700100701     C                   Z-ADD     PT_vabCCM     SUMCCM
456800090211     C                   MOVEL     *BLANKS       SUMSTS
456900090211     C                   MOVEL     *BLANKS       SUMFLG
457000100701     C                   MOVEL     UNH_Rifer_Msg SUMCNI
457100090211     C                   MOVEL     VABCMR        SUMCMR
457200090211     C                   Z-ADD     0             SUMNFE
457300090211     C                   Z-ADD     VABCCM        VADCCM
457400090213     C                   WRITE     EDSUM000                             99
457500090213     c   99              eval      errori_in_Wrt = 'S'
457600090211     C                   END
457700090211     C*
457800090211     C                   ENDSR
457900090211     c*==================================================================*
458000090211      * Manda un Msg in Posta AS Sede a EDP*
458100090211     c*==================================================================*
458200090211     c     SEND_MSG_EDP  begsr
458300090211      *
458400090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
458500090211      *   Lo manda una sola volta per lavoro
458600090211     c                   IF        Snd_Msg_alert  <> 'N' and
458700090211     c                             User_msg <> *blank
458800090211     c                   z-add     1             Jx
458900090211     C                   exsr      CalQEZSNDMG
459000090211     c                   end
459100090211      *
459200090211     c                   endsr
459300090213     c**********************************************************************
459400090213     c* reperimento numero Spedizione
459500090213     c**********************************************************************
459600090213     C     repnum        BEGSR
459700090213      *
459800090213      *    deve controllare sulla tabella "3C" se il numero spedizione
459900090213      *     deve essere mantenuto oppure no
460000090213     C*
460100090213     C                   clear                   Ds3C
460200090213     C                   Z-ADD     CODUT         TBLKUT
460300090213     C                   MOVEL     '3C'          TBLCOD
460400090213     C                   movel(p)  VABccm        TBLKEY
460500090213     C     KTABel        CHAIN     TABEL00F                           30
460600090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
460700090213     C                   MOVEL     TBLUNI        Ds3C
460800090213     C                   END
460900090213      *
461000090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
461100090213     c                   if        §3CFsp <> 'S'
461200090213      *
461300090213     C* NSP => Stacco un numeratore da AZNUM
461400090213     c                   movel     kpjbu         svkpjbu
461500090213     c                   clear                   kpjbu
461600090213     C                   clear                   TRUL33DS
461700090213     C                   eval      I33OPE = *zeros
461800090213     C                   eval      I33CNU = 302
461900090213     C                   eval      I33NUM = 1
462000090213     C                   movel     TRUL33DS      KPJBU
462100090213     C                   call      'TRUL33R'
462200090213     C                   parm                    KPJBA
462300090213     C                   movel     KPJBU         TRUL33DS
462400090213     c                   movel     svkpjbu       kpjbu
462500090213      ****
462600090213     C                   if        O33ERR = *zeros
462700100630     c                   z-add     o33nrf        NUM_Spediz
462800090213     C                   else
462900090213     C                   endif
463000090213      ****
463100090213     c                   ELSE
463200090213      *   se si vuole mantenere il numero spedizione
463300090213      ****
463400090213     C                   MOVEL     WOGGI         WANNO             4 0
463500090213     C                   MOVE      WANNO         KAAA
463600090213     C                   Z-ADD     3             KCNU
463700090213     C                   Z-ADD     VABlnp        KFIL
463800090213     C     KNUF          CHAIN     FLNUF                              9999
463900090213     C     *IN99         IFEQ      '0'
464000090213     C                   ADD       1             NUFNUM
464100100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
464200090213     C                   UPDATE    FLNUF
464300090213     C                   ELSE
464400090213     C                   END
464500090213     C*
464600090213     c                   EndIF
464700090213      **
464800090213     C                   ENDSR
464900090213     C*----------------------------------------------------------------
465000090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
465100090213     C*----------------------------------------------------------------
465200090213     C     MEMCMR        BEGSR
465300090213     C*
465400090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
465500090213     C     §PTCM1        IFEQ      'S'
465600090213     C                   CLEAR                   EDRDE000
465700090213     C                   MOVEL     'TD1154'      KNMC
465800090213     C                   Z-ADD     1             KNRP
465900100701     C                   MOVEL     PT_vabCCM     KLNP1
466000090213     C     KRDE          CHAIN     EDRDE01L                           31
466100090213     C     *IN31         IFEQ      '1'
466200090213     C                   Z-ADD     VABAAS        RDEAAS
466300090213     C                   Z-ADD     VABLNP        RDELNP
466400090213     C                   Z-ADD     VABNRS        RDENRS
466500090213     C                   Z-ADD     VABNSP        RDENSP
466600090213     C                   MOVEL     'TD1154'      RDENMC
466700090213     C                   Z-ADD     1             RDENRP
466800100706     C                   MOVEL     BGM_nrCMR     RDEVAL
466900100701     c                   if          RFF_NrCMR <> *Blank
467000100701     C                   MOVEL     RFF_NrCMR     RDEVAL
467100100701     c                   end
467200090213     C                   WRITE     EDRDE000                             99
467300090213     c   99              eval      errori_in_Wrt = 'S'
467400090213     C                   END
467500090213     C*  Memorizzo qualificatore CMR
467600090213     C                   CLEAR                   EDRDE000
467700090213     C                   MOVEL     'TD1153'      KNMC
467800090213     C                   Z-ADD     1             KNRP
467900100701     C                   MOVEL     PT_vabCCM     KLNP1
468000090213     C     KRDE          CHAIN     EDRDE01L                           31
468100090213     C     *IN31         IFEQ      '1'
468200090213     C                   Z-ADD     VABAAS        RDEAAS
468300090213     C                   Z-ADD     VABLNP        RDELNP
468400090213     C                   Z-ADD     VABNRS        RDENRS
468500090213     C                   Z-ADD     VABNSP        RDENSP
468600090213     C                   Z-ADD     1             RDENRP
468700090213     C                   MOVEL     'TD1153'      RDENMC
468800090213     C                   MOVEL     'CMR'         RDEVAL
468900090213     C                   WRITE     EDRDE000                             99
469000090213     c   99              eval      errori_in_Wrt = 'S'
469100090213     C                   END
469200090213     C*  Memorizzo la data
469300090213     C                   CLEAR                   EDRDE000
469400090213     C                   MOVEL     'TD2380'      KNMC
469500090213     C                   Z-ADD     1             KNRP
469600100701     C                   MOVEL     PT_vabCCM     KLNP1
469700090213     C     KRDE          CHAIN     EDRDE01L                           31
469800090213     C     *IN31         IFEQ      '1'
469900090213     C                   Z-ADD     VABAAS        RDEAAS
470000090213     C                   Z-ADD     VABLNP        RDELNP
470100090213     C                   Z-ADD     VABNRS        RDENRS
470200090213     C                   Z-ADD     VABNSP        RDENSP
470300090213     C                   Z-ADD     1             RDENRP
470400090213     C                   MOVEL     'TD2380'      RDENMC
470500100630     C                   MOVEL     DTM_DtCMR     RDEVAL
470600090213     C                   WRITE     EDRDE000                             99
470700090213     c   99              eval      errori_in_Wrt = 'S'
470800090213     C                   END
470900090213     C*
471000090213     C                   END                                                    §PTCM1 = 'S'
471100090213     C*
471200090213     C                   ENDSR
471300090213     C*----------------------------------------------------------------
471400090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
471500090213     C*----------------------------------------------------------------
471600090213     C     MEMAFB        BEGSR
471700090213     C*
471800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
471900090213     C     §PTNF1        IFEQ      'S'
472000090213     C                   CLEAR                   EDRDE000
472100100701     C                   MOVEL     PT_vabCCM     KLNP1
472200090213     C                   MOVEL     'TD1154'      KNMC
472300090213     C                   Z-ADD     1             KNRP
472400090213     C     KRDE          CHAIN     EDRDE01L                           31
472500090213     C     *IN31         IFEQ      '1'
472600090213     C                   Z-ADD     VABAAS        RDEAAS
472700090213     C                   Z-ADD     VABLNP        RDELNP
472800090213     C                   Z-ADD     VABNRS        RDENRS
472900090213     C                   Z-ADD     VABNSP        RDENSP
473000090213     C                   MOVEL     'TD1154'      RDENMC
473100100701     C                   if        §PTCM1 <> 'S' or
473200100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
473300090213     C                   Z-ADD     1             RDENRP
473400090213     C                   ELSE
473500090213     C                   Z-ADD     2             RDENRP
473600090213     C                   END
473700100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
473800100701     c                   if          RFF_NrFviaggio <> *Blank
473900100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
474000100701     c                   end
474100090213     C                   WRITE     EDRDE000                             99
474200090213     c   99              eval      errori_in_Wrt = 'S'
474300090213     C                   END
474400090213     C*  Memorizzo qualificatore CMR
474500090213     C                   CLEAR                   EDRDE000
474600100701     C                   MOVEL     PT_vabCCM     KLNP1
474700090213     C                   MOVEL     'TD1153'      KNMC
474800090213     C                   Z-ADD     1             KNRP
474900090213     C     KRDE          CHAIN     EDRDE01L                           31
475000090213     C     *IN31         IFEQ      '1'
475100090213     C                   Z-ADD     VABAAS        RDEAAS
475200090213     C                   Z-ADD     VABLNP        RDELNP
475300090213     C                   Z-ADD     VABNRS        RDENRS
475400090213     C                   Z-ADD     VABNSP        RDENSP
475500100701     C                   if        §PTCM1 <> 'S' or
475600100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
475700090213     C                   Z-ADD     1             RDENRP
475800090213     C                   ELSE
475900090213     C                   Z-ADD     2             RDENRP
476000090213     C                   END
476100090213     C                   MOVEL     'TD1153'      RDENMC
476200090213     C                   MOVEL     'AFB'         RDEVAL
476300090213     C                   WRITE     EDRDE000                             99
476400090213     c   99              eval      errori_in_Wrt = 'S'
476500090213     C                   END
476600090213     C*  Memorizzo la data
476700090213     C                   CLEAR                   EDRDE000
476800100701     C                   MOVEL     PT_vabCCM     KLNP1
476900090213     C                   MOVEL     'TD2380'      KNMC
477000090213     C                   Z-ADD     1             KNRP
477100090213     C     KRDE          CHAIN     EDRDE01L                           31
477200090213     C     *IN31         IFEQ      '1'
477300090213     C                   Z-ADD     VABAAS        RDEAAS
477400090213     C                   Z-ADD     VABLNP        RDELNP
477500090213     C                   Z-ADD     VABNRS        RDENRS
477600090213     C                   Z-ADD     VABNSP        RDENSP
477700100701     C                   if        §PTCM1 <> 'S' or
477800100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
477900090213     C                   Z-ADD     1             RDENRP
478000090213     C                   ELSE
478100090213     C                   Z-ADD     2             RDENRP
478200090213     C                   END
478300090213     C                   MOVEL     'TD2380'      RDENMC
478400100630     C                   MOVEL     DTM_DtFViag   RDEVAL
478500090213     C                   WRITE     EDRDE000                             99
478600090213     c   99              eval      errori_in_Wrt = 'S'
478700090213     C                   END
478800090213     C*
478900090213     C                   END                                                    §PTCM1 = 'S'
479000090213     C*
479100090213     C                   ENDSR
479200090213     c*==================================================================*
479300090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
479400090213     c*==================================================================*
479500090213     c     Imp_ADDR_mail begsr
479600090213      *
479700090213     c                   clear                   Indirizzi
479800090213      *
479900090213      *  Lunghezza indirizzi presenti
480000100630     c                   eval      Lunghezza_Indirizzo =
480100100630     c                                       %Len(%TrimR(Mail_Ind))
480200090213     c                   z-add     1             al_char           3 0
480300090213     c                   z-add     1             dal_char          3 0
480400090213     c                   z-add     1             tot_char          3 0
480500090213      *
480600090213     c     successivo    tag
480700090213      *
480800090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
480900090213      *   e aggiunge il dominio Bartolini + il (;) separatore
481000090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
481100090213     c                   eval      tot_char = al_char - dal_char
481200100630     c                   z-add     dal_char      dalCarattere
481300100630     c                   z-add     tot_char      xTOTcaratteri
481400090213      *
481500090213     c                   clear                   Indir_Bartol     40
481600090213     c                   clear                   Indir_Parz       20
481700100630     c                   eval      Indir_Parz =
481800100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
481900100630      *
482000090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
482100090213     c                             %Trim(bart_it)
482200090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
482300090213      *
482400100630     c                   if        al_char = Lunghezza_Indirizzo
482500090213     c                   goto      fine_indmail
482600090213     c                   else
482700090213     c                   eval      dal_char = ( al_char + 1 )
482800090213     c                   goto      successivo
482900090213     c                   end
483000090213      *
483100090213     C     fine_indmail  TAG
483200090213      *
483300090213     C                   ENDSR
483400090213     c*==================================================================*
483500090213      * Manda un Msg x E-mail
483600090213     c*==================================================================*
483700090213     c     Invio_Mail    begsr
483800090213      *
483900090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
484000090213     C* Inizializzo variabili
484100090213     C                   movel     *blanks       wrkEml          253
484200090213     C                   movel     *blanks       wrkMsg         5000
484300090213     C                   movel     *blanks       wrkOgg           44
484400090213      *
484500090213     C* Valorizzo i campi della e-m@ail - indirizzo
484600100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
484700090213     C                   eval      wrkEml = Indirizzi
484800090213     C                   eval      wrkOgg = Oggetto
484900090213     C                   eval      wrkMsg = Messaggio
485000110203      ********
485100110203     C********           call(e)   'TIS701C'
485200110203     C********           parm                    wrkEml
485300110203     C********           parm                    wrkOgg
485400110203     C********           parm                    wrkMsg
485500110203     C*
485600110203      ** *****
485700110203     C                   call(e)   'TRTCT00R2'
485800110203     C                   parm                    wrkEml
485900110203     C                   parm                    wrkOgg
486000110203     C                   parm                    wrkMsg
486100110203     C*
486200090213     C*
486300090213     C                   ENDSR
486400090603      *
486500090211     ***********************************************************************
486600090211     **
486700090211     ** Send Message (QEZSNDMG) API
486800090211     **
486900090211     ***********************************************************************
487000090211     C     CalQEZSNDMG   BEGSR
487100090211      *
487200090211     ** Invio messaggio all'utente.
487300090211     C                   EVAL      SndMg03 = msgDSP
487400090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
487500090211     C                   EVAL      SndMg05(Jx) = User_msg
487600090211     C                   EVAL      SndMg06 = Jx
487700090211     C                   CLEAR                   QUSEC
487800090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
487900090211
488000090211     C                   CALL      'QEZSNDMG'
488100090211     C                   PARM                    SndMg01
488200090211     C                   PARM                    SndMg02
488300090211     C                   PARM                    SndMg03
488400090211     C                   PARM                    SndMg04
488500090211     C                   PARM                    SndMg05
488600090211     C                   PARM                    SndMg06
488700090211     C                   PARM                    SndMg07
488800090211     C                   PARM                    SndMg08
488900090211     C                   PARM                    Qusec
489000090211     C                   PARM                    SndMg10
489100090211     C                   PARM                    SndMg11
489200090211     C                   PARM                    SndMg12
489300090211      *
489400090211     C                   ENDSR
489500100708      * ?------------------------------------------------------------------ */
489600100708      *  Controlla la trasmissione   se completa
489700100708      * ?------------------------------------------------------------------ */
489800100708     c     Check_Trasm   Begsr
489900100708
490000100708     C                   clear                   se_errore
490100100708     C                   clear                   Riga_iNIZIo       1
490200100708     C                   clear                   Riga_fINe         1
490300100708      ** primo  record
490400100708     c     *start        setll     tivin00r
490500100708      *
490600100708     c     rileggi       tag
490700100708     c                   EXSR      LEGGE_TIVIN_R
490800100708      *
490900100708     c                   if        EoFTivin <> 'S'
491000100708      *
491100100708     c                   if        dati = *blank
491200100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
491300100708      * le leggo e le escludo.
491400100708     C                   eval      vinFLG = '1'
491500100708     c                   update    Tivin000
491600100708     c                   goto      rileggi
491700100708     c                   end
491800100708      * ?              /*---------------------- */
491900100708     c                   exsr      Decod_Segmento
492000100708      * ?              /*---------------------- */
492100100708     c                   endif
492200100708      **
492300100708      ** ultimo record
492400100708     c     *hival        setll     tivin00r
492500100708     c     leggiAncora   tag
492600100708     c                   readp     tivin00r
492700100708     c                   if        %Eof(tivin00r)
492800100708     c                   move      'S'           EoFTivin
492900100708     c                   else
493000100708     c                   clear                   dati
493100100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
493200100708      *
493300100708     c                   if        dati = *blank
493400100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
493500100708      * le leggo e le escludo.
493600100708     C                   eval      vinFLG = '1'
493700100708     c                   update    Tivin000
493800100708     c                   goto      leggiAncora
493900100708     c                   end
494000100708      * ?              /*---------------------- */
494100100708     c                   exsr      Decod_Segmento
494200100708      * ?              /*---------------------- */
494300100708     c                   endif
494400100708
494500100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
494600100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
494700100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
494800100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
494900100708      * ?-----> Errore
495000100708     C                   eval      se_errore = 'S'
495100100708     c                   end
495200100708
495300100708     c                   endSR
495400111017      * ?------------------------------------------------------------------ */
495500111017     C*? CHIODI VAB  prima della scrittura del VAB Particolarità su Clienti/Partners
495600111017      * ?------------------------------------------------------------------ */
495700111017     C     Chiodi_VAB    BEGSR
495800111017      *
495900101027      *? ------------------------------------------------------------------------
496000101027      *? ATTENZIONE: (Chiodo E.Arden)
496100101027      *?  x E.Arden o Unilever           501790 e 501791
496200101027      *? Se è un cliente e non un Partner deve togliere la Ragione Sociale
496300101027      *? originale e mettere un punto al posto della RagSoc poichè non
496400101027      *?  deve essere vuoto il campo.
496500101027      *? ------------------------------------------------------------------------
496600101027     C     VABRMO        IFne      *blank
496700110913     C     §PUTPC        andne     'P'
496800110913     C**** §PUTPC        andeq     'C'
496900101027     C     §PTKSC        ifeq      501790
497000101027     C     §PTKSC        oreq      501791
497100101027     C                   MOVEL     *BLANKS       VABRMO
497200101027     C                   MOVEL     '.'           VABRMO
497300101027     c                   end
497400101027     C                   END
497500101027      *
497600101027      *? ------------------------------------------------------------------------
497700101027      *? Chiodo da togliere a gennaio 2005 :
497800101027      *? attenzione solo x ARVATO non deve essere importato l'IMPORTO ASSICURATO
497900101027      *? come da accordi commerciali anche se lo inviano sul tracciato EDI
498000101027      *? ED ANCHE IL VOLUME dal 1/3/2005
498100101027      *? ------------------------------------------------------------------------
498200101027     C     §PTKSC        ifeq      932337
498300101027     C     VABCCM        oreq      52981
498400101027     C     VABCCM        oreq      932353
498500101027     c     VABCCM        oreq      932461
498600101027     C                   clear                   VABIAS                         IMPORTO
498700101027     C                   clear                   VABVAS                         ASSICURATO
498800101027     C                   clear                   VABVLB                         VOLUME BOLLETTATO
498900101027     c                   end
499000101027     C*
499100101027      *? ------------------------------------------------------------------------
499200101027      *? Chiodo x SFS come da richiesta del 25/5/2007:
499300101027      *? mettere sui 2 campi note:
499400101027      *?      not: DDT SUL COLLO
499500101027      *?      nt2: NON CONSEGNARE IN CASO DI AVARIA
499600101027      *? ------------------------------------------------------------------------
499700101027     C     §PTKSC        ifeq      1160950
499800101027     C     VABNOT        andeq     *blank
499900101027     C     VABNT2        andeq     *blank
500000101027     C                   eval      vabNOT = 'DDT SUL COLLO'
500100101027     C                   eval      vabNT2 = 'NON CONSEGNARE IN CASO DI AVARIA'
500200101027     c                   end
500300101027     C*
500400101027      *? ------------------------------------------------------------------------
500500101027      *? Chiodo x SEUR cliente REBELIO  del 24/9/2007:
500600101027      *?  il cliente se ha il COD vuole sempre in contanti mentre
500700101027      *?   su IFCSUM manda sempre il FTX+PAI+++CO' che noi tradurremmo come OM.
500800101027      *? Chiodo Richiesto dalla Villa con Mail il 24/9/2007
500900101027      *? ------------------------------------------------------------------------
501000101027     C     §PTKSC        ifeq      433048
501100101027     C     VABCCM        andeq     433972
501200101027     C     VABTIC        andne     *blank
501300101027     C                   eval      vabTIC = '  '
501400101027     c                   end
501500101027     C*
501600101027     C*  Per non bloccare la bollettazione
501700101027     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
501800101027     C*  o se il peso non è presente... imposto fisso il minimo 0,1
501900130315     c**********         if        vabpkb = 0
502000130315     c**********         z-add     0,1           vabpkb
502100130315     c**********         end
502200101027     C*
502300101027     C*
502400101027      *? ------------------------------------------------------------------------
502500101027      *? Chiodo x AZKAR BISA richiesta  del 01/4/2009: dalla VILLA con mail di Azkar
502600101027      *?  tutti i contrassegni devono essere intestati come "OM"
502700101027      *? Chiodo Richiesto dalla Villa con Mail il 01/4/2009
502800101027      *? ------------------------------------------------------------------------
502900101027     C     §PTKSC        ifeq      22349
503000101027     C     VABCCM        andeq     22349
503100101027     C     VABTIC        ifne      *blank
503200101027     C     VABCAS        orgt      *zeros
503300101027     C                   eval      vabTIC = 'OM'
503400101027     c                   end
503500101027     c                   end
503600110222     C*
503700110627     C*
503800110222      *? ------------------------------------------------------------------------
503900110222      *? Chiodo x Calberson Maison du Monde codice 0040712 le spedizioni devono
504000110222      *?  essere prese TUTTE CON APPUNTAMENTO - richiesta di Giorgio dal Vivo
504100110222      *? La Marca Alfredo e Carrozzo William x conoscenza del 22/2/2011
504200110222      *? ------------------------------------------------------------------------
504300110222     C     VABCCM        ifeq      40712
504400110222     c                   if        vabtc1 = *blank
504500110222     c                   eval      vabtc1 = 'A'
504600110222     c                   elseif    vabtc2 = *blank
504700110222     c                   eval      vabtc2 = 'A'
504800110222     c                   end
504900110222     c                   end
505000101027     C*
505100110509     C*
505200110509      *? ------------------------------------------------------------------------
505300110509      *? Chiodo x GEODIS MAR x i COD devono essere tutti come "OM" solo per 40566
505400110509      *? ------------------------------------------------------------------------
505500110509     C     VABCCM        ifeq      40566
505600110509     c                   if        vabcas > 0
505700110509     c                   eval      vabtic = 'A'
505800110509     c                   end
505900110509     c                   end
506000110509     C*
506100101027     C                   ENDSR
506200111027      * ?------------------------------------------------------------------ */
506300111027     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
506400111027      * ?------------------------------------------------------------------ */
506500111027     C     Write_RESO    BEGSR
506600111027      *
506700111028      *  Imposta nella descrizione del CMR l'intestazione di RESI
506800111028     c                   movel(p)  vabCMR        campo35          35
506900111028     c                   movel(p)  'RESI__'      vabCMR
507000111028     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
507100111028      *
507200111028      *  e sulla 2° parte della ragione sociale viene apposta la dicitura di RESO
507300111028     c                   move(p)   VABrsd        campo44          44
507400111028     c                   movel     '**RESO** '   campo44
507500111028     C                   movel     campo44       VABrsd
507600111028     C                   move      campo44       campo9            9
507700111028      *
507800111028      * Sposta tutta la ragione sociale di (9 carat.) anche sulla 2°ragione sociale
507900111028     c                   move(p)   VABrd2        campo44          44
508000111028     c                   movel     campo9        campo44
508100111028     C                   movel     campo44       VABrd2
508200111028      *
508300111027     c                   endSR
508400100708      * ?------------------------------------------------------------------ */
508500111027     C*? Controllo se era prima una Bolla di Export che sta rientrando
508600111027      * ?------------------------------------------------------------------ */
508700111027     C     ctrl_seERAexp Begsr
508800111027     C*
508900111027     C*   imposta la chiave per controllare la spedizione
509000111027     C*    in sede
509100111027     C                   move      VABRMN        dsspe
509200111027     C                   movel     '20'          dsspe
509300111027     C*
509400111027     C                   move      SPEAAS        TASAAS
509500111027     C                   move      SPELNP        TASLNP
509600111027     C                   move      SPENRS        TASNRS
509700111027     C                   move      SPENSP        TASNSP
509800111027     C     KTAS          chain     titas30c
509900111027     c                   if        %found(titas30c)
510000111027     c                               and tasLNA >299 and tasLNA <=400
510100111027     c                   eval      era_un_export_RESO_dal_PARTNER = 'S'
510200111027     c                   eval                           Sped_RESO = 'S'
510300111027     c                   end
510400111027     C*
510500111027     C                   ENDSR
510600120629      * ?------------------------------------------------------------------ */
510700120629     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
510800111027      * ?------------------------------------------------------------------ */
510900120629     C     CL_ParticolarebegSR
511000120629      *
511100120629     C     Rifer_Cliente IFNE      *BLANKS
511200120629      * ------>>>
511300120629     C                   eval      XX = 1
511400120629     C                   movel     §PTKSC        Key_CL           35
511500120629     C                   movel     Rifer_Cliente WCOM28           28
511600120629     C                   move      WCOM28        Key_CL
511700120629      *
511800120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
511900120629     C     *IN34         IFEQ      '1'
512000120629     C                   movel     Des_Tb_CL(XX) EDIDSCL
512100120629      *
512200120629      * in testata messaggio
512300120629     c                   if        Dati_di_Testata  ='S'
512400120629     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
512500120629     C                   movel     §CLtar        Tes_RFF_FF_tar    3
512600120629     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
512601160209      *-
512602160209     C                   if        §CLftx  <> *blank
512603160209     C                   move      §CLftx        Tes_RFF_FF_fgs    3
512604160209     c                   else
512605160209     C                   move      §CLlnp        Tes_RFF_FF_fgs
512606160209     c                   end
512607160209      *-
512700120629     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
512800120629     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
512900120629     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
513000120629     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
513100120629     C                   movel     §CLtic        Tes_RFF_FF_tic    2
513200120629     c                   else
513300120629      * in dettaglio Messaggio
513400120629     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
513500120629     C                   movel     §CLtar        Det_RFF_FF_tar    3
513600120629     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
513601160209      *-
513602160209     C                   if        §CLftx  <> *blank
513603160209     C                   move      §CLftx        Det_RFF_FF_fgs    3
513604160209     c                   else
513605160209     C                   move      §CLlnp        Det_RFF_FF_fgs
513606160209     c                   end
513607160209      *-
513700120629     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
513800120629     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
513900120629     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
514000120629     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
514100120629     C                   movel     §CLtic        Det_RFF_FF_tic    2
514200120629     c                   end
514300120629      *
514400120629      * imposta la LNP  e la Serie x il dettaglio specifico
514500120629     c
514600120629      *  In base al cod.trattamento merce reperisco tipo tratt.
514700120629      *    per un dettaglio specifico.
514800120629     C                   CLEAR                   DS1B
514900120629     C                   Z-ADD     1             Y                 3 0
515000120629     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
515100120629     C     *IN32         IFEQ      '1'
515200120629     C                   eval       DS1B = Des_Tb_CTM(Y)
515300120629     C                   END
515400120629      *
515500120629      * Se il cliente non era sulla schiera lo aggiunge
515600120629     C     §CLksc        lookup    skCLienti                              39
515700120629     C                   if        *in39 = *off
515800120629     C                   add       1             sk
515900120629     C                   z-add     §CLksc        skCLienti(sk)
516000120629     c                   endif
516100120629      *
516200120629     C                   ENDIF
516300120629      * ------>>>
516400120629     C                   ENDIF
516500120629      *
516600120629      * in Dettaglio  preimposto i campi rilevati dalle tabelle
516700120629      *   per clienti particolari (CL)
516800120629      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
516900120629      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
517000120629      *   successivi per altre decodifiche.
517100120629     c                   if        Dati_di_Testata <>'S'
517200120629      *
517300120629      *   se aveva un RFF+FF in dettaglio
517400120629     C                   if         Det_RFF_FF_ksc > 0
517500120629      *
517600120629     C                   eval      VABccm = Det_RFF_FF_ksc
517700120629     c                   eval      VABlnp = Det_RFF_FF_lnp
517800120629     c                   eval      VABnrs = Det_RFF_FF_nrs
517900120629     C                   eval      VABctm = Det_RFF_FF_ctm
518000120629     C                   movel     Det_RFF_FF_tarVABctr
518001160209     C                   move      Det_RFF_FF_fgsVABfgs
518100120629      *
518200120629      *   altrimenti se aveva un RFF+FF in Testata
518300120629     c                   elseif     Tes_RFF_FF_ksc > 0
518400120629      *
518500120629     C                   eval      VABccm = Tes_RFF_FF_ksc
518600120629     c                   eval      VABlnp = Tes_RFF_FF_lnp
518700120629     c                   eval      VABnrs = Tes_RFF_FF_nrs
518800120629     C                   eval      VABctm = Tes_RFF_FF_ctm
518900120629     C                   movel     Tes_RFF_FF_tarVABctr
518901160209     C                   move      Tes_RFF_FF_fgsVABfgs
519000120629      *
519100120629     c                   end
519200120629      *>>>>>>>>
519300120629     c                   ENDif
519400120629     C*
519500120629     C                   ENDSR
519600121105      *---------------------------------------------------------------*
519700121105      * CTRL caricamento schiere    PT                               -*
519800121105      *---------------------------------------------------------------*
519900121105     C     Check_PT      begsr
520000121105     C*
520100121105     c* Controllo riempimento schiera
520200121105     c                   clear                   trul0sds
520300121105     c                   eval      i0sski='PT_key'
520400121105     c                   eval      i0sele=%elem(PT_key)
520500121105     c                   eval      i0spie=x
520600121105     c                   eval      i0sfile='EDTAB00F'
520700121105     c                   eval      i0ssif=knsif
520800121105     c                   eval      i0spgm='TRTCT93R'
520900121112     c                   move      kpjbu         SvKpjbu
521000121112     c                   clear                   kpjbu
521100121105     c                   movel     trul0sds      kpjbu
521200121105     c                   call      'TRUL0SR'
521300121105     c                   parm                    kpjba
521400121112     c                   eval      kpjbu = SvKpjbu
521500121105     C*
521600121105     C                   ENDSR
521700120629      * ?------------------------------------------------------------------ */
521800090211     O*****************************************************************
521900090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
522000090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
522100090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
522200090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
