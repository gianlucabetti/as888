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
032800121105     D TRUL0SDS      E DS
032900100701      *
033000100701     d PT_KeyXsav      s              3  0 inz(0)
033100100701     D PT_trovata      s              1    inz('N')
033200100701     D PU_key          S             35
033300100701     D PV_key          S             35
033400120629     D PZ_key          S             35
033500100701     D PU_HLD_SpedVAX  S              1
033600100813     D PT_con_piu_Nazioni...
033700100813     d                 s              1
033800120629     D PZ_abilita_NAD_SF_come_RFF_FF...
033900120629     d                 s              1
034000100701      *
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
108000100701     C*
108100100701     C                   move      'N'           PT_trovata
108200100813     C                   clear                   savXX_PT_key
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
177700100701     C*  Se data consegna richiesta imposto già sui campi del VAB
177800101015     C                   If        Quale_campo ='VABDCR'
177900101015     c                              and dtm2380 <> *blank
178000101015      **
178100100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
178200100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
178300100702     C                   EXSR      Converte_Data
178400100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
178500100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
178600100701     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
178700100701     C     WERRO         IFEQ      'S'
178800100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
178900100706     C                             dtm2005  +
179000100702     C                              ') data errata'
179100100701     c                   exsr      Error_DET
179200100701     c                   goto      end_DTM
179300100701     C                   END
179400100702     C                   end
179500100701      **
179600100701     c     end_DTM       endSR
179700100701      * ?================================================================== */
179800100701     C*? Imposta informazioni sul dettaglio del trasporto
179900100701      * ?================================================================== */
180000100701     C     DATI_TDT      BEGSR
180100100701     C*
180200100701      **
180300100701     c     end_TDT       endSR
180400100705      * ?================================================================== */
180500100705     C*? Imposta la Località
180600100705      * ?================================================================== */
180700100705     C     DATI_LOC      BEGSR
180800100705     C*
180900100705      **
181000100705     c     end_LOC       endSR
181100100705      * ?================================================================== */
181200100705     C*? Imposta identificativi delle Merci
181300100705      * ?================================================================== */
181400100705     C     DATI_EQD      BEGSR
181500100705     C*
181600100705      **
181700100705     c     end_EQD       endSR
181800100705      * ?================================================================== */
181900100705     C*? Imposta il numero delle Merci
182000100705      * ?================================================================== */
182100100705     C     DATI_EQN      BEGSR
182200100705     C*
182300100705      **
182400100705     c     end_EQN       endSR
182500100316      * ?================================================================== */
182600100316     C*? Imposta l'inizio del Dettaglio CNI
182700100316      * ?================================================================== */
182800100316     C     DATI_CNI      BEGSR
182900100316     C*
183000100702      ** Inizio Dettaglio riferimento iniziale
183100100702     c                   If        CNI1004 <> *blank
183200100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
183300101027      *
183400101027      *   Nel Riferimento Alfa sempre
183500110707     c                   eval            VABRMA  = CNI_Identificativo_Bolla
183600101027     C                   EVAL      RFF_RifSpediz = CNI_Identificativo_Bolla
183700101027      *
183800101027      *   e nel numerico se di soli numeri e comunque se lo memorizza
183900101027     C                   eval       test_num = CNI1004
184000101027     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
184100101027     C                   SUB       1             lunghezza_num
184200101027     C                   EXSR      Se_Numerico
184300101027     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
184400101027      * ?                                       --------
184500101027     C                   MOVEL     Numero_OK     VABrmn
184600101027      * ?                                       --------
184700101027     C                   z-add     VABrmn        CNI_seNumerico
184800101027      * ?                                       --------
184900101027     C                   END
185000101027      *
185100101027      *
185200100702     c                   end
185300100319      *
185400100316      **
185500100316     c     end_CNI       endSR
185600100316      * ?================================================================== */
185700100316     C*? Imposta le merci pericolose
185800100316      * ?================================================================== */
185900100316     C     DATI_DGS      BEGSR
186000100316     C*
186100100316      **
186200100316     c     end_DGS       endSR
186300100316      * ?================================================================== */
186400100316     C*? Imposta il numero seriale
186500100316      * ?================================================================== */
186600100316     C     DATI_SEL      BEGSR
186700100316     C*
186800100316      **
186900100316     c     end_SEL       endSR
187000100316      * ?================================================================== */
187100100316     C*? Imposta informazioni aggiuntive
187200100316      * ?================================================================== */
187300100316     C     DATI_PIA      BEGSR
187400100316     C*
187500100316      **
187600100316     c     end_PIA       endSR
187700100316      * ?================================================================== */
187800100316     C*?  Tipo Servizio
187900100316      * ?================================================================== */
188000100316     C     DATI_TSR      BEGSR
188100100702      ***
188200100702     C*  Reperisco priorità spedizione:
188300100702     C                   Z-ADD     1             X
188400100702      ***
188500100702      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
188600100702      ***  mentre in tabella EDI EEX è fatto con "1"
188700100702      ***
188800100702     c                   movel(p)  TSR4219       Work_Data__35
188900100702     c                   move      PU_TipoServ   Work_Data__35
189000100705     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
189100100702     C     *IN32         IFEQ      '1'
189200100702     C                   eval       VABTSP = TipoServizio(X)
189300100705     C                   ELSE
189400100705      **  Il tipo bolla non è presente
189500100705     C                   eval      vinMSG = 'TSR non in tabella'
189600100707     c*********          exsr      Error_DET
189700100707     c*********          goto      end_TSR
189800100705     C                   END
189900100702     C*
190000100702     C*  Decodifico servizi speciali
190100100702     C     TSR7085       IFNE      *BLANKS
190200100702     C     TSR7085       ANDNE     *LOVAL
190300100705      *
190400100702     C                   Z-ADD     1             X
190500100702     c                   movel(p)  TSR7085       Work_Data__35
190600100702     c                   move      PU_SpecServ   Work_Data__35
190700100702     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
190800100705     C     *IN32         IFEQ      '1'
190900100702     C                   eval       EDIDSSS = UNI_ServSpec(X)
191000100705      *
191100100702     C     §SSNOT        IFEQ      'S'
191200100702     C                   MOVEL     §SSDES        VABNOT
191300100702     C                   END
191400100702      *
191500120214      * x Espresso     "Pre-noon"    codice "PRN"
191600100702     C     §SSTPS        IFEQ      'S'
191700100702     C                   MOVEL     'E'           VABTSP
191800100702     C                   END
191900110509      *
192000120214      * x H 10:30      "H 10:30"     codice "H10"
192100110509     C     §SSTPS        IFEQ      'H'
192200110509     C                   MOVEL     'H'           VABTSP
192300110509     C                   END
192400100705      *
192500120214      * x Appuntamento "Booking ins" codice "BKI"
192600120403     c                   IF        §SSTPS ='A' and VABTC1 = *blank
192700100702     C                   MOVEL     'A'           VABTC1
192800120403     c                   elseIF    §SSTPS ='A' and VABTC2 = *blank
192900120403     C                   MOVEL     'A'           VABTC2
193000100702     C                   END
193100120403      *
193200120214      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
193300120214     C     §SSTPS        IFEQ      'F'
193400120214     C                   MOVEL     'S'           VABFFD
193500120214     C                   END
193600100705      *
193700100705     C                   ELSE
193800100705      *
193900100705      **  Il tipo bolla non è presente
194000100705     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
194100100707     c*******            exsr      Error_DET
194200100707     c*******            goto      end_TSR
194300100705      *
194400100702     C                   END
194500100705      *
194600100702     C                   END
194700100316      **
194800100316     c     end_TSR       endSR
194900100705      * ?================================================================== */
195000100705     C*?  C.O.D. Monetary Amount
195100100705      * ?================================================================== */
195200100705     C     DATI_MOA      BEGSR
195300100705      *
195400100705      *   Cerca la decodifica del dato in TABELLA
195500120424     C                   movel(p)  Tipo_Seg      etbSGM
195600100705     C                   movel(p)  moa5025       etbVALSGM
195700100705     c                   exsr      In_TAB_EDSTBL
195800100705      *
195900100705     C* Controllo se campo importo numerico con 3 decimali
196000100705     C     moa5004       IFNE      *zeros
196100100705      *
196200100705     C                   If        Quale_campo ='VABIAS'
196300130529      *
196400130529      * il campo è diviso in 2 flag: il primo serve per l'importo da assicurare
196500130529      *    quindi se deve essere considerato è espresso sulla PT.
196600130529     c                   if        %subst(§PTxxx:1:1) = 'S'
196700100707     C     moa5004       div       100           VABIAS                         IMPORTO
196800100705     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
196900130529     c                   end
197000100705      *
197100100705     C                   elseIF    Quale_campo ='VABVMD'
197200100707     C     moa5004       div       100           VABVMD                         MERCE
197300100705     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
197400100705      *
197500100705     C                   elseIF    Quale_campo ='VABCAS'
197600100705     C     §PUCAS        IFEQ      'S'                                          C/Assegno
197700100705      *
197800100707     C     moa5004       div       100           VABCAS
197900100705     C                   MOVEL     moa6345       VABVCA
198000100705     c                   if        Tes_valuta <> *blank and VABVCA = *blank
198100100705     C                   MOVEL     Tes_valuta    VABVCA
198200100705     c                   end
198300110627      **
198400110627      **  se c'è il default ("??")
198500110627     C                   Z-ADD     1             Y
198600110627     c                   movel(p)  '??'          Work_Data__35
198700110627     c                   move      §puTC         Work_Data__35
198800110627     c                   clear                   WData_35_2        2
198900110627     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
199000110627     c                   if        not *in33
199100110627      *
199200100705      *
199300130311      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO ( ma deve leggere il segmento
199400130311      * contenente il Tipo Incasso - sempre successivo al segmento MOA )
199500100705      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
199600130311      *
199700130311      * ATTENZIONE viene impostato di Default ma, in presenza del FTX+PAI,
199800130311      *  viene reimpostato in base a quello sulla tabella "TC".
199900130311      *   Quindi viene modificato da "OM" ....a quello sul FTX+PAI successivo.
200000100705     C     §PUTPC        IFEQ      'P'
200100100705     C                   MOVEL     'OM'          VABTIC
200200100705     C                   MOVEL     'OM'          VABtic_work
200300100705     C                   ENDIF
200400110627      **
200500110627     c                   end
200600100705      *
200700100705     C                   END
200800100705      *
200900100705     C                   EndIF
201000100705     C                   END                                                    Imp.non numer.
201100100705      *
201200100705     c     end_MOA       endSR
201300100705      * ?================================================================== */
201400100705     C*?  FREE TEXT
201500100705      * ?================================================================== */
201600100705     C     DATI_FTX      BEGSR
201700100705      *
201800100705      *   Cerca la decodifica del dato in TABELLA
201900120424     C                   movel(p)  Tipo_Seg      etbSGM
202000100705     C                   movel(p)  ftx4451       etbVALSGM
202100100705     c                   exsr      In_TAB_EDSTBL
202200100705      *
202300100705      * ? solo se si tratta di note descrittive da comunicare in BOLLA
202400100705     c                   if         quale_campo = 'VABNOT'
202500100705     C                   DO        4             X
202600100705      *
202700100705     C     D05(X)        IFNE      *BLANKS
202800100705      *
202900100705     C     Note_1        IFEQ      *BLANKS
203000100705     C                   MOVEL     D05(X)        Note_1
203100100705     C                   MOVE      D05(X)        Note_2
203200100705     C                   ELSE
203300100705     C     Note_2        IFEQ      *BLANKS
203400100705     C                   MOVEL     D05(X)        Note_2
203500100705     C                   END
203600100705     C                   END
203700100705      *
203800100705     C                   ENDif
203900100705      *
204000100705     C                   ENDdo
204100100705     C                   end
204200100705     C*------>>
204300110907      * ? solo se si tratta di note x invio mail di avviso al destinatario
204400110907      *    (nuovo servizio a pagamento attivato nel 2011)
204500110907      *     dovranno essere scritti 2 records nel VAT di tipo I e J
204600110907     c                   if         quale_campo = 'VATNOT'
204700131025     c                                 or
204800131025     c                              quale_campo = 'VATEML'
204900131025      *
205000131025     c                   if        ftx4453 = 'N'
205100131025     c                   eval      FTX_MAIL_come_servizio = 'N'
205200131025     c                   end
205300110907      *
205400110907     C                   DO        4             X
205500110907      *
205600110907     C     D05(X)        IFNE      *BLANKS
205700110907      *
205800110907     C     Note_1t       IFEQ      *BLANKS
205900110907     C                   MOVEL     D05(X)        Note_1t
206000110907     C                   MOVE      D05(X)        Note_2t
206100110907     C                   ELSE
206200110907     C     Note_2t       IFEQ      *BLANKS
206300110907     C                   MOVEL     D05(X)        Note_2t
206400110907     C                   END
206500110907     C                   END
206600110907      *
206700110907     C                   ENDif
206800110907      *
206900110907     C                   ENDdo
207000110907      *
207100110907      * carica il campo da riportare poi sul VAT
207200110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
207300110907     c                             note_1T
207400110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
207500110907     c                             note_2T
207600110907     C                   end
207700131025     C*------>>
207800131025      * ? solo se si tratta di note x invio SMS di avviso al destinatario
207900131025      *    (nuovo servizio a pagamento attivato)
208000131025      *     dovrà essere scritto 1 record nel VAT di tipo S
208100131025     c                   if         quale_campo = 'VATSMS'
208200131025     c                   if        ftx4453 = 'N'
208300131025     c                   eval      FTX_SMS_come_servizio = 'N'
208400131025     c                   end
208500131025      *
208600131025     C                   DO        4             X
208700131025      *
208800131025     C     D05(X)        IFNE      *BLANKS
208900131025      *
209000131025     C     Note_1sms     IFEQ      *BLANKS
209100131025     C                   MOVEL     D05(X)        Note_1sms
209200131025     C                   MOVE      D05(X)        Note_2sms
209300131025     C                   ELSE
209400131025     C     Note_2sms     IFEQ      *BLANKS
209500131025     C                   MOVEL     D05(X)        Note_2sms
209600131025     C                   END
209700131025     C                   END
209800131025      *
209900131025     C                   ENDif
210000131025      *
210100131025     C                   ENDdo
210200131025      *
210300131025      * carica il campo da riportare poi sul VAT
210400131025     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
210500131025     c                             note_1sms
210600131025     C                   end
210700110907     C*------>>
210800100705     C*  Decodifico tipo pagamento C/Assegno
210900100705      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
211000100705      *      (Problema di pulizia in presenza di + righe di testo)
211100100705      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
211200100705      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
211300100705      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
211400100705      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
211500100705     C                   if        wri_vabtic = *blank
211600100705     C                   MOVEL     *BLANKS       VABTIC
211700100705     c                   end
211800100705      **
211900100705     c                   clear                   trovato_TPInc     1            TipoIncasso
212000100705      *
212100100705      * ?Se c'è il tipo Incasso
212200100705     c                   if         quale_campo = 'VABTIC'                      -- 01 --
212300100705      *
212400100705     C                   DO        4             X                              -- 02 --
212500100705     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
212600110623      **
212700130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
212800130311      **  per gestire Partner come AZKAR che ha codice "AAA"
212900130311     C**********         MOVEL     D05(X)        WCTC              2
213000130311     C                   MOVEL     D05(X)        WCTC             10
213100110623      **
213200110623     c                   eval      inviato_tipo_incasso ='S'
213300110623      **
213400110623      **  se non l'avesse trovato e fosse esplicitato un Default ("??")
213500110623      **   come per qualsisi codice venga inviato dal Cliente allora
213600110623      **   deve essere impostato il default (invece di fare un chiodo)
213700110623     C                   Z-ADD     1             Y
213800110623     c                   movel(p)  '??'          Work_Data__35
213900110623     c                   move      §puTC         Work_Data__35
214000110623     c                   clear                   WData_35_2        2
214100110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
214200110623     c                   if            *in33
214300110623     c                   move      'S'           trovato_TPInc
214400110623     C                   MOVEL     TipoIncasso(Y)vabtic
214500110623     c                   movel(p)  '??'          WData_35_2
214600110623     c                   else
214700110623      *
214800130311      *  Se NON HA un default generico definito con "??"
214900130311      *   cerco con il codice arrivato dal Partner/Cliente
215000100705     C                   Z-ADD     1             Y
215100100705     c                   movel(p)  Wctc          Work_Data__35
215200100705     c                   move      §puTC         Work_Data__35
215300100705     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
215400110623     c                   if            *in33
215500110623     c                   move      'S'           trovato_TPInc
215600110623     C                   MOVEL     TipoIncasso(Y)vabtic
215700130326     c                   move      §puTC         WData_35_2        2
215800110623     c                   else
215900110623      *
216000130311      *  Se NON HA TROVATO qualcosa di DEFINITO SU MISURA del Partner/Cliente
216100130311      *   tramite il SUFFISSO presente sulla PT, prova con IL CODICE semplicemente.
216200110623     C                   Z-ADD     1             Y
216300110623     c                   movel(p)  Wctc          Work_Data__35
216400110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
216500110623     c                   if            *in33
216600110623     c                   move      'S'           trovato_TPInc
216700110623     C                   MOVEL     TipoIncasso(Y)vabtic
216800130311     c                   movel     Wctc          WData_35_2        2
216900110623     c                   end
217000110623      *
217100110623     c                   end
217200110623      *
217300110623     c                   end
217400130311      *--------
217500130311      * SOLO SE PARTNER
217600130311      *   IMPOSTO 'OM'    (vecchia regola ormai in disuso se NON siano state
217700130311      *     trovate fra le tabelle dei codici ben precisi x il PARTNER,
217800130311      ***     ossia solo se NON E'STATO PASSATO un codice specifico
217900130311      ** ATTENZIONE NON DEVE ESISTERE IL BLANK in arrivo dal PARTNER ma sempre un CODICE.
218000100705     C     §PUTPC        IFEQ      'P'
218100100705     c     wctc          andne     'TO'
218200110408     c     WData_35_2    andeq     *blank
218300100705     C                   MOVEL     'OM'          VABTIC
218400100705     C                   END                                                    -- 03 --
218500130311      *--------
218600100705     C                   END                                                    -- 03 --
218700100705     C                   ENDdo                                                  -- 02 --
218800100705      *
218900100705     C                   ELSE                                                   -- 01 --
219000130311      *
219100130311      *  PASSA DA QUI se il FTX ha un qualificatore NON RICONOSCIUTO
219200130311      *   e se  aveva impostato blank (nullo) precedentemente....
219300130311      *****
219400100705      * ?Se NON c'è il tipo Incasso Contrassegno particolare
219500100705      *    Deve essere impostato comunque da testata
219600110623     c                   if        vabtic = *blank   and
219700110623     c                             WData_35_2 = *blank
219800100705     C                   MOVEL     VABtic_work   VABTIC
219900100705     c                   end
220000100705      *
220100100705     C                   END                                                    -- 01 --
220200100705      *
220300100705      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
220400100705      *    essere presenti altri record Flat con altre informazioni
220500100705      *    che potrebbero abblancare il Tipo incasso precedentemente
220600100705      *    decodificato
220700100705     c                   if        Trovato_TPInc = 'S' and
220800100705     c                                 wri_vabtic = *blank
220900100705     c                   eval      wri_vabtic = 'S'
221000100705     c                   end
221100100705      *
221200100705      * ?Se Natura Merce      .
221300100705     c                   if         quale_campo = 'VABNAS'                      -- 01 --
221400100705     C                   MOVEL     D05(1)        VABnas
221500100705     c                   End
221600100705      *
221700120613      * ?Consegne Particolari
221800120613     c                   if         quale_campo = 'VABTCX'                      -- 01 --
221900120613      * Imposta il codice di 3 chrs.
222000120613      *   solo i primi 3 caratteri del campo
222100120613     C                   eval      controlla_COD_FTX = D05(1)
222200120613      *
222300120613      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
222400120613     C                   if        controlla_COD_FTX = 'FLR'
222500120613     c                   eval      FTX_ai_piani = 'P'
222600120614     c                   End
222700120613     c                   End
222800120613      *
222900100705     c     end_FTX       endSR
223000090209      * ?================================================================== */
223100090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
223200090209      * ?================================================================== */
223300090209     C     DATI_TOD      BEGSR
223400090209      *
223500090210      *  Trascodifico tipo trasporto
223600090210     c                   clear                   TOD_char3         3
223700100705      *  DEFAULT da PT
223800090210      *  imposta il default dalla tabella PT se c'è
223900090210     C     §puPFA        ifNE      *blank
224000090210     c                   SELECT
224100090210      *  Porto Franco
224200090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
224300090210     C                   movel     '4'           VABCBO                         + C/Ass
224400090210     c                   WHEN      §puPFA = 'F'
224500090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
224600090210      *  Porto Assegnato
224700090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
224800090210     C                   movel     '6'           VABCBO                         + C/Ass
224900090210     C                   WHEN      §puPFA = 'A'
225000090210     C                   movel     '2'           VABCBO                         Senza C/Ass
225100090210     C                   ENDSL
225200090210     c                   end
225300090210      *
225400090210      *   Imposta o il codice o il metodo
225500090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
225600090210     c                   if        tod4053  <> *blank
225700090210     c                   movel(p)  tod4053       TOD_char3
225800090210     c                   else
225900090210     c                   if        tod4215  <> *blank
226000090210     c                   movel(p)  tod4215       TOD_char3
226100090210     c                   end
226200090210     c                   end
226300110328      ****************
226400110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
226500110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
226600110328      ****************
226700110328     c                   eval      Trovata_Tab_TB ='N'
226800110328      *
226900110328     c                   if              TOD_char3 <> *blank
227000110328     c                   movel(p)  TOD_char3     Key_Generica35
227100110328     c                   move      §puTB         Key_Generica35
227200110328     C                   eval      Y = 1
227300110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
227400110328     c   32              eval      Trovata_Tab_TB ='S'
227500110328      *
227600110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
227700110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
227800110328      *
227900110328     c                   if          Trovata_Tab_TB ='N'
228000110328     c                                 and
228100110328     C                               tod4215 <> *blank
228200110328     c                   movel(p)  tod4215       TOD_char3
228300110328     c                   movel(p)  TOD_char3     Key_Generica35
228400110328     c                   move      §puTB         Key_Generica35
228500110328     C                   eval      Y = 1
228600110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
228700110328     c   32              eval      Trovata_Tab_TB ='S'
228800110328     c                   end
228900110328     c                   end
229000110328      *
229100110328      ****************
229200110328      ******* Se vi sono eccezioni sul cliente imposta quelle
229300110328      ****************
229400110328     c                   If          Trovata_Tab_TB ='S'
229500110328      *
229600110328     c                   SELECT
229700110328     c                   WHEN      Decod_Porto(Y) = 'F'
229800110328     c                               and  VABcas > *zeros
229900110328     C                   movel     '4'           VABCBO                         + C/Ass
230000110328     c                   WHEN      Decod_Porto(Y) = 'F'
230100110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
230200110328      *  Porto Assegnato
230300110328     c                   WHEN      VABCAS > *zeros
230400110328     C                   movel     '6'           VABCBO                         + C/Ass
230500110328     C                   OTHER
230600110328     C                   movel     '2'           VABCBO                         Senza C/Ass
230700110328     C                   ENDSL
230800110328      *
230900110328      ****************  altrimenti
231000110328      ******* Se non vi sono eccezioni per il cliente
231100110328      *   Rileva a questo punto lo Standard per tutti
231200110328      ****************
231300110328     c                   elseIf      Trovata_Tab_TB ='N'
231400090210      **
231500090210     c                   if              TOD_char3 <> *blank
231600100705      *   Cerca la decodifica del dato in TABELLA
231700120424     C                   movel(p)  Tipo_Seg      etbSGM
231800100705     C                   movel(p)  TOD_char3     etbVALSGM
231900100705     c                   exsr      In_TAB_EDSTBL
232000090210      *
232100100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
232200100705      *
232300090210     c                   SELECT
232400100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
232500100705     c                               VABcas > *zeros
232600090210     C                   movel     '4'           VABCBO                         + C/Ass
232700100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
232800090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
232900090210      *  Porto Assegnato
233000090210     c                   WHEN      VABCAS > *zeros
233100090210     C                   movel     '6'           VABCBO                         + C/Ass
233200090210     C                   OTHER
233300090210     C                   movel     '2'           VABCBO                         Senza C/Ass
233400090210     C                   ENDSL
233500090210      *
233600090210     c                   end
233700090210     c                   endIF
233800090209      **
233900110328     c                   ENDif
234000110328      **
234100090210      **  Il tipo bolla non è presente
234200090210     c                   if        VABCBO = *blank
234300090210     C                   eval      vinMSG = 'TOD non rilevato'
234400090213     c                   exsr      Error_DET
234500090210     c                   goto      end_TOD
234600090210     c                   end
234700090210      **
234800090210     c     end_TOD       endSR
234900090213      * ?================================================================== */
235000090213     C*?  Anagrafica del mittente e del destinatario
235100090213      * ?================================================================== */
235200091016     C     DATI_NAD      BEGSR
235300100705      *
235400100705      *   Cerca la decodifica del dato in TABELLA
235500120424     C                   movel(p)  Tipo_Seg      etbSGM
235600100705     C                   movel(p)  nad3035       etbVALSGM
235700100705     c                   exsr      In_TAB_EDSTBL
235800100708      *-------
235900120629      * Dato (SHIPPED FROM) usato principalmente x CMR
236000120629      *       ma anche usato al posto di un RFF+FF in testata messaggio
236100120629      *   quindi modificato il nome di riferimento del campo sul EDSTBL
236200120629      *   x renderlo più generico.
236300100708      *-------
236400120629     C********************        IF        Quale_campo ='VABCMR'
236500120629     C                   IF        Quale_campo ='NAD_SF'
236600120629      *-------
236700120629      * Dati di PROVENIENZA MESSAGGIO
236800120629      *-------
236900120629      ***
237000120629      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
237100100708     c                   if        nad30361 <> *blank
237200120629     C                   MOVEL     nad30361      NAD_SF_Cliente
237300100708     c                   elseIf    nad31241 <> *blank
237400120629     C                   MOVEL     nad31241      NAD_SF_Cliente
237500100708     c                   elseIf    nad3039  <> *blank
237600120629     C                   MOVEL     nad3039       NAD_SF_Cliente
237700100708     c                   end
237800120629      ***
237900120629      * ?SOLO SE NON c'è un RFF+FF: di Testata che prevale su tutto.
238000120629      * ?La PLATFORM LIST è usata come il Codice Cliente su RFF+FF:PLATFORMLIST
238100120629      ***
238200120629      * Se partner/cliente è abilitato alla funzione di sostituzione CLIENTE da UNB.
238300120629      *   Il (SHIPPED FROM) in testata funziona come un RFF+FF: a livello di testata.
238400120629      *    per agganciare i dati sulla tab."CL".
238500120629      ***
238600120629     C                   if         PZ_abilita_NAD_SF_come_RFF_FF ='S'
238700120629     c                                 and Tes_RFF_FF_ksc = 0
238800120629      * ?Ma solo se NON c'è un RFF+FF: di Testata che prevale su tutto.
238900120629      *
239000120629      *   imposta la PLATFORM LIST ricevuta del MITTENTE
239100120629      *    questo codice viene poi utilizzato sulla tab.CL (EDTAB) se concordato
239200120629      *    per codificare tutte le spedizioni come un RFF+FF:xxxxxxxxxxxxxxxxx di testata
239300120629     c                   eval      NAD_SF_Cliente = NAD3039
239400120629      *>>>>>>>>
239500120629     C*------------------
239600120629      *    Aggancia il Cliente e tutte le caratteristiche da tab.:"CL"
239700120629      * ?RFF+FF:
239800120629     C                   clear                   Rifer_Cliente    35
239900120629     C                   movel     NAD_SF_ClienteRifer_Cliente
240000120629     c                   exsr      CL_Particolare
240100120629     C*------------------
240200120629      *>>>>>>>>
240300120629     c                   end
240400120629      ***
240500100705      *-------
240600090210      * Dati destinatario
240700100705      *-------
240800100708     C                   elseIF    Quale_campo ='VABRSD'
240900090227      *
241000090227      * Se trovato un indirizzo di destinatario
241100100701     c                   if           Dati_di_Testata = 'N'
241200100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
241300100701     c                   else
241400100701     c                   eval       NAD_destinatario_in_testata = 'S'
241500100701     c                   end
241600090210      *
241700090210     C* Reperisco nazione corrispondente destinatario
241800100705     C                   Z-ADD     1             naz               3 0
241900100705     C                   MOVEL     *BLANKS       VABNZD
242000100705     C                   Z-ADD     0             WFLCPF
242100090210     c                   eval      *in32 = *off
242200100705      ***
242300090210     C                   if        nad3207 <> *BLANKS
242400111028     C     nad3207       LOOKUP    ISO(naz)                               32
242500100705     C                   If        *in32 = *on
242600100705      ***
242700100705      * per reperire il CAP corretto
242800100705     C                   MOVEL     C15(naz)      VABNZD
242900100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
243000100705      ***
243100100705     c                   Endif
243200100705     C                   endif
243300100705      *
243400090210     C* Imposto dati destinatario
243500100705     C                   MOVEL     nad30361      VABRSD
243600100705     C                   if        nad30362 <> *LOVAL
243700100705     C                   MOVEL     nad30362      VABRD2
243800090210     C                   endif
243900100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
244000091019???? C* Imposto dati destinatario
244100091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
244200100705???? C                   MOVEL     nad31241      VABRSD
244300100705???? C                   if        nad31242 <> *LOVAL
244400100705???? C                   MOVEL     nad31242      VABRD2
244500091019???? C                   endif
244600091019???? C                   end
244700100705      *   città/Località
244800090210     C                   MOVEL     nad3164       VABLOD
244900100705      *   Indirizzo
245000100705     C                   MOVEL     nad30421      VABIND
245100111121      *
245200111121     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
245300111121     c                                         %len(%trim(NAD30422))
245400111121???? c                   eval      NAD_Lunghezza_Indirizzo =
245500111121     c                                         %len(%trim(VABIND))
245600111121???? c                   eval      NAD_Lunghezza_ragSociale2 =
245700111121     c                                         %len(%trim(VABRD2))
245800111116      *
245900111121      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
246000111121      *  lo aggiunge nel proseguimento della Ragione Sociale
246100111121     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
246200111121      **
246300111121     c                             and  %len(VABIND) <
246400111121     c                                  NAD_Lunghezza_Indirizzo +
246500111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
246600111121      *
246700111121      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
246800111121      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
246900111121     c                             and  %len(VABRD2) -
247000111121     c                                  NAD_Lunghezza_ragSociale2 -
247100111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
247200111121     c                                  >= 0
247300111121      *
247400111121      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
247500111121     c                             and  NAD_Lunghezza_ragSociale2 <
247600111121     c                                  NAD_Lunghezza_Indirizzo
247700111121      *
247800111121     C     '    '        SCAN      VABRD2        WI
247900111121     c                   if        %Found
248000111121     C                   MOVEA     VABRD2        LOD
248100111121     C                   ADD       1             WI
248200111121     C                   MOVEA     NAD30422      LOD(WI)
248300111121     C                   MOVEA     LOD           VABRD2
248400111121     c                   end
248500111121      *
248600111121      * Altrimenti lo aggiunge direttamente sulla VIA
248700111121     c                   else
248800100705      * Se NAD30422  non è vuoto
248900111116      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
249000111116      *
249100111116     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
249200090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
249300111121      *
249400100705     C* Verifico se nella località c'è dello spazio
249500111116     C     '    '        SCAN      VABIND        WI
249600100705     c                   if        %Found
249700111116     C                   MOVEA     VABIND        LOD
249800090210     C                   ADD       1             WI
249900090210     C                   MOVEA     NAD30422      LOD(WI)
250000111116     C                   MOVEA     LOD           VABIND
250100100705     c                   else
250200111121      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
250300111116     C     '    '        SCAN      VABLOD        WI                4 0
250400100705     c                   if        %Found
250500111116     C                   MOVEA     VABLOD        LOD
250600100705     C                   ADD       1             WI
250700100705     C                   MOVEA     NAD30422      LOD(WI)
250800111116     C                   MOVEA     LOD           VABLOD
250900100705     C                   ENDif
251000090210     C                   ENDif
251100100705      *
251200100705     C                   ENDif
251300111121      *
251400111121     c                   end
251500111121      *
251600111116      **************
251700111116      *** PRIMA provava ad aggiungere prima alla località......
251800111116      **************
251900111116     C*  aggiungo alla LOCALITA' quanto di indirizzo ce n'è in più
252000111116     c*************      if        NAD30422 <> *blanks and NAD30422 <> *loval
252100111116     C* Verifico se nella località c'è dello spazio
252200111116     C*****'    '        SCAN      VABLOD        WI                4 0
252300111116     c*************      if        %Found
252400111116     C*************      MOVEA     VABLOD        LOD
252500111116     C*************      ADD       1             WI
252600111116     C*************      MOVEA     NAD30422      LOD(WI)
252700111116     C*************      MOVEA     LOD           VABLOD
252800111116     c*************      else
252900111116      *  Altrimenti se non c'è spazio dopo la località prova a concatenarlo alla via
253000111116     C*****'    '        SCAN      VABIND        WI
253100111116     c*************      if        %Found
253200111116     C*************      MOVEA     VABIND        LOD
253300111116     C*************      ADD       1             WI
253400111116     C*************      MOVEA     NAD30422      LOD(WI)
253500111116     C*************      MOVEA     LOD           VABIND
253600111116     C*************      ENDif
253700111116     C*************      ENDif
253800111116      *************
253900111116     C*************      ENDif
254000111116      *
254100111116      *
254200111116      *
254300111116      *
254400101027      * RIPORTO LA 2° RAG.SOCIALE NELL'INDIRIZZO.
254500101027     c                   if        vabIND = *blank and VABRD2 <> *BLANK
254600101027      *
254700101027     C                   EVAL      VABIND = VABRD2
254800101027      *
254900101027     C                   ENDif
255000090210      * Converto CAP
255100100705     C                   EXSR      Converte_CAP
255200090603     C*
255300100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
255400100705     C*  record del VAB viene PULITO
255500100316     c                   if        Dati_di_Testata  = 'S'
255600090603     c                   eval       tes_VABrsd = VABrsd
255700090603     c                   eval       tes_VABrd2 = VABrd2
255800090603     c                   eval       tes_VABind = VABind
255900090603     c                   eval       tes_VABlod = VABlod
256000090603     c                   eval       tes_VABnzd = VABnzd
256100090603     c                   end
256200100728      *
256300100728      *   se NON c'è l'identificativo bolla del cliente
256400100728      *    blocca la spedisione e lo segnala.
256500100728     C                   if          vabRSD = *blank
256600100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
256700130315     c******             exsr      Error_DET
256800130315     c******             goto      end_NAD
256900100728     C                   elseIF      vabIND = *blank
257000100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
257100101006     c*********          exsr      Error_DET
257200101006     c*********          goto      end_NAD
257300100728     C                   elseIF      vabLOD = *blank
257400100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
257500101006     c*********          exsr      Error_DET
257600101006     c*********          goto      end_NAD
257700100728     C                   ENDIF
257800100705      *-------
257900090210      * Dati mittente
258000100705      *-------
258100100705     C                   elseIF     Quale_campo ='VABRMO'
258200090210      *
258300100705     C                   movel     nad30361      VABRMO
258400090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
258500090210     C                   movel     §PTNAZ        VABNMO
258600090210      *
258700090210     C                   IF        nad3207 <> *blanks
258800100705     C                   eval      naz = 1
258900100705     C     nad3207       LOOKUP    ISO(naz)                               32
259000090210     C                   if         *in32 = *on
259100100705     C                   MOVEL     C15(naz)      VABNMO
259200090210     C                   endif
259300090210     C                   ENDIF
259400090210      *
259500090210      * Normalizzo CAP mittente originale
259600090210     C                   clear                   T
259700090210     C                   clear                   S
259800090210     C                   MOVEA     *BLANKS       CAPM
259900090210     C                   MOVEA     nad3251       CAP9
260000090210     C                   DO        9             T
260100090210     C     CAP9(T)       IFNE      *BLANKS
260200090210     C                   ADD       1             S
260300090210     C                   MOVE      CAP9(T)       CAPM(S)
260400090210     C                   ENDIF
260500090210     C                   ENDDO
260600090210     C*
260700090210      *     Controllo validità CAP
260800090210     C                   MOVEA     CAPM          VABCMO
260900090210      *
261000090210     C                   CLEAR                   TISI95DS
261100090210     C                   MOVEL     VABCMO        I95CAP
261200090210     C                   MOVEL     VABNMO        I95NAR
261300090210     C                   MOVEL     WOGGI         I95DAT
261400090210     C                   MOVEL     '3'           I95TCN
261500090210     C                   CALL      'TISI95R'
261600090210     C                   PARM                    TISI95DS
261700090210      *     Errore
261800090210     C     O95ERR        IFNE      *BLANKS
261900090210     C                   MOVEL     INDCAE        VABCMO
262000090210     C                   MOVEL     §PTNAZ        VABNMO
262100090210     C                   END
262200090603      *
262300100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
262400100705     C*  record del VAB viene PULITO
262500100316     c                   if        Dati_di_Testata  = 'S'
262600090603     c                   eval      tes_VABrmo = VABrmo
262700090603     c                   eval      tes_VABnmo = VABnmo
262800090603     c                   eval      tes_VABcmo = VABcmo
262900090603     c                   eval      NAD_mittente_in_testata = 'S'
263000100701     c                   else
263100100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
263200090603     c                   end
263300100728      *
263400100728      *   se NON c'è l'identificativo bolla del cliente
263500100728      *    blocca la spedisione e lo segnala.
263600100728     C                   if          vabRMO = *blank
263700100728     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
263800101006     c*********          exsr      Error_DET
263900101006     c*********          goto      end_NAD
264000100728     C                   ENDIF
264100090210      *
264200090210     C                   END
264300090210      *
264400090210     c     end_NAD       endSR
264500090210      * ?================================================================== */
264600090210     C*?  Contatto a cui far riferimento
264700090210      * ?================================================================== */
264800090210     C     DATI_CTA      BEGSR
264900090210      *
265000090210      *  memorizza il riferimento di consegna x il destinatario
265100090210     C     cta3412       ifne      *BLANKS
265200100701     c                   if        Dati_di_Testata  ='S'
265300100701     c                   eval       CTA_Destinatario_in_testata   = cta3412
265400100701     c                   else
265500100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
265600100701     c                   end
265700100701     c                   end
265800090210      *
265900090210     c                   endSR
266000090210      * ?================================================================== */
266100090210     C*?  Contatto telefonico  a cui far riferimento
266200090210      * ?================================================================== */
266300090210     C     DATI_COM      BEGSR
266400090210      *
266500100705      *  memorizza il riferimento di consegna nr.telefonico
266600100705     C                   if        com3148 <> *BLANKS    and
266700100705     C                              com3155 = Telephone_Number
266800100701     c                   if        Dati_di_Testata  ='S'
266900100701     c                   eval      COM_telefono_in_testata   = com3148
267000100701     c                   else
267100100701     c                   eval      COM_telefono_in_dettaglio = com3148
267200090210     c                   end
267300100705     c                   endif
267400090210      *
267500090209     C                   ENDSR
267600090210      * ?================================================================== */
267700090210     C*?  Dati di dettaglio della spedizione e dei colli
267800090210      * ?================================================================== */
267900090210     C     DATI_GID      BEGSR
268000090210      *
268100090210     C* Se ho totali per spedizione metto in campi VAB
268200090210     C     gid1496       IFEQ      99999
268300090210     C     gid1496       orEQ      9999
268400100630     c                   eval      GID_aTotale = 'S'
268500090210     C                   z-add     gid722420     VABNCL
268600090210     C                   ELSE
268700100630     c                   If        GID_aTotale = 'N'
268800100705     C                   z-add     gid722420     GID_ColliInAdd
268900100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
269000090210     C                   END
269100090210     C                   END
269200090212      *
269300100701     C                   ADD       VABNCL        ToT_Colli        16 0
269400090210      *
269500090210     c     end_GID       endSR
269600090210      * ?================================================================== */
269700090210     C*?  Dati di dettaglio misure spedizione e colli
269800090210      * ?================================================================== */
269900090210     C     DATI_MEA      BEGSR
270000090210      *
270100090210     C*  Peso
270200100319     C     mea6311       IFEQ      Weight
270300100319     C     mea6411       ANDEQ     Kilograms
270400090210      *
270500090210     C*  Controllo se ho valore totale o parziale
270600090210      *   sempre il Lordo
270700100319???? C     mea6313       IFEQ      G_Weight
270800100319???? C     mea6313       oreq      Gross_Weight
270900100706???? C     mea6313       oreq      Gross_Weight_E
271000110512???? C     mea6313       oreq      Gross_Weight_D
271100101027???? C     mea6313       oreq      *blank
271200100630     C                   IF        GID_aTotale = 'S'
271300091019???? C                   move      mea6314       mea6314_2        18 2          Peso
271400091019???? C                   Z-ADD     mea6314_2     VABPKB                         Peso
271500090210     C                   ELSE
271600091019???? C                   move      mea6314       mea6314_2                      Peso
271700091019???? C                   ADD       mea6314_2     VABPKB                         Peso
271800090210     C                   END
271900091203     C                   END
272000090210      *
272100100319???? C     mea6313       IFEQ      G_Weight
272200100319???? C     mea6313       oreq      Gross_Weight
272300100706???? C     mea6313       oreq      Gross_Weight_E
272400110512???? C     mea6313       oreq      Gross_Weight_D
272500101027???? C     mea6313       oreq      *blank
272600091019???? C                   move      mea6314       mea6314_2                      Peso
272700100701???? C                   ADD       mea6314_2     ToT_PesoLordo
272800090210     C                   END
272900100706???? C     mea6313       IFEQ      N_Weight
273000100706???? C     mea6313       oreq      Net_Weight
273100100706???? C     mea6313       oreq      Gross_Weight_E
273200110512???? C     mea6313       oreq      Gross_Weight_D
273300101027???? C     mea6313       oreq      *blank
273400091019???? C                   move      mea6314       mea6314_2                      Peso
273500100701???? C                   ADD       mea6314_2     ToT_PesoNetto
273600090210     C                   END
273700090210     C                   END
273800090210      ***
273900090210     C*  Peso in Grammi  se abilitato a utilizzarlo
274000090210      ***
274100100319     C     mea6311       IFEQ      Weight
274200100319     C     mea6411       ANDEQ     Grams
274300090210      *
274400090210     C*  Controllo se ho valore totale o parziale
274500100319???? C     mea6313       IFEQ      G_Weight
274600100319???? C     mea6313       oreq      Gross_Weight
274700100706???? C     mea6313       oreq      Gross_Weight_E
274800110512???? C     mea6313       oreq      Gross_Weight_D
274900101027???? C     mea6313       oreq      *blank
275000100630     c                   If        GID_aTotale = 'S'
275100091019???? c                   move      mea6314       mea6314_2
275200091019???? C     mea6314_2     div       1000          VABPKB                         Peso
275300090210     C                   ELSE
275400091019???? c                   move      mea6314       mea6314_2
275500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
275600091019???? C                   add       Peso_inKG     VABPKB                         Peso
275700090210     C                   END
275800090210     C                   END
275900090210      *
276000100319???? C     mea6313       IFEQ      G_Weight
276100100319???? C     mea6313       oreq      Gross_Weight
276200100706???? C     mea6313       oreq      Gross_Weight_E
276300110512???? C     mea6313       oreq      Gross_Weight_D
276400101027???? C     mea6313       oreq      *blank
276500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
276600100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
276700090210     C                   END
276800100706???? C     mea6313       IFEQ      N_Weight
276900100706???? C     mea6313       oreq      Net_Weight
277000100706???? C     mea6313       oreq      Gross_Weight_E
277100110512???? C     mea6313       oreq      Gross_Weight_D
277200101027???? C     mea6313       oreq      *blank
277300091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
277400100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
277500090210     C                   END
277600090210     C                   END
277700090210      *  Volume
277800100319???? C     mea6311       IFEQ      Volume
277900100319???? C     mea6411       ANDEQ     Cubic_Meters
278000090210      *  Controllo se ho valore totale o parziale
278100100630     c                   If        GID_aTotale = 'S'
278200091019???? C                   move      mea6314       mea6314_2        18 2          Volume
278300091019???? C                   Z-ADD     mea6314_2     VABVLB                         Volume
278400090210     C                   ELSE
278500091019???? C                   move      mea6314       mea6314_2        18 2          Volume
278600091019???? C                   ADD       mea6314_2     VABVLB                         Volume
278700090210     C                   END
278800090210     C                   END
278900100705      *
279000100705      *
279100100705     C* Ricerco CAP in base alle dimensioni
279200090210     C     WCAP          IFNE      *BLANKS
279300100319     C     mea6311       IFEQ      Weight
279400090210      *
279500090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
279600090210     C                   CLEAR                   FNLV55DS
279700090210     C                   MOVEL     'P'           D55TPT
279800090210     C                   MOVEL     WOGGI         D55DRF
279900090210     C                   MOVEL     VABLNP        D55LNP
280000090210     C                   MOVEL     VABLNP        D55LIN
280100090210     C                   CALL      'FNLV55R'
280200090210     C                   PARM                    FNLV55DS
280300090210     C                   MOVE      D55TFP        COMTFP            3 0
280400090210      *
280500090210     C*  PASSO IL TERMINAL PARTENZA
280600090210     C                   CLEAR                   TISI95DS
280700090210     C                   Z-ADD     COMTFP        I95TFP
280800090210     C                   MOVEL     VABTSP        I95TSP
280900090210     C                   MOVEL     'S'           I95FRE
281000090210     C                   MOVEL     '3'           I95TCN
281100090210     C                   MOVEL     WCAP          I95CAP
281200090210     C                   MOVEL     VABNZD        I95NAR
281300090210     C                   MOVEL     VABLOD        I95LOC
281400090210     C                   MOVEL     VABIND        I95IND
281500090210      *
281600090210     C* Se gestita seconda linea di arrivo passo peso - volume
281700090210     C* e numero colli effettivo
281800090210     C     §PULNA        IFEQ      'S'
281900090210     C                   Z-ADD     VABPKB        I95LKG
282000090210     C                   Z-ADD     VABVLB        I95LMC
282100090210     C                   ELSE
282200100706     C*******
282300090210     C* Altrmenti passo 1 Kg e 0,001
282400100706     C*******              Z-ADD1                  I95LKG
282500100706     C*******              Z-ADD0,001              I95LMC
282600100706     C*******
282700090210     C                   END
282800090210     C*
282900090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
283000090210     C                   SELECT
283100090210     C     VABCBO        WHENEQ    '1 '
283200090210     C                   MOVEL     *BLANKS       I95FCA
283300090210     C                   MOVEL     'F'           I95TPO
283400090210     C     VABCBO        WHENEQ    '2 '
283500090210     C                   MOVEL     *BLANKS       I95FCA
283600090210     C                   MOVEL     'A'           I95TPO
283700090210     C     VABCBO        WHENEQ    '4 '
283800090210     C                   MOVEL     'S'           I95FCA
283900090210     C                   MOVEL     'F'           I95TPO
284000090210     C     VABCBO        WHENEQ    '6 '
284100090210     C                   MOVEL     'S'           I95FCA
284200090210     C                   MOVEL     'A'           I95TPO
284300090210     C                   OTHER
284400090210     C                   MOVEL     'F'           I95TPO
284500090210     C     VABCAS        IFGT      0
284600090210     C                   MOVEL     'S'           I95FCA
284700090210     C                   ELSE
284800090210     C                   MOVEL     *BLANKS       I95FCA
284900090210     C                   END
285000090210     C                   ENDSL
285100090210     C*
285200090210     C                   CALL      'TISI95R'
285300090210     C                   PARM                    TISI95DS
285400090210     C* Reperisco i dati da CAP
285500090210     C                   MOVEL     O95PRV        VABPRD
285600100706     C                   MOVEL     WCAP          VABCAD
285700100114      ***
285800100706     c                   MOVEL     O95LNA        VABLNA
285900100706     C                   MOVEL     O95ZNC        VABZNC
286000090210     C*
286100090210     C     O95ERR        IFNE      *BLANKS
286200090210     C     O95FIT        ANDEQ     'S'
286300090210     C*  Cap non trovato
286400090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
286500101006     c*********          exsr      Error_DET
286600101006     C*********          goto      end_MEA
286700090210     C                   END
286800090210     C                   END
286900090210      *
287000090210     C                   ELSE
287100090210      *
287200090210     C                   CLEAR                   VABPRD
287300090210     C                   CLEAR                   VABLNA
287400090210     C                   CLEAR                   VABZNC
287500090210     C                   CLEAR                   VABCAD
287600090210     C*  Cap non trovato
287700090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
287800101006     c*********          exsr      Error_DET
287900101006     C*********          goto      end_MEA
288000090210     C                   END
288100090210      *
288200090210     c     end_MEA       endSR
288300090210      * ?================================================================== */
288400090210     C*?  Dati di dettaglio Codice a barre Segnacolli
288500090210      * ?================================================================== */
288600090210     C     DATI_PCI      BEGSR
288700090210      *
288800090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
288900090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
289000090210     C     §1BF12        IFEQ      'E'
289100100706     C                   EXSR      SEGNaCollo_EEX
289200090210     C                   ELSE
289300090210      *
289400090210      *  La prima volta controllo congruità numero segnacollo
289500090210     C     §PUBS1        IFNE      *BLANKS
289600090210     C     VABnrs        ANDNE     0
289700090210      *  Se il codice trattamento merce prevede che segnacollino i
289800090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
289900090210     C     §1BFG7        IFEQ      'S'
290000090210     C     §1BFG1        OREQ      'S'
290100090210      *  calcolo segnacollo
290200100706     C                   EXSR      Segn_Bartolini
290300090210     C                   END
290400090210     C                   END
290500090210      *
290600090210     C                   END
290700090210      *
290800090210     c     end_PCI       endSR
290900090211     C*----------------------------------------------------------------
291000090211      *? dati finali del dettaglio    UNT
291100090211     C*----------------------------------------------------------------
291200090211     C     DATI_UNT      BEGSR
291300110324      *
291400110324      * Non deve dare messaggio
291500110324     c                   goto      No_UNTctrl
291600090211      *
291700100708     c                   eval      UNT_record = vnREC
291800090213     c                   z-add     vnrec         last_RECord
291900090211      **
292000100319      **   verifica la quadratura di tutti i segmenti ricevuti
292100100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
292200100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
292300100728     C     UNT0074       IFne      Conta_segmenti
292400100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
292500100319     c                             trollare il Messaggio ricevuto'
292600100319      *
292700100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
292800100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
292900100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
293000100319     C                             Bolle import in filiale - messaggio -
293100100319     C                             con quadratura UNT errata controllare EDI -
293200100319     C                             ricevuto su UPLOAD x il cliente :'
293300100319     C                   EVAL      Messaggio = %trim(Messaggio) +
293400100319     C                                         %editc(§PTksc:'X')
293500100319     C                   EVAL      Messaggio = %trim(Messaggio) +
293600100319     C                             ' i segmenti contati sono :' +
293700100319     C                             %editc(Conta_segmenti:'Z')
293800100319     c                   if        §PVinv <> 'N'
293900100319     c                   exsr      invio_mail
294000100319     c                   end
294100100319     c                   end
294200110321     c     No_UNTctrl    tag
294300100319      **
294400100319     c     End_UNT       endSR
294500100316     C*----------------------------------------------------------------
294600100316      *? dati finali     UNT
294700100316     C*----------------------------------------------------------------
294800100316     C     DATI_UNZ      BEGSR
294900100316      *
295000100726     c                   move      'S'           Forza_Uscita
295100100316      **
295200100316     c                   endSR
295300090210      *----------------------------------------------------------------
295400090210      *? Input segnacolli su schiera di 20 elementi
295500090210      *----------------------------------------------------------------
295600090210     C     INP_Segnacol  BEGSR
295700090210      *
295800090210     c                   clear                   quanti_PCI        3 0
295900090210     c                   if        §puEL1 = 0
296000090210     c                   z-add     10            quanti_PCI
296100090210     c                   else
296200090210     c                   z-add     §puEL1        quanti_PCI
296300090210     c                   end
296400090210      *
296500090210      *  Pulisce la schiera dei segnacolli da elaborare
296600090210     c                   clear                   x30
296700090210     c                   z-add     0             Nr_x30            3 0
296800090210      *
296900090210      *   riporto tutto sulla schiera generica X30
297000090210      *
297100090210     c                   do        10            limite            3 0
297200090210      * Limita quanti elementi passati in ogni PCI
297300090210      *   devono essere considerati
297400090210     c                   if        limite > quanti_PCI
297500090210     c                   Leave
297600090210     c                   end
297700090210      *
297800090210      *  Carica la schiera generale di tutti i segnacolli
297900090210     c                   if        D30_1(limite) <> *blank
298000090210     c                   add       1             Nr_x30
298100090210     c                   eval      X30(Nr_x30) = D30_1(limite)
298200090210     c                   end
298300090210     c                   enddo
298400090210      *
298500090210     C                   ENDSR
298600090210      *----------------------------------------------------------------
298700090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
298800090210      *----------------------------------------------------------------
298900100706     C     SEGNaCollo_EEXBEGSR
299000090210      *
299100090210     c                   exsr      INP_Segnacol
299200090210      *
299300090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
299400090210     C                   DO        10            x                              --- 01 ---
299500090210      *
299600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
299700090210     C     x30(X)        ANDNE     *LOVAL
299800090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
299900100701     C                   ADD       1             totale_PCI
300000100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
300100090210     C                   END                                                    --- 03 ---
300200090210      *
300300090210     C                   END                                                    --- 02 ---
300400090210      *
300500090210     C                   ENDSR
300600090210     C*----------------------------------------------------------------
300700090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
300800090210     C*----------------------------------------------------------------
300900100706     C     Segn_BartoliniBEGSR
301000090210      *
301100090210     c                   exsr      INP_Segnacol
301200090210     C*
301300090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
301400090210     C*  in base al numero elementi validi carico sgn.
301500090210     C                   DO        10            X                              --- 01 ---
301600090210     C*
301700090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
301800090210     C     x30(X)        ANDNE     *LOVAL
301900090210     C*
302000100630     C                   CLEAR                   DS_SegnBART
302100090210     C*  Controllo se devo ricevere la linea di partenza
302200090210     C                   MOVEL     'N'           WERRO             1
302300090210     C     §PULP1        IFGT      0                                            --- 03 ---
302400100706     C                   EXSR      REPerisce_LNP
302500090210     C                   END                                                    --- 03 ---
302600100706      *
302700090210     C*  Controllo se devo ricevere la linea di arrivo
302800090210     C     §PULA1        IFGT      0                                            --- 03 ---
302900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
303000100706     C                   EXSR      REPerisce_LNA
303100090210     C                   END                                                    --- 03 ---
303200100706      *
303300090210     C*  Controllo se devo ricevere il numero di serie
303400090210     C     §PUNR1        IFGT      0                                            --- 03 ---
303500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
303600100706     C                   EXSR      REPerisce_NRS
303700090210     C                   END                                                    --- 03 ---
303800100706      *
303900090210     C*  Controllo se devo ricevere il numero segnacollo
304000090210     C     §PUSG1        IFGT      0                                            --- 03 ---
304100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
304200100706     C                   EXSR      REPerisce_SGN
304300090210     C                   END                                                    --- 03 ---
304400100706      *
304500090210     C*  Controllo se devo ricevere la zona di consegna
304600090210     C     §PUZN1        IFGT      0                                            --- 03 ---
304700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
304800100706     C                   EXSR      REPerisce_ZNC
304900090210     C                   END                                                    --- 03 ---
305000100706      *
305100090210     C*  Imposto da VAB i dati a zero se mi è stato passato
305200090210     C*  numero segnacollo valido
305300090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
305400100706      *
305500100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
305600100630     C                   Z-ADD     VABLNP        SegnBART_LNP
305700090210     C                   END                                                    --- 04 ---
305800100706      *
305900100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
306000100630     C                   Z-ADD     VABLNA        SegnBART_LNA
306100090210     C                   END                                                    --- 04 ---
306200100706      *
306300100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
306400100630     C                   Z-ADD     VABNRS        SegnBART_NRS
306500090210     C                   END                                                    --- 04 ---
306600100706      *
306700100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
306800100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
306900090210     C                   END                                                    --- 04 ---
307000090210      *
307100090210     C*  mi reimposto LNA e zona da segnacollo
307200100114      ***
307300100114      *** dovendo accorpare le spedizioni da Conferma x CMR
307400100114      ***  non deve impostare l'instradamento.
307500100630     C                   Z-ADD     SegnBART_LNA  VABLNA
307600100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
307700100114      ***
307800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
307900100701     C                   ADD       1             Conta_Segn
308000100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
308100090210     C                   END                                                    --- 03 ---
308200090210     C*
308300090210     C                   END                                                    --- 02 ---
308400090210     C*
308500090210     C                   END                                                    --- 01 ---
308600090210      *
308700090210     C                   ENDSR
308800090210     C*----------------------------------------------------------------
308900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
309000090210     C*----------------------------------------------------------------
309100100706     C     REPerisce_LNP BEGSR
309200090210     C*
309300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
309400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
309500090210     C*  dal numero segnacollo passatomi dal partner
309600090210     C                   Z-ADD     §PULP1        WP                2 0
309700090210     C     3             SUBST     x30(X):WP     WLNP              3
309800090210     C*
309900090210     C*  controllo che la lnp calcolata sia numerica
310000090210     C                   TESTN                   WLNP                 98
310100090210     C*
310200090210     C*  se è numerica la carico
310300090210     C     *IN98         IFEQ      '1'
310400100630     C                   MOVE      WLNP          SegnBART_LNP
310500090210     C                   ELSE
310600090210     C*  se non è numerica stampo errore
310700090210     C                   MOVEL     'S'           WERRO             1
310800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
310900090213     c                   exsr      Error_DET
311000090210     C                   goto      end_LNP
311100090210     C                   END
311200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
311300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
311400100630     C     SegnBART_LNP  IFNE      VABLNP
311500090210     C                   MOVEL     'S'           WERRO             1
311600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
311700090210     c                             a della Bolla'
311800090213     c                   exsr      Error_DET
311900090210     C                   goto      end_LNP
312000090210     C                   END
312100090210     C*
312200090210     C     end_LNP       ENDSR
312300090210     C*----------------------------------------------------------------
312400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
312500090210     C*----------------------------------------------------------------
312600100706     C     REPerisce_LNA BEGSR
312700090210     C*
312800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
312900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
313000090210     C*  dal numero segnacollo passatomi dal partner
313100090210     C                   Z-ADD     §PULA1        WP                2 0
313200090210     C     3             SUBST     x30(X):WP     WLNA              3
313300090210     C*  controllo che la lna calcolata sia numerica
313400090210     C                   TESTN                   WLNA                 98
313500090210     C*  se è numerica la carico
313600090210     C     *IN98         IFEQ      '1'
313700100630     C                   MOVE      WLNA          SegnBART_LNA
313800090210     C                   ELSE
313900090210     C*  se non è numerica stampo errore
314000090210     C                   MOVEL     'S'           WERRO             1
314100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
314200090213     c                   exsr      Error_DET
314300090210     C                   goto      end_LNA
314400090210     C                   END
314500090210     C*
314600090210     C     end_LNA       ENDSR
314700090210     C*----------------------------------------------------------------
314800090210     C*? REPNRS - Reperisce numero serie
314900090210     C*----------------------------------------------------------------
315000100706     C     REPerisce_NRS BEGSR
315100090210     C*
315200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
315300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
315400090210     C*  dal numero segnacollo passatomi dal partner
315500090210     C                   Z-ADD     §PUNR1        WP                2 0
315600090210     C     2             SUBST     x30(X):WP     WNRS              2
315700090210     C*  controllo che il nrs calcolata sia numerico
315800090210     C                   TESTN                   WNRS                 98
315900090210     C*  se è numerica la carico
316000090210     C     *IN98         IFEQ      '1'
316100100630     C                   MOVE      WNRS          SegnBART_NRS
316200090210     C                   ELSE
316300090210     C*  se non è numerico stampo errore
316400090210     C                   MOVEL     'S'           WERRO             1
316500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
316600090213     c                   exsr      Error_DET
316700090210     C                   goto      end_NRS
316800090210     C                   END
316900090210      *
317000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
317100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
317200100630     C     SegnBART_NRS  IFNE      VABNRS
317300090210     C                   MOVEL     'S'           WERRO             1
317400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
317500090210     c                             o della Bolla'
317600090213     c                   exsr      Error_DET
317700090210     C                   goto      end_NRS
317800090210     C                   END
317900090210     C*
318000090210     C     end_NRS       ENDSR
318100090210     C*----------------------------------------------------------------
318200090210     C*? REPZNC - Reperisce zona consegna
318300090210     C*----------------------------------------------------------------
318400100706     C     REPerisce_ZNC BEGSR
318500090210     C*
318600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
318700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
318800090210     C*  dal numero segnacollo passatomi dal partner
318900090210     C                   Z-ADD     §PUZN1        WP                2 0
319000090210     C     2             SUBST     x30(X):WP     WZNC              2
319100090210     C*  controllo che la zona calcolata sia numerica
319200090210     C                   TESTN                   WZNC                 98
319300090210     C*  se è numerica la carico
319400090210     C     *IN98         IFEQ      '1'
319500100630     C                   MOVE      WZNC          SegnBART_ZNC
319600090210     C                   ELSE
319700090210     C*  se non è numerica stampo errore
319800090210     C                   MOVEL     'S'           WERRO             1
319900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
320000090213     c                   exsr      Error_DET
320100090210     C                   goto      end_ZNC
320200090210     C                   END
320300090210     C*
320400090210     C     end_ZNC       ENDSR
320500090210     C*----------------------------------------------------------------
320600090210     C*? REPSGN - Reperisce numero segnacollo
320700090210     C*----------------------------------------------------------------
320800100706     C     REPerisce_SGN BEGSR
320900090210     C*
321000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
321100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
321200090210     C*  indicata in tabella)
321300090210     C*  dal numero segnacollo passatomi dal partner
321400090210     C                   Z-ADD     §PUSG1        WP                2 0
321500090210     C     7             SUBST     x30(X):WP     WSGN              7
321600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
321700090210     C                   TESTN                   WSGN                 98
321800090210     C*  se è numerico lo carico
321900090210     C     *IN98         IFEQ      '1'
322000100630     C                   MOVE      WSGN          SegnBART_NSC
322100090210     C                   ELSE
322200090210     C*  se non è numerica stampo errore
322300090210     C                   MOVEL     'S'           WERRO             1
322400100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
322500101006     c*********          exsr      Error_DET
322600101006     C*********          goto      end_SGN
322700090210     C                   END
322800090210     C*
322900090210     C     end_SGN       ENDSR
323000051205      *----------------------------------------------------*
323100051205      *   DEFINIZIONE CHIAVI                               *
323200051205      *----------------------------------------------------*
323300051205     C     *INZSR        BEGSR
323400051205      *
323500991129     c     *ENTRY        PLIST
323600060613     C                   parm                    esito             1
323700971215      *
323800050112     C     KTABel        KLIST
323900050112     C                   KFLD                    TBLKUT
324000050112     C                   KFLD                    TBLCOD
324100050112     C                   KFLD                    TBLKEY
324200090210      *
324300090210     C     K_TAB1L       KLIST
324400090213     C                   KFLD                    etbUNB
324500090213     C                   KFLD                    etbSGM
324600090213     C                   KFLD                    etbVALSGM
324700090209     C*
324800090209     C* Definisco chiavi di accesso
324900090209     C     KTAB1         KLIST
325000090209     C                   KFLD                    KKUT
325100090209     C                   KFLD                    KCOD
325200090209     C*
325300090209     C     KNUF          KLIST
325400090209     C                   KFLD                    KAAA
325500090209     C                   KFLD                    KCNU
325600090209     C                   KFLD                    KFIL
325700090209      *
325800090209     C     KVAB1         KLIST
325900090209     C                   KFLD                    KCCM
326000090209     C                   KFLD                    KCMR
326100090209     C                   KFLD                    KCNT
326200090209     C                   KFLD                    KAAS
326300090209     C                   KFLD                    KLNP
326400090209     C                   KFLD                    KNRS
326500090209     C                   KFLD                    KNSP
326600090209      *
326700090209     C     KVAB2         KLIST
326800090209     C                   KFLD                    KCCM
326900090209     C                   KFLD                    KCMR
327000090209      *
327100090209     C     KRDE          KLIST
327200090209     C                   KFLD                    KAAS
327300090209     C                   KFLD                    KLNP1
327400090209     C                   KFLD                    KNRS
327500090209     C                   KFLD                    KNSP
327600090209     C                   KFLD                    KNMC
327700090209     C                   KFLD                    KNRP
327800090209      *
327900090209     C     KACO          KLIST
328000090209     C                   KFLD                    KKUT
328100090209     C                   KFLD                    KKCC
328200090209     C                   KFLD                    KKSC
328300090209      *
328400090209     C     KIND          KLIST
328500090209     C                   KFLD                    KKUT
328600090209     C                   KFLD                    KKCC
328700090209     C                   KFLD                    KKSC
328800111027      *
328900111027     C     KTAS          KLIST
329000111027     C                   KFLD                    TASAAS
329100111027     C                   KFLD                    TASLNP
329200111027     C                   KFLD                    TASNRS
329300111027     C                   KFLD                    TASNSP
329400090209      *
329500090209      * DETERMINO SE SONO BARTOLINI O SDI
329600090209     C                   CLEAR                   TIBS55DS
329700090209     C                   MOVEL     'L'           I50TLA
329800090209     C                   CALL      'TIBS55R'
329900090209     C                   PARM                    TIBS55DS
330000090209      *
330100090209     C* RECUPERO DATI DELL'UTENTE
330200090209     C                   Z-ADD     1             CODUT             1 0
330300090209     C                   CALL      'XPARUT'
330400090209     C                   PARM                    UTEDSE0F
330500090209     C                   MOVEL     RAGUT         RSUT             20
330600090209     C*
330700090209     C* RICERCA CAPOCONTI
330800090209     C                   DO        50            X
330900090209     C                   MOVE      TCU(X)        TCUDS
331000090209     C     F56           IFEQ      'CG'
331100090209     C     F34           ANDEQ     '01'
331200090209     C                   Z-ADD     KCU(X)        KCI               4 0
331300090209     C                   END
331400090209     C                   END
331500090209     C*
331600090211     c                   movel(p)  'EDPAB'       User_Msg         10
331700090211     C*
331800090209     C* IMPOSTO A ZERO VARIABILI DI PGM
331900090209     c                   move      *blank        Snd_Msg_alert     1
332000090209     C                   MOVEL     CA(1)         WCA              78
332100090209     C                   MOVEL     CN(1)         WCN              78
332200090209     C*
332300090209     C* RECUPERO DATA E ORA
332400100630     C                   TIME                    Ora_Data         14 0
332500100630     C                   MOVE      Ora_Data      G02DAT
332600100630     C                   MOVE      Ora_Data      WDATA8            8 0
332700100630     C                   MOVE      WDATA8        Anno2             2 0
332800090209     C                   MOVEL     WDATA8        DATA              6 0
332900100630     C                   MOVE      Anno2         DATA
333000090209     C                   MOVEL     *BLANK        G02ERR
333100090209     C                   CALL      'XSRDA8'
333200090209     C                   PARM                    WLBDA8
333300090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
333400100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
333500090209     C                   TIME                    WORA              6 0
333600090209      * Recupero data e ora
333700090209     C                   TIME                    W0140
333800090209      * UDATE IN GGMMAAAA
333900100630     C                   MOVE      W0140         DataGGMMAAA
334000090209      * UDATE IN AAAAMMGG
334100100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
334200090209     C     *iso          MOVEL     DATA_eur      dateu
334300090209      *
334400090209      * ?              /*--------------------------- */
334500090209     c                   exsr      CARICA_TABELLE
334600090209      * ?              /*--------------------------- */
334700090209      *
334800090209     C                   MOVEL     *ALL'-'       CMP132          132
334900991124      *
335000050414      *------------------
335100991124     C                   ENDSR
335200060621      * ?------------------------------------------------------------------ */
335300090209      *?    CARICA TABELLE SU SCHIERE
335400060621      * ?------------------------------------------------------------------ */
335500090209     C     CARICA_TABELLEBEGSR
335600090205      *
335700090209     C* Caricamento Tabella 1B
335800090209     C                   Z-ADD     0             X                 4 0
335900090209     C                   Z-ADD     CODUT         KKUT
336000090209     C                   MOVEL     '1B'          KCOD
336100090209     C     KTAB1         CHAIN     TABEL00F                           30
336200090209     C     *IN30         DOWEQ     '0'
336300090209     C     TBLFLG        IFEQ      *BLANKS
336400090209     C                   ADD       1             X
336500100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
336600100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
336700090209     C                   END
336800090209     C     KTAB1         READE     TABEL00F                               30
336900090209     C                   END
337000090209      *
337100090209     C* Caricamento Tabella 15
337200090209     C                   Z-ADD     0             X                 4 0
337300090209     C                   Z-ADD     CODUT         KKUT
337400090209     C                   MOVEL     '15'          KCOD
337500090209     C     KTAB1         CHAIN     TABEL00F                           30
337600090209     C     *IN30         DOWEQ     '0'
337700090209     C     TBLFLG        IFEQ      *BLANKS
337800090209     C                   ADD       1             X
337900090209     C                   MOVEL     TBLKEY        C15(X)
338000090209     C                   MOVEL     TBLUNI        DS15
338100090209     C                   MOVEL     §15COD        ISO(X)
338200090209     C                   MOVEL     §15ECF        CPF(X)
338300090209     C                   MOVE      §15PCF        CPF(X)
338400090209     C                   END
338500090209     C     KTAB1         READE     TABEL00F                               30
338600090209     C                   END
338700090209      *
338800090209     C* Caricamento Tabella CV
338900090209     C                   Z-ADD     0             X
339000090209     C                   Z-ADD     CODUT         KKUT
339100090209     C                   MOVEL     'CV'          KCOD
339200090209     C     KTAB1         CHAIN     TABEL00F                           30
339300090209     C     *IN30         DOWEQ     '0'
339400090209     C     TBLFLG        IFEQ      *BLANKS
339500090209     C                   ADD       1             X
339600090209     C                   MOVEL     TBLKEY        CCV(X)
339700090209     C                   MOVEL     TBLUNI        DCV(X)
339800090209     C                   END
339900090209     C     KTAB1         READE     TABEL00F                               30
340000090209     C                   END
340100090216      *
340200100702     C* Caricamento Tabella Priorità trasporto
340300100702     C                   Z-ADD     0             X
340400100702     C                   MOVEL     'TS'          TABCOD
340500100702     C     TABCOD        CHAIN     EDTAB01L                           30
340600100702     C     *IN30         DOWEQ     '0'
340700100702     C     TABFLG        IFEQ      *BLANKS
340800100702     C                   ADD       1             X
340900100702     C                   eval      TipoServizio(X) = TABUNI
341000100705     C                   eval      Key_TipServ(X)  = TABKEY
341100100702     C                   END
341200100702     C     TABCOD        READE     EDTAB01L                               30
341300100702     C                   END
341400100702      *
341500110328      * Caricamento Tabella termini consegna
341600110328     C                   Z-ADD     0             X
341700110328     C                   MOVEL     'TB'          TABCOD
341800110328     C     TABCOD        CHAIN     EDTAB01L                           30
341900110328     C     *IN30         DOWEQ     '0'
342000110328     C     TABFLG        IFEQ      *BLANKS
342100110328     C                   ADD       1             X
342200110328     C                   eval      K35_TpBolla(X) = TABKEY
342300110328     C                   eval      Cod_TpBolla(X) = TABKEY
342400110328     C                   eval      Decod_Porto(X) = TABUNI
342500110328     C                   END
342600110328     C     TABCOD        READE     EDTAB01L                               30
342700110328     C                   END
342800110328      *
342900090216     C* Caricamento Tabella Tipo Incasso C/Assegno
343000090216     C                   Z-ADD     0             X
343100090216     C                   MOVEL     'TC'          TABCOD
343200090216     C     TABCOD        CHAIN     EDTAB01L                           30
343300090216     C     *IN30         DOWEQ     '0'
343400090216     C     TABFLG        IFEQ      *BLANKS
343500090216     C                   ADD       1             X
343600100705     C                   eval      K35_TpIncas(X) = TABKEY
343700100705     C                   eval      TipoIncasso(X) = TABUNI
343800090216     C                   END
343900090216     C     TABCOD        READE     EDTAB01L                               30
344000090216     C                   END
344100090209      *
344200090209      * Caricamento Tabella Partner esteri
344300090209     C                   Z-ADD     0             X
344400090209     C                   MOVEL     'PT'          TABCOD
344500090209     C     TABCOD        CHAIN     EDTAB01L                           30
344600090209     C     *IN30         DOWEQ     '0'
344700090209      *
344800090209     C                   SELECT
344900090209     C     TABFLG        WHENNE    *BLANKS
345000090209      *
345100090209     C                   OTHER
345200090209     C                   ADD       1             X
345300100630     C                   MOVEL     TABKEY        PT_key(X)
345400100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
345500100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
345600100630     C                   MOVEL     TABUNI        PT_uni(X)
345700090209     C                   ENDSL
345800090209      *
345900090209     C     TABCOD        READE     EDTAB01L                               30
346000090209     C                   END
346100121105      *
346200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
346300121105     c                   exsr      ChecK_PT
346400121105      *
346500090209      * salva quanti Codici UNB ha caricato
346600100630     c                   z-add     x             PT_KeyXsav
346700090209      *
346800090209     C                   MOVEL     'PU'          TABCOD
346900090209     C     TABCOD        CHAIN     EDTAB01L                           30
347000090209     C     *IN30         DOWEQ     '0'
347100090209      *
347200090209     C                   SELECT
347300090209     C     TABFLG        WHENNE    *BLANKS
347400090209      *
347500090209     C                   OTHER
347600100630     C                   MOVEL     TABKEY        PU_key
347700090209     C                   Z-ADD     1             X
347800100630     C     PU_key        LOOKUP    PT_key(X)                              32
347900100630     C   32              MOVEL     TABUNI        PU_uni(X)
348000090209     C                   ENDSL
348100090209      *
348200090209     C     TABCOD        READE     EDTAB01L                               30
348300090209     C                   END
348400090209      *
348500090209      * Prosegue tab.PT e PU
348600090209     C                   MOVEL     'PV'          TABCOD
348700090209     C     TABCOD        CHAIN     EDTAB01L                           30
348800090209     C     *IN30         DOWEQ     '0'
348900090209      *
349000090209     C                   SELECT
349100090209     C     TABFLG        WHENNE    *BLANKS
349200090209      *
349300090209     C                   OTHER
349400100630     C                   MOVEL     TABKEY        PV_key
349500090209     C                   Z-ADD     1             X
349600100630     C     PV_key        LOOKUP    PT_key(X)                              32
349700100630     C   32              MOVEL     TABUNI        PV_uni(X)
349800090209     C                   ENDSL
349900090209      *
350000090209     C     TABCOD        READE     EDTAB01L                               30
350100090209     C                   END
350200120629      *
350300120629      * Prosegue tab.PT e PU e PV
350400120629     C                   MOVEL     'PZ'          TABCOD
350500120629     C     TABCOD        CHAIN     EDTAB01L                           30
350600120629     C     *IN30         DOWEQ     '0'
350700120629      *
350800120629     C                   SELECT
350900120629     C     TABFLG        WHENNE    *BLANKS
351000120629      *
351100120629     C                   OTHER
351200120629     C                   MOVEL     TABKEY        PZ_key
351300120629     C                   Z-ADD     1             X
351400120629     C     PZ_key        LOOKUP    PT_key(X)                              32
351500120629     C   32              MOVEL     TABUNI        PZ_uni(X)
351600120629     C                   ENDSL
351700120629      *
351800120629     C     TABCOD        READE     EDTAB01L                               30
351900120629     C                   END
352000090209      *
352100090209     C* Caricamento Tabella codici clienti - tariffe
352200090209     C                   Z-ADD     0             X
352300090209     C                   MOVEL     'CL'          TABCOD
352400090209     C     TABCOD        CHAIN     EDTAB01L                           30
352500090209     C     *IN30         DOWEQ     '0'
352600090209     C     TABFLG        IFEQ      *BLANKS
352700090209     C                   ADD       1             X
352800100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
352900100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
353000090209     C                   END
353100090209     C     TABCOD        READE     EDTAB01L                               30
353200090209     C                   END
353300090209      *
353400090209     C* Caricamento Serivizi speciali
353500090209     C                   Z-ADD     0             X
353600090209     C                   MOVEL     'SS'          TABCOD
353700090209     C     TABCOD        CHAIN     EDTAB01L                           30
353800090209     C     *IN30         DOWEQ     '0'
353900090209     C     TABFLG        IFEQ      *BLANKS
354000090209     C                   ADD       1             X
354100100702     C                   eval       Key_ServSpec(X) = TABKEY
354200100702     C                   eval       SpecialServ(X)  = TABKEY
354300100702     C                   eval       UNI_ServSpec(X) = TABUNI
354400090209     C                   END
354500090209     C     TABCOD        READE     EDTAB01L                               30
354600090209     C                   END
354700090209      *
354800090209     C                   ENDSR
354900090205      * ?------------------------------------------------------------------ */
355000090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
355100090205      * ?------------------------------------------------------------------ */
355200090205     C     *pssr         BEGSR
355300090205     C
355400060710     C                   eval      esito = '2'
355500130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 93'
355600130506      *
355700130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
355800130506     C                             IFCSUM S93A EDI ricevuto errato in UPLOAD -
355900130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
356000130506     c                             + ' verificare il '+
356100130506     c                             %editc(total_segmenti:'Z')
356200130506     c                             + ' segmento -> ' + dati
356300130506      *
356400100319     c                   if        §PVinv <> 'N'
356500100319     c                   exsr      invio_mail
356600100319     c                   end
356700060621     C
356800130506     c                   seton                                        LR
356900130506     c                   return
357000130506     C
357100130506     C                   ENDSR
357200090213      * ?------------------------------------------------------------------ */
357300090213      *?      Segnala Errore su TIVIN
357400090213      * ?------------------------------------------------------------------ */
357500090213     C     Error_DET     BEGSR
357600090213     C
357700100728     c                   eval      Segmento_in_errore = Conta_segmenti
357800090213     C                   eval      vinFLG = '2'
357900090213     c                   eval      Almeno_Un_Due ='S'
358000090213     c                   eval      esito  = '1'
358100090213     c                   eval      se_errore ='S'
358200090213     C                   eval      Err_in_detta = 'S'
358300090213     C
358400090213     C                   ENDSR
358500100705      * ?------------------------------------------------------------------ */
358600100705      *?   Controlla e decodifica Valori VALUE dei segmenti
358700100705      * ?------------------------------------------------------------------ */
358800100705     C     In_TAB_EDSTBL BEGSR
358900100705      *
359000100705      *   Cerca la decodifica del dato in TABELLA
359100100705     c                   clear                   trovata_EDSTBL    1
359200100705     c                   clear                   quale_campo       6
359300100705     c                   clear                   campo_dati_70A   70
359400100705     C                   movel     UNB_Mittente  etbUNB
359500100705     c     K_TAB1L       chain     edstbl01l
359600100705      *
359700100705      * prova quindi senza l'UNB specifico del cliente
359800100705     c                   if        not %Found(edstbl01l)
359900100705     C                   clear                   etbUNB
360000100705     c     K_TAB1L       chain     edstbl01l
360100100705     c                   end
360200100705      *
360300100705      *  Traduce i testi descrittivi se PRESENTE in tabella
360400100705      *   il campo di destinazione è definito sulla destra della descrizione
360500100705      *     ed è lungo 6 come i nomi definiti nei VAB.
360600100705     c                   if        %Found(edstbl01l)
360700100705     c                   move(p)   etbDESCRI     quale_campo
360800100705     c                   movel(p)  etbDATI       campo_dati_70A
360900100705     c                   move      'S'           trovata_EDSTBL
361000100705     c                   end
361100100705      *
361200100705      *
361300100705     C                   ENDSR
361400100705     c*==================================================================*
361500090209     C*? CNVDAT - DECODIFICA LA DATA
361600090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
361700090209     C*         - WDATA  (DATA ALFANUMERICA)
361800090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
361900090209     C*         - WHMS   (ORA NUMERICA)
362000090209     C*----------------------------------------------------------------
362100100702     C     Converte_Data BEGSR
362200090209     C*
362300090209     C                   MOVEL     ' '           WERRO             1
362400090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
362500090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
362600100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
362700090209     C                   Z-ADD     *ZEROS        WHMS              6 0
362800090209     C*
362900120905     C     Work_Format_3 IFEQ      *blank
363000120905     c                   if        %Len(%trim(Work_Data__35)) = 8
363100120905     C                   eval         Work_Format_3 = Data_senza_ora
363200120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
363300120905     C                   eval         Work_Format_3 = Data_con_ora
363400120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
363500120905     C                   eval         Work_Format_3 = Data_con_i_secondi
363600120905     c                   end
363700120905     C                   END
363800120905     C*
363900120905     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
364000100702     C                   MOVEL     Work_Data__35 WCOMO8            8
364100090209     C                   TESTN                   WCOMO8               99
364200090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
364300090209     C  N99              MOVEL     'S'           WERRO             1
364400090209     C                   END
364500090209     C*
364600100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
364700100702     C                   MOVEL     Work_Data__35 WCOMO12          12
364800100120     C                   TESTN                   WCOMO12              99
364900100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
365000100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
365100100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
365200090209     C  N99              MOVEL     'S'           WERRO             1
365300090209     C                   END
365400100120     C*
365500100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
365600100702     C                   MOVEL     Work_Data__35 WCOMO14          14
365700100120     C                   TESTN                   WCOMO14              99
365800100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
365900100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
366000100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
366100100120     C  N99              MOVEL     'S'           WERRO             1
366200100120     C                   END
366300090209     C*
366400090209     C                   ENDSR
366500090210     C*----------------------------------------------------------------
366600100701     C*? Se_Numerico       - CONTROLLO NUMERICO
366700100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
366800100701     C*         - lunghezza_num   (Lunghezza campo output)
366900100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
367000090210     C*----------------------------------------------------------------
367100100701     C     Se_Numerico   BEGSR
367200090210     C*
367300090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
367400100701     C                   MOVEL     'N'           Campo_Numerico    1
367500100701     C                   MOVEL     *BLANKS       Numero_OK        15
367600110224     C*
367700090210     C                   Z-ADD     15            IN                3 0
367800100701     C                   Z-ADD     lunghezza_num IX                3 0
367900110224     C*
368000090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
368100100701     C                   MOVEA     test_num      NDA
368200090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
368300090210     C                   MOVEA     *ALL'0'       NDN
368400100630      *
368500090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
368600100701     C                   Z-ADD     lunghezza_num I                 3 0
368700110224     C*
368800090210     C     I             DOWGT     0                                            --- 1 -->
368900110224     C*
369000090210     C* ESTRAE IL CARATTERE DA ESAMINARE
369100100701     C     1             SUBST     test_num:I    WTEM01            1
369200110224     C*
369300090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
369400090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
369500090210     C     WTEM01        SCAN      WCA                                    99
369600110224     C*
369700090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
369800090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
369900110224     C*
370000090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
370100090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
370200110224     C*
370300090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
370400090210     C* DA DESTRA VERSO SINISTRA
370500090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
370600090210     C                   MOVEL     WTEMP1        NDN(IN)
370700090210     C                   SUB       1             IN
370800090210     C                   END                                                    <-- 3 ---
370900110224     C*
371000090210     C                   END                                                    <-- 2 ---
371100090210     C                   SUB       1             I
371200110224     C*
371300090210     C                   END                                                    <-- 1 ---
371400090210     C*
371500090210     C                   MOVEA     NDN           WNDN             15
371600110224     C*
371700090210     C     WNDN          IFNE      *ZEROS
371800100701     C                   MOVEA     NDN           Numero_OK        15
371900100701     C                   MOVEL     'S'           Campo_Numerico    1
372000090210     C                   END
372100090210     C*
372200090210     C                   ENDSR
372300090210     C*----------------------------------------------------------------
372400090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
372500090210     C*----------------------------------------------------------------
372600100705     C     Converte_CAP  BEGSR
372700090210     C*
372800090210     C                   MOVEL     *BLANKS       WCAP              9
372900090210     C     nad3251       IFNE      '0'
373000090210     C     '  '          SCAN      nad3251       LENGHT            2 0
373100090210     C                   SUB       1             LENGHT
373200090210     C     LENGHT        IFLE      5
373300090210     C     LENGHT        ANDGT     0
373400090210     C                   Z-ADD     LENGHT        T                 2 0
373500090210     C                   Z-ADD     5             S                 2 0
373600090210     C                   MOVEA     nad3251       CAP9
373700090210     C                   MOVEL     *ZEROS        CAP5
373800090210     C                   DO        LENGHT
373900090210     C                   MOVE      CAP9(T)       CAP5(S)
374000090210     C                   SUB       1             S
374100090210     C                   SUB       1             T
374200090210     C                   END
374300090210     C                   MOVEA     CAP5          WCAP5             5
374400090210     C     WCAP5         IFEQ      '0'
374500090210     C                   MOVEL     *BLANKS       WCAP
374600090210     C                   ELSE
374700090210     C                   MOVEA     CAP5          WCAP
374800090210     C                   END
374900090210     C*
375000090210     C                   ELSE
375100090210     C                   MOVEL     nad3251       WCAP
375200090210     C                   END
375300090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
375400090210     C     WCAP          IFNE      *BLANKS
375500090210     C     WFLCPF        ANDNE     0
375600090210     C                   MOVEL     WFLCPF        WELE              1 0
375700090210     C                   MOVE      WFLCPF        WPOS              1 0
375800090210     C                   MOVEL     *BLANK        WCPF
375900090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
376000090210     C                   ELSE
376100090210     C                   MOVEL     *BLANKS       WCPF
376200090210     C                   END
376300090210     C                   END
376400090210     C*
376500090210     C                   ENDSR
376600090211      * ?================================================================== */
376700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
376800090211      * ?================================================================== */
376900090211     C     Scrive_VAB    BEGSR
377000090211      *
377100090211     c                   clear                   err_bloccante     1
377200090211     c                   move      'S'           Almeno_Uno
377300090211      *
377400090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
377500090211     c                   exsr      UPDVAB
377600090211      *
377700090211     C* >>>>>>>>>>>>
377800090211     C*  Richiamo pgm per scrittura immediata VAB
377900090211     C     §PUWRK        IFEQ      ' '
378000110110      ******
378100090211     C*  Richiamo pgm per esecuzione stampa
378200110110     C**** §PTCMR        IFEQ      'S'
378300110110     c******             MOVEL     BGM_nrCMR     d86CMR
378400110110     c******             if            RFF_NrCMR <> *Blank
378500110110     C******             MOVEL     RFF_NrCMR     d86CMR
378600110110     c******             end
378700110110     C******             ELSE
378800110110     C******             MOVEL     BGM_NrFviaggiod86CMR
378900110110     c******             if          RFF_NrFviaggio <> *Blank
379000110110     C******             MOVEL     RFF_NrFviaggiod86CMR
379100110110     c******             end
379200110110     C******             END
379300110110      *
379400110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
379500110110     c                   MOVEL     vabCMR        d86CMR
379600090211      *
379700090213     C                   MOVEL     ' '           d86RIE
379800090213     C                   MOVEL     §PUDTA        d86DTA
379900090213     C                   Z-ADD     1             d86CNT
380000090213     C                   Z-ADD     §PTKSC        d86CCM
380100090213     C                   MOVEL     *BLANKS       d86STA
380200090213     C                   MOVEL     'E'           d86MOD
380300090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
380400090211      * dove poi imposta la DS dei parametri passati
380500090213     C                   MOVEL     'LR'          d86CHI
380600090213     C                   MOVEL     'N'           d86CTL
380700090211      *
380800090211     c                   move      kpjbu         SvKpjbu
380900090211     c                   clear                   kpjbu
381000090216     C                   movel     *on           Commit_on         1
381100090213     C                   MOVEL     TRTC86ds      KPJBU
381200090213      * ?- Chiamata la TRTC86R2 -*
381300090213     C                   CALL      'TRTC86R2'
381400090211     C                   PARM                    KPJBA
381500090216     C                   PARM                    Commit_on
381600090211     c                   move      Svkpjbu       Kpjbu
381700090211      *
381800090211     C*  Controllo pgm per scrittura immediata VAB
381900100630     C     sk            IFNE      0
382000100630     C                   DO        sk            sx
382100100630     C                   Z-ADD     skCLienti(sx) d86CCM
382200090211      *
382300090211     c                   move      kpjbu         SvKpjbu
382400090211     c                   clear                   kpjbu
382500090216     C                   movel     *on           Commit_on         1
382600090213     C                   MOVEL     TRTC86ds      KPJBU
382700090213      * ?- Chiamata la TRTC86R2 -*
382800090213     C                   CALL      'TRTC86R2'
382900090211     C                   PARM                    KPJBA
383000090216     C                   PARM                    Commit_on
383100090211     c                   move      Svkpjbu       Kpjbu
383200090211     C                   END
383300090211     C                   END
383400090211      *
383500090211     C                   END
383600090211      * >>>>>>>>>>>>
383700090211     C                   if        se_errore  = 'S'
383800090211     c                   move      'S'           err_bloccante
383900090211     c                   goto      Error_VAB
384000090211     c                   end
384100090211     C*
384200090211     C     Error_VAB     Endsr
384300090211     C*----------------------------------------------------------------
384400090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
384500090211     C*----------------------------------------------------------------
384600090211     C     UPDVAB        BEGSR
384700090211      *
384800090211     C*  Se non è stato impostato il codice bolla lo imposto io
384900090211     C     VABCBO        IFEQ      *BLANKS
385000090211     C     VABCAS        IFGT      0
385100090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
385200090211     C                   ELSE
385300090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
385400090211     C                   END
385500131025      * se però è stato scritto e non era considerato il Contrassegno
385600131025      *  per motivi di lettura del record MOA successiva al Tipo Bolla
385700131025      *   qui sistema il codice bolla
385800131025     C                   Else
385900131025      *  Porto Franco +COD
386000131025     c                   if        VABCBO = '1' and  VABcas > *zeros
386100131025     C                   movel     '4'           VABCBO                         + C/Ass
386200131025      *  Porto Assegnato +COD
386300131025     c                   elseIF    VABCBO = '2' and  VABcas > *zeros
386400131025     C                   movel     '6'           VABCBO                         + C/Ass
386500131025     C                   ENDif
386600131025     C                   END
386700100701      * Imposta le NOte
386800100701     C     VABNOT        IFEQ      *BLANKS
386900100701     C                   MOVEL     Note_1        VABNOT
387000100701     C                   MOVEL     Note_2        VABNT2
387100100701     C                   ELSE
387200100701     C                   MOVEL     Note_1        VABNT2
387300100701     C                   END
387400090211      *
387500090211     C*  Attribuisco anno spedizione
387600100630     C     DTM_DtParten  IFGT      0                                            anno sped.
387700100630     C                   MOVEL     DTM_DtParten  VABAAS
387800090211     C                   ELSE
387900100630     C     DTM_DtFViag   IFGT      0
388000100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
388100090211     C                   END
388200090211     C                   END
388300090211      *
388400090211      *  Controllo dettaglio segnacolli
388500090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
388600100630     C                   EXSR      CTRsegn_EEX
388700090211     C                   ELSE
388800090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
388900090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
389000100630     C                   EXSR      CTRsegn_BAR
389100090211     C                   END
389200090211     C                   END
389300090211      *
389400090212      *  Imposto linea partenza e serie
389500100701     C                   eval      VABLNP = PT_vabLNP
389600100701     C                   eval      VABNRS = PT_vabNRS
389700090211      *
389800090211      *  Controllo se devo variare il codice trattamento merce
389900100701     C                   eval      VABCTM = PT_vabCTM
390000090211      *
390100090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
390200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
390300090211     c                   clear                   mantiene_nsp      1            *blk/'S'
390400100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
390500090211      *
390600090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
390700090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
390800090211      *
390900090211      *  Imposto codice cliente
391000090211     c                   Select
391100090211      *
391200100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
391300100319     C                   When        Det_RFF_FF_ksc  <> *zeros
391400100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
391500100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
391600090211      * forza LNP/CTM/NRS
391700100319     c                   if        Det_RFF_FF_lnp <> 0
391800100319     C                   eval      VABLNP = Det_RFF_FF_lnp
391900090211     c                   end
392000100319     c                   if        Det_RFF_FF_nrs <> 0
392100100319     C                   eval      VABNRS = Det_RFF_FF_nrs
392200090211     c                   end
392300100319     c                   if        Det_RFF_FF_ctm <> *blank
392400100319     C                   eval      VABCTM = Det_RFF_FF_ctm
392500090211     c                   end
392600110408     c                   if        Det_RFF_FF_tic <> *blank and VABCAS <>0
392700110408     C                   eval      VABTIC = Det_RFF_FF_tic
392800110408     c                   end
392900110408      *
393000090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
393100100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
393200100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
393300090211     c                   end
393400090211      *
393500100319      *  Specificato codice da qualificatore RFF+FF dalla testata
393600100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
393700100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
393800100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
393900090211      * forza LNP/CTM/NRS
394000100319     c                   if        Tes_RFF_FF_LNP  <> 0
394100100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
394200090211     c                   end
394300100319     c                   if        Tes_RFF_FF_NRS  <> 0
394400100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
394500090211     c                   end
394600100319     c                   if        Tes_RFF_FF_CTM  <> *blank
394700100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
394800090211     c                   end
394900110408     c                   if        Tes_RFF_FF_tic  <> *blank and VABCAS <>0
395000110408     C                   eval      VABTIC = Tes_RFF_FF_tic
395100110408     c                   end
395200090211      *
395300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
395400100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
395500100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
395600090211     c                   end
395700090211      *
395800090211      *  Imposto dati da tabella - PT-
395900090211     C                   Other
396000090211      *
396100090211     C                   MOVEL     §PTKSC        VABCCM
396200090211     C                   MOVEL     §PTCTR        VABCTR
396300090211      *
396400090211     C                   Endsl
396500090211      *
396600110408      *  attenzione il tipo Contrassegno solo se c'è un valore
396700110408     c                   if        vabCAS = 0
396800110408     c                   clear                   vabTIC
396900110623     c                   else
397000110623      *
397100110623      * Se non è stato passato il Tipo incasso a fronte di particolarità sullo
397200110623      *  specifico cliente/partner
397300110707     c                   if        inviato_tipo_incasso =  'N' and
397400110707     c                             Det_RFF_FF_ksc > *zeros     and
397500110707     c                             Det_RFF_FF_tic = *blank
397600110623      *  e se c'è "??" sul tipo incasso deve Forzare questo tipo di Incasso sulla
397700110623      *  spedizione.
397800110623     c                   z-add     1             Y
397900110623     c                   movel(p)  '??'          Work_Data__35
398000110623     c                   move      §puTC         Work_Data__35
398100130311     c                   movel     Work_Data__35 WData_35_2        2
398200110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
398300110623     c                   if         *in33
398400110623     c                   eval      vabTIC = TipoIncasso(Y)
398500110623     c                   end
398600110623      **
398700110623     c                   end
398800110623      **
398900110623     c                   end
399000110408      *
399100090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
399200090211      *    o dalla relativa CL
399300090211     C                   MOVEL     VABlnp        VABfgs
399400090211      *
399500090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
399600090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
399700090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
399800090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
399900090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
400000090211      *
400100090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
400200090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
400300090211     c                   else
400400090211      *
400500090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
400600090211     C*  lo attribuisco io
400700090211      **              **------------------**
400800090211     c                   exsr      repnum
400900090211      **              **------------------**
401000100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
401100090211      **              **------------------**
401200090211     c                   end
401300090211      *
401400090211     C*  Altrimenti attribuisco data del giorno
401500090211     C     VABMGS        IFEQ      0
401600090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
401700090211     C                   MOVE      WOGGI         VABMGS                         = oggi
401800090211     C                   END
401900090211     C*  Eseguo posizionamento su EDiVAB
402000090211     C                   Z-ADD     VABAAS        KAAS
402100090211     C                   Z-ADD     VABlnp        KLNP
402200090211     C                   Z-ADD     VABNRS        KNRS
402300090211     C                   Z-ADD     VABNSP        KNSP
402400090211     C                   MOVEL     SAVBOL        SALVA
402500090211     C     KVAB1         CHAIN     EDiVAB1L                           30
402600090211     C                   MOVEL     SALVA         SAVBOL
402700090211     C*  Imposto dati CMR
402800090211     C                   Z-ADD     1             VABCNT
402900090211     **
403000090211      *** Attenzione se si deve trattare il VAX con il CMR
403100090211      *** il campo VABCNT deve essere sempre impostato a (1)
403200090211     C     §puWRK        ifEQ      'S'
403300090211     C     §puNSP        andEQ     'S'
403400090211     C                   Z-ADD     1             VABCNT
403500090211     c                   end
403600090211     **
403700100630     C                   Z-ADD     Data_prepara  VABDTS
403800090212     C                   movel     '20'          VABDTS
403900100113     ** Forza la Data di Oggi
404000100113     C                   z-add     WOggi         VABDTS
404100100113     **
404200100630     C     Ora__prepara  mult      100           VABHMS
404300090211     C     §PTCMR        IFEQ      'S'
404400100630     C                   Z-ADD     DTM_DtCMR     VABDCM
404500100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
404600100701     c                   if          RFF_NrCMR <> *Blank
404700100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
404800100701     c                   end
404900090211     C                   ELSE
405000100630     C                   Z-ADD     DTM_DtFViag   VABDCM
405100100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
405200100701     c                   if          RFF_NrFviaggio <> *Blank
405300100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
405400100701     c                   end
405500090211     C                   END
405600100120      *
405700100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
405800100120      * accorpare la bolle ad un unico destinatario da più parti
405900100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
406000100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
406100120629     C                   if         NAD_SF_Cliente <> *blank
406200120629     c                   eval      VABCMR = NAD_SF_Cliente
406300100708     c                   else
406400100706     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z') +
406500100706     c                                      '_' +  VABRMO
406600100708     c                   end
406700100706     c                   end
406800100120      *
406900090211     C*  Se non è indicato il nome del mittente = Partner mittente
407000090211     C     VABRMO        IFEQ      *BLANKS
407100090211     C                   MOVEL     ACORAG        VABRMO
407200090211     C                   MOVEL     INDCAP        VABCMO
407300090211     C     INDCAE        IFNE      *BLANKS
407400090211     C                   MOVEL     INDCAE        VABCMO
407500090211     C                   END
407600090211     C     INDSTA        IFNE      *BLANKS
407700090211     C                   MOVEL     INDSTA        VABNMO
407800090211     C                   END
407900090211     C                   END
408000090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
408100090211     C     §1BFG1        IFEQ      'N'
408200090211     C     §1BFG7        ANDEQ     'N'
408300090211     C     §1BFG2        ANDEQ     'N'
408400090211     C***                  Z-ADD0         VABNRS
408500090211     C                   END
408600090211     C*  Imposto segancollo da - a
408700090211     C     §1BFG1        IFEQ      'S'
408800090211     C     §1BFG7        ANDEQ     'N'
408900090211     C     §1BF12        ANDNE     'E'
409000090211      *
409100100701     C     Conta_Segn    IFEQ      *ZEROS
409200090211     C                   CLEAR                   VABNCD
409300090211     C                   CLEAR                   VABNCA
409400090211     C                   ELSE
409500100630     C                   Z-ADD     segn_BAR(1)   VABNCD
409600100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
409700090211     C                   ENDIF
409800090211     C*
409900090211     C                   END
410000101027     C*
410100090211     C*  Se cap mittente originale a blank abblenco nazione
410200090211     C     VABCMO        IFEQ      *BLANKS
410300090211     C                   MOVEL     *BLANKS       VABNMO
410400090211     C                   END
410500100701     C*
410600090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
410700090603     C* Se in testata
410800100701     c                   if        NAD_destinatario_in_testata = 'S' and
410900100701     c                             NAD_destinatario_in_dettaglio <> 'S'
411000090603     c                   eval         VABrsd =  tes_VABrsd
411100090603     c                   eval         VABrd2 =  tes_VABrd2
411200090603     c                   eval         VABind =  tes_VABind
411300090603     c                   eval         VABlod =  tes_VABlod
411400090603     c                   eval         VABnzd =  tes_VABnzd
411500090603     c                   end
411600090603     C* Se in testata
411700100701     c                   if        NAD_mittente_in_testata = 'S' and
411800100701     c                             NAD_mittente_in_dettaglio <> 'S'
411900090603     c                   eval         VABrmo = tes_VABrmo
412000090603     c                   eval         VABnmo = tes_VABnmo
412100090603     c                   eval         VABcmo = tes_VABcmo
412200090603     c                   end
412300100921      *
412400101027      *   Forza il numero passato nel CNI nel riferimento Numerico
412500101027     c                   if        §PVCNIRMN = 'S'
412600101027     c                   z-add     CNI_SeNumericovabRMN
412700101027     c                   end
412800101027      *
412900100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
413000100921     C     §puWRK        ifEQ      'S'
413100100921     C     §puraggDES    andeq     'S'
413200100921     c                   clear                   vabLNA
413300100921     c                   clear                   vabZNC
413400100921     c                   clear                   vabCMR
413500100921      *  il nome identico
413600100921     c                   eval      VABCMR  = §punomeCMR
413700100921      *   e in più la data del giorno
413800100921     C     §puinGIO      ifEQ      'S'
413900100921     c                   eval      VABCMR = %trim(VABCMR)
414000100921     c                                    + %editc(WOggi_CMR:'X')
414100100921     c                   end
414200100921      *
414300100921     c                   end
414400111017      *
414500111017      *? ------------------------------------------------------------------------
414600111017      *?  SE si tratta di un Partner che invia un reso e mette nel RIF.MITT.NUMERICO
414700111017      *?   il nostro codice bolla BRT. esegue un controllo automatico su esistenza
414800111017      *?   della bolla precedentemente inviata come EXPORT....ed ora in RESO.
414900111017      *? ------------------------------------------------------------------------
415000111028      ****
415100111028      **** se nel Riferimento Numerico viene passato il riferimento di una
415200111028      ****  nostra bolla di export precedentemente inviata di cui si sta facendo
415300111028      **** il reso. (Vedi AZKAR)    controlla
415400111028      **
415500111028     c                   if        PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
415600111028      *
415700111028      * controlla che contenga un numero di 14 cifre valide
415800111028      *   quindi la prima a sinistra deve essere (0)
415900111028     c                   movel     vabrmn        uno0              1 0
416000111028     c                   if        uno0 = 0
416100111028      *
416200111028      *   poi la seconda da sinistra deve NON essere (0)
416300111028     c                   movel     vabrmn        due00             2 0
416400111028     c                   move      due00         uno0
416500111028     c                   if        uno0 > 0
416600111028      *
416700111028     c                   exsr      ctrl_seERAexp
416800111028      *
416900111028     c                   if           Sped_RESO = 'S'
417000111028     c                   exsr      Write_RESO
417100111028     c                   end
417200111028      *
417300111028     c                   end
417400111028     c                   end
417500111028     c                   end
417600110623      *
417700120614      *?  PARTICOLARITA'
417800120614      * x Consegna ai PIANI "FLOOR" codice "FLR"
417900120614     c                   if        FTX_ai_piani = 'P'
418000120614     c                   If         VABTC1 = *blank
418100120614     C                   MOVEL     'P'           VABTC1
418200120614     c                   elseIF     VABTC2 = *blank
418300120614     C                   MOVEL     'P'           VABTC2
418400120614     C                   END
418500120614     c                   End
418600100921      *
418700101027      *? ------------------------------------------------------------------------
418800101027      *?  PARTICOLARITA' sui Clienti Partners x scrittura VAB  (C H I O D I)
418900101027      *? ------------------------------------------------------------------------
419000101027     C*
419100101027     c                   exsr      Chiodi_VAB
419200101027     C*
419300090603      *
419400090211      *? ------------------------------------------------------------------------
419500090213     C   30              WRITE     EDiVAB00                             99
419600090213     C  N30              UPDATE    EDiVAB00                             99
419700090211      *? ------------------------------------------------------------------------
419800090213     c   99              eval      errori_in_Wrt = 'S'
419900090211     C*
420000090211     c                   clear                   wri_vabtic        1
420100090211     C*
420200090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
420300100706     C                   EXSR      Scrive_Riferim
420400090211     C*
420500090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
420600090211     C     §1BF12        IFEQ      'E'
420700100706     C                   EXSR      Scrive_VAT
420800090211     C                   ELSE
420900090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
421000090211     C     §1BFG7        IFEQ      'S'
421100090211     C     §1BFG1        OREQ      'S'
421200100706     C                   EXSR      Scrive_VAD
421300090211     C                   END
421400090211     C                   END
421500090211      *
421600090211     C* Reimposto codice trattamento merce cliente
421700100628     C                   MOVEL     sav_DS1B      DS1B
421800090211     C**
421900090211     C* Do errore se previsto CMR ma non c'è
422000090211     C     §PTCMR        IFNE      ' '
422100090211     C                   EXSR      MEMCMR
422200090211     C                   END
422300090211     C* Do errore se previsto AFB ma non c'è
422400090211     C     §PTNFV        IFNE      ' '
422500090211     C                   EXSR      MEMAFB
422600090211     C                   END
422700090211     C*  Se il cliente vuole il ritorno delle date di consegna
422800090211     C*  è c'è una squdratura fra i segnacolli e il numero
422900090211     C*  dei colli che ci ha passato scrivo EDSUM
423000100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
423100090211     C                   EXSR      WRTSUM
423200090211     C                   END
423300090211     C*
423400090211     C                   ENDSR
423500090211     C*----------------------------------------------------------------
423600090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
423700090211     C*----------------------------------------------------------------
423800100630     C     CTRsegn_BAR   BEGSR
423900090211     C*
424000090211     C                   MOVEL     'N'           WNSGER            1
424100090211     C*
424200090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
424300100701     C     VABNCL        IFNE      Conta_Segn
424400090211     C                   MOVEL     'S'           WNSGER
424500100630     C                   eval       Squadratura_Colli = 'S'
424600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
424700090211     c                              bollettati'
424800090211     C                   GOTO      FINSGN
424900090211     C                   END
425000100706      *
425100090211     C*  Riordino i segnacolli
425200090211     C     §1BFG7        IFEQ      'N'
425300100630     C                   SORTA     segn_BAR
425400090211     C*  Controllo se sono sequenziali
425500090211     C                   MOVEL     'N'           WNOSEQ            1
425600100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
425700100706     C*
425800100701     C     2             DO        Conta_Segn    X
425900100630     C     segn_BAR(X)   IFNE      WEXSGN
426000090211     C                   MOVEL     'S'           WNOSEQ
426100090211     C                   END
426200100630     C     segn_BAR(X)   ADD       1             WEXSGN
426300090211     C                   END
426400100706     C*
426500090211     C                   END
426600090211     C*
426700090211     C     FINSGN        ENDSR
426800090211     C*----------------------------------------------------------------
426900090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
427000090211     C*----------------------------------------------------------------
427100100630     C     CTRsegn_EEX   BEGSR
427200090211     C*
427300090211     C                   MOVEL     'N'           WNSGER            1
427400090211      *
427500090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
427600090211      *
427700100701     C     VABNCL        IFNE      totale_PCI
427800090211     C                   MOVEL     'S'           WNSGER
427900100630     C                   eval       Squadratura_Colli = 'S'
428000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
428100090211     c                              bollettati'
428200090211     C                   END
428300090211      *
428400090211     C                   ENDSR
428500090211     C*----------------------------------------------------------------
428600090211     C*? ESEGUO SCRITTURA EDiVAD
428700090211     C*----------------------------------------------------------------
428800100706     C     Scrive_VAD    BEGSR
428900090211     C*
429000100701     C* Se non seq. scrivo i record
429100090211     C     §1BFG1        IFEQ      'S'
429200100701     C     Conta_Segn    ANDNE     *ZEROS
429300090211     C*
429400090211     C     §1BFG7        IFEQ      'S'
429500100701     C                   DO        Conta_Segn    X
429600090211     C                   Z-ADD     VABCCM        KCCM
429700100630     C                   Z-ADD     segn_BAR(X)   KNCD
429800090211     C                   Z-ADD     VABCNT        VADCNT
429900090211     C                   MOVEL     VABCMR        VADCMR
430000090211     C                   Z-ADD     VABAAS        VADAAS
430100090211     C                   Z-ADD     VABLNP        VADLNP
430200090211     C                   Z-ADD     VABNRS        VADNRS
430300090211     C                   Z-ADD     VABNSP        VADNSP
430400090211     C                   Z-ADD     1             VADNCL
430500100630     C                   MOVEL     segn_BAR(X)   VADCDP
430600100630     C                   Z-ADD     segn_BAR(X)   VADNCD
430700090211     C                   Z-ADD     *ZEROS        VADNCA
430800090211     C                   Z-ADD     VABCCM        VADCCM
430900090211     C                   movel     VABfgs        VADfgs
431000090213     C                   WRITE     EDiVAD00                             99
431100090213     c   99              eval      errori_in_Wrt = 'S'
431200090211     C                   END
431300090211     C* Se sequenziali scrivo un solo record
431400090211     C                   ELSE
431500090211     C                   Z-ADD     VABAAS        VADAAS
431600090211     C                   Z-ADD     VABLNP        VADLNP
431700090211     C                   Z-ADD     VABNRS        VADNRS
431800090211     C                   Z-ADD     VABNSP        VADNSP
431900090211     C                   Z-ADD     VABNCL        VADNCL
432000100630     C                   Z-ADD     segn_BAR(1)   VADNCD
432100100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
432200090211     C                   Z-ADD     VABCCM        VADCCM
432300090211     C                   movel     VABfgs        VADfgs
432400090213     C                   WRITE     EDiVAD00                             99
432500090213     c   99              eval      errori_in_Wrt = 'S'
432600090211     C                   END
432700090211     C*
432800090211     C                   ELSE
432900100701     C                   DO        Conta_Segn    X
433000090211     C                   Z-ADD     VABCCM        KCCM
433100100630     C                   Z-ADD     segn_BAR(X)   KNCD
433200090211     C                   Z-ADD     VABAAS        VADAAS
433300090211     C                   Z-ADD     VABLNP        VADLNP
433400090211     C                   Z-ADD     VABNRS        VADNRS
433500090211     C                   Z-ADD     VABNSP        VADNSP
433600100630     C                   MOVEL     segn_BAR(X)   VADCDP
433700090211     C                   Z-ADD     1             VADNCL
433800090211     C                   Z-ADD     *ZEROS        VADNCD
433900090211     C                   Z-ADD     *ZEROS        VADNCA
434000090211     C                   Z-ADD     VABCCM        VADCCM
434100090211     C                   movel     VABfgs        VADfgs
434200090213     C                   WRITE     EDiVAD00                             99
434300090213     c   99              eval      errori_in_Wrt = 'S'
434400090211     C                   END
434500090211     C*
434600090211     C                   END
434700090211     C*
434800090211     C                   ENDSR
434900090211     C*----------------------------------------------------------------
435000090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
435100090211     C*----------------------------------------------------------------
435200100706     C     Scrive_RiferimBEGSR
435300090211      *
435400090211      *  riferimento di consegna destinatario
435500100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
435600100701     c                             CTA_destinatario_in_testata   <> *blank
435700090211     C                   Z-ADD     VABCNT        VATCNT
435800090211     C                   MOVEL     VABCMR        VATCMR
435900090211     C                   Z-ADD     VABCCM        VATCCM
436000090211     C                   movel     VABfgs        VATfgs
436100090211     C                   Z-ADD     VABLNP        VATLNP
436200090211     C                   Z-ADD     VABAAS        VATAAS
436300090211     C                   Z-ADD     VABNRS        VATNRS
436400090211     C                   Z-ADD     VABNSP        VATNSP
436500090211     C                   MOVEL     'A'           VATTRC
436600100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
436700100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
436800100701     c                   else
436900100701     C                   eval       VATNOT = CTA_destinatario_in_testata
437000100701     c                   end
437100090213     C                   WRITE     EDiVAT00                             99
437200090213     c   99              eval      errori_in_Wrt = 'S'
437300090211     c                   end
437400090211      *
437500090211      *  riferimento di consegna telefonico
437600100701     c                   if        COM_telefono_in_dettaglio <> *blank or
437700100701     c                             COM_telefono_in_testata   <> *blank
437800090211     C                   Z-ADD     VABCNT        VATCNT
437900090211     C                   MOVEL     VABCMR        VATCMR
438000090211     C                   Z-ADD     VABCCM        VATCCM
438100090211     C                   movel     VABfgs        VATfgs
438200090211     C                   Z-ADD     VABLNP        VATLNP
438300090211     C                   Z-ADD     VABAAS        VATAAS
438400090211     C                   Z-ADD     VABNRS        VATNRS
438500090211     C                   Z-ADD     VABNSP        VATNSP
438600090211     C                   MOVEL     'B'           VATTRC
438700100701     c                   if        COM_telefono_in_dettaglio <> *blank
438800100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
438900100701     c                   else
439000100701     c                   eval      VATNOT = COM_telefono_in_testata
439100100701     c                   end
439200090213     C                   WRITE     EDiVAT00                             99
439300090213     c   99              eval      errori_in_Wrt = 'S'
439400090211     c                   end
439500090211      *
439600090211      * riferimento del Partner che una volta veniva riportato sul BL4
439700100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
439800100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
439900100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
440000100119      *
440100100701     c                   if        RFF_RifSpediz <> *blank
440200090211     C                   Z-ADD     VABCNT        VATCNT
440300090211     C                   MOVEL     VABCMR        VATCMR
440400090211     C                   Z-ADD     VABCCM        VATCCM
440500090211     C                   movel     VABfgs        VATfgs
440600090211     C                   Z-ADD     VABLNP        VATLNP
440700090211     C                   Z-ADD     VABAAS        VATAAS
440800090211     C                   Z-ADD     VABNRS        VATNRS
440900090211     C                   Z-ADD     VABNSP        VATNSP
441000090211     C                   MOVEL     'C'           VATTRC
441100100701     C                   MOVEL     RFF_RifSpediz VATNOT
441200090213     C                   WRITE     EDiVAT00                             99
441300090213     c   99              eval      errori_in_Wrt = 'S'
441400090211     c                   end
441500090211      *
441600110907      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
441700110907      *   sulla consegna della sua merce
441800110907      *
441900110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
442000110907     c                                  <> *blank
442100110907      *
442200110907     C                   Z-ADD     VABCNT        VATCNT
442300110907     C                   MOVEL     VABCMR        VATCMR
442400110907     C                   Z-ADD     VABCCM        VATCCM
442500110907     C                   movel     VABfgs        VATfgs
442600110907     C                   Z-ADD     VABLNP        VATLNP
442700110907     C                   Z-ADD     VABAAS        VATAAS
442800110907     C                   Z-ADD     VABNRS        VATNRS
442900110907     C                   Z-ADD     VABNSP        VATNSP
443000110907     C                   MOVEL     'I'           VATTRC
443100110907     C                   eval       VATNOT =
443200110907     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
443300110907     C                   WRITE     EDiVAT00                             99
443400110907     c   99              eval      errori_in_Wrt = 'S'
443500110907     c                   end
443600110907      *
443700110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
443800110907     c                                 <> *blank
443900110907      *
444000110907     C                   Z-ADD     VABCNT        VATCNT
444100110907     C                   MOVEL     VABCMR        VATCMR
444200110907     C                   Z-ADD     VABCCM        VATCCM
444300110907     C                   movel     VABfgs        VATfgs
444400110907     C                   Z-ADD     VABLNP        VATLNP
444500110907     C                   Z-ADD     VABAAS        VATAAS
444600110907     C                   Z-ADD     VABNRS        VATNRS
444700110907     C                   Z-ADD     VABNSP        VATNSP
444800110907     C                   MOVEL     'J'           VATTRC
444900110907     C                   eval       VATNOT =
445000110907     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
445100110907     C                   WRITE     EDiVAT00                             99
445200110907     c   99              eval      errori_in_Wrt = 'S'
445300110907     c                   end
445400110907      *
445500131025      *
445600131025      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
445700131025      *   sulla consegna della sua merce
445800131025      *
445900131025     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
446000131025     c                                  <> *blank
446100131025     c                             or FTX_MAIL_come_servizio = 'N'
446200131025      *
446300131025     C                   Z-ADD     VABCNT        VATCNT
446400131025     C                   MOVEL     VABCMR        VATCMR
446500131025     C                   Z-ADD     VABCCM        VATCCM
446600131025     C                   movel     VABfgs        VATfgs
446700131025     C                   Z-ADD     VABLNP        VATLNP
446800131025     C                   Z-ADD     VABAAS        VATAAS
446900131025     C                   Z-ADD     VABNRS        VATNRS
447000131025     C                   Z-ADD     VABNSP        VATNSP
447100131025     C                   MOVEL     'S'           VATTRC
447200131025      *
447300131025      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
447400131025      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
447500131025     C                   eval       VATNOT =
447600131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
447700131025      *
447800131025      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
447900131025      * seguenti attivano o meno il servizio x SMS o MAIL
448000131025      *
448100131025      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
448200131025      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
448300131025     C                   eval      %subst(VATNOT:17:1) = 'S'
448400131025     c                   if        FTX_SMS_come_servizio = 'N'
448500131025     c                              or
448600131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
448700131025     c                              = *blank
448800131025     C                   eval      %subst(VATNOT:17:1) = 'N'
448900131025     c                   end
449000131025      *
449100131025     C                   eval      %subst(VATNOT:18:1) = 'S'
449200131025     c                   if        FTX_MAIL_come_servizio = 'N'
449300131025     c                              or
449400131025     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
449500131025     c                              = *blank
449600131025     C                   eval      %subst(VATNOT:18:1) = 'N'
449700131025     c                   end
449800131025      *
449900131025     C                   WRITE     EDiVAT00                             99
450000131025     c   99              eval      errori_in_Wrt = 'S'
450100131025     c                   end
450200131025      *
450300090211     C                   ENDSR
450400090211     C*----------------------------------------------------------------
450500090211     C*? ESEGUO SCRITTURA EDiVAT
450600090211     C*----------------------------------------------------------------
450700100706     C     Scrive_VAT    BEGSR
450800090211      *
450900100701     C                   DO        totale_PCI    X
451000090211     C                   Z-ADD     VABCNT        VATCNT
451100090211     C                   MOVEL     VABCMR        VATCMR
451200090211     C                   Z-ADD     VABCCM        VATCCM
451300090211     C                   movel     VABfgs        VATfgs
451400090211     C                   Z-ADD     VABLNP        VATLNP
451500090211     C                   Z-ADD     VABAAS        VATAAS
451600090211     C                   Z-ADD     VABNRS        VATNRS
451700090211     C                   Z-ADD     VABNSP        VATNSP
451800090211     C                   MOVEL     'E'           VATTRC
451900090211      *
452000090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
452100090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
452200090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
452300100701     c                   if        PU_lunVAT     > 0
452400090211      *
452500090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
452600100630     C                   eval      VATnot =
452700100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
452800090211      *
452900090211     c                   else
453000090211      *
453100090211      * Se non è impostato nulla prende il segnacollo passato così com'è
453200100630     C                   MOVEL     segn_EEX(X)   VATNOT
453300090211     c                   end
453400090211      *
453500090213     C                   WRITE     EDiVAT00                             99
453600090213     c   99              eval      errori_in_Wrt = 'S'
453700090211     C                   END
453800090211      *
453900090211     C                   ENDSR
454000090211     C*----------------------------------------------------------------
454100090211     C*? ESEGUO SCRITTURA EDSUM
454200090211     C*----------------------------------------------------------------
454300090211     C     WRTSUM        BEGSR
454400090211     C*
454500100701     C* Se non seq. scrivo i   record
454600100701     C                   DO        Conta_Segn    X
454700090211     C                   Z-ADD     VABAAS        SUMAAS
454800090211     C                   Z-ADD     VABLNP        SUMFLS
454900100701     C                   MOVEL     PT_vabCCM     SUMLNP
455000090211     C                   Z-ADD     VABLNA        SUMLNA
455100090211     C                   Z-ADD     VABZNC        SUMZNC
455200090211     C                   Z-ADD     VABNRS        SUMNRS
455300090211     C                   Z-ADD     VABNSP        SUMNSP
455400100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
455500100701     C                   Z-ADD     PT_vabCCM     SUMCCM
455600090211     C                   MOVEL     *BLANKS       SUMSTS
455700090211     C                   MOVEL     *BLANKS       SUMFLG
455800100701     C                   MOVEL     UNH_Rifer_Msg SUMCNI
455900090211     C                   MOVEL     VABCMR        SUMCMR
456000090211     C                   Z-ADD     0             SUMNFE
456100090211     C                   Z-ADD     VABCCM        VADCCM
456200090213     C                   WRITE     EDSUM000                             99
456300090213     c   99              eval      errori_in_Wrt = 'S'
456400090211     C                   END
456500090211     C*
456600090211     C                   ENDSR
456700090211     c*==================================================================*
456800090211      * Manda un Msg in Posta AS Sede a EDP*
456900090211     c*==================================================================*
457000090211     c     SEND_MSG_EDP  begsr
457100090211      *
457200090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
457300090211      *   Lo manda una sola volta per lavoro
457400090211     c                   IF        Snd_Msg_alert  <> 'N' and
457500090211     c                             User_msg <> *blank
457600090211     c                   z-add     1             Jx
457700090211     C                   exsr      CalQEZSNDMG
457800090211     c                   end
457900090211      *
458000090211     c                   endsr
458100090213     c**********************************************************************
458200090213     c* reperimento numero Spedizione
458300090213     c**********************************************************************
458400090213     C     repnum        BEGSR
458500090213      *
458600090213      *    deve controllare sulla tabella "3C" se il numero spedizione
458700090213      *     deve essere mantenuto oppure no
458800090213     C*
458900090213     C                   clear                   Ds3C
459000090213     C                   Z-ADD     CODUT         TBLKUT
459100090213     C                   MOVEL     '3C'          TBLCOD
459200090213     C                   movel(p)  VABccm        TBLKEY
459300090213     C     KTABel        CHAIN     TABEL00F                           30
459400090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
459500090213     C                   MOVEL     TBLUNI        Ds3C
459600090213     C                   END
459700090213      *
459800090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
459900090213     c                   if        §3CFsp <> 'S'
460000090213      *
460100090213     C* NSP => Stacco un numeratore da AZNUM
460200090213     c                   movel     kpjbu         svkpjbu
460300090213     c                   clear                   kpjbu
460400090213     C                   clear                   TRUL33DS
460500090213     C                   eval      I33OPE = *zeros
460600090213     C                   eval      I33CNU = 302
460700090213     C                   eval      I33NUM = 1
460800090213     C                   movel     TRUL33DS      KPJBU
460900090213     C                   call      'TRUL33R'
461000090213     C                   parm                    KPJBA
461100090213     C                   movel     KPJBU         TRUL33DS
461200090213     c                   movel     svkpjbu       kpjbu
461300090213      ****
461400090213     C                   if        O33ERR = *zeros
461500100630     c                   z-add     o33nrf        NUM_Spediz
461600090213     C                   else
461700090213     C                   endif
461800090213      ****
461900090213     c                   ELSE
462000090213      *   se si vuole mantenere il numero spedizione
462100090213      ****
462200090213     C                   MOVEL     WOGGI         WANNO             4 0
462300090213     C                   MOVE      WANNO         KAAA
462400090213     C                   Z-ADD     3             KCNU
462500090213     C                   Z-ADD     VABlnp        KFIL
462600090213     C     KNUF          CHAIN     FLNUF                              9999
462700090213     C     *IN99         IFEQ      '0'
462800090213     C                   ADD       1             NUFNUM
462900100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
463000090213     C                   UPDATE    FLNUF
463100090213     C                   ELSE
463200090213     C                   END
463300090213     C*
463400090213     c                   EndIF
463500090213      **
463600090213     C                   ENDSR
463700090213     C*----------------------------------------------------------------
463800090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
463900090213     C*----------------------------------------------------------------
464000090213     C     MEMCMR        BEGSR
464100090213     C*
464200090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
464300090213     C     §PTCM1        IFEQ      'S'
464400090213     C                   CLEAR                   EDRDE000
464500090213     C                   MOVEL     'TD1154'      KNMC
464600090213     C                   Z-ADD     1             KNRP
464700100701     C                   MOVEL     PT_vabCCM     KLNP1
464800090213     C     KRDE          CHAIN     EDRDE01L                           31
464900090213     C     *IN31         IFEQ      '1'
465000090213     C                   Z-ADD     VABAAS        RDEAAS
465100090213     C                   Z-ADD     VABLNP        RDELNP
465200090213     C                   Z-ADD     VABNRS        RDENRS
465300090213     C                   Z-ADD     VABNSP        RDENSP
465400090213     C                   MOVEL     'TD1154'      RDENMC
465500090213     C                   Z-ADD     1             RDENRP
465600100706     C                   MOVEL     BGM_nrCMR     RDEVAL
465700100701     c                   if          RFF_NrCMR <> *Blank
465800100701     C                   MOVEL     RFF_NrCMR     RDEVAL
465900100701     c                   end
466000090213     C                   WRITE     EDRDE000                             99
466100090213     c   99              eval      errori_in_Wrt = 'S'
466200090213     C                   END
466300090213     C*  Memorizzo qualificatore CMR
466400090213     C                   CLEAR                   EDRDE000
466500090213     C                   MOVEL     'TD1153'      KNMC
466600090213     C                   Z-ADD     1             KNRP
466700100701     C                   MOVEL     PT_vabCCM     KLNP1
466800090213     C     KRDE          CHAIN     EDRDE01L                           31
466900090213     C     *IN31         IFEQ      '1'
467000090213     C                   Z-ADD     VABAAS        RDEAAS
467100090213     C                   Z-ADD     VABLNP        RDELNP
467200090213     C                   Z-ADD     VABNRS        RDENRS
467300090213     C                   Z-ADD     VABNSP        RDENSP
467400090213     C                   Z-ADD     1             RDENRP
467500090213     C                   MOVEL     'TD1153'      RDENMC
467600090213     C                   MOVEL     'CMR'         RDEVAL
467700090213     C                   WRITE     EDRDE000                             99
467800090213     c   99              eval      errori_in_Wrt = 'S'
467900090213     C                   END
468000090213     C*  Memorizzo la data
468100090213     C                   CLEAR                   EDRDE000
468200090213     C                   MOVEL     'TD2380'      KNMC
468300090213     C                   Z-ADD     1             KNRP
468400100701     C                   MOVEL     PT_vabCCM     KLNP1
468500090213     C     KRDE          CHAIN     EDRDE01L                           31
468600090213     C     *IN31         IFEQ      '1'
468700090213     C                   Z-ADD     VABAAS        RDEAAS
468800090213     C                   Z-ADD     VABLNP        RDELNP
468900090213     C                   Z-ADD     VABNRS        RDENRS
469000090213     C                   Z-ADD     VABNSP        RDENSP
469100090213     C                   Z-ADD     1             RDENRP
469200090213     C                   MOVEL     'TD2380'      RDENMC
469300100630     C                   MOVEL     DTM_DtCMR     RDEVAL
469400090213     C                   WRITE     EDRDE000                             99
469500090213     c   99              eval      errori_in_Wrt = 'S'
469600090213     C                   END
469700090213     C*
469800090213     C                   END                                                    §PTCM1 = 'S'
469900090213     C*
470000090213     C                   ENDSR
470100090213     C*----------------------------------------------------------------
470200090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
470300090213     C*----------------------------------------------------------------
470400090213     C     MEMAFB        BEGSR
470500090213     C*
470600090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
470700090213     C     §PTNF1        IFEQ      'S'
470800090213     C                   CLEAR                   EDRDE000
470900100701     C                   MOVEL     PT_vabCCM     KLNP1
471000090213     C                   MOVEL     'TD1154'      KNMC
471100090213     C                   Z-ADD     1             KNRP
471200090213     C     KRDE          CHAIN     EDRDE01L                           31
471300090213     C     *IN31         IFEQ      '1'
471400090213     C                   Z-ADD     VABAAS        RDEAAS
471500090213     C                   Z-ADD     VABLNP        RDELNP
471600090213     C                   Z-ADD     VABNRS        RDENRS
471700090213     C                   Z-ADD     VABNSP        RDENSP
471800090213     C                   MOVEL     'TD1154'      RDENMC
471900100701     C                   if        §PTCM1 <> 'S' or
472000100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
472100090213     C                   Z-ADD     1             RDENRP
472200090213     C                   ELSE
472300090213     C                   Z-ADD     2             RDENRP
472400090213     C                   END
472500100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
472600100701     c                   if          RFF_NrFviaggio <> *Blank
472700100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
472800100701     c                   end
472900090213     C                   WRITE     EDRDE000                             99
473000090213     c   99              eval      errori_in_Wrt = 'S'
473100090213     C                   END
473200090213     C*  Memorizzo qualificatore CMR
473300090213     C                   CLEAR                   EDRDE000
473400100701     C                   MOVEL     PT_vabCCM     KLNP1
473500090213     C                   MOVEL     'TD1153'      KNMC
473600090213     C                   Z-ADD     1             KNRP
473700090213     C     KRDE          CHAIN     EDRDE01L                           31
473800090213     C     *IN31         IFEQ      '1'
473900090213     C                   Z-ADD     VABAAS        RDEAAS
474000090213     C                   Z-ADD     VABLNP        RDELNP
474100090213     C                   Z-ADD     VABNRS        RDENRS
474200090213     C                   Z-ADD     VABNSP        RDENSP
474300100701     C                   if        §PTCM1 <> 'S' or
474400100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
474500090213     C                   Z-ADD     1             RDENRP
474600090213     C                   ELSE
474700090213     C                   Z-ADD     2             RDENRP
474800090213     C                   END
474900090213     C                   MOVEL     'TD1153'      RDENMC
475000090213     C                   MOVEL     'AFB'         RDEVAL
475100090213     C                   WRITE     EDRDE000                             99
475200090213     c   99              eval      errori_in_Wrt = 'S'
475300090213     C                   END
475400090213     C*  Memorizzo la data
475500090213     C                   CLEAR                   EDRDE000
475600100701     C                   MOVEL     PT_vabCCM     KLNP1
475700090213     C                   MOVEL     'TD2380'      KNMC
475800090213     C                   Z-ADD     1             KNRP
475900090213     C     KRDE          CHAIN     EDRDE01L                           31
476000090213     C     *IN31         IFEQ      '1'
476100090213     C                   Z-ADD     VABAAS        RDEAAS
476200090213     C                   Z-ADD     VABLNP        RDELNP
476300090213     C                   Z-ADD     VABNRS        RDENRS
476400090213     C                   Z-ADD     VABNSP        RDENSP
476500100701     C                   if        §PTCM1 <> 'S' or
476600100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
476700090213     C                   Z-ADD     1             RDENRP
476800090213     C                   ELSE
476900090213     C                   Z-ADD     2             RDENRP
477000090213     C                   END
477100090213     C                   MOVEL     'TD2380'      RDENMC
477200100630     C                   MOVEL     DTM_DtFViag   RDEVAL
477300090213     C                   WRITE     EDRDE000                             99
477400090213     c   99              eval      errori_in_Wrt = 'S'
477500090213     C                   END
477600090213     C*
477700090213     C                   END                                                    §PTCM1 = 'S'
477800090213     C*
477900090213     C                   ENDSR
478000090213     c*==================================================================*
478100090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
478200090213     c*==================================================================*
478300090213     c     Imp_ADDR_mail begsr
478400090213      *
478500090213     c                   clear                   Indirizzi
478600090213      *
478700090213      *  Lunghezza indirizzi presenti
478800100630     c                   eval      Lunghezza_Indirizzo =
478900100630     c                                       %Len(%TrimR(Mail_Ind))
479000090213     c                   z-add     1             al_char           3 0
479100090213     c                   z-add     1             dal_char          3 0
479200090213     c                   z-add     1             tot_char          3 0
479300090213      *
479400090213     c     successivo    tag
479500090213      *
479600090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
479700090213      *   e aggiunge il dominio Bartolini + il (;) separatore
479800090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
479900090213     c                   eval      tot_char = al_char - dal_char
480000100630     c                   z-add     dal_char      dalCarattere
480100100630     c                   z-add     tot_char      xTOTcaratteri
480200090213      *
480300090213     c                   clear                   Indir_Bartol     40
480400090213     c                   clear                   Indir_Parz       20
480500100630     c                   eval      Indir_Parz =
480600100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
480700100630      *
480800090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
480900090213     c                             %Trim(bart_it)
481000090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
481100090213      *
481200100630     c                   if        al_char = Lunghezza_Indirizzo
481300090213     c                   goto      fine_indmail
481400090213     c                   else
481500090213     c                   eval      dal_char = ( al_char + 1 )
481600090213     c                   goto      successivo
481700090213     c                   end
481800090213      *
481900090213     C     fine_indmail  TAG
482000090213      *
482100090213     C                   ENDSR
482200090213     c*==================================================================*
482300090213      * Manda un Msg x E-mail
482400090213     c*==================================================================*
482500090213     c     Invio_Mail    begsr
482600090213      *
482700090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
482800090213     C* Inizializzo variabili
482900090213     C                   movel     *blanks       wrkEml          253
483000090213     C                   movel     *blanks       wrkMsg         5000
483100090213     C                   movel     *blanks       wrkOgg           44
483200090213      *
483300090213     C* Valorizzo i campi della e-m@ail - indirizzo
483400100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
483500090213     C                   eval      wrkEml = Indirizzi
483600090213     C                   eval      wrkOgg = Oggetto
483700090213     C                   eval      wrkMsg = Messaggio
483800110203      ********
483900110203     C********           call(e)   'TIS701C'
484000110203     C********           parm                    wrkEml
484100110203     C********           parm                    wrkOgg
484200110203     C********           parm                    wrkMsg
484300110203     C*
484400110203      ** *****
484500110203     C                   call(e)   'TRTCT00R2'
484600110203     C                   parm                    wrkEml
484700110203     C                   parm                    wrkOgg
484800110203     C                   parm                    wrkMsg
484900110203     C*
485000090213     C*
485100090213     C                   ENDSR
485200090603      *
485300090211     ***********************************************************************
485400090211     **
485500090211     ** Send Message (QEZSNDMG) API
485600090211     **
485700090211     ***********************************************************************
485800090211     C     CalQEZSNDMG   BEGSR
485900090211      *
486000090211     ** Invio messaggio all'utente.
486100090211     C                   EVAL      SndMg03 = msgDSP
486200090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
486300090211     C                   EVAL      SndMg05(Jx) = User_msg
486400090211     C                   EVAL      SndMg06 = Jx
486500090211     C                   CLEAR                   QUSEC
486600090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
486700090211
486800090211     C                   CALL      'QEZSNDMG'
486900090211     C                   PARM                    SndMg01
487000090211     C                   PARM                    SndMg02
487100090211     C                   PARM                    SndMg03
487200090211     C                   PARM                    SndMg04
487300090211     C                   PARM                    SndMg05
487400090211     C                   PARM                    SndMg06
487500090211     C                   PARM                    SndMg07
487600090211     C                   PARM                    SndMg08
487700090211     C                   PARM                    Qusec
487800090211     C                   PARM                    SndMg10
487900090211     C                   PARM                    SndMg11
488000090211     C                   PARM                    SndMg12
488100090211      *
488200090211     C                   ENDSR
488300100708      * ?------------------------------------------------------------------ */
488400100708      *  Controlla la trasmissione   se completa
488500100708      * ?------------------------------------------------------------------ */
488600100708     c     Check_Trasm   Begsr
488700100708
488800100708     C                   clear                   se_errore
488900100708     C                   clear                   Riga_iNIZIo       1
489000100708     C                   clear                   Riga_fINe         1
489100100708      ** primo  record
489200100708     c     *start        setll     tivin00r
489300100708      *
489400100708     c     rileggi       tag
489500100708     c                   EXSR      LEGGE_TIVIN_R
489600100708      *
489700100708     c                   if        EoFTivin <> 'S'
489800100708      *
489900100708     c                   if        dati = *blank
490000100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
490100100708      * le leggo e le escludo.
490200100708     C                   eval      vinFLG = '1'
490300100708     c                   update    Tivin000
490400100708     c                   goto      rileggi
490500100708     c                   end
490600100708      * ?              /*---------------------- */
490700100708     c                   exsr      Decod_Segmento
490800100708      * ?              /*---------------------- */
490900100708     c                   endif
491000100708      **
491100100708      ** ultimo record
491200100708     c     *hival        setll     tivin00r
491300100708     c     leggiAncora   tag
491400100708     c                   readp     tivin00r
491500100708     c                   if        %Eof(tivin00r)
491600100708     c                   move      'S'           EoFTivin
491700100708     c                   else
491800100708     c                   clear                   dati
491900100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
492000100708      *
492100100708     c                   if        dati = *blank
492200100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
492300100708      * le leggo e le escludo.
492400100708     C                   eval      vinFLG = '1'
492500100708     c                   update    Tivin000
492600100708     c                   goto      leggiAncora
492700100708     c                   end
492800100708      * ?              /*---------------------- */
492900100708     c                   exsr      Decod_Segmento
493000100708      * ?              /*---------------------- */
493100100708     c                   endif
493200100708
493300100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
493400100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
493500100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
493600100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
493700100708      * ?-----> Errore
493800100708     C                   eval      se_errore = 'S'
493900100708     c                   end
494000100708
494100100708     c                   endSR
494200111017      * ?------------------------------------------------------------------ */
494300111017     C*? CHIODI VAB  prima della scrittura del VAB Particolarità su Clienti/Partners
494400111017      * ?------------------------------------------------------------------ */
494500111017     C     Chiodi_VAB    BEGSR
494600111017      *
494700101027      *? ------------------------------------------------------------------------
494800101027      *? ATTENZIONE: (Chiodo E.Arden)
494900101027      *?  x E.Arden o Unilever           501790 e 501791
495000101027      *? Se è un cliente e non un Partner deve togliere la Ragione Sociale
495100101027      *? originale e mettere un punto al posto della RagSoc poichè non
495200101027      *?  deve essere vuoto il campo.
495300101027      *? ------------------------------------------------------------------------
495400101027     C     VABRMO        IFne      *blank
495500110913     C     §PUTPC        andne     'P'
495600110913     C**** §PUTPC        andeq     'C'
495700101027     C     §PTKSC        ifeq      501790
495800101027     C     §PTKSC        oreq      501791
495900101027     C                   MOVEL     *BLANKS       VABRMO
496000101027     C                   MOVEL     '.'           VABRMO
496100101027     c                   end
496200101027     C                   END
496300101027      *
496400101027      *? ------------------------------------------------------------------------
496500101027      *? Chiodo da togliere a gennaio 2005 :
496600101027      *? attenzione solo x ARVATO non deve essere importato l'IMPORTO ASSICURATO
496700101027      *? come da accordi commerciali anche se lo inviano sul tracciato EDI
496800101027      *? ED ANCHE IL VOLUME dal 1/3/2005
496900101027      *? ------------------------------------------------------------------------
497000101027     C     §PTKSC        ifeq      932337
497100101027     C     VABCCM        oreq      52981
497200101027     C     VABCCM        oreq      932353
497300101027     c     VABCCM        oreq      932461
497400101027     C                   clear                   VABIAS                         IMPORTO
497500101027     C                   clear                   VABVAS                         ASSICURATO
497600101027     C                   clear                   VABVLB                         VOLUME BOLLETTATO
497700101027     c                   end
497800101027     C*
497900101027      *? ------------------------------------------------------------------------
498000101027      *? Chiodo x SFS come da richiesta del 25/5/2007:
498100101027      *? mettere sui 2 campi note:
498200101027      *?      not: DDT SUL COLLO
498300101027      *?      nt2: NON CONSEGNARE IN CASO DI AVARIA
498400101027      *? ------------------------------------------------------------------------
498500101027     C     §PTKSC        ifeq      1160950
498600101027     C     VABNOT        andeq     *blank
498700101027     C     VABNT2        andeq     *blank
498800101027     C                   eval      vabNOT = 'DDT SUL COLLO'
498900101027     C                   eval      vabNT2 = 'NON CONSEGNARE IN CASO DI AVARIA'
499000101027     c                   end
499100101027     C*
499200101027      *? ------------------------------------------------------------------------
499300101027      *? Chiodo x SEUR cliente REBELIO  del 24/9/2007:
499400101027      *?  il cliente se ha il COD vuole sempre in contanti mentre
499500101027      *?   su IFCSUM manda sempre il FTX+PAI+++CO' che noi tradurremmo come OM.
499600101027      *? Chiodo Richiesto dalla Villa con Mail il 24/9/2007
499700101027      *? ------------------------------------------------------------------------
499800101027     C     §PTKSC        ifeq      433048
499900101027     C     VABCCM        andeq     433972
500000101027     C     VABTIC        andne     *blank
500100101027     C                   eval      vabTIC = '  '
500200101027     c                   end
500300101027     C*
500400101027     C*  Per non bloccare la bollettazione
500500101027     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
500600101027     C*  o se il peso non è presente... imposto fisso il minimo 0,1
500700130315     c**********         if        vabpkb = 0
500800130315     c**********         z-add     0,1           vabpkb
500900130315     c**********         end
501000101027     C*
501100101027     C*
501200101027      *? ------------------------------------------------------------------------
501300101027      *? Chiodo x AZKAR BISA richiesta  del 01/4/2009: dalla VILLA con mail di Azkar
501400101027      *?  tutti i contrassegni devono essere intestati come "OM"
501500101027      *? Chiodo Richiesto dalla Villa con Mail il 01/4/2009
501600101027      *? ------------------------------------------------------------------------
501700101027     C     §PTKSC        ifeq      22349
501800101027     C     VABCCM        andeq     22349
501900101027     C     VABTIC        ifne      *blank
502000101027     C     VABCAS        orgt      *zeros
502100101027     C                   eval      vabTIC = 'OM'
502200101027     c                   end
502300101027     c                   end
502400110222     C*
502500110627     C*
502600110222      *? ------------------------------------------------------------------------
502700110222      *? Chiodo x Calberson Maison du Monde codice 0040712 le spedizioni devono
502800110222      *?  essere prese TUTTE CON APPUNTAMENTO - richiesta di Giorgio dal Vivo
502900110222      *? La Marca Alfredo e Carrozzo William x conoscenza del 22/2/2011
503000110222      *? ------------------------------------------------------------------------
503100110222     C     VABCCM        ifeq      40712
503200110222     c                   if        vabtc1 = *blank
503300110222     c                   eval      vabtc1 = 'A'
503400110222     c                   elseif    vabtc2 = *blank
503500110222     c                   eval      vabtc2 = 'A'
503600110222     c                   end
503700110222     c                   end
503800101027     C*
503900110509     C*
504000110509      *? ------------------------------------------------------------------------
504100110509      *? Chiodo x GEODIS MAR x i COD devono essere tutti come "OM" solo per 40566
504200110509      *? ------------------------------------------------------------------------
504300110509     C     VABCCM        ifeq      40566
504400110509     c                   if        vabcas > 0
504500110509     c                   eval      vabtic = 'A'
504600110509     c                   end
504700110509     c                   end
504800110509     C*
504900101027     C                   ENDSR
505000111027      * ?------------------------------------------------------------------ */
505100111027     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
505200111027      * ?------------------------------------------------------------------ */
505300111027     C     Write_RESO    BEGSR
505400111027      *
505500111028      *  Imposta nella descrizione del CMR l'intestazione di RESI
505600111028     c                   movel(p)  vabCMR        campo35          35
505700111028     c                   movel(p)  'RESI__'      vabCMR
505800111028     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
505900111028      *
506000111028      *  e sulla 2° parte della ragione sociale viene apposta la dicitura di RESO
506100111028     c                   move(p)   VABrsd        campo44          44
506200111028     c                   movel     '**RESO** '   campo44
506300111028     C                   movel     campo44       VABrsd
506400111028     C                   move      campo44       campo9            9
506500111028      *
506600111028      * Sposta tutta la ragione sociale di (9 carat.) anche sulla 2°ragione sociale
506700111028     c                   move(p)   VABrd2        campo44          44
506800111028     c                   movel     campo9        campo44
506900111028     C                   movel     campo44       VABrd2
507000111028      *
507100111027     c                   endSR
507200100708      * ?------------------------------------------------------------------ */
507300111027     C*? Controllo se era prima una Bolla di Export che sta rientrando
507400111027      * ?------------------------------------------------------------------ */
507500111027     C     ctrl_seERAexp Begsr
507600111027     C*
507700111027     C*   imposta la chiave per controllare la spedizione
507800111027     C*    in sede
507900111027     C                   move      VABRMN        dsspe
508000111027     C                   movel     '20'          dsspe
508100111027     C*
508200111027     C                   move      SPEAAS        TASAAS
508300111027     C                   move      SPELNP        TASLNP
508400111027     C                   move      SPENRS        TASNRS
508500111027     C                   move      SPENSP        TASNSP
508600111027     C     KTAS          chain     titas30c
508700111027     c                   if        %found(titas30c)
508800111027     c                               and tasLNA >299 and tasLNA <=400
508900111027     c                   eval      era_un_export_RESO_dal_PARTNER = 'S'
509000111027     c                   eval                           Sped_RESO = 'S'
509100111027     c                   end
509200111027     C*
509300111027     C                   ENDSR
509400120629      * ?------------------------------------------------------------------ */
509500120629     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
509600111027      * ?------------------------------------------------------------------ */
509700120629     C     CL_ParticolarebegSR
509800120629      *
509900120629     C     Rifer_Cliente IFNE      *BLANKS
510000120629      * ------>>>
510100120629     C                   eval      XX = 1
510200120629     C                   movel     §PTKSC        Key_CL           35
510300120629     C                   movel     Rifer_Cliente WCOM28           28
510400120629     C                   move      WCOM28        Key_CL
510500120629      *
510600120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
510700120629     C     *IN34         IFEQ      '1'
510800120629     C                   movel     Des_Tb_CL(XX) EDIDSCL
510900120629      *
511000120629      * in testata messaggio
511100120629     c                   if        Dati_di_Testata  ='S'
511200120629     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
511300120629     C                   movel     §CLtar        Tes_RFF_FF_tar    3
511400120629     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
511500120629     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
511600120629     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
511700120629     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
511800120629     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
511900120629     C                   movel     §CLtic        Tes_RFF_FF_tic    2
512000120629     c                   else
512100120629      * in dettaglio Messaggio
512200120629     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
512300120629     C                   movel     §CLtar        Det_RFF_FF_tar    3
512400120629     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
512500120629     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
512600120629     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
512700120629     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
512800120629     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
512900120629     C                   movel     §CLtic        Det_RFF_FF_tic    2
513000120629     c                   end
513100120629      *
513200120629      * imposta la LNP  e la Serie x il dettaglio specifico
513300120629     c
513400120629      *  In base al cod.trattamento merce reperisco tipo tratt.
513500120629      *    per un dettaglio specifico.
513600120629     C                   CLEAR                   DS1B
513700120629     C                   Z-ADD     1             Y                 3 0
513800120629     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
513900120629     C     *IN32         IFEQ      '1'
514000120629     C                   eval       DS1B = Des_Tb_CTM(Y)
514100120629     C                   END
514200120629      *
514300120629      * Se il cliente non era sulla schiera lo aggiunge
514400120629     C     §CLksc        lookup    skCLienti                              39
514500120629     C                   if        *in39 = *off
514600120629     C                   add       1             sk
514700120629     C                   z-add     §CLksc        skCLienti(sk)
514800120629     c                   endif
514900120629      *
515000120629     C                   ENDIF
515100120629      * ------>>>
515200120629     C                   ENDIF
515300120629      *
515400120629      * in Dettaglio  preimposto i campi rilevati dalle tabelle
515500120629      *   per clienti particolari (CL)
515600120629      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
515700120629      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
515800120629      *   successivi per altre decodifiche.
515900120629     c                   if        Dati_di_Testata <>'S'
516000120629      *
516100120629      *   se aveva un RFF+FF in dettaglio
516200120629     C                   if         Det_RFF_FF_ksc > 0
516300120629      *
516400120629     C                   eval      VABccm = Det_RFF_FF_ksc
516500120629     c                   eval      VABlnp = Det_RFF_FF_lnp
516600120629     c                   eval      VABnrs = Det_RFF_FF_nrs
516700120629     C                   eval      VABctm = Det_RFF_FF_ctm
516800120629     C                   movel     Det_RFF_FF_tarVABctr
516900120629      *
517000120629      *   altrimenti se aveva un RFF+FF in Testata
517100120629     c                   elseif     Tes_RFF_FF_ksc > 0
517200120629      *
517300120629     C                   eval      VABccm = Tes_RFF_FF_ksc
517400120629     c                   eval      VABlnp = Tes_RFF_FF_lnp
517500120629     c                   eval      VABnrs = Tes_RFF_FF_nrs
517600120629     C                   eval      VABctm = Tes_RFF_FF_ctm
517700120629     C                   movel     Tes_RFF_FF_tarVABctr
517800120629      *
517900120629     c                   end
518000120629      *>>>>>>>>
518100120629     c                   ENDif
518200120629     C*
518300120629     C                   ENDSR
518400121105      *---------------------------------------------------------------*
518500121105      * CTRL caricamento schiere    PT                               -*
518600121105      *---------------------------------------------------------------*
518700121105     C     Check_PT      begsr
518800121105     C*
518900121105     c* Controllo riempimento schiera
519000121105     c                   clear                   trul0sds
519100121105     c                   eval      i0sski='PT_key'
519200121105     c                   eval      i0sele=%elem(PT_key)
519300121105     c                   eval      i0spie=x
519400121105     c                   eval      i0sfile='EDTAB00F'
519500121105     c                   eval      i0ssif=knsif
519600121105     c                   eval      i0spgm='TRTCT93R'
519700121112     c                   move      kpjbu         SvKpjbu
519800121112     c                   clear                   kpjbu
519900121105     c                   movel     trul0sds      kpjbu
520000121105     c                   call      'TRUL0SR'
520100121105     c                   parm                    kpjba
520200121112     c                   eval      kpjbu = SvKpjbu
520300121105     C*
520400121105     C                   ENDSR
520500120629      * ?------------------------------------------------------------------ */
520600090211     O*****************************************************************
520700090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
520800090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
520900090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
521000090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
