000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600100811????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFCSUM vers D96A  *
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
002200111028      *
002300111028???? Ftitas30c  IF   E           K DISK
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
008200111028      *----------------------------------------------------*
008300111028      *  DS x scomposizione numero spedizione
008400111028      *----------------------------------------------------*
008500111028     D DSSPE           DS
008600111028     D  SPEAAS                 1      4  0
008700111028     D  SPELNP                 5      7  0
008800111028     D  SPENRS                 8      9  0
008900111028     D  SPENSP                10     16  0
009000111028      **
009100111028     D Sped_RESO       S              1
009200140610      *  qui viene riportato il codice della nostra ex bolla export o del ORM
009300140610      *  mandato dal Partner in un particolare segmento concordato
009400140610     D  Cod_RESO_ORM...
009500140610     d                 s             14
009600140623     D  Salva_REF_ORM...
009700140623     d                 s             14
009800111028      **
009900100701      *----------------------------------------------------*
010000090205      **  Elenco DS di tutti i Segmenti da tradurre
010100100701      *----------------------------------------------------*
010200100811???? D eds96BGM      E DS
010300100811???? D eds96CNI      E DS
010400100811???? D eds96CNT      E DS
010500100811???? D eds96COM      E DS
010600100811???? D eds96CTA      E DS
010700100811???? D eds96DGS      E DS
010800100811???? D eds96DIM      E DS
010900100811???? D eds96DOC      E DS
011000100811???? D eds96DTM      E DS
011100100811???? D eds96EQD      E DS
011200100811???? D eds96EQN      E DS
011300100811???? D eds96FTX      E DS
011400100628???? D  D05                   16    365    DIM(5)
011500100811???? D eds96GID      E DS
011600100811???? D eds96GIN      E DS
011700100920???? D  GIN_10                 4    353    DIM(10)
011800100811???? D eds96GOR      E DS
011900100811???? D eds96HAN      E DS
012000100811???? D eds96LOC      E DS
012100100811???? D eds96MEA      E DS
012200100811???? D eds96MOA      E DS
012300100811???? D eds96NAD      E DS
012400100811???? D eds96PCI      E DS
012500100920???? D  PCI_10                 4    353    DIM(10)
012600100811???? D eds96PIA      E DS
012700100811???? D eds96RFF      E DS
012800100811???? D eds96RNG      E DS
012900100811???? D eds96SEL      E DS
013000100811???? D eds96TCC      E DS
013100100811???? D eds96TDT      E DS
013200100811???? D eds96TMD      E DS
013300100811???? D eds96TMP      E DS
013400100811???? D eds96TOD      E DS
013500100811???? D eds96TSR      E DS
013600100811???? D eds96UNA      E DS
013700100811???? D eds96UNB      E DS
013800100811???? D eds96UNH      E DS
013900100811???? D eds96UNT      E DS
014000100811???? D eds96UNZ      E DS
014100100701      *----------------------------------------------------*
014200090210      **
014300090210     D  X30            S             35    DIM(20)                              generico segnacolli
014400090205      **
014500090209     D Valori_inErr    ds
014600090209     D  SKout_Errori                  1    DIM(50)
014700100630      **
014800090209     D Descr_Errore    ds
014900090209     D  SKout_DesErr                 50    DIM(50)
015000090209      *
015100090205      *----------------------------------------------------*
015200091016     D EoFTivin        s              1
015300091016      *----------------------------------------------------*
015400090205     D Tipo_seg        S              3
015500100720     D Trovato_seg     S              1
015600100716      *
015700100716     d keyUNBCLI       s             35
015800100716     d keyTIPOMSG      s              6
015900100716     d keyVERSION      s              3
016000100716     d keyRELEASE      s              3
016100100716     d keyAGENCY       s              3
016200100716     d keyASSOCIA      s              6
016300100716      *
016400100720     D DsGenerica      s           2048
016500090209     D segmento        s           2048
016600090209     D Errore          s              1
016700100701      *
016800000223     D W0140           S             14  0
016900991129     D WORA            S              6  0
017000100701     D DataGGMMAAA     S              8  0
017100100701     D DATEU           S              8  0
017200100701     D DATA_eur        S               D   DATFMT(*eur)
017300100701      *
017400100630     D sk              S              3  0 INZ
017500100630     D sx              S              3  0 INZ
017600100628     D XX              S              3  0 INZ
017700110328     d Key_Generica35  s             35
017800100630     D NUM_Spediz      s                   LIKE(vabnsp)
017900100708     D UNT_record      s                   LIKE(vnREC)
018000100708     D IniDET_RECord   s                   LIKE(vnREC)
018100100708     D FinDET_RECord   s                   LIKE(vnREC)
018200090213     D Last_RECord     s                   LIKE(vnREC)
018300101222     D UNZ_SEGMENTO    s              1
018400100318      *------------------------------------------
018500100318      **  Definizione Qualificatori
018600100318      *------------------------------------------
018700100319      *
018800100628???? D Documento_Manifest...
018900100628???? D                 s              3    INZ('785')
019000100701???? D Totale_Peso_Lordo...
019100100701???? D                 s              3    INZ('7  ')
019200100701???? D Totale_Nr_Spedizioni...
019300100701???? D                 s              3    INZ('10 ')
019400100701???? D Totale_Nr_Colli...
019500100701???? D                 s              3    INZ('11 ')
019600100701???? D Totale_Peso_Netto...
019700100701???? D                 s              3    INZ('ZCW')
019800100628???? D Riferimento_Spedizione...
019900100628???? D                 s              3    INZ('AGE')
020000100708???? D Riferimento_Numerico...
020100100708???? D                 s              3    INZ('CU ')
020200100318???? D Riferimento_Cliente_particolare...
020300100318???? D                 s              3    INZ('FF ')
020400100701???? D X_Cargo_Manifest...
020500100318???? D                 s              3    INZ('AFB')
020600100701???? D X_Numero_CMR...
020700100318???? D                 s              3    INZ('CMR')
020800100318???? D Telephone_Number...
020900100318???? D                 s              3    INZ('TE ')
021000100318???? D TeleFax_Number...
021100100318???? D                 s              3    INZ('TX ')
021200100628???? D Porto_Assegnato...
021300100628???? D                 s              3    INZ('001')
021400100628???? D Porto_Franco...
021500100628???? D                 s              3    INZ('002')
021600100628???? D Porto_Franco_uncleared...
021700100628???? D                 s              3    INZ('003')
021800100628???? D Porto_Franco_domicile...
021900100628???? D                 s              3    INZ('004')
022000100318???? D Volume...
022100100318???? D                 s              3    INZ('VOL')
022200100705???? D Weight...
022300100705???? D                 s              3    INZ('WT ')
022400100319???? D G_Weight...
022500100319???? D                 s              3    INZ('G  ')
022600100318???? D Gross_Weight...
022700100318???? D                 s              3    INZ('AAG')
022800100706???? D Gross_Weight_E...
022900100706???? D                 s              3    INZ('AAE')
023000100706???? D N_Weight...
023100100706???? D                 s              3    INZ('N  ')
023200100706???? D Net_Weight...
023300100706???? D                 s              3    INZ('AAF')
023400100318???? D Kilograms...
023500100318???? D                 s              3    INZ('KGM')
023600100319???? D Grams...
023700100319???? D                 s              3    INZ('163')
023800100318???? D Meters...
023900100318???? D                 s              3    INZ('MTR')
024000100318???? D Cubic_Meters...
024100100318???? D                 s              3    INZ('MTQ')
024200100318???? D Shipper_assigned...
024300100318???? D                 s              3    INZ('24 ')
024400100705???? D Data_senza_ora...
024500100628???? D                 s              3    INZ('102')
024600100705???? D Data_con_ora...
024700100628???? D                 s              3    INZ('203')
024800100705???? D Data_con_i_secondi...
024900100705???? D                 s              3    INZ('204')
025000100630???? D Squadratura_Colli...
025100100630???? D                 s              1    INZ('N')
025200120613     D controlla_COD_FTX...
025300120613???? D                 s              3
025400100316      *---------------------------------------------------------------------*
025500100316      **  segmenti Identificativi parti specifiche del messaggio
025600100316      *---------------------------------------------------------------------*
025700100701???? D Segm_Inizio_Messaggio...
025800100701???? D                 s              3    INZ('UNB')
025900100701???? D Segm_Testata_Dettagli...
026000100701???? D                 s              3    INZ('UNH')
026100100701???? D Segm_Inizio_Dettaglio...
026200100316???? D                 s              3    INZ('CNI')
026300100701???? D Segm_Fine_Dettagli...
026400100316???? D                 s              3    INZ('UNT')
026500100701???? D Segm_Fine_messaggio...
026600100316???? D                 s              3    INZ('UNZ')
026700100701???? D seg_imprevisto...
026800100701???? D                 s              1
026900100726???? D Forza_Uscita...
027000100726???? D                 s              1
027100100701      *---------------------------------------------------------------------*
027200100701???? D Dati_di_Testata...
027300100701???? D                 s              1    INZ('S')
027400100701???? D Contatore_Dettagli...
027500100701???? D                 s              5s 0 INZ(0)
027600100728???? D Segmento_in_errore...
027700100728???? D                 s              5s 0 INZ(0)
027800100701      *---------------------------------------------------------------------*
027900100910      *
028000100910      * Tabella partner
028100121105     D PT_key          S             35    DIM(200)
028200121105     D PT_Parz         S             35    DIM(%elem(PT_key))
028300121105     D PT_KSC          S              7    DIM(%elem(PT_key))
028400121105     D PT_uni          S             90    DIM(%elem(PT_key))
028500121105     D PU_uni          S             90    DIM(%elem(PT_key))
028600121105     D PV_uni          S             90    DIM(%elem(PT_key))
028700121105     D PZ_uni          S             90    DIM(%elem(PT_key))
028800121105     D TRUL0SDS      E DS
028900100910      *
029000100910     d PT_KeyXsav      s              3  0 inz(0)
029100100910     D PT_trovata      s              1    inz('N')
029200100910     D PU_key          S             35
029300100910     D PV_key          S             35
029400120629     D PZ_key          S             35
029500100910     D PU_HLD_SpedVAX  S              1
029600111028     D  PU_in_RMN_BOLLA_EXPORT_x_RESO...
029700111028     d                 s              1
029800111028     D  era_un_export_RESO_dal_PARTNER...
029900111028     d                 s              1
030000100910     D PT_con_piu_Nazioni...
030100100910     d                 s              1
030200120629     D PZ_abilita_NAD_SF_come_RFF_FF...
030300120629     d                 s              1
030400100910      *
030500100910     D  PT_vabLNP      s                   LIKE(vablnp)
030600100910     D  PT_vabNRS      s                   LIKE(vabnrs)
030700100910     D  PT_vabCTM      s                   LIKE(vabctm)
030800100910     D  PT_vabCCM      s                   LIKE(vabccm)
030900100910     D  PU_lunVAT      s              2s 0
031000100910     D  PU_TipoServ    s              2a
031100100910     D  PU_SpecServ    s              2a
031200100910      *
031300100910     D Nr_PT           S              3  0 INZ
031400100910     D savXX_PT_key    S              3  0 INZ
031500100628      *
031600100701      *---------------------------------------------------------------------*
031700100630     d UNB_diWork      s                   like(UNB0004)
031800100628     d  UNB_Mittente   s                   like(UNB0004)
031900100706     d BGM_Id_Testata  s             35A   inz(' ')
032000100706     d  BGM_NrFviaggio...
032100100628     d                 s             35A
032200100706     d  BGM_nrCMR      s             35A
032300100701     d  RFF_NrFviaggio...
032400100701     d                 s             35A
032500100701     d  RFF_NrCMR      s             35A
032600100701     d  RFF_RifSpediz  s             35A
032700100910     d  RFF_DalPrimoCollo...
032800100910     d                 s              1A
032900100910     d  RFF_DopoIlPrimoCollo...
033000100910     d                 s              1A
033100100701     d UNB_parziale    s              1    inz('N')
033200100701     d UNB_Parz        s             35    inz(' ')
033300100812     d UNB_Generico    s             31    inz(' ')
033400100701     d UNB_NrTrasmiss  s             14    inz(' ')
033500100701     d UNZ_NrTrasmiss  s             14    inz(' ')
033600100701     d UNH_Rifer_Msg   s             14    inz(' ')
033700100701     d UNH_VABrma      s                   like(VABrma)
033800100701     d UNH_VABrmn      s                   like(VABrmn)
033900120629     d NAD_SF_Cliente  s             35    inz(' ')
034000100910      *
034100100910???? D NAD_destinatario_in_testata...
034200100910???? D                 s              1
034300100910???? D NAD_destinatario_in_dettaglio...
034400100910???? D                 s              1
034500100910???? D NAD_mittente_in_testata...
034600100910???? D                 s              1
034700100910???? D NAD_mittente_in_Dettaglio...
034800100910???? D                 s              1
034900100910     d CTA_destinatario_in_Dettaglio...
035000100910     d                 s             35A
035100100910     d CTA_destinatario_in_testata...
035200100910     d                 s             35A
035300100910     d COM_telefono_in_Dettaglio...
035400100910     d                 s             35A
035500100910     d COM_telefono_in_testata...
035600100910     d                 s             35A
035700110907     d FTX_ServizioMail_xDestinatario...
035800110907     d                 s             70A
035900131025     d FTX_MAIL_come_Servizio...
036000131025     d                 s              1A
036100131025     d FTX_ServizioSMS_xDestinatario...
036200131025     d                 s             70A
036300131025     d FTX_SMS_come_Servizio...
036400131025     d                 s              1A
036500131025     d FTX_ai_piani    s              1A
036600100701      *
036700100630     d  DTM_DtParten   s              8s 0
036800100630     d  DTM_DtFViag    s              8s 0
036900100630     d  DTM_DtCMR      s              8s 0
037000100811     d  DTM_DtConsRic  s              8s 0
037100100811     d  DTM_OraCnsRic  s              4s 0
037200100630     d  DTM_OraFViag   s              4s 0
037300100630     d  DTM_OraCMR     s              4s 0
037400100702     d  DTM_DtArrivo   s              8s 0
037500100702     d  DTM_OraArrivo  s              4s 0
037600100706     d  DTM_OraParten  s              4s 0
037700100702     d  CNI_Identificativo_Bolla...
037800100702     d                 s             35A
037900100813     d  CNI_seNumerico...
038000100813     d                 s                   LIKE(vabRMN)
038100100701     d  GID_aTotale    s              1A
038200100705     d  GID_ColliInAdd...
038300100705     d                 s                   like(gid722420)
038400100910      *---------------                                                      *
038500090209      *  DS x scomposizione segnacollo
038600100630     D DS_SegnBART     DS
038700100630     D  SegnBART_LNP           1      3  0
038800100630     D  SegnBART_LNA           4      6  0
038900100630     D  SegnBART_NRS           7      8  0
039000100630     D  SegnBART_NSC           9     15  0
039100100630     D  SegnBART_ZNC          16     17  0
039200100701      **
039300090209      *---------------                                                      *
039400100701     D VABtic_work     s                   LIKE(vabtic)
039500110623     d inviato_tipo_incasso...
039600110623     d                 s              1
039700100630      *
039800090603     d tes_VABrmo      s                   LIKE(VABrmo)
039900090603     d tes_VABnmo      s                   LIKE(VABnmo)
040000090603     d tes_VABcmo      s                   LIKE(VABcmo)
040100090209     C*
040200090603     d tes_VABrsd      s                   LIKE(VABrsd)
040300090603     d tes_VABrd2      s                   LIKE(VABrd2)
040400090603     d tes_VABind      s                   LIKE(VABind)
040500090603     d tes_VABlod      s                   LIKE(VABlod)
040600090603     d tes_VABnzd      s                   LIKE(VABnzd)
040700090603     C*
040800100813     d tes_VABtsp      s                   LIKE(VABtsp)
040900100813     d tes_VABtc1      s                   LIKE(VABtc1)
041000120403     d tes_VABtc2      s                   LIKE(VABtc2)
041100100813     d tes_VABnot      s                   LIKE(VABnot)
041200120214     d tes_VABffd      s                   LIKE(VABffd)
041300100813     C*
041400090603     d tes_Valuta      s                   LIKE(VABvca)
041500090603     C*
041600100910     D SUvabRMO        s                   LIKE(vabRMO)
041700090209      *---------------------------------------------------------------------*
041800090209      * Schiere
041900090209      *---------------------------------------------------------------------*
042000090209      * Schiera per reperimento nr.seganacollo
042100100630     D segn_BAR        S              7  0 DIM(5000)
042200100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
042300100630      *
042400100630      *
042500090209      * Nr. documento alfanumerico da decodificare
042600090209     D NDA             S              1    DIM(35)
042700100630      *
042800090209      * Nr. documento alfanumerico da già decodificato
042900090209     D NDN             S              1    DIM(15)
043000100630      *
043100090209      * Messaggi di errore
043200090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
043300100630      *
043400090209      * Tabella codici trattamento merce (1B)
043500100701     D Cod_Tb_CTM      S              2    DIM(200)
043600100701     D Des_Tb_CTM      S             67    DIM(200)
043700100630      *
043800090209      * Tabella codici valuta
043900090209     D CCV             S              3    DIM(200)
044000090209     D DCV             S             89    DIM(200)
044100100630      *
044200090209      * Tabella codici nazioni
044300090209     D C15             S              3    DIM(500)
044400090209     D ISO             S              3    DIM(500)
044500090209     D CPF             S              2  0 DIM(500)
044600100630      *
044700090209      * Tabella CL --> codici clienti - tariffa
044800130305     D Cod_Tb_CL       S             35    DIM(999)
044900130305     D Des_Tb_CL       S             90    DIM(999)
045000100630      *
045100090209      * Schiere x caricamento cod.cliente  <>
045200130305     D skCLienti       S              7  0 DIM(999)
045300100630      *
045400100318      * Priorità trasporto
045500100705     D Key_TipServ     S             35    DIM(100)
045600100702     D TipoServizio    S              1    DIM(100)
045700100702      *
045800100702      * Decodifiche servizi speciali
045900100702     D SpecialServ     S             35    DIM(100)
046000100702     D Key_ServSpec    S              3    DIM(100)
046100100702     D UNI_ServSpec    S             90    DIM(100)
046200100630      *
046300090209      * Termini di consegna
046400110328     D K35_TpBolla     S             35    DIM(100)
046500110328     D Cod_TpBolla     S              3    DIM(100)
046600110328     D Decod_Porto     S              1    DIM(100)
046700110328     d Trovata_Tab_TB  S              1
046800100630      *
046900090216      * Tipo pagamento C/Assegno
047000100705     D K35_TpIncas     S             35    DIM(100)
047100100705     D TipoIncasso     S              2    DIM(100)
047200100630      *
047300090209      * Schiere x routine di conversione CAP
047400090209     D CAP5            S              1    DIM(5)
047500090209     D CAP9            S              1    DIM(9)
047600090209     D CAPM            S              1    DIM(9)
047700100630      *
047800090209      * Schiera per memorizzazione FTX ricevuti
047900090209     D QUA             S              3    DIM(100)
048000090209     D FTX             S             70    DIM(100)
048100100630      *
048200090209      * Tabelle capiconto
048300090209      * Schiere x località
048400090209     D LOD             S              1    DIM(35)
048500100630      *
048600060608      **?------------------------------------------------------------------ */
048700090601     C*? Ds Scrittura del VAT "E"
048800060608      **?------------------------------------------------------------------ */
048900060612     D XTOEBCDDS     E DS
049000060620      *
049100060608      **?------------------------------------------------------------------ */
049200100701      *
049300090209      **?------------------------------------------------------------------ */
049400090209     C* Definizione campi di work
049500090209     D kKUT            s                   LIKE(TBLKUT)
049600090209     D kCOD            s                   LIKE(TBLCOD)
049700090209     D kKEY            s                   LIKE(TBLKEY)
049800090209     D kKCC            s                   LIKE(ACOKCC)
049900090209     D kKSC            s                   LIKE(ACOKSC)
050000090209     D kAAA            s                   LIKE(NUFAAA)
050100090209     D kCNU            s                   LIKE(NUFCNU)
050200090209     D kFIL            s                   LIKE(NUFFIL)
050300090209     D kAAS            s                   LIKE(VABAAS)
050400090209     D kLNP            s                   LIKE(VABLNP)
050500090209     D kNRS            s                   LIKE(VABNRS)
050600090209     D kNSP            s                   LIKE(VABNSP)
050700090209     D kCMR            s                   LIKE(VABCMR)
050800090209     D kCCM            s                   LIKE(VABCCM)
050900090209     D kNCD            s                   LIKE(VADNCD)
051000090210     D kCNT            s                   LIKE(VADCNT)
051100090209     D kNMC            s                   LIKE(RDENMC)
051200090209     D kNRP            s                   LIKE(RDENRP)
051300090209     D kLNP1           s                   LIKE(RDELNP)
051400090209      *
051500090209      *  Invio E-mail
051600090209     D Indirizzi       s            253
051700090209     D Oggetto         s             44
051800090209     D Messaggio       s           5000
051900090209      *
052000090209     d   DSmail        ds
052100090209     D Mail_Ind                      44
052200090209     d   IND_char                     1    dim(44)
052300090209      *
052400100630     D Lunghezza_Indirizzo...
052500100630     D                 s              5u 0
052600090209      *
052700100630     D dalCarattere    s              5u 0
052800100630     D xTOTcaratteri   s              5u 0
052900090209      *
053000090209     C*---------------------------------------------------------------*
053100090209     D MsgDSP          S            256                                         Testo generico
053200090209     C*---------------------------------------------------------------*
053300090209     D SndMg01         S             10                                         Message type
053400090209     D                                     INZ('*INFO')
053500090209     D SndMg02         S             10                                         Deluvery mode
053600090209     D                                     INZ('*BREAK')
053700090209     D SndMg03         S            256                                         Message text
053800090209     D SndMg04         S             10I 0                                      Length of text
053900090209     D                                     INZ(%SIZE(SndMg03))
054000090209     D SndMg05         S             10                                         User profile
054100090209     D                                     DIM(299)
054200090209     D SndMg06         S             10I 0                                      Number of user
054300090209     D SndMg07         S             10I 0                                      Message sent indic.
054400090209     D SndMg08         S             10I 0                                      Function requested
054500090209     D SndMg10         S              1                                         Show display
054600090209     D                                     INZ('N')
054700090209     D SndMg11         S             20                                         Qualified MSGQ name
054800090209     D SndMg12         S              4                                         Name type indicator
054900090209     D                                     INZ('*USR')
055000090209      * indice di scihera
055100090209     D Jx              S              5I 0
055200090209      *
055300090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
055400090209      *
055500090209     d bart_it         C                   CONST('@Bartolini.it;')
055600090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
055700060612      * ?================================================================== */
055800060612      * ?   * Campi x decodifica * (INPUT  del Record)
055900100319      * ?================================================================== */
056000090130     D  Dati           s           2048
056100060612      * ?   *--------------------------------------------------------------*
056200060612     D  se_errore      s              1    inz(' ')
056300060612      * ?* ------------------------------------------------------ *
056400060710     D Digits          C                   '0123456789'
056500091019     D*------------------
056600091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
056700091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
056800060612      * ?================================================================== */
056900060612      *   Ciclo principale
057000090205      * ?================================================================== */
057100060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
057200060612     C*
057300060612     C                   if        not %open(tivin00r)
057400060612     C                   open      tivin00r
057500060612     C                   endif
057600060612      **
057700060612      * ?------------------------------------------------------------------ */
057800100708     C*  Controllo dati arrivati da DPD
057900060612      * ?------------------------------------------------------------------ */
058000060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
058100060612      * ?- controllando che la trasmissione sia completa.
058200090205      **
058300060612      * ?              /*---------------------- */
058400090205     c                   goto      nonFare_adesso
058500060612     c                   exsr      check_Trasm
058600090205     c     nonFare_adessotag
058700060612      * ?              /*---------------------- */
058800090205      **
058900060613      **  Errore di trasmissione x tutti i records
059000060613      **   --> file in errore
059100060613     c                   if        se_errore = 'S'
059200060612
059300060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
059400090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
059500090205     c                   exsr      Msg_errato
059600060612      **
059700060612     c                   else
059800060614      **
059900060612      * ?------------------------------------------------------------------ */
060000100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
060100060612      * ?------------------------------------------------------------------ */
060200060612      * ?              /*---------------------- */
060300060612     c                   exsr      Importa_Msg
060400060612      * ?              /*---------------------- */
060500060612
060600060614     c                   end
060700060612      *
060800060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
060900090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
061000090225     c                             esito ='0'
061100060710     C                   eval      esito ='0'
061200090225      *
061300090225     c                   COMMIT
061400110208     C                   endif
061500090224      *
061600090213     c                   if        almeno_un_due ='S'
061700090213     C                   eval      esito ='1'
061800060614     C                   endif
061900100708      * chiude TIVIN
062000100708     C                   if        %open(tivin00r)
062100100708     C                   close     tivin00r
062200100708     C                   endif
062300060614      *
062400060612     c                   SETON                                        LR
062500060612      * ?================================================================== */
062600100708      *? Importa i records della trasmissione
062700060612      * ?------------------------------------------------------------------ */
062800060612     c     Importa_Msg   Begsr
062900060614
063000130506      *   Inizia il conteggio totale delle righe
063100130506     c                   z-add     0             total_segmenti    5 0
063200100726     c                   clear                   Forza_Uscita
063300060612     c     *start        setll     Tivin00r
063400100708
063500100708     c                   EXSR      LEGGE_TIVIN_R
063600100708     c                   dow       EoFTivin <> 'S'
063700060614
063800060614      * solo i record sflaggati da rielaborare
063900110107     c                   IF        vinFLG = *blank
064000110107      *** ++++++
064100090211      *  Sempre Record OK
064200090211      ** Controlli formali sui campi
064300090211     C                   eval      vinFLG = '1'
064400060612     c                   clear                   se_errore
064500110107      *
064600110107      *** >>>>>>
064700110107     c                   IF        vinDTA <> *blank
064800110107      *** >>>>>>
064900060612
065000060612      ** Decodifica record a campi variabili
065100060612      * ?              /*---------------------- */
065200090205     c                   exsr      Decod_Segmento
065300090209      * ?              /*---------------------- */
065400060612
065500060621      *  x errore bloccante nella composizione del VAB/VAT
065600060621     c                   if        err_bloccante ='S'
065700060621     C                   eval      vinFLG = '2'
065800110208     c                   eval      Almeno_Un_Due ='S'
065900090211     c                   eval      esito  = '2'
066000100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
066100100921     C                   EVAL      Messaggio = 'EDI -
066200100921     C                             IFCSUM D96A - msg su UPLOAD -
066300100921     C                             con UNB non presente in tabella PT..'
066400100319     c                   if        §PVinv <> 'N'
066500100319     c                   exsr      invio_mail
066600060621     c                   end
066700100319     c                   end
066800060612
066900110107      *** >>>>>>
067000110107     c                   endIF
067100110107      *** >>>>>>
067200110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
067300110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
067400110107      *         un erroneo messaggio
067500110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
067600060612     c                   update    Tivin000
067700090211
067800090211      *** se non è riconosciuto l'UNB deve uscire direttamente
067900090211     c                   if        se_errore ='S'  and
068000100708     c                             tipo_seg = Segm_Inizio_Messaggio
068100090213     c                   eval      err_bloccante = 'S'
068200090211     c                   exsr      Msg_errato
068300090211     c                   LEAVE
068400090211     c                   endIF
068500090211      ***
068600060614     c                   endIF
068700060612
068800100708     c                   EXSR      LEGGE_TIVIN_R
068900060612     C                   ENDdo
069000060612      **
069100060612     c                   endSR
069200100709      * ?------------------------------------------------------------------ */
069300100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
069400100709      * ?------------------------------------------------------------------ */
069500100709     C     MSG_Errato    BEGSR
069600100709      *
069700100709      * ?  Scrive su tutti i records il tipo di errore
069800100709     c     *start        setll     tivin00r
069900100709     c
070000100709     c                   EXSR      LEGGE_TIVIN_R
070100100709     c                   dow       EoFTivin <> 'S'
070200100709     c
070300100709     c                   if        vinMSG  = *blank
070400100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
070500100709     C                               >> Controllare i DATI su UPLOAD !!'
070600100709     c                   end
070700100709     C                   eval      vinFLG = '2'
070800100709     c                   eval      Almeno_Un_Due ='S'
070900100709     c                   eval      esito  = '2'
071000100709     c                   eval      se_errore ='S'
071100100709     c                   eval      err_bloccante ='S'
071200100709      *
071300100709     c                   update    tivin000
071400100709     c                   EXSR      LEGGE_TIVIN_R
071500100709     c                   endDO
071600100709      *
071700100709     C                   ENDSR
071800100709      * ?------------------------------------------------------------------ */
071900100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
072000100709      * ?------------------------------------------------------------------ */
072100100709     C     DETta_Errato  BEGSR
072200100709      *
072300100709     c                   EXSR      LEGGE_TIVIN_R
072400100709     c                   dow       EoFTivin <> 'S'
072500100709      *
072600100709      * Se ha letto il successivo record
072700100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
072800100709     c                   if        VNREC = UNT_record  or
072900100709     c                             VNREC = finDET_RECord
073000100709     c                   leave
073100100709     c                   end
073200100709      *
073300100709     c                   exsr      Error_DET
073400100709      *
073500100709     c                   if        vinMSG  = *blank
073600100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
073700100709     C                             >> Controllare i DATI di ogni Segmento <<'
073800100709     c                   end
073900100709      *
074000100709     c                   update    tivin000
074100100709     c                   EXSR      LEGGE_TIVIN_R
074200100709     c                   endDO
074300100709      * ------
074400100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
074500100921     C                   EVAL      Messaggio = 'EDI -
074600100811???? C                             IFCSUM D96A  non totalmente tradotto. -
074700100709     C                             Controllare UPLOAD del cliente:'
074800100709     C                   EVAL      Messaggio = %trim(Messaggio) +
074900100709     C                                         %editc(§PTksc:'X')
075000100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
075100100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
075200100709     c                   if        §PVinv <> 'N'
075300100709     c                   exsr      invio_mail
075400100709     c                   end
075500100709      *
075600100709      * ripulisce per un nuovo giro
075700100709     C                   clear                   Err_in_detta
075800100709      *
075900100709     C                   ENDSR
076000100709      * ?------------------------------------------------------------------ */
076100100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
076200100709      * ?------------------------------------------------------------------ */
076300100709     C     LEGGE_TIVIN_R BEGSR
076400100709      *
076500100709     c                   clear                   EoFTivin
076600100709      *  Lettura sequenziale
076700100709     c                   read      Tivin00r
076800100709
076900100709     c                   if        %Eof(tivin00r)
077000100709     c                   move      'S'           EoFTivin
077100100709     c                   else
077200100709      *
077300100709      * ?  imposta il tipo di segmento:
077400100709     c                   clear                   dati
077500100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
077600100728     c                   eval      segmento = %trimL(dati)
077700100728     c                   eval      tipo_seg = %subst(segmento:1:3)
077800100709      **
077900100709     c                   end
078000100709      *
078100100709     C                   ENDSR
078200100120      * ?------------------------------------------------------------------ */
078300100120      *?    Decodifica il segmento ed importa i campi in formato DS
078400100120      * ?------------------------------------------------------------------ */
078500100120     c     Decod_SegmentoBegsr
078600100120      **
078700100706      *** ------------------------------------
078800100706????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT"
078900100706      *** ------------------------------------
079000100706      * OGNI DETTAGLIO inizia dal segmento CNI e per ultimo il msg è chiuso da UNT.
079100100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
079200100706      **
079300100706      **  Scrive alla fine di ogni giro di dettaglio:
079400100706     c                   if           tipo_seg = Segm_Fine_Dettagli
079500100706     c                                   or
079600100706     c                                tipo_seg = Segm_Inizio_Dettaglio  and
079700100706     c                                Contatore_Dettagli > 0
079800100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
079900100708     c                   eval      FinDET_RECord = vnrec
080000100708      **?              /*---------------------- */
080100100708     c                   exsr      Scrive_Dettag
080200100708      **?              /*---------------------- */
080300100812      *
080400100812      *   Ogni nuovo Dettaglio,
080500100812      *    per quei Partner che gestiscono più LInee
080600100812      *    Mediante il Mittente del dettaglio
080700100812     c                   if        UNB_parziale ='S' and
080800100812     c                              PT_con_piu_Nazioni = 'S' and
080900100812     C                                 Naz_salvata = Naz_mittente
081000100812      * ?ReImposta i dati da tabella PT
081100100812      * ?  se fossero stati cambiati dal precedente dettaglio:
081200100812     C                   z-add     savXX_PT_key  Nr_PT
081300100812     c                   exsr      Se_Trovata_PT
081400100812     c                   endIF
081500100812      *
081600100812     c                   ENDif
081700100708      *
081800100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
081900100708      **?              /*---------------------- */
082000100708     c                   exsr      Pulizia_Campi
082100100708      **?              /*---------------------- */
082200100812      *
082300100706      *** ------------------------------------
082400100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
082500100120      * ?  Anche se non utilizzati.
082600100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
082700100317     c                   clear                   seg_imprevisto
082800100720     c                   clear                   trovato_seg
082900100319      *
083000100811      **?          programma che decodifica il segmento
083100100811      **?              /*---------------------- */
083200100811     c                   exsr      chiama_Decoder
083300100811      **?              /*---------------------- */
083400100811      *
083500100319      *  Contatore delle righe ossia dei segmenti del messaggio
083600100319     c                   add       1             Conta_segmenti
083700130506     c                   add       1             total_segmenti
083800100708      *
083900120424      *>>>--->>>                                                    <<<---<<<
084000120424      *   Se ci sono dei Segmenti Particolari da NON considerare
084100120424     c                   IF        trovato_seg ='N' and unz_segmento <>'S'
084200120424      *
084300120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
084400120424     C                   movel(p)  'SGM'         etbSGM
084500120424     C                   movel(p)  Tipo_Seg      etbVALSGM
084600120424     c                   exsr      In_TAB_EDSTBL
084700120424      *
084800120424     C*  Quindi sono dichiarati nel file delle eccezioni
084900120424     C                   If        Quale_campo ='TIPSGM' and
085000120424     c                                                   trovata_EDSTBL = 'S'
085100120424     c                   eval      trovato_seg ='S'
085200120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
085300120424      * errori sul TIVIN.
085400120424     c                   endIf
085500120424     c                   end
085600120424      *>>>--->>>                                                    <<<---<<<
085700100708      *** ------------------------------------
085800100708      *   Decodifica il singolo segmento LETTO
085900100708      *** ------------------------------------
086000100120     c                   select
086100100120      *
086200110110     c                   when      trovato_seg ='N' and unz_segmento <> 'S'
086300100720     c                   move      'S'           seg_imprevisto
086400100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
086500100720     c                   exsr      Error_DET
086600100720     c                   goto      end_Decod
086700100720      **--------
086800100120     c                   when      tipo_seg = 'UNA'
086900100811     c                   movel(p)  DsGenerica    eds96UNA
087000100120      *
087100100120     c                   when      tipo_seg = 'UNB'
087200100811     c                   movel(p)  DsGenerica    eds96UNB
087300100120     C                   EXSR      DATI_UNB
087400100120      * --------
087500100120     c                   when      tipo_seg = 'UNH'
087600100811     c                   movel(p)  DsGenerica    eds96UNH
087700100120     C                   EXSR      DATI_UNH
087800100120      * --------
087900100120     c                   when      tipo_seg = 'BGM'
088000100811     c                   movel(p)  DsGenerica    eds96BGM
088100100316     C                   EXSR      DATI_BGM
088200100811      **--------
088300100811     c                   when      tipo_seg = 'CNT'
088400100811     c                   movel(p)  DsGenerica    eds96CNT
088500100811     C                   EXSR      DATI_CNT
088600100811      **--------
088700100811     c                   when      tipo_seg = 'TDT'
088800100811     c                   movel(p)  DsGenerica    eds96TDT
088900100811     C                   EXSR      DATI_TDT
089000100811      * --------
089100100811     c                   when      tipo_seg = 'TSR'
089200100811     c                   movel(p)  DsGenerica    eds96TSR
089300100811     C                   EXSR      DATI_TSR
089400100811      **--------
089500100811     c                   when      tipo_seg = 'NAD'
089600100811     c                   movel(p)  DsGenerica    eds96NAD
089700100811     C                   EXSR      DATI_NAD
089800100811      **--------
089900100811     c                   when      tipo_seg = 'EQD'
090000100811     c                   movel(p)  DsGenerica    eds96EQD
090100100811     C                   EXSR      DATI_EQD
090200100811      **--------
090300100811     c                   when      tipo_seg = 'EQN'
090400100811     c                   movel(p)  DsGenerica    eds96EQN
090500100811     C                   EXSR      DATI_EQN
090600100811      **--------
090700100811     c                   when      tipo_seg = 'CNI'
090800100811     c                   movel(p)  DsGenerica    eds96CNI
090900100811     C                   EXSR      DATI_CNI
091000100811      **--------
091100100811     c                   when      tipo_seg = 'DTM'
091200100811     c                   movel(p)  DsGenerica    eds96DTM
091300100811     C                   EXSR      DATI_DTM
091400100811      **--------
091500100811     c                   when      tipo_seg = 'FTX'
091600100811     c                   movel(p)  DsGenerica    eds96FTX
091700100811     C                   EXSR      DATI_FTX
091800100811      **--------
091900100811     c                   when      tipo_seg = 'TOD'
092000100811     c                   movel(p)  DsGenerica    eds96TOD
092100100811     C                   EXSR      DATI_TOD
092200100811      **--------
092300100811     c                   when      tipo_seg = 'GID'
092400100811     c                   movel(p)  DsGenerica    eds96GID
092500100811     C                   EXSR      DATI_GID
092600100811      **--------
092700100811     c                   when      tipo_seg = 'MEA'
092800100811     c                   movel(p)  DsGenerica    eds96MEA
092900100811     C                   EXSR      DATI_MEA
093000100811      **--------
093100100811     c                   when      tipo_seg = 'RFF'
093200100811     c                   movel(p)  DsGenerica    eds96RFF
093300100811     C                   EXSR      DATI_RFF
093400100811      **--------
093500100811     c                   when      tipo_seg = 'PCI'
093600100811     c                   movel(p)  DsGenerica    eds96PCI
093700100811     C                   EXSR      DATI_PCI
093800100811      **--------
093900100811     c                   when      tipo_seg = 'GIN'
094000100811     c                   movel(p)  DsGenerica    eds96GIN
094100100811     C                   EXSR      DATI_GIN
094200100316      **--------
094300100316     c                   when      tipo_seg = 'SEL'
094400100811     c                   movel(p)  DsGenerica    eds96SEL
094500100316     C                   EXSR      DATI_SEL
094600100316      **--------
094700100316     c                   when      tipo_seg = 'PIA'
094800100811     c                   movel(p)  DsGenerica    eds96PIA
094900100316     C                   EXSR      DATI_PIA
095000100316      **--------
095100100316     c                   when      tipo_seg = 'LOC'
095200100811     c                   movel(p)  DsGenerica    eds96LOC
095300100316     C                   EXSR      DATI_LOC
095400100316      **--------
095500100316     c                   when      tipo_seg = 'DGS'
095600100811     c                   movel(p)  DsGenerica    eds96DGS
095700100316     C                   EXSR      DATI_DGS
095800100120      **--------
095900100120     c                   when      tipo_seg = 'MOA'
096000100811     c                   movel(p)  DsGenerica    eds96MOA
096100100120     C                   EXSR      DATI_MOA
096200100120      **--------
096300100120     c                   when      tipo_seg = 'CTA'
096400100811     c                   movel(p)  DsGenerica    eds96CTA
096500100120     C                   EXSR      DATI_CTA
096600100120      **--------
096700100120     c                   when      tipo_seg = 'COM'
096800100811     c                   movel(p)  DsGenerica    eds96COM
096900100120     C                   EXSR      DATI_COM
097000100811      **--------
097100100811     c                   when      tipo_seg = 'HAN'
097200100811     c                   movel(p)  DsGenerica    eds96HAN
097300100811     C                   EXSR      DATI_HAN
097400100811      **--------
097500100811     c                   when      tipo_seg = 'GOR'
097600100811     c                   movel(p)  DsGenerica    eds96GOR
097700100811     C                   EXSR      DATI_GOR
097800100811      **--------
097900100811     c                   when      tipo_seg = 'TMP'
098000100811     c                   movel(p)  DsGenerica    eds96TMP
098100100811     C                   EXSR      DATI_TMP
098200100811      **--------
098300100811     c                   when      tipo_seg = 'TMD'
098400100811     c                   movel(p)  DsGenerica    eds96TMD
098500100811     C                   EXSR      DATI_TMD
098600100811      **--------
098700100811     c                   when      tipo_seg = 'TCC'
098800100811     c                   movel(p)  DsGenerica    eds96TCC
098900100811     C                   EXSR      DATI_TCC
099000100811      **--------
099100100811     c                   when      tipo_seg = 'RNG'
099200100811     c                   movel(p)  DsGenerica    eds96RNG
099300100811     C                   EXSR      DATI_RNG
099400100811      **--------
099500100811     c                   when      tipo_seg = 'DOC'
099600100811     c                   movel(p)  DsGenerica    eds96DOC
099700100811     C                   EXSR      DATI_DOC
099800100120      **--------
099900100811     c                   when      tipo_seg = 'DIM'
100000100811     c                   movel(p)  DsGenerica    eds96DIM
100100100811     C                   EXSR      DATI_DIM
100200100811      **--------
100300100120     c                   when      tipo_seg = 'UNT'
100400100811     c                   movel(p)  DsGenerica    eds96UNT
100500100120     C                   EXSR      DATI_UNT
100600100120      **--------
100700100120     c                   when      tipo_seg = 'UNZ'
100800100811     c                   movel(p)  DsGenerica    eds96UNZ
100900100120     C                   EXSR      DATI_UNZ
101000101222     c                   move      'S'           unz_segmento
101100101222      **--------
101200101222     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
101300100120      **--------
101400100120     c                   OTHER
101500100120      **--------
101600110110     c                   if            unz_segmento <> 'S'
101700120424     c                                           and trovata_EDSTBL <> 'S'
101800100120     c                   move      'S'           seg_imprevisto
101900100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
102000100120     c                   exsr      Error_DET
102100100120     c                   goto      end_Decod
102200110110     c                   end
102300100120      **--------
102400100120     c                   endSL
102500100120      **
102600100120     c     end_Decod     endSR
102700100701      * ?================================================================== */
102800100319     C*? Azzera_MSG   - Azzera dati messaggio
102900100701      * ?================================================================== */
103000100708     C     Scrive_Dettag BEGSR
103100100701      **
103200100708      *  Deve flaggare il dettaglio con dei problemi
103300100708      * Imposta le righe in errore sullo specifico dettaglio
103400100708     C                   if        Err_in_detta = 'S'
103500100708      * prima di rieseguire il ciclo
103600100708      *  rilascia il record
103700100708     c                   update    tivin000
103800100708      * ?  Scrive su tutti i records il tipo di errore
103900100708     c     IniDET_RECord setll     tivin00r
104000100708      **?              /*---------------------- */
104100100708     c                   exsr      DETta_Errato
104200100708      **?              /*---------------------- */
104300100708     c                   end
104400100708      *
104500100708      **  Se presente un errore nel record emette una segnalazione msg
104600100708???? c                   if        se_errore <> 'S'
104700100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
104800100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
104900100708     c                              NAD_Destinatario_in_Testata    ='S'
105000100708      * ?              /*---------------------- */
105100100708     c                   exsr      Scrive_VAB
105200100708      * ?              /*---------------------- */
105300100708     c                   else
105400100708     c                   eval      errori_in_Wrt = 'S'
105500100708     c                   end
105600100708      *
105700100708      *  Problemi nella decodifica dei campi VAB/VAT
105800100708     c                   if        errori_in_Wrt = 'S'
105900100921     C                   evalR     vinMSG = 'EDI-Problemi scrittura VAB'
106000100708     c                   exsr      Error_DET
106100100708     c                   ROLBK
106200100708     c                   else
106300100708     c                   COMMIT
106400100708     c                   end
106500100708      *
106600100708     c                   end
106700100708      **
106800100708     c     end_WRidett   endSR
106900100708      * ?================================================================== */
107000100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
107100100708      * ?================================================================== */
107200100708     C     Pulizia_Campi BEGSR
107300100708      **
107400100708      * inzio messaggio azzera tutte le variabili generali del messaggio
107500100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
107600100708      * ?                         --------------
107700100708     C                   EXSR      Azzera_MSG
107800100708      * ?                         --------------
107900100708      *
108000100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
108100100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
108200100708      * ?                         --------------
108300100813     c                   exsr      Testata_msg
108400100708      * ?                         --------------
108500100708      *
108600100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
108700100708      *  Prepara una nuova spedizione
108800100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
108900100708      * ?                         --------------
109000100708     c                   exsr      Nuova_spediz
109100100708      * ?                         --------------
109200100708     c                   end
109300100708      **
109400100708     c     end_clear     endSR
109500100708      * ?================================================================== */
109600100708     C*? Azzera_MSG   - Azzera dati messaggio
109700100708      * ?================================================================== */
109800100708     C     Azzera_MSG    BEGSR
109900100708      **
110000100701     C*  Inizializza variabili
110100100701     C                   Z-ADD     0             skCLienti
110200100701     C                   Z-ADD     0             sk
110300100701     C*
110400100701      *  INIZIALIZZA  dati fondamentali messaggio
110500100701     C                   clear                   EDIDSPT
110600100701     C                   clear                   EDIDSPU
110700100701     C                   clear                   EDIDSPV
110800120629     C                   clear                   EDIDSPZ
110900100701     C*
111000100701     C                   move      'N'           PT_trovata
111100100812     C                   clear                   savXX_PT_key
111200100701     C                   clear                   PT_vabLNP
111300100701     C                   clear                   PT_vabNRS
111400100701     C                   clear                   PT_vabCTM
111500100701     C                   clear                   PT_vabCCM
111600100701     C                   clear                   PU_HLD_SpedVAX
111700111028     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO     = *blank
111800100812     C                   eval        PT_con_piu_Nazioni = *blank
111900100812     c                   move      *blank        Naz_salvata       3
112000090211     C*
112100100701     c                   clear                   UNB_Generico
112200100701     c                   clear                   UNB_Parz
112300100701     c                   clear                   UNB_NrTrasmiss
112400100701     c                   clear                   UNZ_NrTrasmiss
112500100708      *
112600120629     c                   clear                   NAD_SF_Cliente
112700100630      *
112800100630      * per controllare se almeno un record è stato importato sul VAB
112900100630     c                   clear                   Almeno_Uno        1
113000100630     c                   clear                   Almeno_Un_Due     1
113100100708     c                   clear                   IniDET_RECord
113200100708     c                   clear                   FinDET_RECord
113300100630     c                   eval      esito  = '0'
113400100728     c                   eval      Segmento_in_errore = 0
113500100317     C*
113600090211     C                   ENDSR
113700100701      * ?================================================================== */
113800100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
113900100701      * ?================================================================== */
114000100813     C     Testata_msg   BEGSR
114100100701      *
114200100701      *  imposta il Flag per identificare la testata
114300100701     c                   eval      Dati_di_Testata  ='S'
114400100701     C*
114500100701      *   Contatore dei dettagli
114600100701     c                   eval      Contatore_Dettagli = 0
114700100701      **
114800100701      *   Codici identificativi della Testata del messaggio
114900100701     c                   clear                   UNH_Rifer_Msg
115000100706     C* Pulisco dati di work x foglio viaggio
115100100706     C                   clear                   BGM_nrCMR                      * WNRCMR
115200100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
115300100701     c                   clear                   BGM_Id_Testata
115400100701      **
115500100701     C*  Riferimento Foglio Viaggio o CMR
115600100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
115700100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
115800100701      **   Date generali
115900100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
116000100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
116100100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
116200100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
116300100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
116400100702     C                   Z-ADD     0             DTM_DtArrivo                   *
116500100702     C                   Z-ADD     0             DTM_OraArrivo                  *
116600100811     C                   Z-ADD     0             DTM_DtConsRic                  * WvabDCR
116700100811     C                   Z-ADD     0             DTM_OraCnsRic                  * WvabHCR
116800100701      *
116900100701      * pulisce gli RFF+FF di testata
117000100706     C                   clear                   Tes_RFF_FF_KSC
117100100706     C                   clear                   Tes_RFF_FF_TAR
117200100706     C                   clear                   Tes_RFF_FF_LNP
117300100706     C                   clear                   Tes_RFF_FF_NRS
117400100706     C                   clear                   Tes_RFF_FF_CTM
117500100706     C                   clear                   Tes_RFF_FF_BS1
117600100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
117700100701     C*
117800100701     c                   clear                   tes_VABrmo
117900100701     c                   clear                   tes_VABnmo
118000100701     c                   clear                   tes_VABcmo
118100100701     C*
118200100813     c                   clear                   tes_VABtsp
118300100813     c                   clear                   tes_VABtc1
118400120403     c                   clear                   tes_VABtc2
118500100813     c                   clear                   tes_VABnot
118600120214     c                   clear                   tes_VABffd
118700100813     C*
118800100701     c                   clear                   tes_VABrsd
118900100701     c                   clear                   tes_VABrd2
119000100701     c                   clear                   tes_VABind
119100100701     c                   clear                   tes_VABlod
119200100701     c                   clear                   tes_VABnzd
119300100701     C*
119400100701     c                   clear                   tes_Valuta
119500100702     C*
119600100811     c                   clear                   Tot_Peso_msg
119700100811     c                   clear                   Tot_Sped_msg
119800100811     c                   clear                   Tot_Colli_msg
119900100811     c                   clear                   Tot_PNetto_msg
120000100813     C*
120100100813     c                   clear                   AddTot_Colli     16 0
120200100701     C*
120300100701     ** Inizializza campo di work
120400100701     c                   eval      GID_aTotale = 'N'
120500100701     C*
120600100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
120700100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
120800100701     c                   eval      NAD_destinatario_in_testata = *blank
120900100701     c                   eval      NAD_mittente_in_testata = *blank
121000100701     c                   eval      CTA_destinatario_in_testata = *blank
121100100701     c                   eval      COM_telefono_in_testata = *blank
121200100701     C*
121300100701      *   Inizia il conteggio delle righe
121400100706     c                   z-add     0             Conta_segmenti    5 0
121500111028     C*
121600100701     C                   ENDSR
121700100701      * ?================================================================== */
121800100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
121900100701      * ?================================================================== */
122000100701     C     Nuova_spediz  BEGSR
122100100812      **
122200100701     C* Da qui inzia il dettaglio perciò lo memorizzo
122300100701      *  imposta il Flag per identificare la testata
122400100701     c                   eval      Dati_di_Testata  ='N'
122500100708      *
122600100708     c                   z-add     vnrec         IniDET_RECord
122700100812      **
122800100812      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
122900100812      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
123000100812      *
123100100812     c                   if        UNB_parziale ='S' and
123200100812     c                              PT_con_piu_Nazioni = 'S'
123300100812      * ?Cerca dalla riga di questo dettaglio in avanti
123400100812      * ?           *------------------------------*
123500100812     c                   exsr      cerca_Mittente
123600100812     c                   exsr      esito_tab_PT
123700100812      * ?           *------------------------------*
123800100812     c                   endIF
123900100812      **
124000100812      *
124100100701      *  Pulizia dei records
124200100701     C                   CLEAR                   EDiVAB00
124300100701     C                   CLEAR                   EDiVAT00
124400100701     C                   CLEAR                   EDiVAD00
124500100701      **
124600100813      **   Campi da testata messaggio
124700100706     C                   z-add     PT_vabLNP     VABLNP
124800100706     C                   z-add     PT_vabNRS     VABNRS
124900100701     C                   MOVEL     PT_vabCTM     VABCTM
125000100701     C                   z-add     PT_vabCCM     VABCCM
125100100813     C                   movel     tes_VABtsp    VABTSP
125200100813     C                   movel     tes_VABtc1    VABTC1
125300120403     C                   movel     tes_VABtc2    VABTC2
125400100813     C                   movel     tes_VABnot    VABnot
125500120214     C                   movel     tes_VABffd    VABffd
125600100701     C*
125700110707     C                   MOVEL(p)  UNH_vabRMA    VABrma
125800100701     C                   z-add     UNH_vabRMN    VABrmn
125900100701      *
126000100701      *  Dettaglio dei segnacolli Bartolini/EEX
126100100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
126200100701     c                   eval      GID_aTotale = 'N'
126300100701      *
126400100701     C                   clear                   Conta_Segn        5 0
126500100701     C                   clear                   totale_PCI        5 0
126600100701     C                   clear                   RFF_RifSpediz
126700100701      *
126800100701     C                   MOVEA     *HIVAL        segn_BAR
126900100701     C                   MOVEA     *blank        segn_EEX
127000100701      *
127100100701     C*  Azzero codice cliente - tariffa
127200100701     C                   Z-ADD     0             ToT_Colli        16 0
127300100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
127400100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
127500100701      *
127600100701     C*  Azzero codice cliente - tariffa
127700100701     C                   clear                   Det_RFF_FF_ksc
127800100701     C                   clear                   Det_RFF_FF_tar
127900100701     C                   clear                   Det_RFF_FF_lnp
128000100701     C                   clear                   Det_RFF_FF_nrs
128100100701     C                   clear                   Det_RFF_FF_ctm
128200100701     C                   clear                   Det_RFF_FF_bs1
128300100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
128400110411     C                   clear                   Det_RFF_FF_tic
128500100701      *
128600100701     C                   clear                   Note_1           35
128700100701     C                   clear                   Note_2           35
128800110907     C                   clear                   Note_1T          35
128900110907     C                   clear                   Note_2T          35
129000131025     C                   clear                   Note_1sms        35
129100131025     C                   clear                   Note_2sms        35
129200100701     C*  Campi di work
129300100701     c                   clear                   VABtic_work
129400100701     c                   clear                   wri_vabtic        1
129500110623     c                   eval      inviato_tipo_incasso ='N'
129600100701      *
129700100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
129800100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
129900100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
130000100701     c                   eval      COM_telefono_in_Dettaglio = *blank
130100100702     c                   eval      CNI_Identificativo_Bolla = *blank
130200110907     c                   eval      FTX_ServizioMail_xDestinatario = *blank
130300131025     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
130400131025     c                   eval      FTX_MAIL_come_Servizio = *blank
130500131025     c                   eval      FTX_SMS_come_Servizio = *blank
130600120613     c                   eval      FTX_ai_piani = *blank
130700100701      *
130800100701     C                   clear                   Rifer_Alfanum    35
130900100910     C                   clear                   Secondo_Rifer    35
131000100701     C                   clear                   Rifer_Cliente    35
131100100701     C                   clear                   Rifer_Numeric    35
131200100701     C                   clear                   Tipo_Rifer        3
131300110613     C                   clear                   Instradamento     1
131400100701      *
131500100701      * Un errore nel dettaglio
131600100701     C                   clear                   Err_in_detta      1
131700100701     c                   clear                   errori_in_Wrt     1
131800100701      *
131900100910     c                   clear                   Primo_RFFcollo    1
132000100910     c                   clear                   Primo_RCUcollo    1
132100100910     c                   z-add     0             conta_rif         1 0
132200100910     c                   clear                   suVABrmo
132300100910      *
132400100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
132500100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
132600140610      *
132700140610     C*  Resetta l'informazione di spedizione di RESO
132800140610     c                   eval      era_un_export_RESO_dal_PARTNER ='N'
132900140610     c                   eval      Sped_RESO = 'N'
133000140610     c                   eval      Cod_RESO_ORM = *blank
133100140623     c                   eval      Salva_REF_ORM = *blank
133200100701     C*
133300100812     C* ---------------------------------------------------
133400100812     C* Inizializzazione Spedizione con campi da testata :
133500100812     C* ---------------------------------------------------
133600100811     C*  Data Richiesta Consegna  da Testata se c'è;
133700100811     C                   IF        DTM_DtConsRic > 0                            Data cons.rich. test
133800100811     C                   z-add     DTM_DtConsRic VABDCR                         Data cons.rich.
133900100811     C                   z-add     DTM_OraCnsRic VABHCR                         Ora  cons.rich.
134000100811     c                   end
134100100812     C*
134200100812     c                   endSR
134300100812     C*----------------------------------------------------------------
134400100812      *? cerca Nazione del primo Mittente nella sequenza dettagli
134500100812     C*----------------------------------------------------------------
134600100812     C     cerca_MittenteBEGSR
134700100812      *
134800100812     c                   z-add     vnrec         rec_posiz         9 0
134900100812     c                   move      *blank        Naz_mittente      3
135000100812     c                   movel(p)  UNB_Generico  PT_Parziale      35
135100100812     c                   move      'N'           OK_Nazione        1
135200100812      *
135300100812      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
135400100812     c                   call      'TRTCT96R1'
135500100812     c                   parm                    rec_posiz
135600100812     c                   parm                    PT_Parziale
135700100812     c                   parm                    Naz_mittente
135800100812     c                   parm                    Ok_Nazione
135900100812     c                   parm                    User_msg
136000100812     C                   parm                    keyUNBCLI
136100100812     C                   parm      'IFCSUM'      keyTIPOMSG
136200100812     C                   parm      'D'           keyVERSION
136300100812     C                   parm      '96A'         keyRELEASE
136400100812     C                   parm      'UN'          keyAGENCY
136500100812     C                   parm                    keyASSOCIA
136600100812      *
136700100812      *  Re-inizializza l'indicatore di trovato
136800100812     c                   setoff                                       32
136900100812      *
137000100812      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
137100100812      *   da decodificare come UNB.
137200100812     c                   if        Ok_Nazione = 'S'
137300100812     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
137400100812     C                   eval      Nr_PT = 1
137500100812     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
137600100812     c                   Endif
137700100812      *
137800100812     c                   endSR
137900100812     C*----------------------------------------------------------------
138000100812      *? esito ricerca tabella PT decodificata oppure NO
138100100812     C*----------------------------------------------------------------
138200100812     C     esito_tab_PT  BEGSR
138300100812      *
138400100812     C                   move      'N'           PT_trovata
138500100812     C   32              move      'S'           PT_trovata
138600100812      *
138700100812      * ******  Errore generale sul Messaggio
138800100812      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
138900100812      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
139000100812     C                   If        PT_trovata = 'N'
139100100812      * Msg di allerta Traduzione
139200100812     C                   EVAL      Indirizzi = CED_Bart
139300100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
139400100921     C                   EVAL      Messaggio = 'EDI -
139500100921     C                             IFCSUM D96A - msg su UPLOAD -
139600100921     C                             con UNB non presente in tabella PT..'
139700100812      *
139800100812     c                   exsr      invio_mail
139900100812     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
140000100812     C                   eval      vinFLG = '2'
140100110208     c                   eval      Almeno_Un_Due ='S'
140200100812     C                   eval      se_errore   = 'S'
140300100812      *
140400100812      *             *------------------------------*
140500100812     C                   eLSe
140600100812      *             *------------------------------*
140700100812      *
140800100812     c                   exsr      Se_Trovata_PT
140900100812      *
141000100812      *
141100100812     c                   Endif
141200100812      *
141300100701     c                   endSR
141400090210     C*----------------------------------------------------------------
141500090210      *? DECODIFICA DATI UNB
141600090210     C*----------------------------------------------------------------
141700100812     C     Se_Trovata_PT BEGSR
141800090210      *
141900100812      * Se tutto OK su tab.PT:
142000100812      *
142100100812     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
142200100812     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
142300100812     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
142400120629     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
142500100812      *
142600100812      *  Campi impostati come default:
142700100812      *  Se ho AGE/ON.... prendo i riferimenti definito su tab.PT il tipo di
142800100812      *  qualificatore da utilizzare come facciamo x RFF+AGE con EEX.
142900100812      *  Per default se non impostato diventa automaticamente un AGE.
143000100812     c                   if        §PVRFF = *blank
143100100909     c                   eval      §PVRFF = Riferimento_Spedizione
143200100909     c                   end
143300100813     c                   eval      Riferimento_Spedizione = §PVRFF
143400100910     c                   eval      RFF_DalPrimoCollo      = §PVRFS
143500100910     c                   eval      RFF_DopoIlPrimoCollo   = §PVRSE
143600100909      *
143700100812      *  Anche per il numerico prendo come default "CU"
143800100812     c                   if        §PVRCU = *blank
143900100909     c                   eval      §PVRCU = Riferimento_Numerico
144000100909     c                   end
144100100813     c                   eval      Riferimento_Numerico   = §PVRCU
144200100812      *
144300100812      *  Default per i campi Free Text nel dettaglio 'SIN' e 'ICN'
144400100812     c                   if        §PVsin = *blank
144500100812     c                   eval      §PVsin = 'SIN'
144600100812     c                   end
144700100812      *
144800100812     c                   if        §PVicn = *blank
144900100812     c                   eval      §PVicn = 'ICN'
145000100812     c                   end
145100100812      *
145200100812      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
145300100812      *  ricevuti:
145400100812     c                   movel     §PVema        mail_ind         44
145500100812      *
145600100812      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
145700100812     c                   if        mail_ind <> *blank
145800100812     c                   exsr      imp_ADDR_mail
145900100812     c                   end
146000100812      *
146100100812      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
146200100812      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
146300100812      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
146400100812     C                   if        §PULUN = *blank
146500100812     C                   eval      §PULUN = '00'
146600100812     c                   end
146700100812      *
146800100812      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
146900100812      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
147000100812     C                   move      §puLUN        PU_lunVAT
147100100812     C                   move      §puTS         PU_TipoServ
147200100812     C                   move      §puSS         PU_SpecServ
147300111028     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO = §puRES
147400120629     C                   eval       PZ_abilita_NAD_SF_come_RFF_FF = §pzNADsf
147500100812      *
147600100812     C                   Z-ADD     §PTKSC        PT_vabCCM
147700100812     C                   z-add     §PTLNP        PT_vabLNP
147800100812     C                   z-add     §PTNRS        PT_vabNRS
147900100812     C                   MOVEL     §PTCTM        PT_vabCTM
148000100812      *
148100100812      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
148200100812      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
148300100812      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
148400100812      *  i records trasmessi).
148500100812     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
148600100812      *
148700100812      *  In base al cod.trattamento merce reperisco tipo tratt.
148800100812     C                   CLEAR                   DS1B
148900100812     C                   Z-ADD     1             Y                 3 0
149000100812     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
149100100812     C     *IN32         IFEQ      '1'
149200100812     C                   MOVEL     Des_Tb_CTM(Y) DS1B
149300100812     C                   MOVEL     DS1B          sav_DS1B
149400100812     C                   END
149500100812      *
149600100812      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
149700100812     C                   clear                   ACORAG
149800100812     C                   clear                   INDCAE
149900100812     C                   clear                   INDCAP
150000100812     C                   clear                   INDSTA
150100100812      * Decodifico partner
150200100812     C                   Z-ADD     1             KKUT
150300100812     C                   Z-ADD     KCI           KKCC
150400100812     C                   MOVE      §PTKSC        KKSC
150500100812     C     KACO          CHAIN     CNACO00F                           31
150600100812     C     KIND          CHAIN     CNIND00F                           31
150700100812      *
150800100812      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
150900100812      *   questa schiera è utilizzata anche per i clienti.
151000100812     C     §ptKSC        LOOKUP    skCLienti                              39
151100100812     c                   If        *in39 = *off
151200100812     C                   add       1             sk
151300100812     C                   z-add     §ptKSC        skCLienti(sk)
151400100812     C                   Endif
151500100812      *
151600100812     c                   endSR
151700100812     C*----------------------------------------------------------------
151800100812      *? DECODIFICA DATI UNB
151900100812     C*----------------------------------------------------------------
152000100812     C     DATI_UNB      BEGSR
152100100812      *
152200100701      * identificativo Messaggio
152300100701     C                   eval      UNB_NrTrasmiss = unb0020
152400100812      *
152500100812     C                   MOVEL     unb0017       Anno2             2 0
152600100812     C                   z-add     2000          Anno4             4 0
152700100812     C                   add       Anno2         Anno4
152800100812     C                   MOVE      unb0017       WGG               2 0
152900100812     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
153000100812     C                   MOVE      Anno2         Data_messag
153100100812     C                   MOVEL     WGG           Data_messag
153200100812     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
153300100812     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
153400100812     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
153500100812      *
153600100812     c                   clear                   Mittente         35
153700100812     c                   clear                   Mitt_Qualifier    4
153800100812     c                   clear                   Id_Messaggio     14
153900100812     C                   eval      Mittente       = unb0004
154000100812     C                   eval      Mitt_qualifier = unb000720
154100100812     C                   eval      Id_Messaggio   = UNB0022
154200100701      *
154300090209      ** ---------------------------------------------------------------
154400090209      * quindi 1° cosa:
154500090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
154600090209      *     senza questo il messaggio NON è trascodificabile
154700100630     C                   MOVEL     unb0004       UNB_diWork
154800100630     C                   MOVE      unb000720     UNB_diWork
154900100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
155000100630     C                   Z-ADD     1             Nr_PT
155100100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
155200090209      *
155300100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
155400100317      *  la Nazione:
155500100317     C                   If        *IN32 = *off
155600100630     C                   eval      Nr_PT = 1
155700100630     c                   clear                   UNB_diWork
155800100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
155900100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
156000100317      *
156100100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
156200100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
156300100317      *  per cercare di individuare con certezza il codice della tabella PT in base
156400100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
156500100317     C                   If        *IN32 = *on
156600100317      *
156700100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
156800100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
156900100317      * ?  su Partner che hanno + linee con + nazioni.
157000100812      *  se è considerato Parziale
157100100812      *   Salva il codice generico
157200100630     c                   eval      UNB_parziale = 'S'
157300100812     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
157400100812      *             *------------------------------*
157500100812     c                   exsr      cerca_Mittente
157600100812      *             *------------------------------*
157700100812     C                   eval          Naz_salvata = Naz_mittente
157800100317     c                   Endif
157900100317     c                   Endif
158000100812      *
158100100812      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
158200100812      *             *------------------------------*
158300100812     c                   exsr      esito_tab_PT
158400100812      *             *------------------------------*
158500100920      **
158600100920     C                   if        se_errore   = 'S'
158700100920     c                   goto      end_UNB
158800100920     c                   end
158900100812      **
159000100812      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
159100100812     C                   If        PT_trovata = 'S'
159200100812      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
159300100812      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
159400100812      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
159500100812      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
159600100812      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
159700100812     c                   movel     UNB_diWork    UNB_Parz
159800100812     C                   z-add     Nr_PT         savXX_PT_key
159900100812      * ?- Se ha più LINEE da gestire                                      ------ */
160000100812     C                   eval      PT_con_piu_Nazioni = §puPLN
160100100812     c                   end
160200090209      **
160300090210     c     end_UNB       endSR
160400090227      * ?================================================================== */
160500090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
160600090227      * ?================================================================== */
160700090227     C     DATI_UNH      BEGSR
160800100316      *
160900100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
161000100701      *
161100090211      ** Riferimento del cliente
161200100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
161300100701      * ?                                       ------------
161400110707     C                   MOVEL(p)  UNH_Rifer_Msg UNH_VABrma
161500100701      * ?                                       ------------
161600100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
161700100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
161800100701     C                   SUB       1             lunghezza_num
161900100701     C                   EXSR      Se_Numerico
162000100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
162100090211      * ?                                       --------
162200100701     C                   MOVEL     Numero_OK     UNH_VABrmn
162300090211      * ?                                       --------
162400090211     C                   END
162500090303      **
162600090210     c     end_UNH       endSR
162700100701      * ?================================================================== */
162800100701      *? dati iniziali   BGM
162900100701      * ?================================================================== */
163000100701     C     DATI_BGM      BEGSR
163100100701      *
163200100701      *  identificativo testata
163300100706     c                   eval       BGM_Id_Testata = BGM1004
163400100706     C                   IF           BGM_Id_Testata <> *blank
163500100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
163600100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
163700100706     c                   end
163800100706      *
163900100701      **
164000100701     c                   endSR
164100100701      * ?================================================================== */
164200100701     C*?  Contatori in testata o in Dettaglio
164300100701      * ?================================================================== */
164400100701     C     DATI_CNT      BEGSR
164500100701      **
164600100701     c                   IF        Dati_di_Testata  ='S'
164700100701      **
164800100701      *  TOTALE colli, peso, spedizioni:
164900100701     c                   if        cnt6069 = Totale_Peso_Lordo
165000100811     c                   z-add     cnt6066       Tot_Peso_msg     15 3
165100100701     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
165200100811     c                   z-add     cnt6066       Tot_Sped_msg     15 3
165300100701     c                   elseIf    cnt6069 = Totale_Nr_Colli
165400100811     c                   z-add     cnt6066       Tot_Colli_msg    15 3
165500100701     c                   elseIf    cnt6069 = Totale_Peso_Netto
165600100811     c                   z-add     cnt6066       Tot_PNetto_msg   15 3
165700100701     c                   end
165800100701      **
165900100813     c                   elseIF    Dati_di_Testata <>'S'
166000100813     C* Totali della spedizione: dentro il Dettaglio
166100100813      **
166200100813     C*  totale peso
166300100813     c                   if        §pvPEs  = 'S'
166400100813     C*
166500100813     c                   if        CNT6069 = Totale_Peso_Lordo
166600100813      *          Kilogrammi
166700100813     c                   if        CNT6411 = Kilograms
166800100813     C                   Z-ADD     CNT6066       vabPKB                          Peso
166900100813      *          Grammi
167000100813     c                   elseIF    CNT6411 = grams
167100100813     C     CNT6066       div       1000          vabPKB                          Peso
167200100813     c                   end
167300100813     c                   end
167400100813      **
167500100813     c                   end
167600100813     C*  totale colli
167700100813     c                   if        §pvCOl  = 'S'
167800100813     C*
167900100813     c                   if        cnt6069 = Totale_Nr_Colli
168000100813     C                   Z-ADD     cnt6066       vabNCL                          N.Colli
168100100813     C                   ADD       VABNCL        Tot_Colli
168200100813     c                   end
168300100813      **
168400100813     c                   end
168500100813      *
168600100813      **
168700100701     C                   END
168800100701      **
168900100701     c     end_CNT       endSR
169000100701      * ?================================================================== */
169100100909     C*?  i Riferimenti Spedizione / Clienti particolari testata/dettaglio
169200100701      * ?================================================================== */
169300100701     C     DATI_RFF      BEGSR
169400120424      *
169500120424      *   Cerca la decodifica del dato in TABELLA
169600120424     C                   movel(p)  Tipo_Seg      etbSGM
169700120424     C                   movel(p)  RFF1153       etbVALSGM
169800120424     c                   exsr      In_TAB_EDSTBL
169900100701      **
170000100702      * ?   X_Cargo_Manifest o X_Numero_CMR
170100100701      **
170200100701     C*  Se tipo documento CMR ('CMR')
170300100701     C                   if        RFF1153 =  X_Cargo_Manifest
170400100701     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
170500120629      **
170600100701     C*  Se tipo documento AFB (Foglio viaggio)
170700100701     C                   elseIF    RFF1153 =  X_Numero_CMR
170800100701     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
170900100701     C                   END
171000100910     C*
171100120424     C*  Se tipo riferimento Particolare   x CMR/F.Viaggio
171200120424     C                   IF        Quale_campo ='RFFTES'
171300120424     c                              and RFF1153 <> *blank
171400120424     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
171500120424     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
171600120629      **
171700120424     C                   END
171800100910      *  --------------------------*
171900100910      *  Prende il riferimento se non era stato precedentemente caricato
172000100910      *     o se sulla PT c'è la forzatura a livello segnacollo.
172100100910     c                   if        Primo_RFFcollo = *blank
172200100701      *
172300100909      * ? Riferimento singola spedizione
172400100909     C*------------------
172500100910      * ?Riferimento AlfaNumerico preso dal primo dei segnacolli
172600100909     C*------------------
172700100910     C                   IF             Rifer_Alfanum = *BLANK or
172800100910     C                              RFF_DalPrimoCollo = 'S'
172900100910     C*--------
173000100701     c                   if        RFF1153 = Riferimento_Spedizione  and
173100100701     c                                RFF1154 <> *BLANKS
173200100701     C                   movel(p)  RFF1154       Rifer_Alfanum
173300100701      ** controlla se contiene solo numeri
173400100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
173500100701     C                   movel     Rifer_Alfanum test_num
173600100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
173700100701     C                   sub       1             lunghezza_num
173800100701     C                   EXSR      Se_Numerico
173900100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
174000110707     C                   movel(p)  Rifer_Alfanum VABRMA
174100100910     c                   eval      Primo_RFFcollo = 'S'
174200100910      *
174300100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
174400100701     C                              Tipo_Rifer = *blanks
174500100701     C                   movel     Numero_OK     VABRMN
174600100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
174700100701     C                   ENDIF
174800100701     C                   ENDIF
174900100701     C                   ENDIF
175000100910     C                   ENDIF
175100100910      *
175200100910     c                   else
175300100910      *
175400100910      * Se ci sono ulteriori Riferimenti e sono da riportare su VABRMO
175500100910     c                   if        RFF_DalPrimoCollo ='S' and
175600100910     c                             RFF_DopoIlPrimoCollo ='S' and
175700100910     c                             RFF1153 = Riferimento_Spedizione and
175800100910     c                             RFF1154 <> *BLANKS and
175900100910     c                             RFF1154 <> Rifer_Alfanum
176000100910      *
176100100910     c                   eval      conta_rif = conta_rif + 1
176200100910      *
176300100910      *  al max possono essere 2 riferimenti differenti dal principale
176400100910     c                   if        conta_rif <= 2
176500100910     C                   movel     RFF1154       Secondo_Rifer
176600100910      *
176700100910     C                   IF         Secondo_Rifer <> *BLANKS
176800100920     C     ' '           scan      Secondo_Rifer WLUNGH            3 0
176900100910     C                   sub       1             WLUNGH
177000100910      *
177100100910     c                   if        conta_rif = 1
177200100910     c                   clear                   SUvabrmo
177300100910     c                   end
177400100910      *
177500100910     C                   eval      SUvabRMo = %Trim(suVABRMo) + '.' +
177600100910     C                             %Subst(Secondo_Rifer:1:WLUNGH)
177700100910     C                   ENDIF
177800100910     c                   endIF
177900100910      *
178000100910     c                   end
178100100910     c                   endIF
178200100910      *
178300100910     C*----------------------------------------------------------------
178400100910      *  Se ho CU prendo il riferimento numerico
178500100910      *  ECCEZIONE: se ho già impostato FF obbligatorio
178600100910     c                   if        Primo_RCUcollo = *blank
178700100910      *
178800100910     c                   if        Rifer_Numeric = *blank or
178900100910     c                             RFF_DalPrimoCollo = 'S'
179000100909     C*------------------
179100100909      * ?Riferimento Numerico
179200100909     C*------------------
179300100909     C                   if        RFF1153 = Riferimento_Numerico
179400100909     C                              and RFF1154 <> *blanks
179500100909     C                   movel     RFF1154       Rifer_Numeric
179600100909      * Numero Ordine di vendita
179700100909     C                   movel     Rifer_Numeric test_num
179800100909     C     ' '           scan      Rifer_Numeric lunghezza_num
179900100909     C                   sub       1             lunghezza_num
180000100909     C                   EXSR      Se_Numerico
180100100909      *
180200100909     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
180300100910     c                   eval      Primo_RCUcollo = 'S'
180400100909     C                   movel     Numero_OK     VABRMN
180500100909     C                   eval       Tipo_Rifer = Riferimento_Numerico
180600111028      ****
180700100909     C                   Endif
180800100909      *
180900100909     C                   ENDIF
181000100909      *
181100100910     C                   ENDIF
181200100910      *
181300100910     c                   else
181400100910      *
181500100910      * Se ci sono ulteriori Riferimenti Numerici non vengono considerati
181600100910     c                   if        RFF_DalPrimoCollo ='S' and
181700100910     c                             RFF_DopoIlPrimoCollo ='S' and
181800100910     c                             RFF1153 = Riferimento_Spedizione and
181900100910     c                             RFF1154 <> *BLANKS and
182000100910     c                             RFF1154 <> Rifer_Numeric
182100100910      *    ??????
182200100910     c                   end
182300100910      *
182400100910     C                   EndIF
182500100910      *
182600100910     C*----------------------------------------------------------------
182700100909      * NOn c'è l'identificativo bolla del cliente
182800100909     c                   if        (RFF1153 = Riferimento_Spedizione and
182900100909     c                              RFF1154 = *BLANKS)
183000100909     c                             or
183100100909     c                             (RFF1153 = Riferimento_Numerico and
183200100909     c                              RFF1154 = *BLANKS)
183300100909     C                   eval      vinMSG = 'RFF riferimento assente'
183400100909     c                   exsr      Error_DET
183500100909     c                   goto      end_RFF
183600100909     C                   EndIF
183700120629      *>>>>>>>>
183800100909     C*------------------
183900100909      * ?Codice Cliente di riferimento
184000100909     C*------------------
184100110224     C                   clear                   Rifer_Cliente    35
184200100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
184300100701     C                              and RFF1154 <> *BLANKS
184400120426     c                               or
184500120426     C                             Quale_campo ='RFFCLI'  and
184600120426     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
184700120629      *
184800100701     C                   movel     RFF1154       Rifer_Cliente
184900120629      *
185000100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
185100120629     c                   exsr      CL_Particolare
185200120629     C*------------------
185300120426      *>>>>>>>>
185400100702     C                   ENDIF
185500100909      *
185600140610      * ?  A RICEVERE una BOLLA come RESO o BOLLA da ORM
185700140610      * SE IL PARTNER NON VUOLE UTILIZZARE il riferimento NUMERICO e vuole
185800140610      * dare un codice Riferimento PARTICOLARE tabellato su EDSTBL00F x identifare
185900140610      *  un RESO o un ORM
186000140610      *         Memorizzo il codice passato sul campo di 14
186100140610      *  che contiene il nostro identificativo (ex BOLLA Export o ORM export)
186200140610      *
186300140610     C                   if        Quale_campo ='RESORM'
186400140610     c                               and RFF1154 <> *BLANKS
186500140623     C                   movel(p)  RFF1154       salva_REF_ORM
186600140610      *
186700140610      * deve essere ASSOLUTAMENTE un CODICE NUMERICO di 14
186800140610     C                   movel(p)  RFF1154       TEST_Numerico    35
186900140610      *
187000140610     C                   movel     TEST_Numerico test_num
187100140610     C     ' '           scan      TEST_Numerico lunghezza_num
187200140610     C                   sub       1             lunghezza_num
187300140610     C                   EXSR      Se_Numerico
187400140610      *
187500140610      * riferimento restituito da Partner  --> lo imposta solo se numerico
187600140610     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
187700140610      * attenzione Numero_OK (lungo 15) quindi "MOVE"!
187800140610     C                   move      Numero_OK     Cod_RESO_ORM
187900140610     C                   Endif
188000140610      *
188100140610     C                   END
188200140610      *
188300100701     c     end_RFF       endSR
188400100701      * ?================================================================== */
188500100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
188600100701      * ?================================================================== */
188700100701     C     DATI_DTM      BEGSR
188800100705      *
188900100705      *   Cerca la decodifica del dato in TABELLA
189000120424     C                   movel(p)  Tipo_Seg      etbSGM
189100100705     C                   movel(p)  dtm2005       etbVALSGM
189200100705     c                   exsr      In_TAB_EDSTBL
189300100705      *
189400100701     C*  Se data documento  memorizzo numero e data x CMR
189500100705     C                   If        Quale_campo ='DATDOC'
189600101015     c                              and dtm2380 <> *blank
189700100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
189800100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
189900100702     C                   EXSR      Converte_Data
190000100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
190100100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
190200100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
190300100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
190400100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
190500100701     C     WERRO         IFEQ      'S'
190600100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
190700100706     C                             dtm2005  +
190800100702     C                              ') data errata'
190900100701     c                   exsr      Error_DET
191000100701     c                   goto      end_DTM
191100100701     C                   END
191200100701     C                   END
191300100702     C*
191400100702     C* Data Partenza del mezzo
191500100705     C                   If        Quale_campo ='DATPAR'
191600101015     c                              and dtm2380 <> *blank
191700100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
191800100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
191900100702     C                   EXSR      Converte_Data
192000100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
192100100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
192200100702     C     WERRO         IFEQ      'S'
192300100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
192400100706     C                             dtm2005  +
192500100702     C                              ') data errata'
192600100702     c                   exsr      Error_DET
192700100702     c                   goto      end_DTM
192800100702     C                   END
192900100702     C                   END
193000100702     C*
193100100702     C* Data Previsto Arrivo del mezzo
193200100705     C                   If        Quale_campo ='DATARR'
193300101015     c                              and dtm2380 <> *blank
193400100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
193500100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
193600100702     C                   EXSR      Converte_Data
193700100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
193800100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
193900100702     C     WERRO         IFEQ      'S'
194000100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
194100100706     C                             dtm2005  +
194200100702     C                              ') data errata'
194300100702     c                   exsr      Error_DET
194400100702     c                   goto      end_DTM
194500100702     C                   END
194600100702     C                   END
194700100701      **
194800100701     C*  Se data consegna richiesta imposto già sui campi del VAB
194900100705     C                   If        Quale_campo ='VABDCR'
195000101015     c                              and dtm2380 <> *blank
195100100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
195200100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
195300100702     C                   EXSR      Converte_Data
195400100811      **
195500100811     c                   IF        Dati_di_Testata  ='S'
195600100811     C                   MOVEL     WCAMG         DTM_DtConsRic                  Data cons.rich.
195700100811     C                   MOVEL     WHMS          DTM_OraCnsRic                  Ora  cons.rich.
195800100811     c                   end
195900100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
196000100811     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
196100100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
196200100701     C     WERRO         IFEQ      'S'
196300100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
196400100706     C                             dtm2005  +
196500100702     C                              ') data errata'
196600100701     c                   exsr      Error_DET
196700100701     c                   goto      end_DTM
196800100701     C                   END
196900100702     C                   end
197000100701      **
197100100701     c     end_DTM       endSR
197200100813      * ?================================================================== */
197300100813     C*?  Tipo Servizio
197400100813      * ?================================================================== */
197500100813     C     DATI_TSR      BEGSR
197600100813      ***
197700100813     C*  Reperisco priorità spedizione:
197800100813     C                   Z-ADD     1             X
197900100813      ***
198000100813      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
198100100813      ***  mentre in tabella EDI EEX è fatto con "1"
198200100813      ***
198300100813     c                   movel(p)  TSR4219       Work_Data__35
198400100813     c                   move      PU_TipoServ   Work_Data__35
198500100813     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
198600100813     C     *IN32         IFEQ      '1'
198700100813      *
198800100813     c                   if        Dati_di_Testata <>'S'
198900100813     C                   eval             VABTSP = TipoServizio(X)
199000100813     c                   elseIf    Dati_di_Testata = 'S'
199100100813     C                   eval         tes_VABtsp = TipoServizio(X)
199200100813     c                   end
199300100813      *
199400100813     C                   ELSE
199500100813      **  Il tipo bolla non è presente
199600100813     C                   eval      vinMSG = 'TSR non in tabella'
199700100813     c*********          exsr      Error_DET
199800100813     c*********          goto      end_TSR
199900100813     C                   END
200000100813     C*
200100100813     C*  Decodifico servizi speciali
200200100813     C     TSR7085       IFNE      *BLANKS
200300100813     C     TSR7085       ANDNE     *LOVAL
200400100813      *
200500100813     C                   Z-ADD     1             X
200600100813     c                   movel(p)  TSR7085       Work_Data__35
200700100813     c                   move      PU_SpecServ   Work_Data__35
200800100813     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
200900100813      * Particolarità
201000100813     C     *IN32         IFEQ      '1'
201100100813     C                   eval       EDIDSSS = UNI_ServSpec(X)
201200100813      * Descrizione
201300100813     C     §SSNOT        IFEQ      'S'
201400100813     c                   if        Dati_di_Testata <>'S'
201500100813     C                   MOVEL     §SSDES        VABNOT
201600100813     c                   elseIf    Dati_di_Testata = 'S'
201700100813     C                   eval       tes_VABnot = §SSDES
201800100813     c                   end
201900100813     C                   END
202000100813      *
202100100813      * x Espresso     "Pre-noon"
202200100813     C     §SSTPS        IFEQ      'S'
202300100813     c                   if        Dati_di_Testata <>'S'
202400100813     C                   MOVEL     'E'           VABTSP
202500100813     c                   elseIf    Dati_di_Testata = 'S'
202600100813     C                   eval         tes_VABtsp = 'E'
202700100813     c                   end
202800100813     C                   END
202900110509      *
203000110509      * x H10:30       "10H"
203100110509     C     §SSTPS        IFEQ      'H'
203200110509     c                   if        Dati_di_Testata <>'S'
203300110509     C                   MOVEL     'H'           VABTSP
203400110509     c                   elseIf    Dati_di_Testata = 'S'
203500110509     C                   eval         tes_VABtsp = 'H'
203600110509     c                   end
203700110509     C                   END
203800100813      *
203900120403      * x Appuntamento "Booking ins"  "BKI"
204000100813     C     §SSTPS        IFEQ      'A'
204100100813     c                   if        Dati_di_Testata <>'S'
204200120403     c                   if             vabTC1 = *blank
204300100813     C                   MOVEL     'A'           VABTC1
204400120403     c                   elseIF         vabTC2 = *blank
204500120403     C                   MOVEL     'A'           VABTC2
204600120403     c                   end
204700100813     c                   elseIf    Dati_di_Testata = 'S'
204800120403     c                   if         tes_VABtc1 = *blank
204900100813     C                   eval         tes_VABtc1 = 'A'
205000120403     c                   elseIF     tes_VABtc2 = *blank
205100120403     C                   eval         tes_VABtc2 = 'A'
205200120403     c                   end
205300100813     c                   end
205400100813     C                   END
205500100813      *
205600120214      *
205700120214      * x FERMO DEPOSITO "Goods Waiting Collection"  GWC
205800120214     C     §SSTPS        IFEQ      'F'
205900120214     c                   if        Dati_di_Testata <>'S'
206000120214     C                   MOVEL     'S'           VABFFD
206100120214     c                   elseIf    Dati_di_Testata = 'S'
206200120214     C                   eval         tes_VABffd = 'S'
206300120214     c                   end
206400120214     C                   END
206500120214      *
206600100813     C                   ELSE
206700100813      *
206800100813      **  Il tipo bolla non è presente
206900100813     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
207000100813     c*******            exsr      Error_DET
207100100813     c*******            goto      end_TSR
207200100813      *
207300100813     C                   END
207400100813      *
207500100813     C                   END
207600100813      **
207700100813     c     end_TSR       endSR
207800100813      * ?================================================================== */
207900100813     C*?  C.O.D. Monetary Amount
208000100813      * ?================================================================== */
208100100813     C     DATI_MOA      BEGSR
208200100813      *
208300100813      *   Cerca la decodifica del dato in TABELLA
208400120424     C                   movel(p)  Tipo_Seg      etbSGM
208500100813     C                   movel(p)  moa5025       etbVALSGM
208600100813     c                   exsr      In_TAB_EDSTBL
208700100813      *
208800100813     C* Controllo se campo importo numerico con 3 decimali
208900100813     C     moa5004       IFNE      *zeros
209000100813      *
209100100813     C                   If        Quale_campo ='VABIAS'
209200130529      *
209300130529      * il campo è diviso in 2 flag: il primo serve per l'importo da assicurare
209400130529      *    quindi se deve essere considerato è espresso sulla PT.
209500130529     c                   if        %subst(§PTxxx:1:1) = 'S'
209600110726     C     moa5004       div       1000          VABIAS                         IMPORTO
209700100813     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
209800130529     c                   end
209900100813      *
210000100813     C                   elseIF    Quale_campo ='VABVMD'
210100110726     C     moa5004       div       1000          VABVMD                         MERCE
210200100813     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
210300100813      *
210400100813     C                   elseIF    Quale_campo ='VABCAS'
210500100813     C     §PUCAS        IFEQ      'S'                                          C/Assegno
210600100813      *
210700110726     C     moa5004       div       1000          VABCAS
210800100813     C                   MOVEL     moa6345       VABVCA
210900100813     c                   if        Tes_valuta <> *blank and VABVCA = *blank
211000100813     C                   MOVEL     Tes_valuta    VABVCA
211100100813     c                   end
211200110628      **
211300110628      **  se c'è il default ("??")
211400110628     C                   Z-ADD     1             Y
211500110628     c                   movel(p)  '??'          Work_Data__35
211600110628     c                   move      §puTC         Work_Data__35
211700110628     c                   clear                   WData_35_2        2
211800110628     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
211900110628     c                   if        not *in33
212000110628      *
212100130311      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO ( ma deve leggere il segmento
212200130311      * contenente il Tipo Incasso - sempre successivo al segmento MOA )
212300130311      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
212400130311      *
212500130311      * ATTENZIONE viene impostato di Default ma, in presenza del FTX+PAI,
212600130311      *  viene reimpostato in base a quello sulla tabella "TC".
212700130311      *   Quindi viene modificato da "OM" ....a quello sul FTX+PAI successivo.
212800100813     C     §PUTPC        IFEQ      'P'
212900100813     C                   MOVEL     'OM'          VABTIC
213000100813     C                   MOVEL     'OM'          VABtic_work
213100100813     C                   ENDIF
213200110628      **
213300110628     c                   end
213400100813      *
213500100813     C                   END
213600100813      *
213700100813     C                   EndIF
213800100813     C                   END                                                    Imp.non numer.
213900100813      *
214000100813     c     end_MOA       endSR
214100100813      * ?================================================================== */
214200100813     C*?  FREE TEXT
214300100813      * ?================================================================== */
214400100813     C     DATI_FTX      BEGSR
214500100813      *
214600100813      *   Cerca la decodifica del dato in TABELLA
214700120424     C                   movel(p)  Tipo_Seg      etbSGM
214800100813     C                   movel(p)  ftx4451       etbVALSGM
214900100813     c                   exsr      In_TAB_EDSTBL
215000100813      *
215100100813      *
215200100813      * ? solo se si tratta di note descrittive da comunicare in BOLLA
215300100813     c                   if         quale_campo = 'VABNOT'
215400100813     c                                or ftx4451 = §pvSIN
215500100813     c                                or ftx4451 = §pvICN
215600100813     C                   DO        4             X
215700100813      *
215800100813     C     D05(X)        IFNE      *BLANKS
215900100813      *
216000100813     C     Note_1        IFEQ      *BLANKS
216100100813     C                   MOVEL     D05(X)        Note_1
216200100813     C                   MOVE      D05(X)        Note_2
216300100813     C                   ELSE
216400100813     C     Note_2        IFEQ      *BLANKS
216500100813     C                   MOVEL     D05(X)        Note_2
216600100813     C                   END
216700100813     C                   END
216800100813      *
216900100813     C                   ENDif
217000100813      *
217100100813     C                   ENDdo
217200100813     C                   end
217300100813     C*------>>
217400110907      * ? solo se si tratta di note x invio mail di avviso al destinatario
217500110907      *    (nuovo servizio a pagamento attivato nel 2011)
217600110907      *     dovranno essere scritti 2 records nel VAT di tipo I e J
217700110907     c                   if         quale_campo = 'VATNOT'
217800131025     c                                 or
217900131025     c                              quale_campo = 'VATEML'
218000131025      *
218100131025     c                   if        ftx4453 = 'N'
218200131025     c                   eval      FTX_MAIL_come_servizio = 'N'
218300131025     c                   end
218400110907      *
218500110907     C                   DO        4             X
218600110907      *
218700110907     C     D05(X)        IFNE      *BLANKS
218800110907      *
218900110907     C     Note_1t       IFEQ      *BLANKS
219000110907     C                   MOVEL     D05(X)        Note_1t
219100110907     C                   MOVE      D05(X)        Note_2t
219200110907     C                   ELSE
219300110907     C     Note_2t       IFEQ      *BLANKS
219400110907     C                   MOVEL     D05(X)        Note_2t
219500110907     C                   END
219600110907     C                   END
219700110907      *
219800110907     C                   ENDif
219900110907      *
220000110907     C                   ENDdo
220100110907      *
220200110907      * carica il campo da riportare poi sul VAT
220300110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
220400110907     c                             note_1T
220500110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
220600110907     c                             note_2T
220700110907     C                   end
220800131025     C*------>>
220900131025      * ? solo se si tratta di note x invio SMS di avviso al destinatario
221000131025      *    (nuovo servizio a pagamento attivato)
221100131025      *     dovrà essere scritto 1 record nel VAT di tipo S
221200131025     c                   if         quale_campo = 'VATSMS'
221300131025     c                   if        ftx4453 = 'N'
221400131025     c                   eval      FTX_SMS_come_servizio = 'N'
221500131025     c                   end
221600131025      *
221700131025     C                   DO        4             X
221800131025      *
221900131025     C     D05(X)        IFNE      *BLANKS
222000131025      *
222100131025     C     Note_1sms     IFEQ      *BLANKS
222200131025     C                   MOVEL     D05(X)        Note_1sms
222300131025     C                   MOVE      D05(X)        Note_2sms
222400131025     C                   ELSE
222500131025     C     Note_2sms     IFEQ      *BLANKS
222600131025     C                   MOVEL     D05(X)        Note_2sms
222700131025     C                   END
222800131025     C                   END
222900131025      *
223000131025     C                   ENDif
223100131025      *
223200131025     C                   ENDdo
223300131025      *
223400131025      * carica il campo da riportare poi sul VAT
223500131025     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
223600131025     c                             note_1sms
223700131025     C                   end
223800110907     C*------>>
223900100813     C*  Decodifico tipo pagamento C/Assegno
224000100813      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
224100100813      *      (Problema di pulizia in presenza di + righe di testo)
224200100813      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
224300100813      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
224400100813      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
224500100813      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
224600100813     C                   if        wri_vabtic = *blank
224700100813     C                   MOVEL     *BLANKS       VABTIC
224800100813     c                   end
224900100813      **
225000100813     c                   clear                   trovato_TPInc     1            TipoIncasso
225100100813      *
225200100813      * ?Se c'è il tipo Incasso
225300100813     c                   if         quale_campo = 'VABTIC'                      -- 01 --
225400100813      *
225500100813     C                   DO        4             X                              -- 02 --
225600100813     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
225700130311      **
225800130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
225900130311      **  per gestire Partner come AZKAR che ha codice "AAA"
226000130311     C**********         MOVEL     D05(X)        WCTC              2
226100130311     C                   MOVEL     D05(X)        WCTC             10
226200110623      **
226300110623     c                   eval      inviato_tipo_incasso ='S'
226400110623      **
226500110623      **  se non l'avesse trovato e fosse esplicitato un Default ("??")
226600110623      **   come per qualsisi codice venga inviato dal Cliente allora
226700110623      **   deve essere impostato il default (invece di fare un chiodo)
226800110623     C                   Z-ADD     1             Y
226900110623     c                   movel(p)  '??'          Work_Data__35
227000110623     c                   move      §puTC         Work_Data__35
227100110623     c                   clear                   WData_35_2        2
227200110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
227300110623     c                   if            *in33
227400110623     c                   move      'S'           trovato_TPInc
227500110623     C                   MOVEL     TipoIncasso(Y)vabtic
227600110623     c                   movel(p)  '??'          WData_35_2
227700110623     c                   else
227800110623      *
227900110623     C                   Z-ADD     1             Y
228000110623     c                   movel(p)  Wctc          Work_Data__35
228100130326     c                   move      §puTC         Work_Data__35
228200110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
228300110623     c                   if            *in33
228400110623     c                   move      'S'           trovato_TPInc
228500110623     C                   MOVEL     TipoIncasso(Y)vabtic
228600130326     c                   move      §puTC         WData_35_2        2
228700110623     c                   else
228800110623      *
228900110623     C                   Z-ADD     1             Y
229000110623     c                   movel(p)  Wctc          Work_Data__35
229100110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
229200110623     c                   if            *in33
229300110623     c                   move      'S'           trovato_TPInc
229400110623     C                   MOVEL     TipoIncasso(Y)vabtic
229500130311     c                   movel     Wctc          WData_35_2        2
229600110623     c                   end
229700110623      *
229800110623     c                   end
229900110623      *
230000110623     c                   end
230100110623      **
230200130311      *--------
230300130311      * SOLO SE PARTNER
230400130311      *   IMPOSTO 'OM'    (vecchia regola ormai in disuso se NON siano state
230500130311      *     trovate fra le tabelle dei codici ben precisi x il PARTNER,
230600130311      ***     ossia solo se NON E'STATO PASSATO un codice specifico
230700130311      ** ATTENZIONE NON DEVE ESISTERE IL BLANK in arrivo dal PARTNER ma sempre un CODICE.
230800110623     C     §PUTPC        IFEQ      'P'
230900110623     c     wctc          andne     'TO'
231000110623     c     WData_35_2    andeq     *blank
231100110623     C                   MOVEL     'OM'          VABTIC
231200110623     C                   END                                                    -- 03 --
231300110623     C                   END                                                    -- 03 --
231400110623     C                   ENDdo                                                  -- 02 --
231500100813      *
231600100813     C                   ELSE                                                   -- 01 --
231700100813      *
231800100813      * ?Se NON c'è il tipo Incasso Contrassegno particolare
231900100813      *    Deve essere impostato comunque da testata
232000110623     c                   if        vabtic = *blank   and
232100110623     c                             WData_35_2 = *blank
232200100813     C                   MOVEL     VABtic_work   VABTIC
232300100813     c                   end
232400100813      *
232500100813     C                   END                                                    -- 01 --
232600100813      *
232700100813      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
232800100813      *    essere presenti altri record Flat con altre informazioni
232900100813      *    che potrebbero abblancare il Tipo incasso precedentemente
233000100813      *    decodificato
233100100813     c                   if        Trovato_TPInc = 'S' and
233200100813     c                                 wri_vabtic = *blank
233300100813     c                   eval      wri_vabtic = 'S'
233400100813     c                   end
233500100813      *
233600100813      * ?Se Natura Merce      .
233700100910     C*  Natura Merce  (Se abilitato alla trascodifica)
233800100910     c                   if        §PUnas = 'S'
233900100813     c                   if         quale_campo = 'VABNAS'                      -- 01 --
234000100813     C                   MOVEL     D05(1)        VABnas
234100100813     c                   End
234200100910     c                   End
234300100813      *
234400120613      * ?Consegne Particolari
234500120613     c                   if         quale_campo = 'VABTCX'                      -- 01 --
234600120613      * Imposta il codice di 3 chrs.
234700120613      *   solo i primi 3 caratteri del campo
234800120613     C                   eval      controlla_COD_FTX = D05(1)
234900120613      *
235000120613      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
235100120613     C                   if        controlla_COD_FTX = 'FLR'
235200120613     c                   eval      FTX_ai_piani = 'P'
235300120613     c                   End
235400120613     c                   End
235500120613      *
235600120613      *
235700100813     c     end_FTX       endSR
235800100701      * ?================================================================== */
235900100701     C*? Imposta informazioni sul dettaglio del trasporto
236000100701      * ?================================================================== */
236100100701     C     DATI_TDT      BEGSR
236200100701     C*
236300100701      **
236400100701     c     end_TDT       endSR
236500100811      * ?================================================================== */
236600100811     C*? Imposta
236700100811      * ?================================================================== */
236800100811     C     DATI_TMP      BEGSR
236900100811     C*
237000100811      **
237100100811     c     end_TMP       endSR
237200100811      * ?================================================================== */
237300100811     C*? Imposta
237400100811      * ?================================================================== */
237500100811     C     DATI_TMD      BEGSR
237600100811     C*
237700100811      **
237800100811     c     end_TMD       endSR
237900100811      * ?================================================================== */
238000100811     C*? Imposta
238100100811      * ?================================================================== */
238200100811     C     DATI_TCC      BEGSR
238300100811     C*
238400100811      **
238500100811     c     end_TCC       endSR
238600100811      * ?================================================================== */
238700100811     C*? Imposta
238800100811      * ?================================================================== */
238900100811     C     DATI_DIM      BEGSR
239000100811     C*
239100100811      **
239200100811     c     end_DIM       endSR
239300100811      * ?================================================================== */
239400100811     C*? Imposta
239500100811      * ?================================================================== */
239600100811     C     DATI_RNG      BEGSR
239700100811     C*
239800100811      **
239900100811     c     end_RNG       endSR
240000100811      * ?================================================================== */
240100100811     C*? Imposta
240200100811      * ?================================================================== */
240300100811     C     DATI_DOC      BEGSR
240400100811     C*
240500100811      **
240600100811     c     end_DOC       endSR
240700100811      * ?================================================================== */
240800100811     C*? Imposta
240900100811      * ?================================================================== */
241000100811     C     DATI_GIN      BEGSR
241100100811     C*
241200100920      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
241300100920      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
241400100920     C     §1BF12        IFEQ      'E'
241500100920     C                   EXSR      SEGNaCollo_EEX
241600100920     C                   ELSE
241700100920      *
241800100920      *  La prima volta controllo congruità numero segnacollo
241900100920     C     §PUBS1        IFNE      *BLANKS
242000100920     C     VABnrs        ANDNE     0
242100100920      *  Se il codice trattamento merce prevede che segnacollino i
242200100920      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
242300100920     C     §1BFG7        IFEQ      'S'
242400100920     C     §1BFG1        OREQ      'S'
242500100920      *  calcolo segnacollo
242600100920     C                   EXSR      Segn_Bartolini
242700100920     C                   END
242800100920     C                   END
242900100920      *
243000100920     C                   END
243100100811      **
243200100811     c     end_GIN       endSR
243300100811      * ?================================================================== */
243400100811     C*? Imposta
243500100811      * ?================================================================== */
243600100811     C     DATI_HAN      BEGSR
243700100811     C*
243800100811      **
243900100811     c     end_HAN       endSR
244000100811      * ?================================================================== */
244100100811     C*? Imposta
244200100811      * ?================================================================== */
244300100811     C     DATI_GOR      BEGSR
244400100811     C*
244500100811      **
244600100811     c     end_GOR       endSR
244700100705      * ?================================================================== */
244800100705     C*? Imposta la Località
244900100705      * ?================================================================== */
245000100705     C     DATI_LOC      BEGSR
245100100705     C*
245200100705      **
245300100705     c     end_LOC       endSR
245400100705      * ?================================================================== */
245500100705     C*? Imposta identificativi delle Merci
245600100705      * ?================================================================== */
245700100705     C     DATI_EQD      BEGSR
245800100705     C*
245900100705      **
246000100705     c     end_EQD       endSR
246100100705      * ?================================================================== */
246200100705     C*? Imposta il numero delle Merci
246300100705      * ?================================================================== */
246400100705     C     DATI_EQN      BEGSR
246500100705     C*
246600100705      **
246700100705     c     end_EQN       endSR
246800100316      * ?================================================================== */
246900100316     C*? Imposta l'inizio del Dettaglio CNI
247000100316      * ?================================================================== */
247100100316     C     DATI_CNI      BEGSR
247200100316     C*
247300100702      ** Inizio Dettaglio riferimento iniziale
247400100702     c                   If        CNI1004 <> *blank
247500100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
247600100813      *
247700100813      *   Nel Riferimento Alfa sempre
247800100813     c                   eval      VABRMA  = CNI_Identificativo_Bolla
247900100813      *
248000100813      *   e nel numerico se di soli numeri e comunque se lo memorizza
248100100813     c                   clear                   CNI_seNumerico
248200100813     C                   eval       test_num = CNI1004
248300100813     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
248400100813     C                   SUB       1             lunghezza_num
248500100813     C                   EXSR      Se_Numerico
248600100813     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
248700100813      * ?                                       --------
248800100813     C                   MOVEL     Numero_OK     VABrmn
248900100813      * ?                                       --------
249000100813     C                   z-add     VABrmn        CNI_seNumerico
249100100813      * ?                                       --------
249200100813     C                   END
249300100813      *
249400100702     c                   end
249500100319      *
249600100316      **
249700100316     c     end_CNI       endSR
249800100316      * ?================================================================== */
249900100316     C*? Imposta le merci pericolose
250000100316      * ?================================================================== */
250100100316     C     DATI_DGS      BEGSR
250200100316     C*
250300100316      **
250400100316     c     end_DGS       endSR
250500100316      * ?================================================================== */
250600100316     C*? Imposta il numero seriale
250700100316      * ?================================================================== */
250800100316     C     DATI_SEL      BEGSR
250900100316     C*
251000100316      **
251100100316     c     end_SEL       endSR
251200100316      * ?================================================================== */
251300100316     C*? Imposta informazioni aggiuntive
251400100316      * ?================================================================== */
251500100316     C     DATI_PIA      BEGSR
251600100316     C*
251700100316      **
251800100316     c     end_PIA       endSR
251900090209      * ?================================================================== */
252000090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
252100090209      * ?================================================================== */
252200090209     C     DATI_TOD      BEGSR
252300090209      *
252400090210      *  Trascodifico tipo trasporto
252500090210     c                   clear                   TOD_char3         3
252600100705      *  DEFAULT da PT
252700090210      *  imposta il default dalla tabella PT se c'è
252800090210     C     §puPFA        ifNE      *blank
252900090210     c                   SELECT
253000090210      *  Porto Franco
253100090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
253200090210     C                   movel     '4'           VABCBO                         + C/Ass
253300090210     c                   WHEN      §puPFA = 'F'
253400090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
253500090210      *  Porto Assegnato
253600090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
253700090210     C                   movel     '6'           VABCBO                         + C/Ass
253800090210     C                   WHEN      §puPFA = 'A'
253900090210     C                   movel     '2'           VABCBO                         Senza C/Ass
254000090210     C                   ENDSL
254100090210     c                   end
254200090210      *
254300090210      *   Imposta o il codice o il metodo
254400090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
254500090210     c                   if        tod4053  <> *blank
254600090210     c                   movel(p)  tod4053       TOD_char3
254700090210     c                   else
254800090210     c                   if        tod4215  <> *blank
254900090210     c                   movel(p)  tod4215       TOD_char3
255000090210     c                   end
255100090210     c                   end
255200090210      **
255300110328      ****************
255400110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
255500110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
255600110328      ****************
255700110328     c                   eval      Trovata_Tab_TB ='N'
255800110328      *
255900110328     c                   if              TOD_char3 <> *blank
256000110328     c                   movel(p)  TOD_char3     Key_Generica35
256100110328     c                   move      §puTB         Key_Generica35
256200110328     C                   eval      Y = 1
256300110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
256400110328     c   32              eval      Trovata_Tab_TB ='S'
256500110328      *
256600110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
256700110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
256800110328      *
256900110328     c                   if          Trovata_Tab_TB ='N'
257000110328     c                                 and
257100110328     C                               tod4215 <> *blank
257200110328     c                   movel(p)  tod4215       TOD_char3
257300110328     c                   movel(p)  TOD_char3     Key_Generica35
257400110328     c                   move      §puTB         Key_Generica35
257500110328     C                   eval      Y = 1
257600110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
257700110328     c   32              eval      Trovata_Tab_TB ='S'
257800110328     c                   end
257900110328     c                   end
258000110328      *
258100110328      ****************
258200110328      ******* Se vi sono eccezioni sul cliente imposta quelle
258300110328      ****************
258400110328     c                   If          Trovata_Tab_TB ='S'
258500110328      *
258600110328     c                   SELECT
258700110328     c                   WHEN      Decod_Porto(Y) = 'F'
258800110328     c                               and  VABcas > *zeros
258900110328     C                   movel     '4'           VABCBO                         + C/Ass
259000110328     c                   WHEN      Decod_Porto(Y) = 'F'
259100110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
259200110328      *  Porto Assegnato
259300110328     c                   WHEN      VABCAS > *zeros
259400110328     C                   movel     '6'           VABCBO                         + C/Ass
259500110328     C                   OTHER
259600110328     C                   movel     '2'           VABCBO                         Senza C/Ass
259700110328     C                   ENDSL
259800110328      *
259900110328      ****************  altrimenti
260000110328      ******* Se non vi sono eccezioni per il cliente
260100110328      *   Rileva a questo punto lo Standard per tutti
260200110328      ****************
260300110328     c                   elseIf      Trovata_Tab_TB ='N'
260400110328      **
260500090210     c                   if              TOD_char3 <> *blank
260600100705      *   Cerca la decodifica del dato in TABELLA
260700120424     C                   movel(p)  Tipo_Seg      etbSGM
260800100705     C                   movel(p)  TOD_char3     etbVALSGM
260900100705     c                   exsr      In_TAB_EDSTBL
261000090210      *
261100100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
261200100705      *
261300090210     c                   SELECT
261400100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
261500100705     c                               VABcas > *zeros
261600090210     C                   movel     '4'           VABCBO                         + C/Ass
261700100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
261800090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
261900090210      *  Porto Assegnato
262000090210     c                   WHEN      VABCAS > *zeros
262100090210     C                   movel     '6'           VABCBO                         + C/Ass
262200090210     C                   OTHER
262300090210     C                   movel     '2'           VABCBO                         Senza C/Ass
262400090210     C                   ENDSL
262500090210      *
262600090210     c                   end
262700090210     c                   endIF
262800090209      **
262900110328     c                   ENDif
263000110328      **
263100090210      **  Il tipo bolla non è presente
263200090210     c                   if        VABCBO = *blank
263300090210     C                   eval      vinMSG = 'TOD non rilevato'
263400090213     c                   exsr      Error_DET
263500090210     c                   goto      end_TOD
263600090210     c                   end
263700090210      **
263800090210     c     end_TOD       endSR
263900090213      * ?================================================================== */
264000090213     C*?  Anagrafica del mittente e del destinatario
264100090213      * ?================================================================== */
264200091016     C     DATI_NAD      BEGSR
264300100705      *
264400100705      *   Cerca la decodifica del dato in TABELLA
264500100813     c                   clear                   dopo_NAD_CN       1
264600120424     C                   movel(p)  Tipo_Seg      etbSGM
264700100705     C                   movel(p)  nad3035       etbVALSGM
264800100705     c                   exsr      In_TAB_EDSTBL
264900100708      *-------
265000120629      * Dato (SHIPPED FROM) usato principalmente x CMR
265100120629      *       ma anche usato al posto di un RFF+FF in testata messaggio
265200120629      *   quindi modificato il nome di riferimento del campo sul EDSTBL
265300120629      *   x renderlo più generico.
265400100708      *-------
265500120629     C********************        IF        Quale_campo ='VABCMR'
265600120629     C                   IF        Quale_campo ='NAD_SF'
265700120629      *-------
265800120629      * Dati di PROVENIENZA MESSAGGIO
265900120629      *-------
266000120629      ***
266100120629      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
266200100708     c                   if        nad30361 <> *blank
266300120629     C                   MOVEL     nad30361      NAD_SF_Cliente
266400100708     c                   elseIf    nad31241 <> *blank
266500120629     C                   MOVEL     nad31241      NAD_SF_Cliente
266600100708     c                   elseIf    nad3039  <> *blank
266700120629     C                   MOVEL     nad3039       NAD_SF_Cliente
266800100708     c                   end
266900120629      ***
267000120629      * ?SOLO SE NON c'è un RFF+FF: di Testata che prevale su tutto.
267100120629      * ?La PLATFORM LIST è usata come il Codice Cliente su RFF+FF:PLATFORMLIST
267200120629      ***
267300120629      * Se partner/cliente è abilitato alla funzione di sostituzione CLIENTE da UNB.
267400120629      *   Il (SHIPPED FROM) in testata funziona come un RFF+FF: a livello di testata.
267500120629      *    per agganciare i dati sulla tab."CL".
267600120629      ***
267700120629     C                   if         PZ_abilita_NAD_SF_come_RFF_FF ='S'
267800120629     c                                 and Tes_RFF_FF_ksc = 0
267900120629      * ?Ma solo se NON c'è un RFF+FF: di Testata che prevale su tutto.
268000120629      *
268100120629      *   imposta la PLATFORM LIST ricevuta del MITTENTE
268200120629      *    questo codice viene poi utilizzato sulla tab.CL (EDTAB) se concordato
268300120629      *    per codificare tutte le spedizioni come un RFF+FF:xxxxxxxxxxxxxxxxx di testata
268400120629     c                   eval      NAD_SF_Cliente = NAD3039
268500120629      *>>>>>>>>
268600120629     C*------------------
268700120629      *    Aggancia il Cliente e tutte le caratteristiche da tab.:"CL"
268800120629      * ?RFF+FF:
268900120629     C                   clear                   Rifer_Cliente    35
269000120629     C                   movel     NAD_SF_ClienteRifer_Cliente
269100120629     c                   exsr      CL_Particolare
269200120629     C*------------------
269300120629      *>>>>>>>>
269400120629     c                   end
269500120629      ***
269600100705      *-------
269700090210      * Dati destinatario
269800100705      *-------
269900100708     C                   elseIF    Quale_campo ='VABRSD'
270000100813     c                   move      'S'           dopo_NAD_CN
270100090227      *
270200090227      * Se trovato un indirizzo di destinatario
270300100701     c                   if           Dati_di_Testata = 'N'
270400100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
270500100701     c                   else
270600100701     c                   eval       NAD_destinatario_in_testata = 'S'
270700100701     c                   end
270800090210      *
270900090210     C* Reperisco nazione corrispondente destinatario
271000100705     C                   Z-ADD     1             naz               3 0
271100100705     C                   MOVEL     *BLANKS       VABNZD
271200100705     C                   Z-ADD     0             WFLCPF
271300090210     c                   eval      *in32 = *off
271400100705      ***
271500090210     C                   if        nad3207 <> *BLANKS
271600100705     C     nad3207       LOOKUP    ISO(naz)                               32
271700100705     C                   If        *in32 = *on
271800100705      ***
271900100705      * per reperire il CAP corretto
272000100705     C                   MOVEL     C15(naz)      VABNZD
272100100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
272200100705      ***
272300100705     c                   Endif
272400100705     C                   endif
272500100705      *
272600090210     C* Imposto dati destinatario
272700100705     C                   MOVEL     nad30361      VABRSD
272800100705     C                   if        nad30362 <> *LOVAL
272900100705     C                   MOVEL     nad30362      VABRD2
273000090210     C                   endif
273100100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
273200091019???? C* Imposto dati destinatario
273300091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
273400100705???? C                   MOVEL     nad31241      VABRSD
273500100705???? C                   if        nad31242 <> *LOVAL
273600100705???? C                   MOVEL     nad31242      VABRD2
273700091019???? C                   endif
273800091019???? C                   end
273900100705      *   città/Località
274000090210     C                   MOVEL     nad3164       VABLOD
274100100812      *
274200100812      *  Se richiesto in tabella PT di accodare alla Ragione sociale
274300100812      *  l'informazione di indirizzo invertita nel segmento...
274400100812      *  Esigenza nata da Dior che mette l'indirizzo vero nel 2°campo del Indirizzo NAD
274500100812      *  ossia dopo i 2 punti (:) ed invece mette un'informazione sul destinatario/
274600100812      *  indirizzo nel 1° campo dell'Inidirizzo NAD prima dei 2 punti (:).
274700100812      *   Per nostra comodità mettiamo quindi l'informazione aggiuntiva attaccata alla
274800100812      *   Ragione Sociale e non alla Località poichè altrimenti avremmo continui
274900100812      *   problemi di decodifica Località/Cappario durante la conferma bolle.
275000100812     c                   if        §pv2IN = 'S'
275100100812      *
275200100812     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
275300100812      *
275400100812      * Nell'indirizzo il 2°campo ricevuto
275500100812     C                   MOVEL     NAD30422      VABIND
275600100812      * Mentre appiccica all'estensione della ragione sociale le info di consegna
275700100812      *  messe nel 1° campo dell'indirizzo (DIOR)
275800100812      *
275900100812      *  Verifico se c'è spazio sul campo estensione altrimenti vado in coda
276000100812      *    prima all'indirizzo e poi alla località
276100100812      *
276200100812     C* Verifico se c'è dello spazio per metterlo nell'estensione rag.soc.
276300100812     C     '    '        SCAN      VABRD2        WI                4 0
276400100812     c                   if        WI < 10
276500100812     C     '    '        SCAN      VABrd2        WI                4 0
276600100812     C                   MOVEA     VABrd2        LOD
276700100812     C                   ADD       1             WI
276800100812     C                   MOVEA     NAD30421      LOD(WI)
276900100812     C                   MOVEA     LOD           VABrd2
277000100812      *
277100100812     c                   else
277200100812      *  Verifico se c'è spazio sull'indirizzo altrimenti vado in coda alla località
277300100812     C     '    '        SCAN      VABIND        WI                4 0
277400100812     c                   if        WI < 10
277500100812     C     '    '        SCAN      VABIND        WI                4 0
277600100812     C                   MOVEA     VABIND        LOD
277700100812     C                   ADD       1             WI
277800100812     C                   MOVEA     NAD30421      LOD(WI)
277900100812     C                   MOVEA     LOD           VABIND
278000100812      *
278100100812     c                   else
278200100812      *  oppure    accodato nella località
278300100812     C     '    '        SCAN      VABLOD        WI                4 0
278400100812     C                   MOVEA     VABLOD        LOD
278500100812     C                   ADD       1             WI
278600100812     C                   MOVEA     NAD30421      LOD(WI)
278700100812     C                   MOVEA     LOD           VABLOD
278800100812     c                   end
278900100812     c                   end
279000100812      *-----------
279100100812     c                   else
279200100812      *
279300100812     C                   MOVEL     NAD30421      VABIND
279400100812     c                   end
279500100812      *
279600100812      *-----------
279700100812      *  segue lo standard
279800100812     c                   else
279900100812      *-----------
280000100812      *
280100100812     C                   MOVEL     NAD30421      VABIND
280200100812      *
280300100812     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
280400100812     c                   if        NAD30422 <> *blanks  and NAD30422 <> *loval
280500100812      *
280600100812      *  Verifico se c'è spazio sul primo indirizzo x accodare se richiesto
280700100812      *  altrimenti vado in coda alla località
280800100812     C     '    '        SCAN      VABIND        WI                4 0
280900100812      *
281000100812     C* Verifico se c'è dello spazio per metterlo nella località
281100100812      *  oppure    accodato al primo indirizzo
281200100812      *
281300100812     c                   if        WI < 15
281400100812      *
281500100812     C     '    '        SCAN      VABind        WI                4 0
281600100812     C                   MOVEA     VABind        LOD
281700100812     C                   ADD       1             WI
281800100812     C                   MOVEA     NAD30422      LOD(WI)
281900100812     C                   MOVEA     LOD           VABind
282000100812      *
282100100812     c                   else
282200100812      *
282300100812     C     '    '        SCAN      VABLOD        WI                4 0
282400100812     C                   MOVEA     VABLOD        LOD
282500100812     C                   ADD       1             WI
282600100812     C                   MOVEA     NAD30422      LOD(WI)
282700100812     C                   MOVEA     LOD           VABLOD
282800100812      *
282900100812     c                   end
283000100812      *
283100100812     c                   end
283200100812      *
283300100812      *
283400100812     C                   ENDif
283500100812      * ---------
283600090210      * Converto CAP
283700100705     C                   EXSR      Converte_CAP
283800090603     C*
283900100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
284000100705     C*  record del VAB viene PULITO
284100100316     c                   if        Dati_di_Testata  = 'S'
284200090603     c                   eval       tes_VABrsd = VABrsd
284300090603     c                   eval       tes_VABrd2 = VABrd2
284400090603     c                   eval       tes_VABind = VABind
284500090603     c                   eval       tes_VABlod = VABlod
284600090603     c                   eval       tes_VABnzd = VABnzd
284700090603     c                   end
284800100728      *
284900100728      *   se NON c'è l'identificativo bolla del cliente
285000130315      *    blocca la spedizione e lo segnala.
285100100728     C                   if          vabRSD = *blank
285200100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
285300130315     c***********        exsr      Error_DET
285400130315     c***********        goto      end_NAD
285500100728     C                   elseIF      vabIND = *blank
285600100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
285700130315     c***********        exsr      Error_DET
285800130315     c***********        goto      end_NAD
285900100728     C                   elseIF      vabLOD = *blank
286000100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
286100130315     c***********        exsr      Error_DET
286200130315     c***********        goto      end_NAD
286300100728     C                   ENDIF
286400100705      *-------
286500090210      * Dati mittente
286600100705      *-------
286700100705     C                   elseIF     Quale_campo ='VABRMO'
286800090210      *
286900100705     C                   movel     nad30361      VABRMO
287000090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
287100090210     C                   movel     §PTNAZ        VABNMO
287200090210      *
287300090210     C                   IF        nad3207 <> *blanks
287400100705     C                   eval      naz = 1
287500100705     C     nad3207       LOOKUP    ISO(naz)                               32
287600090210     C                   if         *in32 = *on
287700100705     C                   MOVEL     C15(naz)      VABNMO
287800090210     C                   endif
287900090210     C                   ENDIF
288000090210      *
288100090210      * Normalizzo CAP mittente originale
288200090210     C                   clear                   T
288300090210     C                   clear                   S
288400090210     C                   MOVEA     *BLANKS       CAPM
288500090210     C                   MOVEA     nad3251       CAP9
288600090210     C                   DO        9             T
288700090210     C     CAP9(T)       IFNE      *BLANKS
288800090210     C                   ADD       1             S
288900090210     C                   MOVE      CAP9(T)       CAPM(S)
289000090210     C                   ENDIF
289100090210     C                   ENDDO
289200090210     C*
289300090210      *     Controllo validità CAP
289400090210     C                   MOVEA     CAPM          VABCMO
289500090210      *
289600090210     C                   CLEAR                   TISI95DS
289700090210     C                   MOVEL     VABCMO        I95CAP
289800090210     C                   MOVEL     VABNMO        I95NAR
289900090210     C                   MOVEL     WOGGI         I95DAT
290000090210     C                   MOVEL     '3'           I95TCN
290100090210     C                   CALL      'TISI95R'
290200090210     C                   PARM                    TISI95DS
290300090210      *     Errore
290400090210     C     O95ERR        IFNE      *BLANKS
290500090210     C                   MOVEL     INDCAE        VABCMO
290600090210     C                   MOVEL     §PTNAZ        VABNMO
290700090210     C                   END
290800090603      *
290900100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
291000100705     C*  record del VAB viene PULITO
291100100316     c                   if        Dati_di_Testata  = 'S'
291200090603     c                   eval      tes_VABrmo = VABrmo
291300090603     c                   eval      tes_VABnmo = VABnmo
291400090603     c                   eval      tes_VABcmo = VABcmo
291500090603     c                   eval      NAD_mittente_in_testata = 'S'
291600100701     c                   else
291700100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
291800090603     c                   end
291900100728      *
292000100728      *   se NON c'è l'identificativo bolla del cliente
292100100728      *    blocca la spedisione e lo segnala.
292200130315     C                   if          vabRMO = *blank
292300130315     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
292400130315     c************       exsr      Error_DET
292500130315     c************       goto      end_NAD
292600130315     c                   ENDIF
292700090210      *
292800090210     C                   END
292900090210      *
293000090210     c     end_NAD       endSR
293100090210      * ?================================================================== */
293200090210     C*?  Contatto a cui far riferimento
293300090210      * ?================================================================== */
293400090210     C     DATI_CTA      BEGSR
293500090210      *
293600090210      *  memorizza il riferimento di consegna x il destinatario
293700100813     C                   if         cta3412 <> *BLANKS and
293800100813     c                                    dopo_NAD_CN = 'S'
293900100813      *
294000100701     c                   if        Dati_di_Testata  ='S'
294100100812     c                   if           CTA_Destinatario_in_testata   = *blanks
294200100812     c                   eval       CTA_Destinatario_in_testata   = cta3412
294300100812     c                   end
294400100701     c                   else
294500100812     c                   if           CTA_Destinatario_in_dettaglio = *blanks
294600100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
294700100812     c                   end
294800100701     c                   end
294900100701     c                   end
295000090210      *
295100090210     c                   endSR
295200090210      * ?================================================================== */
295300090210     C*?  Contatto telefonico  a cui far riferimento
295400090210      * ?================================================================== */
295500090210     C     DATI_COM      BEGSR
295600090210      *
295700100705      *  memorizza il riferimento di consegna nr.telefonico
295800100813     C                   if         com3148 <> *BLANKS and
295900100813     c                                  COM3155 = 'TE' and
296000100813     c                                    dopo_NAD_CN = 'S'
296100100813      *
296200100701     c                   if        Dati_di_Testata  ='S'
296300100812     c                   if           COM_telefono_in_testata = *blanks
296400100812     c                   eval      COM_telefono_in_testata   = com3148
296500100812     c                   end
296600100701     c                   else
296700100812     c                   if           COM_telefono_in_dettaglio = *blanks
296800100701     c                   eval      COM_telefono_in_dettaglio = com3148
296900100812     c                   end
297000090210     c                   end
297100100705     c                   endif
297200090210      *
297300090209     C                   ENDSR
297400090210      * ?================================================================== */
297500090210     C*?  Dati di dettaglio della spedizione e dei colli
297600090210      * ?================================================================== */
297700090210     C     DATI_GID      BEGSR
297800090210      *
297900100910      *  Se non richiesto quello sulla testata dettaglio
298000100920     c                   if        §pvCOl  <> 'S' or vabPKB = 0
298100100910      *
298200090210     C* Se ho totali per spedizione metto in campi VAB
298300090210     C     gid1496       IFEQ      99999
298400090210     C     gid1496       orEQ      9999
298500100630     c                   eval      GID_aTotale = 'S'
298600090210     C                   z-add     gid722420     VABNCL
298700090210     C                   ELSE
298800100630     c                   If        GID_aTotale = 'N'
298900100705     C                   z-add     gid722420     GID_ColliInAdd
299000100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
299100090210     C                   END
299200090210     C                   END
299300090212      *
299400100701     C                   ADD       VABNCL        ToT_Colli        16 0
299500090210      *
299600100910     C                   END
299700100910      *
299800090210     c     end_GID       endSR
299900090210      * ?================================================================== */
300000090210     C*?  Dati di dettaglio misure spedizione e colli
300100090210      * ?================================================================== */
300200090210     C     DATI_MEA      BEGSR
300300090210      *
300400100910     C*  Se non si deve prendere il peso dal Totale (CNT)
300500100920     c                   if        §pvPEs <> 'S' or vabPKB = 0
300600090210     C*  Peso
300700100319     C     mea6311       IFEQ      Weight
300800100319     C     mea6411       ANDEQ     Kilograms
300900090210      *
301000090210     C*  Controllo se ho valore totale o parziale
301100090210      *   sempre il Lordo
301200100319???? C     mea6313       IFEQ      G_Weight
301300100319???? C     mea6313       oreq      Gross_Weight
301400100706???? C     mea6313       oreq      Gross_Weight_E
301500100630     C                   IF        GID_aTotale = 'S'
301600091019???? C                   move      mea6314       mea6314_2        18 2          Peso
301700110530???? C     mea6314_2     div       10000         VABPKB                         Peso
301800090210     C                   ELSE
301900091019???? C                   move      mea6314       mea6314_2                      Peso
302000110530???? C     mea6314_2     div       10000         Peso_inKG        12 3          Peso
302100110530???? C                   add       Peso_inKG     VABPKB                         Peso
302200090210     C                   END
302300091203     C                   END
302400090210      *
302500100319???? C     mea6313       IFEQ      G_Weight
302600100319???? C     mea6313       oreq      Gross_Weight
302700100706???? C     mea6313       oreq      Gross_Weight_E
302800110530???? C     mea6314_2     div       10000         Peso_inKG        12 3          Peso
302900110530???? C                   ADD       Peso_inKG     ToT_PesoLordo
303000090210     C                   END
303100100706???? C     mea6313       IFEQ      N_Weight
303200100706???? C     mea6313       oreq      Net_Weight
303300100706???? C     mea6313       oreq      Gross_Weight_E
303400110530???? C     mea6314_2     div       10000         Peso_inKG        12 3          Peso
303500110530???? C                   ADD       Peso_inKG     ToT_PesoNetto
303600090210     C                   END
303700090210     C                   END
303800090210      ***
303900090210     C*  Peso in Grammi  se abilitato a utilizzarlo
304000090210      ***
304100100319     C     mea6311       IFEQ      Weight
304200100319     C     mea6411       ANDEQ     Grams
304300090210      *
304400090210     C*  Controllo se ho valore totale o parziale
304500100319???? C     mea6313       IFEQ      G_Weight
304600100319???? C     mea6313       oreq      Gross_Weight
304700100706???? C     mea6313       oreq      Gross_Weight_E
304800100630     c                   If        GID_aTotale = 'S'
304900091019???? c                   move      mea6314       mea6314_2
305000091019???? C     mea6314_2     div       1000          VABPKB                         Peso
305100090210     C                   ELSE
305200091019???? c                   move      mea6314       mea6314_2
305300091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
305400091019???? C                   add       Peso_inKG     VABPKB                         Peso
305500090210     C                   END
305600090210     C                   END
305700090210      *
305800100319???? C     mea6313       IFEQ      G_Weight
305900100319???? C     mea6313       oreq      Gross_Weight
306000100706???? C     mea6313       oreq      Gross_Weight_E
306100091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
306200100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
306300090210     C                   END
306400100706???? C     mea6313       IFEQ      N_Weight
306500100706???? C     mea6313       oreq      Net_Weight
306600100706???? C     mea6313       oreq      Gross_Weight_E
306700091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
306800100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
306900090210     C                   END
307000090210     C                   END
307100100910      *
307200100910     c                   end
307300100910      * -------------
307400090210      *  Volume
307500100319???? C     mea6311       IFEQ      Volume
307600100319???? C     mea6411       ANDEQ     Cubic_Meters
307700090210      *  Controllo se ho valore totale o parziale
307800100630     c                   If        GID_aTotale = 'S'
307900110530???? C                   move      mea6314       mea6314_3        18 3          Volume
308000110530???? C     mea6314_3     div       1000          VABVLB                         Volume
308100110530     C                   ELSE                                                   Volume
308200110530???? C                   move      mea6314       mea6314_3                      Volume
308300110530???? C     mea6314_3     div       1000          Volm_inMQ        12 3          Volume
308400110530???? C                   add       Volm_inMQ     VABVLB                         Volume
308500090210     C                   END
308600090210     C                   END
308700100705      *
308800100910      * -------------
308900100705     C* Ricerco CAP in base alle dimensioni
309000090210     C     WCAP          IFNE      *BLANKS
309100100319     C     mea6311       IFEQ      Weight
309200090210      *
309300090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
309400090210     C                   CLEAR                   FNLV55DS
309500090210     C                   MOVEL     'P'           D55TPT
309600090210     C                   MOVEL     WOGGI         D55DRF
309700090210     C                   MOVEL     VABLNP        D55LNP
309800090210     C                   MOVEL     VABLNP        D55LIN
309900090210     C                   CALL      'FNLV55R'
310000090210     C                   PARM                    FNLV55DS
310100090210     C                   MOVE      D55TFP        COMTFP            3 0
310200090210      *
310300090210     C*  PASSO IL TERMINAL PARTENZA
310400090210     C                   CLEAR                   TISI95DS
310500090210     C                   Z-ADD     COMTFP        I95TFP
310600090210     C                   MOVEL     VABTSP        I95TSP
310700090210     C                   MOVEL     'S'           I95FRE
310800090210     C                   MOVEL     '3'           I95TCN
310900090210     C                   MOVEL     WCAP          I95CAP
311000090210     C                   MOVEL     VABNZD        I95NAR
311100090210     C                   MOVEL     VABLOD        I95LOC
311200090210     C                   MOVEL     VABIND        I95IND
311300090210      *
311400090210     C* Se gestita seconda linea di arrivo passo peso - volume
311500090210     C* e numero colli effettivo
311600090210     C     §PULNA        IFEQ      'S'
311700090210     C                   Z-ADD     VABPKB        I95LKG
311800090210     C                   Z-ADD     VABVLB        I95LMC
311900090210     C                   ELSE
312000100706     C*******
312100090210     C* Altrmenti passo 1 Kg e 0,001
312200100706     C*******              Z-ADD1                  I95LKG
312300100706     C*******              Z-ADD0,001              I95LMC
312400100706     C*******
312500090210     C                   END
312600090210     C*
312700090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
312800090210     C                   SELECT
312900090210     C     VABCBO        WHENEQ    '1 '
313000090210     C                   MOVEL     *BLANKS       I95FCA
313100090210     C                   MOVEL     'F'           I95TPO
313200090210     C     VABCBO        WHENEQ    '2 '
313300090210     C                   MOVEL     *BLANKS       I95FCA
313400090210     C                   MOVEL     'A'           I95TPO
313500090210     C     VABCBO        WHENEQ    '4 '
313600090210     C                   MOVEL     'S'           I95FCA
313700090210     C                   MOVEL     'F'           I95TPO
313800090210     C     VABCBO        WHENEQ    '6 '
313900090210     C                   MOVEL     'S'           I95FCA
314000090210     C                   MOVEL     'A'           I95TPO
314100090210     C                   OTHER
314200090210     C                   MOVEL     'F'           I95TPO
314300090210     C     VABCAS        IFGT      0
314400090210     C                   MOVEL     'S'           I95FCA
314500090210     C                   ELSE
314600090210     C                   MOVEL     *BLANKS       I95FCA
314700090210     C                   END
314800090210     C                   ENDSL
314900090210     C*
315000090210     C                   CALL      'TISI95R'
315100090210     C                   PARM                    TISI95DS
315200090210     C* Reperisco i dati da CAP
315300110613     C                   move      'S'           Instradamento
315400090210     C                   MOVEL     O95PRV        VABPRD
315500100706     C                   MOVEL     WCAP          VABCAD
315600100114      ***
315700100706     c                   MOVEL     O95LNA        VABLNA
315800100706     C                   MOVEL     O95ZNC        VABZNC
315900090210     C*
316000090210     C     O95ERR        IFNE      *BLANKS
316100090210     C     O95FIT        ANDEQ     'S'
316200090210     C*  Cap non trovato
316300090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
316400110224     c********           exsr      Error_DET
316500110224     C********           goto      end_MEA
316600090210     C                   END
316700090210     C                   END
316800090210      *
316900090210     C                   ELSE
317000090210      *
317100090210     C                   CLEAR                   VABPRD
317200090210     C                   CLEAR                   VABLNA
317300090210     C                   CLEAR                   VABZNC
317400090210     C                   CLEAR                   VABCAD
317500090210     C*  Cap non trovato
317600090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
317700110224     c*********          exsr      Error_DET
317800110224     C*********          goto      end_MEA
317900090210     C                   END
318000090210      *
318100090210     c     end_MEA       endSR
318200090210      * ?================================================================== */
318300090210     C*?  Dati di dettaglio Codice a barre Segnacolli
318400090210      * ?================================================================== */
318500090210     C     DATI_PCI      BEGSR
318600090210      *
318700090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
318800090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
318900090210     C     §1BF12        IFEQ      'E'
319000100706     C                   EXSR      SEGNaCollo_EEX
319100090210     C                   ELSE
319200090210      *
319300090210      *  La prima volta controllo congruità numero segnacollo
319400090210     C     §PUBS1        IFNE      *BLANKS
319500090210     C     VABnrs        ANDNE     0
319600090210      *  Se il codice trattamento merce prevede che segnacollino i
319700090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
319800090210     C     §1BFG7        IFEQ      'S'
319900090210     C     §1BFG1        OREQ      'S'
320000090210      *  calcolo segnacollo
320100100706     C                   EXSR      Segn_Bartolini
320200090210     C                   END
320300090210     C                   END
320400090210      *
320500090210     C                   END
320600090210      *
320700090210     c     end_PCI       endSR
320800090211     C*----------------------------------------------------------------
320900090211      *? dati finali del dettaglio    UNT
321000090211     C*----------------------------------------------------------------
321100090211     C     DATI_UNT      BEGSR
321200090211      *
321300110324      *  Deve saltare il controllo e non dare il messaggio
321400110324     c                   goto      No_UNTctrl
321500110324      *
321600100708     c                   eval      UNT_record = vnREC
321700090213     c                   z-add     vnrec         last_RECord
321800090211      **
321900100319      **   verifica la quadratura di tutti i segmenti ricevuti
322000100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
322100100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
322200100728     C     UNT0074       IFne      Conta_segmenti
322300100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
322400100319     c                             trollare il Messaggio ricevuto'
322500100319      *
322600100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
322700100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
322800100921     C                   EVAL      Messaggio = 'EDI -
322900100921     C                             IFCSUM D96A - msg su UPLOAD -
323000100921     C                             con quadratura UNT errata verificare -
323100100921     C                             x il cliente :'
323200100319     C                   EVAL      Messaggio = %trim(Messaggio) +
323300100319     C                                         %editc(§PTksc:'X')
323400100319     C                   EVAL      Messaggio = %trim(Messaggio) +
323500100319     C                             ' i segmenti contati sono :' +
323600100319     C                             %editc(Conta_segmenti:'Z')
323700100319     c                   if        §PVinv <> 'N'
323800100319     c                   exsr      invio_mail
323900100319     c                   end
324000100319     c                   end
324100110321     c     No_UNTctrl    tag
324200100319      **
324300100319     c     End_UNT       endSR
324400100316     C*----------------------------------------------------------------
324500100316      *? dati finali     UNT
324600100316     C*----------------------------------------------------------------
324700100316     C     DATI_UNZ      BEGSR
324800100316      *
324900100726     c                   move      'S'           Forza_Uscita
325000100316      **
325100100316     c                   endSR
325200090210      *----------------------------------------------------------------
325300090210      *? Input segnacolli su schiera di 20 elementi
325400090210      *----------------------------------------------------------------
325500090210     C     INP_Segnacol  BEGSR
325600090210      *
325700090210     c                   clear                   quanti_PCI        3 0
325800090210     c                   if        §puEL1 = 0
325900090210     c                   z-add     10            quanti_PCI
326000090210     c                   else
326100090210     c                   z-add     §puEL1        quanti_PCI
326200090210     c                   end
326300090210      *
326400090210      *  Pulisce la schiera dei segnacolli da elaborare
326500090210     c                   clear                   x30
326600090210     c                   z-add     0             Nr_x30            3 0
326700090210      *
326800090210      *   riporto tutto sulla schiera generica X30
326900090210      *
327000090210     c                   do        10            limite            3 0
327100090210      * Limita quanti elementi passati in ogni PCI
327200090210      *   devono essere considerati
327300090210     c                   if        limite > quanti_PCI
327400090210     c                   Leave
327500090210     c                   end
327600090210      *
327700100920     c                   IF        tipo_seg = 'GIN'
327800100920      *  Carica la schiera generale di tutti i segnacolli
327900100920     c                   if        GIN_10(limite) <> *blank
328000100920     c                   add       1             Nr_x30
328100100920     c                   eval      X30(Nr_x30) = GIN_10(limite)
328200100920     c                   end
328300100920      *
328400100920     c                   elseIF    tipo_seg = 'PCI'
328500090210      *  Carica la schiera generale di tutti i segnacolli
328600100920     c                   if        PCI_10(limite) <> *blank
328700090210     c                   add       1             Nr_x30
328800100920     c                   eval      X30(Nr_x30) = PCI_10(limite)
328900090210     c                   end
329000100920      *
329100100920     c                   endIF
329200100920      *
329300090210     c                   enddo
329400090210      *
329500090210     C                   ENDSR
329600090210      *----------------------------------------------------------------
329700090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
329800090210      *----------------------------------------------------------------
329900100706     C     SEGNaCollo_EEXBEGSR
330000090210      *
330100090210     c                   exsr      INP_Segnacol
330200090210      *
330300090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
330400090210     C                   DO        10            x                              --- 01 ---
330500090210      *
330600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
330700090210     C     x30(X)        ANDNE     *LOVAL
330800090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
330900100701     C                   ADD       1             totale_PCI
331000100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
331100090210     C                   END                                                    --- 03 ---
331200090210      *
331300090210     C                   END                                                    --- 02 ---
331400090210      *
331500090210     C                   ENDSR
331600090210     C*----------------------------------------------------------------
331700090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
331800090210     C*----------------------------------------------------------------
331900100706     C     Segn_BartoliniBEGSR
332000090210      *
332100090210     c                   exsr      INP_Segnacol
332200090210     C*
332300090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
332400090210     C*  in base al numero elementi validi carico sgn.
332500090210     C                   DO        10            X                              --- 01 ---
332600090210     C*
332700090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
332800090210     C     x30(X)        ANDNE     *LOVAL
332900090210     C*
333000100630     C                   CLEAR                   DS_SegnBART
333100090210     C*  Controllo se devo ricevere la linea di partenza
333200090210     C                   MOVEL     'N'           WERRO             1
333300090210     C     §PULP1        IFGT      0                                            --- 03 ---
333400100706     C                   EXSR      REPerisce_LNP
333500090210     C                   END                                                    --- 03 ---
333600100706      *
333700090210     C*  Controllo se devo ricevere la linea di arrivo
333800090210     C     §PULA1        IFGT      0                                            --- 03 ---
333900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
334000100706     C                   EXSR      REPerisce_LNA
334100090210     C                   END                                                    --- 03 ---
334200100706      *
334300090210     C*  Controllo se devo ricevere il numero di serie
334400090210     C     §PUNR1        IFGT      0                                            --- 03 ---
334500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
334600100706     C                   EXSR      REPerisce_NRS
334700090210     C                   END                                                    --- 03 ---
334800100706      *
334900090210     C*  Controllo se devo ricevere il numero segnacollo
335000090210     C     §PUSG1        IFGT      0                                            --- 03 ---
335100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
335200100706     C                   EXSR      REPerisce_SGN
335300090210     C                   END                                                    --- 03 ---
335400100706      *
335500090210     C*  Controllo se devo ricevere la zona di consegna
335600090210     C     §PUZN1        IFGT      0                                            --- 03 ---
335700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
335800100706     C                   EXSR      REPerisce_ZNC
335900090210     C                   END                                                    --- 03 ---
336000100706      *
336100090210     C*  Imposto da VAB i dati a zero se mi è stato passato
336200090210     C*  numero segnacollo valido
336300090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
336400100706      *
336500100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
336600100630     C                   Z-ADD     VABLNP        SegnBART_LNP
336700090210     C                   END                                                    --- 04 ---
336800100706      *
336900100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
337000100630     C                   Z-ADD     VABLNA        SegnBART_LNA
337100090210     C                   END                                                    --- 04 ---
337200100706      *
337300100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
337400100630     C                   Z-ADD     VABNRS        SegnBART_NRS
337500090210     C                   END                                                    --- 04 ---
337600100706      *
337700100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
337800100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
337900090210     C                   END                                                    --- 04 ---
338000090210      *
338100090210     C*  mi reimposto LNA e zona da segnacollo
338200100114      ***
338300100114      *** dovendo accorpare le spedizioni da Conferma x CMR
338400100114      ***  non deve impostare l'instradamento.
338500100630     C                   Z-ADD     SegnBART_LNA  VABLNA
338600100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
338700100114      ***
338800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
338900100701     C                   ADD       1             Conta_Segn
339000100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
339100090210     C                   END                                                    --- 03 ---
339200090210     C*
339300090210     C                   END                                                    --- 02 ---
339400090210     C*
339500090210     C                   END                                                    --- 01 ---
339600090210      *
339700090210     C                   ENDSR
339800090210     C*----------------------------------------------------------------
339900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
340000090210     C*----------------------------------------------------------------
340100100706     C     REPerisce_LNP BEGSR
340200090210     C*
340300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
340400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
340500090210     C*  dal numero segnacollo passatomi dal partner
340600090210     C                   Z-ADD     §PULP1        WP                2 0
340700090210     C     3             SUBST     x30(X):WP     WLNP              3
340800090210     C*
340900090210     C*  controllo che la lnp calcolata sia numerica
341000090210     C                   TESTN                   WLNP                 98
341100090210     C*
341200090210     C*  se è numerica la carico
341300090210     C     *IN98         IFEQ      '1'
341400100630     C                   MOVE      WLNP          SegnBART_LNP
341500090210     C                   ELSE
341600090210     C*  se non è numerica stampo errore
341700090210     C                   MOVEL     'S'           WERRO             1
341800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
341900090213     c                   exsr      Error_DET
342000090210     C                   goto      end_LNP
342100090210     C                   END
342200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
342300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
342400100630     C     SegnBART_LNP  IFNE      VABLNP
342500090210     C                   MOVEL     'S'           WERRO             1
342600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
342700090210     c                             a della Bolla'
342800090213     c                   exsr      Error_DET
342900090210     C                   goto      end_LNP
343000090210     C                   END
343100090210     C*
343200090210     C     end_LNP       ENDSR
343300090210     C*----------------------------------------------------------------
343400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
343500090210     C*----------------------------------------------------------------
343600100706     C     REPerisce_LNA BEGSR
343700090210     C*
343800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
343900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
344000090210     C*  dal numero segnacollo passatomi dal partner
344100090210     C                   Z-ADD     §PULA1        WP                2 0
344200090210     C     3             SUBST     x30(X):WP     WLNA              3
344300090210     C*  controllo che la lna calcolata sia numerica
344400090210     C                   TESTN                   WLNA                 98
344500090210     C*  se è numerica la carico
344600090210     C     *IN98         IFEQ      '1'
344700100630     C                   MOVE      WLNA          SegnBART_LNA
344800090210     C                   ELSE
344900090210     C*  se non è numerica stampo errore
345000090210     C                   MOVEL     'S'           WERRO             1
345100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
345200090213     c                   exsr      Error_DET
345300090210     C                   goto      end_LNA
345400090210     C                   END
345500090210     C*
345600090210     C     end_LNA       ENDSR
345700090210     C*----------------------------------------------------------------
345800090210     C*? REPNRS - Reperisce numero serie
345900090210     C*----------------------------------------------------------------
346000100706     C     REPerisce_NRS BEGSR
346100090210     C*
346200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
346300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
346400090210     C*  dal numero segnacollo passatomi dal partner
346500090210     C                   Z-ADD     §PUNR1        WP                2 0
346600090210     C     2             SUBST     x30(X):WP     WNRS              2
346700090210     C*  controllo che il nrs calcolata sia numerico
346800090210     C                   TESTN                   WNRS                 98
346900090210     C*  se è numerica la carico
347000090210     C     *IN98         IFEQ      '1'
347100100630     C                   MOVE      WNRS          SegnBART_NRS
347200090210     C                   ELSE
347300090210     C*  se non è numerico stampo errore
347400090210     C                   MOVEL     'S'           WERRO             1
347500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
347600090213     c                   exsr      Error_DET
347700090210     C                   goto      end_NRS
347800090210     C                   END
347900090210      *
348000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
348100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
348200100630     C     SegnBART_NRS  IFNE      VABNRS
348300090210     C                   MOVEL     'S'           WERRO             1
348400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
348500090210     c                             o della Bolla'
348600090213     c                   exsr      Error_DET
348700090210     C                   goto      end_NRS
348800090210     C                   END
348900090210     C*
349000090210     C     end_NRS       ENDSR
349100090210     C*----------------------------------------------------------------
349200090210     C*? REPZNC - Reperisce zona consegna
349300090210     C*----------------------------------------------------------------
349400100706     C     REPerisce_ZNC BEGSR
349500090210     C*
349600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
349700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
349800090210     C*  dal numero segnacollo passatomi dal partner
349900090210     C                   Z-ADD     §PUZN1        WP                2 0
350000090210     C     2             SUBST     x30(X):WP     WZNC              2
350100090210     C*  controllo che la zona calcolata sia numerica
350200090210     C                   TESTN                   WZNC                 98
350300090210     C*  se è numerica la carico
350400090210     C     *IN98         IFEQ      '1'
350500100630     C                   MOVE      WZNC          SegnBART_ZNC
350600090210     C                   ELSE
350700090210     C*  se non è numerica stampo errore
350800090210     C                   MOVEL     'S'           WERRO             1
350900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
351000090213     c                   exsr      Error_DET
351100090210     C                   goto      end_ZNC
351200090210     C                   END
351300090210     C*
351400090210     C     end_ZNC       ENDSR
351500090210     C*----------------------------------------------------------------
351600090210     C*? REPSGN - Reperisce numero segnacollo
351700090210     C*----------------------------------------------------------------
351800100706     C     REPerisce_SGN BEGSR
351900090210     C*
352000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
352100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
352200090210     C*  indicata in tabella)
352300090210     C*  dal numero segnacollo passatomi dal partner
352400090210     C                   Z-ADD     §PUSG1        WP                2 0
352500090210     C     7             SUBST     x30(X):WP     WSGN              7
352600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
352700090210     C                   TESTN                   WSGN                 98
352800090210     C*  se è numerico lo carico
352900090210     C     *IN98         IFEQ      '1'
353000100630     C                   MOVE      WSGN          SegnBART_NSC
353100090210     C                   ELSE
353200090210     C*  se non è numerica stampo errore
353300090210     C                   MOVEL     'S'           WERRO             1
353400100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
353500090213     c                   exsr      Error_DET
353600090210     C                   goto      end_SGN
353700090210     C                   END
353800090210     C*
353900090210     C     end_SGN       ENDSR
354000051205      *----------------------------------------------------*
354100051205      *   DEFINIZIONE CHIAVI                               *
354200051205      *----------------------------------------------------*
354300051205     C     *INZSR        BEGSR
354400051205      *
354500991129     c     *ENTRY        PLIST
354600060613     C                   parm                    esito             1
354700971215      *
354800050112     C     KTABel        KLIST
354900050112     C                   KFLD                    TBLKUT
355000050112     C                   KFLD                    TBLCOD
355100050112     C                   KFLD                    TBLKEY
355200090210      *
355300090210     C     K_TAB1L       KLIST
355400090213     C                   KFLD                    etbUNB
355500090213     C                   KFLD                    etbSGM
355600090213     C                   KFLD                    etbVALSGM
355700090209     C*
355800090209     C* Definisco chiavi di accesso
355900090209     C     KTAB1         KLIST
356000090209     C                   KFLD                    KKUT
356100090209     C                   KFLD                    KCOD
356200090209     C*
356300090209     C     KNUF          KLIST
356400090209     C                   KFLD                    KAAA
356500090209     C                   KFLD                    KCNU
356600090209     C                   KFLD                    KFIL
356700090209      *
356800090209     C     KVAB1         KLIST
356900090209     C                   KFLD                    KCCM
357000090209     C                   KFLD                    KCMR
357100090209     C                   KFLD                    KCNT
357200090209     C                   KFLD                    KAAS
357300090209     C                   KFLD                    KLNP
357400090209     C                   KFLD                    KNRS
357500090209     C                   KFLD                    KNSP
357600090209      *
357700090209     C     KVAB2         KLIST
357800090209     C                   KFLD                    KCCM
357900090209     C                   KFLD                    KCMR
358000090209      *
358100090209     C     KRDE          KLIST
358200090209     C                   KFLD                    KAAS
358300090209     C                   KFLD                    KLNP1
358400090209     C                   KFLD                    KNRS
358500090209     C                   KFLD                    KNSP
358600090209     C                   KFLD                    KNMC
358700090209     C                   KFLD                    KNRP
358800090209      *
358900090209     C     KACO          KLIST
359000090209     C                   KFLD                    KKUT
359100090209     C                   KFLD                    KKCC
359200090209     C                   KFLD                    KKSC
359300090209      *
359400090209     C     KIND          KLIST
359500090209     C                   KFLD                    KKUT
359600090209     C                   KFLD                    KKCC
359700090209     C                   KFLD                    KKSC
359800111028      *
359900111028     C     KTAS          KLIST
360000111028     C                   KFLD                    TASAAS
360100111028     C                   KFLD                    TASLNP
360200111028     C                   KFLD                    TASNRS
360300111028     C                   KFLD                    TASNSP
360400090209      *
360500090209      * DETERMINO SE SONO BARTOLINI O SDI
360600090209     C                   CLEAR                   TIBS55DS
360700090209     C                   MOVEL     'L'           I50TLA
360800090209     C                   CALL      'TIBS55R'
360900090209     C                   PARM                    TIBS55DS
361000090209      *
361100090209     C* RECUPERO DATI DELL'UTENTE
361200090209     C                   Z-ADD     1             CODUT             1 0
361300090209     C                   CALL      'XPARUT'
361400090209     C                   PARM                    UTEDSE0F
361500090209     C                   MOVEL     RAGUT         RSUT             20
361600090209     C*
361700090209     C* RICERCA CAPOCONTI
361800090209     C                   DO        50            X
361900090209     C                   MOVE      TCU(X)        TCUDS
362000090209     C     F56           IFEQ      'CG'
362100090209     C     F34           ANDEQ     '01'
362200090209     C                   Z-ADD     KCU(X)        KCI               4 0
362300090209     C                   END
362400090209     C                   END
362500090209     C*
362600090211     c                   movel(p)  'EDPAB'       User_Msg         10
362700090211     C*
362800090209     C* IMPOSTO A ZERO VARIABILI DI PGM
362900090209     c                   move      *blank        Snd_Msg_alert     1
363000090209     C                   MOVEL     CA(1)         WCA              78
363100090209     C                   MOVEL     CN(1)         WCN              78
363200090209     C*
363300090209     C* RECUPERO DATA E ORA
363400100630     C                   TIME                    Ora_Data         14 0
363500100630     C                   MOVE      Ora_Data      G02DAT
363600100630     C                   MOVE      Ora_Data      WDATA8            8 0
363700100630     C                   MOVE      WDATA8        Anno2             2 0
363800090209     C                   MOVEL     WDATA8        DATA              6 0
363900100630     C                   MOVE      Anno2         DATA
364000090209     C                   MOVEL     *BLANK        G02ERR
364100090209     C                   CALL      'XSRDA8'
364200090209     C                   PARM                    WLBDA8
364300090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
364400100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
364500090209     C                   TIME                    WORA              6 0
364600090209      * Recupero data e ora
364700090209     C                   TIME                    W0140
364800090209      * UDATE IN GGMMAAAA
364900100630     C                   MOVE      W0140         DataGGMMAAA
365000090209      * UDATE IN AAAAMMGG
365100100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
365200090209     C     *iso          MOVEL     DATA_eur      dateu
365300090209      *
365400090209      * ?              /*--------------------------- */
365500090209     c                   exsr      CARICA_TABELLE
365600090209      * ?              /*--------------------------- */
365700090209      *
365800090209     C                   MOVEL     *ALL'-'       CMP132          132
365900991124      *
366000050414      *------------------
366100991124     C                   ENDSR
366200060621      * ?------------------------------------------------------------------ */
366300090209      *?    CARICA TABELLE SU SCHIERE
366400060621      * ?------------------------------------------------------------------ */
366500090209     C     CARICA_TABELLEBEGSR
366600090205      *
366700090209     C* Caricamento Tabella 1B
366800090209     C                   Z-ADD     0             X                 4 0
366900090209     C                   Z-ADD     CODUT         KKUT
367000090209     C                   MOVEL     '1B'          KCOD
367100090209     C     KTAB1         CHAIN     TABEL00F                           30
367200090209     C     *IN30         DOWEQ     '0'
367300090209     C     TBLFLG        IFEQ      *BLANKS
367400090209     C                   ADD       1             X
367500100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
367600100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
367700090209     C                   END
367800090209     C     KTAB1         READE     TABEL00F                               30
367900090209     C                   END
368000090209      *
368100090209     C* Caricamento Tabella 15
368200090209     C                   Z-ADD     0             X                 4 0
368300090209     C                   Z-ADD     CODUT         KKUT
368400090209     C                   MOVEL     '15'          KCOD
368500090209     C     KTAB1         CHAIN     TABEL00F                           30
368600090209     C     *IN30         DOWEQ     '0'
368700090209     C     TBLFLG        IFEQ      *BLANKS
368800090209     C                   ADD       1             X
368900090209     C                   MOVEL     TBLKEY        C15(X)
369000090209     C                   MOVEL     TBLUNI        DS15
369100090209     C                   MOVEL     §15COD        ISO(X)
369200090209     C                   MOVEL     §15ECF        CPF(X)
369300090209     C                   MOVE      §15PCF        CPF(X)
369400090209     C                   END
369500090209     C     KTAB1         READE     TABEL00F                               30
369600090209     C                   END
369700090209      *
369800090209     C* Caricamento Tabella CV
369900090209     C                   Z-ADD     0             X
370000090209     C                   Z-ADD     CODUT         KKUT
370100090209     C                   MOVEL     'CV'          KCOD
370200090209     C     KTAB1         CHAIN     TABEL00F                           30
370300090209     C     *IN30         DOWEQ     '0'
370400090209     C     TBLFLG        IFEQ      *BLANKS
370500090209     C                   ADD       1             X
370600090209     C                   MOVEL     TBLKEY        CCV(X)
370700090209     C                   MOVEL     TBLUNI        DCV(X)
370800090209     C                   END
370900090209     C     KTAB1         READE     TABEL00F                               30
371000090209     C                   END
371100090216      *
371200100702     C* Caricamento Tabella Priorità trasporto
371300100702     C                   Z-ADD     0             X
371400100702     C                   MOVEL     'TS'          TABCOD
371500100702     C     TABCOD        CHAIN     EDTAB01L                           30
371600100702     C     *IN30         DOWEQ     '0'
371700100702     C     TABFLG        IFEQ      *BLANKS
371800100702     C                   ADD       1             X
371900100702     C                   eval      TipoServizio(X) = TABUNI
372000100705     C                   eval      Key_TipServ(X)  = TABKEY
372100100702     C                   END
372200100702     C     TABCOD        READE     EDTAB01L                               30
372300100702     C                   END
372400110328      *
372500110328      * Caricamento Tabella termini consegna
372600110328     C                   Z-ADD     0             X
372700110328     C                   MOVEL     'TB'          TABCOD
372800110328     C     TABCOD        CHAIN     EDTAB01L                           30
372900110328     C     *IN30         DOWEQ     '0'
373000110328     C     TABFLG        IFEQ      *BLANKS
373100110328     C                   ADD       1             X
373200110328     C                   eval      K35_TpBolla(X) = TABKEY
373300110328     C                   eval      Cod_TpBolla(X) = TABKEY
373400110328     C                   eval      Decod_Porto(X) = TABUNI
373500110328     C                   END
373600110328     C     TABCOD        READE     EDTAB01L                               30
373700110328     C                   END
373800100702      *
373900090216     C* Caricamento Tabella Tipo Incasso C/Assegno
374000090216     C                   Z-ADD     0             X
374100090216     C                   MOVEL     'TC'          TABCOD
374200090216     C     TABCOD        CHAIN     EDTAB01L                           30
374300090216     C     *IN30         DOWEQ     '0'
374400090216     C     TABFLG        IFEQ      *BLANKS
374500090216     C                   ADD       1             X
374600100705     C                   eval      K35_TpIncas(X) = TABKEY
374700100705     C                   eval      TipoIncasso(X) = TABUNI
374800090216     C                   END
374900090216     C     TABCOD        READE     EDTAB01L                               30
375000090216     C                   END
375100090209      *
375200090209      * Caricamento Tabella Partner esteri
375300090209     C                   Z-ADD     0             X
375400090209     C                   MOVEL     'PT'          TABCOD
375500090209     C     TABCOD        CHAIN     EDTAB01L                           30
375600090209     C     *IN30         DOWEQ     '0'
375700090209      *
375800090209     C                   SELECT
375900090209     C     TABFLG        WHENNE    *BLANKS
376000090209      *
376100090209     C                   OTHER
376200090209     C                   ADD       1             X
376300100630     C                   MOVEL     TABKEY        PT_key(X)
376400100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
376500100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
376600100630     C                   MOVEL     TABUNI        PT_uni(X)
376700090209     C                   ENDSL
376800090209      *
376900090209     C     TABCOD        READE     EDTAB01L                               30
377000090209     C                   END
377100121105      *
377200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
377300121105     c                   exsr      ChecK_PT
377400121105      *
377500090209      * salva quanti Codici UNB ha caricato
377600100630     c                   z-add     x             PT_KeyXsav
377700090209      *
377800090209     C                   MOVEL     'PU'          TABCOD
377900090209     C     TABCOD        CHAIN     EDTAB01L                           30
378000090209     C     *IN30         DOWEQ     '0'
378100090209      *
378200090209     C                   SELECT
378300090209     C     TABFLG        WHENNE    *BLANKS
378400090209      *
378500090209     C                   OTHER
378600100630     C                   MOVEL     TABKEY        PU_key
378700090209     C                   Z-ADD     1             X
378800100630     C     PU_key        LOOKUP    PT_key(X)                              32
378900100630     C   32              MOVEL     TABUNI        PU_uni(X)
379000090209     C                   ENDSL
379100090209      *
379200090209     C     TABCOD        READE     EDTAB01L                               30
379300090209     C                   END
379400090209      *
379500090209      * Prosegue tab.PT e PU
379600090209     C                   MOVEL     'PV'          TABCOD
379700090209     C     TABCOD        CHAIN     EDTAB01L                           30
379800090209     C     *IN30         DOWEQ     '0'
379900090209      *
380000090209     C                   SELECT
380100090209     C     TABFLG        WHENNE    *BLANKS
380200090209      *
380300090209     C                   OTHER
380400100630     C                   MOVEL     TABKEY        PV_key
380500090209     C                   Z-ADD     1             X
380600100630     C     PV_key        LOOKUP    PT_key(X)                              32
380700100630     C   32              MOVEL     TABUNI        PV_uni(X)
380800090209     C                   ENDSL
380900090209      *
381000090209     C     TABCOD        READE     EDTAB01L                               30
381100090209     C                   END
381200090209      *
381300120629      * Prosegue tab.PT e PU e PV
381400120629     C                   MOVEL     'PZ'          TABCOD
381500120629     C     TABCOD        CHAIN     EDTAB01L                           30
381600120629     C     *IN30         DOWEQ     '0'
381700120629      *
381800120629     C                   SELECT
381900120629     C     TABFLG        WHENNE    *BLANKS
382000120629      *
382100120629     C                   OTHER
382200120629     C                   MOVEL     TABKEY        PZ_key
382300120629     C                   Z-ADD     1             X
382400120629     C     PZ_key        LOOKUP    PT_key(X)                              32
382500120629     C   32              MOVEL     TABUNI        PZ_uni(X)
382600120629     C                   ENDSL
382700120629      *
382800120629     C     TABCOD        READE     EDTAB01L                               30
382900120629     C                   END
383000120629      *
383100090209     C* Caricamento Tabella codici clienti - tariffe
383200090209     C                   Z-ADD     0             X
383300090209     C                   MOVEL     'CL'          TABCOD
383400090209     C     TABCOD        CHAIN     EDTAB01L                           30
383500090209     C     *IN30         DOWEQ     '0'
383600090209     C     TABFLG        IFEQ      *BLANKS
383700090209     C                   ADD       1             X
383800100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
383900100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
384000090209     C                   END
384100090209     C     TABCOD        READE     EDTAB01L                               30
384200090209     C                   END
384300090209      *
384400090209     C* Caricamento Serivizi speciali
384500090209     C                   Z-ADD     0             X
384600090209     C                   MOVEL     'SS'          TABCOD
384700090209     C     TABCOD        CHAIN     EDTAB01L                           30
384800090209     C     *IN30         DOWEQ     '0'
384900090209     C     TABFLG        IFEQ      *BLANKS
385000090209     C                   ADD       1             X
385100100702     C                   eval       Key_ServSpec(X) = TABKEY
385200100702     C                   eval       SpecialServ(X)  = TABKEY
385300100702     C                   eval       UNI_ServSpec(X) = TABUNI
385400090209     C                   END
385500090209     C     TABCOD        READE     EDTAB01L                               30
385600090209     C                   END
385700090209      *
385800090209     C                   ENDSR
385900090205      * ?------------------------------------------------------------------ */
386000090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
386100090205      * ?------------------------------------------------------------------ */
386200090205     C     *pssr         BEGSR
386300090205     C
386400060710     C                   eval      esito = '2'
386500100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
386600110617      *
386700130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
386800130506     C                             IFCSUM D96A EDI ricevuto errato in UPLOAD -
386900130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
387000130506     c                             + ' verificare il '+
387100130506     c                             %editc(total_segmenti:'Z')
387200130506     c                             + ' segmento -> ' + dati
387300110617      *
387400100319     c                   if        §PVinv <> 'N'
387500100319     c                   exsr      invio_mail
387600100319     c                   end
387700130507      *
387800130507     c                   seton                                        LR
387900130507     c                   return
388000130507      *
388100090213     C                   ENDSR
388200090213      * ?------------------------------------------------------------------ */
388300090213      *?      Segnala Errore su TIVIN
388400090213      * ?------------------------------------------------------------------ */
388500090213     C     Error_DET     BEGSR
388600090213     C
388700100728     c                   eval      Segmento_in_errore = Conta_segmenti
388800090213     C                   eval      vinFLG = '2'
388900090213     c                   eval      Almeno_Un_Due ='S'
389000090213     c                   eval      esito  = '1'
389100090213     c                   eval      se_errore ='S'
389200090213     C                   eval      Err_in_detta = 'S'
389300090213     C
389400090213     C                   ENDSR
389500100705      * ?------------------------------------------------------------------ */
389600100705      *?   Controlla e decodifica Valori VALUE dei segmenti
389700100705      * ?------------------------------------------------------------------ */
389800100705     C     In_TAB_EDSTBL BEGSR
389900100705      *
390000100705      *   Cerca la decodifica del dato in TABELLA
390100100705     c                   clear                   trovata_EDSTBL    1
390200100705     c                   clear                   quale_campo       6
390300100705     c                   clear                   campo_dati_70A   70
390400100705     C                   movel     UNB_Mittente  etbUNB
390500100705     c     K_TAB1L       chain     edstbl01l
390600100705      *
390700100705      * prova quindi senza l'UNB specifico del cliente
390800100705     c                   if        not %Found(edstbl01l)
390900100705     C                   clear                   etbUNB
391000100705     c     K_TAB1L       chain     edstbl01l
391100100705     c                   end
391200100705      *
391300100705      *  Traduce i testi descrittivi se PRESENTE in tabella
391400100705      *   il campo di destinazione è definito sulla destra della descrizione
391500100705      *     ed è lungo 6 come i nomi definiti nei VAB.
391600100705     c                   if        %Found(edstbl01l)
391700100705     c                   move(p)   etbDESCRI     quale_campo
391800100705     c                   movel(p)  etbDATI       campo_dati_70A
391900100705     c                   move      'S'           trovata_EDSTBL
392000100705     c                   end
392100100705      *
392200100705      *
392300100705     C                   ENDSR
392400100705     c*==================================================================*
392500090209     C*? CNVDAT - DECODIFICA LA DATA
392600090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
392700090209     C*         - WDATA  (DATA ALFANUMERICA)
392800090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
392900090209     C*         - WHMS   (ORA NUMERICA)
393000090209     C*----------------------------------------------------------------
393100100702     C     Converte_Data BEGSR
393200090209     C*
393300090209     C                   MOVEL     ' '           WERRO             1
393400090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
393500090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
393600100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
393700090209     C                   Z-ADD     *ZEROS        WHMS              6 0
393800090209     C*
393900120905     C     Work_Format_3 IFEQ      *blank
394000120905     c                   if        %Len(%trim(Work_Data__35)) = 8
394100120905     C                   eval         Work_Format_3 = Data_senza_ora
394200120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
394300120905     C                   eval         Work_Format_3 = Data_con_ora
394400120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
394500120905     C                   eval         Work_Format_3 = Data_con_i_secondi
394600120905     c                   end
394700120905     C                   END
394800120905     C*
394900100705     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
395000100702     C                   MOVEL     Work_Data__35 WCOMO8            8
395100090209     C                   TESTN                   WCOMO8               99
395200090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
395300090209     C  N99              MOVEL     'S'           WERRO             1
395400090209     C                   END
395500090209     C*
395600100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
395700100702     C                   MOVEL     Work_Data__35 WCOMO12          12
395800100120     C                   TESTN                   WCOMO12              99
395900100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
396000100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
396100100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
396200090209     C  N99              MOVEL     'S'           WERRO             1
396300090209     C                   END
396400100120     C*
396500100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
396600110527     C                   MOVEL     *all'0'       WCOMO14          14
396700110527     C                   MOVEL     Work_Data__35 WCOMO14          14
396800100120     C                   TESTN                   WCOMO14              99
396900100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
397000100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
397100100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
397200100120     C  N99              MOVEL     'S'           WERRO             1
397300100120     C                   END
397400090209     C*
397500090209     C                   ENDSR
397600090210     C*----------------------------------------------------------------
397700100701     C*? Se_Numerico       - CONTROLLO NUMERICO
397800100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
397900100701     C*         - lunghezza_num   (Lunghezza campo output)
398000100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
398100090210     C*----------------------------------------------------------------
398200100701     C     Se_Numerico   BEGSR
398300090210     C*
398400090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
398500100701     C                   MOVEL     'N'           Campo_Numerico    1
398600100701     C                   MOVEL     *BLANKS       Numero_OK        15
398700090210     C                   Z-ADD     15            IN                3 0
398800100701     C                   Z-ADD     lunghezza_num IX                3 0
398900090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
399000100701     C                   MOVEA     test_num      NDA
399100090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
399200090210     C                   MOVEA     *ALL'0'       NDN
399300100630      *
399400090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
399500100701     C                   Z-ADD     lunghezza_num I                 3 0
399600090210     C     I             DOWGT     0                                            --- 1 -->
399700090210     C* ESTRAE IL CARATTERE DA ESAMINARE
399800100701     C     1             SUBST     test_num:I    WTEM01            1
399900090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
400000090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
400100090210     C     WTEM01        SCAN      WCA                                    99
400200090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
400300090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
400400090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
400500090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
400600090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
400700090210     C* DA DESTRA VERSO SINISTRA
400800090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
400900090210     C                   MOVEL     WTEMP1        NDN(IN)
401000090210     C                   SUB       1             IN
401100090210     C                   END                                                    <-- 3 ---
401200090210     C                   END                                                    <-- 2 ---
401300090210     C                   SUB       1             I
401400090210     C                   END                                                    <-- 1 ---
401500090210     C*
401600090210     C                   MOVEA     NDN           WNDN             15
401700090210     C     WNDN          IFNE      *ZEROS
401800100701     C                   MOVEA     NDN           Numero_OK        15
401900100701     C                   MOVEL     'S'           Campo_Numerico    1
402000090210     C                   END
402100090210     C*
402200090210     C                   ENDSR
402300090210     C*----------------------------------------------------------------
402400090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
402500090210     C*----------------------------------------------------------------
402600100705     C     Converte_CAP  BEGSR
402700090210     C*
402800090210     C                   MOVEL     *BLANKS       WCAP              9
402900090210     C     nad3251       IFNE      '0'
403000090210     C     '  '          SCAN      nad3251       LENGHT            2 0
403100090210     C                   SUB       1             LENGHT
403200090210     C     LENGHT        IFLE      5
403300090210     C     LENGHT        ANDGT     0
403400090210     C                   Z-ADD     LENGHT        T                 2 0
403500090210     C                   Z-ADD     5             S                 2 0
403600090210     C                   MOVEA     nad3251       CAP9
403700090210     C                   MOVEL     *ZEROS        CAP5
403800090210     C                   DO        LENGHT
403900090210     C                   MOVE      CAP9(T)       CAP5(S)
404000090210     C                   SUB       1             S
404100090210     C                   SUB       1             T
404200090210     C                   END
404300090210     C                   MOVEA     CAP5          WCAP5             5
404400090210     C     WCAP5         IFEQ      '0'
404500090210     C                   MOVEL     *BLANKS       WCAP
404600090210     C                   ELSE
404700090210     C                   MOVEA     CAP5          WCAP
404800090210     C                   END
404900090210     C*
405000090210     C                   ELSE
405100090210     C                   MOVEL     nad3251       WCAP
405200090210     C                   END
405300090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
405400090210     C     WCAP          IFNE      *BLANKS
405500090210     C     WFLCPF        ANDNE     0
405600090210     C                   MOVEL     WFLCPF        WELE              1 0
405700090210     C                   MOVE      WFLCPF        WPOS              1 0
405800090210     C                   MOVEL     *BLANK        WCPF
405900090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
406000090210     C                   ELSE
406100090210     C                   MOVEL     *BLANKS       WCPF
406200090210     C                   END
406300090210     C                   END
406400090210     C*
406500090210     C                   ENDSR
406600090211      * ?================================================================== */
406700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
406800090211      * ?================================================================== */
406900090211     C     Scrive_VAB    BEGSR
407000090211      *
407100090211     c                   clear                   err_bloccante     1
407200090211     c                   move      'S'           Almeno_Uno
407300090211      *
407400090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
407500090211     c                   exsr      UPDVAB
407600090211      *
407700090211     C* >>>>>>>>>>>>
407800090211     C*  Richiamo pgm per scrittura immediata VAB
407900090211     C     §PUWRK        IFEQ      ' '
408000090211      *
408100090211     C*  Richiamo pgm per esecuzione stampa
408200110110     C**** §PTCMR        IFEQ      'S'
408300110110     c******             MOVEL     BGM_nrCMR     d86CMR
408400110110     c******             if            RFF_NrCMR <> *Blank
408500110110     C******             MOVEL     RFF_NrCMR     d86CMR
408600110110     c******             end
408700110110     C******             ELSE
408800110110     C******             MOVEL     BGM_NrFviaggiod86CMR
408900110110     c******             if          RFF_NrFviaggio <> *Blank
409000110110     C******             MOVEL     RFF_NrFviaggiod86CMR
409100110110     c******             end
409200110110     C******             END
409300110110      *
409400110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
409500110110     c                   MOVEL     vabCMR        d86CMR
409600090211      *
409700090213     C                   MOVEL     ' '           d86RIE
409800090213     C                   MOVEL     §PUDTA        d86DTA
409900090213     C                   Z-ADD     1             d86CNT
410000090213     C                   Z-ADD     §PTKSC        d86CCM
410100090213     C                   MOVEL     *BLANKS       d86STA
410200090213     C                   MOVEL     'E'           d86MOD
410300090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
410400090211      * dove poi imposta la DS dei parametri passati
410500090213     C                   MOVEL     'LR'          d86CHI
410600090213     C                   MOVEL     'N'           d86CTL
410700090211      *
410800090211     c                   move      kpjbu         SvKpjbu
410900090211     c                   clear                   kpjbu
411000090216     C                   movel     *on           Commit_on         1
411100090213     C                   MOVEL     TRTC86ds      KPJBU
411200090213      * ?- Chiamata la TRTC86R2 -*
411300090213     C                   CALL      'TRTC86R2'
411400090211     C                   PARM                    KPJBA
411500090216     C                   PARM                    Commit_on
411600090211     c                   move      Svkpjbu       Kpjbu
411700090211      *
411800090211     C*  Controllo pgm per scrittura immediata VAB
411900100630     C     sk            IFNE      0
412000100630     C                   DO        sk            sx
412100100630     C                   Z-ADD     skCLienti(sx) d86CCM
412200090211      *
412300090211     c                   move      kpjbu         SvKpjbu
412400090211     c                   clear                   kpjbu
412500090216     C                   movel     *on           Commit_on         1
412600090213     C                   MOVEL     TRTC86ds      KPJBU
412700090213      * ?- Chiamata la TRTC86R2 -*
412800090213     C                   CALL      'TRTC86R2'
412900090211     C                   PARM                    KPJBA
413000090216     C                   PARM                    Commit_on
413100090211     c                   move      Svkpjbu       Kpjbu
413200090211     C                   END
413300090211     C                   END
413400090211      *
413500090211     C                   END
413600090211      * >>>>>>>>>>>>
413700090211     C                   if        se_errore  = 'S'
413800090211     c                   move      'S'           err_bloccante
413900090211     c                   goto      Error_VAB
414000090211     c                   end
414100090211     C*
414200090211     C     Error_VAB     Endsr
414300090211     C*----------------------------------------------------------------
414400090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
414500090211     C*----------------------------------------------------------------
414600090211     C     UPDVAB        BEGSR
414700090211      *
414800090211     C*  Se non è stato impostato il codice bolla lo imposto io
414900090211     C     VABCBO        IFEQ      *BLANKS
415000090211     C     VABCAS        IFGT      0
415100090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
415200090211     C                   ELSE
415300090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
415400090211     C                   END
415500131025      * se però è stato scritto e non era considerato il Contrassegno
415600131025      *  per motivi di lettura del record MOA successiva al Tipo Bolla
415700131025      *   qui sistema il codice bolla
415800131025     C                   Else
415900131025      *  Porto Franco +COD
416000131025     c                   if        VABCBO = '1' and  VABcas > *zeros
416100131025     C                   movel     '4'           VABCBO                         + C/Ass
416200131025      *  Porto Assegnato +COD
416300131025     c                   elseIF    VABCBO = '2' and  VABcas > *zeros
416400131025     C                   movel     '6'           VABCBO                         + C/Ass
416500131025     C                   ENDif
416600090211     C                   END
416700100701      * Imposta le NOte
416800100701     C     VABNOT        IFEQ      *BLANKS
416900100701     C                   MOVEL     Note_1        VABNOT
417000100701     C                   MOVEL     Note_2        VABNT2
417100100701     C                   ELSE
417200100701     C                   MOVEL     Note_1        VABNT2
417300100701     C                   END
417400090211      *
417500090211     C*  Attribuisco anno spedizione
417600100630     C     DTM_DtParten  IFGT      0                                            anno sped.
417700100630     C                   MOVEL     DTM_DtParten  VABAAS
417800090211     C                   ELSE
417900100630     C     DTM_DtFViag   IFGT      0
418000100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
418100090211     C                   END
418200090211     C                   END
418300090211      *
418400090211      *  Controllo dettaglio segnacolli
418500090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
418600100630     C                   EXSR      CTRsegn_EEX
418700090211     C                   ELSE
418800090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
418900090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
419000100630     C                   EXSR      CTRsegn_BAR
419100090211     C                   END
419200090211     C                   END
419300090211      *
419400090212      *  Imposto linea partenza e serie
419500100701     C                   eval      VABLNP = PT_vabLNP
419600100701     C                   eval      VABNRS = PT_vabNRS
419700090211      *
419800090211      *  Controllo se devo variare il codice trattamento merce
419900100701     C                   eval      VABCTM = PT_vabCTM
420000090211      *
420100090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
420200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
420300090211     c                   clear                   mantiene_nsp      1            *blk/'S'
420400100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
420500090211      *
420600090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
420700090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
420800090211      *
420900090211      *  Imposto codice cliente
421000090211     c                   Select
421100090211      *
421200100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
421300100319     C                   When        Det_RFF_FF_ksc  <> *zeros
421400100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
421500100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
421600090211      * forza LNP/CTM/NRS
421700100319     c                   if        Det_RFF_FF_lnp <> 0
421800100319     C                   eval      VABLNP = Det_RFF_FF_lnp
421900090211     c                   end
422000140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
422100140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
422200140901     c*******            if        Det_RFF_FF_nrs <> 0
422300100319     C                   eval      VABNRS = Det_RFF_FF_nrs
422400140901     c*******            end
422500100319     c                   if        Det_RFF_FF_ctm <> *blank
422600100319     C                   eval      VABCTM = Det_RFF_FF_ctm
422700090211     c                   end
422800110411     c                   if        Det_RFF_FF_tic <> *blank and VABCAS <>0
422900110411     C                   eval      VABTIC = Det_RFF_FF_tic
423000110411     c                   end
423100090211      *
423200090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
423300100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
423400100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
423500090211     c                   end
423600090211      *
423700100319      *  Specificato codice da qualificatore RFF+FF dalla testata
423800100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
423900100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
424000100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
424100090211      * forza LNP/CTM/NRS
424200100319     c                   if        Tes_RFF_FF_LNP  <> 0
424300100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
424400090211     c                   end
424500140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
424600140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
424700140901     c*******            if        Tes_RFF_FF_NRS  <> 0
424800100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
424900140901     c*******            end
425000100319     c                   if        Tes_RFF_FF_CTM  <> *blank
425100100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
425200090211     c                   end
425300110411     c                   if        Tes_RFF_FF_tic  <> *blank and VABCAS <>0
425400110411     C                   eval      VABTIC = Tes_RFF_FF_tic
425500110411     c                   end
425600090211      *
425700090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
425800100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
425900100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
426000090211     c                   end
426100090211      *
426200090211      *  Imposto dati da tabella - PT-
426300090211     C                   Other
426400090211      *
426500090211     C                   MOVEL     §PTKSC        VABCCM
426600090211     C                   MOVEL     §PTCTR        VABCTR
426700090211      *
426800090211     C                   Endsl
426900110411      *
427000110411      *  attenzione il tipo Contrassegno solo se c'è un valore
427100110411     c                   if        vabCAS = 0
427200110411     c                   clear                   vabTIC
427300110623     c                   else
427400110623      *
427500110623      * Se non è stato passato il Tipo incasso a fronte di particolarità sullo
427600110623      *  specifico cliente/partner
427700110707     c                   if        inviato_tipo_incasso =  'N' and
427800110707     c                             Det_RFF_FF_ksc > *zeros     and
427900110707     c                             Det_RFF_FF_tic = *blank
428000110623      *  e se c'è "??" sul tipo incasso deve Forzare questo tipo di Incasso sulla
428100110623      *  spedizione.
428200110623     c                   z-add     1             Y
428300110623     c                   movel(p)  '??'          Work_Data__35
428400110623     c                   move      §puTC         Work_Data__35
428500110623     c                   move      Work_Data__35 WData_35_2        2
428600110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
428700110623     c                   if         *in33
428800110623     c                   eval      vabTIC = TipoIncasso(Y)
428900110623     c                   end
429000110623      **
429100110623     c                   end
429200110623      **
429300110411     c                   end
429400090211      *
429500090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
429600090211      *    o dalla relativa CL
429700090211     C                   MOVEL     VABlnp        VABfgs
429800090211      *
429900090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
430000090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
430100090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
430200090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
430300090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
430400090211      *
430500090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
430600090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
430700090211     c                   else
430800090211      *
430900090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
431000090211     C*  lo attribuisco io
431100090211      **              **------------------**
431200090211     c                   exsr      repnum
431300090211      **              **------------------**
431400100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
431500090211      **              **------------------**
431600090211     c                   end
431700090211      *
431800090211     C*  Altrimenti attribuisco data del giorno
431900090211     C     VABMGS        IFEQ      0
432000090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
432100090211     C                   MOVE      WOGGI         VABMGS                         = oggi
432200090211     C                   END
432300090211     C*  Eseguo posizionamento su EDiVAB
432400090211     C                   Z-ADD     VABAAS        KAAS
432500090211     C                   Z-ADD     VABlnp        KLNP
432600090211     C                   Z-ADD     VABNRS        KNRS
432700090211     C                   Z-ADD     VABNSP        KNSP
432800090211     C                   MOVEL     SAVBOL        SALVA
432900090211     C     KVAB1         CHAIN     EDiVAB1L                           30
433000090211     C                   MOVEL     SALVA         SAVBOL
433100090211     C*  Imposto dati CMR
433200090211     C                   Z-ADD     1             VABCNT
433300090211     **
433400090211      *** Attenzione se si deve trattare il VAX con il CMR
433500090211      *** il campo VABCNT deve essere sempre impostato a (1)
433600090211     C     §puWRK        ifEQ      'S'
433700090211     C     §puNSP        andEQ     'S'
433800090211     C                   Z-ADD     1             VABCNT
433900090211     c                   end
434000090211     **
434100100630     C                   Z-ADD     Data_prepara  VABDTS
434200090212     C                   movel     '20'          VABDTS
434300100113     ** Forza la Data di Oggi
434400100113     C                   z-add     WOggi         VABDTS
434500100113     **
434600100630     C     Ora__prepara  mult      100           VABHMS
434700090211     C     §PTCMR        IFEQ      'S'
434800100630     C                   Z-ADD     DTM_DtCMR     VABDCM
434900100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
435000100701     c                   if          RFF_NrCMR <> *Blank
435100100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
435200100701     c                   end
435300090211     C                   ELSE
435400100630     C                   Z-ADD     DTM_DtFViag   VABDCM
435500100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
435600100701     c                   if          RFF_NrFviaggio <> *Blank
435700100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
435800100701     c                   end
435900090211     C                   END
436000100120      *
436100100813     c                   if          VABCMR = *Blank
436200100813     C                   MOVEL(p)  BGM_Id_TestataVABCMR
436300100813     c                   else
436400100813      *
436500100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
436600100120      * accorpare la bolle ad un unico destinatario da più parti
436700100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
436800100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
436900120629     C                   if         NAD_SF_Cliente <> *blank
437000120629     c                   eval      VABCMR = NAD_SF_Cliente
437100100708     c                   else
437200100813     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z')    +
437300100813     c                                      '_' + %editc(Data_Prepara:'Z') +
437400100813     c                                      '_' + %editc(Ora__Prepara:'Z')
437500100708     c                   end
437600100706     c                   end
437700100120      *
437800100813     c                   end
437900100921      *
438000100921     c                   if        vabdcm = 0
438100100921     C                   Z-ADD     Data_Prepara  VABDCM
438200100921     c                   end
438300100813      *
438400090211     C*  Se non è indicato il nome del mittente = Partner mittente
438500090211     C     VABRMO        IFEQ      *BLANKS
438600090211     C                   MOVEL     ACORAG        VABRMO
438700090211     C                   MOVEL     INDCAP        VABCMO
438800090211     C     INDCAE        IFNE      *BLANKS
438900090211     C                   MOVEL     INDCAE        VABCMO
439000090211     C                   END
439100090211     C     INDSTA        IFNE      *BLANKS
439200090211     C                   MOVEL     INDSTA        VABNMO
439300090211     C                   END
439400090211     C                   END
439500090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
439600090211     C     §1BFG1        IFEQ      'N'
439700090211     C     §1BFG7        ANDEQ     'N'
439800090211     C     §1BFG2        ANDEQ     'N'
439900090211     C***                  Z-ADD0         VABNRS
440000090211     C                   END
440100090211     C*  Imposto segancollo da - a
440200090211     C     §1BFG1        IFEQ      'S'
440300090211     C     §1BFG7        ANDEQ     'N'
440400090211     C     §1BF12        ANDNE     'E'
440500090211      *
440600100701     C     Conta_Segn    IFEQ      *ZEROS
440700090211     C                   CLEAR                   VABNCD
440800090211     C                   CLEAR                   VABNCA
440900090211     C                   ELSE
441000100630     C                   Z-ADD     segn_BAR(1)   VABNCD
441100100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
441200090211     C                   ENDIF
441300090211     C*
441400090211     C                   END
441500090211     C*
441600090211     C*  Se cap mittente originale a blank abblenco nazione
441700090211     C     VABCMO        IFEQ      *BLANKS
441800090211     C                   MOVEL     *BLANKS       VABNMO
441900090211     C                   END
442000100701     C*
442100090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
442200090603     C* Se in testata
442300100701     c                   if        NAD_destinatario_in_testata = 'S' and
442400100701     c                             NAD_destinatario_in_dettaglio <> 'S'
442500090603     c                   eval         VABrsd =  tes_VABrsd
442600090603     c                   eval         VABrd2 =  tes_VABrd2
442700090603     c                   eval         VABind =  tes_VABind
442800090603     c                   eval         VABlod =  tes_VABlod
442900090603     c                   eval         VABnzd =  tes_VABnzd
443000090603     c                   end
443100090603     C* Se in testata
443200100701     c                   if        NAD_mittente_in_testata = 'S' and
443300100701     c                             NAD_mittente_in_dettaglio <> 'S'
443400090603     c                   eval         VABrmo = tes_VABrmo
443500090603     c                   eval         VABnmo = tes_VABnmo
443600090603     c                   eval         VABcmo = tes_VABcmo
443700090603     c                   end
443800110613      *
443900110613      * Non ha calcolato l'instradamento poichè non aveva segmenti MEA
444000110613     C                   if         Instradamento = *blank
444100110613     c                               and vabCAD = *blank and vabpkb >0
444200110613     c                   exsr      Instrada_sped
444300110613     c                   end
444400100813      *
444500100813     C*  Per non bloccare la bollettazione
444600100813     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
444700100813     C*  o se il peso non è presente... imposto fisso il minimo 0,1
444800130315     c**************     if        vabpkb = 0
444900130315     c**************     z-add     0,1           vabpkb
445000130315     c**************     end
445100100813     C*
445200100813      *
445300100813     C*  Se cap mittente originale a blank abblenco nazione
445400100813     C     VABCMO        IFEQ      *BLANKS
445500100813     C                   MOVEL     *BLANKS       VABNMO
445600100813     C                   END
445700100813      *
445800100813      *   Forza il numero passato nel CNI nel riferimento Numerico
445900100813     c                   if        §PVCNIRMN = 'S'
446000100813     c                   z-add     CNI_SeNumericovabRMN
446100100813     c                   end
446200100921      *
446300100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
446400100921     C     §puWRK        ifEQ      'S'
446500100921     C     §puraggDES    andeq     'S'
446600100921     c                   clear                   vabLNA
446700100921     c                   clear                   vabZNC
446800100921     c                   clear                   vabCMR
446900100921      *  il nome identico
447000100921     c                   eval      VABCMR  = §punomeCMR
447100100921      *   e in più la data del giorno
447200100921     C     §puinGIO      ifEQ      'S'
447300100921     c                   eval      VABCMR = %trim(VABCMR)
447400100921     c                                    + %editc(WOggi_CMR:'X')
447500100921     c                   end
447600100921      *
447700100921     c                   end
447800100921      *
447900111028      ****
448000111028      **** se nel Riferimento Numerico viene passato il riferimento di una
448100111028      ****  nostra bolla di export precedentemente inviata di cui si sta facendo
448200111028      **** il reso. (Vedi AZKAR)    controlla
448300111028      **
448400111028     c                   if        PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
448500111028      *
448600111028      * controlla che contenga un numero di 14 cifre valide
448700111028      *   quindi la prima a sinistra deve essere (0)
448800111028     c                   movel     vabrmn        uno0              1 0
448900111028     c                   if        uno0 = 0
449000111028      *
449100111028      *   poi la seconda da sinistra deve NON essere (0)
449200111028     c                   movel     vabrmn        due00             2 0
449300111028     c                   move      due00         uno0
449400111028     c                   if        uno0 > 0
449500111028      *
449600140610     C                   move      VABRMN        dsspe
449700111028     c                   exsr      ctrl_seERAexp
449800111028      *
449900111028     c                   if           Sped_RESO = 'S'
450000111028     c                   exsr      Write_RESO
450100111028     c                   end
450200111028      *
450300111028     c                   end
450400111028     c                   end
450500111028     c                   end
450600111028      *
450700140610      *? ------------------------------------------------------------------------
450800140610      *?  SE il PARTNER è molto particolare ed utilizza un segmento di riferimento
450900140610      *?    per restituire i RESI o BOLLE generate da nostri ORM export
451000140610      *?  il caso: FR-EXPRESS - CALBER:ZZ e NON volendo utilizzare il RFF+CU,
451100140610      *?  utilizziamo questa tecnica x intercettare le spedizioni da segnalare in bollettazione.
451200140610      *? ------------------------------------------------------------------------
451300140610      ****
451400140610      **** se quindi il codice identificativo arriva carico
451500140610      ****  si verifica che sia un RESO oppure un ORM.
451600140610      **
451700140610      **  DEVE comunque essere abilitato a riceverlo mediante la "PT"
451800140623     c                   if        salva_REF_ORM <> *blank  and
451900140610     c                             PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
452000140610      **
452100140610     C                   move      Cod_RESO_ORM  dsspe
452200140610     c                   exsr      ctrl_seERAexp
452300140610      *
452400140610     c                   if           Sped_RESO = 'S'
452500140610     c                   exsr      Write_RESO
452600140610     c                   else
452700140610     c                   exsr      Write_da_ORM
452800140610     c                   end
452900140610      *
453000140610     c                   end
453100140610      *? ------------------------------------------------------------------------
453200140610      *?  PARTICOLARITA'
453300140610      *? ------------------------------------------------------------------------
453400120614      *
453500120614      * x Consegna ai PIANI "Floor"   "FLR"
453600120614     c                   If        FTX_ai_piani = 'P'
453700120614     c                   if             vabTC1 = *blank
453800120614     C                   MOVEL     'P'           VABTC1
453900120614     c                   elseIF         vabTC2 = *blank
454000120614     C                   MOVEL     'P'           VABTC2
454100120614     c                   end
454200120614     C                   END
454300120614      *
454400100813      *
454500090211      *? ------------------------------------------------------------------------
454600090213     C   30              WRITE     EDiVAB00                             99
454700090213     C  N30              UPDATE    EDiVAB00                             99
454800090211      *? ------------------------------------------------------------------------
454900090213     c   99              eval      errori_in_Wrt = 'S'
455000090211     C*
455100090211     c                   clear                   wri_vabtic        1
455200090211     C*
455300090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
455400100706     C                   EXSR      Scrive_Riferim
455500090211     C*
455600090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
455700090211     C     §1BF12        IFEQ      'E'
455800100706     C                   EXSR      Scrive_VAT
455900090211     C                   ELSE
456000090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
456100090211     C     §1BFG7        IFEQ      'S'
456200090211     C     §1BFG1        OREQ      'S'
456300100706     C                   EXSR      Scrive_VAD
456400090211     C                   END
456500090211     C                   END
456600090211      *
456700090211     C* Reimposto codice trattamento merce cliente
456800100628     C                   MOVEL     sav_DS1B      DS1B
456900090211     C**
457000090211     C* Do errore se previsto CMR ma non c'è
457100090211     C     §PTCMR        IFNE      ' '
457200090211     C                   EXSR      MEMCMR
457300090211     C                   END
457400090211     C* Do errore se previsto AFB ma non c'è
457500090211     C     §PTNFV        IFNE      ' '
457600090211     C                   EXSR      MEMAFB
457700090211     C                   END
457800090211     C*  Se il cliente vuole il ritorno delle date di consegna
457900090211     C*  è c'è una squdratura fra i segnacolli e il numero
458000090211     C*  dei colli che ci ha passato scrivo EDSUM
458100100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
458200090211     C                   EXSR      WRTSUM
458300090211     C                   END
458400090211     C*
458500090211     C                   ENDSR
458600090211     C*----------------------------------------------------------------
458700090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
458800090211     C*----------------------------------------------------------------
458900100630     C     CTRsegn_BAR   BEGSR
459000090211     C*
459100090211     C                   MOVEL     'N'           WNSGER            1
459200090211     C*
459300090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
459400100701     C     VABNCL        IFNE      Conta_Segn
459500090211     C                   MOVEL     'S'           WNSGER
459600100630     C                   eval       Squadratura_Colli = 'S'
459700090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
459800090211     c                              bollettati'
459900090211     C                   GOTO      FINSGN
460000090211     C                   END
460100100706      *
460200090211     C*  Riordino i segnacolli
460300090211     C     §1BFG7        IFEQ      'N'
460400100630     C                   SORTA     segn_BAR
460500090211     C*  Controllo se sono sequenziali
460600090211     C                   MOVEL     'N'           WNOSEQ            1
460700100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
460800100706     C*
460900100701     C     2             DO        Conta_Segn    X
461000100630     C     segn_BAR(X)   IFNE      WEXSGN
461100090211     C                   MOVEL     'S'           WNOSEQ
461200090211     C                   END
461300100630     C     segn_BAR(X)   ADD       1             WEXSGN
461400090211     C                   END
461500100706     C*
461600090211     C                   END
461700090211     C*
461800090211     C     FINSGN        ENDSR
461900090211     C*----------------------------------------------------------------
462000090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
462100090211     C*----------------------------------------------------------------
462200100630     C     CTRsegn_EEX   BEGSR
462300090211     C*
462400090211     C                   MOVEL     'N'           WNSGER            1
462500090211      *
462600090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
462700090211      *
462800100701     C     VABNCL        IFNE      totale_PCI
462900090211     C                   MOVEL     'S'           WNSGER
463000100630     C                   eval       Squadratura_Colli = 'S'
463100090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
463200090211     c                              bollettati'
463300090211     C                   END
463400090211      *
463500090211     C                   ENDSR
463600090211     C*----------------------------------------------------------------
463700090211     C*? ESEGUO SCRITTURA EDiVAD
463800090211     C*----------------------------------------------------------------
463900100706     C     Scrive_VAD    BEGSR
464000090211     C*
464100100701     C* Se non seq. scrivo i record
464200090211     C     §1BFG1        IFEQ      'S'
464300100701     C     Conta_Segn    ANDNE     *ZEROS
464400090211     C*
464500090211     C     §1BFG7        IFEQ      'S'
464600100701     C                   DO        Conta_Segn    X
464700090211     C                   Z-ADD     VABCCM        KCCM
464800100630     C                   Z-ADD     segn_BAR(X)   KNCD
464900090211     C                   Z-ADD     VABCNT        VADCNT
465000090211     C                   MOVEL     VABCMR        VADCMR
465100090211     C                   Z-ADD     VABAAS        VADAAS
465200090211     C                   Z-ADD     VABLNP        VADLNP
465300090211     C                   Z-ADD     VABNRS        VADNRS
465400090211     C                   Z-ADD     VABNSP        VADNSP
465500090211     C                   Z-ADD     1             VADNCL
465600100630     C                   MOVEL     segn_BAR(X)   VADCDP
465700100630     C                   Z-ADD     segn_BAR(X)   VADNCD
465800090211     C                   Z-ADD     *ZEROS        VADNCA
465900090211     C                   Z-ADD     VABCCM        VADCCM
466000090211     C                   movel     VABfgs        VADfgs
466100090213     C                   WRITE     EDiVAD00                             99
466200090213     c   99              eval      errori_in_Wrt = 'S'
466300090211     C                   END
466400090211     C* Se sequenziali scrivo un solo record
466500090211     C                   ELSE
466600090211     C                   Z-ADD     VABAAS        VADAAS
466700090211     C                   Z-ADD     VABLNP        VADLNP
466800090211     C                   Z-ADD     VABNRS        VADNRS
466900090211     C                   Z-ADD     VABNSP        VADNSP
467000090211     C                   Z-ADD     VABNCL        VADNCL
467100100630     C                   Z-ADD     segn_BAR(1)   VADNCD
467200100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
467300090211     C                   Z-ADD     VABCCM        VADCCM
467400090211     C                   movel     VABfgs        VADfgs
467500140902     C                   MOVEL     VABCMR        VADCMR
467600090213     C                   WRITE     EDiVAD00                             99
467700090213     c   99              eval      errori_in_Wrt = 'S'
467800090211     C                   END
467900090211     C*
468000090211     C                   ELSE
468100100701     C                   DO        Conta_Segn    X
468200090211     C                   Z-ADD     VABCCM        KCCM
468300100630     C                   Z-ADD     segn_BAR(X)   KNCD
468400090211     C                   Z-ADD     VABAAS        VADAAS
468500090211     C                   Z-ADD     VABLNP        VADLNP
468600090211     C                   Z-ADD     VABNRS        VADNRS
468700090211     C                   Z-ADD     VABNSP        VADNSP
468800100630     C                   MOVEL     segn_BAR(X)   VADCDP
468900090211     C                   Z-ADD     1             VADNCL
469000090211     C                   Z-ADD     *ZEROS        VADNCD
469100090211     C                   Z-ADD     *ZEROS        VADNCA
469200090211     C                   Z-ADD     VABCCM        VADCCM
469300090211     C                   movel     VABfgs        VADfgs
469400140902     C                   MOVEL     VABCMR        VADCMR
469500090213     C                   WRITE     EDiVAD00                             99
469600090213     c   99              eval      errori_in_Wrt = 'S'
469700090211     C                   END
469800090211     C*
469900090211     C                   END
470000090211     C*
470100090211     C                   ENDSR
470200090211     C*----------------------------------------------------------------
470300090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
470400090211     C*----------------------------------------------------------------
470500100706     C     Scrive_RiferimBEGSR
470600090211      *
470700090211      *  riferimento di consegna destinatario
470800100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
470900100701     c                             CTA_destinatario_in_testata   <> *blank
471000090211     C                   Z-ADD     VABCNT        VATCNT
471100090211     C                   MOVEL     VABCMR        VATCMR
471200090211     C                   Z-ADD     VABCCM        VATCCM
471300090211     C                   movel     VABfgs        VATfgs
471400090211     C                   Z-ADD     VABLNP        VATLNP
471500090211     C                   Z-ADD     VABAAS        VATAAS
471600090211     C                   Z-ADD     VABNRS        VATNRS
471700090211     C                   Z-ADD     VABNSP        VATNSP
471800090211     C                   MOVEL     'A'           VATTRC
471900100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
472000100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
472100100701     c                   else
472200100701     C                   eval       VATNOT = CTA_destinatario_in_testata
472300100701     c                   end
472400090213     C                   WRITE     EDiVAT00                             99
472500090213     c   99              eval      errori_in_Wrt = 'S'
472600090211     c                   end
472700090211      *
472800090211      *  riferimento di consegna telefonico
472900100701     c                   if        COM_telefono_in_dettaglio <> *blank or
473000100701     c                             COM_telefono_in_testata   <> *blank
473100090211     C                   Z-ADD     VABCNT        VATCNT
473200090211     C                   MOVEL     VABCMR        VATCMR
473300090211     C                   Z-ADD     VABCCM        VATCCM
473400090211     C                   movel     VABfgs        VATfgs
473500090211     C                   Z-ADD     VABLNP        VATLNP
473600090211     C                   Z-ADD     VABAAS        VATAAS
473700090211     C                   Z-ADD     VABNRS        VATNRS
473800090211     C                   Z-ADD     VABNSP        VATNSP
473900090211     C                   MOVEL     'B'           VATTRC
474000100701     c                   if        COM_telefono_in_dettaglio <> *blank
474100100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
474200100701     c                   else
474300100701     c                   eval      VATNOT = COM_telefono_in_testata
474400100701     c                   end
474500090213     C                   WRITE     EDiVAT00                             99
474600090213     c   99              eval      errori_in_Wrt = 'S'
474700090211     c                   end
474800090211      *
474900090211      * riferimento del Partner che una volta veniva riportato sul BL4
475000100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
475100100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
475200100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
475300100119      *
475400100701     c                   if        RFF_RifSpediz <> *blank
475500090211     C                   Z-ADD     VABCNT        VATCNT
475600090211     C                   MOVEL     VABCMR        VATCMR
475700090211     C                   Z-ADD     VABCCM        VATCCM
475800090211     C                   movel     VABfgs        VATfgs
475900090211     C                   Z-ADD     VABLNP        VATLNP
476000090211     C                   Z-ADD     VABAAS        VATAAS
476100090211     C                   Z-ADD     VABNRS        VATNRS
476200090211     C                   Z-ADD     VABNSP        VATNSP
476300090211     C                   MOVEL     'C'           VATTRC
476400100701     C                   MOVEL     RFF_RifSpediz VATNOT
476500090213     C                   WRITE     EDiVAT00                             99
476600090213     c   99              eval      errori_in_Wrt = 'S'
476700090211     c                   end
476800110907      **
476900110907      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
477000110907      *   sulla consegna della sua merce
477100110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
477200110907     c                                  <> *blank
477300110907      *
477400110907     C                   Z-ADD     VABCNT        VATCNT
477500110907     C                   MOVEL     VABCMR        VATCMR
477600110907     C                   Z-ADD     VABCCM        VATCCM
477700110907     C                   movel     VABfgs        VATfgs
477800110907     C                   Z-ADD     VABLNP        VATLNP
477900110907     C                   Z-ADD     VABAAS        VATAAS
478000110907     C                   Z-ADD     VABNRS        VATNRS
478100110907     C                   Z-ADD     VABNSP        VATNSP
478200110907     C                   MOVEL     'I'           VATTRC
478300110907     C                   eval       VATNOT =
478400110907     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
478500110907     C                   WRITE     EDiVAT00                             99
478600110907     c   99              eval      errori_in_Wrt = 'S'
478700110907     c                   end
478800110907      *
478900110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
479000110907     c                                 <> *blank
479100110907      *
479200110907     C                   Z-ADD     VABCNT        VATCNT
479300110907     C                   MOVEL     VABCMR        VATCMR
479400110907     C                   Z-ADD     VABCCM        VATCCM
479500110907     C                   movel     VABfgs        VATfgs
479600110907     C                   Z-ADD     VABLNP        VATLNP
479700110907     C                   Z-ADD     VABAAS        VATAAS
479800110907     C                   Z-ADD     VABNRS        VATNRS
479900110907     C                   Z-ADD     VABNSP        VATNSP
480000110907     C                   MOVEL     'J'           VATTRC
480100110907     C                   eval       VATNOT =
480200110907     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
480300110907     C                   WRITE     EDiVAT00                             99
480400110907     c   99              eval      errori_in_Wrt = 'S'
480500110907     c                   end
480600110907      *
480700131025      *
480800131025      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
480900131025      *   sulla consegna della sua merce
481000131025      *
481100131025     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
481200131025     c                                  <> *blank
481300131025     c                             or FTX_MAIL_come_servizio = 'N'
481400131025      *
481500131025     C                   Z-ADD     VABCNT        VATCNT
481600131025     C                   MOVEL     VABCMR        VATCMR
481700131025     C                   Z-ADD     VABCCM        VATCCM
481800131025     C                   movel     VABfgs        VATfgs
481900131025     C                   Z-ADD     VABLNP        VATLNP
482000131025     C                   Z-ADD     VABAAS        VATAAS
482100131025     C                   Z-ADD     VABNRS        VATNRS
482200131025     C                   Z-ADD     VABNSP        VATNSP
482300131025     C                   MOVEL     'S'           VATTRC
482400131025      *
482500131025      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
482600131025      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
482700131025     C                   eval       VATNOT =
482800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
482900131025      *
483000131025      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
483100131025      * seguenti attivano o meno il servizio x SMS o MAIL
483200131025      *
483300131025      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
483400131025      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
483500131025     C                   eval      %subst(VATNOT:17:1) = 'S'
483600131025     c                   if        FTX_SMS_come_servizio = 'N'
483700131025     c                              or
483800131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
483900131025     c                              = *blank
484000131025     C                   eval      %subst(VATNOT:17:1) = 'N'
484100131025     c                   end
484200131025      *
484300131025     C                   eval      %subst(VATNOT:18:1) = 'S'
484400131025     c                   if        FTX_MAIL_come_servizio = 'N'
484500131025     c                              or
484600131025     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
484700131025     c                              = *blank
484800131025     C                   eval      %subst(VATNOT:18:1) = 'N'
484900131025     c                   end
485000131025      *
485100131025     C                   WRITE     EDiVAT00                             99
485200131025     c   99              eval      errori_in_Wrt = 'S'
485300131025     c                   end
485400131025      *
485500100119      *
485600090211     C                   ENDSR
485700090211     C*----------------------------------------------------------------
485800090211     C*? ESEGUO SCRITTURA EDiVAT
485900090211     C*----------------------------------------------------------------
486000100706     C     Scrive_VAT    BEGSR
486100090211      *
486200100701     C                   DO        totale_PCI    X
486300090211     C                   Z-ADD     VABCNT        VATCNT
486400090211     C                   MOVEL     VABCMR        VATCMR
486500090211     C                   Z-ADD     VABCCM        VATCCM
486600090211     C                   movel     VABfgs        VATfgs
486700090211     C                   Z-ADD     VABLNP        VATLNP
486800090211     C                   Z-ADD     VABAAS        VATAAS
486900090211     C                   Z-ADD     VABNRS        VATNRS
487000090211     C                   Z-ADD     VABNSP        VATNSP
487100090211     C                   MOVEL     'E'           VATTRC
487200090211      *
487300090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
487400090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
487500090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
487600100701     c                   if        PU_lunVAT     > 0
487700090211      *
487800090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
487900100630     C                   eval      VATnot =
488000100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
488100090211      *
488200090211     c                   else
488300090211      *
488400090211      * Se non è impostato nulla prende il segnacollo passato così com'è
488500100630     C                   MOVEL     segn_EEX(X)   VATNOT
488600090211     c                   end
488700090211      *
488800090213     C                   WRITE     EDiVAT00                             99
488900090213     c   99              eval      errori_in_Wrt = 'S'
489000090211     C                   END
489100090211      *
489200090211     C                   ENDSR
489300090211     C*----------------------------------------------------------------
489400090211     C*? ESEGUO SCRITTURA EDSUM
489500090211     C*----------------------------------------------------------------
489600090211     C     WRTSUM        BEGSR
489700090211     C*
489800100701     C* Se non seq. scrivo i   record
489900100701     C                   DO        Conta_Segn    X
490000090211     C                   Z-ADD     VABAAS        SUMAAS
490100090211     C                   Z-ADD     VABLNP        SUMFLS
490200100701     C                   MOVEL     PT_vabCCM     SUMLNP
490300090211     C                   Z-ADD     VABLNA        SUMLNA
490400090211     C                   Z-ADD     VABZNC        SUMZNC
490500090211     C                   Z-ADD     VABNRS        SUMNRS
490600090211     C                   Z-ADD     VABNSP        SUMNSP
490700100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
490800100701     C                   Z-ADD     PT_vabCCM     SUMCCM
490900090211     C                   MOVEL     *BLANKS       SUMSTS
491000090211     C                   MOVEL     *BLANKS       SUMFLG
491100100813     C                   eval       SUMCNI = CNI_Identificativo_Bolla
491200090211     C                   MOVEL     VABCMR        SUMCMR
491300090211     C                   Z-ADD     0             SUMNFE
491400090211     C                   Z-ADD     VABCCM        VADCCM
491500090213     C                   WRITE     EDSUM000                             99
491600090213     c   99              eval      errori_in_Wrt = 'S'
491700090211     C                   END
491800090211     C*
491900090211     C                   ENDSR
492000090211     c*==================================================================*
492100090211      * Manda un Msg in Posta AS Sede a EDP*
492200090211     c*==================================================================*
492300090211     c     SEND_MSG_EDP  begsr
492400090211      *
492500090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
492600090211      *   Lo manda una sola volta per lavoro
492700090211     c                   IF        Snd_Msg_alert  <> 'N' and
492800090211     c                             User_msg <> *blank
492900090211     c                   z-add     1             Jx
493000090211     C                   exsr      CalQEZSNDMG
493100090211     c                   end
493200090211      *
493300090211     c                   endsr
493400090213     c**********************************************************************
493500090213     c* reperimento numero Spedizione
493600090213     c**********************************************************************
493700090213     C     repnum        BEGSR
493800090213      *
493900090213      *    deve controllare sulla tabella "3C" se il numero spedizione
494000090213      *     deve essere mantenuto oppure no
494100090213     C*
494200090213     C                   clear                   Ds3C
494300090213     C                   Z-ADD     CODUT         TBLKUT
494400090213     C                   MOVEL     '3C'          TBLCOD
494500090213     C                   movel(p)  VABccm        TBLKEY
494600090213     C     KTABel        CHAIN     TABEL00F                           30
494700090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
494800090213     C                   MOVEL     TBLUNI        Ds3C
494900090213     C                   END
495000090213      *
495100090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
495200090213     c                   if        §3CFsp <> 'S'
495300090213      *
495400090213     C* NSP => Stacco un numeratore da AZNUM
495500090213     c                   movel     kpjbu         svkpjbu
495600090213     c                   clear                   kpjbu
495700090213     C                   clear                   TRUL33DS
495800090213     C                   eval      I33OPE = *zeros
495900090213     C                   eval      I33CNU = 302
496000090213     C                   eval      I33NUM = 1
496100090213     C                   movel     TRUL33DS      KPJBU
496200090213     C                   call      'TRUL33R'
496300090213     C                   parm                    KPJBA
496400090213     C                   movel     KPJBU         TRUL33DS
496500090213     c                   movel     svkpjbu       kpjbu
496600090213      ****
496700090213     C                   if        O33ERR = *zeros
496800100630     c                   z-add     o33nrf        NUM_Spediz
496900090213     C                   else
497000090213     C                   endif
497100090213      ****
497200090213     c                   ELSE
497300090213      *   se si vuole mantenere il numero spedizione
497400090213      ****
497500090213     C                   MOVEL     WOGGI         WANNO             4 0
497600090213     C                   MOVE      WANNO         KAAA
497700090213     C                   Z-ADD     3             KCNU
497800090213     C                   Z-ADD     VABlnp        KFIL
497900090213     C     KNUF          CHAIN     FLNUF                              9999
498000090213     C     *IN99         IFEQ      '0'
498100090213     C                   ADD       1             NUFNUM
498200100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
498300090213     C                   UPDATE    FLNUF
498400090213     C                   ELSE
498500090213     C                   END
498600090213     C*
498700090213     c                   EndIF
498800090213      **
498900090213     C                   ENDSR
499000090213     C*----------------------------------------------------------------
499100090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
499200090213     C*----------------------------------------------------------------
499300090213     C     MEMCMR        BEGSR
499400090213     C*
499500090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
499600090213     C     §PTCM1        IFEQ      'S'
499700090213     C                   CLEAR                   EDRDE000
499800090213     C                   MOVEL     'TD1154'      KNMC
499900090213     C                   Z-ADD     1             KNRP
500000100701     C                   MOVEL     PT_vabCCM     KLNP1
500100090213     C     KRDE          CHAIN     EDRDE01L                           31
500200090213     C     *IN31         IFEQ      '1'
500300090213     C                   Z-ADD     VABAAS        RDEAAS
500400090213     C                   Z-ADD     VABLNP        RDELNP
500500090213     C                   Z-ADD     VABNRS        RDENRS
500600090213     C                   Z-ADD     VABNSP        RDENSP
500700090213     C                   MOVEL     'TD1154'      RDENMC
500800090213     C                   Z-ADD     1             RDENRP
500900100706     C                   MOVEL     BGM_nrCMR     RDEVAL
501000100701     c                   if          RFF_NrCMR <> *Blank
501100100701     C                   MOVEL     RFF_NrCMR     RDEVAL
501200100701     c                   end
501300090213     C                   WRITE     EDRDE000                             99
501400090213     c   99              eval      errori_in_Wrt = 'S'
501500090213     C                   END
501600090213     C*  Memorizzo qualificatore CMR
501700090213     C                   CLEAR                   EDRDE000
501800090213     C                   MOVEL     'TD1153'      KNMC
501900090213     C                   Z-ADD     1             KNRP
502000100701     C                   MOVEL     PT_vabCCM     KLNP1
502100090213     C     KRDE          CHAIN     EDRDE01L                           31
502200090213     C     *IN31         IFEQ      '1'
502300090213     C                   Z-ADD     VABAAS        RDEAAS
502400090213     C                   Z-ADD     VABLNP        RDELNP
502500090213     C                   Z-ADD     VABNRS        RDENRS
502600090213     C                   Z-ADD     VABNSP        RDENSP
502700090213     C                   Z-ADD     1             RDENRP
502800090213     C                   MOVEL     'TD1153'      RDENMC
502900090213     C                   MOVEL     'CMR'         RDEVAL
503000090213     C                   WRITE     EDRDE000                             99
503100090213     c   99              eval      errori_in_Wrt = 'S'
503200090213     C                   END
503300090213     C*  Memorizzo la data
503400090213     C                   CLEAR                   EDRDE000
503500090213     C                   MOVEL     'TD2380'      KNMC
503600090213     C                   Z-ADD     1             KNRP
503700100701     C                   MOVEL     PT_vabCCM     KLNP1
503800090213     C     KRDE          CHAIN     EDRDE01L                           31
503900090213     C     *IN31         IFEQ      '1'
504000090213     C                   Z-ADD     VABAAS        RDEAAS
504100090213     C                   Z-ADD     VABLNP        RDELNP
504200090213     C                   Z-ADD     VABNRS        RDENRS
504300090213     C                   Z-ADD     VABNSP        RDENSP
504400090213     C                   Z-ADD     1             RDENRP
504500090213     C                   MOVEL     'TD2380'      RDENMC
504600100630     C                   MOVEL     DTM_DtCMR     RDEVAL
504700090213     C                   WRITE     EDRDE000                             99
504800090213     c   99              eval      errori_in_Wrt = 'S'
504900090213     C                   END
505000090213     C*
505100090213     C                   END                                                    §PTCM1 = 'S'
505200090213     C*
505300090213     C                   ENDSR
505400090213     C*----------------------------------------------------------------
505500090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
505600090213     C*----------------------------------------------------------------
505700090213     C     MEMAFB        BEGSR
505800090213     C*
505900090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
506000090213     C     §PTNF1        IFEQ      'S'
506100090213     C                   CLEAR                   EDRDE000
506200100701     C                   MOVEL     PT_vabCCM     KLNP1
506300090213     C                   MOVEL     'TD1154'      KNMC
506400090213     C                   Z-ADD     1             KNRP
506500090213     C     KRDE          CHAIN     EDRDE01L                           31
506600090213     C     *IN31         IFEQ      '1'
506700090213     C                   Z-ADD     VABAAS        RDEAAS
506800090213     C                   Z-ADD     VABLNP        RDELNP
506900090213     C                   Z-ADD     VABNRS        RDENRS
507000090213     C                   Z-ADD     VABNSP        RDENSP
507100090213     C                   MOVEL     'TD1154'      RDENMC
507200100701     C                   if        §PTCM1 <> 'S' or
507300100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
507400090213     C                   Z-ADD     1             RDENRP
507500090213     C                   ELSE
507600090213     C                   Z-ADD     2             RDENRP
507700090213     C                   END
507800100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
507900100701     c                   if          RFF_NrFviaggio <> *Blank
508000100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
508100100701     c                   end
508200090213     C                   WRITE     EDRDE000                             99
508300090213     c   99              eval      errori_in_Wrt = 'S'
508400090213     C                   END
508500090213     C*  Memorizzo qualificatore CMR
508600090213     C                   CLEAR                   EDRDE000
508700100701     C                   MOVEL     PT_vabCCM     KLNP1
508800090213     C                   MOVEL     'TD1153'      KNMC
508900090213     C                   Z-ADD     1             KNRP
509000090213     C     KRDE          CHAIN     EDRDE01L                           31
509100090213     C     *IN31         IFEQ      '1'
509200090213     C                   Z-ADD     VABAAS        RDEAAS
509300090213     C                   Z-ADD     VABLNP        RDELNP
509400090213     C                   Z-ADD     VABNRS        RDENRS
509500090213     C                   Z-ADD     VABNSP        RDENSP
509600100701     C                   if        §PTCM1 <> 'S' or
509700100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
509800090213     C                   Z-ADD     1             RDENRP
509900090213     C                   ELSE
510000090213     C                   Z-ADD     2             RDENRP
510100090213     C                   END
510200090213     C                   MOVEL     'TD1153'      RDENMC
510300090213     C                   MOVEL     'AFB'         RDEVAL
510400090213     C                   WRITE     EDRDE000                             99
510500090213     c   99              eval      errori_in_Wrt = 'S'
510600090213     C                   END
510700090213     C*  Memorizzo la data
510800090213     C                   CLEAR                   EDRDE000
510900100701     C                   MOVEL     PT_vabCCM     KLNP1
511000090213     C                   MOVEL     'TD2380'      KNMC
511100090213     C                   Z-ADD     1             KNRP
511200090213     C     KRDE          CHAIN     EDRDE01L                           31
511300090213     C     *IN31         IFEQ      '1'
511400090213     C                   Z-ADD     VABAAS        RDEAAS
511500090213     C                   Z-ADD     VABLNP        RDELNP
511600090213     C                   Z-ADD     VABNRS        RDENRS
511700090213     C                   Z-ADD     VABNSP        RDENSP
511800100701     C                   if        §PTCM1 <> 'S' or
511900100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
512000090213     C                   Z-ADD     1             RDENRP
512100090213     C                   ELSE
512200090213     C                   Z-ADD     2             RDENRP
512300090213     C                   END
512400090213     C                   MOVEL     'TD2380'      RDENMC
512500100630     C                   MOVEL     DTM_DtFViag   RDEVAL
512600090213     C                   WRITE     EDRDE000                             99
512700090213     c   99              eval      errori_in_Wrt = 'S'
512800090213     C                   END
512900090213     C*
513000090213     C                   END                                                    §PTCM1 = 'S'
513100090213     C*
513200090213     C                   ENDSR
513300090213     c*==================================================================*
513400090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
513500090213     c*==================================================================*
513600090213     c     Imp_ADDR_mail begsr
513700090213      *
513800090213     c                   clear                   Indirizzi
513900090213      *
514000090213      *  Lunghezza indirizzi presenti
514100100630     c                   eval      Lunghezza_Indirizzo =
514200100630     c                                       %Len(%TrimR(Mail_Ind))
514300090213     c                   z-add     1             al_char           3 0
514400090213     c                   z-add     1             dal_char          3 0
514500090213     c                   z-add     1             tot_char          3 0
514600090213      *
514700090213     c     successivo    tag
514800090213      *
514900090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
515000090213      *   e aggiunge il dominio Bartolini + il (;) separatore
515100090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
515200090213     c                   eval      tot_char = al_char - dal_char
515300100630     c                   z-add     dal_char      dalCarattere
515400100630     c                   z-add     tot_char      xTOTcaratteri
515500090213      *
515600090213     c                   clear                   Indir_Bartol     40
515700090213     c                   clear                   Indir_Parz       20
515800100630     c                   eval      Indir_Parz =
515900100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
516000100630      *
516100090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
516200090213     c                             %Trim(bart_it)
516300090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
516400090213      *
516500100630     c                   if        al_char = Lunghezza_Indirizzo
516600090213     c                   goto      fine_indmail
516700090213     c                   else
516800090213     c                   eval      dal_char = ( al_char + 1 )
516900090213     c                   goto      successivo
517000090213     c                   end
517100090213      *
517200090213     C     fine_indmail  TAG
517300090213      *
517400090213     C                   ENDSR
517500090213     c*==================================================================*
517600090213      * Manda un Msg x E-mail
517700090213     c*==================================================================*
517800090213     c     Invio_Mail    begsr
517900090213      *
518000090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
518100090213     C* Inizializzo variabili
518200090213     C                   movel     *blanks       wrkEml          253
518300090213     C                   movel     *blanks       wrkMsg         5000
518400090213     C                   movel     *blanks       wrkOgg           44
518500090213      *
518600090213     C* Valorizzo i campi della e-m@ail - indirizzo
518700100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
518800090213     C                   eval      wrkEml = Indirizzi
518900090213     C                   eval      wrkOgg = Oggetto
519000090213     C                   eval      wrkMsg = Messaggio
519100110203      ********
519200110203     C********           call(e)   'TIS701C'
519300110203     C********           parm                    wrkEml
519400110203     C********           parm                    wrkOgg
519500110203     C********           parm                    wrkMsg
519600110203      ** *****
519700110203     C                   call(e)   'TRTCT00R2'
519800110203     C                   parm                    wrkEml
519900110203     C                   parm                    wrkOgg
520000110203     C                   parm                    wrkMsg
520100090213     C*
520200090213     C                   ENDSR
520300090211     ***********************************************************************
520400090211     **
520500090211     ** Send Message (QEZSNDMG) API
520600090211     **
520700090211     ***********************************************************************
520800090211     C     CalQEZSNDMG   BEGSR
520900090211      *
521000090211     ** Invio messaggio all'utente.
521100090211     C                   EVAL      SndMg03 = msgDSP
521200090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
521300090211     C                   EVAL      SndMg05(Jx) = User_msg
521400090211     C                   EVAL      SndMg06 = Jx
521500090211     C                   CLEAR                   QUSEC
521600090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
521700090211
521800090211     C                   CALL      'QEZSNDMG'
521900090211     C                   PARM                    SndMg01
522000090211     C                   PARM                    SndMg02
522100090211     C                   PARM                    SndMg03
522200090211     C                   PARM                    SndMg04
522300090211     C                   PARM                    SndMg05
522400090211     C                   PARM                    SndMg06
522500090211     C                   PARM                    SndMg07
522600090211     C                   PARM                    SndMg08
522700090211     C                   PARM                    Qusec
522800090211     C                   PARM                    SndMg10
522900090211     C                   PARM                    SndMg11
523000090211     C                   PARM                    SndMg12
523100090211      *
523200090211     C                   ENDSR
523300100708      * ?------------------------------------------------------------------ */
523400100708      *  Controlla la trasmissione   se completa
523500100708      * ?------------------------------------------------------------------ */
523600100708     c     Check_Trasm   Begsr
523700100708
523800100708     C                   clear                   se_errore
523900100708     C                   clear                   Riga_iNIZIo       1
524000100708     C                   clear                   Riga_fINe         1
524100100708      ** primo  record
524200100708     c     *start        setll     tivin00r
524300100708      *
524400100708     c     rileggi       tag
524500100708     c                   EXSR      LEGGE_TIVIN_R
524600100708      *
524700100708     c                   if        EoFTivin <> 'S'
524800100708      *
524900100708     c                   if        dati = *blank
525000100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
525100100708      * le leggo e le escludo.
525200100708     C                   eval      vinFLG = '1'
525300100708     c                   update    Tivin000
525400100708     c                   goto      rileggi
525500100708     c                   end
525600100708      * ?              /*---------------------- */
525700100708     c                   exsr      Decod_Segmento
525800100708      * ?              /*---------------------- */
525900100708     c                   endif
526000100708      **
526100100708      ** ultimo record
526200100708     c     *hival        setll     tivin00r
526300100708     c     leggiAncora   tag
526400100708     c                   readp     tivin00r
526500100708     c                   if        %Eof(tivin00r)
526600100708     c                   move      'S'           EoFTivin
526700100708     c                   else
526800100708     c                   clear                   dati
526900100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
527000100708      *
527100100708     c                   if        dati = *blank
527200100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
527300100708      * le leggo e le escludo.
527400100708     C                   eval      vinFLG = '1'
527500100708     c                   update    Tivin000
527600100708     c                   goto      leggiAncora
527700100708     c                   end
527800100708      * ?              /*---------------------- */
527900100708     c                   exsr      Decod_Segmento
528000100708      * ?              /*---------------------- */
528100100708     c                   endif
528200100708
528300100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
528400100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
528500100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
528600100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
528700100708      * ?-----> Errore
528800100708     C                   eval      se_errore = 'S'
528900100708     c                   end
529000100811      *
529100100708     c                   endSR
529200100708      * ?------------------------------------------------------------------ */
529300100811      *
529400100811      * ?------------------------------------------------------------------ */
529500100811     c     chiama_DecoderbegSR
529600100811      *
529700100811     C                   clear                   keyUNBCLI
529800100811     C                   clear                   keyTIPOMSG
529900100811     C                   clear                   keyVERSION
530000100811     C                   clear                   keyRELEASE
530100100811     C                   clear                   keyAGENCY
530200100811     C                   clear                   keyASSOCIA
530300110530      **--------
530400110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
530500110530      **--------
530600110530     c                   movel     UNB_Mittente  keyUNBCLI
530700100811      **--------
530800100811      *  Ritorna la DS GENERICA oppure l'errore
530900100811      **--------
531000100811      *  Chiamata generica x decodificare il singolo segmento letto
531100100811     c                   call      'TRTCT00R1'
531200100811      * input
531300100811     c                   parm                    tipo_seg
531400100811     c                   parm                    segmento
531500100811     C                   parm                    keyUNBCLI
531600100811     C                   parm      'IFCSUM'      keyTIPOMSG
531700100811     C                   parm      'D'           keyVERSION
531800100811     C                   parm      '96A'         keyRELEASE
531900100811     C                   parm      'UN'          keyAGENCY
532000100811     C                   parm                    keyASSOCIA
532100100811      *output
532200100811     c                   parm                    Errore
532300100811     c                   parm                    DsGenerica
532400100811     c                   parm                    Valori_inErr
532500100811     c                   parm                    Descr_Errore
532600100811     c                   parm                    trovato_seg
532700100811      *
532800100811     c                   endSR
532900110613      * ?------------------------------------------------------------------ */
533000110613      *
533100100811      * ?------------------------------------------------------------------ */
533200110613     c     Instrada_sped begsr
533300110613      *
533400110613      * -------------
533500110613     C* Ricerco CAP in base alle dimensioni
533600110613     C     Wcap          IFne      *BLANKS
533700110613      *
533800110613      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
533900110613     C                   CLEAR                   FNLV55DS
534000110613     C                   MOVEL     'P'           D55TPT
534100110613     C                   MOVEL     WOGGI         D55DRF
534200110613     C                   MOVEL     VABLNP        D55LNP
534300110613     C                   MOVEL     VABLNP        D55LIN
534400110613     C                   CALL      'FNLV55R'
534500110613     C                   PARM                    FNLV55DS
534600110613     C                   MOVE      D55TFP        COMTFP            3 0
534700110613      *
534800110613     C*  PASSO IL TERMINAL PARTENZA
534900110613     C                   CLEAR                   TISI95DS
535000110613     C                   Z-ADD     COMTFP        I95TFP
535100110613     C                   MOVEL     VABTSP        I95TSP
535200110613     C                   MOVEL     'S'           I95FRE
535300110613     C                   MOVEL     '3'           I95TCN
535400110613     C                   MOVEL     WCAP          I95CAP
535500110613     C                   MOVEL     VABNZD        I95NAR
535600110613     C                   MOVEL     VABLOD        I95LOC
535700110613     C                   MOVEL     VABIND        I95IND
535800110613      *
535900110613     C* Se gestita seconda linea di arrivo passo peso - volume
536000110613     C* e numero colli effettivo
536100110613     C     §PULNA        IFEQ      'S'
536200110613     C                   Z-ADD     VABPKB        I95LKG
536300110613     C                   Z-ADD     VABVLB        I95LMC
536400110613     C                   END
536500110613     C*
536600110613     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
536700110613     C                   SELECT
536800110613     C     VABCBO        WHENEQ    '1 '
536900110613     C                   MOVEL     *BLANKS       I95FCA
537000110613     C                   MOVEL     'F'           I95TPO
537100110613     C     VABCBO        WHENEQ    '2 '
537200110613     C                   MOVEL     *BLANKS       I95FCA
537300110613     C                   MOVEL     'A'           I95TPO
537400110613     C     VABCBO        WHENEQ    '4 '
537500110613     C                   MOVEL     'S'           I95FCA
537600110613     C                   MOVEL     'F'           I95TPO
537700110613     C     VABCBO        WHENEQ    '6 '
537800110613     C                   MOVEL     'S'           I95FCA
537900110613     C                   MOVEL     'A'           I95TPO
538000110613     C                   OTHER
538100110613     C                   MOVEL     'F'           I95TPO
538200110613     C     VABCAS        IFGT      0
538300110613     C                   MOVEL     'S'           I95FCA
538400110613     C                   ELSE
538500110613     C                   MOVEL     *BLANKS       I95FCA
538600110613     C                   END
538700110613     C                   ENDSL
538800110613     C*
538900110613     C                   CALL      'TISI95R'
539000110613     C                   PARM                    TISI95DS
539100110613     C*
539200110613     C* Reperisco i dati da CAP
539300110613     C                   MOVEL     O95PRV        VABPRD
539400110613     C                   MOVEL     WCAP          VABCAD
539500110613      ***
539600110613     c                   MOVEL     O95LNA        VABLNA
539700110613     C                   MOVEL     O95ZNC        VABZNC
539800110613     C*
539900110613     C     O95ERR        IFNE      *BLANKS
540000110613     C     O95FIT        ANDEQ     'S'
540100110613     C*  Cap non trovato
540200110613     C********           eval      vinMSG = 'MEA (CAP) non trovato'
540300110613     c********           exsr      Error_DET
540400110613     C********           goto      end_MEA
540500110613     C                   END
540600110613      *
540700110613     C                   ELSE
540800110613      *
540900110613     C                   CLEAR                   VABPRD
541000110613     C                   CLEAR                   VABLNA
541100110613     C                   CLEAR                   VABZNC
541200110613     C                   CLEAR                   VABCAD
541300110613     C*  Cap non trovato
541400110613     C*********          eval      vinMSG = 'MEA (CAP) mancante'
541500110613     c*********          exsr      Error_DET
541600110613     C*********          goto      end_MEA
541700110613     C                   END
541800110613      *
541900110613     c                   endSR
542000110613      * ?------------------------------------------------------------------ */
542100111028     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
542200111028      * ?------------------------------------------------------------------ */
542300111028     C     Write_RESO    BEGSR
542400111028      *
542500111028      *  Imposta nella descrizione del CMR l'intestazione di RESI
542600111028     c                   movel(p)  vabCMR        campo35          35
542700111028     c                   movel(p)  'RESI__'      vabCMR
542800111028     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
542900111028      *
543000111028      *  e sulla 2° parte della ragione sociale viene apposta la dicitura di RESO
543100111028     c                   move(p)   VABrsd        campo44          44
543200111028     c                   movel     '**RESO** '   campo44
543300111028     C                   movel     campo44       VABrsd
543400111028     C                   move      campo44       campo9            9
543500111028      *
543600111028      * Sposta tutta la ragione sociale di (9 carat.) anche sulla 2°ragione sociale
543700111028     c                   move(p)   VABrd2        campo44          44
543800111028     c                   movel     campo9        campo44
543900111028     C                   movel     campo44       VABrd2
544000140610      *
544100140610      *  e sulle NOTE  viene impostato "Sp.EXP:"
544200140610     c                   clear                   campo57          57
544300140610     c                   eval      campo57 = 'ExSped.' + %trim(cod_RESO_ORM)
544400140610     c                   move      VABnot        campo57
544500140610     C                   movel     campo57       VABnot
544600140610     C                   move      campo57       campo22          22
544700140610      *
544800140610      * Sposta tutta la ragione sociale di (x carat.) anche sulla 2°ragione sociale
544900140610     c                   move(p)   VABnt2        campo57
545000140610     c                   movel     campo22       campo57
545100140610     C                   movel     campo57       VABnt2
545200111028      *
545300111028     c                   endSR
545400111028      * ?------------------------------------------------------------------ */
545500111028     C*? Controllo se era prima una Bolla di Export che sta rientrando
545600111028      * ?------------------------------------------------------------------ */
545700111028     C     ctrl_seERAexp Begsr
545800111028     C*
545900111028     C*   imposta la chiave per controllare la spedizione
546000111028     C*    in sede
546100111028     C                   movel     '20'          dsspe
546200111028     C*
546300111028     C                   move      SPEAAS        TASAAS
546400111028     C                   move      SPELNP        TASLNP
546500111028     C                   move      SPENRS        TASNRS
546600111028     C                   move      SPENSP        TASNSP
546700111028     C     KTAS          chain     titas30c
546800111028     c                   if        %found(titas30c)
546900111028     c                               and tasLNA >299 and tasLNA <=400
547000111028     c                   eval      era_un_export_RESO_dal_PARTNER = 'S'
547100111028     c                   eval                           Sped_RESO = 'S'
547200111028     c                   end
547300111028     C*
547400111028     C                   ENDSR
547500140610      * ?------------------------------------------------------------------ */
547600140610     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
547700140610      * ?------------------------------------------------------------------ */
547800140610     C     Write_da_ORM  BEGSR
547900140610      *
548000140610      *  Imposta nella descrizione del CMR l'intestazione di RESI
548100140610     c                   movel(p)  vabCMR        campo35          35
548200140610     c                   movel(p)  'DaORM_'      vabCMR
548300140610     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
548400140623      *
548500140623      *  e sulle NOTE  viene impostato da ORM con il codice dell'ORM generante
548600140623     c                   clear                   campo70          70
548700140623     c                   clear                   note_70          70
548800140623     c                   eval      campo70 = 'DaORM:' + %Trim(salva_REF_ORM)
548900140623     c                   eval      note_70 = %trim(VABNOT) +'/'+ %trim(VABNT2)
549000140623     c                   eval      campo70 = %Trim(campo70)+'/'+ %Trim(note_70)
549100140623     c                   eval      VABNOT = %subst(campo70:1:35)
549200140623     c                   eval      VABNT2 = %subst(campo70:36:35)
549300140610      *
549400140610     c                   endSR
549500111028      * ?------------------------------------------------------------------ */
549600120629     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
549700120629      * ?------------------------------------------------------------------ */
549800120629     C     CL_ParticolarebegSR
549900120629      *
550000120629     C     Rifer_Cliente IFNE      *BLANKS
550100120629      * ------>>>
550200120629     C                   eval      XX = 1
550300120629     C                   movel     §PTKSC        Key_CL           35
550400120629     C                   movel     Rifer_Cliente WCOM28           28
550500120629     C                   move      WCOM28        Key_CL
550600120629      *
550700120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
550800120629      *
550900120629      * ?Se il Partner è uno di quelli che gestisce + linee quindi ha un UNB parziale
551000120629      * ?potrebbe non agganciare correttamente il cliente quindi occorre decodificare
551100120629      * ?il dato cercandolo anche con gli altri codici KSC del Partner Generico ossia
551200120629      * ?con i primi 31 caratteri dell'UNB.
551300120629     c  N34              if        UNB_parziale ='S' and
551400120629     c                              PT_con_piu_Nazioni = 'S'
551500120629     c                   do        PT_KeyXsav    x
551600120629      *
551700120629     c                   if        PT_Parz(x) = UNB_generico
551800120629      * ------>>>
551900120629      * -- Il problema è che potrebbe avere lo stesso codice FF uguale
552000120629      * --   su più records dello stesso partner multinazione
552100120629     C                   eval      XX = 1
552200120629     C                   movel     PT_KSC(X)     Key_CL           35
552300120629     C                   movel     Rifer_Cliente WCOM28           28
552400120629     C                   move      WCOM28        Key_CL
552500120629      *
552600120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
552700120629      * se trovato esce immediatamente
552800120629     c   34              leave
552900120629     c                   end
553000120629      *
553100120629     C                   endDO
553200120629     c                   endIF
553300120629      *
553400120629      *   Se trovato vale x tutto il messaggio
553500120629     C     *IN34         IFEQ      '1'
553600120629     C                   movel     Des_Tb_CL(XX) EDIDSCL
553700120629      *
553800120629      * in testata messaggio
553900120629     c                   if        Dati_di_Testata  ='S'
554000120629     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
554100120629     C                   movel     §CLtar        Tes_RFF_FF_tar    3
554200120629     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
554300120629     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
554400120629     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
554500120629     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
554600120629     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
554700120629     C                   movel     §CLtic        Tes_RFF_FF_tic    2
554800120629     c                   else
554900120629      * in dettaglio Messaggio
555000120629     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
555100120629     C                   movel     §CLtar        Det_RFF_FF_tar    3
555200120629     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
555300120629     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
555400120629     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
555500120629     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
555600120629     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
555700120629     C                   movel     §CLtic        Det_RFF_FF_tic    2
555800120629     c                   end
555900120629      *
556000120629      * imposta la LNP  e la Serie x il dettaglio specifico
556100120629     c
556200120629      *  In base al cod.trattamento merce reperisco tipo tratt.
556300120629      *    per un dettaglio specifico.
556400120629     C                   CLEAR                   DS1B
556500120629     C                   Z-ADD     1             Y                 3 0
556600120629     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
556700120629     C     *IN32         IFEQ      '1'
556800120629     C                   eval       DS1B = Des_Tb_CTM(Y)
556900120629     C                   END
557000120629      *
557100120629      * Se il cliente non era sulla schiera lo aggiunge
557200120629     C     §CLksc        lookup    skCLienti                              39
557300120629     C                   if        *in39 = *off
557400120629     C                   add       1             sk
557500120629     C                   z-add     §CLksc        skCLienti(sk)
557600120629     c                   endif
557700120629      *
557800120629     C                   ENDIF
557900120629     C                   ENDIF
558000120629      *
558100120629      * in Dettaglio  preimposto i campi rilevati dalle tabelle
558200120629      *   per clienti particolari (CL)
558300120629      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
558400120629      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
558500120629      *   successivi per altre decodifiche.
558600120629     c                   if        Dati_di_Testata <>'S'
558700120629      *
558800120629      *   se aveva un RFF+FF in dettaglio
558900120629     C                   if         Det_RFF_FF_ksc > 0
559000120629      *
559100120629     C                   eval      VABccm = Det_RFF_FF_ksc
559200120629     c                   eval      VABlnp = Det_RFF_FF_lnp
559300120629     c                   eval      VABnrs = Det_RFF_FF_nrs
559400120629     C                   eval      VABctm = Det_RFF_FF_ctm
559500120629     C                   movel     Det_RFF_FF_tarVABctr
559600120629      *
559700120629      *   altrimenti se aveva un RFF+FF in Testata
559800120629     c                   elseif     Tes_RFF_FF_ksc > 0
559900120629      *
560000120629     C                   eval      VABccm = Tes_RFF_FF_ksc
560100120629     c                   eval      VABlnp = Tes_RFF_FF_lnp
560200120629     c                   eval      VABnrs = Tes_RFF_FF_nrs
560300120629     C                   eval      VABctm = Tes_RFF_FF_ctm
560400120629     C                   movel     Tes_RFF_FF_tarVABctr
560500120629      *
560600120629     c                   end
560700120629      *>>>>>>>>
560800120629     c                   ENDif
560900120629      *
561000120629     C                   ENDSR
561100121105      *---------------------------------------------------------------*
561200121105      * CTRL caricamento schiere    PT                               -*
561300121105      *---------------------------------------------------------------*
561400121105     C     Check_PT      begsr
561500121105     C*
561600121105     c* Controllo riempimento schiera
561700121105     c                   clear                   trul0sds
561800121105     c                   eval      i0sski='PT_key'
561900121105     c                   eval      i0sele=%elem(PT_key)
562000121105     c                   eval      i0spie=x
562100121105     c                   eval      i0sfile='EDTAB00F'
562200121105     c                   eval      i0ssif=knsif
562300121105     c                   eval      i0spgm='TRTCT96R'
562400121112     c                   move      kpjbu         SvKpjbu
562500121112     c                   clear                   kpjbu
562600121105     c                   movel     trul0sds      kpjbu
562700121105     c                   call      'TRUL0SR'
562800121105     c                   parm                    kpjba
562900121112     c                   eval      kpjbu = SvKpjbu
563000121105     C*
563100121105     C                   ENDSR
563200120629      * ?------------------------------------------------------------------ */
563300090211     O*****************************************************************
563400090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
563500090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
563600090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
563700090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
