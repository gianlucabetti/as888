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
006600160209     D EDIDSPS       E DS
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
008300111028      *----------------------------------------------------*
008400111028      *  DS x scomposizione numero spedizione
008500111028      *----------------------------------------------------*
008600111028     D DSSPE           DS
008700111028     D  SPEAAS                 1      4  0
008800111028     D  SPELNP                 5      7  0
008900111028     D  SPENRS                 8      9  0
009000111028     D  SPENSP                10     16  0
009100111028      **
009200111028     D Sped_RESO       S              1
009300140610      *  qui viene riportato il codice della nostra ex bolla export o del ORM
009400140610      *  mandato dal Partner in un particolare segmento concordato
009500140610     D  Cod_RESO_ORM...
009600140610     d                 s             14
009700140623     D  Salva_REF_ORM...
009800140623     d                 s             14
009900111028      **
010000100701      *----------------------------------------------------*
010100090205      **  Elenco DS di tutti i Segmenti da tradurre
010200100701      *----------------------------------------------------*
010300100811???? D eds96BGM      E DS
010400100811???? D eds96CNI      E DS
010500100811???? D eds96CNT      E DS
010600100811???? D eds96COM      E DS
010700100811???? D eds96CTA      E DS
010800100811???? D eds96DGS      E DS
010900100811???? D eds96DIM      E DS
011000100811???? D eds96DOC      E DS
011100100811???? D eds96DTM      E DS
011200100811???? D eds96EQD      E DS
011300100811???? D eds96EQN      E DS
011400100811???? D eds96FTX      E DS
011500100628???? D  D05                   16    365    DIM(5)
011600100811???? D eds96GID      E DS
011700100811???? D eds96GIN      E DS
011800100920???? D  GIN_10                 4    353    DIM(10)
011900100811???? D eds96GOR      E DS
012000100811???? D eds96HAN      E DS
012100100811???? D eds96LOC      E DS
012200100811???? D eds96MEA      E DS
012300100811???? D eds96MOA      E DS
012400100811???? D eds96NAD      E DS
012500100811???? D eds96PCI      E DS
012600100920???? D  PCI_10                 4    353    DIM(10)
012700100811???? D eds96PIA      E DS
012800100811???? D eds96RFF      E DS
012900100811???? D eds96RNG      E DS
013000100811???? D eds96SEL      E DS
013100100811???? D eds96TCC      E DS
013200100811???? D eds96TDT      E DS
013300100811???? D eds96TMD      E DS
013400100811???? D eds96TMP      E DS
013500100811???? D eds96TOD      E DS
013600100811???? D eds96TSR      E DS
013700100811???? D eds96UNA      E DS
013800100811???? D eds96UNB      E DS
013900100811???? D eds96UNH      E DS
014000100811???? D eds96UNT      E DS
014100100811???? D eds96UNZ      E DS
014200100701      *----------------------------------------------------*
014300090210      **
014400090210     D  X30            S             35    DIM(20)                              generico segnacolli
014500090205      **
014600090209     D Valori_inErr    ds
014700090209     D  SKout_Errori                  1    DIM(50)
014800100630      **
014900090209     D Descr_Errore    ds
015000090209     D  SKout_DesErr                 50    DIM(50)
015100090209      *
015200090205      *----------------------------------------------------*
015300091016     D EoFTivin        s              1
015400091016      *----------------------------------------------------*
015500090205     D Tipo_seg        S              3
015600100720     D Trovato_seg     S              1
015700100716      *
015800100716     d keyUNBCLI       s             35
015900100716     d keyTIPOMSG      s              6
016000100716     d keyVERSION      s              3
016100100716     d keyRELEASE      s              3
016200100716     d keyAGENCY       s              3
016300100716     d keyASSOCIA      s              6
016400100716      *
016500100720     D DsGenerica      s           2048
016600090209     D segmento        s           2048
016700090209     D Errore          s              1
016800100701      *
016900000223     D W0140           S             14  0
017000991129     D WORA            S              6  0
017100100701     D DataGGMMAAA     S              8  0
017200100701     D DATEU           S              8  0
017300100701     D DATA_eur        S               D   DATFMT(*eur)
017400100701      *
017500100630     D sk              S              3  0 INZ
017600100630     D sx              S              3  0 INZ
017700100628     D XX              S              3  0 INZ
017800110328     d Key_Generica35  s             35
017900100630     D NUM_Spediz      s                   LIKE(vabnsp)
018000100708     D UNT_record      s                   LIKE(vnREC)
018100100708     D IniDET_RECord   s                   LIKE(vnREC)
018200100708     D FinDET_RECord   s                   LIKE(vnREC)
018300090213     D Last_RECord     s                   LIKE(vnREC)
018400101222     D UNZ_SEGMENTO    s              1
018500100318      *------------------------------------------
018600100318      **  Definizione Qualificatori
018700100318      *------------------------------------------
018800100319      *
018900100628???? D Documento_Manifest...
019000100628???? D                 s              3    INZ('785')
019100100701???? D Totale_Peso_Lordo...
019200100701???? D                 s              3    INZ('7  ')
019300100701???? D Totale_Nr_Spedizioni...
019400100701???? D                 s              3    INZ('10 ')
019500100701???? D Totale_Nr_Colli...
019600100701???? D                 s              3    INZ('11 ')
019700100701???? D Totale_Peso_Netto...
019800100701???? D                 s              3    INZ('ZCW')
019900100628???? D Riferimento_Spedizione...
020000100628???? D                 s              3    INZ('AGE')
020100100708???? D Riferimento_Numerico...
020200100708???? D                 s              3    INZ('CU ')
020300100318???? D Riferimento_Cliente_particolare...
020400100318???? D                 s              3    INZ('FF ')
020500100701???? D X_Cargo_Manifest...
020600100318???? D                 s              3    INZ('AFB')
020700100701???? D X_Numero_CMR...
020800100318???? D                 s              3    INZ('CMR')
020900100318???? D Telephone_Number...
021000100318???? D                 s              3    INZ('TE ')
021100100318???? D TeleFax_Number...
021200100318???? D                 s              3    INZ('TX ')
021300100628???? D Porto_Assegnato...
021400100628???? D                 s              3    INZ('001')
021500100628???? D Porto_Franco...
021600100628???? D                 s              3    INZ('002')
021700100628???? D Porto_Franco_uncleared...
021800100628???? D                 s              3    INZ('003')
021900100628???? D Porto_Franco_domicile...
022000100628???? D                 s              3    INZ('004')
022100100318???? D Volume...
022200100318???? D                 s              3    INZ('VOL')
022300100705???? D Weight...
022400100705???? D                 s              3    INZ('WT ')
022500100319???? D G_Weight...
022600100319???? D                 s              3    INZ('G  ')
022700100318???? D Gross_Weight...
022800100318???? D                 s              3    INZ('AAG')
022900100706???? D Gross_Weight_E...
023000100706???? D                 s              3    INZ('AAE')
023100100706???? D N_Weight...
023200100706???? D                 s              3    INZ('N  ')
023300100706???? D Net_Weight...
023400100706???? D                 s              3    INZ('AAF')
023500100318???? D Kilograms...
023600100318???? D                 s              3    INZ('KGM')
023700100319???? D Grams...
023800100319???? D                 s              3    INZ('163')
023900100318???? D Meters...
024000100318???? D                 s              3    INZ('MTR')
024100100318???? D Cubic_Meters...
024200100318???? D                 s              3    INZ('MTQ')
024300100318???? D Shipper_assigned...
024400100318???? D                 s              3    INZ('24 ')
024500100705???? D Data_senza_ora...
024600100628???? D                 s              3    INZ('102')
024700100705???? D Data_con_ora...
024800100628???? D                 s              3    INZ('203')
024900100705???? D Data_con_i_secondi...
025000100705???? D                 s              3    INZ('204')
025100100630???? D Squadratura_Colli...
025200100630???? D                 s              1    INZ('N')
025300120613     D controlla_COD_FTX...
025400120613???? D                 s              3
025500100316      *---------------------------------------------------------------------*
025600100316      **  segmenti Identificativi parti specifiche del messaggio
025700100316      *---------------------------------------------------------------------*
025800100701???? D Segm_Inizio_Messaggio...
025900100701???? D                 s              3    INZ('UNB')
026000100701???? D Segm_Testata_Dettagli...
026100100701???? D                 s              3    INZ('UNH')
026200100701???? D Segm_Inizio_Dettaglio...
026300100316???? D                 s              3    INZ('CNI')
026400100701???? D Segm_Fine_Dettagli...
026500100316???? D                 s              3    INZ('UNT')
026600100701???? D Segm_Fine_messaggio...
026700100316???? D                 s              3    INZ('UNZ')
026800100701???? D seg_imprevisto...
026900100701???? D                 s              1
027000100726???? D Forza_Uscita...
027100100726???? D                 s              1
027200100701      *---------------------------------------------------------------------*
027300100701???? D Dati_di_Testata...
027400100701???? D                 s              1    INZ('S')
027500100701???? D Contatore_Dettagli...
027600100701???? D                 s              5s 0 INZ(0)
027700100728???? D Segmento_in_errore...
027800100728???? D                 s              5s 0 INZ(0)
027900100701      *---------------------------------------------------------------------*
028000100910      *
028100100910      * Tabella partner
028200121105     D PT_key          S             35    DIM(200)
028300121105     D PT_Parz         S             35    DIM(%elem(PT_key))
028400121105     D PT_KSC          S              7    DIM(%elem(PT_key))
028500121105     D PT_uni          S             90    DIM(%elem(PT_key))
028600121105     D PU_uni          S             90    DIM(%elem(PT_key))
028700121105     D PV_uni          S             90    DIM(%elem(PT_key))
028800121105     D PZ_uni          S             90    DIM(%elem(PT_key))
028900160209     D PS_uni          S             90    DIM(%elem(PT_key))
029000121105     D TRUL0SDS      E DS
029100100910      *
029200100910     d PT_KeyXsav      s              3  0 inz(0)
029300100910     D PT_trovata      s              1    inz('N')
029400100910     D PU_key          S             35
029500100910     D PV_key          S             35
029600160209     D PS_key          S             35
029700120629     D PZ_key          S             35
029800100910     D PU_HLD_SpedVAX  S              1
029900111028     D  PU_in_RMN_BOLLA_EXPORT_x_RESO...
030000111028     d                 s              1
030100111028     D  era_un_export_RESO_dal_PARTNER...
030200111028     d                 s              1
030300100910     D PT_con_piu_Nazioni...
030400100910     d                 s              1
030500120629     D PZ_abilita_NAD_SF_come_RFF_FF...
030600120629     d                 s              1
030700100910      *
030800160209     D  PS_vabFGS      s              3A
030900100910     D  PT_vabLNP      s                   LIKE(vablnp)
031000100910     D  PT_vabNRS      s                   LIKE(vabnrs)
031100100910     D  PT_vabCTM      s                   LIKE(vabctm)
031200100910     D  PT_vabCCM      s                   LIKE(vabccm)
031300100910     D  PU_lunVAT      s              2s 0
031400100910     D  PU_TipoServ    s              2a
031500100910     D  PU_SpecServ    s              2a
031600100910      *
031700100910     D Nr_PT           S              3  0 INZ
031800100910     D savXX_PT_key    S              3  0 INZ
031900100628      *
032000100701      *---------------------------------------------------------------------*
032100100630     d UNB_diWork      s                   like(UNB0004)
032200100628     d  UNB_Mittente   s                   like(UNB0004)
032300100706     d BGM_Id_Testata  s             35A   inz(' ')
032400100706     d  BGM_NrFviaggio...
032500100628     d                 s             35A
032600100706     d  BGM_nrCMR      s             35A
032700100701     d  RFF_NrFviaggio...
032800100701     d                 s             35A
032900100701     d  RFF_NrCMR      s             35A
033000100701     d  RFF_RifSpediz  s             35A
033100100910     d  RFF_DalPrimoCollo...
033200100910     d                 s              1A
033300100910     d  RFF_DopoIlPrimoCollo...
033400100910     d                 s              1A
033500100701     d UNB_parziale    s              1    inz('N')
033600100701     d UNB_Parz        s             35    inz(' ')
033700100812     d UNB_Generico    s             31    inz(' ')
033800100701     d UNB_NrTrasmiss  s             14    inz(' ')
033900100701     d UNZ_NrTrasmiss  s             14    inz(' ')
034000100701     d UNH_Rifer_Msg   s             14    inz(' ')
034100100701     d UNH_VABrma      s                   like(VABrma)
034200100701     d UNH_VABrmn      s                   like(VABrmn)
034300120629     d NAD_SF_Cliente  s             35    inz(' ')
034400100910      *
034500100910???? D NAD_destinatario_in_testata...
034600100910???? D                 s              1
034700100910???? D NAD_destinatario_in_dettaglio...
034800100910???? D                 s              1
034900100910???? D NAD_mittente_in_testata...
035000100910???? D                 s              1
035100100910???? D NAD_mittente_in_Dettaglio...
035200100910???? D                 s              1
035300100910     d CTA_destinatario_in_Dettaglio...
035400100910     d                 s             35A
035500100910     d CTA_destinatario_in_testata...
035600100910     d                 s             35A
035700100910     d COM_telefono_in_Dettaglio...
035800100910     d                 s             35A
035900100910     d COM_telefono_in_testata...
036000100910     d                 s             35A
036100110907     d FTX_ServizioMail_xDestinatario...
036200110907     d                 s             70A
036300131025     d FTX_MAIL_come_Servizio...
036400131025     d                 s              1A
036500131025     d FTX_ServizioSMS_xDestinatario...
036600131025     d                 s             70A
036700131025     d FTX_SMS_come_Servizio...
036800131025     d                 s              1A
036900131025     d FTX_ai_piani    s              1A
037000100701      *
037100100630     d  DTM_DtParten   s              8s 0
037200100630     d  DTM_DtFViag    s              8s 0
037300100630     d  DTM_DtCMR      s              8s 0
037400100811     d  DTM_DtConsRic  s              8s 0
037500100811     d  DTM_OraCnsRic  s              4s 0
037600100630     d  DTM_OraFViag   s              4s 0
037700100630     d  DTM_OraCMR     s              4s 0
037800100702     d  DTM_DtArrivo   s              8s 0
037900100702     d  DTM_OraArrivo  s              4s 0
038000100706     d  DTM_OraParten  s              4s 0
038100100702     d  CNI_Identificativo_Bolla...
038200100702     d                 s             35A
038300100813     d  CNI_seNumerico...
038400100813     d                 s                   LIKE(vabRMN)
038500100701     d  GID_aTotale    s              1A
038600100705     d  GID_ColliInAdd...
038700100705     d                 s                   like(gid722420)
038800100910      *---------------                                                      *
038900090209      *  DS x scomposizione segnacollo
039000100630     D DS_SegnBART     DS
039100100630     D  SegnBART_LNP           1      3  0
039200100630     D  SegnBART_LNA           4      6  0
039300100630     D  SegnBART_NRS           7      8  0
039400100630     D  SegnBART_NSC           9     15  0
039500100630     D  SegnBART_ZNC          16     17  0
039600100701      **
039700090209      *---------------                                                      *
039800100701     D VABtic_work     s                   LIKE(vabtic)
039900110623     d inviato_tipo_incasso...
040000110623     d                 s              1
040100100630      *
040200090603     d tes_VABrmo      s                   LIKE(VABrmo)
040300090603     d tes_VABnmo      s                   LIKE(VABnmo)
040400090603     d tes_VABcmo      s                   LIKE(VABcmo)
040500090209     C*
040600090603     d tes_VABrsd      s                   LIKE(VABrsd)
040700090603     d tes_VABrd2      s                   LIKE(VABrd2)
040800090603     d tes_VABind      s                   LIKE(VABind)
040900090603     d tes_VABlod      s                   LIKE(VABlod)
041000090603     d tes_VABnzd      s                   LIKE(VABnzd)
041100090603     C*
041200100813     d tes_VABtsp      s                   LIKE(VABtsp)
041300100813     d tes_VABtc1      s                   LIKE(VABtc1)
041400120403     d tes_VABtc2      s                   LIKE(VABtc2)
041500100813     d tes_VABnot      s                   LIKE(VABnot)
041600120214     d tes_VABffd      s                   LIKE(VABffd)
041700100813     C*
041800090603     d tes_Valuta      s                   LIKE(VABvca)
041900090603     C*
042000100910     D SUvabRMO        s                   LIKE(vabRMO)
042100090209      *---------------------------------------------------------------------*
042200090209      * Schiere
042300090209      *---------------------------------------------------------------------*
042400090209      * Schiera per reperimento nr.seganacollo
042500100630     D segn_BAR        S              7  0 DIM(5000)
042600100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
042700100630      *
042800100630      *
042900090209      * Nr. documento alfanumerico da decodificare
043000090209     D NDA             S              1    DIM(35)
043100100630      *
043200090209      * Nr. documento alfanumerico da già decodificato
043300090209     D NDN             S              1    DIM(15)
043400100630      *
043500090209      * Messaggi di errore
043600090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
043700100630      *
043800090209      * Tabella codici trattamento merce (1B)
043900100701     D Cod_Tb_CTM      S              2    DIM(200)
044000100701     D Des_Tb_CTM      S             67    DIM(200)
044100100630      *
044200090209      * Tabella codici valuta
044300090209     D CCV             S              3    DIM(200)
044400090209     D DCV             S             89    DIM(200)
044500100630      *
044600090209      * Tabella codici nazioni
044700090209     D C15             S              3    DIM(500)
044800090209     D ISO             S              3    DIM(500)
044900090209     D CPF             S              2  0 DIM(500)
045000100630      *
045100090209      * Tabella CL --> codici clienti - tariffa
045200130305     D Cod_Tb_CL       S             35    DIM(999)
045300130305     D Des_Tb_CL       S             90    DIM(999)
045400100630      *
045500090209      * Schiere x caricamento cod.cliente  <>
045600130305     D skCLienti       S              7  0 DIM(999)
045700100630      *
045800100318      * Priorità trasporto
045900100705     D Key_TipServ     S             35    DIM(100)
046000100702     D TipoServizio    S              1    DIM(100)
046100100702      *
046200100702      * Decodifiche servizi speciali
046300100702     D SpecialServ     S             35    DIM(100)
046400100702     D Key_ServSpec    S              3    DIM(100)
046500100702     D UNI_ServSpec    S             90    DIM(100)
046600100630      *
046700090209      * Termini di consegna
046800110328     D K35_TpBolla     S             35    DIM(100)
046900110328     D Cod_TpBolla     S              3    DIM(100)
047000110328     D Decod_Porto     S              1    DIM(100)
047100110328     d Trovata_Tab_TB  S              1
047200100630      *
047300090216      * Tipo pagamento C/Assegno
047400100705     D K35_TpIncas     S             35    DIM(100)
047500100705     D TipoIncasso     S              2    DIM(100)
047600100630      *
047700090209      * Schiere x routine di conversione CAP
047800090209     D CAP5            S              1    DIM(5)
047900090209     D CAP9            S              1    DIM(9)
048000090209     D CAPM            S              1    DIM(9)
048100100630      *
048200090209      * Schiera per memorizzazione FTX ricevuti
048300090209     D QUA             S              3    DIM(100)
048400090209     D FTX             S             70    DIM(100)
048500100630      *
048600090209      * Tabelle capiconto
048700090209      * Schiere x località
048800090209     D LOD             S              1    DIM(35)
048900100630      *
049000060608      **?------------------------------------------------------------------ */
049100090601     C*? Ds Scrittura del VAT "E"
049200060608      **?------------------------------------------------------------------ */
049300060612     D XTOEBCDDS     E DS
049400060620      *
049500060608      **?------------------------------------------------------------------ */
049600100701      *
049700090209      **?------------------------------------------------------------------ */
049800090209     C* Definizione campi di work
049900090209     D kKUT            s                   LIKE(TBLKUT)
050000090209     D kCOD            s                   LIKE(TBLCOD)
050100090209     D kKEY            s                   LIKE(TBLKEY)
050200090209     D kKCC            s                   LIKE(ACOKCC)
050300090209     D kKSC            s                   LIKE(ACOKSC)
050400090209     D kAAA            s                   LIKE(NUFAAA)
050500090209     D kCNU            s                   LIKE(NUFCNU)
050600090209     D kFIL            s                   LIKE(NUFFIL)
050700090209     D kAAS            s                   LIKE(VABAAS)
050800090209     D kLNP            s                   LIKE(VABLNP)
050900090209     D kNRS            s                   LIKE(VABNRS)
051000090209     D kNSP            s                   LIKE(VABNSP)
051100090209     D kCMR            s                   LIKE(VABCMR)
051200090209     D kCCM            s                   LIKE(VABCCM)
051300090209     D kNCD            s                   LIKE(VADNCD)
051400090210     D kCNT            s                   LIKE(VADCNT)
051500090209     D kNMC            s                   LIKE(RDENMC)
051600090209     D kNRP            s                   LIKE(RDENRP)
051700090209     D kLNP1           s                   LIKE(RDELNP)
051800090209      *
051900090209      *  Invio E-mail
052000090209     D Indirizzi       s            253
052100090209     D Oggetto         s             44
052200090209     D Messaggio       s           5000
052300090209      *
052400090209     d   DSmail        ds
052500090209     D Mail_Ind                      44
052600090209     d   IND_char                     1    dim(44)
052700090209      *
052800100630     D Lunghezza_Indirizzo...
052900100630     D                 s              5u 0
053000090209      *
053100100630     D dalCarattere    s              5u 0
053200100630     D xTOTcaratteri   s              5u 0
053300090209      *
053400090209     C*---------------------------------------------------------------*
053500090209     D MsgDSP          S            256                                         Testo generico
053600090209     C*---------------------------------------------------------------*
053700090209     D SndMg01         S             10                                         Message type
053800090209     D                                     INZ('*INFO')
053900090209     D SndMg02         S             10                                         Deluvery mode
054000090209     D                                     INZ('*BREAK')
054100090209     D SndMg03         S            256                                         Message text
054200090209     D SndMg04         S             10I 0                                      Length of text
054300090209     D                                     INZ(%SIZE(SndMg03))
054400090209     D SndMg05         S             10                                         User profile
054500090209     D                                     DIM(299)
054600090209     D SndMg06         S             10I 0                                      Number of user
054700090209     D SndMg07         S             10I 0                                      Message sent indic.
054800090209     D SndMg08         S             10I 0                                      Function requested
054900090209     D SndMg10         S              1                                         Show display
055000090209     D                                     INZ('N')
055100090209     D SndMg11         S             20                                         Qualified MSGQ name
055200090209     D SndMg12         S              4                                         Name type indicator
055300090209     D                                     INZ('*USR')
055400090209      * indice di scihera
055500090209     D Jx              S              5I 0
055600090209      *
055700090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
055800090209      *
055900090209     d bart_it         C                   CONST('@Bartolini.it;')
056000090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
056100060612      * ?================================================================== */
056200060612      * ?   * Campi x decodifica * (INPUT  del Record)
056300100319      * ?================================================================== */
056400090130     D  Dati           s           2048
056500060612      * ?   *--------------------------------------------------------------*
056600060612     D  se_errore      s              1    inz(' ')
056700060612      * ?* ------------------------------------------------------ *
056800060710     D Digits          C                   '0123456789'
056900091019     D*------------------
057000091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
057100091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
057200060612      * ?================================================================== */
057300060612      *   Ciclo principale
057400090205      * ?================================================================== */
057500060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
057600060612     C*
057700060612     C                   if        not %open(tivin00r)
057800060612     C                   open      tivin00r
057900060612     C                   endif
058000060612      **
058100060612      * ?------------------------------------------------------------------ */
058200100708     C*  Controllo dati arrivati da DPD
058300060612      * ?------------------------------------------------------------------ */
058400060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
058500060612      * ?- controllando che la trasmissione sia completa.
058600090205      **
058700060612      * ?              /*---------------------- */
058800090205     c                   goto      nonFare_adesso
058900060612     c                   exsr      check_Trasm
059000090205     c     nonFare_adessotag
059100060612      * ?              /*---------------------- */
059200090205      **
059300060613      **  Errore di trasmissione x tutti i records
059400060613      **   --> file in errore
059500060613     c                   if        se_errore = 'S'
059600060612
059700060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
059800090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
059900090205     c                   exsr      Msg_errato
060000060612      **
060100060612     c                   else
060200060614      **
060300060612      * ?------------------------------------------------------------------ */
060400100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
060500060612      * ?------------------------------------------------------------------ */
060600060612      * ?              /*---------------------- */
060700060612     c                   exsr      Importa_Msg
060800060612      * ?              /*---------------------- */
060900060612
061000060614     c                   end
061100060612      *
061200060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
061300090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
061400090225     c                             esito ='0'
061500060710     C                   eval      esito ='0'
061600090225      *
061700090225     c                   COMMIT
061800110208     C                   endif
061900090224      *
062000090213     c                   if        almeno_un_due ='S'
062100090213     C                   eval      esito ='1'
062200060614     C                   endif
062300100708      * chiude TIVIN
062400100708     C                   if        %open(tivin00r)
062500100708     C                   close     tivin00r
062600100708     C                   endif
062700060614      *
062800060612     c                   SETON                                        LR
062900060612      * ?================================================================== */
063000100708      *? Importa i records della trasmissione
063100060612      * ?------------------------------------------------------------------ */
063200060612     c     Importa_Msg   Begsr
063300060614
063400130506      *   Inizia il conteggio totale delle righe
063500130506     c                   z-add     0             total_segmenti    5 0
063600100726     c                   clear                   Forza_Uscita
063700060612     c     *start        setll     Tivin00r
063800100708
063900100708     c                   EXSR      LEGGE_TIVIN_R
064000100708     c                   dow       EoFTivin <> 'S'
064100060614
064200060614      * solo i record sflaggati da rielaborare
064300110107     c                   IF        vinFLG = *blank
064400110107      *** ++++++
064500090211      *  Sempre Record OK
064600090211      ** Controlli formali sui campi
064700090211     C                   eval      vinFLG = '1'
064800060612     c                   clear                   se_errore
064900110107      *
065000110107      *** >>>>>>
065100110107     c                   IF        vinDTA <> *blank
065200110107      *** >>>>>>
065300060612
065400060612      ** Decodifica record a campi variabili
065500060612      * ?              /*---------------------- */
065600090205     c                   exsr      Decod_Segmento
065700090209      * ?              /*---------------------- */
065800060612
065900060621      *  x errore bloccante nella composizione del VAB/VAT
066000060621     c                   if        err_bloccante ='S'
066100060621     C                   eval      vinFLG = '2'
066200110208     c                   eval      Almeno_Un_Due ='S'
066300090211     c                   eval      esito  = '2'
066400100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
066500100921     C                   EVAL      Messaggio = 'EDI -
066600100921     C                             IFCSUM D96A - msg su UPLOAD -
066700100921     C                             con UNB non presente in tabella PT..'
066800100319     c                   if        §PVinv <> 'N'
066900100319     c                   exsr      invio_mail
067000060621     c                   end
067100100319     c                   end
067200060612
067300110107      *** >>>>>>
067400110107     c                   endIF
067500110107      *** >>>>>>
067600110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
067700110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
067800110107      *         un erroneo messaggio
067900110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
068000060612     c                   update    Tivin000
068100090211
068200090211      *** se non è riconosciuto l'UNB deve uscire direttamente
068300090211     c                   if        se_errore ='S'  and
068400100708     c                             tipo_seg = Segm_Inizio_Messaggio
068500090213     c                   eval      err_bloccante = 'S'
068600090211     c                   exsr      Msg_errato
068700090211     c                   LEAVE
068800090211     c                   endIF
068900090211      ***
069000060614     c                   endIF
069100060612
069200100708     c                   EXSR      LEGGE_TIVIN_R
069300060612     C                   ENDdo
069400060612      **
069500060612     c                   endSR
069600100709      * ?------------------------------------------------------------------ */
069700100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
069800100709      * ?------------------------------------------------------------------ */
069900100709     C     MSG_Errato    BEGSR
070000100709      *
070100100709      * ?  Scrive su tutti i records il tipo di errore
070200100709     c     *start        setll     tivin00r
070300100709     c
070400100709     c                   EXSR      LEGGE_TIVIN_R
070500100709     c                   dow       EoFTivin <> 'S'
070600100709     c
070700100709     c                   if        vinMSG  = *blank
070800100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
070900100709     C                               >> Controllare i DATI su UPLOAD !!'
071000100709     c                   end
071100100709     C                   eval      vinFLG = '2'
071200100709     c                   eval      Almeno_Un_Due ='S'
071300100709     c                   eval      esito  = '2'
071400100709     c                   eval      se_errore ='S'
071500100709     c                   eval      err_bloccante ='S'
071600100709      *
071700100709     c                   update    tivin000
071800100709     c                   EXSR      LEGGE_TIVIN_R
071900100709     c                   endDO
072000100709      *
072100100709     C                   ENDSR
072200100709      * ?------------------------------------------------------------------ */
072300100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
072400100709      * ?------------------------------------------------------------------ */
072500100709     C     DETta_Errato  BEGSR
072600100709      *
072700100709     c                   EXSR      LEGGE_TIVIN_R
072800100709     c                   dow       EoFTivin <> 'S'
072900100709      *
073000100709      * Se ha letto il successivo record
073100100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
073200100709     c                   if        VNREC = UNT_record  or
073300100709     c                             VNREC = finDET_RECord
073400100709     c                   leave
073500100709     c                   end
073600100709      *
073700100709     c                   exsr      Error_DET
073800100709      *
073900100709     c                   if        vinMSG  = *blank
074000100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
074100100709     C                             >> Controllare i DATI di ogni Segmento <<'
074200100709     c                   end
074300100709      *
074400100709     c                   update    tivin000
074500100709     c                   EXSR      LEGGE_TIVIN_R
074600100709     c                   endDO
074700100709      * ------
074800100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
074900100921     C                   EVAL      Messaggio = 'EDI -
075000100811???? C                             IFCSUM D96A  non totalmente tradotto. -
075100100709     C                             Controllare UPLOAD del cliente:'
075200100709     C                   EVAL      Messaggio = %trim(Messaggio) +
075300100709     C                                         %editc(§PTksc:'X')
075400100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
075500100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
075600100709     c                   if        §PVinv <> 'N'
075700100709     c                   exsr      invio_mail
075800100709     c                   end
075900100709      *
076000100709      * ripulisce per un nuovo giro
076100100709     C                   clear                   Err_in_detta
076200100709      *
076300100709     C                   ENDSR
076400100709      * ?------------------------------------------------------------------ */
076500100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
076600100709      * ?------------------------------------------------------------------ */
076700100709     C     LEGGE_TIVIN_R BEGSR
076800100709      *
076900100709     c                   clear                   EoFTivin
077000100709      *  Lettura sequenziale
077100100709     c                   read      Tivin00r
077200100709
077300100709     c                   if        %Eof(tivin00r)
077400100709     c                   move      'S'           EoFTivin
077500100709     c                   else
077600100709      *
077700100709      * ?  imposta il tipo di segmento:
077800100709     c                   clear                   dati
077900100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
078000100728     c                   eval      segmento = %trimL(dati)
078100100728     c                   eval      tipo_seg = %subst(segmento:1:3)
078200100709      **
078300100709     c                   end
078400100709      *
078500100709     C                   ENDSR
078600100120      * ?------------------------------------------------------------------ */
078700100120      *?    Decodifica il segmento ed importa i campi in formato DS
078800100120      * ?------------------------------------------------------------------ */
078900100120     c     Decod_SegmentoBegsr
079000100120      **
079100100706      *** ------------------------------------
079200100706????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT"
079300100706      *** ------------------------------------
079400100706      * OGNI DETTAGLIO inizia dal segmento CNI e per ultimo il msg è chiuso da UNT.
079500100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
079600100706      **
079700100706      **  Scrive alla fine di ogni giro di dettaglio:
079800100706     c                   if           tipo_seg = Segm_Fine_Dettagli
079900100706     c                                   or
080000100706     c                                tipo_seg = Segm_Inizio_Dettaglio  and
080100100706     c                                Contatore_Dettagli > 0
080200100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
080300100708     c                   eval      FinDET_RECord = vnrec
080400100708      **?              /*---------------------- */
080500100708     c                   exsr      Scrive_Dettag
080600100708      **?              /*---------------------- */
080700100812      *
080800100812      *   Ogni nuovo Dettaglio,
080900100812      *    per quei Partner che gestiscono più LInee
081000100812      *    Mediante il Mittente del dettaglio
081100100812     c                   if        UNB_parziale ='S' and
081200100812     c                              PT_con_piu_Nazioni = 'S' and
081300100812     C                                 Naz_salvata = Naz_mittente
081400100812      * ?ReImposta i dati da tabella PT
081500100812      * ?  se fossero stati cambiati dal precedente dettaglio:
081600100812     C                   z-add     savXX_PT_key  Nr_PT
081700100812     c                   exsr      Se_Trovata_PT
081800100812     c                   endIF
081900100812      *
082000100812     c                   ENDif
082100100708      *
082200100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
082300100708      **?              /*---------------------- */
082400100708     c                   exsr      Pulizia_Campi
082500100708      **?              /*---------------------- */
082600100812      *
082700100706      *** ------------------------------------
082800100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
082900100120      * ?  Anche se non utilizzati.
083000100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
083100100317     c                   clear                   seg_imprevisto
083200100720     c                   clear                   trovato_seg
083300100319      *
083400100811      **?          programma che decodifica il segmento
083500100811      **?              /*---------------------- */
083600100811     c                   exsr      chiama_Decoder
083700100811      **?              /*---------------------- */
083800100811      *
083900100319      *  Contatore delle righe ossia dei segmenti del messaggio
084000100319     c                   add       1             Conta_segmenti
084100130506     c                   add       1             total_segmenti
084200100708      *
084300120424      *>>>--->>>                                                    <<<---<<<
084400120424      *   Se ci sono dei Segmenti Particolari da NON considerare
084500120424     c                   IF        trovato_seg ='N' and unz_segmento <>'S'
084600120424      *
084700120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
084800120424     C                   movel(p)  'SGM'         etbSGM
084900120424     C                   movel(p)  Tipo_Seg      etbVALSGM
085000120424     c                   exsr      In_TAB_EDSTBL
085100120424      *
085200120424     C*  Quindi sono dichiarati nel file delle eccezioni
085300120424     C                   If        Quale_campo ='TIPSGM' and
085400120424     c                                                   trovata_EDSTBL = 'S'
085500120424     c                   eval      trovato_seg ='S'
085600120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
085700120424      * errori sul TIVIN.
085800120424     c                   endIf
085900120424     c                   end
086000120424      *>>>--->>>                                                    <<<---<<<
086100100708      *** ------------------------------------
086200100708      *   Decodifica il singolo segmento LETTO
086300100708      *** ------------------------------------
086400100120     c                   select
086500100120      *
086600110110     c                   when      trovato_seg ='N' and unz_segmento <> 'S'
086700100720     c                   move      'S'           seg_imprevisto
086800100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
086900100720     c                   exsr      Error_DET
087000100720     c                   goto      end_Decod
087100100720      **--------
087200100120     c                   when      tipo_seg = 'UNA'
087300100811     c                   movel(p)  DsGenerica    eds96UNA
087400100120      *
087500100120     c                   when      tipo_seg = 'UNB'
087600100811     c                   movel(p)  DsGenerica    eds96UNB
087700100120     C                   EXSR      DATI_UNB
087800100120      * --------
087900100120     c                   when      tipo_seg = 'UNH'
088000100811     c                   movel(p)  DsGenerica    eds96UNH
088100100120     C                   EXSR      DATI_UNH
088200100120      * --------
088300100120     c                   when      tipo_seg = 'BGM'
088400100811     c                   movel(p)  DsGenerica    eds96BGM
088500100316     C                   EXSR      DATI_BGM
088600100811      **--------
088700100811     c                   when      tipo_seg = 'CNT'
088800100811     c                   movel(p)  DsGenerica    eds96CNT
088900100811     C                   EXSR      DATI_CNT
089000100811      **--------
089100100811     c                   when      tipo_seg = 'TDT'
089200100811     c                   movel(p)  DsGenerica    eds96TDT
089300100811     C                   EXSR      DATI_TDT
089400100811      * --------
089500100811     c                   when      tipo_seg = 'TSR'
089600100811     c                   movel(p)  DsGenerica    eds96TSR
089700100811     C                   EXSR      DATI_TSR
089800100811      **--------
089900100811     c                   when      tipo_seg = 'NAD'
090000100811     c                   movel(p)  DsGenerica    eds96NAD
090100100811     C                   EXSR      DATI_NAD
090200100811      **--------
090300100811     c                   when      tipo_seg = 'EQD'
090400100811     c                   movel(p)  DsGenerica    eds96EQD
090500100811     C                   EXSR      DATI_EQD
090600100811      **--------
090700100811     c                   when      tipo_seg = 'EQN'
090800100811     c                   movel(p)  DsGenerica    eds96EQN
090900100811     C                   EXSR      DATI_EQN
091000100811      **--------
091100100811     c                   when      tipo_seg = 'CNI'
091200100811     c                   movel(p)  DsGenerica    eds96CNI
091300100811     C                   EXSR      DATI_CNI
091400100811      **--------
091500100811     c                   when      tipo_seg = 'DTM'
091600100811     c                   movel(p)  DsGenerica    eds96DTM
091700100811     C                   EXSR      DATI_DTM
091800100811      **--------
091900100811     c                   when      tipo_seg = 'FTX'
092000100811     c                   movel(p)  DsGenerica    eds96FTX
092100100811     C                   EXSR      DATI_FTX
092200100811      **--------
092300100811     c                   when      tipo_seg = 'TOD'
092400100811     c                   movel(p)  DsGenerica    eds96TOD
092500100811     C                   EXSR      DATI_TOD
092600100811      **--------
092700100811     c                   when      tipo_seg = 'GID'
092800100811     c                   movel(p)  DsGenerica    eds96GID
092900100811     C                   EXSR      DATI_GID
093000100811      **--------
093100100811     c                   when      tipo_seg = 'MEA'
093200100811     c                   movel(p)  DsGenerica    eds96MEA
093300100811     C                   EXSR      DATI_MEA
093400100811      **--------
093500100811     c                   when      tipo_seg = 'RFF'
093600100811     c                   movel(p)  DsGenerica    eds96RFF
093700100811     C                   EXSR      DATI_RFF
093800100811      **--------
093900100811     c                   when      tipo_seg = 'PCI'
094000100811     c                   movel(p)  DsGenerica    eds96PCI
094100100811     C                   EXSR      DATI_PCI
094200100811      **--------
094300100811     c                   when      tipo_seg = 'GIN'
094400100811     c                   movel(p)  DsGenerica    eds96GIN
094500100811     C                   EXSR      DATI_GIN
094600100316      **--------
094700100316     c                   when      tipo_seg = 'SEL'
094800100811     c                   movel(p)  DsGenerica    eds96SEL
094900100316     C                   EXSR      DATI_SEL
095000100316      **--------
095100100316     c                   when      tipo_seg = 'PIA'
095200100811     c                   movel(p)  DsGenerica    eds96PIA
095300100316     C                   EXSR      DATI_PIA
095400100316      **--------
095500100316     c                   when      tipo_seg = 'LOC'
095600100811     c                   movel(p)  DsGenerica    eds96LOC
095700100316     C                   EXSR      DATI_LOC
095800100316      **--------
095900100316     c                   when      tipo_seg = 'DGS'
096000100811     c                   movel(p)  DsGenerica    eds96DGS
096100100316     C                   EXSR      DATI_DGS
096200100120      **--------
096300100120     c                   when      tipo_seg = 'MOA'
096400100811     c                   movel(p)  DsGenerica    eds96MOA
096500100120     C                   EXSR      DATI_MOA
096600100120      **--------
096700100120     c                   when      tipo_seg = 'CTA'
096800100811     c                   movel(p)  DsGenerica    eds96CTA
096900100120     C                   EXSR      DATI_CTA
097000100120      **--------
097100100120     c                   when      tipo_seg = 'COM'
097200100811     c                   movel(p)  DsGenerica    eds96COM
097300100120     C                   EXSR      DATI_COM
097400100811      **--------
097500100811     c                   when      tipo_seg = 'HAN'
097600100811     c                   movel(p)  DsGenerica    eds96HAN
097700100811     C                   EXSR      DATI_HAN
097800100811      **--------
097900100811     c                   when      tipo_seg = 'GOR'
098000100811     c                   movel(p)  DsGenerica    eds96GOR
098100100811     C                   EXSR      DATI_GOR
098200100811      **--------
098300100811     c                   when      tipo_seg = 'TMP'
098400100811     c                   movel(p)  DsGenerica    eds96TMP
098500100811     C                   EXSR      DATI_TMP
098600100811      **--------
098700100811     c                   when      tipo_seg = 'TMD'
098800100811     c                   movel(p)  DsGenerica    eds96TMD
098900100811     C                   EXSR      DATI_TMD
099000100811      **--------
099100100811     c                   when      tipo_seg = 'TCC'
099200100811     c                   movel(p)  DsGenerica    eds96TCC
099300100811     C                   EXSR      DATI_TCC
099400100811      **--------
099500100811     c                   when      tipo_seg = 'RNG'
099600100811     c                   movel(p)  DsGenerica    eds96RNG
099700100811     C                   EXSR      DATI_RNG
099800100811      **--------
099900100811     c                   when      tipo_seg = 'DOC'
100000100811     c                   movel(p)  DsGenerica    eds96DOC
100100100811     C                   EXSR      DATI_DOC
100200100120      **--------
100300100811     c                   when      tipo_seg = 'DIM'
100400100811     c                   movel(p)  DsGenerica    eds96DIM
100500100811     C                   EXSR      DATI_DIM
100600100811      **--------
100700100120     c                   when      tipo_seg = 'UNT'
100800100811     c                   movel(p)  DsGenerica    eds96UNT
100900100120     C                   EXSR      DATI_UNT
101000100120      **--------
101100100120     c                   when      tipo_seg = 'UNZ'
101200100811     c                   movel(p)  DsGenerica    eds96UNZ
101300100120     C                   EXSR      DATI_UNZ
101400101222     c                   move      'S'           unz_segmento
101500101222      **--------
101600101222     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
101700100120      **--------
101800100120     c                   OTHER
101900100120      **--------
102000110110     c                   if            unz_segmento <> 'S'
102100120424     c                                           and trovata_EDSTBL <> 'S'
102200100120     c                   move      'S'           seg_imprevisto
102300100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
102400100120     c                   exsr      Error_DET
102500100120     c                   goto      end_Decod
102600110110     c                   end
102700100120      **--------
102800100120     c                   endSL
102900100120      **
103000100120     c     end_Decod     endSR
103100100701      * ?================================================================== */
103200100319     C*? Azzera_MSG   - Azzera dati messaggio
103300100701      * ?================================================================== */
103400100708     C     Scrive_Dettag BEGSR
103500100701      **
103600100708      *  Deve flaggare il dettaglio con dei problemi
103700100708      * Imposta le righe in errore sullo specifico dettaglio
103800100708     C                   if        Err_in_detta = 'S'
103900100708      * prima di rieseguire il ciclo
104000100708      *  rilascia il record
104100100708     c                   update    tivin000
104200100708      * ?  Scrive su tutti i records il tipo di errore
104300100708     c     IniDET_RECord setll     tivin00r
104400100708      **?              /*---------------------- */
104500100708     c                   exsr      DETta_Errato
104600100708      **?              /*---------------------- */
104700100708     c                   end
104800100708      *
104900100708      **  Se presente un errore nel record emette una segnalazione msg
105000100708???? c                   if        se_errore <> 'S'
105100100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
105200100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
105300100708     c                              NAD_Destinatario_in_Testata    ='S'
105400100708      * ?              /*---------------------- */
105500100708     c                   exsr      Scrive_VAB
105600100708      * ?              /*---------------------- */
105700100708     c                   else
105800100708     c                   eval      errori_in_Wrt = 'S'
105900100708     c                   end
106000100708      *
106100100708      *  Problemi nella decodifica dei campi VAB/VAT
106200100708     c                   if        errori_in_Wrt = 'S'
106300100921     C                   evalR     vinMSG = 'EDI-Problemi scrittura VAB'
106400100708     c                   exsr      Error_DET
106500100708     c                   ROLBK
106600100708     c                   else
106700100708     c                   COMMIT
106800100708     c                   end
106900100708      *
107000100708     c                   end
107100100708      **
107200100708     c     end_WRidett   endSR
107300100708      * ?================================================================== */
107400100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
107500100708      * ?================================================================== */
107600100708     C     Pulizia_Campi BEGSR
107700100708      **
107800100708      * inzio messaggio azzera tutte le variabili generali del messaggio
107900100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
108000100708      * ?                         --------------
108100100708     C                   EXSR      Azzera_MSG
108200100708      * ?                         --------------
108300100708      *
108400100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
108500100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
108600100708      * ?                         --------------
108700100813     c                   exsr      Testata_msg
108800100708      * ?                         --------------
108900100708      *
109000100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
109100100708      *  Prepara una nuova spedizione
109200100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
109300100708      * ?                         --------------
109400100708     c                   exsr      Nuova_spediz
109500100708      * ?                         --------------
109600100708     c                   end
109700100708      **
109800100708     c     end_clear     endSR
109900100708      * ?================================================================== */
110000100708     C*? Azzera_MSG   - Azzera dati messaggio
110100100708      * ?================================================================== */
110200100708     C     Azzera_MSG    BEGSR
110300100708      **
110400100701     C*  Inizializza variabili
110500100701     C                   Z-ADD     0             skCLienti
110600100701     C                   Z-ADD     0             sk
110700100701     C*
110800100701      *  INIZIALIZZA  dati fondamentali messaggio
110900100701     C                   clear                   EDIDSPT
111000100701     C                   clear                   EDIDSPU
111100100701     C                   clear                   EDIDSPV
111200120629     C                   clear                   EDIDSPZ
111300160209     C                   clear                   EDIDSPS
111400100701     C*
111500100701     C                   move      'N'           PT_trovata
111600100812     C                   clear                   savXX_PT_key
111700160209     C                   clear                   PS_vabFGS
111800100701     C                   clear                   PT_vabLNP
111900100701     C                   clear                   PT_vabNRS
112000100701     C                   clear                   PT_vabCTM
112100100701     C                   clear                   PT_vabCCM
112200100701     C                   clear                   PU_HLD_SpedVAX
112300111028     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO     = *blank
112400100812     C                   eval        PT_con_piu_Nazioni = *blank
112500100812     c                   move      *blank        Naz_salvata       3
112600090211     C*
112700100701     c                   clear                   UNB_Generico
112800100701     c                   clear                   UNB_Parz
112900100701     c                   clear                   UNB_NrTrasmiss
113000100701     c                   clear                   UNZ_NrTrasmiss
113100100708      *
113200120629     c                   clear                   NAD_SF_Cliente
113300100630      *
113400100630      * per controllare se almeno un record è stato importato sul VAB
113500100630     c                   clear                   Almeno_Uno        1
113600100630     c                   clear                   Almeno_Un_Due     1
113700100708     c                   clear                   IniDET_RECord
113800100708     c                   clear                   FinDET_RECord
113900100630     c                   eval      esito  = '0'
114000100728     c                   eval      Segmento_in_errore = 0
114100100317     C*
114200090211     C                   ENDSR
114300100701      * ?================================================================== */
114400100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
114500100701      * ?================================================================== */
114600100813     C     Testata_msg   BEGSR
114700100701      *
114800100701      *  imposta il Flag per identificare la testata
114900100701     c                   eval      Dati_di_Testata  ='S'
115000100701     C*
115100100701      *   Contatore dei dettagli
115200100701     c                   eval      Contatore_Dettagli = 0
115300100701      **
115400100701      *   Codici identificativi della Testata del messaggio
115500100701     c                   clear                   UNH_Rifer_Msg
115600100706     C* Pulisco dati di work x foglio viaggio
115700100706     C                   clear                   BGM_nrCMR                      * WNRCMR
115800100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
115900100701     c                   clear                   BGM_Id_Testata
116000100701      **
116100100701     C*  Riferimento Foglio Viaggio o CMR
116200100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
116300100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
116400100701      **   Date generali
116500100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
116600100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
116700100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
116800100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
116900100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
117000100702     C                   Z-ADD     0             DTM_DtArrivo                   *
117100100702     C                   Z-ADD     0             DTM_OraArrivo                  *
117200100811     C                   Z-ADD     0             DTM_DtConsRic                  * WvabDCR
117300100811     C                   Z-ADD     0             DTM_OraCnsRic                  * WvabHCR
117400100701      *
117500100701      * pulisce gli RFF+FF di testata
117600100706     C                   clear                   Tes_RFF_FF_KSC
117700100706     C                   clear                   Tes_RFF_FF_TAR
117800100706     C                   clear                   Tes_RFF_FF_LNP
117900160209     C                   clear                   Tes_RFF_FF_FGS
118000100706     C                   clear                   Tes_RFF_FF_NRS
118100100706     C                   clear                   Tes_RFF_FF_CTM
118200100706     C                   clear                   Tes_RFF_FF_BS1
118300100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
118400100701     C*
118500100701     c                   clear                   tes_VABrmo
118600100701     c                   clear                   tes_VABnmo
118700100701     c                   clear                   tes_VABcmo
118800100701     C*
118900100813     c                   clear                   tes_VABtsp
119000100813     c                   clear                   tes_VABtc1
119100120403     c                   clear                   tes_VABtc2
119200100813     c                   clear                   tes_VABnot
119300120214     c                   clear                   tes_VABffd
119400100813     C*
119500100701     c                   clear                   tes_VABrsd
119600100701     c                   clear                   tes_VABrd2
119700100701     c                   clear                   tes_VABind
119800100701     c                   clear                   tes_VABlod
119900100701     c                   clear                   tes_VABnzd
120000100701     C*
120100100701     c                   clear                   tes_Valuta
120200100702     C*
120300100811     c                   clear                   Tot_Peso_msg
120400100811     c                   clear                   Tot_Sped_msg
120500100811     c                   clear                   Tot_Colli_msg
120600100811     c                   clear                   Tot_PNetto_msg
120700100813     C*
120800100813     c                   clear                   AddTot_Colli     16 0
120900100701     C*
121000100701     ** Inizializza campo di work
121100100701     c                   eval      GID_aTotale = 'N'
121200100701     C*
121300100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
121400100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
121500100701     c                   eval      NAD_destinatario_in_testata = *blank
121600100701     c                   eval      NAD_mittente_in_testata = *blank
121700100701     c                   eval      CTA_destinatario_in_testata = *blank
121800100701     c                   eval      COM_telefono_in_testata = *blank
121900100701     C*
122000100701      *   Inizia il conteggio delle righe
122100100706     c                   z-add     0             Conta_segmenti    5 0
122200111028     C*
122300100701     C                   ENDSR
122400100701      * ?================================================================== */
122500100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
122600100701      * ?================================================================== */
122700100701     C     Nuova_spediz  BEGSR
122800100812      **
122900100701     C* Da qui inzia il dettaglio perciò lo memorizzo
123000100701      *  imposta il Flag per identificare la testata
123100100701     c                   eval      Dati_di_Testata  ='N'
123200100708      *
123300100708     c                   z-add     vnrec         IniDET_RECord
123400100812      **
123500100812      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
123600100812      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
123700100812      *
123800100812     c                   if        UNB_parziale ='S' and
123900100812     c                              PT_con_piu_Nazioni = 'S'
124000100812      * ?Cerca dalla riga di questo dettaglio in avanti
124100100812      * ?           *------------------------------*
124200100812     c                   exsr      cerca_Mittente
124300100812     c                   exsr      esito_tab_PT
124400100812      * ?           *------------------------------*
124500100812     c                   endIF
124600100812      **
124700100812      *
124800100701      *  Pulizia dei records
124900100701     C                   CLEAR                   EDiVAB00
125000100701     C                   CLEAR                   EDiVAT00
125100100701     C                   CLEAR                   EDiVAD00
125200100701      **
125300100813      **   Campi da testata messaggio
125400100706     C                   z-add     PT_vabLNP     VABLNP
125500100706     C                   z-add     PT_vabNRS     VABNRS
125600100701     C                   MOVEL     PT_vabCTM     VABCTM
125700100701     C                   z-add     PT_vabCCM     VABCCM
125800100813     C                   movel     tes_VABtsp    VABTSP
125900100813     C                   movel     tes_VABtc1    VABTC1
126000120403     C                   movel     tes_VABtc2    VABTC2
126100100813     C                   movel     tes_VABnot    VABnot
126200120214     C                   movel     tes_VABffd    VABffd
126300100701     C*
126400110707     C                   MOVEL(p)  UNH_vabRMA    VABrma
126500100701     C                   z-add     UNH_vabRMN    VABrmn
126600100701      *
126700100701      *  Dettaglio dei segnacolli Bartolini/EEX
126800100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
126900100701     c                   eval      GID_aTotale = 'N'
127000100701      *
127100100701     C                   clear                   Conta_Segn        5 0
127200100701     C                   clear                   totale_PCI        5 0
127300100701     C                   clear                   RFF_RifSpediz
127400100701      *
127500100701     C                   MOVEA     *HIVAL        segn_BAR
127600100701     C                   MOVEA     *blank        segn_EEX
127700100701      *
127800100701     C*  Azzero codice cliente - tariffa
127900100701     C                   Z-ADD     0             ToT_Colli        16 0
128000100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
128100100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
128200100701      *
128300100701     C*  Azzero codice cliente - tariffa
128400100701     C                   clear                   Det_RFF_FF_ksc
128500100701     C                   clear                   Det_RFF_FF_tar
128600100701     C                   clear                   Det_RFF_FF_lnp
128700160209     C                   clear                   Det_RFF_FF_fgs
128800100701     C                   clear                   Det_RFF_FF_nrs
128900100701     C                   clear                   Det_RFF_FF_ctm
129000100701     C                   clear                   Det_RFF_FF_bs1
129100100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
129200110411     C                   clear                   Det_RFF_FF_tic
129300100701      *
129400100701     C                   clear                   Note_1           35
129500100701     C                   clear                   Note_2           35
129600110907     C                   clear                   Note_1T          35
129700110907     C                   clear                   Note_2T          35
129800131025     C                   clear                   Note_1sms        35
129900131025     C                   clear                   Note_2sms        35
130000100701     C*  Campi di work
130100100701     c                   clear                   VABtic_work
130200100701     c                   clear                   wri_vabtic        1
130300110623     c                   eval      inviato_tipo_incasso ='N'
130400100701      *
130500100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
130600100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
130700100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
130800100701     c                   eval      COM_telefono_in_Dettaglio = *blank
130900100702     c                   eval      CNI_Identificativo_Bolla = *blank
131000110907     c                   eval      FTX_ServizioMail_xDestinatario = *blank
131100131025     c                   eval      FTX_ServizioSMS_xDestinatario = *blank
131200131025     c                   eval      FTX_MAIL_come_Servizio = *blank
131300131025     c                   eval      FTX_SMS_come_Servizio = *blank
131400120613     c                   eval      FTX_ai_piani = *blank
131500100701      *
131600100701     C                   clear                   Rifer_Alfanum    35
131700100910     C                   clear                   Secondo_Rifer    35
131800100701     C                   clear                   Rifer_Cliente    35
131900100701     C                   clear                   Rifer_Numeric    35
132000100701     C                   clear                   Tipo_Rifer        3
132100110613     C                   clear                   Instradamento     1
132200100701      *
132300100701      * Un errore nel dettaglio
132400100701     C                   clear                   Err_in_detta      1
132500100701     c                   clear                   errori_in_Wrt     1
132600100701      *
132700100910     c                   clear                   Primo_RFFcollo    1
132800100910     c                   clear                   Primo_RCUcollo    1
132900100910     c                   z-add     0             conta_rif         1 0
133000100910     c                   clear                   suVABrmo
133100100910      *
133200100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
133300100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
133400140610      *
133500140610     C*  Resetta l'informazione di spedizione di RESO
133600140610     c                   eval      era_un_export_RESO_dal_PARTNER ='N'
133700140610     c                   eval      Sped_RESO = 'N'
133800140610     c                   eval      Cod_RESO_ORM = *blank
133900140623     c                   eval      Salva_REF_ORM = *blank
134000100701     C*
134100100812     C* ---------------------------------------------------
134200100812     C* Inizializzazione Spedizione con campi da testata :
134300100812     C* ---------------------------------------------------
134400100811     C*  Data Richiesta Consegna  da Testata se c'è;
134500100811     C                   IF        DTM_DtConsRic > 0                            Data cons.rich. test
134600100811     C                   z-add     DTM_DtConsRic VABDCR                         Data cons.rich.
134700100811     C                   z-add     DTM_OraCnsRic VABHCR                         Ora  cons.rich.
134800100811     c                   end
134900100812     C*
135000100812     c                   endSR
135100100812     C*----------------------------------------------------------------
135200100812      *? cerca Nazione del primo Mittente nella sequenza dettagli
135300100812     C*----------------------------------------------------------------
135400100812     C     cerca_MittenteBEGSR
135500100812      *
135600100812     c                   z-add     vnrec         rec_posiz         9 0
135700100812     c                   move      *blank        Naz_mittente      3
135800100812     c                   movel(p)  UNB_Generico  PT_Parziale      35
135900100812     c                   move      'N'           OK_Nazione        1
136000100812      *
136100100812      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
136200100812     c                   call      'TRTCT96R1'
136300100812     c                   parm                    rec_posiz
136400100812     c                   parm                    PT_Parziale
136500100812     c                   parm                    Naz_mittente
136600100812     c                   parm                    Ok_Nazione
136700100812     c                   parm                    User_msg
136800100812     C                   parm                    keyUNBCLI
136900100812     C                   parm      'IFCSUM'      keyTIPOMSG
137000100812     C                   parm      'D'           keyVERSION
137100100812     C                   parm      '96A'         keyRELEASE
137200100812     C                   parm      'UN'          keyAGENCY
137300100812     C                   parm                    keyASSOCIA
137400100812      *
137500100812      *  Re-inizializza l'indicatore di trovato
137600100812     c                   setoff                                       32
137700100812      *
137800100812      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
137900100812      *   da decodificare come UNB.
138000100812     c                   if        Ok_Nazione = 'S'
138100100812     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
138200100812     C                   eval      Nr_PT = 1
138300100812     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
138400100812     c                   Endif
138500100812      *
138600100812     c                   endSR
138700100812     C*----------------------------------------------------------------
138800100812      *? esito ricerca tabella PT decodificata oppure NO
138900100812     C*----------------------------------------------------------------
139000100812     C     esito_tab_PT  BEGSR
139100100812      *
139200100812     C                   move      'N'           PT_trovata
139300100812     C   32              move      'S'           PT_trovata
139400100812      *
139500100812      * ******  Errore generale sul Messaggio
139600100812      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
139700100812      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
139800100812     C                   If        PT_trovata = 'N'
139900100812      * Msg di allerta Traduzione
140000100812     C                   EVAL      Indirizzi = CED_Bart
140100100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
140200100921     C                   EVAL      Messaggio = 'EDI -
140300100921     C                             IFCSUM D96A - msg su UPLOAD -
140400100921     C                             con UNB non presente in tabella PT..'
140500100812      *
140600100812     c                   exsr      invio_mail
140700100812     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
140800100812     C                   eval      vinFLG = '2'
140900110208     c                   eval      Almeno_Un_Due ='S'
141000100812     C                   eval      se_errore   = 'S'
141100100812      *
141200100812      *             *------------------------------*
141300100812     C                   eLSe
141400100812      *             *------------------------------*
141500100812      *
141600100812     c                   exsr      Se_Trovata_PT
141700100812      *
141800100812      *
141900100812     c                   Endif
142000100812      *
142100100701     c                   endSR
142200090210     C*----------------------------------------------------------------
142300090210      *? DECODIFICA DATI UNB
142400090210     C*----------------------------------------------------------------
142500100812     C     Se_Trovata_PT BEGSR
142600090210      *
142700100812      * Se tutto OK su tab.PT:
142800100812      *
142900100812     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
143000100812     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
143100100812     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
143200120629     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
143300160209     C                   MOVEL     PS_uni(Nr_PT) EDIDSPS
143400100812      *
143500100812      *  Campi impostati come default:
143600100812      *  Se ho AGE/ON.... prendo i riferimenti definito su tab.PT il tipo di
143700100812      *  qualificatore da utilizzare come facciamo x RFF+AGE con EEX.
143800100812      *  Per default se non impostato diventa automaticamente un AGE.
143900100812     c                   if        §PVRFF = *blank
144000100909     c                   eval      §PVRFF = Riferimento_Spedizione
144100100909     c                   end
144200100813     c                   eval      Riferimento_Spedizione = §PVRFF
144300100910     c                   eval      RFF_DalPrimoCollo      = §PVRFS
144400100910     c                   eval      RFF_DopoIlPrimoCollo   = §PVRSE
144500100909      *
144600100812      *  Anche per il numerico prendo come default "CU"
144700100812     c                   if        §PVRCU = *blank
144800100909     c                   eval      §PVRCU = Riferimento_Numerico
144900100909     c                   end
145000100813     c                   eval      Riferimento_Numerico   = §PVRCU
145100100812      *
145200100812      *  Default per i campi Free Text nel dettaglio 'SIN' e 'ICN'
145300100812     c                   if        §PVsin = *blank
145400100812     c                   eval      §PVsin = 'SIN'
145500100812     c                   end
145600100812      *
145700100812     c                   if        §PVicn = *blank
145800100812     c                   eval      §PVicn = 'ICN'
145900100812     c                   end
146000100812      *
146100100812      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
146200100812      *  ricevuti:
146300100812     c                   movel     §PVema        mail_ind         44
146400100812      *
146500100812      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
146600100812     c                   if        mail_ind <> *blank
146700100812     c                   exsr      imp_ADDR_mail
146800100812     c                   end
146900100812      *
147000100812      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
147100100812      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
147200100812      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
147300100812     C                   if        §PULUN = *blank
147400100812     C                   eval      §PULUN = '00'
147500100812     c                   end
147600100812      *
147700100812      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
147800100812      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
147900100812     C                   move      §puLUN        PU_lunVAT
148000100812     C                   move      §puTS         PU_TipoServ
148100100812     C                   move      §puSS         PU_SpecServ
148200111028     C                   eval       PU_in_RMN_BOLLA_EXPORT_x_RESO = §puRES
148300120629     C                   eval       PZ_abilita_NAD_SF_come_RFF_FF = §pzNADsf
148400100812      *
148500100812     C                   Z-ADD     §PTKSC        PT_vabCCM
148600100812     C                   z-add     §PTLNP        PT_vabLNP
148700100812     C                   z-add     §PTNRS        PT_vabNRS
148800100812     C                   MOVEL     §PTCTM        PT_vabCTM
148900160209      *
149000160209     C                   move      §PTLNP        PS_vabFGS
149100160209     c                   if         §PSfgs <> *blank and
149200160209     c                              §PSfgs <> *zeros
149300160209     C                   move      §PSfgs        PS_vabFGS
149400160209     c                   end
149500100812      *
149600100812      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
149700100812      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
149800100812      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
149900100812      *  i records trasmessi).
150000100812     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
150100100812      *
150200100812      *  In base al cod.trattamento merce reperisco tipo tratt.
150300100812     C                   CLEAR                   DS1B
150400100812     C                   Z-ADD     1             Y                 3 0
150500100812     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
150600100812     C     *IN32         IFEQ      '1'
150700100812     C                   MOVEL     Des_Tb_CTM(Y) DS1B
150800100812     C                   MOVEL     DS1B          sav_DS1B
150900100812     C                   END
151000100812      *
151100100812      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
151200100812     C                   clear                   ACORAG
151300100812     C                   clear                   INDCAE
151400100812     C                   clear                   INDCAP
151500100812     C                   clear                   INDSTA
151600100812      * Decodifico partner
151700100812     C                   Z-ADD     1             KKUT
151800100812     C                   Z-ADD     KCI           KKCC
151900100812     C                   MOVE      §PTKSC        KKSC
152000100812     C     KACO          CHAIN     CNACO00F                           31
152100100812     C     KIND          CHAIN     CNIND00F                           31
152200100812      *
152300100812      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
152400100812      *   questa schiera è utilizzata anche per i clienti.
152500100812     C     §ptKSC        LOOKUP    skCLienti                              39
152600100812     c                   If        *in39 = *off
152700100812     C                   add       1             sk
152800100812     C                   z-add     §ptKSC        skCLienti(sk)
152900100812     C                   Endif
153000100812      *
153100100812     c                   endSR
153200100812     C*----------------------------------------------------------------
153300100812      *? DECODIFICA DATI UNB
153400100812     C*----------------------------------------------------------------
153500100812     C     DATI_UNB      BEGSR
153600100812      *
153700100701      * identificativo Messaggio
153800100701     C                   eval      UNB_NrTrasmiss = unb0020
153900100812      *
154000100812     C                   MOVEL     unb0017       Anno2             2 0
154100100812     C                   z-add     2000          Anno4             4 0
154200100812     C                   add       Anno2         Anno4
154300100812     C                   MOVE      unb0017       WGG               2 0
154400100812     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
154500100812     C                   MOVE      Anno2         Data_messag
154600100812     C                   MOVEL     WGG           Data_messag
154700100812     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
154800100812     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
154900100812     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
155000100812      *
155100100812     c                   clear                   Mittente         35
155200100812     c                   clear                   Mitt_Qualifier    4
155300100812     c                   clear                   Id_Messaggio     14
155400100812     C                   eval      Mittente       = unb0004
155500100812     C                   eval      Mitt_qualifier = unb000720
155600100812     C                   eval      Id_Messaggio   = UNB0022
155700100701      *
155800090209      ** ---------------------------------------------------------------
155900090209      * quindi 1° cosa:
156000090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
156100090209      *     senza questo il messaggio NON è trascodificabile
156200100630     C                   MOVEL     unb0004       UNB_diWork
156300100630     C                   MOVE      unb000720     UNB_diWork
156400100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
156500100630     C                   Z-ADD     1             Nr_PT
156600100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
156700090209      *
156800100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
156900100317      *  la Nazione:
157000100317     C                   If        *IN32 = *off
157100100630     C                   eval      Nr_PT = 1
157200100630     c                   clear                   UNB_diWork
157300100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
157400100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
157500100317      *
157600100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
157700100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
157800100317      *  per cercare di individuare con certezza il codice della tabella PT in base
157900100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
158000100317     C                   If        *IN32 = *on
158100100317      *
158200100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
158300100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
158400100317      * ?  su Partner che hanno + linee con + nazioni.
158500100812      *  se è considerato Parziale
158600100812      *   Salva il codice generico
158700100630     c                   eval      UNB_parziale = 'S'
158800100812     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
158900100812      *             *------------------------------*
159000100812     c                   exsr      cerca_Mittente
159100100812      *             *------------------------------*
159200100812     C                   eval          Naz_salvata = Naz_mittente
159300100317     c                   Endif
159400100317     c                   Endif
159500100812      *
159600100812      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
159700100812      *             *------------------------------*
159800100812     c                   exsr      esito_tab_PT
159900100812      *             *------------------------------*
160000100920      **
160100100920     C                   if        se_errore   = 'S'
160200100920     c                   goto      end_UNB
160300100920     c                   end
160400100812      **
160500100812      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
160600100812     C                   If        PT_trovata = 'S'
160700100812      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
160800100812      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
160900100812      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
161000100812      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
161100100812      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
161200100812     c                   movel     UNB_diWork    UNB_Parz
161300100812     C                   z-add     Nr_PT         savXX_PT_key
161400100812      * ?- Se ha più LINEE da gestire                                      ------ */
161500100812     C                   eval      PT_con_piu_Nazioni = §puPLN
161600100812     c                   end
161700090209      **
161800090210     c     end_UNB       endSR
161900090227      * ?================================================================== */
162000090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
162100090227      * ?================================================================== */
162200090227     C     DATI_UNH      BEGSR
162300100316      *
162400100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
162500100701      *
162600090211      ** Riferimento del cliente
162700100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
162800100701      * ?                                       ------------
162900110707     C                   MOVEL(p)  UNH_Rifer_Msg UNH_VABrma
163000100701      * ?                                       ------------
163100100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
163200100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
163300100701     C                   SUB       1             lunghezza_num
163400100701     C                   EXSR      Se_Numerico
163500100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
163600090211      * ?                                       --------
163700100701     C                   MOVEL     Numero_OK     UNH_VABrmn
163800090211      * ?                                       --------
163900090211     C                   END
164000090303      **
164100090210     c     end_UNH       endSR
164200100701      * ?================================================================== */
164300100701      *? dati iniziali   BGM
164400100701      * ?================================================================== */
164500100701     C     DATI_BGM      BEGSR
164600100701      *
164700100701      *  identificativo testata
164800100706     c                   eval       BGM_Id_Testata = BGM1004
164900100706     C                   IF           BGM_Id_Testata <> *blank
165000100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
165100100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
165200100706     c                   end
165300100706      *
165400100701      **
165500100701     c                   endSR
165600100701      * ?================================================================== */
165700100701     C*?  Contatori in testata o in Dettaglio
165800100701      * ?================================================================== */
165900100701     C     DATI_CNT      BEGSR
166000100701      **
166100100701     c                   IF        Dati_di_Testata  ='S'
166200100701      **
166300100701      *  TOTALE colli, peso, spedizioni:
166400100701     c                   if        cnt6069 = Totale_Peso_Lordo
166500100811     c                   z-add     cnt6066       Tot_Peso_msg     15 3
166600100701     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
166700100811     c                   z-add     cnt6066       Tot_Sped_msg     15 3
166800100701     c                   elseIf    cnt6069 = Totale_Nr_Colli
166900100811     c                   z-add     cnt6066       Tot_Colli_msg    15 3
167000100701     c                   elseIf    cnt6069 = Totale_Peso_Netto
167100100811     c                   z-add     cnt6066       Tot_PNetto_msg   15 3
167200100701     c                   end
167300100701      **
167400100813     c                   elseIF    Dati_di_Testata <>'S'
167500100813     C* Totali della spedizione: dentro il Dettaglio
167600100813      **
167700100813     C*  totale peso
167800100813     c                   if        §pvPEs  = 'S'
167900100813     C*
168000100813     c                   if        CNT6069 = Totale_Peso_Lordo
168100100813      *          Kilogrammi
168200100813     c                   if        CNT6411 = Kilograms
168300100813     C                   Z-ADD     CNT6066       vabPKB                          Peso
168400100813      *          Grammi
168500100813     c                   elseIF    CNT6411 = grams
168600100813     C     CNT6066       div       1000          vabPKB                          Peso
168700100813     c                   end
168800100813     c                   end
168900100813      **
169000100813     c                   end
169100100813     C*  totale colli
169200100813     c                   if        §pvCOl  = 'S'
169300100813     C*
169400100813     c                   if        cnt6069 = Totale_Nr_Colli
169500100813     C                   Z-ADD     cnt6066       vabNCL                          N.Colli
169600100813     C                   ADD       VABNCL        Tot_Colli
169700100813     c                   end
169800100813      **
169900100813     c                   end
170000100813      *
170100100813      **
170200100701     C                   END
170300100701      **
170400100701     c     end_CNT       endSR
170500100701      * ?================================================================== */
170600100909     C*?  i Riferimenti Spedizione / Clienti particolari testata/dettaglio
170700100701      * ?================================================================== */
170800100701     C     DATI_RFF      BEGSR
170900120424      *
171000120424      *   Cerca la decodifica del dato in TABELLA
171100120424     C                   movel(p)  Tipo_Seg      etbSGM
171200120424     C                   movel(p)  RFF1153       etbVALSGM
171300120424     c                   exsr      In_TAB_EDSTBL
171400100701      **
171500100702      * ?   X_Cargo_Manifest o X_Numero_CMR
171600100701      **
171700100701     C*  Se tipo documento CMR ('CMR')
171800100701     C                   if        RFF1153 =  X_Cargo_Manifest
171900100701     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
172000120629      **
172100100701     C*  Se tipo documento AFB (Foglio viaggio)
172200100701     C                   elseIF    RFF1153 =  X_Numero_CMR
172300100701     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
172400100701     C                   END
172500100910     C*
172600120424     C*  Se tipo riferimento Particolare   x CMR/F.Viaggio
172700120424     C                   IF        Quale_campo ='RFFTES'
172800120424     c                              and RFF1153 <> *blank
172900120424     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
173000120424     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
173100120629      **
173200120424     C                   END
173300100910      *  --------------------------*
173400100910      *  Prende il riferimento se non era stato precedentemente caricato
173500100910      *     o se sulla PT c'è la forzatura a livello segnacollo.
173600100910     c                   if        Primo_RFFcollo = *blank
173700100701      *
173800100909      * ? Riferimento singola spedizione
173900100909     C*------------------
174000100910      * ?Riferimento AlfaNumerico preso dal primo dei segnacolli
174100100909     C*------------------
174200100910     C                   IF             Rifer_Alfanum = *BLANK or
174300100910     C                              RFF_DalPrimoCollo = 'S'
174400100910     C*--------
174500100701     c                   if        RFF1153 = Riferimento_Spedizione  and
174600100701     c                                RFF1154 <> *BLANKS
174700100701     C                   movel(p)  RFF1154       Rifer_Alfanum
174800100701      ** controlla se contiene solo numeri
174900100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
175000100701     C                   movel     Rifer_Alfanum test_num
175100100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
175200100701     C                   sub       1             lunghezza_num
175300100701     C                   EXSR      Se_Numerico
175400100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
175500160610      *
175600160610      **
175700160610     C*******            MOVEL(p)  Rifer_Alfanum VABRMA
175800160610      *
175900160610      ****_________ sposta a DESTRA elimina ZERI in più _____A G E _____R M A_________
176000160610      * es.: 00|000000000094652 dalla 3 x 15 caratteri  Taglia i 2 a sinistrA
176100160610      *
176200160610     c                   eval      VABRMA =
176300160610     c                             %subst(Rifer_Alfanum:lunghezza_num-14:15)
176400160610      *
176500160610      ****_________ sposta a DESTRA elimina ZERI in più _____A G E _____R M A_________
176600160610      **
176700160610      *
176800100910     c                   eval      Primo_RFFcollo = 'S'
176900100910      *
177000100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
177100100701     C                              Tipo_Rifer = *blanks
177200100701     C                   movel     Numero_OK     VABRMN
177300100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
177400100701     C                   ENDIF
177500100701     C                   ENDIF
177600100701     C                   ENDIF
177700100910     C                   ENDIF
177800100910      *
177900100910     c                   else
178000100910      *
178100100910      * Se ci sono ulteriori Riferimenti e sono da riportare su VABRMO
178200100910     c                   if        RFF_DalPrimoCollo ='S' and
178300100910     c                             RFF_DopoIlPrimoCollo ='S' and
178400100910     c                             RFF1153 = Riferimento_Spedizione and
178500100910     c                             RFF1154 <> *BLANKS and
178600100910     c                             RFF1154 <> Rifer_Alfanum
178700100910      *
178800100910     c                   eval      conta_rif = conta_rif + 1
178900100910      *
179000100910      *  al max possono essere 2 riferimenti differenti dal principale
179100100910     c                   if        conta_rif <= 2
179200100910     C                   movel     RFF1154       Secondo_Rifer
179300100910      *
179400100910     C                   IF         Secondo_Rifer <> *BLANKS
179500100920     C     ' '           scan      Secondo_Rifer WLUNGH            3 0
179600100910     C                   sub       1             WLUNGH
179700100910      *
179800100910     c                   if        conta_rif = 1
179900100910     c                   clear                   SUvabrmo
180000100910     c                   end
180100100910      *
180200100910     C                   eval      SUvabRMo = %Trim(suVABRMo) + '.' +
180300100910     C                             %Subst(Secondo_Rifer:1:WLUNGH)
180400100910     C                   ENDIF
180500100910     c                   endIF
180600100910      *
180700100910     c                   end
180800100910     c                   endIF
180900100910      *
181000100910     C*----------------------------------------------------------------
181100100910      *  Se ho CU prendo il riferimento numerico
181200100910      *  ECCEZIONE: se ho già impostato FF obbligatorio
181300100910     c                   if        Primo_RCUcollo = *blank
181400100910      *
181500100910     c                   if        Rifer_Numeric = *blank or
181600100910     c                             RFF_DalPrimoCollo = 'S'
181700100909     C*------------------
181800100909      * ?Riferimento Numerico
181900100909     C*------------------
182000100909     C                   if        RFF1153 = Riferimento_Numerico
182100100909     C                              and RFF1154 <> *blanks
182200100909     C                   movel     RFF1154       Rifer_Numeric
182300100909      * Numero Ordine di vendita
182400100909     C                   movel     Rifer_Numeric test_num
182500100909     C     ' '           scan      Rifer_Numeric lunghezza_num
182600100909     C                   sub       1             lunghezza_num
182700100909     C                   EXSR      Se_Numerico
182800100909      *
182900100909     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
183000100910     c                   eval      Primo_RCUcollo = 'S'
183100100909     C                   movel     Numero_OK     VABRMN
183200100909     C                   eval       Tipo_Rifer = Riferimento_Numerico
183300111028      ****
183400100909     C                   Endif
183500100909      *
183600100909     C                   ENDIF
183700100909      *
183800100910     C                   ENDIF
183900100910      *
184000100910     c                   else
184100100910      *
184200100910      * Se ci sono ulteriori Riferimenti Numerici non vengono considerati
184300100910     c                   if        RFF_DalPrimoCollo ='S' and
184400100910     c                             RFF_DopoIlPrimoCollo ='S' and
184500100910     c                             RFF1153 = Riferimento_Spedizione and
184600100910     c                             RFF1154 <> *BLANKS and
184700100910     c                             RFF1154 <> Rifer_Numeric
184800100910      *    ??????
184900100910     c                   end
185000100910      *
185100100910     C                   EndIF
185200100910      *
185300100910     C*----------------------------------------------------------------
185400100909      * NOn c'è l'identificativo bolla del cliente
185500100909     c                   if        (RFF1153 = Riferimento_Spedizione and
185600100909     c                              RFF1154 = *BLANKS)
185700100909     c                             or
185800100909     c                             (RFF1153 = Riferimento_Numerico and
185900100909     c                              RFF1154 = *BLANKS)
186000100909     C                   eval      vinMSG = 'RFF riferimento assente'
186100100909     c                   exsr      Error_DET
186200100909     c                   goto      end_RFF
186300100909     C                   EndIF
186400120629      *>>>>>>>>
186500100909     C*------------------
186600100909      * ?Codice Cliente di riferimento
186700100909     C*------------------
186800110224     C                   clear                   Rifer_Cliente    35
186900100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
187000100701     C                              and RFF1154 <> *BLANKS
187100120426     c                               or
187200120426     C                             Quale_campo ='RFFCLI'  and
187300120426     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
187400120629      *
187500100701     C                   movel     RFF1154       Rifer_Cliente
187600120629      *
187700100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
187800120629     c                   exsr      CL_Particolare
187900120629     C*------------------
188000120426      *>>>>>>>>
188100100702     C                   ENDIF
188200100909      *
188300140610      * ?  A RICEVERE una BOLLA come RESO o BOLLA da ORM
188400140610      * SE IL PARTNER NON VUOLE UTILIZZARE il riferimento NUMERICO e vuole
188500140610      * dare un codice Riferimento PARTICOLARE tabellato su EDSTBL00F x identifare
188600140610      *  un RESO o un ORM
188700140610      *         Memorizzo il codice passato sul campo di 14
188800140610      *  che contiene il nostro identificativo (ex BOLLA Export o ORM export)
188900140610      *
189000140610     C                   if        Quale_campo ='RESORM'
189100140610     c                               and RFF1154 <> *BLANKS
189200140623     C                   movel(p)  RFF1154       salva_REF_ORM
189300140610      *
189400140610      * deve essere ASSOLUTAMENTE un CODICE NUMERICO di 14
189500140919     C                   clear                   TEST_Numerico    35
189600140919     C                   eval       TEST_Numerico = %trim(RFF1154)
189700140610      *
189800140610     C                   movel     TEST_Numerico test_num
189900140610     C     ' '           scan      TEST_Numerico lunghezza_num
190000140610     C                   sub       1             lunghezza_num
190100140610     C                   EXSR      Se_Numerico
190200140610      *
190300140610      * riferimento restituito da Partner  --> lo imposta solo se numerico
190400140610     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
190500140610      * attenzione Numero_OK (lungo 15) quindi "MOVE"!
190600140610     C                   move      Numero_OK     Cod_RESO_ORM
190700140610     C                   Endif
190800140610      *
190900140610     C                   END
191000140610      *
191100100701     c     end_RFF       endSR
191200100701      * ?================================================================== */
191300100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
191400100701      * ?================================================================== */
191500100701     C     DATI_DTM      BEGSR
191600100705      *
191700100705      *   Cerca la decodifica del dato in TABELLA
191800120424     C                   movel(p)  Tipo_Seg      etbSGM
191900100705     C                   movel(p)  dtm2005       etbVALSGM
192000100705     c                   exsr      In_TAB_EDSTBL
192100100705      *
192200100701     C*  Se data documento  memorizzo numero e data x CMR
192300100705     C                   If        Quale_campo ='DATDOC'
192400101015     c                              and dtm2380 <> *blank
192500100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
192600100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
192700100702     C                   EXSR      Converte_Data
192800100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
192900100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
193000100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
193100100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
193200100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
193300100701     C     WERRO         IFEQ      'S'
193400100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
193500100706     C                             dtm2005  +
193600100702     C                              ') data errata'
193700100701     c                   exsr      Error_DET
193800100701     c                   goto      end_DTM
193900100701     C                   END
194000100701     C                   END
194100100702     C*
194200100702     C* Data Partenza del mezzo
194300100705     C                   If        Quale_campo ='DATPAR'
194400101015     c                              and dtm2380 <> *blank
194500100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
194600100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
194700100702     C                   EXSR      Converte_Data
194800100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
194900100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
195000100702     C     WERRO         IFEQ      'S'
195100100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
195200100706     C                             dtm2005  +
195300100702     C                              ') data errata'
195400100702     c                   exsr      Error_DET
195500100702     c                   goto      end_DTM
195600100702     C                   END
195700100702     C                   END
195800100702     C*
195900100702     C* Data Previsto Arrivo del mezzo
196000100705     C                   If        Quale_campo ='DATARR'
196100101015     c                              and dtm2380 <> *blank
196200100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
196300100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
196400100702     C                   EXSR      Converte_Data
196500100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
196600100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
196700100702     C     WERRO         IFEQ      'S'
196800100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
196900100706     C                             dtm2005  +
197000100702     C                              ') data errata'
197100100702     c                   exsr      Error_DET
197200100702     c                   goto      end_DTM
197300100702     C                   END
197400100702     C                   END
197500100701      **
197600100701     C*  Se data consegna richiesta imposto già sui campi del VAB
197700100705     C                   If        Quale_campo ='VABDCR'
197800101015     c                              and dtm2380 <> *blank
197900100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
198000100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
198100100702     C                   EXSR      Converte_Data
198200100811      **
198300100811     c                   IF        Dati_di_Testata  ='S'
198400100811     C                   MOVEL     WCAMG         DTM_DtConsRic                  Data cons.rich.
198500100811     C                   MOVEL     WHMS          DTM_OraCnsRic                  Ora  cons.rich.
198600100811     c                   end
198700100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
198800100811     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
198900100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
199000100701     C     WERRO         IFEQ      'S'
199100100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
199200100706     C                             dtm2005  +
199300100702     C                              ') data errata'
199400100701     c                   exsr      Error_DET
199500100701     c                   goto      end_DTM
199600100701     C                   END
199700100702     C                   end
199800100701      **
199900100701     c     end_DTM       endSR
200000100813      * ?================================================================== */
200100100813     C*?  Tipo Servizio
200200100813      * ?================================================================== */
200300100813     C     DATI_TSR      BEGSR
200400100813      ***
200500100813     C*  Reperisco priorità spedizione:
200600100813     C                   Z-ADD     1             X
200700100813      ***
200800100813      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
200900100813      ***  mentre in tabella EDI EEX è fatto con "1"
201000100813      ***
201100100813     c                   movel(p)  TSR4219       Work_Data__35
201200100813     c                   move      PU_TipoServ   Work_Data__35
201300100813     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
201400100813     C     *IN32         IFEQ      '1'
201500100813      *
201600100813     c                   if        Dati_di_Testata <>'S'
201700100813     C                   eval             VABTSP = TipoServizio(X)
201800100813     c                   elseIf    Dati_di_Testata = 'S'
201900100813     C                   eval         tes_VABtsp = TipoServizio(X)
202000100813     c                   end
202100100813      *
202200100813     C                   ELSE
202300100813      **  Il tipo bolla non è presente
202400100813     C                   eval      vinMSG = 'TSR non in tabella'
202500100813     c*********          exsr      Error_DET
202600100813     c*********          goto      end_TSR
202700100813     C                   END
202800100813     C*
202900100813     C*  Decodifico servizi speciali
203000100813     C     TSR7085       IFNE      *BLANKS
203100100813     C     TSR7085       ANDNE     *LOVAL
203200100813      *
203300100813     C                   Z-ADD     1             X
203400100813     c                   movel(p)  TSR7085       Work_Data__35
203500100813     c                   move      PU_SpecServ   Work_Data__35
203600100813     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
203700100813      * Particolarità
203800100813     C     *IN32         IFEQ      '1'
203900100813     C                   eval       EDIDSSS = UNI_ServSpec(X)
204000100813      * Descrizione
204100100813     C     §SSNOT        IFEQ      'S'
204200100813     c                   if        Dati_di_Testata <>'S'
204300100813     C                   MOVEL     §SSDES        VABNOT
204400100813     c                   elseIf    Dati_di_Testata = 'S'
204500100813     C                   eval       tes_VABnot = §SSDES
204600100813     c                   end
204700100813     C                   END
204800100813      *
204900100813      * x Espresso     "Pre-noon"
205000100813     C     §SSTPS        IFEQ      'S'
205100100813     c                   if        Dati_di_Testata <>'S'
205200100813     C                   MOVEL     'E'           VABTSP
205300100813     c                   elseIf    Dati_di_Testata = 'S'
205400100813     C                   eval         tes_VABtsp = 'E'
205500100813     c                   end
205600100813     C                   END
205700110509      *
205800110509      * x H10:30       "10H"
205900110509     C     §SSTPS        IFEQ      'H'
206000110509     c                   if        Dati_di_Testata <>'S'
206100110509     C                   MOVEL     'H'           VABTSP
206200110509     c                   elseIf    Dati_di_Testata = 'S'
206300110509     C                   eval         tes_VABtsp = 'H'
206400110509     c                   end
206500110509     C                   END
206600100813      *
206700120403      * x Appuntamento "Booking ins"  "BKI"
206800100813     C     §SSTPS        IFEQ      'A'
206900100813     c                   if        Dati_di_Testata <>'S'
207000120403     c                   if             vabTC1 = *blank
207100100813     C                   MOVEL     'A'           VABTC1
207200120403     c                   elseIF         vabTC2 = *blank
207300120403     C                   MOVEL     'A'           VABTC2
207400120403     c                   end
207500100813     c                   elseIf    Dati_di_Testata = 'S'
207600120403     c                   if         tes_VABtc1 = *blank
207700100813     C                   eval         tes_VABtc1 = 'A'
207800120403     c                   elseIF     tes_VABtc2 = *blank
207900120403     C                   eval         tes_VABtc2 = 'A'
208000120403     c                   end
208100100813     c                   end
208200100813     C                   END
208300100813      *
208400120214      *
208500120214      * x FERMO DEPOSITO "Goods Waiting Collection"  GWC
208600120214     C     §SSTPS        IFEQ      'F'
208700120214     c                   if        Dati_di_Testata <>'S'
208800120214     C                   MOVEL     'S'           VABFFD
208900120214     c                   elseIf    Dati_di_Testata = 'S'
209000120214     C                   eval         tes_VABffd = 'S'
209100120214     c                   end
209200120214     C                   END
209300120214      *
209400100813     C                   ELSE
209500100813      *
209600100813      **  Il tipo bolla non è presente
209700100813     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
209800100813     c*******            exsr      Error_DET
209900100813     c*******            goto      end_TSR
210000100813      *
210100100813     C                   END
210200100813      *
210300100813     C                   END
210400100813      **
210500100813     c     end_TSR       endSR
210600100813      * ?================================================================== */
210700100813     C*?  C.O.D. Monetary Amount
210800100813      * ?================================================================== */
210900100813     C     DATI_MOA      BEGSR
211000100813      *
211100100813      *   Cerca la decodifica del dato in TABELLA
211200120424     C                   movel(p)  Tipo_Seg      etbSGM
211300100813     C                   movel(p)  moa5025       etbVALSGM
211400100813     c                   exsr      In_TAB_EDSTBL
211500100813      *
211600100813     C* Controllo se campo importo numerico con 3 decimali
211700100813     C     moa5004       IFNE      *zeros
211800100813      *
211900100813     C                   If        Quale_campo ='VABIAS'
212000130529      *
212100130529      * il campo è diviso in 2 flag: il primo serve per l'importo da assicurare
212200130529      *    quindi se deve essere considerato è espresso sulla PT.
212300130529     c                   if        %subst(§PTxxx:1:1) = 'S'
212400110726     C     moa5004       div       1000          VABIAS                         IMPORTO
212500100813     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
212600130529     c                   end
212700100813      *
212800100813     C                   elseIF    Quale_campo ='VABVMD'
212900110726     C     moa5004       div       1000          VABVMD                         MERCE
213000100813     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
213100100813      *
213200100813     C                   elseIF    Quale_campo ='VABCAS'
213300100813     C     §PUCAS        IFEQ      'S'                                          C/Assegno
213400100813      *
213500110726     C     moa5004       div       1000          VABCAS
213600100813     C                   MOVEL     moa6345       VABVCA
213700100813     c                   if        Tes_valuta <> *blank and VABVCA = *blank
213800100813     C                   MOVEL     Tes_valuta    VABVCA
213900100813     c                   end
214000110628      **
214100110628      **  se c'è il default ("??")
214200110628     C                   Z-ADD     1             Y
214300110628     c                   movel(p)  '??'          Work_Data__35
214400110628     c                   move      §puTC         Work_Data__35
214500110628     c                   clear                   WData_35_2        2
214600110628     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
214700110628     c                   if        not *in33
214800110628      *
214900130311      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO ( ma deve leggere il segmento
215000130311      * contenente il Tipo Incasso - sempre successivo al segmento MOA )
215100130311      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
215200130311      *
215300130311      * ATTENZIONE viene impostato di Default ma, in presenza del FTX+PAI,
215400130311      *  viene reimpostato in base a quello sulla tabella "TC".
215500130311      *   Quindi viene modificato da "OM" ....a quello sul FTX+PAI successivo.
215600100813     C     §PUTPC        IFEQ      'P'
215700100813     C                   MOVEL     'OM'          VABTIC
215800100813     C                   MOVEL     'OM'          VABtic_work
215900100813     C                   ENDIF
216000110628      **
216100110628     c                   end
216200100813      *
216300100813     C                   END
216400100813      *
216500100813     C                   EndIF
216600100813     C                   END                                                    Imp.non numer.
216700100813      *
216800100813     c     end_MOA       endSR
216900100813      * ?================================================================== */
217000100813     C*?  FREE TEXT
217100100813      * ?================================================================== */
217200100813     C     DATI_FTX      BEGSR
217300100813      *
217400100813      *   Cerca la decodifica del dato in TABELLA
217500120424     C                   movel(p)  Tipo_Seg      etbSGM
217600100813     C                   movel(p)  ftx4451       etbVALSGM
217700100813     c                   exsr      In_TAB_EDSTBL
217800100813      *
217900100813      *
218000100813      * ? solo se si tratta di note descrittive da comunicare in BOLLA
218100100813     c                   if         quale_campo = 'VABNOT'
218200100813     c                                or ftx4451 = §pvSIN
218300100813     c                                or ftx4451 = §pvICN
218400100813     C                   DO        4             X
218500100813      *
218600100813     C     D05(X)        IFNE      *BLANKS
218700100813      *
218800100813     C     Note_1        IFEQ      *BLANKS
218900100813     C                   MOVEL     D05(X)        Note_1
219000100813     C                   MOVE      D05(X)        Note_2
219100100813     C                   ELSE
219200100813     C     Note_2        IFEQ      *BLANKS
219300100813     C                   MOVEL     D05(X)        Note_2
219400100813     C                   END
219500100813     C                   END
219600100813      *
219700100813     C                   ENDif
219800100813      *
219900100813     C                   ENDdo
220000100813     C                   end
220100100813     C*------>>
220200110907      * ? solo se si tratta di note x invio mail di avviso al destinatario
220300110907      *    (nuovo servizio a pagamento attivato nel 2011)
220400110907      *     dovranno essere scritti 2 records nel VAT di tipo I e J
220500110907     c                   if         quale_campo = 'VATNOT'
220600131025     c                                 or
220700131025     c                              quale_campo = 'VATEML'
220800131025      *
220900131025     c                   if        ftx4453 = 'N'
221000131025     c                   eval      FTX_MAIL_come_servizio = 'N'
221100131025     c                   end
221200110907      *
221300110907     C                   DO        4             X
221400110907      *
221500110907     C     D05(X)        IFNE      *BLANKS
221600110907      *
221700110907     C     Note_1t       IFEQ      *BLANKS
221800110907     C                   MOVEL     D05(X)        Note_1t
221900110907     C                   MOVE      D05(X)        Note_2t
222000110907     C                   ELSE
222100110907     C     Note_2t       IFEQ      *BLANKS
222200110907     C                   MOVEL     D05(X)        Note_2t
222300110907     C                   END
222400110907     C                   END
222500110907      *
222600110907     C                   ENDif
222700110907      *
222800110907     C                   ENDdo
222900110907      *
223000110907      * carica il campo da riportare poi sul VAT
223100110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:1:35) =
223200110907     c                             note_1T
223300110907     c                   eval      %subst(FTX_ServizioMail_xDestinatario:36:35)=
223400110907     c                             note_2T
223500110907     C                   end
223600131025     C*------>>
223700131025      * ? solo se si tratta di note x invio SMS di avviso al destinatario
223800131025      *    (nuovo servizio a pagamento attivato)
223900131025      *     dovrà essere scritto 1 record nel VAT di tipo S
224000131025     c                   if         quale_campo = 'VATSMS'
224100131025     c                   if        ftx4453 = 'N'
224200131025     c                   eval      FTX_SMS_come_servizio = 'N'
224300131025     c                   end
224400131025      *
224500131025     C                   DO        4             X
224600131025      *
224700131025     C     D05(X)        IFNE      *BLANKS
224800131025      *
224900131025     C     Note_1sms     IFEQ      *BLANKS
225000131025     C                   MOVEL     D05(X)        Note_1sms
225100131025     C                   MOVE      D05(X)        Note_2sms
225200131025     C                   ELSE
225300131025     C     Note_2sms     IFEQ      *BLANKS
225400131025     C                   MOVEL     D05(X)        Note_2sms
225500131025     C                   END
225600131025     C                   END
225700131025      *
225800131025     C                   ENDif
225900131025      *
226000131025     C                   ENDdo
226100131025      *
226200131025      * carica il campo da riportare poi sul VAT
226300131025     c                   eval      %subst(FTX_ServizioSMS_xDestinatario:1:35) =
226400131025     c                             note_1sms
226500131025     C                   end
226600110907     C*------>>
226700100813     C*  Decodifico tipo pagamento C/Assegno
226800100813      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
226900100813      *      (Problema di pulizia in presenza di + righe di testo)
227000100813      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
227100100813      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
227200100813      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
227300100813      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
227400100813     C                   if        wri_vabtic = *blank
227500100813     C                   MOVEL     *BLANKS       VABTIC
227600100813     c                   end
227700100813      **
227800100813     c                   clear                   trovato_TPInc     1            TipoIncasso
227900100813      *
228000100813      * ?Se c'è il tipo Incasso
228100100813     c                   if         quale_campo = 'VABTIC'                      -- 01 --
228200100813      *
228300100813     C                   DO        4             X                              -- 02 --
228400100813     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
228500130311      **
228600130311      ** Prima era di 2, ora è stato portato a 10 alfa (il codice del Tipo Incasso)
228700130311      **  per gestire Partner come AZKAR che ha codice "AAA"
228800130311     C**********         MOVEL     D05(X)        WCTC              2
228900130311     C                   MOVEL     D05(X)        WCTC             10
229000110623      **
229100110623     c                   eval      inviato_tipo_incasso ='S'
229200110623      **
229300110623      **  se non l'avesse trovato e fosse esplicitato un Default ("??")
229400110623      **   come per qualsisi codice venga inviato dal Cliente allora
229500110623      **   deve essere impostato il default (invece di fare un chiodo)
229600110623     C                   Z-ADD     1             Y
229700110623     c                   movel(p)  '??'          Work_Data__35
229800110623     c                   move      §puTC         Work_Data__35
229900110623     c                   clear                   WData_35_2        2
230000110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
230100110623     c                   if            *in33
230200110623     c                   move      'S'           trovato_TPInc
230300110623     C                   MOVEL     TipoIncasso(Y)vabtic
230400110623     c                   movel(p)  '??'          WData_35_2
230500110623     c                   else
230600110623      *
230700110623     C                   Z-ADD     1             Y
230800110623     c                   movel(p)  Wctc          Work_Data__35
230900130326     c                   move      §puTC         Work_Data__35
231000110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
231100110623     c                   if            *in33
231200110623     c                   move      'S'           trovato_TPInc
231300110623     C                   MOVEL     TipoIncasso(Y)vabtic
231400130326     c                   move      §puTC         WData_35_2        2
231500110623     c                   else
231600110623      *
231700110623     C                   Z-ADD     1             Y
231800110623     c                   movel(p)  Wctc          Work_Data__35
231900110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
232000110623     c                   if            *in33
232100110623     c                   move      'S'           trovato_TPInc
232200110623     C                   MOVEL     TipoIncasso(Y)vabtic
232300130311     c                   movel     Wctc          WData_35_2        2
232400110623     c                   end
232500110623      *
232600110623     c                   end
232700110623      *
232800110623     c                   end
232900110623      **
233000130311      *--------
233100130311      * SOLO SE PARTNER
233200130311      *   IMPOSTO 'OM'    (vecchia regola ormai in disuso se NON siano state
233300130311      *     trovate fra le tabelle dei codici ben precisi x il PARTNER,
233400130311      ***     ossia solo se NON E'STATO PASSATO un codice specifico
233500130311      ** ATTENZIONE NON DEVE ESISTERE IL BLANK in arrivo dal PARTNER ma sempre un CODICE.
233600110623     C     §PUTPC        IFEQ      'P'
233700110623     c     wctc          andne     'TO'
233800110623     c     WData_35_2    andeq     *blank
233900110623     C                   MOVEL     'OM'          VABTIC
234000110623     C                   END                                                    -- 03 --
234100110623     C                   END                                                    -- 03 --
234200110623     C                   ENDdo                                                  -- 02 --
234300100813      *
234400100813     C                   ELSE                                                   -- 01 --
234500100813      *
234600100813      * ?Se NON c'è il tipo Incasso Contrassegno particolare
234700100813      *    Deve essere impostato comunque da testata
234800110623     c                   if        vabtic = *blank   and
234900110623     c                             WData_35_2 = *blank
235000100813     C                   MOVEL     VABtic_work   VABTIC
235100100813     c                   end
235200100813      *
235300100813     C                   END                                                    -- 01 --
235400100813      *
235500100813      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
235600100813      *    essere presenti altri record Flat con altre informazioni
235700100813      *    che potrebbero abblancare il Tipo incasso precedentemente
235800100813      *    decodificato
235900100813     c                   if        Trovato_TPInc = 'S' and
236000100813     c                                 wri_vabtic = *blank
236100100813     c                   eval      wri_vabtic = 'S'
236200100813     c                   end
236300100813      *
236400100813      * ?Se Natura Merce      .
236500100910     C*  Natura Merce  (Se abilitato alla trascodifica)
236600100910     c                   if        §PUnas = 'S'
236700100813     c                   if         quale_campo = 'VABNAS'                      -- 01 --
236800100813     C                   MOVEL     D05(1)        VABnas
236900100813     c                   End
237000100910     c                   End
237100100813      *
237200120613      * ?Consegne Particolari
237300120613     c                   if         quale_campo = 'VABTCX'                      -- 01 --
237400120613      * Imposta il codice di 3 chrs.
237500120613      *   solo i primi 3 caratteri del campo
237600120613     C                   eval      controlla_COD_FTX = D05(1)
237700120613      *
237800120613      ****  se i primi 3 caratteri sono "FLR" FLOOR --> Ai piani
237900120613     C                   if        controlla_COD_FTX = 'FLR'
238000120613     c                   eval      FTX_ai_piani = 'P'
238100120613     c                   End
238200120613     c                   End
238300120613      *
238400120613      *
238500100813     c     end_FTX       endSR
238600100701      * ?================================================================== */
238700100701     C*? Imposta informazioni sul dettaglio del trasporto
238800100701      * ?================================================================== */
238900100701     C     DATI_TDT      BEGSR
239000100701     C*
239100100701      **
239200100701     c     end_TDT       endSR
239300100811      * ?================================================================== */
239400100811     C*? Imposta
239500100811      * ?================================================================== */
239600100811     C     DATI_TMP      BEGSR
239700100811     C*
239800100811      **
239900100811     c     end_TMP       endSR
240000100811      * ?================================================================== */
240100100811     C*? Imposta
240200100811      * ?================================================================== */
240300100811     C     DATI_TMD      BEGSR
240400100811     C*
240500100811      **
240600100811     c     end_TMD       endSR
240700100811      * ?================================================================== */
240800100811     C*? Imposta
240900100811      * ?================================================================== */
241000100811     C     DATI_TCC      BEGSR
241100100811     C*
241200100811      **
241300100811     c     end_TCC       endSR
241400100811      * ?================================================================== */
241500100811     C*? Imposta
241600100811      * ?================================================================== */
241700100811     C     DATI_DIM      BEGSR
241800100811     C*
241900100811      **
242000100811     c     end_DIM       endSR
242100100811      * ?================================================================== */
242200100811     C*? Imposta
242300100811      * ?================================================================== */
242400100811     C     DATI_RNG      BEGSR
242500100811     C*
242600100811      **
242700100811     c     end_RNG       endSR
242800100811      * ?================================================================== */
242900100811     C*? Imposta
243000100811      * ?================================================================== */
243100100811     C     DATI_DOC      BEGSR
243200100811     C*
243300100811      **
243400100811     c     end_DOC       endSR
243500100811      * ?================================================================== */
243600100811     C*? Imposta
243700100811      * ?================================================================== */
243800100811     C     DATI_GIN      BEGSR
243900100811     C*
244000100920      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
244100100920      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
244200100920     C     §1BF12        IFEQ      'E'
244300100920     C                   EXSR      SEGNaCollo_EEX
244400100920     C                   ELSE
244500100920      *
244600100920      *  La prima volta controllo congruità numero segnacollo
244700100920     C     §PUBS1        IFNE      *BLANKS
244800100920     C     VABnrs        ANDNE     0
244900100920      *  Se il codice trattamento merce prevede che segnacollino i
245000100920      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
245100100920     C     §1BFG7        IFEQ      'S'
245200100920     C     §1BFG1        OREQ      'S'
245300100920      *  calcolo segnacollo
245400100920     C                   EXSR      Segn_Bartolini
245500100920     C                   END
245600100920     C                   END
245700100920      *
245800100920     C                   END
245900100811      **
246000100811     c     end_GIN       endSR
246100100811      * ?================================================================== */
246200100811     C*? Imposta
246300100811      * ?================================================================== */
246400100811     C     DATI_HAN      BEGSR
246500100811     C*
246600100811      **
246700100811     c     end_HAN       endSR
246800100811      * ?================================================================== */
246900100811     C*? Imposta
247000100811      * ?================================================================== */
247100100811     C     DATI_GOR      BEGSR
247200100811     C*
247300100811      **
247400100811     c     end_GOR       endSR
247500100705      * ?================================================================== */
247600100705     C*? Imposta la Località
247700100705      * ?================================================================== */
247800100705     C     DATI_LOC      BEGSR
247900100705     C*
248000100705      **
248100100705     c     end_LOC       endSR
248200100705      * ?================================================================== */
248300100705     C*? Imposta identificativi delle Merci
248400100705      * ?================================================================== */
248500100705     C     DATI_EQD      BEGSR
248600100705     C*
248700100705      **
248800100705     c     end_EQD       endSR
248900100705      * ?================================================================== */
249000100705     C*? Imposta il numero delle Merci
249100100705      * ?================================================================== */
249200100705     C     DATI_EQN      BEGSR
249300100705     C*
249400100705      **
249500100705     c     end_EQN       endSR
249600100316      * ?================================================================== */
249700100316     C*? Imposta l'inizio del Dettaglio CNI
249800100316      * ?================================================================== */
249900100316     C     DATI_CNI      BEGSR
250000100316     C*
250100100702      ** Inizio Dettaglio riferimento iniziale
250200100702     c                   If        CNI1004 <> *blank
250300100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
250400100813      *
250500100813      *   Nel Riferimento Alfa sempre
250600100813     c                   eval      VABRMA  = CNI_Identificativo_Bolla
250700100813      *
250800100813      *   e nel numerico se di soli numeri e comunque se lo memorizza
250900100813     c                   clear                   CNI_seNumerico
251000100813     C                   eval       test_num = CNI1004
251100100813     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
251200100813     C                   SUB       1             lunghezza_num
251300100813     C                   EXSR      Se_Numerico
251400100813     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
251500100813      * ?                                       --------
251600100813     C                   MOVEL     Numero_OK     VABrmn
251700100813      * ?                                       --------
251800100813     C                   z-add     VABrmn        CNI_seNumerico
251900100813      * ?                                       --------
252000100813     C                   END
252100100813      *
252200100702     c                   end
252300100319      *
252400100316      **
252500100316     c     end_CNI       endSR
252600100316      * ?================================================================== */
252700100316     C*? Imposta le merci pericolose
252800100316      * ?================================================================== */
252900100316     C     DATI_DGS      BEGSR
253000100316     C*
253100100316      **
253200100316     c     end_DGS       endSR
253300100316      * ?================================================================== */
253400100316     C*? Imposta il numero seriale
253500100316      * ?================================================================== */
253600100316     C     DATI_SEL      BEGSR
253700100316     C*
253800100316      **
253900100316     c     end_SEL       endSR
254000100316      * ?================================================================== */
254100100316     C*? Imposta informazioni aggiuntive
254200100316      * ?================================================================== */
254300100316     C     DATI_PIA      BEGSR
254400100316     C*
254500100316      **
254600100316     c     end_PIA       endSR
254700090209      * ?================================================================== */
254800090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
254900090209      * ?================================================================== */
255000090209     C     DATI_TOD      BEGSR
255100090209      *
255200090210      *  Trascodifico tipo trasporto
255300090210     c                   clear                   TOD_char3         3
255400100705      *  DEFAULT da PT
255500090210      *  imposta il default dalla tabella PT se c'è
255600090210     C     §puPFA        ifNE      *blank
255700090210     c                   SELECT
255800090210      *  Porto Franco
255900090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
256000090210     C                   movel     '4'           VABCBO                         + C/Ass
256100090210     c                   WHEN      §puPFA = 'F'
256200090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
256300090210      *  Porto Assegnato
256400090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
256500090210     C                   movel     '6'           VABCBO                         + C/Ass
256600090210     C                   WHEN      §puPFA = 'A'
256700090210     C                   movel     '2'           VABCBO                         Senza C/Ass
256800090210     C                   ENDSL
256900090210     c                   end
257000090210      *
257100090210      *   Imposta o il codice o il metodo
257200090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
257300090210     c                   if        tod4053  <> *blank
257400090210     c                   movel(p)  tod4053       TOD_char3
257500090210     c                   else
257600090210     c                   if        tod4215  <> *blank
257700090210     c                   movel(p)  tod4215       TOD_char3
257800090210     c                   end
257900090210     c                   end
258000090210      **
258100110328      ****************
258200110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
258300110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
258400110328      ****************
258500110328     c                   eval      Trovata_Tab_TB ='N'
258600110328      *
258700110328     c                   if              TOD_char3 <> *blank
258800110328     c                   movel(p)  TOD_char3     Key_Generica35
258900110328     c                   move      §puTB         Key_Generica35
259000110328     C                   eval      Y = 1
259100110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
259200110328     c   32              eval      Trovata_Tab_TB ='S'
259300110328      *
259400110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
259500110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
259600110328      *
259700110328     c                   if          Trovata_Tab_TB ='N'
259800110328     c                                 and
259900110328     C                               tod4215 <> *blank
260000110328     c                   movel(p)  tod4215       TOD_char3
260100110328     c                   movel(p)  TOD_char3     Key_Generica35
260200110328     c                   move      §puTB         Key_Generica35
260300110328     C                   eval      Y = 1
260400110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
260500110328     c   32              eval      Trovata_Tab_TB ='S'
260600110328     c                   end
260700110328     c                   end
260800110328      *
260900110328      ****************
261000110328      ******* Se vi sono eccezioni sul cliente imposta quelle
261100110328      ****************
261200110328     c                   If          Trovata_Tab_TB ='S'
261300110328      *
261400110328     c                   SELECT
261500110328     c                   WHEN      Decod_Porto(Y) = 'F'
261600110328     c                               and  VABcas > *zeros
261700110328     C                   movel     '4'           VABCBO                         + C/Ass
261800110328     c                   WHEN      Decod_Porto(Y) = 'F'
261900110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
262000110328      *  Porto Assegnato
262100110328     c                   WHEN      VABCAS > *zeros
262200110328     C                   movel     '6'           VABCBO                         + C/Ass
262300110328     C                   OTHER
262400110328     C                   movel     '2'           VABCBO                         Senza C/Ass
262500110328     C                   ENDSL
262600110328      *
262700110328      ****************  altrimenti
262800110328      ******* Se non vi sono eccezioni per il cliente
262900110328      *   Rileva a questo punto lo Standard per tutti
263000110328      ****************
263100110328     c                   elseIf      Trovata_Tab_TB ='N'
263200110328      **
263300090210     c                   if              TOD_char3 <> *blank
263400100705      *   Cerca la decodifica del dato in TABELLA
263500120424     C                   movel(p)  Tipo_Seg      etbSGM
263600100705     C                   movel(p)  TOD_char3     etbVALSGM
263700100705     c                   exsr      In_TAB_EDSTBL
263800090210      *
263900100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
264000100705      *
264100090210     c                   SELECT
264200100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
264300100705     c                               VABcas > *zeros
264400090210     C                   movel     '4'           VABCBO                         + C/Ass
264500100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
264600090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
264700090210      *  Porto Assegnato
264800090210     c                   WHEN      VABCAS > *zeros
264900090210     C                   movel     '6'           VABCBO                         + C/Ass
265000090210     C                   OTHER
265100090210     C                   movel     '2'           VABCBO                         Senza C/Ass
265200090210     C                   ENDSL
265300090210      *
265400090210     c                   end
265500090210     c                   endIF
265600090209      **
265700110328     c                   ENDif
265800110328      **
265900090210      **  Il tipo bolla non è presente
266000090210     c                   if        VABCBO = *blank
266100090210     C                   eval      vinMSG = 'TOD non rilevato'
266200090213     c                   exsr      Error_DET
266300090210     c                   goto      end_TOD
266400090210     c                   end
266500090210      **
266600090210     c     end_TOD       endSR
266700090213      * ?================================================================== */
266800090213     C*?  Anagrafica del mittente e del destinatario
266900090213      * ?================================================================== */
267000091016     C     DATI_NAD      BEGSR
267100100705      *
267200100705      *   Cerca la decodifica del dato in TABELLA
267300100813     c                   clear                   dopo_NAD_CN       1
267400120424     C                   movel(p)  Tipo_Seg      etbSGM
267500100705     C                   movel(p)  nad3035       etbVALSGM
267600100705     c                   exsr      In_TAB_EDSTBL
267700100708      *-------
267800120629      * Dato (SHIPPED FROM) usato principalmente x CMR
267900120629      *       ma anche usato al posto di un RFF+FF in testata messaggio
268000120629      *   quindi modificato il nome di riferimento del campo sul EDSTBL
268100120629      *   x renderlo più generico.
268200100708      *-------
268300120629     C********************        IF        Quale_campo ='VABCMR'
268400120629     C                   IF        Quale_campo ='NAD_SF'
268500120629      *-------
268600120629      * Dati di PROVENIENZA MESSAGGIO
268700120629      *-------
268800120629      ***
268900120629      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
269000100708     c                   if        nad30361 <> *blank
269100120629     C                   MOVEL     nad30361      NAD_SF_Cliente
269200100708     c                   elseIf    nad31241 <> *blank
269300120629     C                   MOVEL     nad31241      NAD_SF_Cliente
269400100708     c                   elseIf    nad3039  <> *blank
269500120629     C                   MOVEL     nad3039       NAD_SF_Cliente
269600100708     c                   end
269700120629      ***
269800120629      * ?SOLO SE NON c'è un RFF+FF: di Testata che prevale su tutto.
269900120629      * ?La PLATFORM LIST è usata come il Codice Cliente su RFF+FF:PLATFORMLIST
270000120629      ***
270100120629      * Se partner/cliente è abilitato alla funzione di sostituzione CLIENTE da UNB.
270200120629      *   Il (SHIPPED FROM) in testata funziona come un RFF+FF: a livello di testata.
270300120629      *    per agganciare i dati sulla tab."CL".
270400120629      ***
270500120629     C                   if         PZ_abilita_NAD_SF_come_RFF_FF ='S'
270600120629     c                                 and Tes_RFF_FF_ksc = 0
270700120629      * ?Ma solo se NON c'è un RFF+FF: di Testata che prevale su tutto.
270800120629      *
270900120629      *   imposta la PLATFORM LIST ricevuta del MITTENTE
271000120629      *    questo codice viene poi utilizzato sulla tab.CL (EDTAB) se concordato
271100120629      *    per codificare tutte le spedizioni come un RFF+FF:xxxxxxxxxxxxxxxxx di testata
271200120629     c                   eval      NAD_SF_Cliente = NAD3039
271300120629      *>>>>>>>>
271400120629     C*------------------
271500120629      *    Aggancia il Cliente e tutte le caratteristiche da tab.:"CL"
271600120629      * ?RFF+FF:
271700120629     C                   clear                   Rifer_Cliente    35
271800120629     C                   movel     NAD_SF_ClienteRifer_Cliente
271900120629     c                   exsr      CL_Particolare
272000120629     C*------------------
272100120629      *>>>>>>>>
272200120629     c                   end
272300120629      ***
272400100705      *-------
272500090210      * Dati destinatario
272600100705      *-------
272700100708     C                   elseIF    Quale_campo ='VABRSD'
272800100813     c                   move      'S'           dopo_NAD_CN
272900090227      *
273000090227      * Se trovato un indirizzo di destinatario
273100100701     c                   if           Dati_di_Testata = 'N'
273200100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
273300100701     c                   else
273400100701     c                   eval       NAD_destinatario_in_testata = 'S'
273500100701     c                   end
273600090210      *
273700090210     C* Reperisco nazione corrispondente destinatario
273800100705     C                   Z-ADD     1             naz               3 0
273900100705     C                   MOVEL     *BLANKS       VABNZD
274000100705     C                   Z-ADD     0             WFLCPF
274100090210     c                   eval      *in32 = *off
274200100705      ***
274300090210     C                   if        nad3207 <> *BLANKS
274400100705     C     nad3207       LOOKUP    ISO(naz)                               32
274500100705     C                   If        *in32 = *on
274600100705      ***
274700100705      * per reperire il CAP corretto
274800100705     C                   MOVEL     C15(naz)      VABNZD
274900100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
275000100705      ***
275100100705     c                   Endif
275200100705     C                   endif
275300100705      *
275400090210     C* Imposto dati destinatario
275500100705     C                   MOVEL     nad30361      VABRSD
275600100705     C                   if        nad30362 <> *LOVAL
275700100705     C                   MOVEL     nad30362      VABRD2
275800090210     C                   endif
275900100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
276000091019???? C* Imposto dati destinatario
276100091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
276200100705???? C                   MOVEL     nad31241      VABRSD
276300100705???? C                   if        nad31242 <> *LOVAL
276400100705???? C                   MOVEL     nad31242      VABRD2
276500091019???? C                   endif
276600091019???? C                   end
276700100705      *   città/Località
276800090210     C                   MOVEL     nad3164       VABLOD
276900100812      *
277000100812      *  Se richiesto in tabella PT di accodare alla Ragione sociale
277100100812      *  l'informazione di indirizzo invertita nel segmento...
277200100812      *  Esigenza nata da Dior che mette l'indirizzo vero nel 2°campo del Indirizzo NAD
277300100812      *  ossia dopo i 2 punti (:) ed invece mette un'informazione sul destinatario/
277400100812      *  indirizzo nel 1° campo dell'Inidirizzo NAD prima dei 2 punti (:).
277500100812      *   Per nostra comodità mettiamo quindi l'informazione aggiuntiva attaccata alla
277600100812      *   Ragione Sociale e non alla Località poichè altrimenti avremmo continui
277700100812      *   problemi di decodifica Località/Cappario durante la conferma bolle.
277800100812     c                   if        §pv2IN = 'S'
277900100812      *
278000100812     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
278100100812      *
278200100812      * Nell'indirizzo il 2°campo ricevuto
278300100812     C                   MOVEL     NAD30422      VABIND
278400100812      * Mentre appiccica all'estensione della ragione sociale le info di consegna
278500100812      *  messe nel 1° campo dell'indirizzo (DIOR)
278600100812      *
278700100812      *  Verifico se c'è spazio sul campo estensione altrimenti vado in coda
278800100812      *    prima all'indirizzo e poi alla località
278900100812      *
279000100812     C* Verifico se c'è dello spazio per metterlo nell'estensione rag.soc.
279100100812     C     '    '        SCAN      VABRD2        WI                4 0
279200100812     c                   if        WI < 10
279300100812     C     '    '        SCAN      VABrd2        WI                4 0
279400100812     C                   MOVEA     VABrd2        LOD
279500100812     C                   ADD       1             WI
279600100812     C                   MOVEA     NAD30421      LOD(WI)
279700100812     C                   MOVEA     LOD           VABrd2
279800100812      *
279900100812     c                   else
280000100812      *  Verifico se c'è spazio sull'indirizzo altrimenti vado in coda alla località
280100100812     C     '    '        SCAN      VABIND        WI                4 0
280200100812     c                   if        WI < 10
280300100812     C     '    '        SCAN      VABIND        WI                4 0
280400100812     C                   MOVEA     VABIND        LOD
280500100812     C                   ADD       1             WI
280600100812     C                   MOVEA     NAD30421      LOD(WI)
280700100812     C                   MOVEA     LOD           VABIND
280800100812      *
280900100812     c                   else
281000100812      *  oppure    accodato nella località
281100100812     C     '    '        SCAN      VABLOD        WI                4 0
281200100812     C                   MOVEA     VABLOD        LOD
281300100812     C                   ADD       1             WI
281400100812     C                   MOVEA     NAD30421      LOD(WI)
281500100812     C                   MOVEA     LOD           VABLOD
281600100812     c                   end
281700100812     c                   end
281800100812      *-----------
281900100812     c                   else
282000100812      *
282100100812     C                   MOVEL     NAD30421      VABIND
282200100812     c                   end
282300100812      *
282400100812      *-----------
282500100812      *  segue lo standard
282600100812     c                   else
282700100812      *-----------
282800100812      *
282900100812     C                   MOVEL     NAD30421      VABIND
283000100812      *
283100100812     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
283200100812     c                   if        NAD30422 <> *blanks  and NAD30422 <> *loval
283300100812      *
283400100812      *  Verifico se c'è spazio sul primo indirizzo x accodare se richiesto
283500100812      *  altrimenti vado in coda alla località
283600100812     C     '    '        SCAN      VABIND        WI                4 0
283700100812      *
283800100812     C* Verifico se c'è dello spazio per metterlo nella località
283900100812      *  oppure    accodato al primo indirizzo
284000100812      *
284100100812     c                   if        WI < 15
284200100812      *
284300100812     C     '    '        SCAN      VABind        WI                4 0
284400100812     C                   MOVEA     VABind        LOD
284500100812     C                   ADD       1             WI
284600100812     C                   MOVEA     NAD30422      LOD(WI)
284700100812     C                   MOVEA     LOD           VABind
284800100812      *
284900100812     c                   else
285000100812      *
285100100812     C     '    '        SCAN      VABLOD        WI                4 0
285200100812     C                   MOVEA     VABLOD        LOD
285300100812     C                   ADD       1             WI
285400100812     C                   MOVEA     NAD30422      LOD(WI)
285500100812     C                   MOVEA     LOD           VABLOD
285600100812      *
285700100812     c                   end
285800100812      *
285900100812     c                   end
286000100812      *
286100100812      *
286200100812     C                   ENDif
286300100812      * ---------
286400090210      * Converto CAP
286500100705     C                   EXSR      Converte_CAP
286600090603     C*
286700100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
286800100705     C*  record del VAB viene PULITO
286900100316     c                   if        Dati_di_Testata  = 'S'
287000090603     c                   eval       tes_VABrsd = VABrsd
287100090603     c                   eval       tes_VABrd2 = VABrd2
287200090603     c                   eval       tes_VABind = VABind
287300090603     c                   eval       tes_VABlod = VABlod
287400090603     c                   eval       tes_VABnzd = VABnzd
287500090603     c                   end
287600100728      *
287700100728      *   se NON c'è l'identificativo bolla del cliente
287800130315      *    blocca la spedizione e lo segnala.
287900100728     C                   if          vabRSD = *blank
288000100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
288100130315     c***********        exsr      Error_DET
288200130315     c***********        goto      end_NAD
288300100728     C                   elseIF      vabIND = *blank
288400100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
288500130315     c***********        exsr      Error_DET
288600130315     c***********        goto      end_NAD
288700100728     C                   elseIF      vabLOD = *blank
288800100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
288900130315     c***********        exsr      Error_DET
289000130315     c***********        goto      end_NAD
289100100728     C                   ENDIF
289200100705      *-------
289300090210      * Dati mittente
289400100705      *-------
289500100705     C                   elseIF     Quale_campo ='VABRMO'
289600090210      *
289700100705     C                   movel     nad30361      VABRMO
289800090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
289900090210     C                   movel     §PTNAZ        VABNMO
290000090210      *
290100090210     C                   IF        nad3207 <> *blanks
290200100705     C                   eval      naz = 1
290300100705     C     nad3207       LOOKUP    ISO(naz)                               32
290400090210     C                   if         *in32 = *on
290500100705     C                   MOVEL     C15(naz)      VABNMO
290600090210     C                   endif
290700090210     C                   ENDIF
290800090210      *
290900090210      * Normalizzo CAP mittente originale
291000090210     C                   clear                   T
291100090210     C                   clear                   S
291200090210     C                   MOVEA     *BLANKS       CAPM
291300090210     C                   MOVEA     nad3251       CAP9
291400090210     C                   DO        9             T
291500090210     C     CAP9(T)       IFNE      *BLANKS
291600090210     C                   ADD       1             S
291700090210     C                   MOVE      CAP9(T)       CAPM(S)
291800090210     C                   ENDIF
291900090210     C                   ENDDO
292000090210     C*
292100090210      *     Controllo validità CAP
292200090210     C                   MOVEA     CAPM          VABCMO
292300090210      *
292400090210     C                   CLEAR                   TISI95DS
292500090210     C                   MOVEL     VABCMO        I95CAP
292600090210     C                   MOVEL     VABNMO        I95NAR
292700090210     C                   MOVEL     WOGGI         I95DAT
292800090210     C                   MOVEL     '3'           I95TCN
292900090210     C                   CALL      'TISI95R'
293000090210     C                   PARM                    TISI95DS
293100090210      *     Errore
293200090210     C     O95ERR        IFNE      *BLANKS
293300090210     C                   MOVEL     INDCAE        VABCMO
293400090210     C                   MOVEL     §PTNAZ        VABNMO
293500090210     C                   END
293600090603      *
293700100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
293800100705     C*  record del VAB viene PULITO
293900100316     c                   if        Dati_di_Testata  = 'S'
294000090603     c                   eval      tes_VABrmo = VABrmo
294100090603     c                   eval      tes_VABnmo = VABnmo
294200090603     c                   eval      tes_VABcmo = VABcmo
294300090603     c                   eval      NAD_mittente_in_testata = 'S'
294400100701     c                   else
294500100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
294600090603     c                   end
294700100728      *
294800100728      *   se NON c'è l'identificativo bolla del cliente
294900100728      *    blocca la spedisione e lo segnala.
295000130315     C                   if          vabRMO = *blank
295100130315     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
295200130315     c************       exsr      Error_DET
295300130315     c************       goto      end_NAD
295400130315     c                   ENDIF
295500090210      *
295600090210     C                   END
295700090210      *
295800090210     c     end_NAD       endSR
295900090210      * ?================================================================== */
296000090210     C*?  Contatto a cui far riferimento
296100090210      * ?================================================================== */
296200090210     C     DATI_CTA      BEGSR
296300090210      *
296400090210      *  memorizza il riferimento di consegna x il destinatario
296500100813     C                   if         cta3412 <> *BLANKS and
296600100813     c                                    dopo_NAD_CN = 'S'
296700100813      *
296800100701     c                   if        Dati_di_Testata  ='S'
296900100812     c                   if           CTA_Destinatario_in_testata   = *blanks
297000100812     c                   eval       CTA_Destinatario_in_testata   = cta3412
297100100812     c                   end
297200100701     c                   else
297300100812     c                   if           CTA_Destinatario_in_dettaglio = *blanks
297400100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
297500100812     c                   end
297600100701     c                   end
297700100701     c                   end
297800090210      *
297900090210     c                   endSR
298000090210      * ?================================================================== */
298100090210     C*?  Contatto telefonico  a cui far riferimento
298200090210      * ?================================================================== */
298300090210     C     DATI_COM      BEGSR
298400090210      *
298500100705      *  memorizza il riferimento di consegna nr.telefonico
298600100813     C                   if         com3148 <> *BLANKS and
298700100813     c                                  COM3155 = 'TE' and
298800100813     c                                    dopo_NAD_CN = 'S'
298900100813      *
299000100701     c                   if        Dati_di_Testata  ='S'
299100100812     c                   if           COM_telefono_in_testata = *blanks
299200100812     c                   eval      COM_telefono_in_testata   = com3148
299300100812     c                   end
299400100701     c                   else
299500100812     c                   if           COM_telefono_in_dettaglio = *blanks
299600100701     c                   eval      COM_telefono_in_dettaglio = com3148
299700100812     c                   end
299800090210     c                   end
299900100705     c                   endif
300000090210      *
300100090209     C                   ENDSR
300200090210      * ?================================================================== */
300300090210     C*?  Dati di dettaglio della spedizione e dei colli
300400090210      * ?================================================================== */
300500090210     C     DATI_GID      BEGSR
300600090210      *
300700100910      *  Se non richiesto quello sulla testata dettaglio
300800100920     c                   if        §pvCOl  <> 'S' or vabPKB = 0
300900100910      *
301000090210     C* Se ho totali per spedizione metto in campi VAB
301100090210     C     gid1496       IFEQ      99999
301200090210     C     gid1496       orEQ      9999
301300100630     c                   eval      GID_aTotale = 'S'
301400090210     C                   z-add     gid722420     VABNCL
301500090210     C                   ELSE
301600100630     c                   If        GID_aTotale = 'N'
301700100705     C                   z-add     gid722420     GID_ColliInAdd
301800100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
301900090210     C                   END
302000090210     C                   END
302100090212      *
302200100701     C                   ADD       VABNCL        ToT_Colli        16 0
302300090210      *
302400100910     C                   END
302500100910      *
302600090210     c     end_GID       endSR
302700090210      * ?================================================================== */
302800090210     C*?  Dati di dettaglio misure spedizione e colli
302900090210      * ?================================================================== */
303000090210     C     DATI_MEA      BEGSR
303100090210      *
303200100910     C*  Se non si deve prendere il peso dal Totale (CNT)
303300100920     c                   if        §pvPEs <> 'S' or vabPKB = 0
303400090210     C*  Peso
303500100319     C     mea6311       IFEQ      Weight
303600100319     C     mea6411       ANDEQ     Kilograms
303700090210      *
303800090210     C*  Controllo se ho valore totale o parziale
303900090210      *   sempre il Lordo
304000100319???? C     mea6313       IFEQ      G_Weight
304100100319???? C     mea6313       oreq      Gross_Weight
304200100706???? C     mea6313       oreq      Gross_Weight_E
304300100630     C                   IF        GID_aTotale = 'S'
304400091019???? C                   move      mea6314       mea6314_2        18 2          Peso
304500110530???? C     mea6314_2     div       10000         VABPKB                         Peso
304600090210     C                   ELSE
304700091019???? C                   move      mea6314       mea6314_2                      Peso
304800110530???? C     mea6314_2     div       10000         Peso_inKG        12 3          Peso
304900110530???? C                   add       Peso_inKG     VABPKB                         Peso
305000090210     C                   END
305100091203     C                   END
305200090210      *
305300100319???? C     mea6313       IFEQ      G_Weight
305400100319???? C     mea6313       oreq      Gross_Weight
305500100706???? C     mea6313       oreq      Gross_Weight_E
305600110530???? C     mea6314_2     div       10000         Peso_inKG        12 3          Peso
305700110530???? C                   ADD       Peso_inKG     ToT_PesoLordo
305800090210     C                   END
305900100706???? C     mea6313       IFEQ      N_Weight
306000100706???? C     mea6313       oreq      Net_Weight
306100100706???? C     mea6313       oreq      Gross_Weight_E
306200110530???? C     mea6314_2     div       10000         Peso_inKG        12 3          Peso
306300110530???? C                   ADD       Peso_inKG     ToT_PesoNetto
306400090210     C                   END
306500090210     C                   END
306600090210      ***
306700090210     C*  Peso in Grammi  se abilitato a utilizzarlo
306800090210      ***
306900100319     C     mea6311       IFEQ      Weight
307000100319     C     mea6411       ANDEQ     Grams
307100090210      *
307200090210     C*  Controllo se ho valore totale o parziale
307300100319???? C     mea6313       IFEQ      G_Weight
307400100319???? C     mea6313       oreq      Gross_Weight
307500100706???? C     mea6313       oreq      Gross_Weight_E
307600100630     c                   If        GID_aTotale = 'S'
307700091019???? c                   move      mea6314       mea6314_2
307800091019???? C     mea6314_2     div       1000          VABPKB                         Peso
307900090210     C                   ELSE
308000091019???? c                   move      mea6314       mea6314_2
308100091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
308200091019???? C                   add       Peso_inKG     VABPKB                         Peso
308300090210     C                   END
308400090210     C                   END
308500090210      *
308600100319???? C     mea6313       IFEQ      G_Weight
308700100319???? C     mea6313       oreq      Gross_Weight
308800100706???? C     mea6313       oreq      Gross_Weight_E
308900091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
309000100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
309100090210     C                   END
309200100706???? C     mea6313       IFEQ      N_Weight
309300100706???? C     mea6313       oreq      Net_Weight
309400100706???? C     mea6313       oreq      Gross_Weight_E
309500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
309600100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
309700090210     C                   END
309800090210     C                   END
309900100910      *
310000100910     c                   end
310100100910      * -------------
310200090210      *  Volume
310300100319???? C     mea6311       IFEQ      Volume
310400100319???? C     mea6411       ANDEQ     Cubic_Meters
310500090210      *  Controllo se ho valore totale o parziale
310600100630     c                   If        GID_aTotale = 'S'
310700110530???? C                   move      mea6314       mea6314_3        18 3          Volume
310800110530???? C     mea6314_3     div       1000          VABVLB                         Volume
310900110530     C                   ELSE                                                   Volume
311000110530???? C                   move      mea6314       mea6314_3                      Volume
311100110530???? C     mea6314_3     div       1000          Volm_inMQ        12 3          Volume
311200110530???? C                   add       Volm_inMQ     VABVLB                         Volume
311300090210     C                   END
311400090210     C                   END
311500100705      *
311600100910      * -------------
311700100705     C* Ricerco CAP in base alle dimensioni
311800090210     C     WCAP          IFNE      *BLANKS
311900100319     C     mea6311       IFEQ      Weight
312000090210      *
312100090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
312200090210     C                   CLEAR                   FNLV55DS
312300090210     C                   MOVEL     'P'           D55TPT
312400090210     C                   MOVEL     WOGGI         D55DRF
312500090210     C                   MOVEL     VABLNP        D55LNP
312600090210     C                   MOVEL     VABLNP        D55LIN
312700090210     C                   CALL      'FNLV55R'
312800090210     C                   PARM                    FNLV55DS
312900090210     C                   MOVE      D55TFP        COMTFP            3 0
313000090210      *
313100090210     C*  PASSO IL TERMINAL PARTENZA
313200090210     C                   CLEAR                   TISI95DS
313300090210     C                   Z-ADD     COMTFP        I95TFP
313400090210     C                   MOVEL     VABTSP        I95TSP
313500090210     C                   MOVEL     'S'           I95FRE
313600090210     C                   MOVEL     '3'           I95TCN
313700090210     C                   MOVEL     WCAP          I95CAP
313800090210     C                   MOVEL     VABNZD        I95NAR
313900090210     C                   MOVEL     VABLOD        I95LOC
314000090210     C                   MOVEL     VABIND        I95IND
314100090210      *
314200090210     C* Se gestita seconda linea di arrivo passo peso - volume
314300090210     C* e numero colli effettivo
314400090210     C     §PULNA        IFEQ      'S'
314500090210     C                   Z-ADD     VABPKB        I95LKG
314600090210     C                   Z-ADD     VABVLB        I95LMC
314700090210     C                   ELSE
314800100706     C*******
314900090210     C* Altrmenti passo 1 Kg e 0,001
315000100706     C*******              Z-ADD1                  I95LKG
315100100706     C*******              Z-ADD0,001              I95LMC
315200100706     C*******
315300090210     C                   END
315400090210     C*
315500090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
315600090210     C                   SELECT
315700090210     C     VABCBO        WHENEQ    '1 '
315800090210     C                   MOVEL     *BLANKS       I95FCA
315900090210     C                   MOVEL     'F'           I95TPO
316000090210     C     VABCBO        WHENEQ    '2 '
316100090210     C                   MOVEL     *BLANKS       I95FCA
316200090210     C                   MOVEL     'A'           I95TPO
316300090210     C     VABCBO        WHENEQ    '4 '
316400090210     C                   MOVEL     'S'           I95FCA
316500090210     C                   MOVEL     'F'           I95TPO
316600090210     C     VABCBO        WHENEQ    '6 '
316700090210     C                   MOVEL     'S'           I95FCA
316800090210     C                   MOVEL     'A'           I95TPO
316900090210     C                   OTHER
317000090210     C                   MOVEL     'F'           I95TPO
317100090210     C     VABCAS        IFGT      0
317200090210     C                   MOVEL     'S'           I95FCA
317300090210     C                   ELSE
317400090210     C                   MOVEL     *BLANKS       I95FCA
317500090210     C                   END
317600090210     C                   ENDSL
317700090210     C*
317800090210     C                   CALL      'TISI95R'
317900090210     C                   PARM                    TISI95DS
318000090210     C* Reperisco i dati da CAP
318100110613     C                   move      'S'           Instradamento
318200090210     C                   MOVEL     O95PRV        VABPRD
318300100706     C                   MOVEL     WCAP          VABCAD
318400100114      ***
318500100706     c                   MOVEL     O95LNA        VABLNA
318600100706     C                   MOVEL     O95ZNC        VABZNC
318700090210     C*
318800090210     C     O95ERR        IFNE      *BLANKS
318900090210     C     O95FIT        ANDEQ     'S'
319000090210     C*  Cap non trovato
319100090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
319200110224     c********           exsr      Error_DET
319300110224     C********           goto      end_MEA
319400090210     C                   END
319500090210     C                   END
319600090210      *
319700090210     C                   ELSE
319800090210      *
319900090210     C                   CLEAR                   VABPRD
320000090210     C                   CLEAR                   VABLNA
320100090210     C                   CLEAR                   VABZNC
320200090210     C                   CLEAR                   VABCAD
320300090210     C*  Cap non trovato
320400090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
320500110224     c*********          exsr      Error_DET
320600110224     C*********          goto      end_MEA
320700090210     C                   END
320800090210      *
320900090210     c     end_MEA       endSR
321000090210      * ?================================================================== */
321100090210     C*?  Dati di dettaglio Codice a barre Segnacolli
321200090210      * ?================================================================== */
321300090210     C     DATI_PCI      BEGSR
321400090210      *
321500090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
321600090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
321700090210     C     §1BF12        IFEQ      'E'
321800100706     C                   EXSR      SEGNaCollo_EEX
321900090210     C                   ELSE
322000090210      *
322100090210      *  La prima volta controllo congruità numero segnacollo
322200090210     C     §PUBS1        IFNE      *BLANKS
322300090210     C     VABnrs        ANDNE     0
322400090210      *  Se il codice trattamento merce prevede che segnacollino i
322500090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
322600090210     C     §1BFG7        IFEQ      'S'
322700090210     C     §1BFG1        OREQ      'S'
322800090210      *  calcolo segnacollo
322900100706     C                   EXSR      Segn_Bartolini
323000090210     C                   END
323100090210     C                   END
323200090210      *
323300090210     C                   END
323400090210      *
323500090210     c     end_PCI       endSR
323600090211     C*----------------------------------------------------------------
323700090211      *? dati finali del dettaglio    UNT
323800090211     C*----------------------------------------------------------------
323900090211     C     DATI_UNT      BEGSR
324000090211      *
324100110324      *  Deve saltare il controllo e non dare il messaggio
324200110324     c                   goto      No_UNTctrl
324300110324      *
324400100708     c                   eval      UNT_record = vnREC
324500090213     c                   z-add     vnrec         last_RECord
324600090211      **
324700100319      **   verifica la quadratura di tutti i segmenti ricevuti
324800100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
324900100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
325000100728     C     UNT0074       IFne      Conta_segmenti
325100100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
325200100319     c                             trollare il Messaggio ricevuto'
325300100319      *
325400100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
325500100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
325600100921     C                   EVAL      Messaggio = 'EDI -
325700100921     C                             IFCSUM D96A - msg su UPLOAD -
325800100921     C                             con quadratura UNT errata verificare -
325900100921     C                             x il cliente :'
326000100319     C                   EVAL      Messaggio = %trim(Messaggio) +
326100100319     C                                         %editc(§PTksc:'X')
326200100319     C                   EVAL      Messaggio = %trim(Messaggio) +
326300100319     C                             ' i segmenti contati sono :' +
326400100319     C                             %editc(Conta_segmenti:'Z')
326500100319     c                   if        §PVinv <> 'N'
326600100319     c                   exsr      invio_mail
326700100319     c                   end
326800100319     c                   end
326900110321     c     No_UNTctrl    tag
327000100319      **
327100100319     c     End_UNT       endSR
327200100316     C*----------------------------------------------------------------
327300100316      *? dati finali     UNT
327400100316     C*----------------------------------------------------------------
327500100316     C     DATI_UNZ      BEGSR
327600100316      *
327700100726     c                   move      'S'           Forza_Uscita
327800100316      **
327900100316     c                   endSR
328000090210      *----------------------------------------------------------------
328100090210      *? Input segnacolli su schiera di 20 elementi
328200090210      *----------------------------------------------------------------
328300090210     C     INP_Segnacol  BEGSR
328400090210      *
328500090210     c                   clear                   quanti_PCI        3 0
328600090210     c                   if        §puEL1 = 0
328700090210     c                   z-add     10            quanti_PCI
328800090210     c                   else
328900090210     c                   z-add     §puEL1        quanti_PCI
329000090210     c                   end
329100090210      *
329200090210      *  Pulisce la schiera dei segnacolli da elaborare
329300090210     c                   clear                   x30
329400090210     c                   z-add     0             Nr_x30            3 0
329500090210      *
329600090210      *   riporto tutto sulla schiera generica X30
329700090210      *
329800090210     c                   do        10            limite            3 0
329900090210      * Limita quanti elementi passati in ogni PCI
330000090210      *   devono essere considerati
330100090210     c                   if        limite > quanti_PCI
330200090210     c                   Leave
330300090210     c                   end
330400090210      *
330500100920     c                   IF        tipo_seg = 'GIN'
330600100920      *  Carica la schiera generale di tutti i segnacolli
330700100920     c                   if        GIN_10(limite) <> *blank
330800100920     c                   add       1             Nr_x30
330900160621      *
331000160621      * elimina gli zeri a sinistra del numero presente sul Barcode
331100160621     c                   movel     GIN_10(limite)alfa35           35
331200160621     c                   do        34            zr                3 0
331300160621     c                   if        %subst(alfa35:1:1)<>'0'
331400160621     c                   leave
331500160621     c                   else
331600160621     c                   eval      alfa35 = %subst(alfa35:2:34) + ' '
331700160621     c                   end
331800160621     c                   enddo
331900160621      *
332000160621     c                   eval      GIN_10(limite) = alfa35
332100100920     c                   eval      X30(Nr_x30) = GIN_10(limite)
332200100920     c                   end
332300100920      *
332400100920     c                   elseIF    tipo_seg = 'PCI'
332500090210      *  Carica la schiera generale di tutti i segnacolli
332600100920     c                   if        PCI_10(limite) <> *blank
332700090210     c                   add       1             Nr_x30
332800100920     c                   eval      X30(Nr_x30) = PCI_10(limite)
332900090210     c                   end
333000100920      *
333100100920     c                   endIF
333200100920      *
333300090210     c                   enddo
333400090210      *
333500090210     C                   ENDSR
333600090210      *----------------------------------------------------------------
333700090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
333800090210      *----------------------------------------------------------------
333900100706     C     SEGNaCollo_EEXBEGSR
334000090210      *
334100090210     c                   exsr      INP_Segnacol
334200090210      *
334300090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
334400090210     C                   DO        10            x                              --- 01 ---
334500090210      *
334600090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
334700090210     C     x30(X)        ANDNE     *LOVAL
334800090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
334900100701     C                   ADD       1             totale_PCI
335000100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
335100090210     C                   END                                                    --- 03 ---
335200090210      *
335300090210     C                   END                                                    --- 02 ---
335400090210      *
335500090210     C                   ENDSR
335600090210     C*----------------------------------------------------------------
335700090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
335800090210     C*----------------------------------------------------------------
335900100706     C     Segn_BartoliniBEGSR
336000090210      *
336100090210     c                   exsr      INP_Segnacol
336200090210     C*
336300090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
336400090210     C*  in base al numero elementi validi carico sgn.
336500090210     C                   DO        10            X                              --- 01 ---
336600090210     C*
336700090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
336800090210     C     x30(X)        ANDNE     *LOVAL
336900090210     C*
337000100630     C                   CLEAR                   DS_SegnBART
337100090210     C*  Controllo se devo ricevere la linea di partenza
337200090210     C                   MOVEL     'N'           WERRO             1
337300090210     C     §PULP1        IFGT      0                                            --- 03 ---
337400100706     C                   EXSR      REPerisce_LNP
337500090210     C                   END                                                    --- 03 ---
337600100706      *
337700090210     C*  Controllo se devo ricevere la linea di arrivo
337800090210     C     §PULA1        IFGT      0                                            --- 03 ---
337900090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
338000100706     C                   EXSR      REPerisce_LNA
338100090210     C                   END                                                    --- 03 ---
338200100706      *
338300090210     C*  Controllo se devo ricevere il numero di serie
338400090210     C     §PUNR1        IFGT      0                                            --- 03 ---
338500090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
338600100706     C                   EXSR      REPerisce_NRS
338700090210     C                   END                                                    --- 03 ---
338800100706      *
338900090210     C*  Controllo se devo ricevere il numero segnacollo
339000090210     C     §PUSG1        IFGT      0                                            --- 03 ---
339100090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
339200100706     C                   EXSR      REPerisce_SGN
339300090210     C                   END                                                    --- 03 ---
339400100706      *
339500090210     C*  Controllo se devo ricevere la zona di consegna
339600090210     C     §PUZN1        IFGT      0                                            --- 03 ---
339700090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
339800100706     C                   EXSR      REPerisce_ZNC
339900090210     C                   END                                                    --- 03 ---
340000100706      *
340100090210     C*  Imposto da VAB i dati a zero se mi è stato passato
340200090210     C*  numero segnacollo valido
340300090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
340400100706      *
340500100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
340600100630     C                   Z-ADD     VABLNP        SegnBART_LNP
340700090210     C                   END                                                    --- 04 ---
340800100706      *
340900100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
341000100630     C                   Z-ADD     VABLNA        SegnBART_LNA
341100090210     C                   END                                                    --- 04 ---
341200100706      *
341300100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
341400100630     C                   Z-ADD     VABNRS        SegnBART_NRS
341500090210     C                   END                                                    --- 04 ---
341600100706      *
341700100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
341800100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
341900090210     C                   END                                                    --- 04 ---
342000090210      *
342100090210     C*  mi reimposto LNA e zona da segnacollo
342200100114      ***
342300100114      *** dovendo accorpare le spedizioni da Conferma x CMR
342400100114      ***  non deve impostare l'instradamento.
342500100630     C                   Z-ADD     SegnBART_LNA  VABLNA
342600100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
342700100114      ***
342800090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
342900100701     C                   ADD       1             Conta_Segn
343000100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
343100090210     C                   END                                                    --- 03 ---
343200090210     C*
343300090210     C                   END                                                    --- 02 ---
343400090210     C*
343500090210     C                   END                                                    --- 01 ---
343600090210      *
343700090210     C                   ENDSR
343800090210     C*----------------------------------------------------------------
343900090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
344000090210     C*----------------------------------------------------------------
344100100706     C     REPerisce_LNP BEGSR
344200090210     C*
344300090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
344400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
344500090210     C*  dal numero segnacollo passatomi dal partner
344600090210     C                   Z-ADD     §PULP1        WP                2 0
344700090210     C     3             SUBST     x30(X):WP     WLNP              3
344800090210     C*
344900090210     C*  controllo che la lnp calcolata sia numerica
345000090210     C                   TESTN                   WLNP                 98
345100090210     C*
345200090210     C*  se è numerica la carico
345300090210     C     *IN98         IFEQ      '1'
345400100630     C                   MOVE      WLNP          SegnBART_LNP
345500090210     C                   ELSE
345600090210     C*  se non è numerica stampo errore
345700090210     C                   MOVEL     'S'           WERRO             1
345800090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
345900090213     c                   exsr      Error_DET
346000090210     C                   goto      end_LNP
346100090210     C                   END
346200090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
346300090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
346400100630     C     SegnBART_LNP  IFNE      VABLNP
346500090210     C                   MOVEL     'S'           WERRO             1
346600090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
346700090210     c                             a della Bolla'
346800090213     c                   exsr      Error_DET
346900090210     C                   goto      end_LNP
347000090210     C                   END
347100090210     C*
347200090210     C     end_LNP       ENDSR
347300090210     C*----------------------------------------------------------------
347400090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
347500090210     C*----------------------------------------------------------------
347600100706     C     REPerisce_LNA BEGSR
347700090210     C*
347800090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
347900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
348000090210     C*  dal numero segnacollo passatomi dal partner
348100090210     C                   Z-ADD     §PULA1        WP                2 0
348200090210     C     3             SUBST     x30(X):WP     WLNA              3
348300090210     C*  controllo che la lna calcolata sia numerica
348400090210     C                   TESTN                   WLNA                 98
348500090210     C*  se è numerica la carico
348600090210     C     *IN98         IFEQ      '1'
348700100630     C                   MOVE      WLNA          SegnBART_LNA
348800090210     C                   ELSE
348900090210     C*  se non è numerica stampo errore
349000090210     C                   MOVEL     'S'           WERRO             1
349100090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
349200090213     c                   exsr      Error_DET
349300090210     C                   goto      end_LNA
349400090210     C                   END
349500090210     C*
349600090210     C     end_LNA       ENDSR
349700090210     C*----------------------------------------------------------------
349800090210     C*? REPNRS - Reperisce numero serie
349900090210     C*----------------------------------------------------------------
350000100706     C     REPerisce_NRS BEGSR
350100090210     C*
350200090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
350300090210     C*  (a partire dalla posizione iniziale indicata in tabella)
350400090210     C*  dal numero segnacollo passatomi dal partner
350500090210     C                   Z-ADD     §PUNR1        WP                2 0
350600090210     C     2             SUBST     x30(X):WP     WNRS              2
350700090210     C*  controllo che il nrs calcolata sia numerico
350800090210     C                   TESTN                   WNRS                 98
350900090210     C*  se è numerica la carico
351000090210     C     *IN98         IFEQ      '1'
351100100630     C                   MOVE      WNRS          SegnBART_NRS
351200090210     C                   ELSE
351300090210     C*  se non è numerico stampo errore
351400090210     C                   MOVEL     'S'           WERRO             1
351500090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
351600090213     c                   exsr      Error_DET
351700090210     C                   goto      end_NRS
351800090210     C                   END
351900090210      *
352000090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
352100090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
352200100630     C     SegnBART_NRS  IFNE      VABNRS
352300090210     C                   MOVEL     'S'           WERRO             1
352400090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
352500090210     c                             o della Bolla'
352600090213     c                   exsr      Error_DET
352700090210     C                   goto      end_NRS
352800090210     C                   END
352900090210     C*
353000090210     C     end_NRS       ENDSR
353100090210     C*----------------------------------------------------------------
353200090210     C*? REPZNC - Reperisce zona consegna
353300090210     C*----------------------------------------------------------------
353400100706     C     REPerisce_ZNC BEGSR
353500090210     C*
353600090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
353700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
353800090210     C*  dal numero segnacollo passatomi dal partner
353900090210     C                   Z-ADD     §PUZN1        WP                2 0
354000090210     C     2             SUBST     x30(X):WP     WZNC              2
354100090210     C*  controllo che la zona calcolata sia numerica
354200090210     C                   TESTN                   WZNC                 98
354300090210     C*  se è numerica la carico
354400090210     C     *IN98         IFEQ      '1'
354500100630     C                   MOVE      WZNC          SegnBART_ZNC
354600090210     C                   ELSE
354700090210     C*  se non è numerica stampo errore
354800090210     C                   MOVEL     'S'           WERRO             1
354900090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
355000090213     c                   exsr      Error_DET
355100090210     C                   goto      end_ZNC
355200090210     C                   END
355300090210     C*
355400090210     C     end_ZNC       ENDSR
355500090210     C*----------------------------------------------------------------
355600090210     C*? REPSGN - Reperisce numero segnacollo
355700090210     C*----------------------------------------------------------------
355800100706     C     REPerisce_SGN BEGSR
355900090210     C*
356000090210     C*  se viene passata nr.segnacollo estraggo sottostringa
356100090210     C*  lunga 7 chr (a partire dalla posizione iniziale
356200090210     C*  indicata in tabella)
356300090210     C*  dal numero segnacollo passatomi dal partner
356400090210     C                   Z-ADD     §PUSG1        WP                2 0
356500090210     C     7             SUBST     x30(X):WP     WSGN              7
356600090210     C*  controllo che il nr.segnacollo calcolato sia numerico
356700090210     C                   TESTN                   WSGN                 98
356800090210     C*  se è numerico lo carico
356900090210     C     *IN98         IFEQ      '1'
357000100630     C                   MOVE      WSGN          SegnBART_NSC
357100090210     C                   ELSE
357200090210     C*  se non è numerica stampo errore
357300090210     C                   MOVEL     'S'           WERRO             1
357400100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
357500090213     c                   exsr      Error_DET
357600090210     C                   goto      end_SGN
357700090210     C                   END
357800090210     C*
357900090210     C     end_SGN       ENDSR
358000051205      *----------------------------------------------------*
358100051205      *   DEFINIZIONE CHIAVI                               *
358200051205      *----------------------------------------------------*
358300051205     C     *INZSR        BEGSR
358400051205      *
358500991129     c     *ENTRY        PLIST
358600060613     C                   parm                    esito             1
358700971215      *
358800050112     C     KTABel        KLIST
358900050112     C                   KFLD                    TBLKUT
359000050112     C                   KFLD                    TBLCOD
359100050112     C                   KFLD                    TBLKEY
359200090210      *
359300090210     C     K_TAB1L       KLIST
359400090213     C                   KFLD                    etbUNB
359500090213     C                   KFLD                    etbSGM
359600090213     C                   KFLD                    etbVALSGM
359700090209     C*
359800090209     C* Definisco chiavi di accesso
359900090209     C     KTAB1         KLIST
360000090209     C                   KFLD                    KKUT
360100090209     C                   KFLD                    KCOD
360200090209     C*
360300090209     C     KNUF          KLIST
360400090209     C                   KFLD                    KAAA
360500090209     C                   KFLD                    KCNU
360600090209     C                   KFLD                    KFIL
360700090209      *
360800090209     C     KVAB1         KLIST
360900090209     C                   KFLD                    KCCM
361000090209     C                   KFLD                    KCMR
361100090209     C                   KFLD                    KCNT
361200090209     C                   KFLD                    KAAS
361300090209     C                   KFLD                    KLNP
361400090209     C                   KFLD                    KNRS
361500090209     C                   KFLD                    KNSP
361600090209      *
361700090209     C     KVAB2         KLIST
361800090209     C                   KFLD                    KCCM
361900090209     C                   KFLD                    KCMR
362000090209      *
362100090209     C     KRDE          KLIST
362200090209     C                   KFLD                    KAAS
362300090209     C                   KFLD                    KLNP1
362400090209     C                   KFLD                    KNRS
362500090209     C                   KFLD                    KNSP
362600090209     C                   KFLD                    KNMC
362700090209     C                   KFLD                    KNRP
362800090209      *
362900090209     C     KACO          KLIST
363000090209     C                   KFLD                    KKUT
363100090209     C                   KFLD                    KKCC
363200090209     C                   KFLD                    KKSC
363300090209      *
363400090209     C     KIND          KLIST
363500090209     C                   KFLD                    KKUT
363600090209     C                   KFLD                    KKCC
363700090209     C                   KFLD                    KKSC
363800111028      *
363900111028     C     KTAS          KLIST
364000111028     C                   KFLD                    TASAAS
364100111028     C                   KFLD                    TASLNP
364200111028     C                   KFLD                    TASNRS
364300111028     C                   KFLD                    TASNSP
364400090209      *
364500090209      * DETERMINO SE SONO BARTOLINI O SDI
364600090209     C                   CLEAR                   TIBS55DS
364700090209     C                   MOVEL     'L'           I50TLA
364800090209     C                   CALL      'TIBS55R'
364900090209     C                   PARM                    TIBS55DS
365000090209      *
365100090209     C* RECUPERO DATI DELL'UTENTE
365200090209     C                   Z-ADD     1             CODUT             1 0
365300090209     C                   CALL      'XPARUT'
365400090209     C                   PARM                    UTEDSE0F
365500090209     C                   MOVEL     RAGUT         RSUT             20
365600090209     C*
365700090209     C* RICERCA CAPOCONTI
365800090209     C                   DO        50            X
365900090209     C                   MOVE      TCU(X)        TCUDS
366000090209     C     F56           IFEQ      'CG'
366100090209     C     F34           ANDEQ     '01'
366200090209     C                   Z-ADD     KCU(X)        KCI               4 0
366300090209     C                   END
366400090209     C                   END
366500090209     C*
366600090211     c                   movel(p)  'EDPAB'       User_Msg         10
366700090211     C*
366800090209     C* IMPOSTO A ZERO VARIABILI DI PGM
366900090209     c                   move      *blank        Snd_Msg_alert     1
367000090209     C                   MOVEL     CA(1)         WCA              78
367100090209     C                   MOVEL     CN(1)         WCN              78
367200090209     C*
367300090209     C* RECUPERO DATA E ORA
367400100630     C                   TIME                    Ora_Data         14 0
367500100630     C                   MOVE      Ora_Data      G02DAT
367600100630     C                   MOVE      Ora_Data      WDATA8            8 0
367700100630     C                   MOVE      WDATA8        Anno2             2 0
367800090209     C                   MOVEL     WDATA8        DATA              6 0
367900100630     C                   MOVE      Anno2         DATA
368000090209     C                   MOVEL     *BLANK        G02ERR
368100090209     C                   CALL      'XSRDA8'
368200090209     C                   PARM                    WLBDA8
368300090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
368400100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
368500090209     C                   TIME                    WORA              6 0
368600090209      * Recupero data e ora
368700090209     C                   TIME                    W0140
368800090209      * UDATE IN GGMMAAAA
368900100630     C                   MOVE      W0140         DataGGMMAAA
369000090209      * UDATE IN AAAAMMGG
369100100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
369200090209     C     *iso          MOVEL     DATA_eur      dateu
369300090209      *
369400090209      * ?              /*--------------------------- */
369500090209     c                   exsr      CARICA_TABELLE
369600090209      * ?              /*--------------------------- */
369700090209      *
369800090209     C                   MOVEL     *ALL'-'       CMP132          132
369900991124      *
370000050414      *------------------
370100991124     C                   ENDSR
370200060621      * ?------------------------------------------------------------------ */
370300090209      *?    CARICA TABELLE SU SCHIERE
370400060621      * ?------------------------------------------------------------------ */
370500090209     C     CARICA_TABELLEBEGSR
370600090205      *
370700090209     C* Caricamento Tabella 1B
370800090209     C                   Z-ADD     0             X                 4 0
370900090209     C                   Z-ADD     CODUT         KKUT
371000090209     C                   MOVEL     '1B'          KCOD
371100090209     C     KTAB1         CHAIN     TABEL00F                           30
371200090209     C     *IN30         DOWEQ     '0'
371300090209     C     TBLFLG        IFEQ      *BLANKS
371400090209     C                   ADD       1             X
371500100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
371600100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
371700090209     C                   END
371800090209     C     KTAB1         READE     TABEL00F                               30
371900090209     C                   END
372000090209      *
372100090209     C* Caricamento Tabella 15
372200090209     C                   Z-ADD     0             X                 4 0
372300090209     C                   Z-ADD     CODUT         KKUT
372400090209     C                   MOVEL     '15'          KCOD
372500090209     C     KTAB1         CHAIN     TABEL00F                           30
372600090209     C     *IN30         DOWEQ     '0'
372700090209     C     TBLFLG        IFEQ      *BLANKS
372800090209     C                   ADD       1             X
372900090209     C                   MOVEL     TBLKEY        C15(X)
373000090209     C                   MOVEL     TBLUNI        DS15
373100090209     C                   MOVEL     §15COD        ISO(X)
373200090209     C                   MOVEL     §15ECF        CPF(X)
373300090209     C                   MOVE      §15PCF        CPF(X)
373400090209     C                   END
373500090209     C     KTAB1         READE     TABEL00F                               30
373600090209     C                   END
373700090209      *
373800090209     C* Caricamento Tabella CV
373900090209     C                   Z-ADD     0             X
374000090209     C                   Z-ADD     CODUT         KKUT
374100090209     C                   MOVEL     'CV'          KCOD
374200090209     C     KTAB1         CHAIN     TABEL00F                           30
374300090209     C     *IN30         DOWEQ     '0'
374400090209     C     TBLFLG        IFEQ      *BLANKS
374500090209     C                   ADD       1             X
374600090209     C                   MOVEL     TBLKEY        CCV(X)
374700090209     C                   MOVEL     TBLUNI        DCV(X)
374800090209     C                   END
374900090209     C     KTAB1         READE     TABEL00F                               30
375000090209     C                   END
375100090216      *
375200100702     C* Caricamento Tabella Priorità trasporto
375300100702     C                   Z-ADD     0             X
375400100702     C                   MOVEL     'TS'          TABCOD
375500100702     C     TABCOD        CHAIN     EDTAB01L                           30
375600100702     C     *IN30         DOWEQ     '0'
375700100702     C     TABFLG        IFEQ      *BLANKS
375800100702     C                   ADD       1             X
375900100702     C                   eval      TipoServizio(X) = TABUNI
376000100705     C                   eval      Key_TipServ(X)  = TABKEY
376100100702     C                   END
376200100702     C     TABCOD        READE     EDTAB01L                               30
376300100702     C                   END
376400110328      *
376500110328      * Caricamento Tabella termini consegna
376600110328     C                   Z-ADD     0             X
376700110328     C                   MOVEL     'TB'          TABCOD
376800110328     C     TABCOD        CHAIN     EDTAB01L                           30
376900110328     C     *IN30         DOWEQ     '0'
377000110328     C     TABFLG        IFEQ      *BLANKS
377100110328     C                   ADD       1             X
377200110328     C                   eval      K35_TpBolla(X) = TABKEY
377300110328     C                   eval      Cod_TpBolla(X) = TABKEY
377400110328     C                   eval      Decod_Porto(X) = TABUNI
377500110328     C                   END
377600110328     C     TABCOD        READE     EDTAB01L                               30
377700110328     C                   END
377800100702      *
377900090216     C* Caricamento Tabella Tipo Incasso C/Assegno
378000090216     C                   Z-ADD     0             X
378100090216     C                   MOVEL     'TC'          TABCOD
378200090216     C     TABCOD        CHAIN     EDTAB01L                           30
378300090216     C     *IN30         DOWEQ     '0'
378400090216     C     TABFLG        IFEQ      *BLANKS
378500090216     C                   ADD       1             X
378600100705     C                   eval      K35_TpIncas(X) = TABKEY
378700100705     C                   eval      TipoIncasso(X) = TABUNI
378800090216     C                   END
378900090216     C     TABCOD        READE     EDTAB01L                               30
379000090216     C                   END
379100090209      *
379200090209      * Caricamento Tabella Partner esteri
379300090209     C                   Z-ADD     0             X
379400090209     C                   MOVEL     'PT'          TABCOD
379500090209     C     TABCOD        CHAIN     EDTAB01L                           30
379600090209     C     *IN30         DOWEQ     '0'
379700090209      *
379800090209     C                   SELECT
379900090209     C     TABFLG        WHENNE    *BLANKS
380000090209      *
380100090209     C                   OTHER
380200090209     C                   ADD       1             X
380300100630     C                   MOVEL     TABKEY        PT_key(X)
380400100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
380500100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
380600100630     C                   MOVEL     TABUNI        PT_uni(X)
380700090209     C                   ENDSL
380800090209      *
380900090209     C     TABCOD        READE     EDTAB01L                               30
381000090209     C                   END
381100121105      *
381200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
381300121105     c                   exsr      ChecK_PT
381400121105      *
381500090209      * salva quanti Codici UNB ha caricato
381600100630     c                   z-add     x             PT_KeyXsav
381700090209      *
381800090209     C                   MOVEL     'PU'          TABCOD
381900090209     C     TABCOD        CHAIN     EDTAB01L                           30
382000090209     C     *IN30         DOWEQ     '0'
382100090209      *
382200090209     C                   SELECT
382300090209     C     TABFLG        WHENNE    *BLANKS
382400090209      *
382500090209     C                   OTHER
382600100630     C                   MOVEL     TABKEY        PU_key
382700090209     C                   Z-ADD     1             X
382800100630     C     PU_key        LOOKUP    PT_key(X)                              32
382900100630     C   32              MOVEL     TABUNI        PU_uni(X)
383000090209     C                   ENDSL
383100090209      *
383200090209     C     TABCOD        READE     EDTAB01L                               30
383300090209     C                   END
383400090209      *
383500090209      * Prosegue tab.PT e PU
383600090209     C                   MOVEL     'PV'          TABCOD
383700090209     C     TABCOD        CHAIN     EDTAB01L                           30
383800090209     C     *IN30         DOWEQ     '0'
383900090209      *
384000090209     C                   SELECT
384100090209     C     TABFLG        WHENNE    *BLANKS
384200090209      *
384300090209     C                   OTHER
384400100630     C                   MOVEL     TABKEY        PV_key
384500090209     C                   Z-ADD     1             X
384600100630     C     PV_key        LOOKUP    PT_key(X)                              32
384700100630     C   32              MOVEL     TABUNI        PV_uni(X)
384800090209     C                   ENDSL
384900090209      *
385000090209     C     TABCOD        READE     EDTAB01L                               30
385100090209     C                   END
385200160209      *
385300160209      * Prosegue tab.PT e PU e PV e PS
385400160209     C                   MOVEL     'PS'          TABCOD
385500160209     C     TABCOD        CHAIN     EDTAB01L                           30
385600160209     C     *IN30         DOWEQ     '0'
385700160209      *
385800160209     C                   SELECT
385900160209     C     TABFLG        WHENNE    *BLANKS
386000160209      *
386100160209     C                   OTHER
386200160209     C                   MOVEL     TABKEY        PS_key
386300160209     C                   Z-ADD     1             X
386400160209     C     PS_key        LOOKUP    PT_key(X)                              32
386500160209     C   32              MOVEL     TABUNI        PS_uni(X)
386600160209     C                   ENDSL
386700160209      *
386800160209     C     TABCOD        READE     EDTAB01L                               30
386900160209     C                   END
387000090209      *
387100120629      * Prosegue tab.PT e PU e PV
387200120629     C                   MOVEL     'PZ'          TABCOD
387300120629     C     TABCOD        CHAIN     EDTAB01L                           30
387400120629     C     *IN30         DOWEQ     '0'
387500120629      *
387600120629     C                   SELECT
387700120629     C     TABFLG        WHENNE    *BLANKS
387800120629      *
387900120629     C                   OTHER
388000120629     C                   MOVEL     TABKEY        PZ_key
388100120629     C                   Z-ADD     1             X
388200120629     C     PZ_key        LOOKUP    PT_key(X)                              32
388300120629     C   32              MOVEL     TABUNI        PZ_uni(X)
388400120629     C                   ENDSL
388500120629      *
388600120629     C     TABCOD        READE     EDTAB01L                               30
388700120629     C                   END
388800120629      *
388900090209     C* Caricamento Tabella codici clienti - tariffe
389000090209     C                   Z-ADD     0             X
389100090209     C                   MOVEL     'CL'          TABCOD
389200090209     C     TABCOD        CHAIN     EDTAB01L                           30
389300090209     C     *IN30         DOWEQ     '0'
389400090209     C     TABFLG        IFEQ      *BLANKS
389500090209     C                   ADD       1             X
389600100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
389700100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
389800090209     C                   END
389900090209     C     TABCOD        READE     EDTAB01L                               30
390000090209     C                   END
390100090209      *
390200090209     C* Caricamento Serivizi speciali
390300090209     C                   Z-ADD     0             X
390400090209     C                   MOVEL     'SS'          TABCOD
390500090209     C     TABCOD        CHAIN     EDTAB01L                           30
390600090209     C     *IN30         DOWEQ     '0'
390700090209     C     TABFLG        IFEQ      *BLANKS
390800090209     C                   ADD       1             X
390900100702     C                   eval       Key_ServSpec(X) = TABKEY
391000100702     C                   eval       SpecialServ(X)  = TABKEY
391100100702     C                   eval       UNI_ServSpec(X) = TABUNI
391200090209     C                   END
391300090209     C     TABCOD        READE     EDTAB01L                               30
391400090209     C                   END
391500090209      *
391600090209     C                   ENDSR
391700090205      * ?------------------------------------------------------------------ */
391800090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
391900090205      * ?------------------------------------------------------------------ */
392000090205     C     *pssr         BEGSR
392100090205     C
392200060710     C                   eval      esito = '2'
392300100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
392400110617      *
392500130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
392600130506     C                             IFCSUM D96A EDI ricevuto errato in UPLOAD -
392700130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
392800130506     c                             + ' verificare il '+
392900130506     c                             %editc(total_segmenti:'Z')
393000130506     c                             + ' segmento -> ' + dati
393100110617      *
393200100319     c                   if        §PVinv <> 'N'
393300100319     c                   exsr      invio_mail
393400100319     c                   end
393500130507      *
393600130507     c                   seton                                        LR
393700130507     c                   return
393800130507      *
393900090213     C                   ENDSR
394000090213      * ?------------------------------------------------------------------ */
394100090213      *?      Segnala Errore su TIVIN
394200090213      * ?------------------------------------------------------------------ */
394300090213     C     Error_DET     BEGSR
394400090213     C
394500100728     c                   eval      Segmento_in_errore = Conta_segmenti
394600090213     C                   eval      vinFLG = '2'
394700090213     c                   eval      Almeno_Un_Due ='S'
394800090213     c                   eval      esito  = '1'
394900090213     c                   eval      se_errore ='S'
395000090213     C                   eval      Err_in_detta = 'S'
395100090213     C
395200090213     C                   ENDSR
395300100705      * ?------------------------------------------------------------------ */
395400100705      *?   Controlla e decodifica Valori VALUE dei segmenti
395500100705      * ?------------------------------------------------------------------ */
395600100705     C     In_TAB_EDSTBL BEGSR
395700100705      *
395800100705      *   Cerca la decodifica del dato in TABELLA
395900100705     c                   clear                   trovata_EDSTBL    1
396000100705     c                   clear                   quale_campo       6
396100100705     c                   clear                   campo_dati_70A   70
396200100705     C                   movel     UNB_Mittente  etbUNB
396300100705     c     K_TAB1L       chain     edstbl01l
396400100705      *
396500100705      * prova quindi senza l'UNB specifico del cliente
396600100705     c                   if        not %Found(edstbl01l)
396700100705     C                   clear                   etbUNB
396800100705     c     K_TAB1L       chain     edstbl01l
396900100705     c                   end
397000100705      *
397100100705      *  Traduce i testi descrittivi se PRESENTE in tabella
397200100705      *   il campo di destinazione è definito sulla destra della descrizione
397300100705      *     ed è lungo 6 come i nomi definiti nei VAB.
397400100705     c                   if        %Found(edstbl01l)
397500100705     c                   move(p)   etbDESCRI     quale_campo
397600100705     c                   movel(p)  etbDATI       campo_dati_70A
397700100705     c                   move      'S'           trovata_EDSTBL
397800100705     c                   end
397900100705      *
398000100705      *
398100100705     C                   ENDSR
398200100705     c*==================================================================*
398300090209     C*? CNVDAT - DECODIFICA LA DATA
398400090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
398500090209     C*         - WDATA  (DATA ALFANUMERICA)
398600090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
398700090209     C*         - WHMS   (ORA NUMERICA)
398800090209     C*----------------------------------------------------------------
398900100702     C     Converte_Data BEGSR
399000090209     C*
399100090209     C                   MOVEL     ' '           WERRO             1
399200090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
399300090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
399400100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
399500090209     C                   Z-ADD     *ZEROS        WHMS              6 0
399600090209     C*
399700120905     C     Work_Format_3 IFEQ      *blank
399800120905     c                   if        %Len(%trim(Work_Data__35)) = 8
399900120905     C                   eval         Work_Format_3 = Data_senza_ora
400000120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
400100120905     C                   eval         Work_Format_3 = Data_con_ora
400200120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
400300120905     C                   eval         Work_Format_3 = Data_con_i_secondi
400400120905     c                   end
400500120905     C                   END
400600120905     C*
400700100705     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
400800100702     C                   MOVEL     Work_Data__35 WCOMO8            8
400900090209     C                   TESTN                   WCOMO8               99
401000090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
401100090209     C  N99              MOVEL     'S'           WERRO             1
401200090209     C                   END
401300090209     C*
401400100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
401500100702     C                   MOVEL     Work_Data__35 WCOMO12          12
401600100120     C                   TESTN                   WCOMO12              99
401700100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
401800100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
401900100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
402000090209     C  N99              MOVEL     'S'           WERRO             1
402100090209     C                   END
402200100120     C*
402300100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
402400110527     C                   MOVEL     *all'0'       WCOMO14          14
402500110527     C                   MOVEL     Work_Data__35 WCOMO14          14
402600100120     C                   TESTN                   WCOMO14              99
402700100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
402800100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
402900100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
403000100120     C  N99              MOVEL     'S'           WERRO             1
403100100120     C                   END
403200090209     C*
403300090209     C                   ENDSR
403400090210     C*----------------------------------------------------------------
403500100701     C*? Se_Numerico       - CONTROLLO NUMERICO
403600100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
403700100701     C*         - lunghezza_num   (Lunghezza campo output)
403800100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
403900090210     C*----------------------------------------------------------------
404000100701     C     Se_Numerico   BEGSR
404100090210     C*
404200090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
404300100701     C                   MOVEL     'N'           Campo_Numerico    1
404400100701     C                   MOVEL     *BLANKS       Numero_OK        15
404500090210     C                   Z-ADD     15            IN                3 0
404600100701     C                   Z-ADD     lunghezza_num IX                3 0
404700090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
404800100701     C                   MOVEA     test_num      NDA
404900090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
405000090210     C                   MOVEA     *ALL'0'       NDN
405100100630      *
405200090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
405300100701     C                   Z-ADD     lunghezza_num I                 3 0
405400090210     C     I             DOWGT     0                                            --- 1 -->
405500090210     C* ESTRAE IL CARATTERE DA ESAMINARE
405600100701     C     1             SUBST     test_num:I    WTEM01            1
405700090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
405800090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
405900090210     C     WTEM01        SCAN      WCA                                    99
406000090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
406100090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
406200090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
406300090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
406400090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
406500090210     C* DA DESTRA VERSO SINISTRA
406600090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
406700090210     C                   MOVEL     WTEMP1        NDN(IN)
406800090210     C                   SUB       1             IN
406900090210     C                   END                                                    <-- 3 ---
407000090210     C                   END                                                    <-- 2 ---
407100090210     C                   SUB       1             I
407200090210     C                   END                                                    <-- 1 ---
407300090210     C*
407400090210     C                   MOVEA     NDN           WNDN             15
407500090210     C     WNDN          IFNE      *ZEROS
407600100701     C                   MOVEA     NDN           Numero_OK        15
407700100701     C                   MOVEL     'S'           Campo_Numerico    1
407800090210     C                   END
407900090210     C*
408000090210     C                   ENDSR
408100090210     C*----------------------------------------------------------------
408200090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
408300090210     C*----------------------------------------------------------------
408400100705     C     Converte_CAP  BEGSR
408500090210     C*
408600090210     C                   MOVEL     *BLANKS       WCAP              9
408700090210     C     nad3251       IFNE      '0'
408800090210     C     '  '          SCAN      nad3251       LENGHT            2 0
408900090210     C                   SUB       1             LENGHT
409000090210     C     LENGHT        IFLE      5
409100090210     C     LENGHT        ANDGT     0
409200090210     C                   Z-ADD     LENGHT        T                 2 0
409300090210     C                   Z-ADD     5             S                 2 0
409400090210     C                   MOVEA     nad3251       CAP9
409500090210     C                   MOVEL     *ZEROS        CAP5
409600090210     C                   DO        LENGHT
409700090210     C                   MOVE      CAP9(T)       CAP5(S)
409800090210     C                   SUB       1             S
409900090210     C                   SUB       1             T
410000090210     C                   END
410100090210     C                   MOVEA     CAP5          WCAP5             5
410200090210     C     WCAP5         IFEQ      '0'
410300090210     C                   MOVEL     *BLANKS       WCAP
410400090210     C                   ELSE
410500090210     C                   MOVEA     CAP5          WCAP
410600090210     C                   END
410700090210     C*
410800090210     C                   ELSE
410900090210     C                   MOVEL     nad3251       WCAP
411000090210     C                   END
411100090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
411200090210     C     WCAP          IFNE      *BLANKS
411300090210     C     WFLCPF        ANDNE     0
411400090210     C                   MOVEL     WFLCPF        WELE              1 0
411500090210     C                   MOVE      WFLCPF        WPOS              1 0
411600090210     C                   MOVEL     *BLANK        WCPF
411700090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
411800090210     C                   ELSE
411900090210     C                   MOVEL     *BLANKS       WCPF
412000090210     C                   END
412100090210     C                   END
412200090210     C*
412300090210     C                   ENDSR
412400090211      * ?================================================================== */
412500090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
412600090211      * ?================================================================== */
412700090211     C     Scrive_VAB    BEGSR
412800090211      *
412900090211     c                   clear                   err_bloccante     1
413000090211     c                   move      'S'           Almeno_Uno
413100090211      *
413200090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
413300090211     c                   exsr      UPDVAB
413400090211      *
413500090211     C* >>>>>>>>>>>>
413600090211     C*  Richiamo pgm per scrittura immediata VAB
413700090211     C     §PUWRK        IFEQ      ' '
413800090211      *
413900090211     C*  Richiamo pgm per esecuzione stampa
414000110110     C**** §PTCMR        IFEQ      'S'
414100110110     c******             MOVEL     BGM_nrCMR     d86CMR
414200110110     c******             if            RFF_NrCMR <> *Blank
414300110110     C******             MOVEL     RFF_NrCMR     d86CMR
414400110110     c******             end
414500110110     C******             ELSE
414600110110     C******             MOVEL     BGM_NrFviaggiod86CMR
414700110110     c******             if          RFF_NrFviaggio <> *Blank
414800110110     C******             MOVEL     RFF_NrFviaggiod86CMR
414900110110     c******             end
415000110110     C******             END
415100110110      *
415200110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
415300110110     c                   MOVEL     vabCMR        d86CMR
415400090211      *
415500090213     C                   MOVEL     ' '           d86RIE
415600090213     C                   MOVEL     §PUDTA        d86DTA
415700090213     C                   Z-ADD     1             d86CNT
415800090213     C                   Z-ADD     §PTKSC        d86CCM
415900090213     C                   MOVEL     *BLANKS       d86STA
416000090213     C                   MOVEL     'E'           d86MOD
416100090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
416200090211      * dove poi imposta la DS dei parametri passati
416300090213     C                   MOVEL     'LR'          d86CHI
416400090213     C                   MOVEL     'N'           d86CTL
416500090211      *
416600090211     c                   move      kpjbu         SvKpjbu
416700090211     c                   clear                   kpjbu
416800090216     C                   movel     *on           Commit_on         1
416900090213     C                   MOVEL     TRTC86ds      KPJBU
417000090213      * ?- Chiamata la TRTC86R2 -*
417100090213     C                   CALL      'TRTC86R2'
417200090211     C                   PARM                    KPJBA
417300090216     C                   PARM                    Commit_on
417400090211     c                   move      Svkpjbu       Kpjbu
417500090211      *
417600090211     C*  Controllo pgm per scrittura immediata VAB
417700100630     C     sk            IFNE      0
417800100630     C                   DO        sk            sx
417900100630     C                   Z-ADD     skCLienti(sx) d86CCM
418000090211      *
418100090211     c                   move      kpjbu         SvKpjbu
418200090211     c                   clear                   kpjbu
418300090216     C                   movel     *on           Commit_on         1
418400090213     C                   MOVEL     TRTC86ds      KPJBU
418500090213      * ?- Chiamata la TRTC86R2 -*
418600090213     C                   CALL      'TRTC86R2'
418700090211     C                   PARM                    KPJBA
418800090216     C                   PARM                    Commit_on
418900090211     c                   move      Svkpjbu       Kpjbu
419000090211     C                   END
419100090211     C                   END
419200090211      *
419300090211     C                   END
419400090211      * >>>>>>>>>>>>
419500090211     C                   if        se_errore  = 'S'
419600090211     c                   move      'S'           err_bloccante
419700090211     c                   goto      Error_VAB
419800090211     c                   end
419900090211     C*
420000090211     C     Error_VAB     Endsr
420100090211     C*----------------------------------------------------------------
420200090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
420300090211     C*----------------------------------------------------------------
420400090211     C     UPDVAB        BEGSR
420500090211      *
420600090211     C*  Se non è stato impostato il codice bolla lo imposto io
420700090211     C     VABCBO        IFEQ      *BLANKS
420800090211     C     VABCAS        IFGT      0
420900090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
421000090211     C                   ELSE
421100090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
421200090211     C                   END
421300131025      * se però è stato scritto e non era considerato il Contrassegno
421400131025      *  per motivi di lettura del record MOA successiva al Tipo Bolla
421500131025      *   qui sistema il codice bolla
421600131025     C                   Else
421700131025      *  Porto Franco +COD
421800131025     c                   if        VABCBO = '1' and  VABcas > *zeros
421900131025     C                   movel     '4'           VABCBO                         + C/Ass
422000131025      *  Porto Assegnato +COD
422100131025     c                   elseIF    VABCBO = '2' and  VABcas > *zeros
422200131025     C                   movel     '6'           VABCBO                         + C/Ass
422300131025     C                   ENDif
422400090211     C                   END
422500100701      * Imposta le NOte
422600100701     C     VABNOT        IFEQ      *BLANKS
422700100701     C                   MOVEL     Note_1        VABNOT
422800100701     C                   MOVEL     Note_2        VABNT2
422900100701     C                   ELSE
423000100701     C                   MOVEL     Note_1        VABNT2
423100100701     C                   END
423200090211      *
423300090211     C*  Attribuisco anno spedizione
423400100630     C     DTM_DtParten  IFGT      0                                            anno sped.
423500100630     C                   MOVEL     DTM_DtParten  VABAAS
423600090211     C                   ELSE
423700100630     C     DTM_DtFViag   IFGT      0
423800100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
423900090211     C                   END
424000090211     C                   END
424100090211      *
424200090211      *  Controllo dettaglio segnacolli
424300090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
424400100630     C                   EXSR      CTRsegn_EEX
424500090211     C                   ELSE
424600090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
424700090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
424800100630     C                   EXSR      CTRsegn_BAR
424900090211     C                   END
425000090211     C                   END
425100090211      *
425200090212      *  Imposto linea partenza e serie
425300100701     C                   eval      VABLNP = PT_vabLNP
425400100701     C                   eval      VABNRS = PT_vabNRS
425500090211      *
425600160209      *  e la Filiale di GESTIONE
425700160209     c                   if        PS_vabFGS  <> *blank
425800160209     C                   MOVE      PS_vabFGS     VABfgs
425900160209     c                   else
426000160209     C                   z-add     VABlnp        VABfgs
426100160209     c                   end
426200160209      *
426300090211      *  Controllo se devo variare il codice trattamento merce
426400100701     C                   eval      VABCTM = PT_vabCTM
426500090211      *
426600090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
426700090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
426800090211     c                   clear                   mantiene_nsp      1            *blk/'S'
426900100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
427000090211      *
427100090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
427200090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
427300090211      *
427400090211      *  Imposto codice cliente
427500090211     c                   Select
427600090211      *
427700100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
427800100319     C                   When        Det_RFF_FF_ksc  <> *zeros
427900100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
428000100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
428100090211      * forza LNP/CTM/NRS
428200100319     c                   if        Det_RFF_FF_lnp <> 0
428300100319     C                   eval      VABLNP = Det_RFF_FF_lnp
428400090211     c                   end
428500160209     c                   if        Det_RFF_FF_fgs <> *blank
428600160209     C                   move      Det_RFF_FF_fgsVABFGS
428700160209     c                   else
428800160209     c                   if        Det_RFF_FF_lnp <> 0
428900160209     C                   eval      VABfgs = Det_RFF_FF_lnp
429000160209     c                   end
429100160209     c                   end
429200140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
429300140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
429400140901     c*******            if        Det_RFF_FF_nrs <> 0
429500100319     C                   eval      VABNRS = Det_RFF_FF_nrs
429600140901     c*******            end
429700100319     c                   if        Det_RFF_FF_ctm <> *blank
429800100319     C                   eval      VABCTM = Det_RFF_FF_ctm
429900090211     c                   end
430000110411     c                   if        Det_RFF_FF_tic <> *blank and VABCAS <>0
430100110411     C                   eval      VABTIC = Det_RFF_FF_tic
430200110411     c                   end
430300090211      *
430400090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
430500100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
430600100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
430700090211     c                   end
430800090211      *
430900100319      *  Specificato codice da qualificatore RFF+FF dalla testata
431000100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
431100100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
431200100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
431300090211      * forza LNP/CTM/NRS
431400100319     c                   if        Tes_RFF_FF_LNP  <> 0
431500100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
431600090211     c                   end
431700160209     c                   if        Tes_RFF_FF_fgs <> *blank
431800160209     C                   move      Tes_RFF_FF_fgsVABFGS
431900160209     c                   else
432000160209     c                   if        Tes_RFF_FF_lnp <> 0
432100160209     C                   eval      VABfgs = Tes_RFF_FF_lnp
432200160209     c                   end
432300160209     c                   end
432400140901     c***** ATTENZIONE la serie può essere (0) al contrario di quanto presente sulla PT
432500140901     c**** se sulla PT si sta trattando un Disk B con SERIE e nella CL invece è un Disk C
432600140901     c*******            if        Tes_RFF_FF_NRS  <> 0
432700100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
432800140901     c*******            end
432900100319     c                   if        Tes_RFF_FF_CTM  <> *blank
433000100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
433100090211     c                   end
433200110411     c                   if        Tes_RFF_FF_tic  <> *blank and VABCAS <>0
433300110411     C                   eval      VABTIC = Tes_RFF_FF_tic
433400110411     c                   end
433500090211      *
433600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
433700100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
433800100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
433900090211     c                   end
434000090211      *
434100090211      *  Imposto dati da tabella - PT-
434200090211     C                   Other
434300090211      *
434400090211     C                   MOVEL     §PTKSC        VABCCM
434500090211     C                   MOVEL     §PTCTR        VABCTR
434600090211      *
434700090211     C                   Endsl
434800110411      *
434900110411      *  attenzione il tipo Contrassegno solo se c'è un valore
435000110411     c                   if        vabCAS = 0
435100110411     c                   clear                   vabTIC
435200110623     c                   else
435300110623      *
435400110623      * Se non è stato passato il Tipo incasso a fronte di particolarità sullo
435500110623      *  specifico cliente/partner
435600110707     c                   if        inviato_tipo_incasso =  'N' and
435700110707     c                             Det_RFF_FF_ksc > *zeros     and
435800110707     c                             Det_RFF_FF_tic = *blank
435900110623      *  e se c'è "??" sul tipo incasso deve Forzare questo tipo di Incasso sulla
436000110623      *  spedizione.
436100110623     c                   z-add     1             Y
436200110623     c                   movel(p)  '??'          Work_Data__35
436300110623     c                   move      §puTC         Work_Data__35
436400110623     c                   move      Work_Data__35 WData_35_2        2
436500110623     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
436600110623     c                   if         *in33
436700110623     c                   eval      vabTIC = TipoIncasso(Y)
436800110623     c                   end
436900110623      **
437000110623     c                   end
437100110623      **
437200110411     c                   end
437300090211      *
437400090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
437500090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
437600090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
437700090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
437800090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
437900090211      *
438000090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
438100090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
438200090211     c                   else
438300090211      *
438400090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
438500090211     C*  lo attribuisco io
438600090211      **              **------------------**
438700090211     c                   exsr      repnum
438800090211      **              **------------------**
438900100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
439000090211      **              **------------------**
439100090211     c                   end
439200090211      *
439300090211     C*  Altrimenti attribuisco data del giorno
439400090211     C     VABMGS        IFEQ      0
439500090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
439600090211     C                   MOVE      WOGGI         VABMGS                         = oggi
439700090211     C                   END
439800090211     C*  Eseguo posizionamento su EDiVAB
439900090211     C                   Z-ADD     VABAAS        KAAS
440000090211     C                   Z-ADD     VABlnp        KLNP
440100090211     C                   Z-ADD     VABNRS        KNRS
440200090211     C                   Z-ADD     VABNSP        KNSP
440300090211     C                   MOVEL     SAVBOL        SALVA
440400090211     C     KVAB1         CHAIN     EDiVAB1L                           30
440500090211     C                   MOVEL     SALVA         SAVBOL
440600090211     C*  Imposto dati CMR
440700090211     C                   Z-ADD     1             VABCNT
440800090211     **
440900090211      *** Attenzione se si deve trattare il VAX con il CMR
441000090211      *** il campo VABCNT deve essere sempre impostato a (1)
441100090211     C     §puWRK        ifEQ      'S'
441200090211     C     §puNSP        andEQ     'S'
441300090211     C                   Z-ADD     1             VABCNT
441400090211     c                   end
441500090211     **
441600100630     C                   Z-ADD     Data_prepara  VABDTS
441700090212     C                   movel     '20'          VABDTS
441800100113     ** Forza la Data di Oggi
441900100113     C                   z-add     WOggi         VABDTS
442000100113     **
442100100630     C     Ora__prepara  mult      100           VABHMS
442200090211     C     §PTCMR        IFEQ      'S'
442300100630     C                   Z-ADD     DTM_DtCMR     VABDCM
442400100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
442500100701     c                   if          RFF_NrCMR <> *Blank
442600100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
442700100701     c                   end
442800090211     C                   ELSE
442900100630     C                   Z-ADD     DTM_DtFViag   VABDCM
443000100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
443100100701     c                   if          RFF_NrFviaggio <> *Blank
443200100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
443300100701     c                   end
443400090211     C                   END
443500100120      *
443600100813     c                   if          VABCMR = *Blank
443700100813     C                   MOVEL(p)  BGM_Id_TestataVABCMR
443800100813     c                   else
443900100813      *
444000100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
444100100120      * accorpare la bolle ad un unico destinatario da più parti
444200100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
444300100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
444400120629     C                   if         NAD_SF_Cliente <> *blank
444500120629     c                   eval      VABCMR = NAD_SF_Cliente
444600100708     c                   else
444700100813     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z')    +
444800100813     c                                      '_' + %editc(Data_Prepara:'Z') +
444900100813     c                                      '_' + %editc(Ora__Prepara:'Z')
445000100708     c                   end
445100100706     c                   end
445200100120      *
445300100813     c                   end
445400100921      *
445500100921     c                   if        vabdcm = 0
445600100921     C                   Z-ADD     Data_Prepara  VABDCM
445700100921     c                   end
445800100813      *
445900090211     C*  Se non è indicato il nome del mittente = Partner mittente
446000090211     C     VABRMO        IFEQ      *BLANKS
446100090211     C                   MOVEL     ACORAG        VABRMO
446200090211     C                   MOVEL     INDCAP        VABCMO
446300090211     C     INDCAE        IFNE      *BLANKS
446400090211     C                   MOVEL     INDCAE        VABCMO
446500090211     C                   END
446600090211     C     INDSTA        IFNE      *BLANKS
446700090211     C                   MOVEL     INDSTA        VABNMO
446800090211     C                   END
446900090211     C                   END
447000090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
447100090211     C     §1BFG1        IFEQ      'N'
447200090211     C     §1BFG7        ANDEQ     'N'
447300090211     C     §1BFG2        ANDEQ     'N'
447400090211     C***                  Z-ADD0         VABNRS
447500090211     C                   END
447600090211     C*  Imposto segancollo da - a
447700090211     C     §1BFG1        IFEQ      'S'
447800090211     C     §1BFG7        ANDEQ     'N'
447900090211     C     §1BF12        ANDNE     'E'
448000090211      *
448100100701     C     Conta_Segn    IFEQ      *ZEROS
448200090211     C                   CLEAR                   VABNCD
448300090211     C                   CLEAR                   VABNCA
448400090211     C                   ELSE
448500100630     C                   Z-ADD     segn_BAR(1)   VABNCD
448600100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
448700090211     C                   ENDIF
448800090211     C*
448900090211     C                   END
449000090211     C*
449100090211     C*  Se cap mittente originale a blank abblenco nazione
449200090211     C     VABCMO        IFEQ      *BLANKS
449300090211     C                   MOVEL     *BLANKS       VABNMO
449400090211     C                   END
449500100701     C*
449600090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
449700090603     C* Se in testata
449800100701     c                   if        NAD_destinatario_in_testata = 'S' and
449900100701     c                             NAD_destinatario_in_dettaglio <> 'S'
450000090603     c                   eval         VABrsd =  tes_VABrsd
450100090603     c                   eval         VABrd2 =  tes_VABrd2
450200090603     c                   eval         VABind =  tes_VABind
450300090603     c                   eval         VABlod =  tes_VABlod
450400090603     c                   eval         VABnzd =  tes_VABnzd
450500090603     c                   end
450600090603     C* Se in testata
450700100701     c                   if        NAD_mittente_in_testata = 'S' and
450800100701     c                             NAD_mittente_in_dettaglio <> 'S'
450900090603     c                   eval         VABrmo = tes_VABrmo
451000090603     c                   eval         VABnmo = tes_VABnmo
451100090603     c                   eval         VABcmo = tes_VABcmo
451200090603     c                   end
451300110613      *
451400110613      * Non ha calcolato l'instradamento poichè non aveva segmenti MEA
451500110613     C                   if         Instradamento = *blank
451600110613     c                               and vabCAD = *blank and vabpkb >0
451700110613     c                   exsr      Instrada_sped
451800110613     c                   end
451900100813      *
452000100813     C*  Per non bloccare la bollettazione
452100100813     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
452200100813     C*  o se il peso non è presente... imposto fisso il minimo 0,1
452300130315     c**************     if        vabpkb = 0
452400130315     c**************     z-add     0,1           vabpkb
452500130315     c**************     end
452600100813     C*
452700100813      *
452800100813     C*  Se cap mittente originale a blank abblenco nazione
452900100813     C     VABCMO        IFEQ      *BLANKS
453000100813     C                   MOVEL     *BLANKS       VABNMO
453100100813     C                   END
453200100813      *
453300100813      *   Forza il numero passato nel CNI nel riferimento Numerico
453400100813     c                   if        §PVCNIRMN = 'S'
453500100813     c                   z-add     CNI_SeNumericovabRMN
453600100813     c                   end
453700100921      *
453800100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
453900100921     C     §puWRK        ifEQ      'S'
454000100921     C     §puraggDES    andeq     'S'
454100100921     c                   clear                   vabLNA
454200100921     c                   clear                   vabZNC
454300100921     c                   clear                   vabCMR
454400100921      *  il nome identico
454500100921     c                   eval      VABCMR  = §punomeCMR
454600100921      *   e in più la data del giorno
454700100921     C     §puinGIO      ifEQ      'S'
454800100921     c                   eval      VABCMR = %trim(VABCMR)
454900100921     c                                    + %editc(WOggi_CMR:'X')
455000100921     c                   end
455100100921      *
455200100921     c                   end
455300100921      *
455400111028      ****
455500111028      **** se nel Riferimento Numerico viene passato il riferimento di una
455600111028      ****  nostra bolla di export precedentemente inviata di cui si sta facendo
455700111028      **** il reso. (Vedi AZKAR)    controlla
455800111028      **
455900111028     c                   if        PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
456000111028      *
456100111028      * controlla che contenga un numero di 14 cifre valide
456200111028      *   quindi la prima a sinistra deve essere (0)
456300111028     c                   movel     vabrmn        uno0              1 0
456400111028     c                   if        uno0 = 0
456500111028      *
456600111028      *   poi la seconda da sinistra deve NON essere (0)
456700111028     c                   movel     vabrmn        due00             2 0
456800111028     c                   move      due00         uno0
456900111028     c                   if        uno0 > 0
457000111028      *
457100140610     C                   move      VABRMN        dsspe
457200111028     c                   exsr      ctrl_seERAexp
457300111028      *
457400111028     c                   if           Sped_RESO = 'S'
457500111028     c                   exsr      Write_RESO
457600111028     c                   end
457700111028      *
457800111028     c                   end
457900111028     c                   end
458000111028     c                   end
458100111028      *
458200140610      *? ------------------------------------------------------------------------
458300140610      *?  SE il PARTNER è molto particolare ed utilizza un segmento di riferimento
458400140610      *?    per restituire i RESI o BOLLE generate da nostri ORM export
458500140610      *?  il caso: FR-EXPRESS - CALBER:ZZ e NON volendo utilizzare il RFF+CU,
458600140610      *?  utilizziamo questa tecnica x intercettare le spedizioni da segnalare in bollettazione.
458700140610      *? ------------------------------------------------------------------------
458800140610      ****
458900140610      **** se quindi il codice identificativo arriva carico
459000140610      ****  si verifica che sia un RESO oppure un ORM.
459100140610      **
459200140610      **  DEVE comunque essere abilitato a riceverlo mediante la "PT"
459300140623     c                   if        salva_REF_ORM <> *blank  and
459400140610     c                             PU_in_RMN_BOLLA_EXPORT_x_RESO = 'S'
459500140610      **
459600140919      **  Attenzione il codice può arrivare vuoto per problemi di caratteri
459700140919      **   inviati sposizionati dal Partner.
459800140919     C                   if          Cod_RESO_ORM <> *blank
459900140610     C                   move      Cod_RESO_ORM  dsspe
460000140610     c                   exsr      ctrl_seERAexp
460100140919     c                   end
460200140610      *
460300140610     c                   if           Sped_RESO = 'S'
460400140610     c                   exsr      Write_RESO
460500140610     c                   else
460600140610     c                   exsr      Write_da_ORM
460700140610     c                   end
460800140610      *
460900140610     c                   end
461000140610      *? ------------------------------------------------------------------------
461100140610      *?  PARTICOLARITA'
461200140610      *? ------------------------------------------------------------------------
461300120614      *
461400120614      * x Consegna ai PIANI "Floor"   "FLR"
461500120614     c                   If        FTX_ai_piani = 'P'
461600120614     c                   if             vabTC1 = *blank
461700120614     C                   MOVEL     'P'           VABTC1
461800120614     c                   elseIF         vabTC2 = *blank
461900120614     C                   MOVEL     'P'           VABTC2
462000120614     c                   end
462100120614     C                   END
462200120614      *
462300160621      *****
462400160621      ***** davanti alla REAGIONE SOCIALE del DESTINATARIO si vuole "JENNIFER"
462500160621      *****
462600160621     c                   move(p)   VABrsd        campo44          44
462700160621     c                   movel     'JENNYFER-'   campo44
462800160621     C                   movel     campo44       VABrsd
462900160621     C                   move      campo44       campo9            9
463000160621     c                   move(p)   VABrd2        campo44          44
463100160621     c                   movel     campo9        campo44
463200160621     C                   movel     campo44       VABrd2
463300160621      *
463400160622     C                   MOVEL     *zeros        VABDCR                         Data cons.rich.
463500160622     C                   MOVEL     *zeros        VABHCR                         Ora  cons.rich.
463600160622     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
463700160621      *****
463800090211      *? ------------------------------------------------------------------------
463900090213     C   30              WRITE     EDiVAB00                             99
464000090213     C  N30              UPDATE    EDiVAB00                             99
464100090211      *? ------------------------------------------------------------------------
464200090213     c   99              eval      errori_in_Wrt = 'S'
464300090211     C*
464400090211     c                   clear                   wri_vabtic        1
464500090211     C*
464600090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
464700100706     C                   EXSR      Scrive_Riferim
464800090211     C*
464900090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
465000090211     C     §1BF12        IFEQ      'E'
465100100706     C                   EXSR      Scrive_VAT
465200090211     C                   ELSE
465300090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
465400090211     C     §1BFG7        IFEQ      'S'
465500090211     C     §1BFG1        OREQ      'S'
465600100706     C                   EXSR      Scrive_VAD
465700090211     C                   END
465800090211     C                   END
465900090211      *
466000090211     C* Reimposto codice trattamento merce cliente
466100100628     C                   MOVEL     sav_DS1B      DS1B
466200090211     C**
466300090211     C* Do errore se previsto CMR ma non c'è
466400090211     C     §PTCMR        IFNE      ' '
466500090211     C                   EXSR      MEMCMR
466600090211     C                   END
466700090211     C* Do errore se previsto AFB ma non c'è
466800090211     C     §PTNFV        IFNE      ' '
466900090211     C                   EXSR      MEMAFB
467000090211     C                   END
467100090211     C*  Se il cliente vuole il ritorno delle date di consegna
467200090211     C*  è c'è una squdratura fra i segnacolli e il numero
467300090211     C*  dei colli che ci ha passato scrivo EDSUM
467400100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
467500090211     C                   EXSR      WRTSUM
467600090211     C                   END
467700090211     C*
467800090211     C                   ENDSR
467900090211     C*----------------------------------------------------------------
468000090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
468100090211     C*----------------------------------------------------------------
468200100630     C     CTRsegn_BAR   BEGSR
468300090211     C*
468400090211     C                   MOVEL     'N'           WNSGER            1
468500090211     C*
468600090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
468700100701     C     VABNCL        IFNE      Conta_Segn
468800090211     C                   MOVEL     'S'           WNSGER
468900100630     C                   eval       Squadratura_Colli = 'S'
469000090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
469100090211     c                              bollettati'
469200090211     C                   GOTO      FINSGN
469300090211     C                   END
469400100706      *
469500090211     C*  Riordino i segnacolli
469600090211     C     §1BFG7        IFEQ      'N'
469700100630     C                   SORTA     segn_BAR
469800090211     C*  Controllo se sono sequenziali
469900090211     C                   MOVEL     'N'           WNOSEQ            1
470000100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
470100100706     C*
470200100701     C     2             DO        Conta_Segn    X
470300100630     C     segn_BAR(X)   IFNE      WEXSGN
470400090211     C                   MOVEL     'S'           WNOSEQ
470500090211     C                   END
470600100630     C     segn_BAR(X)   ADD       1             WEXSGN
470700090211     C                   END
470800100706     C*
470900090211     C                   END
471000090211     C*
471100090211     C     FINSGN        ENDSR
471200090211     C*----------------------------------------------------------------
471300090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
471400090211     C*----------------------------------------------------------------
471500100630     C     CTRsegn_EEX   BEGSR
471600090211     C*
471700090211     C                   MOVEL     'N'           WNSGER            1
471800090211      *
471900090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
472000090211      *
472100100701     C     VABNCL        IFNE      totale_PCI
472200090211     C                   MOVEL     'S'           WNSGER
472300100630     C                   eval       Squadratura_Colli = 'S'
472400090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
472500090211     c                              bollettati'
472600090211     C                   END
472700090211      *
472800090211     C                   ENDSR
472900090211     C*----------------------------------------------------------------
473000090211     C*? ESEGUO SCRITTURA EDiVAD
473100090211     C*----------------------------------------------------------------
473200100706     C     Scrive_VAD    BEGSR
473300090211     C*
473400100701     C* Se non seq. scrivo i record
473500090211     C     §1BFG1        IFEQ      'S'
473600100701     C     Conta_Segn    ANDNE     *ZEROS
473700090211     C*
473800090211     C     §1BFG7        IFEQ      'S'
473900100701     C                   DO        Conta_Segn    X
474000090211     C                   Z-ADD     VABCCM        KCCM
474100100630     C                   Z-ADD     segn_BAR(X)   KNCD
474200090211     C                   Z-ADD     VABCNT        VADCNT
474300090211     C                   MOVEL     VABCMR        VADCMR
474400090211     C                   Z-ADD     VABAAS        VADAAS
474500090211     C                   Z-ADD     VABLNP        VADLNP
474600090211     C                   Z-ADD     VABNRS        VADNRS
474700090211     C                   Z-ADD     VABNSP        VADNSP
474800090211     C                   Z-ADD     1             VADNCL
474900100630     C                   MOVEL     segn_BAR(X)   VADCDP
475000100630     C                   Z-ADD     segn_BAR(X)   VADNCD
475100090211     C                   Z-ADD     *ZEROS        VADNCA
475200090211     C                   Z-ADD     VABCCM        VADCCM
475300090211     C                   movel     VABfgs        VADfgs
475400090213     C                   WRITE     EDiVAD00                             99
475500090213     c   99              eval      errori_in_Wrt = 'S'
475600090211     C                   END
475700090211     C* Se sequenziali scrivo un solo record
475800090211     C                   ELSE
475900090211     C                   Z-ADD     VABAAS        VADAAS
476000090211     C                   Z-ADD     VABLNP        VADLNP
476100090211     C                   Z-ADD     VABNRS        VADNRS
476200090211     C                   Z-ADD     VABNSP        VADNSP
476300090211     C                   Z-ADD     VABNCL        VADNCL
476400100630     C                   Z-ADD     segn_BAR(1)   VADNCD
476500100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
476600090211     C                   Z-ADD     VABCCM        VADCCM
476700090211     C                   movel     VABfgs        VADfgs
476800140902     C                   MOVEL     VABCMR        VADCMR
476900090213     C                   WRITE     EDiVAD00                             99
477000090213     c   99              eval      errori_in_Wrt = 'S'
477100090211     C                   END
477200090211     C*
477300090211     C                   ELSE
477400100701     C                   DO        Conta_Segn    X
477500090211     C                   Z-ADD     VABCCM        KCCM
477600100630     C                   Z-ADD     segn_BAR(X)   KNCD
477700090211     C                   Z-ADD     VABAAS        VADAAS
477800090211     C                   Z-ADD     VABLNP        VADLNP
477900090211     C                   Z-ADD     VABNRS        VADNRS
478000090211     C                   Z-ADD     VABNSP        VADNSP
478100100630     C                   MOVEL     segn_BAR(X)   VADCDP
478200090211     C                   Z-ADD     1             VADNCL
478300090211     C                   Z-ADD     *ZEROS        VADNCD
478400090211     C                   Z-ADD     *ZEROS        VADNCA
478500090211     C                   Z-ADD     VABCCM        VADCCM
478600090211     C                   movel     VABfgs        VADfgs
478700140902     C                   MOVEL     VABCMR        VADCMR
478800090213     C                   WRITE     EDiVAD00                             99
478900090213     c   99              eval      errori_in_Wrt = 'S'
479000090211     C                   END
479100090211     C*
479200090211     C                   END
479300090211     C*
479400090211     C                   ENDSR
479500090211     C*----------------------------------------------------------------
479600090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
479700090211     C*----------------------------------------------------------------
479800100706     C     Scrive_RiferimBEGSR
479900090211      *
480000090211      *  riferimento di consegna destinatario
480100100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
480200100701     c                             CTA_destinatario_in_testata   <> *blank
480300090211     C                   Z-ADD     VABCNT        VATCNT
480400090211     C                   MOVEL     VABCMR        VATCMR
480500090211     C                   Z-ADD     VABCCM        VATCCM
480600090211     C                   movel     VABfgs        VATfgs
480700090211     C                   Z-ADD     VABLNP        VATLNP
480800090211     C                   Z-ADD     VABAAS        VATAAS
480900090211     C                   Z-ADD     VABNRS        VATNRS
481000090211     C                   Z-ADD     VABNSP        VATNSP
481100090211     C                   MOVEL     'A'           VATTRC
481200100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
481300100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
481400100701     c                   else
481500100701     C                   eval       VATNOT = CTA_destinatario_in_testata
481600100701     c                   end
481700090213     C                   WRITE     EDiVAT00                             99
481800090213     c   99              eval      errori_in_Wrt = 'S'
481900090211     c                   end
482000090211      *
482100090211      *  riferimento di consegna telefonico
482200100701     c                   if        COM_telefono_in_dettaglio <> *blank or
482300100701     c                             COM_telefono_in_testata   <> *blank
482400090211     C                   Z-ADD     VABCNT        VATCNT
482500090211     C                   MOVEL     VABCMR        VATCMR
482600090211     C                   Z-ADD     VABCCM        VATCCM
482700090211     C                   movel     VABfgs        VATfgs
482800090211     C                   Z-ADD     VABLNP        VATLNP
482900090211     C                   Z-ADD     VABAAS        VATAAS
483000090211     C                   Z-ADD     VABNRS        VATNRS
483100090211     C                   Z-ADD     VABNSP        VATNSP
483200090211     C                   MOVEL     'B'           VATTRC
483300100701     c                   if        COM_telefono_in_dettaglio <> *blank
483400100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
483500100701     c                   else
483600100701     c                   eval      VATNOT = COM_telefono_in_testata
483700100701     c                   end
483800090213     C                   WRITE     EDiVAT00                             99
483900090213     c   99              eval      errori_in_Wrt = 'S'
484000090211     c                   end
484100090211      *
484200090211      * riferimento del Partner che una volta veniva riportato sul BL4
484300100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
484400100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
484500100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
484600100119      *
484700100701     c                   if        RFF_RifSpediz <> *blank
484800090211     C                   Z-ADD     VABCNT        VATCNT
484900090211     C                   MOVEL     VABCMR        VATCMR
485000090211     C                   Z-ADD     VABCCM        VATCCM
485100090211     C                   movel     VABfgs        VATfgs
485200090211     C                   Z-ADD     VABLNP        VATLNP
485300090211     C                   Z-ADD     VABAAS        VATAAS
485400090211     C                   Z-ADD     VABNRS        VATNRS
485500090211     C                   Z-ADD     VABNSP        VATNSP
485600090211     C                   MOVEL     'C'           VATTRC
485700100701     C                   MOVEL     RFF_RifSpediz VATNOT
485800090213     C                   WRITE     EDiVAT00                             99
485900090213     c   99              eval      errori_in_Wrt = 'S'
486000090211     c                   end
486100110907      **
486200110907      * Indirizzi mail x comunicazione (servizio a pagamento) al Destinatario
486300110907      *   sulla consegna della sua merce
486400110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:1:35)
486500110907     c                                  <> *blank
486600110907      *
486700110907     C                   Z-ADD     VABCNT        VATCNT
486800110907     C                   MOVEL     VABCMR        VATCMR
486900110907     C                   Z-ADD     VABCCM        VATCCM
487000110907     C                   movel     VABfgs        VATfgs
487100110907     C                   Z-ADD     VABLNP        VATLNP
487200110907     C                   Z-ADD     VABAAS        VATAAS
487300110907     C                   Z-ADD     VABNRS        VATNRS
487400110907     C                   Z-ADD     VABNSP        VATNSP
487500110907     C                   MOVEL     'I'           VATTRC
487600110907     C                   eval       VATNOT =
487700110907     c                             %subst(FTX_ServizioMail_xDestinatario:1:35)
487800110907     C                   WRITE     EDiVAT00                             99
487900110907     c   99              eval      errori_in_Wrt = 'S'
488000110907     c                   end
488100110907      *
488200110907     c                   if        %subst(FTX_ServizioMail_xDestinatario:36:35)
488300110907     c                                 <> *blank
488400110907      *
488500110907     C                   Z-ADD     VABCNT        VATCNT
488600110907     C                   MOVEL     VABCMR        VATCMR
488700110907     C                   Z-ADD     VABCCM        VATCCM
488800110907     C                   movel     VABfgs        VATfgs
488900110907     C                   Z-ADD     VABLNP        VATLNP
489000110907     C                   Z-ADD     VABAAS        VATAAS
489100110907     C                   Z-ADD     VABNRS        VATNRS
489200110907     C                   Z-ADD     VABNSP        VATNSP
489300110907     C                   MOVEL     'J'           VATTRC
489400110907     C                   eval       VATNOT =
489500110907     c                             %subst(FTX_ServizioMail_xDestinatario:36:35)
489600110907     C                   WRITE     EDiVAT00                             99
489700110907     c   99              eval      errori_in_Wrt = 'S'
489800110907     c                   end
489900110907      *
490000131025      *
490100131025      * Indirizzi SMS  x comunicazione (servizio a pagamento) al Destinatario
490200131025      *   sulla consegna della sua merce
490300131025      *
490400131025     c                   if        %subst(FTX_ServizioSMS_xDestinatario:1:16)
490500131025     c                                  <> *blank
490600131025     c                             or FTX_MAIL_come_servizio = 'N'
490700131025      *
490800131025     C                   Z-ADD     VABCNT        VATCNT
490900131025     C                   MOVEL     VABCMR        VATCMR
491000131025     C                   Z-ADD     VABCCM        VATCCM
491100131025     C                   movel     VABfgs        VATfgs
491200131025     C                   Z-ADD     VABLNP        VATLNP
491300131025     C                   Z-ADD     VABAAS        VATAAS
491400131025     C                   Z-ADD     VABNRS        VATNRS
491500131025     C                   Z-ADD     VABNSP        VATNSP
491600131025     C                   MOVEL     'S'           VATTRC
491700131025      *
491800131025      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
491900131025      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
492000131025     C                   eval       VATNOT =
492100131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
492200131025      *
492300131025      * SUL record del SMS vi è una DS 16+1+1 dove i primi 16 sono il cell.e i 2 flags
492400131025      * seguenti attivano o meno il servizio x SMS o MAIL
492500131025      *
492600131025      * se si vuole solo avere l'info nel sistema senza però attivare il servizio
492700131025      *  a pagamento per mandare un SMS o una MAIL di avviso di consegna
492800131025     C                   eval      %subst(VATNOT:17:1) = 'S'
492900131025     c                   if        FTX_SMS_come_servizio = 'N'
493000131025     c                              or
493100131025     c                             %subst(FTX_ServizioSMS_xDestinatario:1:16)
493200131025     c                              = *blank
493300131025     C                   eval      %subst(VATNOT:17:1) = 'N'
493400131025     c                   end
493500131025      *
493600131025     C                   eval      %subst(VATNOT:18:1) = 'S'
493700131025     c                   if        FTX_MAIL_come_servizio = 'N'
493800131025     c                              or
493900131025     c                             %subst(FTX_ServizioMAIL_xDestinatario:1:35)
494000131025     c                              = *blank
494100131025     C                   eval      %subst(VATNOT:18:1) = 'N'
494200131025     c                   end
494300131025      *
494400131025     C                   WRITE     EDiVAT00                             99
494500131025     c   99              eval      errori_in_Wrt = 'S'
494600131025     c                   end
494700131025      *
494800100119      *
494900090211     C                   ENDSR
495000090211     C*----------------------------------------------------------------
495100090211     C*? ESEGUO SCRITTURA EDiVAT
495200090211     C*----------------------------------------------------------------
495300100706     C     Scrive_VAT    BEGSR
495400090211      *
495500100701     C                   DO        totale_PCI    X
495600090211     C                   Z-ADD     VABCNT        VATCNT
495700090211     C                   MOVEL     VABCMR        VATCMR
495800090211     C                   Z-ADD     VABCCM        VATCCM
495900090211     C                   movel     VABfgs        VATfgs
496000090211     C                   Z-ADD     VABLNP        VATLNP
496100090211     C                   Z-ADD     VABAAS        VATAAS
496200090211     C                   Z-ADD     VABNRS        VATNRS
496300090211     C                   Z-ADD     VABNSP        VATNSP
496400090211     C                   MOVEL     'E'           VATTRC
496500090211      *
496600090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
496700090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
496800090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
496900100701     c                   if        PU_lunVAT     > 0
497000090211      *
497100090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
497200100630     C                   eval      VATnot =
497300100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
497400090211      *
497500090211     c                   else
497600090211      *
497700090211      * Se non è impostato nulla prende il segnacollo passato così com'è
497800100630     C                   MOVEL     segn_EEX(X)   VATNOT
497900090211     c                   end
498000090211      *
498100090213     C                   WRITE     EDiVAT00                             99
498200090213     c   99              eval      errori_in_Wrt = 'S'
498300090211     C                   END
498400090211      *
498500090211     C                   ENDSR
498600090211     C*----------------------------------------------------------------
498700090211     C*? ESEGUO SCRITTURA EDSUM
498800090211     C*----------------------------------------------------------------
498900090211     C     WRTSUM        BEGSR
499000090211     C*
499100100701     C* Se non seq. scrivo i   record
499200100701     C                   DO        Conta_Segn    X
499300090211     C                   Z-ADD     VABAAS        SUMAAS
499400090211     C                   Z-ADD     VABLNP        SUMFLS
499500100701     C                   MOVEL     PT_vabCCM     SUMLNP
499600090211     C                   Z-ADD     VABLNA        SUMLNA
499700090211     C                   Z-ADD     VABZNC        SUMZNC
499800090211     C                   Z-ADD     VABNRS        SUMNRS
499900090211     C                   Z-ADD     VABNSP        SUMNSP
500000100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
500100100701     C                   Z-ADD     PT_vabCCM     SUMCCM
500200090211     C                   MOVEL     *BLANKS       SUMSTS
500300090211     C                   MOVEL     *BLANKS       SUMFLG
500400100813     C                   eval       SUMCNI = CNI_Identificativo_Bolla
500500090211     C                   MOVEL     VABCMR        SUMCMR
500600090211     C                   Z-ADD     0             SUMNFE
500700090211     C                   Z-ADD     VABCCM        VADCCM
500800090213     C                   WRITE     EDSUM000                             99
500900090213     c   99              eval      errori_in_Wrt = 'S'
501000090211     C                   END
501100090211     C*
501200090211     C                   ENDSR
501300090211     c*==================================================================*
501400090211      * Manda un Msg in Posta AS Sede a EDP*
501500090211     c*==================================================================*
501600090211     c     SEND_MSG_EDP  begsr
501700090211      *
501800090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
501900090211      *   Lo manda una sola volta per lavoro
502000090211     c                   IF        Snd_Msg_alert  <> 'N' and
502100090211     c                             User_msg <> *blank
502200090211     c                   z-add     1             Jx
502300090211     C                   exsr      CalQEZSNDMG
502400090211     c                   end
502500090211      *
502600090211     c                   endsr
502700090213     c**********************************************************************
502800090213     c* reperimento numero Spedizione
502900090213     c**********************************************************************
503000090213     C     repnum        BEGSR
503100090213      *
503200090213      *    deve controllare sulla tabella "3C" se il numero spedizione
503300090213      *     deve essere mantenuto oppure no
503400090213     C*
503500090213     C                   clear                   Ds3C
503600090213     C                   Z-ADD     CODUT         TBLKUT
503700090213     C                   MOVEL     '3C'          TBLCOD
503800090213     C                   movel(p)  VABccm        TBLKEY
503900090213     C     KTABel        CHAIN     TABEL00F                           30
504000090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
504100090213     C                   MOVEL     TBLUNI        Ds3C
504200090213     C                   END
504300090213      *
504400090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
504500090213     c                   if        §3CFsp <> 'S'
504600090213      *
504700090213     C* NSP => Stacco un numeratore da AZNUM
504800090213     c                   movel     kpjbu         svkpjbu
504900090213     c                   clear                   kpjbu
505000090213     C                   clear                   TRUL33DS
505100090213     C                   eval      I33OPE = *zeros
505200090213     C                   eval      I33CNU = 302
505300090213     C                   eval      I33NUM = 1
505400090213     C                   movel     TRUL33DS      KPJBU
505500090213     C                   call      'TRUL33R'
505600090213     C                   parm                    KPJBA
505700090213     C                   movel     KPJBU         TRUL33DS
505800090213     c                   movel     svkpjbu       kpjbu
505900090213      ****
506000090213     C                   if        O33ERR = *zeros
506100100630     c                   z-add     o33nrf        NUM_Spediz
506200090213     C                   else
506300090213     C                   endif
506400090213      ****
506500090213     c                   ELSE
506600090213      *   se si vuole mantenere il numero spedizione
506700090213      ****
506800090213     C                   MOVEL     WOGGI         WANNO             4 0
506900090213     C                   MOVE      WANNO         KAAA
507000090213     C                   Z-ADD     3             KCNU
507100090213     C                   Z-ADD     VABlnp        KFIL
507200090213     C     KNUF          CHAIN     FLNUF                              9999
507300090213     C     *IN99         IFEQ      '0'
507400090213     C                   ADD       1             NUFNUM
507500100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
507600090213     C                   UPDATE    FLNUF
507700090213     C                   ELSE
507800090213     C                   END
507900090213     C*
508000090213     c                   EndIF
508100090213      **
508200090213     C                   ENDSR
508300090213     C*----------------------------------------------------------------
508400090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
508500090213     C*----------------------------------------------------------------
508600090213     C     MEMCMR        BEGSR
508700090213     C*
508800090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
508900090213     C     §PTCM1        IFEQ      'S'
509000090213     C                   CLEAR                   EDRDE000
509100090213     C                   MOVEL     'TD1154'      KNMC
509200090213     C                   Z-ADD     1             KNRP
509300100701     C                   MOVEL     PT_vabCCM     KLNP1
509400090213     C     KRDE          CHAIN     EDRDE01L                           31
509500090213     C     *IN31         IFEQ      '1'
509600090213     C                   Z-ADD     VABAAS        RDEAAS
509700090213     C                   Z-ADD     VABLNP        RDELNP
509800090213     C                   Z-ADD     VABNRS        RDENRS
509900090213     C                   Z-ADD     VABNSP        RDENSP
510000090213     C                   MOVEL     'TD1154'      RDENMC
510100090213     C                   Z-ADD     1             RDENRP
510200100706     C                   MOVEL     BGM_nrCMR     RDEVAL
510300100701     c                   if          RFF_NrCMR <> *Blank
510400100701     C                   MOVEL     RFF_NrCMR     RDEVAL
510500100701     c                   end
510600090213     C                   WRITE     EDRDE000                             99
510700090213     c   99              eval      errori_in_Wrt = 'S'
510800090213     C                   END
510900090213     C*  Memorizzo qualificatore CMR
511000090213     C                   CLEAR                   EDRDE000
511100090213     C                   MOVEL     'TD1153'      KNMC
511200090213     C                   Z-ADD     1             KNRP
511300100701     C                   MOVEL     PT_vabCCM     KLNP1
511400090213     C     KRDE          CHAIN     EDRDE01L                           31
511500090213     C     *IN31         IFEQ      '1'
511600090213     C                   Z-ADD     VABAAS        RDEAAS
511700090213     C                   Z-ADD     VABLNP        RDELNP
511800090213     C                   Z-ADD     VABNRS        RDENRS
511900090213     C                   Z-ADD     VABNSP        RDENSP
512000090213     C                   Z-ADD     1             RDENRP
512100090213     C                   MOVEL     'TD1153'      RDENMC
512200090213     C                   MOVEL     'CMR'         RDEVAL
512300090213     C                   WRITE     EDRDE000                             99
512400090213     c   99              eval      errori_in_Wrt = 'S'
512500090213     C                   END
512600090213     C*  Memorizzo la data
512700090213     C                   CLEAR                   EDRDE000
512800090213     C                   MOVEL     'TD2380'      KNMC
512900090213     C                   Z-ADD     1             KNRP
513000100701     C                   MOVEL     PT_vabCCM     KLNP1
513100090213     C     KRDE          CHAIN     EDRDE01L                           31
513200090213     C     *IN31         IFEQ      '1'
513300090213     C                   Z-ADD     VABAAS        RDEAAS
513400090213     C                   Z-ADD     VABLNP        RDELNP
513500090213     C                   Z-ADD     VABNRS        RDENRS
513600090213     C                   Z-ADD     VABNSP        RDENSP
513700090213     C                   Z-ADD     1             RDENRP
513800090213     C                   MOVEL     'TD2380'      RDENMC
513900100630     C                   MOVEL     DTM_DtCMR     RDEVAL
514000090213     C                   WRITE     EDRDE000                             99
514100090213     c   99              eval      errori_in_Wrt = 'S'
514200090213     C                   END
514300090213     C*
514400090213     C                   END                                                    §PTCM1 = 'S'
514500090213     C*
514600090213     C                   ENDSR
514700090213     C*----------------------------------------------------------------
514800090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
514900090213     C*----------------------------------------------------------------
515000090213     C     MEMAFB        BEGSR
515100090213     C*
515200090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
515300090213     C     §PTNF1        IFEQ      'S'
515400090213     C                   CLEAR                   EDRDE000
515500100701     C                   MOVEL     PT_vabCCM     KLNP1
515600090213     C                   MOVEL     'TD1154'      KNMC
515700090213     C                   Z-ADD     1             KNRP
515800090213     C     KRDE          CHAIN     EDRDE01L                           31
515900090213     C     *IN31         IFEQ      '1'
516000090213     C                   Z-ADD     VABAAS        RDEAAS
516100090213     C                   Z-ADD     VABLNP        RDELNP
516200090213     C                   Z-ADD     VABNRS        RDENRS
516300090213     C                   Z-ADD     VABNSP        RDENSP
516400090213     C                   MOVEL     'TD1154'      RDENMC
516500100701     C                   if        §PTCM1 <> 'S' or
516600100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
516700090213     C                   Z-ADD     1             RDENRP
516800090213     C                   ELSE
516900090213     C                   Z-ADD     2             RDENRP
517000090213     C                   END
517100100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
517200100701     c                   if          RFF_NrFviaggio <> *Blank
517300100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
517400100701     c                   end
517500090213     C                   WRITE     EDRDE000                             99
517600090213     c   99              eval      errori_in_Wrt = 'S'
517700090213     C                   END
517800090213     C*  Memorizzo qualificatore CMR
517900090213     C                   CLEAR                   EDRDE000
518000100701     C                   MOVEL     PT_vabCCM     KLNP1
518100090213     C                   MOVEL     'TD1153'      KNMC
518200090213     C                   Z-ADD     1             KNRP
518300090213     C     KRDE          CHAIN     EDRDE01L                           31
518400090213     C     *IN31         IFEQ      '1'
518500090213     C                   Z-ADD     VABAAS        RDEAAS
518600090213     C                   Z-ADD     VABLNP        RDELNP
518700090213     C                   Z-ADD     VABNRS        RDENRS
518800090213     C                   Z-ADD     VABNSP        RDENSP
518900100701     C                   if        §PTCM1 <> 'S' or
519000100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
519100090213     C                   Z-ADD     1             RDENRP
519200090213     C                   ELSE
519300090213     C                   Z-ADD     2             RDENRP
519400090213     C                   END
519500090213     C                   MOVEL     'TD1153'      RDENMC
519600090213     C                   MOVEL     'AFB'         RDEVAL
519700090213     C                   WRITE     EDRDE000                             99
519800090213     c   99              eval      errori_in_Wrt = 'S'
519900090213     C                   END
520000090213     C*  Memorizzo la data
520100090213     C                   CLEAR                   EDRDE000
520200100701     C                   MOVEL     PT_vabCCM     KLNP1
520300090213     C                   MOVEL     'TD2380'      KNMC
520400090213     C                   Z-ADD     1             KNRP
520500090213     C     KRDE          CHAIN     EDRDE01L                           31
520600090213     C     *IN31         IFEQ      '1'
520700090213     C                   Z-ADD     VABAAS        RDEAAS
520800090213     C                   Z-ADD     VABLNP        RDELNP
520900090213     C                   Z-ADD     VABNRS        RDENRS
521000090213     C                   Z-ADD     VABNSP        RDENSP
521100100701     C                   if        §PTCM1 <> 'S' or
521200100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
521300090213     C                   Z-ADD     1             RDENRP
521400090213     C                   ELSE
521500090213     C                   Z-ADD     2             RDENRP
521600090213     C                   END
521700090213     C                   MOVEL     'TD2380'      RDENMC
521800100630     C                   MOVEL     DTM_DtFViag   RDEVAL
521900090213     C                   WRITE     EDRDE000                             99
522000090213     c   99              eval      errori_in_Wrt = 'S'
522100090213     C                   END
522200090213     C*
522300090213     C                   END                                                    §PTCM1 = 'S'
522400090213     C*
522500090213     C                   ENDSR
522600090213     c*==================================================================*
522700090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
522800090213     c*==================================================================*
522900090213     c     Imp_ADDR_mail begsr
523000090213      *
523100090213     c                   clear                   Indirizzi
523200090213      *
523300090213      *  Lunghezza indirizzi presenti
523400100630     c                   eval      Lunghezza_Indirizzo =
523500100630     c                                       %Len(%TrimR(Mail_Ind))
523600090213     c                   z-add     1             al_char           3 0
523700090213     c                   z-add     1             dal_char          3 0
523800090213     c                   z-add     1             tot_char          3 0
523900090213      *
524000090213     c     successivo    tag
524100090213      *
524200090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
524300090213      *   e aggiunge il dominio Bartolini + il (;) separatore
524400090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
524500090213     c                   eval      tot_char = al_char - dal_char
524600100630     c                   z-add     dal_char      dalCarattere
524700100630     c                   z-add     tot_char      xTOTcaratteri
524800090213      *
524900090213     c                   clear                   Indir_Bartol     40
525000090213     c                   clear                   Indir_Parz       20
525100100630     c                   eval      Indir_Parz =
525200100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
525300100630      *
525400090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
525500090213     c                             %Trim(bart_it)
525600090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
525700090213      *
525800100630     c                   if        al_char = Lunghezza_Indirizzo
525900090213     c                   goto      fine_indmail
526000090213     c                   else
526100090213     c                   eval      dal_char = ( al_char + 1 )
526200090213     c                   goto      successivo
526300090213     c                   end
526400090213      *
526500090213     C     fine_indmail  TAG
526600090213      *
526700090213     C                   ENDSR
526800090213     c*==================================================================*
526900090213      * Manda un Msg x E-mail
527000090213     c*==================================================================*
527100090213     c     Invio_Mail    begsr
527200090213      *
527300090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
527400090213     C* Inizializzo variabili
527500090213     C                   movel     *blanks       wrkEml          253
527600090213     C                   movel     *blanks       wrkMsg         5000
527700090213     C                   movel     *blanks       wrkOgg           44
527800090213      *
527900090213     C* Valorizzo i campi della e-m@ail - indirizzo
528000100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
528100090213     C                   eval      wrkEml = Indirizzi
528200090213     C                   eval      wrkOgg = Oggetto
528300090213     C                   eval      wrkMsg = Messaggio
528400110203      ********
528500110203     C********           call(e)   'TIS701C'
528600110203     C********           parm                    wrkEml
528700110203     C********           parm                    wrkOgg
528800110203     C********           parm                    wrkMsg
528900110203      ** *****
529000110203     C                   call(e)   'TRTCT00R2'
529100110203     C                   parm                    wrkEml
529200110203     C                   parm                    wrkOgg
529300110203     C                   parm                    wrkMsg
529400090213     C*
529500090213     C                   ENDSR
529600090211     ***********************************************************************
529700090211     **
529800090211     ** Send Message (QEZSNDMG) API
529900090211     **
530000090211     ***********************************************************************
530100090211     C     CalQEZSNDMG   BEGSR
530200090211      *
530300090211     ** Invio messaggio all'utente.
530400090211     C                   EVAL      SndMg03 = msgDSP
530500090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
530600090211     C                   EVAL      SndMg05(Jx) = User_msg
530700090211     C                   EVAL      SndMg06 = Jx
530800090211     C                   CLEAR                   QUSEC
530900090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
531000090211
531100090211     C                   CALL      'QEZSNDMG'
531200090211     C                   PARM                    SndMg01
531300090211     C                   PARM                    SndMg02
531400090211     C                   PARM                    SndMg03
531500090211     C                   PARM                    SndMg04
531600090211     C                   PARM                    SndMg05
531700090211     C                   PARM                    SndMg06
531800090211     C                   PARM                    SndMg07
531900090211     C                   PARM                    SndMg08
532000090211     C                   PARM                    Qusec
532100090211     C                   PARM                    SndMg10
532200090211     C                   PARM                    SndMg11
532300090211     C                   PARM                    SndMg12
532400090211      *
532500090211     C                   ENDSR
532600100708      * ?------------------------------------------------------------------ */
532700100708      *  Controlla la trasmissione   se completa
532800100708      * ?------------------------------------------------------------------ */
532900100708     c     Check_Trasm   Begsr
533000100708
533100100708     C                   clear                   se_errore
533200100708     C                   clear                   Riga_iNIZIo       1
533300100708     C                   clear                   Riga_fINe         1
533400100708      ** primo  record
533500100708     c     *start        setll     tivin00r
533600100708      *
533700100708     c     rileggi       tag
533800100708     c                   EXSR      LEGGE_TIVIN_R
533900100708      *
534000100708     c                   if        EoFTivin <> 'S'
534100100708      *
534200100708     c                   if        dati = *blank
534300100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
534400100708      * le leggo e le escludo.
534500100708     C                   eval      vinFLG = '1'
534600100708     c                   update    Tivin000
534700100708     c                   goto      rileggi
534800100708     c                   end
534900100708      * ?              /*---------------------- */
535000100708     c                   exsr      Decod_Segmento
535100100708      * ?              /*---------------------- */
535200100708     c                   endif
535300100708      **
535400100708      ** ultimo record
535500100708     c     *hival        setll     tivin00r
535600100708     c     leggiAncora   tag
535700100708     c                   readp     tivin00r
535800100708     c                   if        %Eof(tivin00r)
535900100708     c                   move      'S'           EoFTivin
536000100708     c                   else
536100100708     c                   clear                   dati
536200100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
536300100708      *
536400100708     c                   if        dati = *blank
536500100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
536600100708      * le leggo e le escludo.
536700100708     C                   eval      vinFLG = '1'
536800100708     c                   update    Tivin000
536900100708     c                   goto      leggiAncora
537000100708     c                   end
537100100708      * ?              /*---------------------- */
537200100708     c                   exsr      Decod_Segmento
537300100708      * ?              /*---------------------- */
537400100708     c                   endif
537500100708
537600100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
537700100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
537800100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
537900100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
538000100708      * ?-----> Errore
538100100708     C                   eval      se_errore = 'S'
538200100708     c                   end
538300100811      *
538400100708     c                   endSR
538500100708      * ?------------------------------------------------------------------ */
538600100811      *
538700100811      * ?------------------------------------------------------------------ */
538800100811     c     chiama_DecoderbegSR
538900100811      *
539000100811     C                   clear                   keyUNBCLI
539100100811     C                   clear                   keyTIPOMSG
539200100811     C                   clear                   keyVERSION
539300100811     C                   clear                   keyRELEASE
539400100811     C                   clear                   keyAGENCY
539500100811     C                   clear                   keyASSOCIA
539600110530      **--------
539700110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
539800110530      **--------
539900110530     c                   movel     UNB_Mittente  keyUNBCLI
540000100811      **--------
540100100811      *  Ritorna la DS GENERICA oppure l'errore
540200100811      **--------
540300100811      *  Chiamata generica x decodificare il singolo segmento letto
540400100811     c                   call      'TRTCT00R1'
540500100811      * input
540600100811     c                   parm                    tipo_seg
540700100811     c                   parm                    segmento
540800100811     C                   parm                    keyUNBCLI
540900100811     C                   parm      'IFCSUM'      keyTIPOMSG
541000100811     C                   parm      'D'           keyVERSION
541100100811     C                   parm      '96A'         keyRELEASE
541200100811     C                   parm      'UN'          keyAGENCY
541300100811     C                   parm                    keyASSOCIA
541400100811      *output
541500100811     c                   parm                    Errore
541600100811     c                   parm                    DsGenerica
541700100811     c                   parm                    Valori_inErr
541800100811     c                   parm                    Descr_Errore
541900100811     c                   parm                    trovato_seg
542000100811      *
542100100811     c                   endSR
542200110613      * ?------------------------------------------------------------------ */
542300110613      *
542400100811      * ?------------------------------------------------------------------ */
542500110613     c     Instrada_sped begsr
542600110613      *
542700110613      * -------------
542800110613     C* Ricerco CAP in base alle dimensioni
542900110613     C     Wcap          IFne      *BLANKS
543000160610      *
543100160610      *________ATTENZIONE MANDA IL CAP CON ZERI DAVANTI__________
543200160610     C                   IF        %SUBST(WCAP:9:1)<>' ' AND
543300160610     C                             %SUBST(WCAP:1:4) ='0000'
543400160610     c                   eval       Wcap = %SUBST(WCAP:5:5)
543500160610     c                   end
543600160610      *________ATTENZIONE MANDA IL CAP CON ZERI DAVANTI__________
543700110613      *
543800110613      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
543900110613     C                   CLEAR                   FNLV55DS
544000110613     C                   MOVEL     'P'           D55TPT
544100110613     C                   MOVEL     WOGGI         D55DRF
544200110613     C                   MOVEL     VABLNP        D55LNP
544300110613     C                   MOVEL     VABLNP        D55LIN
544400110613     C                   CALL      'FNLV55R'
544500110613     C                   PARM                    FNLV55DS
544600110613     C                   MOVE      D55TFP        COMTFP            3 0
544700110613      *
544800110613     C*  PASSO IL TERMINAL PARTENZA
544900110613     C                   CLEAR                   TISI95DS
545000110613     C                   Z-ADD     COMTFP        I95TFP
545100110613     C                   MOVEL     VABTSP        I95TSP
545200110613     C                   MOVEL     'S'           I95FRE
545300110613     C                   MOVEL     '3'           I95TCN
545400110613     C                   MOVEL     WCAP          I95CAP
545500110613     C                   MOVEL     VABNZD        I95NAR
545600110613     C                   MOVEL     VABLOD        I95LOC
545700110613     C                   MOVEL     VABIND        I95IND
545800110613      *
545900110613     C* Se gestita seconda linea di arrivo passo peso - volume
546000110613     C* e numero colli effettivo
546100110613     C     §PULNA        IFEQ      'S'
546200110613     C                   Z-ADD     VABPKB        I95LKG
546300110613     C                   Z-ADD     VABVLB        I95LMC
546400110613     C                   END
546500110613     C*
546600110613     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
546700110613     C                   SELECT
546800110613     C     VABCBO        WHENEQ    '1 '
546900110613     C                   MOVEL     *BLANKS       I95FCA
547000110613     C                   MOVEL     'F'           I95TPO
547100110613     C     VABCBO        WHENEQ    '2 '
547200110613     C                   MOVEL     *BLANKS       I95FCA
547300110613     C                   MOVEL     'A'           I95TPO
547400110613     C     VABCBO        WHENEQ    '4 '
547500110613     C                   MOVEL     'S'           I95FCA
547600110613     C                   MOVEL     'F'           I95TPO
547700110613     C     VABCBO        WHENEQ    '6 '
547800110613     C                   MOVEL     'S'           I95FCA
547900110613     C                   MOVEL     'A'           I95TPO
548000110613     C                   OTHER
548100110613     C                   MOVEL     'F'           I95TPO
548200110613     C     VABCAS        IFGT      0
548300110613     C                   MOVEL     'S'           I95FCA
548400110613     C                   ELSE
548500110613     C                   MOVEL     *BLANKS       I95FCA
548600110613     C                   END
548700110613     C                   ENDSL
548800110613     C*
548900110613     C                   CALL      'TISI95R'
549000110613     C                   PARM                    TISI95DS
549100110613     C*
549200110613     C* Reperisco i dati da CAP
549300110613     C                   MOVEL     O95PRV        VABPRD
549400110613     C                   MOVEL     WCAP          VABCAD
549500110613      ***
549600110613     c                   MOVEL     O95LNA        VABLNA
549700110613     C                   MOVEL     O95ZNC        VABZNC
549800110613     C*
549900110613     C     O95ERR        IFNE      *BLANKS
550000110613     C     O95FIT        ANDEQ     'S'
550100110613     C*  Cap non trovato
550200110613     C********           eval      vinMSG = 'MEA (CAP) non trovato'
550300110613     c********           exsr      Error_DET
550400110613     C********           goto      end_MEA
550500110613     C                   END
550600110613      *
550700110613     C                   ELSE
550800110613      *
550900110613     C                   CLEAR                   VABPRD
551000110613     C                   CLEAR                   VABLNA
551100110613     C                   CLEAR                   VABZNC
551200110613     C                   CLEAR                   VABCAD
551300110613     C*  Cap non trovato
551400110613     C*********          eval      vinMSG = 'MEA (CAP) mancante'
551500110613     c*********          exsr      Error_DET
551600110613     C*********          goto      end_MEA
551700110613     C                   END
551800110613      *
551900110613     c                   endSR
552000110613      * ?------------------------------------------------------------------ */
552100111028     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
552200111028      * ?------------------------------------------------------------------ */
552300111028     C     Write_RESO    BEGSR
552400111028      *
552500111028      *  Imposta nella descrizione del CMR l'intestazione di RESI
552600111028     c                   movel(p)  vabCMR        campo35          35
552700111028     c                   movel(p)  'RESI__'      vabCMR
552800111028     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
552900111028      *
553000111028      *  e sulla 2° parte della ragione sociale viene apposta la dicitura di RESO
553100111028     c                   move(p)   VABrsd        campo44          44
553200111028     c                   movel     '**RESO** '   campo44
553300111028     C                   movel     campo44       VABrsd
553400111028     C                   move      campo44       campo9            9
553500111028      *
553600111028      * Sposta tutta la ragione sociale di (9 carat.) anche sulla 2°ragione sociale
553700111028     c                   move(p)   VABrd2        campo44          44
553800111028     c                   movel     campo9        campo44
553900111028     C                   movel     campo44       VABrd2
554000140610      *
554100140610      *  e sulle NOTE  viene impostato "Sp.EXP:"
554200140610     c                   clear                   campo57          57
554300140610     c                   eval      campo57 = 'ExSped.' + %trim(cod_RESO_ORM)
554400140610     c                   move      VABnot        campo57
554500140610     C                   movel     campo57       VABnot
554600140610     C                   move      campo57       campo22          22
554700140610      *
554800140610      * Sposta tutta la ragione sociale di (x carat.) anche sulla 2°ragione sociale
554900140610     c                   move(p)   VABnt2        campo57
555000140610     c                   movel     campo22       campo57
555100140610     C                   movel     campo57       VABnt2
555200111028      *
555300111028     c                   endSR
555400111028      * ?------------------------------------------------------------------ */
555500111028     C*? Controllo se era prima una Bolla di Export che sta rientrando
555600111028      * ?------------------------------------------------------------------ */
555700111028     C     ctrl_seERAexp Begsr
555800111028     C*
555900111028     C*   imposta la chiave per controllare la spedizione
556000111028     C*    in sede
556100111028     C                   movel     '20'          dsspe
556200111028     C*
556300111028     C                   move      SPEAAS        TASAAS
556400111028     C                   move      SPELNP        TASLNP
556500111028     C                   move      SPENRS        TASNRS
556600111028     C                   move      SPENSP        TASNSP
556700111028     C     KTAS          chain     titas30c
556800111028     c                   if        %found(titas30c)
556900111028     c                               and tasLNA >299 and tasLNA <=400
557000111028     c                   eval      era_un_export_RESO_dal_PARTNER = 'S'
557100111028     c                   eval                           Sped_RESO = 'S'
557200111028     c                   end
557300111028     C*
557400111028     C                   ENDSR
557500140610      * ?------------------------------------------------------------------ */
557600140610     C*? Controllo se si tratta di un RESO mediante il rif.mitt.NUMERICO
557700140610      * ?------------------------------------------------------------------ */
557800140610     C     Write_da_ORM  BEGSR
557900140610      *
558000140610      *  Imposta nella descrizione del CMR l'intestazione di RESI
558100140610     c                   movel(p)  vabCMR        campo35          35
558200140610     c                   movel(p)  'DaORM_'      vabCMR
558300140610     c                   eval      vabCMR = %trim(vabCMR) + %trim(campo35)
558400140623      *
558500140623      *  e sulle NOTE  viene impostato da ORM con il codice dell'ORM generante
558600140623     c                   clear                   campo70          70
558700140623     c                   clear                   note_70          70
558800140623     c                   eval      campo70 = 'DaORM:' + %Trim(salva_REF_ORM)
558900140623     c                   eval      note_70 = %trim(VABNOT) +'/'+ %trim(VABNT2)
559000140623     c                   eval      campo70 = %Trim(campo70)+'/'+ %Trim(note_70)
559100140623     c                   eval      VABNOT = %subst(campo70:1:35)
559200140623     c                   eval      VABNT2 = %subst(campo70:36:35)
559300140610      *
559400140610     c                   endSR
559500111028      * ?------------------------------------------------------------------ */
559600120629     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
559700120629      * ?------------------------------------------------------------------ */
559800120629     C     CL_ParticolarebegSR
559900120629      *
560000120629     C     Rifer_Cliente IFNE      *BLANKS
560100120629      * ------>>>
560200120629     C                   eval      XX = 1
560300120629     C                   movel     §PTKSC        Key_CL           35
560400120629     C                   movel     Rifer_Cliente WCOM28           28
560500120629     C                   move      WCOM28        Key_CL
560600120629      *
560700120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
560800120629      *
560900120629      * ?Se il Partner è uno di quelli che gestisce + linee quindi ha un UNB parziale
561000120629      * ?potrebbe non agganciare correttamente il cliente quindi occorre decodificare
561100120629      * ?il dato cercandolo anche con gli altri codici KSC del Partner Generico ossia
561200120629      * ?con i primi 31 caratteri dell'UNB.
561300120629     c  N34              if        UNB_parziale ='S' and
561400120629     c                              PT_con_piu_Nazioni = 'S'
561500120629     c                   do        PT_KeyXsav    x
561600120629      *
561700120629     c                   if        PT_Parz(x) = UNB_generico
561800120629      * ------>>>
561900120629      * -- Il problema è che potrebbe avere lo stesso codice FF uguale
562000120629      * --   su più records dello stesso partner multinazione
562100120629     C                   eval      XX = 1
562200120629     C                   movel     PT_KSC(X)     Key_CL           35
562300120629     C                   movel     Rifer_Cliente WCOM28           28
562400120629     C                   move      WCOM28        Key_CL
562500120629      *
562600120629     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
562700120629      * se trovato esce immediatamente
562800120629     c   34              leave
562900120629     c                   end
563000120629      *
563100120629     C                   endDO
563200120629     c                   endIF
563300120629      *
563400120629      *   Se trovato vale x tutto il messaggio
563500120629     C     *IN34         IFEQ      '1'
563600120629     C                   movel     Des_Tb_CL(XX) EDIDSCL
563700120629      *
563800120629      * in testata messaggio
563900120629     c                   if        Dati_di_Testata  ='S'
564000120629     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
564100120629     C                   movel     §CLtar        Tes_RFF_FF_tar    3
564200120629     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
564300160209      *-
564400160209     C                   if        §CLftx   <> *blank
564500160209     C                   move      §CLftx        Tes_RFF_FF_fgs    3
564600160209     c                   else
564700160209     C                   move      §CLlnp        Tes_RFF_FF_fgs
564800160209     c                   end
564900160209      *-
565000120629     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
565100120629     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
565200120629     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
565300120629     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
565400120629     C                   movel     §CLtic        Tes_RFF_FF_tic    2
565500120629     c                   else
565600120629      * in dettaglio Messaggio
565700120629     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
565800120629     C                   movel     §CLtar        Det_RFF_FF_tar    3
565900120629     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
566000160209      *-
566100160209     C                   if        §CLftx   <> *blank
566200160209     C                   move      §CLftx        Det_RFF_FF_fgs    3
566300160209     c                   else
566400160209     C                   move      §CLlnp        Det_RFF_FF_fgs
566500160209     c                   end
566600160209      *-
566700120629     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
566800120629     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
566900120629     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
567000120629     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
567100120629     C                   movel     §CLtic        Det_RFF_FF_tic    2
567200120629     c                   end
567300120629      *
567400120629      * imposta la LNP  e la Serie x il dettaglio specifico
567500120629     c
567600120629      *  In base al cod.trattamento merce reperisco tipo tratt.
567700120629      *    per un dettaglio specifico.
567800120629     C                   CLEAR                   DS1B
567900120629     C                   Z-ADD     1             Y                 3 0
568000120629     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
568100120629     C     *IN32         IFEQ      '1'
568200120629     C                   eval       DS1B = Des_Tb_CTM(Y)
568300120629     C                   END
568400120629      *
568500120629      * Se il cliente non era sulla schiera lo aggiunge
568600120629     C     §CLksc        lookup    skCLienti                              39
568700120629     C                   if        *in39 = *off
568800120629     C                   add       1             sk
568900120629     C                   z-add     §CLksc        skCLienti(sk)
569000120629     c                   endif
569100120629      *
569200120629     C                   ENDIF
569300120629     C                   ENDIF
569400120629      *
569500120629      * in Dettaglio  preimposto i campi rilevati dalle tabelle
569600120629      *   per clienti particolari (CL)
569700120629      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
569800120629      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
569900120629      *   successivi per altre decodifiche.
570000120629     c                   if        Dati_di_Testata <>'S'
570100120629      *
570200120629      *   se aveva un RFF+FF in dettaglio
570300120629     C                   if         Det_RFF_FF_ksc > 0
570400120629      *
570500120629     C                   eval      VABccm = Det_RFF_FF_ksc
570600120629     c                   eval      VABlnp = Det_RFF_FF_lnp
570700120629     c                   eval      VABnrs = Det_RFF_FF_nrs
570800120629     C                   eval      VABctm = Det_RFF_FF_ctm
570900120629     C                   movel     Det_RFF_FF_tarVABctr
571000160209     C                   move      Det_RFF_FF_fgsVABfgs
571100120629      *
571200120629      *   altrimenti se aveva un RFF+FF in Testata
571300120629     c                   elseif     Tes_RFF_FF_ksc > 0
571400120629      *
571500120629     C                   eval      VABccm = Tes_RFF_FF_ksc
571600120629     c                   eval      VABlnp = Tes_RFF_FF_lnp
571700120629     c                   eval      VABnrs = Tes_RFF_FF_nrs
571800120629     C                   eval      VABctm = Tes_RFF_FF_ctm
571900120629     C                   movel     Tes_RFF_FF_tarVABctr
572000160209     C                   move      Tes_RFF_FF_fgsVABfgs
572100120629      *
572200120629     c                   end
572300120629      *>>>>>>>>
572400120629     c                   ENDif
572500120629      *
572600120629     C                   ENDSR
572700121105      *---------------------------------------------------------------*
572800121105      * CTRL caricamento schiere    PT                               -*
572900121105      *---------------------------------------------------------------*
573000121105     C     Check_PT      begsr
573100121105     C*
573200121105     c* Controllo riempimento schiera
573300121105     c                   clear                   trul0sds
573400121105     c                   eval      i0sski='PT_key'
573500121105     c                   eval      i0sele=%elem(PT_key)
573600121105     c                   eval      i0spie=x
573700121105     c                   eval      i0sfile='EDTAB00F'
573800121105     c                   eval      i0ssif=knsif
573900121105     c                   eval      i0spgm='TRTCT96R'
574000121112     c                   move      kpjbu         SvKpjbu
574100121112     c                   clear                   kpjbu
574200121105     c                   movel     trul0sds      kpjbu
574300121105     c                   call      'TRUL0SR'
574400121105     c                   parm                    kpjba
574500121112     c                   eval      kpjbu = SvKpjbu
574600121105     C*
574700121105     C                   ENDSR
574800120629      * ?------------------------------------------------------------------ */
574900090211     O*****************************************************************
575000090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
575100090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
575200090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
575300090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
