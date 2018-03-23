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
002200090209      *
002300090213     FEDiVAB1L  UF A E           K DISK    commit
002400090213     FEDiVAD0F  O  A E           K DISK    commit
002500090213     FEDiVAT0F  O  A E           K DISK    commit
002600090209      *
002700090213     FEDSUM00F  O  A E           K DISK    commit   INFDS(SRECDS)
002800100701      *----------------------------------------------------*
002900100701      * Stringhe per conversione alfanumerico/numerico N.Documento
003000100701     D CA              S             78    DIM(1) CTDATA PERRCD(1)
003100100701     D CN              S             78    DIM(1) CTDATA PERRCD(1)
003200940321      *----------------------------------------------------*
003300090209      * numero relativo di record
003400090209     D SRECDS          DS
003500090209     D  NRREC                397    400B 0
003600090209      *
003700090213      * numero relativo di record
003800090213     D VINRECDS        DS
003900090213     D  VNREC                397    400B 0
004000100701      * ---------------------------------------------------
004100100701     D KPJBA         E DS
004200100701     D     svkpjbu     s                   like(kpjbu)
004300100701     D FNLV55DS      E DS
004400100701     D TIBS55DS      E DS
004500100701     D TISI95DS      E DS
004600100701     D UTEDSE0F      E DS
004700100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004800100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
004900100701      * Ds scomposzione tipo capoconti
005000100701     D TCUDS           DS
005100100701     D  F34                    3      4
005200100701     D  F56                    5      6
005300100701      *
005400100701      * Numeratore Bolle (302)
005500100701     D trul33ds      E DS
005600100701     D Ds3C          E DS
005700100701     D DS1B          E DS
005800100701     d sav_DS1B        s                   like(Ds1B)
005900100701     D DS15          E DS
006000100701     D DSCV          E DS
006100100701     D TRTC86DS      E DS
006200100701     D EDIDSPT       E DS
006300100701     D EDIDSPU       E DS
006400100701     D EDIDSPV       E DS
006500100701     D EDIDSCL       E DS
006600100701     D EDIDSSS       E DS
006700100701     D EDIDSTC       E DS
006800100701      **
006900100701      *  DS x salvataggio dati impostati in EDiVAB
007000100701     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
007100100701     D SALVA           DS           545
007200100701      **
007300100701     D WLBDA8          DS
007400100701     D  G02DAT                 1      8  0
007500100701     D  G02INV                 9     16  0
007600100701     D  G02ERR                17     17
007700100701     D  G02TGI                18     22  0
007800090213      *
007900100701      *----------------------------------------------------*
008000090205      **  Elenco DS di tutti i Segmenti da tradurre
008100100701      *----------------------------------------------------*
008200100811???? D eds96BGM      E DS
008300100811???? D eds96CNI      E DS
008400100811???? D eds96CNT      E DS
008500100811???? D eds96COM      E DS
008600100811???? D eds96CTA      E DS
008700100811???? D eds96DGS      E DS
008800100811???? D eds96DIM      E DS
008900100811???? D eds96DOC      E DS
009000100811???? D eds96DTM      E DS
009100100811???? D eds96EQD      E DS
009200100811???? D eds96EQN      E DS
009300100811???? D eds96FTX      E DS
009400100628???? D  D05                   16    365    DIM(5)
009500100811???? D eds96GID      E DS
009600100811???? D eds96GIN      E DS
009700100920???? D  GIN_10                 4    353    DIM(10)
009800100811???? D eds96GOR      E DS
009900100811???? D eds96HAN      E DS
010000100811???? D eds96LOC      E DS
010100100811???? D eds96MEA      E DS
010200100811???? D eds96MOA      E DS
010300100811???? D eds96NAD      E DS
010400100811???? D eds96PCI      E DS
010500100920???? D  PCI_10                 4    353    DIM(10)
010600100811???? D eds96PIA      E DS
010700100811???? D eds96RFF      E DS
010800100811???? D eds96RNG      E DS
010900100811???? D eds96SEL      E DS
011000100811???? D eds96TCC      E DS
011100100811???? D eds96TDT      E DS
011200100811???? D eds96TMD      E DS
011300100811???? D eds96TMP      E DS
011400100811???? D eds96TOD      E DS
011500100811???? D eds96TSR      E DS
011600100811???? D eds96UNA      E DS
011700100811???? D eds96UNB      E DS
011800100811???? D eds96UNH      E DS
011900100811???? D eds96UNT      E DS
012000100811???? D eds96UNZ      E DS
012100100701      *----------------------------------------------------*
012200090210      **
012300090210     D  X30            S             35    DIM(20)                              generico segnacolli
012400090205      **
012500090209     D Valori_inErr    ds
012600090209     D  SKout_Errori                  1    DIM(50)
012700100630      **
012800090209     D Descr_Errore    ds
012900090209     D  SKout_DesErr                 50    DIM(50)
013000090209      *
013100090205      *----------------------------------------------------*
013200091016     D EoFTivin        s              1
013300091016      *----------------------------------------------------*
013400090205     D Tipo_seg        S              3
013500100720     D Trovato_seg     S              1
013600100716      *
013700100716     d keyUNBCLI       s             35
013800100716     d keyTIPOMSG      s              6
013900100716     d keyVERSION      s              3
014000100716     d keyRELEASE      s              3
014100100716     d keyAGENCY       s              3
014200100716     d keyASSOCIA      s              6
014300100716      *
014400100720     D DsGenerica      s           2048
014500090209     D segmento        s           2048
014600090209     D Errore          s              1
014700100701      *
014800000223     D W0140           S             14  0
014900991129     D WORA            S              6  0
015000100701     D DataGGMMAAA     S              8  0
015100100701     D DATEU           S              8  0
015200100701     D DATA_eur        S               D   DATFMT(*eur)
015300100701      *
015400100630     D sk              S              3  0 INZ
015500100630     D sx              S              3  0 INZ
015600100628     D XX              S              3  0 INZ
015700110328     d Key_Generica35  s             35
015800100630     D NUM_Spediz      s                   LIKE(vabnsp)
015900100708     D UNT_record      s                   LIKE(vnREC)
016000100708     D IniDET_RECord   s                   LIKE(vnREC)
016100100708     D FinDET_RECord   s                   LIKE(vnREC)
016200090213     D Last_RECord     s                   LIKE(vnREC)
016300101222     D UNZ_SEGMENTO    s              1
016400100318      *------------------------------------------
016500100318      **  Definizione Qualificatori
016600100318      *------------------------------------------
016700100319      *
016800100628???? D Documento_Manifest...
016900100628???? D                 s              3    INZ('785')
017000100701???? D Totale_Peso_Lordo...
017100100701???? D                 s              3    INZ('7  ')
017200100701???? D Totale_Nr_Spedizioni...
017300100701???? D                 s              3    INZ('10 ')
017400100701???? D Totale_Nr_Colli...
017500100701???? D                 s              3    INZ('11 ')
017600100701???? D Totale_Peso_Netto...
017700100701???? D                 s              3    INZ('ZCW')
017800100628???? D Riferimento_Spedizione...
017900100628???? D                 s              3    INZ('AGE')
018000100708???? D Riferimento_Numerico...
018100100708???? D                 s              3    INZ('CU ')
018200100318???? D Riferimento_Cliente_particolare...
018300100318???? D                 s              3    INZ('FF ')
018400100701???? D X_Cargo_Manifest...
018500100318???? D                 s              3    INZ('AFB')
018600100701???? D X_Numero_CMR...
018700100318???? D                 s              3    INZ('CMR')
018800100318???? D Telephone_Number...
018900100318???? D                 s              3    INZ('TE ')
019000100318???? D TeleFax_Number...
019100100318???? D                 s              3    INZ('TX ')
019200100628???? D Porto_Assegnato...
019300100628???? D                 s              3    INZ('001')
019400100628???? D Porto_Franco...
019500100628???? D                 s              3    INZ('002')
019600100628???? D Porto_Franco_uncleared...
019700100628???? D                 s              3    INZ('003')
019800100628???? D Porto_Franco_domicile...
019900100628???? D                 s              3    INZ('004')
020000100318???? D Volume...
020100110126???? D                 s              3    INZ('AAE')
020200110126???? D Volume_W...
020300110126???? D                 s              3    INZ('AAW')
020400100705???? D Weight...
020500110126???? D                 s              3    INZ('AAE')
020600100319???? D G_Weight...
020700100319???? D                 s              3    INZ('G  ')
020800100318???? D Gross_Weight...
020900100318???? D                 s              3    INZ('AAG')
021000110126???? D Gross_Weight_D...
021100110126???? D                 s              3    INZ('AAD')
021200100706???? D N_Weight...
021300100706???? D                 s              3    INZ('N  ')
021400100706???? D Net_Weight...
021500100706???? D                 s              3    INZ('AAF')
021600100318???? D Kilograms...
021700100318???? D                 s              3    INZ('KGM')
021800100319???? D Grams...
021900100319???? D                 s              3    INZ('163')
022000100318???? D Meters...
022100100318???? D                 s              3    INZ('MTR')
022200100318???? D Cubic_Meters...
022300100318???? D                 s              3    INZ('MTQ')
022400100318???? D Shipper_assigned...
022500100318???? D                 s              3    INZ('24 ')
022600100705???? D Data_senza_ora...
022700100628???? D                 s              3    INZ('102')
022800100705???? D Data_con_ora...
022900100628???? D                 s              3    INZ('203')
023000100705???? D Data_con_i_secondi...
023100100705???? D                 s              3    INZ('204')
023200100630???? D Squadratura_Colli...
023300100630???? D                 s              1    INZ('N')
023400100316      *---------------------------------------------------------------------*
023500100316      **  segmenti Identificativi parti specifiche del messaggio
023600100316      *---------------------------------------------------------------------*
023700100701???? D Segm_Inizio_Messaggio...
023800100701???? D                 s              3    INZ('UNB')
023900100701???? D Segm_Testata_Dettagli...
024000100701???? D                 s              3    INZ('UNH')
024100100701???? D Segm_Inizio_Dettaglio...
024200100316???? D                 s              3    INZ('CNI')
024300100701???? D Segm_Fine_Dettagli...
024400100316???? D                 s              3    INZ('UNT')
024500100701???? D Segm_Fine_messaggio...
024600100316???? D                 s              3    INZ('UNZ')
024700100701???? D seg_imprevisto...
024800100701???? D                 s              1
024900100726???? D Forza_Uscita...
025000100726???? D                 s              1
025100100701      *---------------------------------------------------------------------*
025200100701???? D Dati_di_Testata...
025300100701???? D                 s              1    INZ('S')
025400100701???? D Contatore_Dettagli...
025500100701???? D                 s              5s 0 INZ(0)
025600100728???? D Segmento_in_errore...
025700100728???? D                 s              5s 0 INZ(0)
025800100701      *---------------------------------------------------------------------*
025900100910      *
026000100910      * Tabella partner
026100121105     D PT_key          S             35    DIM(200)
026200121105     D PT_Parz         S             35    DIM(%elem(PT_key))
026300121105     D PT_KSC          S              7    DIM(%elem(PT_key))
026400121105     D PT_uni          S             90    DIM(%elem(PT_key))
026500121105     D PU_uni          S             90    DIM(%elem(PT_key))
026600121105     D PV_uni          S             90    DIM(%elem(PT_key))
026700121105     D TRUL0SDS      E DS
026800100910      *
026900100910     d PT_KeyXsav      s              3  0 inz(0)
027000100910     D PT_trovata      s              1    inz('N')
027100100910     D PU_key          S             35
027200100910     D PV_key          S             35
027300100910     D PU_HLD_SpedVAX  S              1
027400100910     D PT_con_piu_Nazioni...
027500100910     d                 s              1
027600100910      *
027700100910     D  PT_vabLNP      s                   LIKE(vablnp)
027800100910     D  PT_vabNRS      s                   LIKE(vabnrs)
027900100910     D  PT_vabCTM      s                   LIKE(vabctm)
028000100910     D  PT_vabCCM      s                   LIKE(vabccm)
028100100910     D  PU_lunVAT      s              2s 0
028200100910     D  PU_TipoServ    s              2a
028300100910     D  PU_SpecServ    s              2a
028400100910      *
028500100910     D Nr_PT           S              3  0 INZ
028600100910     D savXX_PT_key    S              3  0 INZ
028700100628      *
028800100701      *---------------------------------------------------------------------*
028900100630     d UNB_diWork      s                   like(UNB0004)
029000100628     d  UNB_Mittente   s                   like(UNB0004)
029100100706     d BGM_Id_Testata  s             35A   inz(' ')
029200100706     d  BGM_NrFviaggio...
029300100628     d                 s             35A
029400100706     d  BGM_nrCMR      s             35A
029500100701     d  RFF_NrFviaggio...
029600100701     d                 s             35A
029700100701     d  RFF_NrCMR      s             35A
029800100701     d  RFF_RifSpediz  s             35A
029900100910     d  RFF_DalPrimoCollo...
030000100910     d                 s              1A
030100100910     d  RFF_DopoIlPrimoCollo...
030200100910     d                 s              1A
030300100701     d UNB_parziale    s              1    inz('N')
030400100701     d UNB_Parz        s             35    inz(' ')
030500100812     d UNB_Generico    s             31    inz(' ')
030600100701     d UNB_NrTrasmiss  s             14    inz(' ')
030700100701     d UNZ_NrTrasmiss  s             14    inz(' ')
030800100701     d UNH_Rifer_Msg   s             14    inz(' ')
030900100701     d UNH_VABrma      s                   like(VABrma)
031000100701     d UNH_VABrmn      s                   like(VABrmn)
031100100708     d NAD_SF_x_CMR    s             35    inz(' ')
031200100910      *
031300100910???? D NAD_destinatario_in_testata...
031400100910???? D                 s              1
031500100910???? D NAD_destinatario_in_dettaglio...
031600100910???? D                 s              1
031700100910???? D NAD_mittente_in_testata...
031800100910???? D                 s              1
031900100910???? D NAD_mittente_in_Dettaglio...
032000100910???? D                 s              1
032100100910     d CTA_destinatario_in_Dettaglio...
032200100910     d                 s             35A
032300100910     d CTA_destinatario_in_testata...
032400100910     d                 s             35A
032500100910     d COM_telefono_in_Dettaglio...
032600100910     d                 s             35A
032700100910     d COM_telefono_in_testata...
032800100910     d                 s             35A
032900100701      *
033000100630     d  DTM_DtParten   s              8s 0
033100100630     d  DTM_DtFViag    s              8s 0
033200100630     d  DTM_DtCMR      s              8s 0
033300100811     d  DTM_DtConsRic  s              8s 0
033400100811     d  DTM_OraCnsRic  s              4s 0
033500100630     d  DTM_OraFViag   s              4s 0
033600100630     d  DTM_OraCMR     s              4s 0
033700100702     d  DTM_DtArrivo   s              8s 0
033800100702     d  DTM_OraArrivo  s              4s 0
033900100706     d  DTM_OraParten  s              4s 0
034000100702     d  CNI_Identificativo_Bolla...
034100100702     d                 s             35A
034200100813     d  CNI_seNumerico...
034300100813     d                 s                   LIKE(vabRMN)
034400100701     d  GID_aTotale    s              1A
034500100705     d  GID_ColliInAdd...
034600100705     d                 s                   like(gid722420)
034700100910      *---------------                                                      *
034800090209      *  DS x scomposizione segnacollo
034900100630     D DS_SegnBART     DS
035000100630     D  SegnBART_LNP           1      3  0
035100100630     D  SegnBART_LNA           4      6  0
035200100630     D  SegnBART_NRS           7      8  0
035300100630     D  SegnBART_NSC           9     15  0
035400100630     D  SegnBART_ZNC          16     17  0
035500100701      **
035600090209      *---------------                                                      *
035700100701     D VABtic_work     s                   LIKE(vabtic)
035800100630      *
035900090603     d tes_VABrmo      s                   LIKE(VABrmo)
036000090603     d tes_VABnmo      s                   LIKE(VABnmo)
036100090603     d tes_VABcmo      s                   LIKE(VABcmo)
036200090209     C*
036300090603     d tes_VABrsd      s                   LIKE(VABrsd)
036400090603     d tes_VABrd2      s                   LIKE(VABrd2)
036500090603     d tes_VABind      s                   LIKE(VABind)
036600090603     d tes_VABlod      s                   LIKE(VABlod)
036700090603     d tes_VABnzd      s                   LIKE(VABnzd)
036800090603     C*
036900100813     d tes_VABtsp      s                   LIKE(VABtsp)
037000100813     d tes_VABtc1      s                   LIKE(VABtc1)
037100100813     d tes_VABnot      s                   LIKE(VABnot)
037200100813     C*
037300090603     d tes_Valuta      s                   LIKE(VABvca)
037400090603     C*
037500100910     D SUvabRMO        s                   LIKE(vabRMO)
037600090209      *---------------------------------------------------------------------*
037700090209      * Schiere
037800090209      *---------------------------------------------------------------------*
037900090209      * Schiera per reperimento nr.seganacollo
038000100630     D segn_BAR        S              7  0 DIM(5000)
038100100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
038200100630      *
038300100630      *
038400090209      * Nr. documento alfanumerico da decodificare
038500090209     D NDA             S              1    DIM(35)
038600100630      *
038700090209      * Nr. documento alfanumerico da già decodificato
038800090209     D NDN             S              1    DIM(15)
038900100630      *
039000090209      * Messaggi di errore
039100090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
039200100630      *
039300090209      * Tabella codici trattamento merce (1B)
039400100701     D Cod_Tb_CTM      S              2    DIM(200)
039500100701     D Des_Tb_CTM      S             67    DIM(200)
039600100630      *
039700090209      * Tabella codici valuta
039800090209     D CCV             S              3    DIM(200)
039900090209     D DCV             S             89    DIM(200)
040000100630      *
040100090209      * Tabella codici nazioni
040200090209     D C15             S              3    DIM(500)
040300090209     D ISO             S              3    DIM(500)
040400090209     D CPF             S              2  0 DIM(500)
040500100630      *
040600090209      * Tabella CL --> codici clienti - tariffa
040700130305     D Cod_Tb_CL       S             35    DIM(999)
040800130305     D Des_Tb_CL       S             90    DIM(999)
040900100630      *
041000090209      * Schiere x caricamento cod.cliente  <>
041100130305     D skCLienti       S              7  0 DIM(999)
041200100630      *
041300100318      * Priorità trasporto
041400100705     D Key_TipServ     S             35    DIM(100)
041500100702     D TipoServizio    S              1    DIM(100)
041600100702      *
041700100702      * Decodifiche servizi speciali
041800100702     D SpecialServ     S             35    DIM(100)
041900100702     D Key_ServSpec    S              3    DIM(100)
042000100702     D UNI_ServSpec    S             90    DIM(100)
042100100630      *
042200090209      * Termini di consegna
042300110328     D K35_TpBolla     S             35    DIM(100)
042400110328     D Cod_TpBolla     S              3    DIM(100)
042500110328     D Decod_Porto     S              1    DIM(100)
042600110328     d Trovata_Tab_TB  S              1
042700100630      *
042800090216      * Tipo pagamento C/Assegno
042900100705     D K35_TpIncas     S             35    DIM(100)
043000100705     D TipoIncasso     S              2    DIM(100)
043100100630      *
043200090209      * Schiere x routine di conversione CAP
043300090209     D CAP5            S              1    DIM(5)
043400090209     D CAP9            S              1    DIM(9)
043500090209     D CAPM            S              1    DIM(9)
043600100630      *
043700090209      * Schiera per memorizzazione FTX ricevuti
043800090209     D QUA             S              3    DIM(100)
043900090209     D FTX             S             70    DIM(100)
044000100630      *
044100090209      * Tabelle capiconto
044200090209      * Schiere x località
044300090209     D LOD             S              1    DIM(35)
044400100630      *
044500060608      **?------------------------------------------------------------------ */
044600090601     C*? Ds Scrittura del VAT "E"
044700060608      **?------------------------------------------------------------------ */
044800060612     D XTOEBCDDS     E DS
044900060620      *
045000060608      **?------------------------------------------------------------------ */
045100100701      *
045200090209      **?------------------------------------------------------------------ */
045300090209     C* Definizione campi di work
045400090209     D kKUT            s                   LIKE(TBLKUT)
045500090209     D kCOD            s                   LIKE(TBLCOD)
045600090209     D kKEY            s                   LIKE(TBLKEY)
045700090209     D kKCC            s                   LIKE(ACOKCC)
045800090209     D kKSC            s                   LIKE(ACOKSC)
045900090209     D kAAA            s                   LIKE(NUFAAA)
046000090209     D kCNU            s                   LIKE(NUFCNU)
046100090209     D kFIL            s                   LIKE(NUFFIL)
046200090209     D kAAS            s                   LIKE(VABAAS)
046300090209     D kLNP            s                   LIKE(VABLNP)
046400090209     D kNRS            s                   LIKE(VABNRS)
046500090209     D kNSP            s                   LIKE(VABNSP)
046600090209     D kCMR            s                   LIKE(VABCMR)
046700090209     D kCCM            s                   LIKE(VABCCM)
046800090209     D kNCD            s                   LIKE(VADNCD)
046900090210     D kCNT            s                   LIKE(VADCNT)
047000090209     D kNMC            s                   LIKE(RDENMC)
047100090209     D kNRP            s                   LIKE(RDENRP)
047200090209     D kLNP1           s                   LIKE(RDELNP)
047300090209      *
047400090209      *  Invio E-mail
047500090209     D Indirizzi       s            253
047600090209     D Oggetto         s             44
047700090209     D Messaggio       s           5000
047800090209      *
047900090209     d   DSmail        ds
048000090209     D Mail_Ind                      44
048100090209     d   IND_char                     1    dim(44)
048200090209      *
048300100630     D Lunghezza_Indirizzo...
048400100630     D                 s              5u 0
048500090209      *
048600100630     D dalCarattere    s              5u 0
048700100630     D xTOTcaratteri   s              5u 0
048800090209      *
048900090209     C*---------------------------------------------------------------*
049000090209     D MsgDSP          S            256                                         Testo generico
049100090209     C*---------------------------------------------------------------*
049200090209     D SndMg01         S             10                                         Message type
049300090209     D                                     INZ('*INFO')
049400090209     D SndMg02         S             10                                         Deluvery mode
049500090209     D                                     INZ('*BREAK')
049600090209     D SndMg03         S            256                                         Message text
049700090209     D SndMg04         S             10I 0                                      Length of text
049800090209     D                                     INZ(%SIZE(SndMg03))
049900090209     D SndMg05         S             10                                         User profile
050000090209     D                                     DIM(299)
050100090209     D SndMg06         S             10I 0                                      Number of user
050200090209     D SndMg07         S             10I 0                                      Message sent indic.
050300090209     D SndMg08         S             10I 0                                      Function requested
050400090209     D SndMg10         S              1                                         Show display
050500090209     D                                     INZ('N')
050600090209     D SndMg11         S             20                                         Qualified MSGQ name
050700090209     D SndMg12         S              4                                         Name type indicator
050800090209     D                                     INZ('*USR')
050900090209      * indice di scihera
051000090209     D Jx              S              5I 0
051100090209      *
051200090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
051300090209      *
051400090209     d bart_it         C                   CONST('@Bartolini.it;')
051500090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
051600060612      * ?================================================================== */
051700060612      * ?   * Campi x decodifica * (INPUT  del Record)
051800100319      * ?================================================================== */
051900090130     D  Dati           s           2048
052000060612      * ?   *--------------------------------------------------------------*
052100060612     D  se_errore      s              1    inz(' ')
052200060612      * ?* ------------------------------------------------------ *
052300060710     D Digits          C                   '0123456789'
052400091019     D*------------------
052500091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
052600091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
052700060612      * ?================================================================== */
052800060612      *   Ciclo principale
052900090205      * ?================================================================== */
053000060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
053100060612     C*
053200060612     C                   if        not %open(tivin00r)
053300060612     C                   open      tivin00r
053400060612     C                   endif
053500060612      **
053600060612      * ?------------------------------------------------------------------ */
053700100708     C*  Controllo dati arrivati da DPD
053800060612      * ?------------------------------------------------------------------ */
053900060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
054000060612      * ?- controllando che la trasmissione sia completa.
054100090205      **
054200060612      * ?              /*---------------------- */
054300090205     c                   goto      nonFare_adesso
054400060612     c                   exsr      check_Trasm
054500090205     c     nonFare_adessotag
054600060612      * ?              /*---------------------- */
054700090205      **
054800060613      **  Errore di trasmissione x tutti i records
054900060613      **   --> file in errore
055000060613     c                   if        se_errore = 'S'
055100060612
055200060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
055300090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
055400090205     c                   exsr      Msg_errato
055500060612      **
055600060612     c                   else
055700060614      **
055800060612      * ?------------------------------------------------------------------ */
055900100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
056000060612      * ?------------------------------------------------------------------ */
056100060612      * ?              /*---------------------- */
056200060612     c                   exsr      Importa_Msg
056300060612      * ?              /*---------------------- */
056400060612
056500060614     c                   end
056600060612      *
056700060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
056800090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
056900090225     c                             esito ='0'
057000060710     C                   eval      esito ='0'
057100090225      *
057200090225     c                   COMMIT
057300090224      *
057400090213     c                   if        almeno_un_due ='S'
057500090213     C                   eval      esito ='1'
057600060614     C                   endif
057700090213     C                   endif
057800100708      * chiude TIVIN
057900100708     C                   if        %open(tivin00r)
058000100708     C                   close     tivin00r
058100100708     C                   endif
058200060614      *
058300060612     c                   SETON                                        LR
058400060612      * ?================================================================== */
058500100708      *? Importa i records della trasmissione
058600060612      * ?------------------------------------------------------------------ */
058700060612     c     Importa_Msg   Begsr
058800060614
058900100726     c                   clear                   Forza_Uscita
059000060612     c     *start        setll     Tivin00r
059100100708
059200100708     c                   EXSR      LEGGE_TIVIN_R
059300100708     c                   dow       EoFTivin <> 'S'
059400060614
059500060614      * solo i record sflaggati da rielaborare
059600110107     c                   IF        vinFLG = *blank
059700110107      *** ++++++
059800090211      *  Sempre Record OK
059900090211      ** Controlli formali sui campi
060000090211     C                   eval      vinFLG = '1'
060100060612     c                   clear                   se_errore
060200110107      *
060300110107      *** >>>>>>
060400110107     c                   IF        vinDTA <> *blank
060500110107      *** >>>>>>
060600060612
060700060612      ** Decodifica record a campi variabili
060800060612      * ?              /*---------------------- */
060900090205     c                   exsr      Decod_Segmento
061000090209      * ?              /*---------------------- */
061100060612
061200060621      *  x errore bloccante nella composizione del VAB/VAT
061300060621     c                   if        err_bloccante ='S'
061400060621     C                   eval      vinFLG = '2'
061500090211     c                   eval      esito  = '2'
061600100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
061700100921     C                   EVAL      Messaggio = 'EDI -
061800100921     C                             IFCSUM D96A - msg su UPLOAD -
061900100921     C                             con UNB non presente in tabella PT..'
062000100319     c                   if        §PVinv <> 'N'
062100100319     c                   exsr      invio_mail
062200060621     c                   end
062300100319     c                   end
062400060612
062500110107      *** >>>>>>
062600110107     c                   endIF
062700110107      *** >>>>>>
062800110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
062900110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
063000110107      *         un erroneo messaggio
063100110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
063200060612     c                   update    Tivin000
063300090211
063400090211      *** se non è riconosciuto l'UNB deve uscire direttamente
063500090211     c                   if        se_errore ='S'  and
063600100708     c                             tipo_seg = Segm_Inizio_Messaggio
063700090213     c                   eval      err_bloccante = 'S'
063800090211     c                   exsr      Msg_errato
063900090211     c                   LEAVE
064000090211     c                   endIF
064100090211      ***
064200060614     c                   endIF
064300060612
064400100708     c                   EXSR      LEGGE_TIVIN_R
064500060612     C                   ENDdo
064600060612      **
064700060612     c                   endSR
064800100709      * ?------------------------------------------------------------------ */
064900100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
065000100709      * ?------------------------------------------------------------------ */
065100100709     C     MSG_Errato    BEGSR
065200100709      *
065300100709      * ?  Scrive su tutti i records il tipo di errore
065400100709     c     *start        setll     tivin00r
065500100709     c
065600100709     c                   EXSR      LEGGE_TIVIN_R
065700100709     c                   dow       EoFTivin <> 'S'
065800100709     c
065900100709     c                   if        vinMSG  = *blank
066000100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
066100100709     C                               >> Controllare i DATI su UPLOAD !!'
066200100709     c                   end
066300100709     C                   eval      vinFLG = '2'
066400100709     c                   eval      Almeno_Un_Due ='S'
066500100709     c                   eval      esito  = '2'
066600100709     c                   eval      se_errore ='S'
066700100709     c                   eval      err_bloccante ='S'
066800100709      *
066900100709     c                   update    tivin000
067000100709     c                   EXSR      LEGGE_TIVIN_R
067100100709     c                   endDO
067200100709      *
067300100709     C                   ENDSR
067400100709      * ?------------------------------------------------------------------ */
067500100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
067600100709      * ?------------------------------------------------------------------ */
067700100709     C     DETta_Errato  BEGSR
067800100709      *
067900100709     c                   EXSR      LEGGE_TIVIN_R
068000100709     c                   dow       EoFTivin <> 'S'
068100100709      *
068200100709      * Se ha letto il successivo record
068300100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
068400100709     c                   if        VNREC = UNT_record  or
068500100709     c                             VNREC = finDET_RECord
068600100709     c                   leave
068700100709     c                   end
068800100709      *
068900100709     c                   exsr      Error_DET
069000100709      *
069100100709     c                   if        vinMSG  = *blank
069200100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
069300100709     C                             >> Controllare i DATI di ogni Segmento <<'
069400100709     c                   end
069500100709      *
069600100709     c                   update    tivin000
069700100709     c                   EXSR      LEGGE_TIVIN_R
069800100709     c                   endDO
069900100709      * ------
070000100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
070100100921     C                   EVAL      Messaggio = 'EDI -
070200100811???? C                             IFCSUM D96A  non totalmente tradotto. -
070300100709     C                             Controllare UPLOAD del cliente:'
070400100709     C                   EVAL      Messaggio = %trim(Messaggio) +
070500100709     C                                         %editc(§PTksc:'X')
070600100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
070700100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
070800100709     c                   if        §PVinv <> 'N'
070900100709     c                   exsr      invio_mail
071000100709     c                   end
071100100709      *
071200100709      * ripulisce per un nuovo giro
071300100709     C                   clear                   Err_in_detta
071400100709      *
071500100709     C                   ENDSR
071600100709      * ?------------------------------------------------------------------ */
071700100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
071800100709      * ?------------------------------------------------------------------ */
071900100709     C     LEGGE_TIVIN_R BEGSR
072000100709      *
072100100709     c                   clear                   EoFTivin
072200100709      *  Lettura sequenziale
072300100709     c                   read      Tivin00r
072400100709
072500100709     c                   if        %Eof(tivin00r)
072600100709     c                   move      'S'           EoFTivin
072700100709     c                   else
072800100709      *
072900100709      * ?  imposta il tipo di segmento:
073000100709     c                   clear                   dati
073100100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
073200100728     c                   eval      segmento = %trimL(dati)
073300100728     c                   eval      tipo_seg = %subst(segmento:1:3)
073400100709      **
073500100709     c                   end
073600100709      *
073700100709     C                   ENDSR
073800100120      * ?------------------------------------------------------------------ */
073900100120      *?    Decodifica il segmento ed importa i campi in formato DS
074000100120      * ?------------------------------------------------------------------ */
074100100120     c     Decod_SegmentoBegsr
074200100120      **
074300100706      *** ------------------------------------
074400100706????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine "UNT"
074500100706      *** ------------------------------------
074600100706      * OGNI DETTAGLIO inizia dal segmento CNI e per ultimo il msg è chiuso da UNT.
074700100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
074800100706      **
074900100706      **  Scrive alla fine di ogni giro di dettaglio:
075000100706     c                   if           tipo_seg = Segm_Fine_Dettagli
075100100706     c                                   or
075200100706     c                                tipo_seg = Segm_Inizio_Dettaglio  and
075300100706     c                                Contatore_Dettagli > 0
075400100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
075500100708     c                   eval      FinDET_RECord = vnrec
075600100708      **?              /*---------------------- */
075700100708     c                   exsr      Scrive_Dettag
075800100708      **?              /*---------------------- */
075900100812      *
076000100812      *   Ogni nuovo Dettaglio,
076100100812      *    per quei Partner che gestiscono più LInee
076200100812      *    Mediante il Mittente del dettaglio
076300100812     c                   if        UNB_parziale ='S' and
076400100812     c                              PT_con_piu_Nazioni = 'S' and
076500100812     C                                 Naz_salvata = Naz_mittente
076600100812      * ?ReImposta i dati da tabella PT
076700100812      * ?  se fossero stati cambiati dal precedente dettaglio:
076800100812     C                   z-add     savXX_PT_key  Nr_PT
076900100812     c                   exsr      Se_Trovata_PT
077000100812     c                   endIF
077100100812      *
077200100812     c                   ENDif
077300100708      *
077400100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
077500100708      **?              /*---------------------- */
077600100708     c                   exsr      Pulizia_Campi
077700100708      **?              /*---------------------- */
077800100812      *
077900100706      *** ------------------------------------
078000100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
078100100120      * ?  Anche se non utilizzati.
078200100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
078300100317     c                   clear                   seg_imprevisto
078400100720     c                   clear                   trovato_seg
078500100319      *
078600100811      **?          programma che decodifica il segmento
078700100811      **?              /*---------------------- */
078800100811     c                   exsr      chiama_Decoder
078900100811      **?              /*---------------------- */
079000100811      *
079100100319      *  Contatore delle righe ossia dei segmenti del messaggio
079200100319     c                   add       1             Conta_segmenti
079300100708      *
079400100708      *** ------------------------------------
079500100708      *   Decodifica il singolo segmento LETTO
079600100708      *** ------------------------------------
079700100120     c                   select
079800100120      *
079900110110     c                   when      trovato_seg ='N' and unz_segmento <> 'S'
080000100720     c                   move      'S'           seg_imprevisto
080100100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
080200100720     c                   exsr      Error_DET
080300100720     c                   goto      end_Decod
080400100720      **--------
080500100120     c                   when      tipo_seg = 'UNA'
080600100811     c                   movel(p)  DsGenerica    eds96UNA
080700100120      *
080800100120     c                   when      tipo_seg = 'UNB'
080900100811     c                   movel(p)  DsGenerica    eds96UNB
081000100120     C                   EXSR      DATI_UNB
081100100120      * --------
081200100120     c                   when      tipo_seg = 'UNH'
081300100811     c                   movel(p)  DsGenerica    eds96UNH
081400100120     C                   EXSR      DATI_UNH
081500100120      * --------
081600100120     c                   when      tipo_seg = 'BGM'
081700100811     c                   movel(p)  DsGenerica    eds96BGM
081800100316     C                   EXSR      DATI_BGM
081900100811      **--------
082000100811     c                   when      tipo_seg = 'CNT'
082100100811     c                   movel(p)  DsGenerica    eds96CNT
082200100811     C                   EXSR      DATI_CNT
082300100811      **--------
082400100811     c                   when      tipo_seg = 'TDT'
082500100811     c                   movel(p)  DsGenerica    eds96TDT
082600100811     C                   EXSR      DATI_TDT
082700100811      * --------
082800100811     c                   when      tipo_seg = 'TSR'
082900100811     c                   movel(p)  DsGenerica    eds96TSR
083000100811     C                   EXSR      DATI_TSR
083100100811      **--------
083200100811     c                   when      tipo_seg = 'NAD'
083300100811     c                   movel(p)  DsGenerica    eds96NAD
083400100811     C                   EXSR      DATI_NAD
083500100811      **--------
083600100811     c                   when      tipo_seg = 'EQD'
083700100811     c                   movel(p)  DsGenerica    eds96EQD
083800100811     C                   EXSR      DATI_EQD
083900100811      **--------
084000100811     c                   when      tipo_seg = 'EQN'
084100100811     c                   movel(p)  DsGenerica    eds96EQN
084200100811     C                   EXSR      DATI_EQN
084300100811      **--------
084400100811     c                   when      tipo_seg = 'CNI'
084500100811     c                   movel(p)  DsGenerica    eds96CNI
084600100811     C                   EXSR      DATI_CNI
084700100811      **--------
084800100811     c                   when      tipo_seg = 'DTM'
084900100811     c                   movel(p)  DsGenerica    eds96DTM
085000100811     C                   EXSR      DATI_DTM
085100100811      **--------
085200100811     c                   when      tipo_seg = 'FTX'
085300100811     c                   movel(p)  DsGenerica    eds96FTX
085400100811     C                   EXSR      DATI_FTX
085500100811      **--------
085600100811     c                   when      tipo_seg = 'TOD'
085700100811     c                   movel(p)  DsGenerica    eds96TOD
085800100811     C                   EXSR      DATI_TOD
085900100811      **--------
086000100811     c                   when      tipo_seg = 'GID'
086100100811     c                   movel(p)  DsGenerica    eds96GID
086200100811     C                   EXSR      DATI_GID
086300100811      **--------
086400100811     c                   when      tipo_seg = 'MEA'
086500100811     c                   movel(p)  DsGenerica    eds96MEA
086600100811     C                   EXSR      DATI_MEA
086700100811      **--------
086800100811     c                   when      tipo_seg = 'RFF'
086900100811     c                   movel(p)  DsGenerica    eds96RFF
087000100811     C                   EXSR      DATI_RFF
087100100811      **--------
087200100811     c                   when      tipo_seg = 'PCI'
087300100811     c                   movel(p)  DsGenerica    eds96PCI
087400100811     C                   EXSR      DATI_PCI
087500100811      **--------
087600100811     c                   when      tipo_seg = 'GIN'
087700100811     c                   movel(p)  DsGenerica    eds96GIN
087800100811     C                   EXSR      DATI_GIN
087900100316      **--------
088000100316     c                   when      tipo_seg = 'SEL'
088100100811     c                   movel(p)  DsGenerica    eds96SEL
088200100316     C                   EXSR      DATI_SEL
088300100316      **--------
088400100316     c                   when      tipo_seg = 'PIA'
088500100811     c                   movel(p)  DsGenerica    eds96PIA
088600100316     C                   EXSR      DATI_PIA
088700100316      **--------
088800100316     c                   when      tipo_seg = 'LOC'
088900100811     c                   movel(p)  DsGenerica    eds96LOC
089000100316     C                   EXSR      DATI_LOC
089100100316      **--------
089200100316     c                   when      tipo_seg = 'DGS'
089300100811     c                   movel(p)  DsGenerica    eds96DGS
089400100316     C                   EXSR      DATI_DGS
089500100120      **--------
089600100120     c                   when      tipo_seg = 'MOA'
089700100811     c                   movel(p)  DsGenerica    eds96MOA
089800100120     C                   EXSR      DATI_MOA
089900100120      **--------
090000100120     c                   when      tipo_seg = 'CTA'
090100100811     c                   movel(p)  DsGenerica    eds96CTA
090200100120     C                   EXSR      DATI_CTA
090300100120      **--------
090400100120     c                   when      tipo_seg = 'COM'
090500100811     c                   movel(p)  DsGenerica    eds96COM
090600100120     C                   EXSR      DATI_COM
090700100811      **--------
090800100811     c                   when      tipo_seg = 'HAN'
090900100811     c                   movel(p)  DsGenerica    eds96HAN
091000100811     C                   EXSR      DATI_HAN
091100100811      **--------
091200100811     c                   when      tipo_seg = 'GOR'
091300100811     c                   movel(p)  DsGenerica    eds96GOR
091400100811     C                   EXSR      DATI_GOR
091500100811      **--------
091600100811     c                   when      tipo_seg = 'TMP'
091700100811     c                   movel(p)  DsGenerica    eds96TMP
091800100811     C                   EXSR      DATI_TMP
091900100811      **--------
092000100811     c                   when      tipo_seg = 'TMD'
092100100811     c                   movel(p)  DsGenerica    eds96TMD
092200100811     C                   EXSR      DATI_TMD
092300100811      **--------
092400100811     c                   when      tipo_seg = 'TCC'
092500100811     c                   movel(p)  DsGenerica    eds96TCC
092600100811     C                   EXSR      DATI_TCC
092700100811      **--------
092800100811     c                   when      tipo_seg = 'RNG'
092900100811     c                   movel(p)  DsGenerica    eds96RNG
093000100811     C                   EXSR      DATI_RNG
093100100811      **--------
093200100811     c                   when      tipo_seg = 'DOC'
093300100811     c                   movel(p)  DsGenerica    eds96DOC
093400100811     C                   EXSR      DATI_DOC
093500100120      **--------
093600100811     c                   when      tipo_seg = 'DIM'
093700100811     c                   movel(p)  DsGenerica    eds96DIM
093800100811     C                   EXSR      DATI_DIM
093900100811      **--------
094000100120     c                   when      tipo_seg = 'UNT'
094100100811     c                   movel(p)  DsGenerica    eds96UNT
094200100120     C                   EXSR      DATI_UNT
094300100120      **--------
094400100120     c                   when      tipo_seg = 'UNZ'
094500100811     c                   movel(p)  DsGenerica    eds96UNZ
094600100120     C                   EXSR      DATI_UNZ
094700101222     c                   move      'S'           unz_segmento
094800101222      **--------
094900101222     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
095000100120      **--------
095100100120     c                   OTHER
095200100120      **--------
095300110110     c                   if            unz_segmento <> 'S'
095400100120     c                   move      'S'           seg_imprevisto
095500100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
095600100120     c                   exsr      Error_DET
095700100120     c                   goto      end_Decod
095800110110     c                   end
095900100120      **--------
096000100120     c                   endSL
096100100120      **
096200100120     c     end_Decod     endSR
096300100701      * ?================================================================== */
096400100319     C*? Azzera_MSG   - Azzera dati messaggio
096500100701      * ?================================================================== */
096600100708     C     Scrive_Dettag BEGSR
096700100701      **
096800100708      *  Deve flaggare il dettaglio con dei problemi
096900100708      * Imposta le righe in errore sullo specifico dettaglio
097000100708     C                   if        Err_in_detta = 'S'
097100100708      * prima di rieseguire il ciclo
097200100708      *  rilascia il record
097300100708     c                   update    tivin000
097400100708      * ?  Scrive su tutti i records il tipo di errore
097500100708     c     IniDET_RECord setll     tivin00r
097600100708      **?              /*---------------------- */
097700100708     c                   exsr      DETta_Errato
097800100708      **?              /*---------------------- */
097900100708     c                   end
098000100708      *
098100100708      **  Se presente un errore nel record emette una segnalazione msg
098200100708???? c                   if        se_errore <> 'S'
098300100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
098400100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
098500100708     c                              NAD_Destinatario_in_Testata    ='S'
098600100708      * ?              /*---------------------- */
098700100708     c                   exsr      Scrive_VAB
098800100708      * ?              /*---------------------- */
098900100708     c                   else
099000100708     c                   eval      errori_in_Wrt = 'S'
099100100708     c                   end
099200100708      *
099300100708      *  Problemi nella decodifica dei campi VAB/VAT
099400100708     c                   if        errori_in_Wrt = 'S'
099500100921     C                   evalR     vinMSG = 'EDI-Problemi scrittura VAB'
099600100708     c                   exsr      Error_DET
099700100708     c                   ROLBK
099800100708     c                   else
099900100708     c                   COMMIT
100000100708     c                   end
100100100708      *
100200100708     c                   end
100300100708      **
100400100708     c     end_WRidett   endSR
100500100708      * ?================================================================== */
100600100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
100700100708      * ?================================================================== */
100800100708     C     Pulizia_Campi BEGSR
100900100708      **
101000100708      * inzio messaggio azzera tutte le variabili generali del messaggio
101100100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
101200100708      * ?                         --------------
101300100708     C                   EXSR      Azzera_MSG
101400100708      * ?                         --------------
101500100708      *
101600100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
101700100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
101800100708      * ?                         --------------
101900100813     c                   exsr      Testata_msg
102000100708      * ?                         --------------
102100100708      *
102200100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
102300100708      *  Prepara una nuova spedizione
102400100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
102500100708      * ?                         --------------
102600100708     c                   exsr      Nuova_spediz
102700100708      * ?                         --------------
102800100708     c                   end
102900100708      **
103000100708     c     end_clear     endSR
103100100708      * ?================================================================== */
103200100708     C*? Azzera_MSG   - Azzera dati messaggio
103300100708      * ?================================================================== */
103400100708     C     Azzera_MSG    BEGSR
103500100708      **
103600100701     C*  Inizializza variabili
103700100701     C                   Z-ADD     0             skCLienti
103800100701     C                   Z-ADD     0             sk
103900100701     C*
104000100701      *  INIZIALIZZA  dati fondamentali messaggio
104100100701     C                   clear                   EDIDSPT
104200100701     C                   clear                   EDIDSPU
104300100701     C                   clear                   EDIDSPV
104400100701     C*
104500100701     C                   move      'N'           PT_trovata
104600100812     C                   clear                   savXX_PT_key
104700100701     C                   clear                   PT_vabLNP
104800100701     C                   clear                   PT_vabNRS
104900100701     C                   clear                   PT_vabCTM
105000100701     C                   clear                   PT_vabCCM
105100100701     C                   clear                   PU_HLD_SpedVAX
105200100812     C                   eval        PT_con_piu_Nazioni = *blank
105300100812     c                   move      *blank        Naz_salvata       3
105400090211     C*
105500100701     c                   clear                   UNB_Generico
105600100701     c                   clear                   UNB_Parz
105700100701     c                   clear                   UNB_NrTrasmiss
105800100701     c                   clear                   UNZ_NrTrasmiss
105900100708      *
106000100708     c                   clear                   NAD_SF_x_CMR
106100100630      *
106200100630      * per controllare se almeno un record è stato importato sul VAB
106300100630     c                   clear                   Almeno_Uno        1
106400100630     c                   clear                   Almeno_Un_Due     1
106500100708     c                   clear                   IniDET_RECord
106600100708     c                   clear                   FinDET_RECord
106700100630     c                   eval      esito  = '0'
106800100728     c                   eval      Segmento_in_errore = 0
106900100317     C*
107000090211     C                   ENDSR
107100100701      * ?================================================================== */
107200100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
107300100701      * ?================================================================== */
107400100813     C     Testata_msg   BEGSR
107500100701      *
107600100701      *  imposta il Flag per identificare la testata
107700100701     c                   eval      Dati_di_Testata  ='S'
107800100701     C*
107900100701      *   Contatore dei dettagli
108000100701     c                   eval      Contatore_Dettagli = 0
108100100701      **
108200100701      *   Codici identificativi della Testata del messaggio
108300100701     c                   clear                   UNH_Rifer_Msg
108400100706     C* Pulisco dati di work x foglio viaggio
108500100706     C                   clear                   BGM_nrCMR                      * WNRCMR
108600100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
108700100701     c                   clear                   BGM_Id_Testata
108800100701      **
108900100701     C*  Riferimento Foglio Viaggio o CMR
109000100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
109100100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
109200100701      **   Date generali
109300100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
109400100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
109500100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
109600100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
109700100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
109800100702     C                   Z-ADD     0             DTM_DtArrivo                   *
109900100702     C                   Z-ADD     0             DTM_OraArrivo                  *
110000100811     C                   Z-ADD     0             DTM_DtConsRic                  * WvabDCR
110100100811     C                   Z-ADD     0             DTM_OraCnsRic                  * WvabHCR
110200100701      *
110300100701      * pulisce gli RFF+FF di testata
110400100706     C                   clear                   Tes_RFF_FF_KSC
110500100706     C                   clear                   Tes_RFF_FF_TAR
110600100706     C                   clear                   Tes_RFF_FF_LNP
110700100706     C                   clear                   Tes_RFF_FF_NRS
110800100706     C                   clear                   Tes_RFF_FF_CTM
110900100706     C                   clear                   Tes_RFF_FF_BS1
111000100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
111100100701     C*
111200100701     c                   clear                   tes_VABrmo
111300100701     c                   clear                   tes_VABnmo
111400100701     c                   clear                   tes_VABcmo
111500100701     C*
111600100813     c                   clear                   tes_VABtsp
111700100813     c                   clear                   tes_VABtc1
111800100813     c                   clear                   tes_VABnot
111900100813     C*
112000100701     c                   clear                   tes_VABrsd
112100100701     c                   clear                   tes_VABrd2
112200100701     c                   clear                   tes_VABind
112300100701     c                   clear                   tes_VABlod
112400100701     c                   clear                   tes_VABnzd
112500100701     C*
112600100701     c                   clear                   tes_Valuta
112700100702     C*
112800100811     c                   clear                   Tot_Peso_msg
112900100811     c                   clear                   Tot_Sped_msg
113000100811     c                   clear                   Tot_Colli_msg
113100100811     c                   clear                   Tot_PNetto_msg
113200100813     C*
113300100813     c                   clear                   AddTot_Colli     16 0
113400100701     C*
113500100701     ** Inizializza campo di work
113600100701     c                   eval      GID_aTotale = 'N'
113700100701     C*
113800100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
113900100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
114000100701     c                   eval      NAD_destinatario_in_testata = *blank
114100100701     c                   eval      NAD_mittente_in_testata = *blank
114200100701     c                   eval      CTA_destinatario_in_testata = *blank
114300100701     c                   eval      COM_telefono_in_testata = *blank
114400100701     C*
114500100701      *   Inizia il conteggio delle righe
114600100706     c                   z-add     0             Conta_segmenti    5 0
114700100701     C*
114800100701     C                   ENDSR
114900100701      * ?================================================================== */
115000100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
115100100701      * ?================================================================== */
115200100701     C     Nuova_spediz  BEGSR
115300100812      **
115400100701     C* Da qui inzia il dettaglio perciò lo memorizzo
115500100701      *  imposta il Flag per identificare la testata
115600100701     c                   eval      Dati_di_Testata  ='N'
115700100708      *
115800100708     c                   z-add     vnrec         IniDET_RECord
115900100812      **
116000100812      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
116100100812      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
116200100812      *
116300100812     c                   if        UNB_parziale ='S' and
116400100812     c                              PT_con_piu_Nazioni = 'S'
116500100812      * ?Cerca dalla riga di questo dettaglio in avanti
116600100812      * ?           *------------------------------*
116700100812     c                   exsr      cerca_Mittente
116800100812     c                   exsr      esito_tab_PT
116900100812      * ?           *------------------------------*
117000100812     c                   endIF
117100100812      **
117200100812      *
117300100701      *  Pulizia dei records
117400100701     C                   CLEAR                   EDiVAB00
117500100701     C                   CLEAR                   EDiVAT00
117600100701     C                   CLEAR                   EDiVAD00
117700100701      **
117800100813      **   Campi da testata messaggio
117900100706     C                   z-add     PT_vabLNP     VABLNP
118000100706     C                   z-add     PT_vabNRS     VABNRS
118100100701     C                   MOVEL     PT_vabCTM     VABCTM
118200100701     C                   z-add     PT_vabCCM     VABCCM
118300100813     C                   movel     tes_VABtsp    VABTSP
118400100813     C                   movel     tes_VABtc1    VABTC1
118500100813     C                   movel     tes_VABnot    VABnot
118600100701     C*
118700100701     C                   MOVEL     UNH_vabRMA    VABrma
118800100701     C                   z-add     UNH_vabRMN    VABrmn
118900100701      *
119000100701      *  Dettaglio dei segnacolli Bartolini/EEX
119100100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
119200100701     c                   eval      GID_aTotale = 'N'
119300100701      *
119400100701     C                   clear                   Conta_Segn        5 0
119500100701     C                   clear                   totale_PCI        5 0
119600100701     C                   clear                   RFF_RifSpediz
119700100701      *
119800100701     C                   MOVEA     *HIVAL        segn_BAR
119900100701     C                   MOVEA     *blank        segn_EEX
120000100701      *
120100100701     C*  Azzero codice cliente - tariffa
120200100701     C                   Z-ADD     0             ToT_Colli        16 0
120300100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
120400100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
120500100701      *
120600100701     C*  Azzero codice cliente - tariffa
120700100701     C                   clear                   Det_RFF_FF_ksc
120800100701     C                   clear                   Det_RFF_FF_tar
120900100701     C                   clear                   Det_RFF_FF_lnp
121000100701     C                   clear                   Det_RFF_FF_nrs
121100100701     C                   clear                   Det_RFF_FF_ctm
121200100701     C                   clear                   Det_RFF_FF_bs1
121300100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
121400100701      *
121500100701     C                   clear                   Note_1           35
121600100701     C                   clear                   Note_2           35
121700100701     C*  Campi di work
121800100701     c                   clear                   VABtic_work
121900100701     c                   clear                   wri_vabtic        1
122000100701      *
122100100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
122200100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
122300100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
122400100701     c                   eval      COM_telefono_in_Dettaglio = *blank
122500100702     c                   eval      CNI_Identificativo_Bolla = *blank
122600100701      *
122700100701     C                   clear                   Rifer_Alfanum    35
122800100910     C                   clear                   Secondo_Rifer    35
122900100701     C                   clear                   Rifer_Numeric    35
123000100701     C                   clear                   Tipo_Rifer        3
123100100701      *
123200100701      * Un errore nel dettaglio
123300100701     C                   clear                   Err_in_detta      1
123400100701     c                   clear                   errori_in_Wrt     1
123500100701      *
123600100910     c                   clear                   Primo_RFFcollo    1
123700100910     c                   clear                   Primo_RCUcollo    1
123800100910     c                   z-add     0             conta_rif         1 0
123900100910     c                   clear                   suVABrmo
124000100910      *
124100100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
124200100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
124300100701     C*
124400100812     C* ---------------------------------------------------
124500100812     C* Inizializzazione Spedizione con campi da testata :
124600100812     C* ---------------------------------------------------
124700100811     C*  Data Richiesta Consegna  da Testata se c'è;
124800100811     C                   IF        DTM_DtConsRic > 0                            Data cons.rich. test
124900100811     C                   z-add     DTM_DtConsRic VABDCR                         Data cons.rich.
125000100811     C                   z-add     DTM_OraCnsRic VABHCR                         Ora  cons.rich.
125100100811     c                   end
125200100812     C*
125300100812     c                   endSR
125400100812     C*----------------------------------------------------------------
125500100812      *? cerca Nazione del primo Mittente nella sequenza dettagli
125600100812     C*----------------------------------------------------------------
125700100812     C     cerca_MittenteBEGSR
125800100812      *
125900100812     c                   z-add     vnrec         rec_posiz         9 0
126000100812     c                   move      *blank        Naz_mittente      3
126100100812     c                   movel(p)  UNB_Generico  PT_Parziale      35
126200100812     c                   move      'N'           OK_Nazione        1
126300100812      *
126400100812      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
126500110126     c                   call      'TRTCT96R1'
126600100812     c                   parm                    rec_posiz
126700100812     c                   parm                    PT_Parziale
126800100812     c                   parm                    Naz_mittente
126900100812     c                   parm                    Ok_Nazione
127000100812     c                   parm                    User_msg
127100100812     C                   parm                    keyUNBCLI
127200100812     C                   parm      'IFCSUM'      keyTIPOMSG
127300100812     C                   parm      'D'           keyVERSION
127400100812     C                   parm      '96A'         keyRELEASE
127500100812     C                   parm      'UN'          keyAGENCY
127600100812     C                   parm                    keyASSOCIA
127700100812      *
127800100812      *  Re-inizializza l'indicatore di trovato
127900100812     c                   setoff                                       32
128000100812      *
128100100812      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
128200100812      *   da decodificare come UNB.
128300100812     c                   if        Ok_Nazione = 'S'
128400100812     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
128500100812     C                   eval      Nr_PT = 1
128600100812     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
128700100812     c                   Endif
128800100812      *
128900100812     c                   endSR
129000100812     C*----------------------------------------------------------------
129100100812      *? esito ricerca tabella PT decodificata oppure NO
129200100812     C*----------------------------------------------------------------
129300100812     C     esito_tab_PT  BEGSR
129400100812      *
129500100812     C                   move      'N'           PT_trovata
129600100812     C   32              move      'S'           PT_trovata
129700100812      *
129800100812      * ******  Errore generale sul Messaggio
129900100812      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
130000100812      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
130100100812     C                   If        PT_trovata = 'N'
130200100812      * Msg di allerta Traduzione
130300100812     C                   EVAL      Indirizzi = CED_Bart
130400100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
130500100921     C                   EVAL      Messaggio = 'EDI -
130600100921     C                             IFCSUM D96A - msg su UPLOAD -
130700100921     C                             con UNB non presente in tabella PT..'
130800100812      *
130900100812     c                   exsr      invio_mail
131000100812     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
131100100812     C                   eval      vinFLG = '2'
131200100812     C                   eval      se_errore   = 'S'
131300100812      *
131400100812      *             *------------------------------*
131500100812     C                   eLSe
131600100812      *             *------------------------------*
131700100812      *
131800100812     c                   exsr      Se_Trovata_PT
131900100812      *
132000100812      *
132100100812     c                   Endif
132200100812      *
132300100701     c                   endSR
132400090210     C*----------------------------------------------------------------
132500090210      *? DECODIFICA DATI UNB
132600090210     C*----------------------------------------------------------------
132700100812     C     Se_Trovata_PT BEGSR
132800090210      *
132900100812      * Se tutto OK su tab.PT:
133000100812      *
133100100812     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
133200100812     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
133300100812     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
133400100812      *
133500100812      *  Campi impostati come default:
133600100812      *  Se ho AGE/ON.... prendo i riferimenti definito su tab.PT il tipo di
133700100812      *  qualificatore da utilizzare come facciamo x RFF+AGE con EEX.
133800100812      *  Per default se non impostato diventa automaticamente un AGE.
133900100812     c                   if        §PVRFF = *blank
134000100909     c                   eval      §PVRFF = Riferimento_Spedizione
134100100909     c                   end
134200100813     c                   eval      Riferimento_Spedizione = §PVRFF
134300100910     c                   eval      RFF_DalPrimoCollo      = §PVRFS
134400100910     c                   eval      RFF_DopoIlPrimoCollo   = §PVRSE
134500100909      *
134600100812      *  Anche per il numerico prendo come default "CU"
134700100812     c                   if        §PVRCU = *blank
134800100909     c                   eval      §PVRCU = Riferimento_Numerico
134900100909     c                   end
135000100813     c                   eval      Riferimento_Numerico   = §PVRCU
135100100812      *
135200100812      *  Default per i campi Free Text nel dettaglio 'SIN' e 'ICN'
135300100812     c                   if        §PVsin = *blank
135400100812     c                   eval      §PVsin = 'SIN'
135500100812     c                   end
135600100812      *
135700100812     c                   if        §PVicn = *blank
135800100812     c                   eval      §PVicn = 'ICN'
135900100812     c                   end
136000100812      *
136100100812      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
136200100812      *  ricevuti:
136300100812     c                   movel     §PVema        mail_ind         44
136400100812      *
136500100812      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
136600100812     c                   if        mail_ind <> *blank
136700100812     c                   exsr      imp_ADDR_mail
136800100812     c                   end
136900100812      *
137000100812      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
137100100812      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
137200100812      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
137300100812     C                   if        §PULUN = *blank
137400100812     C                   eval      §PULUN = '00'
137500100812     c                   end
137600100812      *
137700100812      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
137800100812      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
137900100812     C                   move      §puLUN        PU_lunVAT
138000100812     C                   move      §puTS         PU_TipoServ
138100100812     C                   move      §puSS         PU_SpecServ
138200100812      *
138300100812     C                   Z-ADD     §PTKSC        PT_vabCCM
138400100812     C                   z-add     §PTLNP        PT_vabLNP
138500100812     C                   z-add     §PTNRS        PT_vabNRS
138600100812     C                   MOVEL     §PTCTM        PT_vabCTM
138700100812      *
138800100812      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
138900100812      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
139000100812      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
139100100812      *  i records trasmessi).
139200100812     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
139300100812      *
139400100812      *  In base al cod.trattamento merce reperisco tipo tratt.
139500100812     C                   CLEAR                   DS1B
139600100812     C                   Z-ADD     1             Y                 3 0
139700100812     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
139800100812     C     *IN32         IFEQ      '1'
139900100812     C                   MOVEL     Des_Tb_CTM(Y) DS1B
140000100812     C                   MOVEL     DS1B          sav_DS1B
140100100812     C                   END
140200100812      *
140300100812      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
140400100812     C                   clear                   ACORAG
140500100812     C                   clear                   INDCAE
140600100812     C                   clear                   INDCAP
140700100812     C                   clear                   INDSTA
140800100812      * Decodifico partner
140900100812     C                   Z-ADD     1             KKUT
141000100812     C                   Z-ADD     KCI           KKCC
141100100812     C                   MOVE      §PTKSC        KKSC
141200100812     C     KACO          CHAIN     CNACO00F                           31
141300100812     C     KIND          CHAIN     CNIND00F                           31
141400100812      *
141500100812      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
141600100812      *   questa schiera è utilizzata anche per i clienti.
141700100812     C     §ptKSC        LOOKUP    skCLienti                              39
141800100812     c                   If        *in39 = *off
141900100812     C                   add       1             sk
142000100812     C                   z-add     §ptKSC        skCLienti(sk)
142100100812     C                   Endif
142200100812      *
142300100812     c                   endSR
142400100812     C*----------------------------------------------------------------
142500100812      *? DECODIFICA DATI UNB
142600100812     C*----------------------------------------------------------------
142700100812     C     DATI_UNB      BEGSR
142800100812      *
142900100701      * identificativo Messaggio
143000100701     C                   eval      UNB_NrTrasmiss = unb0020
143100100812      *
143200100812     C                   MOVEL     unb0017       Anno2             2 0
143300100812     C                   z-add     2000          Anno4             4 0
143400100812     C                   add       Anno2         Anno4
143500100812     C                   MOVE      unb0017       WGG               2 0
143600100812     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
143700100812     C                   MOVE      Anno2         Data_messag
143800100812     C                   MOVEL     WGG           Data_messag
143900100812     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
144000100812     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
144100100812     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
144200100812      *
144300100812     c                   clear                   Mittente         35
144400100812     c                   clear                   Mitt_Qualifier    4
144500100812     c                   clear                   Id_Messaggio     14
144600100812     C                   eval      Mittente       = unb0004
144700100812     C                   eval      Mitt_qualifier = unb000720
144800100812     C                   eval      Id_Messaggio   = UNB0022
144900100701      *
145000090209      ** ---------------------------------------------------------------
145100090209      * quindi 1° cosa:
145200090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
145300090209      *     senza questo il messaggio NON è trascodificabile
145400100630     C                   MOVEL     unb0004       UNB_diWork
145500100630     C                   MOVE      unb000720     UNB_diWork
145600100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
145700100630     C                   Z-ADD     1             Nr_PT
145800100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
145900090209      *
146000100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
146100100317      *  la Nazione:
146200100317     C                   If        *IN32 = *off
146300100630     C                   eval      Nr_PT = 1
146400100630     c                   clear                   UNB_diWork
146500100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
146600100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
146700100317      *
146800100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
146900100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
147000100317      *  per cercare di individuare con certezza il codice della tabella PT in base
147100100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
147200100317     C                   If        *IN32 = *on
147300100317      *
147400100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
147500100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
147600100317      * ?  su Partner che hanno + linee con + nazioni.
147700100812      *  se è considerato Parziale
147800100812      *   Salva il codice generico
147900100630     c                   eval      UNB_parziale = 'S'
148000100812     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
148100100812      *             *------------------------------*
148200100812     c                   exsr      cerca_Mittente
148300100812      *             *------------------------------*
148400100812     C                   eval          Naz_salvata = Naz_mittente
148500100317     c                   Endif
148600100317     c                   Endif
148700100812      *
148800100812      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
148900100812      *             *------------------------------*
149000100812     c                   exsr      esito_tab_PT
149100100812      *             *------------------------------*
149200100920      **
149300100920     C                   if        se_errore   = 'S'
149400100920     c                   goto      end_UNB
149500100920     c                   end
149600100812      **
149700100812      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
149800100812     C                   If        PT_trovata = 'S'
149900100812      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
150000100812      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
150100100812      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
150200100812      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
150300100812      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
150400100812     c                   movel     UNB_diWork    UNB_Parz
150500100812     C                   z-add     Nr_PT         savXX_PT_key
150600100812      * ?- Se ha più LINEE da gestire                                      ------ */
150700100812     C                   eval      PT_con_piu_Nazioni = §puPLN
150800100812     c                   end
150900090209      **
151000090210     c     end_UNB       endSR
151100090227      * ?================================================================== */
151200090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
151300090227      * ?================================================================== */
151400090227     C     DATI_UNH      BEGSR
151500100316      *
151600100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
151700100701      *
151800090211      ** Riferimento del cliente
151900100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
152000100701      * ?                                       ------------
152100100701     C                   MOVEL     UNH_Rifer_Msg UNH_VABrma
152200100701      * ?                                       ------------
152300100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
152400100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
152500100701     C                   SUB       1             lunghezza_num
152600100701     C                   EXSR      Se_Numerico
152700100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
152800090211      * ?                                       --------
152900100701     C                   MOVEL     Numero_OK     UNH_VABrmn
153000090211      * ?                                       --------
153100090211     C                   END
153200090303      **
153300090210     c     end_UNH       endSR
153400100701      * ?================================================================== */
153500100701      *? dati iniziali   BGM
153600100701      * ?================================================================== */
153700100701     C     DATI_BGM      BEGSR
153800100701      *
153900100701      *  identificativo testata
154000100706     c                   eval       BGM_Id_Testata = BGM1004
154100100706     C                   IF           BGM_Id_Testata <> *blank
154200100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
154300100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
154400100706     c                   end
154500100706      *
154600100701      **
154700100701     c                   endSR
154800100701      * ?================================================================== */
154900100701     C*?  Contatori in testata o in Dettaglio
155000100701      * ?================================================================== */
155100100701     C     DATI_CNT      BEGSR
155200100701      **
155300100701     c                   IF        Dati_di_Testata  ='S'
155400100701      **
155500100701      *  TOTALE colli, peso, spedizioni:
155600100701     c                   if        cnt6069 = Totale_Peso_Lordo
155700100811     c                   z-add     cnt6066       Tot_Peso_msg     15 3
155800100701     c                   elseIf    cnt6069 = Totale_Nr_Spedizioni
155900100811     c                   z-add     cnt6066       Tot_Sped_msg     15 3
156000100701     c                   elseIf    cnt6069 = Totale_Nr_Colli
156100100811     c                   z-add     cnt6066       Tot_Colli_msg    15 3
156200100701     c                   elseIf    cnt6069 = Totale_Peso_Netto
156300100811     c                   z-add     cnt6066       Tot_PNetto_msg   15 3
156400100701     c                   end
156500100701      **
156600100813     c                   elseIF    Dati_di_Testata <>'S'
156700100813     C* Totali della spedizione: dentro il Dettaglio
156800100813      **
156900100813     C*  totale peso
157000100813     c                   if        §pvPEs  = 'S'
157100100813     C*
157200100813     c                   if        CNT6069 = Totale_Peso_Lordo
157300100813      *          Kilogrammi
157400100813     c                   if        CNT6411 = Kilograms
157500100813     C                   Z-ADD     CNT6066       vabPKB                          Peso
157600100813      *          Grammi
157700100813     c                   elseIF    CNT6411 = grams
157800100813     C     CNT6066       div       1000          vabPKB                          Peso
157900100813     c                   end
158000100813     c                   end
158100100813      **
158200100813     c                   end
158300100813     C*  totale colli
158400100813     c                   if        §pvCOl  = 'S'
158500100813     C*
158600100813     c                   if        cnt6069 = Totale_Nr_Colli
158700100813     C                   Z-ADD     cnt6066       vabNCL                          N.Colli
158800100813     C                   ADD       VABNCL        Tot_Colli
158900100813     c                   end
159000100813      **
159100100813     c                   end
159200100813      *
159300100813      **
159400100701     C                   END
159500100701      **
159600100701     c     end_CNT       endSR
159700100701      * ?================================================================== */
159800100909     C*?  i Riferimenti Spedizione / Clienti particolari testata/dettaglio
159900100701      * ?================================================================== */
160000100701     C     DATI_RFF      BEGSR
160100100701      **
160200100702      * ?   X_Cargo_Manifest o X_Numero_CMR
160300100701      **
160400100701     C*  Se tipo documento CMR ('CMR')
160500100701     C                   if        RFF1153 =  X_Cargo_Manifest
160600100701     C                   MOVEL     RFF1154       RFF_NrFviaggio                 * WNUMFV
160700100701     C*  Se tipo documento AFB (Foglio viaggio)
160800100701     C                   elseIF    RFF1153 =  X_Numero_CMR
160900100701     C                   MOVEL     RFF1154       RFF_NrCMR                      * WNRCMR
161000100701     C                   END
161100100910     C*
161200100910      *  --------------------------*
161300100910      *  Prende il riferimento se non era stato precedentemente caricato
161400100910      *     o se sulla PT c'è la forzatura a livello segnacollo.
161500100910     c                   if        Primo_RFFcollo = *blank
161600100701      *
161700100909      * ? Riferimento singola spedizione
161800100909     C*------------------
161900100910      * ?Riferimento AlfaNumerico preso dal primo dei segnacolli
162000100909     C*------------------
162100100910     C                   IF             Rifer_Alfanum = *BLANK or
162200100910     C                              RFF_DalPrimoCollo = 'S'
162300100910     C*--------
162400100701     c                   if        RFF1153 = Riferimento_Spedizione  and
162500100701     c                                RFF1154 <> *BLANKS
162600100701     C                   movel(p)  RFF1154       Rifer_Alfanum
162700100701      ** controlla se contiene solo numeri
162800100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
162900100701     C                   movel     Rifer_Alfanum test_num
163000100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
163100100701     C                   sub       1             lunghezza_num
163200100701     C                   EXSR      Se_Numerico
163300100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
163400100701     C                   movel     Rifer_Alfanum VABRMA
163500100910     c                   eval      Primo_RFFcollo = 'S'
163600100910      *
163700100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
163800100701     C                              Tipo_Rifer = *blanks
163900100701     C                   movel     Numero_OK     VABRMN
164000100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
164100100701     C                   ENDIF
164200100701     C                   ENDIF
164300100701     C                   ENDIF
164400100910     C                   ENDIF
164500100910      *
164600100910     c                   else
164700100910      *
164800100910      * Se ci sono ulteriori Riferimenti e sono da riportare su VABRMO
164900100910     c                   if        RFF_DalPrimoCollo ='S' and
165000100910     c                             RFF_DopoIlPrimoCollo ='S' and
165100100910     c                             RFF1153 = Riferimento_Spedizione and
165200100910     c                             RFF1154 <> *BLANKS and
165300100910     c                             RFF1154 <> Rifer_Alfanum
165400100910      *
165500100910     c                   eval      conta_rif = conta_rif + 1
165600100910      *
165700100910      *  al max possono essere 2 riferimenti differenti dal principale
165800100910     c                   if        conta_rif <= 2
165900100910     C                   movel     RFF1154       Secondo_Rifer
166000100910      *
166100100910     C                   IF         Secondo_Rifer <> *BLANKS
166200100920     C     ' '           scan      Secondo_Rifer WLUNGH            3 0
166300100910     C                   sub       1             WLUNGH
166400100910      *
166500100910     c                   if        conta_rif = 1
166600100910     c                   clear                   SUvabrmo
166700100910     c                   end
166800100910      *
166900100910     C                   eval      SUvabRMo = %Trim(suVABRMo) + '.' +
167000100910     C                             %Subst(Secondo_Rifer:1:WLUNGH)
167100100910     C                   ENDIF
167200100910     c                   endIF
167300100910      *
167400100910     c                   end
167500100910     c                   endIF
167600100910      *
167700100910     C*----------------------------------------------------------------
167800100910      *  Se ho CU prendo il riferimento numerico
167900100910      *  ECCEZIONE: se ho già impostato FF obbligatorio
168000100910     c                   if        Primo_RCUcollo = *blank
168100100910      *
168200100910     c                   if        Rifer_Numeric = *blank or
168300100910     c                             RFF_DalPrimoCollo = 'S'
168400100909     C*------------------
168500100909      * ?Riferimento Numerico
168600100909     C*------------------
168700100909     C                   if        RFF1153 = Riferimento_Numerico
168800100909     C                              and RFF1154 <> *blanks
168900100909     C                   movel     RFF1154       Rifer_Numeric
169000100909      * Numero Ordine di vendita
169100100909     C                   movel     Rifer_Numeric test_num
169200100909     C     ' '           scan      Rifer_Numeric lunghezza_num
169300100909     C                   sub       1             lunghezza_num
169400100909     C                   EXSR      Se_Numerico
169500100909      *
169600100909     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
169700100910     c                   eval      Primo_RCUcollo = 'S'
169800100909     C                   movel     Numero_OK     VABRMN
169900100909     C                   eval       Tipo_Rifer = Riferimento_Numerico
170000100909     C                   Endif
170100100909      *
170200100909     C                   ENDIF
170300100909      *
170400100910     C                   ENDIF
170500100910      *
170600100910     c                   else
170700100910      *
170800100910      * Se ci sono ulteriori Riferimenti Numerici non vengono considerati
170900100910     c                   if        RFF_DalPrimoCollo ='S' and
171000100910     c                             RFF_DopoIlPrimoCollo ='S' and
171100100910     c                             RFF1153 = Riferimento_Spedizione and
171200100910     c                             RFF1154 <> *BLANKS and
171300100910     c                             RFF1154 <> Rifer_Numeric
171400100910      *    ??????
171500100910     c                   end
171600100910      *
171700100910     C                   EndIF
171800100910      *
171900100910     C*----------------------------------------------------------------
172000100909      * NOn c'è l'identificativo bolla del cliente
172100100909     c                   if        (RFF1153 = Riferimento_Spedizione and
172200100909     c                              RFF1154 = *BLANKS)
172300100909     c                             or
172400100909     c                             (RFF1153 = Riferimento_Numerico and
172500100909     c                              RFF1154 = *BLANKS)
172600100909     C                   eval      vinMSG = 'RFF riferimento assente'
172700100909     c                   exsr      Error_DET
172800100909     c                   goto      end_RFF
172900100909     C                   EndIF
173000100909     C*------------------
173100100909      * ?Codice Cliente di riferimento
173200100909     C*------------------
173300110224     C                   clear                   Rifer_Cliente    35
173400100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
173500100701     C                              and RFF1154 <> *BLANKS
173600100701     C                   movel     RFF1154       Rifer_Cliente
173700100701     c                   END
173800100909      *
173900100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
174000100701      *
174100100701     C     Rifer_Cliente IFNE      *BLANKS
174200100701      *
174300100701     C                   eval      XX = 1
174400100701     C                   movel     §PTKSC        Key_CL           35
174500100701     C                   movel     Rifer_Cliente WCOM28           28
174600100701     C                   move      WCOM28        Key_CL
174700100701      *
174800100701     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
174900100813      *
175000100813      * ?Se il Partner è uno di quelli che gestisce + linee quindi ha un UNB parziale
175100100813      * ?potrebbe non agganciare correttamente il cliente quindi occorre decodificare
175200100813      * ?il dato cercandolo anche con gli altri codici KSC del Partner Generico ossia
175300100813      * ?con i primi 31 caratteri dell'UNB.
175400100813     c  N34              if        UNB_parziale ='S' and
175500100813     c                              PT_con_piu_Nazioni = 'S'
175600100813     c                   do        PT_KeyXsav    x
175700100813      *
175800100813     c                   if        PT_Parz(x) = UNB_generico
175900100813     C                   movel     PT_KSC(X)     Key_CL           35
176000100813     C                   movel     Rifer_Cliente WCOM28           28
176100100813     C                   move      WCOM28        Key_CL
176200100813     C                   eval      XX = 1
176300100813     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
176400100813      * se trovato esce immediatamente
176500100813     c   34              leave
176600100813     c                   end
176700100813      *
176800100813     C                   endDO
176900100813     c                   endIF
177000100813      *
177100100813      *   Se trovato vale x tutto il messaggio
177200100701     C     *IN34         IFEQ      '1'
177300100701     C                   movel     Des_Tb_CL(XX) EDIDSCL
177400100813      *
177500100701      * in testata messaggio
177600100701     c                   if        Dati_di_Testata  ='S'
177700100701     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
177800100701     C                   movel     §CLtar        Tes_RFF_FF_tar    3
177900100701     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
178000100701     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
178100100701     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
178200100701     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
178300100701     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
178400100701     c                   else
178500100701      * in dettaglio Messaggio
178600100701     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
178700100701     C                   movel     §CLtar        Det_RFF_FF_tar    3
178800100701     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
178900100701     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
179000100701     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
179100100701     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
179200100701     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
179300100701     c                   end
179400100701      *
179500100701      * imposta la LNP  e la Serie x il dettaglio specifico
179600100702     c
179700100701      *  In base al cod.trattamento merce reperisco tipo tratt.
179800100701      *    per un dettaglio specifico.
179900100701     C                   CLEAR                   DS1B
180000100701     C                   Z-ADD     1             Y                 3 0
180100100706     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
180200100701     C     *IN32         IFEQ      '1'
180300100701     C                   eval       DS1B = Des_Tb_CTM(Y)
180400100701     C                   END
180500100701      *
180600100701      * Se il cliente non era sulla schiera lo aggiunge
180700100701     C     §CLksc        lookup    skCLienti                              39
180800100701     C                   if        *in39 = *off
180900100701     C                   add       1             sk
181000100701     C                   z-add     §CLksc        skCLienti(sk)
181100100701     c                   endif
181200100701      *
181300100701     C                   ENDIF
181400100701     C                   ENDIF
181500100702      *
181600100702      * in Dettaglio  preimposto i campi rilevati dalle tabelle
181700100702      *   per clienti particolari (CL)
181800100702      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
181900100702      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
182000100702      *   successivi per altre decodifiche.
182100100702     c                   if        Dati_di_Testata <>'S'
182200100702      *
182300100702      *   se aveva un RFF+FF in dettaglio
182400100702     C                   if         Det_RFF_FF_ksc > 0
182500100702      *
182600100702     C                   eval      VABccm = Det_RFF_FF_ksc
182700100706     C                   movel     Det_RFF_FF_tarVABctr
182800100702     c                   eval      VABlnp = Det_RFF_FF_lnp
182900100702     c                   eval      VABnrs = Det_RFF_FF_nrs
183000100702     C                   eval      VABctm = Det_RFF_FF_ctm
183100100702      *
183200100702      *   altrimenti se aveva un RFF+FF in Testata
183300100702     c                   elseif     Tes_RFF_FF_ksc > 0
183400100702      *
183500100702     C                   eval      VABccm = Tes_RFF_FF_ksc
183600100706     C                   movel     Tes_RFF_FF_tarVABctr
183700100702     c                   eval      VABlnp = Tes_RFF_FF_lnp
183800100702     c                   eval      VABnrs = Tes_RFF_FF_nrs
183900100702     C                   eval      VABctm = Tes_RFF_FF_ctm
184000100702      *
184100100702     c                   end
184200100702      *
184300100702     C                   ENDIF
184400100909      *
184500100701     c     end_RFF       endSR
184600100701      * ?================================================================== */
184700100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
184800100701      * ?================================================================== */
184900100701     C     DATI_DTM      BEGSR
185000100705      *
185100100705      *   Cerca la decodifica del dato in TABELLA
185200100705     C                   movel(p)  'DTM'         etbSGM
185300100705     C                   movel(p)  dtm2005       etbVALSGM
185400100705     c                   exsr      In_TAB_EDSTBL
185500100705      *
185600100701     C*  Se data documento  memorizzo numero e data x CMR
185700100705     C                   If        Quale_campo ='DATDOC'
185800101015     c                              and dtm2380 <> *blank
185900100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
186000100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
186100100702     C                   EXSR      Converte_Data
186200100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
186300100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
186400100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
186500100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
186600100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
186700100701     C     WERRO         IFEQ      'S'
186800100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
186900100706     C                             dtm2005  +
187000100702     C                              ') data errata'
187100100701     c                   exsr      Error_DET
187200100701     c                   goto      end_DTM
187300100701     C                   END
187400100701     C                   END
187500100702     C*
187600100702     C* Data Partenza del mezzo
187700100705     C                   If        Quale_campo ='DATPAR'
187800101015     c                              and dtm2380 <> *blank
187900100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
188000100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
188100100702     C                   EXSR      Converte_Data
188200100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
188300100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
188400100702     C     WERRO         IFEQ      'S'
188500100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
188600100706     C                             dtm2005  +
188700100702     C                              ') data errata'
188800100702     c                   exsr      Error_DET
188900100702     c                   goto      end_DTM
189000100702     C                   END
189100100702     C                   END
189200100702     C*
189300100702     C* Data Previsto Arrivo del mezzo
189400100705     C                   If        Quale_campo ='DATARR'
189500101015     c                              and dtm2380 <> *blank
189600100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
189700100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
189800100702     C                   EXSR      Converte_Data
189900100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
190000100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
190100100702     C     WERRO         IFEQ      'S'
190200100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
190300100706     C                             dtm2005  +
190400100702     C                              ') data errata'
190500100702     c                   exsr      Error_DET
190600100702     c                   goto      end_DTM
190700100702     C                   END
190800100702     C                   END
190900100701      **
191000100701     C*  Se data consegna richiesta imposto già sui campi del VAB
191100100705     C                   If        Quale_campo ='VABDCR'
191200101015     c                              and dtm2380 <> *blank
191300100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
191400100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
191500100702     C                   EXSR      Converte_Data
191600100811      **
191700100811     c                   IF        Dati_di_Testata  ='S'
191800100811     C                   MOVEL     WCAMG         DTM_DtConsRic                  Data cons.rich.
191900100811     C                   MOVEL     WHMS          DTM_OraCnsRic                  Ora  cons.rich.
192000100811     c                   end
192100100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
192200100811     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
192300100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
192400100701     C     WERRO         IFEQ      'S'
192500100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
192600100706     C                             dtm2005  +
192700100702     C                              ') data errata'
192800100701     c                   exsr      Error_DET
192900100701     c                   goto      end_DTM
193000100701     C                   END
193100100702     C                   end
193200100701      **
193300100701     c     end_DTM       endSR
193400100813      * ?================================================================== */
193500100813     C*?  Tipo Servizio
193600100813      * ?================================================================== */
193700100813     C     DATI_TSR      BEGSR
193800100813      ***
193900100813     C*  Reperisco priorità spedizione:
194000100813     C                   Z-ADD     1             X
194100100813      ***
194200100813      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
194300100813      ***  mentre in tabella EDI EEX è fatto con "1"
194400100813      ***
194500100813     c                   movel(p)  TSR4219       Work_Data__35
194600100813     c                   move      PU_TipoServ   Work_Data__35
194700100813     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
194800100813     C     *IN32         IFEQ      '1'
194900100813      *
195000100813     c                   if        Dati_di_Testata <>'S'
195100100813     C                   eval             VABTSP = TipoServizio(X)
195200100813     c                   elseIf    Dati_di_Testata = 'S'
195300100813     C                   eval         tes_VABtsp = TipoServizio(X)
195400100813     c                   end
195500100813      *
195600100813     C                   ELSE
195700100813      **  Il tipo bolla non è presente
195800100813     C                   eval      vinMSG = 'TSR non in tabella'
195900100813     c*********          exsr      Error_DET
196000100813     c*********          goto      end_TSR
196100100813     C                   END
196200100813     C*
196300100813     C*  Decodifico servizi speciali
196400100813     C     TSR7085       IFNE      *BLANKS
196500100813     C     TSR7085       ANDNE     *LOVAL
196600100813      *
196700100813     C                   Z-ADD     1             X
196800100813     c                   movel(p)  TSR7085       Work_Data__35
196900100813     c                   move      PU_SpecServ   Work_Data__35
197000100813     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
197100100813      * Particolarità
197200100813     C     *IN32         IFEQ      '1'
197300100813     C                   eval       EDIDSSS = UNI_ServSpec(X)
197400100813      * Descrizione
197500100813     C     §SSNOT        IFEQ      'S'
197600100813     c                   if        Dati_di_Testata <>'S'
197700100813     C                   MOVEL     §SSDES        VABNOT
197800100813     c                   elseIf    Dati_di_Testata = 'S'
197900100813     C                   eval       tes_VABnot = §SSDES
198000100813     c                   end
198100100813     C                   END
198200100813      *
198300100813      * x Espresso     "Pre-noon"
198400100813     C     §SSTPS        IFEQ      'S'
198500100813     c                   if        Dati_di_Testata <>'S'
198600100813     C                   MOVEL     'E'           VABTSP
198700100813     c                   elseIf    Dati_di_Testata = 'S'
198800100813     C                   eval         tes_VABtsp = 'E'
198900100813     c                   end
199000100813     C                   END
199100100813      *
199200100813      * x Appuntamento "Booking ins"
199300100813     C     §SSTPS        IFEQ      'A'
199400100813     c                   if        Dati_di_Testata <>'S'
199500100813     C                   MOVEL     'A'           VABTC1
199600100813     c                   elseIf    Dati_di_Testata = 'S'
199700100813     C                   eval         tes_VABtc1 = 'A'
199800100813     c                   end
199900100813     C                   END
200000100813      *
200100100813     C                   ELSE
200200100813      *
200300100813      **  Il tipo bolla non è presente
200400100813     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
200500100813     c*******            exsr      Error_DET
200600100813     c*******            goto      end_TSR
200700100813      *
200800100813     C                   END
200900100813      *
201000100813     C                   END
201100100813      **
201200100813     c     end_TSR       endSR
201300100813      * ?================================================================== */
201400100813     C*?  C.O.D. Monetary Amount
201500100813      * ?================================================================== */
201600100813     C     DATI_MOA      BEGSR
201700100813      *
201800100813      *   Cerca la decodifica del dato in TABELLA
201900100813     C                   movel(p)  'MOA'         etbSGM
202000100813     C                   movel(p)  moa5025       etbVALSGM
202100100813     c                   exsr      In_TAB_EDSTBL
202200100813      *
202300100813     C* Controllo se campo importo numerico con 3 decimali
202400100813     C     moa5004       IFNE      *zeros
202500100813      *
202600100813     C                   If        Quale_campo ='VABIAS'
202700110126     C     moa5004       div       1000          VABIAS                         IMPORTO
202800100813     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
202900100813      *
203000100813     C                   elseIF    Quale_campo ='VABVMD'
203100110126     C     moa5004       div       1000          VABVMD                         MERCE
203200100813     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
203300100813      *
203400100813     C                   elseIF    Quale_campo ='VABCAS'
203500100813     C     §PUCAS        IFEQ      'S'                                          C/Assegno
203600100813      *
203700110126     C     moa5004       div       1000          VABCAS
203800100813     C                   MOVEL     moa6345       VABVCA
203900100813     c                   if        Tes_valuta <> *blank and VABVCA = *blank
204000100813     C                   MOVEL     Tes_valuta    VABVCA
204100100813     c                   end
204200100813      *
204300100813      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
204400100813      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
204500100813     C     §PUTPC        IFEQ      'P'
204600100813     C                   MOVEL     'OM'          VABTIC
204700100813     C                   MOVEL     'OM'          VABtic_work
204800100813     C                   ENDIF
204900100813      *
205000100813     C                   END
205100100813      *
205200100813     C                   EndIF
205300100813     C                   END                                                    Imp.non numer.
205400100813      *
205500100813     c     end_MOA       endSR
205600100813      * ?================================================================== */
205700100813     C*?  FREE TEXT
205800100813      * ?================================================================== */
205900100813     C     DATI_FTX      BEGSR
206000100813      *
206100100813      *   Cerca la decodifica del dato in TABELLA
206200100813     C                   movel(p)  'FTX'         etbSGM
206300100813     C                   movel(p)  ftx4451       etbVALSGM
206400100813     c                   exsr      In_TAB_EDSTBL
206500100813      *
206600100813      *
206700100813      * ? solo se si tratta di note descrittive da comunicare in BOLLA
206800100813     c                   if         quale_campo = 'VABNOT'
206900100813     c                                or ftx4451 = §pvSIN
207000100813     c                                or ftx4451 = §pvICN
207100100813     C                   DO        4             X
207200100813      *
207300100813     C     D05(X)        IFNE      *BLANKS
207400100813      *
207500100813     C     Note_1        IFEQ      *BLANKS
207600100813     C                   MOVEL     D05(X)        Note_1
207700100813     C                   MOVE      D05(X)        Note_2
207800100813     C                   ELSE
207900100813     C     Note_2        IFEQ      *BLANKS
208000100813     C                   MOVEL     D05(X)        Note_2
208100100813     C                   END
208200100813     C                   END
208300100813      *
208400100813     C                   ENDif
208500100813      *
208600100813     C                   ENDdo
208700100813     C                   end
208800100813     C*------>>
208900100813     C*  Decodifico tipo pagamento C/Assegno
209000100813      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
209100100813      *      (Problema di pulizia in presenza di + righe di testo)
209200100813      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
209300100813      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
209400100813      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
209500100813      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
209600100813     C                   if        wri_vabtic = *blank
209700100813     C                   MOVEL     *BLANKS       VABTIC
209800100813     c                   end
209900100813      **
210000100813     c                   clear                   trovato_TPInc     1            TipoIncasso
210100100813      *
210200100813      * ?Se c'è il tipo Incasso
210300100813     c                   if         quale_campo = 'VABTIC'                      -- 01 --
210400100813      *
210500100813     C                   DO        4             X                              -- 02 --
210600100813     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
210700100813     C                   MOVEL     D05(X)        WCTC              2
210800100813     C                   Z-ADD     1             Y
210900100813     c                   movel(p)  Wctc          Work_Data__35
211000100813     c                   move      §puTC         Work_Data__35
211100100813     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
211200100813      * SE PARTNER IMPOSTO 'OM'
211300100813     C     §PUTPC        IFEQ      'P'
211400100813     c     wctc          andne     'TO'
211500100813     C                   MOVEL     'OM'          VABTIC
211600100813     C                   ELSE
211700100813     C   33              eval      VABTIC = TipoIncasso(Y)
211800100813     c   33              move      'S'           trovato_TPInc
211900100813     C                   END                                                    -- 03 --
212000100813     C                   END                                                    -- 03 --
212100100813     C                   ENDdo                                                  -- 02 --
212200100813      *
212300100813     C                   ELSE                                                   -- 01 --
212400100813      *
212500100813      * ?Se NON c'è il tipo Incasso Contrassegno particolare
212600100813      *    Deve essere impostato comunque da testata
212700100813     c                   if        vabtic = *blank
212800100813     C                   MOVEL     VABtic_work   VABTIC
212900100813     c                   end
213000100813      *
213100100813     C                   END                                                    -- 01 --
213200100813      *
213300100813      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
213400100813      *    essere presenti altri record Flat con altre informazioni
213500100813      *    che potrebbero abblancare il Tipo incasso precedentemente
213600100813      *    decodificato
213700100813     c                   if        Trovato_TPInc = 'S' and
213800100813     c                                 wri_vabtic = *blank
213900100813     c                   eval      wri_vabtic = 'S'
214000100813     c                   end
214100100813      *
214200100813      * ?Se Natura Merce      .
214300100910     C*  Natura Merce  (Se abilitato alla trascodifica)
214400100910     c                   if        §PUnas = 'S'
214500100813     c                   if         quale_campo = 'VABNAS'                      -- 01 --
214600100813     C                   MOVEL     D05(1)        VABnas
214700100813     c                   End
214800100910     c                   End
214900100813      *
215000100813     c     end_FTX       endSR
215100100701      * ?================================================================== */
215200100701     C*? Imposta informazioni sul dettaglio del trasporto
215300100701      * ?================================================================== */
215400100701     C     DATI_TDT      BEGSR
215500100701     C*
215600100701      **
215700100701     c     end_TDT       endSR
215800100811      * ?================================================================== */
215900100811     C*? Imposta
216000100811      * ?================================================================== */
216100100811     C     DATI_TMP      BEGSR
216200100811     C*
216300100811      **
216400100811     c     end_TMP       endSR
216500100811      * ?================================================================== */
216600100811     C*? Imposta
216700100811      * ?================================================================== */
216800100811     C     DATI_TMD      BEGSR
216900100811     C*
217000100811      **
217100100811     c     end_TMD       endSR
217200100811      * ?================================================================== */
217300100811     C*? Imposta
217400100811      * ?================================================================== */
217500100811     C     DATI_TCC      BEGSR
217600100811     C*
217700100811      **
217800100811     c     end_TCC       endSR
217900100811      * ?================================================================== */
218000100811     C*? Imposta
218100100811      * ?================================================================== */
218200100811     C     DATI_DIM      BEGSR
218300100811     C*
218400100811      **
218500100811     c     end_DIM       endSR
218600100811      * ?================================================================== */
218700100811     C*? Imposta
218800100811      * ?================================================================== */
218900100811     C     DATI_RNG      BEGSR
219000100811     C*
219100100811      **
219200100811     c     end_RNG       endSR
219300100811      * ?================================================================== */
219400100811     C*? Imposta
219500100811      * ?================================================================== */
219600100811     C     DATI_DOC      BEGSR
219700100811     C*
219800100811      **
219900100811     c     end_DOC       endSR
220000100811      * ?================================================================== */
220100100811     C*? Imposta
220200100811      * ?================================================================== */
220300100811     C     DATI_GIN      BEGSR
220400100811     C*
220500100920      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
220600100920      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
220700100920     C     §1BF12        IFEQ      'E'
220800100920     C                   EXSR      SEGNaCollo_EEX
220900100920     C                   ELSE
221000100920      *
221100100920      *  La prima volta controllo congruità numero segnacollo
221200100920     C     §PUBS1        IFNE      *BLANKS
221300100920     C     VABnrs        ANDNE     0
221400100920      *  Se il codice trattamento merce prevede che segnacollino i
221500100920      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
221600100920     C     §1BFG7        IFEQ      'S'
221700100920     C     §1BFG1        OREQ      'S'
221800100920      *  calcolo segnacollo
221900100920     C                   EXSR      Segn_Bartolini
222000100920     C                   END
222100100920     C                   END
222200100920      *
222300100920     C                   END
222400100811      **
222500100811     c     end_GIN       endSR
222600100811      * ?================================================================== */
222700100811     C*? Imposta
222800100811      * ?================================================================== */
222900100811     C     DATI_HAN      BEGSR
223000100811     C*
223100100811      **
223200100811     c     end_HAN       endSR
223300100811      * ?================================================================== */
223400100811     C*? Imposta
223500100811      * ?================================================================== */
223600100811     C     DATI_GOR      BEGSR
223700100811     C*
223800100811      **
223900100811     c     end_GOR       endSR
224000100705      * ?================================================================== */
224100100705     C*? Imposta la Località
224200100705      * ?================================================================== */
224300100705     C     DATI_LOC      BEGSR
224400100705     C*
224500100705      **
224600100705     c     end_LOC       endSR
224700100705      * ?================================================================== */
224800100705     C*? Imposta identificativi delle Merci
224900100705      * ?================================================================== */
225000100705     C     DATI_EQD      BEGSR
225100100705     C*
225200100705      **
225300100705     c     end_EQD       endSR
225400100705      * ?================================================================== */
225500100705     C*? Imposta il numero delle Merci
225600100705      * ?================================================================== */
225700100705     C     DATI_EQN      BEGSR
225800100705     C*
225900100705      **
226000100705     c     end_EQN       endSR
226100100316      * ?================================================================== */
226200100316     C*? Imposta l'inizio del Dettaglio CNI
226300100316      * ?================================================================== */
226400100316     C     DATI_CNI      BEGSR
226500100316     C*
226600100702      ** Inizio Dettaglio riferimento iniziale
226700100702     c                   If        CNI1004 <> *blank
226800100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
226900100813      *
227000100813      *   Nel Riferimento Alfa sempre
227100100813     c                   eval      VABRMA  = CNI_Identificativo_Bolla
227200100813      *
227300100813      *   e nel numerico se di soli numeri e comunque se lo memorizza
227400100813     c                   clear                   CNI_seNumerico
227500100813     C                   eval       test_num = CNI1004
227600100813     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
227700100813     C                   SUB       1             lunghezza_num
227800100813     C                   EXSR      Se_Numerico
227900100813     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
228000100813      * ?                                       --------
228100100813     C                   MOVEL     Numero_OK     VABrmn
228200100813      * ?                                       --------
228300100813     C                   z-add     VABrmn        CNI_seNumerico
228400100813      * ?                                       --------
228500100813     C                   END
228600100813      *
228700100702     c                   end
228800100319      *
228900100316      **
229000100316     c     end_CNI       endSR
229100100316      * ?================================================================== */
229200100316     C*? Imposta le merci pericolose
229300100316      * ?================================================================== */
229400100316     C     DATI_DGS      BEGSR
229500100316     C*
229600100316      **
229700100316     c     end_DGS       endSR
229800100316      * ?================================================================== */
229900100316     C*? Imposta il numero seriale
230000100316      * ?================================================================== */
230100100316     C     DATI_SEL      BEGSR
230200100316     C*
230300100316      **
230400100316     c     end_SEL       endSR
230500100316      * ?================================================================== */
230600100316     C*? Imposta informazioni aggiuntive
230700100316      * ?================================================================== */
230800100316     C     DATI_PIA      BEGSR
230900100316     C*
231000100316      **
231100100316     c     end_PIA       endSR
231200090209      * ?================================================================== */
231300090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
231400090209      * ?================================================================== */
231500090209     C     DATI_TOD      BEGSR
231600090209      *
231700090210      *  Trascodifico tipo trasporto
231800090210     c                   clear                   TOD_char3         3
231900100705      *  DEFAULT da PT
232000090210      *  imposta il default dalla tabella PT se c'è
232100090210     C     §puPFA        ifNE      *blank
232200090210     c                   SELECT
232300090210      *  Porto Franco
232400090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
232500090210     C                   movel     '4'           VABCBO                         + C/Ass
232600090210     c                   WHEN      §puPFA = 'F'
232700090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
232800090210      *  Porto Assegnato
232900090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
233000090210     C                   movel     '6'           VABCBO                         + C/Ass
233100090210     C                   WHEN      §puPFA = 'A'
233200090210     C                   movel     '2'           VABCBO                         Senza C/Ass
233300090210     C                   ENDSL
233400090210     c                   end
233500090210      *
233600090210      *   Imposta o il codice o il metodo
233700090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
233800090210     c                   if        tod4053  <> *blank
233900090210     c                   movel(p)  tod4053       TOD_char3
234000090210     c                   else
234100090210     c                   if        tod4215  <> *blank
234200090210     c                   movel(p)  tod4215       TOD_char3
234300090210     c                   end
234400090210     c                   end
234500110328      ****************
234600110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
234700110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
234800110328      ****************
234900110328     c                   eval      Trovata_Tab_TB ='N'
235000110328      *
235100110328     c                   if              TOD_char3 <> *blank
235200110328     c                   movel(p)  TOD_char3     Key_Generica35
235300110328     c                   move      §puTB         Key_Generica35
235400110328     C                   eval      Y = 1
235500110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
235600110328     c   32              eval      Trovata_Tab_TB ='S'
235700110328      *
235800110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
235900110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
236000110328      *
236100110328     c                   if          Trovata_Tab_TB ='N'
236200110328     c                                 and
236300110328     C                               tod4215 <> *blank
236400110328     c                   movel(p)  tod4215       TOD_char3
236500110328     c                   movel(p)  TOD_char3     Key_Generica35
236600110328     c                   move      §puTB         Key_Generica35
236700110328     C                   eval      Y = 1
236800110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
236900110328     c   32              eval      Trovata_Tab_TB ='S'
237000110328     c                   end
237100110328     c                   end
237200110328      *
237300110328      ****************
237400110328      ******* Se vi sono eccezioni sul cliente imposta quelle
237500110328      ****************
237600110328     c                   If          Trovata_Tab_TB ='S'
237700110328      *
237800110328     c                   SELECT
237900110328     c                   WHEN      Decod_Porto(Y) = 'F'
238000110328     c                               and  VABcas > *zeros
238100110328     C                   movel     '4'           VABCBO                         + C/Ass
238200110328     c                   WHEN      Decod_Porto(Y) = 'F'
238300110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
238400110328      *  Porto Assegnato
238500110328     c                   WHEN      VABCAS > *zeros
238600110328     C                   movel     '6'           VABCBO                         + C/Ass
238700110328     C                   OTHER
238800110328     C                   movel     '2'           VABCBO                         Senza C/Ass
238900110328     C                   ENDSL
239000110328      *
239100110328      ****************  altrimenti
239200110328      ******* Se non vi sono eccezioni per il cliente
239300110328      *   Rileva a questo punto lo Standard per tutti
239400110328      ****************
239500110328     c                   elseIf      Trovata_Tab_TB ='N'
239600090210      **
239700090210     c                   if              TOD_char3 <> *blank
239800100705      *   Cerca la decodifica del dato in TABELLA
239900100705     C                   movel(p)  'TOD'         etbSGM
240000100705     C                   movel(p)  TOD_char3     etbVALSGM
240100100705     c                   exsr      In_TAB_EDSTBL
240200090210      *
240300100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
240400100705      *
240500090210     c                   SELECT
240600100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
240700100705     c                               VABcas > *zeros
240800090210     C                   movel     '4'           VABCBO                         + C/Ass
240900100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
241000090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
241100090210      *  Porto Assegnato
241200090210     c                   WHEN      VABCAS > *zeros
241300090210     C                   movel     '6'           VABCBO                         + C/Ass
241400090210     C                   OTHER
241500090210     C                   movel     '2'           VABCBO                         Senza C/Ass
241600090210     C                   ENDSL
241700090210      *
241800090210     c                   end
241900090210     c                   endIF
242000110328      **
242100110328     c                   ENDif
242200090209      **
242300090210      **  Il tipo bolla non è presente
242400090210     c                   if        VABCBO = *blank
242500090210     C                   eval      vinMSG = 'TOD non rilevato'
242600090213     c                   exsr      Error_DET
242700090210     c                   goto      end_TOD
242800090210     c                   end
242900090210      **
243000090210     c     end_TOD       endSR
243100090213      * ?================================================================== */
243200090213     C*?  Anagrafica del mittente e del destinatario
243300090213      * ?================================================================== */
243400091016     C     DATI_NAD      BEGSR
243500100705      *
243600100705      *   Cerca la decodifica del dato in TABELLA
243700100813     c                   clear                   dopo_NAD_CN       1
243800100705     C                   movel(p)  'NAD'         etbSGM
243900100705     C                   movel(p)  nad3035       etbVALSGM
244000100705     c                   exsr      In_TAB_EDSTBL
244100100708      *-------
244200100708      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
244300100708      *-------
244400100708     C                   IF        Quale_campo ='VABCMR'
244500100708     c                   if        nad30361 <> *blank
244600100708     C                   MOVEL     nad30361      NAD_SF_x_CMR
244700100708     c                   elseIf    nad31241 <> *blank
244800100708     C                   MOVEL     nad31241      NAD_SF_x_CMR
244900100708     c                   elseIf    nad3039  <> *blank
245000100708     C                   MOVEL     nad3039       NAD_SF_x_CMR
245100100708     c                   end
245200100705      *-------
245300090210      * Dati destinatario
245400100705      *-------
245500100708     C                   elseIF    Quale_campo ='VABRSD'
245600100813     c                   move      'S'           dopo_NAD_CN
245700090227      *
245800090227      * Se trovato un indirizzo di destinatario
245900100701     c                   if           Dati_di_Testata = 'N'
246000100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
246100100701     c                   else
246200100701     c                   eval       NAD_destinatario_in_testata = 'S'
246300100701     c                   end
246400090210      *
246500090210     C* Reperisco nazione corrispondente destinatario
246600100705     C                   Z-ADD     1             naz               3 0
246700100705     C                   MOVEL     *BLANKS       VABNZD
246800100705     C                   Z-ADD     0             WFLCPF
246900090210     c                   eval      *in32 = *off
247000100705      ***
247100090210     C                   if        nad3207 <> *BLANKS
247200100705     C     nad3207       LOOKUP    ISO(naz)                               32
247300100705     C                   If        *in32 = *on
247400100705      ***
247500100705      * per reperire il CAP corretto
247600100705     C                   MOVEL     C15(naz)      VABNZD
247700100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
247800100705      ***
247900100705     c                   Endif
248000100705     C                   endif
248100100705      *
248200090210     C* Imposto dati destinatario
248300100705     C                   MOVEL     nad30361      VABRSD
248400100705     C                   if        nad30362 <> *LOVAL
248500100705     C                   MOVEL     nad30362      VABRD2
248600090210     C                   endif
248700100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
248800091019???? C* Imposto dati destinatario
248900091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
249000100705???? C                   MOVEL     nad31241      VABRSD
249100100705???? C                   if        nad31242 <> *LOVAL
249200100705???? C                   MOVEL     nad31242      VABRD2
249300091019???? C                   endif
249400091019???? C                   end
249500100705      *   città/Località
249600090210     C                   MOVEL     nad3164       VABLOD
249700100812      *
249800100812      *  Se richiesto in tabella PT di accodare alla Ragione sociale
249900100812      *  l'informazione di indirizzo invertita nel segmento...
250000100812      *  Esigenza nata da Dior che mette l'indirizzo vero nel 2°campo del Indirizzo NAD
250100100812      *  ossia dopo i 2 punti (:) ed invece mette un'informazione sul destinatario/
250200100812      *  indirizzo nel 1° campo dell'Inidirizzo NAD prima dei 2 punti (:).
250300100812      *   Per nostra comodità mettiamo quindi l'informazione aggiuntiva attaccata alla
250400100812      *   Ragione Sociale e non alla Località poichè altrimenti avremmo continui
250500100812      *   problemi di decodifica Località/Cappario durante la conferma bolle.
250600100812     c                   if        §pv2IN = 'S'
250700100812      *
250800100812     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
250900100812      *
251000100812      * Nell'indirizzo il 2°campo ricevuto
251100100812     C                   MOVEL     NAD30422      VABIND
251200100812      * Mentre appiccica all'estensione della ragione sociale le info di consegna
251300100812      *  messe nel 1° campo dell'indirizzo (DIOR)
251400100812      *
251500100812      *  Verifico se c'è spazio sul campo estensione altrimenti vado in coda
251600100812      *    prima all'indirizzo e poi alla località
251700100812      *
251800100812     C* Verifico se c'è dello spazio per metterlo nell'estensione rag.soc.
251900100812     C     '    '        SCAN      VABRD2        WI                4 0
252000100812     c                   if        WI < 10
252100100812     C     '    '        SCAN      VABrd2        WI                4 0
252200100812     C                   MOVEA     VABrd2        LOD
252300100812     C                   ADD       1             WI
252400100812     C                   MOVEA     NAD30421      LOD(WI)
252500100812     C                   MOVEA     LOD           VABrd2
252600100812      *
252700100812     c                   else
252800100812      *  Verifico se c'è spazio sull'indirizzo altrimenti vado in coda alla località
252900100812     C     '    '        SCAN      VABIND        WI                4 0
253000100812     c                   if        WI < 10
253100100812     C     '    '        SCAN      VABIND        WI                4 0
253200100812     C                   MOVEA     VABIND        LOD
253300100812     C                   ADD       1             WI
253400100812     C                   MOVEA     NAD30421      LOD(WI)
253500100812     C                   MOVEA     LOD           VABIND
253600100812      *
253700100812     c                   else
253800100812      *  oppure    accodato nella località
253900100812     C     '    '        SCAN      VABLOD        WI                4 0
254000100812     C                   MOVEA     VABLOD        LOD
254100100812     C                   ADD       1             WI
254200100812     C                   MOVEA     NAD30421      LOD(WI)
254300100812     C                   MOVEA     LOD           VABLOD
254400100812     c                   end
254500100812     c                   end
254600100812      *-----------
254700100812     c                   else
254800100812      *
254900100812     C                   MOVEL     NAD30421      VABIND
255000100812     c                   end
255100100812      *
255200100812      *-----------
255300100812      *  segue lo standard
255400100812     c                   else
255500100812      *-----------
255600100812      *
255700100812     C                   MOVEL     NAD30421      VABIND
255800100812      *
255900100812     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
256000100812     c                   if        NAD30422 <> *blanks  and NAD30422 <> *loval
256100100812      *
256200100812      *  Verifico se c'è spazio sul primo indirizzo x accodare se richiesto
256300100812      *  altrimenti vado in coda alla località
256400100812     C     '    '        SCAN      VABIND        WI                4 0
256500100812      *
256600100812     C* Verifico se c'è dello spazio per metterlo nella località
256700100812      *  oppure    accodato al primo indirizzo
256800100812      *
256900100812     c                   if        WI < 15
257000100812      *
257100100812     C     '    '        SCAN      VABind        WI                4 0
257200100812     C                   MOVEA     VABind        LOD
257300100812     C                   ADD       1             WI
257400100812     C                   MOVEA     NAD30422      LOD(WI)
257500100812     C                   MOVEA     LOD           VABind
257600100812      *
257700100812     c                   else
257800100812      *
257900100812     C     '    '        SCAN      VABLOD        WI                4 0
258000100812     C                   MOVEA     VABLOD        LOD
258100100812     C                   ADD       1             WI
258200100812     C                   MOVEA     NAD30422      LOD(WI)
258300100812     C                   MOVEA     LOD           VABLOD
258400100812      *
258500100812     c                   end
258600100812      *
258700100812     c                   end
258800100812      *
258900100812      *
259000100812     C                   ENDif
259100100812      * ---------
259200090210      * Converto CAP
259300100705     C                   EXSR      Converte_CAP
259400090603     C*
259500100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
259600100705     C*  record del VAB viene PULITO
259700100316     c                   if        Dati_di_Testata  = 'S'
259800090603     c                   eval       tes_VABrsd = VABrsd
259900090603     c                   eval       tes_VABrd2 = VABrd2
260000090603     c                   eval       tes_VABind = VABind
260100090603     c                   eval       tes_VABlod = VABlod
260200090603     c                   eval       tes_VABnzd = VABnzd
260300090603     c                   end
260400100728      *
260500100728      *   se NON c'è l'identificativo bolla del cliente
260600100728      *    blocca la spedisione e lo segnala.
260700100728     C                   if          vabRSD = *blank
260800100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
260900100728     c                   exsr      Error_DET
261000100728     c                   goto      end_NAD
261100100728     C                   elseIF      vabIND = *blank
261200100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
261300100728     c                   exsr      Error_DET
261400100728     c                   goto      end_NAD
261500100728     C                   elseIF      vabLOD = *blank
261600100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
261700100728     c                   exsr      Error_DET
261800100728     c                   goto      end_NAD
261900100728     C                   ENDIF
262000100705      *-------
262100090210      * Dati mittente
262200100705      *-------
262300100705     C                   elseIF     Quale_campo ='VABRMO'
262400090210      *
262500100705     C                   movel     nad30361      VABRMO
262600090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
262700090210     C                   movel     §PTNAZ        VABNMO
262800090210      *
262900090210     C                   IF        nad3207 <> *blanks
263000100705     C                   eval      naz = 1
263100100705     C     nad3207       LOOKUP    ISO(naz)                               32
263200090210     C                   if         *in32 = *on
263300100705     C                   MOVEL     C15(naz)      VABNMO
263400090210     C                   endif
263500090210     C                   ENDIF
263600090210      *
263700090210      * Normalizzo CAP mittente originale
263800090210     C                   clear                   T
263900090210     C                   clear                   S
264000090210     C                   MOVEA     *BLANKS       CAPM
264100090210     C                   MOVEA     nad3251       CAP9
264200090210     C                   DO        9             T
264300090210     C     CAP9(T)       IFNE      *BLANKS
264400090210     C                   ADD       1             S
264500090210     C                   MOVE      CAP9(T)       CAPM(S)
264600090210     C                   ENDIF
264700090210     C                   ENDDO
264800090210     C*
264900090210      *     Controllo validità CAP
265000090210     C                   MOVEA     CAPM          VABCMO
265100090210      *
265200090210     C                   CLEAR                   TISI95DS
265300090210     C                   MOVEL     VABCMO        I95CAP
265400090210     C                   MOVEL     VABNMO        I95NAR
265500090210     C                   MOVEL     WOGGI         I95DAT
265600090210     C                   MOVEL     '3'           I95TCN
265700090210     C                   CALL      'TISI95R'
265800090210     C                   PARM                    TISI95DS
265900090210      *     Errore
266000090210     C     O95ERR        IFNE      *BLANKS
266100090210     C                   MOVEL     INDCAE        VABCMO
266200090210     C                   MOVEL     §PTNAZ        VABNMO
266300090210     C                   END
266400090603      *
266500100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
266600100705     C*  record del VAB viene PULITO
266700100316     c                   if        Dati_di_Testata  = 'S'
266800090603     c                   eval      tes_VABrmo = VABrmo
266900090603     c                   eval      tes_VABnmo = VABnmo
267000090603     c                   eval      tes_VABcmo = VABcmo
267100090603     c                   eval      NAD_mittente_in_testata = 'S'
267200100701     c                   else
267300100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
267400090603     c                   end
267500100728      *
267600100728      *   se NON c'è l'identificativo bolla del cliente
267700100728      *    blocca la spedisione e lo segnala.
267800100728     C                   if          vabRMO = *blank
267900100728     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
268000100728     c                   exsr      Error_DET
268100100728     c                   goto      end_NAD
268200100728     C                   ENDIF
268300090210      *
268400090210     C                   END
268500090210      *
268600090210     c     end_NAD       endSR
268700090210      * ?================================================================== */
268800090210     C*?  Contatto a cui far riferimento
268900090210      * ?================================================================== */
269000090210     C     DATI_CTA      BEGSR
269100090210      *
269200090210      *  memorizza il riferimento di consegna x il destinatario
269300100813     C                   if         cta3412 <> *BLANKS and
269400100813     c                                    dopo_NAD_CN = 'S'
269500100813      *
269600100701     c                   if        Dati_di_Testata  ='S'
269700100812     c                   if           CTA_Destinatario_in_testata   = *blanks
269800100812     c                   eval       CTA_Destinatario_in_testata   = cta3412
269900100812     c                   end
270000100701     c                   else
270100100812     c                   if           CTA_Destinatario_in_dettaglio = *blanks
270200100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
270300100812     c                   end
270400100701     c                   end
270500100701     c                   end
270600090210      *
270700090210     c                   endSR
270800090210      * ?================================================================== */
270900090210     C*?  Contatto telefonico  a cui far riferimento
271000090210      * ?================================================================== */
271100090210     C     DATI_COM      BEGSR
271200090210      *
271300100705      *  memorizza il riferimento di consegna nr.telefonico
271400100813     C                   if         com3148 <> *BLANKS and
271500100813     c                                  COM3155 = 'TE' and
271600100813     c                                    dopo_NAD_CN = 'S'
271700100813      *
271800100701     c                   if        Dati_di_Testata  ='S'
271900100812     c                   if           COM_telefono_in_testata = *blanks
272000100812     c                   eval      COM_telefono_in_testata   = com3148
272100100812     c                   end
272200100701     c                   else
272300100812     c                   if           COM_telefono_in_dettaglio = *blanks
272400100701     c                   eval      COM_telefono_in_dettaglio = com3148
272500100812     c                   end
272600090210     c                   end
272700100705     c                   endif
272800090210      *
272900090209     C                   ENDSR
273000090210      * ?================================================================== */
273100090210     C*?  Dati di dettaglio della spedizione e dei colli
273200090210      * ?================================================================== */
273300090210     C     DATI_GID      BEGSR
273400090210      *
273500100910      *  Se non richiesto quello sulla testata dettaglio
273600100920     c                   if        §pvCOl  <> 'S' or vabPKB = 0
273700100910      *
273800090210     C* Se ho totali per spedizione metto in campi VAB
273900090210     C     gid1496       IFEQ      99999
274000090210     C     gid1496       orEQ      9999
274100100630     c                   eval      GID_aTotale = 'S'
274200090210     C                   z-add     gid722420     VABNCL
274300090210     C                   ELSE
274400100630     c                   If        GID_aTotale = 'N'
274500100705     C                   z-add     gid722420     GID_ColliInAdd
274600100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
274700090210     C                   END
274800090210     C                   END
274900090212      *
275000100701     C                   ADD       VABNCL        ToT_Colli        16 0
275100090210      *
275200100910     C                   END
275300100910      *
275400090210     c     end_GID       endSR
275500090210      * ?================================================================== */
275600090210     C*?  Dati di dettaglio misure spedizione e colli
275700090210      * ?================================================================== */
275800090210     C     DATI_MEA      BEGSR
275900090210      *
276000100910     C*  Se non si deve prendere il peso dal Totale (CNT)
276100100920     c                   if        §pvPEs <> 'S' or vabPKB = 0
276200090210     C*  Peso
276300100319     C     mea6311       IFEQ      Weight
276400100319     C     mea6411       ANDEQ     Kilograms
276500090210      *
276600090210     C*  Controllo se ho valore totale o parziale
276700090210      *   sempre il Lordo
276800100319???? C     mea6313       IFEQ      G_Weight
276900100319???? C     mea6313       oreq      Gross_Weight
277000110126???? C     mea6313       oreq      Gross_Weight_D
277100100630     C                   IF        GID_aTotale = 'S'
277200110126???? C                   move      mea6314       mea6314_6        18 6          Peso
277300110126???? C                   Z-ADD     mea6314_6     VABPKB                         Peso
277400090210     C                   ELSE
277500110126???? C                   move      mea6314       mea6314_6                      Peso
277600110126???? C                   ADD       mea6314_6     VABPKB                         Peso
277700090210     C                   END
277800091203     C                   END
277900090210      *
278000100319???? C     mea6313       IFEQ      G_Weight
278100100319???? C     mea6313       oreq      Gross_Weight
278200110126???? C     mea6313       oreq      Gross_Weight_D
278300110126???? C                   move      mea6314       mea6314_6                      Peso
278400110126???? C                   ADD       mea6314_6     ToT_PesoLordo
278500090210     C                   END
278600100706???? C     mea6313       IFEQ      N_Weight
278700100706???? C     mea6313       oreq      Net_Weight
278800110126???? C     mea6313       oreq      Gross_Weight_D
278900110126???? C                   move      mea6314       mea6314_6                      Peso
279000110126???? C                   ADD       mea6314_6     ToT_PesoNetto
279100090210     C                   END
279200090210     C                   END
279300090210      ***
279400090210     C*  Peso in Grammi  se abilitato a utilizzarlo
279500090210      ***
279600100319     C     mea6311       IFEQ      Weight
279700100319     C     mea6411       ANDEQ     Grams
279800090210      *
279900090210     C*  Controllo se ho valore totale o parziale
280000100319???? C     mea6313       IFEQ      G_Weight
280100100319???? C     mea6313       oreq      Gross_Weight
280200110126???? C     mea6313       oreq      Gross_Weight_D
280300100630     c                   If        GID_aTotale = 'S'
280400110126???? c                   move      mea6314       mea6314_6
280500110126???? C     mea6314_6     div       1000          VABPKB                         Peso
280600090210     C                   ELSE
280700110126???? c                   move      mea6314       mea6314_6
280800110126???? C     mea6314_6     div       1000          Peso_inKG        12 3          Peso
280900091019???? C                   add       Peso_inKG     VABPKB                         Peso
281000090210     C                   END
281100090210     C                   END
281200090210      *
281300100319???? C     mea6313       IFEQ      G_Weight
281400100319???? C     mea6313       oreq      Gross_Weight
281500110126???? C     mea6313       oreq      Gross_Weight_D
281600110126???? C     mea6314_6     div       1000          Peso_inKG        12 3          Peso
281700100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
281800090210     C                   END
281900100706???? C     mea6313       IFEQ      N_Weight
282000100706???? C     mea6313       oreq      Net_Weight
282100110126???? C     mea6313       oreq      Gross_Weight_D
282200110126???? C     mea6314_6     div       1000          Peso_inKG        12 3          Peso
282300100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
282400090210     C                   END
282500090210     C                   END
282600100910      *
282700100910     c                   end
282800100910      * -------------
282900090210      *  Volume
283000100319???? C     mea6311       IFEQ      Volume
283100100319???? C     mea6411       ANDEQ     Cubic_Meters
283200090210      *  Controllo se ho valore totale o parziale
283300100630     c                   If        GID_aTotale = 'S'
283400110126???? C                   move      mea6314       mea6314_6                      Volume
283500110126???? C                   Z-ADD     mea6314_6     VABVLB                         Volume
283600090210     C                   ELSE
283700110126???? C                   move      mea6314       mea6314_6                      Volume
283800110126???? C                   ADD       mea6314_6     VABVLB                         Volume
283900090210     C                   END
284000090210     C                   END
284100100705      *
284200100910      * -------------
284300100705     C* Ricerco CAP in base alle dimensioni
284400090210     C     WCAP          IFNE      *BLANKS
284500100319     C     mea6311       IFEQ      Weight
284600090210      *
284700090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
284800090210     C                   CLEAR                   FNLV55DS
284900090210     C                   MOVEL     'P'           D55TPT
285000090210     C                   MOVEL     WOGGI         D55DRF
285100090210     C                   MOVEL     VABLNP        D55LNP
285200090210     C                   MOVEL     VABLNP        D55LIN
285300090210     C                   CALL      'FNLV55R'
285400090210     C                   PARM                    FNLV55DS
285500090210     C                   MOVE      D55TFP        COMTFP            3 0
285600090210      *
285700090210     C*  PASSO IL TERMINAL PARTENZA
285800090210     C                   CLEAR                   TISI95DS
285900090210     C                   Z-ADD     COMTFP        I95TFP
286000090210     C                   MOVEL     VABTSP        I95TSP
286100090210     C                   MOVEL     'S'           I95FRE
286200090210     C                   MOVEL     '3'           I95TCN
286300090210     C                   MOVEL     WCAP          I95CAP
286400090210     C                   MOVEL     VABNZD        I95NAR
286500090210     C                   MOVEL     VABLOD        I95LOC
286600090210     C                   MOVEL     VABIND        I95IND
286700090210      *
286800090210     C* Se gestita seconda linea di arrivo passo peso - volume
286900090210     C* e numero colli effettivo
287000090210     C     §PULNA        IFEQ      'S'
287100090210     C                   Z-ADD     VABPKB        I95LKG
287200090210     C                   Z-ADD     VABVLB        I95LMC
287300090210     C                   ELSE
287400100706     C*******
287500090210     C* Altrmenti passo 1 Kg e 0,001
287600100706     C*******              Z-ADD1                  I95LKG
287700100706     C*******              Z-ADD0,001              I95LMC
287800100706     C*******
287900090210     C                   END
288000090210     C*
288100090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
288200090210     C                   SELECT
288300090210     C     VABCBO        WHENEQ    '1 '
288400090210     C                   MOVEL     *BLANKS       I95FCA
288500090210     C                   MOVEL     'F'           I95TPO
288600090210     C     VABCBO        WHENEQ    '2 '
288700090210     C                   MOVEL     *BLANKS       I95FCA
288800090210     C                   MOVEL     'A'           I95TPO
288900090210     C     VABCBO        WHENEQ    '4 '
289000090210     C                   MOVEL     'S'           I95FCA
289100090210     C                   MOVEL     'F'           I95TPO
289200090210     C     VABCBO        WHENEQ    '6 '
289300090210     C                   MOVEL     'S'           I95FCA
289400090210     C                   MOVEL     'A'           I95TPO
289500090210     C                   OTHER
289600090210     C                   MOVEL     'F'           I95TPO
289700090210     C     VABCAS        IFGT      0
289800090210     C                   MOVEL     'S'           I95FCA
289900090210     C                   ELSE
290000090210     C                   MOVEL     *BLANKS       I95FCA
290100090210     C                   END
290200090210     C                   ENDSL
290300090210     C*
290400090210     C                   CALL      'TISI95R'
290500090210     C                   PARM                    TISI95DS
290600090210     C* Reperisco i dati da CAP
290700090210     C                   MOVEL     O95PRV        VABPRD
290800100706     C                   MOVEL     WCAP          VABCAD
290900100114      ***
291000100706     c                   MOVEL     O95LNA        VABLNA
291100100706     C                   MOVEL     O95ZNC        VABZNC
291200090210     C*
291300090210     C     O95ERR        IFNE      *BLANKS
291400090210     C     O95FIT        ANDEQ     'S'
291500090210     C*  Cap non trovato
291600090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
291700110324     c*******            exsr      Error_DET
291800110324     C*******            goto      end_MEA
291900090210     C                   END
292000090210     C                   END
292100090210      *
292200090210     C                   ELSE
292300090210      *
292400090210     C                   CLEAR                   VABPRD
292500090210     C                   CLEAR                   VABLNA
292600090210     C                   CLEAR                   VABZNC
292700090210     C                   CLEAR                   VABCAD
292800090210     C*  Cap non trovato
292900090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
293000110324     c********           exsr      Error_DET
293100110324     C********           goto      end_MEA
293200090210     C                   END
293300090210      *
293400090210     c     end_MEA       endSR
293500090210      * ?================================================================== */
293600090210     C*?  Dati di dettaglio Codice a barre Segnacolli
293700090210      * ?================================================================== */
293800090210     C     DATI_PCI      BEGSR
293900090210      *
294000090210      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
294100090210      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
294200090210     C     §1BF12        IFEQ      'E'
294300100706     C                   EXSR      SEGNaCollo_EEX
294400090210     C                   ELSE
294500090210      *
294600090210      *  La prima volta controllo congruità numero segnacollo
294700090210     C     §PUBS1        IFNE      *BLANKS
294800090210     C     VABnrs        ANDNE     0
294900090210      *  Se il codice trattamento merce prevede che segnacollino i
295000090210      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
295100090210     C     §1BFG7        IFEQ      'S'
295200090210     C     §1BFG1        OREQ      'S'
295300090210      *  calcolo segnacollo
295400100706     C                   EXSR      Segn_Bartolini
295500090210     C                   END
295600090210     C                   END
295700090210      *
295800090210     C                   END
295900090210      *
296000090210     c     end_PCI       endSR
296100090211     C*----------------------------------------------------------------
296200090211      *? dati finali del dettaglio    UNT
296300090211     C*----------------------------------------------------------------
296400090211     C     DATI_UNT      BEGSR
296500090211      *
296600110324      * Non deve dare messaggio
296700110324     c                   goto      No_UNTctrl
296800110324      *
296900100708     c                   eval      UNT_record = vnREC
297000090213     c                   z-add     vnrec         last_RECord
297100090211      **
297200100319      **   verifica la quadratura di tutti i segmenti ricevuti
297300100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
297400100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
297500100728     C     UNT0074       IFne      Conta_segmenti
297600100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
297700100319     c                             trollare il Messaggio ricevuto'
297800100319      *
297900100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
298000100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
298100100921     C                   EVAL      Messaggio = 'EDI -
298200100921     C                             IFCSUM D96A - msg su UPLOAD -
298300100921     C                             con quadratura UNT errata verificare -
298400100921     C                             x il cliente :'
298500100319     C                   EVAL      Messaggio = %trim(Messaggio) +
298600100319     C                                         %editc(§PTksc:'X')
298700100319     C                   EVAL      Messaggio = %trim(Messaggio) +
298800100319     C                             ' i segmenti contati sono :' +
298900100319     C                             %editc(Conta_segmenti:'Z')
299000100319     c                   if        §PVinv <> 'N'
299100100319     c                   exsr      invio_mail
299200100319     c                   end
299300100319     c                   end
299400110324     c     No_UNTctrl    tag
299500100319      **
299600100319     c     End_UNT       endSR
299700100316     C*----------------------------------------------------------------
299800100316      *? dati finali     UNT
299900100316     C*----------------------------------------------------------------
300000100316     C     DATI_UNZ      BEGSR
300100100316      *
300200100726     c                   move      'S'           Forza_Uscita
300300100316      **
300400100316     c                   endSR
300500090210      *----------------------------------------------------------------
300600090210      *? Input segnacolli su schiera di 20 elementi
300700090210      *----------------------------------------------------------------
300800090210     C     INP_Segnacol  BEGSR
300900090210      *
301000090210     c                   clear                   quanti_PCI        3 0
301100090210     c                   if        §puEL1 = 0
301200090210     c                   z-add     10            quanti_PCI
301300090210     c                   else
301400090210     c                   z-add     §puEL1        quanti_PCI
301500090210     c                   end
301600090210      *
301700090210      *  Pulisce la schiera dei segnacolli da elaborare
301800090210     c                   clear                   x30
301900090210     c                   z-add     0             Nr_x30            3 0
302000090210      *
302100090210      *   riporto tutto sulla schiera generica X30
302200090210      *
302300090210     c                   do        10            limite            3 0
302400090210      * Limita quanti elementi passati in ogni PCI
302500090210      *   devono essere considerati
302600090210     c                   if        limite > quanti_PCI
302700090210     c                   Leave
302800090210     c                   end
302900090210      *
303000100920     c                   IF        tipo_seg = 'GIN'
303100100920      *  Carica la schiera generale di tutti i segnacolli
303200100920     c                   if        GIN_10(limite) <> *blank
303300100920     c                   add       1             Nr_x30
303400100920     c                   eval      X30(Nr_x30) = GIN_10(limite)
303500100920     c                   end
303600100920      *
303700100920     c                   elseIF    tipo_seg = 'PCI'
303800090210      *  Carica la schiera generale di tutti i segnacolli
303900100920     c                   if        PCI_10(limite) <> *blank
304000090210     c                   add       1             Nr_x30
304100100920     c                   eval      X30(Nr_x30) = PCI_10(limite)
304200090210     c                   end
304300100920      *
304400100920     c                   endIF
304500100920      *
304600090210     c                   enddo
304700090210      *
304800090210     C                   ENDSR
304900090210      *----------------------------------------------------------------
305000090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
305100090210      *----------------------------------------------------------------
305200100706     C     SEGNaCollo_EEXBEGSR
305300090210      *
305400090210     c                   exsr      INP_Segnacol
305500090210      *
305600090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
305700090210     C                   DO        10            x                              --- 01 ---
305800090210      *
305900090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
306000090210     C     x30(X)        ANDNE     *LOVAL
306100090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
306200100701     C                   ADD       1             totale_PCI
306300100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
306400090210     C                   END                                                    --- 03 ---
306500090210      *
306600090210     C                   END                                                    --- 02 ---
306700090210      *
306800090210     C                   ENDSR
306900090210     C*----------------------------------------------------------------
307000090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
307100090210     C*----------------------------------------------------------------
307200100706     C     Segn_BartoliniBEGSR
307300090210      *
307400090210     c                   exsr      INP_Segnacol
307500090210     C*
307600090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
307700090210     C*  in base al numero elementi validi carico sgn.
307800090210     C                   DO        10            X                              --- 01 ---
307900090210     C*
308000090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
308100090210     C     x30(X)        ANDNE     *LOVAL
308200090210     C*
308300100630     C                   CLEAR                   DS_SegnBART
308400090210     C*  Controllo se devo ricevere la linea di partenza
308500090210     C                   MOVEL     'N'           WERRO             1
308600090210     C     §PULP1        IFGT      0                                            --- 03 ---
308700100706     C                   EXSR      REPerisce_LNP
308800090210     C                   END                                                    --- 03 ---
308900100706      *
309000090210     C*  Controllo se devo ricevere la linea di arrivo
309100090210     C     §PULA1        IFGT      0                                            --- 03 ---
309200090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
309300100706     C                   EXSR      REPerisce_LNA
309400090210     C                   END                                                    --- 03 ---
309500100706      *
309600090210     C*  Controllo se devo ricevere il numero di serie
309700090210     C     §PUNR1        IFGT      0                                            --- 03 ---
309800090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
309900100706     C                   EXSR      REPerisce_NRS
310000090210     C                   END                                                    --- 03 ---
310100100706      *
310200090210     C*  Controllo se devo ricevere il numero segnacollo
310300090210     C     §PUSG1        IFGT      0                                            --- 03 ---
310400090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
310500100706     C                   EXSR      REPerisce_SGN
310600090210     C                   END                                                    --- 03 ---
310700100706      *
310800090210     C*  Controllo se devo ricevere la zona di consegna
310900090210     C     §PUZN1        IFGT      0                                            --- 03 ---
311000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
311100100706     C                   EXSR      REPerisce_ZNC
311200090210     C                   END                                                    --- 03 ---
311300100706      *
311400090210     C*  Imposto da VAB i dati a zero se mi è stato passato
311500090210     C*  numero segnacollo valido
311600090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
311700100706      *
311800100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
311900100630     C                   Z-ADD     VABLNP        SegnBART_LNP
312000090210     C                   END                                                    --- 04 ---
312100100706      *
312200100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
312300100630     C                   Z-ADD     VABLNA        SegnBART_LNA
312400090210     C                   END                                                    --- 04 ---
312500100706      *
312600100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
312700100630     C                   Z-ADD     VABNRS        SegnBART_NRS
312800090210     C                   END                                                    --- 04 ---
312900100706      *
313000100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
313100100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
313200090210     C                   END                                                    --- 04 ---
313300090210      *
313400090210     C*  mi reimposto LNA e zona da segnacollo
313500100114      ***
313600100114      *** dovendo accorpare le spedizioni da Conferma x CMR
313700100114      ***  non deve impostare l'instradamento.
313800100630     C                   Z-ADD     SegnBART_LNA  VABLNA
313900100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
314000100114      ***
314100090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
314200100701     C                   ADD       1             Conta_Segn
314300100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
314400090210     C                   END                                                    --- 03 ---
314500090210     C*
314600090210     C                   END                                                    --- 02 ---
314700090210     C*
314800090210     C                   END                                                    --- 01 ---
314900090210      *
315000090210     C                   ENDSR
315100090210     C*----------------------------------------------------------------
315200090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
315300090210     C*----------------------------------------------------------------
315400100706     C     REPerisce_LNP BEGSR
315500090210     C*
315600090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
315700090210     C*  (a partire dalla posizione iniziale indicata in tabella)
315800090210     C*  dal numero segnacollo passatomi dal partner
315900090210     C                   Z-ADD     §PULP1        WP                2 0
316000090210     C     3             SUBST     x30(X):WP     WLNP              3
316100090210     C*
316200090210     C*  controllo che la lnp calcolata sia numerica
316300090210     C                   TESTN                   WLNP                 98
316400090210     C*
316500090210     C*  se è numerica la carico
316600090210     C     *IN98         IFEQ      '1'
316700100630     C                   MOVE      WLNP          SegnBART_LNP
316800090210     C                   ELSE
316900090210     C*  se non è numerica stampo errore
317000090210     C                   MOVEL     'S'           WERRO             1
317100090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
317200090213     c                   exsr      Error_DET
317300090210     C                   goto      end_LNP
317400090210     C                   END
317500090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
317600090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
317700100630     C     SegnBART_LNP  IFNE      VABLNP
317800090210     C                   MOVEL     'S'           WERRO             1
317900090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
318000090210     c                             a della Bolla'
318100090213     c                   exsr      Error_DET
318200090210     C                   goto      end_LNP
318300090210     C                   END
318400090210     C*
318500090210     C     end_LNP       ENDSR
318600090210     C*----------------------------------------------------------------
318700090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
318800090210     C*----------------------------------------------------------------
318900100706     C     REPerisce_LNA BEGSR
319000090210     C*
319100090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
319200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
319300090210     C*  dal numero segnacollo passatomi dal partner
319400090210     C                   Z-ADD     §PULA1        WP                2 0
319500090210     C     3             SUBST     x30(X):WP     WLNA              3
319600090210     C*  controllo che la lna calcolata sia numerica
319700090210     C                   TESTN                   WLNA                 98
319800090210     C*  se è numerica la carico
319900090210     C     *IN98         IFEQ      '1'
320000100630     C                   MOVE      WLNA          SegnBART_LNA
320100090210     C                   ELSE
320200090210     C*  se non è numerica stampo errore
320300090210     C                   MOVEL     'S'           WERRO             1
320400090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
320500090213     c                   exsr      Error_DET
320600090210     C                   goto      end_LNA
320700090210     C                   END
320800090210     C*
320900090210     C     end_LNA       ENDSR
321000090210     C*----------------------------------------------------------------
321100090210     C*? REPNRS - Reperisce numero serie
321200090210     C*----------------------------------------------------------------
321300100706     C     REPerisce_NRS BEGSR
321400090210     C*
321500090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
321600090210     C*  (a partire dalla posizione iniziale indicata in tabella)
321700090210     C*  dal numero segnacollo passatomi dal partner
321800090210     C                   Z-ADD     §PUNR1        WP                2 0
321900090210     C     2             SUBST     x30(X):WP     WNRS              2
322000090210     C*  controllo che il nrs calcolata sia numerico
322100090210     C                   TESTN                   WNRS                 98
322200090210     C*  se è numerica la carico
322300090210     C     *IN98         IFEQ      '1'
322400100630     C                   MOVE      WNRS          SegnBART_NRS
322500090210     C                   ELSE
322600090210     C*  se non è numerico stampo errore
322700090210     C                   MOVEL     'S'           WERRO             1
322800090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
322900090213     c                   exsr      Error_DET
323000090210     C                   goto      end_NRS
323100090210     C                   END
323200090210      *
323300090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
323400090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
323500100630     C     SegnBART_NRS  IFNE      VABNRS
323600090210     C                   MOVEL     'S'           WERRO             1
323700090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
323800090210     c                             o della Bolla'
323900090213     c                   exsr      Error_DET
324000090210     C                   goto      end_NRS
324100090210     C                   END
324200090210     C*
324300090210     C     end_NRS       ENDSR
324400090210     C*----------------------------------------------------------------
324500090210     C*? REPZNC - Reperisce zona consegna
324600090210     C*----------------------------------------------------------------
324700100706     C     REPerisce_ZNC BEGSR
324800090210     C*
324900090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
325000090210     C*  (a partire dalla posizione iniziale indicata in tabella)
325100090210     C*  dal numero segnacollo passatomi dal partner
325200090210     C                   Z-ADD     §PUZN1        WP                2 0
325300090210     C     2             SUBST     x30(X):WP     WZNC              2
325400090210     C*  controllo che la zona calcolata sia numerica
325500090210     C                   TESTN                   WZNC                 98
325600090210     C*  se è numerica la carico
325700090210     C     *IN98         IFEQ      '1'
325800100630     C                   MOVE      WZNC          SegnBART_ZNC
325900090210     C                   ELSE
326000090210     C*  se non è numerica stampo errore
326100090210     C                   MOVEL     'S'           WERRO             1
326200090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
326300090213     c                   exsr      Error_DET
326400090210     C                   goto      end_ZNC
326500090210     C                   END
326600090210     C*
326700090210     C     end_ZNC       ENDSR
326800090210     C*----------------------------------------------------------------
326900090210     C*? REPSGN - Reperisce numero segnacollo
327000090210     C*----------------------------------------------------------------
327100100706     C     REPerisce_SGN BEGSR
327200090210     C*
327300090210     C*  se viene passata nr.segnacollo estraggo sottostringa
327400090210     C*  lunga 7 chr (a partire dalla posizione iniziale
327500090210     C*  indicata in tabella)
327600090210     C*  dal numero segnacollo passatomi dal partner
327700090210     C                   Z-ADD     §PUSG1        WP                2 0
327800090210     C     7             SUBST     x30(X):WP     WSGN              7
327900090210     C*  controllo che il nr.segnacollo calcolato sia numerico
328000090210     C                   TESTN                   WSGN                 98
328100090210     C*  se è numerico lo carico
328200090210     C     *IN98         IFEQ      '1'
328300100630     C                   MOVE      WSGN          SegnBART_NSC
328400090210     C                   ELSE
328500090210     C*  se non è numerica stampo errore
328600090210     C                   MOVEL     'S'           WERRO             1
328700100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
328800090213     c                   exsr      Error_DET
328900090210     C                   goto      end_SGN
329000090210     C                   END
329100090210     C*
329200090210     C     end_SGN       ENDSR
329300051205      *----------------------------------------------------*
329400051205      *   DEFINIZIONE CHIAVI                               *
329500051205      *----------------------------------------------------*
329600051205     C     *INZSR        BEGSR
329700051205      *
329800991129     c     *ENTRY        PLIST
329900060613     C                   parm                    esito             1
330000971215      *
330100050112     C     KTABel        KLIST
330200050112     C                   KFLD                    TBLKUT
330300050112     C                   KFLD                    TBLCOD
330400050112     C                   KFLD                    TBLKEY
330500090210      *
330600090210     C     K_TAB1L       KLIST
330700090213     C                   KFLD                    etbUNB
330800090213     C                   KFLD                    etbSGM
330900090213     C                   KFLD                    etbVALSGM
331000090209     C*
331100090209     C* Definisco chiavi di accesso
331200090209     C     KTAB1         KLIST
331300090209     C                   KFLD                    KKUT
331400090209     C                   KFLD                    KCOD
331500090209     C*
331600090209     C     KNUF          KLIST
331700090209     C                   KFLD                    KAAA
331800090209     C                   KFLD                    KCNU
331900090209     C                   KFLD                    KFIL
332000090209      *
332100090209     C     KVAB1         KLIST
332200090209     C                   KFLD                    KCCM
332300090209     C                   KFLD                    KCMR
332400090209     C                   KFLD                    KCNT
332500090209     C                   KFLD                    KAAS
332600090209     C                   KFLD                    KLNP
332700090209     C                   KFLD                    KNRS
332800090209     C                   KFLD                    KNSP
332900090209      *
333000090209     C     KVAB2         KLIST
333100090209     C                   KFLD                    KCCM
333200090209     C                   KFLD                    KCMR
333300090209      *
333400090209     C     KRDE          KLIST
333500090209     C                   KFLD                    KAAS
333600090209     C                   KFLD                    KLNP1
333700090209     C                   KFLD                    KNRS
333800090209     C                   KFLD                    KNSP
333900090209     C                   KFLD                    KNMC
334000090209     C                   KFLD                    KNRP
334100090209      *
334200090209     C     KACO          KLIST
334300090209     C                   KFLD                    KKUT
334400090209     C                   KFLD                    KKCC
334500090209     C                   KFLD                    KKSC
334600090209      *
334700090209     C     KIND          KLIST
334800090209     C                   KFLD                    KKUT
334900090209     C                   KFLD                    KKCC
335000090209     C                   KFLD                    KKSC
335100090209      *
335200090209      * DETERMINO SE SONO BARTOLINI O SDI
335300090209     C                   CLEAR                   TIBS55DS
335400090209     C                   MOVEL     'L'           I50TLA
335500090209     C                   CALL      'TIBS55R'
335600090209     C                   PARM                    TIBS55DS
335700090209      *
335800090209     C* RECUPERO DATI DELL'UTENTE
335900090209     C                   Z-ADD     1             CODUT             1 0
336000090209     C                   CALL      'XPARUT'
336100090209     C                   PARM                    UTEDSE0F
336200090209     C                   MOVEL     RAGUT         RSUT             20
336300090209     C*
336400090209     C* RICERCA CAPOCONTI
336500090209     C                   DO        50            X
336600090209     C                   MOVE      TCU(X)        TCUDS
336700090209     C     F56           IFEQ      'CG'
336800090209     C     F34           ANDEQ     '01'
336900090209     C                   Z-ADD     KCU(X)        KCI               4 0
337000090209     C                   END
337100090209     C                   END
337200090209     C*
337300090211     c                   movel(p)  'EDPAB'       User_Msg         10
337400090211     C*
337500090209     C* IMPOSTO A ZERO VARIABILI DI PGM
337600090209     c                   move      *blank        Snd_Msg_alert     1
337700090209     C                   MOVEL     CA(1)         WCA              78
337800090209     C                   MOVEL     CN(1)         WCN              78
337900090209     C*
338000090209     C* RECUPERO DATA E ORA
338100100630     C                   TIME                    Ora_Data         14 0
338200100630     C                   MOVE      Ora_Data      G02DAT
338300100630     C                   MOVE      Ora_Data      WDATA8            8 0
338400100630     C                   MOVE      WDATA8        Anno2             2 0
338500090209     C                   MOVEL     WDATA8        DATA              6 0
338600100630     C                   MOVE      Anno2         DATA
338700090209     C                   MOVEL     *BLANK        G02ERR
338800090209     C                   CALL      'XSRDA8'
338900090209     C                   PARM                    WLBDA8
339000090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
339100100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
339200090209     C                   TIME                    WORA              6 0
339300090209      * Recupero data e ora
339400090209     C                   TIME                    W0140
339500090209      * UDATE IN GGMMAAAA
339600100630     C                   MOVE      W0140         DataGGMMAAA
339700090209      * UDATE IN AAAAMMGG
339800100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
339900090209     C     *iso          MOVEL     DATA_eur      dateu
340000090209      *
340100090209      * ?              /*--------------------------- */
340200090209     c                   exsr      CARICA_TABELLE
340300090209      * ?              /*--------------------------- */
340400090209      *
340500090209     C                   MOVEL     *ALL'-'       CMP132          132
340600991124      *
340700050414      *------------------
340800991124     C                   ENDSR
340900060621      * ?------------------------------------------------------------------ */
341000090209      *?    CARICA TABELLE SU SCHIERE
341100060621      * ?------------------------------------------------------------------ */
341200090209     C     CARICA_TABELLEBEGSR
341300090205      *
341400090209     C* Caricamento Tabella 1B
341500090209     C                   Z-ADD     0             X                 4 0
341600090209     C                   Z-ADD     CODUT         KKUT
341700090209     C                   MOVEL     '1B'          KCOD
341800090209     C     KTAB1         CHAIN     TABEL00F                           30
341900090209     C     *IN30         DOWEQ     '0'
342000090209     C     TBLFLG        IFEQ      *BLANKS
342100090209     C                   ADD       1             X
342200100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
342300100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
342400090209     C                   END
342500090209     C     KTAB1         READE     TABEL00F                               30
342600090209     C                   END
342700090209      *
342800090209     C* Caricamento Tabella 15
342900090209     C                   Z-ADD     0             X                 4 0
343000090209     C                   Z-ADD     CODUT         KKUT
343100090209     C                   MOVEL     '15'          KCOD
343200090209     C     KTAB1         CHAIN     TABEL00F                           30
343300090209     C     *IN30         DOWEQ     '0'
343400090209     C     TBLFLG        IFEQ      *BLANKS
343500090209     C                   ADD       1             X
343600090209     C                   MOVEL     TBLKEY        C15(X)
343700090209     C                   MOVEL     TBLUNI        DS15
343800090209     C                   MOVEL     §15COD        ISO(X)
343900090209     C                   MOVEL     §15ECF        CPF(X)
344000090209     C                   MOVE      §15PCF        CPF(X)
344100090209     C                   END
344200090209     C     KTAB1         READE     TABEL00F                               30
344300090209     C                   END
344400090209      *
344500090209     C* Caricamento Tabella CV
344600090209     C                   Z-ADD     0             X
344700090209     C                   Z-ADD     CODUT         KKUT
344800090209     C                   MOVEL     'CV'          KCOD
344900090209     C     KTAB1         CHAIN     TABEL00F                           30
345000090209     C     *IN30         DOWEQ     '0'
345100090209     C     TBLFLG        IFEQ      *BLANKS
345200090209     C                   ADD       1             X
345300090209     C                   MOVEL     TBLKEY        CCV(X)
345400090209     C                   MOVEL     TBLUNI        DCV(X)
345500090209     C                   END
345600090209     C     KTAB1         READE     TABEL00F                               30
345700090209     C                   END
345800090216      *
345900100702     C* Caricamento Tabella Priorità trasporto
346000100702     C                   Z-ADD     0             X
346100100702     C                   MOVEL     'TS'          TABCOD
346200100702     C     TABCOD        CHAIN     EDTAB01L                           30
346300100702     C     *IN30         DOWEQ     '0'
346400100702     C     TABFLG        IFEQ      *BLANKS
346500100702     C                   ADD       1             X
346600100702     C                   eval      TipoServizio(X) = TABUNI
346700100705     C                   eval      Key_TipServ(X)  = TABKEY
346800100702     C                   END
346900100702     C     TABCOD        READE     EDTAB01L                               30
347000100702     C                   END
347100100702      *
347200110328      * Caricamento Tabella termini consegna
347300110328     C                   Z-ADD     0             X
347400110328     C                   MOVEL     'TB'          TABCOD
347500110328     C     TABCOD        CHAIN     EDTAB01L                           30
347600110328     C     *IN30         DOWEQ     '0'
347700110328     C     TABFLG        IFEQ      *BLANKS
347800110328     C                   ADD       1             X
347900110328     C                   eval      K35_TpBolla(X) = TABKEY
348000110328     C                   eval      Cod_TpBolla(X) = TABKEY
348100110328     C                   eval      Decod_Porto(X) = TABUNI
348200110328     C                   END
348300110328     C     TABCOD        READE     EDTAB01L                               30
348400110328     C                   END
348500110328      *
348600090216     C* Caricamento Tabella Tipo Incasso C/Assegno
348700090216     C                   Z-ADD     0             X
348800090216     C                   MOVEL     'TC'          TABCOD
348900090216     C     TABCOD        CHAIN     EDTAB01L                           30
349000090216     C     *IN30         DOWEQ     '0'
349100090216     C     TABFLG        IFEQ      *BLANKS
349200090216     C                   ADD       1             X
349300100705     C                   eval      K35_TpIncas(X) = TABKEY
349400100705     C                   eval      TipoIncasso(X) = TABUNI
349500090216     C                   END
349600090216     C     TABCOD        READE     EDTAB01L                               30
349700090216     C                   END
349800090209      *
349900090209      * Caricamento Tabella Partner esteri
350000090209     C                   Z-ADD     0             X
350100090209     C                   MOVEL     'PT'          TABCOD
350200090209     C     TABCOD        CHAIN     EDTAB01L                           30
350300090209     C     *IN30         DOWEQ     '0'
350400090209      *
350500090209     C                   SELECT
350600090209     C     TABFLG        WHENNE    *BLANKS
350700090209      *
350800090209     C                   OTHER
350900090209     C                   ADD       1             X
351000100630     C                   MOVEL     TABKEY        PT_key(X)
351100100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
351200100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
351300100630     C                   MOVEL     TABUNI        PT_uni(X)
351400090209     C                   ENDSL
351500090209      *
351600090209     C     TABCOD        READE     EDTAB01L                               30
351700090209     C                   END
351800121105      *
351900121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
352000121105     c                   exsr      ChecK_PT
352100121105      *
352200090209      * salva quanti Codici UNB ha caricato
352300100630     c                   z-add     x             PT_KeyXsav
352400090209      *
352500090209     C                   MOVEL     'PU'          TABCOD
352600090209     C     TABCOD        CHAIN     EDTAB01L                           30
352700090209     C     *IN30         DOWEQ     '0'
352800090209      *
352900090209     C                   SELECT
353000090209     C     TABFLG        WHENNE    *BLANKS
353100090209      *
353200090209     C                   OTHER
353300100630     C                   MOVEL     TABKEY        PU_key
353400090209     C                   Z-ADD     1             X
353500100630     C     PU_key        LOOKUP    PT_key(X)                              32
353600100630     C   32              MOVEL     TABUNI        PU_uni(X)
353700090209     C                   ENDSL
353800090209      *
353900090209     C     TABCOD        READE     EDTAB01L                               30
354000090209     C                   END
354100090209      *
354200090209      * Prosegue tab.PT e PU
354300090209     C                   MOVEL     'PV'          TABCOD
354400090209     C     TABCOD        CHAIN     EDTAB01L                           30
354500090209     C     *IN30         DOWEQ     '0'
354600090209      *
354700090209     C                   SELECT
354800090209     C     TABFLG        WHENNE    *BLANKS
354900090209      *
355000090209     C                   OTHER
355100100630     C                   MOVEL     TABKEY        PV_key
355200090209     C                   Z-ADD     1             X
355300100630     C     PV_key        LOOKUP    PT_key(X)                              32
355400100630     C   32              MOVEL     TABUNI        PV_uni(X)
355500090209     C                   ENDSL
355600090209      *
355700090209     C     TABCOD        READE     EDTAB01L                               30
355800090209     C                   END
355900090209      *
356000090209     C* Caricamento Tabella codici clienti - tariffe
356100090209     C                   Z-ADD     0             X
356200090209     C                   MOVEL     'CL'          TABCOD
356300090209     C     TABCOD        CHAIN     EDTAB01L                           30
356400090209     C     *IN30         DOWEQ     '0'
356500090209     C     TABFLG        IFEQ      *BLANKS
356600090209     C                   ADD       1             X
356700100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
356800100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
356900090209     C                   END
357000090209     C     TABCOD        READE     EDTAB01L                               30
357100090209     C                   END
357200090209      *
357300090209     C* Caricamento Serivizi speciali
357400090209     C                   Z-ADD     0             X
357500090209     C                   MOVEL     'SS'          TABCOD
357600090209     C     TABCOD        CHAIN     EDTAB01L                           30
357700090209     C     *IN30         DOWEQ     '0'
357800090209     C     TABFLG        IFEQ      *BLANKS
357900090209     C                   ADD       1             X
358000100702     C                   eval       Key_ServSpec(X) = TABKEY
358100100702     C                   eval       SpecialServ(X)  = TABKEY
358200100702     C                   eval       UNI_ServSpec(X) = TABUNI
358300090209     C                   END
358400090209     C     TABCOD        READE     EDTAB01L                               30
358500090209     C                   END
358600090209      *
358700090209     C                   ENDSR
358800090205      * ?------------------------------------------------------------------ */
358900090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
359000090205      * ?------------------------------------------------------------------ */
359100090205     C     *pssr         BEGSR
359200090205     C
359300060710     C                   eval      esito = '2'
359400100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
359500100921     C                   EVAL      Messaggio = 'EDI -
359600100921     C                             IFCSUM D96A - messaggio -
359700090603     C                             EDI ricevuto errato su UPLOAD.'
359800100319     c                   if        §PVinv <> 'N'
359900100319     c                   exsr      invio_mail
360000100319     c                   end
360100060621     C
360200090213     C                   ENDSR
360300090213      * ?------------------------------------------------------------------ */
360400090213      *?      Segnala Errore su TIVIN
360500090213      * ?------------------------------------------------------------------ */
360600090213     C     Error_DET     BEGSR
360700090213     C
360800100728     c                   eval      Segmento_in_errore = Conta_segmenti
360900090213     C                   eval      vinFLG = '2'
361000090213     c                   eval      Almeno_Un_Due ='S'
361100090213     c                   eval      esito  = '1'
361200090213     c                   eval      se_errore ='S'
361300090213     C                   eval      Err_in_detta = 'S'
361400090213     C
361500090213     C                   ENDSR
361600100705      * ?------------------------------------------------------------------ */
361700100705      *?   Controlla e decodifica Valori VALUE dei segmenti
361800100705      * ?------------------------------------------------------------------ */
361900100705     C     In_TAB_EDSTBL BEGSR
362000100705      *
362100100705      *   Cerca la decodifica del dato in TABELLA
362200100705     c                   clear                   trovata_EDSTBL    1
362300100705     c                   clear                   quale_campo       6
362400100705     c                   clear                   campo_dati_70A   70
362500100705     C                   movel     UNB_Mittente  etbUNB
362600100705     c     K_TAB1L       chain     edstbl01l
362700100705      *
362800100705      * prova quindi senza l'UNB specifico del cliente
362900100705     c                   if        not %Found(edstbl01l)
363000100705     C                   clear                   etbUNB
363100100705     c     K_TAB1L       chain     edstbl01l
363200100705     c                   end
363300100705      *
363400100705      *  Traduce i testi descrittivi se PRESENTE in tabella
363500100705      *   il campo di destinazione è definito sulla destra della descrizione
363600100705      *     ed è lungo 6 come i nomi definiti nei VAB.
363700100705     c                   if        %Found(edstbl01l)
363800100705     c                   move(p)   etbDESCRI     quale_campo
363900100705     c                   movel(p)  etbDATI       campo_dati_70A
364000100705     c                   move      'S'           trovata_EDSTBL
364100100705     c                   end
364200100705      *
364300100705      *
364400100705     C                   ENDSR
364500100705     c*==================================================================*
364600090209     C*? CNVDAT - DECODIFICA LA DATA
364700090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
364800090209     C*         - WDATA  (DATA ALFANUMERICA)
364900090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
365000090209     C*         - WHMS   (ORA NUMERICA)
365100090209     C*----------------------------------------------------------------
365200100702     C     Converte_Data BEGSR
365300090209     C*
365400090209     C                   MOVEL     ' '           WERRO             1
365500090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
365600090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
365700100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
365800090209     C                   Z-ADD     *ZEROS        WHMS              6 0
365900090209     C*
366000100705     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
366100100702     C                   MOVEL     Work_Data__35 WCOMO8            8
366200090209     C                   TESTN                   WCOMO8               99
366300090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
366400090209     C  N99              MOVEL     'S'           WERRO             1
366500090209     C                   END
366600090209     C*
366700100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
366800100702     C                   MOVEL     Work_Data__35 WCOMO12          12
366900100120     C                   TESTN                   WCOMO12              99
367000100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
367100100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
367200100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
367300090209     C  N99              MOVEL     'S'           WERRO             1
367400090209     C                   END
367500100120     C*
367600100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
367700100702     C                   MOVEL     Work_Data__35 WCOMO14          14
367800100120     C                   TESTN                   WCOMO14              99
367900100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
368000100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
368100100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
368200100120     C  N99              MOVEL     'S'           WERRO             1
368300100120     C                   END
368400090209     C*
368500090209     C                   ENDSR
368600090210     C*----------------------------------------------------------------
368700100701     C*? Se_Numerico       - CONTROLLO NUMERICO
368800100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
368900100701     C*         - lunghezza_num   (Lunghezza campo output)
369000100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
369100090210     C*----------------------------------------------------------------
369200100701     C     Se_Numerico   BEGSR
369300090210     C*
369400090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
369500100701     C                   MOVEL     'N'           Campo_Numerico    1
369600100701     C                   MOVEL     *BLANKS       Numero_OK        15
369700090210     C                   Z-ADD     15            IN                3 0
369800100701     C                   Z-ADD     lunghezza_num IX                3 0
369900090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
370000100701     C                   MOVEA     test_num      NDA
370100090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
370200090210     C                   MOVEA     *ALL'0'       NDN
370300100630      *
370400090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
370500100701     C                   Z-ADD     lunghezza_num I                 3 0
370600090210     C     I             DOWGT     0                                            --- 1 -->
370700090210     C* ESTRAE IL CARATTERE DA ESAMINARE
370800100701     C     1             SUBST     test_num:I    WTEM01            1
370900090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
371000090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
371100090210     C     WTEM01        SCAN      WCA                                    99
371200090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
371300090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
371400090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
371500090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
371600090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
371700090210     C* DA DESTRA VERSO SINISTRA
371800090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
371900090210     C                   MOVEL     WTEMP1        NDN(IN)
372000090210     C                   SUB       1             IN
372100090210     C                   END                                                    <-- 3 ---
372200090210     C                   END                                                    <-- 2 ---
372300090210     C                   SUB       1             I
372400090210     C                   END                                                    <-- 1 ---
372500090210     C*
372600090210     C                   MOVEA     NDN           WNDN             15
372700090210     C     WNDN          IFNE      *ZEROS
372800100701     C                   MOVEA     NDN           Numero_OK        15
372900100701     C                   MOVEL     'S'           Campo_Numerico    1
373000090210     C                   END
373100090210     C*
373200090210     C                   ENDSR
373300090210     C*----------------------------------------------------------------
373400090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
373500090210     C*----------------------------------------------------------------
373600100705     C     Converte_CAP  BEGSR
373700090210     C*
373800090210     C                   MOVEL     *BLANKS       WCAP              9
373900090210     C     nad3251       IFNE      '0'
374000090210     C     '  '          SCAN      nad3251       LENGHT            2 0
374100090210     C                   SUB       1             LENGHT
374200090210     C     LENGHT        IFLE      5
374300090210     C     LENGHT        ANDGT     0
374400090210     C                   Z-ADD     LENGHT        T                 2 0
374500090210     C                   Z-ADD     5             S                 2 0
374600090210     C                   MOVEA     nad3251       CAP9
374700090210     C                   MOVEL     *ZEROS        CAP5
374800090210     C                   DO        LENGHT
374900090210     C                   MOVE      CAP9(T)       CAP5(S)
375000090210     C                   SUB       1             S
375100090210     C                   SUB       1             T
375200090210     C                   END
375300090210     C                   MOVEA     CAP5          WCAP5             5
375400090210     C     WCAP5         IFEQ      '0'
375500090210     C                   MOVEL     *BLANKS       WCAP
375600090210     C                   ELSE
375700090210     C                   MOVEA     CAP5          WCAP
375800090210     C                   END
375900090210     C*
376000090210     C                   ELSE
376100090210     C                   MOVEL     nad3251       WCAP
376200090210     C                   END
376300090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
376400090210     C     WCAP          IFNE      *BLANKS
376500090210     C     WFLCPF        ANDNE     0
376600090210     C                   MOVEL     WFLCPF        WELE              1 0
376700090210     C                   MOVE      WFLCPF        WPOS              1 0
376800090210     C                   MOVEL     *BLANK        WCPF
376900090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
377000090210     C                   ELSE
377100090210     C                   MOVEL     *BLANKS       WCPF
377200090210     C                   END
377300090210     C                   END
377400090210     C*
377500090210     C                   ENDSR
377600090211      * ?================================================================== */
377700090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
377800090211      * ?================================================================== */
377900090211     C     Scrive_VAB    BEGSR
378000090211      *
378100090211     c                   clear                   err_bloccante     1
378200090211     c                   move      'S'           Almeno_Uno
378300090211      *
378400090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
378500090211     c                   exsr      UPDVAB
378600090211      *
378700090211     C* >>>>>>>>>>>>
378800090211     C*  Richiamo pgm per scrittura immediata VAB
378900090211     C     §PUWRK        IFEQ      ' '
379000090211      *
379100090211     C*  Richiamo pgm per esecuzione stampa
379200110110     C**** §PTCMR        IFEQ      'S'
379300110110     c******             MOVEL     BGM_nrCMR     d86CMR
379400110110     c******             if            RFF_NrCMR <> *Blank
379500110110     C******             MOVEL     RFF_NrCMR     d86CMR
379600110110     c******             end
379700110110     C******             ELSE
379800110110     C******             MOVEL     BGM_NrFviaggiod86CMR
379900110110     c******             if          RFF_NrFviaggio <> *Blank
380000110110     C******             MOVEL     RFF_NrFviaggiod86CMR
380100110110     c******             end
380200110110     C******             END
380300110110      *
380400110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
380500110110     c                   MOVEL     vabCMR        d86CMR
380600090211      *
380700090213     C                   MOVEL     ' '           d86RIE
380800090213     C                   MOVEL     §PUDTA        d86DTA
380900090213     C                   Z-ADD     1             d86CNT
381000090213     C                   Z-ADD     §PTKSC        d86CCM
381100090213     C                   MOVEL     *BLANKS       d86STA
381200090213     C                   MOVEL     'E'           d86MOD
381300090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
381400090211      * dove poi imposta la DS dei parametri passati
381500090213     C                   MOVEL     'LR'          d86CHI
381600090213     C                   MOVEL     'N'           d86CTL
381700090211      *
381800090211     c                   move      kpjbu         SvKpjbu
381900090211     c                   clear                   kpjbu
382000090216     C                   movel     *on           Commit_on         1
382100090213     C                   MOVEL     TRTC86ds      KPJBU
382200090213      * ?- Chiamata la TRTC86R2 -*
382300090213     C                   CALL      'TRTC86R2'
382400090211     C                   PARM                    KPJBA
382500090216     C                   PARM                    Commit_on
382600090211     c                   move      Svkpjbu       Kpjbu
382700090211      *
382800090211     C*  Controllo pgm per scrittura immediata VAB
382900100630     C     sk            IFNE      0
383000100630     C                   DO        sk            sx
383100100630     C                   Z-ADD     skCLienti(sx) d86CCM
383200090211      *
383300090211     c                   move      kpjbu         SvKpjbu
383400090211     c                   clear                   kpjbu
383500090216     C                   movel     *on           Commit_on         1
383600090213     C                   MOVEL     TRTC86ds      KPJBU
383700090213      * ?- Chiamata la TRTC86R2 -*
383800090213     C                   CALL      'TRTC86R2'
383900090211     C                   PARM                    KPJBA
384000090216     C                   PARM                    Commit_on
384100090211     c                   move      Svkpjbu       Kpjbu
384200090211     C                   END
384300090211     C                   END
384400090211      *
384500090211     C                   END
384600090211      * >>>>>>>>>>>>
384700090211     C                   if        se_errore  = 'S'
384800090211     c                   move      'S'           err_bloccante
384900090211     c                   goto      Error_VAB
385000090211     c                   end
385100090211     C*
385200090211     C     Error_VAB     Endsr
385300090211     C*----------------------------------------------------------------
385400090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
385500090211     C*----------------------------------------------------------------
385600090211     C     UPDVAB        BEGSR
385700090211      *
385800090211     C*  Se non è stato impostato il codice bolla lo imposto io
385900090211     C     VABCBO        IFEQ      *BLANKS
386000090211     C     VABCAS        IFGT      0
386100090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
386200090211     C                   ELSE
386300090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
386400090211     C                   END
386500090211     C                   END
386600100701      * Imposta le NOte
386700100701     C     VABNOT        IFEQ      *BLANKS
386800100701     C                   MOVEL     Note_1        VABNOT
386900100701     C                   MOVEL     Note_2        VABNT2
387000100701     C                   ELSE
387100100701     C                   MOVEL     Note_1        VABNT2
387200100701     C                   END
387300090211      *
387400090211     C*  Attribuisco anno spedizione
387500100630     C     DTM_DtParten  IFGT      0                                            anno sped.
387600100630     C                   MOVEL     DTM_DtParten  VABAAS
387700090211     C                   ELSE
387800100630     C     DTM_DtFViag   IFGT      0
387900100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
388000090211     C                   END
388100090211     C                   END
388200090211      *
388300090211      *  Controllo dettaglio segnacolli
388400090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
388500100630     C                   EXSR      CTRsegn_EEX
388600090211     C                   ELSE
388700090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
388800090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
388900100630     C                   EXSR      CTRsegn_BAR
389000090211     C                   END
389100090211     C                   END
389200090211      *
389300090212      *  Imposto linea partenza e serie
389400100701     C                   eval      VABLNP = PT_vabLNP
389500100701     C                   eval      VABNRS = PT_vabNRS
389600090211      *
389700090211      *  Controllo se devo variare il codice trattamento merce
389800100701     C                   eval      VABCTM = PT_vabCTM
389900090211      *
390000090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
390100090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
390200090211     c                   clear                   mantiene_nsp      1            *blk/'S'
390300100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
390400090211      *
390500090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
390600090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
390700090211      *
390800090211      *  Imposto codice cliente
390900090211     c                   Select
391000090211      *
391100100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
391200100319     C                   When        Det_RFF_FF_ksc  <> *zeros
391300100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
391400100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
391500090211      * forza LNP/CTM/NRS
391600100319     c                   if        Det_RFF_FF_lnp <> 0
391700100319     C                   eval      VABLNP = Det_RFF_FF_lnp
391800090211     c                   end
391900100319     c                   if        Det_RFF_FF_nrs <> 0
392000100319     C                   eval      VABNRS = Det_RFF_FF_nrs
392100090211     c                   end
392200100319     c                   if        Det_RFF_FF_ctm <> *blank
392300100319     C                   eval      VABCTM = Det_RFF_FF_ctm
392400090211     c                   end
392500090211      *
392600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
392700100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
392800100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
392900090211     c                   end
393000090211      *
393100100319      *  Specificato codice da qualificatore RFF+FF dalla testata
393200100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
393300100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
393400100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
393500090211      * forza LNP/CTM/NRS
393600100319     c                   if        Tes_RFF_FF_LNP  <> 0
393700100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
393800090211     c                   end
393900100319     c                   if        Tes_RFF_FF_NRS  <> 0
394000100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
394100090211     c                   end
394200100319     c                   if        Tes_RFF_FF_CTM  <> *blank
394300100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
394400090211     c                   end
394500090211      *
394600090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
394700100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
394800100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
394900090211     c                   end
395000090211      *
395100090211      *  Imposto dati da tabella - PT-
395200090211     C                   Other
395300090211      *
395400090211     C                   MOVEL     §PTKSC        VABCCM
395500090211     C                   MOVEL     §PTCTR        VABCTR
395600090211      *
395700090211     C                   Endsl
395800090211      *
395900090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
396000090211      *    o dalla relativa CL
396100090211     C                   MOVEL     VABlnp        VABfgs
396200090211      *
396300090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
396400090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
396500090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
396600090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
396700090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
396800090211      *
396900090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
397000090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
397100090211     c                   else
397200090211      *
397300090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
397400090211     C*  lo attribuisco io
397500090211      **              **------------------**
397600090211     c                   exsr      repnum
397700090211      **              **------------------**
397800100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
397900090211      **              **------------------**
398000090211     c                   end
398100090211      *
398200090211     C*  Altrimenti attribuisco data del giorno
398300090211     C     VABMGS        IFEQ      0
398400090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
398500090211     C                   MOVE      WOGGI         VABMGS                         = oggi
398600090211     C                   END
398700090211     C*  Eseguo posizionamento su EDiVAB
398800090211     C                   Z-ADD     VABAAS        KAAS
398900090211     C                   Z-ADD     VABlnp        KLNP
399000090211     C                   Z-ADD     VABNRS        KNRS
399100090211     C                   Z-ADD     VABNSP        KNSP
399200090211     C                   MOVEL     SAVBOL        SALVA
399300090211     C     KVAB1         CHAIN     EDiVAB1L                           30
399400090211     C                   MOVEL     SALVA         SAVBOL
399500090211     C*  Imposto dati CMR
399600090211     C                   Z-ADD     1             VABCNT
399700090211     **
399800090211      *** Attenzione se si deve trattare il VAX con il CMR
399900090211      *** il campo VABCNT deve essere sempre impostato a (1)
400000090211     C     §puWRK        ifEQ      'S'
400100090211     C     §puNSP        andEQ     'S'
400200090211     C                   Z-ADD     1             VABCNT
400300090211     c                   end
400400090211     **
400500100630     C                   Z-ADD     Data_prepara  VABDTS
400600090212     C                   movel     '20'          VABDTS
400700100113     ** Forza la Data di Oggi
400800100113     C                   z-add     WOggi         VABDTS
400900100113     **
401000100630     C     Ora__prepara  mult      100           VABHMS
401100090211     C     §PTCMR        IFEQ      'S'
401200100630     C                   Z-ADD     DTM_DtCMR     VABDCM
401300100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
401400100701     c                   if          RFF_NrCMR <> *Blank
401500100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
401600100701     c                   end
401700090211     C                   ELSE
401800100630     C                   Z-ADD     DTM_DtFViag   VABDCM
401900100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
402000100701     c                   if          RFF_NrFviaggio <> *Blank
402100100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
402200100701     c                   end
402300090211     C                   END
402400100120      *
402500100813     c                   if          VABCMR = *Blank
402600100813     C                   MOVEL(p)  BGM_Id_TestataVABCMR
402700100813     c                   else
402800100813      *
402900100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
403000100120      * accorpare la bolle ad un unico destinatario da più parti
403100100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
403200100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
403300100708     C                   if         NAD_SF_x_CMR <> *blank
403400100708     c                   eval      VABCMR = NAD_SF_x_CMR
403500100708     c                   else
403600100813     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z')    +
403700100813     c                                      '_' + %editc(Data_Prepara:'Z') +
403800100813     c                                      '_' + %editc(Ora__Prepara:'Z')
403900100708     c                   end
404000100706     c                   end
404100100120      *
404200100813     c                   end
404300100921      *
404400100921     c                   if        vabdcm = 0
404500100921     C                   Z-ADD     Data_Prepara  VABDCM
404600100921     c                   end
404700100813      *
404800090211     C*  Se non è indicato il nome del mittente = Partner mittente
404900090211     C     VABRMO        IFEQ      *BLANKS
405000090211     C                   MOVEL     ACORAG        VABRMO
405100090211     C                   MOVEL     INDCAP        VABCMO
405200090211     C     INDCAE        IFNE      *BLANKS
405300090211     C                   MOVEL     INDCAE        VABCMO
405400090211     C                   END
405500090211     C     INDSTA        IFNE      *BLANKS
405600090211     C                   MOVEL     INDSTA        VABNMO
405700090211     C                   END
405800090211     C                   END
405900090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
406000090211     C     §1BFG1        IFEQ      'N'
406100090211     C     §1BFG7        ANDEQ     'N'
406200090211     C     §1BFG2        ANDEQ     'N'
406300090211     C***                  Z-ADD0         VABNRS
406400090211     C                   END
406500090211     C*  Imposto segancollo da - a
406600090211     C     §1BFG1        IFEQ      'S'
406700090211     C     §1BFG7        ANDEQ     'N'
406800090211     C     §1BF12        ANDNE     'E'
406900090211      *
407000100701     C     Conta_Segn    IFEQ      *ZEROS
407100090211     C                   CLEAR                   VABNCD
407200090211     C                   CLEAR                   VABNCA
407300090211     C                   ELSE
407400100630     C                   Z-ADD     segn_BAR(1)   VABNCD
407500100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
407600090211     C                   ENDIF
407700090211     C*
407800090211     C                   END
407900090211     C*
408000090211     C*  Se cap mittente originale a blank abblenco nazione
408100090211     C     VABCMO        IFEQ      *BLANKS
408200090211     C                   MOVEL     *BLANKS       VABNMO
408300090211     C                   END
408400100701     C*
408500090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
408600090603     C* Se in testata
408700100701     c                   if        NAD_destinatario_in_testata = 'S' and
408800100701     c                             NAD_destinatario_in_dettaglio <> 'S'
408900090603     c                   eval         VABrsd =  tes_VABrsd
409000090603     c                   eval         VABrd2 =  tes_VABrd2
409100090603     c                   eval         VABind =  tes_VABind
409200090603     c                   eval         VABlod =  tes_VABlod
409300090603     c                   eval         VABnzd =  tes_VABnzd
409400090603     c                   end
409500090603     C* Se in testata
409600100701     c                   if        NAD_mittente_in_testata = 'S' and
409700100701     c                             NAD_mittente_in_dettaglio <> 'S'
409800090603     c                   eval         VABrmo = tes_VABrmo
409900090603     c                   eval         VABnmo = tes_VABnmo
410000090603     c                   eval         VABcmo = tes_VABcmo
410100090603     c                   end
410200100813      *
410300100813     C*  Per non bloccare la bollettazione
410400100813     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
410500100813     C*  o se il peso non è presente... imposto fisso il minimo 0,1
410600100813     c                   if        vabpkb = 0
410700100813     c                   z-add     0,1           vabpkb
410800100813     c                   end
410900100813     C*
411000100813      *
411100100813     C*  Se cap mittente originale a blank abblenco nazione
411200100813     C     VABCMO        IFEQ      *BLANKS
411300100813     C                   MOVEL     *BLANKS       VABNMO
411400100813     C                   END
411500100813      *
411600100813      *   Forza il numero passato nel CNI nel riferimento Numerico
411700100813     c                   if        §PVCNIRMN = 'S'
411800100813     c                   z-add     CNI_SeNumericovabRMN
411900100813     c                   end
412000100921      *
412100100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
412200100921     C     §puWRK        ifEQ      'S'
412300100921     C     §puraggDES    andeq     'S'
412400100921     c                   clear                   vabLNA
412500100921     c                   clear                   vabZNC
412600100921     c                   clear                   vabCMR
412700100921      *  il nome identico
412800100921     c                   eval      VABCMR  = §punomeCMR
412900100921      *   e in più la data del giorno
413000100921     C     §puinGIO      ifEQ      'S'
413100100921     c                   eval      VABCMR = %trim(VABCMR)
413200100921     c                                    + %editc(WOggi_CMR:'X')
413300100921     c                   end
413400100921      *
413500100921     c                   end
413600100921      *
413700110310      *  FORZATURA : devono essere tutti "OM"  --- richiesta del cliente
413800110505      *  FORZATURA : dal 5/5/2011 su richiesta via mail della comerciale Beatrice Marianna,
413900110505      *              che segue il cliente, tutti i COD devono essere espressi con
414000110505      *              incasso di tipo "TO"
414100110310     C     VABcas        ifne      0
414200110505     C*******            MOVEL     'OM'          VABTIC
414300110505     C                   MOVEL     'TO'          VABTIC
414400110310     c                   end
414500100813      *
414600090211      *? ------------------------------------------------------------------------
414700090213     C   30              WRITE     EDiVAB00                             99
414800090213     C  N30              UPDATE    EDiVAB00                             99
414900090211      *? ------------------------------------------------------------------------
415000090213     c   99              eval      errori_in_Wrt = 'S'
415100090211     C*
415200090211     c                   clear                   wri_vabtic        1
415300090211     C*
415400090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
415500100706     C                   EXSR      Scrive_Riferim
415600090211     C*
415700090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
415800090211     C     §1BF12        IFEQ      'E'
415900100706     C                   EXSR      Scrive_VAT
416000090211     C                   ELSE
416100090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
416200090211     C     §1BFG7        IFEQ      'S'
416300090211     C     §1BFG1        OREQ      'S'
416400100706     C                   EXSR      Scrive_VAD
416500090211     C                   END
416600090211     C                   END
416700090211      *
416800090211     C* Reimposto codice trattamento merce cliente
416900100628     C                   MOVEL     sav_DS1B      DS1B
417000090211     C**
417100090211     C* Do errore se previsto CMR ma non c'è
417200090211     C     §PTCMR        IFNE      ' '
417300090211     C                   EXSR      MEMCMR
417400090211     C                   END
417500090211     C* Do errore se previsto AFB ma non c'è
417600090211     C     §PTNFV        IFNE      ' '
417700090211     C                   EXSR      MEMAFB
417800090211     C                   END
417900090211     C*  Se il cliente vuole il ritorno delle date di consegna
418000090211     C*  è c'è una squdratura fra i segnacolli e il numero
418100090211     C*  dei colli che ci ha passato scrivo EDSUM
418200100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
418300090211     C                   EXSR      WRTSUM
418400090211     C                   END
418500090211     C*
418600090211     C                   ENDSR
418700090211     C*----------------------------------------------------------------
418800090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
418900090211     C*----------------------------------------------------------------
419000100630     C     CTRsegn_BAR   BEGSR
419100090211     C*
419200090211     C                   MOVEL     'N'           WNSGER            1
419300090211     C*
419400090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
419500100701     C     VABNCL        IFNE      Conta_Segn
419600090211     C                   MOVEL     'S'           WNSGER
419700100630     C                   eval       Squadratura_Colli = 'S'
419800090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
419900090211     c                              bollettati'
420000090211     C                   GOTO      FINSGN
420100090211     C                   END
420200100706      *
420300090211     C*  Riordino i segnacolli
420400090211     C     §1BFG7        IFEQ      'N'
420500100630     C                   SORTA     segn_BAR
420600090211     C*  Controllo se sono sequenziali
420700090211     C                   MOVEL     'N'           WNOSEQ            1
420800100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
420900100706     C*
421000100701     C     2             DO        Conta_Segn    X
421100100630     C     segn_BAR(X)   IFNE      WEXSGN
421200090211     C                   MOVEL     'S'           WNOSEQ
421300090211     C                   END
421400100630     C     segn_BAR(X)   ADD       1             WEXSGN
421500090211     C                   END
421600100706     C*
421700090211     C                   END
421800090211     C*
421900090211     C     FINSGN        ENDSR
422000090211     C*----------------------------------------------------------------
422100090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
422200090211     C*----------------------------------------------------------------
422300100630     C     CTRsegn_EEX   BEGSR
422400090211     C*
422500090211     C                   MOVEL     'N'           WNSGER            1
422600090211      *
422700090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
422800090211      *
422900100701     C     VABNCL        IFNE      totale_PCI
423000090211     C                   MOVEL     'S'           WNSGER
423100100630     C                   eval       Squadratura_Colli = 'S'
423200090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
423300090211     c                              bollettati'
423400090211     C                   END
423500090211      *
423600090211     C                   ENDSR
423700090211     C*----------------------------------------------------------------
423800090211     C*? ESEGUO SCRITTURA EDiVAD
423900090211     C*----------------------------------------------------------------
424000100706     C     Scrive_VAD    BEGSR
424100090211     C*
424200100701     C* Se non seq. scrivo i record
424300090211     C     §1BFG1        IFEQ      'S'
424400100701     C     Conta_Segn    ANDNE     *ZEROS
424500090211     C*
424600090211     C     §1BFG7        IFEQ      'S'
424700100701     C                   DO        Conta_Segn    X
424800090211     C                   Z-ADD     VABCCM        KCCM
424900100630     C                   Z-ADD     segn_BAR(X)   KNCD
425000090211     C                   Z-ADD     VABCNT        VADCNT
425100090211     C                   MOVEL     VABCMR        VADCMR
425200090211     C                   Z-ADD     VABAAS        VADAAS
425300090211     C                   Z-ADD     VABLNP        VADLNP
425400090211     C                   Z-ADD     VABNRS        VADNRS
425500090211     C                   Z-ADD     VABNSP        VADNSP
425600090211     C                   Z-ADD     1             VADNCL
425700100630     C                   MOVEL     segn_BAR(X)   VADCDP
425800100630     C                   Z-ADD     segn_BAR(X)   VADNCD
425900090211     C                   Z-ADD     *ZEROS        VADNCA
426000090211     C                   Z-ADD     VABCCM        VADCCM
426100090211     C                   movel     VABfgs        VADfgs
426200090213     C                   WRITE     EDiVAD00                             99
426300090213     c   99              eval      errori_in_Wrt = 'S'
426400090211     C                   END
426500090211     C* Se sequenziali scrivo un solo record
426600090211     C                   ELSE
426700090211     C                   Z-ADD     VABAAS        VADAAS
426800090211     C                   Z-ADD     VABLNP        VADLNP
426900090211     C                   Z-ADD     VABNRS        VADNRS
427000090211     C                   Z-ADD     VABNSP        VADNSP
427100090211     C                   Z-ADD     VABNCL        VADNCL
427200100630     C                   Z-ADD     segn_BAR(1)   VADNCD
427300100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
427400090211     C                   Z-ADD     VABCCM        VADCCM
427500090211     C                   movel     VABfgs        VADfgs
427600090213     C                   WRITE     EDiVAD00                             99
427700090213     c   99              eval      errori_in_Wrt = 'S'
427800090211     C                   END
427900090211     C*
428000090211     C                   ELSE
428100100701     C                   DO        Conta_Segn    X
428200090211     C                   Z-ADD     VABCCM        KCCM
428300100630     C                   Z-ADD     segn_BAR(X)   KNCD
428400090211     C                   Z-ADD     VABAAS        VADAAS
428500090211     C                   Z-ADD     VABLNP        VADLNP
428600090211     C                   Z-ADD     VABNRS        VADNRS
428700090211     C                   Z-ADD     VABNSP        VADNSP
428800100630     C                   MOVEL     segn_BAR(X)   VADCDP
428900090211     C                   Z-ADD     1             VADNCL
429000090211     C                   Z-ADD     *ZEROS        VADNCD
429100090211     C                   Z-ADD     *ZEROS        VADNCA
429200090211     C                   Z-ADD     VABCCM        VADCCM
429300090211     C                   movel     VABfgs        VADfgs
429400090213     C                   WRITE     EDiVAD00                             99
429500090213     c   99              eval      errori_in_Wrt = 'S'
429600090211     C                   END
429700090211     C*
429800090211     C                   END
429900090211     C*
430000090211     C                   ENDSR
430100090211     C*----------------------------------------------------------------
430200090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
430300090211     C*----------------------------------------------------------------
430400100706     C     Scrive_RiferimBEGSR
430500090211      *
430600090211      *  riferimento di consegna destinatario
430700100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
430800100701     c                             CTA_destinatario_in_testata   <> *blank
430900090211     C                   Z-ADD     VABCNT        VATCNT
431000090211     C                   MOVEL     VABCMR        VATCMR
431100090211     C                   Z-ADD     VABCCM        VATCCM
431200090211     C                   movel     VABfgs        VATfgs
431300090211     C                   Z-ADD     VABLNP        VATLNP
431400090211     C                   Z-ADD     VABAAS        VATAAS
431500090211     C                   Z-ADD     VABNRS        VATNRS
431600090211     C                   Z-ADD     VABNSP        VATNSP
431700090211     C                   MOVEL     'A'           VATTRC
431800100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
431900100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
432000100701     c                   else
432100100701     C                   eval       VATNOT = CTA_destinatario_in_testata
432200100701     c                   end
432300090213     C                   WRITE     EDiVAT00                             99
432400090213     c   99              eval      errori_in_Wrt = 'S'
432500090211     c                   end
432600090211      *
432700090211      *  riferimento di consegna telefonico
432800100701     c                   if        COM_telefono_in_dettaglio <> *blank or
432900100701     c                             COM_telefono_in_testata   <> *blank
433000090211     C                   Z-ADD     VABCNT        VATCNT
433100090211     C                   MOVEL     VABCMR        VATCMR
433200090211     C                   Z-ADD     VABCCM        VATCCM
433300090211     C                   movel     VABfgs        VATfgs
433400090211     C                   Z-ADD     VABLNP        VATLNP
433500090211     C                   Z-ADD     VABAAS        VATAAS
433600090211     C                   Z-ADD     VABNRS        VATNRS
433700090211     C                   Z-ADD     VABNSP        VATNSP
433800090211     C                   MOVEL     'B'           VATTRC
433900100701     c                   if        COM_telefono_in_dettaglio <> *blank
434000100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
434100100701     c                   else
434200100701     c                   eval      VATNOT = COM_telefono_in_testata
434300100701     c                   end
434400090213     C                   WRITE     EDiVAT00                             99
434500090213     c   99              eval      errori_in_Wrt = 'S'
434600090211     c                   end
434700090211      *
434800090211      * riferimento del Partner che una volta veniva riportato sul BL4
434900100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
435000100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
435100100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
435200100119      *
435300100701     c                   if        RFF_RifSpediz <> *blank
435400090211     C                   Z-ADD     VABCNT        VATCNT
435500090211     C                   MOVEL     VABCMR        VATCMR
435600090211     C                   Z-ADD     VABCCM        VATCCM
435700090211     C                   movel     VABfgs        VATfgs
435800090211     C                   Z-ADD     VABLNP        VATLNP
435900090211     C                   Z-ADD     VABAAS        VATAAS
436000090211     C                   Z-ADD     VABNRS        VATNRS
436100090211     C                   Z-ADD     VABNSP        VATNSP
436200090211     C                   MOVEL     'C'           VATTRC
436300100701     C                   MOVEL     RFF_RifSpediz VATNOT
436400090213     C                   WRITE     EDiVAT00                             99
436500090213     c   99              eval      errori_in_Wrt = 'S'
436600090211     c                   end
436700100119      *
436800090211     C                   ENDSR
436900090211     C*----------------------------------------------------------------
437000090211     C*? ESEGUO SCRITTURA EDiVAT
437100090211     C*----------------------------------------------------------------
437200100706     C     Scrive_VAT    BEGSR
437300090211      *
437400100701     C                   DO        totale_PCI    X
437500090211     C                   Z-ADD     VABCNT        VATCNT
437600090211     C                   MOVEL     VABCMR        VATCMR
437700090211     C                   Z-ADD     VABCCM        VATCCM
437800090211     C                   movel     VABfgs        VATfgs
437900090211     C                   Z-ADD     VABLNP        VATLNP
438000090211     C                   Z-ADD     VABAAS        VATAAS
438100090211     C                   Z-ADD     VABNRS        VATNRS
438200090211     C                   Z-ADD     VABNSP        VATNSP
438300090211     C                   MOVEL     'E'           VATTRC
438400090211      *
438500090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
438600090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
438700090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
438800100701     c                   if        PU_lunVAT     > 0
438900090211      *
439000090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
439100100630     C                   eval      VATnot =
439200100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
439300090211      *
439400090211     c                   else
439500090211      *
439600090211      * Se non è impostato nulla prende il segnacollo passato così com'è
439700100630     C                   MOVEL     segn_EEX(X)   VATNOT
439800090211     c                   end
439900090211      *
440000090213     C                   WRITE     EDiVAT00                             99
440100090213     c   99              eval      errori_in_Wrt = 'S'
440200090211     C                   END
440300090211      *
440400090211     C                   ENDSR
440500090211     C*----------------------------------------------------------------
440600090211     C*? ESEGUO SCRITTURA EDSUM
440700090211     C*----------------------------------------------------------------
440800090211     C     WRTSUM        BEGSR
440900090211     C*
441000100701     C* Se non seq. scrivo i   record
441100100701     C                   DO        Conta_Segn    X
441200090211     C                   Z-ADD     VABAAS        SUMAAS
441300090211     C                   Z-ADD     VABLNP        SUMFLS
441400100701     C                   MOVEL     PT_vabCCM     SUMLNP
441500090211     C                   Z-ADD     VABLNA        SUMLNA
441600090211     C                   Z-ADD     VABZNC        SUMZNC
441700090211     C                   Z-ADD     VABNRS        SUMNRS
441800090211     C                   Z-ADD     VABNSP        SUMNSP
441900100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
442000100701     C                   Z-ADD     PT_vabCCM     SUMCCM
442100090211     C                   MOVEL     *BLANKS       SUMSTS
442200090211     C                   MOVEL     *BLANKS       SUMFLG
442300100813     C                   eval       SUMCNI = CNI_Identificativo_Bolla
442400090211     C                   MOVEL     VABCMR        SUMCMR
442500090211     C                   Z-ADD     0             SUMNFE
442600090211     C                   Z-ADD     VABCCM        VADCCM
442700090213     C                   WRITE     EDSUM000                             99
442800090213     c   99              eval      errori_in_Wrt = 'S'
442900090211     C                   END
443000090211     C*
443100090211     C                   ENDSR
443200090211     c*==================================================================*
443300090211      * Manda un Msg in Posta AS Sede a EDP*
443400090211     c*==================================================================*
443500090211     c     SEND_MSG_EDP  begsr
443600090211      *
443700090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
443800090211      *   Lo manda una sola volta per lavoro
443900090211     c                   IF        Snd_Msg_alert  <> 'N' and
444000090211     c                             User_msg <> *blank
444100090211     c                   z-add     1             Jx
444200090211     C                   exsr      CalQEZSNDMG
444300090211     c                   end
444400090211      *
444500090211     c                   endsr
444600090213     c**********************************************************************
444700090213     c* reperimento numero Spedizione
444800090213     c**********************************************************************
444900090213     C     repnum        BEGSR
445000090213      *
445100090213      *    deve controllare sulla tabella "3C" se il numero spedizione
445200090213      *     deve essere mantenuto oppure no
445300090213     C*
445400090213     C                   clear                   Ds3C
445500090213     C                   Z-ADD     CODUT         TBLKUT
445600090213     C                   MOVEL     '3C'          TBLCOD
445700090213     C                   movel(p)  VABccm        TBLKEY
445800090213     C     KTABel        CHAIN     TABEL00F                           30
445900090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
446000090213     C                   MOVEL     TBLUNI        Ds3C
446100090213     C                   END
446200090213      *
446300090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
446400090213     c                   if        §3CFsp <> 'S'
446500090213      *
446600090213     C* NSP => Stacco un numeratore da AZNUM
446700090213     c                   movel     kpjbu         svkpjbu
446800090213     c                   clear                   kpjbu
446900090213     C                   clear                   TRUL33DS
447000090213     C                   eval      I33OPE = *zeros
447100090213     C                   eval      I33CNU = 302
447200090213     C                   eval      I33NUM = 1
447300090213     C                   movel     TRUL33DS      KPJBU
447400090213     C                   call      'TRUL33R'
447500090213     C                   parm                    KPJBA
447600090213     C                   movel     KPJBU         TRUL33DS
447700090213     c                   movel     svkpjbu       kpjbu
447800090213      ****
447900090213     C                   if        O33ERR = *zeros
448000100630     c                   z-add     o33nrf        NUM_Spediz
448100090213     C                   else
448200090213     C                   endif
448300090213      ****
448400090213     c                   ELSE
448500090213      *   se si vuole mantenere il numero spedizione
448600090213      ****
448700090213     C                   MOVEL     WOGGI         WANNO             4 0
448800090213     C                   MOVE      WANNO         KAAA
448900090213     C                   Z-ADD     3             KCNU
449000090213     C                   Z-ADD     VABlnp        KFIL
449100090213     C     KNUF          CHAIN     FLNUF                              9999
449200090213     C     *IN99         IFEQ      '0'
449300090213     C                   ADD       1             NUFNUM
449400100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
449500090213     C                   UPDATE    FLNUF
449600090213     C                   ELSE
449700090213     C                   END
449800090213     C*
449900090213     c                   EndIF
450000090213      **
450100090213     C                   ENDSR
450200090213     C*----------------------------------------------------------------
450300090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
450400090213     C*----------------------------------------------------------------
450500090213     C     MEMCMR        BEGSR
450600090213     C*
450700090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
450800090213     C     §PTCM1        IFEQ      'S'
450900090213     C                   CLEAR                   EDRDE000
451000090213     C                   MOVEL     'TD1154'      KNMC
451100090213     C                   Z-ADD     1             KNRP
451200100701     C                   MOVEL     PT_vabCCM     KLNP1
451300090213     C     KRDE          CHAIN     EDRDE01L                           31
451400090213     C     *IN31         IFEQ      '1'
451500090213     C                   Z-ADD     VABAAS        RDEAAS
451600090213     C                   Z-ADD     VABLNP        RDELNP
451700090213     C                   Z-ADD     VABNRS        RDENRS
451800090213     C                   Z-ADD     VABNSP        RDENSP
451900090213     C                   MOVEL     'TD1154'      RDENMC
452000090213     C                   Z-ADD     1             RDENRP
452100100706     C                   MOVEL     BGM_nrCMR     RDEVAL
452200100701     c                   if          RFF_NrCMR <> *Blank
452300100701     C                   MOVEL     RFF_NrCMR     RDEVAL
452400100701     c                   end
452500090213     C                   WRITE     EDRDE000                             99
452600090213     c   99              eval      errori_in_Wrt = 'S'
452700090213     C                   END
452800090213     C*  Memorizzo qualificatore CMR
452900090213     C                   CLEAR                   EDRDE000
453000090213     C                   MOVEL     'TD1153'      KNMC
453100090213     C                   Z-ADD     1             KNRP
453200100701     C                   MOVEL     PT_vabCCM     KLNP1
453300090213     C     KRDE          CHAIN     EDRDE01L                           31
453400090213     C     *IN31         IFEQ      '1'
453500090213     C                   Z-ADD     VABAAS        RDEAAS
453600090213     C                   Z-ADD     VABLNP        RDELNP
453700090213     C                   Z-ADD     VABNRS        RDENRS
453800090213     C                   Z-ADD     VABNSP        RDENSP
453900090213     C                   Z-ADD     1             RDENRP
454000090213     C                   MOVEL     'TD1153'      RDENMC
454100090213     C                   MOVEL     'CMR'         RDEVAL
454200090213     C                   WRITE     EDRDE000                             99
454300090213     c   99              eval      errori_in_Wrt = 'S'
454400090213     C                   END
454500090213     C*  Memorizzo la data
454600090213     C                   CLEAR                   EDRDE000
454700090213     C                   MOVEL     'TD2380'      KNMC
454800090213     C                   Z-ADD     1             KNRP
454900100701     C                   MOVEL     PT_vabCCM     KLNP1
455000090213     C     KRDE          CHAIN     EDRDE01L                           31
455100090213     C     *IN31         IFEQ      '1'
455200090213     C                   Z-ADD     VABAAS        RDEAAS
455300090213     C                   Z-ADD     VABLNP        RDELNP
455400090213     C                   Z-ADD     VABNRS        RDENRS
455500090213     C                   Z-ADD     VABNSP        RDENSP
455600090213     C                   Z-ADD     1             RDENRP
455700090213     C                   MOVEL     'TD2380'      RDENMC
455800100630     C                   MOVEL     DTM_DtCMR     RDEVAL
455900090213     C                   WRITE     EDRDE000                             99
456000090213     c   99              eval      errori_in_Wrt = 'S'
456100090213     C                   END
456200090213     C*
456300090213     C                   END                                                    §PTCM1 = 'S'
456400090213     C*
456500090213     C                   ENDSR
456600090213     C*----------------------------------------------------------------
456700090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
456800090213     C*----------------------------------------------------------------
456900090213     C     MEMAFB        BEGSR
457000090213     C*
457100090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
457200090213     C     §PTNF1        IFEQ      'S'
457300090213     C                   CLEAR                   EDRDE000
457400100701     C                   MOVEL     PT_vabCCM     KLNP1
457500090213     C                   MOVEL     'TD1154'      KNMC
457600090213     C                   Z-ADD     1             KNRP
457700090213     C     KRDE          CHAIN     EDRDE01L                           31
457800090213     C     *IN31         IFEQ      '1'
457900090213     C                   Z-ADD     VABAAS        RDEAAS
458000090213     C                   Z-ADD     VABLNP        RDELNP
458100090213     C                   Z-ADD     VABNRS        RDENRS
458200090213     C                   Z-ADD     VABNSP        RDENSP
458300090213     C                   MOVEL     'TD1154'      RDENMC
458400100701     C                   if        §PTCM1 <> 'S' or
458500100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
458600090213     C                   Z-ADD     1             RDENRP
458700090213     C                   ELSE
458800090213     C                   Z-ADD     2             RDENRP
458900090213     C                   END
459000100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
459100100701     c                   if          RFF_NrFviaggio <> *Blank
459200100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
459300100701     c                   end
459400090213     C                   WRITE     EDRDE000                             99
459500090213     c   99              eval      errori_in_Wrt = 'S'
459600090213     C                   END
459700090213     C*  Memorizzo qualificatore CMR
459800090213     C                   CLEAR                   EDRDE000
459900100701     C                   MOVEL     PT_vabCCM     KLNP1
460000090213     C                   MOVEL     'TD1153'      KNMC
460100090213     C                   Z-ADD     1             KNRP
460200090213     C     KRDE          CHAIN     EDRDE01L                           31
460300090213     C     *IN31         IFEQ      '1'
460400090213     C                   Z-ADD     VABAAS        RDEAAS
460500090213     C                   Z-ADD     VABLNP        RDELNP
460600090213     C                   Z-ADD     VABNRS        RDENRS
460700090213     C                   Z-ADD     VABNSP        RDENSP
460800100701     C                   if        §PTCM1 <> 'S' or
460900100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
461000090213     C                   Z-ADD     1             RDENRP
461100090213     C                   ELSE
461200090213     C                   Z-ADD     2             RDENRP
461300090213     C                   END
461400090213     C                   MOVEL     'TD1153'      RDENMC
461500090213     C                   MOVEL     'AFB'         RDEVAL
461600090213     C                   WRITE     EDRDE000                             99
461700090213     c   99              eval      errori_in_Wrt = 'S'
461800090213     C                   END
461900090213     C*  Memorizzo la data
462000090213     C                   CLEAR                   EDRDE000
462100100701     C                   MOVEL     PT_vabCCM     KLNP1
462200090213     C                   MOVEL     'TD2380'      KNMC
462300090213     C                   Z-ADD     1             KNRP
462400090213     C     KRDE          CHAIN     EDRDE01L                           31
462500090213     C     *IN31         IFEQ      '1'
462600090213     C                   Z-ADD     VABAAS        RDEAAS
462700090213     C                   Z-ADD     VABLNP        RDELNP
462800090213     C                   Z-ADD     VABNRS        RDENRS
462900090213     C                   Z-ADD     VABNSP        RDENSP
463000100701     C                   if        §PTCM1 <> 'S' or
463100100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
463200090213     C                   Z-ADD     1             RDENRP
463300090213     C                   ELSE
463400090213     C                   Z-ADD     2             RDENRP
463500090213     C                   END
463600090213     C                   MOVEL     'TD2380'      RDENMC
463700100630     C                   MOVEL     DTM_DtFViag   RDEVAL
463800090213     C                   WRITE     EDRDE000                             99
463900090213     c   99              eval      errori_in_Wrt = 'S'
464000090213     C                   END
464100090213     C*
464200090213     C                   END                                                    §PTCM1 = 'S'
464300090213     C*
464400090213     C                   ENDSR
464500090213     c*==================================================================*
464600090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
464700090213     c*==================================================================*
464800090213     c     Imp_ADDR_mail begsr
464900090213      *
465000090213     c                   clear                   Indirizzi
465100090213      *
465200090213      *  Lunghezza indirizzi presenti
465300100630     c                   eval      Lunghezza_Indirizzo =
465400100630     c                                       %Len(%TrimR(Mail_Ind))
465500090213     c                   z-add     1             al_char           3 0
465600090213     c                   z-add     1             dal_char          3 0
465700090213     c                   z-add     1             tot_char          3 0
465800090213      *
465900090213     c     successivo    tag
466000090213      *
466100090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
466200090213      *   e aggiunge il dominio Bartolini + il (;) separatore
466300090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
466400090213     c                   eval      tot_char = al_char - dal_char
466500100630     c                   z-add     dal_char      dalCarattere
466600100630     c                   z-add     tot_char      xTOTcaratteri
466700090213      *
466800090213     c                   clear                   Indir_Bartol     40
466900090213     c                   clear                   Indir_Parz       20
467000100630     c                   eval      Indir_Parz =
467100100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
467200100630      *
467300090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
467400090213     c                             %Trim(bart_it)
467500090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
467600090213      *
467700100630     c                   if        al_char = Lunghezza_Indirizzo
467800090213     c                   goto      fine_indmail
467900090213     c                   else
468000090213     c                   eval      dal_char = ( al_char + 1 )
468100090213     c                   goto      successivo
468200090213     c                   end
468300090213      *
468400090213     C     fine_indmail  TAG
468500090213      *
468600090213     C                   ENDSR
468700090213     c*==================================================================*
468800090213      * Manda un Msg x E-mail
468900090213     c*==================================================================*
469000090213     c     Invio_Mail    begsr
469100090213      *
469200090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
469300090213     C* Inizializzo variabili
469400090213     C                   movel     *blanks       wrkEml          253
469500090213     C                   movel     *blanks       wrkMsg         5000
469600090213     C                   movel     *blanks       wrkOgg           44
469700090213      *
469800090213     C* Valorizzo i campi della e-m@ail - indirizzo
469900100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
470000090213     C                   eval      wrkEml = Indirizzi
470100090213     C                   eval      wrkOgg = Oggetto
470200090213     C                   eval      wrkMsg = Messaggio
470300110203      ********
470400110203     C********           call(e)   'TIS701C'
470500110203     C********           parm                    wrkEml
470600110203     C********           parm                    wrkOgg
470700110203     C********           parm                    wrkMsg
470800110203      ** *****
470900110203     C                   call(e)   'TRTCT00R2'
471000110203     C                   parm                    wrkEml
471100110203     C                   parm                    wrkOgg
471200110203     C                   parm                    wrkMsg
471300090213     C*
471400090213     C                   ENDSR
471500090603      *
471600090211     ***********************************************************************
471700090211     **
471800090211     ** Send Message (QEZSNDMG) API
471900090211     **
472000090211     ***********************************************************************
472100090211     C     CalQEZSNDMG   BEGSR
472200090211      *
472300090211     ** Invio messaggio all'utente.
472400090211     C                   EVAL      SndMg03 = msgDSP
472500090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
472600090211     C                   EVAL      SndMg05(Jx) = User_msg
472700090211     C                   EVAL      SndMg06 = Jx
472800090211     C                   CLEAR                   QUSEC
472900090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
473000090211
473100090211     C                   CALL      'QEZSNDMG'
473200090211     C                   PARM                    SndMg01
473300090211     C                   PARM                    SndMg02
473400090211     C                   PARM                    SndMg03
473500090211     C                   PARM                    SndMg04
473600090211     C                   PARM                    SndMg05
473700090211     C                   PARM                    SndMg06
473800090211     C                   PARM                    SndMg07
473900090211     C                   PARM                    SndMg08
474000090211     C                   PARM                    Qusec
474100090211     C                   PARM                    SndMg10
474200090211     C                   PARM                    SndMg11
474300090211     C                   PARM                    SndMg12
474400090211      *
474500090211     C                   ENDSR
474600100708      * ?------------------------------------------------------------------ */
474700100708      *  Controlla la trasmissione   se completa
474800100708      * ?------------------------------------------------------------------ */
474900100708     c     Check_Trasm   Begsr
475000100708
475100100708     C                   clear                   se_errore
475200100708     C                   clear                   Riga_iNIZIo       1
475300100708     C                   clear                   Riga_fINe         1
475400100708      ** primo  record
475500100708     c     *start        setll     tivin00r
475600100708      *
475700100708     c     rileggi       tag
475800100708     c                   EXSR      LEGGE_TIVIN_R
475900100708      *
476000100708     c                   if        EoFTivin <> 'S'
476100100708      *
476200100708     c                   if        dati = *blank
476300100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
476400100708      * le leggo e le escludo.
476500100708     C                   eval      vinFLG = '1'
476600100708     c                   update    Tivin000
476700100708     c                   goto      rileggi
476800100708     c                   end
476900100708      * ?              /*---------------------- */
477000100708     c                   exsr      Decod_Segmento
477100100708      * ?              /*---------------------- */
477200100708     c                   endif
477300100708      **
477400100708      ** ultimo record
477500100708     c     *hival        setll     tivin00r
477600100708     c     leggiAncora   tag
477700100708     c                   readp     tivin00r
477800100708     c                   if        %Eof(tivin00r)
477900100708     c                   move      'S'           EoFTivin
478000100708     c                   else
478100100708     c                   clear                   dati
478200100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
478300100708      *
478400100708     c                   if        dati = *blank
478500100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
478600100708      * le leggo e le escludo.
478700100708     C                   eval      vinFLG = '1'
478800100708     c                   update    Tivin000
478900100708     c                   goto      leggiAncora
479000100708     c                   end
479100100708      * ?              /*---------------------- */
479200100708     c                   exsr      Decod_Segmento
479300100708      * ?              /*---------------------- */
479400100708     c                   endif
479500100708
479600100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
479700100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
479800100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
479900100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
480000100708      * ?-----> Errore
480100100708     C                   eval      se_errore = 'S'
480200100708     c                   end
480300100811      *
480400100708     c                   endSR
480500100708      * ?------------------------------------------------------------------ */
480600100811      *
480700100811      * ?------------------------------------------------------------------ */
480800100811     c     chiama_DecoderbegSR
480900100811      *
481000100811     C                   clear                   keyUNBCLI
481100100811     C                   clear                   keyTIPOMSG
481200100811     C                   clear                   keyVERSION
481300100811     C                   clear                   keyRELEASE
481400100811     C                   clear                   keyAGENCY
481500100811     C                   clear                   keyASSOCIA
481600100811      **--------
481700100811      *  Ritorna la DS GENERICA oppure l'errore
481800100811      **--------
481900100811      *  Chiamata generica x decodificare il singolo segmento letto
482000100811     c                   call      'TRTCT00R1'
482100100811      * input
482200100811     c                   parm                    tipo_seg
482300100811     c                   parm                    segmento
482400100811     C                   parm                    keyUNBCLI
482500100811     C                   parm      'IFCSUM'      keyTIPOMSG
482600100811     C                   parm      'D'           keyVERSION
482700100811     C                   parm      '96A'         keyRELEASE
482800100811     C                   parm      'UN'          keyAGENCY
482900100811     C                   parm                    keyASSOCIA
483000100811      *output
483100100811     c                   parm                    Errore
483200100811     c                   parm                    DsGenerica
483300100811     c                   parm                    Valori_inErr
483400100811     c                   parm                    Descr_Errore
483500100811     c                   parm                    trovato_seg
483600100811      *
483700100811     c                   endSR
483800121105      *---------------------------------------------------------------*
483900121105      * CTRL caricamento schiere    PT                               -*
484000121105      *---------------------------------------------------------------*
484100121105     C     Check_PT      begsr
484200121105     C*
484300121105     c* Controllo riempimento schiera
484400121105     c                   clear                   trul0sds
484500121105     c                   eval      i0sski='PT_key'
484600121105     c                   eval      i0sele=%elem(PT_key)
484700121105     c                   eval      i0spie=x
484800121105     c                   eval      i0sfile='EDTAB00F'
484900121105     c                   eval      i0ssif=knsif
485000121105     c                   eval      i0spgm='TRTCT16R'
485100121113     c                   move      kpjbu         SvKpjbu
485200121113     c                   clear                   kpjbu
485300121105     c                   movel     trul0sds      kpjbu
485400121105     c                   call      'TRUL0SR'
485500121105     c                   parm                    kpjba
485600121113     c                   eval      kpjbu  =  SvKpjbu
485700121105     C*
485800121105     C                   ENDSR
485900100811      * ?------------------------------------------------------------------ */
486000090211     O*****************************************************************
486100090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
486200090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
486300090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
486400090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
