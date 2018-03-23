000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600100811????  *  TRASCODIFICA : MESSAGGI EDI  -   BOLLE IMPORT   con  IFCSUM vers D96A  *
000700110117????  *      è già in formato FLAT non occorre eseguire lo split dei campi      *
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
002800090213     FEDSUM00F  O  A E           K DISK    commit   INFDS(SRECDS)
002900100701      *----------------------------------------------------*
003000100701      * Stringhe per conversione alfanumerico/numerico N.Documento
003100100701     D CA              S             78    DIM(1) CTDATA PERRCD(1)
003200100701     D CN              S             78    DIM(1) CTDATA PERRCD(1)
003300940321      *----------------------------------------------------*
003400090209      * numero relativo di record
003500090209     D SRECDS          DS
003600090209     D  NRREC                397    400B 0
003700090209      *
003800090213      * numero relativo di record
003900090213     D VINRECDS        DS
004000090213     D  VNREC                397    400B 0
004100100701      * ---------------------------------------------------
004200100701     D KPJBA         E DS
004300100701     D     svkpjbu     s                   like(kpjbu)
004400100701     D FNLV55DS      E DS
004500100701     D TIBS55DS      E DS
004600100701     D TISI95DS      E DS
004700100701     D UTEDSE0F      E DS
004800100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
004900100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
005000100701      * Ds scomposzione tipo capoconti
005100100701     D TCUDS           DS
005200100701     D  F34                    3      4
005300100701     D  F56                    5      6
005400100701      *
005500100701      * Numeratore Bolle (302)
005600100701     D trul33ds      E DS
005700100701     D Ds3C          E DS
005800100701     D DS1B          E DS
005900100701     d sav_DS1B        s                   like(Ds1B)
006000100701     D DS15          E DS
006100100701     D DSCV          E DS
006200100701     D TRTC86DS      E DS
006300100701     D EDIDSPT       E DS
006400100701     D EDIDSPU       E DS
006500100701     D EDIDSPV       E DS
006600100701     D EDIDSCL       E DS
006700100701     D EDIDSSS       E DS
006800100701     D EDIDSTC       E DS
006900100701      **
007000100701      *  DS x salvataggio dati impostati in EDiVAB
007100100701     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
007200100701     D SALVA           DS           545
007300100701      **
007400100701     D WLBDA8          DS
007500100701     D  G02DAT                 1      8  0
007600100701     D  G02INV                 9     16  0
007700100701     D  G02ERR                17     17
007800100701     D  G02TGI                18     22  0
007900090213      *
008000100701      *----------------------------------------------------*
008100090205      **  Elenco DS di tutti i Segmenti da tradurre
008200100701      *----------------------------------------------------*
008300110117???? D eds10BGM      E DS
008400110117???? D eds10CNI      E DS
008500110127???? D eds10COM      E DS
008600110127???? D eds10CTA      E DS
008700110117???? D eds10DOC      E DS
008800110117???? D eds10DTM      E DS
008900110127???? D eds10FTX      E DS
009000110127???? D  D05                   16    365    DIM(5)
009100110117???? D eds10GID      E DS
009200110126???? D eds10GIN      E DS
009300110127???? D  GIN_10                 9    358    DIM(10)
009400110117???? D eds10MEA      E DS
009500110127???? D eds10MOA      E DS
009600110117???? D eds10NAD      E DS
009700110117???? D eds10PCI      E DS
009800110127???? D  PCI_10                 9    358    DIM(10)
009900110117???? D eds10RFF      E DS
010000110117???? D eds10TDT      E DS
010100110117???? D eds10TOD      E DS
010200110117???? D eds10TSR      E DS
010300110117???? D eds10UNB      E DS
010400110117???? D eds10UNH      E DS
010500110127???? D eds10UNT      E DS
010600100701      *----------------------------------------------------*
010700090210      **
010800090210     D  X30            S             35    DIM(20)                              generico segnacolli
010900090205      **
011000090209     D Valori_inErr    ds
011100090209     D  SKout_Errori                  1    DIM(50)
011200100630      **
011300090209     D Descr_Errore    ds
011400090209     D  SKout_DesErr                 50    DIM(50)
011500090209      *
011600090205      *----------------------------------------------------*
011700091016     D EoFTivin        s              1
011800091016      *----------------------------------------------------*
011900090205     D Tipo_seg        S              3
012000100720     D Trovato_seg     S              1
012100100716      *
012200100716     d keyUNBCLI       s             35
012300100716     d keyTIPOMSG      s              6
012400100716     d keyVERSION      s              3
012500100716     d keyRELEASE      s              3
012600100716     d keyAGENCY       s              3
012700100716     d keyASSOCIA      s              6
012800100716      *
012900100720     D DsGenerica      s           2048
013000090209     D segmento        s           2048
013100090209     D Errore          s              1
013200100701      *
013300000223     D W0140           S             14  0
013400991129     D WORA            S              6  0
013500100701     D DataGGMMAAA     S              8  0
013600100701     D DATEU           S              8  0
013700100701     D DATA_eur        S               D   DATFMT(*eur)
013800100701      *
013900100630     D sk              S              3  0 INZ
014000100630     D sx              S              3  0 INZ
014100100628     D XX              S              3  0 INZ
014200110328     d Key_Generica35  s             35
014300100630     D NUM_Spediz      s                   LIKE(vabnsp)
014400110117     D UNH_record      s                   LIKE(vnREC)
014500100708     D IniDET_RECord   s                   LIKE(vnREC)
014600100708     D FinDET_RECord   s                   LIKE(vnREC)
014700090213     D Last_RECord     s                   LIKE(vnREC)
014800110117     D UNH_SEGMENTO    s              1
014900110117     D segmento_end    s              1
015000100318      *------------------------------------------
015100100318      **  Definizione Qualificatori
015200100318      *------------------------------------------
015300100319      *
015400100628???? D Documento_Manifest...
015500110117???? D                 s              3    INZ('630')
015600100701???? D Totale_Peso_Lordo...
015700100701???? D                 s              3    INZ('7  ')
015800100701???? D Totale_Nr_Spedizioni...
015900100701???? D                 s              3    INZ('10 ')
016000100701???? D Totale_Nr_Colli...
016100100701???? D                 s              3    INZ('11 ')
016200100701???? D Totale_Peso_Netto...
016300100701???? D                 s              3    INZ('ZCW')
016400100628???? D Riferimento_Spedizione...
016500110117???? D                 s              3    INZ('ACL')
016600100708???? D Riferimento_Numerico...
016700110117???? D                 s              3    INZ('ADE')
016800100318???? D Riferimento_Cliente_particolare...
016900100318???? D                 s              3    INZ('FF ')
017000100701???? D X_Cargo_Manifest...
017100100318???? D                 s              3    INZ('AFB')
017200100701???? D X_Numero_CMR...
017300100318???? D                 s              3    INZ('CMR')
017400100318???? D Telephone_Number...
017500100318???? D                 s              3    INZ('TE ')
017600100318???? D TeleFax_Number...
017700100318???? D                 s              3    INZ('TX ')
017800100628???? D Porto_Assegnato...
017900100628???? D                 s              3    INZ('001')
018000100628???? D Porto_Franco...
018100100628???? D                 s              3    INZ('002')
018200100628???? D Porto_Franco_uncleared...
018300100628???? D                 s              3    INZ('003')
018400100628???? D Porto_Franco_domicile...
018500100628???? D                 s              3    INZ('004')
018600100318???? D Volume...
018700100318???? D                 s              3    INZ('VOL')
018800100705???? D Weight...
018900100705???? D                 s              3    INZ('WT ')
019000100319???? D G_Weight...
019100110117???? D                 s              3    INZ('AAD')
019200100318???? D Gross_Weight...
019300110117???? D                 s              3    INZ('AAG')
019400100706???? D Gross_Weight_E...
019500100706???? D                 s              3    INZ('AAE')
019600100706???? D N_Weight...
019700100706???? D                 s              3    INZ('N  ')
019800100706???? D Net_Weight...
019900100706???? D                 s              3    INZ('AAF')
020000100318???? D Kilograms...
020100100318???? D                 s              3    INZ('KGM')
020200100319???? D Grams...
020300100319???? D                 s              3    INZ('163')
020400100318???? D Meters...
020500100318???? D                 s              3    INZ('MTR')
020600100318???? D Cubic_Meters...
020700100318???? D                 s              3    INZ('MTQ')
020800100318???? D Shipper_assigned...
020900100318???? D                 s              3    INZ('24 ')
021000100705???? D Data_senza_ora...
021100100628???? D                 s              3    INZ('102')
021200100705???? D Data_con_ora...
021300100628???? D                 s              3    INZ('203')
021400100705???? D Data_con_i_secondi...
021500100705???? D                 s              3    INZ('204')
021600100630???? D Squadratura_Colli...
021700100630???? D                 s              1    INZ('N')
021800100316      *---------------------------------------------------------------------*
021900100316      **  segmenti Identificativi parti specifiche del messaggio
022000100316      *---------------------------------------------------------------------*
022100100701???? D Segm_Inizio_Messaggio...
022200100701???? D                 s              3    INZ('UNB')
022300100701???? D Segm_Testata_Dettagli...
022400100701???? D                 s              3    INZ('UNH')
022500100701???? D Segm_Inizio_Dettaglio...
022600100316???? D                 s              3    INZ('CNI')
022700100701???? D seg_imprevisto...
022800100701???? D                 s              1
022900100726???? D Forza_Uscita...
023000100726???? D                 s              1
023100100701      *---------------------------------------------------------------------*
023200100701???? D Dati_di_Testata...
023300100701???? D                 s              1    INZ('S')
023400100701???? D Contatore_Dettagli...
023500100701???? D                 s              5s 0 INZ(0)
023600100728???? D Segmento_in_errore...
023700100728???? D                 s              5s 0 INZ(0)
023800100701      *---------------------------------------------------------------------*
023900100910      *
024000100910      * Tabella partner
024100121105     D PT_key          S             35    DIM(200)
024200121105     D PT_Parz         S             35    DIM(%elem(PT_key))
024300121105     D PT_KSC          S              7    DIM(%elem(PT_key))
024400121105     D PT_uni          S             90    DIM(%elem(PT_key))
024500121105     D PU_uni          S             90    DIM(%elem(PT_key))
024600121105     D PV_uni          S             90    DIM(%elem(PT_key))
024700121105     D TRUL0SDS      E DS
024800100910      *
024900100910     d PT_KeyXsav      s              3  0 inz(0)
025000100910     D PT_trovata      s              1    inz('N')
025100100910     D PU_key          S             35
025200100910     D PV_key          S             35
025300100910     D PU_HLD_SpedVAX  S              1
025400100910     D PT_con_piu_Nazioni...
025500100910     d                 s              1
025600100910      *
025700100910     D  PT_vabLNP      s                   LIKE(vablnp)
025800100910     D  PT_vabNRS      s                   LIKE(vabnrs)
025900100910     D  PT_vabCTM      s                   LIKE(vabctm)
026000100910     D  PT_vabCCM      s                   LIKE(vabccm)
026100100910     D  PU_lunVAT      s              2s 0
026200100910     D  PU_TipoServ    s              2a
026300100910     D  PU_SpecServ    s              2a
026400100910      *
026500100910     D Nr_PT           S              3  0 INZ
026600100910     D savXX_PT_key    S              3  0 INZ
026700100628      *
026800100701      *---------------------------------------------------------------------*
026900100630     d UNB_diWork      s                   like(UNB0004)
027000100628     d  UNB_Mittente   s                   like(UNB0004)
027100100706     d BGM_Id_Testata  s             35A   inz(' ')
027200100706     d  BGM_NrFviaggio...
027300100628     d                 s             35A
027400100706     d  BGM_nrCMR      s             35A
027500100701     d  RFF_NrFviaggio...
027600100701     d                 s             35A
027700100701     d  RFF_NrCMR      s             35A
027800100701     d  RFF_RifSpediz  s             35A
027900100910     d  RFF_DalPrimoCollo...
028000100910     d                 s              1A
028100100910     d  RFF_DopoIlPrimoCollo...
028200100910     d                 s              1A
028300100701     d UNB_parziale    s              1    inz('N')
028400100701     d UNB_Parz        s             35    inz(' ')
028500100812     d UNB_Generico    s             31    inz(' ')
028600100701     d UNB_NrTrasmiss  s             14    inz(' ')
028700100701     d UNZ_NrTrasmiss  s             14    inz(' ')
028800100701     d UNH_Rifer_Msg   s             14    inz(' ')
028900100701     d UNH_VABrma      s                   like(VABrma)
029000100701     d UNH_VABrmn      s                   like(VABrmn)
029100100708     d NAD_SF_x_CMR    s             35    inz(' ')
029200100910      *
029300100910???? D NAD_destinatario_in_testata...
029400100910???? D                 s              1
029500100910???? D NAD_destinatario_in_dettaglio...
029600100910???? D                 s              1
029700100910???? D NAD_mittente_in_testata...
029800100910???? D                 s              1
029900100910???? D NAD_mittente_in_Dettaglio...
030000100910???? D                 s              1
030100100910     d CTA_destinatario_in_Dettaglio...
030200100910     d                 s             35A
030300100910     d CTA_destinatario_in_testata...
030400100910     d                 s             35A
030500100910     d COM_telefono_in_Dettaglio...
030600100910     d                 s             35A
030700100910     d COM_telefono_in_testata...
030800100910     d                 s             35A
030900100701      *
031000100630     d  DTM_DtParten   s              8s 0
031100100630     d  DTM_DtFViag    s              8s 0
031200100630     d  DTM_DtCMR      s              8s 0
031300100811     d  DTM_DtConsRic  s              8s 0
031400100811     d  DTM_OraCnsRic  s              4s 0
031500100630     d  DTM_OraFViag   s              4s 0
031600100630     d  DTM_OraCMR     s              4s 0
031700100702     d  DTM_DtArrivo   s              8s 0
031800100702     d  DTM_OraArrivo  s              4s 0
031900100706     d  DTM_OraParten  s              4s 0
032000100702     d  CNI_Identificativo_Bolla...
032100100702     d                 s             35A
032200100813     d  CNI_seNumerico...
032300100813     d                 s                   LIKE(vabRMN)
032400100701     d  GID_aTotale    s              1A
032500100705     d  GID_ColliInAdd...
032600100705     d                 s                   like(gid722420)
032700100910      *---------------                                                      *
032800090209      *  DS x scomposizione segnacollo
032900100630     D DS_SegnBART     DS
033000100630     D  SegnBART_LNP           1      3  0
033100100630     D  SegnBART_LNA           4      6  0
033200100630     D  SegnBART_NRS           7      8  0
033300100630     D  SegnBART_NSC           9     15  0
033400100630     D  SegnBART_ZNC          16     17  0
033500100701      **
033600090209      *---------------                                                      *
033700100701     D VABtic_work     s                   LIKE(vabtic)
033800100630      *
033900090603     d tes_VABrmo      s                   LIKE(VABrmo)
034000090603     d tes_VABnmo      s                   LIKE(VABnmo)
034100090603     d tes_VABcmo      s                   LIKE(VABcmo)
034200090209     C*
034300090603     d tes_VABrsd      s                   LIKE(VABrsd)
034400090603     d tes_VABrd2      s                   LIKE(VABrd2)
034500090603     d tes_VABind      s                   LIKE(VABind)
034600090603     d tes_VABlod      s                   LIKE(VABlod)
034700090603     d tes_VABnzd      s                   LIKE(VABnzd)
034800090603     C*
034900100813     d tes_VABtsp      s                   LIKE(VABtsp)
035000100813     d tes_VABtc1      s                   LIKE(VABtc1)
035100100813     d tes_VABnot      s                   LIKE(VABnot)
035200100813     C*
035300090603     d tes_Valuta      s                   LIKE(VABvca)
035400090603     C*
035500100910     D SUvabRMO        s                   LIKE(vabRMO)
035600090209      *---------------------------------------------------------------------*
035700090209      * Schiere
035800090209      *---------------------------------------------------------------------*
035900090209      * Schiera per reperimento nr.seganacollo
036000100630     D segn_BAR        S              7  0 DIM(5000)
036100100630     D segn_EEX        S             35    DIM(5000)                            EUROEXPRESS
036200100630      *
036300100630      *
036400090209      * Nr. documento alfanumerico da decodificare
036500090209     D NDA             S              1    DIM(35)
036600100630      *
036700090209      * Nr. documento alfanumerico da già decodificato
036800090209     D NDN             S              1    DIM(15)
036900100630      *
037000090209      * Messaggi di errore
037100090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
037200100630      *
037300090209      * Tabella codici trattamento merce (1B)
037400100701     D Cod_Tb_CTM      S              2    DIM(200)
037500100701     D Des_Tb_CTM      S             67    DIM(200)
037600100630      *
037700090209      * Tabella codici valuta
037800090209     D CCV             S              3    DIM(200)
037900090209     D DCV             S             89    DIM(200)
038000100630      *
038100090209      * Tabella codici nazioni
038200090209     D C15             S              3    DIM(500)
038300090209     D ISO             S              3    DIM(500)
038400090209     D CPF             S              2  0 DIM(500)
038500100630      *
038600090209      * Tabella CL --> codici clienti - tariffa
038700130305     D Cod_Tb_CL       S             35    DIM(999)
038800130305     D Des_Tb_CL       S             90    DIM(999)
038900100630      *
039000090209      * Schiere x caricamento cod.cliente  <>
039100130305     D skCLienti       S              7  0 DIM(999)
039200100630      *
039300100318      * Priorità trasporto
039400100705     D Key_TipServ     S             35    DIM(100)
039500100702     D TipoServizio    S              1    DIM(100)
039600100702      *
039700100702      * Decodifiche servizi speciali
039800100702     D SpecialServ     S             35    DIM(100)
039900100702     D Key_ServSpec    S              3    DIM(100)
040000100702     D UNI_ServSpec    S             90    DIM(100)
040100100630      *
040200090209      * Termini di consegna
040300110328     D K35_TpBolla     S             35    DIM(100)
040400110328     D Cod_TpBolla     S              3    DIM(100)
040500110328     D Decod_Porto     S              1    DIM(100)
040600110328     d Trovata_Tab_TB  S              1
040700100630      *
040800090216      * Tipo pagamento C/Assegno
040900100705     D K35_TpIncas     S             35    DIM(100)
041000100705     D TipoIncasso     S              2    DIM(100)
041100100630      *
041200090209      * Schiere x routine di conversione CAP
041300090209     D CAP5            S              1    DIM(5)
041400090209     D CAP9            S              1    DIM(9)
041500090209     D CAPM            S              1    DIM(9)
041600100630      *
041700090209      * Schiera per memorizzazione FTX ricevuti
041800090209     D QUA             S              3    DIM(100)
041900090209     D FTX             S             70    DIM(100)
042000100630      *
042100090209      * Tabelle capiconto
042200090209      * Schiere x località
042300090209     D LOD             S              1    DIM(35)
042400100630      *
042500060608      **?------------------------------------------------------------------ */
042600090601     C*? Ds Scrittura del VAT "E"
042700060608      **?------------------------------------------------------------------ */
042800060612     D XTOEBCDDS     E DS
042900060620      *
043000060608      **?------------------------------------------------------------------ */
043100100701      *
043200090209      **?------------------------------------------------------------------ */
043300090209     C* Definizione campi di work
043400090209     D kKUT            s                   LIKE(TBLKUT)
043500090209     D kCOD            s                   LIKE(TBLCOD)
043600090209     D kKEY            s                   LIKE(TBLKEY)
043700090209     D kKCC            s                   LIKE(ACOKCC)
043800090209     D kKSC            s                   LIKE(ACOKSC)
043900090209     D kAAA            s                   LIKE(NUFAAA)
044000090209     D kCNU            s                   LIKE(NUFCNU)
044100090209     D kFIL            s                   LIKE(NUFFIL)
044200090209     D kAAS            s                   LIKE(VABAAS)
044300090209     D kLNP            s                   LIKE(VABLNP)
044400090209     D kNRS            s                   LIKE(VABNRS)
044500090209     D kNSP            s                   LIKE(VABNSP)
044600090209     D kCMR            s                   LIKE(VABCMR)
044700090209     D kCCM            s                   LIKE(VABCCM)
044800090209     D kNCD            s                   LIKE(VADNCD)
044900090210     D kCNT            s                   LIKE(VADCNT)
045000090209     D kNMC            s                   LIKE(RDENMC)
045100090209     D kNRP            s                   LIKE(RDENRP)
045200090209     D kLNP1           s                   LIKE(RDELNP)
045300090209      *
045400090209      *  Invio E-mail
045500090209     D Indirizzi       s            253
045600090209     D Oggetto         s             44
045700090209     D Messaggio       s           5000
045800090209      *
045900090209     d   DSmail        ds
046000090209     D Mail_Ind                      44
046100090209     d   IND_char                     1    dim(44)
046200090209      *
046300100630     D Lunghezza_Indirizzo...
046400100630     D                 s              5u 0
046500090209      *
046600100630     D dalCarattere    s              5u 0
046700100630     D xTOTcaratteri   s              5u 0
046800090209      *
046900090209     C*---------------------------------------------------------------*
047000090209     D MsgDSP          S            256                                         Testo generico
047100090209     C*---------------------------------------------------------------*
047200090209     D SndMg01         S             10                                         Message type
047300090209     D                                     INZ('*INFO')
047400090209     D SndMg02         S             10                                         Deluvery mode
047500090209     D                                     INZ('*BREAK')
047600090209     D SndMg03         S            256                                         Message text
047700090209     D SndMg04         S             10I 0                                      Length of text
047800090209     D                                     INZ(%SIZE(SndMg03))
047900090209     D SndMg05         S             10                                         User profile
048000090209     D                                     DIM(299)
048100090209     D SndMg06         S             10I 0                                      Number of user
048200090209     D SndMg07         S             10I 0                                      Message sent indic.
048300090209     D SndMg08         S             10I 0                                      Function requested
048400090209     D SndMg10         S              1                                         Show display
048500090209     D                                     INZ('N')
048600090209     D SndMg11         S             20                                         Qualified MSGQ name
048700090209     D SndMg12         S              4                                         Name type indicator
048800090209     D                                     INZ('*USR')
048900090209      * indice di scihera
049000090209     D Jx              S              5I 0
049100090209      *
049200090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
049300090209      *
049400090209     d bart_it         C                   CONST('@Bartolini.it;')
049500090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
049600060612      * ?================================================================== */
049700060612      * ?   * Campi x decodifica * (INPUT  del Record)
049800100319      * ?================================================================== */
049900090130     D  Dati           s           2048
050000060612      * ?   *--------------------------------------------------------------*
050100060612     D  se_errore      s              1    inz(' ')
050200060612      * ?* ------------------------------------------------------ *
050300060710     D Digits          C                   '0123456789'
050400091019     D*------------------
050500091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
050600091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
050700060612      * ?================================================================== */
050800060612      *   Ciclo principale
050900090205      * ?================================================================== */
051000060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
051100060612     C*
051200060612     C                   if        not %open(tivin00r)
051300060612     C                   open      tivin00r
051400060612     C                   endif
051500110117
051600060612      * ?              /*---------------------- */
051700060612     c                   exsr      Importa_Msg
051800060612      * ?              /*---------------------- */
051900060612
052000060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
052100090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
052200090225     c                             esito ='0'
052300060710     C                   eval      esito ='0'
052400090225      *
052500090225     c                   COMMIT
052600090224      *
052700090213     c                   if        almeno_un_due ='S'
052800090213     C                   eval      esito ='1'
052900060614     C                   endif
053000090213     C                   endif
053100100708      * chiude TIVIN
053200100708     C                   if        %open(tivin00r)
053300100708     C                   close     tivin00r
053400100708     C                   endif
053500060614      *
053600060612     c                   SETON                                        LR
053700060612      * ?================================================================== */
053800100708      *? Importa i records della trasmissione
053900060612      * ?------------------------------------------------------------------ */
054000060612     c     Importa_Msg   Begsr
054100060614
054200100726     c                   clear                   Forza_Uscita
054300060612     c     *start        setll     Tivin00r
054400100708
054500100708     c                   EXSR      LEGGE_TIVIN_R
054600100708     c                   dow       EoFTivin <> 'S'
054700060614
054800060614      * solo i record sflaggati da rielaborare
054900110107     c                   IF        vinFLG = *blank
055000110107      *** ++++++
055100090211      *  Sempre Record OK
055200090211      ** Controlli formali sui campi
055300090211     C                   eval      vinFLG = '1'
055400060612     c                   clear                   se_errore
055500110107      *
055600110107      *** >>>>>>
055700110107     c                   IF        vinDTA <> *blank
055800110107      *** >>>>>>
055900060612
056000060612      ** Decodifica record a campi variabili
056100060612      * ?              /*---------------------- */
056200090205     c                   exsr      Decod_Segmento
056300090209      * ?              /*---------------------- */
056400060612
056500060621      *  x errore bloccante nella composizione del VAB/VAT
056600060621     c                   if        err_bloccante ='S'
056700060621     C                   eval      vinFLG = '2'
056800090211     c                   eval      esito  = '2'
056900100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
057000100921     C                   EVAL      Messaggio = 'EDI -
057100100921     C                             IFCSUM D96A - msg su UPLOAD -
057200100921     C                             con UNB non presente in tabella PT..'
057300100319     c                   if        §PVinv <> 'N'
057400100319     c                   exsr      invio_mail
057500060621     c                   end
057600100319     c                   end
057700060612
057800110107      *** >>>>>>
057900110107     c                   endIF
058000110107      *** >>>>>>
058100110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
058200110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
058300110107      *         un erroneo messaggio
058400110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
058500060612     c                   update    Tivin000
058600090211
058700090211      *** se non è riconosciuto l'UNB deve uscire direttamente
058800090211     c                   if        se_errore ='S'  and
058900100708     c                             tipo_seg = Segm_Inizio_Messaggio
059000090213     c                   eval      err_bloccante = 'S'
059100090211     c                   exsr      Msg_errato
059200090211     c                   LEAVE
059300090211     c                   endIF
059400090211      ***
059500060614     c                   endIF
059600060612
059700100708     c                   EXSR      LEGGE_TIVIN_R
059800060612     C                   ENDdo
059900110127      ***
060000110127      ***   Come ultimo dettaglio lo scrive a EoF
060100110127     c                   If           Contatore_Dettagli > 0
060200110127      *
060300110127      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
060400110127     c                   eval      FinDET_RECord = vnrec
060500110127      **?              /*---------------------- */
060600110127     c                   exsr      Scrive_Dettag
060700110127      **?              /*---------------------- */
060800110127      *
060900110127     c                   end
061000060612      **
061100060612     c                   endSR
061200100709      * ?------------------------------------------------------------------ */
061300100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
061400100709      * ?------------------------------------------------------------------ */
061500100709     C     MSG_Errato    BEGSR
061600100709      *
061700100709      * ?  Scrive su tutti i records il tipo di errore
061800100709     c     *start        setll     tivin00r
061900100709     c
062000100709     c                   EXSR      LEGGE_TIVIN_R
062100100709     c                   dow       EoFTivin <> 'S'
062200100709     c
062300100709     c                   if        vinMSG  = *blank
062400100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
062500100709     C                               >> Controllare i DATI su UPLOAD !!'
062600100709     c                   end
062700100709     C                   eval      vinFLG = '2'
062800100709     c                   eval      Almeno_Un_Due ='S'
062900100709     c                   eval      esito  = '2'
063000100709     c                   eval      se_errore ='S'
063100100709     c                   eval      err_bloccante ='S'
063200100709      *
063300100709     c                   update    tivin000
063400100709     c                   EXSR      LEGGE_TIVIN_R
063500100709     c                   endDO
063600100709      *
063700100709     C                   ENDSR
063800100709      * ?------------------------------------------------------------------ */
063900100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
064000100709      * ?------------------------------------------------------------------ */
064100100709     C     LEGGE_TIVIN_R BEGSR
064200100709      *
064300100709     c                   clear                   EoFTivin
064400100709      *  Lettura sequenziale
064500100709     c                   read      Tivin00r
064600100709
064700100709     c                   if        %Eof(tivin00r)
064800100709     c                   move      'S'           EoFTivin
064900100709     c                   else
065000100709      *
065100100709      * ?  imposta il tipo di segmento:
065200100709     c                   clear                   dati
065300100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
065400100728     c                   eval      segmento = %trimL(dati)
065500100728     c                   eval      tipo_seg = %subst(segmento:1:3)
065600100709      **
065700100709     c                   end
065800100709      *
065900100709     C                   ENDSR
066000100120      * ?------------------------------------------------------------------ */
066100100120      *?    Decodifica il segmento ed importa i campi in formato DS
066200100120      * ?------------------------------------------------------------------ */
066300100120     c     Decod_SegmentoBegsr
066400100120      **
066500100706      *** ------------------------------------
066600110117????  *** A FINE CICLO DI UN DETTAGLIO:   -->  e alla fine del file EoF
066700100706      *** ------------------------------------
066800110117      * OGNI DETTAGLIO inizia dal segmento UNH fino a quello successivo.
066900100706      *  Quindi ad ogni nuovo dettaglio e per fine messaggio scrive il VAB.
067000100706      **
067100100706      **  Scrive alla fine di ogni giro di dettaglio:
067200110127     c                   If           tipo_seg = Segm_Testata_dettagli  and
067300100706     c                                Contatore_Dettagli > 0
067400110117      *
067500100708      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
067600100708     c                   eval      FinDET_RECord = vnrec
067700100708      **?              /*---------------------- */
067800100708     c                   exsr      Scrive_Dettag
067900100708      **?              /*---------------------- */
068000100812      *
068100100812      *   Ogni nuovo Dettaglio,
068200100812      *    per quei Partner che gestiscono più LInee
068300100812      *    Mediante il Mittente del dettaglio
068400100812     c                   if        UNB_parziale ='S' and
068500100812     c                              PT_con_piu_Nazioni = 'S' and
068600100812     C                                 Naz_salvata = Naz_mittente
068700100812      * ?ReImposta i dati da tabella PT
068800100812      * ?  se fossero stati cambiati dal precedente dettaglio:
068900100812     C                   z-add     savXX_PT_key  Nr_PT
069000100812     c                   exsr      Se_Trovata_PT
069100100812     c                   endIF
069200100812      *
069300100812     c                   ENDif
069400100708      *
069500100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
069600100708      **?              /*---------------------- */
069700100708     c                   exsr      Pulizia_Campi
069800100708      **?              /*---------------------- */
069900100812      *
070000100706      *** ------------------------------------
070100100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
070200100120      * ?  Anche se non utilizzati.
070300100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
070400100317     c                   clear                   seg_imprevisto
070500100720     c                   clear                   trovato_seg
070600100319      *
070700110117      **?   Il RECORD del TIVIN è già decodificato come FLAT FILE non occorre splittarlo.
070800110117      **?   ma va semplicemente incasellato nella propria DS per tipo record (SEGMENTO).
070900110117     C                   eval      DsGenerica = Segmento
071000100811      *
071100100319      *  Contatore delle righe ossia dei segmenti del messaggio
071200100319     c                   add       1             Conta_segmenti
071300100708      *
071400100708      *** ------------------------------------
071500100708      *   Decodifica il singolo segmento LETTO
071600100708      *** ------------------------------------
071700100120     c                   select
071800100720      **--------
071900100120     c                   when      tipo_seg = 'UNB'
072000110117     c                   movel(p)  DsGenerica    eds10UNB
072100100120     C                   EXSR      DATI_UNB
072200100120      * --------
072300100120     c                   when      tipo_seg = 'UNH'
072400110117     c                   movel(p)  DsGenerica    eds10UNH
072500100120     C                   EXSR      DATI_UNH
072600110117     c                   clear                   segmento_end
072700100120      * --------
072800100120     c                   when      tipo_seg = 'BGM'
072900110117     c                   movel(p)  DsGenerica    eds10BGM
073000100316     C                   EXSR      DATI_BGM
073100100811      **--------
073200100811     c                   when      tipo_seg = 'TDT'
073300110117     c                   movel(p)  DsGenerica    eds10TDT
073400100811     C                   EXSR      DATI_TDT
073500100811      * --------
073600100811     c                   when      tipo_seg = 'TSR'
073700110117     c                   movel(p)  DsGenerica    eds10TSR
073800100811     C                   EXSR      DATI_TSR
073900100811      **--------
074000100811     c                   when      tipo_seg = 'NAD'
074100110117     c                   movel(p)  DsGenerica    eds10NAD
074200100811     C                   EXSR      DATI_NAD
074300100811      **--------
074400100811     c                   when      tipo_seg = 'CNI'
074500110117     c                   movel(p)  DsGenerica    eds10CNI
074600100811     C                   EXSR      DATI_CNI
074700100811      **--------
074800100811     c                   when      tipo_seg = 'DTM'
074900110117     c                   movel(p)  DsGenerica    eds10DTM
075000100811     C                   EXSR      DATI_DTM
075100100811      **--------
075200100811     c                   when      tipo_seg = 'TOD'
075300110117     c                   movel(p)  DsGenerica    eds10TOD
075400100811     C                   EXSR      DATI_TOD
075500100811      **--------
075600100811     c                   when      tipo_seg = 'GID'
075700110117     c                   movel(p)  DsGenerica    eds10GID
075800100811     C                   EXSR      DATI_GID
075900100811      **--------
076000100811     c                   when      tipo_seg = 'MEA'
076100110117     c                   movel(p)  DsGenerica    eds10MEA
076200100811     C                   EXSR      DATI_MEA
076300100811      **--------
076400100811     c                   when      tipo_seg = 'RFF'
076500110117     c                   movel(p)  DsGenerica    eds10RFF
076600100811     C                   EXSR      DATI_RFF
076700100811      **--------
076800100811     c                   when      tipo_seg = 'PCI'
076900110117     c                   movel(p)  DsGenerica    eds10PCI
077000100811     C                   EXSR      DATI_PCI
077100100811      **--------
077200100811     c                   when      tipo_seg = 'GIN'
077300110117     c                   movel(p)  DsGenerica    eds10GIN
077400100811     C                   EXSR      DATI_GIN
077500110117     c                   move      'S'           segmento_end
077600100811      **--------
077700100811     c                   when      tipo_seg = 'DOC'
077800110117     c                   movel(p)  DsGenerica    eds10DOC
077900100811     C                   EXSR      DATI_DOC
078000101222      **--------
078100110126     c                   when      tipo_seg = 'MOA'
078200110126     c******             movel(p)  DsGenerica    eds10MOA
078300110126     C                   EXSR      DATI_MOA
078400110126      **--------
078500110126     c                   when      tipo_seg = 'FTX'
078600110126     c******             movel(p)  DsGenerica    eds10FTX
078700110126     C                   EXSR      DATI_FTX
078800110126      **--------
078900110117     c                   When       tipo_seg = *blank and segmento = *blank
079000110117      **--------
079100110117     c                   when       tipo_seg <> *blank
079200110117     c                   move      'S'           seg_imprevisto
079300110117     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
079400110117     c                   exsr      Error_DET
079500110117     c                   goto      end_Decod
079600100120      **--------
079700100120     c                   OTHER
079800100120      **--------
079900110117     c                   if         segmento_end <> 'S'
080000100120     c                   move      'S'           seg_imprevisto
080100100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
080200100120     c                   exsr      Error_DET
080300100120     c                   goto      end_Decod
080400110110     c                   end
080500100120      **--------
080600100120     c                   endSL
080700100120      **
080800100120     c     end_Decod     endSR
080900100701      * ?================================================================== */
081000100319     C*? Azzera_MSG   - Azzera dati messaggio
081100100701      * ?================================================================== */
081200100708     C     Scrive_Dettag BEGSR
081300100701      **
081400100708      *  Deve flaggare il dettaglio con dei problemi
081500100708      * Imposta le righe in errore sullo specifico dettaglio
081600100708     C                   if        Err_in_detta = 'S'
081700110117      *
081800100708      * prima di rieseguire il ciclo
081900100708      *  rilascia il record
082000110202     c                   update    tivin000                             99
082100110117      *
082200100708      * ?  Scrive su tutti i records il tipo di errore
082300100708     c     IniDET_RECord setll     tivin00r
082400100708      **?              /*---------------------- */
082500100708     c                   exsr      DETta_Errato
082600100708      **?              /*---------------------- */
082700100708     c                   end
082800100708      *
082900100708      **  Se presente un errore nel record emette una segnalazione msg
083000100708???? c                   if        se_errore <> 'S'
083100100708      *  se è arrivato in fondo al dettaglio scrive VAB e VAT
083200100708     c                   if         NAD_Destinatario_in_Dettaglio  ='S'  or
083300100708     c                              NAD_Destinatario_in_Testata    ='S'
083400100708      * ?              /*---------------------- */
083500100708     c                   exsr      Scrive_VAB
083600100708      * ?              /*---------------------- */
083700100708     c                   else
083800100708     c                   eval      errori_in_Wrt = 'S'
083900100708     c                   end
084000100708      *
084100100708      *  Problemi nella decodifica dei campi VAB/VAT
084200100708     c                   if        errori_in_Wrt = 'S'
084300100921     C                   evalR     vinMSG = 'EDI-Problemi scrittura VAB'
084400100708     c                   exsr      Error_DET
084500100708     c                   ROLBK
084600100708     c                   else
084700100708     c                   COMMIT
084800100708     c                   end
084900100708      *
085000100708     c                   end
085100100708      **
085200100708     c     end_WRidett   endSR
085300110117      * ?------------------------------------------------------------------ */
085400110117      *?    Flagga il TIVIN come messaggio completamente ERRATO
085500110117      * ?------------------------------------------------------------------ */
085600110117     C     DETta_Errato  BEGSR
085700110117      *
085800110117     c                   EXSR      LEGGE_TIVIN_R
085900110117     c                   dow       EoFTivin <> 'S'
086000110117      *
086100110117      * Se ha letto il successivo record
086200110117      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
086300110117     c                   if        VNREC = finDET_RECord
086400110117     c                   leave
086500110117     c                   end
086600110117      *
086700110117     c                   exsr      Error_DET
086800110117      *
086900110117     c                   if        vinMSG  = *blank
087000110117     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
087100110117     C                             >> Controllare i DATI di ogni Segmento <<'
087200110117     c                   end
087300110117      *
087400110117     c                   update    tivin000
087500110117     c                   EXSR      LEGGE_TIVIN_R
087600110117     c                   endDO
087700110117      * ------
087800110117???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
087900110117     C                   EVAL      Messaggio = 'EDI -
088000110117???? C                             IFCSUM D96A  non totalmente tradotto. -
088100110117     C                             Controllare UPLOAD del cliente:'
088200110117     C                   EVAL      Messaggio = %trim(Messaggio) +
088300110117     C                                         %editc(§PTksc:'X')
088400110117     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
088500110117     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
088600110117     c                   if        §PVinv <> 'N'
088700110117     c                   exsr      invio_mail
088800110117     c                   end
088900110117      *
089000110117      * ripulisce per un nuovo giro
089100110117     C                   clear                   Err_in_detta
089200110117      *
089300110117     C                   ENDSR
089400100708      * ?================================================================== */
089500100708     C*? Pulizia Campi -  3 livelli di messaggio TestaMSG/TestaDET/Dettaglio
089600100708      * ?================================================================== */
089700100708     C     Pulizia_Campi BEGSR
089800100708      **
089900100708      * inzio messaggio azzera tutte le variabili generali del messaggio
090000100708     c                   If        tipo_seg = Segm_Inizio_Messaggio
090100100708      * ?                         --------------
090200100708     C                   EXSR      Azzera_MSG
090300100708      * ?                         --------------
090400100708      *
090500100708      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
090600100708     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
090700100708      * ?                         --------------
090800100813     c                   exsr      Testata_msg
090900100708      * ?                         --------------
091000100708      *
091100100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
091200100708      *  Prepara una nuova spedizione
091300100708     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
091400100708      * ?                         --------------
091500100708     c                   exsr      Nuova_spediz
091600100708      * ?                         --------------
091700100708     c                   end
091800100708      **
091900100708     c     end_clear     endSR
092000100708      * ?================================================================== */
092100100708     C*? Azzera_MSG   - Azzera dati messaggio
092200100708      * ?================================================================== */
092300100708     C     Azzera_MSG    BEGSR
092400100708      **
092500100701     C*  Inizializza variabili
092600100701     C                   Z-ADD     0             skCLienti
092700100701     C                   Z-ADD     0             sk
092800100701     C*
092900100701      *  INIZIALIZZA  dati fondamentali messaggio
093000100701     C                   clear                   EDIDSPT
093100100701     C                   clear                   EDIDSPU
093200100701     C                   clear                   EDIDSPV
093300100701     C*
093400100701     C                   move      'N'           PT_trovata
093500100812     C                   clear                   savXX_PT_key
093600100701     C                   clear                   PT_vabLNP
093700100701     C                   clear                   PT_vabNRS
093800100701     C                   clear                   PT_vabCTM
093900100701     C                   clear                   PT_vabCCM
094000100701     C                   clear                   PU_HLD_SpedVAX
094100100812     C                   eval        PT_con_piu_Nazioni = *blank
094200100812     c                   move      *blank        Naz_salvata       3
094300090211     C*
094400100701     c                   clear                   UNB_Generico
094500100701     c                   clear                   UNB_Parz
094600100701     c                   clear                   UNB_NrTrasmiss
094700100701     c                   clear                   UNZ_NrTrasmiss
094800100708      *
094900100708     c                   clear                   NAD_SF_x_CMR
095000100630      *
095100100630      * per controllare se almeno un record è stato importato sul VAB
095200100630     c                   clear                   Almeno_Uno        1
095300100630     c                   clear                   Almeno_Un_Due     1
095400100708     c                   clear                   IniDET_RECord
095500100708     c                   clear                   FinDET_RECord
095600100630     c                   eval      esito  = '0'
095700100728     c                   eval      Segmento_in_errore = 0
095800110127      *   Contatore dei dettagli
095900110127     c                   eval      Contatore_Dettagli = 0
096000100317     C*
096100090211     C                   ENDSR
096200100701      * ?================================================================== */
096300100701     C*? Testata delle spedizioni  Inizilizza i campi di testata
096400100701      * ?================================================================== */
096500100813     C     Testata_msg   BEGSR
096600100701      *
096700100701      *  imposta il Flag per identificare la testata
096800100701     c                   eval      Dati_di_Testata  ='S'
096900100701     C*
097000100701      **
097100100701      *   Codici identificativi della Testata del messaggio
097200100701     c                   clear                   UNH_Rifer_Msg
097300100706     C* Pulisco dati di work x foglio viaggio
097400100706     C                   clear                   BGM_nrCMR                      * WNRCMR
097500100706     C                   clear                   BGM_NrFviaggio                 * WNUMFV
097600100701     c                   clear                   BGM_Id_Testata
097700100701      **
097800100701     C*  Riferimento Foglio Viaggio o CMR
097900100701     C                   MOVEL     *BLANKS       RFF_NrFviaggio                 * WNUMFV
098000100701     C                   MOVEL     *BLANKS       RFF_NrCMR                      * WNRCMR
098100100701      **   Date generali
098200100701     C                   Z-ADD     0             DTM_DtParten                   * WDATPA
098300100701     C                   Z-ADD     0             DTM_DtFViag                    * WDATFV
098400100701     C                   Z-ADD     0             DTM_OraFViag                   * WHHNFV
098500100701     C                   Z-ADD     0             DTM_DtCMR                      * WDTCMR
098600100701     C                   Z-ADD     0             DTM_OraCMR                     * WHHCMR
098700100702     C                   Z-ADD     0             DTM_DtArrivo                   *
098800100702     C                   Z-ADD     0             DTM_OraArrivo                  *
098900100811     C                   Z-ADD     0             DTM_DtConsRic                  * WvabDCR
099000100811     C                   Z-ADD     0             DTM_OraCnsRic                  * WvabHCR
099100100701      *
099200100701      * pulisce gli RFF+FF di testata
099300100706     C                   clear                   Tes_RFF_FF_KSC
099400100706     C                   clear                   Tes_RFF_FF_TAR
099500100706     C                   clear                   Tes_RFF_FF_LNP
099600100706     C                   clear                   Tes_RFF_FF_NRS
099700100706     C                   clear                   Tes_RFF_FF_CTM
099800100706     C                   clear                   Tes_RFF_FF_BS1
099900100706     C                   clear                   Tes_RFF_FF_nsp                 *blk/S
100000100701     C*
100100100701     c                   clear                   tes_VABrmo
100200100701     c                   clear                   tes_VABnmo
100300100701     c                   clear                   tes_VABcmo
100400100701     C*
100500100813     c                   clear                   tes_VABtsp
100600100813     c                   clear                   tes_VABtc1
100700100813     c                   clear                   tes_VABnot
100800100813     C*
100900100701     c                   clear                   tes_VABrsd
101000100701     c                   clear                   tes_VABrd2
101100100701     c                   clear                   tes_VABind
101200100701     c                   clear                   tes_VABlod
101300100701     c                   clear                   tes_VABnzd
101400100701     C*
101500100701     c                   clear                   tes_Valuta
101600100813     C*
101700100813     c                   clear                   AddTot_Colli     16 0
101800100701     C*
101900100701     ** Inizializza campo di work
102000100701     c                   eval      GID_aTotale = 'N'
102100100701     C*
102200100701     C*  Pulisce il MITTENTE e il DESTINATARIO se nel messaggio
102300100701     C*   Non sono definiti a Dettaglio ma in TESTATA (una volta x tutte)
102400100701     c                   eval      NAD_destinatario_in_testata = *blank
102500100701     c                   eval      NAD_mittente_in_testata = *blank
102600100701     c                   eval      CTA_destinatario_in_testata = *blank
102700100701     c                   eval      COM_telefono_in_testata = *blank
102800100701     C*
102900100701      *   Inizia il conteggio delle righe
103000100706     c                   z-add     0             Conta_segmenti    5 0
103100100701     C*
103200100701     C                   ENDSR
103300100701      * ?================================================================== */
103400100701     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
103500100701      * ?================================================================== */
103600100701     C     Nuova_spediz  BEGSR
103700100812      **
103800100701     C* Da qui inzia il dettaglio perciò lo memorizzo
103900100701      *  imposta il Flag per identificare la testata
104000100701     c                   eval      Dati_di_Testata  ='N'
104100100708      *
104200110117     c                   z-add     vnrec         IniDET_RECord
104300100812      **
104400100812      **  Se si tratta di un Partner che gestisce più Linee x più Nazioni
104500100812      **   controllo che ogni dettaglio si riferisca alla tab.PT corretta.
104600100812      *
104700100812     c                   if        UNB_parziale ='S' and
104800100812     c                              PT_con_piu_Nazioni = 'S'
104900100812      * ?Cerca dalla riga di questo dettaglio in avanti
105000100812      * ?           *------------------------------*
105100100812     c                   exsr      cerca_Mittente
105200100812     c                   exsr      esito_tab_PT
105300100812      * ?           *------------------------------*
105400100812     c                   endIF
105500100812      **
105600100812      *
105700100701      *  Pulizia dei records
105800100701     C                   CLEAR                   EDiVAB00
105900100701     C                   CLEAR                   EDiVAT00
106000100701     C                   CLEAR                   EDiVAD00
106100100701      **
106200100813      **   Campi da testata messaggio
106300100706     C                   z-add     PT_vabLNP     VABLNP
106400100706     C                   z-add     PT_vabNRS     VABNRS
106500100701     C                   MOVEL     PT_vabCTM     VABCTM
106600100701     C                   z-add     PT_vabCCM     VABCCM
106700100813     C                   movel     tes_VABtsp    VABTSP
106800100813     C                   movel     tes_VABtc1    VABTC1
106900100813     C                   movel     tes_VABnot    VABnot
107000100701     C*
107100100701     C                   MOVEL     UNH_vabRMA    VABrma
107200100701     C                   z-add     UNH_vabRMN    VABrmn
107300100701      *
107400100701      *  Dettaglio dei segnacolli Bartolini/EEX
107500100701     c                   eval      Squadratura_Colli = 'N'                      Squad.nr.colli
107600100701     c                   eval      GID_aTotale = 'N'
107700100701      *
107800100701     C                   clear                   Conta_Segn        5 0
107900100701     C                   clear                   totale_PCI        5 0
108000100701     C                   clear                   RFF_RifSpediz
108100100701      *
108200100701     C                   MOVEA     *HIVAL        segn_BAR
108300100701     C                   MOVEA     *blank        segn_EEX
108400100701      *
108500100701     C*  Azzero codice cliente - tariffa
108600100701     C                   Z-ADD     0             ToT_Colli        16 0
108700100701     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
108800100701     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
108900100701      *
109000100701     C*  Azzero codice cliente - tariffa
109100100701     C                   clear                   Det_RFF_FF_ksc
109200100701     C                   clear                   Det_RFF_FF_tar
109300100701     C                   clear                   Det_RFF_FF_lnp
109400100701     C                   clear                   Det_RFF_FF_nrs
109500100701     C                   clear                   Det_RFF_FF_ctm
109600100701     C                   clear                   Det_RFF_FF_bs1
109700100701     C                   clear                   Det_RFF_FF_nsp                 *blk/S
109800100701      *
109900100701     C                   clear                   Note_1           35
110000100701     C                   clear                   Note_2           35
110100100701     C*  Campi di work
110200100701     c                   clear                   VABtic_work
110300100701     c                   clear                   wri_vabtic        1
110400100701      *
110500100701     c                   eval      NAD_Destinatario_in_Dettaglio = *blank
110600100701     c                   eval      NAD_mittente_in_Dettaglio = *blank
110700100701     c                   eval      CTA_destinatario_in_Dettaglio = *blank
110800100701     c                   eval      COM_telefono_in_Dettaglio = *blank
110900100702     c                   eval      CNI_Identificativo_Bolla = *blank
111000100701      *
111100100701     C                   clear                   Rifer_Alfanum    35
111200100910     C                   clear                   Secondo_Rifer    35
111300110224     C                   clear                   Rifer_Numeric    35
111400100701     C                   clear                   Tipo_Rifer        3
111500100701      *
111600100701      * Un errore nel dettaglio
111700100701     C                   clear                   Err_in_detta      1
111800100701     c                   clear                   errori_in_Wrt     1
111900100701      *
112000100910     c                   clear                   Primo_RFFcollo    1
112100100910     c                   clear                   Primo_RCUcollo    1
112200100910     c                   z-add     0             conta_rif         1 0
112300100910     c                   clear                   suVABrmo
112400100910      *
112500100701      * Imposta il flag dei Dati di Testata a NO poichè inzia il Dettaglio
112600100701     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
112700100701     C*
112800100812     C* ---------------------------------------------------
112900100812     C* Inizializzazione Spedizione con campi da testata :
113000100812     C* ---------------------------------------------------
113100100811     C*  Data Richiesta Consegna  da Testata se c'è;
113200100811     C                   IF        DTM_DtConsRic > 0                            Data cons.rich. test
113300100811     C                   z-add     DTM_DtConsRic VABDCR                         Data cons.rich.
113400100811     C                   z-add     DTM_OraCnsRic VABHCR                         Ora  cons.rich.
113500100811     c                   end
113600100812     C*
113700100812     c                   endSR
113800100812     C*----------------------------------------------------------------
113900100812      *? cerca Nazione del primo Mittente nella sequenza dettagli
114000100812     C*----------------------------------------------------------------
114100100812     C     cerca_MittenteBEGSR
114200100812      *
114300100812     c                   z-add     vnrec         rec_posiz         9 0
114400100812     c                   move      *blank        Naz_mittente      3
114500100812     c                   movel(p)  UNB_Generico  PT_Parziale      35
114600100812     c                   move      'N'           OK_Nazione        1
114700110118      ********
114800110118      ********  Per il Cliente NON deve eseguire la ricerca
114900110118      ********
115000100812      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
115100110118     c********           call      'TRTCT96R1'
115200110118     c********           parm                    rec_posiz
115300110118     c********           parm                    PT_Parziale
115400110118     c********           parm                    Naz_mittente
115500110118     c********           parm                    Ok_Nazione
115600110118     c********           parm                    User_msg
115700110118     C********           parm                    keyUNBCLI
115800110118     C********           parm      'IFCSUM'      keyTIPOMSG
115900110118     C********           parm      'D'           keyVERSION
116000110118     C********           parm      '96A'         keyRELEASE
116100110118     C********           parm      'UN'          keyAGENCY
116200110118     C********           parm                    keyASSOCIA
116300100812      *
116400100812      *  Re-inizializza l'indicatore di trovato
116500100812     c                   setoff                                       32
116600100812      *
116700100812      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
116800100812      *   da decodificare come UNB.
116900100812     c                   if        Ok_Nazione = 'S'
117000100812     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
117100100812     C                   eval      Nr_PT = 1
117200100812     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
117300100812     c                   Endif
117400100812      *
117500100812     c                   endSR
117600100812     C*----------------------------------------------------------------
117700100812      *? esito ricerca tabella PT decodificata oppure NO
117800100812     C*----------------------------------------------------------------
117900100812     C     esito_tab_PT  BEGSR
118000100812      *
118100100812     C                   move      'N'           PT_trovata
118200100812     C   32              move      'S'           PT_trovata
118300100812      *
118400100812      * ******  Errore generale sul Messaggio
118500100812      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
118600100812      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
118700100812     C                   If        PT_trovata = 'N'
118800100812      * Msg di allerta Traduzione
118900100812     C                   EVAL      Indirizzi = CED_Bart
119000100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM'
119100100921     C                   EVAL      Messaggio = 'EDI -
119200100921     C                             IFCSUM D96A - msg su UPLOAD -
119300100921     C                             con UNB non presente in tabella PT..'
119400100812      *
119500100812     c                   exsr      invio_mail
119600100812     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
119700100812     C                   eval      vinFLG = '2'
119800100812     C                   eval      se_errore   = 'S'
119900100812      *
120000100812      *             *------------------------------*
120100100812     C                   eLSe
120200100812      *             *------------------------------*
120300100812      *
120400100812     c                   exsr      Se_Trovata_PT
120500100812      *
120600100812      *
120700100812     c                   Endif
120800100812      *
120900100701     c                   endSR
121000090210     C*----------------------------------------------------------------
121100090210      *? DECODIFICA DATI UNB
121200090210     C*----------------------------------------------------------------
121300100812     C     Se_Trovata_PT BEGSR
121400090210      *
121500100812      * Se tutto OK su tab.PT:
121600100812      *
121700100812     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
121800100812     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
121900100812     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
122000100812      *
122100100812      *  Campi impostati come default:
122200100812      *  Se ho AGE/ON.... prendo i riferimenti definito su tab.PT il tipo di
122300100812      *  qualificatore da utilizzare come facciamo x RFF+AGE con EEX.
122400100812      *  Per default se non impostato diventa automaticamente un AGE.
122500100812     c                   if        §PVRFF = *blank
122600100909     c                   eval      §PVRFF = Riferimento_Spedizione
122700100909     c                   end
122800100813     c                   eval      Riferimento_Spedizione = §PVRFF
122900100910     c                   eval      RFF_DalPrimoCollo      = §PVRFS
123000100910     c                   eval      RFF_DopoIlPrimoCollo   = §PVRSE
123100100909      *
123200100812      *  Anche per il numerico prendo come default "CU"
123300100812     c                   if        §PVRCU = *blank
123400100909     c                   eval      §PVRCU = Riferimento_Numerico
123500100909     c                   end
123600100813     c                   eval      Riferimento_Numerico   = §PVRCU
123700100812      *
123800100812      *  Default per i campi Free Text nel dettaglio 'SIN' e 'ICN'
123900100812     c                   if        §PVsin = *blank
124000100812     c                   eval      §PVsin = 'SIN'
124100100812     c                   end
124200100812      *
124300100812     c                   if        §PVicn = *blank
124400100812     c                   eval      §PVicn = 'ICN'
124500100812     c                   end
124600100812      *
124700100812      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
124800100812      *  ricevuti:
124900100812     c                   movel     §PVema        mail_ind         44
125000100812      *
125100100812      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
125200100812     c                   if        mail_ind <> *blank
125300100812     c                   exsr      imp_ADDR_mail
125400100812     c                   end
125500100812      *
125600100812      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
125700100812      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
125800100812      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
125900100812     C                   if        §PULUN = *blank
126000100812     C                   eval      §PULUN = '00'
126100100812     c                   end
126200100812      *
126300100812      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
126400100812      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
126500100812     C                   move      §puLUN        PU_lunVAT
126600100812     C                   move      §puTS         PU_TipoServ
126700100812     C                   move      §puSS         PU_SpecServ
126800100812      *
126900100812     C                   Z-ADD     §PTKSC        PT_vabCCM
127000100812     C                   z-add     §PTLNP        PT_vabLNP
127100100812     C                   z-add     §PTNRS        PT_vabNRS
127200100812     C                   MOVEL     §PTCTM        PT_vabCTM
127300100812      *
127400100812      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
127500100812      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
127600100812      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
127700100812      *  i records trasmessi).
127800100812     C                   MOVEL     §puNSP        PU_HLD_SpedVAX                 *blk/S
127900100812      *
128000100812      *  In base al cod.trattamento merce reperisco tipo tratt.
128100100812     C                   CLEAR                   DS1B
128200100812     C                   Z-ADD     1             Y                 3 0
128300100812     C     PT_vabCTM     LOOKUP    Cod_Tb_CTM(Y)                          32
128400100812     C     *IN32         IFEQ      '1'
128500100812     C                   MOVEL     Des_Tb_CTM(Y) DS1B
128600100812     C                   MOVEL     DS1B          sav_DS1B
128700100812     C                   END
128800100812      *
128900100812      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
129000100812     C                   clear                   ACORAG
129100100812     C                   clear                   INDCAE
129200100812     C                   clear                   INDCAP
129300100812     C                   clear                   INDSTA
129400100812      * Decodifico partner
129500100812     C                   Z-ADD     1             KKUT
129600100812     C                   Z-ADD     KCI           KKCC
129700100812     C                   MOVE      §PTKSC        KKSC
129800100812     C     KACO          CHAIN     CNACO00F                           31
129900100812     C     KIND          CHAIN     CNIND00F                           31
130000100812      *
130100100812      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
130200100812      *   questa schiera è utilizzata anche per i clienti.
130300100812     C     §ptKSC        LOOKUP    skCLienti                              39
130400100812     c                   If        *in39 = *off
130500100812     C                   add       1             sk
130600100812     C                   z-add     §ptKSC        skCLienti(sk)
130700100812     C                   Endif
130800100812      *
130900100812     c                   endSR
131000100812     C*----------------------------------------------------------------
131100100812      *? DECODIFICA DATI UNB
131200100812     C*----------------------------------------------------------------
131300100812     C     DATI_UNB      BEGSR
131400100812      *
131500100701      * identificativo Messaggio
131600100812     C                   MOVEL     unb0017       Anno2             2 0
131700100812     C                   z-add     2000          Anno4             4 0
131800100812     C                   add       Anno2         Anno4
131900100812     C                   MOVE      unb0017       WGG               2 0
132000100812     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
132100100812     C                   MOVE      Anno2         Data_messag
132200100812     C                   MOVEL     WGG           Data_messag
132300100812     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
132400100812     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
132500100812     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
132600100812      *
132700100812     c                   clear                   Mittente         35
132800100812     c                   clear                   Mitt_Qualifier    4
132900100812     c                   clear                   Id_Messaggio     14
133000110127      *  MITTENTE non presente sul messaggio quindi chiodato per trovare la tabella PT
133100110127      *
133200110127     C                   eval      unb0004        = 'CAMAIEU'
133300110127     C                   eval      unb000720      = '22'
133400100701      *
133500110127     C                   eval      Mittente       = unb0004
133600110127     C                   eval      Mitt_qualifier = unb000720
133700110127      *
133800090209      ** ---------------------------------------------------------------
133900090209      * quindi 1° cosa:
134000090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
134100090209      *     senza questo il messaggio NON è trascodificabile
134200100630     C                   MOVEL     unb0004       UNB_diWork
134300100630     C                   MOVE      unb000720     UNB_diWork
134400100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
134500100630     C                   Z-ADD     1             Nr_PT
134600100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
134700090209      *
134800100317      * Poi occorre verificare se il codice UNB è fra quelli parzializzati con
134900100317      *  la Nazione:
135000100317     C                   If        *IN32 = *off
135100100630     C                   eval      Nr_PT = 1
135200100630     c                   clear                   UNB_diWork
135300100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
135400100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
135500100317      *
135600100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
135700100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
135800100317      *  per cercare di individuare con certezza il codice della tabella PT in base
135900100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
136000100317     C                   If        *IN32 = *on
136100100317      *
136200100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
136300100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
136400100317      * ?  su Partner che hanno + linee con + nazioni.
136500100812      *  se è considerato Parziale
136600100812      *   Salva il codice generico
136700100630     c                   eval      UNB_parziale = 'S'
136800100812     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
136900100812      *             *------------------------------*
137000100812     c                   exsr      cerca_Mittente
137100100812      *             *------------------------------*
137200100812     C                   eval          Naz_salvata = Naz_mittente
137300100317     c                   Endif
137400100317     c                   Endif
137500100812      *
137600100812      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
137700100812      *             *------------------------------*
137800100812     c                   exsr      esito_tab_PT
137900100812      *             *------------------------------*
138000100920      **
138100100920     C                   if        se_errore   = 'S'
138200100920     c                   goto      end_UNB
138300100920     c                   end
138400100812      **
138500100812      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
138600100812     C                   If        PT_trovata = 'S'
138700100812      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
138800100812      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
138900100812      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
139000100812      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
139100100812      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
139200100812     c                   movel     UNB_diWork    UNB_Parz
139300100812     C                   z-add     Nr_PT         savXX_PT_key
139400100812      * ?- Se ha più LINEE da gestire                                      ------ */
139500100812     C                   eval      PT_con_piu_Nazioni = §puPLN
139600100812     c                   end
139700090209      **
139800090210     c     end_UNB       endSR
139900090227      * ?================================================================== */
140000090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
140100090227      * ?================================================================== */
140200090227     C     DATI_UNH      BEGSR
140300100316      *
140400100701      *  Imposta di Default i dati rilevati dalla testata e dal messaggio:
140500110117     c                   eval      UNH_record = vnREC
140600100701      *
140700090211      ** Riferimento del cliente
140800100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
140900100701      * ?                                       ------------
141000100701     C                   MOVEL     UNH_Rifer_Msg UNH_VABrma
141100100701      * ?                                       ------------
141200100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
141300100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
141400100701     C                   SUB       1             lunghezza_num
141500100701     C                   EXSR      Se_Numerico
141600100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
141700090211      * ?                                       --------
141800100701     C                   MOVEL     Numero_OK     UNH_VABrmn
141900090211      * ?                                       --------
142000090211     C                   END
142100090303      **
142200090210     c     end_UNH       endSR
142300100701      * ?================================================================== */
142400100701      *? dati iniziali   BGM
142500100701      * ?================================================================== */
142600100701     C     DATI_BGM      BEGSR
142700100701      *
142800100701      *  identificativo testata
142900110118     c                   eval       BGM_Id_Testata = BGM1000
143000100706     C                   IF           BGM_Id_Testata <> *blank
143100100706     C                   MOVEL     BGM_Id_TestataBGM_NrFviaggio
143200100706     C                   MOVEL     BGM_Id_TestataBGM_nrCMR
143300100706     c                   end
143400100706      *
143500100701      **
143600100701     c                   endSR
143700100701      * ?================================================================== */
143800100909     C*?  i Riferimenti Spedizione / Clienti particolari testata/dettaglio
143900100701      * ?================================================================== */
144000100701     C     DATI_RFF      BEGSR
144100100701      **
144200100702      * ?   X_Cargo_Manifest o X_Numero_CMR
144300110118      **     imposto x entrambi
144400110118     C                   MOVEL     UNH_Rifer_Msg RFF_NrFviaggio                 * WNUMFV
144500110118     C                   MOVEL     UNH_Rifer_Msg RFF_NrCMR                      * WNRCMR
144600100910     C*
144700100910      *  --------------------------*
144800100910      *  Prende il riferimento se non era stato precedentemente caricato
144900100910      *     o se sulla PT c'è la forzatura a livello segnacollo.
145000100910     c                   if        Primo_RFFcollo = *blank
145100100701      *
145200100909      * ? Riferimento singola spedizione
145300100909     C*------------------
145400100910      * ?Riferimento AlfaNumerico preso dal primo dei segnacolli
145500100909     C*------------------
145600100910     C                   IF             Rifer_Alfanum = *BLANK or
145700100910     C                              RFF_DalPrimoCollo = 'S'
145800100910     C*--------
145900100701     c                   if        RFF1153 = Riferimento_Spedizione  and
146000100701     c                                RFF1154 <> *BLANKS
146100100701     C                   movel(p)  RFF1154       Rifer_Alfanum
146200100701      ** controlla se contiene solo numeri
146300100701     C                   movel     Rifer_Alfanum RFF_RifSpediz
146400100701     C                   movel     Rifer_Alfanum test_num
146500100701     C     ' '           scan      Rifer_Alfanum lunghezza_num     3 0
146600100701     C                   sub       1             lunghezza_num
146700100701     C                   EXSR      Se_Numerico
146800100701     C                   IF         Campo_Numerico = 'S'                          Ctrl numerico
146900100701     C                   movel     Rifer_Alfanum VABRMA
147000100910     c                   eval      Primo_RFFcollo = 'S'
147100100910      *
147200100701     C                   IF         Tipo_Rifer = Riferimento_Spedizione  or
147300100701     C                              Tipo_Rifer = *blanks
147400100701     C                   movel     Numero_OK     VABRMN
147500100701     C                   eval       Tipo_Rifer = Riferimento_Spedizione
147600100701     C                   ENDIF
147700100701     C                   ENDIF
147800100701     C                   ENDIF
147900100910     C                   ENDIF
148000100910      *
148100100910     c                   else
148200100910      *
148300100910      * Se ci sono ulteriori Riferimenti e sono da riportare su VABRMO
148400100910     c                   if        RFF_DalPrimoCollo ='S' and
148500100910     c                             RFF_DopoIlPrimoCollo ='S' and
148600100910     c                             RFF1153 = Riferimento_Spedizione and
148700100910     c                             RFF1154 <> *BLANKS and
148800100910     c                             RFF1154 <> Rifer_Alfanum
148900100910      *
149000100910     c                   eval      conta_rif = conta_rif + 1
149100100910      *
149200100910      *  al max possono essere 2 riferimenti differenti dal principale
149300100910     c                   if        conta_rif <= 2
149400100910     C                   movel     RFF1154       Secondo_Rifer
149500100910      *
149600100910     C                   IF         Secondo_Rifer <> *BLANKS
149700100920     C     ' '           scan      Secondo_Rifer WLUNGH            3 0
149800100910     C                   sub       1             WLUNGH
149900100910      *
150000100910     c                   if        conta_rif = 1
150100100910     c                   clear                   SUvabrmo
150200100910     c                   end
150300100910      *
150400100910     C                   eval      SUvabRMo = %Trim(suVABRMo) + '.' +
150500100910     C                             %Subst(Secondo_Rifer:1:WLUNGH)
150600100910     C                   ENDIF
150700100910     c                   endIF
150800100910      *
150900100910     c                   end
151000100910     c                   endIF
151100100910      *
151200100910     C*----------------------------------------------------------------
151300100910      *  Se ho CU prendo il riferimento numerico
151400100910      *  ECCEZIONE: se ho già impostato FF obbligatorio
151500100910     c                   if        Primo_RCUcollo = *blank
151600100910      *
151700100910     c                   if        Rifer_Numeric = *blank or
151800100910     c                             RFF_DalPrimoCollo = 'S'
151900100909     C*------------------
152000100909      * ?Riferimento Numerico
152100100909     C*------------------
152200100909     C                   if        RFF1153 = Riferimento_Numerico
152300100909     C                              and RFF1154 <> *blanks
152400100909     C                   movel     RFF1154       Rifer_Numeric
152500100909      * Numero Ordine di vendita
152600100909     C                   movel     Rifer_Numeric test_num
152700100909     C     ' '           scan      Rifer_Numeric lunghezza_num
152800100909     C                   sub       1             lunghezza_num
152900100909     C                   EXSR      Se_Numerico
153000100909      *
153100100909     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
153200100910     c                   eval      Primo_RCUcollo = 'S'
153300100909     C                   movel     Numero_OK     VABRMN
153400100909     C                   eval       Tipo_Rifer = Riferimento_Numerico
153500100909     C                   Endif
153600100909      *
153700100909     C                   ENDIF
153800100909      *
153900100910     C                   ENDIF
154000100910      *
154100100910     c                   else
154200100910      *
154300100910      * Se ci sono ulteriori Riferimenti Numerici non vengono considerati
154400100910     c                   if        RFF_DalPrimoCollo ='S' and
154500100910     c                             RFF_DopoIlPrimoCollo ='S' and
154600100910     c                             RFF1153 = Riferimento_Spedizione and
154700100910     c                             RFF1154 <> *BLANKS and
154800100910     c                             RFF1154 <> Rifer_Numeric
154900100910      *    ??????
155000100910     c                   end
155100100910      *
155200100910     C                   EndIF
155300100910      *
155400100910     C*----------------------------------------------------------------
155500100909      * NOn c'è l'identificativo bolla del cliente
155600100909     c                   if        (RFF1153 = Riferimento_Spedizione and
155700100909     c                              RFF1154 = *BLANKS)
155800100909     c                             or
155900100909     c                             (RFF1153 = Riferimento_Numerico and
156000100909     c                              RFF1154 = *BLANKS)
156100100909     C                   eval      vinMSG = 'RFF riferimento assente'
156200100909     c                   exsr      Error_DET
156300100909     c                   goto      end_RFF
156400100909     C                   EndIF
156500100909     C*------------------
156600100909      * ?Codice Cliente di riferimento
156700100909     C*------------------
156800110224     C                   clear                   Rifer_Cliente    35
156900100701     C                   if        RFF1153 = Riferimento_Cliente_particolare
157000100701     C                              and RFF1154 <> *BLANKS
157100100701     C                   movel     RFF1154       Rifer_Cliente
157200100701     c                   END
157300100909      *
157400100701      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
157500100701      *
157600100701     C     Rifer_Cliente IFNE      *BLANKS
157700100701      *
157800100701     C                   eval      XX = 1
157900100701     C                   movel     §PTKSC        Key_CL           35
158000100701     C                   movel     Rifer_Cliente WCOM28           28
158100100701     C                   move      WCOM28        Key_CL
158200100701      *
158300100701     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
158400100813      *
158500100813      * ?Se il Partner è uno di quelli che gestisce + linee quindi ha un UNB parziale
158600100813      * ?potrebbe non agganciare correttamente il cliente quindi occorre decodificare
158700100813      * ?il dato cercandolo anche con gli altri codici KSC del Partner Generico ossia
158800100813      * ?con i primi 31 caratteri dell'UNB.
158900100813     c  N34              if        UNB_parziale ='S' and
159000100813     c                              PT_con_piu_Nazioni = 'S'
159100100813     c                   do        PT_KeyXsav    x
159200100813      *
159300100813     c                   if        PT_Parz(x) = UNB_generico
159400100813     C                   movel     PT_KSC(X)     Key_CL           35
159500100813     C                   movel     Rifer_Cliente WCOM28           28
159600100813     C                   move      WCOM28        Key_CL
159700100813     C                   eval      XX = 1
159800100813     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
159900100813      * se trovato esce immediatamente
160000100813     c   34              leave
160100100813     c                   end
160200100813      *
160300100813     C                   endDO
160400100813     c                   endIF
160500100813      *
160600100813      *   Se trovato vale x tutto il messaggio
160700100701     C     *IN34         IFEQ      '1'
160800100701     C                   movel     Des_Tb_CL(XX) EDIDSCL
160900100813      *
161000100701      * in testata messaggio
161100100701     c                   if        Dati_di_Testata  ='S'
161200100701     C                   z-add     §CLksc        Tes_RFF_FF_ksc    7 0
161300100701     C                   movel     §CLtar        Tes_RFF_FF_tar    3
161400100701     C                   z-add     §CLlnp        Tes_RFF_FF_lnp    3 0
161500100701     C                   z-add     §CLNRS        Tes_RFF_FF_nrs    2 0
161600100701     C                   movel     §CLCTM        Tes_RFF_FF_ctm    2
161700100701     C                   movel     §CLBS1        Tes_RFF_FF_bs1    1
161800100701     C                   movel     §CLnsp        Tes_RFF_FF_nsp    1            *blk/S
161900100701     c                   else
162000100701      * in dettaglio Messaggio
162100100701     C                   z-add     §CLksc        Det_RFF_FF_ksc    7 0
162200100701     C                   movel     §CLtar        Det_RFF_FF_tar    3
162300100701     C                   z-add     §CLlnp        Det_RFF_FF_lnp    3 0
162400100701     C                   z-add     §CLNRS        Det_RFF_FF_nrs    2 0
162500100701     C                   movel     §CLCTM        Det_RFF_FF_ctm    2
162600100701     C                   movel     §CLBS1        Det_RFF_FF_bs1    1
162700100701     C                   movel     §CLnsp        Det_RFF_FF_nsp    1            *blk/S
162800100701     c                   end
162900100701      *
163000100701      * imposta la LNP  e la Serie x il dettaglio specifico
163100100702     c
163200100701      *  In base al cod.trattamento merce reperisco tipo tratt.
163300100701      *    per un dettaglio specifico.
163400100701     C                   CLEAR                   DS1B
163500100701     C                   Z-ADD     1             Y                 3 0
163600100706     C     §CLctm        LOOKUP    Cod_Tb_CTM(Y)                          32
163700100701     C     *IN32         IFEQ      '1'
163800100701     C                   eval       DS1B = Des_Tb_CTM(Y)
163900100701     C                   END
164000100701      *
164100100701      * Se il cliente non era sulla schiera lo aggiunge
164200100701     C     §CLksc        lookup    skCLienti                              39
164300100701     C                   if        *in39 = *off
164400100701     C                   add       1             sk
164500100701     C                   z-add     §CLksc        skCLienti(sk)
164600100701     c                   endif
164700100701      *
164800100701     C                   ENDIF
164900100701     C                   ENDIF
165000100702      *
165100100702      * in Dettaglio  preimposto i campi rilevati dalle tabelle
165200100702      *   per clienti particolari (CL)
165300100702      * Vengono decodificati anche a questo livello poichè ci sono dei campi del "VAB",
165400100702      *  pulito a livello di inizio dettaglio bolla, che servono in altri segmenti
165500100702      *   successivi per altre decodifiche.
165600100702     c                   if        Dati_di_Testata <>'S'
165700100702      *
165800100702      *   se aveva un RFF+FF in dettaglio
165900100702     C                   if         Det_RFF_FF_ksc > 0
166000100702      *
166100100702     C                   eval      VABccm = Det_RFF_FF_ksc
166200100706     C                   movel     Det_RFF_FF_tarVABctr
166300100702     c                   eval      VABlnp = Det_RFF_FF_lnp
166400100702     c                   eval      VABnrs = Det_RFF_FF_nrs
166500100702     C                   eval      VABctm = Det_RFF_FF_ctm
166600100702      *
166700100702      *   altrimenti se aveva un RFF+FF in Testata
166800100702     c                   elseif     Tes_RFF_FF_ksc > 0
166900100702      *
167000100702     C                   eval      VABccm = Tes_RFF_FF_ksc
167100100706     C                   movel     Tes_RFF_FF_tarVABctr
167200100702     c                   eval      VABlnp = Tes_RFF_FF_lnp
167300100702     c                   eval      VABnrs = Tes_RFF_FF_nrs
167400100702     C                   eval      VABctm = Tes_RFF_FF_ctm
167500100702      *
167600100702     c                   end
167700100702      *
167800100702     C                   ENDIF
167900100909      *
168000100701     c     end_RFF       endSR
168100100701      * ?================================================================== */
168200100701     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
168300100701      * ?================================================================== */
168400100701     C     DATI_DTM      BEGSR
168500100705      *
168600100705      *   Cerca la decodifica del dato in TABELLA
168700100705     C                   movel(p)  'DTM'         etbSGM
168800100705     C                   movel(p)  dtm2005       etbVALSGM
168900100705     c                   exsr      In_TAB_EDSTBL
169000100705      *
169100100701     C*  Se data documento  memorizzo numero e data x CMR
169200100705     C                   If        Quale_campo ='DATDOC'
169300101015     c                              and dtm2380 <> *blank
169400100702     C                   MOVEL     dtm2380       Work_Data__35    35            Data
169500100702     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
169600100702     C                   EXSR      Converte_Data
169700100701     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
169800100701     C                   MOVEL     WCAMG         DTM_DtCMR                      Data CMR
169900100701     C                   MOVEL     WHMS          DTM_OraCMR                     Ora  CMR
170000100701     C                   MOVEL     WCAMG         DTM_DtFViag                    Data CMR
170100100701     C                   MOVEL     WHMS          DTM_OraFViag                   Ora  CMR
170200100701     C     WERRO         IFEQ      'S'
170300100706     C                   eval      vinMSG = 'DTM ( Data_Documento ' +
170400100706     C                             dtm2005  +
170500100702     C                              ') data errata'
170600100701     c                   exsr      Error_DET
170700100701     c                   goto      end_DTM
170800100701     C                   END
170900100701     C                   END
171000100702     C*
171100100702     C* Data Partenza del mezzo
171200100705     C                   If        Quale_campo ='DATPAR'
171300101015     c                              and dtm2380 <> *blank
171400100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
171500100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
171600100702     C                   EXSR      Converte_Data
171700100706     C                   MOVEL     WCAMG         DTM_DtParten                   Data
171800100706     C                   MOVEL     WHMS          DTM_OraParten                  Ora
171900100702     C     WERRO         IFEQ      'S'
172000100706     C                   eval      vinMSG = 'DTM ( Data Partenza ' +
172100100706     C                             dtm2005  +
172200100702     C                              ') data errata'
172300100702     c                   exsr      Error_DET
172400100702     c                   goto      end_DTM
172500100702     C                   END
172600100702     C                   END
172700100702     C*
172800100702     C* Data Previsto Arrivo del mezzo
172900100705     C                   If        Quale_campo ='DATARR'
173000101015     c                              and dtm2380 <> *blank
173100100705     C                   MOVEL     dtm2380       Work_Data__35                  Data
173200100705     C                   MOVEL     dtm2379       Work_Format_3                  Formato
173300100702     C                   EXSR      Converte_Data
173400100702     C                   MOVEL     WCAMG         DTM_DtArrivo                   Data
173500100702     C                   MOVEL     WHMS          DTM_OraArrivo                  Ora
173600100702     C     WERRO         IFEQ      'S'
173700100706     C                   eval      vinMSG = 'DTM ( Data Arrivo ' +
173800100706     C                             dtm2005  +
173900100702     C                              ') data errata'
174000100702     c                   exsr      Error_DET
174100100702     c                   goto      end_DTM
174200100702     C                   END
174300100702     C                   END
174400100701      **
174500100701     C*  Se data consegna richiesta imposto già sui campi del VAB
174600100705     C                   If        Quale_campo ='VABDCR'
174700101015     c                              and dtm2380 <> *blank
174800100702     C                   MOVEL     dtm2380       Work_Data__35                  Data
174900100702     C                   MOVEL     dtm2379       Work_Format_3                  Formato
175000100702     C                   EXSR      Converte_Data
175100100811      **
175200100811     c                   IF        Dati_di_Testata  ='S'
175300100811     C                   MOVEL     WCAMG         DTM_DtConsRic                  Data cons.rich.
175400100811     C                   MOVEL     WHMS          DTM_OraCnsRic                  Ora  cons.rich.
175500100811     c                   end
175600100701     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
175700100811     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
175800100701     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
175900100701     C     WERRO         IFEQ      'S'
176000100706     C                   eval      vinMSG = 'DTM ( Data_Richiesta_Consegna ' +
176100100706     C                             dtm2005  +
176200100702     C                              ') data errata'
176300100701     c                   exsr      Error_DET
176400100701     c                   goto      end_DTM
176500100701     C                   END
176600100702     C                   end
176700100701      **
176800100701     c     end_DTM       endSR
176900100813      * ?================================================================== */
177000100813     C*?  Tipo Servizio
177100100813      * ?================================================================== */
177200100813     C     DATI_TSR      BEGSR
177300100813      ***
177400110127      ***  Non è importante quindi al momento viene lasciato blank
177500110127     c                   goto      end_TSR
177600110127      ***
177700110127      ***
177800110127      ***
177900110127      ***
178000100813     C*  Reperisco priorità spedizione:
178100100813     C                   Z-ADD     1             X
178200100813      ***
178300110127     c                   movel(p)  TSR7273201    Work_Data__35
178400100813     c                   move      PU_TipoServ   Work_Data__35
178500100813     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
178600100813     C     *IN32         IFEQ      '1'
178700100813      *
178800100813     c                   if        Dati_di_Testata <>'S'
178900100813     C                   eval             VABTSP = TipoServizio(X)
179000100813     c                   elseIf    Dati_di_Testata = 'S'
179100100813     C                   eval         tes_VABtsp = TipoServizio(X)
179200100813     c                   end
179300100813      *
179400100813     C                   ELSE
179500100813      **  Il tipo bolla non è presente
179600100813     C                   eval      vinMSG = 'TSR non in tabella'
179700100813     c*********          exsr      Error_DET
179800100813     c*********          goto      end_TSR
179900100813     C                   END
180000100813     C*
180100100813     C*  Decodifico servizi speciali
180200100813     C     TSR7085       IFNE      *BLANKS
180300100813     C     TSR7085       ANDNE     *LOVAL
180400100813      *
180500100813     C                   Z-ADD     1             X
180600100813     c                   movel(p)  TSR7085       Work_Data__35
180700100813     c                   move      PU_SpecServ   Work_Data__35
180800100813     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
180900100813      * Particolarità
181000100813     C     *IN32         IFEQ      '1'
181100100813     C                   eval       EDIDSSS = UNI_ServSpec(X)
181200100813      * Descrizione
181300100813     C     §SSNOT        IFEQ      'S'
181400100813     c                   if        Dati_di_Testata <>'S'
181500100813     C                   MOVEL     §SSDES        VABNOT
181600100813     c                   elseIf    Dati_di_Testata = 'S'
181700100813     C                   eval       tes_VABnot = §SSDES
181800100813     c                   end
181900100813     C                   END
182000100813      *
182100100813      * x Espresso     "Pre-noon"
182200100813     C     §SSTPS        IFEQ      'S'
182300100813     c                   if        Dati_di_Testata <>'S'
182400100813     C                   MOVEL     'E'           VABTSP
182500100813     c                   elseIf    Dati_di_Testata = 'S'
182600100813     C                   eval         tes_VABtsp = 'E'
182700100813     c                   end
182800100813     C                   END
182900100813      *
183000100813      * x Appuntamento "Booking ins"
183100100813     C     §SSTPS        IFEQ      'A'
183200100813     c                   if        Dati_di_Testata <>'S'
183300100813     C                   MOVEL     'A'           VABTC1
183400100813     c                   elseIf    Dati_di_Testata = 'S'
183500100813     C                   eval         tes_VABtc1 = 'A'
183600100813     c                   end
183700100813     C                   END
183800100813      *
183900100813     C                   ELSE
184000100813      *
184100100813      **  Il tipo bolla non è presente
184200100813     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
184300100813     c*******            exsr      Error_DET
184400100813     c*******            goto      end_TSR
184500100813      *
184600100813     C                   END
184700100813      *
184800100813     C                   END
184900100813      **
185000100813     c     end_TSR       endSR
185100100813      * ?================================================================== */
185200100813     C*?  C.O.D. Monetary Amount
185300100813      * ?================================================================== */
185400100813     C     DATI_MOA      BEGSR
185500100813      *
185600100813      *   Cerca la decodifica del dato in TABELLA
185700100813     C                   movel(p)  'MOA'         etbSGM
185800100813     C                   movel(p)  moa5025       etbVALSGM
185900100813     c                   exsr      In_TAB_EDSTBL
186000100813      *
186100100813     C* Controllo se campo importo numerico con 3 decimali
186200100813     C     moa5004       IFNE      *zeros
186300100813      *
186400100813     C                   If        Quale_campo ='VABIAS'
186500100813     C                   z-add     moa5004       VABIAS                         IMPORTO
186600100813     C                   MOVEL     moa6345       VABVAS                         ASSICURATO
186700100813      *
186800100813     C                   elseIF    Quale_campo ='VABVMD'
186900100813     C                   z-add     moa5004       VABVMD                         MERCE
187000100813     C                   MOVEL     moa6345       VABVAD                         DICHIARATO
187100100813      *
187200100813     C                   elseIF    Quale_campo ='VABCAS'
187300100813     C     §PUCAS        IFEQ      'S'                                          C/Assegno
187400100813      *
187500100813     C                   z-add     moa5004       VABCAS
187600100813     C                   MOVEL     moa6345       VABVCA
187700100813     c                   if        Tes_valuta <> *blank and VABVCA = *blank
187800100813     C                   MOVEL     Tes_valuta    VABVCA
187900100813     c                   end
188000100813      *
188100100813      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
188200100813      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
188300100813     C     §PUTPC        IFEQ      'P'
188400100813     C                   MOVEL     'OM'          VABTIC
188500100813     C                   MOVEL     'OM'          VABtic_work
188600100813     C                   ENDIF
188700100813      *
188800100813     C                   END
188900100813      *
189000100813     C                   EndIF
189100100813     C                   END                                                    Imp.non numer.
189200100813      *
189300100813     c     end_MOA       endSR
189400100813      * ?================================================================== */
189500100813     C*?  FREE TEXT
189600100813      * ?================================================================== */
189700100813     C     DATI_FTX      BEGSR
189800100813      *
189900100813      *   Cerca la decodifica del dato in TABELLA
190000100813     C                   movel(p)  'FTX'         etbSGM
190100100813     C                   movel(p)  ftx4451       etbVALSGM
190200100813     c                   exsr      In_TAB_EDSTBL
190300100813      *
190400100813      *
190500100813      * ? solo se si tratta di note descrittive da comunicare in BOLLA
190600100813     c                   if         quale_campo = 'VABNOT'
190700100813     c                                or ftx4451 = §pvSIN
190800100813     c                                or ftx4451 = §pvICN
190900100813     C                   DO        4             X
191000100813      *
191100100813     C     D05(X)        IFNE      *BLANKS
191200100813      *
191300100813     C     Note_1        IFEQ      *BLANKS
191400100813     C                   MOVEL     D05(X)        Note_1
191500100813     C                   MOVE      D05(X)        Note_2
191600100813     C                   ELSE
191700100813     C     Note_2        IFEQ      *BLANKS
191800100813     C                   MOVEL     D05(X)        Note_2
191900100813     C                   END
192000100813     C                   END
192100100813      *
192200100813     C                   ENDif
192300100813      *
192400100813     C                   ENDdo
192500100813     C                   end
192600100813     C*------>>
192700100813     C*  Decodifico tipo pagamento C/Assegno
192800100813      *    lo pulisco solo se non l'ho decodificato e non l'ho ancora scritto
192900100813      *      (Problema di pulizia in presenza di + righe di testo)
193000100813      *  --> succedeva che al primo giro aveva letto una Free Text con le informazioni
193100100813      *     x il tipo incasso poi arrivava una seconda riga Free Text con altro tipo
193200100813      *       di informazioni, la riga del VAB non era stata ancora scritta e qui
193300100813      *       abblenkava il VABTIC togliendo il Tipo Incasso inviato.
193400100813     C                   if        wri_vabtic = *blank
193500100813     C                   MOVEL     *BLANKS       VABTIC
193600100813     c                   end
193700100813      **
193800100813     c                   clear                   trovato_TPInc     1            TipoIncasso
193900100813      *
194000100813      * ?Se c'è il tipo Incasso
194100100813     c                   if         quale_campo = 'VABTIC'                      -- 01 --
194200100813      *
194300100813     C                   DO        4             X                              -- 02 --
194400100813     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
194500100813     C                   MOVEL     D05(X)        WCTC              2
194600100813     C                   Z-ADD     1             Y
194700100813     c                   movel(p)  Wctc          Work_Data__35
194800100813     c                   move      §puTC         Work_Data__35
194900100813     C     Work_Data__35 LOOKUP    K35_TpIncas(Y)                         33
195000100813      * SE PARTNER IMPOSTO 'OM'
195100100813     C     §PUTPC        IFEQ      'P'
195200100813     c     wctc          andne     'TO'
195300100813     C                   MOVEL     'OM'          VABTIC
195400100813     C                   ELSE
195500100813     C   33              eval      VABTIC = TipoIncasso(Y)
195600100813     c   33              move      'S'           trovato_TPInc
195700100813     C                   END                                                    -- 03 --
195800100813     C                   END                                                    -- 03 --
195900100813     C                   ENDdo                                                  -- 02 --
196000100813      *
196100100813     C                   ELSE                                                   -- 01 --
196200100813      *
196300100813      * ?Se NON c'è il tipo Incasso Contrassegno particolare
196400100813      *    Deve essere impostato comunque da testata
196500100813     c                   if        vabtic = *blank
196600100813     C                   MOVEL     VABtic_work   VABTIC
196700100813     c                   end
196800100813      *
196900100813     C                   END                                                    -- 01 --
197000100813      *
197100100813      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
197200100813      *    essere presenti altri record Flat con altre informazioni
197300100813      *    che potrebbero abblancare il Tipo incasso precedentemente
197400100813      *    decodificato
197500100813     c                   if        Trovato_TPInc = 'S' and
197600100813     c                                 wri_vabtic = *blank
197700100813     c                   eval      wri_vabtic = 'S'
197800100813     c                   end
197900100813      *
198000100813      * ?Se Natura Merce      .
198100100910     C*  Natura Merce  (Se abilitato alla trascodifica)
198200100910     c                   if        §PUnas = 'S'
198300100813     c                   if         quale_campo = 'VABNAS'                      -- 01 --
198400100813     C                   MOVEL     D05(1)        VABnas
198500100813     c                   End
198600100910     c                   End
198700100813      *
198800100813     c     end_FTX       endSR
198900100701      * ?================================================================== */
199000100701     C*? Imposta informazioni sul dettaglio del trasporto
199100100701      * ?================================================================== */
199200100701     C     DATI_TDT      BEGSR
199300100701     C*
199400100701      **
199500100701     c     end_TDT       endSR
199600100811      * ?================================================================== */
199700100811     C*? Imposta
199800100811      * ?================================================================== */
199900100811     C     DATI_DOC      BEGSR
200000100811     C*
200100100811      **
200200100811     c     end_DOC       endSR
200300100811      * ?================================================================== */
200400100811     C*? Imposta
200500100811      * ?================================================================== */
200600100811     C     DATI_GIN      BEGSR
200700100811     C*
200800100920      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
200900100920      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
201000100920     C     §1BF12        IFEQ      'E'
201100100920     C                   EXSR      SEGNaCollo_EEX
201200100920     C                   ELSE
201300100920      *
201400100920      *  La prima volta controllo congruità numero segnacollo
201500100920     C     §PUBS1        IFNE      *BLANKS
201600100920     C     VABnrs        ANDNE     0
201700100920      *  Se il codice trattamento merce prevede che segnacollino i
201800100920      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
201900100920     C     §1BFG7        IFEQ      'S'
202000100920     C     §1BFG1        OREQ      'S'
202100100920      *  calcolo segnacollo
202200100920     C                   EXSR      Segn_Bartolini
202300100920     C                   END
202400100920     C                   END
202500100920      *
202600100920     C                   END
202700100811      **
202800100811     c     end_GIN       endSR
202900100316      * ?================================================================== */
203000100316     C*? Imposta l'inizio del Dettaglio CNI
203100100316      * ?================================================================== */
203200100316     C     DATI_CNI      BEGSR
203300100316     C*
203400100702      ** Inizio Dettaglio riferimento iniziale
203500100702     c                   If        CNI1004 <> *blank
203600100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
203700100813      *
203800100813      *   Nel Riferimento Alfa sempre
203900100813     c                   eval      VABRMA  = CNI_Identificativo_Bolla
204000100813      *
204100100813      *   e nel numerico se di soli numeri e comunque se lo memorizza
204200100813     c                   clear                   CNI_seNumerico
204300100813     C                   eval       test_num = CNI1004
204400100813     C     ' '           SCAN      CNI1004       lunghezza_num     3 0
204500100813     C                   SUB       1             lunghezza_num
204600100813     C                   EXSR      Se_Numerico
204700100813     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
204800100813      * ?                                       --------
204900100813     C                   MOVEL     Numero_OK     VABrmn
205000100813      * ?                                       --------
205100100813     C                   z-add     VABrmn        CNI_seNumerico
205200100813      * ?                                       --------
205300100813     C                   END
205400100813      *
205500100702     c                   end
205600100319      *
205700100316      **
205800100316     c     end_CNI       endSR
205900090209      * ?================================================================== */
206000090209     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
206100090209      * ?================================================================== */
206200090209     C     DATI_TOD      BEGSR
206300090209      *
206400090210      *  Trascodifico tipo trasporto
206500090210     c                   clear                   TOD_char3         3
206600100705      *  DEFAULT da PT
206700090210      *  imposta il default dalla tabella PT se c'è
206800090210     C     §puPFA        ifNE      *blank
206900090210     c                   SELECT
207000090210      *  Porto Franco
207100090210     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
207200090210     C                   movel     '4'           VABCBO                         + C/Ass
207300090210     c                   WHEN      §puPFA = 'F'
207400090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
207500090210      *  Porto Assegnato
207600090210     c                   WHEN      §puPFA = 'A'  and  VABCAS > *zeros
207700090210     C                   movel     '6'           VABCBO                         + C/Ass
207800090210     C                   WHEN      §puPFA = 'A'
207900090210     C                   movel     '2'           VABCBO                         Senza C/Ass
208000090210     C                   ENDSL
208100090210     c                   end
208200090210      *
208300090210      *   Imposta o il codice o il metodo
208400090210      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
208500110126     c                   if        tod4055  <> *blank
208600110126     c                   movel(p)  tod4055       TOD_char3
208700090210     c                   else
208800090210     c                   if        tod4215  <> *blank
208900090210     c                   movel(p)  tod4215       TOD_char3
209000090210     c                   end
209100090210     c                   end
209200110328      ****************
209300110328      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
209400110328      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
209500110328      ****************
209600110328     c                   eval      Trovata_Tab_TB ='N'
209700110328      *
209800110328     c                   if              TOD_char3 <> *blank
209900110328     c                   movel(p)  TOD_char3     Key_Generica35
210000110328     c                   move      §puTB         Key_Generica35
210100110328     C                   eval      Y = 1
210200110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
210300110328     c   32              eval      Trovata_Tab_TB ='S'
210400110328      *
210500110328      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
210600110328      *   nell'altro campo 4215 quindi comunque ci si riprova:
210700110328      *
210800110328     c                   if          Trovata_Tab_TB ='N'
210900110328     c                                 and
211000110328     C                               tod4215 <> *blank
211100110328     c                   movel(p)  tod4215       TOD_char3
211200110328     c                   movel(p)  TOD_char3     Key_Generica35
211300110328     c                   move      §puTB         Key_Generica35
211400110328     C                   eval      Y = 1
211500110328     C     Key_Generica35LOOKUP    K35_TpBolla(Y)                         32
211600110328     c   32              eval      Trovata_Tab_TB ='S'
211700110328     c                   end
211800110328     c                   end
211900110328      *
212000110328      ****************
212100110328      ******* Se vi sono eccezioni sul cliente imposta quelle
212200110328      ****************
212300110328     c                   If          Trovata_Tab_TB ='S'
212400110328      *
212500110328     c                   SELECT
212600110328     c                   WHEN      Decod_Porto(Y) = 'F'
212700110328     c                               and  VABcas > *zeros
212800110328     C                   movel     '4'           VABCBO                         + C/Ass
212900110328     c                   WHEN      Decod_Porto(Y) = 'F'
213000110328     C                   movel     '1'           VABCBO                         Senza C/Ass.
213100110328      *  Porto Assegnato
213200110328     c                   WHEN      VABCAS > *zeros
213300110328     C                   movel     '6'           VABCBO                         + C/Ass
213400110328     C                   OTHER
213500110328     C                   movel     '2'           VABCBO                         Senza C/Ass
213600110328     C                   ENDSL
213700110328      *
213800110328      ****************  altrimenti
213900110328      ******* Se non vi sono eccezioni per il cliente
214000110328      *   Rileva a questo punto lo Standard per tutti
214100110328      ****************
214200110328     c                   elseIf      Trovata_Tab_TB ='N'
214300090210      **
214400090210     c                   if              TOD_char3 <> *blank
214500100705      *   Cerca la decodifica del dato in TABELLA
214600100705     C                   movel(p)  'TOD'         etbSGM
214700100705     C                   movel(p)  TOD_char3     etbVALSGM
214800100705     c                   exsr      In_TAB_EDSTBL
214900090210      *
215000100705     c                   if        trovata_EDSTBL ='S' and Quale_campo ='VABCBO'
215100100705      *
215200090210     c                   SELECT
215300100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F' and
215400100705     c                               VABcas > *zeros
215500090210     C                   movel     '4'           VABCBO                         + C/Ass
215600100705     c                   WHEN      %subst(campo_dati_70A:1:1) = 'F'
215700090210     C                   movel     '1'           VABCBO                         Senza C/Ass.
215800090210      *  Porto Assegnato
215900090210     c                   WHEN      VABCAS > *zeros
216000090210     C                   movel     '6'           VABCBO                         + C/Ass
216100090210     C                   OTHER
216200090210     C                   movel     '2'           VABCBO                         Senza C/Ass
216300090210     C                   ENDSL
216400090210      *
216500090210     c                   end
216600090210     c                   endIF
216700090209      **
216800110328     c                   endIF
216900110328      **
217000090210      **  Il tipo bolla non è presente
217100090210     c                   if        VABCBO = *blank
217200090210     C                   eval      vinMSG = 'TOD non rilevato'
217300090213     c                   exsr      Error_DET
217400090210     c                   goto      end_TOD
217500090210     c                   end
217600090210      **
217700090210     c     end_TOD       endSR
217800090213      * ?================================================================== */
217900090213     C*?  Anagrafica del mittente e del destinatario
218000090213      * ?================================================================== */
218100091016     C     DATI_NAD      BEGSR
218200100705      *
218300100705      *   Cerca la decodifica del dato in TABELLA
218400100813     c                   clear                   dopo_NAD_CN       1
218500100705     C                   movel(p)  'NAD'         etbSGM
218600100705     C                   movel(p)  nad3035       etbVALSGM
218700100705     c                   exsr      In_TAB_EDSTBL
218800100708      *-------
218900100708      * Dato utilizzato in assenza di CMR    (SHIPPED FROM)
219000100708      *-------
219100100708     C                   IF        Quale_campo ='VABCMR'
219200100708     c                   if        nad30361 <> *blank
219300100708     C                   MOVEL     nad30361      NAD_SF_x_CMR
219400100708     c                   elseIf    nad31241 <> *blank
219500100708     C                   MOVEL     nad31241      NAD_SF_x_CMR
219600100708     c                   elseIf    nad3039  <> *blank
219700100708     C                   MOVEL     nad3039       NAD_SF_x_CMR
219800100708     c                   end
219900100705      *-------
220000090210      * Dati destinatario
220100100705      *-------
220200100708     C                   elseIF    Quale_campo ='VABRSD'
220300100813     c                   move      'S'           dopo_NAD_CN
220400090227      *
220500090227      * Se trovato un indirizzo di destinatario
220600100701     c                   if           Dati_di_Testata = 'N'
220700100701     c                   eval       NAD_Destinatario_in_Dettaglio ='S'
220800100701     c                   else
220900100701     c                   eval       NAD_destinatario_in_testata = 'S'
221000100701     c                   end
221100090210      *
221200090210     C* Reperisco nazione corrispondente destinatario
221300100705     C                   Z-ADD     1             naz               3 0
221400100705     C                   MOVEL     *BLANKS       VABNZD
221500100705     C                   Z-ADD     0             WFLCPF
221600090210     c                   eval      *in32 = *off
221700100705      ***
221800090210     C                   if        nad3207 <> *BLANKS
221900100705     C     nad3207       LOOKUP    ISO(naz)                               32
222000100705     C                   If        *in32 = *on
222100100705      ***
222200100705      * per reperire il CAP corretto
222300100705     C                   MOVEL     C15(naz)      VABNZD
222400100705     C                   MOVEL     CPF(naz)      WFLCPF            2 0
222500100705      ***
222600100705     c                   Endif
222700100705     C                   endif
222800100705      *
222900090210     C* Imposto dati destinatario
223000110127     C                   MOVEL     nad31241      VABRSD
223100100705????  * se DAL PARTY NAME non aveva nulla cerca sulla descrizione Nome/Cognome
223200091019???? C* Imposto dati destinatario
223300091019???? c                   if        vabRSD = *blank and VABRD2 = *blank
223400110127???? C                   MOVEL     nad30361      VABRD2
223500091019???? C                   end
223600100705      *   città/Località
223700090210     C                   MOVEL     nad3164       VABLOD
223800100812      *
223900100812      *  Se richiesto in tabella PT di accodare alla Ragione sociale
224000100812      *  l'informazione di indirizzo invertita nel segmento...
224100100812      *  Esigenza nata da Dior che mette l'indirizzo vero nel 2°campo del Indirizzo NAD
224200100812      *  ossia dopo i 2 punti (:) ed invece mette un'informazione sul destinatario/
224300100812      *  indirizzo nel 1° campo dell'Inidirizzo NAD prima dei 2 punti (:).
224400100812      *   Per nostra comodità mettiamo quindi l'informazione aggiuntiva attaccata alla
224500100812      *   Ragione Sociale e non alla Località poichè altrimenti avremmo continui
224600100812      *   problemi di decodifica Località/Cappario durante la conferma bolle.
224700100812     c                   if        §pv2IN = 'S'
224800100812      *
224900100812     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
225000100812      *
225100110127      * Nell'indirizzo il 2°campo ricevuto
225200110127     C                   MOVEL     NAD30422      VABIND
225300110127      * Mentre appiccica all'estensione della ragione sociale le info di consegna
225400110127      *  messe nel 1° campo dell'indirizzo (DIOR)
225500110127      *
225600100812      *  Verifico se c'è spazio sul campo estensione altrimenti vado in coda
225700100812      *    prima all'indirizzo e poi alla località
225800100812      *
225900100812     C* Verifico se c'è dello spazio per metterlo nell'estensione rag.soc.
226000100812     C     '    '        SCAN      VABRD2        WI                4 0
226100100812     c                   if        WI < 10
226200100812     C     '    '        SCAN      VABrd2        WI                4 0
226300100812     C                   MOVEA     VABrd2        LOD
226400100812     C                   ADD       1             WI
226500110127     C                   MOVEA     NAD30422      LOD(WI)
226600100812     C                   MOVEA     LOD           VABrd2
226700100812      *
226800100812     c                   else
226900100812      *  Verifico se c'è spazio sull'indirizzo altrimenti vado in coda alla località
227000100812     C     '    '        SCAN      VABIND        WI                4 0
227100100812     c                   if        WI < 10
227200100812     C     '    '        SCAN      VABIND        WI                4 0
227300100812     C                   MOVEA     VABIND        LOD
227400100812     C                   ADD       1             WI
227500100812     C                   MOVEA     NAD30421      LOD(WI)
227600100812     C                   MOVEA     LOD           VABIND
227700100812      *
227800100812     c                   else
227900100812      *  oppure    accodato nella località
228000100812     C     '    '        SCAN      VABLOD        WI                4 0
228100100812     C                   MOVEA     VABLOD        LOD
228200100812     C                   ADD       1             WI
228300100812     C                   MOVEA     NAD30421      LOD(WI)
228400100812     C                   MOVEA     LOD           VABLOD
228500100812     c                   end
228600100812     c                   end
228700100812      *-----------
228800100812     c                   else
228900100812      *
229000100812     C                   MOVEL     NAD30421      VABIND
229100100812     c                   end
229200100812      *
229300100812      *-----------
229400100812      *  segue lo standard
229500100812     c                   else
229600100812      *-----------
229700100812      *
229800100812     C                   MOVEL     NAD30421      VABIND
229900100812      *
230000100812     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
230100100812     c                   if        NAD30422 <> *blanks  and NAD30422 <> *loval
230200100812      *
230300100812      *  Verifico se c'è spazio sul primo indirizzo x accodare se richiesto
230400100812      *  altrimenti vado in coda alla località
230500100812     C     '    '        SCAN      VABIND        WI                4 0
230600100812      *
230700100812     C* Verifico se c'è dello spazio per metterlo nella località
230800100812      *  oppure    accodato al primo indirizzo
230900100812      *
231000100812     c                   if        WI < 15
231100100812      *
231200100812     C     '    '        SCAN      VABind        WI                4 0
231300100812     C                   MOVEA     VABind        LOD
231400100812     C                   ADD       1             WI
231500100812     C                   MOVEA     NAD30422      LOD(WI)
231600100812     C                   MOVEA     LOD           VABind
231700100812      *
231800100812     c                   else
231900100812      *
232000100812     C     '    '        SCAN      VABLOD        WI                4 0
232100100812     C                   MOVEA     VABLOD        LOD
232200100812     C                   ADD       1             WI
232300100812     C                   MOVEA     NAD30422      LOD(WI)
232400100812     C                   MOVEA     LOD           VABLOD
232500100812      *
232600100812     c                   end
232700100812      *
232800100812     c                   end
232900100812      *
233000100812      *
233100100812     C                   ENDif
233200100812      * ---------
233300090210      * Converto CAP
233400100705     C                   EXSR      Converte_CAP
233500090603     C*
233600100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
233700100705     C*  record del VAB viene PULITO
233800100316     c                   if        Dati_di_Testata  = 'S'
233900090603     c                   eval       tes_VABrsd = VABrsd
234000090603     c                   eval       tes_VABrd2 = VABrd2
234100090603     c                   eval       tes_VABind = VABind
234200090603     c                   eval       tes_VABlod = VABlod
234300090603     c                   eval       tes_VABnzd = VABnzd
234400090603     c                   end
234500100728      *
234600100728      *   se NON c'è l'identificativo bolla del cliente
234700100728      *    blocca la spedisione e lo segnala.
234800100728     C                   if          vabRSD = *blank
234900100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
235000100728     c                   exsr      Error_DET
235100100728     c                   goto      end_NAD
235200100728     C                   elseIF      vabIND = *blank
235300100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
235400100728     c                   exsr      Error_DET
235500100728     c                   goto      end_NAD
235600100728     C                   elseIF      vabLOD = *blank
235700100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
235800100728     c                   exsr      Error_DET
235900100728     c                   goto      end_NAD
236000100728     C                   ENDIF
236100100705      *-------
236200090210      * Dati mittente
236300100705      *-------
236400100705     C                   elseIF     Quale_campo ='VABRMO'
236500090210      *
236600110127     C                   movel     nad31241      VABRMO
236700090210      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
236800090210     C                   movel     §PTNAZ        VABNMO
236900090210      *
237000090210     C                   IF        nad3207 <> *blanks
237100100705     C                   eval      naz = 1
237200100705     C     nad3207       LOOKUP    ISO(naz)                               32
237300090210     C                   if         *in32 = *on
237400100705     C                   MOVEL     C15(naz)      VABNMO
237500090210     C                   endif
237600090210     C                   ENDIF
237700090210      *
237800090210      * Normalizzo CAP mittente originale
237900090210     C                   clear                   T
238000090210     C                   clear                   S
238100090210     C                   MOVEA     *BLANKS       CAPM
238200090210     C                   MOVEA     nad3251       CAP9
238300090210     C                   DO        9             T
238400090210     C     CAP9(T)       IFNE      *BLANKS
238500090210     C                   ADD       1             S
238600090210     C                   MOVE      CAP9(T)       CAPM(S)
238700090210     C                   ENDIF
238800090210     C                   ENDDO
238900090210     C*
239000090210      *     Controllo validità CAP
239100090210     C                   MOVEA     CAPM          VABCMO
239200090210      *
239300090210     C                   CLEAR                   TISI95DS
239400090210     C                   MOVEL     VABCMO        I95CAP
239500090210     C                   MOVEL     VABNMO        I95NAR
239600090210     C                   MOVEL     WOGGI         I95DAT
239700090210     C                   MOVEL     '3'           I95TCN
239800090210     C                   CALL      'TISI95R'
239900090210     C                   PARM                    TISI95DS
240000090210      *     Errore
240100090210     C     O95ERR        IFNE      *BLANKS
240200090210     C                   MOVEL     INDCAE        VABCMO
240300090210     C                   MOVEL     §PTNAZ        VABNMO
240400090210     C                   END
240500090603      *
240600100705     C* Se in testata  SALVA i campi poichè ad ogni dettaglio il
240700100705     C*  record del VAB viene PULITO
240800100316     c                   if        Dati_di_Testata  = 'S'
240900090603     c                   eval      tes_VABrmo = VABrmo
241000090603     c                   eval      tes_VABnmo = VABnmo
241100090603     c                   eval      tes_VABcmo = VABcmo
241200090603     c                   eval      NAD_mittente_in_testata = 'S'
241300100701     c                   else
241400100701     c                   eval      NAD_mittente_in_Dettaglio = 'S'
241500090603     c                   end
241600100728      *
241700100728      *   se NON c'è l'identificativo bolla del cliente
241800100728      *    blocca la spedisione e lo segnala.
241900100728     C                   if          vabRMO = *blank
242000100728     C                   eval      vinMSG = 'NAD Riferimento Mittente assente'
242100100728     c                   exsr      Error_DET
242200100728     c                   goto      end_NAD
242300100728     C                   ENDIF
242400090210      *
242500090210     C                   END
242600090210      *
242700090210     c     end_NAD       endSR
242800090210      * ?================================================================== */
242900090210     C*?  Contatto a cui far riferimento
243000090210      * ?================================================================== */
243100090210     C     DATI_CTA      BEGSR
243200090210      *
243300090210      *  memorizza il riferimento di consegna x il destinatario
243400100813     C                   if         cta3412 <> *BLANKS and
243500100813     c                                    dopo_NAD_CN = 'S'
243600100813      *
243700100701     c                   if        Dati_di_Testata  ='S'
243800100812     c                   if           CTA_Destinatario_in_testata   = *blanks
243900100812     c                   eval       CTA_Destinatario_in_testata   = cta3412
244000100812     c                   end
244100100701     c                   else
244200100812     c                   if           CTA_Destinatario_in_dettaglio = *blanks
244300100701     c                   eval       CTA_Destinatario_in_dettaglio = cta3412
244400100812     c                   end
244500100701     c                   end
244600100701     c                   end
244700090210      *
244800090210     c                   endSR
244900090210      * ?================================================================== */
245000090210     C*?  Contatto telefonico  a cui far riferimento
245100090210      * ?================================================================== */
245200090210     C     DATI_COM      BEGSR
245300090210      *
245400100705      *  memorizza il riferimento di consegna nr.telefonico
245500100813     C                   if         com3148 <> *BLANKS and
245600100813     c                                  COM3155 = 'TE' and
245700100813     c                                    dopo_NAD_CN = 'S'
245800100813      *
245900100701     c                   if        Dati_di_Testata  ='S'
246000100812     c                   if           COM_telefono_in_testata = *blanks
246100100812     c                   eval      COM_telefono_in_testata   = com3148
246200100812     c                   end
246300100701     c                   else
246400100812     c                   if           COM_telefono_in_dettaglio = *blanks
246500100701     c                   eval      COM_telefono_in_dettaglio = com3148
246600100812     c                   end
246700090210     c                   end
246800100705     c                   endif
246900090210      *
247000090209     C                   ENDSR
247100090210      * ?================================================================== */
247200090210     C*?  Dati di dettaglio della spedizione e dei colli
247300090210      * ?================================================================== */
247400090210     C     DATI_GID      BEGSR
247500090210      *
247600100910      *  Se non richiesto quello sulla testata dettaglio
247700100920     c                   if        §pvCOl  <> 'S' or vabPKB = 0
247800100910      *
247900090210     C* Se ho totali per spedizione metto in campi VAB
248000090210     C     gid1496       IFEQ      99999
248100090210     C     gid1496       orEQ      9999
248200100630     c                   eval      GID_aTotale = 'S'
248300090210     C                   z-add     gid722420     VABNCL
248400090210     C                   ELSE
248500100630     c                   If        GID_aTotale = 'N'
248600100705     C                   z-add     gid722420     GID_ColliInAdd
248700100705     C                   eval      VABNCL = VABNCL + GID_ColliInAdd
248800090210     C                   END
248900090210     C                   END
249000090212      *
249100100701     C                   ADD       VABNCL        ToT_Colli        16 0
249200090210      *
249300100910     C                   END
249400100910      *
249500090210     c     end_GID       endSR
249600090210      * ?================================================================== */
249700090210     C*?  Dati di dettaglio misure spedizione e colli
249800090210      * ?================================================================== */
249900090210     C     DATI_MEA      BEGSR
250000090210      *
250100100910     C*  Se non si deve prendere il peso dal Totale (CNT)
250200100920     c                   if        §pvPEs <> 'S' or vabPKB = 0
250300090210     C*  Peso
250400110127     C     mea6311       IFEQ      Gross_Weight_E
250500100319     C     mea6411       ANDEQ     Kilograms
250600090210      *   sempre il Lordo
250700110127     C*  Peso in Grammi poichè senza decimali va tradotto in kg
250800110127???? C                   move      mea6314       mea6314_3        18 3          Peso
250900110127???? C                   Z-ADD     mea6314_3     VABPKB                         Peso
251000090210      *
251100110127???? C                   move      mea6314       mea6314_3                      Peso
251200110127???? C                   ADD       mea6314_3     ToT_PesoLordo
251300110127      *
251400090210     C                   END
251500090210      ***
251600110127     C                   END
251700110127      *
251800100910      * -------------
251900090210      *  Volume
252000100319???? C     mea6311       IFEQ      Volume
252100100319???? C     mea6411       ANDEQ     Cubic_Meters
252200090210      *  Controllo se ho valore totale o parziale
252300100630     c                   If        GID_aTotale = 'S'
252400110127???? C                   move      mea6314       mea6314_3                      Volume
252500110127???? C                   Z-ADD     mea6314_3     VABVLB                         Volume
252600090210     C                   ELSE
252700110127???? C                   move      mea6314       mea6314_3                      Volume
252800110127???? C                   ADD       mea6314_3     VABVLB                         Volume
252900090210     C                   END
253000090210     C                   END
253100100705      *
253200100910      * -------------
253300100705     C* Ricerco CAP in base alle dimensioni
253400090210     C     WCAP          IFNE      *BLANKS
253500110202     C     mea6311       IFEQ      Gross_Weight_E
253600090210      *
253700090210      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
253800090210     C                   CLEAR                   FNLV55DS
253900090210     C                   MOVEL     'P'           D55TPT
254000090210     C                   MOVEL     WOGGI         D55DRF
254100090210     C                   MOVEL     VABLNP        D55LNP
254200090210     C                   MOVEL     VABLNP        D55LIN
254300090210     C                   CALL      'FNLV55R'
254400090210     C                   PARM                    FNLV55DS
254500090210     C                   MOVE      D55TFP        COMTFP            3 0
254600090210      *
254700090210     C*  PASSO IL TERMINAL PARTENZA
254800090210     C                   CLEAR                   TISI95DS
254900090210     C                   Z-ADD     COMTFP        I95TFP
255000090210     C                   MOVEL     VABTSP        I95TSP
255100090210     C                   MOVEL     'S'           I95FRE
255200090210     C                   MOVEL     '3'           I95TCN
255300090210     C                   MOVEL     WCAP          I95CAP
255400090210     C                   MOVEL     VABNZD        I95NAR
255500090210     C                   MOVEL     VABLOD        I95LOC
255600090210     C                   MOVEL     VABIND        I95IND
255700090210      *
255800090210     C* Se gestita seconda linea di arrivo passo peso - volume
255900090210     C* e numero colli effettivo
256000090210     C     §PULNA        IFEQ      'S'
256100090210     C                   Z-ADD     VABPKB        I95LKG
256200090210     C                   Z-ADD     VABVLB        I95LMC
256300090210     C                   ELSE
256400100706     C*******
256500090210     C* Altrmenti passo 1 Kg e 0,001
256600100706     C*******              Z-ADD1                  I95LKG
256700100706     C*******              Z-ADD0,001              I95LMC
256800100706     C*******
256900090210     C                   END
257000090210     C*
257100090210     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
257200090210     C                   SELECT
257300090210     C     VABCBO        WHENEQ    '1 '
257400090210     C                   MOVEL     *BLANKS       I95FCA
257500090210     C                   MOVEL     'F'           I95TPO
257600090210     C     VABCBO        WHENEQ    '2 '
257700090210     C                   MOVEL     *BLANKS       I95FCA
257800090210     C                   MOVEL     'A'           I95TPO
257900090210     C     VABCBO        WHENEQ    '4 '
258000090210     C                   MOVEL     'S'           I95FCA
258100090210     C                   MOVEL     'F'           I95TPO
258200090210     C     VABCBO        WHENEQ    '6 '
258300090210     C                   MOVEL     'S'           I95FCA
258400090210     C                   MOVEL     'A'           I95TPO
258500090210     C                   OTHER
258600090210     C                   MOVEL     'F'           I95TPO
258700090210     C     VABCAS        IFGT      0
258800090210     C                   MOVEL     'S'           I95FCA
258900090210     C                   ELSE
259000090210     C                   MOVEL     *BLANKS       I95FCA
259100090210     C                   END
259200090210     C                   ENDSL
259300090210     C*
259400090210     C                   CALL      'TISI95R'
259500090210     C                   PARM                    TISI95DS
259600090210     C* Reperisco i dati da CAP
259700090210     C                   MOVEL     O95PRV        VABPRD
259800100706     C                   MOVEL     WCAP          VABCAD
259900100114      ***
260000100706     c                   MOVEL     O95LNA        VABLNA
260100100706     C                   MOVEL     O95ZNC        VABZNC
260200090210     C*
260300090210     C     O95ERR        IFNE      *BLANKS
260400090210     C     O95FIT        ANDEQ     'S'
260500090210     C*  Cap non trovato
260600090210     C                   eval      vinMSG = 'MEA (CAP) non trovato'
260700110324     c********           exsr      Error_DET
260800110324     C********           goto      end_MEA
260900090210     C                   END
261000090210     C                   END
261100090210      *
261200090210     C                   ELSE
261300090210      *
261400090210     C                   CLEAR                   VABPRD
261500090210     C                   CLEAR                   VABLNA
261600090210     C                   CLEAR                   VABZNC
261700090210     C                   CLEAR                   VABCAD
261800090210     C*  Cap non trovato
261900090210     C                   eval      vinMSG = 'MEA (CAP) mancante'
262000110324     c*******            exsr      Error_DET
262100110324     C*******            goto      end_MEA
262200090210     C                   END
262300090210      *
262400090210     c     end_MEA       endSR
262500090210      * ?================================================================== */
262600090210     C*?  Dati di dettaglio Codice a barre Segnacolli
262700090210      * ?================================================================== */
262800090210     C     DATI_PCI      BEGSR
262900090210      *
263000090210      *
263100090210     c     end_PCI       endSR
263200090211     C*----------------------------------------------------------------
263300090211      *? dati finali del dettaglio    UNT
263400090211     C*----------------------------------------------------------------
263500090211     C     DATI_UNT      BEGSR
263600090211      *
263700110324      *
263800110324     c                   goto      salta
263900110324      *
264000090213     c                   z-add     vnrec         last_RECord
264100090211      **
264200100319      **   verifica la quadratura di tutti i segmenti ricevuti
264300100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
264400100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
264500100728     C     UNT0074       IFne      Conta_segmenti
264600100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
264700100319     c                             trollare il Messaggio ricevuto'
264800100319      *
264900100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
265000100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
265100100921     C                   EVAL      Messaggio = 'EDI -
265200100921     C                             IFCSUM D96A - msg su UPLOAD -
265300100921     C                             con quadratura UNT errata verificare -
265400100921     C                             x il cliente :'
265500100319     C                   EVAL      Messaggio = %trim(Messaggio) +
265600100319     C                                         %editc(§PTksc:'X')
265700100319     C                   EVAL      Messaggio = %trim(Messaggio) +
265800100319     C                             ' i segmenti contati sono :' +
265900100319     C                             %editc(Conta_segmenti:'Z')
266000100319     c                   if        §PVinv <> 'N'
266100100319     c                   exsr      invio_mail
266200100319     c                   end
266300100319     c                   end
266400110324      **
266500110324     c     salta         tag
266600100319      **
266700100319     c     End_UNT       endSR
266800100316     C*----------------------------------------------------------------
266900100316      *? dati finali     UNT
267000100316     C*----------------------------------------------------------------
267100100316     C     DATI_UNZ      BEGSR
267200100316      *
267300100726     c                   move      'S'           Forza_Uscita
267400100316      **
267500100316     c                   endSR
267600090210      *----------------------------------------------------------------
267700090210      *? Input segnacolli su schiera di 20 elementi
267800090210      *----------------------------------------------------------------
267900090210     C     INP_Segnacol  BEGSR
268000090210      *
268100090210     c                   clear                   quanti_PCI        3 0
268200090210     c                   if        §puEL1 = 0
268300090210     c                   z-add     10            quanti_PCI
268400090210     c                   else
268500090210     c                   z-add     §puEL1        quanti_PCI
268600090210     c                   end
268700090210      *
268800090210      *  Pulisce la schiera dei segnacolli da elaborare
268900090210     c                   clear                   x30
269000090210     c                   z-add     0             Nr_x30            3 0
269100090210      *
269200090210      *   riporto tutto sulla schiera generica X30
269300090210      *
269400090210     c                   do        10            limite            3 0
269500090210      * Limita quanti elementi passati in ogni PCI
269600090210      *   devono essere considerati
269700090210     c                   if        limite > quanti_PCI
269800090210     c                   Leave
269900090210     c                   end
270000090210      *
270100100920     c                   IF        tipo_seg = 'GIN'
270200100920      *  Carica la schiera generale di tutti i segnacolli
270300100920     c                   if        GIN_10(limite) <> *blank
270400100920     c                   add       1             Nr_x30
270500100920     c                   eval      X30(Nr_x30) = GIN_10(limite)
270600100920     c                   end
270700100920      *
270800100920     c                   elseIF    tipo_seg = 'PCI'
270900090210      *  Carica la schiera generale di tutti i segnacolli
271000100920     c                   if        PCI_10(limite) <> *blank
271100090210     c                   add       1             Nr_x30
271200100920     c                   eval      X30(Nr_x30) = PCI_10(limite)
271300090210     c                   end
271400100920      *
271500100920     c                   endIF
271600100920      *
271700090210     c                   enddo
271800090210      *
271900090210     C                   ENDSR
272000090210      *----------------------------------------------------------------
272100090210      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
272200090210      *----------------------------------------------------------------
272300100706     C     SEGNaCollo_EEXBEGSR
272400090210      *
272500090210     c                   exsr      INP_Segnacol
272600090210      *
272700090210      *  Loop lettura PCI: fino a che non arrivo a fine schiera (10 elementi)
272800090210     C                   DO        10            x                              --- 01 ---
272900090210      *
273000110131     c                   if        x30(x) <> *blank and x30(x) <> *loval
273100110131     c                             and %subst(x30(x):1:2) <> *blank
273200110131      *
273300090210      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
273400100701     C                   ADD       1             totale_PCI
273500100701     C                   eval       segn_EEX(totale_PCI) = x30(X)
273600090210     C                   END                                                    --- 03 ---
273700090210      *
273800090210     C                   END                                                    --- 02 ---
273900090210      *
274000090210     C                   ENDSR
274100090210     C*----------------------------------------------------------------
274200090210     C*? SGNELA - DECODIFICA elabora nr.segnacollo
274300090210     C*----------------------------------------------------------------
274400100706     C     Segn_BartoliniBEGSR
274500090210      *
274600090210     c                   exsr      INP_Segnacol
274700090210     C*
274800090210     C*  Loop lettura PCI: fino a che non arrivo a fine sch.10 elem.
274900090210     C*  in base al numero elementi validi carico sgn.
275000090210     C                   DO        10            X                              --- 01 ---
275100090210     C*
275200090210     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
275300090210     C     x30(X)        ANDNE     *LOVAL
275400090210     C*
275500100630     C                   CLEAR                   DS_SegnBART
275600090210     C*  Controllo se devo ricevere la linea di partenza
275700090210     C                   MOVEL     'N'           WERRO             1
275800090210     C     §PULP1        IFGT      0                                            --- 03 ---
275900100706     C                   EXSR      REPerisce_LNP
276000090210     C                   END                                                    --- 03 ---
276100100706      *
276200090210     C*  Controllo se devo ricevere la linea di arrivo
276300090210     C     §PULA1        IFGT      0                                            --- 03 ---
276400090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
276500100706     C                   EXSR      REPerisce_LNA
276600090210     C                   END                                                    --- 03 ---
276700100706      *
276800090210     C*  Controllo se devo ricevere il numero di serie
276900090210     C     §PUNR1        IFGT      0                                            --- 03 ---
277000090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
277100100706     C                   EXSR      REPerisce_NRS
277200090210     C                   END                                                    --- 03 ---
277300100706      *
277400090210     C*  Controllo se devo ricevere il numero segnacollo
277500090210     C     §PUSG1        IFGT      0                                            --- 03 ---
277600090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
277700100706     C                   EXSR      REPerisce_SGN
277800090210     C                   END                                                    --- 03 ---
277900100706      *
278000090210     C*  Controllo se devo ricevere la zona di consegna
278100090210     C     §PUZN1        IFGT      0                                            --- 03 ---
278200090210     C     WERRO         ANDEQ     'N'                                          --- 03 ---
278300100706     C                   EXSR      REPerisce_ZNC
278400090210     C                   END                                                    --- 03 ---
278500100706      *
278600090210     C*  Imposto da VAB i dati a zero se mi è stato passato
278700090210     C*  numero segnacollo valido
278800090210     C     WERRO         IFEQ      'N'                                          --- 03 ---
278900100706      *
279000100630     C     SegnBART_LNP  IFEQ      0                                            --- 04 ---
279100100630     C                   Z-ADD     VABLNP        SegnBART_LNP
279200090210     C                   END                                                    --- 04 ---
279300100706      *
279400100630     C     SegnBART_LNA  IFEQ      0                                            --- 04 ---
279500100630     C                   Z-ADD     VABLNA        SegnBART_LNA
279600090210     C                   END                                                    --- 04 ---
279700100706      *
279800100630     C     SegnBART_NRS  IFEQ      0                                            --- 04 ---
279900100630     C                   Z-ADD     VABNRS        SegnBART_NRS
280000090210     C                   END                                                    --- 04 ---
280100100706      *
280200100630     C     SegnBART_ZNC  IFEQ      0                                            --- 04 ---
280300100630     C                   Z-ADD     VABZNC        SegnBART_ZNC
280400090210     C                   END                                                    --- 04 ---
280500090210      *
280600090210     C*  mi reimposto LNA e zona da segnacollo
280700100114      ***
280800100114      *** dovendo accorpare le spedizioni da Conferma x CMR
280900100114      ***  non deve impostare l'instradamento.
281000100630     C                   Z-ADD     SegnBART_LNA  VABLNA
281100100630     C                   Z-ADD     SegnBART_ZNC  VABZNC
281200100114      ***
281300090210     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
281400100701     C                   ADD       1             Conta_Segn
281500100701     C                   eval       segn_BAR(Conta_Segn) = SegnBART_NSC
281600090210     C                   END                                                    --- 03 ---
281700090210     C*
281800090210     C                   END                                                    --- 02 ---
281900090210     C*
282000090210     C                   END                                                    --- 01 ---
282100090210      *
282200090210     C                   ENDSR
282300090210     C*----------------------------------------------------------------
282400090210     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
282500090210     C*----------------------------------------------------------------
282600100706     C     REPerisce_LNP BEGSR
282700090210     C*
282800090210     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
282900090210     C*  (a partire dalla posizione iniziale indicata in tabella)
283000090210     C*  dal numero segnacollo passatomi dal partner
283100090210     C                   Z-ADD     §PULP1        WP                2 0
283200090210     C     3             SUBST     x30(X):WP     WLNP              3
283300090210     C*
283400090210     C*  controllo che la lnp calcolata sia numerica
283500090210     C                   TESTN                   WLNP                 98
283600090210     C*
283700090210     C*  se è numerica la carico
283800090210     C     *IN98         IFEQ      '1'
283900100630     C                   MOVE      WLNP          SegnBART_LNP
284000090210     C                   ELSE
284100090210     C*  se non è numerica stampo errore
284200090210     C                   MOVEL     'S'           WERRO             1
284300090210     C                   eval      vinMSG = 'PCI (LNP) non numerica'
284400090213     c                   exsr      Error_DET
284500090210     C                   goto      end_LNP
284600090210     C                   END
284700090210     C*  se la lnp non è uguale a quella calcolata per EDiVAB
284800090210     C*  lo segnalo e prendo per buona la lnp del EDiVAB
284900100630     C     SegnBART_LNP  IFNE      VABLNP
285000090210     C                   MOVEL     'S'           WERRO             1
285100090210     C                   eval      vinMSG = 'PCI (LNP) segnacollo non = a quell-
285200090210     c                             a della Bolla'
285300090213     c                   exsr      Error_DET
285400090210     C                   goto      end_LNP
285500090210     C                   END
285600090210     C*
285700090210     C     end_LNP       ENDSR
285800090210     C*----------------------------------------------------------------
285900090210     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
286000090210     C*----------------------------------------------------------------
286100100706     C     REPerisce_LNA BEGSR
286200090210     C*
286300090210     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
286400090210     C*  (a partire dalla posizione iniziale indicata in tabella)
286500090210     C*  dal numero segnacollo passatomi dal partner
286600090210     C                   Z-ADD     §PULA1        WP                2 0
286700090210     C     3             SUBST     x30(X):WP     WLNA              3
286800090210     C*  controllo che la lna calcolata sia numerica
286900090210     C                   TESTN                   WLNA                 98
287000090210     C*  se è numerica la carico
287100090210     C     *IN98         IFEQ      '1'
287200100630     C                   MOVE      WLNA          SegnBART_LNA
287300090210     C                   ELSE
287400090210     C*  se non è numerica stampo errore
287500090210     C                   MOVEL     'S'           WERRO             1
287600090210     C                   eval      vinMSG = 'PCI (LNA) non numerica'
287700090213     c                   exsr      Error_DET
287800090210     C                   goto      end_LNA
287900090210     C                   END
288000090210     C*
288100090210     C     end_LNA       ENDSR
288200090210     C*----------------------------------------------------------------
288300090210     C*? REPNRS - Reperisce numero serie
288400090210     C*----------------------------------------------------------------
288500100706     C     REPerisce_NRS BEGSR
288600090210     C*
288700090210     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
288800090210     C*  (a partire dalla posizione iniziale indicata in tabella)
288900090210     C*  dal numero segnacollo passatomi dal partner
289000090210     C                   Z-ADD     §PUNR1        WP                2 0
289100090210     C     2             SUBST     x30(X):WP     WNRS              2
289200090210     C*  controllo che il nrs calcolata sia numerico
289300090210     C                   TESTN                   WNRS                 98
289400090210     C*  se è numerica la carico
289500090210     C     *IN98         IFEQ      '1'
289600100630     C                   MOVE      WNRS          SegnBART_NRS
289700090210     C                   ELSE
289800090210     C*  se non è numerico stampo errore
289900090210     C                   MOVEL     'S'           WERRO             1
290000090210     C                   eval      vinMSG = 'PCI (NRS) non numerica'
290100090213     c                   exsr      Error_DET
290200090210     C                   goto      end_NRS
290300090210     C                   END
290400090210      *
290500090210     C*  se il nrs non è uguale a quella calcolata per EDiVAB
290600090210     C*  lo segnalo e prendo per buona il nrs del segnacollo
290700100630     C     SegnBART_NRS  IFNE      VABNRS
290800090210     C                   MOVEL     'S'           WERRO             1
290900090210     C                   eval      vinMSG = 'PCI (NRS) segnacollo non = a quell-
291000090210     c                             o della Bolla'
291100090213     c                   exsr      Error_DET
291200090210     C                   goto      end_NRS
291300090210     C                   END
291400090210     C*
291500090210     C     end_NRS       ENDSR
291600090210     C*----------------------------------------------------------------
291700090210     C*? REPZNC - Reperisce zona consegna
291800090210     C*----------------------------------------------------------------
291900100706     C     REPerisce_ZNC BEGSR
292000090210     C*
292100090210     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
292200090210     C*  (a partire dalla posizione iniziale indicata in tabella)
292300090210     C*  dal numero segnacollo passatomi dal partner
292400090210     C                   Z-ADD     §PUZN1        WP                2 0
292500090210     C     2             SUBST     x30(X):WP     WZNC              2
292600090210     C*  controllo che la zona calcolata sia numerica
292700090210     C                   TESTN                   WZNC                 98
292800090210     C*  se è numerica la carico
292900090210     C     *IN98         IFEQ      '1'
293000100630     C                   MOVE      WZNC          SegnBART_ZNC
293100090210     C                   ELSE
293200090210     C*  se non è numerica stampo errore
293300090210     C                   MOVEL     'S'           WERRO             1
293400090210     C                   eval      vinMSG = 'PCI (ZNC) non numerica'
293500090213     c                   exsr      Error_DET
293600090210     C                   goto      end_ZNC
293700090210     C                   END
293800090210     C*
293900090210     C     end_ZNC       ENDSR
294000090210     C*----------------------------------------------------------------
294100090210     C*? REPSGN - Reperisce numero segnacollo
294200090210     C*----------------------------------------------------------------
294300100706     C     REPerisce_SGN BEGSR
294400090210     C*
294500090210     C*  se viene passata nr.segnacollo estraggo sottostringa
294600090210     C*  lunga 7 chr (a partire dalla posizione iniziale
294700090210     C*  indicata in tabella)
294800090210     C*  dal numero segnacollo passatomi dal partner
294900090210     C                   Z-ADD     §PUSG1        WP                2 0
295000090210     C     7             SUBST     x30(X):WP     WSGN              7
295100090210     C*  controllo che il nr.segnacollo calcolato sia numerico
295200090210     C                   TESTN                   WSGN                 98
295300090210     C*  se è numerico lo carico
295400090210     C     *IN98         IFEQ      '1'
295500100630     C                   MOVE      WSGN          SegnBART_NSC
295600090210     C                   ELSE
295700090210     C*  se non è numerica stampo errore
295800090210     C                   MOVEL     'S'           WERRO             1
295900100630     C                   eval      vinMSG = 'PCI segnacollo non numerico'
296000090213     c                   exsr      Error_DET
296100090210     C                   goto      end_SGN
296200090210     C                   END
296300090210     C*
296400090210     C     end_SGN       ENDSR
296500051205      *----------------------------------------------------*
296600051205      *   DEFINIZIONE CHIAVI                               *
296700051205      *----------------------------------------------------*
296800051205     C     *INZSR        BEGSR
296900051205      *
297000991129     c     *ENTRY        PLIST
297100060613     C                   parm                    esito             1
297200971215      *
297300050112     C     KTABel        KLIST
297400050112     C                   KFLD                    TBLKUT
297500050112     C                   KFLD                    TBLCOD
297600050112     C                   KFLD                    TBLKEY
297700090210      *
297800090210     C     K_TAB1L       KLIST
297900090213     C                   KFLD                    etbUNB
298000090213     C                   KFLD                    etbSGM
298100090213     C                   KFLD                    etbVALSGM
298200090209     C*
298300090209     C* Definisco chiavi di accesso
298400090209     C     KTAB1         KLIST
298500090209     C                   KFLD                    KKUT
298600090209     C                   KFLD                    KCOD
298700090209     C*
298800090209     C     KNUF          KLIST
298900090209     C                   KFLD                    KAAA
299000090209     C                   KFLD                    KCNU
299100090209     C                   KFLD                    KFIL
299200090209      *
299300090209     C     KVAB1         KLIST
299400090209     C                   KFLD                    KCCM
299500090209     C                   KFLD                    KCMR
299600090209     C                   KFLD                    KCNT
299700090209     C                   KFLD                    KAAS
299800090209     C                   KFLD                    KLNP
299900090209     C                   KFLD                    KNRS
300000090209     C                   KFLD                    KNSP
300100090209      *
300200090209     C     KVAB2         KLIST
300300090209     C                   KFLD                    KCCM
300400090209     C                   KFLD                    KCMR
300500090209      *
300600090209     C     KRDE          KLIST
300700090209     C                   KFLD                    KAAS
300800090209     C                   KFLD                    KLNP1
300900090209     C                   KFLD                    KNRS
301000090209     C                   KFLD                    KNSP
301100090209     C                   KFLD                    KNMC
301200090209     C                   KFLD                    KNRP
301300090209      *
301400090209     C     KACO          KLIST
301500090209     C                   KFLD                    KKUT
301600090209     C                   KFLD                    KKCC
301700090209     C                   KFLD                    KKSC
301800090209      *
301900090209     C     KIND          KLIST
302000090209     C                   KFLD                    KKUT
302100090209     C                   KFLD                    KKCC
302200090209     C                   KFLD                    KKSC
302300090209      *
302400090209      * DETERMINO SE SONO BARTOLINI O SDI
302500090209     C                   CLEAR                   TIBS55DS
302600090209     C                   MOVEL     'L'           I50TLA
302700090209     C                   CALL      'TIBS55R'
302800090209     C                   PARM                    TIBS55DS
302900090209      *
303000090209     C* RECUPERO DATI DELL'UTENTE
303100090209     C                   Z-ADD     1             CODUT             1 0
303200090209     C                   CALL      'XPARUT'
303300090209     C                   PARM                    UTEDSE0F
303400090209     C                   MOVEL     RAGUT         RSUT             20
303500090209     C*
303600090209     C* RICERCA CAPOCONTI
303700090209     C                   DO        50            X
303800090209     C                   MOVE      TCU(X)        TCUDS
303900090209     C     F56           IFEQ      'CG'
304000090209     C     F34           ANDEQ     '01'
304100090209     C                   Z-ADD     KCU(X)        KCI               4 0
304200090209     C                   END
304300090209     C                   END
304400090209     C*
304500090211     c                   movel(p)  'EDPAB'       User_Msg         10
304600090211     C*
304700090209     C* IMPOSTO A ZERO VARIABILI DI PGM
304800090209     c                   move      *blank        Snd_Msg_alert     1
304900090209     C                   MOVEL     CA(1)         WCA              78
305000090209     C                   MOVEL     CN(1)         WCN              78
305100090209     C*
305200090209     C* RECUPERO DATA E ORA
305300100630     C                   TIME                    Ora_Data         14 0
305400100630     C                   MOVE      Ora_Data      G02DAT
305500100630     C                   MOVE      Ora_Data      WDATA8            8 0
305600100630     C                   MOVE      WDATA8        Anno2             2 0
305700090209     C                   MOVEL     WDATA8        DATA              6 0
305800100630     C                   MOVE      Anno2         DATA
305900090209     C                   MOVEL     *BLANK        G02ERR
306000090209     C                   CALL      'XSRDA8'
306100090209     C                   PARM                    WLBDA8
306200090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
306300100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
306400090209     C                   TIME                    WORA              6 0
306500090209      * Recupero data e ora
306600090209     C                   TIME                    W0140
306700090209      * UDATE IN GGMMAAAA
306800100630     C                   MOVE      W0140         DataGGMMAAA
306900090209      * UDATE IN AAAAMMGG
307000100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
307100090209     C     *iso          MOVEL     DATA_eur      dateu
307200090209      *
307300090209      * ?              /*--------------------------- */
307400090209     c                   exsr      CARICA_TABELLE
307500090209      * ?              /*--------------------------- */
307600090209      *
307700090209     C                   MOVEL     *ALL'-'       CMP132          132
307800991124      *
307900050414      *------------------
308000991124     C                   ENDSR
308100060621      * ?------------------------------------------------------------------ */
308200090209      *?    CARICA TABELLE SU SCHIERE
308300060621      * ?------------------------------------------------------------------ */
308400090209     C     CARICA_TABELLEBEGSR
308500090205      *
308600090209     C* Caricamento Tabella 1B
308700090209     C                   Z-ADD     0             X                 4 0
308800090209     C                   Z-ADD     CODUT         KKUT
308900090209     C                   MOVEL     '1B'          KCOD
309000090209     C     KTAB1         CHAIN     TABEL00F                           30
309100090209     C     *IN30         DOWEQ     '0'
309200090209     C     TBLFLG        IFEQ      *BLANKS
309300090209     C                   ADD       1             X
309400100701     C                   MOVEL     TBLKEY        Cod_Tb_CTM(X)
309500100701     C                   MOVEL     TBLUNI        Des_Tb_CTM(X)
309600090209     C                   END
309700090209     C     KTAB1         READE     TABEL00F                               30
309800090209     C                   END
309900090209      *
310000090209     C* Caricamento Tabella 15
310100090209     C                   Z-ADD     0             X                 4 0
310200090209     C                   Z-ADD     CODUT         KKUT
310300090209     C                   MOVEL     '15'          KCOD
310400090209     C     KTAB1         CHAIN     TABEL00F                           30
310500090209     C     *IN30         DOWEQ     '0'
310600090209     C     TBLFLG        IFEQ      *BLANKS
310700090209     C                   ADD       1             X
310800090209     C                   MOVEL     TBLKEY        C15(X)
310900090209     C                   MOVEL     TBLUNI        DS15
311000090209     C                   MOVEL     §15COD        ISO(X)
311100090209     C                   MOVEL     §15ECF        CPF(X)
311200090209     C                   MOVE      §15PCF        CPF(X)
311300090209     C                   END
311400090209     C     KTAB1         READE     TABEL00F                               30
311500090209     C                   END
311600090209      *
311700090209     C* Caricamento Tabella CV
311800090209     C                   Z-ADD     0             X
311900090209     C                   Z-ADD     CODUT         KKUT
312000090209     C                   MOVEL     'CV'          KCOD
312100090209     C     KTAB1         CHAIN     TABEL00F                           30
312200090209     C     *IN30         DOWEQ     '0'
312300090209     C     TBLFLG        IFEQ      *BLANKS
312400090209     C                   ADD       1             X
312500090209     C                   MOVEL     TBLKEY        CCV(X)
312600090209     C                   MOVEL     TBLUNI        DCV(X)
312700090209     C                   END
312800090209     C     KTAB1         READE     TABEL00F                               30
312900090209     C                   END
313000090216      *
313100100702     C* Caricamento Tabella Priorità trasporto
313200100702     C                   Z-ADD     0             X
313300100702     C                   MOVEL     'TS'          TABCOD
313400100702     C     TABCOD        CHAIN     EDTAB01L                           30
313500100702     C     *IN30         DOWEQ     '0'
313600100702     C     TABFLG        IFEQ      *BLANKS
313700100702     C                   ADD       1             X
313800100702     C                   eval      TipoServizio(X) = TABUNI
313900100705     C                   eval      Key_TipServ(X)  = TABKEY
314000100702     C                   END
314100100702     C     TABCOD        READE     EDTAB01L                               30
314200100702     C                   END
314300100702      *
314400110328      * Caricamento Tabella termini consegna
314500110328     C                   Z-ADD     0             X
314600110328     C                   MOVEL     'TB'          TABCOD
314700110328     C     TABCOD        CHAIN     EDTAB01L                           30
314800110328     C     *IN30         DOWEQ     '0'
314900110328     C     TABFLG        IFEQ      *BLANKS
315000110328     C                   ADD       1             X
315100110328     C                   eval      K35_TpBolla(X) = TABKEY
315200110328     C                   eval      Cod_TpBolla(X) = TABKEY
315300110328     C                   eval      Decod_Porto(X) = TABUNI
315400110328     C                   END
315500110328     C     TABCOD        READE     EDTAB01L                               30
315600110328     C                   END
315700110328      *
315800090216     C* Caricamento Tabella Tipo Incasso C/Assegno
315900090216     C                   Z-ADD     0             X
316000090216     C                   MOVEL     'TC'          TABCOD
316100090216     C     TABCOD        CHAIN     EDTAB01L                           30
316200090216     C     *IN30         DOWEQ     '0'
316300090216     C     TABFLG        IFEQ      *BLANKS
316400090216     C                   ADD       1             X
316500100705     C                   eval      K35_TpIncas(X) = TABKEY
316600100705     C                   eval      TipoIncasso(X) = TABUNI
316700090216     C                   END
316800090216     C     TABCOD        READE     EDTAB01L                               30
316900090216     C                   END
317000090209      *
317100090209      * Caricamento Tabella Partner esteri
317200090209     C                   Z-ADD     0             X
317300090209     C                   MOVEL     'PT'          TABCOD
317400090209     C     TABCOD        CHAIN     EDTAB01L                           30
317500090209     C     *IN30         DOWEQ     '0'
317600090209      *
317700090209     C                   SELECT
317800090209     C     TABFLG        WHENNE    *BLANKS
317900090209      *
318000090209     C                   OTHER
318100090209     C                   ADD       1             X
318200100630     C                   MOVEL     TABKEY        PT_key(X)
318300100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
318400100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
318500100630     C                   MOVEL     TABUNI        PT_uni(X)
318600090209     C                   ENDSL
318700090209      *
318800090209     C     TABCOD        READE     EDTAB01L                               30
318900090209     C                   END
319000121105      *
319100121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
319200121105     c                   exsr      ChecK_PT
319300121105      *
319400090209      * salva quanti Codici UNB ha caricato
319500100630     c                   z-add     x             PT_KeyXsav
319600090209      *
319700090209     C                   MOVEL     'PU'          TABCOD
319800090209     C     TABCOD        CHAIN     EDTAB01L                           30
319900090209     C     *IN30         DOWEQ     '0'
320000090209      *
320100090209     C                   SELECT
320200090209     C     TABFLG        WHENNE    *BLANKS
320300090209      *
320400090209     C                   OTHER
320500100630     C                   MOVEL     TABKEY        PU_key
320600090209     C                   Z-ADD     1             X
320700100630     C     PU_key        LOOKUP    PT_key(X)                              32
320800100630     C   32              MOVEL     TABUNI        PU_uni(X)
320900090209     C                   ENDSL
321000090209      *
321100090209     C     TABCOD        READE     EDTAB01L                               30
321200090209     C                   END
321300090209      *
321400090209      * Prosegue tab.PT e PU
321500090209     C                   MOVEL     'PV'          TABCOD
321600090209     C     TABCOD        CHAIN     EDTAB01L                           30
321700090209     C     *IN30         DOWEQ     '0'
321800090209      *
321900090209     C                   SELECT
322000090209     C     TABFLG        WHENNE    *BLANKS
322100090209      *
322200090209     C                   OTHER
322300100630     C                   MOVEL     TABKEY        PV_key
322400090209     C                   Z-ADD     1             X
322500100630     C     PV_key        LOOKUP    PT_key(X)                              32
322600100630     C   32              MOVEL     TABUNI        PV_uni(X)
322700090209     C                   ENDSL
322800090209      *
322900090209     C     TABCOD        READE     EDTAB01L                               30
323000090209     C                   END
323100090209      *
323200090209     C* Caricamento Tabella codici clienti - tariffe
323300090209     C                   Z-ADD     0             X
323400090209     C                   MOVEL     'CL'          TABCOD
323500090209     C     TABCOD        CHAIN     EDTAB01L                           30
323600090209     C     *IN30         DOWEQ     '0'
323700090209     C     TABFLG        IFEQ      *BLANKS
323800090209     C                   ADD       1             X
323900100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
324000100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
324100090209     C                   END
324200090209     C     TABCOD        READE     EDTAB01L                               30
324300090209     C                   END
324400090209      *
324500090209     C* Caricamento Serivizi speciali
324600090209     C                   Z-ADD     0             X
324700090209     C                   MOVEL     'SS'          TABCOD
324800090209     C     TABCOD        CHAIN     EDTAB01L                           30
324900090209     C     *IN30         DOWEQ     '0'
325000090209     C     TABFLG        IFEQ      *BLANKS
325100090209     C                   ADD       1             X
325200100702     C                   eval       Key_ServSpec(X) = TABKEY
325300100702     C                   eval       SpecialServ(X)  = TABKEY
325400100702     C                   eval       UNI_ServSpec(X) = TABUNI
325500090209     C                   END
325600090209     C     TABCOD        READE     EDTAB01L                               30
325700090209     C                   END
325800090209      *
325900090209     C                   ENDSR
326000090205      * ?------------------------------------------------------------------ */
326100090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
326200090205      * ?------------------------------------------------------------------ */
326300090205     C     *pssr         BEGSR
326400090205     C
326500060710     C                   eval      esito = '2'
326600100921???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFCSUM 96'
326700100921     C                   EVAL      Messaggio = 'EDI -
326800100921     C                             IFCSUM D96A - messaggio -
326900090603     C                             EDI ricevuto errato su UPLOAD.'
327000100319     c                   if        §PVinv <> 'N'
327100100319     c                   exsr      invio_mail
327200100319     c                   end
327300060621     C
327400090213     C                   ENDSR
327500090213      * ?------------------------------------------------------------------ */
327600090213      *?      Segnala Errore su TIVIN
327700090213      * ?------------------------------------------------------------------ */
327800090213     C     Error_DET     BEGSR
327900090213     C
328000100728     c                   eval      Segmento_in_errore = Conta_segmenti
328100090213     C                   eval      vinFLG = '2'
328200090213     c                   eval      Almeno_Un_Due ='S'
328300090213     c                   eval      esito  = '1'
328400090213     c                   eval      se_errore ='S'
328500090213     C                   eval      Err_in_detta = 'S'
328600090213     C
328700090213     C                   ENDSR
328800100705      * ?------------------------------------------------------------------ */
328900100705      *?   Controlla e decodifica Valori VALUE dei segmenti
329000100705      * ?------------------------------------------------------------------ */
329100100705     C     In_TAB_EDSTBL BEGSR
329200100705      *
329300100705      *   Cerca la decodifica del dato in TABELLA
329400100705     c                   clear                   trovata_EDSTBL    1
329500100705     c                   clear                   quale_campo       6
329600100705     c                   clear                   campo_dati_70A   70
329700100705     C                   movel     UNB_Mittente  etbUNB
329800100705     c     K_TAB1L       chain     edstbl01l
329900100705      *
330000100705      * prova quindi senza l'UNB specifico del cliente
330100100705     c                   if        not %Found(edstbl01l)
330200100705     C                   clear                   etbUNB
330300100705     c     K_TAB1L       chain     edstbl01l
330400100705     c                   end
330500100705      *
330600100705      *  Traduce i testi descrittivi se PRESENTE in tabella
330700100705      *   il campo di destinazione è definito sulla destra della descrizione
330800100705      *     ed è lungo 6 come i nomi definiti nei VAB.
330900100705     c                   if        %Found(edstbl01l)
331000100705     c                   move(p)   etbDESCRI     quale_campo
331100100705     c                   movel(p)  etbDATI       campo_dati_70A
331200100705     c                   move      'S'           trovata_EDSTBL
331300100705     c                   end
331400100705      *
331500100705      *
331600100705     C                   ENDSR
331700100705     c*==================================================================*
331800090209     C*? CNVDAT - DECODIFICA LA DATA
331900090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
332000090209     C*         - WDATA  (DATA ALFANUMERICA)
332100090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
332200090209     C*         - WHMS   (ORA NUMERICA)
332300090209     C*----------------------------------------------------------------
332400100702     C     Converte_Data BEGSR
332500090209     C*
332600090209     C                   MOVEL     ' '           WERRO             1
332700090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
332800090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
332900100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
333000090209     C                   Z-ADD     *ZEROS        WHMS              6 0
333100090209     C*
333200100705     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
333300100702     C                   MOVEL     Work_Data__35 WCOMO8            8
333400090209     C                   TESTN                   WCOMO8               99
333500090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
333600090209     C  N99              MOVEL     'S'           WERRO             1
333700090209     C                   END
333800090209     C*
333900100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
334000100702     C                   MOVEL     Work_Data__35 WCOMO12          12
334100100120     C                   TESTN                   WCOMO12              99
334200100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
334300100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
334400100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
334500090209     C  N99              MOVEL     'S'           WERRO             1
334600090209     C                   END
334700100120     C*
334800100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
334900100702     C                   MOVEL     Work_Data__35 WCOMO14          14
335000100120     C                   TESTN                   WCOMO14              99
335100100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
335200100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
335300100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
335400100120     C  N99              MOVEL     'S'           WERRO             1
335500100120     C                   END
335600090209     C*
335700090209     C                   ENDSR
335800090210     C*----------------------------------------------------------------
335900100701     C*? Se_Numerico       - CONTROLLO NUMERICO
336000100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
336100100701     C*         - lunghezza_num   (Lunghezza campo output)
336200100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
336300090210     C*----------------------------------------------------------------
336400100701     C     Se_Numerico   BEGSR
336500090210     C*
336600090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
336700100701     C                   MOVEL     'N'           Campo_Numerico    1
336800100701     C                   MOVEL     *BLANKS       Numero_OK        15
336900090210     C                   Z-ADD     15            IN                3 0
337000100701     C                   Z-ADD     lunghezza_num IX                3 0
337100090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
337200100701     C                   MOVEA     test_num      NDA
337300090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
337400090210     C                   MOVEA     *ALL'0'       NDN
337500100630      *
337600090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
337700100701     C                   Z-ADD     lunghezza_num I                 3 0
337800090210     C     I             DOWGT     0                                            --- 1 -->
337900090210     C* ESTRAE IL CARATTERE DA ESAMINARE
338000100701     C     1             SUBST     test_num:I    WTEM01            1
338100090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
338200090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
338300090210     C     WTEM01        SCAN      WCA                                    99
338400090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
338500090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
338600090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
338700090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
338800090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
338900090210     C* DA DESTRA VERSO SINISTRA
339000090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
339100090210     C                   MOVEL     WTEMP1        NDN(IN)
339200090210     C                   SUB       1             IN
339300090210     C                   END                                                    <-- 3 ---
339400090210     C                   END                                                    <-- 2 ---
339500090210     C                   SUB       1             I
339600090210     C                   END                                                    <-- 1 ---
339700090210     C*
339800090210     C                   MOVEA     NDN           WNDN             15
339900090210     C     WNDN          IFNE      *ZEROS
340000100701     C                   MOVEA     NDN           Numero_OK        15
340100100701     C                   MOVEL     'S'           Campo_Numerico    1
340200090210     C                   END
340300090210     C*
340400090210     C                   ENDSR
340500090210     C*----------------------------------------------------------------
340600090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
340700090210     C*----------------------------------------------------------------
340800100705     C     Converte_CAP  BEGSR
340900090210     C*
341000090210     C                   MOVEL     *BLANKS       WCAP              9
341100090210     C     nad3251       IFNE      '0'
341200090210     C     '  '          SCAN      nad3251       LENGHT            2 0
341300090210     C                   SUB       1             LENGHT
341400090210     C     LENGHT        IFLE      5
341500090210     C     LENGHT        ANDGT     0
341600090210     C                   Z-ADD     LENGHT        T                 2 0
341700090210     C                   Z-ADD     5             S                 2 0
341800090210     C                   MOVEA     nad3251       CAP9
341900090210     C                   MOVEL     *ZEROS        CAP5
342000090210     C                   DO        LENGHT
342100090210     C                   MOVE      CAP9(T)       CAP5(S)
342200090210     C                   SUB       1             S
342300090210     C                   SUB       1             T
342400090210     C                   END
342500090210     C                   MOVEA     CAP5          WCAP5             5
342600090210     C     WCAP5         IFEQ      '0'
342700090210     C                   MOVEL     *BLANKS       WCAP
342800090210     C                   ELSE
342900090210     C                   MOVEA     CAP5          WCAP
343000090210     C                   END
343100090210     C*
343200090210     C                   ELSE
343300090210     C                   MOVEL     nad3251       WCAP
343400090210     C                   END
343500090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
343600090210     C     WCAP          IFNE      *BLANKS
343700090210     C     WFLCPF        ANDNE     0
343800090210     C                   MOVEL     WFLCPF        WELE              1 0
343900090210     C                   MOVE      WFLCPF        WPOS              1 0
344000090210     C                   MOVEL     *BLANK        WCPF
344100090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
344200090210     C                   ELSE
344300090210     C                   MOVEL     *BLANKS       WCPF
344400090210     C                   END
344500090210     C                   END
344600090210     C*
344700090210     C                   ENDSR
344800090211      * ?================================================================== */
344900090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
345000090211      * ?================================================================== */
345100090211     C     Scrive_VAB    BEGSR
345200090211      *
345300090211     c                   clear                   err_bloccante     1
345400090211     c                   move      'S'           Almeno_Uno
345500090211      *
345600090211     **  Imposta i campi del VAB e scrive EDIVAB/VAT/VAD
345700090211     c                   exsr      UPDVAB
345800090211      *
345900090211     C* >>>>>>>>>>>>
346000090211     C*  Richiamo pgm per scrittura immediata VAB
346100090211     C     §PUWRK        IFEQ      ' '
346200090211      *
346300090211     C*  Richiamo pgm per esecuzione stampa
346400110110     C**** §PTCMR        IFEQ      'S'
346500110110     c******             MOVEL     BGM_nrCMR     d86CMR
346600110110     c******             if            RFF_NrCMR <> *Blank
346700110110     C******             MOVEL     RFF_NrCMR     d86CMR
346800110110     c******             end
346900110110     C******             ELSE
347000110110     C******             MOVEL     BGM_NrFviaggiod86CMR
347100110110     c******             if          RFF_NrFviaggio <> *Blank
347200110110     C******             MOVEL     RFF_NrFviaggiod86CMR
347300110110     c******             end
347400110110     C******             END
347500110110      *
347600110110      * Deve impostare lo stesso campo appena scritto sul VABCMR
347700110110     c                   MOVEL     vabCMR        d86CMR
347800090211      *
347900090213     C                   MOVEL     ' '           d86RIE
348000090213     C                   MOVEL     §PUDTA        d86DTA
348100090213     C                   Z-ADD     1             d86CNT
348200090213     C                   Z-ADD     §PTKSC        d86CCM
348300090213     C                   MOVEL     *BLANKS       d86STA
348400090213     C                   MOVEL     'E'           d86MOD
348500090211      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
348600090211      * dove poi imposta la DS dei parametri passati
348700090213     C                   MOVEL     'LR'          d86CHI
348800090213     C                   MOVEL     'N'           d86CTL
348900090211      *
349000090211     c                   move      kpjbu         SvKpjbu
349100090211     c                   clear                   kpjbu
349200090216     C                   movel     *on           Commit_on         1
349300090213     C                   MOVEL     TRTC86ds      KPJBU
349400090213      * ?- Chiamata la TRTC86R2 -*
349500090213     C                   CALL      'TRTC86R2'
349600090211     C                   PARM                    KPJBA
349700090216     C                   PARM                    Commit_on
349800090211     c                   move      Svkpjbu       Kpjbu
349900090211      *
350000090211     C*  Controllo pgm per scrittura immediata VAB
350100100630     C     sk            IFNE      0
350200100630     C                   DO        sk            sx
350300100630     C                   Z-ADD     skCLienti(sx) d86CCM
350400090211      *
350500090211     c                   move      kpjbu         SvKpjbu
350600090211     c                   clear                   kpjbu
350700090216     C                   movel     *on           Commit_on         1
350800090213     C                   MOVEL     TRTC86ds      KPJBU
350900090213      * ?- Chiamata la TRTC86R2 -*
351000090213     C                   CALL      'TRTC86R2'
351100090211     C                   PARM                    KPJBA
351200090216     C                   PARM                    Commit_on
351300090211     c                   move      Svkpjbu       Kpjbu
351400090211     C                   END
351500090211     C                   END
351600090211      *
351700090211     C                   END
351800090211      * >>>>>>>>>>>>
351900090211     C                   if        se_errore  = 'S'
352000090211     c                   move      'S'           err_bloccante
352100090211     c                   goto      Error_VAB
352200090211     c                   end
352300090211     C*
352400090211     C     Error_VAB     Endsr
352500090211     C*----------------------------------------------------------------
352600090211     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
352700090211     C*----------------------------------------------------------------
352800090211     C     UPDVAB        BEGSR
352900090211      *
353000090211     C*  Se non è stato impostato il codice bolla lo imposto io
353100090211     C     VABCBO        IFEQ      *BLANKS
353200090211     C     VABCAS        IFGT      0
353300090211     C                   MOVEL     '4'           VABCBO                         + C/Ass
353400090211     C                   ELSE
353500090211     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
353600090211     C                   END
353700090211     C                   END
353800100701      * Imposta le NOte
353900100701     C     VABNOT        IFEQ      *BLANKS
354000100701     C                   MOVEL     Note_1        VABNOT
354100100701     C                   MOVEL     Note_2        VABNT2
354200100701     C                   ELSE
354300100701     C                   MOVEL     Note_1        VABNT2
354400100701     C                   END
354500090211      *
354600090211     C*  Attribuisco anno spedizione
354700100630     C     DTM_DtParten  IFGT      0                                            anno sped.
354800100630     C                   MOVEL     DTM_DtParten  VABAAS
354900090211     C                   ELSE
355000100630     C     DTM_DtFViag   IFGT      0
355100100630     C                   MOVEL     DTM_DtFViag   VABAAS                         anno sped.
355200090211     C                   END
355300090211     C                   END
355400090211      *
355500090211      *  Controllo dettaglio segnacolli
355600090211     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
355700100630     C                   EXSR      CTRsegn_EEX
355800090211     C                   ELSE
355900090211     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
356000090211     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
356100100630     C                   EXSR      CTRsegn_BAR
356200090211     C                   END
356300090211     C                   END
356400090211      *
356500090212      *  Imposto linea partenza e serie
356600100701     C                   eval      VABLNP = PT_vabLNP
356700100701     C                   eval      VABNRS = PT_vabNRS
356800090211      *
356900090211      *  Controllo se devo variare il codice trattamento merce
357000100701     C                   eval      VABCTM = PT_vabCTM
357100090211      *
357200090211      * Controllo se deve mantenere il numero bolla passato dal messaggio
357300090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
357400090211     c                   clear                   mantiene_nsp      1            *blk/'S'
357500100701     c                   eval      mantiene_nsp = PU_HLD_SpedVAX                *blk/'S'
357600090211      *
357700090211      * (Se esistono delle Particolarità sul cliente prioritariamente prende
357800090211      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
357900090211      *
358000090211      *  Imposto codice cliente
358100090211     c                   Select
358200090211      *
358300100319      *  Specificato codice da qualificatore RFF+FF dal dettaglio
358400100319     C                   When        Det_RFF_FF_ksc  <> *zeros
358500100319     C                   Z-ADD     Det_RFF_FF_kscVABCCM
358600100319     C                   MOVEL     Det_RFF_FF_tarVABCTR
358700090211      * forza LNP/CTM/NRS
358800100319     c                   if        Det_RFF_FF_lnp <> 0
358900100319     C                   eval      VABLNP = Det_RFF_FF_lnp
359000090211     c                   end
359100100319     c                   if        Det_RFF_FF_nrs <> 0
359200100319     C                   eval      VABNRS = Det_RFF_FF_nrs
359300090211     c                   end
359400100319     c                   if        Det_RFF_FF_ctm <> *blank
359500100319     C                   eval      VABCTM = Det_RFF_FF_ctm
359600090211     c                   end
359700090211      *
359800090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
359900100319     c                   if        Det_RFF_FF_nsp <> *blank                            *blk/'S'
360000100319     c                   eval      mantiene_nsp = Det_RFF_FF_nsp                *blk/'S'
360100090211     c                   end
360200090211      *
360300100319      *  Specificato codice da qualificatore RFF+FF dalla testata
360400100319     C                   When        Tes_RFF_FF_KSC   <> *zeros
360500100319     C                   Z-ADD     Tes_RFF_FF_KSCVABCCM
360600100319     C                   MOVEL     Tes_RFF_FF_TARVABCTR
360700090211      * forza LNP/CTM/NRS
360800100319     c                   if        Tes_RFF_FF_LNP  <> 0
360900100319     C                   eval      VABLNP = Tes_RFF_FF_LNP
361000090211     c                   end
361100100319     c                   if        Tes_RFF_FF_NRS  <> 0
361200100319     C                   eval      VABNRS = Tes_RFF_FF_NRS
361300090211     c                   end
361400100319     c                   if        Tes_RFF_FF_CTM  <> *blank
361500100319     C                   eval      VABCTM = Tes_RFF_FF_CTM
361600090211     c                   end
361700090211      *
361800090211      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
361900100319     c                   if        Tes_RFF_FF_nsp  <> *blank                    *blk/'S'
362000100319     c                   eval      mantiene_nsp = Tes_RFF_FF_nsp                *blk/'S'
362100090211     c                   end
362200090211      *
362300090211      *  Imposto dati da tabella - PT-
362400090211     C                   Other
362500090211      *
362600090211     C                   MOVEL     §PTKSC        VABCCM
362700090211     C                   MOVEL     §PTCTR        VABCTR
362800090211      *
362900090211     C                   Endsl
363000090211      *
363100090211      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
363200090211      *    o dalla relativa CL
363300090211     C                   MOVEL     VABlnp        VABfgs
363400090211      *
363500090211      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
363600090211      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
363700090211      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
363800090211      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
363900090211      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
364000090211      *
364100090211     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
364200090211     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
364300090211     c                   else
364400090211      *
364500090211     C*  Se non è previsto che il cliente attribuisca nr.sped.
364600090211     C*  lo attribuisco io
364700090211      **              **------------------**
364800090211     c                   exsr      repnum
364900090211      **              **------------------**
365000100630     C                   Z-ADD     NUM_Spediz    VABNSP                         *NUM.SPEDIZIONE
365100090211      **              **------------------**
365200090211     c                   end
365300090211      *
365400090211     C*  Altrimenti attribuisco data del giorno
365500090211     C     VABMGS        IFEQ      0
365600090211     C                   MOVEL     WOGGI         VABAAS                         data sped.
365700090211     C                   MOVE      WOGGI         VABMGS                         = oggi
365800090211     C                   END
365900090211     C*  Eseguo posizionamento su EDiVAB
366000090211     C                   Z-ADD     VABAAS        KAAS
366100090211     C                   Z-ADD     VABlnp        KLNP
366200090211     C                   Z-ADD     VABNRS        KNRS
366300090211     C                   Z-ADD     VABNSP        KNSP
366400090211     C                   MOVEL     SAVBOL        SALVA
366500090211     C     KVAB1         CHAIN     EDiVAB1L                           30
366600090211     C                   MOVEL     SALVA         SAVBOL
366700090211     C*  Imposto dati CMR
366800090211     C                   Z-ADD     1             VABCNT
366900090211     **
367000090211      *** Attenzione se si deve trattare il VAX con il CMR
367100090211      *** il campo VABCNT deve essere sempre impostato a (1)
367200090211     C     §puWRK        ifEQ      'S'
367300090211     C     §puNSP        andEQ     'S'
367400090211     C                   Z-ADD     1             VABCNT
367500090211     c                   end
367600090211     **
367700100630     C                   Z-ADD     Data_prepara  VABDTS
367800090212     C                   movel     '20'          VABDTS
367900100113     ** Forza la Data di Oggi
368000100113     C                   z-add     WOggi         VABDTS
368100100113     **
368200100630     C     Ora__prepara  mult      100           VABHMS
368300090211     C     §PTCMR        IFEQ      'S'
368400100630     C                   Z-ADD     DTM_DtCMR     VABDCM
368500100706     C                   MOVEL(p)  BGM_nrCMR     VABCMR
368600100701     c                   if          RFF_NrCMR <> *Blank
368700100706     C                   MOVEL(p)  RFF_NrCMR     VABCMR
368800100701     c                   end
368900090211     C                   ELSE
369000100630     C                   Z-ADD     DTM_DtFViag   VABDCM
369100100706     C                   MOVEL(p)  BGM_NrFviaggioVABCMR
369200100701     c                   if          RFF_NrFviaggio <> *Blank
369300100706     C                   MOVEL(p)  RFF_NrFviaggioVABCMR
369400100701     c                   end
369500090211     C                   END
369600100120      *
369700100813     c                   if          VABCMR = *Blank
369800100813     C                   MOVEL(p)  BGM_Id_TestataVABCMR
369900100813     c                   else
370000100813      *
370100100317      * Per IFCSUM occorre fare un unico CMR giornaliero per permettere di
370200100120      * accorpare la bolle ad un unico destinatario da più parti
370300100317      *  IFCSUM quindi deve utilizzare la conferma x CMR.
370400100706     c                   if        RFF_NrCMR =*Blank and RFF_NrFviaggio =*Blank
370500100708     C                   if         NAD_SF_x_CMR <> *blank
370600100708     c                   eval      VABCMR = NAD_SF_x_CMR
370700100708     c                   else
370800100813     c                   eval      VABCMR = 'EDI_' + %editc(VABCCM:'Z')    +
370900100813     c                                      '_' + %editc(Data_Prepara:'Z') +
371000100813     c                                      '_' + %editc(Ora__Prepara:'Z')
371100100708     c                   end
371200100706     c                   end
371300100120      *
371400100813     c                   end
371500100921      *
371600100921     c                   if        vabdcm = 0
371700100921     C                   Z-ADD     Data_Prepara  VABDCM
371800100921     c                   end
371900100813      *
372000090211     C*  Se non è indicato il nome del mittente = Partner mittente
372100090211     C     VABRMO        IFEQ      *BLANKS
372200090211     C                   MOVEL     ACORAG        VABRMO
372300090211     C                   MOVEL     INDCAP        VABCMO
372400090211     C     INDCAE        IFNE      *BLANKS
372500090211     C                   MOVEL     INDCAE        VABCMO
372600090211     C                   END
372700090211     C     INDSTA        IFNE      *BLANKS
372800090211     C                   MOVEL     INDSTA        VABNMO
372900090211     C                   END
373000090211     C                   END
373100090211     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
373200090211     C     §1BFG1        IFEQ      'N'
373300090211     C     §1BFG7        ANDEQ     'N'
373400090211     C     §1BFG2        ANDEQ     'N'
373500090211     C***                  Z-ADD0         VABNRS
373600090211     C                   END
373700090211     C*  Imposto segancollo da - a
373800090211     C     §1BFG1        IFEQ      'S'
373900090211     C     §1BFG7        ANDEQ     'N'
374000090211     C     §1BF12        ANDNE     'E'
374100090211      *
374200100701     C     Conta_Segn    IFEQ      *ZEROS
374300090211     C                   CLEAR                   VABNCD
374400090211     C                   CLEAR                   VABNCA
374500090211     C                   ELSE
374600100630     C                   Z-ADD     segn_BAR(1)   VABNCD
374700100701     C                   eval      VABNCA = segn_BAR(Conta_Segn)
374800090211     C                   ENDIF
374900090211     C*
375000090211     C                   END
375100090211     C*
375200090211     C*  Se cap mittente originale a blank abblenco nazione
375300090211     C     VABCMO        IFEQ      *BLANKS
375400090211     C                   MOVEL     *BLANKS       VABNMO
375500090211     C                   END
375600100701     C*
375700090603      * Se Mittente o Destinatario presi dalla Testata del messaggio:
375800090603     C* Se in testata
375900100701     c                   if        NAD_destinatario_in_testata = 'S' and
376000100701     c                             NAD_destinatario_in_dettaglio <> 'S'
376100090603     c                   eval         VABrsd =  tes_VABrsd
376200090603     c                   eval         VABrd2 =  tes_VABrd2
376300090603     c                   eval         VABind =  tes_VABind
376400090603     c                   eval         VABlod =  tes_VABlod
376500090603     c                   eval         VABnzd =  tes_VABnzd
376600090603     c                   end
376700090603     C* Se in testata
376800100701     c                   if        NAD_mittente_in_testata = 'S' and
376900100701     c                             NAD_mittente_in_dettaglio <> 'S'
377000090603     c                   eval         VABrmo = tes_VABrmo
377100090603     c                   eval         VABnmo = tes_VABnmo
377200090603     c                   eval         VABcmo = tes_VABcmo
377300090603     c                   end
377400100813      *
377500100813     C*  Per non bloccare la bollettazione
377600100813     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
377700100813     C*  o se il peso non è presente... imposto fisso il minimo 0,1
377800100813     c                   if        vabpkb = 0
377900100813     c                   z-add     0,1           vabpkb
378000100813     c                   end
378100100813     C*
378200100813      *
378300100813     C*  Se cap mittente originale a blank abblenco nazione
378400100813     C     VABCMO        IFEQ      *BLANKS
378500100813     C                   MOVEL     *BLANKS       VABNMO
378600100813     C                   END
378700100813      *
378800100813      *   Forza il numero passato nel CNI nel riferimento Numerico
378900100813     c                   if        §PVCNIRMN = 'S'
379000100813     c                   z-add     CNI_SeNumericovabRMN
379100100813     c                   end
379200100921      *
379300100921      * Se richiesta conferma x CMR con raggruppamento spedizioni x destinatario
379400100921     C     §puWRK        ifEQ      'S'
379500100921     C     §puraggDES    andeq     'S'
379600100921     c                   clear                   vabLNA
379700100921     c                   clear                   vabZNC
379800100921     c                   clear                   vabCMR
379900100921      *  il nome identico
380000100921     c                   eval      VABCMR  = §punomeCMR
380100100921      *   e in più la data del giorno
380200100921     C     §puinGIO      ifEQ      'S'
380300100921     c                   eval      VABCMR = %trim(VABCMR)
380400100921     c                                    + %editc(WOggi_CMR:'X')
380500100921     c                   end
380600100921      *
380700100921     c                   end
380800100921      *
380900100813      *
381000090211      *? ------------------------------------------------------------------------
381100090213     C   30              WRITE     EDiVAB00                             99
381200090213     C  N30              UPDATE    EDiVAB00                             99
381300090211      *? ------------------------------------------------------------------------
381400090213     c   99              eval      errori_in_Wrt = 'S'
381500090211     C*
381600090211     c                   clear                   wri_vabtic        1
381700090211     C*
381800090211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
381900100706     C                   EXSR      Scrive_Riferim
382000090211     C*
382100090211      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
382200090211     C     §1BF12        IFEQ      'E'
382300100706     C                   EXSR      Scrive_VAT
382400090211     C                   ELSE
382500090211      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
382600090211     C     §1BFG7        IFEQ      'S'
382700090211     C     §1BFG1        OREQ      'S'
382800100706     C                   EXSR      Scrive_VAD
382900090211     C                   END
383000090211     C                   END
383100090211      *
383200090211     C* Reimposto codice trattamento merce cliente
383300100628     C                   MOVEL     sav_DS1B      DS1B
383400090211     C**
383500090211     C* Do errore se previsto CMR ma non c'è
383600090211     C     §PTCMR        IFNE      ' '
383700090211     C                   EXSR      MEMCMR
383800090211     C                   END
383900090211     C* Do errore se previsto AFB ma non c'è
384000090211     C     §PTNFV        IFNE      ' '
384100090211     C                   EXSR      MEMAFB
384200090211     C                   END
384300090211     C*  Se il cliente vuole il ritorno delle date di consegna
384400090211     C*  è c'è una squdratura fra i segnacolli e il numero
384500090211     C*  dei colli che ci ha passato scrivo EDSUM
384600100630     c                   if        §puDTA = 'S' and Squadratura_Colli = 'S'
384700090211     C                   EXSR      WRTSUM
384800090211     C                   END
384900090211     C*
385000090211     C                   ENDSR
385100090211     C*----------------------------------------------------------------
385200090211     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
385300090211     C*----------------------------------------------------------------
385400100630     C     CTRsegn_BAR   BEGSR
385500090211     C*
385600090211     C                   MOVEL     'N'           WNSGER            1
385700090211     C*
385800090211     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
385900100701     C     VABNCL        IFNE      Conta_Segn
386000090211     C                   MOVEL     'S'           WNSGER
386100100630     C                   eval       Squadratura_Colli = 'S'
386200090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
386300090211     c                              bollettati'
386400090211     C                   GOTO      FINSGN
386500090211     C                   END
386600100706      *
386700090211     C*  Riordino i segnacolli
386800090211     C     §1BFG7        IFEQ      'N'
386900100630     C                   SORTA     segn_BAR
387000090211     C*  Controllo se sono sequenziali
387100090211     C                   MOVEL     'N'           WNOSEQ            1
387200100630     C     segn_BAR(1)   ADD       1             WEXSGN            7 0
387300100706     C*
387400100701     C     2             DO        Conta_Segn    X
387500100630     C     segn_BAR(X)   IFNE      WEXSGN
387600090211     C                   MOVEL     'S'           WNOSEQ
387700090211     C                   END
387800100630     C     segn_BAR(X)   ADD       1             WEXSGN
387900090211     C                   END
388000100706     C*
388100090211     C                   END
388200090211     C*
388300090211     C     FINSGN        ENDSR
388400090211     C*----------------------------------------------------------------
388500090211     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
388600090211     C*----------------------------------------------------------------
388700100630     C     CTRsegn_EEX   BEGSR
388800090211     C*
388900090211     C                   MOVEL     'N'           WNSGER            1
389000090211      *
389100090211      *  Controllo se il nr.segnacolli corrisponde coi bollettati
389200090211      *
389300100701     C     VABNCL        IFNE      totale_PCI
389400090211     C                   MOVEL     'S'           WNSGER
389500100630     C                   eval       Squadratura_Colli = 'S'
389600090211     C                   eval      vinMSG = 'il Nr.SGN non corrisponde ai colli-
389700090211     c                              bollettati'
389800090211     C                   END
389900090211      *
390000090211     C                   ENDSR
390100090211     C*----------------------------------------------------------------
390200090211     C*? ESEGUO SCRITTURA EDiVAD
390300090211     C*----------------------------------------------------------------
390400100706     C     Scrive_VAD    BEGSR
390500090211     C*
390600100701     C* Se non seq. scrivo i record
390700090211     C     §1BFG1        IFEQ      'S'
390800100701     C     Conta_Segn    ANDNE     *ZEROS
390900090211     C*
391000090211     C     §1BFG7        IFEQ      'S'
391100100701     C                   DO        Conta_Segn    X
391200090211     C                   Z-ADD     VABCCM        KCCM
391300100630     C                   Z-ADD     segn_BAR(X)   KNCD
391400090211     C                   Z-ADD     VABCNT        VADCNT
391500090211     C                   MOVEL     VABCMR        VADCMR
391600090211     C                   Z-ADD     VABAAS        VADAAS
391700090211     C                   Z-ADD     VABLNP        VADLNP
391800090211     C                   Z-ADD     VABNRS        VADNRS
391900090211     C                   Z-ADD     VABNSP        VADNSP
392000090211     C                   Z-ADD     1             VADNCL
392100100630     C                   MOVEL     segn_BAR(X)   VADCDP
392200100630     C                   Z-ADD     segn_BAR(X)   VADNCD
392300090211     C                   Z-ADD     *ZEROS        VADNCA
392400090211     C                   Z-ADD     VABCCM        VADCCM
392500090211     C                   movel     VABfgs        VADfgs
392600090213     C                   WRITE     EDiVAD00                             99
392700090213     c   99              eval      errori_in_Wrt = 'S'
392800090211     C                   END
392900090211     C* Se sequenziali scrivo un solo record
393000090211     C                   ELSE
393100090211     C                   Z-ADD     VABAAS        VADAAS
393200090211     C                   Z-ADD     VABLNP        VADLNP
393300090211     C                   Z-ADD     VABNRS        VADNRS
393400090211     C                   Z-ADD     VABNSP        VADNSP
393500090211     C                   Z-ADD     VABNCL        VADNCL
393600100630     C                   Z-ADD     segn_BAR(1)   VADNCD
393700100701     C                   eval      VADNCA = segn_BAR(Conta_Segn)
393800090211     C                   Z-ADD     VABCCM        VADCCM
393900090211     C                   movel     VABfgs        VADfgs
394000090213     C                   WRITE     EDiVAD00                             99
394100090213     c   99              eval      errori_in_Wrt = 'S'
394200090211     C                   END
394300090211     C*
394400090211     C                   ELSE
394500100701     C                   DO        Conta_Segn    X
394600090211     C                   Z-ADD     VABCCM        KCCM
394700100630     C                   Z-ADD     segn_BAR(X)   KNCD
394800090211     C                   Z-ADD     VABAAS        VADAAS
394900090211     C                   Z-ADD     VABLNP        VADLNP
395000090211     C                   Z-ADD     VABNRS        VADNRS
395100090211     C                   Z-ADD     VABNSP        VADNSP
395200100630     C                   MOVEL     segn_BAR(X)   VADCDP
395300090211     C                   Z-ADD     1             VADNCL
395400090211     C                   Z-ADD     *ZEROS        VADNCD
395500090211     C                   Z-ADD     *ZEROS        VADNCA
395600090211     C                   Z-ADD     VABCCM        VADCCM
395700090211     C                   movel     VABfgs        VADfgs
395800090213     C                   WRITE     EDiVAD00                             99
395900090213     c   99              eval      errori_in_Wrt = 'S'
396000090211     C                   END
396100090211     C*
396200090211     C                   END
396300090211     C*
396400090211     C                   ENDSR
396500090211     C*----------------------------------------------------------------
396600090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
396700090211     C*----------------------------------------------------------------
396800100706     C     Scrive_RiferimBEGSR
396900090211      *
397000090211      *  riferimento di consegna destinatario
397100100701     c                   if        CTA_destinatario_in_dettaglio <> *blank or
397200100701     c                             CTA_destinatario_in_testata   <> *blank
397300090211     C                   Z-ADD     VABCNT        VATCNT
397400090211     C                   MOVEL     VABCMR        VATCMR
397500090211     C                   Z-ADD     VABCCM        VATCCM
397600090211     C                   movel     VABfgs        VATfgs
397700090211     C                   Z-ADD     VABLNP        VATLNP
397800090211     C                   Z-ADD     VABAAS        VATAAS
397900090211     C                   Z-ADD     VABNRS        VATNRS
398000090211     C                   Z-ADD     VABNSP        VATNSP
398100090211     C                   MOVEL     'A'           VATTRC
398200100701     c                   if        CTA_destinatario_in_dettaglio <> *blank
398300100701     C                   eval       VATNOT = CTA_destinatario_in_dettaglio
398400100701     c                   else
398500100701     C                   eval       VATNOT = CTA_destinatario_in_testata
398600100701     c                   end
398700090213     C                   WRITE     EDiVAT00                             99
398800090213     c   99              eval      errori_in_Wrt = 'S'
398900090211     c                   end
399000090211      *
399100090211      *  riferimento di consegna telefonico
399200100701     c                   if        COM_telefono_in_dettaglio <> *blank or
399300100701     c                             COM_telefono_in_testata   <> *blank
399400090211     C                   Z-ADD     VABCNT        VATCNT
399500090211     C                   MOVEL     VABCMR        VATCMR
399600090211     C                   Z-ADD     VABCCM        VATCCM
399700090211     C                   movel     VABfgs        VATfgs
399800090211     C                   Z-ADD     VABLNP        VATLNP
399900090211     C                   Z-ADD     VABAAS        VATAAS
400000090211     C                   Z-ADD     VABNRS        VATNRS
400100090211     C                   Z-ADD     VABNSP        VATNSP
400200090211     C                   MOVEL     'B'           VATTRC
400300100701     c                   if        COM_telefono_in_dettaglio <> *blank
400400100701     c                   eval      VATNOT = COM_telefono_in_dettaglio
400500100701     c                   else
400600100701     c                   eval      VATNOT = COM_telefono_in_testata
400700100701     c                   end
400800090213     C                   WRITE     EDiVAT00                             99
400900090213     c   99              eval      errori_in_Wrt = 'S'
401000090211     c                   end
401100090211      *
401200090211      * riferimento del Partner che una volta veniva riportato sul BL4
401300100119      *   DOVENDO utilizzare il processo di accorpamento spedizioni x CMR
401400100119      *   NON si deve andare a scrivere il riferimento Partner sul VAT
401500100119      *   ma verrà utilizzato solo il RMA e RMN sul VAB.
401600100119      *
401700100701     c                   if        RFF_RifSpediz <> *blank
401800090211     C                   Z-ADD     VABCNT        VATCNT
401900090211     C                   MOVEL     VABCMR        VATCMR
402000090211     C                   Z-ADD     VABCCM        VATCCM
402100090211     C                   movel     VABfgs        VATfgs
402200090211     C                   Z-ADD     VABLNP        VATLNP
402300090211     C                   Z-ADD     VABAAS        VATAAS
402400090211     C                   Z-ADD     VABNRS        VATNRS
402500090211     C                   Z-ADD     VABNSP        VATNSP
402600090211     C                   MOVEL     'C'           VATTRC
402700100701     C                   MOVEL     RFF_RifSpediz VATNOT
402800090213     C                   WRITE     EDiVAT00                             99
402900090213     c   99              eval      errori_in_Wrt = 'S'
403000090211     c                   end
403100100119      *
403200090211     C                   ENDSR
403300090211     C*----------------------------------------------------------------
403400090211     C*? ESEGUO SCRITTURA EDiVAT
403500090211     C*----------------------------------------------------------------
403600100706     C     Scrive_VAT    BEGSR
403700090211      *
403800100701     C                   DO        totale_PCI    X
403900090211     C                   Z-ADD     VABCNT        VATCNT
404000090211     C                   MOVEL     VABCMR        VATCMR
404100090211     C                   Z-ADD     VABCCM        VATCCM
404200090211     C                   movel     VABfgs        VATfgs
404300090211     C                   Z-ADD     VABLNP        VATLNP
404400090211     C                   Z-ADD     VABAAS        VATAAS
404500090211     C                   Z-ADD     VABNRS        VATNRS
404600090211     C                   Z-ADD     VABNSP        VATNSP
404700090211     C                   MOVEL     'E'           VATTRC
404800090211      *
404900090211      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
405000090211      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
405100090211      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
405200100701     c                   if        PU_lunVAT     > 0
405300090211      *
405400090211      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
405500100630     C                   eval      VATnot =
405600100701     C                                 %subst(segn_EEX(X):1:PU_lunVAT)
405700090211      *
405800090211     c                   else
405900090211      *
406000090211      * Se non è impostato nulla prende il segnacollo passato così com'è
406100100630     C                   MOVEL     segn_EEX(X)   VATNOT
406200090211     c                   end
406300090211      *
406400090213     C                   WRITE     EDiVAT00                             99
406500090213     c   99              eval      errori_in_Wrt = 'S'
406600090211     C                   END
406700090211      *
406800090211     C                   ENDSR
406900090211     C*----------------------------------------------------------------
407000090211     C*? ESEGUO SCRITTURA EDSUM
407100090211     C*----------------------------------------------------------------
407200090211     C     WRTSUM        BEGSR
407300090211     C*
407400100701     C* Se non seq. scrivo i   record
407500100701     C                   DO        Conta_Segn    X
407600090211     C                   Z-ADD     VABAAS        SUMAAS
407700090211     C                   Z-ADD     VABLNP        SUMFLS
407800100701     C                   MOVEL     PT_vabCCM     SUMLNP
407900090211     C                   Z-ADD     VABLNA        SUMLNA
408000090211     C                   Z-ADD     VABZNC        SUMZNC
408100090211     C                   Z-ADD     VABNRS        SUMNRS
408200090211     C                   Z-ADD     VABNSP        SUMNSP
408300100630     C                   Z-ADD     segn_BAR(X)   SUMNSC
408400100701     C                   Z-ADD     PT_vabCCM     SUMCCM
408500090211     C                   MOVEL     *BLANKS       SUMSTS
408600090211     C                   MOVEL     *BLANKS       SUMFLG
408700100813     C                   eval       SUMCNI = CNI_Identificativo_Bolla
408800090211     C                   MOVEL     VABCMR        SUMCMR
408900090211     C                   Z-ADD     0             SUMNFE
409000090211     C                   Z-ADD     VABCCM        VADCCM
409100090213     C                   WRITE     EDSUM000                             99
409200090213     c   99              eval      errori_in_Wrt = 'S'
409300090211     C                   END
409400090211     C*
409500090211     C                   ENDSR
409600090211     c*==================================================================*
409700090211      * Manda un Msg in Posta AS Sede a EDP*
409800090211     c*==================================================================*
409900090211     c     SEND_MSG_EDP  begsr
410000090211      *
410100090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
410200090211      *   Lo manda una sola volta per lavoro
410300090211     c                   IF        Snd_Msg_alert  <> 'N' and
410400090211     c                             User_msg <> *blank
410500090211     c                   z-add     1             Jx
410600090211     C                   exsr      CalQEZSNDMG
410700090211     c                   end
410800090211      *
410900090211     c                   endsr
411000090213     c**********************************************************************
411100090213     c* reperimento numero Spedizione
411200090213     c**********************************************************************
411300090213     C     repnum        BEGSR
411400090213      *
411500090213      *    deve controllare sulla tabella "3C" se il numero spedizione
411600090213      *     deve essere mantenuto oppure no
411700090213     C*
411800090213     C                   clear                   Ds3C
411900090213     C                   Z-ADD     CODUT         TBLKUT
412000090213     C                   MOVEL     '3C'          TBLCOD
412100090213     C                   movel(p)  VABccm        TBLKEY
412200090213     C     KTABel        CHAIN     TABEL00F                           30
412300090213     C                   IF        %Found(TABEL00F) and tblflg = *blank
412400090213     C                   MOVEL     TBLUNI        Ds3C
412500090213     C                   END
412600090213      *
412700090213      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
412800090213     c                   if        §3CFsp <> 'S'
412900090213      *
413000090213     C* NSP => Stacco un numeratore da AZNUM
413100090213     c                   movel     kpjbu         svkpjbu
413200090213     c                   clear                   kpjbu
413300090213     C                   clear                   TRUL33DS
413400090213     C                   eval      I33OPE = *zeros
413500090213     C                   eval      I33CNU = 302
413600090213     C                   eval      I33NUM = 1
413700090213     C                   movel     TRUL33DS      KPJBU
413800090213     C                   call      'TRUL33R'
413900090213     C                   parm                    KPJBA
414000090213     C                   movel     KPJBU         TRUL33DS
414100090213     c                   movel     svkpjbu       kpjbu
414200090213      ****
414300090213     C                   if        O33ERR = *zeros
414400100630     c                   z-add     o33nrf        NUM_Spediz
414500090213     C                   else
414600090213     C                   endif
414700090213      ****
414800090213     c                   ELSE
414900090213      *   se si vuole mantenere il numero spedizione
415000090213      ****
415100090213     C                   MOVEL     WOGGI         WANNO             4 0
415200090213     C                   MOVE      WANNO         KAAA
415300090213     C                   Z-ADD     3             KCNU
415400090213     C                   Z-ADD     VABlnp        KFIL
415500090213     C     KNUF          CHAIN     FLNUF                              9999
415600090213     C     *IN99         IFEQ      '0'
415700090213     C                   ADD       1             NUFNUM
415800100630     C                   Z-ADD     NUFNUM        NUM_Spediz                     *NUM.SPEDIZIONE
415900090213     C                   UPDATE    FLNUF
416000090213     C                   ELSE
416100090213     C                   END
416200090213     C*
416300090213     c                   EndIF
416400090213      **
416500090213     C                   ENDSR
416600090213     C*----------------------------------------------------------------
416700090213     C*? MEMCMR - MEMORIZZO NUMERO CMR
416800090213     C*----------------------------------------------------------------
416900090213     C     MEMCMR        BEGSR
417000090213     C*
417100090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
417200090213     C     §PTCM1        IFEQ      'S'
417300090213     C                   CLEAR                   EDRDE000
417400090213     C                   MOVEL     'TD1154'      KNMC
417500090213     C                   Z-ADD     1             KNRP
417600100701     C                   MOVEL     PT_vabCCM     KLNP1
417700090213     C     KRDE          CHAIN     EDRDE01L                           31
417800090213     C     *IN31         IFEQ      '1'
417900090213     C                   Z-ADD     VABAAS        RDEAAS
418000090213     C                   Z-ADD     VABLNP        RDELNP
418100090213     C                   Z-ADD     VABNRS        RDENRS
418200090213     C                   Z-ADD     VABNSP        RDENSP
418300090213     C                   MOVEL     'TD1154'      RDENMC
418400090213     C                   Z-ADD     1             RDENRP
418500100706     C                   MOVEL     BGM_nrCMR     RDEVAL
418600100701     c                   if          RFF_NrCMR <> *Blank
418700100701     C                   MOVEL     RFF_NrCMR     RDEVAL
418800100701     c                   end
418900090213     C                   WRITE     EDRDE000                             99
419000090213     c   99              eval      errori_in_Wrt = 'S'
419100090213     C                   END
419200090213     C*  Memorizzo qualificatore CMR
419300090213     C                   CLEAR                   EDRDE000
419400090213     C                   MOVEL     'TD1153'      KNMC
419500090213     C                   Z-ADD     1             KNRP
419600100701     C                   MOVEL     PT_vabCCM     KLNP1
419700090213     C     KRDE          CHAIN     EDRDE01L                           31
419800090213     C     *IN31         IFEQ      '1'
419900090213     C                   Z-ADD     VABAAS        RDEAAS
420000090213     C                   Z-ADD     VABLNP        RDELNP
420100090213     C                   Z-ADD     VABNRS        RDENRS
420200090213     C                   Z-ADD     VABNSP        RDENSP
420300090213     C                   Z-ADD     1             RDENRP
420400090213     C                   MOVEL     'TD1153'      RDENMC
420500090213     C                   MOVEL     'CMR'         RDEVAL
420600090213     C                   WRITE     EDRDE000                             99
420700090213     c   99              eval      errori_in_Wrt = 'S'
420800090213     C                   END
420900090213     C*  Memorizzo la data
421000090213     C                   CLEAR                   EDRDE000
421100090213     C                   MOVEL     'TD2380'      KNMC
421200090213     C                   Z-ADD     1             KNRP
421300100701     C                   MOVEL     PT_vabCCM     KLNP1
421400090213     C     KRDE          CHAIN     EDRDE01L                           31
421500090213     C     *IN31         IFEQ      '1'
421600090213     C                   Z-ADD     VABAAS        RDEAAS
421700090213     C                   Z-ADD     VABLNP        RDELNP
421800090213     C                   Z-ADD     VABNRS        RDENRS
421900090213     C                   Z-ADD     VABNSP        RDENSP
422000090213     C                   Z-ADD     1             RDENRP
422100090213     C                   MOVEL     'TD2380'      RDENMC
422200100630     C                   MOVEL     DTM_DtCMR     RDEVAL
422300090213     C                   WRITE     EDRDE000                             99
422400090213     c   99              eval      errori_in_Wrt = 'S'
422500090213     C                   END
422600090213     C*
422700090213     C                   END                                                    §PTCM1 = 'S'
422800090213     C*
422900090213     C                   ENDSR
423000090213     C*----------------------------------------------------------------
423100090213     C*? MEMAFB - MEMORIZZO NUMERO AFB
423200090213     C*----------------------------------------------------------------
423300090213     C     MEMAFB        BEGSR
423400090213     C*
423500090213     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
423600090213     C     §PTNF1        IFEQ      'S'
423700090213     C                   CLEAR                   EDRDE000
423800100701     C                   MOVEL     PT_vabCCM     KLNP1
423900090213     C                   MOVEL     'TD1154'      KNMC
424000090213     C                   Z-ADD     1             KNRP
424100090213     C     KRDE          CHAIN     EDRDE01L                           31
424200090213     C     *IN31         IFEQ      '1'
424300090213     C                   Z-ADD     VABAAS        RDEAAS
424400090213     C                   Z-ADD     VABLNP        RDELNP
424500090213     C                   Z-ADD     VABNRS        RDENRS
424600090213     C                   Z-ADD     VABNSP        RDENSP
424700090213     C                   MOVEL     'TD1154'      RDENMC
424800100701     C                   if        §PTCM1 <> 'S' or
424900100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
425000090213     C                   Z-ADD     1             RDENRP
425100090213     C                   ELSE
425200090213     C                   Z-ADD     2             RDENRP
425300090213     C                   END
425400100706     C                   MOVEL     BGM_NrFviaggioRDEVAL
425500100701     c                   if          RFF_NrFviaggio <> *Blank
425600100701     C                   MOVEL     RFF_NrFviaggioRDEVAL
425700100701     c                   end
425800090213     C                   WRITE     EDRDE000                             99
425900090213     c   99              eval      errori_in_Wrt = 'S'
426000090213     C                   END
426100090213     C*  Memorizzo qualificatore CMR
426200090213     C                   CLEAR                   EDRDE000
426300100701     C                   MOVEL     PT_vabCCM     KLNP1
426400090213     C                   MOVEL     'TD1153'      KNMC
426500090213     C                   Z-ADD     1             KNRP
426600090213     C     KRDE          CHAIN     EDRDE01L                           31
426700090213     C     *IN31         IFEQ      '1'
426800090213     C                   Z-ADD     VABAAS        RDEAAS
426900090213     C                   Z-ADD     VABLNP        RDELNP
427000090213     C                   Z-ADD     VABNRS        RDENRS
427100090213     C                   Z-ADD     VABNSP        RDENSP
427200100701     C                   if        §PTCM1 <> 'S' or
427300100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
427400090213     C                   Z-ADD     1             RDENRP
427500090213     C                   ELSE
427600090213     C                   Z-ADD     2             RDENRP
427700090213     C                   END
427800090213     C                   MOVEL     'TD1153'      RDENMC
427900090213     C                   MOVEL     'AFB'         RDEVAL
428000090213     C                   WRITE     EDRDE000                             99
428100090213     c   99              eval      errori_in_Wrt = 'S'
428200090213     C                   END
428300090213     C*  Memorizzo la data
428400090213     C                   CLEAR                   EDRDE000
428500100701     C                   MOVEL     PT_vabCCM     KLNP1
428600090213     C                   MOVEL     'TD2380'      KNMC
428700090213     C                   Z-ADD     1             KNRP
428800090213     C     KRDE          CHAIN     EDRDE01L                           31
428900090213     C     *IN31         IFEQ      '1'
429000090213     C                   Z-ADD     VABAAS        RDEAAS
429100090213     C                   Z-ADD     VABLNP        RDELNP
429200090213     C                   Z-ADD     VABNRS        RDENRS
429300090213     C                   Z-ADD     VABNSP        RDENSP
429400100701     C                   if        §PTCM1 <> 'S' or
429500100706     C                             (BGM_nrCMR = *blank and RFF_NrCMR = *blank)
429600090213     C                   Z-ADD     1             RDENRP
429700090213     C                   ELSE
429800090213     C                   Z-ADD     2             RDENRP
429900090213     C                   END
430000090213     C                   MOVEL     'TD2380'      RDENMC
430100100630     C                   MOVEL     DTM_DtFViag   RDEVAL
430200090213     C                   WRITE     EDRDE000                             99
430300090213     c   99              eval      errori_in_Wrt = 'S'
430400090213     C                   END
430500090213     C*
430600090213     C                   END                                                    §PTCM1 = 'S'
430700090213     C*
430800090213     C                   ENDSR
430900090213     c*==================================================================*
431000090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
431100090213     c*==================================================================*
431200090213     c     Imp_ADDR_mail begsr
431300090213      *
431400090213     c                   clear                   Indirizzi
431500090213      *
431600090213      *  Lunghezza indirizzi presenti
431700100630     c                   eval      Lunghezza_Indirizzo =
431800100630     c                                       %Len(%TrimR(Mail_Ind))
431900090213     c                   z-add     1             al_char           3 0
432000090213     c                   z-add     1             dal_char          3 0
432100090213     c                   z-add     1             tot_char          3 0
432200090213      *
432300090213     c     successivo    tag
432400090213      *
432500090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
432600090213      *   e aggiunge il dominio Bartolini + il (;) separatore
432700090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
432800090213     c                   eval      tot_char = al_char - dal_char
432900100630     c                   z-add     dal_char      dalCarattere
433000100630     c                   z-add     tot_char      xTOTcaratteri
433100090213      *
433200090213     c                   clear                   Indir_Bartol     40
433300090213     c                   clear                   Indir_Parz       20
433400100630     c                   eval      Indir_Parz =
433500100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
433600100630      *
433700090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
433800090213     c                             %Trim(bart_it)
433900090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
434000090213      *
434100100630     c                   if        al_char = Lunghezza_Indirizzo
434200090213     c                   goto      fine_indmail
434300090213     c                   else
434400090213     c                   eval      dal_char = ( al_char + 1 )
434500090213     c                   goto      successivo
434600090213     c                   end
434700090213      *
434800090213     C     fine_indmail  TAG
434900090213      *
435000090213     C                   ENDSR
435100090213     c*==================================================================*
435200090213      * Manda un Msg x E-mail
435300090213     c*==================================================================*
435400090213     c     Invio_Mail    begsr
435500090213      *
435600090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
435700090213     C* Inizializzo variabili
435800090213     C                   movel     *blanks       wrkEml          253
435900090213     C                   movel     *blanks       wrkMsg         5000
436000090213     C                   movel     *blanks       wrkOgg           44
436100090213      *
436200090213     C* Valorizzo i campi della e-m@ail - indirizzo
436300100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
436400090213     C                   eval      wrkEml = Indirizzi
436500090213     C                   eval      wrkOgg = Oggetto
436600090213     C                   eval      wrkMsg = Messaggio
436700110203      ********
436800110203     C********           call(e)   'TIS701C'
436900110203     C********           parm                    wrkEml
437000110203     C********           parm                    wrkOgg
437100110203     C********           parm                    wrkMsg
437200110203     C*
437300110203      ** *****
437400110203     C                   call(e)   'TRTCT00R2'
437500110203     C                   parm                    wrkEml
437600110203     C                   parm                    wrkOgg
437700110203     C                   parm                    wrkMsg
437800090213     C*
437900090213     C                   ENDSR
438000090603      *
438100090211     ***********************************************************************
438200090211     **
438300090211     ** Send Message (QEZSNDMG) API
438400090211     **
438500090211     ***********************************************************************
438600090211     C     CalQEZSNDMG   BEGSR
438700090211      *
438800090211     ** Invio messaggio all'utente.
438900090211     C                   EVAL      SndMg03 = msgDSP
439000090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
439100090211     C                   EVAL      SndMg05(Jx) = User_msg
439200090211     C                   EVAL      SndMg06 = Jx
439300090211     C                   CLEAR                   QUSEC
439400090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
439500090211
439600090211     C                   CALL      'QEZSNDMG'
439700090211     C                   PARM                    SndMg01
439800090211     C                   PARM                    SndMg02
439900090211     C                   PARM                    SndMg03
440000090211     C                   PARM                    SndMg04
440100090211     C                   PARM                    SndMg05
440200090211     C                   PARM                    SndMg06
440300090211     C                   PARM                    SndMg07
440400090211     C                   PARM                    SndMg08
440500090211     C                   PARM                    Qusec
440600090211     C                   PARM                    SndMg10
440700090211     C                   PARM                    SndMg11
440800090211     C                   PARM                    SndMg12
440900090211      *
441000090211     C                   ENDSR
441100121105      *---------------------------------------------------------------*
441200121105      * CTRL caricamento schiere    PT                               -*
441300121105      *---------------------------------------------------------------*
441400121105     C     Check_PT      begsr
441500121105     C*
441600121105     c* Controllo riempimento schiera
441700121105     c                   clear                   trul0sds
441800121105     c                   eval      i0sski='PT_key'
441900121105     c                   eval      i0sele=%elem(PT_key)
442000121105     c                   eval      i0spie=x
442100121105     c                   eval      i0sfile='EDTAB00F'
442200121105     c                   eval      i0ssif=knsif
442300121105     c                   eval      i0spgm='TRTCT10R'
442400121113     c                   move      kpjbu         SvKpjbu
442500121113     c                   clear                   kpjbu
442600121105     c                   movel     trul0sds      kpjbu
442700121105     c                   call      'TRUL0SR'
442800121105     c                   parm                    kpjba
442900121113     c                   eval      kpjbu  =  SvKpjbu
443000121105     C*
443100121105     C                   ENDSR
443200090211     O*****************************************************************
443300090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
443400090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
443500090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
443600090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
