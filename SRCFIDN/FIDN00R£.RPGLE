000100980721
000200980721      ****************************************************************
000300990510      *  Se ricompilato il formato video, ricompilare i programmi:
000400990510      *    FIDN17R - FIDN21R - FIDN25R - FIDN35R
000500990510      ****************************************************************
000600150701
000700150701     /*PRM  dbgview(*source)
000800150701     /*CMD  ovrdbf file(FNBLP01L) tofile(FILTRAPRD/FNBLP01L) +
000900150701     /*CMD         ovrscope(*calllvl)
001000150701     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
001100150701     /*CMD         ovrscope(*calllvl)
001200150701     /*CMD  ovrdbf file(TIGCP21L) tofile(UNITRAGRP/TIGCP21L) +
001300150701     /*CMD         ovrscope(*calllvl)
001400150701     /*CMD  ovrdbf file(TNTMD01L) tofile(GAITRAGRPS/TNTMD01L) +
001500150701     /*CMD         ovrscope(*calllvl)
001600150701     /*CMD  ovrdbf file(TNTMD02L) tofile(GAITRAGRPS/TNTMD02L) +
001700150701     /*CMD         ovrscope(*calllvl)
001800150701     /*CMD  ovrdbf file(TNTMD03L) tofile(GAITRAGRPS/TNTMD03L) +
001900150701     /*CMD         ovrscope(*calllvl)
002000150701     /*END  dltovr file(*all) lvl(*)
002100150701     /*END
002200990510
002300990510      ****************************************************************
002400981008      *  FASI C.A. FISSATE A PROGRAMMA
002500980721      ****************************************************************
002600981126      *    025 = Lettera per avaria                           (WFlav)
002700981126      *    040 = Invio a sede documentazione C.A. per eventi  (WFise)
002800981015      *    050 = Sollecito lettera per avaria                 (WFsla)
002900981015      *    100 = Conferma C.A.                                (WFcon)
003000981106      *    230 = Elenco C.A. da contabilizzare                (WFecc)
003100981106      *    450 = Invio a sede documentazione C.A. per avaria  (WFisa)
003200981203      *    545 = Stampa avviso danno                          (WFsav)
003300990208      *    700 = Stampa progetto di Liquidazione              (WFspl)
003400990420      *    900 = Chiusura C.A.                                (WFcch)
003500981008      ****************************************************************
003600980623
003700980521      ****************************************************************
003800980521      *  RIEPILOGO INDICATORI
003900981008      ****************************************************************
004000980521      * 05 - SPROTEGGO P.O. IN GESTIONE
004100110329      * 06 - NON ABILITO titTO FUNZIONALE F18
004200980521      * 07 - NON VISUALIZZO "P.O. IN GESTIONE" per interrog. e gestione (solo sede). Abbinare a IN06
004300050217      * 08 - NON ABILITO TASTO FUNZIONALE F14   (sede)
004400980608      * 09 - PROTEZIONE Liquidazione/pratica (Ricerca CA)
004500980608      * 10 - PROTEZIONE Codice Fase (Ricerca CA)
004600980925      * 11 - SPENTO EMETTO: Data esecuzione fase (dal/al)
004700980925      *      ACCESO EMETTO: Richiesta visualizzazione completa
004800980612      * 12 - NON ABILITO TASTO FUNZIONALE F10
004900980612      * 13 - IMPOSTO costante per fase da eseguire (A-G) o eseguita (I-M)
005000980731      * 14 - ABILITO TASTO FUNZIONALE F06 se è prevista l'opzione 'G'
005100981008      * 15 - ABILITO TASTO FUNZIONALE F01 se è prevista la conferma totale
005200990510      * 15 - ABILITO TASTO FUNZIONALE F01 se è prevista la conferma totale
005300120822      * 16 - libero (Utente CONSULDANNI non +)
005400990312      * 20 - GESTIONE SUBFILE
005500990312      * 21 - GESTIONE SUBFILE
005600990312      * 22 - GESTIONE SUBFILE
005700990312      * 23 - GESTIONE SUBFILE - ROLLUP
005800990315      * 24 - LETTURA FILE PER CARICAMENTO SUBFILE
005900990323      * 25 - LETTURA FILE PER CARICAMENTO SUBFILE
006000990505      * 26 - DISABILITO - ROLLUP
006100980521      * 28 - ERRORE GENERICO DSPF
006200980521      * 30 - COMODO
006300980521      * 31 - COMODO
006400980528      * 32 - COMODO
006500980521      * 40 - ERRORE p.o. in gestione
006600980521      * 44 - ERRORE filiale CA
006700980521      * 45 - ERRORE NUMERO CA
006800980521      * 46 - ERRORE anno CA
006900980521      * 47 - ERRORE linea di partenza spedizione
007000980521      * 48 - ERRORE NUMERO spedizione
007100980521      * 49 - ERRORE anno spedizione
007200980521      * 50 - ERRORE tipo anomalia
007300980522      * 51 - ERRORE NUMERO evento
007400980522      * 52 - ERRORE anno evento
007500980522      * 53 - ERRORE data CA (dal)
007600980522      * 54 - ERRORE data CA (al)
007700980522      * 55 - ERRORE Liquidazione/pratica (Ricerca CA)
007800980522      * 56 - ERRORE Codice Fase (Ricerca CA)
007900980522      * 57 - ERRORE data esecuzione fase (dal)
008000980522      * 58 - ERRORE data esecuzione fase (al)
008100980522      * 59 - ERRORE P.O. per selezione
008200980522      * 60 - ERRORE Tipo selezione
008300990322      * 61 - ERRORE numero pratica assicurativa
008400990322      * 62 - ERRORE ditta pratica assicurativa
008500990322      * 63 - ERRORE anno pratica assicurativa
008600041026      * 64 - ERRORE numero assegno
008700041026      * 65 - ERRORE data assegno
008800150701      * 66 - ERRORE cod. Beneficiario
008900150701      * 67 - ERRORE rag.soc. Beneficiario
009000980610      * 81 - NON VISUALIZZO Numero C.A. e Numero sped. se ripetute in video 2
009100980622      * 82 - SCELTA bloccata in video 2
009200980622      * 83 - SCELTA errata in video 2
009300981209      * 84 - NON EMETTO la costante "Evento" nel subfile
009400990312      * 85 - NON EMETTO la costante "Pratica" nel subfile
009500160226      * 86 - Beneficiario discordante: HI in S02
009600980624      * 90 - riemissione videata
009700981008      * 91 - ricarico il subfile
009800990317      ****************************************************************
009900980623
010000980528      ****************************************************************
010100980622      *  LEGENDA SELEZIONI -  elementi schiera SEL
010200990317      ****************************************************************
010300020821      *  1 -    Libero
010400980528      *  2 - NUMERO C.A.
010500980528      *  3 - SPEDIZIONE
010600980528      *  4 - TIPO ANOMALIA
010700980528      *  5 - NUMERO EVENTO
010800980528      *  6 - DATA CREAZIONE C.A.
010900980528      *  7 - LIQUIDAZIONE TRANSATTIVA / PRATICA ASSICURATIVA
011000980528      *  8 - CODICE FASE
011100990322      *  9 - DATA ESECUZIONE FASE                    * no per Gestione
011200980528      * 10 - P.O. PER SELEZIONE
011300990322      * 11 - NUMERO PRATICA                          * solo sede
011400990322      * 12 - MITTENTE                                * solo sede
011500990322      * 13 - DESTINATARIO                            * solo sede
011600041110      * 14 - NUMERO E DATA ASSEGNO                   * solo sede
011700080718      * 15 - APERTE / CHIUSE                         * solo sede
011800990317      ****************************************************************
011900980623
012000980521     H DECEDIT('0,') DATEDIT(*DMY.)
012100980623
012200981204     fFIDN00D   CF   E             WORKSTN  sfile(fi00s02:nrr1)
012300981204     fFNDCT01L  IF   E           K DISK
012400981204     fFNDCT03L  IF   E           K DISK    RENAME(fndct000:fndct003)
012500981204     fFNDCT04L  IF   E           K DISK    RENAME(fndct000:fndct004)
012600050217     f                                     usropn
012700050217     fFndct24l  if   e           k disk    RENAME(fndct000:fndct024)
012800050217     f                                     usropn
012900990317     fFNDCT10L  IF   E           K DISK    RENAME(fndct000:fndct010)
013000981204     fFNDCF01L  IF   E           K DISK
013100981204     fFNDET01L  IF   E           K DISK
013200981204     fFNDKA01L  IF   E           K DISK
013300040715     fFNDCK01L  IF   E           K DISK
013400041110     fFNDCL03L  IF   E           K DISK
013500981204     fTABEL00F  IF   E           K DISK
013600981204     fTNTBE01L  IF   E           K DISK
013700981204     fAZORG01L  IF   E           K DISK
013800981204      *  filiale
013900981204     fFNBLP01L  IF   E           K DISK    usropn
014000020821     f                                             prefix(ARB:3)
014100981204     fFNARB01L  IF   E           K DISK    usropn
014200050331     fTigcp21l  IF   E           K DISK    usropn
014300981204      *  sede
014400981204     fTNTMD01L  IF   E           K DISK    usropn
014500990323     fTNTMD02L  IF   E           K DISK    usropn  RENAME(tntmd000:tntmdMIT)
014600990323     f                                             PREFIX(£)
014700990323     fTNTMD03L  IF   E           K DISK    usropn  RENAME(tntmd000:tntmdDES)
014800990323     f                                             PREFIX(£)
014900980623
015000980521      *------------------------------------------------------------------------*
015100980608     D Kaas            S                   LIKE(V1Caas)
015200980527     D Klnp            S                   LIKE(V1Clnp)
015300980527     D Knrs            S                   LIKE(V1Cnrs)
015400980527     D Knsp            S                   LIKE(V1Cnsp)
015500980527     D Kfca            S                   LIKE(DFAfca)
015600980527     D Kaac            S                   LIKE(V1Caac)
015700980527     D Kfil            S                   LIKE(V1Cfil)
015800980527     D Knca            S                   LIKE(V1Cnca)
015900980527     D Kmgc            S                   LIKE(DCTmgc)
016000980525     D Kcod            S                   LIKE(TBLCOD)
016100020821     D wlnab           S                   LIKE(ARBLNA)
016200980521     D waas            S                   LIKE(ARBAAS)
016300980521     D wnsp            S                   LIKE(ARBNSP)
016400980624     D wf03            S                   LIKE(O00f03)
016500980529     D wi05fca         S                   LIKE(I05FCA)
016600980529     D wdatac          S                   LIKE(V1Cdcaa)
016700990215     D wfgs            S                   LIKE(V1Cfgs)
016800980601     D w_msg           S                   LIKE(V1Cmsg)
016900980601     D w_ctad          S                   LIKE(V1Ctad)
017000980601     D w_dtad          S                   LIKE(V1Dtad)
017100980529     D w_linea         S                   LIKE(ARBlnp)
017200980522     D w_anno          S                   LIKE(ARBaas)
017300041026     D w_v1cdea        S                   LIKE(V1Cdea)
017400041026     D w_v1cdcaa       S                   LIKE(V1Cdcaa)
017500980525     D w_v1cdefa       S                   LIKE(V1Cdefa)
017600980601     D w_v1cfca        S                   LIKE(DCTfca)
017700981204     D w_v2crsb        S                   LIKE(V2Crsb)
017800981204     D w_v2htpb        S                   LIKE(V2Htpb)
017900020222     D w_nrrMAX        S                   LIKE(V2CNRR)   INZ(9995)
018000990420     D wdctfca         S                   LIKE(DCTfca)
018100980702     D wfrg            s                   LIKE(GCPfrg)
018200980923     D Wtbecod         S                   LIKE(TBEcod)
018300981204     D Wtrc            S                   LIKE(DKAtrc)
018400981229     D W§DCTlnpc       S                   LIKE(DCTlnp)
018500981126     D WFlav           S                   LIKE(DCTfca)   INZ(025)
018600981126     D WFise           S                   LIKE(DCTfca)   INZ(040)
018700981015     D WFsla           S                   LIKE(DCTfca)   INZ(050)
018800981015     D WFcon           S                   LIKE(DCTfca)   INZ(100)
018900981106     D WFecc           S                   LIKE(DCTfca)   INZ(230)
019000981106     D WFisa           S                   LIKE(DCTfca)   INZ(450)
019100981203     D WFsav           S                   LIKE(DCTfca)   INZ(545)
019200990208     D WFspl           S                   LIKE(DCTfca)   INZ(700)
019300990420     D WFcch           S                   LIKE(DCTfca)   INZ(900)
019400980702
019500980622     D W_esefase       S              1
019600980622     D W_POcrea        S              1
019700980622     D W_POgest        S              1
019800020821     D W_POinte        S              1    inz('N')
019900990215     D W_POsele        S              1
020000980702     D W_giace         S              1
020100020128     D $ErrFRANCH      S              1    inz(*off)
020200980804     D W_inkf          S              1
020300980521     D wtibol          S              1
020400990312     D wendpag         S              1
020500981204     D W001A           S              1
020600990324     D W035A           S             35
020700981204     D Wcari1          S              1
020800980521     D xx              S              3  0
020900990324     D Wlm             S              3  0
021000990324     D Wld             S              3  0
021100980721     D W0140           S             14  0
021200980526     D W0010           S              1  0
021300980521     D Wdtgio          S              8  0
021400980521     D dateu           S              8  0
021500990414     D numcacar        S              4  0
021600990414     D numcaval        S              4  0
021700981008     D numcaesc        S              4  0
021800990315     D nrr1            S              4  0                                      Assoluto
021900990315     D nrr2            S              4  0                                      Comodo x lettura
022000990315     D nrr3            S              4  0                                      rcd elaborato
022100990315     D nrr23           S              4  0                                      1° rcd pag x rollup
022200990312     D wcapagina       S              4  0
022300981204     D W0040           S              4  0
022400981204     D Wdsnumca        S             14  0
022500981204     D Mdsnumca        S             14  0
022600980923
022700980604     D DATA_eur        S               D   DATFMT(*eur)
022800980721     D DATA_oggi       S               D   DATFMT(*eur)
022900980721     D DATA_f15        S               D   DATFMT(*eur)
023000980604      *
023100980622      *  titolo videata (lunghezza massima 34)
023200980624     D TIT_A           C                   CONST('***       APERTURA C. A.     -
023300980624     D                                       ***')
023400980925     D TIT_G           C                   CONST('***   AVANZAMENTO FASE C. A. -
023500980521     D                                       ***')
023600980527     D TIT_I           C                   CONST('***    INTERROGAZIONE C. A.  -
023700980527     D                                       ***')
023800980619     D TIT_M           C                   CONST('***     MANUTENZIONE C. A.   -
023900980619     D                                       ***')
024000980623      *
024100980622      *  Descrizione opzioni (lunghezza massima 78)
024200981022     D DOPZ1_A         C                   CONST('2=Manutenzione   4=Note in St-
024300981022     D                                     ampa   5=Visual. C.A.   9=Visual. bo-
024400981022     D                                     lla')
024500981022     D DOPZ2_A         C                   CONST(' ')
024600981022     D DOPZ1_G         C                   CONST('2=Manutenzione   4=Note in St-
024700981022     D                                     ampa   5=Visual. C.A.   9=Visual. bo-
024800981022     D                                     lla')
024900981022     D DOPZ2_G         C                   CONST('G=Gestione')
025000981022     D DOPZ1_G1        C                   CONST('2=Manutenzione   4=Note in St-
025100981022     D                                     ampa   5=Visual. C.A.   9=Visual. bo-
025200981022     D                                     lla')
025300981022     D DOPZ2_G1        C                   CONST('E=Esclusione     G=Gestione')
025400981022     D DOPZ1_I         C                   CONST('4=Note in Stampa   5=Visual. -
025500981022     D                                     C.A.   8=Gestione Note   9=Visual. b-
025600981022     D                                     olla')
025700981022     D DOPZ2_I         C                   CONST(' ')
025800990406     D DOPZ1_M         C                   CONST('2=Manutenzione    4=Note in S-
025900990406     D                                     tampa    5=Visual. C.A.    8=Gestion-
026000990406     D                                     e Note')
026100990406     D DOPZ2_M         C                   CONST('9=Visual. bolla')
026200990406     D DOPZ2_M1        C                   CONST('9=Visual. bolla   R=Manut. Re-
026300990406     D                                     spon.')
026400120822      * per Consuldanni non esiste +
026500120822     D**DOPZ1_CG        C                   CONST('5=Visual. C.A.   G=Gestione  -
026600120822     D**                                                                         -
026700120822     D**                                         ')
026800120822     D**DOPZ1_CI        C                   CONST('5=Visual. C.A.               -
026900120822     D**                                                                         -
027000120822     D**                                         ')
027100120822     D**DOPZ2_C         C                   CONST(' ')
027200981022      *
027300980622      *  Opzioni valide
027400981022     D OPZ_A           S             20    INZ('2459')
027500981022     D OPZ_G           S             20    INZ('2459G')
027600981022     D OPZ_G1          S             20    INZ('2459GE')
027700981022     D OPZ_I           S             20    INZ('4589')
027800981022     D OPZ_M           S             20    INZ('24589')
027900990406     D OPZ_M1          S             20    INZ('24589R')
028000120822     D******OPZ_CG          S             20    INZ('5G')
028100120822     D******OPZ_CI          S             20    INZ('5')
028200980622      *
028300980521      *   S C H I E R E
028400110930     D MSG             S             78    DIM(40) CTDATA PERRCD(1)             MSG VIDEO
028500980622     D SEL             S              1    DIM(20)                              SELEZIONI
028600980622     D OPZ             S              1    DIM(20)                              OPZIONI
028700980521     D L6              S              3  0 DIM(30)                              P.O.GESTITI
028800990215     D WL6             S              3  0 DIM(30)                              P.O.GESTITI
028900980702     D S3A             S              2    DIM(30)                              Tipo bolla
029000980923     D KTA             S              2    DIM(50)                              Chiave TAD
029100980923     D DTA             S            256    DIM(50)                              Dettaglio TAD
029200980521      *
029300980521      *   D S   I N T E R N E / E S T E R N E
029400980521      *
029500980521     D DS_V1_CA        DS                  INZ
029600980521     D  v1cfil
029700980521     D  v1cnca
029800980521      *
029900980521     D DS_V1_SPE       DS                  INZ
030000980521     D  v1clnp
030100980521     D  v1cnrs
030200980521     D  v1cnsp
030300980722      *
030400980722     D DSNUMCA         DS                  INZ
030500980722     D  v2haac
030600980722     D  v2cfil
030700980722     D  v2cnca
030800980525      *
030900161122     D*DS_RICBOLA      DS                  INZ
031000161122     D* DSRBA_aas             14     17  0
031100161122     D* DSRBA_lnp             18     20  0
031200161122     D* DSRBA_nrs             21     22  0
031300161122     D* DSRBA_nsp             23     29  0
031400161122     D* DSRBA_f03             30     30
031500161122     D* DSRBA_flg             31     31
031600980624      *
031700161122     D*DS_RICBOLP      DS                  INZ
031800161122     D* DSRBP_aas              1      4  0
031900161122     D* DSRBP_lnp              5      7  0
032000161122     D* DSRBP_nrs              8      9  0
032100161122     D* DSRBP_nsp             10     16  0
032200161122     D* DSRBP_f03             17     17
032300161122     D* DSRBP_flg             31     31
032400981009      *
032500161122     D*DS_VISBOLP      DS                  INZ
032600161122     D* DSVBP_aas              1      4  0
032700161122     D* DSVBP_lnp              5      7  0
032800161122     D* DSVBP_nrs              8      9  0
032900161122     D* DSVBP_nsp             10     16  0
033000161122     D* DSVBP_f03             17     17
033100161122     D* DSVBP_rsut            23     42
033200161122     D* DSVBP_sim             44     46  0
033300980624      *
033301161122     d fnlru6ds      e ds
033302161122      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
033303161122     D PARAMu6ds       DS
033304161122     d* CAMPI DI OUTPUT:
033305161122     d* . Key spedizione
033306161122     D  pa1AAS                14     17  0
033307161122     D  pa1LNP                18     20  0
033308161122     D  pa1NRS                21     22  0
033309161122     D  pa1NSP                23     29  0
033310161122     d* . Flag '1'=Premuto F3=Fine
033311161122     D  PA1F03                30     30
033312161122     d* CAMPI DI INPUT:
033313161122     d* . Modalità di richiamo
033314161122     D  PA1FLG                31     31
033315161122     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
033316161122     d*                                in interrogazione della bolla ed evitare
033317161122     d*                                errore di chiamata ricorsiva
033318161122     D  PA1GIA               144    144
033319161122     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
033320161122     d  pa1rma               149    163
033321161122     d  pa1xco               164    164
033322161122     d* non utilizzato
033323161122     d  pa1flo               165    256
033324161122      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
033325161122     D PARAMU6ds1      DS
033326161122     d** INPUT/OUTPUT (se richiesta la selezione bolla)
033327161122     D  pa1AAS_               14     17  0
033328161122     D  pa1LNP_               18     20  0
033329161122     D  pa1NRS_               21     22  0
033330161122     D  pa1NSP_               23     29  0
033331161122     d** OUTPUT
033332161122     D  PA1F03_               30     30
033333161122     d** INPUT
033334161122     D  PA1FLG_               31     31
033335161122     D*    SE RICH DA PGM GIAC. = 1
033336161122     D  PA1GIA_              144    144
033337161122     d** OUTPUT
033338161122     d  pa1rma_              149    163
033339161122     d  pa1xco_              164    164
033340161122     d** INPUT
033341161122     d*   Tipo lancio: "C" -           CHIUSO CON LR
033342161122     d*                "L" - ELABORO E CHIUDO CON LR
033343161122     d*                " " - ELABORO E CHIUDO IN RETRN
033344161122     d  pa1tla_              165    165
033345161124     d  pa1err_              166    166
033346161124     d  pa1flo_              167    256
033400980525     D                 DS                  INZ
033500980525     D w_v1cdcad               1      8  0
033600980525     D  w_AAdcad               1      4  0
033700980525     D  w_MGdcad               5      8  0
033800980521      *
033900980525     D                 DS                  INZ
034000980525     D w_v1cdefd               1      8  0
034100980525      *
034200980521     D WLBDAT          DS                  INZ
034300980521     D  G02DAT                 1      8  0
034400980521     D  G02INV                 9     16  0
034500980521     D  G02ERR                17     17
034600980521     D  G02TGI                18     22  0
034700980525      *
034800980521      * Ricezione dati da azione
034900980521     D DS_AZIONE       DS                  INZ
035000980521     D  AZN_tipo               1      1
035100980521     D  AZN_fase               2      4
035200980521     D  AZN_trpr               5      5
035300980525      *
035400980521     D FIDN00DS      E DS
035500980529     D FIDN05DS      E DS
035600980722     D FIDN10DS      E DS
035700040714      *
035800040714     D FIDN12DS      E DS
035900040714     d  skKey                 26    305    dim(20)
036000040714     d  skAnn                306    325    dim(20)
036100040714     d  skDca                326    485  0 dim(20)
036200040714     d  skFca                486    545  0 dim(20)
036300040714     d  skDch                546    705  0 dim(20)
036400040714     d  skCch                706    745    dim(20)
036500040714
036600040714     d dsKey           ds
036700040714     d  dsaac                         4  0
036800040714     d  dsfil                         3  0
036900040714     d  dsnca                         7  0
037000040714      *
037100081215     D FIDNBEDS      E DS
037200990806     D TNSB50DS      E DS
037300980529     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
037400130806     D************ UT§DSE0F      E DS
037500980601     D TIBS02DS      E DS
037600980601     D FNLV50DS      E DS
037700130806     D************ CNCR80        E DS
037800981229     D DDCT01        E DS
037900980601     D DTAD          E DS
038000980601     D DS3A          E DS
038100990406     D DSTD          E DS
038200010910     D DGEDDN        E DS                  INZ
038300980525      *
038400130806      * - Parametri x Controllo profilo utenti
038500130806     d TIBS34ds      e ds                  inz
038600130806      * - Ds di riferimento al file esterno AZUTE00F
038700130806     d AZUTEds       e ds                  extname(AZUTE00F)
038800130806      * - Ds per dati organigramma
038900130806     d dDatiUte      e ds
039000130806
039100980525      * DS PER trul06r - caricamento £X
039200980521     D TRUL06DS      E DS
039300980521     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
039400980521      *
039500980521     D KPJBA         E DS
039600020430     D* rem                    1      3
039700020430     D* remfil                 4      6
039800130806
039900130806     d dUTE01        e ds
040000980521      *
040100980521     D                SDS
040200980521     D  VTCPGM                 1     10
040300981009
040400981009      *  titolo videata (lunghezza massima 34)
040500981009     D Kannullata      C                   CONST('   ANNULLATA                 -
040600981009     D                                         ')
040700980623
040800980623      *------------------------------------------------------------------------*
040900980521      *---------------  C A L C O L O  ----------------------------------------*
041000980521      *------------------------------------------------------------------------*
041100980623
041200980521     C     for01         tag
041300981104     c                   eval      Wcari1 = 'S'
041400980623
041500980521      * Emissione VIDEO1
041600980521     C                   exsr      EMISd01
041700980623
041800990115      * Azzero Campi ed Indicatori
041900990115     c     41            do        89            XX
042000990115     c                   eval      *IN(XX) = *OFF
042100990115     c                   enddo
042200990115     c                   clear                   V1Cmsg
042300990115     c                   eval      *IN28 = *off
042400990115     c                   eval      *IN90 = *off
042500990115
042600980525      * f3=Fine
042700980521     c   kc              goto      fine
042800980623
042900980521      * f18=Cambio P.O. in gestione
043000980522     c   ks              eval      *in05 = *on
043100980521     c   ks              goto      for01
043200980623
043300980521      * 05 on --> controllo P.O. in gestione
043400980521     c     *in05         ifeq      *on
043500980521     C                   exsr      ctrfgs
043600980521     c   90              goto      for01
043700980608     c                   eval      *in05 = *off
043800980521     c                   end
043900980623
044000161122      * F07=interrogazione bolle.
044100981009     C                   If        *INKG = *ON
044200980623
044300020821     C                   If        V1Cfgs = 046                                 bolle SEDE
044400990806     c                   clear                   TNSB50DS
044500990910     C                   MOVEL     'D00'         I50OP0
044600990806     C                   MOVEL(P)  TNSB50DS      KPJBU
044700990806     C                   CALL      'TNSB50R'
044800980522     C                   PARM                    KPJBA
044900980526     c                   else                                                   bolle ARRIVI
045000980624      *
045100161122     c*                  clear                   DS_ricbolA
045200161122     C*                  MOVEL     2             DSRBA_flg
045300161122     C*                  MOVEL(P)  DS_ricbola    KPJBU
045400161122     C*                  CALL      'FNLR36R'
045500161122     C*                  PARM                    KPJBA
045600161122     C*                  MOVEL     kpjbu         DS_ricbola
045700161122     c*                  if          DSRBA_nsp > *zeros
045800161122     C*                  z-add     DSRBA_aas     V1Caas
045900161122     C*                  z-add     DSRBA_lnp     V1Clnp
046000161122     C*                  z-add     DSRBA_nrs     V1Cnrs
046100161122     C*                  z-add     DSRBA_nsp     V1Cnsp
046200161122     C*                  endif
046202161122     c                   clear                   paramu6ds
046203161122     C                   MOVEL     '2'           Pa1FLG
046204161122     C                   MOVEL     PARAmu6ds     KPJBU
046210161122     C                   CALL      'FNLRU6R'
046211161122     C                   PARM                    KPJBA
046213161122     C                   MOVEL     KPJBU         PARAMu6ds
046214161122     C                   IF        pa1nsp>*zeros
046215161122     C                   Z-ADD     Pa1AAS        V1CAAS
046216161122     C                   Z-ADD     Pa1LNP        V1CLNP
046217161122     C                   Z-ADD     Pa1NRS        V1CNRS
046218161122     C                   Z-ADD     Pa1NSP        V1CNSP
046219161122     C                   ENDIF
046300990201     C                   endif
046400980624      *
046500980522     C                   goto      for01
046600980522     C                   endif
046700980623
046800980526      * F14=interrogazione bolle partenze
046900161122     C*                  If        *INKN = *ON
047000161122     c*                  clear                   DS_ricbolP
047100161122     C*                  MOVEL     1             DSRBP_flg
047200161122     C*                  MOVEL(P)  DS_ricbolp    KPJBU
047300161122     C*                  CALL      'FNLS04R'
047400161122     C*                  PARM                    KPJBA
047500161122     C*                  MOVEL     kpjbu         DS_ricbolp
047600161122     c*                  if          DSRBP_nsp > *zeros
047700161122     C*                  z-add     DSRBP_aas     V1Caas
047800161122     C*                  z-add     DSRBP_lnp     V1Clnp
047900161122     C*                  z-add     DSRBP_nrs     V1Cnrs
048000161122     C*                  z-add     DSRBP_nsp     V1Cnsp
048100161122     C*                  endif
048200161122     C*                  goto      for01
048300161122     C*                  endif
048400980623
048500980525      * Controlli 1a videata
048600980521     c                   exsr      ctrd01
048700980623
048800980521     c   90              goto      for01
048900980623
049000990312      * In Apertura se premuto F10 controllo se selezioni valide, se ok richiamo pgm per apertura
049100980527     C                   if        *INKJ = *ON
049200980527     c                   exsr      ctrd01_A
049300990312     c  N90              EXSR      sub_selopn
049400980610     c                   goto      for01
049500980610     c                   ENDIF
049600980623
049700990406      * Imposto opzioni in base al tipo elaborazione
049800990406     c                   exsr      CARopz
049900981008
050000980526      * Carico subfile
050100990315     C     RIE02         TAG
050200990315      *
050300981102     C                   eval      *in91 = *off
050400990315     C                   eval      *in23 = *off
050500990315     C                   clear                   nrr1
050600990315     c                   clear                   nrr3
050700990315     c                   clear                   Numcaval
050800990414     c                   clear                   Numcacar
050900981102      *
051000980526     C                   exsr      RIEd02
051100980623
051200990115      * Se non ho caricato record torno a VIDEO 1, se arrivo da Video 1 emetto prima una videata di
051300990115      *    segnalazione. Se nel caricamento ho trovato errori torno a Video 1
051400981104     C                   if        NRR1 = *zeros
051500990115     C     *INKL         DOWEQ     *OFF
051600981104     C     WCARI1        andeq     'S'
051700990115     C     *IN28         andeq     *OFF
051800980527     C                   EXFMT     FI00D03
051900980528     C                   ENDDO
052000980527     c                   goto      for01
052100980609     C                   endif
052200981104     c                   clear                   Wcari1
052300980623
052400980526      * Emissione VIDEO2
052500980623     C     for02         tag
052600980526     C                   exsr      EMISd02
052700980623
052800990315      * F03 = Fine
052900980526     c   kc              goto      fine
053000980623
053100990315      * F12 = Precedente
053200980526     c   kl              goto      for01
053300990315
053400990315      * ROLLUP = Carico il subfile
053500990505     c                   IF        *IN23 = *ON  and  AZN_tipo <> 'G'
053600990315     c                   exsr      Ried02
053700990315     c                   If        Wcapagina = *zeros
053800990315     c                   movel     msg(36)       V2Cmsg
053900990315     c                   eval      *in28 = *on
054000990315     c                   Endif
054100990315     c                   GOTO      for02
054200990315     c                   ENDIF
054300980623
054400981026      * I Controlli in video 2 sono diversi se ho selezionato fasi particolari ed ho premuto F01
054500981026     c                   IF        *INKA = *ON   and
054600981203     c                              (W_V1Cfca = WFisa  or  W_V1Cfca = WFise
054700981203     c                               or  W_V1Cfca = WFecc  or  W_V1Cfca = WFsav)
054800981008     c                   exsr      ctrd02ka
054900981008     c                   ELSE
055000981008     c                   exsr      ctrd02
055100981008     c                   ENDIF
055200980623
055300990315      * Ricarico il SFL per aggiornamento dati
055400981008     c   91              goto      rie02
055500981008
055600981008      * Riemetto video 2
055700981008     c                   goto      for02
055800980623
055900980521     c     fine          tag
056000981204
056100981204      * Chiudo pgm aperti
056200981029     c                   clear                   tibs02ds
056300981029     c                   movel     'C'           t02tla
056400981029     c                   call      'TIBS02R'
056500981029     c                   parm                    kpjba
056600981029     c                   parm                    tibs02ds
056700981204
056800981204     c                   EVAL      *INLR = *ON
056900980521      **********************************************************************
057000980521      * EMISSIONE VIDEO 1
057100980521      **********************************************************************
057200980521     C     EMISD01       BEGSR
057300980521      *
057400990325      * In apertura, se non ci sono errori, mi posiziono sulla linea di partenza del n° spedizione
057500990310     c                   IF        AZN_tipo = 'A'  and  *IN47 = *off
057600990310     c     41            Do        89            XX
057700041026     c                   If        XX <> 47  and  XX < 65
057800990310     c                   eval      *IN47 = (*IN(XX) = *OFF)
057900990310     c  N47              leave
058000990310     c                   EndIf
058100990310     c                   EndDo
058200990310     c                   ENDIF
058300990310
058400980521     c                   write     Fi00t01
058500130403      * Imposto a video "BAR" nel campo ditta della pratica assicurativa. Metto il campo protetto
058600130403     c                   eval      v1cdit = 'BAR'
058700990316      * Se sede emetto videata diversa
058800980528     c                   IF        SIMFEL = *zeros
058900990316     c                   write     Fi00z04
059000990316     c                   exfmt     Fi00d04
059100980521     c                   ELSE
059200980521     c                   write     Fi00z01
059300990316     c                   exfmt     Fi00d01
059400980521     c                   ENDIF
059500990316
059600980605     c                   setoff                                       289047
059700980521     c                   clear                   v1cmsg
059800990316
059900980521     C                   ENDSR
060000980521      **********************************************************************
060100980521      * CONTROLLO IL P.O. IN IN GESTIONE
060200980521      **********************************************************************
060300980521     C     CTRFGS        beGSR
060400980521      *
060500980609     C                   setoff                                       9040
060600980521     c                   clear                   v1dfgs
060700980609     C                   clear                   FNLV50DS
060800980521     C                   MOVEL     KNMUS         D50PRU
060900980521     C                   MOVEL     V1cfgs        D50FGS
061000980521     C                   CALL      'FNLV50R'
061100980521     C                   PARM                    FNLV50DS
061200980521      *
061300980521     C                   if        D50ERR <> *blanks
061400980521     C                   MOVEL     D50MSG        V1cMSG
061500980521     C                   SETON                                        409028
061600980609     C                   else
061700980521      *
061800990215     C                   z-add     v1cfgs        Wfgs
061900990217     C                   CLEAR                   L6
062000980521     C                   EXSR      CARL6
062100990215     C                   MOVEA     LIN           L6
062200980521      * decodifica
062300980609     c     v1cfgs        CHAIN     azorg                              31
062400040121     C  N31              movel     orgDES        V1DFGS
062500981009      *
062600980609     C                   endif
062700980521      *
062800980609     c                   endsr
062900980521      **************************************************************************
063000980521      * CARICO SCHIERA L6 DA TABELLA £6
063100980521      **************************************************************************
063200980521     C     carl6         BEGSR
063300980521      *
063400980521     C                   CLEAR                   TRUL06DS
063500980521     C                   MOVE      '£6'          D06COD
063600990215     C                   MOVEL(P)  Wfgs          D06key
063700981009     C                   MOVEL(P)  TRUL06DS      KPJBU
063800980521     C                   CALL      'TRUL06R'
063900980521     C                   PARM                    KPJBA
064000980521     C                   MOVEL     KPJBU         TRUL06DS
064100980521      *
064200980521     c                   endsr
064300980521      **********************************************************************
064400980521      * CONTROLLI VIDEO 1
064500980521      **********************************************************************
064600980521     C     Ctrd01        BEGSR
064700990115      *
064800990115      * Azzero campi di comodo e schiera selezioni
064900980521     c                   clear                   wtibol
065000980521     c                   clear                   waas
065100980521     c                   clear                   wnsp
065200980521     c                   clear                   wlnab
065300980622     C                   clear                   sel
065400990324     c                   clear                   Wlm
065500990324     c                   clear                   Wld
065600980521      *
065700980527     C                   SELECT
065800980527      *
065900020821     C                   WHEN      V1Cnca > 0  and V1Cnsp = 0
066000980527     C                   EXSR      CTRD01_ca                                    controlli numero CA
066100990111     c                   If        *in28 = *off  and  AZN_tipo <> 'I'
066200990111     C                   EXSR      CTRD01_sel                                   controlli selezioni
066300990111     c                   Endif
066400980527      *
066500020821     C                   WHEN      V1Cnca = 0  and V1Cnsp > 0
066600980527     C                   EXSR      CTRD01_spe                                   controlli spedizione
066700990111     c                   If        *in28 = *off  and  AZN_tipo <> 'I'
066800990111     C                   EXSR      CTRD01_sel                                   controlli selezioni
066900990111     c                   Endif
067000980527      *
067100020821     C                   WHEN         DS_V1_CA  = *ZEROS and
067200980527     C                                DS_V1_SPE = *ZEROS
067300980527     C                   EXSR      CTRD01_sel                                   controlli selezioni
067400980527      *
067500980527     C                   OTHER
067600980527     C                   MOVEL     MSG(12)       v1CMSG                         errore
067700980527     c                   seton                                        2890
067800980527      *
067900980527     C                   ENDSL
068000980527      *
068100980527     c                   ENDSR
068200980527      **********************************************************************
068300980622      * CONTROLLI VIDEO 1     * controllo selezione CA *
068400980527      **********************************************************************
068500980527     C     CTRD01_ca     BEGSR
068600980527      *
068700980527      * Controllo linea creazione
068800980527     C                   z-add     V1Cfil        W_linea
068900980527     c                   exsr      CTR_linea
069000980527     c                   IF        *IN28 = *ON
069100980527     c                   eval        *IN44 = *on
069200980527     C                   ELSE
069300980527      *
069400980527      * controllo l'anno
069500980527     c                   z-add     V1Caac        W_anno
069600980527     c                   exsr      CTR_anno
069700980527     c                   if        *IN28 = *ON
069800980527     c                   eval        *IN46 = *on
069900980527     C                   else
070000980527     c                   z-add     W_anno        V1Caac
070100980622     c                   movel     '1'           SEL(2)
070200980527     c                   endif
070300980527      *
070400980527     c                   ENDIF
070500980527      *
070600980527     c                   ENDSR
070700980527      **********************************************************************
070800980622      * CONTROLLI VIDEO 1     * controllo selezione spedizione *
070900980527      **********************************************************************
071000980527     C     CTRD01_spe    BEGSR
071100980527      *
071200980527      * Controllo linea di partenza
071300980527     C                   z-add     V1Clnp        W_linea
071400980527     c                   exsr      CTR_linea
071500980527     c                   IF        *IN28 = *ON
071600980527     c                   eval        *IN47 = *on
071700980527     C                   ELSE
071800980527      *
071900980527      * controllo l'anno
072000980527     c                   z-add     V1Caas        W_anno
072100980527     c                   exsr      CTR_anno
072200980527     c                   IF        *IN28 = *ON
072300980527     c                   eval        *IN49 = *on
072400980527     c                   else
072500980527     c                   z-add     W_anno        V1Caas
072600980622     c                   movel     '1'           SEL(3)
072700980527     c                   endif
072800980527      *
072900980527     c                   endif
073000980527      *
073100980527     c     ev1spe        ENDSR
073200980527      **********************************************************************
073300980527      * CONTROLLI VIDEO 1     * controllo selezioni *
073400980527      **********************************************************************
073500980527     C     CTRD01_sel    BEGSR
073600980527      *
073700980527      * Controllo il tipo anomalia
073800980601     c                   movel     V1Ctad        W_ctad
073900980527     C                   EXSR      CTR_anomal
074000980601     c                   movel     W_ctad        V1Ctad
074100980601     c                   movel     W_dtad        V1Dtad
074200980601     c   28              movel     W_MSG         V1Cmsg
074300980601     C                   IF        V1Ctad <> *blanks
074400980622     c                   movel     '1'           SEL(4)
074500980527     C                   endif
074600980601     C   90              GOTO      ev1sel
074700980527      *---------------------------------------------
074800981026     c                   clear                   DETtad
074900981026      * Se numero evento > 0 controllo
075000981026     c                   IF        V1Cnev > 0
075100981026      *    anno evento
075200981026     c                   z-add     V1Caae        W_anno
075300981026     c                   exsr      CTR_anno
075400981026     c                   IF        *IN28 = *ON
075500981026     c                   eval        *IN52 = *on
075600981026     C                   ELSE
075700981026     c                   z-add     W_anno        V1Caae
075800981026      *    controllo su file eventi
075900981026     c     KDET          CHAIN     FNDET000                           28
076000981026     c                   IF        *IN28 =*ON
076100981026     c                   movel     MSG(33)       V1Cmsg
076200981026     c                   seton                                        9051
076300981026     C                   ELSE
076400981026     c                   movel     '1'           SEL(5)
076500981026     c                   ENDIF
076600981026     c                   ENDIF
076700981026      *
076800981026     c                   ENDIF
076900981026     C   28              goto      ev1sel
077000980527      *---------------------------------------------
077100980527      * Controllo data C.A. "dal"
077200980527     C                   clear                   W_v1cdcad
077300980527     C                   IF        V1Cdcad <> 0
077400980527     C                   MOVE      V1Cdcad       G02DAT
077500980527     C                   eval      g02err = *blanks
077600980527     C                   CALL      'XSRDA8'
077700980527     C                   PARM                    WLBDAT
077800980527     C                   if        G02ERR = '1'
077900980527     C                   SETON                                        289053
078000980527     C                   MOVEL     MSG(15)       V1CMSG
078100980527     C                   goto      ev1sel
078200980527     C                   else
078300980527     C                   MOVEL     G02DAT        V1Cdcad
078400980527     C                   MOVE      G02INV        W_v1cdcad
078500980527     C                   endif
078600980527     C                   ENDIF
078700980527      * Controllo data C.A. "al"
078800980527     C                   clear                   W_v1cdcaa
078900980527     C                   IF        V1Cdcaa <> 0
079000980527     C                   move      V1cdcaa       G02DAT
079100980527     C                   eval      g02err = *blanks
079200980527     C                   CALL      'XSRDA8'
079300980527     C                   PARM                    WLBDAT
079400980527     C                   if        G02ERR = '1'
079500980527     C                   SETON                                        289054
079600980527     C                   MOVEL     MSG(15)       V1CMSG
079700980527     C                   goto      ev1sel
079800980527     C                   else
079900980527     C                   MOVEL     G02DAT        V1Cdcaa
080000980527     C                   MOVE      G02INV        W_v1cdcaa
080100980527     C                   endif
080200980527      * data C.A. "dal" maggiore di data C.A. "al"
080300980527     C                   if        W_v1cdcaa < W_v1cdcad
080400980527     C                   SETON                                        289053
080500980527     C                   MOVEL     MSG(16)       V1CMSG
080600980527     C                   goto      ev1sel
080700980527     C                   endif
080800980527     C                   ENDIF
080900980527      * Se data C.A. "al" è vuota, la imposto come la data C.A. "dal"
081000980527     C                   if        W_v1cdcaa = 0  AND  W_v1cdcad <> 0
081100980527     C                   z-add     W_v1cdcad     W_v1cdcaa
081200980527     C                   endif
081300980622      * Imposto selezione
081400980527     C                   if        W_v1cdcaa > *zeros
081500980622     c                   movel     '1'           SEL(6)
081600980527     C                   endif
081700980527      *---------------------------------------------
081800980622      * Imposto selezione Liquidazione transattiva/Pratica assicurativa
081900980527     c                   IF        V1cfpr <> *blanks
082000980622     c                   movel     '1'           SEL(7)
082100980527     C                   endif
082200980527      *---------------------------------------------
082300980605      * Controllo codice fase solo se non ricevuta in KPJBA
082400980527      *
082500990201     C                   IF        *IN10 = *off
082600990201     C                   clear                   V1Dfca
082700990201     C                   IF        V1Cfca <> *blanks  and  V1Cfca <> '000'
082800980615      *
082900980529      * Ricerca
083000980605     C     '?'           SCAN      v1CFCA                                 30
083100980529     C                   IF        *IN30 = *ON
083200980529     C                   seton                                        9056
083300980529     c                   clear                   FIDN05DS
083400980529     c                   movel     'R'           I05MOD
083500980529     C                   CALL      'FIDN05R'
083600980604     c                   PARM                    KPJBA
083700980604     c                   PARM                    FIDN05DS
083800980605     c                   if        O05err = 'E'
083900980605     c                   movel     '000'         V1Cfca
084000980605     c                   else
084100980529     c                   movel     O05REC        DS_FNDFA
084200980605     c                   movel     DFAfca        V1Cfca
084300980615     c                   if        AZN_tipo = 'G'  and  DFAdgc <> *blanks
084400980611     c                   movel     DFAdgc        V1Dfca
084500980611     c                   else
084600980605     c                   movel     DFAdes        V1Dfca
084700980529     C                   endif
084800980611     C                   endif
084900980527      *
085000980529     c                   ELSE
085100980529      *
085200980529     C                   testn                   V1Cfca               30
085300980529      * testo ultimo carattere
085400980529     C   30              move      V1CFCA        W001A
085500980529     C   30              eval      *in30 = (W001A >= '0')
085600980529      * Se codice non numerico errore
085700980529     C                   IF         *IN30 = *OFF
085800980529     C                   eval      *in28 = *on
085900980529     C                   ELSE
086000980529      * Controllo
086100980529     c                   clear                   FIDN05DS
086200980529     c                   movel     'C'           I05MOD
086300980529     c                   movel     V1Cfca        I05fca
086400980529     C                   CALL      'FIDN05R'
086500980604     c                   PARM                    KPJBA
086600980529     c                   PARM                    FIDN05DS
086700980529     c                   if        O05rec <> *blanks
086800980529     c                   movel     O05REC        DS_FNDFA
086900980615     c                   if        AZN_tipo = 'G'  and  DFAdgc <> *blanks
087000980611     c                   movel     DFAdgc        V1Dfca
087100980611     c                   else
087200980611     c                   movel     DFAdes        V1Dfca
087300980611     C                   endif
087400980529     C                   endif
087500980527      *
087600980529     C                   endif
087700980529     C                   ENDIF
087800980529      *
087900980605     C                   IF        *IN28 = *ON  or  O05err = 'E'
088000980605     C                             or  DFAatb <> *blanks
088100980605     C                   seton                                        289056
088200980605     C                   movel     MSG(17)       V1CMSG
088300980605     C                   ELSE
088400980605      *
088500980608      * Utilizzabile in Gestione
088600980907     c                   if        AZN_tipo = 'G' and DFAfgc <> *blank
088700980605     C                   seton                                        289056
088800980605     C                   movel     MSG(21)       V1CMSG
088900980529     C                   else
089000980605      *
089100980527      * Controllo se selezione "Liq.Trans./Pratica Ass." è congruente con cod. fase
089200980529     c                   IF        V1cfpr <> *blanks  and  DFAval <> *blanks
089300980527     c                             and V1Cfpr <>  DFAval
089400980605     C                   seton                                        289056
089500980605     C                   movel     MSG(18)       V1CMSG
089600980605     C                   ENDIF
089700980529     C                   endif
089800980605     C                   ENDIF
089900980605      *
090000980605     C                   ENDIF
090100990201     C                   ENDIF
090200980529      *
090300980622      * Imposto selezione e memorizzo fase
090400980605     C                   IF        V1Cfca <> *blanks  and  V1Cfca <> '000'
090500980605     C                             and  *IN28 = *off
090600981026     C                   movel     V1Cfca        W_V1Cfca
090700980622     c                   movel     '1'           SEL(8)
090800981026     C                   ELSE
090900981026     C                   clear                   W_V1Cfca
091000981026     C                   ENDIF
091100980529      *
091200980529     C   90              goto      ev1sel
091300980527      *---------------------------------------------
091400980527      * Controllo data esecuzione fase "dal"
091500980527     C                   clear                   W_v1cdefd
091600980527     C                   IF        V1Cdefd <> 0
091700980527     C                   MOVE      V1Cdefd       G02DAT
091800980527     C                   eval      g02err = *blanks
091900980527     C                   CALL      'XSRDA8'
092000980527     C                   PARM                    WLBDAT
092100980527     C                   if        G02ERR = '1'
092200980527     C                   SETON                                        289057
092300980527     C                   MOVEL     MSG(15)       V1CMSG
092400980527     C                   goto      ev1sel
092500980527     C                   else
092600980527     C                   MOVEL     G02DAT        V1Cdefd
092700980527     C                   MOVE      G02INV        W_v1cdefd
092800980527     C                   endif
092900980527     C                   ENDIF
093000980527      * Controllo data esecuzione fase "al"
093100980527     C                   clear                   W_v1cdefa
093200980527     C                   IF        V1Cdefa <> 0
093300980527     C                   move      V1cdefa       G02DAT
093400980527     C                   eval      g02err = *blanks
093500980527     C                   CALL      'XSRDA8'
093600980527     C                   PARM                    WLBDAT
093700980527     C                   if        G02ERR = '1'
093800980527     C                   SETON                                        289058
093900980527     C                   MOVEL     MSG(15)       V1CMSG
094000980527     C                   goto      ev1sel
094100980527     C                   else
094200980527     C                   MOVEL     G02DAT        V1Cdefa
094300980527     C                   MOVE      G02INV        W_v1cdefa
094400980527     C                   endif
094500980527      * data esecuzione fase "dal" maggiore della data esecuzione fase "al"
094600980527     C                   if        W_v1cdefa < W_v1cdefd
094700980527     C                   SETON                                        289057
094800980527     C                   MOVEL     MSG(16)       V1CMSG
094900980527     C                   goto      ev1sel
095000980527     C                   endif
095100980527     C                   ENDIF
095200980527      * Se data esecuzione fase "al" è vuota, la imposto come la data esecuzione fase "dal"
095300980527     C                   if        W_v1cdefa = 0  AND  W_v1cdefd <> 0
095400980527     C                   z-add     W_v1cdefd     W_v1cdefa
095500980527     C                   endif
095600980527      * Se impostata la data esecuzione fase è obbligatorio il codice fase
095700980527     C                   IF        V1CFCA = *BLANKS   and   V1CDEFA <> *ZEROS
095800980527     C                   MOVEL     MSG(20)       V1CMSG
095900980527     C                   SETON                                        289056
096000980527     C                   goto      ev1sel
096100980527     C                   endif
096200980622      * Imposto selezione
096300980527     C                   if        W_v1cdefa > *zeros
096400980622     c                   movel     '1'           SEL(9)
096500980527     C                   endif
096600980527      *---------------------------------------------
096700980527      * Controllo P.O per selezione
096800980527     c                   eval      V1Dpos = *blanks
096900981026     C                   IF        V1Cpos > *zeros
097000980622     c                   movel     '1'           SEL(10)
097100980527     C                   z-add     V1Cpos        W_linea
097200980527     c                   exsr      CTR_linea
097300980527     c                   if        *IN28 = *ON
097400980527     c                   eval        *IN59 = *on
097500980527     C                   goto      ev1sel
097600980527     c                   else
097700990215     C                   z-add     v1cpos        Wfgs
097800990217     C                   CLEAR                   WL6
097900990215     C                   EXSR      CARL6
098000990215     C                   movea     LIN           WL6
098100980527     c                   movel     ORGDES        V1Dpos
098200980527     c                   endif
098300980527     c                   ENDIF
098400981026      *---------------------------------------------
098500981026      * Se impostata fase INVIO A SEDE DOCUMENTI C.A. PER DENUNCIA EVENTI obbligo scelta n. evento
098600981026     C                   IF        W_V1Cfca = WFise  and  SEL(5) <> '1'
098700981026     c                   seton                                        289051
098800981026     C                   MOVEL     MSG(32)       V1CMSG
098900981026     C                   goto      ev1sel
099000981026     c                   ENDIF
099100990322      *---------------------------------------------
099200990324      * Controllo pratica assicurativa
099300990322      *
099400990322     c                   SELECT
099500130403     c                   WHEN      V1Cprn = *zeros  and  V1Cdit = 'BAR'
099600130403     c**                 WHEN      V1Cprn = *zeros
099700130403     c**                 movel     msg(8)        V1Cmsg
099800130403     C**                 seton                                        289061
099900130403     c**                 WHEN      V1Cdit <> 'BAR'  and  V1Cdit <> 'SDI'
100000130403     c**                 movel     msg(11)       V1Cmsg
100100130403     C**                 seton                                        289062
100200990322     c                   OTHER
100300990322     c                   z-add     V1Cpra        W_anno
100400990322     c                   exsr      CTR_anno
100500990322     c                   if        *IN28 = *ON
100600990322     C                   seton                                        289063
100700990322     c                   movel     msg(9)        V1Cmsg
100800990322     C                   else
100900990322     c                   z-add     W_anno        V1Cpra
101000990322     c                   movel     '1'           SEL(11)
101100990322     c                   endif
101200990322     c                   ENDSL
101300990322      *
101400990322     C   28              goto      ev1sel
101500080718      *---------------------------------------------
101600080718      * Controllo C.A. APERTE O CHIUSA
101700080718     c                   IF        V1CACE <> *blanks
101800080718     c                   movel     '1'           SEL(15)
101900080718     c                   ENDIF
102000990322      *---------------------------------------------
102100990324      * Controllo mittente
102200990324     c                   IF        V1Cmit <> *blanks
102300990322     c                   movel     '1'           SEL(12)
102400990324     c     ' '           checkr    V1Cmit        Wlm
102500990322     c                   ENDIF
102600990322      *---------------------------------------------
102700990324      * Controllo destinatario
102800990324     c                   IF        V1Cdes <> *blanks
102900990322     c                   movel     '1'           SEL(13)
103000990324     c     ' '           checkr    V1Cdes        Wld
103100990322     c                   ENDIF
103200041026      *---------------------------------------------
103300041026      * Controllo numero e data assegno
103400041026      *
103500041026     c                   SELECT
103600041111     c                   WHEN      V1Cdea = *zeros  and  V1Casn = *blanks
103700041111     c                   WHEN      V1Cdea = *zeros  and  V1Casn <> *blanks
103800041111     C                   clear                   W_v1cdea
103900041111     c                   movel     '1'           SEL(14)
104000041111     c                   WHEN      V1Cdea > *zeros  and  V1Casn = *blanks
104100041026     c                   movel     msg(3)        V1Cmsg
104200041111     C                   seton                                        289064
104300041110     c                   WHEN      V1Cdea > *zeros  and  V1Casn <> *blanks
104400041026     C                   move      V1Cdea        G02DAT
104500041026     C                   eval      g02err = *blanks
104600041111     C                   call      'XSRDA8'
104700041111     C                   parm                    WLBDAT
104800041111     C                   If        G02ERR = '1'
104900041111     C                   seton                                        289065
105000041111     C                   movel     MSG(15)       V1CMSG
105100041111     C                   Else
105200041111     C                   movel     G02DAT        V1Cdea
105300041111     C                   move      G02INV        W_v1cdea
105400041111     c                   movel     '1'           SEL(14)
105500041111     C                   EndIf
105600041026      *
105700041026     c                   ENDSL
105800041026      *
105900041026     C   28              goto      ev1sel
106000150701      *---------------------------------------------
106100150701      * -?Controllo codice e ragione sociale Beneficiario?
106200150701     c                   select
106300150701     c                   when      V1Cksc <> *zeros  and
106400150714     c                             V1Casn =  *blanks
106500150701     c                   seton                                        289066
106600150701     c                   eval      V1Cmsg =  Msg(39)
106700150701     c                   leavesr
106800150701     c                   when      V1Dksc <> *blank  and
106900150714     c                             V1Casn =  *blanks
107000150701     c                   seton                                        289067
107100150701     c                   eval      V1Cmsg =  Msg(39)
107200150701     c                   leavesr
107300150701     c                   when      V1Cksc <> *zeros  and
107400150701     c                             V1Dksc <> *blank
107500150701     c                   seton                                        289066
107600150701     c                   eval      V1Cmsg =  Msg(40)
107700150701     c                   leavesr
107800150701     c                   endsl
107900990322      *---------------------------------------------
108000990322      *
108100980527     c     ev1sel        ENDSR
108200980521      **********************************************************************
108300980527      * CONTROLLI VIDEO 1     * APERTURA *
108400980521      **********************************************************************
108500980521     C     Ctrd01_A      BEGSR
108600980521      *
108700020821      * apertura obbligatoria per spedizione
108800020821     C                   IF        V1Cnsp = 0
108900020821     C                   MOVEL     MSG(5)        v1CMSG
109000980527     c                   seton                                        289047
109100980521     C                   GOTO      ecd01_A
109200980521     c                   end
109300990127
109400980521      * ----------------------------
109500980521      * APERTURA C.A. PER SPEDIZIONE
109600980521      * ----------------------------
109700980521      *
109800980521      * Controllo linea di partenza
109900980522     C                   z-add     V1Clnp        W_linea
110000980527     c                   exsr      CTR_linea
110100980522     c                   IF        *IN28 = *ON
110200980522     c                   eval        *IN47 = *on
110300980522     C                   goto      ecd01_A
110400980522     c                   ENDIF
110500980522      *
110600980522      * controllo l'anno spedizione
110700020821     c                   z-add     V1Caas        W_anno
110800980527     c                   exsr      CTR_anno
110900020821    2c                   IF        *IN28 = *ON
111000980522     c                   eval        *IN49 = *on
111100980522     C                   goto      ecd01_A
111200980522     C                   ELSE
111300020821     c                   z-add     W_anno        V1Caas
111400020821    2c                   ENDIF
111500980521      *
111600981218      * la sped. deve essere in gestione al p.o. in gestione e deve esistere su fnblp o fnarb
111700981218      * se bolla locale apro solamente in arrivo
111800981218     c                   z-add     V1Caas        Kaas
111900981218     c                   z-add     V1Clnp        Klnp
112000981218     c                   z-add     V1Cnrs        Knrs
112100981218     c                   z-add     V1Cnsp        Knsp
112200981218      *
112300981218      * Cerco in partenza
112400981218     c     kspe          chain     fnblp01l                           30
112500020821    2c                   IF        *in30 = *off
112600981218      *   determino se partenza in gestione
112700981218     c     V1Clnp        lookup    l6                                     30
112800020821    3c                   If        *in30 = *on
112900110930     c
113000110930     c* Se bolla non borderizzata --> errore
113100110930     c                   if        arbdbr=0
113200110930     c                   seton                                        489028
113300110930     c                   movel     msg(37)       v1cmsg
113400110930     c                   goto      ecd01_A
113500110930     c                   endif
113600110930     c
113700110930     c* Se bolla chiusa in partenza --> errore
113800110930     c                   if        arbft1='N' and arbft2='N'
113900110930     c                   seton                                        489028
114000110930     c                   movel     msg(38)       v1cmsg
114100110930     c                   goto      ecd01_A
114200110930     c                   endif
114300110930     c
114400981218      *   se l'arrivo non è in gestione, quindi la sped. non è locale, gestione Partenza
114500981218      *   viceversa se è locale aggancio FNARB
114600981218     c     arblna        lookup    l6                                     30
114700981218     c  N30              movel     'P'           wtibol
114800110620     c* Se è locale ma dirottata--> la tratto come "P"
114900110620     c*  queto controllo lo devo fare qui perchè per le bolle autogenerate non mi crea FNARB
115000110620     c*  su locale se bolle legata ma solo blp e arb dell'ultima figlia
115100110620     c
115200110620     c   30              if        arblnp=arblna and arbcca='1'
115300110620     c                   movel     'P'           wtibol
115400110620     c                   endif
115500110620     c
115600020821    3c                   Endif
115700981218    2c                   ENDIF
115800981218      *
115900981218      * Se non trovata in partenza o bolla locale cerco in arrivo
116000020821    2c                   IF        Wtibol = *blanks
116100981218     c     kspe          chain     fnarb01l                           30
116200981218    3c                   If        *in30 = *off
116300981218     c     arblna        lookup    l6                                     30
116400981218     c   30              movel     'A'           wtibol
116500110329     c   30              if        arblnp=arblna and arbcca='1'
116600110329     c                   movel     'P'           wtibol
116700020821    3c                   Endif
116800110329     c
116900110329    3c                   Endif
117000020821    2c                   ENDIF
117100981218      *
117200981218      * Se non trovata errore
117300020821    2c                   IF        Wtibol <> *blanks
117400981218     c                   z-add     arblna        wlnab
117500020821    2c                   ELSE
117600981218     c                   seton                                        489028
117700981218     c                   movel     msg(2)        v1cmsg
117800981218     c                   goto      ecd01_A
117900020821    2c                   ENDIF
118000980521      *
118100980521      * non è possibile aprire c.a. su bolle di recupero
118200980521     c     arbcbo        lookup    s3a                                    30
118300980521     c                   IF        *in30 = *on
118400980522     c                   movel     msg(10)       v1cmsg
118500980521     c                   seton                                        489028
118600980521     C                   GOTO      ecd01_A
118700980521     c                   endif
118800980521      *
118900980521     c     ecd01_A       ENDSR
119000980608      *****************************************************************
119100980608      * CARICAMENTO SUBFILE VIDEO 2
119200980608      *****************************************************************
119300990315     C     RIED02        BEGSR
119400980527      *
119500980527      * Pulisco subfile
119600990315     c                   IF        *IN23 = *off
119700990315     C                   eval         *IN21 = *OFF
119800980527     C                   WRITE     FI00C02
119900990315     C                   eval         *IN20 = *ON
120000990315     C                   eval         *IN21 = *ON
120100990315     c                   ENDIF
120200990315      *
120300990312     c                   clear                   Wendpag
120400990312     c                   clear                   Wcapagina
120500990420     c                   z-add     NUMcacar      NRR1
120600980526      *
120700980526     c                   SELECT
120800980527      * numero C.A.
120900980622     C                   WHEN      SEL(2) = '1'
121000990315     C                   exsr      RIESEL2
121100980527      * spedizione
121200980622     C                   WHEN      SEL(3) = '1'
121300980622     C                   exsr      RIESEL3
121400041110      * numero e data assegno
121500041026     C                   WHEN      SEL(14) = '1'
121600041026     C                   exsr      RIESEL14
121700990316      * mittente
121800990316     C                   WHEN      SEL(12) = '1'
121900990316     C                   exsr      RIESEL12
122000990316      * destinatario
122100990316     C                   WHEN      SEL(13) = '1'
122200990316     C                   exsr      RIESEL13
122300980608      * numero evento
122400980622     C                   WHEN      SEL(5) = '1'
122500980622     C                   exsr      RIESEL5
122600990316      * numero pratica
122700990316     C                   WHEN      SEL(11) = '1'
122800990316     C                   exsr      RIESEL11
122900980526      * data C.A.
123000980622     C                   WHEN      SEL(6) = '1'
123100980622     C                   exsr      RIESEL6
123200980526      *
123300980526     C                   OTHER
123400980622     C                   exsr      RIESEL
123500980526      *
123600980526     C                   ENDSL
123700980527      *
123800990121     c                   z-add     1             V2Cnrr
123900990420     c                   z-add     NRR1          NUMcacar
124000990121      *
124100980526     c                   ENDSR
124200980527      *****************************************************************
124300980622      * CARICAMENTO SUBFILE * SELEZIONE 2 - numero C. A.
124400980527      *****************************************************************
124500980622     C     RIESEL2       BEGSR
124600980528      *
124700990315      * E' possibile una sola C.A. valida, ROLLUP non valido
124800990315      * Se la CA è annullata e non sono in Apertura messaggio
124900990315     C                   IF        *in23 = *off
125000990315      *
125100980527     c                   z-add     V1Caac        Kaac
125200980527     c                   z-add     V1Cfil        Kfil
125300980527     c                   z-add     V1Cnca        Knca
125400990315     C     kdct          CHAIN     fndct000                           24
125500990115      *
125600990312     c                   SELECT
125700990312      *
125800990315     c                   WHEN      *IN24 = *ON
125900990312      *
126000990312     C                   WHEN      AZN_tipo <> 'A'  and  DCTatb <> *blanks
126100990115     c                   movel     msg(35)       v1cmsg
126200990115     c                   eval      *in28 = *on
126300990312      *
126400990312     C                   WHEN      DCTatb <> *blanks
126500980527      *
126600990312     c                   OTHER
126700980527     C                   exsr      CTR_selez
126800990312      *
126900990312     c                   ENDSL
127000990315      *
127100990315     c                   ENDIF
127200980527      *
127300980527     c                   ENDSR
127400980527      *****************************************************************
127500980622      * CARICAMENTO SUBFILE * SELEZIONE 3 - spedizione
127600980527      *****************************************************************
127700980622     C     RIESEL3       BEGSR
127800980527      *
127900040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
128000040714      * SIA COME MAMMA CHE COME FIGLIA
128100040714      *
128200040714     c                   IF        *IN23 = *OFF
128300040714     c                   clear                   fidn12ds
128400040714     c                   eval      i12aas = V1Caas
128500040714     c                   eval      i12lnp = V1Clnp
128600040714     c                   eval      i12nrs = V1Cnrs
128700040714     c                   eval      i12nsp = V1Cnsp
128800040714     c                   eval      i12fel = 'E'
128900040714      *
129000040714     c                   call      'FIDN12R'
129100040714     c                   parm                    fidn12ds
129200040714      *
129300040714     c                   z-add     *zeros        ii                2 0
129400040714     c                   ENDIF
129500040714      * se trovata almeno una CA
129600040714     c                   if        o12nca > 0
129700040714     c                   dow       ii <  o12nca  and Wendpag <> 'S'
129800040714     c                   add       1             ii
129900040714     c                   movea     skkey(ii)     dskey
130000040714     C                   z-add     dsaac         kaac
130100040714     C                   z-add     dsfil         kfil
130200040714     C                   z-add     dsnca         knca
130300040714     c     kdct          chain     FNDCT000
130400040714     c                   If        %found(fndct01l)
130500040714     C                   exsr      CTR_selez
130600040714      * se raggiunto il mimite massimo di record: esco.
130700040714     C                   if        NRR1 >= w_nrrMAX
130800040714     C                   leave
130900040714     C                   endif
131000040714     C                   endif
131100040714      *
131200040714     c                   enddo
131300040714     c                   endif
131400040714      *
131500980527     c                   ENDSR
131600980527      *****************************************************************
131700980622      * CARICAMENTO SUBFILE * SELEZIONE 5 - numero evento
131800980527      *****************************************************************
131900980622     C     RIESEL5       BEGSR
132000980527      *
132100990315     c                   IF        *IN23 = *OFF
132200990315     C     keve          chain     fndct003                           24
132300990315     c                   ENDIF
132400980527      *
132500990315     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
132600980527     C                   exsr      CTR_selez
132700020222      * se raggiunto il mimite massimo di record: esco.
132800020222     C                   if        NRR1 >= w_nrrMAX
132900020222     C                   leave
133000020222     C                   endif
133100990315     C     keve          reade     fndct003                               24
133200980527     C                   ENDDO
133300980527      *
133400980527     c                   ENDSR
133500980527      *****************************************************************
133600980622      * CARICAMENTO SUBFILE * SELEZIONE 6 - data apertura C.A.
133700980527      *****************************************************************
133800980622     C     RIESEL6       BEGSR
133900980527      *
134000990315     c                   IF        *IN23 = *OFF
134100980527     c                   z-add     W_AAdcad      Kaac
134200990315     c                   z-add     W_MGdcad      Kmgc
134300990315     c                   clear                   Kfil
134400990315     c                   clear                   Knca
134500980527      *
134600050217     C   08kdct4         setll     fndct004
134700050217     C  n08kdct4         setll     fndct024
134800050217     C   08              read      fndct004                               24
134900050217     C  n08              read      fndct024                               24
135000990315     c                   ENDIF
135100980527      *
135200990315     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
135300980527     C                   exsr      CTR_selez
135400980528      * Se supero data limite fine lettura
135500990315     C                   If        wdatac > W_V1Cdcaa
135600990315     C                   eval      *IN24 = *on
135700990315     c                   Else
135800020222      * se raggiunto il mimite massimo di record: esco.
135900020222     C                   if        NRR1 >= w_nrrMAX
136000020222     C                   leave
136100020222     C                   endif
136200050217     C   08              read      fndct004                               24
136300050217     C  n08              read      fndct024                               24
136400990315     c                   Endif
136500980527     C                   ENDDO
136600980527      *
136700980527     c                   ENDSR
136800990323      *****************************************************************
136900990323      * CARICAMENTO SUBFILE * SELEZIONE 11 - numero pratica assicurativa
137000990323      *****************************************************************
137100990323     C     RIESEL11      BEGSR
137200990323      *
137300990323     c                   IF        *IN23 = *OFF
137400990323     C     kpra          chain     fndct010                           24
137500990323     c                   ENDIF
137600990323      *
137700990323     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
137800990323     C                   exsr      CTR_selez
137900020222      * se raggiunto il mimite massimo di record: esco.
138000020222     C                   if        NRR1 >= w_nrrMAX
138100020222     C                   leave
138200020222     C                   endif
138300990323     C     kpra          reade     fndct010                               24
138400990323     C                   ENDDO
138500990323      *
138600990323     c                   ENDSR
138700990323      *****************************************************************
138800990323      * CARICAMENTO SUBFILE * SELEZIONE 12 - mittente
138900990323      *****************************************************************
139000990323     C     RIESEL12      BEGSR
139100990323      *
139200990323     c                   IF        *IN23 = *OFF
139300990324     c                   movel(P)  V1Cmit        W035a
139400990324     c     W035A         setll     tntmdMIT
139500990324     C                   read      tntmdMIT                               24
139600990323     c                   eval      *in25 = *on
139700990324     c                   ENDIF
139800990323      *
139900990324     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'  and
140000990324     c                             %subst(V1Cmit:1:Wlm) = %subst(£TMDrsm:1:Wlm)
140100990323      * leggo le C.A. per numero spedizione
140200990323      * se esco dalla lettura CA x fine pagina ma ho ancora CA per la spedizione
140300990323      * non devo leggere TNTMD e se fanno rollup non devo chainare FNDCT
140400040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
140500040714      * SIA COME MAMMA CHE COME FIGLIA
140600040714      *
140700040714     C                   IF        *IN25 = *ON
140800040714     c                   clear                   fidn12ds
140900040714      *
141000040714     c                   eval      i12aas = £TMDaas
141100040714     c                   eval      i12lnp = £TMDlnp
141200040714     c                   eval      i12nrs = £TMDnrs
141300040714     c                   eval      i12nsp = £TMDnsp
141400040714     c                   eval      i12fel = 'E'
141500040714      *
141600040714     c                   call      'FIDN12R'
141700040714     c                   parm                    fidn12ds
141800040714      *
141900040714     c                   z-add     *zeros        ii                2 0
142000040714     c                   ENDIF
142100040714      * se trovata almeno una CA
142200040714     c                   if        o12nca > 0
142300040714     c                   dow       ii <  o12nca  and Wendpag <> 'S'
142400040714     c                   add       1             ii
142500040714     c                   movea     skkey(ii)     dskey
142600040714     C                   z-add     dsaac         kaac
142700040714     C                   z-add     dsfil         kfil
142800040714     C                   z-add     dsnca         knca
142900040714     c     kdct          chain     FNDCT000
143000040714     c                   If        %found(fndct01l)
143100040714     C                   exsr      CTR_selez
143200040714      * se raggiunto il mimite massimo di record: esco.
143300040714     C                   if        NRR1 >= w_nrrMAX
143400040714     C                   leave
143500040714     C                   endif
143600040714     C                   endif
143700040714      *
143800040714     c                   enddo
143900040714     c                   endif
144000040714      * SE HO LETTO TUTTE LE CA IN SCHIERA ACCENDO IL 25
144100040714      *
144200040714     C                   EVAL      *IN25 = (II = O12NCA)
144300990323      *
144400990323     c                   IF        *IN25 = *ON
144500990324     C                   read      tntmdMIT                               24
144600990323     c                   ENDIF
144700990323      *
144800990323     C                   ENDDO
144900990323      *
145000990323     c                   ENDSR
145100990323      *****************************************************************
145200990323      * CARICAMENTO SUBFILE * SELEZIONE 13 - destinatario
145300990323      *****************************************************************
145400990323     C     RIESEL13      BEGSR
145500990323      *
145600990323     c                   IF        *IN23 = *OFF
145700990324     c                   movel(P)  V1Cdes        W035a
145800990324     c     W035A         setll     tntmdDES
145900990324     C                   read      tntmdDES                               24
146000990323     c                   eval      *in25 = *on
146100990323     c                   ENDIF
146200990323      *
146300990324     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'  and
146400990324     c                             %subst(V1Cdes:1:Wld) = %subst(£TMDrsd:1:Wld)
146500990323      * leggo le C.A. per numero spedizione
146600990323      * se esco dalla lettura CA x fine pagina ma ho ancora CA per la spedizione
146700990323      * non devo leggere TNTMD e se fanno rollup non devo chainare FNDCT
146800040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
146900040714      * SIA COME MAMMA CHE COME FIGLIA
147000040714      *
147100040714     C                   IF        *IN25 = *ON
147200040714     c                   clear                   fidn12ds
147300040714      *
147400040714     c                   eval      i12aas = £TMDaas
147500040714     c                   eval      i12lnp = £TMDlnp
147600040714     c                   eval      i12nrs = £TMDnrs
147700040714     c                   eval      i12nsp = £TMDnsp
147800040714     c                   eval      i12fel = 'E'
147900040714      *
148000040714     c                   call      'FIDN12R'
148100040714     c                   parm                    fidn12ds
148200040714      *
148300040714     c                   z-add     *zeros        ii                2 0
148400040714     c                   ENDIF
148500040714      * se trovata almeno una CA
148600040714     c                   if        o12nca > 0
148700040714     c                   dow       ii <  o12nca  and Wendpag <> 'S'
148800040714     c                   add       1             ii
148900040714     c                   movea     skkey(ii)     dskey
149000040714     C                   z-add     dsaac         kaac
149100040714     C                   z-add     dsfil         kfil
149200040714     C                   z-add     dsnca         knca
149300040714     c     kdct          chain     FNDCT000
149400040714     c                   If        %found(fndct01l)
149500040714     C                   exsr      CTR_selez
149600040714      * se raggiunto il mimite massimo di record: esco.
149700040714     C                   if        NRR1 >= w_nrrMAX
149800040714     C                   leave
149900040714     C                   endif
150000040714     C                   endif
150100040714      *
150200040714     c                   enddo
150300040714     c                   endif
150400040714      * SE HO LETTO TUTTE LE CA IN SCHIERA ACCENDO IL 25
150500040714      *
150600040714     C                   EVAL      *IN25 = (II = O12NCA)
150700990323      *
150800990323     c                   IF        *IN25 = *ON
150900990324     C                   read      tntmdDES                               24
151000990323     c                   ENDIF
151100990323      *
151200990323     C                   ENDDO
151300990323      *
151400990323     c                   ENDSR
151500041026      *****************************************************************
151600041026      * CARICAMENTO SUBFILE * SELEZIONE 14 - data + numero assegno
151700041026      *****************************************************************
151800041026     c     RIESEL14      BEGSR
151900041026      *
152000041026     c                   IF        *IN23 = *OFF
152100041110     c                   if        V1Cdea > *zeros
152200041026     C     Kdcl02        chain     fndcl000                           24
152300041026     c                   else
152400041026     C     Kdcl01        chain     fndcl000                           24
152500041026     C                   endif
152600041026     c                   ENDIF
152700041026      *
152800041026     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
152900041026      *
153000041026     C                   z-add     dclaac        kaac
153100041026     C                   z-add     dclfil        kfil
153200041026     C                   z-add     dclnca        knca
153300041026     c     kdct          chain     FNDCT000                           25
153400150701      *
153500150701      /free
153600150701           // -?Eventuale reperimento del Beneficiario del bonifico?
153700150701           If  V1Cksc <> *zero  or  V1Dksc <> *blank;
153800150701             chain  ( DCLaac : DCLfil : DCLnca : 'P' )  FNDKA000;
153900150701             if  NOT %found(FNDKA01L)  or  DKAatb <> *blank;
154000150701               clear  DKAksc;
154100150701               clear  DKArag;
154200150701             endif;
154300150701           EndIf;
154400150701      /end-free
154500041026      *
154600041026     C                   exsr      CTR_selez
154700041026      * se raggiunto il mimite massimo di record: esco.
154800041026     C                   if        NRR1 >= w_nrrMAX
154900041026     C                   leave
155000041026     C                   endif
155100041026      *
155200041110     c                   if        V1Cdea > *zeros
155300041026     C     Kdcl02        reade     fndcl000                               24
155400041026     c                   else
155500041026     C     Kdcl01        reade     fndcl000                               24
155600041026     C                   endif
155700041026      *
155800041026     C                   ENDDO
155900041026      *
156000041026     c                   ENDSR
156100980527      *****************************************************************
156200980622      * CARICAMENTO SUBFILE * SELEZIONE ? -
156300980527      *****************************************************************
156400980622     C     RIESEL        BEGSR
156500980527      *
156600990315     c                   IF        *IN23 = *OFF
156700050217     C   08*LOVAL        setll     fndct004
156800050217     C  n08*LOVAL        setll     fndct024
156900050217     C   08              read      fndct004                               24
157000050217     C  n08              read      fndct024                               24
157100990315     c                   ENDIF
157200980527      *
157300990315     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
157400980527     C                   exsr      CTR_selez
157500020222      * se raggiunto il mimite massimo di record: esco.
157600020222     C                   if        NRR1 >= w_nrrMAX
157700020222     C                   leave
157800020222     C                   endif
157900050217     C   08              read      fndct004                               24
158000050217     C  n08              read      fndct024                               24
158100980527     C                   ENDDO
158200980527      *
158300980527     c                   ENDSR
158400980527      *****************************************************************
158500980527      * CONTROLLO SE C.A. SELEZIONABILE PER CARICAMENTO SUBFILE
158600980527      *****************************************************************
158700980527     C     CTR_selez     BEGSR
158800980528      *
158900990215     c                   movel(P)  DCTflo        DDCT01
159000990215     c                   movel(P)  §DCTlnpc      W§DCTlnpc
159100990215      *
159200990215      * Compongo la data creazione evento per confronto
159300990215     C                   eval      wdatac = (DCTaac * 10000) + DCTmgc
159400020821      *
159500020821      * Cerco in L6 il P.O. di competenza se interrogazione o
159600020821      *   manutenzione fase
159700020821     c                   eval      W_POinte = 'N'
159800020821     c                   if        AZN_tipo = 'I' or AZN_tipo = 'M'
159900020821     c                   if        V1Cfgs = 046
160000020821     c                   eval      W_POinte = 'S'
160100020821     c                   else
160200020821     c     DCTlna        lookup    L6                                     31
160300020821     c  N31w§DCTlnpc     lookup    L6                                     31
160400020821     c   31              eval      W_POinte = 'S'
160500020821     c                   endif
160600020821     c                   endif
160700980528      *
160800981014      * Cerco in L6 il P.O. gestione fase
160900980622     c                   eval      W_POgest = 'N'
161000981014     C                   if        DCTgfc > *zeros
161100980528     C     DCTgfc        LOOKUP    L6                                     31
161200980622     c   31              eval      W_POgest = 'S'
161300980528     C                   endif
161400980528      *
161500981014      * Cerco in L6 il P.O creazione fase per Apertura
161600980622     c                   eval      W_POcrea = 'N'
161700980622     C                   if        AZN_tipo = 'A'
161800980528     C     DCTfil        LOOKUP    L6                                     32
161900980622     c   32              eval      W_POcrea = 'S'
162000980528     C                   endif
162100980608      *
162200980608      * Cerco nel file fasi se selezionata la data esecuzione fase
162300980622     c                   eval      W_esefase = 'N'
162400980622     C                   if        SEL(9) = '1'
162500980608      *
162600980608     c                   z-add     dctaac        kaac
162700980608     c                   z-add     dctfil        kfil
162800980608     c                   z-add     dctnca        knca
162900980608     c     KDCF          SETLL     FNDCF000
163000980608      *
163100980608     c                   DO        *HIVAL
163200980608     c     KDCF          reade     FNDCF000                               33
163300980608     c  N33              if        DCFfca = W_V1Cfca  AND
163400980608     c                             DCFdfc <= W_v1cdefa  AND  DCFdfc >= W_v1cdefd
163500980622     c                   eval      W_esefase = 'S'
163600980608     c                   else
163700980608     c                   eval      *IN33 = (DCFdfc > W_v1cdefa)
163800980608     c                   endif
163900980608     c  N33              ENDDO
164000980608      *
164100980608     C                   endif
164200990215      *
164300990215      * Cerco lnp o lna nella £6 del P.O selezione
164400990215     c                   eval      W_POsele = 'N'
164500990215     C                   IF           SEL(10) = '1'
164600990215      *
164700990215     c                   If         V1Cpot = *blanks  or  V1Cpot = 'A'
164800990215     C     DCTlna        LOOKUP    WL6                                    32
164900990215     c   32              eval      W_POsele = 'S'
165000990215     C                   Endif
165100990215      *
165200990215     c                   If         V1Cpot = *blanks  or  V1Cpot = 'P'
165300990215     C     W§DCTlnpc     lookup    WL6                                    32
165400990215     c   32              eval      W_POsele = 'S'
165500990215     C                   Endif
165600990215      *
165700990215     C                   ENDIF
165800990215
165900980528      *  -------------------------------------------------
166000980528      *  C O N T R O L L I   E X T R A   S E L E Z I O N E
166100990215      *  -------------------------------------------------
166200990115      * - Elaboro le annullate solo se il tipo azione è "A"
166300980710      * - Se il tipo azione è "G" non carico C.A. chiuse
166400980710      * - Se il tipo azione è "G" la filiale gestione fase deve essere in £6
166500020821      * - Se il tipo azione è "I" o "M" DCTLNA o §DCTLNPC devono essere in £6
166600980803      * - Se il tipo azione è "A" la manutenzione è consentita al P.O. creazione se CA non trasmessa
166700980528      *  -------------------------------------------------
166800990215
166900980527     c                   SELECT
167000980528      *
167100980608     C                   WHEN      AZN_tipo <> 'A'  AND  DCTatb <> *blanks        ANNULLATE
167200980528      *
167300980908     C                   WHEN      AZN_tipo = 'G'  AND  DCTdch <> *zeros          C.A. CHIUSA
167400980710      *
167500980710     C                   WHEN      AZN_tipo = 'G'  AND  W_POgest = 'N'            FASE GESTIBILE
167600020821      *
167700020821     C                   WHEN      (AZN_tipo = 'I'  or  AZN_tipo = 'M')           Fase gestibile
167800020821     C                             and W_POinte = 'N'
167900990507      *
168000120822     C********           WHEN      AZN_tipo = 'I'  AND  DCTfca < §STDfrc          RICERCA DA FASE nn
168100120822     c*********                                    AND  *IN16 = *ON               PER CONSULDANNI
168200980528      *
168300980528     C                   WHEN      AZN_tipo = 'A'  AND
168400980622     c                              (W_POcrea = 'N' OR  (W_POcrea = 'S'           MAN. P.O. APERTURA
168500050217     C**!!!                          AND  (DCTDT1 > 0)) )                         SE NON TRASMESSA
168600050217     c                               and  (dateu > wdatac)) )                     SE NON APERTA OGGI
168700050217
168800050217     c                   WHEN      Not *In08 and dctdt2 > *zeros                  C.A. X FILIALE
168900990507      *
169000990507     C                   WHEN      SEL(6) = '1'  AND                              DATA C.A.
169100990507     C                              (Wdatac < W_V1Cdcad  OR  Wdatac > W_V1Cdcaa)
169200990507      *
169300990507     C                   WHEN      SEL(8) = '1'  AND  SEL(9) <> '1'  AND          FASE C.A. SENZA
169400990507     C                              AZN_tipo <> 'G'  AND  DCTfca <> W_V1Cfca       DATA C.A.
169500990507      *
169600990507     C                   WHEN      SEL(8) = '1'  AND  SEL(9) <> '1'  AND          FASE C.A. SENZA
169700990507     C                              AZN_tipo = 'G'  AND  DCTfca >= W_V1Cfca        DATA C.A. x GEST.
169800990507      *
169900990507     C                   WHEN      SEL(9) = '1'  AND                              DATA ELABOR. C.A.
170000990507     C                              W_esefase = 'N'
170100990507      *
170200990507     C                   WHEN      SEL(5) = '1'  AND                              NUMERO EVENTO
170300990507     C                              (V1Cnev <> DCTnev  OR  V1Caae <> DCTaae)
170400990507      *
170500990507     C                   WHEN      SEL(11) = '1'  AND                             NUMERO PRATICA ASS
170600990507     C                              (V1Cdit <> DCTdit  OR  V1Cpra <> DCTpra
170700990507     C                                                 OR  V1Cprn <> DCTprn)
170800990507      *
170900990507     C                   WHEN      SEL(10) = '1'  AND                             P.O. Part. Arrivo
171000990507     c                              W_POsele = 'N'
171100041026      *
171200041026     C                   WHEN      SEL(14) <> '1'  AND                            MITTEN./DESTINAT.
171300041026     C                             SEL(12) = '1'  AND  SEL(13) = '1'  AND         MITTEN./DESTINAT.
171400990507     c                             %subst(V1Cdes:1:Wld) <> %subst(£TMDrsd:1:Wld)
171500150701      *
171600150701     c                   WHEN      Sel(14) = '1'   and  (                         BENEFICIARIO
171700150701     c                             (V1Cksc <> *zeros  and  V1Cksc <> DKAksc) or   (codice o
171800150701     c                             (V1Dksc <> *blanks and  %trimR(V1Dksc) <>       ragione sociale)
171900150701     c                               %subst(DKArag:1:%len(%trimR(V1Dksc)))) )
172000980527      *
172100980622     C                   WHEN      SEL(4) = '1'  AND                              CODICE ANOMALIA
172200980527     C                              V1Ctad <> DCTtad
172300980527      *
172400980622     C                   WHEN      SEL(7) = '1'  AND                              LIQ.TRA./PRAT.ASS.
172500980527     C                              V1Cfpr <> DCTfpr
172600080718      *
172700080718     C                   WHEN      SEL(15)= '1'  AND                              C.A.APERTE/CHIUSE
172800080718     C                              ((V1Cace = 'A' and dctcch <> ' ')
172900080718     C                                           OR
173000080718     C                               (V1Cace = 'C' and dctcch =  ' '))
173100990323      *
173200980527     c                   OTHER
173300980527     C                   exsr      RICsbfl
173400980527      *
173500980527     C                   ENDSL
173600980527      *
173700980527     c                   ENDSR
173800980527      **********************************************************************
173900980528      * SCRITTURA RCD SBFL
174000980527      **********************************************************************
174100980528     C     RICsbfl       BEGSR
174200980527      *
174300981008     c                   clear                   wi05fca
174400981008     c                   clear                   w0010
174500980529      *
174600980529     c                   DO        *HIVAL
174700980608      *
174800980610     c                   eval      *IN81 = *OFF
174900980623     c                   eval      *IN82 = *OFF
175000980610     c                   eval      *IN32 = *ON
175100980608     c                   clear                   FIDN05DS
175200980721
175300980608      * Interrogazione o Apertura controllo la fase attuale, Gestione ricerco la fase successiva
175400980608     c                   IF        AZN_tipo <> 'G'
175500980608     c                   movel     'C'           I05MOD
175600980608     c                   ELSE
175700980608      *
175800980605     c                   movel     'F'           I05MOD
175900980529     c                   movel     DCTfpr        I05fpr
176000980529     c                   movel     DCTptr        I05tpc
176100981019     c                   if        DCTnev > *ZEROS
176200980608     c                   movel     'E'           I05fpe
176300980608     c                   else
176400980608     c                   movel     'N'           I05fpe
176500980608     c                   endif
176600981030     c                   if        DCTnde > *ZEROS
176700981030     c                   movel     'D'           I05fde
176800981102     c                   else
176900981102     c                   movel     'N'           I05fde
177000981030     c                   endif
177100980529     c                   ENDIF
177200980529      *
177300980601      * Se esiste fase memorizzata utilizzo quella per la ricerca, altrimenti quella del file.
177400980529     c                   IF        WI05fca > *zeros
177500980529     c                   z-add     wi05fca       I05fca
177600980529     c                   ELSE
177700980529     c                   z-add     DCTfca        I05fca
177800980721      * per fase esistente recupero la data di esecuzione fase
177900990420     c                   z-add     DCTaac        Kaac
178000980721     c                   z-add     DCTfil        Kfil
178100980721     c                   z-add     DCTnca        Knca
178200980721     c     kdcf          setgt     fndcf01l
178300980721     c     kdcf          readpe    fndcf01l                               30
178400980721     c                   if        *in30 = *off and dcffca = dctfca
178500980721     c                   z-add     DCFdfc        i05dta
178600980721     c                   endif
178700980529     c                   ENDIF
178800980721      * valorizzo il tipo anomalia CA
178900980721     c                   move      DCTtad        i05tad
179000981125      * valorizzo il numero C.A.
179100981125     c                   z-add     dctaac        i05aac
179200981125     c                   z-add     dctfil        i05fil
179300981125     c                   z-add     dctnca        i05nca
179400980529      *
179500980529     C                   CALL      'FIDN05R'
179600980604     c                   PARM                    KPJBA
179700980529     c                   PARM                    FIDN05DS
179800980529      *
179900980617      * Errore ricerca vado a fine
180000980529     c                   IF        O05err <> 'E'
180100980529     c                   movel     O05REC        DS_FNDFA
180200980601     c                   ELSE
180300980605     c                   iter
180400980529     C                   ENDIF
180500980721
180600980608      * Per Gestione effettuo controlli sulla fase ricavata
180700980608     c                   IF        AZN_tipo = 'G'
180800981008      * A)  Elaboro la lettera reso avaria e il suo sollecito solo se la merce danneggiata è resa
180900980721      *      Non carico il subfile e ripeto la ricerca.
181000980721     c                   if        DCTdsd <> 'R'  and
181100981008     c                             (DFAfca = WFlav  or  DFAfca = WFsla)
181200980608     c                   eval      *IN32 = *OFF
181300980805     c                   z-add     DFAfca        WI05fca
181400980721     c                   iter
181500980805     C                   ENDIF
181600980805      * B)  Se non è obbligatoria
181700980805      *      Carico il subfile e ripeto la ricerca.
181800980805     c                   if        DFAobl <> 'S'
181900980805     c                   eval      *IN32 = *OFF
182000980805     c                   z-add     DFAfca        WI05fca
182100980805     C                   ENDIF
182200980805      * C)  Se non è stata selezionata una fase e non è utilizzabile in gestione.
182300980805      *      Non carico il subfile e ripeto la ricerca.
182400980805     c                   IF        SEL(8) <> '1'  and  DFAfgc = 'N'
182500980805     c                   eval      *IN32 = *OFF
182600980805     c                   z-add     DFAfca        WI05fca
182700980608     c                   iter
182800980805     C                   ENDIF
182900980805      * D)  Se è diversa dall'eventuale fase selezionata.
183000980805      *      Non carico il subfile e ripeto la ricerca se minore e non obbligatoria
183100980805     c                   IF        SEL(8) = '1'  and  DFAfca <> W_V1Cfca
183200980805      *
183300980805     c                   if        DFAfca < W_V1Cfca  and  DFAobl <> 'S'
183400980805     c                   eval      *IN32 = *OFF
183500980805     c                   z-add     DFAfca        WI05fca
183600980805     C                   else
183700980805     c                   eval      *IN32 = *ON
183800980805     C                   endif
183900980805      *
184000980805     c                   iter
184100980805     C                   ENDIF
184200980805      * E)  Se è uguale all'eventuale fase selezionata
184300980805      *      Carico il subfile e non ripeto la ricerca.
184400980622     c                   IF        SEL(8) = '1'  and  W_V1Cfca = DFAfca
184500980608     c                   eval      *IN32 = *ON
184600980805     C                   ENDIF
184700981126      * F)  Se la fase ricavata non è eseguibile proteggo il campo scelta tranne se si tratta della
184800981126      *      conferma C.A. senza entrata diretta (scelte 1,2,3)
184900981126     c                   SELECT
185000981126     c                   WHEN      O05ese <> 'N'
185100981126     c                   WHEN      DFAfca = WFcon  AND
185200020821     c                             (SEL(2) <> *blanks  or  SEL(3) <> *blanks)
185300981126     c                   OTHER
185400981126     c                   eval      *IN82 = *ON
185500981126     c                   ENDSL
185600981126      * G)  Se la fase ricavata è gestita bloccata
185700981027      *      Visualizzazione richiesta:     proteggo il campo della scelta
185800981027      *      Visualizzazione non richiesta: non carico il subfile e ripeto la ricerca solo se è
185900981027      *                                     facoltativa
186000980925     C                   IF        DFAfgc = 'B'
186100980925      *
186200980925     C                   IF        V1Cscc = 'S'
186300981126     c                   eval      *IN82 = *ON
186400980925     c                   ELSE
186500981027     c                   eval      *IN32 = (DFAobl = 'S')
186600980925     c                   z-add     DFAfca        WI05fca
186700980925     c                   iter
186800980925     c                   ENDIF
186900980925      *
187000980925     c                   ENDIF
187100020128      *
187200120822      * H)  Se l'utente è CONSULDANNI  (Non esiste + )
187300020128      *     &  la fase è 700=Progetto di liquidazione
187400020128      *        con il flag di franchigia = 'F'
187500020128      *      Non carico il subfile e ripeto la ricerca.
187600120822     c****               if        KNMUS    = §STDpco   and
187700120822     c****                         DFAfca   = WFspl     and
187800120822     c****                         §DCTpfra = 'F'
187900120822     c****               eval      *IN32 = *OFF
188000120822     c****               z-add     DFAfca        WI05fca
188100120822     c****               iter
188200120822     c****               endif
188300020128      *
188400020128      * I)  Se l'utente non è né CONSULDANNI né EDPxx
188500020128      *     &  la fase è 700=Progetto di liquidazione
188600020128      *        con il flag di franchigia <> 'F'
188700020128      *      Non carico il subfile e ripeto la ricerca.
188800050208      * =>  NON PIU'!!!   Ora si carica !
188900050208      *     (vedi progetto 560 - Danni pagamenti Bartolini)
189000050208     ***c                   if        KNMUS             <> §STDpco   and
189100050208     ***c                             %subst(KNMUS:1:3) <> 'EDP'     and
189200050208     ***c                             AZN_tipo          =  'G'       and
189300050208     ***c                             *in13             =  *on       and
189400050208     ***c                             §DCTpfra          <> 'F'       and
189500050208     ***c                             W_V1Cfca          =  WFspl
189600050208     ***c                   eval      *IN32 = *OFF
189700050208     ***c                   z-add     DFAfca        WI05fca
189800050208     ***c                   iter
189900050208     ***c                   endif
190000080603      *
190100080603      * I)  Se la fase deve essere dalla sede e il po gestione non è la sede
190200080603      *     passo oltre
190300080603      *
190400080603     c                   if        dfagfs = 'S' and dctgfc <> 46
190500080603     c                   iter
190600080603     c                   endif
190700980721      *
190800980721      * Fine controlli Gestione
190900980721     C                   ENDIF
191000980529      *
191100980608      * Carico
191200980622     c                   movel     *blanks       V2Csce
191300980623     c                   movel     *blanks       V2Hi81
191400980623     c                   movel     *blanks       V2Hi82
191500160226     c                   movel     *blanks       V2Hi86
191600980721     c   82              movel     '1'           V2Hi82
191700980702     c                   movel     W_pogest      V2Hges
191800980908     c                   eval      V2Hchi = (DCTdch > 0)
191900980731     c                   movel     DFAcog        V2Hcog
192000980529     c                   z-add     DCTaac        V2Haac
192100980529     c                   z-add     DCTaas        V2Haas
192200980604     c                   z-add     DCTlna        V2Hlna
192300980610     c                   movel     DFApgm        V2Hpgm
192400980529     c                   z-add     DCTfil        V2Cfil
192500980601     c                   z-add     DCTnca        V2Cnca
192600980529     c                   z-add     DCTaac        V2Cdca
192700980529     c                   movel     DCTmgc        V2Cdca
192800980605     c     *mdy          move      V2Cdca        DATA_eur
192900980605     c     *dmy          move      DATA_eur      V2Cdca
193000980529     c                   move      DCTaas        V2caas2
193100980529     c                   z-add     DCTlnp        V2Clnp
193200980529     c                   z-add     DCTnrs        V2Cnrs
193300980529     c                   z-add     DCTnsp        V2Cnsp
193400981204     c                   movel     DCTtad        W_ctad
193500980615     c                   z-add     DFAfca        V2Cfca                         Calcolata
193600980615     c                   z-add     DCTfca        V2Hfca                         Attuale
193700990312     c                   movel     DCTtad        V2Ctad
193800981204     c                   z-add     DCTnev        V2Cnev
193900981204     c                   z-add     DCTaae        V2Caae
194000990312     c                   z-add     DCTpra        V2Cpra
194100990312     c                   z-add     DCTprn        V2Cprn
194200990312     c                   IF        DCTprn > *zeros
194300990312     c                   movel     DCTdit        V2Cdit
194400990312     c                   ELSE
194500990312     c                   clear                   V2Cdit
194600990312     c                   ENDIF
194700040908      *
194800040908     c                   exsr      IMPtpb
194900981204      *
195000981204      * Ricavo ragione sociale beneficiario se cambia C.A.
195100981204     c                   movel     DSnumca       Wdsnumca
195200981204     c                   IF        Wdsnumca <> Mdsnumca
195300981204     c                   exsr      ragsocben
195400981204     c                   z-add     Wdsnumca      Mdsnumca
195500981204     c                   ENDIF
195600981204     c                   movel     W_v2crsb      V2Crsb
195700981204     c                   movel     W_v2htpb      V2Htpb
195800980610      *
195900981204      * Descrizione fase
196000981009     c                   IF        DCTatb <> *blanks
196100981009     c                   movel(P)  Kannullata    V2Dfca
196200981009     c                   ELSE
196300980608     c                   IF        AZN_tipo = 'G'  and  DFADGC <> *blanks
196400980608     c                   movel     DFAdgc        V2Dfca
196500980608     c                   ELSE
196600980608     c                   movel     DFAdes        V2Dfca
196700980608     c                   ENDIF
196800981009     c                   ENDIF
196900980529      *
197000980610      * Gestione di più fasi per C.A.
197100980610     c                   IF        AZN_tipo = 'G'
197200980610     c                   eval      *IN81 = (W0010 > 0)
197300980623     c   81              eval      V2HI81 = '1'
197400980610     c                   eval      w0010 = 1
197500990505     c                   ELSE
197600990505     c                   add       1             Wcapagina
197700980610     c                   ENDIF
197800981209      * costante evento
197900981209     c                   eval      *IN84 = (V2Cnev = *zeros)
198000990312      * costante pratica
198100990312     c                   eval      *IN85 = (V2Cprn = *zeros)
198200160226      *?Beneficiario discordante?
198300160226      *?(Fase da eseguire = 230 e?
198400160226      *?(beneficiario Mittente     su spedizioni in Porto Assegnato o?
198500160226      * ?beneficiario Destinatario su spedizioni in Porto Franco))?
198600160226     c                   movel     DCTflo        dDCT01
198700160226     c                   eval      *in86 = (V1Cfca  = %editc(WFecc:'X') and
198800160226     c                                      *in13   = *on               and
198900160226     c                                     ((§DCTport = 'A' and DCTptr = 'M')
199000160226     c                                       or
199100160226     c                                      (§DCTport = 'F' and DCTptr = 'D')))
199200160226     c   86              movel     *on           V2Hi86
199300981209      * Posizione cursore
199400981104     c                   eval      *IN83 = (*IN82 = *OFF  and  *IN83 = *OFF)
199500981104      *
199600981008     c  N82              add       1             Numcaval
199700990414     c                   add       1             Numcacar
199800981008     c                   add       1             NRR1
199900990315      *
200000990315      * posizione cursore pagina nuova per ROLLUP
200100990315      * carico 16 rcd per volta tranne se gestione
200200990315     c                   SELECT
200300990315     c                   WHEN      AZN_tipo = 'G'
200400990315     c                   WHEN      Wcapagina = 1
200500990315     c                   eval      nrr23 = nrr1
200600990315     c                   WHEN      Wcapagina = 16
200700990312     c                   eval      Wendpag = 'S'
200800990315     c                   ENDSL
200900990312      *
201000980529     c                   write     fi00s02
201100020222      *
201200020222      * Se ho raggiunto il limite di record caricabili (9999)
201300020222      *    emetto il subfile senza completare la lettura
201400020222      *    (pió d'axé an spöl brisa !!!)
201500020222     C                   if        NRR1 >= w_nrrMAX
201600020222     c                   leave
201700020222     C                   endif
201800980529      *
201900980604     c  N32              ENDDO
202000980529      *
202100980528     c     Ericsbfl      ENDSR
202200981204      **********************************************************************
202300990908      * IMPOSTO TIPO BOLLA  Partenza/Arrivo/Storichino/Sede
202400981204      **********************************************************************
202500981204     C     IMPtpb        BEGSR
202600981204      *
202700981204     c                   clear                   W_V2Htpb
202800981204      *
202900981204     c                   IF        SIMFEL = *ZEROS
203000981204     c                   movel     'S'           W_V2Htpb
203100981204     c                   ELSE
203200981204      *
203300040715      * verifico se CA con legami se LNA CA presente in L6 del P.O. gestione
203400040715      *
203500040715     c                   if        §dctlega = 'S'
203600040715     c     dctlna        lookup    L6                                     17
203700040715     c                   If        %found
203800040715      * aggancio FNDCK per il recupero della spedizione figlia
203900040715     c     Kdct          chain     Fndck01l
204000040715     c                   If        %found(Fndck01l)
204100040715      * imposto il tipo bolla uguale ad "A" arrivo
204200040715     c                   movel     'A'           W_V2Htpb
204300040715     c                   eval      V2Haas = DCKaas
204400040715     c                   move      DCKaas        V2caas2
204500040715     c                   eval      V2Clnp = DCKlnp
204600040715     c                   eval      V2Cnrs = DCKnrs
204700040715     c                   eval      V2Cnsp = DCKnsp
204800040715      *
204900040715     c                   z-add     V2Haas        Kaas
205000040715     c                   z-add     V2Clnp        Klnp
205100040715     c                   z-add     V2Cnrs        Knrs
205200040715     c                   z-add     V2Cnsp        Knsp
205300040715      * aggancio
205400040715     c     kspe          chain     fnarb000                           30
205500040715      * se non trovato nelle bolle arrivi vado nelle bolle partenze
205600040715     c                   if        *in30 = *on
205700040715     c     kspe          chain     fnblp000                           30
205800040715      *
205900040715     c                   if        *IN30 = *OFF
206000040715     c                   movel     'P'           W_V2Htpb
206100040715     c                   else
206200040715     c                   clear                   W_v2htpb
206300040716      * Ripristino con il vecchio numero di spedizione
206400040716     c                   eval      V2Haas = DCTaas
206500040716     c                   move      DCTaas        V2caas2
206600040716     c                   eval      V2Clnp = DCTlnp
206700040716     c                   eval      V2Cnrs = DCTnrs
206800040716     c                   eval      V2Cnsp = DCTnsp
206900040715     c                   endif
207000040715      *
207100040715     c                   endif
207200040715      *
207300040715     c                   endif
207400040715     c                   endif
207500040715     c                   ENDIF
207600040715      * SE TIPO BOLLA NON ATTRIBUITO
207700040715     c                   IF        W_V2Htpb = ' '
207800040715     c                   z-add     V2Haas        Kaas
207900981204     c                   z-add     V2Clnp        Klnp
208000981204     c                   z-add     V2Cnrs        Knrs
208100981204     c                   z-add     V2Cnsp        Knsp
208200981204      *
208300981204     c     kspe          chain     fnblp000                           30
208400981204      *
208500981204     c                   if        *IN30 = *OFF
208600981204     c                   movel     'P'           W_V2Htpb
208700981204     C                   else
208800981204     c     kspe          chain     fnarb000                           30
208900981204     c  N30              movel     'A'           W_V2Htpb
209000040715     c                   endif
209100040715      *
209200981204     c                   endif
209300981204      *
209400981204     c                   ENDIF
209500981204      *
209600981204     C                   ENDSR
209700981204      **********************************************************************
209800981204      * RECUPERO LA RAGIONE SOCIALE BENEFICIARIO
209900981204      **********************************************************************
210000981204     C     RAGSOCBEN     BEGSR
210100981204      *
210200981204     c                   clear                   W_V2Crsb
210300990208     c                   MOVE      DCTksc        w0040
210400990420      *
210500990420      * Imposto fase CA, se CA chiusa prendo l'ultima valida
210600990420     c                   IF        DCTfca <> WFcch
210700990420     c                   z-add     DCTFCA        WDCTfca
210800990420     c                   ELSE
210900990420     c                   z-add     DCTaac        Kaac
211000990420     c                   z-add     DCTfil        Kfil
211100990420     c                   z-add     DCTnca        Knca
211200990420     c     kdcf          setgt     fndcf01l
211300990420     c     kdcf          readpe    fndcf01l                               30
211400990420     c                   DOW       *in30 = *off
211500990420     c                   if        DCFfca <> DCTfca
211600990420     c                   z-add     DCFfca        WDCTfca
211700990420     c                   eval      *in30 = *on
211800990420     c                   else
211900990420     c     kdcf          readpe    fndcf01l                               30
212000990420     c                   endif
212100990420     c                   ENDDO
212200990420     c                   ENDIF
212300990420      *
212400990208      * Se ho stampato il progetto di liquidazione uso l'intestatario
212500990420     c                   IF        Wdctfca >= WFspl
212600990208     c                   movel     'I'           wtrc
212700990208     c     Kdka          chain     FNDKA000                           30
212800990909     c                   if        *in30 = *off  and  DKAatb = *blanks
212900990909     c                   movel     DKArag        W_V2Crsb
213000990909     c                   endif
213100990208     c                   ELSE
213200081215      * altrimanti cerco il beneficiario con il programma esterno
213300081215     c                   clear                   fidnbeds
213400081215      *
213500081215     c                   movel     'B'           IBEmod
213600081215     c                   z-add     dctaac        IBEaac
213700081215     c                   z-add     dctfil        IBEfil
213800081215     c                   z-add     dctnca        IBEnca
213900081215     c                   z-add     dctaas        IBEaas
214000081215     c                   z-add     dctlnp        IBElnp
214100081215     c                   z-add     dctnrs        IBEnrs
214200081215     c                   z-add     dctnsp        IBEnsp
214300081215     c                   z-add     dctksc        IBEksc
214400081215     c                   movel     dctptr        IBEptr
214500081215     c                   movel     dctflo        IBEflo
214600081215     c                   movel     W_V2HTPB      IBEtpb
214700081218     c                   If        %subst(knsif:7:1) = 'P'
214800081215     c                   eval      IBEsif = 'P'
214900081218     c                   EndIf
215000081215      *
215100081215     c                   call      'FIDNBER'
215200081215     c                   parm                    fidnbeds
215300081215      *
215400081215     c                   If        oberag <> *blanks  and dctptr <> ' '
215500081215     c                   movel     oberag        W_V2Crsb
215600081215     c                   endif
215700990208      *
215800990208     c                   ENDIF
215900981204      *
216000981204     c                   ENDSR
216100980601      **********************************************************************
216200980601      * EMISSIONE VIDEO 2
216300980601      **********************************************************************
216400980601     C     EMISD02       BEGSR
216500980601      *
216600990121     c                   SELECT
216700990315     c                   WHEN      NRR3 > *zeros
216800990315     c                   z-add     nrr3          V2Cnrr
216900990315     c                   WHEN      *IN23 = *ON  and NRR23 > *zeros
217000990315     c                   z-add     nrr23         V2Cnrr
217100990315     c                   WHEN      *IN90 = *ON  and NRR1 > *zeros
217200990315     c                   z-add     nrr1          V2Cnrr
217300990121     c                   OTHER
217400980610     c                   z-add     1             V2Cnrr
217500990121     c                   ENDSL
217600990121      *
217700980604     c                   seton                                        2021
217800980604      *
217900980601     c                   write     Fi00Z02
218000980601     c                   exfmt     Fi00C02
218100980608      *
218200980624     c                   setoff                                       2890
218300980601     c                   clear                   v2cmsg
218400990121     c                   clear                   nrr3
218500980601      *
218600980601     C                   ENDSR
218700981008      **********************************************************************
218800981008      * CONTROLLI VIDEO 2 SE PREMUTO KA
218900981008      **********************************************************************
219000981008     C     CTRD02KA      BEGSR
219100981008      *
219200981008     c                   clear                   NUMCAESC
219300981008     c                   clear                   W_GIACE
219400981211      *
219500981211     c                   z-add     1             nrr2
219600981211     c     NRR2          CHAIN     FI00S02                            32
219700981211      *
219800981211     c     *IN32         doweq     *OFF
219900981008      *
220000981027      * ERRORE : per scelte diverse dall'esclusione o da blanks,
220100981027      *          le C.A. senza n° spedizione devono obbligatoriamente essere escluse
220200981008      *
220300981027     c                   SELECT
220400981211      *
220500981211     C                   WHEN      V2Cnsp = *zeros  and  V2Csce <> 'E'
220600981211     c                   movel     MSG(34)       V2Cmsg
220700981211     c                   seton                                        289083
220800981211     c                   eval      *IN32 = *ON
220900981211      *
221000981211     C                   WHEN      V2Csce = *blanks
221100981027      *
221200981027     c                   WHEN      V2Csce = 'E'
221300981008     c                   add       1             numcaesc
221400981008      *
221500981211     C                   OTHER
221600981008     c                   movel     MSG(28)       V2Cmsg
221700981008     c                   seton                                        289083
221800981008     c                   eval      *IN32 = *ON
221900981027      *
222000981027     c                   ENDSL
222100981211      *
222200981211     c                   If        *IN32 = *OFF
222300981211     c                   add       1             nrr2
222400981211     c     NRR2          CHAIN     FI00S02                            32
222500981211     c                   EndIf
222600981008      *
222700981008     c                   ENDDO
222800981008      *
222900981008      * ERRORE: se tutte le C.A. risultano escluse
223000981008     c                   IF        NUMCAESC = NUMCAVAL
223100981008     c                   movel     MSG(29)       V2Cmsg
223200981008     c                   seton                                        289083
223300981008     c                   ENDIF
223400981008      *
223500981027      * Se ci sono errori riemetto il SUBF altrimenti richiamo il PGM di Gestione e ricarico il SUBF
223600981027     c                   IF        *IN90 = *OFF
223700981027     c                   exsr      SUB_SELD02
223800981027     c                   eval      *IN91 = *ON
223900981027     c                   ENDIF
224000981008      *
224100981008     C                   ENDSR
224200980601      **********************************************************************
224300980601      * CONTROLLI VIDEO 2
224400980601      **********************************************************************
224500980601     C     Ctrd02        BEGSR
224600980601      *
224700980623     c                   z-add     1             nrr2
224800980702     c                   clear                   W_GIACE
224900981008      *
225000981008      * Se premuto F1 o F6 imposto campo conferma
225100981008     c                   IF        *INKF = *ON  or  *INKA = *ON
225200981008     C                   eval      W_INKF = '1'
225300981008     c                   ELSE
225400981008     C                   eval      W_INKF = '0'
225500981008     c                   ENDIF
225600980623      *
225700980623     c     NRR2          CHAIN     FI00S02                            32
225800980601      *
225900980604     c     *IN32         doweq     *OFF
226000980624     c     *IN90         andeq     *OFF
226100980622      *
226200981008      * Forzo scelta 'G': se scelta blank non bloccata ed è stato premuto F1
226300981008     c                   IF        V2Csce = *blanks  and  *INKA = *ON
226400981008     c                             and  V2HI82 <> '1'
226500981008     c                   eval      V2Csce = 'G'
226600981008     c                   ENDIF
226700981008      *
226800980622     c     V2Csce        LOOKUP    OPZ                                    31
226900981008
227000981008
227100980622     c                   SELECT
227200981008
227300980623     c                   WHEN      *IN31 = *OFF                                 OPZ non gestita
227400980623     c                   movel     MSG(22)       V2Cmsg
227500980624     c                   seton                                        289083
227600981008
227700981009     c                   WHEN      V2Csce = 'E'  and  *INKA = *OFF              OPZ non consentita
227800981009     c                   movel     MSG(30)       V2Cmsg
227900981009     c                   seton                                        289083
228000981009
228100981008     C                   WHEN      V2Csce = *blanks                             nessuna scelta
228200981008
228300981008     C                   WHEN      V2Csce = 'E'                                 esclusa
228400981008     c                   movel     *blanks       V2Csce
228500981008
228600981022     c                   WHEN      V2Csce = '4'                                 NOTE PER STAMPA
228700981022     c                   exsr      SUB_stnote
228800980826     c                   movel     *blanks       V2Csce
228900981008     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
229000990121     c                   z-add     nrr1          nrr3
229100981022
229200981022     c                   WHEN      V2Csce = '8'                                 NOTE
229300981022     c                   exsr      SUB_note
229400981022     c                   movel     *blanks       V2Csce
229500981022     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
229600990121     c                   z-add     nrr1          nrr3
229700981008
229800981027     c                   WHEN      V2Csce = 'G'  and  V2Cnsp = *zeros           gest. key sped. 0
229900980731     c                   movel     MSG(23)       V2Cmsg
230000980731     c                   seton                                        289083
230100981008
230200981027     c                   WHEN      V2Csce = 'G'  and  V2Hcog = 'S'              Obbligatorio F6
230300981008     c                              and W_INKF = '0'
230400980731     c                   movel     MSG(27)       V2Cmsg
230500980731     c                   seton                                        289083
230600981008
230700981027     c                   WHEN      V2Csce = '2'  and  V2Hges = 'N'              non gestibile
230800980803     c                              and  AZN_tipo <> 'A'                          tranne apertura
230900980702     c                   movel     MSG(24)       V2Cmsg
231000980702     c                   seton                                        289083
231100981008
231200981027     c                   WHEN      V2Csce = '2'  and  V2Hchi = '1'              non gestibile
231300980710     c                   movel     MSG(26)       V2Cmsg
231400980710     c                   seton                                        289083
231500990406
231600990406     c                   WHEN      V2Csce = 'R'  and  V2Hges = 'N'              non gestibile
231700990406     c                   movel     MSG(24)       V2Cmsg
231800990406     c                   seton                                        289083
231900981008
232000980702     c                   WHEN      V2Csce = '9'                                 visualizza sped.
232100980624     c                   exsr      SUB_visbol
232200980624     c                   movel     *blanks       V2Csce
232300980624     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
232400990121     c                   z-add     nrr1          nrr3
232500981008
232600980622     c                   OTHER
232700020128
232800020128     c                   eval      $ErrFRANCH   =  *off
232900020128if  3c                   if            AZN_tipo =  'G'
233000020128     c                             and V2Csce   =  'G'
233100020128     c                             and V2Cfca   =  WFspl
233200020128     c                   exsr      CtrFRANCH                                    Controllo Franchigia
233300020128if  4c                   if        $ErrFRANCH   <> *off                         Franchigia errore
233400020128sel 5c                   select
233500020128w   5c                   when      $ErrFRANCH   =  '1'
233600020128     c                   movel     MSG(13)       V2Cmsg
233700050208w   5***c                   when      $ErrFRANCH   =  '2'
233800050208     ***c                   movel     MSG(14)       V2Cmsg
233900020128w   5c                   other
234000020128     c                   movel     *all'? '      V2Cmsg
234100020128e   5c                   endsl
234200020128     c                   seton                                        289083
234300020128e   4c                   endif
234400020128e   3c                   endif
234500020128
234600020128if  3c                   if        $ErrFranch   = *off
234700020128
234800981204     c                   if        V2Csce = 'G'  and  SIMFEL <> *zeros          Controllo Giacenza
234900981008     c                   exsr      CTRGIA
235000980702     c                   endif
235100981008     c                   IF        W_giace = 'S'                                Giacenza errore
235200980702     c                   movel     MSG(25)       V2Cmsg
235300980702     c                   seton                                        289083
235400980702     c                   ELSE
235500981008      * elaboro selezione
235600981211     c                   z-add     1             NUMCAVAL
235700981008     c                   exsr      SUB_SELD02
235800990406      *     torno al subfile se il chiamato ritorna F3 o msg errore che emetto a video
235900990406     c                   eval      *in90 = (O00F03 = 'S'  or  O00ERR = 'E')
236000990406     c                   IF        O00err = 'E'  and  O00msg <> *blanks
236100990406     c                   movel     O00msg        V2Cmsg
236200990406     c                   eval      *in28 = *on
236300990406     c                   ENDIF
236400990406      *
236500990121     c                   z-add     nrr1          nrr3
236600981214      *     se ho modificato o gestito almeno una C.A. ricarico il subfile
236700981214     c  N91              eval      *in91 = (*in90 = *off  and  O00F12 <> 'S'
236800981008     c                                     and (V2Csce = 'G'  or  V2Csce = '2'))
236900981008     c                   movel     *blanks       V2Csce                         azzero scelta
237000980702     c                   ENDIF
237100020128
237200020128e   3c                   endif
237300981008
237400980622     c                   ENDSL
237500981008
237600981008
237700980623     c                   eval      *IN81 = (V2HI81 = '1')
237800980623     c                   eval      *IN82 = (V2HI82 = '1')
237900981209     c                   eval      *IN84 = (V2Cnev = *zeros)
238000160226     c                   eval      *IN86 = (V2HI86 = *on)
238100980623      *
238200980622     c                   update    fi00s02
238300980803     c   83              eval      *IN83 = *OFF
238400980623      *
238500980624     c                   IF        *IN90 = *OFF
238600980623     c                   add       1             nrr2
238700980623     c     NRR2          CHAIN     FI00S02                            32
238800980804     c                   ENDIF
238900980604      *
239000980623     c                   enddo
239100981008      *
239200980601     C                   ENDSR
239300980604      **********************************************************************
239400980624      * VISUALIZZAZIONE SPEDIZIONE
239500980604      **********************************************************************
239600980624     C     SUB_VISBOL    BEGSR
239700980604      *
239800980624     c                   movel     *blanks       WF03
239900980624      *
240000980624     c                   SELECT
240100980624      * partenza
240200161122     c*                  WHEN      V2Htpb = 'P'
240300161122     c*                  clear                   DSVBP_F03
240400161122     C*                  z-add     V2Haas        DSVBP_aas
240500161122     C*                  z-add     V2Clnp        DSVBP_lnp
240600161122     C*                  z-add     V2Cnrs        DSVBP_nrs
240700161122     C*                  z-add     V2Cnsp        DSVBP_nsp
240800161122     c*                  movel(P)  RSUT          DSVBP_rsut
240900161122     C*                  z-add     simfel        DSVBP_sim
241000161122     C*                  movel(P)  DS_visbolp    KPJBU
241100161122     C*                  call      'FNLS05R'
241200161122     C*                  parm                    KPJBA
241300980624      *
241400161122     C*                  movel     KPJBU         DS_visbolp
241500161122     c*                  if        DSVBP_f03 <> *BLANKS
241600161122     C*                  movel     'S'           WF03
241700161122     c*                  endif
241800161122      * filiale
241900161122     c                   WHEN      V2Htpb <> 'S'
242000161122     c                   clear                   paramu6ds1
242100161122     C                   z-add     V2Haas        pa1aas_
242200161122     C                   z-add     V2Clnp        pa1lnp_
242300161122     C                   z-add     V2Cnrs        pa1nrs_
242400161122     C                   z-add     V2Cnsp        pa1nsp_
242401161123     C                   eval      PA1tla_='L'
242600161122     C                   movel(P)  paramu6ds1    KPJBU
242700161122     C                   call      'FNLRU6R1'
242800980624     C                   parm                    KPJBA
242900980624      *
243000161122     C                   movel     KPJBU         paramu6ds1
243100161122     c                   if        pa1f03_   <> *BLANKS
243200980624     C                   movel     'S'           WF03
243300980624     c                   endif
243400980624      * sede
243500981204     c                   WHEN      V2Htpb = 'S'
243600990806     c                   clear                   TNSB50DS
243700990910     C                   MOVEL     'D05'         I50OP0
243800990806     C                   z-add     V2Haas        D50aas
243900990806     C                   z-add     V2Clnp        D50lnp
244000990806     C                   z-add     V2Cnrs        D50nrs
244100990806     C                   z-add     V2Cnsp        D50nsp
244200980624      *
244300990806     C                   MOVEL(P)  TNSB50DS      KPJBU
244400990806     C                   CALL      'TNSB50R'
244500990806     C                   PARM                    KPJBA
244600990806      *
244700990806     C                   movel     KPJBU         TNSB50DS
244800990806     c                   if        O50F03 <> *BLANKS
244900980624     C                   movel     'S'           WF03
245000980624     c                   endif
245100980624      *
245200980624     c                   ENDSL
245300980624      *
245400980624     C                   ENDSR
245500980722      **********************************************************************
245600981022      * VISUALIZZO NOTE PER STAMPA
245700980722      **********************************************************************
245800981022     C     SUB_stnote    BEGSR
245900980722      *
246000981026     C                   clear                   FIDN10DS
246100981026     c     *dmy          move      V2Cdca        DATA_eur
246200981026     c     *eur          move      DATA_eur      i10dta
246300981026     c                   movel     'D'           i10trc
246400981026     c                   movel     'T'           i10std
246500981026     c                   movel     'C'           i10tpd
246600990121     c                   movel     dsnumca       i10nkt
246700981026      *
246800990121     c                   IF        AZN_tipo = 'I'  or
246900990121     c                              (AZN_tipo = 'M'
247000990121     c                                   and  (V2Hges = 'N'  or  V2HCHI = '1'))
247100990121     c                   movel     'I'           i10flm
247200990121     c                   ELSE
247300990121     c                   movel     'V'           i10flm
247400990121     c                   ENDIF
247500981026      *
247600981026     c                   movel(P)  fidn10ds      kpjbu
247700981026     C                   CALL      'FIDN10R'
247800981026     C                   PARM                    kpjba
247900981026      *
248000981026     c                   movel     *blanks       WF03
248100981026     C                   movel     KPJBU         fidn10ds
248200981026     c                   if        o10f03    <> *BLANKS
248300981026     C                   movel     'S'           WF03
248400981026     c                   endif
248500981022      *
248600980722     C                   ENDSR
248700981022      **********************************************************************
248800981022      * VISUALIZZO NOTE
248900981022      **********************************************************************
249000981022     C     SUB_note      BEGSR
249100981022      *
249200981022     C                   clear                   FIDN10DS
249300981022     c     *dmy          move      V2Cdca        DATA_eur
249400981022     c     *eur          move      DATA_eur      i10dta
249500981022     c                   movel     'N'           i10trc
249600981022     c                   movel     'C'           i10tpd
249700981022      *
249800981022     c                   movel     'V'           i10flm
249900981022     c                   movel     dsnumca       i10nkt
250000981022     c                   movel     V2Hfca        i10nks
250100981022      *
250200981022     c                   movel(P)  fidn10ds      kpjbu
250300981022     C                   CALL      'FIDN10R'
250400981022     C                   PARM                    kpjba
250500981022      *
250600981022     c                   movel     *blanks       WF03
250700981022     C                   movel     KPJBU         fidn10ds
250800981022     c                   if        o10f03    <> *BLANKS
250900981022     C                   movel     'S'           WF03
251000981022     c                   endif
251100981022      *
251200981022     C                   ENDSR
251300980702      **********************************************************************
251400980702      * CONTROLLO SE PER LA SPEDIZIONE CI SONO GIACENZE APERTE
251500980702      **********************************************************************
251600980702     C     CTRGIA        BEGSR
251700980702      *
251800040715      *       verifico solo le giacenze in arrivo
251900050331     c     kgiac         chain     Tigcp21l                           30
252000050331     c                   if        *in30 = *off  and gcpfas < 40
252100980702     c                   eval      W_giace = 'S'
252200980702     c                   endif
252300980702      *
252400980702     C                   ENDSR
252500020128      **********************************************************************
252600020128      * CONTROLLO FLAG FRANCHIGIA
252700020128      **********************************************************************
252800020128     C     CtrFRANCH     BEGSR
252900020128      *
253000020128     c                   z-add     V2Haac        Kaac
253100020128     c                   z-add     V2Cfil        Kfil
253200020128     c                   z-add     V2Cnca        Knca
253300020128     c     kdct          chain     FNDCT000
253400020128if  1c                   if        %found(FNDCT01L)
253500020128     c                   movel     DCTflo        DDCT01
253600020128     c                   else
253700020128     c                   clear                   DDCT01
253800020128e   1c                   endif
253900020128      *
254000020128sel 1c                   select
254100020128w   1c                   when      not %found(FNDCT01L)
254200020128     c                             or  DCTatb   = 'A'
254300020128     c                   movel     'N'           $ErrFRANCH
254400120822w   1c****               when          KNMUS    =  §STDpco
254500120822     c****                         and §DCTpfra =  'F'
254600120822     c****               movel     '1'           $ErrFRANCH
254700050208w   1***c                   when          KNMUS             <> §STDpco
254800050208     ***c                             and %subst(KNMUS:1:3) <> 'EDP'
254900050208     ***c                             and §DCTpfra <> 'F'
255000050208     ***c                   movel     '2'           $ErrFRANCH
255100020128e   1c                   endsl
255200020128      *
255300020128     C     EndCtrFran    ENDSR
255400980624      **********************************************************************
255500980624      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 2/5/G
255600980624      **********************************************************************
255700980624     C     SUB_SELD02    BEGSR
255800980624      *
255900980604     c                   clear                   FIDN00DS
256000980604      *
256100980610     c                   SELECT
256200980610     c                   WHEN      V2Csce = '2'
256300980611     c                   movel     'M'           I00mod
256400980615     c                   z-add     V2Hfca        I00fca
256500980610     c                   WHEN      V2Csce = '5'
256600980610     c                   movel     'I'           I00mod
256700980615     c                   z-add     V2Hfca        I00fca
256800980610     c                   WHEN      V2Csce = 'G'
256900980610     c                   movel     'G'           I00mod
257000980615     c                   z-add     V2Cfca        I00fca
257100990406     c                   WHEN      V2Csce = 'R'
257200990406     c                   movel     'E'           I00mod
257300990406     c                   z-add     V2Hfca        I00fca
257400981027     c                   WHEN      *INKA = *ON
257500981027     c                   movel     'G'           I00mod
257600981027     c                   z-add     W_V1Cfca      I00fca
257700980610     c                   OTHER
257800980610     c                   movel     AZN_tipo      I00mod
257900980615     c                   z-add     V2Hfca        I00fca
258000980610     c                   ENDSL
258100980610      *
258200980609     c                   z-add     V1Cfgs        I00fgs
258300980604     c                   z-add     V2Haac        I00aac
258400980604     c                   z-add     V2Cfil        I00fil
258500980604     c                   z-add     V2Cnca        I00nca
258600980604     c                   z-add     V2Haas        I00aas
258700980604     c                   z-add     V2Clnp        I00lnp
258800980604     c                   z-add     V2Cnrs        I00nrs
258900980604     c                   z-add     V2Cnsp        I00nsp
259000981027     c                   z-add     V2Hlna        I00lna
259100981027     c                   movel     V2Ctad        I00tad
259200990908     c                   z-add     V2Caae        I00aae
259300990908     c                   z-add     V2Cnev        I00nev
259400990908     c                   z-add     numcaval      I00nce
259500980604      *
259600010709     c                   movel     V2Htpb        I00tpb
259700010709      *
259800981009     c                   movel(P)  FIDN00DS      kpjbu
259900981102
260000981102      *
260100981102      *  RICHIAMO PGM PER ELABORAZIONE SCELTA
260200981102      *
260300981102     c                   SELECT
260400990406      * Manutenzione C.A./Responsabilità
260500990406     C                   WHEN      V2Csce = '2'  or  V2Csce = 'R'
260600981102     c                   call      'FIDN03R'
260700981102     c                   parm                    kpjba
260800981102      * Interrogazione
260900981102     C                   WHEN      V2Csce = '5'
261000981102     c                   call      'FIDN01R'
261100981102     c                   parm                    kpjba
261200981102      * Gestione
261300981102     C                   OTHER
261400980604     c                   call      V2HPGM
261500980604     c                   parm                    kpjba
261600980604      *
261700981102     c                   ENDSL
261800981102
261900980604     c                   movel     kpjbu         FIDN00DS
262000980702      *
262100980702     c                   IF        V2Csce = 'G'  AND
262200980702     c                             O00F12 = *blanks  AND  O00F03 = *BLANKS
262300980702     c                   movel     '1'           V2Hi82
262400980702     c                   ENDIF
262500980604      *
262600980604     C                   ENDSR
262700980609      **********************************************************************
262800980609      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER APERTURA
262900980609      **********************************************************************
263000980609     C     SUB_SELOPN    BEGSR
263100980609      *
263200980609     c                   clear                   FIDN00DS
263300980609      *
263400980609     c                   movel     AZN_tipo      I00mod
263500980609     c                   MOVEL     V1Cfca        I00fca
263600980609     c                   z-add     V1Cfgs        I00fgs
263700980609      *
263800980609     c                   IF        V1Cnsp > 0
263900980609     c                   z-add     V1Caas        I00aas
264000980609     c                   z-add     V1Clnp        I00lnp
264100980609     c                   z-add     V1Cnrs        I00nrs
264200980609     c                   z-add     V1Cnsp        I00nsp
264300980609     c                   z-add     Wlnab         I00lna
264400980609     c                   ENDIF
264500980609      *
264600980609     c                   movel     V1Ctad        I00tad
264700980609     c                   movel     Wtibol        I00tpb
264800980609      *
264900980609      * Aggancio anagrafico fasi per il nome del PGM da chiamare
265000980609     c                   clear                   FIDN05DS
265100980609     c                   movel     'C'           I05MOD
265200980609     c                   movel     V1Cfca        I05fca
265300980609     C                   CALL      'FIDN05R'
265400980609     c                   PARM                    KPJBA
265500980609     c                   PARM                    FIDN05DS
265600980610      *
265700980609     c                   if        O05rec <> *blanks
265800980609     c                   movel     O05REC        DS_FNDFA
265900980609     C                   endif
266000980609      *
266100981009     c                   movel(P)  FIDN00DS      kpjbu
266200980609     c                   call      DFApgm
266300980609     c                   parm                    kpjba
266400980609      *
266500980609     c                   movel     kpjbu         FIDN00DS
266600980609      *
266700980618     c                   if        O00err <> *blanks
266800980618     c                   movel     O00msg        V1CMSG
266900990406     c                   eval      *in28 = *on
267000990406     c                   eval      *in90 = *on
267100980618     C                   endif
267200980618      *
267300980609     C                   ENDSR
267400980528      **********************************************************************
267500980528      * CONTROLLI STANDARD    * linea *
267600980528      **********************************************************************
267700980528     C     CTR_linea     BEGSR
267800980528      *
267900990406     C                   IF        W_linea = *zeros
268000980527     c                   MOVEL     MSG(6)        v1CMSG
268100990406     c                   eval      *in28 = *on
268200990406     c                   eval      *in90 = *on
268300990406     c                   ELSE
268400990406      *
268500980527     c     W_linea       chain     azorg01l                           30
268600980527     c                   if        *IN30 = *on   OR   orgfva <> *blanks
268700980527     c                   MOVEL     MSG(7)        v1CMSG
268800990406     c                   eval      *in28 = *on
268900990406     c                   eval      *in90 = *on
269000980527     c                   endif
269100980527     c                   ENDIF
269200980527      *
269300980527     c                   ENDSR
269400980527      **********************************************************************
269500980527      * CONTROLLI STANDARD    * anno *
269600980527      **********************************************************************
269700980527     C     CTR_anno      BEGSR
269800980527      *
269900980527     c                   IF        W_anno = 0
270000980527     c                   MOVEL     MSG(9)        v1CMSG
270100980527     c                   seton                                        9028
270200980527     C                   else
270300980527      *
270400980527      *   sistemo anno di due cifre
270500980527     C                   if        W_anno < 100  and W_anno > 60
270600980527     C                   ADD       1900          W_anno
270700980527     C                   endif
270800980527     C                   if        W_anno < 100  and W_anno <= 60
270900980527     C                   ADD       2000          W_anno
271000980527     C                   endif
271100980527      *
271200980527     C                   ENDIF
271300980527      *
271400980527     c                   ENDSR
271500980527      **********************************************************************
271600980527      * CONTROLLI STANDARD    * tipo anomalia *
271700980527      **********************************************************************
271800980527     C     CTR_anomal    BEGSR
271900980527      *
272000980601     C                   eval      W_dtad = *BLANKS
272100980601     C                   clear                   tibs02ds
272200980527      *
272300980527      * Ricerca
272400980601     C     '?'           SCAN      W_ctad                                 31
272500980527      *
272600980527    1C                   if        *in31 = *ON
272700980601     C                   SETON                                        5090
272800980601     C                   MOVEL     'R'           t02mod
272900980601     C                   MOVEL     knsif         t02sif
273000980601     C                   MOVEL     'TAD'         t02cod
273100980527      *
273200980601     C                   CALL      'TIBS02R'
273300980601     C                   PARM                    KPJBA
273400980601     C                   PARM                    TIBS02DS
273500980527      *
273600981019     C                   IF        T02err = *BLANKS
273700981019     C                   movel     T02uni        DTAD
273800981019     C                   movel     T02ke1        W_ctad
273900981019     C                   movel     §TADdesc      W_dtad
274000981019      * Se Anomalia per Eventi non è valida
274100981019     C                   IF        §TADragr = 'E'
274200981019     C                   seton                                        502890
274300981019     c                   movel     MSG(31)       W_MSG
274400981019     C                   ENDIF
274500981019      *
274600981019     C                   ELSE
274700980601     c                   eval      *in28 = *on
274800981019     c                   movel     T02msg        W_MSG
274900981019     C                   ENDIF
275000980527      *
275100980601     C                   ELSE
275200980601      *
275300980527      *  Obbligatorio solo per apertura
275400990111     C                   if        W_ctad = *BLANKS  or  W_ctad = '00'
275500990111     c                   clear                   W_ctad
275600980601     C                   if        AZN_tipo = 'A'  and  *INKJ = *ON
275700980601     C                   MOVEL     MSG(1)        W_MSG
275800980527     C                   SETON                                        502890
275900980527     C                   endif
276000980527      *
276100980601     C                   ELSE
276200980601      *
276300980622      * Controllo
276400980601     C                   MOVEL     'C'           t02mod
276500980601     C                   MOVEL     knsif         t02sif
276600980601     C                   MOVEL     'TAD'         t02cod
276700980601     C                   MOVEL     W_ctad        t02ke1
276800980601      *
276900980601     C                   CALL      'TIBS02R'
277000980601     C                   PARM                    KPJBA
277100980601     C                   PARM                    TIBS02DS
277200980601      *
277300981019     C                   IF        t02err = *BLANKS
277400981019     C                   movel     T02uni        DTAD
277500981019     C                   movel     T02ke1        W_ctad
277600981019     C                   movel     §TADdesc      W_dtad
277700981019      * Se Anomalia per Eventi non è valida
277800981019     C                   IF        §TADragr = 'E'
277900981019     C                   seton                                        502890
278000981019     c                   movel     MSG(31)       W_MSG
278100981019     C                   ENDIF
278200981019      *
278300981019     C                   ELSE
278400981019     C                   seton                                        502890
278500980601     c                   movel     t02msg        W_MSG
278600981019     C                   ENDIF
278700980527      *
278800980601     C                   ENDIF
278900980601     C                   ENDIF
279000980601      *
279100980601     c                   ENDSR
279200980615      *****************************************************************
279300990201      * IMPOSTO CAMPI DI VIDEO 1 PREDEFINITI
279400980615      *****************************************************************
279500980615     C     INZD01        BEGSR
279600980615      *
279700980615      *    Se ricevuto test "Liq.Trans/Pratica Ass." preimposto a video e blocco il campo
279800980615     C                   IF        AZN_trpr = 'T'  or  AZN_trpr = 'P'
279900980615     c                   eval      V1Cfpr = AZN_trpr
280000980615     c                   eval      *IN09 = *ON
280100980615     C                   ENDIF
280200980615      *
280300980615      *    Se ricevuto Codice fase valido preimposto a video e blocco il campo
280400980615     C                   IF        AZN_fase <> *blanks
280500980615     C                   testn                   AZN_fase             31
280600980615     C   31              move      AZN_fase      W001A
280700980615     C   31              eval      *in31 = (W001A >= '0')
280800980615      *
280900980615     C                   if        *IN31 = *ON
281000980615      * Controllo
281100980615     c                   clear                   FIDN05DS
281200980615     c                   movel     'C'           I05MOD
281300980615     C                   move      AZN_fase      I05fca
281400980615     C                   CALL      'FIDN05R'
281500980615     C                   PARM                    KPJBA
281600980615     c                   PARM                    FIDN05DS
281700980615     c                   if        O05rec <> *blanks
281800980615     c                   movel     O05REC        DS_FNDFA
281900980615     C                   endif
282000980615     c                   if        O05ERR = 'E' or DFAatb <> *blanks
282100980615     c                   goto      fine
282200980615     c                   else
282300980615     c                   movel     DFAfca        V1Cfca
282400981026     c                   movel     DFAfca        W_V1Cfca
282500980615     c                   if        AZN_tipo = 'G' and DFAdgc <> *blanks
282600980615     c                   movel     DFAdgc        V1Dfca
282700980615     c                   else
282800980615     c                   movel     DFAdes        V1Dfca
282900980615     C                   endif
283000980615     c                   eval      *IN10 = *ON
283100980615     C                   endif
283200980615      *
283300980615     c                   else
283400980615     c                   goto      fine
283500980615     C                   endif
283600980615     C                   ENDIF
283700980615      *
283800980925      *    Se Gestione emetto rich. visualizzazione altrimenti data esecuzione fase
283900980615     c                   EVAL      *IN11 = (AZN_tipo = 'G')
284000980615      *
284100980615      *    Se Gestione o Apertura imposto costanti differenti per cod. fase
284200980615     c                   EVAL      *IN13 = (AZN_tipo = 'G')
284300980615     c  N13              EVAL      *IN13 = (AZN_tipo = 'A')
284400980615      *
284500990324      *    Imposto: ANNO SPEDIZIONE, ANNO C.A., ANNO EVENTO, ANNO PRATICA
284600980615     c                   move      wdtgio        v1caas
284700980615     c                   move      wdtgio        v1caac
284800980615     c                   move      wdtgio        v1caae
284900990324     c                   move      wdtgio        v1cpra
285000980615      *
285100980615     c                   ENDSR
285200990406      *****************************************************************
285300990406      * IMPOSTO OPZIONI
285400990406      *****************************************************************
285500990406     C     CAROPZ        BEGSR
285600990406      *
285700990406     c                   clear                   opz
285800990406      *
285900990406     C                   SELECT
286000990406      *
286100990406     C                   WHEN      AZN_tipo = 'A'
286200990406     c                   movel     DOPZ1_A       V2Dse1
286300990406     c                   movel     DOPZ2_A       V2Dse2
286400990406     c                   movea     OPZ_A         OPZ
286500120822      *   Consuldanni  non esiste +
286600120822     C*****              WHEN      AZN_tipo = 'G'  and  *IN16 = *ON
286700120822     c*****              movel     DOPZ1_CG      V2Dse1
286800120822     c*****              movel     DOPZ2_C       V2Dse2
286900120822     c*****              movea     OPZ_CG        OPZ
287000990406      *
287100990406     C                   WHEN      AZN_tipo = 'G'  and  SEL(8) = '1'
287200990406     c                   movel(P)  DOPZ1_G1      V2Dse1
287300990406     c                   movel(P)  DOPZ2_G1      V2Dse2
287400990406     c                   movea     OPZ_G1        OPZ
287500990406     c                   eval      *IN15 = *ON                                  Abilito F01
287600990406      *
287700990406     C                   WHEN      AZN_tipo = 'G'
287800990406     c                   movel(P)  DOPZ1_G       V2Dse1
287900990406     c                   movel(P)  DOPZ2_G       V2Dse2
288000990406     c                   movea     OPZ_G         OPZ
288100990406     c                   eval      *IN15 = *OFF                                 Abilito F01
288200120822      *   Consuldanni  non esiste +
288300120822     c*****              WHEN      AZN_tipo = 'I'  and  *IN16 = *ON
288400120822     c*****              movel     DOPZ1_CI      V2Dse1
288500120822     c*****              movel     DOPZ2_C       V2Dse2
288600120822     c*****              movea     OPZ_CI        OPZ
288700990506      *
288800990406     C                   WHEN      AZN_tipo = 'I'
288900990406     c                   movel     DOPZ1_I       V2Dse1
289000990406     c                   movel     DOPZ2_I       V2Dse2
289100990406     c                   movea     OPZ_I         OPZ
289200990406      *
289300990915     C                   WHEN      AZN_tipo = 'M'  and  (SIMFEL = *zeros  or
289400130806     c*********                               %SUBST(knmus:1:3) = §STDpv1  or
289500130806     c*********                               %SUBST(knmus:1:3) = §STDpv2)
289600130806     c                                        §UTEmresca = 'S')
289700990406     c                   movel     DOPZ1_M       V2Dse1
289800990406     c                   movel     DOPZ2_M1      V2Dse2
289900990406     c                   movea     OPZ_M1        OPZ
290000990406      *
290100990406     C                   WHEN      AZN_tipo = 'M'
290200990406     c                   movel     DOPZ1_M       V2Dse1
290300990406     c                   movel     DOPZ2_M       V2Dse2
290400990406     c                   movea     OPZ_M         OPZ
290500990406      *
290600990406     C                   ENDSL
290700990406      *
290800990406     c                   ENDSR
290900980527      *****************************************************************
291000980923      * CARICO TABELLE UTILIZZATE IN APERTURA
291100980527      *****************************************************************
291200980923     C     CARTABAPE     BEGSR
291300020821      *
291400980923      * carico schiera dei codici bolla di recupero
291500980923     C                   eval      xx = *zeros
291600980923     C                   MOVE      '3A'          KCOD
291700980923     C     ktab          SETLL     TABEL00F
291800980923     C     ktab          READE     TABEL00F                               32
291900980923     C     *in32         DOWEQ     *OFF
292000980923     c                   movel     tbluni        ds3a
292100980923     c                   IF        §3arbl =  'R'
292200980923     C                   ADD       1             XX
292300980923     C                   MOVEL     TBLKEY        s3a(xx)
292400980923     c                   ENDIF
292500980923     C     ktab          READE     TABEL00F                               32
292600980923     C                   ENDDO
292700980923      *
292800980923     c                   ENDSR
292900980923      *****************************************************************
293000980923      * ROUTINE INIZIALE
293100980923      *****************************************************************
293200980923     C     *INZSR        BEGSR
293300980923      *
293400980521     C     *ENTRY        PLIST
293500980521     C                   PARM                    KPJBA
293600981009     C                   MOVEL(P)  KPJBU         DS_AZIONE
293700980521      *
293800990406      * Controllo il tipo azione. Se è valido imposto il titolo, viceversa vado a fine pgm
293900980521     C                   SELECT
294000980623      *
294100980521     C                   WHEN      AZN_tipo = 'A'
294200980604     c                   movel     TIT_A         VTCtit
294300981008      *
294400980521     C                   WHEN      AZN_tipo = 'G'
294500980604     c                   movel     TIT_G         VTCtit
294600981008     c                   eval      *in14 = *on                                  Abilito F06
294700990505     c                   eval      *in26 = *on                                  Disabilito ROLLUP
294800980623      *
294900980521     C                   WHEN      AZN_tipo = 'I'
295000980604     c                   movel     TIT_I         VTCtit
295100980623      *
295200990406     C                   WHEN      AZN_tipo = 'M'
295300980619     c                   movel     TIT_M         VTCtit
295400980623      *
295500980521     C                   OTHER
295600980521     C                   goto      FINE
295700990406      *
295800980521     C                   ENDSL
295900980612      *
296000130806     C                   Z-ADD     1             CODUT             1 0
296100130806     C*********          CALL      'X§PARUT'
296200130806     C*********          PARM                    UT§DSE0F
296300130806     C*********          MOVEL     RAGUT         RSUT
296400130806     C*********          MOVEL     REC80         CNCR80
296500130806
296600130806     c                   exsr      DatiJob
296700130806      * Imposto le abilitazioni dell'utente
296800130806     c                   eval      dute01 = UTEfaf
296900980615      *
297000981028      * Se Apertura in sede vado a fine
297100981028     C                   IF        AZN_tipo = 'A'  and  SIMFEL = *ZEROS
297200981028     C                   goto      FINE
297300981028    2C                   ENDIF
297400981028      *
297500981028      * Se Apertura abilito F10
297600980615     C                   eval      *IN12 = (AZN_tipo <> 'A')
297700980521      *
297800030228      * "P.O. IN GESTIONE"
297900030228     c                   SELECT
298000030228      *   se sono in sede non visualizzo e non modifico
298100030228     C                   WHEN      SIMFEL = *zeros
298200030228     c                   movel     046           v1cfgs
298300030228     C                   eval      *in06 = *on
298400030228     C                   eval      *in07 = *on
298500030228      *   se sono 2° livello visualizzo e non modifico
298600130806     c***********        WHEN      simtpp = '2' or simtpp = *blanks
298700130806     c                   WHEN      DUTlpo = '2' or DUTlpo = *blanks
298800130806     c**********         movel     simpou        v1cfgs
298900130806     c                   movel     DUTpou        v1cfgs
299000030228     c                   eval      *in06 = *on
299100030228     C                   eval      *in07 = *off
299200030228      *   negli altri casi visualizzo e modifico
299300030228     c                   OTHER
299400130806     c*********          movel     simpou        v1cfgs
299500130806     c                   movel     DUTpou        v1cfgs
299600030228     c                   eval      *in06 = *off
299700030228     C                   eval      *in07 = *off
299800030228    2C                   ENDSL
299900030228      *
300000040121      *  DECODIFICO P.O. IN GESTIONE
300100030228     C     V1CFGS        CHAIN     AZORG01L                           31
300200040121     C  N31              MOVEL     orgDES        V1DFGS
300300980923      *
300400030228      * Se sede disattivo F14 e forzo p.o. 46 in L6, se p.o. carico L6
300500981028     C                   IF        SIMFEL = *zeros
300600020502     C                   eval      L6(1) = v1cfgs
300700980604     C                   eval      *IN08 = *ON
300800980528     C                   ELSE
300900990215     C                   z-add     v1cfgs        Wfgs
301000990217     C                   CLEAR                   L6
301100990215     C                   EXSR      CARL6
301200990215     C                   movea     LIN           L6
301300980528    2C                   ENDIF
301400010904
301500010904      * AGGANCIO TABELLA GED
301600010910     c                   clear                   DGEDDN
301700010904     c                   clear                   TIBS02DS
301800010904     C                   MOVEL     'C'           t02mod
301900010904     C                   MOVEL     knsif         t02sif
302000010904     C                   MOVEL     'GED'         t02cod
302100010904     C                   MOVEL(P)  'DANNI'       t02ke1
302200010904      *
302300010904     C                   CALL      'TIBS02R'
302400010904     C                   PARM                    KPJBA
302500010904     C                   PARM                    TIBS02DS
302600010904      *
302700010904     C                   if        t02err = *BLANKS
302800010910     C                   MOVEL     T02UNI        DGEDDN
302900010904     C                   endif
303000980923
303100990406      * AGGANCIO TABELLA STD
303200990406     c                   clear                   TIBS02DS
303300990406     C                   MOVEL     'C'           t02mod
303400990406     C                   MOVEL     knsif         t02sif
303500990406     C                   MOVEL     'STD'         t02cod
303600990406     C                   MOVEL(P)  1             t02ke1
303700010910     C                   MOVEL(P)  §gedDBA       t02ke2
303800990406      *
303900990406     C                   CALL      'TIBS02R'
304000990406     C                   PARM                    KPJBA
304100990406     C                   PARM                    TIBS02DS
304200990406      *
304300990406     C                   if        t02err = *BLANKS
304400990406     C                   MOVEL     T02UNI        DSTD
304500120822     c******             eval      *in16 = (%SUBST(knmus:1:5) = §STDpco)
304600990406     C                   endif
304700990406
304800981019      * CARICO SCHIERA TABELLA TAD
304900980923     c                   z-add     *zeros        xx
305000980923     C                   MOVEL     'TAD'         Wtbecod
305100980923     c     Wtbecod       chain     tntbe000                           31
305200980923      *
305300980923     c                   DOW       *in31 = *off
305400981019     c                   movel     TBEuni        DTAD
305500981019      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
305600980923     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
305700981019      *                            and TBEatb = *BLANKS  and  §TADragr <> 'E'
305800980923     c                   add       1             xx
305900980923     c                   movel     TBEke1        KTA(XX)
306000980923     c                   movel     TBEuni        DTA(XX)
306100980923     c                   endif
306200980923     c     Wtbecod       reade     tntbe000                               31
306300980923     c                   enddo
306400980923
306500980923      * reperisco data e ora
306600980923     C                   TIME                    W0140
306700980923      * UDATE IN GGMMAAAA
306800980923     C                   MOVE      W0140         WDTGIO
306900980923      * UDATE IN AAAAMMGG
307000980923     C                   Z-ADD     WDTGIO        G02DAT
307100980923     C                   MOVEL     *BLANK        G02ERR
307200980923     C                   CALL      'XSRDA8'
307300980923     C                   PARM                    WLBDAT
307400980923     C                   MOVEL     G02INV        DATEU
307500980923     C     *iso          MOVEL     DATEU         DATA_oggi
307600980923
307700981204      * Apertura files
307800981204     c                   IF        SIMFEL = *zeros
307900981204     c                   open      TNTMD01L
308000990322     c                   open      TNTMD02L
308100990322     c                   open      TNTMD03L
308200050217     c                   open      fndct04l
308300981204     c                   ELSE
308400981204     c                   open      FNBLP01L
308500981204     c                   open      FNARB01L
308600050331     c                   open      Tigcp21L
308700050217     c                   open      fndct24l
308800981204     c                   IF        AZN_tipo = 'A'
308900981204      *   Per Apertura carico tabelle apposite
309000980923     c                   EXSR      CARTABAPE
309100980923     c                   ENDIF
309200981204     c                   ENDIF
309300980923
309400980923      * Imposto campi di video 1 predefiniti
309500980923     c                   exsr      inzd01
309600980923
309700980521     C     KTAB          KLIST
309800980521     C                   KFLD                    CODUT
309900980521     C                   KFLD                    KCOD
310000980526      *
310100980527     C     kspe          KLIST
310200981008     C                   KFLD                    kAAS
310300980527     C                   KFLD                    kLNP
310400980527     C                   KFLD                    kNRS
310500980527     C                   KFLD                    kNSP
310600981204      *
310700981026     C     KDET          KLIST
310800981026     C                   KFLD                    V1Caae
310900981026     C                   KFLD                    V1Cnev
311000980702      *
311100980702     c     kgiac         klist
311200980702     c                   kfld                    V2Haas
311300980702     c                   kfld                    V2Clnp
311400980702     c                   kfld                    V2Cnrs
311500980702     c                   kfld                    V2Cnsp
311600980702     c                   kfld                    Wfrg
311700981204      *
311800981204     c     kdka          klist
311900981204     c                   kfld                    V2Haac
312000981204     c                   kfld                    V2Cfil
312100981204     c                   kfld                    V2Cnca
312200981204     c                   kfld                    WTRC
312300981204      *
312400981204     c     ktmd          klist
312500981204     c                   kfld                    V2Haas
312600981204     c                   kfld                    V2Clnp
312700981204     c                   kfld                    V2Cnrs
312800981204     c                   kfld                    V2Cnsp
312900980527      *
313000980527     C     Kdct          klist
313100980527     C                   KFLD                    kaac
313200980527     C                   KFLD                    kfil
313300980527     C                   KFLD                    knca
313400980527      *
313500980527     C     Kdct4         klist
313600980527     C                   KFLD                    kaac
313700980527     C                   KFLD                    kmgc
313800980527     C                   KFLD                    kfil
313900980527     C                   KFLD                    knca
314000980527      *
314100980527     C     Keve          klist
314200980527     C                   KFLD                    V1Caae
314300980527     C                   KFLD                    V1Cnev
314400990323      *
314500990323     C     Kpra          klist
314600990323     C                   KFLD                    V1Cdit
314700990323     C                   KFLD                    V1Cpra
314800990323     C                   KFLD                    V1Cprn
314900980608      *
315000980608     c     kdcf          klist
315100980608     c                   kfld                    Kaac
315200980608     c                   kfld                    kfil
315300980608     c                   kfld                    knca
315400041026      *
315500041026     c     kdcl01        klist
315600041110     c                   kfld                    V1Casn
315700041026      *
315800041026     c     kdcl02        klist
315900041110     c                   kfld                    V1Casn
316000041026     c                   kfld                    w_V1Cdea
316100980525      *
316200980521     C                   ENDSR
316300130806      *---------------------------------------------------------------*
316400130806      *?Reperimento Dati del job (Utente/Operativi)                  ?*
316500130806      *---------------------------------------------------------------*
316600130806     c     DatiJob       BegSr
316700130806      *
316800130806     c     *dtaara       define    §azute        AZUTEds
316900130806     c     *dtaara       define    §datiute      dDatiUte
317000130806      *
317100130806     c                   in(E)     *dtaara
317200130806     c                   IF        %ERROR or RSUT = *blanks
317300130806     c                   clear                   Tibs34Ds
317400130806     c                   call      'TIBS34R'
317500130806     c                   parm                    Tibs34Ds
317600130806     c                   in        *dtaara
317700130806     c                   ENDIF
317800130806      *
317900130806     c                   EndSr
318000980521      *---------------------------------------------------------------------------------------------
318100980521** MSG  Lungh. 78                                                            *
318200981019Immettere il Tipo Anomalia                                                          1
318300980521Spedizione inesistente o in gestione ad altro p.o.                                  2
318400150914Se inserita la data è obbligatorio indicare il numero assegno o bonifico            3
318500980610Tipo Anomalia inesistente o annullato                                               4
318600020821Per aprire una C.A. imettere la spedizione                                          5
318700980522Immettere la linea                                                                  6
318800980522Linea inesistente o annullata                                                       7
318900990322Immettere numero pratica assicurativa                                               8
319000980521Immettere l'anno                                                                    9
319100980522Impossibile aprire C.A. su bolle di recupero                                        10
319200990322Immettere codice società. BAR = Bartolini, SDI = S.D.I.                             11
319300980522Immettere una selezione valida                                                      12
319400020128Fase non eseguibile per anomalia in franchigia                                      13
319500050208? LIBERO ? -   Fase non eseguibile per anomalia NON in franchigia                   14
319600980525Immettere data formalmente valida                                                   15
319700980525Immettere data DAL minore o uguale alla data AL                                     16
319800980623Codice Fase inesistente o annullato                                                 17
319900980525Test "Liquid.Trans/Pratica ass." non congruente con Codice Fase                     18
320000980525Data esecuzione fase maggiore della data C.A.                                       19
320100980605Immettere un codice fase                                                            20
320200980608Codice Fase non utilizzabile                                                        21
320300980623Scegliere un opzione valida                                                         22
320400980731C. A. non gestibile: manca il numero spedizione                                     23
320500980702C. A. non manutenzionabile: in gestione ad altro P.O.                               24
320600980702C. A. non gestibile: la spedizione risulta in giacenza                              25
320700980710C. A. non manutenzionabile: la C.A. è chiusa                                        26
320800980731Premere F6 per gestire la fase selezionata                                          27
320900981008Opzione non consentita con la Conferma Totale                                       28
321000981008Tutte le C.A. sono state escluse, nessuna elaborazione effettuata                   29
321100981009Opzione non consentita con la Conferma                                              30
321200981019Tipo anomalia valido solo per Gestione Eventi                                       31
321300981026Per la fase selezionata è obbligatorio selezionare un evento                        32
321400981026Numero Evento inesistente                                                           33
321500981027C. A. non gestibile: manca il numero spedizione, escludere dall'elaborazione        34
321600990115C. A. annullata                                                                     35
321700990312Scorrimento indietro o in avanti oltre il primo o l'ultimo record del file.         36
321800110930Impossibile aprire C.A. su bolla ancora da borderizzare                             37
321900110930Impossibile aprire C.A. su bolla chiusa in partenza                                 38
322000150701Beneficiario impostabile se inseriti gli estremi del pagamento                      39
322100150701Del Beneficiario inserire  il codice  o  la ragione sociale (anche parziale)        40
