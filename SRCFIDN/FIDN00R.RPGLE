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
033400161122     d fnlru6ds      e ds
033500161122      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
033600161122     D PARAMu6ds       DS
033700161122     d* CAMPI DI OUTPUT:
033800161122     d* . Key spedizione
033900161122     D  pa1AAS                14     17  0
034000161122     D  pa1LNP                18     20  0
034100161122     D  pa1NRS                21     22  0
034200161122     D  pa1NSP                23     29  0
034300161122     d* . Flag '1'=Premuto F3=Fine
034400161122     D  PA1F03                30     30
034500161122     d* CAMPI DI INPUT:
034600161122     d* . Modalità di richiamo
034700161122     D  PA1FLG                31     31
034800161122     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
034900161122     d*                                in interrogazione della bolla ed evitare
035000161122     d*                                errore di chiamata ricorsiva
035100161122     D  PA1GIA               144    144
035200161122     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
035300161122     d  pa1rma               149    163
035400161122     d  pa1xco               164    164
035500161122     d* non utilizzato
035600161122     d  pa1flo               165    256
035700161122      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
035800161122     D PARAMU6ds1      DS
035900161122     d** INPUT/OUTPUT (se richiesta la selezione bolla)
036000161122     D  pa1AAS_               14     17  0
036100161122     D  pa1LNP_               18     20  0
036200161122     D  pa1NRS_               21     22  0
036300161122     D  pa1NSP_               23     29  0
036400161122     d** OUTPUT
036500161122     D  PA1F03_               30     30
036600161122     d** INPUT
036700161122     D  PA1FLG_               31     31
036800161122     D*    SE RICH DA PGM GIAC. = 1
036900161122     D  PA1GIA_              144    144
037000161122     d** OUTPUT
037100161122     d  pa1rma_              149    163
037200161122     d  pa1xco_              164    164
037300161122     d** INPUT
037400161122     d*   Tipo lancio: "C" -           CHIUSO CON LR
037500161122     d*                "L" - ELABORO E CHIUDO CON LR
037600161122     d*                " " - ELABORO E CHIUDO IN RETRN
037700161122     d  pa1tla_              165    165
037800161124     d  pa1err_              166    166
037900161124     d  pa1flo_              167    256
038000980525     D                 DS                  INZ
038100980525     D w_v1cdcad               1      8  0
038200980525     D  w_AAdcad               1      4  0
038300980525     D  w_MGdcad               5      8  0
038400980521      *
038500980525     D                 DS                  INZ
038600980525     D w_v1cdefd               1      8  0
038700980525      *
038800980521     D WLBDAT          DS                  INZ
038900980521     D  G02DAT                 1      8  0
039000980521     D  G02INV                 9     16  0
039100980521     D  G02ERR                17     17
039200980521     D  G02TGI                18     22  0
039300980525      *
039400980521      * Ricezione dati da azione
039500980521     D DS_AZIONE       DS                  INZ
039600980521     D  AZN_tipo               1      1
039700980521     D  AZN_fase               2      4
039800980521     D  AZN_trpr               5      5
039900980525      *
040000980521     D FIDN00DS      E DS
040100980529     D FIDN05DS      E DS
040200980722     D FIDN10DS      E DS
040300040714      *
040400040714     D FIDN12DS      E DS
040500040714     d  skKey                 26    305    dim(20)
040600040714     d  skAnn                306    325    dim(20)
040700040714     d  skDca                326    485  0 dim(20)
040800040714     d  skFca                486    545  0 dim(20)
040900040714     d  skDch                546    705  0 dim(20)
041000040714     d  skCch                706    745    dim(20)
041100040714
041200040714     d dsKey           ds
041300040714     d  dsaac                         4  0
041400040714     d  dsfil                         3  0
041500040714     d  dsnca                         7  0
041600040714      *
041700081215     D FIDNBEDS      E DS
041800990806     D TNSB50DS      E DS
041900980529     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
042000130806     D************ UT§DSE0F      E DS
042100980601     D TIBS02DS      E DS
042200980601     D FNLV50DS      E DS
042300130806     D************ CNCR80        E DS
042400981229     D DDCT01        E DS
042500980601     D DTAD          E DS
042600980601     D DS3A          E DS
042700990406     D DSTD          E DS
042800010910     D DGEDDN        E DS                  INZ
042900980525      *
043000130806      * - Parametri x Controllo profilo utenti
043100130806     d TIBS34ds      e ds                  inz
043200130806      * - Ds di riferimento al file esterno AZUTE00F
043300130806     d AZUTEds       e ds                  extname(AZUTE00F)
043400130806      * - Ds per dati organigramma
043500130806     d dDatiUte      e ds
043600130806
043700980525      * DS PER trul06r - caricamento £X
043800980521     D TRUL06DS      E DS
043900980521     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
044000980521      *
044100980521     D KPJBA         E DS
044200020430     D* rem                    1      3
044300020430     D* remfil                 4      6
044400130806
044500130806     d dUTE01        e ds
044600980521      *
044700980521     D                SDS
044800980521     D  VTCPGM                 1     10
044900981009
045000981009      *  titolo videata (lunghezza massima 34)
045100981009     D Kannullata      C                   CONST('   ANNULLATA                 -
045200981009     D                                         ')
045300980623
045400980623      *------------------------------------------------------------------------*
045500980521      *---------------  C A L C O L O  ----------------------------------------*
045600980521      *------------------------------------------------------------------------*
045700980623
045800980521     C     for01         tag
045900981104     c                   eval      Wcari1 = 'S'
046000980623
046100980521      * Emissione VIDEO1
046200980521     C                   exsr      EMISd01
046300980623
046400990115      * Azzero Campi ed Indicatori
046500990115     c     41            do        89            XX
046600990115     c                   eval      *IN(XX) = *OFF
046700990115     c                   enddo
046800990115     c                   clear                   V1Cmsg
046900990115     c                   eval      *IN28 = *off
047000990115     c                   eval      *IN90 = *off
047100990115
047200980525      * f3=Fine
047300980521     c   kc              goto      fine
047400980623
047500980521      * f18=Cambio P.O. in gestione
047600980522     c   ks              eval      *in05 = *on
047700980521     c   ks              goto      for01
047800980623
047900980521      * 05 on --> controllo P.O. in gestione
048000980521     c     *in05         ifeq      *on
048100980521     C                   exsr      ctrfgs
048200980521     c   90              goto      for01
048300980608     c                   eval      *in05 = *off
048400980521     c                   end
048500980623
048600161122      * F07=interrogazione bolle.
048700981009     C                   If        *INKG = *ON
048800980623
048900020821     C                   If        V1Cfgs = 046                                 bolle SEDE
049000990806     c                   clear                   TNSB50DS
049100990910     C                   MOVEL     'D00'         I50OP0
049200990806     C                   MOVEL(P)  TNSB50DS      KPJBU
049300990806     C                   CALL      'TNSB50R'
049400980522     C                   PARM                    KPJBA
049500980526     c                   else                                                   bolle ARRIVI
049600980624      *
049700161122     c*                  clear                   DS_ricbolA
049800161122     C*                  MOVEL     2             DSRBA_flg
049900161122     C*                  MOVEL(P)  DS_ricbola    KPJBU
050000161122     C*                  CALL      'FNLR36R'
050100161122     C*                  PARM                    KPJBA
050200161122     C*                  MOVEL     kpjbu         DS_ricbola
050300161122     c*                  if          DSRBA_nsp > *zeros
050400161122     C*                  z-add     DSRBA_aas     V1Caas
050500161122     C*                  z-add     DSRBA_lnp     V1Clnp
050600161122     C*                  z-add     DSRBA_nrs     V1Cnrs
050700161122     C*                  z-add     DSRBA_nsp     V1Cnsp
050800161122     C*                  endif
050900161122     c                   clear                   paramu6ds
051000161122     C                   MOVEL     '2'           Pa1FLG
051100161122     C                   MOVEL     PARAmu6ds     KPJBU
051200161122     C                   CALL      'FNLRU6R'
051300161122     C                   PARM                    KPJBA
051400161122     C                   MOVEL     KPJBU         PARAMu6ds
051500161122     C                   IF        pa1nsp>*zeros
051600161122     C                   Z-ADD     Pa1AAS        V1CAAS
051700161122     C                   Z-ADD     Pa1LNP        V1CLNP
051800161122     C                   Z-ADD     Pa1NRS        V1CNRS
051900161122     C                   Z-ADD     Pa1NSP        V1CNSP
052000161122     C                   ENDIF
052100990201     C                   endif
052200980624      *
052300980522     C                   goto      for01
052400980522     C                   endif
052500980623
052600980526      * F14=interrogazione bolle partenze
052700161122     C*                  If        *INKN = *ON
052800161122     c*                  clear                   DS_ricbolP
052900161122     C*                  MOVEL     1             DSRBP_flg
053000161122     C*                  MOVEL(P)  DS_ricbolp    KPJBU
053100161122     C*                  CALL      'FNLS04R'
053200161122     C*                  PARM                    KPJBA
053300161122     C*                  MOVEL     kpjbu         DS_ricbolp
053400161122     c*                  if          DSRBP_nsp > *zeros
053500161122     C*                  z-add     DSRBP_aas     V1Caas
053600161122     C*                  z-add     DSRBP_lnp     V1Clnp
053700161122     C*                  z-add     DSRBP_nrs     V1Cnrs
053800161122     C*                  z-add     DSRBP_nsp     V1Cnsp
053900161122     C*                  endif
054000161122     C*                  goto      for01
054100161122     C*                  endif
054200980623
054300980525      * Controlli 1a videata
054400980521     c                   exsr      ctrd01
054500980623
054600980521     c   90              goto      for01
054700980623
054800990312      * In Apertura se premuto F10 controllo se selezioni valide, se ok richiamo pgm per apertura
054900980527     C                   if        *INKJ = *ON
055000980527     c                   exsr      ctrd01_A
055100990312     c  N90              EXSR      sub_selopn
055200980610     c                   goto      for01
055300980610     c                   ENDIF
055400980623
055500990406      * Imposto opzioni in base al tipo elaborazione
055600990406     c                   exsr      CARopz
055700981008
055800980526      * Carico subfile
055900990315     C     RIE02         TAG
056000990315      *
056100981102     C                   eval      *in91 = *off
056200990315     C                   eval      *in23 = *off
056300990315     C                   clear                   nrr1
056400990315     c                   clear                   nrr3
056500990315     c                   clear                   Numcaval
056600990414     c                   clear                   Numcacar
056700981102      *
056800980526     C                   exsr      RIEd02
056900980623
057000990115      * Se non ho caricato record torno a VIDEO 1, se arrivo da Video 1 emetto prima una videata di
057100990115      *    segnalazione. Se nel caricamento ho trovato errori torno a Video 1
057200981104     C                   if        NRR1 = *zeros
057300990115     C     *INKL         DOWEQ     *OFF
057400981104     C     WCARI1        andeq     'S'
057500990115     C     *IN28         andeq     *OFF
057600980527     C                   EXFMT     FI00D03
057700980528     C                   ENDDO
057800980527     c                   goto      for01
057900980609     C                   endif
058000981104     c                   clear                   Wcari1
058100980623
058200980526      * Emissione VIDEO2
058300980623     C     for02         tag
058400980526     C                   exsr      EMISd02
058500980623
058600990315      * F03 = Fine
058700980526     c   kc              goto      fine
058800980623
058900990315      * F12 = Precedente
059000980526     c   kl              goto      for01
059100990315
059200990315      * ROLLUP = Carico il subfile
059300990505     c                   IF        *IN23 = *ON  and  AZN_tipo <> 'G'
059400990315     c                   exsr      Ried02
059500990315     c                   If        Wcapagina = *zeros
059600990315     c                   movel     msg(36)       V2Cmsg
059700990315     c                   eval      *in28 = *on
059800990315     c                   Endif
059900990315     c                   GOTO      for02
060000990315     c                   ENDIF
060100980623
060200981026      * I Controlli in video 2 sono diversi se ho selezionato fasi particolari ed ho premuto F01
060300981026     c                   IF        *INKA = *ON   and
060400981203     c                              (W_V1Cfca = WFisa  or  W_V1Cfca = WFise
060500981203     c                               or  W_V1Cfca = WFecc  or  W_V1Cfca = WFsav)
060600981008     c                   exsr      ctrd02ka
060700981008     c                   ELSE
060800981008     c                   exsr      ctrd02
060900981008     c                   ENDIF
061000980623
061100990315      * Ricarico il SFL per aggiornamento dati
061200981008     c   91              goto      rie02
061300981008
061400981008      * Riemetto video 2
061500981008     c                   goto      for02
061600980623
061700980521     c     fine          tag
061800981204
061900981204      * Chiudo pgm aperti
062000981029     c                   clear                   tibs02ds
062100981029     c                   movel     'C'           t02tla
062200981029     c                   call      'TIBS02R'
062300981029     c                   parm                    kpjba
062400981029     c                   parm                    tibs02ds
062500981204
062600981204     c                   EVAL      *INLR = *ON
062700980521      **********************************************************************
062800980521      * EMISSIONE VIDEO 1
062900980521      **********************************************************************
063000980521     C     EMISD01       BEGSR
063100980521      *
063200990325      * In apertura, se non ci sono errori, mi posiziono sulla linea di partenza del n° spedizione
063300990310     c                   IF        AZN_tipo = 'A'  and  *IN47 = *off
063400990310     c     41            Do        89            XX
063500041026     c                   If        XX <> 47  and  XX < 65
063600990310     c                   eval      *IN47 = (*IN(XX) = *OFF)
063700990310     c  N47              leave
063800990310     c                   EndIf
063900990310     c                   EndDo
064000990310     c                   ENDIF
064100990310
064200980521     c                   write     Fi00t01
064300130403      * Imposto a video "BAR" nel campo ditta della pratica assicurativa. Metto il campo protetto
064400130403     c                   eval      v1cdit = 'BAR'
064500990316      * Se sede emetto videata diversa
064600980528     c                   IF        SIMFEL = *zeros
064700990316     c                   write     Fi00z04
064800990316     c                   exfmt     Fi00d04
064900980521     c                   ELSE
065000980521     c                   write     Fi00z01
065100990316     c                   exfmt     Fi00d01
065200980521     c                   ENDIF
065300990316
065400980605     c                   setoff                                       289047
065500980521     c                   clear                   v1cmsg
065600990316
065700980521     C                   ENDSR
065800980521      **********************************************************************
065900980521      * CONTROLLO IL P.O. IN IN GESTIONE
066000980521      **********************************************************************
066100980521     C     CTRFGS        beGSR
066200980521      *
066300980609     C                   setoff                                       9040
066400980521     c                   clear                   v1dfgs
066500980609     C                   clear                   FNLV50DS
066600980521     C                   MOVEL     KNMUS         D50PRU
066700980521     C                   MOVEL     V1cfgs        D50FGS
066800170426     c                   eval      d50tcn='E'
066900980521     C                   CALL      'FNLV50R'
067000980521     C                   PARM                    FNLV50DS
067100980521      *
067200980521     C                   if        D50ERR <> *blanks
067300170426     C                   MOVEL(P)  D50MSG        V1cMSG
067400980521     C                   SETON                                        409028
067500980609     C                   else
067600980521      *
067700990215     C                   z-add     v1cfgs        Wfgs
067800990217     C                   CLEAR                   L6
067900980521     C                   EXSR      CARL6
068000990215     C                   MOVEA     LIN           L6
068100980521      * decodifica
068200980609     c     v1cfgs        CHAIN     azorg                              31
068300040121     C  N31              movel     orgDES        V1DFGS
068400981009      *
068500980609     C                   endif
068600980521      *
068700980609     c                   endsr
068800980521      **************************************************************************
068900980521      * CARICO SCHIERA L6 DA TABELLA £6
069000980521      **************************************************************************
069100980521     C     carl6         BEGSR
069200980521      *
069300980521     C                   CLEAR                   TRUL06DS
069400980521     C                   MOVE      '£6'          D06COD
069500990215     C                   MOVEL(P)  Wfgs          D06key
069600981009     C                   MOVEL(P)  TRUL06DS      KPJBU
069700980521     C                   CALL      'TRUL06R'
069800980521     C                   PARM                    KPJBA
069900980521     C                   MOVEL     KPJBU         TRUL06DS
070000980521      *
070100980521     c                   endsr
070200980521      **********************************************************************
070300980521      * CONTROLLI VIDEO 1
070400980521      **********************************************************************
070500980521     C     Ctrd01        BEGSR
070600990115      *
070700990115      * Azzero campi di comodo e schiera selezioni
070800980521     c                   clear                   wtibol
070900980521     c                   clear                   waas
071000980521     c                   clear                   wnsp
071100980521     c                   clear                   wlnab
071200980622     C                   clear                   sel
071300990324     c                   clear                   Wlm
071400990324     c                   clear                   Wld
071500980521      *
071600980527     C                   SELECT
071700980527      *
071800020821     C                   WHEN      V1Cnca > 0  and V1Cnsp = 0
071900980527     C                   EXSR      CTRD01_ca                                    controlli numero CA
072000990111     c                   If        *in28 = *off  and  AZN_tipo <> 'I'
072100990111     C                   EXSR      CTRD01_sel                                   controlli selezioni
072200990111     c                   Endif
072300980527      *
072400020821     C                   WHEN      V1Cnca = 0  and V1Cnsp > 0
072500980527     C                   EXSR      CTRD01_spe                                   controlli spedizione
072600990111     c                   If        *in28 = *off  and  AZN_tipo <> 'I'
072700990111     C                   EXSR      CTRD01_sel                                   controlli selezioni
072800990111     c                   Endif
072900980527      *
073000020821     C                   WHEN         DS_V1_CA  = *ZEROS and
073100980527     C                                DS_V1_SPE = *ZEROS
073200980527     C                   EXSR      CTRD01_sel                                   controlli selezioni
073300980527      *
073400980527     C                   OTHER
073500980527     C                   MOVEL     MSG(12)       v1CMSG                         errore
073600980527     c                   seton                                        2890
073700980527      *
073800980527     C                   ENDSL
073900980527      *
074000980527     c                   ENDSR
074100980527      **********************************************************************
074200980622      * CONTROLLI VIDEO 1     * controllo selezione CA *
074300980527      **********************************************************************
074400980527     C     CTRD01_ca     BEGSR
074500980527      *
074600980527      * Controllo linea creazione
074700980527     C                   z-add     V1Cfil        W_linea
074800980527     c                   exsr      CTR_linea
074900980527     c                   IF        *IN28 = *ON
075000980527     c                   eval        *IN44 = *on
075100980527     C                   ELSE
075200980527      *
075300980527      * controllo l'anno
075400980527     c                   z-add     V1Caac        W_anno
075500980527     c                   exsr      CTR_anno
075600980527     c                   if        *IN28 = *ON
075700980527     c                   eval        *IN46 = *on
075800980527     C                   else
075900980527     c                   z-add     W_anno        V1Caac
076000980622     c                   movel     '1'           SEL(2)
076100980527     c                   endif
076200980527      *
076300980527     c                   ENDIF
076400980527      *
076500980527     c                   ENDSR
076600980527      **********************************************************************
076700980622      * CONTROLLI VIDEO 1     * controllo selezione spedizione *
076800980527      **********************************************************************
076900980527     C     CTRD01_spe    BEGSR
077000980527      *
077100980527      * Controllo linea di partenza
077200980527     C                   z-add     V1Clnp        W_linea
077300980527     c                   exsr      CTR_linea
077400980527     c                   IF        *IN28 = *ON
077500980527     c                   eval        *IN47 = *on
077600980527     C                   ELSE
077700980527      *
077800980527      * controllo l'anno
077900980527     c                   z-add     V1Caas        W_anno
078000980527     c                   exsr      CTR_anno
078100980527     c                   IF        *IN28 = *ON
078200980527     c                   eval        *IN49 = *on
078300980527     c                   else
078400980527     c                   z-add     W_anno        V1Caas
078500980622     c                   movel     '1'           SEL(3)
078600980527     c                   endif
078700980527      *
078800980527     c                   endif
078900980527      *
079000980527     c     ev1spe        ENDSR
079100980527      **********************************************************************
079200980527      * CONTROLLI VIDEO 1     * controllo selezioni *
079300980527      **********************************************************************
079400980527     C     CTRD01_sel    BEGSR
079500980527      *
079600980527      * Controllo il tipo anomalia
079700980601     c                   movel     V1Ctad        W_ctad
079800980527     C                   EXSR      CTR_anomal
079900980601     c                   movel     W_ctad        V1Ctad
080000980601     c                   movel     W_dtad        V1Dtad
080100980601     c   28              movel     W_MSG         V1Cmsg
080200980601     C                   IF        V1Ctad <> *blanks
080300980622     c                   movel     '1'           SEL(4)
080400980527     C                   endif
080500980601     C   90              GOTO      ev1sel
080600980527      *---------------------------------------------
080700981026     c                   clear                   DETtad
080800981026      * Se numero evento > 0 controllo
080900981026     c                   IF        V1Cnev > 0
081000981026      *    anno evento
081100981026     c                   z-add     V1Caae        W_anno
081200981026     c                   exsr      CTR_anno
081300981026     c                   IF        *IN28 = *ON
081400981026     c                   eval        *IN52 = *on
081500981026     C                   ELSE
081600981026     c                   z-add     W_anno        V1Caae
081700981026      *    controllo su file eventi
081800981026     c     KDET          CHAIN     FNDET000                           28
081900981026     c                   IF        *IN28 =*ON
082000981026     c                   movel     MSG(33)       V1Cmsg
082100981026     c                   seton                                        9051
082200981026     C                   ELSE
082300981026     c                   movel     '1'           SEL(5)
082400981026     c                   ENDIF
082500981026     c                   ENDIF
082600981026      *
082700981026     c                   ENDIF
082800981026     C   28              goto      ev1sel
082900980527      *---------------------------------------------
083000980527      * Controllo data C.A. "dal"
083100980527     C                   clear                   W_v1cdcad
083200980527     C                   IF        V1Cdcad <> 0
083300980527     C                   MOVE      V1Cdcad       G02DAT
083400980527     C                   eval      g02err = *blanks
083500980527     C                   CALL      'XSRDA8'
083600980527     C                   PARM                    WLBDAT
083700980527     C                   if        G02ERR = '1'
083800980527     C                   SETON                                        289053
083900980527     C                   MOVEL     MSG(15)       V1CMSG
084000980527     C                   goto      ev1sel
084100980527     C                   else
084200980527     C                   MOVEL     G02DAT        V1Cdcad
084300980527     C                   MOVE      G02INV        W_v1cdcad
084400980527     C                   endif
084500980527     C                   ENDIF
084600980527      * Controllo data C.A. "al"
084700980527     C                   clear                   W_v1cdcaa
084800980527     C                   IF        V1Cdcaa <> 0
084900980527     C                   move      V1cdcaa       G02DAT
085000980527     C                   eval      g02err = *blanks
085100980527     C                   CALL      'XSRDA8'
085200980527     C                   PARM                    WLBDAT
085300980527     C                   if        G02ERR = '1'
085400980527     C                   SETON                                        289054
085500980527     C                   MOVEL     MSG(15)       V1CMSG
085600980527     C                   goto      ev1sel
085700980527     C                   else
085800980527     C                   MOVEL     G02DAT        V1Cdcaa
085900980527     C                   MOVE      G02INV        W_v1cdcaa
086000980527     C                   endif
086100980527      * data C.A. "dal" maggiore di data C.A. "al"
086200980527     C                   if        W_v1cdcaa < W_v1cdcad
086300980527     C                   SETON                                        289053
086400980527     C                   MOVEL     MSG(16)       V1CMSG
086500980527     C                   goto      ev1sel
086600980527     C                   endif
086700980527     C                   ENDIF
086800980527      * Se data C.A. "al" è vuota, la imposto come la data C.A. "dal"
086900980527     C                   if        W_v1cdcaa = 0  AND  W_v1cdcad <> 0
087000980527     C                   z-add     W_v1cdcad     W_v1cdcaa
087100980527     C                   endif
087200980622      * Imposto selezione
087300980527     C                   if        W_v1cdcaa > *zeros
087400980622     c                   movel     '1'           SEL(6)
087500980527     C                   endif
087600980527      *---------------------------------------------
087700980622      * Imposto selezione Liquidazione transattiva/Pratica assicurativa
087800980527     c                   IF        V1cfpr <> *blanks
087900980622     c                   movel     '1'           SEL(7)
088000980527     C                   endif
088100980527      *---------------------------------------------
088200980605      * Controllo codice fase solo se non ricevuta in KPJBA
088300980527      *
088400990201     C                   IF        *IN10 = *off
088500990201     C                   clear                   V1Dfca
088600990201     C                   IF        V1Cfca <> *blanks  and  V1Cfca <> '000'
088700980615      *
088800980529      * Ricerca
088900980605     C     '?'           SCAN      v1CFCA                                 30
089000980529     C                   IF        *IN30 = *ON
089100980529     C                   seton                                        9056
089200980529     c                   clear                   FIDN05DS
089300980529     c                   movel     'R'           I05MOD
089400980529     C                   CALL      'FIDN05R'
089500980604     c                   PARM                    KPJBA
089600980604     c                   PARM                    FIDN05DS
089700980605     c                   if        O05err = 'E'
089800980605     c                   movel     '000'         V1Cfca
089900980605     c                   else
090000980529     c                   movel     O05REC        DS_FNDFA
090100980605     c                   movel     DFAfca        V1Cfca
090200980615     c                   if        AZN_tipo = 'G'  and  DFAdgc <> *blanks
090300980611     c                   movel     DFAdgc        V1Dfca
090400980611     c                   else
090500980605     c                   movel     DFAdes        V1Dfca
090600980529     C                   endif
090700980611     C                   endif
090800980527      *
090900980529     c                   ELSE
091000980529      *
091100980529     C                   testn                   V1Cfca               30
091200980529      * testo ultimo carattere
091300980529     C   30              move      V1CFCA        W001A
091400980529     C   30              eval      *in30 = (W001A >= '0')
091500980529      * Se codice non numerico errore
091600980529     C                   IF         *IN30 = *OFF
091700980529     C                   eval      *in28 = *on
091800980529     C                   ELSE
091900980529      * Controllo
092000980529     c                   clear                   FIDN05DS
092100980529     c                   movel     'C'           I05MOD
092200980529     c                   movel     V1Cfca        I05fca
092300980529     C                   CALL      'FIDN05R'
092400980604     c                   PARM                    KPJBA
092500980529     c                   PARM                    FIDN05DS
092600980529     c                   if        O05rec <> *blanks
092700980529     c                   movel     O05REC        DS_FNDFA
092800980615     c                   if        AZN_tipo = 'G'  and  DFAdgc <> *blanks
092900980611     c                   movel     DFAdgc        V1Dfca
093000980611     c                   else
093100980611     c                   movel     DFAdes        V1Dfca
093200980611     C                   endif
093300980529     C                   endif
093400980527      *
093500980529     C                   endif
093600980529     C                   ENDIF
093700980529      *
093800980605     C                   IF        *IN28 = *ON  or  O05err = 'E'
093900980605     C                             or  DFAatb <> *blanks
094000980605     C                   seton                                        289056
094100980605     C                   movel     MSG(17)       V1CMSG
094200980605     C                   ELSE
094300980605      *
094400980608      * Utilizzabile in Gestione
094500980907     c                   if        AZN_tipo = 'G' and DFAfgc <> *blank
094600980605     C                   seton                                        289056
094700980605     C                   movel     MSG(21)       V1CMSG
094800980529     C                   else
094900980605      *
095000980527      * Controllo se selezione "Liq.Trans./Pratica Ass." è congruente con cod. fase
095100980529     c                   IF        V1cfpr <> *blanks  and  DFAval <> *blanks
095200980527     c                             and V1Cfpr <>  DFAval
095300980605     C                   seton                                        289056
095400980605     C                   movel     MSG(18)       V1CMSG
095500980605     C                   ENDIF
095600980529     C                   endif
095700980605     C                   ENDIF
095800980605      *
095900980605     C                   ENDIF
096000990201     C                   ENDIF
096100980529      *
096200980622      * Imposto selezione e memorizzo fase
096300980605     C                   IF        V1Cfca <> *blanks  and  V1Cfca <> '000'
096400980605     C                             and  *IN28 = *off
096500981026     C                   movel     V1Cfca        W_V1Cfca
096600980622     c                   movel     '1'           SEL(8)
096700981026     C                   ELSE
096800981026     C                   clear                   W_V1Cfca
096900981026     C                   ENDIF
097000980529      *
097100980529     C   90              goto      ev1sel
097200980527      *---------------------------------------------
097300980527      * Controllo data esecuzione fase "dal"
097400980527     C                   clear                   W_v1cdefd
097500980527     C                   IF        V1Cdefd <> 0
097600980527     C                   MOVE      V1Cdefd       G02DAT
097700980527     C                   eval      g02err = *blanks
097800980527     C                   CALL      'XSRDA8'
097900980527     C                   PARM                    WLBDAT
098000980527     C                   if        G02ERR = '1'
098100980527     C                   SETON                                        289057
098200980527     C                   MOVEL     MSG(15)       V1CMSG
098300980527     C                   goto      ev1sel
098400980527     C                   else
098500980527     C                   MOVEL     G02DAT        V1Cdefd
098600980527     C                   MOVE      G02INV        W_v1cdefd
098700980527     C                   endif
098800980527     C                   ENDIF
098900980527      * Controllo data esecuzione fase "al"
099000980527     C                   clear                   W_v1cdefa
099100980527     C                   IF        V1Cdefa <> 0
099200980527     C                   move      V1cdefa       G02DAT
099300980527     C                   eval      g02err = *blanks
099400980527     C                   CALL      'XSRDA8'
099500980527     C                   PARM                    WLBDAT
099600980527     C                   if        G02ERR = '1'
099700980527     C                   SETON                                        289058
099800980527     C                   MOVEL     MSG(15)       V1CMSG
099900980527     C                   goto      ev1sel
100000980527     C                   else
100100980527     C                   MOVEL     G02DAT        V1Cdefa
100200980527     C                   MOVE      G02INV        W_v1cdefa
100300980527     C                   endif
100400980527      * data esecuzione fase "dal" maggiore della data esecuzione fase "al"
100500980527     C                   if        W_v1cdefa < W_v1cdefd
100600980527     C                   SETON                                        289057
100700980527     C                   MOVEL     MSG(16)       V1CMSG
100800980527     C                   goto      ev1sel
100900980527     C                   endif
101000980527     C                   ENDIF
101100980527      * Se data esecuzione fase "al" è vuota, la imposto come la data esecuzione fase "dal"
101200980527     C                   if        W_v1cdefa = 0  AND  W_v1cdefd <> 0
101300980527     C                   z-add     W_v1cdefd     W_v1cdefa
101400980527     C                   endif
101500980527      * Se impostata la data esecuzione fase è obbligatorio il codice fase
101600980527     C                   IF        V1CFCA = *BLANKS   and   V1CDEFA <> *ZEROS
101700980527     C                   MOVEL     MSG(20)       V1CMSG
101800980527     C                   SETON                                        289056
101900980527     C                   goto      ev1sel
102000980527     C                   endif
102100980622      * Imposto selezione
102200980527     C                   if        W_v1cdefa > *zeros
102300980622     c                   movel     '1'           SEL(9)
102400980527     C                   endif
102500980527      *---------------------------------------------
102600980527      * Controllo P.O per selezione
102700980527     c                   eval      V1Dpos = *blanks
102800981026     C                   IF        V1Cpos > *zeros
102900980622     c                   movel     '1'           SEL(10)
103000980527     C                   z-add     V1Cpos        W_linea
103100980527     c                   exsr      CTR_linea
103200980527     c                   if        *IN28 = *ON
103300980527     c                   eval        *IN59 = *on
103400980527     C                   goto      ev1sel
103500980527     c                   else
103600990215     C                   z-add     v1cpos        Wfgs
103700990217     C                   CLEAR                   WL6
103800990215     C                   EXSR      CARL6
103900990215     C                   movea     LIN           WL6
104000980527     c                   movel     ORGDES        V1Dpos
104100980527     c                   endif
104200980527     c                   ENDIF
104300981026      *---------------------------------------------
104400981026      * Se impostata fase INVIO A SEDE DOCUMENTI C.A. PER DENUNCIA EVENTI obbligo scelta n. evento
104500981026     C                   IF        W_V1Cfca = WFise  and  SEL(5) <> '1'
104600981026     c                   seton                                        289051
104700981026     C                   MOVEL     MSG(32)       V1CMSG
104800981026     C                   goto      ev1sel
104900981026     c                   ENDIF
105000990322      *---------------------------------------------
105100990324      * Controllo pratica assicurativa
105200990322      *
105300990322     c                   SELECT
105400130403     c                   WHEN      V1Cprn = *zeros  and  V1Cdit = 'BAR'
105500130403     c**                 WHEN      V1Cprn = *zeros
105600130403     c**                 movel     msg(8)        V1Cmsg
105700130403     C**                 seton                                        289061
105800130403     c**                 WHEN      V1Cdit <> 'BAR'  and  V1Cdit <> 'SDI'
105900130403     c**                 movel     msg(11)       V1Cmsg
106000130403     C**                 seton                                        289062
106100990322     c                   OTHER
106200990322     c                   z-add     V1Cpra        W_anno
106300990322     c                   exsr      CTR_anno
106400990322     c                   if        *IN28 = *ON
106500990322     C                   seton                                        289063
106600990322     c                   movel     msg(9)        V1Cmsg
106700990322     C                   else
106800990322     c                   z-add     W_anno        V1Cpra
106900990322     c                   movel     '1'           SEL(11)
107000990322     c                   endif
107100990322     c                   ENDSL
107200990322      *
107300990322     C   28              goto      ev1sel
107400080718      *---------------------------------------------
107500080718      * Controllo C.A. APERTE O CHIUSA
107600080718     c                   IF        V1CACE <> *blanks
107700080718     c                   movel     '1'           SEL(15)
107800080718     c                   ENDIF
107900990322      *---------------------------------------------
108000990324      * Controllo mittente
108100990324     c                   IF        V1Cmit <> *blanks
108200990322     c                   movel     '1'           SEL(12)
108300990324     c     ' '           checkr    V1Cmit        Wlm
108400990322     c                   ENDIF
108500990322      *---------------------------------------------
108600990324      * Controllo destinatario
108700990324     c                   IF        V1Cdes <> *blanks
108800990322     c                   movel     '1'           SEL(13)
108900990324     c     ' '           checkr    V1Cdes        Wld
109000990322     c                   ENDIF
109100041026      *---------------------------------------------
109200041026      * Controllo numero e data assegno
109300041026      *
109400041026     c                   SELECT
109500041111     c                   WHEN      V1Cdea = *zeros  and  V1Casn = *blanks
109600041111     c                   WHEN      V1Cdea = *zeros  and  V1Casn <> *blanks
109700041111     C                   clear                   W_v1cdea
109800041111     c                   movel     '1'           SEL(14)
109900041111     c                   WHEN      V1Cdea > *zeros  and  V1Casn = *blanks
110000041026     c                   movel     msg(3)        V1Cmsg
110100041111     C                   seton                                        289064
110200041110     c                   WHEN      V1Cdea > *zeros  and  V1Casn <> *blanks
110300041026     C                   move      V1Cdea        G02DAT
110400041026     C                   eval      g02err = *blanks
110500041111     C                   call      'XSRDA8'
110600041111     C                   parm                    WLBDAT
110700041111     C                   If        G02ERR = '1'
110800041111     C                   seton                                        289065
110900041111     C                   movel     MSG(15)       V1CMSG
111000041111     C                   Else
111100041111     C                   movel     G02DAT        V1Cdea
111200041111     C                   move      G02INV        W_v1cdea
111300041111     c                   movel     '1'           SEL(14)
111400041111     C                   EndIf
111500041026      *
111600041026     c                   ENDSL
111700041026      *
111800041026     C   28              goto      ev1sel
111900150701      *---------------------------------------------
112000150701      * -?Controllo codice e ragione sociale Beneficiario?
112100150701     c                   select
112200150701     c                   when      V1Cksc <> *zeros  and
112300150714     c                             V1Casn =  *blanks
112400150701     c                   seton                                        289066
112500150701     c                   eval      V1Cmsg =  Msg(39)
112600150701     c                   leavesr
112700150701     c                   when      V1Dksc <> *blank  and
112800150714     c                             V1Casn =  *blanks
112900150701     c                   seton                                        289067
113000150701     c                   eval      V1Cmsg =  Msg(39)
113100150701     c                   leavesr
113200150701     c                   when      V1Cksc <> *zeros  and
113300150701     c                             V1Dksc <> *blank
113400150701     c                   seton                                        289066
113500150701     c                   eval      V1Cmsg =  Msg(40)
113600150701     c                   leavesr
113700150701     c                   endsl
113800990322      *---------------------------------------------
113900990322      *
114000980527     c     ev1sel        ENDSR
114100980521      **********************************************************************
114200980527      * CONTROLLI VIDEO 1     * APERTURA *
114300980521      **********************************************************************
114400980521     C     Ctrd01_A      BEGSR
114500980521      *
114600020821      * apertura obbligatoria per spedizione
114700020821     C                   IF        V1Cnsp = 0
114800020821     C                   MOVEL     MSG(5)        v1CMSG
114900980527     c                   seton                                        289047
115000980521     C                   GOTO      ecd01_A
115100980521     c                   end
115200990127
115300980521      * ----------------------------
115400980521      * APERTURA C.A. PER SPEDIZIONE
115500980521      * ----------------------------
115600980521      *
115700980521      * Controllo linea di partenza
115800980522     C                   z-add     V1Clnp        W_linea
115900980527     c                   exsr      CTR_linea
116000980522     c                   IF        *IN28 = *ON
116100980522     c                   eval        *IN47 = *on
116200980522     C                   goto      ecd01_A
116300980522     c                   ENDIF
116400980522      *
116500980522      * controllo l'anno spedizione
116600020821     c                   z-add     V1Caas        W_anno
116700980527     c                   exsr      CTR_anno
116800020821    2c                   IF        *IN28 = *ON
116900980522     c                   eval        *IN49 = *on
117000980522     C                   goto      ecd01_A
117100980522     C                   ELSE
117200020821     c                   z-add     W_anno        V1Caas
117300020821    2c                   ENDIF
117400980521      *
117500981218      * la sped. deve essere in gestione al p.o. in gestione e deve esistere su fnblp o fnarb
117600981218      * se bolla locale apro solamente in arrivo
117700981218     c                   z-add     V1Caas        Kaas
117800981218     c                   z-add     V1Clnp        Klnp
117900981218     c                   z-add     V1Cnrs        Knrs
118000981218     c                   z-add     V1Cnsp        Knsp
118100981218      *
118200981218      * Cerco in partenza
118300981218     c     kspe          chain     fnblp01l                           30
118400020821    2c                   IF        *in30 = *off
118500981218      *   determino se partenza in gestione
118600981218     c     V1Clnp        lookup    l6                                     30
118700170426    3c                   If        *in30 = *on and d50val_P='S'
118800110930     c
118900110930     c* Se bolla non borderizzata --> errore
119000110930     c                   if        arbdbr=0
119100110930     c                   seton                                        489028
119200110930     c                   movel     msg(37)       v1cmsg
119300110930     c                   goto      ecd01_A
119400110930     c                   endif
119500110930     c
119600110930     c* Se bolla chiusa in partenza --> errore
119700110930     c                   if        arbft1='N' and arbft2='N'
119800110930     c                   seton                                        489028
119900110930     c                   movel     msg(38)       v1cmsg
120000110930     c                   goto      ecd01_A
120100110930     c                   endif
120200110930     c
120300981218      *   se l'arrivo non è in gestione, quindi la sped. non è locale, gestione Partenza
120400981218      *   viceversa se è locale aggancio FNARB
120500981218     c     arblna        lookup    l6                                     30
120600170426     c                   if        %lookup(arblna:l6)=0 or d50val_a='N'
120700170426     c**n30              movel     'P'           wtibol
120800170426     c                   movel     'P'           wtibol
120900170426     c                   else
121000110620     c* Se è locale ma dirottata--> la tratto come "P"
121100110620     c*  queto controllo lo devo fare qui perchè per le bolle autogenerate non mi crea FNARB
121200110620     c*  su locale se bolle legata ma solo blp e arb dell'ultima figlia
121300110620     c
121400170426     c** 30              if        arblnp=arblna and arbcca='1'
121500170426     c                   if        arblnp=arblna and arbcca='1'
121600110620     c                   movel     'P'           wtibol
121700110620     c                   endif
121800170426     c                   endif
121900110620     c
122000020821    3c                   Endif
122100981218    2c                   ENDIF
122200981218      *
122300981218      * Se non trovata in partenza o bolla locale cerco in arrivo
122400020821    2c                   IF        Wtibol = *blanks
122500981218     c     kspe          chain     fnarb01l                           30
122600981218    3c                   If        *in30 = *off
122700981218     c     arblna        lookup    l6                                     30
122800170426     c
122900170426    4c                   if        *in30 and d50val_a='S'
123000170426     c                   movel     'A'           wtibol
123100170426    5c                   if        arblnp=arblna and arbcca='1'
123200110329     c                   movel     'P'           wtibol
123300170426    5c                   Endif
123400170426    4c                   Endif
123500110329     c
123600110329    3c                   Endif
123700020821    2c                   ENDIF
123800981218      *
123900981218      * Se non trovata errore
124000020821    2c                   IF        Wtibol <> *blanks
124100981218     c                   z-add     arblna        wlnab
124200020821    2c                   ELSE
124300981218     c                   seton                                        489028
124400981218     c                   movel     msg(2)        v1cmsg
124500981218     c                   goto      ecd01_A
124600020821    2c                   ENDIF
124700980521      *
124800980521      * non è possibile aprire c.a. su bolle di recupero
124900980521     c     arbcbo        lookup    s3a                                    30
125000980521     c                   IF        *in30 = *on
125100980522     c                   movel     msg(10)       v1cmsg
125200980521     c                   seton                                        489028
125300980521     C                   GOTO      ecd01_A
125400980521     c                   endif
125500980521      *
125600980521     c     ecd01_A       ENDSR
125700980608      *****************************************************************
125800980608      * CARICAMENTO SUBFILE VIDEO 2
125900980608      *****************************************************************
126000990315     C     RIED02        BEGSR
126100980527      *
126200980527      * Pulisco subfile
126300990315     c                   IF        *IN23 = *off
126400990315     C                   eval         *IN21 = *OFF
126500980527     C                   WRITE     FI00C02
126600990315     C                   eval         *IN20 = *ON
126700990315     C                   eval         *IN21 = *ON
126800990315     c                   ENDIF
126900990315      *
127000990312     c                   clear                   Wendpag
127100990312     c                   clear                   Wcapagina
127200990420     c                   z-add     NUMcacar      NRR1
127300980526      *
127400980526     c                   SELECT
127500980527      * numero C.A.
127600980622     C                   WHEN      SEL(2) = '1'
127700990315     C                   exsr      RIESEL2
127800980527      * spedizione
127900980622     C                   WHEN      SEL(3) = '1'
128000980622     C                   exsr      RIESEL3
128100041110      * numero e data assegno
128200041026     C                   WHEN      SEL(14) = '1'
128300041026     C                   exsr      RIESEL14
128400990316      * mittente
128500990316     C                   WHEN      SEL(12) = '1'
128600990316     C                   exsr      RIESEL12
128700990316      * destinatario
128800990316     C                   WHEN      SEL(13) = '1'
128900990316     C                   exsr      RIESEL13
129000980608      * numero evento
129100980622     C                   WHEN      SEL(5) = '1'
129200980622     C                   exsr      RIESEL5
129300990316      * numero pratica
129400990316     C                   WHEN      SEL(11) = '1'
129500990316     C                   exsr      RIESEL11
129600980526      * data C.A.
129700980622     C                   WHEN      SEL(6) = '1'
129800980622     C                   exsr      RIESEL6
129900980526      *
130000980526     C                   OTHER
130100980622     C                   exsr      RIESEL
130200980526      *
130300980526     C                   ENDSL
130400980527      *
130500990121     c                   z-add     1             V2Cnrr
130600990420     c                   z-add     NRR1          NUMcacar
130700990121      *
130800980526     c                   ENDSR
130900980527      *****************************************************************
131000980622      * CARICAMENTO SUBFILE * SELEZIONE 2 - numero C. A.
131100980527      *****************************************************************
131200980622     C     RIESEL2       BEGSR
131300980528      *
131400990315      * E' possibile una sola C.A. valida, ROLLUP non valido
131500990315      * Se la CA è annullata e non sono in Apertura messaggio
131600990315     C                   IF        *in23 = *off
131700990315      *
131800980527     c                   z-add     V1Caac        Kaac
131900980527     c                   z-add     V1Cfil        Kfil
132000980527     c                   z-add     V1Cnca        Knca
132100990315     C     kdct          CHAIN     fndct000                           24
132200990115      *
132300990312     c                   SELECT
132400990312      *
132500990315     c                   WHEN      *IN24 = *ON
132600990312      *
132700990312     C                   WHEN      AZN_tipo <> 'A'  and  DCTatb <> *blanks
132800990115     c                   movel     msg(35)       v1cmsg
132900990115     c                   eval      *in28 = *on
133000990312      *
133100990312     C                   WHEN      DCTatb <> *blanks
133200980527      *
133300990312     c                   OTHER
133400980527     C                   exsr      CTR_selez
133500990312      *
133600990312     c                   ENDSL
133700990315      *
133800990315     c                   ENDIF
133900980527      *
134000980527     c                   ENDSR
134100980527      *****************************************************************
134200980622      * CARICAMENTO SUBFILE * SELEZIONE 3 - spedizione
134300980527      *****************************************************************
134400980622     C     RIESEL3       BEGSR
134500980527      *
134600040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
134700040714      * SIA COME MAMMA CHE COME FIGLIA
134800040714      *
134900040714     c                   IF        *IN23 = *OFF
135000040714     c                   clear                   fidn12ds
135100040714     c                   eval      i12aas = V1Caas
135200040714     c                   eval      i12lnp = V1Clnp
135300040714     c                   eval      i12nrs = V1Cnrs
135400040714     c                   eval      i12nsp = V1Cnsp
135500040714     c                   eval      i12fel = 'E'
135600040714      *
135700040714     c                   call      'FIDN12R'
135800040714     c                   parm                    fidn12ds
135900040714      *
136000040714     c                   z-add     *zeros        ii                2 0
136100040714     c                   ENDIF
136200040714      * se trovata almeno una CA
136300040714     c                   if        o12nca > 0
136400040714     c                   dow       ii <  o12nca  and Wendpag <> 'S'
136500040714     c                   add       1             ii
136600040714     c                   movea     skkey(ii)     dskey
136700040714     C                   z-add     dsaac         kaac
136800040714     C                   z-add     dsfil         kfil
136900040714     C                   z-add     dsnca         knca
137000040714     c     kdct          chain     FNDCT000
137100040714     c                   If        %found(fndct01l)
137200040714     C                   exsr      CTR_selez
137300040714      * se raggiunto il mimite massimo di record: esco.
137400040714     C                   if        NRR1 >= w_nrrMAX
137500040714     C                   leave
137600040714     C                   endif
137700040714     C                   endif
137800040714      *
137900040714     c                   enddo
138000040714     c                   endif
138100040714      *
138200980527     c                   ENDSR
138300980527      *****************************************************************
138400980622      * CARICAMENTO SUBFILE * SELEZIONE 5 - numero evento
138500980527      *****************************************************************
138600980622     C     RIESEL5       BEGSR
138700980527      *
138800990315     c                   IF        *IN23 = *OFF
138900990315     C     keve          chain     fndct003                           24
139000990315     c                   ENDIF
139100980527      *
139200990315     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
139300980527     C                   exsr      CTR_selez
139400020222      * se raggiunto il mimite massimo di record: esco.
139500020222     C                   if        NRR1 >= w_nrrMAX
139600020222     C                   leave
139700020222     C                   endif
139800990315     C     keve          reade     fndct003                               24
139900980527     C                   ENDDO
140000980527      *
140100980527     c                   ENDSR
140200980527      *****************************************************************
140300980622      * CARICAMENTO SUBFILE * SELEZIONE 6 - data apertura C.A.
140400980527      *****************************************************************
140500980622     C     RIESEL6       BEGSR
140600980527      *
140700990315     c                   IF        *IN23 = *OFF
140800980527     c                   z-add     W_AAdcad      Kaac
140900990315     c                   z-add     W_MGdcad      Kmgc
141000990315     c                   clear                   Kfil
141100990315     c                   clear                   Knca
141200980527      *
141300050217     C   08kdct4         setll     fndct004
141400050217     C  n08kdct4         setll     fndct024
141500050217     C   08              read      fndct004                               24
141600050217     C  n08              read      fndct024                               24
141700990315     c                   ENDIF
141800980527      *
141900990315     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
142000980527     C                   exsr      CTR_selez
142100980528      * Se supero data limite fine lettura
142200990315     C                   If        wdatac > W_V1Cdcaa
142300990315     C                   eval      *IN24 = *on
142400990315     c                   Else
142500020222      * se raggiunto il mimite massimo di record: esco.
142600020222     C                   if        NRR1 >= w_nrrMAX
142700020222     C                   leave
142800020222     C                   endif
142900050217     C   08              read      fndct004                               24
143000050217     C  n08              read      fndct024                               24
143100990315     c                   Endif
143200980527     C                   ENDDO
143300980527      *
143400980527     c                   ENDSR
143500990323      *****************************************************************
143600990323      * CARICAMENTO SUBFILE * SELEZIONE 11 - numero pratica assicurativa
143700990323      *****************************************************************
143800990323     C     RIESEL11      BEGSR
143900990323      *
144000990323     c                   IF        *IN23 = *OFF
144100990323     C     kpra          chain     fndct010                           24
144200990323     c                   ENDIF
144300990323      *
144400990323     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
144500990323     C                   exsr      CTR_selez
144600020222      * se raggiunto il mimite massimo di record: esco.
144700020222     C                   if        NRR1 >= w_nrrMAX
144800020222     C                   leave
144900020222     C                   endif
145000990323     C     kpra          reade     fndct010                               24
145100990323     C                   ENDDO
145200990323      *
145300990323     c                   ENDSR
145400990323      *****************************************************************
145500990323      * CARICAMENTO SUBFILE * SELEZIONE 12 - mittente
145600990323      *****************************************************************
145700990323     C     RIESEL12      BEGSR
145800990323      *
145900990323     c                   IF        *IN23 = *OFF
146000990324     c                   movel(P)  V1Cmit        W035a
146100990324     c     W035A         setll     tntmdMIT
146200990324     C                   read      tntmdMIT                               24
146300990323     c                   eval      *in25 = *on
146400990324     c                   ENDIF
146500990323      *
146600990324     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'  and
146700990324     c                             %subst(V1Cmit:1:Wlm) = %subst(£TMDrsm:1:Wlm)
146800990323      * leggo le C.A. per numero spedizione
146900990323      * se esco dalla lettura CA x fine pagina ma ho ancora CA per la spedizione
147000990323      * non devo leggere TNTMD e se fanno rollup non devo chainare FNDCT
147100040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
147200040714      * SIA COME MAMMA CHE COME FIGLIA
147300040714      *
147400040714     C                   IF        *IN25 = *ON
147500040714     c                   clear                   fidn12ds
147600040714      *
147700040714     c                   eval      i12aas = £TMDaas
147800040714     c                   eval      i12lnp = £TMDlnp
147900040714     c                   eval      i12nrs = £TMDnrs
148000040714     c                   eval      i12nsp = £TMDnsp
148100040714     c                   eval      i12fel = 'E'
148200040714      *
148300040714     c                   call      'FIDN12R'
148400040714     c                   parm                    fidn12ds
148500040714      *
148600040714     c                   z-add     *zeros        ii                2 0
148700040714     c                   ENDIF
148800040714      * se trovata almeno una CA
148900040714     c                   if        o12nca > 0
149000040714     c                   dow       ii <  o12nca  and Wendpag <> 'S'
149100040714     c                   add       1             ii
149200040714     c                   movea     skkey(ii)     dskey
149300040714     C                   z-add     dsaac         kaac
149400040714     C                   z-add     dsfil         kfil
149500040714     C                   z-add     dsnca         knca
149600040714     c     kdct          chain     FNDCT000
149700040714     c                   If        %found(fndct01l)
149800040714     C                   exsr      CTR_selez
149900040714      * se raggiunto il mimite massimo di record: esco.
150000040714     C                   if        NRR1 >= w_nrrMAX
150100040714     C                   leave
150200040714     C                   endif
150300040714     C                   endif
150400040714      *
150500040714     c                   enddo
150600040714     c                   endif
150700040714      * SE HO LETTO TUTTE LE CA IN SCHIERA ACCENDO IL 25
150800040714      *
150900040714     C                   EVAL      *IN25 = (II = O12NCA)
151000990323      *
151100990323     c                   IF        *IN25 = *ON
151200990324     C                   read      tntmdMIT                               24
151300990323     c                   ENDIF
151400990323      *
151500990323     C                   ENDDO
151600990323      *
151700990323     c                   ENDSR
151800990323      *****************************************************************
151900990323      * CARICAMENTO SUBFILE * SELEZIONE 13 - destinatario
152000990323      *****************************************************************
152100990323     C     RIESEL13      BEGSR
152200990323      *
152300990323     c                   IF        *IN23 = *OFF
152400990324     c                   movel(P)  V1Cdes        W035a
152500990324     c     W035A         setll     tntmdDES
152600990324     C                   read      tntmdDES                               24
152700990323     c                   eval      *in25 = *on
152800990323     c                   ENDIF
152900990323      *
153000990324     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'  and
153100990324     c                             %subst(V1Cdes:1:Wld) = %subst(£TMDrsd:1:Wld)
153200990323      * leggo le C.A. per numero spedizione
153300990323      * se esco dalla lettura CA x fine pagina ma ho ancora CA per la spedizione
153400990323      * non devo leggere TNTMD e se fanno rollup non devo chainare FNDCT
153500040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
153600040714      * SIA COME MAMMA CHE COME FIGLIA
153700040714      *
153800040714     C                   IF        *IN25 = *ON
153900040714     c                   clear                   fidn12ds
154000040714      *
154100040714     c                   eval      i12aas = £TMDaas
154200040714     c                   eval      i12lnp = £TMDlnp
154300040714     c                   eval      i12nrs = £TMDnrs
154400040714     c                   eval      i12nsp = £TMDnsp
154500040714     c                   eval      i12fel = 'E'
154600040714      *
154700040714     c                   call      'FIDN12R'
154800040714     c                   parm                    fidn12ds
154900040714      *
155000040714     c                   z-add     *zeros        ii                2 0
155100040714     c                   ENDIF
155200040714      * se trovata almeno una CA
155300040714     c                   if        o12nca > 0
155400040714     c                   dow       ii <  o12nca  and Wendpag <> 'S'
155500040714     c                   add       1             ii
155600040714     c                   movea     skkey(ii)     dskey
155700040714     C                   z-add     dsaac         kaac
155800040714     C                   z-add     dsfil         kfil
155900040714     C                   z-add     dsnca         knca
156000040714     c     kdct          chain     FNDCT000
156100040714     c                   If        %found(fndct01l)
156200040714     C                   exsr      CTR_selez
156300040714      * se raggiunto il mimite massimo di record: esco.
156400040714     C                   if        NRR1 >= w_nrrMAX
156500040714     C                   leave
156600040714     C                   endif
156700040714     C                   endif
156800040714      *
156900040714     c                   enddo
157000040714     c                   endif
157100040714      * SE HO LETTO TUTTE LE CA IN SCHIERA ACCENDO IL 25
157200040714      *
157300040714     C                   EVAL      *IN25 = (II = O12NCA)
157400990323      *
157500990323     c                   IF        *IN25 = *ON
157600990324     C                   read      tntmdDES                               24
157700990323     c                   ENDIF
157800990323      *
157900990323     C                   ENDDO
158000990323      *
158100990323     c                   ENDSR
158200041026      *****************************************************************
158300041026      * CARICAMENTO SUBFILE * SELEZIONE 14 - data + numero assegno
158400041026      *****************************************************************
158500041026     c     RIESEL14      BEGSR
158600041026      *
158700041026     c                   IF        *IN23 = *OFF
158800041110     c                   if        V1Cdea > *zeros
158900041026     C     Kdcl02        chain     fndcl000                           24
159000041026     c                   else
159100041026     C     Kdcl01        chain     fndcl000                           24
159200041026     C                   endif
159300041026     c                   ENDIF
159400041026      *
159500041026     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
159600041026      *
159700041026     C                   z-add     dclaac        kaac
159800041026     C                   z-add     dclfil        kfil
159900041026     C                   z-add     dclnca        knca
160000041026     c     kdct          chain     FNDCT000                           25
160100150701      *
160200150701      /free
160300150701           // -?Eventuale reperimento del Beneficiario del bonifico?
160400150701           If  V1Cksc <> *zero  or  V1Dksc <> *blank;
160500150701             chain  ( DCLaac : DCLfil : DCLnca : 'P' )  FNDKA000;
160600150701             if  NOT %found(FNDKA01L)  or  DKAatb <> *blank;
160700150701               clear  DKAksc;
160800150701               clear  DKArag;
160900150701             endif;
161000150701           EndIf;
161100150701      /end-free
161200041026      *
161300041026     C                   exsr      CTR_selez
161400041026      * se raggiunto il mimite massimo di record: esco.
161500041026     C                   if        NRR1 >= w_nrrMAX
161600041026     C                   leave
161700041026     C                   endif
161800041026      *
161900041110     c                   if        V1Cdea > *zeros
162000041026     C     Kdcl02        reade     fndcl000                               24
162100041026     c                   else
162200041026     C     Kdcl01        reade     fndcl000                               24
162300041026     C                   endif
162400041026      *
162500041026     C                   ENDDO
162600041026      *
162700041026     c                   ENDSR
162800980527      *****************************************************************
162900980622      * CARICAMENTO SUBFILE * SELEZIONE ? -
163000980527      *****************************************************************
163100980622     C     RIESEL        BEGSR
163200980527      *
163300990315     c                   IF        *IN23 = *OFF
163400050217     C   08*LOVAL        setll     fndct004
163500050217     C  n08*LOVAL        setll     fndct024
163600050217     C   08              read      fndct004                               24
163700050217     C  n08              read      fndct024                               24
163800990315     c                   ENDIF
163900980527      *
164000990315     c                   DOW       *IN24 = *off  and  Wendpag <> 'S'
164100980527     C                   exsr      CTR_selez
164200020222      * se raggiunto il mimite massimo di record: esco.
164300020222     C                   if        NRR1 >= w_nrrMAX
164400020222     C                   leave
164500020222     C                   endif
164600050217     C   08              read      fndct004                               24
164700050217     C  n08              read      fndct024                               24
164800980527     C                   ENDDO
164900980527      *
165000980527     c                   ENDSR
165100980527      *****************************************************************
165200980527      * CONTROLLO SE C.A. SELEZIONABILE PER CARICAMENTO SUBFILE
165300980527      *****************************************************************
165400980527     C     CTR_selez     BEGSR
165500980528      *
165600990215     c                   movel(P)  DCTflo        DDCT01
165700990215     c                   movel(P)  §DCTlnpc      W§DCTlnpc
165800990215      *
165900990215      * Compongo la data creazione evento per confronto
166000990215     C                   eval      wdatac = (DCTaac * 10000) + DCTmgc
166100020821      *
166200020821      * Cerco in L6 il P.O. di competenza se interrogazione o
166300020821      *   manutenzione fase
166400020821     c                   eval      W_POinte = 'N'
166500020821     c                   if        AZN_tipo = 'I' or AZN_tipo = 'M'
166600020821     c                   if        V1Cfgs = 046
166700020821     c                   eval      W_POinte = 'S'
166800020821     c                   else
166900170426
167000020821     c     DCTlna        lookup    L6                                     31
167100170426     c                   if        *in31 and d50val_a='N'
167200170426     c                   setoff                                       31
167300170426     c                   endif
167400170426     c
167500170426     c                   if        not *in31
167600170426     c     w§DCTlnpc     lookup    L6                                     31
167700170426     c                   if        *in31 and d50val_p='N'
167800170426     c                   setoff                                       31
167900170426     c                   endif
168000170426     c                   endif
168100170426
168200020821     c   31              eval      W_POinte = 'S'
168300020821     c                   endif
168400020821     c                   endif
168500980528      *
168600981014      * Cerco in L6 il P.O. gestione fase
168700980622     c                   eval      W_POgest = 'N'
168800170426    1C                   if        DCTgfc > *zeros
168900980528     C     DCTgfc        LOOKUP    L6                                     31
169000170426    2c                   if        *in31 and simfel>0
169100170426     c                   setoff                                       32
169200170426     c     DCTlna        lookup    L6                                     32
169300170426    3c                   if        *in32 and d50val_a='N'
169400170426     c                   setoff                                           32
169500170426    3c                   endif
169600170426     c
169700170523     c                   if        not *in32
169800170523     c     w§DCTlnpc     lookup    L6                                     32
169900170426    3c                   if        *in32 and d50val_p='N'
170000170426     c                   setoff                                       32
170100170426    3c                   endif
170200170523    3c                   endif
170300170426     c
170400170426    3c                   if        not *in32
170500170426     c                   setoff                                       31
170600170426    3c                   endif
170700170426    2c                   endif
170800170426
170900170426     c   31              eval      W_POgest = 'S'
171000170426    1C                   endif
171100980528      *
171200981014      * Cerco in L6 il P.O creazione fase per Apertura
171300980622     c                   eval      W_POcrea = 'N'
171400170426    1C                   if        AZN_tipo = 'A'
171500170426     C     DCTfil        LOOKUP    L6                                     31
171600170426    2c                   if        *in31 and simfel>0
171700170426     c                   setoff                                       32
171800170426     c     DCTlna        lookup    L6                                     32
171900170426    3c                   if        *in32 and d50val_a='N'
172000170426     c                   setoff                                           32
172100170426    3c                   endif
172200170426     c
172300170426     c  n32w§DCTlnpc     lookup    L6                                     32
172400170426    3c                   if        *in32 and d50val_p='N'
172500170426     c                   setoff                                       32
172600170426    3c                   endif
172700170426     c
172800170426    3c                   if        not *in32
172900170426     c                   setoff                                       31
173000170426    3c                   endif
173100170426    2c                   endif
173200170426
173300170426     c   31              eval      W_POcrea = 'S'
173400170426    1C                   endif
173500980608      *
173600980608      * Cerco nel file fasi se selezionata la data esecuzione fase
173700980622     c                   eval      W_esefase = 'N'
173800980622     C                   if        SEL(9) = '1'
173900980608      *
174000980608     c                   z-add     dctaac        kaac
174100980608     c                   z-add     dctfil        kfil
174200980608     c                   z-add     dctnca        knca
174300980608     c     KDCF          SETLL     FNDCF000
174400980608      *
174500980608     c                   DO        *HIVAL
174600980608     c     KDCF          reade     FNDCF000                               33
174700980608     c  N33              if        DCFfca = W_V1Cfca  AND
174800980608     c                             DCFdfc <= W_v1cdefa  AND  DCFdfc >= W_v1cdefd
174900980622     c                   eval      W_esefase = 'S'
175000980608     c                   else
175100980608     c                   eval      *IN33 = (DCFdfc > W_v1cdefa)
175200980608     c                   endif
175300980608     c  N33              ENDDO
175400980608      *
175500980608     C                   endif
175600990215      *
175700990215      * Cerco lnp o lna nella £6 del P.O selezione
175800990215     c                   eval      W_POsele = 'N'
175900990215     C                   IF           SEL(10) = '1'
176000990215      *
176100990215     c                   If         V1Cpot = *blanks  or  V1Cpot = 'A'
176200990215     C     DCTlna        LOOKUP    WL6                                    32
176300990215     c   32              eval      W_POsele = 'S'
176400990215     C                   Endif
176500990215      *
176600990215     c                   If         V1Cpot = *blanks  or  V1Cpot = 'P'
176700990215     C     W§DCTlnpc     lookup    WL6                                    32
176800990215     c   32              eval      W_POsele = 'S'
176900990215     C                   Endif
177000990215      *
177100990215     C                   ENDIF
177200990215
177300980528      *  -------------------------------------------------
177400980528      *  C O N T R O L L I   E X T R A   S E L E Z I O N E
177500990215      *  -------------------------------------------------
177600990115      * - Elaboro le annullate solo se il tipo azione è "A"
177700980710      * - Se il tipo azione è "G" non carico C.A. chiuse
177800980710      * - Se il tipo azione è "G" la filiale gestione fase deve essere in £6
177900020821      * - Se il tipo azione è "I" o "M" DCTLNA o §DCTLNPC devono essere in £6
178000980803      * - Se il tipo azione è "A" la manutenzione è consentita al P.O. creazione se CA non trasmessa
178100980528      *  -------------------------------------------------
178200990215
178300980527     c                   SELECT
178400980528      *
178500980608     C                   WHEN      AZN_tipo <> 'A'  AND  DCTatb <> *blanks        ANNULLATE
178600980528      *
178700980908     C                   WHEN      AZN_tipo = 'G'  AND  DCTdch <> *zeros          C.A. CHIUSA
178800980710      *
178900980710     C                   WHEN      AZN_tipo = 'G'  AND  W_POgest = 'N'            FASE GESTIBILE
179000020821      *
179100020821     C                   WHEN      (AZN_tipo = 'I'  or  AZN_tipo = 'M')           Fase gestibile
179200020821     C                             and W_POinte = 'N'
179300990507      *
179400120822     C********           WHEN      AZN_tipo = 'I'  AND  DCTfca < §STDfrc          RICERCA DA FASE nn
179500120822     c*********                                    AND  *IN16 = *ON               PER CONSULDANNI
179600980528      *
179700980528     C                   WHEN      AZN_tipo = 'A'  AND
179800980622     c                              (W_POcrea = 'N' OR  (W_POcrea = 'S'           MAN. P.O. APERTURA
179900050217     C**!!!                          AND  (DCTDT1 > 0)) )                         SE NON TRASMESSA
180000050217     c                               and  (dateu > wdatac)) )                     SE NON APERTA OGGI
180100050217
180200050217     c                   WHEN      Not *In08 and dctdt2 > *zeros                  C.A. X FILIALE
180300990507      *
180400990507     C                   WHEN      SEL(6) = '1'  AND                              DATA C.A.
180500990507     C                              (Wdatac < W_V1Cdcad  OR  Wdatac > W_V1Cdcaa)
180600990507      *
180700990507     C                   WHEN      SEL(8) = '1'  AND  SEL(9) <> '1'  AND          FASE C.A. SENZA
180800990507     C                              AZN_tipo <> 'G'  AND  DCTfca <> W_V1Cfca       DATA C.A.
180900990507      *
181000990507     C                   WHEN      SEL(8) = '1'  AND  SEL(9) <> '1'  AND          FASE C.A. SENZA
181100990507     C                              AZN_tipo = 'G'  AND  DCTfca >= W_V1Cfca        DATA C.A. x GEST.
181200990507      *
181300990507     C                   WHEN      SEL(9) = '1'  AND                              DATA ELABOR. C.A.
181400990507     C                              W_esefase = 'N'
181500990507      *
181600990507     C                   WHEN      SEL(5) = '1'  AND                              NUMERO EVENTO
181700990507     C                              (V1Cnev <> DCTnev  OR  V1Caae <> DCTaae)
181800990507      *
181900990507     C                   WHEN      SEL(11) = '1'  AND                             NUMERO PRATICA ASS
182000990507     C                              (V1Cdit <> DCTdit  OR  V1Cpra <> DCTpra
182100990507     C                                                 OR  V1Cprn <> DCTprn)
182200990507      *
182300990507     C                   WHEN      SEL(10) = '1'  AND                             P.O. Part. Arrivo
182400990507     c                              W_POsele = 'N'
182500041026      *
182600041026     C                   WHEN      SEL(14) <> '1'  AND                            MITTEN./DESTINAT.
182700041026     C                             SEL(12) = '1'  AND  SEL(13) = '1'  AND         MITTEN./DESTINAT.
182800990507     c                             %subst(V1Cdes:1:Wld) <> %subst(£TMDrsd:1:Wld)
182900150701      *
183000150701     c                   WHEN      Sel(14) = '1'   and  (                         BENEFICIARIO
183100150701     c                             (V1Cksc <> *zeros  and  V1Cksc <> DKAksc) or   (codice o
183200150701     c                             (V1Dksc <> *blanks and  %trimR(V1Dksc) <>       ragione sociale)
183300150701     c                               %subst(DKArag:1:%len(%trimR(V1Dksc)))) )
183400980527      *
183500980622     C                   WHEN      SEL(4) = '1'  AND                              CODICE ANOMALIA
183600980527     C                              V1Ctad <> DCTtad
183700980527      *
183800980622     C                   WHEN      SEL(7) = '1'  AND                              LIQ.TRA./PRAT.ASS.
183900980527     C                              V1Cfpr <> DCTfpr
184000080718      *
184100080718     C                   WHEN      SEL(15)= '1'  AND                              C.A.APERTE/CHIUSE
184200080718     C                              ((V1Cace = 'A' and dctcch <> ' ')
184300080718     C                                           OR
184400080718     C                               (V1Cace = 'C' and dctcch =  ' '))
184500990323      *
184600980527     c                   OTHER
184700980527     C                   exsr      RICsbfl
184800980527      *
184900980527     C                   ENDSL
185000980527      *
185100980527     c                   ENDSR
185200980527      **********************************************************************
185300980528      * SCRITTURA RCD SBFL
185400980527      **********************************************************************
185500980528     C     RICsbfl       BEGSR
185600980527      *
185700981008     c                   clear                   wi05fca
185800981008     c                   clear                   w0010
185900980529      *
186000980529     c                   DO        *HIVAL
186100980608      *
186200980610     c                   eval      *IN81 = *OFF
186300980623     c                   eval      *IN82 = *OFF
186400980610     c                   eval      *IN32 = *ON
186500980608     c                   clear                   FIDN05DS
186600980721
186700980608      * Interrogazione o Apertura controllo la fase attuale, Gestione ricerco la fase successiva
186800980608     c                   IF        AZN_tipo <> 'G'
186900980608     c                   movel     'C'           I05MOD
187000980608     c                   ELSE
187100980608      *
187200980605     c                   movel     'F'           I05MOD
187300980529     c                   movel     DCTfpr        I05fpr
187400980529     c                   movel     DCTptr        I05tpc
187500981019     c                   if        DCTnev > *ZEROS
187600980608     c                   movel     'E'           I05fpe
187700980608     c                   else
187800980608     c                   movel     'N'           I05fpe
187900980608     c                   endif
188000981030     c                   if        DCTnde > *ZEROS
188100981030     c                   movel     'D'           I05fde
188200981102     c                   else
188300981102     c                   movel     'N'           I05fde
188400981030     c                   endif
188500980529     c                   ENDIF
188600980529      *
188700980601      * Se esiste fase memorizzata utilizzo quella per la ricerca, altrimenti quella del file.
188800980529     c                   IF        WI05fca > *zeros
188900980529     c                   z-add     wi05fca       I05fca
189000980529     c                   ELSE
189100980529     c                   z-add     DCTfca        I05fca
189200980721      * per fase esistente recupero la data di esecuzione fase
189300990420     c                   z-add     DCTaac        Kaac
189400980721     c                   z-add     DCTfil        Kfil
189500980721     c                   z-add     DCTnca        Knca
189600980721     c     kdcf          setgt     fndcf01l
189700980721     c     kdcf          readpe    fndcf01l                               30
189800980721     c                   if        *in30 = *off and dcffca = dctfca
189900980721     c                   z-add     DCFdfc        i05dta
190000980721     c                   endif
190100980529     c                   ENDIF
190200980721      * valorizzo il tipo anomalia CA
190300980721     c                   move      DCTtad        i05tad
190400981125      * valorizzo il numero C.A.
190500981125     c                   z-add     dctaac        i05aac
190600981125     c                   z-add     dctfil        i05fil
190700981125     c                   z-add     dctnca        i05nca
190800980529      *
190900980529     C                   CALL      'FIDN05R'
191000980604     c                   PARM                    KPJBA
191100980529     c                   PARM                    FIDN05DS
191200980529      *
191300980617      * Errore ricerca vado a fine
191400980529     c                   IF        O05err <> 'E'
191500980529     c                   movel     O05REC        DS_FNDFA
191600980601     c                   ELSE
191700980605     c                   iter
191800980529     C                   ENDIF
191900980721
192000980608      * Per Gestione effettuo controlli sulla fase ricavata
192100980608     c                   IF        AZN_tipo = 'G'
192200981008      * A)  Elaboro la lettera reso avaria e il suo sollecito solo se la merce danneggiata è resa
192300980721      *      Non carico il subfile e ripeto la ricerca.
192400980721     c                   if        DCTdsd <> 'R'  and
192500981008     c                             (DFAfca = WFlav  or  DFAfca = WFsla)
192600980608     c                   eval      *IN32 = *OFF
192700980805     c                   z-add     DFAfca        WI05fca
192800980721     c                   iter
192900980805     C                   ENDIF
193000980805      * B)  Se non è obbligatoria
193100980805      *      Carico il subfile e ripeto la ricerca.
193200980805     c                   if        DFAobl <> 'S'
193300980805     c                   eval      *IN32 = *OFF
193400980805     c                   z-add     DFAfca        WI05fca
193500980805     C                   ENDIF
193600980805      * C)  Se non è stata selezionata una fase e non è utilizzabile in gestione.
193700980805      *      Non carico il subfile e ripeto la ricerca.
193800980805     c                   IF        SEL(8) <> '1'  and  DFAfgc = 'N'
193900980805     c                   eval      *IN32 = *OFF
194000980805     c                   z-add     DFAfca        WI05fca
194100980608     c                   iter
194200980805     C                   ENDIF
194300980805      * D)  Se è diversa dall'eventuale fase selezionata.
194400980805      *      Non carico il subfile e ripeto la ricerca se minore e non obbligatoria
194500980805     c                   IF        SEL(8) = '1'  and  DFAfca <> W_V1Cfca
194600980805      *
194700980805     c                   if        DFAfca < W_V1Cfca  and  DFAobl <> 'S'
194800980805     c                   eval      *IN32 = *OFF
194900980805     c                   z-add     DFAfca        WI05fca
195000980805     C                   else
195100980805     c                   eval      *IN32 = *ON
195200980805     C                   endif
195300980805      *
195400980805     c                   iter
195500980805     C                   ENDIF
195600980805      * E)  Se è uguale all'eventuale fase selezionata
195700980805      *      Carico il subfile e non ripeto la ricerca.
195800980622     c                   IF        SEL(8) = '1'  and  W_V1Cfca = DFAfca
195900980608     c                   eval      *IN32 = *ON
196000980805     C                   ENDIF
196100981126      * F)  Se la fase ricavata non è eseguibile proteggo il campo scelta tranne se si tratta della
196200981126      *      conferma C.A. senza entrata diretta (scelte 1,2,3)
196300981126     c                   SELECT
196400981126     c                   WHEN      O05ese <> 'N'
196500981126     c                   WHEN      DFAfca = WFcon  AND
196600020821     c                             (SEL(2) <> *blanks  or  SEL(3) <> *blanks)
196700981126     c                   OTHER
196800981126     c                   eval      *IN82 = *ON
196900981126     c                   ENDSL
197000981126      * G)  Se la fase ricavata è gestita bloccata
197100981027      *      Visualizzazione richiesta:     proteggo il campo della scelta
197200981027      *      Visualizzazione non richiesta: non carico il subfile e ripeto la ricerca solo se è
197300981027      *                                     facoltativa
197400980925     C                   IF        DFAfgc = 'B'
197500980925      *
197600980925     C                   IF        V1Cscc = 'S'
197700981126     c                   eval      *IN82 = *ON
197800980925     c                   ELSE
197900981027     c                   eval      *IN32 = (DFAobl = 'S')
198000980925     c                   z-add     DFAfca        WI05fca
198100980925     c                   iter
198200980925     c                   ENDIF
198300980925      *
198400980925     c                   ENDIF
198500020128      *
198600120822      * H)  Se l'utente è CONSULDANNI  (Non esiste + )
198700020128      *     &  la fase è 700=Progetto di liquidazione
198800020128      *        con il flag di franchigia = 'F'
198900020128      *      Non carico il subfile e ripeto la ricerca.
199000120822     c****               if        KNMUS    = §STDpco   and
199100120822     c****                         DFAfca   = WFspl     and
199200120822     c****                         §DCTpfra = 'F'
199300120822     c****               eval      *IN32 = *OFF
199400120822     c****               z-add     DFAfca        WI05fca
199500120822     c****               iter
199600120822     c****               endif
199700020128      *
199800020128      * I)  Se l'utente non è né CONSULDANNI né EDPxx
199900020128      *     &  la fase è 700=Progetto di liquidazione
200000020128      *        con il flag di franchigia <> 'F'
200100020128      *      Non carico il subfile e ripeto la ricerca.
200200050208      * =>  NON PIU'!!!   Ora si carica !
200300050208      *     (vedi progetto 560 - Danni pagamenti Bartolini)
200400050208     ***c                   if        KNMUS             <> §STDpco   and
200500050208     ***c                             %subst(KNMUS:1:3) <> 'EDP'     and
200600050208     ***c                             AZN_tipo          =  'G'       and
200700050208     ***c                             *in13             =  *on       and
200800050208     ***c                             §DCTpfra          <> 'F'       and
200900050208     ***c                             W_V1Cfca          =  WFspl
201000050208     ***c                   eval      *IN32 = *OFF
201100050208     ***c                   z-add     DFAfca        WI05fca
201200050208     ***c                   iter
201300050208     ***c                   endif
201400080603      *
201500080603      * I)  Se la fase deve essere dalla sede e il po gestione non è la sede
201600080603      *     passo oltre
201700080603      *
201800080603     c                   if        dfagfs = 'S' and dctgfc <> 46
201900080603     c                   iter
202000080603     c                   endif
202100980721      *
202200980721      * Fine controlli Gestione
202300980721     C                   ENDIF
202400980529      *
202500980608      * Carico
202600980622     c                   movel     *blanks       V2Csce
202700980623     c                   movel     *blanks       V2Hi81
202800980623     c                   movel     *blanks       V2Hi82
202900160226     c                   movel     *blanks       V2Hi86
203000980721     c   82              movel     '1'           V2Hi82
203100980702     c                   movel     W_pogest      V2Hges
203200980908     c                   eval      V2Hchi = (DCTdch > 0)
203300980731     c                   movel     DFAcog        V2Hcog
203400980529     c                   z-add     DCTaac        V2Haac
203500980529     c                   z-add     DCTaas        V2Haas
203600980604     c                   z-add     DCTlna        V2Hlna
203700980610     c                   movel     DFApgm        V2Hpgm
203800980529     c                   z-add     DCTfil        V2Cfil
203900980601     c                   z-add     DCTnca        V2Cnca
204000980529     c                   z-add     DCTaac        V2Cdca
204100980529     c                   movel     DCTmgc        V2Cdca
204200980605     c     *mdy          move      V2Cdca        DATA_eur
204300980605     c     *dmy          move      DATA_eur      V2Cdca
204400980529     c                   move      DCTaas        V2caas2
204500980529     c                   z-add     DCTlnp        V2Clnp
204600980529     c                   z-add     DCTnrs        V2Cnrs
204700980529     c                   z-add     DCTnsp        V2Cnsp
204800981204     c                   movel     DCTtad        W_ctad
204900980615     c                   z-add     DFAfca        V2Cfca                         Calcolata
205000980615     c                   z-add     DCTfca        V2Hfca                         Attuale
205100990312     c                   movel     DCTtad        V2Ctad
205200981204     c                   z-add     DCTnev        V2Cnev
205300981204     c                   z-add     DCTaae        V2Caae
205400990312     c                   z-add     DCTpra        V2Cpra
205500990312     c                   z-add     DCTprn        V2Cprn
205600990312     c                   IF        DCTprn > *zeros
205700990312     c                   movel     DCTdit        V2Cdit
205800990312     c                   ELSE
205900990312     c                   clear                   V2Cdit
206000990312     c                   ENDIF
206100040908      *
206200040908     c                   exsr      IMPtpb
206300981204      *
206400981204      * Ricavo ragione sociale beneficiario se cambia C.A.
206500981204     c                   movel     DSnumca       Wdsnumca
206600981204     c                   IF        Wdsnumca <> Mdsnumca
206700981204     c                   exsr      ragsocben
206800981204     c                   z-add     Wdsnumca      Mdsnumca
206900981204     c                   ENDIF
207000981204     c                   movel     W_v2crsb      V2Crsb
207100981204     c                   movel     W_v2htpb      V2Htpb
207200980610      *
207300981204      * Descrizione fase
207400981009     c                   IF        DCTatb <> *blanks
207500981009     c                   movel(P)  Kannullata    V2Dfca
207600981009     c                   ELSE
207700980608     c                   IF        AZN_tipo = 'G'  and  DFADGC <> *blanks
207800980608     c                   movel     DFAdgc        V2Dfca
207900980608     c                   ELSE
208000980608     c                   movel     DFAdes        V2Dfca
208100980608     c                   ENDIF
208200981009     c                   ENDIF
208300980529      *
208400980610      * Gestione di più fasi per C.A.
208500980610     c                   IF        AZN_tipo = 'G'
208600980610     c                   eval      *IN81 = (W0010 > 0)
208700980623     c   81              eval      V2HI81 = '1'
208800980610     c                   eval      w0010 = 1
208900990505     c                   ELSE
209000990505     c                   add       1             Wcapagina
209100980610     c                   ENDIF
209200981209      * costante evento
209300981209     c                   eval      *IN84 = (V2Cnev = *zeros)
209400990312      * costante pratica
209500990312     c                   eval      *IN85 = (V2Cprn = *zeros)
209600160226      *?Beneficiario discordante?
209700160226      *?(Fase da eseguire = 230 e?
209800160226      *?(beneficiario Mittente     su spedizioni in Porto Assegnato o?
209900160226      * ?beneficiario Destinatario su spedizioni in Porto Franco))?
210000160226     c                   movel     DCTflo        dDCT01
210100160226     c                   eval      *in86 = (V1Cfca  = %editc(WFecc:'X') and
210200160226     c                                      *in13   = *on               and
210300160226     c                                     ((§DCTport = 'A' and DCTptr = 'M')
210400160226     c                                       or
210500160226     c                                      (§DCTport = 'F' and DCTptr = 'D')))
210600160226     c   86              movel     *on           V2Hi86
210700981209      * Posizione cursore
210800981104     c                   eval      *IN83 = (*IN82 = *OFF  and  *IN83 = *OFF)
210900981104      *
211000981008     c  N82              add       1             Numcaval
211100990414     c                   add       1             Numcacar
211200981008     c                   add       1             NRR1
211300990315      *
211400990315      * posizione cursore pagina nuova per ROLLUP
211500990315      * carico 16 rcd per volta tranne se gestione
211600990315     c                   SELECT
211700990315     c                   WHEN      AZN_tipo = 'G'
211800990315     c                   WHEN      Wcapagina = 1
211900990315     c                   eval      nrr23 = nrr1
212000990315     c                   WHEN      Wcapagina = 16
212100990312     c                   eval      Wendpag = 'S'
212200990315     c                   ENDSL
212300990312      *
212400980529     c                   write     fi00s02
212500020222      *
212600020222      * Se ho raggiunto il limite di record caricabili (9999)
212700020222      *    emetto il subfile senza completare la lettura
212800020222      *    (pió d'axé an spöl brisa !!!)
212900020222     C                   if        NRR1 >= w_nrrMAX
213000020222     c                   leave
213100020222     C                   endif
213200980529      *
213300980604     c  N32              ENDDO
213400980529      *
213500980528     c     Ericsbfl      ENDSR
213600981204      **********************************************************************
213700990908      * IMPOSTO TIPO BOLLA  Partenza/Arrivo/Storichino/Sede
213800981204      **********************************************************************
213900981204     C     IMPtpb        BEGSR
214000981204      *
214100981204     c                   clear                   W_V2Htpb
214200981204      *
214300981204     c                   IF        SIMFEL = *ZEROS
214400981204     c                   movel     'S'           W_V2Htpb
214500981204     c                   ELSE
214600981204      *
214700040715      * verifico se CA con legami se LNA CA presente in L6 del P.O. gestione
214800040715      *
214900040715     c                   if        §dctlega = 'S'
215000170426     c***  dctlna        lookup    L6                                     17
215100170426     c
215200170426     c                   if        %lookup(dctlna:l6)>0 and d50val_a='S'
215300170426     c*****              If        %found
215400040715      * aggancio FNDCK per il recupero della spedizione figlia
215500040715     c     Kdct          chain     Fndck01l
215600040715     c                   If        %found(Fndck01l)
215700040715      * imposto il tipo bolla uguale ad "A" arrivo
215800040715     c                   movel     'A'           W_V2Htpb
215900040715     c                   eval      V2Haas = DCKaas
216000040715     c                   move      DCKaas        V2caas2
216100040715     c                   eval      V2Clnp = DCKlnp
216200040715     c                   eval      V2Cnrs = DCKnrs
216300040715     c                   eval      V2Cnsp = DCKnsp
216400040715      *
216500040715     c                   z-add     V2Haas        Kaas
216600040715     c                   z-add     V2Clnp        Klnp
216700040715     c                   z-add     V2Cnrs        Knrs
216800040715     c                   z-add     V2Cnsp        Knsp
216900040715      * aggancio
217000040715     c     kspe          chain     fnarb000                           30
217100040715      * se non trovato nelle bolle arrivi vado nelle bolle partenze
217200040715     c                   if        *in30 = *on
217300040715     c     kspe          chain     fnblp000                           30
217400040715      *
217500040715     c                   if        *IN30 = *OFF
217600040715     c                   movel     'P'           W_V2Htpb
217700040715     c                   else
217800040715     c                   clear                   W_v2htpb
217900040716      * Ripristino con il vecchio numero di spedizione
218000040716     c                   eval      V2Haas = DCTaas
218100040716     c                   move      DCTaas        V2caas2
218200040716     c                   eval      V2Clnp = DCTlnp
218300040716     c                   eval      V2Cnrs = DCTnrs
218400040716     c                   eval      V2Cnsp = DCTnsp
218500040715     c                   endif
218600040715      *
218700040715     c                   endif
218800040715      *
218900040715     c                   endif
219000040715     c                   endif
219100040715     c                   ENDIF
219200040715      * SE TIPO BOLLA NON ATTRIBUITO
219300040715     c                   IF        W_V2Htpb = ' '
219400040715     c                   z-add     V2Haas        Kaas
219500981204     c                   z-add     V2Clnp        Klnp
219600981204     c                   z-add     V2Cnrs        Knrs
219700981204     c                   z-add     V2Cnsp        Knsp
219800981204      *
219900981204     c     kspe          chain     fnblp000                           30
220000981204      *
220100981204     c                   if        *IN30 = *OFF
220200981204     c                   movel     'P'           W_V2Htpb
220300981204     C                   else
220400981204     c     kspe          chain     fnarb000                           30
220500981204     c  N30              movel     'A'           W_V2Htpb
220600040715     c                   endif
220700040715      *
220800981204     c                   endif
220900981204      *
221000981204     c                   ENDIF
221100981204      *
221200981204     C                   ENDSR
221300981204      **********************************************************************
221400981204      * RECUPERO LA RAGIONE SOCIALE BENEFICIARIO
221500981204      **********************************************************************
221600981204     C     RAGSOCBEN     BEGSR
221700981204      *
221800981204     c                   clear                   W_V2Crsb
221900990208     c                   MOVE      DCTksc        w0040
222000990420      *
222100990420      * Imposto fase CA, se CA chiusa prendo l'ultima valida
222200990420     c                   IF        DCTfca <> WFcch
222300990420     c                   z-add     DCTFCA        WDCTfca
222400990420     c                   ELSE
222500990420     c                   z-add     DCTaac        Kaac
222600990420     c                   z-add     DCTfil        Kfil
222700990420     c                   z-add     DCTnca        Knca
222800990420     c     kdcf          setgt     fndcf01l
222900990420     c     kdcf          readpe    fndcf01l                               30
223000990420     c                   DOW       *in30 = *off
223100990420     c                   if        DCFfca <> DCTfca
223200990420     c                   z-add     DCFfca        WDCTfca
223300990420     c                   eval      *in30 = *on
223400990420     c                   else
223500990420     c     kdcf          readpe    fndcf01l                               30
223600990420     c                   endif
223700990420     c                   ENDDO
223800990420     c                   ENDIF
223900990420      *
224000990208      * Se ho stampato il progetto di liquidazione uso l'intestatario
224100990420     c                   IF        Wdctfca >= WFspl
224200990208     c                   movel     'I'           wtrc
224300990208     c     Kdka          chain     FNDKA000                           30
224400990909     c                   if        *in30 = *off  and  DKAatb = *blanks
224500990909     c                   movel     DKArag        W_V2Crsb
224600990909     c                   endif
224700990208     c                   ELSE
224800081215      * altrimanti cerco il beneficiario con il programma esterno
224900081215     c                   clear                   fidnbeds
225000081215      *
225100081215     c                   movel     'B'           IBEmod
225200081215     c                   z-add     dctaac        IBEaac
225300081215     c                   z-add     dctfil        IBEfil
225400081215     c                   z-add     dctnca        IBEnca
225500081215     c                   z-add     dctaas        IBEaas
225600081215     c                   z-add     dctlnp        IBElnp
225700081215     c                   z-add     dctnrs        IBEnrs
225800081215     c                   z-add     dctnsp        IBEnsp
225900081215     c                   z-add     dctksc        IBEksc
226000081215     c                   movel     dctptr        IBEptr
226100081215     c                   movel     dctflo        IBEflo
226200081215     c                   movel     W_V2HTPB      IBEtpb
226300081218     c                   If        %subst(knsif:7:1) = 'P'
226400081215     c                   eval      IBEsif = 'P'
226500081218     c                   EndIf
226600081215      *
226700081215     c                   call      'FIDNBER'
226800081215     c                   parm                    fidnbeds
226900081215      *
227000081215     c                   If        oberag <> *blanks  and dctptr <> ' '
227100081215     c                   movel     oberag        W_V2Crsb
227200081215     c                   endif
227300990208      *
227400990208     c                   ENDIF
227500981204      *
227600981204     c                   ENDSR
227700980601      **********************************************************************
227800980601      * EMISSIONE VIDEO 2
227900980601      **********************************************************************
228000980601     C     EMISD02       BEGSR
228100980601      *
228200990121     c                   SELECT
228300990315     c                   WHEN      NRR3 > *zeros
228400990315     c                   z-add     nrr3          V2Cnrr
228500990315     c                   WHEN      *IN23 = *ON  and NRR23 > *zeros
228600990315     c                   z-add     nrr23         V2Cnrr
228700990315     c                   WHEN      *IN90 = *ON  and NRR1 > *zeros
228800990315     c                   z-add     nrr1          V2Cnrr
228900990121     c                   OTHER
229000980610     c                   z-add     1             V2Cnrr
229100990121     c                   ENDSL
229200990121      *
229300980604     c                   seton                                        2021
229400980604      *
229500980601     c                   write     Fi00Z02
229600980601     c                   exfmt     Fi00C02
229700980608      *
229800980624     c                   setoff                                       2890
229900980601     c                   clear                   v2cmsg
230000990121     c                   clear                   nrr3
230100980601      *
230200980601     C                   ENDSR
230300981008      **********************************************************************
230400981008      * CONTROLLI VIDEO 2 SE PREMUTO KA
230500981008      **********************************************************************
230600981008     C     CTRD02KA      BEGSR
230700981008      *
230800981008     c                   clear                   NUMCAESC
230900981008     c                   clear                   W_GIACE
231000981211      *
231100981211     c                   z-add     1             nrr2
231200981211     c     NRR2          CHAIN     FI00S02                            32
231300981211      *
231400981211     c     *IN32         doweq     *OFF
231500981008      *
231600981027      * ERRORE : per scelte diverse dall'esclusione o da blanks,
231700981027      *          le C.A. senza n° spedizione devono obbligatoriamente essere escluse
231800981008      *
231900981027     c                   SELECT
232000981211      *
232100981211     C                   WHEN      V2Cnsp = *zeros  and  V2Csce <> 'E'
232200981211     c                   movel     MSG(34)       V2Cmsg
232300981211     c                   seton                                        289083
232400981211     c                   eval      *IN32 = *ON
232500981211      *
232600981211     C                   WHEN      V2Csce = *blanks
232700981027      *
232800981027     c                   WHEN      V2Csce = 'E'
232900981008     c                   add       1             numcaesc
233000981008      *
233100981211     C                   OTHER
233200981008     c                   movel     MSG(28)       V2Cmsg
233300981008     c                   seton                                        289083
233400981008     c                   eval      *IN32 = *ON
233500981027      *
233600981027     c                   ENDSL
233700981211      *
233800981211     c                   If        *IN32 = *OFF
233900981211     c                   add       1             nrr2
234000981211     c     NRR2          CHAIN     FI00S02                            32
234100981211     c                   EndIf
234200981008      *
234300981008     c                   ENDDO
234400981008      *
234500981008      * ERRORE: se tutte le C.A. risultano escluse
234600981008     c                   IF        NUMCAESC = NUMCAVAL
234700981008     c                   movel     MSG(29)       V2Cmsg
234800981008     c                   seton                                        289083
234900981008     c                   ENDIF
235000981008      *
235100981027      * Se ci sono errori riemetto il SUBF altrimenti richiamo il PGM di Gestione e ricarico il SUBF
235200981027     c                   IF        *IN90 = *OFF
235300981027     c                   exsr      SUB_SELD02
235400981027     c                   eval      *IN91 = *ON
235500981027     c                   ENDIF
235600981008      *
235700981008     C                   ENDSR
235800980601      **********************************************************************
235900980601      * CONTROLLI VIDEO 2
236000980601      **********************************************************************
236100980601     C     Ctrd02        BEGSR
236200980601      *
236300980623     c                   z-add     1             nrr2
236400980702     c                   clear                   W_GIACE
236500981008      *
236600981008      * Se premuto F1 o F6 imposto campo conferma
236700981008     c                   IF        *INKF = *ON  or  *INKA = *ON
236800981008     C                   eval      W_INKF = '1'
236900981008     c                   ELSE
237000981008     C                   eval      W_INKF = '0'
237100981008     c                   ENDIF
237200980623      *
237300980623     c     NRR2          CHAIN     FI00S02                            32
237400980601      *
237500980604     c     *IN32         doweq     *OFF
237600980624     c     *IN90         andeq     *OFF
237700980622      *
237800981008      * Forzo scelta 'G': se scelta blank non bloccata ed è stato premuto F1
237900981008     c                   IF        V2Csce = *blanks  and  *INKA = *ON
238000981008     c                             and  V2HI82 <> '1'
238100981008     c                   eval      V2Csce = 'G'
238200981008     c                   ENDIF
238300981008      *
238400980622     c     V2Csce        LOOKUP    OPZ                                    31
238500981008
238600981008
238700980622     c                   SELECT
238800981008
238900980623     c                   WHEN      *IN31 = *OFF                                 OPZ non gestita
239000980623     c                   movel     MSG(22)       V2Cmsg
239100980624     c                   seton                                        289083
239200981008
239300981009     c                   WHEN      V2Csce = 'E'  and  *INKA = *OFF              OPZ non consentita
239400981009     c                   movel     MSG(30)       V2Cmsg
239500981009     c                   seton                                        289083
239600981009
239700981008     C                   WHEN      V2Csce = *blanks                             nessuna scelta
239800981008
239900981008     C                   WHEN      V2Csce = 'E'                                 esclusa
240000981008     c                   movel     *blanks       V2Csce
240100981008
240200981022     c                   WHEN      V2Csce = '4'                                 NOTE PER STAMPA
240300981022     c                   exsr      SUB_stnote
240400980826     c                   movel     *blanks       V2Csce
240500981008     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
240600990121     c                   z-add     nrr1          nrr3
240700981022
240800981022     c                   WHEN      V2Csce = '8'                                 NOTE
240900981022     c                   exsr      SUB_note
241000981022     c                   movel     *blanks       V2Csce
241100981022     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
241200990121     c                   z-add     nrr1          nrr3
241300981008
241400981027     c                   WHEN      V2Csce = 'G'  and  V2Cnsp = *zeros           gest. key sped. 0
241500980731     c                   movel     MSG(23)       V2Cmsg
241600980731     c                   seton                                        289083
241700981008
241800981027     c                   WHEN      V2Csce = 'G'  and  V2Hcog = 'S'              Obbligatorio F6
241900981008     c                              and W_INKF = '0'
242000980731     c                   movel     MSG(27)       V2Cmsg
242100980731     c                   seton                                        289083
242200981008
242300981027     c                   WHEN      V2Csce = '2'  and  V2Hges = 'N'              non gestibile
242400980803     c                              and  AZN_tipo <> 'A'                          tranne apertura
242500980702     c                   movel     MSG(24)       V2Cmsg
242600980702     c                   seton                                        289083
242700981008
242800981027     c                   WHEN      V2Csce = '2'  and  V2Hchi = '1'              non gestibile
242900980710     c                   movel     MSG(26)       V2Cmsg
243000980710     c                   seton                                        289083
243100990406
243200990406     c                   WHEN      V2Csce = 'R'  and  V2Hges = 'N'              non gestibile
243300990406     c                   movel     MSG(24)       V2Cmsg
243400990406     c                   seton                                        289083
243500981008
243600980702     c                   WHEN      V2Csce = '9'                                 visualizza sped.
243700980624     c                   exsr      SUB_visbol
243800980624     c                   movel     *blanks       V2Csce
243900980624     c                   eval      *in90 = (WF03 = 'S')                         per F3 torno al SFL
244000990121     c                   z-add     nrr1          nrr3
244100981008
244200980622     c                   OTHER
244300020128
244400020128     c                   eval      $ErrFRANCH   =  *off
244500020128if  3c                   if            AZN_tipo =  'G'
244600020128     c                             and V2Csce   =  'G'
244700020128     c                             and V2Cfca   =  WFspl
244800020128     c                   exsr      CtrFRANCH                                    Controllo Franchigia
244900020128if  4c                   if        $ErrFRANCH   <> *off                         Franchigia errore
245000020128sel 5c                   select
245100020128w   5c                   when      $ErrFRANCH   =  '1'
245200020128     c                   movel     MSG(13)       V2Cmsg
245300050208w   5***c                   when      $ErrFRANCH   =  '2'
245400050208     ***c                   movel     MSG(14)       V2Cmsg
245500020128w   5c                   other
245600020128     c                   movel     *all'? '      V2Cmsg
245700020128e   5c                   endsl
245800020128     c                   seton                                        289083
245900020128e   4c                   endif
246000020128e   3c                   endif
246100020128
246200020128if  3c                   if        $ErrFranch   = *off
246300020128
246400981204     c                   if        V2Csce = 'G'  and  SIMFEL <> *zeros          Controllo Giacenza
246500981008     c                   exsr      CTRGIA
246600980702     c                   endif
246700981008     c                   IF        W_giace = 'S'                                Giacenza errore
246800980702     c                   movel     MSG(25)       V2Cmsg
246900980702     c                   seton                                        289083
247000980702     c                   ELSE
247100981008      * elaboro selezione
247200981211     c                   z-add     1             NUMCAVAL
247300981008     c                   exsr      SUB_SELD02
247400990406      *     torno al subfile se il chiamato ritorna F3 o msg errore che emetto a video
247500990406     c                   eval      *in90 = (O00F03 = 'S'  or  O00ERR = 'E')
247600990406     c                   IF        O00err = 'E'  and  O00msg <> *blanks
247700990406     c                   movel     O00msg        V2Cmsg
247800990406     c                   eval      *in28 = *on
247900990406     c                   ENDIF
248000990406      *
248100990121     c                   z-add     nrr1          nrr3
248200981214      *     se ho modificato o gestito almeno una C.A. ricarico il subfile
248300981214     c  N91              eval      *in91 = (*in90 = *off  and  O00F12 <> 'S'
248400981008     c                                     and (V2Csce = 'G'  or  V2Csce = '2'))
248500981008     c                   movel     *blanks       V2Csce                         azzero scelta
248600980702     c                   ENDIF
248700020128
248800020128e   3c                   endif
248900981008
249000980622     c                   ENDSL
249100981008
249200981008
249300980623     c                   eval      *IN81 = (V2HI81 = '1')
249400980623     c                   eval      *IN82 = (V2HI82 = '1')
249500981209     c                   eval      *IN84 = (V2Cnev = *zeros)
249600160226     c                   eval      *IN86 = (V2HI86 = *on)
249700980623      *
249800980622     c                   update    fi00s02
249900980803     c   83              eval      *IN83 = *OFF
250000980623      *
250100980624     c                   IF        *IN90 = *OFF
250200980623     c                   add       1             nrr2
250300980623     c     NRR2          CHAIN     FI00S02                            32
250400980804     c                   ENDIF
250500980604      *
250600980623     c                   enddo
250700981008      *
250800980601     C                   ENDSR
250900980604      **********************************************************************
251000980624      * VISUALIZZAZIONE SPEDIZIONE
251100980604      **********************************************************************
251200980624     C     SUB_VISBOL    BEGSR
251300980604      *
251400980624     c                   movel     *blanks       WF03
251500980624      *
251600980624     c                   SELECT
251700980624      * partenza
251800161122     c*                  WHEN      V2Htpb = 'P'
251900161122     c*                  clear                   DSVBP_F03
252000161122     C*                  z-add     V2Haas        DSVBP_aas
252100161122     C*                  z-add     V2Clnp        DSVBP_lnp
252200161122     C*                  z-add     V2Cnrs        DSVBP_nrs
252300161122     C*                  z-add     V2Cnsp        DSVBP_nsp
252400161122     c*                  movel(P)  RSUT          DSVBP_rsut
252500161122     C*                  z-add     simfel        DSVBP_sim
252600161122     C*                  movel(P)  DS_visbolp    KPJBU
252700161122     C*                  call      'FNLS05R'
252800161122     C*                  parm                    KPJBA
252900980624      *
253000161122     C*                  movel     KPJBU         DS_visbolp
253100161122     c*                  if        DSVBP_f03 <> *BLANKS
253200161122     C*                  movel     'S'           WF03
253300161122     c*                  endif
253400161122      * filiale
253500161122     c                   WHEN      V2Htpb <> 'S'
253600161122     c                   clear                   paramu6ds1
253700161122     C                   z-add     V2Haas        pa1aas_
253800161122     C                   z-add     V2Clnp        pa1lnp_
253900161122     C                   z-add     V2Cnrs        pa1nrs_
254000161122     C                   z-add     V2Cnsp        pa1nsp_
254100161123     C                   eval      PA1tla_='L'
254200161122     C                   movel(P)  paramu6ds1    KPJBU
254300161122     C                   call      'FNLRU6R1'
254400980624     C                   parm                    KPJBA
254500980624      *
254600161122     C                   movel     KPJBU         paramu6ds1
254700161122     c                   if        pa1f03_   <> *BLANKS
254800980624     C                   movel     'S'           WF03
254900980624     c                   endif
255000980624      * sede
255100981204     c                   WHEN      V2Htpb = 'S'
255200990806     c                   clear                   TNSB50DS
255300990910     C                   MOVEL     'D05'         I50OP0
255400990806     C                   z-add     V2Haas        D50aas
255500990806     C                   z-add     V2Clnp        D50lnp
255600990806     C                   z-add     V2Cnrs        D50nrs
255700990806     C                   z-add     V2Cnsp        D50nsp
255800980624      *
255900990806     C                   MOVEL(P)  TNSB50DS      KPJBU
256000990806     C                   CALL      'TNSB50R'
256100990806     C                   PARM                    KPJBA
256200990806      *
256300990806     C                   movel     KPJBU         TNSB50DS
256400990806     c                   if        O50F03 <> *BLANKS
256500980624     C                   movel     'S'           WF03
256600980624     c                   endif
256700980624      *
256800980624     c                   ENDSL
256900980624      *
257000980624     C                   ENDSR
257100980722      **********************************************************************
257200981022      * VISUALIZZO NOTE PER STAMPA
257300980722      **********************************************************************
257400981022     C     SUB_stnote    BEGSR
257500980722      *
257600981026     C                   clear                   FIDN10DS
257700981026     c     *dmy          move      V2Cdca        DATA_eur
257800981026     c     *eur          move      DATA_eur      i10dta
257900981026     c                   movel     'D'           i10trc
258000981026     c                   movel     'T'           i10std
258100981026     c                   movel     'C'           i10tpd
258200990121     c                   movel     dsnumca       i10nkt
258300981026      *
258400990121     c                   IF        AZN_tipo = 'I'  or
258500990121     c                              (AZN_tipo = 'M'
258600990121     c                                   and  (V2Hges = 'N'  or  V2HCHI = '1'))
258700990121     c                   movel     'I'           i10flm
258800990121     c                   ELSE
258900990121     c                   movel     'V'           i10flm
259000990121     c                   ENDIF
259100981026      *
259200981026     c                   movel(P)  fidn10ds      kpjbu
259300981026     C                   CALL      'FIDN10R'
259400981026     C                   PARM                    kpjba
259500981026      *
259600981026     c                   movel     *blanks       WF03
259700981026     C                   movel     KPJBU         fidn10ds
259800981026     c                   if        o10f03    <> *BLANKS
259900981026     C                   movel     'S'           WF03
260000981026     c                   endif
260100981022      *
260200980722     C                   ENDSR
260300981022      **********************************************************************
260400981022      * VISUALIZZO NOTE
260500981022      **********************************************************************
260600981022     C     SUB_note      BEGSR
260700981022      *
260800981022     C                   clear                   FIDN10DS
260900981022     c     *dmy          move      V2Cdca        DATA_eur
261000981022     c     *eur          move      DATA_eur      i10dta
261100981022     c                   movel     'N'           i10trc
261200981022     c                   movel     'C'           i10tpd
261300981022      *
261400981022     c                   movel     'V'           i10flm
261500981022     c                   movel     dsnumca       i10nkt
261600981022     c                   movel     V2Hfca        i10nks
261700981022      *
261800981022     c                   movel(P)  fidn10ds      kpjbu
261900981022     C                   CALL      'FIDN10R'
262000981022     C                   PARM                    kpjba
262100981022      *
262200981022     c                   movel     *blanks       WF03
262300981022     C                   movel     KPJBU         fidn10ds
262400981022     c                   if        o10f03    <> *BLANKS
262500981022     C                   movel     'S'           WF03
262600981022     c                   endif
262700981022      *
262800981022     C                   ENDSR
262900980702      **********************************************************************
263000980702      * CONTROLLO SE PER LA SPEDIZIONE CI SONO GIACENZE APERTE
263100980702      **********************************************************************
263200980702     C     CTRGIA        BEGSR
263300980702      *
263400040715      *       verifico solo le giacenze in arrivo
263500050331     c     kgiac         chain     Tigcp21l                           30
263600050331     c                   if        *in30 = *off  and gcpfas < 40
263700980702     c                   eval      W_giace = 'S'
263800980702     c                   endif
263900980702      *
264000980702     C                   ENDSR
264100020128      **********************************************************************
264200020128      * CONTROLLO FLAG FRANCHIGIA
264300020128      **********************************************************************
264400020128     C     CtrFRANCH     BEGSR
264500020128      *
264600020128     c                   z-add     V2Haac        Kaac
264700020128     c                   z-add     V2Cfil        Kfil
264800020128     c                   z-add     V2Cnca        Knca
264900020128     c     kdct          chain     FNDCT000
265000020128if  1c                   if        %found(FNDCT01L)
265100020128     c                   movel     DCTflo        DDCT01
265200020128     c                   else
265300020128     c                   clear                   DDCT01
265400020128e   1c                   endif
265500020128      *
265600020128sel 1c                   select
265700020128w   1c                   when      not %found(FNDCT01L)
265800020128     c                             or  DCTatb   = 'A'
265900020128     c                   movel     'N'           $ErrFRANCH
266000120822w   1c****               when          KNMUS    =  §STDpco
266100120822     c****                         and §DCTpfra =  'F'
266200120822     c****               movel     '1'           $ErrFRANCH
266300050208w   1***c                   when          KNMUS             <> §STDpco
266400050208     ***c                             and %subst(KNMUS:1:3) <> 'EDP'
266500050208     ***c                             and §DCTpfra <> 'F'
266600050208     ***c                   movel     '2'           $ErrFRANCH
266700020128e   1c                   endsl
266800020128      *
266900020128     C     EndCtrFran    ENDSR
267000980624      **********************************************************************
267100980624      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER SELEZIONE 2/5/G
267200980624      **********************************************************************
267300980624     C     SUB_SELD02    BEGSR
267400980624      *
267500980604     c                   clear                   FIDN00DS
267600980604      *
267700980610     c                   SELECT
267800980610     c                   WHEN      V2Csce = '2'
267900980611     c                   movel     'M'           I00mod
268000980615     c                   z-add     V2Hfca        I00fca
268100980610     c                   WHEN      V2Csce = '5'
268200980610     c                   movel     'I'           I00mod
268300980615     c                   z-add     V2Hfca        I00fca
268400980610     c                   WHEN      V2Csce = 'G'
268500980610     c                   movel     'G'           I00mod
268600980615     c                   z-add     V2Cfca        I00fca
268700990406     c                   WHEN      V2Csce = 'R'
268800990406     c                   movel     'E'           I00mod
268900990406     c                   z-add     V2Hfca        I00fca
269000981027     c                   WHEN      *INKA = *ON
269100981027     c                   movel     'G'           I00mod
269200981027     c                   z-add     W_V1Cfca      I00fca
269300980610     c                   OTHER
269400980610     c                   movel     AZN_tipo      I00mod
269500980615     c                   z-add     V2Hfca        I00fca
269600980610     c                   ENDSL
269700980610      *
269800980609     c                   z-add     V1Cfgs        I00fgs
269900980604     c                   z-add     V2Haac        I00aac
270000980604     c                   z-add     V2Cfil        I00fil
270100980604     c                   z-add     V2Cnca        I00nca
270200980604     c                   z-add     V2Haas        I00aas
270300980604     c                   z-add     V2Clnp        I00lnp
270400980604     c                   z-add     V2Cnrs        I00nrs
270500980604     c                   z-add     V2Cnsp        I00nsp
270600981027     c                   z-add     V2Hlna        I00lna
270700981027     c                   movel     V2Ctad        I00tad
270800990908     c                   z-add     V2Caae        I00aae
270900990908     c                   z-add     V2Cnev        I00nev
271000990908     c                   z-add     numcaval      I00nce
271100980604      *
271200010709     c                   movel     V2Htpb        I00tpb
271300010709      *
271400981009     c                   movel(P)  FIDN00DS      kpjbu
271500981102
271600981102      *
271700981102      *  RICHIAMO PGM PER ELABORAZIONE SCELTA
271800981102      *
271900981102     c                   SELECT
272000990406      * Manutenzione C.A./Responsabilità
272100990406     C                   WHEN      V2Csce = '2'  or  V2Csce = 'R'
272200981102     c                   call      'FIDN03R'
272300981102     c                   parm                    kpjba
272400981102      * Interrogazione
272500981102     C                   WHEN      V2Csce = '5'
272600981102     c                   call      'FIDN01R'
272700981102     c                   parm                    kpjba
272800981102      * Gestione
272900981102     C                   OTHER
273000980604     c                   call      V2HPGM
273100980604     c                   parm                    kpjba
273200980604      *
273300981102     c                   ENDSL
273400981102
273500980604     c                   movel     kpjbu         FIDN00DS
273600980702      *
273700980702     c                   IF        V2Csce = 'G'  AND
273800980702     c                             O00F12 = *blanks  AND  O00F03 = *BLANKS
273900980702     c                   movel     '1'           V2Hi82
274000980702     c                   ENDIF
274100980604      *
274200980604     C                   ENDSR
274300980609      **********************************************************************
274400980609      * PREPARAZIONE DS PASSAGGIO PARAMETRI PER APERTURA
274500980609      **********************************************************************
274600980609     C     SUB_SELOPN    BEGSR
274700980609      *
274800980609     c                   clear                   FIDN00DS
274900980609      *
275000980609     c                   movel     AZN_tipo      I00mod
275100980609     c                   MOVEL     V1Cfca        I00fca
275200980609     c                   z-add     V1Cfgs        I00fgs
275300980609      *
275400980609     c                   IF        V1Cnsp > 0
275500980609     c                   z-add     V1Caas        I00aas
275600980609     c                   z-add     V1Clnp        I00lnp
275700980609     c                   z-add     V1Cnrs        I00nrs
275800980609     c                   z-add     V1Cnsp        I00nsp
275900980609     c                   z-add     Wlnab         I00lna
276000980609     c                   ENDIF
276100980609      *
276200980609     c                   movel     V1Ctad        I00tad
276300980609     c                   movel     Wtibol        I00tpb
276400980609      *
276500980609      * Aggancio anagrafico fasi per il nome del PGM da chiamare
276600980609     c                   clear                   FIDN05DS
276700980609     c                   movel     'C'           I05MOD
276800980609     c                   movel     V1Cfca        I05fca
276900980609     C                   CALL      'FIDN05R'
277000980609     c                   PARM                    KPJBA
277100980609     c                   PARM                    FIDN05DS
277200980610      *
277300980609     c                   if        O05rec <> *blanks
277400980609     c                   movel     O05REC        DS_FNDFA
277500980609     C                   endif
277600980609      *
277700981009     c                   movel(P)  FIDN00DS      kpjbu
277800980609     c                   call      DFApgm
277900980609     c                   parm                    kpjba
278000980609      *
278100980609     c                   movel     kpjbu         FIDN00DS
278200980609      *
278300980618     c                   if        O00err <> *blanks
278400980618     c                   movel     O00msg        V1CMSG
278500990406     c                   eval      *in28 = *on
278600990406     c                   eval      *in90 = *on
278700980618     C                   endif
278800980618      *
278900980609     C                   ENDSR
279000980528      **********************************************************************
279100980528      * CONTROLLI STANDARD    * linea *
279200980528      **********************************************************************
279300980528     C     CTR_linea     BEGSR
279400980528      *
279500990406     C                   IF        W_linea = *zeros
279600980527     c                   MOVEL     MSG(6)        v1CMSG
279700990406     c                   eval      *in28 = *on
279800990406     c                   eval      *in90 = *on
279900990406     c                   ELSE
280000990406      *
280100980527     c     W_linea       chain     azorg01l                           30
280200980527     c                   if        *IN30 = *on   OR   orgfva <> *blanks
280300980527     c                   MOVEL     MSG(7)        v1CMSG
280400990406     c                   eval      *in28 = *on
280500990406     c                   eval      *in90 = *on
280600980527     c                   endif
280700980527     c                   ENDIF
280800980527      *
280900980527     c                   ENDSR
281000980527      **********************************************************************
281100980527      * CONTROLLI STANDARD    * anno *
281200980527      **********************************************************************
281300980527     C     CTR_anno      BEGSR
281400980527      *
281500980527     c                   IF        W_anno = 0
281600980527     c                   MOVEL     MSG(9)        v1CMSG
281700980527     c                   seton                                        9028
281800980527     C                   else
281900980527      *
282000980527      *   sistemo anno di due cifre
282100980527     C                   if        W_anno < 100  and W_anno > 60
282200980527     C                   ADD       1900          W_anno
282300980527     C                   endif
282400980527     C                   if        W_anno < 100  and W_anno <= 60
282500980527     C                   ADD       2000          W_anno
282600980527     C                   endif
282700980527      *
282800980527     C                   ENDIF
282900980527      *
283000980527     c                   ENDSR
283100980527      **********************************************************************
283200980527      * CONTROLLI STANDARD    * tipo anomalia *
283300980527      **********************************************************************
283400980527     C     CTR_anomal    BEGSR
283500980527      *
283600980601     C                   eval      W_dtad = *BLANKS
283700980601     C                   clear                   tibs02ds
283800980527      *
283900980527      * Ricerca
284000980601     C     '?'           SCAN      W_ctad                                 31
284100980527      *
284200980527    1C                   if        *in31 = *ON
284300980601     C                   SETON                                        5090
284400980601     C                   MOVEL     'R'           t02mod
284500980601     C                   MOVEL     knsif         t02sif
284600980601     C                   MOVEL     'TAD'         t02cod
284700980527      *
284800980601     C                   CALL      'TIBS02R'
284900980601     C                   PARM                    KPJBA
285000980601     C                   PARM                    TIBS02DS
285100980527      *
285200981019     C                   IF        T02err = *BLANKS
285300981019     C                   movel     T02uni        DTAD
285400981019     C                   movel     T02ke1        W_ctad
285500981019     C                   movel     §TADdesc      W_dtad
285600981019      * Se Anomalia per Eventi non è valida
285700981019     C                   IF        §TADragr = 'E'
285800981019     C                   seton                                        502890
285900981019     c                   movel     MSG(31)       W_MSG
286000981019     C                   ENDIF
286100981019      *
286200981019     C                   ELSE
286300980601     c                   eval      *in28 = *on
286400981019     c                   movel     T02msg        W_MSG
286500981019     C                   ENDIF
286600980527      *
286700980601     C                   ELSE
286800980601      *
286900980527      *  Obbligatorio solo per apertura
287000990111     C                   if        W_ctad = *BLANKS  or  W_ctad = '00'
287100990111     c                   clear                   W_ctad
287200980601     C                   if        AZN_tipo = 'A'  and  *INKJ = *ON
287300980601     C                   MOVEL     MSG(1)        W_MSG
287400980527     C                   SETON                                        502890
287500980527     C                   endif
287600980527      *
287700980601     C                   ELSE
287800980601      *
287900980622      * Controllo
288000980601     C                   MOVEL     'C'           t02mod
288100980601     C                   MOVEL     knsif         t02sif
288200980601     C                   MOVEL     'TAD'         t02cod
288300980601     C                   MOVEL     W_ctad        t02ke1
288400980601      *
288500980601     C                   CALL      'TIBS02R'
288600980601     C                   PARM                    KPJBA
288700980601     C                   PARM                    TIBS02DS
288800980601      *
288900981019     C                   IF        t02err = *BLANKS
289000981019     C                   movel     T02uni        DTAD
289100981019     C                   movel     T02ke1        W_ctad
289200981019     C                   movel     §TADdesc      W_dtad
289300981019      * Se Anomalia per Eventi non è valida
289400981019     C                   IF        §TADragr = 'E'
289500981019     C                   seton                                        502890
289600981019     c                   movel     MSG(31)       W_MSG
289700981019     C                   ENDIF
289800981019      *
289900981019     C                   ELSE
290000981019     C                   seton                                        502890
290100980601     c                   movel     t02msg        W_MSG
290200981019     C                   ENDIF
290300980527      *
290400980601     C                   ENDIF
290500980601     C                   ENDIF
290600980601      *
290700980601     c                   ENDSR
290800980615      *****************************************************************
290900990201      * IMPOSTO CAMPI DI VIDEO 1 PREDEFINITI
291000980615      *****************************************************************
291100980615     C     INZD01        BEGSR
291200980615      *
291300980615      *    Se ricevuto test "Liq.Trans/Pratica Ass." preimposto a video e blocco il campo
291400980615     C                   IF        AZN_trpr = 'T'  or  AZN_trpr = 'P'
291500980615     c                   eval      V1Cfpr = AZN_trpr
291600980615     c                   eval      *IN09 = *ON
291700980615     C                   ENDIF
291800980615      *
291900980615      *    Se ricevuto Codice fase valido preimposto a video e blocco il campo
292000980615     C                   IF        AZN_fase <> *blanks
292100980615     C                   testn                   AZN_fase             31
292200980615     C   31              move      AZN_fase      W001A
292300980615     C   31              eval      *in31 = (W001A >= '0')
292400980615      *
292500980615     C                   if        *IN31 = *ON
292600980615      * Controllo
292700980615     c                   clear                   FIDN05DS
292800980615     c                   movel     'C'           I05MOD
292900980615     C                   move      AZN_fase      I05fca
293000980615     C                   CALL      'FIDN05R'
293100980615     C                   PARM                    KPJBA
293200980615     c                   PARM                    FIDN05DS
293300980615     c                   if        O05rec <> *blanks
293400980615     c                   movel     O05REC        DS_FNDFA
293500980615     C                   endif
293600980615     c                   if        O05ERR = 'E' or DFAatb <> *blanks
293700980615     c                   goto      fine
293800980615     c                   else
293900980615     c                   movel     DFAfca        V1Cfca
294000981026     c                   movel     DFAfca        W_V1Cfca
294100980615     c                   if        AZN_tipo = 'G' and DFAdgc <> *blanks
294200980615     c                   movel     DFAdgc        V1Dfca
294300980615     c                   else
294400980615     c                   movel     DFAdes        V1Dfca
294500980615     C                   endif
294600980615     c                   eval      *IN10 = *ON
294700980615     C                   endif
294800980615      *
294900980615     c                   else
295000980615     c                   goto      fine
295100980615     C                   endif
295200980615     C                   ENDIF
295300980615      *
295400980925      *    Se Gestione emetto rich. visualizzazione altrimenti data esecuzione fase
295500980615     c                   EVAL      *IN11 = (AZN_tipo = 'G')
295600980615      *
295700980615      *    Se Gestione o Apertura imposto costanti differenti per cod. fase
295800980615     c                   EVAL      *IN13 = (AZN_tipo = 'G')
295900980615     c  N13              EVAL      *IN13 = (AZN_tipo = 'A')
296000980615      *
296100990324      *    Imposto: ANNO SPEDIZIONE, ANNO C.A., ANNO EVENTO, ANNO PRATICA
296200980615     c                   move      wdtgio        v1caas
296300980615     c                   move      wdtgio        v1caac
296400980615     c                   move      wdtgio        v1caae
296500990324     c                   move      wdtgio        v1cpra
296600980615      *
296700980615     c                   ENDSR
296800990406      *****************************************************************
296900990406      * IMPOSTO OPZIONI
297000990406      *****************************************************************
297100990406     C     CAROPZ        BEGSR
297200990406      *
297300990406     c                   clear                   opz
297400990406      *
297500990406     C                   SELECT
297600990406      *
297700990406     C                   WHEN      AZN_tipo = 'A'
297800990406     c                   movel     DOPZ1_A       V2Dse1
297900990406     c                   movel     DOPZ2_A       V2Dse2
298000990406     c                   movea     OPZ_A         OPZ
298100120822      *   Consuldanni  non esiste +
298200120822     C*****              WHEN      AZN_tipo = 'G'  and  *IN16 = *ON
298300120822     c*****              movel     DOPZ1_CG      V2Dse1
298400120822     c*****              movel     DOPZ2_C       V2Dse2
298500120822     c*****              movea     OPZ_CG        OPZ
298600990406      *
298700990406     C                   WHEN      AZN_tipo = 'G'  and  SEL(8) = '1'
298800990406     c                   movel(P)  DOPZ1_G1      V2Dse1
298900990406     c                   movel(P)  DOPZ2_G1      V2Dse2
299000990406     c                   movea     OPZ_G1        OPZ
299100990406     c                   eval      *IN15 = *ON                                  Abilito F01
299200990406      *
299300990406     C                   WHEN      AZN_tipo = 'G'
299400990406     c                   movel(P)  DOPZ1_G       V2Dse1
299500990406     c                   movel(P)  DOPZ2_G       V2Dse2
299600990406     c                   movea     OPZ_G         OPZ
299700990406     c                   eval      *IN15 = *OFF                                 Abilito F01
299800120822      *   Consuldanni  non esiste +
299900120822     c*****              WHEN      AZN_tipo = 'I'  and  *IN16 = *ON
300000120822     c*****              movel     DOPZ1_CI      V2Dse1
300100120822     c*****              movel     DOPZ2_C       V2Dse2
300200120822     c*****              movea     OPZ_CI        OPZ
300300990506      *
300400990406     C                   WHEN      AZN_tipo = 'I'
300500990406     c                   movel     DOPZ1_I       V2Dse1
300600990406     c                   movel     DOPZ2_I       V2Dse2
300700990406     c                   movea     OPZ_I         OPZ
300800990406      *
300900990915     C                   WHEN      AZN_tipo = 'M'  and  (SIMFEL = *zeros  or
301000130806     c*********                               %SUBST(knmus:1:3) = §STDpv1  or
301100130806     c*********                               %SUBST(knmus:1:3) = §STDpv2)
301200130806     c                                        §UTEmresca = 'S')
301300990406     c                   movel     DOPZ1_M       V2Dse1
301400990406     c                   movel     DOPZ2_M1      V2Dse2
301500990406     c                   movea     OPZ_M1        OPZ
301600990406      *
301700990406     C                   WHEN      AZN_tipo = 'M'
301800990406     c                   movel     DOPZ1_M       V2Dse1
301900990406     c                   movel     DOPZ2_M       V2Dse2
302000990406     c                   movea     OPZ_M         OPZ
302100990406      *
302200990406     C                   ENDSL
302300990406      *
302400990406     c                   ENDSR
302500980527      *****************************************************************
302600980923      * CARICO TABELLE UTILIZZATE IN APERTURA
302700980527      *****************************************************************
302800980923     C     CARTABAPE     BEGSR
302900020821      *
303000980923      * carico schiera dei codici bolla di recupero
303100980923     C                   eval      xx = *zeros
303200980923     C                   MOVE      '3A'          KCOD
303300980923     C     ktab          SETLL     TABEL00F
303400980923     C     ktab          READE     TABEL00F                               32
303500980923     C     *in32         DOWEQ     *OFF
303600980923     c                   movel     tbluni        ds3a
303700980923     c                   IF        §3arbl =  'R'
303800980923     C                   ADD       1             XX
303900980923     C                   MOVEL     TBLKEY        s3a(xx)
304000980923     c                   ENDIF
304100980923     C     ktab          READE     TABEL00F                               32
304200980923     C                   ENDDO
304300980923      *
304400980923     c                   ENDSR
304500980923      *****************************************************************
304600980923      * ROUTINE INIZIALE
304700980923      *****************************************************************
304800980923     C     *INZSR        BEGSR
304900980923      *
305000980521     C     *ENTRY        PLIST
305100980521     C                   PARM                    KPJBA
305200981009     C                   MOVEL(P)  KPJBU         DS_AZIONE
305300980521      *
305400990406      * Controllo il tipo azione. Se è valido imposto il titolo, viceversa vado a fine pgm
305500980521     C                   SELECT
305600980623      *
305700980521     C                   WHEN      AZN_tipo = 'A'
305800980604     c                   movel     TIT_A         VTCtit
305900981008      *
306000980521     C                   WHEN      AZN_tipo = 'G'
306100980604     c                   movel     TIT_G         VTCtit
306200981008     c                   eval      *in14 = *on                                  Abilito F06
306300990505     c                   eval      *in26 = *on                                  Disabilito ROLLUP
306400980623      *
306500980521     C                   WHEN      AZN_tipo = 'I'
306600980604     c                   movel     TIT_I         VTCtit
306700980623      *
306800990406     C                   WHEN      AZN_tipo = 'M'
306900980619     c                   movel     TIT_M         VTCtit
307000980623      *
307100980521     C                   OTHER
307200980521     C                   goto      FINE
307300990406      *
307400980521     C                   ENDSL
307500980612      *
307600130806     C                   Z-ADD     1             CODUT             1 0
307700130806     C*********          CALL      'X§PARUT'
307800130806     C*********          PARM                    UT§DSE0F
307900130806     C*********          MOVEL     RAGUT         RSUT
308000130806     C*********          MOVEL     REC80         CNCR80
308100130806
308200130806     c                   exsr      DatiJob
308300130806      * Imposto le abilitazioni dell'utente
308400130806     c                   eval      dute01 = UTEfaf
308500980615      *
308600981028      * Se Apertura in sede vado a fine
308700981028     C                   IF        AZN_tipo = 'A'  and  SIMFEL = *ZEROS
308800981028     C                   goto      FINE
308900981028    2C                   ENDIF
309000981028      *
309100981028      * Se Apertura abilito F10
309200980615     C                   eval      *IN12 = (AZN_tipo <> 'A')
309300980521      *
309400030228      * "P.O. IN GESTIONE"
309500030228     c                   SELECT
309600030228      *   se sono in sede non visualizzo e non modifico
309700030228     C                   WHEN      SIMFEL = *zeros
309800030228     c                   movel     046           v1cfgs
309900030228     C                   eval      *in06 = *on
310000030228     C                   eval      *in07 = *on
310100030228      *   se sono 2° livello visualizzo e non modifico
310200130806     c***********        WHEN      simtpp = '2' or simtpp = *blanks
310300130806     c                   WHEN      DUTlpo = '2' or DUTlpo = *blanks
310400130806     c**********         movel     simpou        v1cfgs
310500130806     c                   movel     DUTpou        v1cfgs
310600030228     c                   eval      *in06 = *on
310700030228     C                   eval      *in07 = *off
310800030228      *   negli altri casi visualizzo e modifico
310900030228     c                   OTHER
311000130806     c*********          movel     simpou        v1cfgs
311100130806     c                   movel     DUTpou        v1cfgs
311200030228     c                   eval      *in06 = *off
311300030228     C                   eval      *in07 = *off
311400030228    2C                   ENDSL
311500030228      *
311600040121      *  DECODIFICO P.O. IN GESTIONE
311700030228     C     V1CFGS        CHAIN     AZORG01L                           31
311800040121     C  N31              MOVEL     orgDES        V1DFGS
311900980923      *
312000030228      * Se sede disattivo F14 e forzo p.o. 46 in L6, se p.o. carico L6
312100981028     C                   IF        SIMFEL = *zeros
312200020502     C                   eval      L6(1) = v1cfgs
312300980604     C                   eval      *IN08 = *ON
312400980528     C                   ELSE
312500170426     C**                 z-add     v1cfgs        Wfgs
312600170426     C**                 CLEAR                   L6
312700170426     C**                 EXSR      CARL6
312800170426     C**                 movea     LIN           L6
312900170426    2C**                 ENDIF
313000170426     c* controllo filiale gestione
313100170426     c                   exsr      CTRFGS
313200170426     c                   endif
313300170426     c
313400010904      * AGGANCIO TABELLA GED
313500010910     c                   clear                   DGEDDN
313600010904     c                   clear                   TIBS02DS
313700010904     C                   MOVEL     'C'           t02mod
313800010904     C                   MOVEL     knsif         t02sif
313900010904     C                   MOVEL     'GED'         t02cod
314000010904     C                   MOVEL(P)  'DANNI'       t02ke1
314100010904      *
314200010904     C                   CALL      'TIBS02R'
314300010904     C                   PARM                    KPJBA
314400010904     C                   PARM                    TIBS02DS
314500010904      *
314600010904     C                   if        t02err = *BLANKS
314700010910     C                   MOVEL     T02UNI        DGEDDN
314800010904     C                   endif
314900980923
315000990406      * AGGANCIO TABELLA STD
315100990406     c                   clear                   TIBS02DS
315200990406     C                   MOVEL     'C'           t02mod
315300990406     C                   MOVEL     knsif         t02sif
315400990406     C                   MOVEL     'STD'         t02cod
315500990406     C                   MOVEL(P)  1             t02ke1
315600010910     C                   MOVEL(P)  §gedDBA       t02ke2
315700990406      *
315800990406     C                   CALL      'TIBS02R'
315900990406     C                   PARM                    KPJBA
316000990406     C                   PARM                    TIBS02DS
316100990406      *
316200990406     C                   if        t02err = *BLANKS
316300990406     C                   MOVEL     T02UNI        DSTD
316400120822     c******             eval      *in16 = (%SUBST(knmus:1:5) = §STDpco)
316500990406     C                   endif
316600990406
316700981019      * CARICO SCHIERA TABELLA TAD
316800980923     c                   z-add     *zeros        xx
316900980923     C                   MOVEL     'TAD'         Wtbecod
317000980923     c     Wtbecod       chain     tntbe000                           31
317100980923      *
317200980923     c                   DOW       *in31 = *off
317300981019     c                   movel     TBEuni        DTAD
317400981019      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
317500980923     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
317600981019      *                            and TBEatb = *BLANKS  and  §TADragr <> 'E'
317700980923     c                   add       1             xx
317800980923     c                   movel     TBEke1        KTA(XX)
317900980923     c                   movel     TBEuni        DTA(XX)
318000980923     c                   endif
318100980923     c     Wtbecod       reade     tntbe000                               31
318200980923     c                   enddo
318300980923
318400980923      * reperisco data e ora
318500980923     C                   TIME                    W0140
318600980923      * UDATE IN GGMMAAAA
318700980923     C                   MOVE      W0140         WDTGIO
318800980923      * UDATE IN AAAAMMGG
318900980923     C                   Z-ADD     WDTGIO        G02DAT
319000980923     C                   MOVEL     *BLANK        G02ERR
319100980923     C                   CALL      'XSRDA8'
319200980923     C                   PARM                    WLBDAT
319300980923     C                   MOVEL     G02INV        DATEU
319400980923     C     *iso          MOVEL     DATEU         DATA_oggi
319500980923
319600981204      * Apertura files
319700981204     c                   IF        SIMFEL = *zeros
319800981204     c                   open      TNTMD01L
319900990322     c                   open      TNTMD02L
320000990322     c                   open      TNTMD03L
320100050217     c                   open      fndct04l
320200981204     c                   ELSE
320300981204     c                   open      FNBLP01L
320400981204     c                   open      FNARB01L
320500050331     c                   open      Tigcp21L
320600050217     c                   open      fndct24l
320700981204     c                   IF        AZN_tipo = 'A'
320800981204      *   Per Apertura carico tabelle apposite
320900980923     c                   EXSR      CARTABAPE
321000980923     c                   ENDIF
321100981204     c                   ENDIF
321200980923
321300980923      * Imposto campi di video 1 predefiniti
321400980923     c                   exsr      inzd01
321500980923
321600980521     C     KTAB          KLIST
321700980521     C                   KFLD                    CODUT
321800980521     C                   KFLD                    KCOD
321900980526      *
322000980527     C     kspe          KLIST
322100981008     C                   KFLD                    kAAS
322200980527     C                   KFLD                    kLNP
322300980527     C                   KFLD                    kNRS
322400980527     C                   KFLD                    kNSP
322500981204      *
322600981026     C     KDET          KLIST
322700981026     C                   KFLD                    V1Caae
322800981026     C                   KFLD                    V1Cnev
322900980702      *
323000980702     c     kgiac         klist
323100980702     c                   kfld                    V2Haas
323200980702     c                   kfld                    V2Clnp
323300980702     c                   kfld                    V2Cnrs
323400980702     c                   kfld                    V2Cnsp
323500980702     c                   kfld                    Wfrg
323600981204      *
323700981204     c     kdka          klist
323800981204     c                   kfld                    V2Haac
323900981204     c                   kfld                    V2Cfil
324000981204     c                   kfld                    V2Cnca
324100981204     c                   kfld                    WTRC
324200981204      *
324300981204     c     ktmd          klist
324400981204     c                   kfld                    V2Haas
324500981204     c                   kfld                    V2Clnp
324600981204     c                   kfld                    V2Cnrs
324700981204     c                   kfld                    V2Cnsp
324800980527      *
324900980527     C     Kdct          klist
325000980527     C                   KFLD                    kaac
325100980527     C                   KFLD                    kfil
325200980527     C                   KFLD                    knca
325300980527      *
325400980527     C     Kdct4         klist
325500980527     C                   KFLD                    kaac
325600980527     C                   KFLD                    kmgc
325700980527     C                   KFLD                    kfil
325800980527     C                   KFLD                    knca
325900980527      *
326000980527     C     Keve          klist
326100980527     C                   KFLD                    V1Caae
326200980527     C                   KFLD                    V1Cnev
326300990323      *
326400990323     C     Kpra          klist
326500990323     C                   KFLD                    V1Cdit
326600990323     C                   KFLD                    V1Cpra
326700990323     C                   KFLD                    V1Cprn
326800980608      *
326900980608     c     kdcf          klist
327000980608     c                   kfld                    Kaac
327100980608     c                   kfld                    kfil
327200980608     c                   kfld                    knca
327300041026      *
327400041026     c     kdcl01        klist
327500041110     c                   kfld                    V1Casn
327600041026      *
327700041026     c     kdcl02        klist
327800041110     c                   kfld                    V1Casn
327900041026     c                   kfld                    w_V1Cdea
328000980525      *
328100980521     C                   ENDSR
328200130806      *---------------------------------------------------------------*
328300130806      *?Reperimento Dati del job (Utente/Operativi)                  ?*
328400130806      *---------------------------------------------------------------*
328500130806     c     DatiJob       BegSr
328600130806      *
328700130806     c     *dtaara       define    §azute        AZUTEds
328800130806     c     *dtaara       define    §datiute      dDatiUte
328900130806      *
329000130806     c                   in(E)     *dtaara
329100130806     c                   IF        %ERROR or RSUT = *blanks
329200130806     c                   clear                   Tibs34Ds
329300130806     c                   call      'TIBS34R'
329400130806     c                   parm                    Tibs34Ds
329500130806     c                   in        *dtaara
329600130806     c                   ENDIF
329700130806      *
329800130806     c                   EndSr
329900980521      *---------------------------------------------------------------------------------------------
330000980521** MSG  Lungh. 78                                                            *
330100981019Immettere il Tipo Anomalia                                                          1
330200170427Spedizione inesistente o in gestione ad altra filiale                               2
330300150914Se inserita la data è obbligatorio indicare il numero assegno o bonifico            3
330400980610Tipo Anomalia inesistente o annullato                                               4
330500020821Per aprire una C.A. imettere la spedizione                                          5
330600980522Immettere la linea                                                                  6
330700980522Linea inesistente o annullata                                                       7
330800990322Immettere numero pratica assicurativa                                               8
330900980521Immettere l'anno                                                                    9
331000980522Impossibile aprire C.A. su bolle di recupero                                        10
331100990322Immettere codice società. BAR = Bartolini, SDI = S.D.I.                             11
331200980522Immettere una selezione valida                                                      12
331300020128Fase non eseguibile per anomalia in franchigia                                      13
331400050208? LIBERO ? -   Fase non eseguibile per anomalia NON in franchigia                   14
331500980525Immettere data formalmente valida                                                   15
331600980525Immettere data DAL minore o uguale alla data AL                                     16
331700980623Codice Fase inesistente o annullato                                                 17
331800980525Test "Liquid.Trans/Pratica ass." non congruente con Codice Fase                     18
331900980525Data esecuzione fase maggiore della data C.A.                                       19
332000980605Immettere un codice fase                                                            20
332100980608Codice Fase non utilizzabile                                                        21
332200980623Scegliere un opzione valida                                                         22
332300980731C. A. non gestibile: manca il numero spedizione                                     23
332400170427C. A. non manutenzionabile: in gestione ad altra filiale                            24
332500980702C. A. non gestibile: la spedizione risulta in giacenza                              25
332600980710C. A. non manutenzionabile: la C.A. è chiusa                                        26
332700980731Premere F6 per gestire la fase selezionata                                          27
332800981008Opzione non consentita con la Conferma Totale                                       28
332900981008Tutte le C.A. sono state escluse, nessuna elaborazione effettuata                   29
333000981009Opzione non consentita con la Conferma                                              30
333100981019Tipo anomalia valido solo per Gestione Eventi                                       31
333200981026Per la fase selezionata è obbligatorio selezionare un evento                        32
333300981026Numero Evento inesistente                                                           33
333400981027C. A. non gestibile: manca il numero spedizione, escludere dall'elaborazione        34
333500990115C. A. annullata                                                                     35
333600990312Scorrimento indietro o in avanti oltre il primo o l'ultimo record del file.         36
333700110930Impossibile aprire C.A. su bolla ancora da borderizzare                             37
333800110930Impossibile aprire C.A. su bolla chiusa in partenza                                 38
333900150701Beneficiario impostabile se inseriti gli estremi del pagamento                      39
334000150701Del Beneficiario inserire  il codice  o  la ragione sociale (anche parziale)        40
