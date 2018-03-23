000100120712     /*PRM dbgview(*source)
000200120712     /*CMD ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120712     /*CMD        ovrscope(*calllvl)
000400120712     /*CMD ovrdbf file(TNTMD01L) tofile(GAITRAGRPS/TNTMD01L) +
000500120712     /*CMD        ovrscope(*calllvl)
000600120712     /*CMD ovrdbf file(TNCSB03L) tofile(GAITRAGRPS/TNCSB03L) +
000700120712     /*CMD        ovrscope(*calllvl)
000800120712     /*END dltovr file(TITAS30C TNTMD01L TNCSB03L) lvl(*)
000900120712     /*END
001000980618      *-------------------------------------------------------------------------*
001100981112      *    I N T E R R O G A Z I O N E   C   A                                  *
001200981112      *-------------------------------------------------------------------------*
001300981201      ****************************************************************
001400981201      *  FASI C.A. FISSATE A PROGRAMMA
001500981201      ****************************************************************
001600981201      *    575 =    Sollecito invio documenti SOL = Schiera fasi solleciti
001700981201      *    605 = 1° Sollecito invio documenti SOL = Schiera fasi solleciti
001800981201      *    615 = 2° Sollecito invio documenti SOL = Schiera fasi solleciti
001900981201      *    630 = 3° Sollecito invio documenti SOL = Schiera fasi solleciti
002000050718      *    660 =    Fine ricezione documenti
002100980625      ****************************************************************
002200980625      *  RIEPILOGO INDICATORI
002300131014      ****************************************************************
002400090324      * 01   - Utente EDP
002500090324      * 03   - Abilito il comando dell'interrogazione responsabilità
002600981222      * 04   - Alta intensità importo da assicurare
002700981222      * 05   - Visualizzo importo Fattura immediata
002800981222      * 06   - Visualizzo importo contrassegno
002900981117      * 07 on- Abilito il comando dell'interrogazione evento
003000981117      * 07off- Abilito il comando interrogazione responsabilità
003100981117      * 08   - abilito il comando Cliente contatto
003200981221      * 09   - abilito il comando Rivalsa
003300981201      * 10   - visualizzo Num/data importo valuta Documento in SBF Documenti ricevuti
003400981201      * 11   - visualizzo Data ultimo sollecito richiesta documenti a clienti
003500981201      * 12   - segnacollo presente in un altra CA
003600990510      * 13   - visualizzo Responsabilità Internazionale
003700981214      * 15   - visualizzo Beneficiario Alternativo
003800981214      * 16   - visualizzo Rivalsa
003900131014      * 17   - visualizzo Dati relativi all'affidamento pratica ad A.I.G.
004000981117      * 18   - abilito il comando Altri dati per intestatario P.L.
004100981201      * 19   - visualizzo la data e la cuasale di chiusa al posto della fase ca
004200980728      * 20   - COMODO Gestione subfile
004300980728      * 21   - COMODO Gestione subfile
004400980728      * 22   - COMODO Gestione subfile
004500990506      * 21/27- COMODO per ricerca capoconti
004600980728      * 28   - ERRORE GENERICO DSPF
004700980728      * 29   - RIEMISSIONE FORMATO VIDEO
004800980728      * 30   - COMODO
004900980728      * 31   - COMODO
005000000128      * 32   - COMODO
005100130726      * 33   -
005200000128      * 34   - Visualizzo estremi pagamento pratiche assicurative
005300990209      * 35   - C.A. Annullata
005400131014      * 36   - C.A. Affidata ad A.I.G. solo dalla fase 660 in poi
005500990329      * 38   - C.A. già in pagamento
005600990329      * 39   - C.A. Estratta per il pagamento
005700981201      * 40   - COMODO Gestione subfile 4
005800981201      * 41   - COMODO Gestione subfile 4
005900131014      * 47   - C.A. con record in FNDRA (dati rimborso A.I.G.) attiva tasto funzionale
006000050908      * 48   - C.A. esclusa dal rimborso
006100981201      * 50   - COMODO Gestione subfile 5
006200981201      * 51   - COMODO Gestione subfile 5
006300990128      * 60   - COMODO Gestione subfile 5
006400990128      * 61   - COMODO Gestione subfile 5
006500990128      * 62   - Condiziona il campo calcolo responsabilità
006600990129      * 63   - Condiziona il campo calcolo responsabilità
006700990129      * 64   - Condiziona il campo calcolo responsabilità
006800990215      * 65   - Condiziona i campi collo ritrovato
006900990215      * 66   - Condiziona i campi chiusura segnacollo
007000990216      * 67   - Attivo il sfldrop per visualizzare collo ritrovato /chiusura
007100990216      * 68   - Responsabilità non importante ai fini della reportistica
007200990217      * 69   - Nascondo i campi del P.O. responsabile xchè vuoto
007300990409      * 70   - Disabilito interrogazione Mandato se non esiste il file dei mandati
007400990609      * 71   - Data invio copia denuncia ai clienti valorizzata
007500000131      * 72   - Emissione estremi di pagamento P.A. liquidate
007600000131      * 73   - Emissione estremi di pagamento L.T. liquidate
007700060412      * 74   - Emissione estremi di pagamento P.A. no P.L. liquidate
007800080715      * 75   - Attivo tasto funzionale Fxx per visulizz spunta senza
007900080715      *        anomalia per ADPA ADPV
008000990104      * 80   - COMODO Gestione '+' segue descrizioni
008100990104      * 81   - COMODO Gestione '+' segue descrizioni
008200990104      * 82   - COMODO Gestione '+' segue descrizioni
008300990104      * 83   - COMODO Gestione '+' segue descrizioni
008400990104      * 90   - COMODO per capire se ci sono altre righe di descrizione
008500980625      ***************************************************************
008600980618
008700951009     H DECEDIT('0,') DATEDIT(*DMY.)
008800980625      * ---------------------------------------------------------
008900981112     fFIDN01D   CF   E             WORKSTN sfile(FI01S03:nrr1)
009000981116     f                                     sfile(fi01s04:nrr4)
009100990129     f                                     sfile(fi01s05:nrr5)
009200990129     f                                     sfile(fi01s06:nrr6)
009300981005     fTABEL00F  IF   E           K DISK
009400981113     fFNDCT01L  IF   E           K DISK
009500990217     fFNDCR02L  IF   E           K DISK
009600981113     fFNDCS01L  IF   E           K DISK
009700981005     fFNDET01L  IF   E           K DISK
009800981113     fFNDET02L  IF   E           K DISK    rename(fndet000:fndet002)
009900981113     fFNDCD01l  IF   E           k DISK
010000981113     fFNDCD02l  IF   E           k DISK    rename(fndcd000:fndcd002)
010100981113     fFNDCL01l  IF   E           k DISK
010200981116     fFNDCf01l  IF   E           k DISK
010300981113     fFNDCC01l  IF   E           k DISK
010400981113     fFNDKA01l  IF   E           k DISK
010500990409     fFNDPT01L  IF   E           K DISK
010600040716     fFNDCK01l  IF   E           k DISK
010700050909     fFNDRA01l  IF   E           k DISK
010800981005     fAZORG01L  IF   E           K DISK
010900990315     fCNABI01L  IF   E           K DISK
011000981006      * filiale
011100981006     fFNBLT01L  IF   E           K DISK     usropn
011200020827     f                                      prefix(ART:3)
011300981229     fFNLBL02L  IF   E           K DISK     usropn
011400981006     fFNART01L  IF   E           K DISK     usropn
011500991104     fFIAR601L  IF   E           K DISK     usropn
011600051114     fFiAR901L  IF   E           K DISK     usropn
011700981113     fFNBLP01L  iF   E           K DISK     usropn
011800020827     f                                      prefix(ARB:3)
011900981113     fFNARB01L  iF   E           K DISK     usropn
012000981005     fCNACO00F  IF   E           K DISK     usropn
012100981020     fFNCLS01L  IF   E           K DISK     usropn
012200981006      * sede
012300990806     fTITAS30C  IF   E           K DISK     usropn
012400981005     fTNTMD01L  IF   E           K DISK     usropn
012500020827     f                                      prefix(ARB:3)
012600981221     fTNCSB03L  IF   E           K DISK     usropn
012700080715     ffnbrve1l  IF   E           K DISK     extfile(Wlibfile) usropn
012800080715     f                                      prefix(E_)
012900080715     f                                      rename(fnbrv000:fnbrve00)
013000080715     ffnbrv07l  IF   E           K DISK     extfile(WlibBRV) usropn
013100980611      * ---------------------------------------------------------
013200990128     D CMD             S             78    DIM(5) CTDATA PERRCD(1)              CMD video
013300981117     D CMD2            S             25    DIM(3) CTDATA PERRCD(1)              CMD video
013400150630     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG video
013500981201     D SOL             S              3  0 DIM(4) CTDATA PERRCD(10)             MSG video
013600981006     D nsc             S              7  0 DIM(100)
013700980623     D aac             S              4  0 DIM(100)
013800980623     D fil             S              3  0 DIM(100)
013900980623     D nca             S              7  0 DIM(100)
014000150630     D* schiera note testo
014100150630     D TNO             S             35    DIM(6)                               6 Note testo
014200990217      *----------------------------------
014300990217     D MAI             C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
014400990217     D MIN             C                   CONST('abcdefghijklmnopqrstuvwxyz')
014500150630      *  Gestione A.I.G.
014600150630     D Cost_AIG        C                   CONST('SEDE-A.I.G. ')
014700150630      *  Evento fortuito
014800150630     D Cost_fortu      C                   CONST('  FORTUITO  ')
014900990217      *----------------------------------
015000980717
015100981020     D FIDN00DS      E DS
015200981029     d  ds_numca               6     19
015300980717     D FIDN05DS      E DS
015400981113     D FIDN06DS      E DS
015500981020     D FIDN10DS      E DS
015600990409     D FIDN30DS      E DS
015700990806     D TNSB50DS      E DS
015800981020     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
015900981020     D TIBS02DS      E DS
016000991104     D FNLV16DS      E DS
016100080715     D FNLV53DS      E DS
016200981112     d TIBS69DS      E DS
016300981005     d DS_cnaco      E DS                  extname(CNACO00F)
016400981005     d DS_cnind      E DS                  extname(CNIND00F)
016500981005     d DS_cnclp      E DS                  extname(CNCLP00F)
016600981005     d DS_fncls      E DS                  extname(FNCLS00F)
016700150630      *
016800150630      * -?Parametri x Controllo profilo utenti?
016900150630     d TIBS34DS      e ds                  inz
017000150630      * -?Ds di riferimento al file esterno AZUTE00F?
017100150630     d AZUTEDS       e ds                  extname(AZUTE00F)
017200150630      * -?Ds per dati organigramma?
017300150630     d DDatiUte      e ds
017400150630      *
017500981020     D KPJBA         E DS
017600981116     D DTAD          E DS
017700981201     D DCDD          E DS
017800981116     D DCCH          E DS
017900981020     D DSTB          E DS
018000990506     D DSTD          E DS
018100010910     D DGEDDN        E DS
018200981230     D DDCT01        E DS
018300990216     D DDCR01        E DS
018400020124     d ddcf01        e ds
018500991104     D DTASV         E DS
018600150630      * -?Gestione Anagrafiche per Danni?
018700150630     d fidnU1ds      e ds                  inz
018800040719      *
018900040719     D FIDN12DS      E DS
019000040719     d  skKey                 26    305    dim(20)
019100040719     d  skAnn                306    325    dim(20)
019200040719     d  skDca                326    485  0 dim(20)
019300040719     d  skFca                486    545  0 dim(20)
019400040719     d  skDch                546    705  0 dim(20)
019500040719     d  skCch                706    745    dim(20)
019600040719
019700040719     d dsKey           ds
019800040719     d  dsaac                         4  0
019900040719     d  dsfil                         3  0
020000040719     d  dsnca                         7  0
020100040719      *
020200980615
020300981116     d                 ds
020400981116     d  DS_dctkey              1     14
020500981116     d  dctaac                 1      4  0
020600981116     d  dctfil                 5      7  0
020700981116     d  dctnca                 8     14  0
020800980610     d                 ds                  INZ
020900980727     d  DS_arbdat              1      8  0
021000980525     d  arbaas                 1      4  0
021100980525     d  arbmgs                 5      8  0
021200990128
021300990128     D DS_VISBOLP      DS                  INZ
021400990128     D  DSVBP_aas              1      4  0
021500990128     D  DSVBP_lnp              5      7  0
021600990128     D  DSVBP_nrs              8      9  0
021700990128     D  DSVBP_nsp             10     16  0
021800990128     D  DSVBP_f03             17     17
021900990129     D  DSVBP_flg             18     18
022000990128     D  DSVBP_rsut            23     42
022100990128     D  DSVBP_cml             43     43
022200990128     D  DSVBP_sim             44     46  0
022300990128      *
022400990128     D DS_RICBOLA      DS                  INZ
022500990128     D  DSRBA_aas             14     17  0
022600990128     D  DSRBA_lnp             18     20  0
022700990128     D  DSRBA_nrs             21     22  0
022800990128     D  DSRBA_nsp             23     29  0
022900990128     D  DSRBA_f03             30     30
023000990128     D  DSRBA_flg             31     31
023100980615
023200971016     D WLBDAT          DS                  INZ
023300971016     D  G02DAT                 1      8  0
023400971016     D  G02INV                 9     16  0
023500971016     D  G02ERR                17     17
023600971016     D  G02TGI                18     22  0
023700980615
023800980615
023900951009     D                SDS
024000971016     D  VTCPGM                 1     10
024100980615
024200150630     d $Trc_I          s               n   inz
024300150630      *
024400981005     d Datasped        S               D   datfmt(*eur)
024500980623     D Wdataeur        S               D   DATFMT(*eur)
024600990216     D Wdatadmy        S               D   DATFMT(*dmy)
024700991116     D trcar6          S                   LIKE(AR6trc)
024800981029     D wksc            S                   LIKE(ARBksc)
024900981020     D flgtb1          S                   LIKE(§TBtpo)
025000981020     D Wtrc            S                   LIKE(DKAtrc)
025100980625     D Kcod            S                   LIKE(TBLcod)
025200980611     D Kkey            S                   LIKE(TBLkey)
025300980611     D ktpd            s                   LIKE(DCStpd)
025400980611     D knkt            s                   LIKE(DCSnkt)
025500980611     D kstd            s                   LIKE(DCSstd)
025600980611     D knks            s                   LIKE(DCSnks)
025700980611     D ktrc            s                   LIKE(DCStrc)
025800981116     D kpno            s                   LIKE(DCSpno)
025900980611     D kdim            s                   LIKE(DCSdim)
026000980611     D khim            s                   LIKE(DCShim)
026100980611     D ccc             s                   LIKE(ACOkcc)
026200990104     d Wdataca         s                   LIKE(VtCdc1)
026300990104     d Wdcsnot         s                   LIKE(dcsnot)
026400990305     d W§dctport       s                   LIKE(§dctport)
026500990315     d Wabi            s                   LIKE(abiabi)
026600990315     d Wcab            s                   LIKE(abicab)
026700050718     d Wfase           s                   LIKE(dctfca) inz(660)
026800080715     d KNSC            s                   LIKE(dcdnsc)
026900080715     d WLibFile        s             21    inz('FILTRAxxx /FNBRVE1L')
027000080715     d WLibBRV         s             21    inz('FILTRAxxx /FNBRV07L')
027100080715     dDataiso          s               d   datfmt(*iso)
027200080715     dDataeur          s               d   datfmt(*eur)
027300980625
027400980625     D Wkey1           s             15
027500981005     D wtibs69r        s              1
027600980720     D w0140           s             14  0
027700980720     D w0080           s              8  0
027800980720     D w0040           s              4  0
027900980708     D II              s              3  0
028000981113     D XX              s              3  0
028100980611     D wdtgio          s              8  0
028200980623     D nrr1            s              5  0
028300981201     d nrr4            s              5  0
028400981201     d nrr5            s              5  0
028500990129     d nrr6            s              5  0
028600981201     d C               s              1  0                                      * indice schiera CMD
028700981113     D wclico          s              4  0                                      * =8888 CLI.CONTATTO
028800050718
028900980625      * ---------------------------------------------------------
029000990806     ITITAS000
029100981005     I              TASAAS                      ARBAAS
029200981005     I              TASLNP                      ARBLNP
029300981005     I              TASNRS                      ARBNRS
029400981005     I              TASNSP                      ARBNSP
029500981005     I              TASMGS                      ARBMGS
029600981005     I              TASNCL                      ARBNCL
029700981005     I              TASKSC                      ARBKSC
029800981005     I              TASCCM                      ARBCCM
029900990806     I              TASPKF                      ARBPKF
030000981005     I              TASFVF                      ARBFVF
030100981005     I              TASVLF                      ARBVLF
030200981006     I              TASLNA                      ARBLNA
030300981013     I              TASIAS                      ARBIAS
030400981013     I              TASVAS                      ARBVAS
030500981013     I              TASTSP                      ARBTSP
030600981013     I              TASCTR                      ARBCTR
030700981104     I              TASLL1                      WDDT
030800990217     I              TASRMN                      ARBRMN
030900990309     I              TASCCA                      ARBCCA
031000981005
031100990806     ITITAS010
031200981005     I              TASAAS                      ARBAAS
031300981005     I              TASLNP                      ARBLNP
031400981005     I              TASNRS                      ARBNRS
031500981005     I              TASNSP                      ARBNSP
031600981005     I              TASMGS                      ARBMGS
031700981005     I              TASNCL                      ARBNCL
031800981005     I              TASKSC                      ARBKSC
031900981005     I              TASCCM                      ARBCCM
032000990806     I              TASPKF                      ARBPKF
032100981005     I              TASFVF                      ARBFVF
032200981005     I              TASVLF                      ARBVLF
032300981006     I              TASLNA                      ARBLNA
032400981013     I              TASIAS                      ARBIAS
032500981013     I              TASVAS                      ARBVAS
032600981013     I              TASTSP                      ARBTSP
032700981013     I              TASCTR                      ARBCTR
032800981104     I              TASLL1                      WDDT
032900990301     I              TASRMN                      ARBRMN
033000990309     I              TASCCA                      ARBCCA
033100981005
033200990806     ITITASP00
033300981005     I              TASAAS                      ARBAAS
033400981005     I              TASLNP                      ARBLNP
033500981005     I              TASNRS                      ARBNRS
033600981005     I              TASNSP                      ARBNSP
033700981005     I              TASMGS                      ARBMGS
033800981005     I              TASNCL                      ARBNCL
033900981005     I              TASKSC                      ARBKSC
034000981005     I              TASCCM                      ARBCCM
034100990806     I              TASPKF                      ARBPKF
034200981005     I              TASFVF                      ARBFVF
034300981005     I              TASVLF                      ARBVLF
034400981006     I              TASLNA                      ARBLNA
034500981013     I              TASIAS                      ARBIAS
034600981013     I              TASVAS                      ARBVAS
034700981013     I              TASTSP                      ARBTSP
034800981013     I              TASCTR                      ARBCTR
034900981104     I              TASLL1                      WDDT
035000990301     I              TASRMN                      ARBRMN
035100990309     I              TASCCA                      ARBCCA
035200981005
035300981221     ITNCSB000
035400981221     I              CSBCAS                      AR9CAS
035500981221     I              CSBVCA                      AR9VCA
035600981221     I              CSBCMB                      AR9CMB
035700980625      * ---------------------------------------------------------
035800980615
035900981020
036000980716      *   G E S T I O N E   C A M P I   F O R M A T I   V I D E O
036100980716
036200981113      * Riempio Prima videata
036300981113     C                   EXSR      CAR_W1
036400981113     c
036500981113      *
036600981113      *            T A S T I    F U N Z I O N A L I
036700981113     C                   DO        *hival
036800981022      *
036900981112     C                   EXFMT     FI01D01
037000980626      *
037100971020     C                   CLEAR                   V1CMSG
037200980619     C                   SETOFF                                       2829
037300980625
037400980625      * F3 = FINE
037500980623     C                   IF        *INKC = *ON
037600980626     C                   movel     'S'           O00F03
037700981020     C                   movel     FIDN00DS      KPJBU
037800981113     C                   leave
037900980623     C                   ENDIF
038000980625
038100980625      * F12 = RITORNO
038200980619     C                   IF        *INKL = *ON
038300980626     C                   movel     'S'           O00F12
038400981020     C                   movel     FIDN00DS      KPJBU
038500981113     C                   leave
038600980619     C                   ENDIF
038700981113
038800981113     c                   SELECT
038900981113      * F07 = EVENTO Interrogazione
039000981113     C                   WHEN      *INKG = *on
039100981113      * se non c'è un evento errore
039200981113     C                   IF        dctnev = *zeros
039300981113     c                   movel     msg(1)        v1cmsg
039400981113     c                   seton                                        28
039500981113     c                   else
039600981113     c                   exsr      SUB_F7
039700981113     c                   endif
039800981113
039900981113      * F08 = CLIENTE CONTATTO
040000981113     C                   WHEN      *INKH = *on
040100981113      * se non c'è un cliente contatto errore
040200981113     C                   IF        wclico <> 8888
040300981113     c                   movel     msg(2)        v1cmsg
040400981113     c                   seton                                        28
040500981113     c                   else
040600981113     c                   exsr      SUB_F8
040700981113     c                   endif
040800981113
040900981113      * F09 = RIVALSA
041000981113     C                   WHEN      *INKI = *on
041100981113      * se non c'è rivalsa ERRORE
041200981214     C                   IF        V1CASS = *BLANKS
041300981113     c                   movel     msg(3)        v1cmsg
041400981113     c                   seton                                        28
041500981113     c                   else
041600981113     c                   exsr      SUB_F9
041700981113     c                   endif
041800981113
041900981113      * F10 = NOTE TESTO
042000981113     C                   WHEN      *INKJ = *on
042100981113     c                   exsr      SUB_F10
042200981113
042300981113      * F13 = DOCUMENTI RICHIESTI AL CLIENTE
042400981113     C                   WHEN      *INKM = *on
042500981113     c* controllo se sono stati richiesti dei documenti a fronte di questa CA
042600981113     c     kdcc          chain     fndcc01l                           30
042700981113     c                   if        *in30 = *on
042800981113     c                   movel     msg(5)        v1cmsg
042900981113     c                   seton                                        28
043000981113     c                   else
043100981201     c                   exsr      SUB_F13
043200981113     c                   endif
043300981113
043400981113      * F14 = RESPONSABILITA'
043500981113     C                   WHEN      *INKN = *on
043600990128     c                   exsr      SUB_F14
043700980716
043800981113      * F15 = IMPORTI
043900981113     C                   WHEN      *INKP = *on
044000981113     c                   exsr      SUB_F15
044100981113
044200990128      * F18 = NOTE
044300990128     C                   WHEN      *INKS = *ON
044400990128     c                   exsr      SUB_F18
044500990128
044600000309      * F19 = Interrogazione Bolle  (Test chiamata ricorsiva)
044700000309     C                   WHEN      *INKT = *ON  and  %PARMS <> 2
044800990128     c                   exsr      SUB_F19
044900000309
045000000309      * F19 = Interrogazione Bolle Non abilitata
045100000309     C                   WHEN      *INKT = *ON
045200000309     C                   movel     'S'           O00F03
045300000309     C                   movel     FIDN00DS      KPJBU
045400000309     C                   leave
045500981113
045600981113      * F20 = STORICO FASI
045700981113     C                   WHEN      *INKU = *ON
045800981116     c                   exsr      SUB_F20
045900981113
046000981113      * F21 = DETTAGLIO COLLI
046100981113     C                   WHEN      *INKV = *ON
046200981113      * Controllo se ci sono colli per questa CA
046300981113     c                   if        v1cncn = 0
046400981113     c                   movel     msg(4)        v1cmsg
046500981113     c                   seton                                        28
046600981113     c                   else
046700981113     c                   exsr      SUB_F21
046800981113     c                   ENDIF
046900981113
047000981113      * F24 = ALTRI TASTI
047100981113     C                   WHEN      *INKY = *ON
047200990506     c                   exsr      SUB_F24
047300981113
047400981113     c                   ENDSL
047500981113
047600981113      * controlli videata
047700981113     c                   exsr      ctrd01
047800981113
047900981113     C                   ENDDO
048000981112
048100020827      * Fine - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
048200981020
048300980623      * Chiudo pgm di servizo
048400981020     c                   clear                   FIDN10DS
048500971031     c                   movel     'C'           i10tla
048600981020     c                   movel     FIDN10DS      kpjbu
048700971030     c                   call      'FIDN10R'
048800971030     c                   parm                    kpjba
048900980623      *
049000981005      *
049100981005     C                   IF        Wtibs69r  <>  *BLANKS
049200981005     C                   EVAL      I69TLA  = 'C'
049300981005     C                   CALL      'TIBS69R'
049400981005     C                   PARM                    TIBS69DS
049500981005     C                   ENDIF
049600981029      *
049700981029     c                   clear                   tibs02ds
049800981029     c                   movel     'C'           t02tla
049900981029     c                   call      'TIBS02R'
050000981029     c                   parm                    kpjba
050100981029     c                   parm                    tibs02ds
050200080715      *
050300080715     c                   clear                   fnlv53ds
050400080715     c                   movel     'C'           d53tla
050500080715     c                   call      'FNLV53R'
050600080715     c                   parm                    fnlv53ds
050700981029      *
050800981029     c                   movel     FIDN00DS      kpjbu
050900981029      *
051000981005     C                   EVAL      *INLR = *ON
051100980615      ****************************************************************
051200980615      *  CARICAMENTO DATI DI TESTATA VIDEO 1
051300980615      ****************************************************************
051400981113     C     CAR_W1        BEGSR
051500980625
051600980625
051700981112      *  aggancio dati CA
051800980525     c                   z-add     i00fil        vtcfil
051900980525     c                   z-add     i00nca        vtcnca
052000981201     c     knumca        chain     fndct000                           30
052100990209     c*
052200990209     c* controllo se annullata
052300990209     c                   eval      *in35 = (dctatb = 'A')
052400040716      * in caso di tipo bolla ='A' recupero il numero bolla prima da FNDCK e se non c'è da FNDCT
052500040716     c                   If        i00tpb = 'A'
052600040716     c     Knumca        chain     fndck01l
052700040716     c                   If        %found(fndck01l)
052800040716     c                   eval      DCTaas = DCKaas
052900040716     c                   eval      DCTlnp = DCKlnp
053000040719     c                   eval      DCTnrs = DCKnrs
053100040716     c                   eval      DCTnsp = DCKnsp
053200040716      *
053300040716     c                   eval      i00aas = DCKaas
053400040716     c                   eval      i00lnp = DCKlnp
053500040719     c                   eval      i00nrs = DCKnrs
053600040716     c                   eval      i00nsp = DCKnsp
053700040716      *
053800040716     c                   endif
053900040716     c                   endif
054000040716      *
054100990118      * se ho il numero spedizione chaino la bolla
054200990118     c                   IF        dctNSP > *ZEROS
054300990118      *
054400990118     c                   if         i00tpb = 'S'                                SEDE
054500990118     c     kbol01        chain     tntmd000                           30
054600990806     c     kbol01        chain     TITAS30C                           30
054700990118     c                   exsr      CTR_tipobo                                   Ctr tipo bolla
054800990118     c                   if        FLGTB1 = *blanks
054900990806     c     kbol01        reade     TITAS30C                               30
055000990118     c                   endif
055100990118      *
055200990118     c                   else
055300990118      *
055400990118     c                   if         i00tpb = 'P'
055500990118     c     kbol01        chain     fnblp000                           30
055600990118     c                   else
055700990118     c     kbol01        chain     fnarb000                           30
055800990118     c                   endif
055900990118     c                   endif
056000990118      *
056100990118     c  N30*iso          move      DS_arbdat     datasped
056200990118     c                   ENDIF
056300060412
056400981230     C* valorizzo i flag operativi
056500981230     c                   movel     dctflo        ddct01
056600980525     C                   CLEAR                   WLBDAT
056700980611     c                   move      dctmgc        w0080
056800980526     c                   movel     dctaac        w0080
056900980526     C                   Z-ADD     w0080         G02inv
057000980525     C                   movel     '3'           G02err
057100980525     C                   CALL      'XSRDA8'
057200980525     C                   PARM                    WLBDAT
057300980525     c                   z-add     g02dat        vtcdc1
057400980720     c                   z-add     VTCdc1        Wdataca
057500990118      *
057600990118     c                   IF        dctnsp <> *zeros
057700990118     c                   move      datasped      vtcdsp
057800990118     C                   move      dctlnp        VTCLNP
057900990118     C                   move      dctnrs        VTCNRS
058000990118     C                   move      dctnsp        VTCNSP
058100990118     C                   movel     dctlna        VTcLNA
058200990118     c                   ENDIF
058300981113
058400981113      * TIPO ANOMALIA
058500990118     C                   MOVEL(P)  dctTad        WKEY1
058600981113     C                   EXSR      CHTAD
058700981113     C                   MOVEL     WKEY1         VTCtad
058800981113     c                   movel     §taddesc      vtdtad
058900981113
059000981113      * Da file descrizioni: descrizioni aggiuntive
059100981113     c                   movel     'C'           ktpd
059200981113     c                   movel(p)  DS_numca      knkt
059300981113     c                   movel     'A'           kstd
059400981113     c                   clear                   kdim
059500981113     c                   clear                   khim
059600981113     c                   clear                   knks
059700981113     c                   movel     'D'           ktrc
059800981113     c                   exsr      chadcs
059900981113     c  n30              movel     dcsnot        vtdagg
060000990104     c                   eval      *in80 = (not *in30 and  *in90)
060100981113
060200981113      * Numero Evento
060300981113     c                   IF        DCTnev > *zeros
060400981117     c                   seton                                        07
060500981113      *
060600981113     c                   move      dctnev        vtcnev
060700981113     c                   z-add     dctaae        vtcaae
060800981113      *   reperisco data evento  e tipo  anomalia evento
060900981113     C     KDET          chain     FNDET000                           31
061000981113     c                   if        *IN31 = *OFF
061100981113     C                   movel(P)  DETtad        WKEY1
061200981113     C                   exsr      CHTAD
061300981113     c                   movel     §taddesc      vtdaev
061400050718
061500050718     c                   If        §tadevfo  = ' '
061600050718     c                   clear                   vtdfor
061700050718     c                   else
061800050718     c                   eval      vtdfor = cost_fortu
061900050718     c                   endif
062000981113      * data evento
062100981113     c                   move      detmge        w0080
062200981113     c                   movel     detaae        w0080
062300981113     C                   Z-ADD     w0080         G02inv
062400981113     C                   movel     '3'           G02err
062500981113     C                   CALL      'XSRDA8'
062600981113     C                   PARM                    WLBDAT
062700981113     c                   z-add     g02dat        vtcdev
062800981113     c                   endif
062900981113      *
063000981113     c                   ENDIF
063100990129      *
063200990129     c* Se è spento lo 07 controllo se devo visyualizzare il comando della responsabilità
063300990129      *
063400990129     c* per no evento controllo se c'è responsabilità
063500990216     c                   if        dctcar <> ' '
063600990129     c                   eval      *in03 = *on
063700990129     c                   endif
063800981005
063900990305      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
064000990305      *  sped. è "Mamma di cambio porto" e mi trovo in filiale, in questo caso inverto il porto C.A.
064100990305     c                   SELECT
064200110503     c***                WHEN      ARBcca <> '9'
064300110503     c                   WHEN      §DCTcca <> '9'
064400990305     c                   movel     §DCTport      W§DCTport
064500990305     c                   WHEN      §DCTport = 'A'
064600990305     c                   movel     'F'           W§DCTport
064700990305     c                   WHEN      §DCTport = 'F'
064800990305     c                   movel     'A'           W§DCTport
064900990305     c                   ENDSL
065000981201      *  Se assegnato in partenza verifico il codice destinatario
065100991115     c                   clear                   ar6ksc
065200990305     c                   IF        I00tpb = 'P'  and  w§dctport = 'A'
065300991115      * sostituito file fnbl4000 con fiar6
065400991116     C                   EVAL      trcar6 = '1'
065500991116     c     KAR6          CHAIN     FIAR6000                           30
065600981201     c                   ENDIF
065700981201      *
065800981005      * Codici mittente/destinario
065900990305     c                   IF        w§dctport = 'A'
066000980716     c                   move      arbccm        VTCksm
066100991115     c                   if        ar6ksc <> *ZEROS
066200991115     c                   movel     ar6ksc        VTCksd
066300981201     c                   else
066400981201     c                   movel     arbksc        VTCksd
066500981201     c                   endif
066600981201      *
066700981005     c                   ELSE
066800981005     c                   move      arbksc        VTCksm
066900981201     c                   clear                   VTCksd
067000981005     c                   ENDIF
067100980716
067200980716      * dati bolla
067300981230     c                   IF        §dctport = 'F'
067400981230     c                   movel(p)  'FRANCO'      VTDcbo
067500981230     c                   ELSE
067600981230     c                   movel(P)  'ASSEGNATO'   VTDcbo
067700981005     c                   ENDIF
067800980716     c                   movel     arbrsm        VTCrsm
067900980716     c                   move      arbcam        VTCcam
068000980716     c                   movel     arblom        VTClom
068100980716     c                   movel     arbprm        VTCprm
068200980716     c                   movel     arbnzm        VTCnam
068300980716     c                   movel     arbrsd        VTCrsd
068400980716     c                   move      arbcad        VTCcad
068500980716     c                   movel     arblod        VTClod
068600980716     c                   movel     arbprd        VTCprd
068700980716     c                   movel     arbnzd        VTCnad
068800981113     c                   z-add     arbncl        VTCncl                         * colli
068900981113     c                   z-add     arbpkf        VTCpkf                         * peso
069000981113     c                   movel     arbfvf        VTCfvf                         * flag volume
069100981113     c                   z-add     arbvlf        VTCvlf                         * volume
069200981117     c                   movel     dctnas        VTCnas                         * natura merce
069300990215     c                   movel     arbrmn        VTCrmn                         * riferimento mitten
069400981113
069500981126      * Flag tipo gestione
069600020124     c                   select
069700020124     c                   when      dctfpr = 'T'
069800981113     c                   eval      v1dfpr = 'LIQUIDAZIONE TRANSATTIVA'
069900020124     c                   when      dctfpr = 'P' and §dctpfra <> 'F'
070000020124     c                   eval      v1dfpr = 'PRATICA ASSICURATIVA'
070100020124     c                   when      dctfpr = 'P' and §dctpfra = 'F'
070200060412     c                   eval      v1dfpr = 'PRATICA ASSICURAT.NO PROG.LIQU.'
070300020124     c                   endsl
070400981126      * Gestione con Mittente / Destinatario
070500981113     c                   if        dctptr = 'M'
070600981113     c                   eval      v1dptr = 'MITTENTE'
070700981113     c                   endif
070800981113     c                   if        dctptr = 'D'
070900981113     c                   eval      v1dptr = 'DESTINATARIO'
071000981113     c                   endif
071100981113
071200981113      * RIVALSA  SI/NO
071300981214     c                   setoff                                       16
071400981113     c                   movel     'R'           Wtrc
071500981201     c     KDKA          CHAIN     FNDKA000                           30
071600981113      *
071700990909     c                   IF        *IN30 = *OFF  and  DKAatb <> 'A'
071800981214     c                   seton                                        16
071900981214     c                   movel     dkarag        V1CASS
072000981117     c                   seton                                        09
072100981113     c                   ENDIF
072200981113
072300981113      * CLIENTE DA CONTATTARE
072400981214      * se non c'è rivalsa e per non vedere un buco vuoto nella videata accendo indicatore
072500981214      * di visualizzazione Beneficiario alternativo
072600981214
072700981214     c  N16              seton                                        15
072800981214
072900981113     c                   IF        DCTKSC > *ZEROS
073000981113     c                   z-add     dctksc        v1cksc
073100981113     c                   move      v1cksc        wclico
073200981113      *   se codificato decodifico da p.d.c.
073300981113     C                   IF        wclico <> 8888
073400981113     c                   clear                   ACOrag
073500981113     c                   z-add     DCTksc        Wksc
073600981113     c                   exsr      CTR_cnaco
073700981113     c  N30              movel     ACOrag        V1Dksc
073800981113     c                   ELSE
073900981113      *   se non codificato decodifico da anagrafica clienti c.a.
074000981113     c                   movel     'C'           Wtrc
074100981113     c     KDKA          CHAIN     FNDKA000                           30
074200981117     c                   seton                                        08
074300981113     c                   movel     dkarag        v1dksc
074400981113     c                   ENDIF
074500981113     c                   ENDIF
074600981113
074700981113      * Da file descrizioni: nominativo persona da contattare
074800981113     c                   movel     'C'           ktpd
074900981113     c                   movel(P)  DS_numca      knkt
075000981113     c                   movel     'N'           kstd
075100981113     c                   clear                   knks
075200981113     c                   movel     'D'           ktrc
075300981113     c                   exsr      chadcs
075400981113     c  n30              movel     dcsnot        V1Dnoc
075500980717
075600980717      * FASE C.A.
075700981116      * SE C.A. chiusa non decodifico la fase
075800981116     c                   if        dctdch > 0
075900981116     c                   seton                                        19
076000981116     c     *iso          move      DCTdch        Wdataeur
076100981116     c     *eur          movel     Wdataeur      V1Cdch
076200981116     c* decodifico se c'è la causale di chiusura
076300981116     c                   if        dctcch <> *blanks
076400981116     c                   eval      v1ccch = dctcch
076500981116      *
076600981116     c                   clear                   TIBS02DS
076700981116     C                   MOVEL     'C'           t02mod
076800981116     C                   MOVEL     knsif         t02sif
076900981116     C                   MOVEL     'CCH'         t02cod
077000981116     C                   MOVEL(P)  dctcch        t02ke1
077100981116      *
077200981116     C                   CALL      'TIBS02R'
077300981116     C                   PARM                    KPJBA
077400981116     C                   PARM                    TIBS02DS
077500981116      *
077600981116     C                   if        t02err = *BLANKS
077700981116     C                   MOVEL     T02UNI        Dcch
077800981116     C                   MOVEL     §CCHdesc      V1Dcch
077900981116     C                   endif
078000981116     c                   endif
078100990329     c*
078200990329     c* controllo se è già stata fatta almeno un'estrazione o il pagamento
078300990329     c                   if        §dctpaga = 'C'
078400990329     c                   seton                                        39        estratto
078500990329     c                   endif
078600990329     c*
078700990329     c                   if        §dctpaga = 'P'
078800990329     c                   seton                                        38        in pagamento
078900990329     c                   endif
079000981116     c*
079100981116     c                   else
079200981116     c*
079300981116     c                   setoff                                       19
079400981116     c*
079500981116     C                   z-add     dctfca        V1Cfca
079600980717     c                   CLEAR                   FIDN05DS
079700980717     c                   movel     'C'           I05MOD
079800981113     c                   z-add     V1Cfca        I05fca
079900980717     C                   CALL      'FIDN05R'
080000980717     c                   PARM                    KPJBA
080100980717     c                   PARM                    FIDN05DS
080200980717     c                   IF        O05err <> 'E'
080300980717     c                   movel     O05REC        DS_FNDFA
080400981113     c                   movel     DFAdes        V1Dfca
080500980717     c                   ENDIF
080600981116     c*
080700981116     c                   endif
080800980717
080900980717      * P.O. gestione
081000050718     C                   setoff                                       36
081100981116     C                   z-add     dctgfc        V1Cgfc
081200981113     C     V1Cgfc        chain     AZORG                              31
081300980717     C  N31              if        ORGDE5 <> *BLANKS
081400981113     C                   movel     ORGDE5        V1Dgfc
081500980717     C                   else
081600981113     C                   movel     ORGDES        V1Dgfc
081700980717     C                   endif
081800131014      * se C.A. in fase > = 660 verifico se affidata ad A.I.G.
081900050719     c                   If        dctfca >=  wfase  and
082000050718     c                             §dctaffi = 'A'
082100131014     c                   eval      v1dgfc = cost_AIG
082200050718     c                   seton                                        36
082300050718     c                   endif
082400981113
082500981113      * Dati Denuncia
082600981117     c                   if        dctnde > 0
082700981113     C                   eval      v1caad = dctaad
082800981117     C                   movel     dctpod        v1cpod
082900981113     C                   eval      v1cnde = dctnde
083000050718      * data Denuncia
083100981113     c     kdet2         chain     fndet02l                           30
083200981113     C                   Z-ADD     detdsb        G02inv
083300981113     C                   movel     '3'           G02err
083400981113     C                   CALL      'XSRDA8'
083500981113     C                   PARM                    WLBDAT
083600981113     c                   z-add     g02dat        v1cdsb
083700981113     c                   endif
083800981113      * richiesta copia denuncia
083900981113     c                   if        dctrcd = 'S'                                 Rich. copia denuncia
084000990609     c                   eval      v1drcd = ' SI'
084100981113     c                   else
084200990609      * se data invio copia denuncia è diversa da zero significa che è stata già inviata
084300990609     c                   if        dctdic = *zeros
084400990609     c                   eval      v1drcd = ' NO'
084500990609     c                   else
084600990609     c                   eval      v1drcd = 'INV'
084700990609     c                   endif
084800981113     c                   endif
084900981113
085000981113      * Pratica assicurativa
085100981211     C                   eval      v1cdit = dctdit
085200981113     C                   eval      v1cpra = dctpra
085300981113     C                   eval      v1cprn = dctprn
085400981211
085500981211      * Se valorizzata la responsabilità la visualizzo
085600981211     c                   setoff                                       13
085700990510     c                   if        DCTres = 'V'                                 Test respons.INTERNA
085800981214     c                   eval      v1dres = 'VETTORE'
085900981214     c                   seton                                        13
086000981211     c                   endif
086100990510     c                   if        DCTres = 'P'                                 Test respons.INTERNA
086200981214     c                   eval      v1dres = 'PARTNER'
086300981211     c                   seton                                        13
086400981211     c                   endif
086500981113
086600981113      * Quantità Danneggiate o Mancanti
086700981113     c                   z-add     DCTnpz        V1Cnpz                         Pezzi danneggiati
086800981113     c                   z-add     DCTncn        V1Cncn                         Colli danneggiati
086900981113     c                   z-add     DCTpkd        V1Cpkd                         Kg danneggiati
087000981113
087100981113      * Da file descrizioni: descrizione pezzi danneggiati
087200981113     c                   movel     'C'           ktpd
087300981113     c                   movel(P)  DS_numca      knkt
087400981113     c                   movel     'P'           kstd
087500981113     c                   clear                   knks
087600981113     c                   movel     'D'           ktrc
087700981113     c                   exsr      chadcs
087800981113     c  n30              movel     dcsnot        V1Dnpz
087900990104     c                   eval      *in81 = (not *in30 and  *in90)
088000981113
088100981113      * Da file descrizioni: descrizione pezzi presenti
088200981113     c                   movel     'K'           kstd
088300981113     c                   clear                   knks
088400981113     c                   movel     'D'           ktrc
088500981113     c                   clear                   kdim
088600981113     c                   clear                   khim
088700981113     c                   exsr      chadcs
088800981113     c  n30              movel     dcsnot        V1dprs
088900990104     c                   eval      *in82 = (not *in30 and  *in90)
089000981113
089100981113      * Da file descrizioni: motivo del danno
089200981113     c                   movel     'C'           ktpd
089300981113     c                   movel(P)  DS_numca      knkt
089400981113     c                   movel     'M'           kstd
089500981113     c                   clear                   knks
089600981113     c                   movel     'D'           ktrc
089700981113     c                   clear                   kdim
089800981113     c                   clear                   khim
089900981113     c                   exsr      chadcs
090000981113     c  n30              movel     dcsnot        v1dmtv
090100990104     c                   eval      *in83 = (not *in30 and  *in90)
090200981113
090300981113      * Chi Verifica i Dati sul DDT
090400981113     c                   if        DCTtdt = 'P'                                 Test DDT
090500981113     c                   eval      v1dtdt = 'PARTENZA'
090600981113     c                   endif
090700981113     c                   if        DCTtdt = 'A'                                 Test DDT
090800981113     c                   eval      v1dtdt = 'ARRIVO  '
090900981113     c                   endif
091000981113     c                   if        DCTtdt = ' '                                 Test DDT
091100981113     c                   eval      v1dtdt = 'NESSUNO '
091200981113     c                   endif
091300981113
091400981113      * Presentata Riserva
091500981113     c                   if        DCTcrc  = 'S'
091600981113     c                   eval      V1Dcrc  = 'SI'                               Riserva
091700981113     c                   endif
091800981113     c                   if        DCTcrc  <> 'S'
091900981113     c                   eval      V1Dcrc  = 'NO'                               Riserva
092000981113     c                   endif
092100981113
092200981113      * Disposizioni sulla Merce danneggiata
092300981113     c                   if        dctdsd  = 'C'                                Test merce dannegg.
092400981113     c                   eval      v1ddsd = 'CONSEGNATA'
092500981113     c                   endif
092600981113     c                   if        dctdsd  = 'D'                                Test merce dannegg.
092700981113     c                   eval      v1ddsd = 'DISTRUTTA '
092800981113     c                   endif
092900981113     c                   if        dctdsd  = 'R'                                Test merce dannegg.
093000981113     c                   eval      v1ddsd = 'RESA      '
093100981113     c                   endif
093200981124     c                   if        dctdsd  = 'M'                                Test merce dannegg.
093300981124     c                   eval      v1ddsd = 'MAGAZZINO'
093400981124     c                   endif
093500981102
093600981113      * Carico la prima serie di comandi
093700990506     c                   z-add     1             c
093800990506     c                   movea     CMD(1)        v1ccmd
093900981113      *
094000980611     C                   ENDSR
094100981113      ****************************************************************
094200981113      * F7  = INTERROGAZIONE EVNTO
094300981113      ****************************************************************
094400981113     C     SUB_F7        BEGSR
094500981113      *
094600981113     c                   clear                   FIDN06DS
094700981113     c                   eval      i06mod = 'I'
094800981113     c                   eval      i06aae = dctaae
094900981113     c                   eval      i06nev = dctnev
095000981113     c                   movel     fidn06ds      kpjbu
095100981113     c                   call      'FIDN06R'
095200981113     c                   parm                    kpjba
095300981113      *
095400981113     c                   endsr
095500981113      ****************************************************************
095600981113      * F8  = DATI ANAGRAFICI CLIENTE DA CONTATTARE
095700981113      ****************************************************************
095800981113     C     SUB_F8        BEGSR
095900981113      *
096000981113     c                   movel     'C'           Wtrc
096100981113     c                   exsr      MANCLI
096200981113      *
096300981113     c                   endsr
096400981113      ****************************************************************
096500981113      * F8 -2°- DATI ANAGRAFICI INTESTATARIO PROGETTO DI LIQUIDAZIONE
096600981113      ****************************************************************
096700981113     C     SUB2_F8       BEGSR
096800981113      *
096900150630     c                   If        $Trc_I
097000981113     c                   movel     'I'           Wtrc
097100150630     c                   Else
097200150630     c                   eval      Wtrc = 'P'
097300150630     c                   EndIf
097400981113     c                   exsr      MANCLI
097500981116     c* se indicatore 30 acceso emetto msg errore
097600981116     c                   if        *in30 = *on
097700150630     c                   If        $Trc_I
097800981116     c                   movel     msg(6)        v2cmsg
097900150630     c                   Else
098000150630     c                   movel     Msg(8)        V2Cmsg
098100150630     c                   EndIf
098200981116     c                   seton                                        28
098300981116     c                   endif
098400981113      *
098500981113     c                   endsr
098600981113      ****************************************************************
098700981113      * F9  = DATI ANAGRAFICI DELLA RIVALSA
098800981113      ****************************************************************
098900981113     C     SUB_F9        BEGSR
099000981113      *
099100981113     c                   movel     'R'           Wtrc
099200981113     c                   exsr      MANCLI
099300981113      *
099400981113     c                   endsr
099500981113      ****************************************************************
099600981113      * F10 = GESTIONE NOTE TESTO
099700981113      ****************************************************************
099800981113     C     SUB_F10       BEGSR
099900981113      *
100000981113     C                   clear                   FIDN10DS
100100981113     c                   z-add     Wdataca       i10dta
100200981113     c                   movel     'D'           i10trc
100300981113     c                   movel     'C'           i10tpd
100400981113     c                   movel     'T'           i10std
100500981113      *
100600981113     c                   movel     'I'           i10flm
100700981113     c                   movel     DS_numca      i10nkt
100800981113      *
100900981113     c                   movel(P)  FIDN10DS      kpjbu
101000981113     C                   CALL      'FIDN10R'
101100981113     C                   PARM                    kpjba
101200981113      *
101300981113     c                   ENDSR
101400980615      ****************************************************************
101500981113      *  F15    VIDEATA IMPORTI CA
101600980615      ****************************************************************
101700981113     C     SUB_F15       BEGSR
101800981113
101900981221      *
102000981221      * Importo contrassegno
102100981221     c                   select
102200981221      * Controllo se esiste l'importo del contrassegno
102300051114     c                   when      i00tpb <>'S'                                 * Partenza
102400051114     c     kbol01        chain     fiar9000                           30
102500981221     c                   when      i00tpb = 'S'  and tasfca = 'S'               * Sede + F.C/A = 'S'
102600981221     c     kbol01        chain     tncsb000                           30
102700981221     c                   endsl
102800981221      * se record trovato accendo l'indicatore 06 che evidenzia l'importo
102900981221     c                   if        not *in30  and ar9cas > 0
103000981221     c                   seton                                        06
103100981221     c                   z-add     ar9cas        v2ccas
103200981221     c                   move      ar9vca        v2cvca
103300981221     c                   endif
103400981221      *
103500981221      * Controllo se c'è stata fattura immediata
103600981221     c                   select
103700981230     c                   when      i00tpb = 'A' and §dctport = 'A'
103800981229     c* controllo se bolla legata a causa di un cambio di porto
103900110503     c****               if        arbcca = '9'
104000110503     c                   if        §DCTcca = '9'
104100981229     c* aggancio il file legame bolla
104200981229     c     kbol01        chain     fnlbl02l                           30
104300991116     C                   EVAL      trcar6 = '1'
104400991116     c  n30kar6lb        chain     fiar6000                           30
104500981229     c                   else
104600991116     C                   EVAL      trcar6 = '1'
104700991116     c     kar6          chain     fiar6000                           30
104800981229     c                   endif
104900981229     c*
105000981221     c                   if        not  *in30 and ar6nft > 0  and ar6dft > 0
105100981221     c                   seton                                        05
105200981222     c                   z-add     ar6ift        v2cfti
105300991104     c                   move      ar6div        v2cvfi
105400010912     c                   if        V2Cfti <> 0 and V2Cvfi = *blanks
105500010912     c                   movel     'ITL'         V2Cvfi
105600010912     c                   endif
105700981221     c                   endif
105800981221      *
105900981230     c                   when      i00tpb = 'S' and §dctport = 'A'
106000981221      *
106100991104     c                   move      arbksc        w0040
106200981221     c                   if        w0040 = 9999  and tasnft > 0
106300981221     c                   seton                                        05
106400981222     c* calcolo il valore della fattura immediata
106500981222     c                   exsr      CALCOLO
106600981222     c*
106700981221     c                   endif
106800981221      *
106900981221     c                   endsl
107000990319      *
107100990319      * Importo da assicurare solo se ho la spedizione e in sede se non impostato dalla fatturazione
107200990319     c                   SELECT
107300990319     c                   WHEN      SIMFEL <> *zeros
107400990319     C                   z-add     ARBIAS        V2Cias               04        Imp. da assicurare
107500990319     C                   movel     ARBVAS        V2Cvas                           Valuta
107600990319     c                   WHEN      TASFCM <> 'F'
107700990319     C                   z-add     ARBIAS        V2Cias               04        Imp. da assicurare
107800990319     C                   movel     ARBVAS        V2Cvas                           Valuta
107900990319     c                   ENDSL
108000010912     c*------------------
108100010912     c                   if        V2Cias <> 0 and V2Cvas = *blanks
108200010912     c                   movel     'ITL'         V2Cvas
108300010912     c                   endif
108400010912     c*------------------
108500981020
108600981112      * Importo massimo risarcibile
108700981020     c                   z-add     DCTipv        V2Cipv
108800981020     c                   movel     DCTvpv        V2Cvpv                           Valuta
108900981112     c* Tipo inserimento
109000981020     c                   IF        DCTfpv = 'M'
109100981116     c                   eval      V2Dfpv = 'INSERITO MANUALMENTE'
109200981112     c                   Else
109300981020     c                   IF        DCTfpv = 'C'
109400981116     c                   eval      V2Dfpv = 'CALCOLATO'
109500981020     c                   ENDIF
109600981020     c                   ENDIF
109700981113
109800981113      * Aggancio file Liquidazione C.A.
109900981113     c     kdcd          CHAIN     FNDCL000                           30
110000981113     c                   IF        *IN30 = *OFF
110100981113
110200981113      * Importo richiesto
110300981113     c                   z-add     DClipr        V2Cipr
110400981113     c                   movel     dclvpr        v2cvpr
110500981113      * Importo trattenuto
110600981113     c                   z-add     DCLipt        V2Cipt
110700981113     c                   movel     DCLvpt        V2Cvpt
110800981113     c                   if        DCLdpt > *zeros
110900981113     c     *iso          move      DCLdpt        Wdataeur
111000981113     c     *eur          movel     Wdataeur      V2Cdpt
111100981113     c                   endif
111200981113     c                   ENDIF
111300990315     c*
111400000131     c* dati relativi la LIQUIDAZIONE TRANSATTIVA e PRATICA Assicurativa
111500990315      *
111600990315     c                   if        dctfpr = 'T'
111700000131     c*
111800000131     c                   setoff                                       72
111900000131     c                   seton                                        73
112000000131     c                   else
112100000131     c                   seton                                        72
112200000131     c                   setoff                                       73
112300000131     c*
112400000131     c                   endif
112500020205
112600020205     c                   if        §dctpfra = 'F'
112700020205     c                   seton                                        74
112800020205     c                   endif
112900990315      *
113000990315      * Data emissione Assegno
113100990315     c                   if        DCLdea > *zeros
113200990315     c     *iso          move      DCLdea        Wdataeur
113300990315     c     *eur          movel     Wdataeur      V2Cdea
113400990315     c                   endif
113500990315      *
113600990315      * Numero  Assegno
113700161205      * o Bonifico
113800161205     c                   clear                   V2Dasn
113900161205     c                   SELECT
114000161205     c                   WHEN      dclasn = *blanks
114100161205     c                   eval      V2Dasn = 'Ass/Bon'
114200161205     c                   WHEN      %subst(dclasn:1:1) = 'B'
114300161205     c                   eval      V2Dasn = 'Bonifico'
114400161206     c                   OTHER
114500161205     c                   eval      V2Dasn = 'Assegno'
114600161205     c                   ENDSL
114700990315     c                   movel     dclasn        v2casn
114800990315      *
114900990315      * ABI / CAB
115000990315     c                   movel     dclabi        v2cabi
115100990315     c                   movel     dclcab        v2ccab
115200990315      *
115300990315      * Decodifica ABI / CAB
115400990315     c                   exsr      SUB_BANCA
115500150630      *
115600150630      * -?Beneficiario pagamento C.A.?
115700151111      *  ?(SE Liquidazione Transattiva o Pratica in Franchigia)?
115800151112     c                   If        ( *in73  or  *in74 )  and  *in38
115900150630     c                   movel     'P'           Wtrc
116000150630     c     KDKA          chain     FNDKA000
116100150630     c                   if        %found(FNDKA01L)  and  DKAatb <> 'A'
116200150630     c                   movel     DKAksc        V2Cpli
116300150630     c                   movel     DKArag        V2Dpli
116400150630     c                   seton                                        18
116500150630     c                   endif
116600150630     c                   EndIf
116700990315      *
116800981113      * Intestatario Progetto Liquidazione
116900150630     c                   clear                   $Trc_I
117000981113     c                   movel     'I'           Wtrc
117100981113     c     KDKA          CHAIN     FNDKA000                           30
117200990909     c                   IF        *IN30 = *OFF  and  DKAatb <> 'A'
117300990909     c                   movel     dkarag        v2dpli
117400990909     c                   movel     dkaksc        v2cpli
117500990909     c                   seton                                        18
117600150630     c                   eval      $Trc_I = *on
117700990909     c                   ENDIF
117800150630      *
117900981113      * Data stampa progetto di liquidazione
118000981113     c                   if        DCLdds > *zeros
118100981113     c     *iso          move      DCLdds        Wdataeur
118200981113     c     *eur          movel     Wdataeur      V2Cdds
118300981113     c                   endif
118400981113      * Data accettazione progetto di liquidazione
118500981113     c                   if        DCLdpl > *zeros
118600981113     c     *iso          move      DCLdpl        Wdataeur
118700981113     c     *eur          movel     Wdataeur      V2Cdpl
118800981113     c                   endif
118900981113
119000990315      * Importo liquidato  se è pratica è in campo se è liquidazione in un altro
119100990315     c                   if        dctfpr = 'T'
119200981113     c                   z-add     DClipl        V2Cipl
119300981113     c                   movel     dclvpl        v2cvpl
119400990315      *
119500990315     c                   else
119600990315      *
119700990315     c                   z-add     DClipl        V2Cipp
119800990315     c                   movel     dclvpl        v2cvpp
119900000131      *
120000000131     c* se pratica trattenuta da cliente e pagamento già avvenuto imposto come importo
120100000131     c* liquidato la differenza
120200010918     c                   if        dclipl <= dclipt
120300000131     c                   z-add     0             v2cipl
120400010918     c                   clear                   v2cvpl
120500000131     c                   else
120600000131     c                   eval      v2cipl = dclipl - dclipt
120700000131     c                   movel     dclvpl        v2cvpl
120800000131     c                   endif
120900000131     c*
121000990315     c                   endif
121100990315      *
121200981113      * Data valuta pagamento
121300981113     c                   if        DCLvlp > *zeros
121400981113     c     *iso          move      DCLvlp        Wdataeur
121500981113     c     *eur          movel     Wdataeur      V2Cvlp
121600981113     c                   endif
121700981116      * Carico le Note del calcolo progetto di liquidazione
121800981116     c                   if        dcldds > *zeros
121900981116      * cerco le note
122000981116     c                   movel     'C'           ktpd
122100981116     c                   movel(p)  DS_numca      knkt
122200981116     c                   movel     'C'           kstd
122300981116     c                   clear                   kdim
122400981116     c                   clear                   khim
122500981116     c                   clear                   knks
122600981116     c                   movel     'D'           ktrc
122700050928     c                   z-add     1             kpno
122800981116     c*
122900990126     c                   dow       kpno <  7
123000981116     c*
123100981116     c     kdcspno       chain     fndcs01l                           30
123200981116     c*
123300981116     c                   if        *in30 = *off
123400981116     c*
123500990126     c* aggiorno la schiera delle note
123600990126     c                   z-add     kpno          x                 2 0
123700990126     c*
123800990205     c                   if        x <= 4
123900990126     c                   movel(P)  dcsnot        tno(x)
124000990126    1c                   endif
124100990126     c*
124200990126    1c                   endif
124300981116     c                   add       1             kpno
124400990126     c  n30              enddo
124500990126     c*
124600990126     c                   movea     tno           v2cpl1
124700990205     c                   movea     tno(3)        v2cpl2
124800990126     c**
124900981116     c                   endif
125000990409      * Aggancio il file Mandato Assicurativo
125100990409      *
125200990409     c     kdcd          chain     fndpt01l                           70
125300990504     c                   IF        *in70 = *off  and  DPTatb = 'A'
125400990504     c                   eval        *in70 = *on
125500990504     c                   ENDIF
125600131014      * Aggancio il file Richiesta rimborso ad A.I.G.
125700050909      *
125800050909     c     kdcd          chain     fndra01l
125900050909     c                   IF        %found and DRAatb = ' '
126000050929     c                                    and DRAdrr > *zeros
126100050909     c                   eval        *in47 = *on
126200050909      * verifico se CA esclusa dal rimborso
126300050909     c                   eval        *in48 = DRAesc = 'E'
126400050909     c                   else
126500050909     c                   eval        *in47 = *off
126600050909     c                   eval        *in48 = *off
126700050909     c                   ENDIF
126800131014      * verifico se pratica affidata ad A.I.G.
126900050909      *
127000050909     c                   eval      *in17 = (§dctaffi = 'A')
127100020206
127200020206      * Se pratica assicurativa in Franchigia non devo far vedere i campi valorizzati relativi al
127300020206      * progetto di liquidazione
127400150630      *?...Neanche se Liquidazione Transattiva!?
127500150630      *?Ma il cliente (V2CPLI) è il Beneficiario, in questi casi (e va mostrato)?
127600150630     c*//                if        *in74
127700150630     c*//                clear                   v2cpli
127800150630     c*//                clear                   v2dpli
127900150630     c                   if        *in73  or  *in74
128000020206     c                   clear                   v2cdds
128100020206     c                   clear                   v2cdpl
128200020206     c                   clear                   v2cipp
128300020206     c                   clear                   v2cvpp
128400020206     c                   clear                   v2cvlp
128500020211     c                   clear                   v2cpl1
128600020211     c                   clear                   v2cpl2
128700020206     c                   endif
128800981113      *
128900981113      * GESTIONE FORMATO VIDEO
129000981113     c                   do        *hival
129100981113      *
129200981113     c                   exfmt     fi01d02
129300981116     c                   setoff                                       28
129400981116     c                   clear                   v2cmsg
129500981113
129600981113      * K12=VIDEATA PRECEDENTE
129700981113     c                   if        *INKL = *ON
129800981113     c                   leave
129900981113     c                   endif
130000981116
130100981116      * K8=GESTIONE DATI INTESTATARIO PROGETTO DI LIQUIDAZIONE
130200150630      *?o F8=BENEFICIARIO?
130300981116     c                   if        *INKH = *ON
130400981116     c                   exsr      SUB2_F8
130500981116     c                   endif
130600981006      *
130700990409
130800990409      * K9=VISUALIZZO MANDATO ASSICURATIVO
130900990409     c                   if        *INKI = *ON
131000990409     C*
131100990409      * imposto DS per richiamo pgm di visualizzazione mandato
131200990409     c                   clear                   fidn30ds
131300990409     c                   z-add     dptksc        I30ksc
131400990409     c                   z-add     dptctr        I30ctr
131500990409     c                   z-add     dptprg        I30prg
131600990409     c                   movel     dptftc        I30ftc
131700990409     c                   movel     dptfvm        I30fvm
131800990409     c                   z-add     dptvlm        I30vlm
131900990409     c                   movel     dptvvm        I30vvm
132000990409      *
132100990409     c                   call      'FIDN30R'
132200990409     c                   parm                    KPJBA
132300990409     c                   parm                    FIDN30DS
132400990409      *
132500990409     c                   endif
132600050928
132700131014      * K2=VISUALIZZAZIONE DISPOSIZIONI A.I.G.
132800050928     c                   if        *INKB = *ON
132900050928     c                   exsr      SUB2_F2
133000050928     c                   endif
133100050909
133200131014      * K11=GESTIONE DATI RICHIESTA RIMBORSO AD A.I.G.
133300050909     c                   if        *INKK = *ON
133400050909     c                   exsr      SUB2_F11
133500050909     c                   endif
133600050909      *
133700990409      *
133800981113     c                   ENDDO
133900981014
134000981014     C                   ENDSR
134100981222      ****************************************************************
134200981222      * CALCOLO TOTALE FATTURA IMMEDIATA
134300981222      ****************************************************************
134400981222     C     CALCOLO       BEGSR
134500981222      *
134600991104      * calcolo il totale fattura immediata richiamando FNLV16
134700991104     C                   clear                   fnlv16ds
134800991104     C                   clear                   dtasv
134900991104     C                   eval      d17tla = 'L'
135000991104     C                   eval      d17tbo = '0'
135100991104     C                   eval      d17crb = 'V'
135200991104     C                   eval      d17aas = arbaas
135300991104     C                   eval      d17lns = arblnp
135400991104     C                   eval      d17nrs = arbnrs
135500991104     C                   eval      d17nsp = arbnsp
135600991104     C                   eval      d17por = taspor
135700991104     C                   eval      d17tbs = tastbl
135800991104     C                   eval      d17fim = 'F'
135900991104     C                   call      'FNLV16R'
136000991104     C                   parm                    fnlv16ds
136100991104     C                   parm                    dtasv
136200991104      * se non ci sono errori carico il totale fattura
136300991104     C                   if        o17err = ' '
136400991104     c                   z-add     o17ift        v2cfti
136500990806     C                   EVAL      V2CVFI = TASDIV
136600010912     c                   if        V2Cfti <> 0 and V2Cvfi = *blanks
136700010912     c                   movel     'ITL'         V2Cvfi
136800010912     c                   endif
136900991104     c                   endif
137000990806
137100981222     c*
137200981222     c                   endsr
137300981113      ****************************************************************
137400981113      * F18 = GESTIONE NOTE
137500981113      ****************************************************************
137600981113     C     SUB_F18       BEGSR
137700981113      *
137800981113     C                   clear                   FIDN10DS
137900981113     c                   z-add     Wdataca       i10dta
138000981113     c                   movel     'N'           i10trc
138100981113     c                   movel     'C'           i10tpd
138200981113      *
138300981113     c                   movel     'I'           i10flm
138400981113     c                   movel     DS_numca      i10nkt
138500981113     c                   movel     dctfca        i10nks
138600981113      *
138700981113     c                   movel(P)  FIDN10DS      kpjbu
138800981113     C                   CALL      'FIDN10R'
138900981113     C                   PARM                    kpjba
139000981113      *
139100981113     c                   ENDSR
139200990128      **********************************************************************
139300990128      * F19 = VISUALIZZA BOLLA
139400990128      **********************************************************************
139500990128     C     SUB_F19       BEGSR
139600990128      *
139700990128     c                   SELECT
139800990128      * partenza
139900990128     c                   WHEN      I00tpb = 'P'
140000990129     c                   clear                   DSVBP_f03
140100990129     c                   movel     'C'           DSVBP_flg
140200990128     C                   z-add     dctaas        DSVBP_aas
140300990128     C                   z-add     dctlnp        DSVBP_lnp
140400990128     C                   z-add     dctnrs        DSVBP_nrs
140500990128     C                   z-add     dctnsp        DSVBP_nsp
140600990128     c                   movel(P)  RSUT          DSVBP_rsut
140700990128     C                   z-add     simfel        DSVBP_sim
140800990128     C                   movel(P)  DS_visbolp    KPJBU
140900990128     C                   call      'FNLS05R'
141000990128     C                   parm                    KPJBA
141100990128      *
141200990128      * arrivo
141300990128     c                   WHEN      i00tpb = 'A'
141400990128     c                   clear                   DS_ricbolA
141500990128     C                   z-add     dctaas        DSRBA_aas
141600990128     C                   z-add     dctlnp        DSRBA_lnp
141700990128     C                   z-add     dctnrs        DSRBA_nrs
141800990128     C                   z-add     dctnsp        DSRBA_nsp
141900990129     C                   movel     'C'           DSRBA_flg
142000990128     C                   movel(P)  DS_ricbolA    KPJBU
142100990128     C                   call      'FNLR36R'
142200990128     C                   parm                    KPJBA
142300990128      * sede
142400990128     c                   WHEN      i00tpb = 'S'
142500990806     c                   clear                   TNSB50DS
142600990806     C                   movel     'D05'         I50OP0
142700990806     C                   z-add     dctaas        D50aas
142800990806     C                   z-add     dctlnp        D50lnp
142900990806     C                   z-add     dctnrs        D50nrs
143000990806     C                   z-add     dctnsp        D50nsp
143100990806     C                   movel(P)  TNSB50DS      KPJBU
143200990806     C                   call      'TNSB50R'
143300990128     C                   parm                    KPJBA
143400990128      *
143500990128     c                   ENDSL
143600990128      *
143700990128     C                   ENDSR
143800981113      **********************************************************************
143900981113      * LETTURA RECORD FILE DESCRIZIONI
144000981113      **********************************************************************
144100981113     C     Chadcs        BEGSR
144200981113      *
144300990104     c                   setoff                                           90
144400990104      *
144500981113     c     kdcs          setll     fndcs01l
144600981113     c     kdcs          reade     fndcs01l                               30
144700990104     c                   if        not *in30
144800990104     c                   eval      wdcsnot = dcsnot
144900981113      *
145000990104     c* controllo se esistono altre righe
145100981113     c     kdcs          reade     fndcs01l                               30
145200990104     c  n30              seton                                            90
145300990104     c                   eval      dcsnot = wdcsnot
145400990104     c                   setoff                                           30
145500990104     c                   endif
145600981113      *
145700981113     c                   endsr
145800980615      **********************************************************************
145900980716      *  CONTROLLI VIDEATA 1
146000980615      **********************************************************************
146100971020     C     CTRD01        BEGSR
146200981113      *         V I S U A L I Z Z O     D E S C R I Z I O N I
146300980626
146400981020      * DESCRIZIONE ANOMALIA
146500981113     c                   IF        V1Cfag = '+'
146600980619      * Richiamo pgm gestione descrizioni
146700981020     C                   CLEAR                   FIDN10DS
146800980619      *
146900981113     c                   movel     'I'           i10flm
147000981029     c                   movel     DS_numca      i10nkt
147100980720     c                   z-add     Wdataca       i10dta
147200971030     c                   movel     'C'           i10tpd
147300971030     c                   movel     'A'           i10std
147400971030     c                   movel     'D'           i10trc
147500981020     c                   movel(P)  FIDN10DS      kpjbu
147600980619      *
147700980619     C                   CALL      'FIDN10R'
147800971030     C                   PARM                    kpjba
147900980619      *
148000981113     c                   clear                   v1cfag
148100981113     c                   endif
148200981113
148300981113      * DESCRIZIONE PEZZI DANNEGGIATI/MANCANTI
148400981113     c                   IF        V1Cfpz = '+'
148500981113      * Richiamo pgm gestione descrizioni
148600981113     C                   CLEAR                   FIDN10DS
148700981113     C                   MOVEL     'I'           I10FLM
148800981113     c                   MOVEL     DS_numca      i10nkt
148900981113     c                   z-add     Wdataca       i10dta
149000981113     c                   movel     'C'           i10tpd
149100981113     c                   movel     'P'           i10std
149200981113     c                   movel     'D'           i10trc
149300981113     c                   movel(P)  FIDN10DS      kpjbu
149400981113      *
149500981113     C                   CALL      'FIDN10R'
149600981113     C                   PARM                    kpjba
149700981113      *
149800981113     c                   clear                   v1cfpz
149900981113     c                   endif
150000981113
150100981113      * DESCRIZIONE PEZZI PRESENTI
150200981113     c                   IF        V1Cfps = '+'
150300981113      * Richiamo pgm gestione descrizioni
150400981113     C                   CLEAR                   FIDN10DS
150500981113     C                   MOVEL     'I'           I10FLM
150600981113     c                   MOVEL     DS_numca      i10nkt
150700981113     c                   z-add     Wdataca       i10dta
150800981113     c                   movel     'C'           i10tpd
150900981113     c                   movel     'K'           i10std
151000981113     c                   movel     'D'           i10trc
151100981113     c                   movel(P)  FIDN10DS      kpjbu
151200981113      *
151300981113     C                   CALL      'FIDN10R'
151400981113     C                   PARM                    kpjba
151500981113      *
151600981113     c                   clear                   v1cfps
151700981113     c                   endif
151800980626
151900980619      * MOTIVO DEL DANNO
152000981113     c                   IF        V1Cfmt = '+'
152100980623      * Richiamo pgm gestione descrizioni
152200981020     C                   CLEAR                   FIDN10DS
152300981113     C                   MOVEL     'I'           I10FLM
152400981029     c                   MOVEL     DS_numca      i10nkt
152500980720     c                   z-add     Wdataca       i10dta
152600980623     c                   movel     'C'           i10tpd
152700980609     c                   movel     'M'           i10std
152800980609     c                   movel     'D'           i10trc
152900981020     c                   movel(P)  FIDN10DS      kpjbu
153000980626      *
153100980722     C                   CALL      'FIDN10R'
153200971030     C                   PARM                    kpjba
153300980626      *
153400981113     c                   clear                   v1cfmt
153500981113     c                   endif
153600980724
153700020827     C                   ENDSR
153800981201      *****************************************************************
153900981201      *  F13 = DOCUMENTI RICHIESTI
154000981201      *****************************************************************
154100981201     C     SUB_F13       BEGSR
154200981201      *
154300981201      * Carico i documenti che sono stati richiesti
154400981201     c                   exsr      cardcc
154500981201      *
154600981201     c* controllo se è stato fatto almeno un sollecito e quando
154700981201     C* controllo se l'ultima fase eseguita è un sollecito oppure no
154800981201     c     kdcd          setgt     fndcf01l
154900981201     c     kdcd          readpe    fndcf01l                               32
155000981201     c* se il record esiste  controllo se la fase letta è un sollecito
155100981201     c                   dow       not *in32
155200981201     c                   setoff                                           11
155300981201     c     DCFFCA        LOOKUP    SOL                                    11
155400981201     c* 11 acceso visualizzo ANCHE LA DATA
155500981201     c* Data sollecito
155600981201     C                   CLEAR                   WLBDAT
155700981201     C                   Z-ADD     dcfdfc        G02inv
155800981201     C                   movel     '3'           G02err
155900981201     C                   CALL      'XSRDA8'
156000981201     C                   PARM                    WLBDAT
156100981201     c                   z-add     g02dat        v5culs
156200981201     c   11              leave
156300981201     c*
156400981201     c*
156500981201     c     kdcd          readpe    fndcf01l                               32
156600981201     c                   enddo
156700990216     c*
156800990216     c                   setoff                                           67
156900981201     c* emetto la videata
157000981201     c                   z-add     1             nrr5
157100981201     c                   z-add     1             v5cnrr
157200981201     c                   do        *hival
157300981201     C                   WRITE     FI01Z03
157400981201     C                   WRITE     FI01T05
157500981201     C                   EXFMT     FI01C05
157600981201      * F12=RITORNO
157700981201     C                   IF        *inkl = *on
157800981201     C                   leave
157900981201     C                   endif
158000981201     C*
158100981201     C                   ENDDO
158200981201      *
158300981201     C                   ENDSR
158400981201      *****************************************************************
158500981201      *  RIEMPIMENTO SUBFILE DOCUMENTI RICHIESTI
158600981201      *****************************************************************
158700981201     C     CARDCC        BEGSR
158800981201      *
158900981201     C                   setoff                                       51
159000981201     C                   WRITE     FI01C05
159100981201     C                   seton                                        5051
159200981201      *
159300981201     C                   move      *ZEROS        NRR5
159400981201
159500981201     c     kdcd          setll     fndcc000
159600981201     c     kdcd          reade     fndcc000                               31
159700981201      *
159800981201     C     *IN31         DOWEQ     *OFF
159900981201      *
160000990715     c                   if        dccatb = ' '
160100990715      *
160200981201     c                   if        dccdrz > 0
160300981201     c     *iso          move      DCcdrz        Wdataeur
160400981201     c     *eur          movel     Wdataeur      V5sdrz
160500981201     c                   else
160600981201     c                   clear                   v5sdrz
160700981201     c                   endif
160800981201      *
160900981201     c                   setoff                                       10
161000981201     C                   MOVEl     dcccdd        v5scdd
161100981201     c*
161200981201     c                   clear                   tibs02ds
161300981201     c                   clear                   dcdd
161400981201     C                   MOVEL     'C'           t02mod
161500981201     C                   MOVEL     knsif         t02sif
161600981201     C                   MOVEL     'CDD'         t02cod
161700981201     C                   MOVEL(P)  dcccdd        t02ke1
161800981201      *
161900981201     C                   CALL      'TIBS02R'
162000981201     C                   PARM                    KPJBA
162100981201     C                   PARM                    tibs02ds
162200981201      *
162300981201    1C                   if        t02err = *BLANKS
162400981201     C                   MOVEL     T02UNI        DCDD
162500981201     c                   movel     §CDDdesc      v5sdoc
162600981201     c* controllo quali sono i dati previsti per questo documento
162700981201     c                   if        §cddpnum = 'S'
162800981201     c* e se è già pervenuto altrimenti non faccio vedere i campi vuoti
162900981201     c                             and dccdrz > 0
163000981201     c                   seton                                        10
163100981201     c                   else
163200981201     c                   setoff                                       10
163300981201     c                   endif
163400981201    1C                   endif
163500981201      * recupero i dati dal file fndcl01l
163600981201     c                   if        *in10
163700981201     c     kdcd          chain     fndcl01l                           30
163800981201     c                   if        not *in30
163900981201      * dta documento
164000981201     C                   CLEAR                   WLBDAT
164100981201     C                   Z-ADD     dclddc        G02inv
164200981201     C                   movel     '3'           G02err
164300981201     C                   CALL      'XSRDA8'
164400981201     C                   PARM                    WLBDAT
164500981201     c                   z-add     g02dat        v5sddc
164600981201      * numero documento
164700981201     c                   z-add     dclndc        v5sndc
164800981201      * importo + valuta
164900981201     c                   z-add     dclipr        v5sipr
165000981201     c                   move      dclvpr        v5svpr
165100981201      *
165200981201     c                   else
165300981201     c                   setoff                                       10
165400981201    1C                   endif
165500981201      *
165600981201    1C                   endif
165700981201      *
165800981201     C                   add       1             NRR5
165900981201     C                   WRITE     FI01S05
166000990715      *
166100990715     c                   endif
166200981201      *
166300981201     c     kdcd          reade     fndcc000                               31
166400981201     C                   ENDDO
166500981201      *
166600981201     c                   eval      *IN50 = ( nrr5 <> *zeros)
166700981201      *
166800981201     C                   ENDSR
166900990129      *****************************************************************
167000990129      *  F14 = RESPONSABILITA'
167100990129      *****************************************************************
167200990129     C     SUB_F14       BEGSR
167300080715     c                   clear                   v6cmsg
167400990129     c*
167500990129     c* se manuale
167600990129     c                   if        dctcar = 'M'
167700990129     c                   seton                                        62
167800990129     c                   endif
167900990129     c* se automatico
168000990129     c                   if        dctcar = 'A'
168100990129     c                   seton                                        63
168200990129     c                   endif
168300990129     c* in attesa di ricalcolo
168400990211     c                   if        dctcar = 'T' or dctcar = 'R'  or dctcar = 'C'
168500990129     c                   seton                                        64
168600990129     c                   endif
168700990216     c* se il flag DCTTCR della testata è uguale ad 'S' non considero la responsabilità
168800990216     c                   if        dcttcr = 'S'
168900990216     c                   seton                                        68
169000990216     c                   endif
169100990129      *
169200990129      * Carico i p.o. responsabili
169300990129     c                   exsr      cardcr
169400990129      *
169500990217     c                   setoff                                       67
169600990129     c* emetto la videata
169700990129     c                   z-add     1             nrr6
169800990129     c                   z-add     1             v6cnrr
169900080715    1c                   do        *hival
170000990129     C                   WRITE     FI01Z03
170100990129     C                   WRITE     FI01T05
170200990129     C                   EXFMT     FI01C06
170300080715     c                   setoff                                       28
170400990129      * F12=RITORNO
170500080715    2C                   IF        *inkl = *on
170600990129     C                   leave
170700080715    2C                   endif
170800080715     c
170900080715      * F05=Visualizza spunta senza anomalia
171000080715    2C                   IF        *inke = *on
171100080715     c* se 1 solo segnacollo, visualizzo subito, altrimenti mi devo
171200080715     c*  posizionare
171300080715    3c                   if        Adpaconta=1
171400080715     c                   EXSR      Wind06
171500080715     c                   setoff                                       31
171600080715   x3c                   else
171700080715     c                   seton                                        31
171800080715    4C     V6CPOS        IFGT      0
171900080715     C     V6CPOS        CHAIN     fi01s06                            31
172000080715     c  n31              z-add     v6snsc        AdpaNSC
172100081106     c  n31              movel     v6spor        AdpaPOR
172200080715     c  n31              EXSR      Wind06
172300080715    4c                   endif
172400080715     c
172500080715    4c                   if        v6cpos=0 or *in31
172600080715     c                   eval      v6cmsg=msg(7)
172700080715     c                   seton                                        28
172800080715    4c                   endif
172900080715    3c                   endif
173000080715     c
173100080715    2c                   endif
173200990129     C*
173300080715    1C                   ENDDO
173400990217      *
173500990129      *
173600990129     C                   ENDSR
173700990129      *****************************************************************
173800990129      *  RIEMPIMENTO SUBFILE DOCUMENTI RICHIESTI
173900990129      *****************************************************************
174000990129     C     CARDCR        BEGSR
174100990129      *
174200080715     C                   setoff                                       617528
174300990129     C                   WRITE     FI01C06
174400990129     C                   seton                                        6061
174500990129      *
174600990129     C                   move      *ZEROS        NRR6
174700080715     c                   clear                   AdpaConta         3 0
174800080715     c                   clear                   AdpaNSC           7 0
174900081106     c                   clear                   AdpaPOR           3
175000990129
175100990129     c     kdcd          setll     fndcr000
175200990129     c     kdcd          reade     fndcr000                               31
175300990129      *
175400990129     C     *IN31         DOWEQ     *OFF
175500990129      *
175600990129     c                   if        dcratb  = ' '
175700990129     c                   eval      v6sper = dcrper
175800990217     c* se punto operativo non valorizzato accendo il 69
175900990217     c                   if        dcrpor = 0
176000990217     c                   seton                                        69
176100990217     c                   else
176200990217     c                   setoff                                       69
176300990217     c*
176400990129     c                   movel     dcrpor        v6spor
176500990129     c* decodifico il P.O. responsabile
176600990129     C     dcrpor        chain     AZORG                              30
176700990129     C  n30              movel     ORGDES        v6sdpo
176800990129     C   30              movel     *blanks       v6sdpo
176900990211     c* decodifico la tabella calcolo responsabilità
177000990211      *
177100990211     c                   setoff                                       10
177200990211     C                   MOVEl     dcrtcr        v6stcr
177300990211     c*
177400990211     c                   clear                   tibs02ds
177500990211     C                   MOVEL     'C'           t02mod
177600990211     C                   MOVEL     knsif         t02sif
177700990211     C                   MOVEL     'TCR'         t02cod
177800990211     C                   MOVEL(P)  dcrtcr        t02ke1
177900990211      *
178000990211     C                   CALL      'TIBS02R'
178100990211     C                   PARM                    KPJBA
178200990211     C                   PARM                    tibs02ds
178300990211      *
178400990211    1C                   if        t02err = *BLANKS
178500990217     c                   movel     T02UNI        wdtcr            15
178600990217     c     MAI:MIN       XLATE     wdtcr         v6stcr
178700990211      *
178800990211    1C                   endif
178900990217      *
179000990217    1C                   endif
179100990211     c* decodifico la tabella tipo attribuzione responsabilità
179200990129      *
179300990129     c                   setoff                                       10
179400990216     C                   MOVEl     dcrflo        ddcr01
179500990216     c* segnacollo
179600990217     C                   MOVEl     §dcrnsc       v6snsc
179700990216     c*
179800990216     C                   MOVEl     §dcrtre       v6stre
179900090324     c* terminal responsabile
180000090324     c                   movel     §dcrter       v6ster
180100090324     c* decodifico il P.O. responsabile
180200090324     c                   if        v6ster>*zeros
180300090324     c                   movel     v6ster        w0030             3 0
180400090324     C     w0030         chain     AZORG                              30
180500090324     C  n30              movel     ORGDES        v6sdter
180600090324     C   30              movel     *blanks       v6sdter
180700090324     c                   endif
180800090324     c
180900080715     c                   if        §dcrtre='ADPA' or §dcrtre='ADPV'
181000080715     c                   seton                                        75
181100080715     c                   eval      Adpaconta=Adpaconta+1
181200080715     c                   eval      AdpaNSC=v6snsc
181300081106     c                   eval      AdpaPOR=v6spor
181400080715     c                   endif
181500990129     c*
181600990129     c                   clear                   tibs02ds
181700990129     C                   MOVEL     'C'           t02mod
181800990129     C                   MOVEL     knsif         t02sif
181900990211     C                   MOVEL     'TRE'         t02cod
182000990211     C                   MOVEL(P)  v6stre        t02ke1
182100990129      *
182200990129     C                   CALL      'TIBS02R'
182300990129     C                   PARM                    KPJBA
182400990129     C                   PARM                    tibs02ds
182500990129      *
182600990129    1C                   if        t02err = *BLANKS
182700990211     c                   movel     T02UNI        v6sdtr
182800990129      *
182900990129    1C                   endif
183000990129      *
183100990129     C                   add       1             NRR6
183200990129     C                   WRITE     FI01S06
183300990129      *
183400990129     c                   endif
183500990129      *
183600990129     c     kdcd          reade     fndcr000                               31
183700990129     C                   ENDDO
183800990129      *
183900990129     c                   eval      *IN60 = ( nrr6 <> *zeros)
184000990129      *
184100990129     C                   ENDSR
184200080715      *****************************************************************
184300080715      *  windos del sfl della reposnsabilità
184400080715      *****************************************************************
184500080715     C     wind06        BEGSR
184600080715     c* Aggancio fndcd per avere segnacollo
184700080715     c                   clear                   w06nsc
184800080715     c                   eval      knsc=AdpaNSC
184900080715     c     kdcde         chain     fndcd01l
185000080715    1c                   if        %found(fndcd01l)
185100080715     c* cerco il segnacollo con anomalia del p.o. responsabile  e poi
185200080715     c*  vedo se c'e' spunte senza anomalia in fnbrve
185300080715     c     kbrv          setll     fnbrv07l
185400080715     c     kbrv          reade     fnbrv07l
185500080715    2c                   dow       not %eof(fnbrv07l)
185600081106    3c                   if        brvcan<>' '  and %editc(brvfgs:'X')=adpapor
185700080715     c     kbrve         chain     fnbrve1l
185800080715    4c                   if        %found(fnbrve1l) and e_brvcan=' '
185900080715     c* riempo videata
186000080715     c                   eval      w06lnp=brvlnp
186100080715     c                   eval      w06lna=brvlna
186200080715     c                   eval      w06nrs=brvnrs
186300080715     c                   eval      w06nsc=brvnsc
186400080715     c                   eval      w06npg=brvnpg
186500080715     c                   eval      w06nfv=brvnfv
186600080715     c                   eval      w06fgs=brvfgs
186700080715     c                   clear                   w06dfv
186800080715     c                   clear                   fnlv53ds
186900080715     c                   eval      d53nfv=w06nfv
187000080715     c                   eval      d53fgs=w06fgs
187100080715     c                   eval      d53npg=w06npg
187200080715     c                   call      'FNLV53R'
187300080715     c                   parm                    fnlv53ds
187400080715     c
187500080715     c                   if        d53dfv>0
187600080715     c                   eval      Dataiso=%date(d53dfv:*iso)
187700080715     c                   eval      Dataeur=dataiso
187800080715     c                   eval      w06dfv=%dec(Dataeur)
187900080715     c                   endif
188000080715     c
188100080715     c* spunta senza anomalia
188200080715     c                   eval      Dataiso=%date(e_brvdfs:*iso)
188300080715     c                   eval      Dataeur=dataiso
188400080715     c                   eval      w06dfs=%dec(Dataeur)
188500080715     c
188600080715     c                   eval      Dataiso=%date(e_brvdcs:*iso)
188700080715     c                   eval      Dataeur=dataiso
188800080715     c                   eval      w06dcs=%dec(Dataeur)
188900080715     c
189000080715     c                   eval      w06hms=E_brvhms
189100080715     c                   eval      w06hcs=E_brvhcs
189200080715     c                   eval      w06nps=%editc(E_brvnps:'X')
189300080715     c                   clear                   w06tap
189400080715     c                   if        e_brvtap<>' '
189500080715     C                   CLEAR                   tibs02ds
189600080715     C                   MOVEL     'C'           T02MOD
189700080715     C                   MOVEL     KNSIF         T02SIF
189800080715     C                   MOVEL     'APS'         T02COD
189900080715     C                   MOVEL     e_BRVTAP      T02KE1
190000080715     C                   CALL      'TIBS02R'
190100080715     C                   PARM                    KPJBA
190200080715     C                   PARM                    tibs02ds
190300080715     c
190400080715     c                   if        t02err<>' '
190500080715     c                   eval      t02uni=*all'?'
190600080715     c                   endif
190700080715     c
190800080715     c                   eval      w06tap=e_brvtap+'-'+%subst(t02uni:1:9)
190900080715     c                   endif
191000080715     c
191100080715     c* spunta con   anomalia
191200080715     c                   eval      w06can='"'+brvcan+'"'
191300080715     c
191400080715     c                   eval      Dataiso=%date(brvdfs:*iso)
191500080715     c                   eval      Dataeur=dataiso
191600080715     c                   eval      w06cdfs=%dec(Dataeur)
191700080715     c
191800080715     c                   eval      Dataiso=%date(brvdcs:*iso)
191900080715     c                   eval      Dataeur=dataiso
192000080715     c                   eval      w06cdcs=%dec(Dataeur)
192100080715     c
192200080715     c                   eval      w06chms=brvhms
192300080715     c                   eval      w06chcs=brvhcs
192400080715     c                   eval      w06cnps=%editc(brvnps:'X')
192500080715     c                   clear                   w06ctap
192600080715     c                   if        brvtap<>' '
192700080715     C                   CLEAR                   tibs02ds
192800080715     C                   MOVEL     'C'           T02MOD
192900080715     C                   MOVEL     KNSIF         T02SIF
193000080715     C                   MOVEL     'APS'         T02COD
193100080715     C                   MOVEL     BRVTAP        T02KE1
193200080715     C                   CALL      'TIBS02R'
193300080715     C                   PARM                    KPJBA
193400080715     C                   PARM                    tibs02ds
193500080715     c
193600080715     c                   if        t02err<>' '
193700080715     c                   eval      t02uni=*all'?'
193800080715     c                   endif
193900080715     c
194000080715     c                   eval      w06ctap=brvtap+'-'+%subst(t02uni:1:9)
194100080715     c                   endif
194200080715     c
194300080715     c                   leave
194400080715    4c                   endif
194500080715    3c                   endif
194600080715     c
194700080715     c     kbrv          reade     fnbrv07l
194800080715    2c                   enddo
194900080715     c
195000080715     c* se ho trovato visualizzo
195100080715     c                   if        w06nsc>0
195200080715     c                   do        *HIVAL
195300080715     c                   exfmt     FI01W06
195400080715     c   KL              leave
195500080715     c                   enddo
195600080715    1c                   endif
195700080715     c
195800080715    1c                   endif
195900080715      *
196000080715     c                   setoff                                       31
196100080715     C                   ENDSR
196200981116      *****************************************************************
196300981116      *  F20 = STORICO FASI
196400981116      *****************************************************************
196500981116     C     SUB_F20       BEGSR
196600990610     c* controllo la data invio denuncia per visualizzare l'invio al cliente
196700990610      * se data invio copia denuncia è diversa da zero significa che è stata già inviata
196800990610     c                   if        dctdic <> *zeros
196900990610     c*  giro la data stampa/invio copia denuncia
197000990610     c                   seton                                        71
197100990610     C                   Z-ADD     dctdic        G02inv
197200990610     C                   movel     '3'           G02err
197300990610     C                   CALL      'XSRDA8'
197400990610     C                   PARM                    WLBDAT
197500990610     c                   z-add     g02dat        v1cdic
197600990610     c                   endif
197700981116      *
197800981116      * Carico le fasi che sono state eseguite fino adesso
197900981116     c                   exsr      carfca
198000981116      *
198100990216     c                   setoff                                           67
198200981116     c* emetto la videata
198300981116     c                   z-add     1             nrr4
198400981116     c                   z-add     1             v4cnrr
198500981116     c                   do        *hival
198600981116     C                   WRITE     FI01Z03
198700981116     C                   WRITE     FI01T03
198800981116     C                   EXFMT     FI01C04
198900981116      * F12=RITORNO
199000981116     C                   IF        *inkl = *on
199100981116     C                   leave
199200981116     C                   endif
199300981116     C*
199400981116     C                   ENDDO
199500990610     c*
199600990610     c                   setoff                                       71
199700981116      *
199800981116     C                   ENDSR
199900981116      *****************************************************************
200000981116      *  RIEMPIMENTO SUBFILE STORICO FASI
200100981116      *****************************************************************
200200981116     C     CARFCA        BEGSR
200300981116      *
200400981126     C                   setoff                                       41
200500981116     C                   WRITE     FI01C04
200600981116     C                   seton                                        4041
200700981116      *
200800981116     C                   move      *ZEROS        NRR4
200900981116
201000981116     c     kdcd          setll     fndcf000
201100981116     c     kdcd          reade     fndcf000                               31
201200981116      *
201300981116     C     *IN31         DOWEQ     *OFF
201400981116      *
201500990216     c     *iso          move      DCfdfc        Wdatadmy
201600990216     c     *dmy          movel     Wdatadmy      V4sdfc
201700981116      *
201800981116     c                   z-add     dcfhfc        v4shfc
201900981116      *
202000981116     C                   z-add     dcffca        V4sfca
202100981116     c                   CLEAR                   FIDN05DS
202200981116     c                   movel     'C'           I05MOD
202300981116     c                   z-add     V4sfca        I05fca
202400981116     C                   CALL      'FIDN05R'
202500981116     c                   PARM                    KPJBA
202600981116     c                   PARM                    FIDN05DS
202700981116     c                   IF        O05err <> 'E'
202800981116     c                   movel     O05REC        DS_FNDFA
202900981116     c                   movel     DFAdes        V4Dfca
203000981116     c                   ENDIF
203100981116     c*
203200981116     c                   z-add     dcffev        v4sfev
203300981116     c                   move      dcfpru        v4spru
203400990216     c                   move      dcfptr        v4sptr
203500020124
203600020124     c                   movel     dcflet        ddcf01
203700020124     c                   eval      v4spfr = §dcfpfra
203800020124
203900981116      *
204000981116     C                   add       1             NRR4
204100981116     C                   WRITE     FI01S04
204200981116      *
204300981116     c     kdcd          reade     fndcf000                               31
204400981116     C                   ENDDO
204500981116      *
204600981116     c                   eval      *IN40 = ( nrr4 <> *zeros)
204700981116      *
204800981116     C                   ENDSR
204900981113      *****************************************************************
205000981113      *  F21 = DETTAGLIO SEGNACOLLI
205100981113      *****************************************************************
205200981113     C     SUB_F21       BEGSR
205300990216     c*
205400990216     c                   setoff                                       67
205500981116      *
205600981116      * Carico tutti i segnacolli presenti nella spedizione presa in questione
205700981116     c                   exsr      carnsc
205800990309      * Controllo se esitono i colli in ART / BLT (Bolle legate)
205900990309     c                   if        SIMFEL <> 0
206000990309     C                   if        i00tpb = 'A'
206100990309     C     KBOL          SETLL     FNART000                               31
206200990309     C                   else
206300990309     C     KBOL          SETLL     FNBLT000                               31
206400990309     C                   endif
206500990309     C                   endif
206600990309      *
206700981113      *
206800990309      * se sono in sede oppure non ho la bolla in linea carico i segnacolli dal dettaglio C.A
206900990309     c                   IF        SIMFEL = 0  or not *in31
207000981113     C                   exsr      RIES01S
207100981113     c                   ELSE
207200981113     C                   exsr      RIES01
207300981113     C                   ENDIF
207400981113      *
207500981113     c* emetto la videata
207600981113     c                   z-add     1             nrr1
207700981116     c                   z-add     1             v1cnrr
207800981113     c                   do        *hival
207900981113     C                   WRITE     FI01Z03
208000981116     C                   WRITE     FI01T03
208100981113     c  n20              write     fi01d03
208200981116     C                   EXFMT     FI01C03
208300981113     C                   CLEAR                   V1CMSG
208400981113     C                   SETOFF                                       2829
208500981113      * F12=RITORNO
208600981113     C                   IF        *inkl = *on
208700981113     C                   leave
208800981113     C                   endif
208900981113     C*
209000981113     C                   ENDDO
209100981113      *
209200981113     C                   ENDSR
209300990506      *****************************************************************
209400990506      *  F24 = ALTRI TASTI
209500990506      *****************************************************************
209600990506     C     SUB_F24       BEGSR
209700990506      *
209800990506     c                   add       1             c
209900990506      *
210000990506      * recupero la stringa di comandi da visualizzare
210100130726     c                   if        C > 3
210200990506     c                   z-add     1             c
210300990506     c                   endif
210400990506      *
210500990506     c                   movel     CMD(c)        v1ccmd
210600990506      *
210700990506      * se si tratta della terza stringa di comandi, della seconda se profilo CONSULDANNI,
210800990506      *  carico i comandi in relazione ai dati presenti nella CA
210900130726     c                   if        C = 3
211000990506      *
211100990506     c                   SELECT
211200990506     c                   WHEN      *IN07 = *ON
211300990506     c                   movel     CMD(4)        v1ccmd                         * evento
211400990506     c                   WHEN      *in03 = *ON
211500990506     c                   movel     CMD(3)        v1ccmd                         * si responsabilità
211600990506     c                   OTHER
211700990506     c                   movel     CMD(5)        v1ccmd                         * no responsabilità
211800990506     c                   ENDSL
211900990506      *
212000990506      * Verifico se inserire nella stringa F8=beneficiario alter. o F9=rivalsa
212100990506     c                   SELECT
212200990506     c                   WHEN      *in08 = *on  and  *in09 = *off
212300990506     c                   movel     cmd2(1)       v1ccmd
212400990506     c                   WHEN      *in08 = *off  and  *in09 = *on
212500990506     c                   movel     cmd2(2)       v1ccmd
212600990506     c                   WHEN      *in08 = *on  and  *in09 = *on
212700990506     c                   movel     cmd2(3)       v1ccmd
212800990506     c                   ENDSL
212900990506      *
213000990506     c                   ENDIF
213100990506      *
213200990506     C                   ENDSR
213300981116      ****************************************************************
213400981116      * CARICAMENTO SEGNACOLLI IN ALTRE C.A. E CONTEGGIO PROPRI
213500981116      ****************************************************************
213600981116     C     CARNSC        BEGSR
213700981116      *
213800981116     c                   clear                   nsc
213900981116     c                   clear                   aac
214000981116     c                   clear                   fil
214100981116     c                   clear                   nca
214200981116     c                   clear                   II
214300040719      ***
214400040719      **** istruzioni asteriscate sostituite da quelle nuove
214500040719      ***
214600040719     c**""*kbol01        setll     fndct2
214700040719     c**""*kbol01        reade     fndct2                                 30
214800040719      *
214900040719     c**""**in30         doweq     *off
215000040719      *
215100040719     c**""**             IF        DCTatb = *blanks  and  DCTdch = *zeros
215200040719      *
215300040719     c**""*kdcd          setll     fndcd000
215400040719     c**""*kdcd          reade     fndcd000                               30
215500040719      *
215600040719     c**""**in30         doweq     *off
215700040719      * Non considero annullati e chiusi in modo diverso
215800040719     c**""**             IF         DCDatb <> 'A'
215900040719      * Conto i miei segnacolli
216000040719     c**""**             if        ds_dctkey = DS_numca
216100040719     c**""**             ELSE
216200040719      * Memorizzo segnacolli di altre C.A.
216300040719     c**""**             add       1             II
216400040719     c**""**             z-add     DCDnsc        nsc(II)
216500040719     c**""**             z-add     DCTaac        aac(II)
216600040719     c**""**             z-add     DCTfil        fil(II)
216700040719     c**""**             z-add     DCTnca        nca(II)
216800040719     c**""**             ENDIF
216900040719      *
217000040719     c**""**             ENDIF
217100040719      *
217200040719     c**""*kdcd          reade     fndcd000                               30
217300040719     c**""**             ENDDO
217400040719      *
217500040719     c**""**             ENDIF
217600040719      *
217700040719     c**""*kbol01        reade     fndct2                                 30
217800040719     c**""**             enddo
217900040719      ***
218000040719      ***
218100040719
218200040719      * richiamo il programma di comodo per recuperare le CA legate alla spedizione in
218300040719      * questione
218400040719     c                   clear                   fidn12ds
218500040719     c                   eval      i12aas = i00aas
218600040719     c                   eval      i12lnp = i00lnp
218700040719     c                   eval      i12nrs = i00nrs
218800040719     c                   eval      i12nsp = i00nsp
218900040719     c                   eval      i12fel = 'E'
219000040719     c                   eval      i12fan = 'E'
219100040719     c                   eval      i12fch = 'E'
219200040719      *
219300040719     c                   call      'FIDN12R'
219400040719     c                   parm                    fidn12ds
219500040719      *
219600040719     c                   z-add     *zeros        aa                2 0
219700040719      * se trovata almeno una CA
219800040719     c                   if        o12nca > 0
219900040719     c                   dow       aa <  o12nca
220000040719     c                   add       1             aa
220100040719     c                   movea     skkey(aa)     dskey
220200040719     C                   z-add     dsaac         dctaac
220300040719     C                   z-add     dsfil         dctfil
220400040719     C                   z-add     dsnca         dctnca
220500981116      *
220600981116      *
220700981116     c     kdcd          setll     fndcd000
220800981201     c     kdcd          reade     fndcd000                               30
220900981116      *
221000981116     c     *in30         doweq     *off
221100990125      * Non considero annullati e chiusi in modo diverso
221200990216     c                   IF         DCDatb <> 'A'
221300981116      * Conto i miei segnacolli
221400981116     c                   if        ds_dctkey = DS_numca
221500981116     c                   ELSE
221600981116      * Memorizzo segnacolli di altre C.A.
221700981116     c                   add       1             II
221800981116     c                   z-add     DCDnsc        nsc(II)
221900981116     c                   z-add     DCTaac        aac(II)
222000981116     c                   z-add     DCTfil        fil(II)
222100981116     c                   z-add     DCTnca        nca(II)
222200981116     c                   ENDIF
222300981116      *
222400981116     c                   ENDIF
222500981116      *
222600981116     c     kdcd          reade     fndcd000                               30
222700981116     c                   ENDDO
222800981116      *
222900040719     c                   ENDDO
223000981116      *
223100040719     c                   endif
223200981116      *
223300981116     c* recupero i dati della testata ca
223400981116     c     knumca        chain     fndct01l                           30
223500040716      *
223600040716      * in caso di tipo bolla ='A' recupero il numero bolla prima da FNDCK e se non c'è da FNDCT
223700040716     c                   If        i00tpb = 'A'
223800040716     c     Knumca        chain     fndck01l
223900040716     c                   If        %found(fndck01l)
224000040716     c                   eval      DCTaas = DCKaas
224100040716     c                   eval      DCTlnp = DCKlnp
224200040719     c                   eval      DCTnrs = DCKnrs
224300040716     c                   eval      DCTnsp = DCKnsp
224400040716      *
224500040716     c                   eval      i00aas = DCKaas
224600040716     c                   eval      i00lnp = DCKlnp
224700040719     c                   eval      i00nrs = DCKnrs
224800040716     c                   eval      i00nsp = DCKnsp
224900040716      *
225000040716     c                   endif
225100040716     c                   endif
225200981116      *
225300981116     c                   endsr
225400980623      *****************************************************************
225500981006      *  RIEMPIMENTO SUBFILE SEGNACOLLI SEDE o NON PRESENTE DETTAGLIO
225600980623      *****************************************************************
225700981006     C     RIES01S       BEGSR
225800980623      *
225900981126     C                   setoff                                       21
226000981113     C                   WRITE     FI01C03
226100980720     C                   seton                                        2021
226200980623      *
226300980720     C                   move      *ZEROS        NRR1
226400980626
226500981006     c     kdcd          setll     fndcd000
226600981113     c     kdcd          reade     fndcd000                               31
226700981006      *
226800980626     C     *IN31         DOWEQ     *OFF
226900990215      * pulisco tutto
227000990215     c                   eval      *in65 = *off
227100990215     c                   clear                   v1sfgs
227200990215     c                   clear                   v1sdfv
227300990215     c                   clear                   v1snps
227400990215      * pulisco tutto
227500990215     c                   eval      *in66 = *off
227600990215     c                   clear                   v1sdch
227700990215     c                   clear                   v1scch
227800980626      *
227900990216     C                   IF        DCDatb = *blanks
228000981029     C                   move      DCDfls        V1Sfls
228100981029     C                   move      DCDnsc        V1Snsc
228200981029     C                   move      DCDcan        V1Scan
228300980626      * Decodifico cod. anomalia
228400981029     C                   movel     '3E'          KCOD
228500981029     C                   movel(P)  DCDcan        KKEY
228600971020     C     KTAB          CHAIN     TABEL00F                           31
228700981029     C  N31              movel     TBLUNI        V1DCAN
228800981029     C   31              clear                   V1DCAN
228900990215      * Aggiungo i campi relativo al ritrovamento colli
229000990215     c                   if        dcdfgs > 0
229100990215     c                   eval      *in65 = *on
229200990216     c                   eval      *in67 = *on
229300990215     c*
229400990215     c                   movel     dcdfgs        v1sfgs
229500990215     c* data ritrovamento collo
229600990215     c     *iso          move      dcddfv        Wdataeur
229700990215     c     *eur          movel     Wdataeur      v1sdfv
229800990215      *
229900990215     c                   eval      v1snps = dcdnps
230000990215      *
230100990215     c                   endif
230200990215      * Aggiungo i campi relativo alla chiusura del collo
230300990215     c                   if        dcddch > 0
230400990215     c                   eval      *in66 = *on
230500990216     c                   eval      *in67 = *on
230600990215      * data chiusura
230700990215     c     *iso          move      dcddch        Wdataeur
230800990215     c     *eur          movel     Wdataeur      v1sdch
230900990215      *decodifico la causale
231000990215     c                   clear                   TIBS02DS
231100990215     C                   MOVEL     'C'           t02mod
231200990215     C                   MOVEL     knsif         t02sif
231300990215     C                   MOVEL     'CCH'         t02cod
231400990215     C                   MOVEL(P)  dcdcch        t02ke1
231500990215      *
231600990215     C                   CALL      'TIBS02R'
231700990215     C                   PARM                    KPJBA
231800990215     C                   PARM                    TIBS02DS
231900990215      *
232000990215     C                   if        t02err = *BLANKS
232100990215     C                   MOVEL     T02UNI        Dcch
232200990215     C                   MOVEL     §CCHdesc      V1scch
232300990215     c                   endif
232400990215      *
232500990215     c                   endif
232600980720      *
232700981029     C                   add       1             NRR1
232800981113     C                   WRITE     FI01S03
232900981029     C                   ENDIF
233000980626      *
233100981113     c     kdcd          reade     fndcd000                               31
233200980626     C                   ENDDO
233300981116      * Carico segnacolli contenuti in altre C.A.
233400981116     c                   eval      *IN11 = *OFF
233500981116      *
233600981116     c                   DO        100           II
233700981116     c                   IF        nca(II) = *zeros
233800981116     c                   z-add     101           II
233900981116     c                   ELSE
234000981116      *
234100981116     c                   move      fil(II)       V1Sfil
234200981117     c                   movel     aac(II)       V1Saac
234300981116     c                   z-add     nca(II)       V1Snca
234400981116     C                   z-add     nsc(II)       V1Snsc
234500981116     C                   clear                   V1Scan
234600981116     C                   clear                   V1Dcan
234700981116     C                   add       1             NRR1
234800981116     C                   write     FI01S03
234900981116     C                   ENDIF
235000981116     c                   ENDDO
235100981116      *
235200980626     c                   eval      *IN20 = ( nrr1 <> *zeros)
235300980623      *
235400971020     C                   ENDSR
235500981006      *****************************************************************
235600981006      *  RIEMPIMENTO SUBFILE SEGNACOLLI
235700981006      *****************************************************************
235800981006     C     RIES01        BEGSR
235900981006      *
236000981126     C                   setoff                                       21
236100981113     C                   WRITE     FI01C03
236200981006     C                   seton                                        2021
236300981006      *
236400981006     C                   move      *ZEROS        NRR1
236500981006      *
236600981006     C                   if        i00tpb = 'A'
236700981006     C     KBOL          SETLL     FNART000
236800981006     C     KBOL          READE     FNART000                               31
236900981006     C                   else
237000981006     C     KBOL          SETLL     FNBLT000
237100981006     C     KBOL          READE     FNBLT000                               31
237200981006     C                   endif
237300981006
237400981006     C     *IN31         DOWEQ     *OFF
237500990305      * pulisco tutto
237600990305     c                   eval      *in65 = *off
237700990305     c                   clear                   v1sfgs
237800990305     c                   clear                   v1sdfv
237900990305     c                   clear                   v1snps
238000990305      * pulisco tutto
238100990305     c                   eval      *in66 = *off
238200990305     c                   clear                   v1sdch
238300990305     c                   clear                   v1scch
238400990305      *
238500981006     C                   IF        ARTATB = *BLANKS
238600981006      *
238700981116      * proteggo campo scelta se segnacollo già presente su altra c.a.
238800981116     c                   z-add     1             II
238900981116     c     artnsc        lookup    nsc(II)                                12
239000990125     c                   If        *in12 = *ON
239100981116     c                   move      fil(II)       v1sfil
239200981117     c                   z-add     aac(II)       v1saac
239300981116     c                   z-add     nca(II)       v1snca
239400990125     c                   Else
239500990125     c                   clear                   v1saac
239600990125     c                   clear                   v1sfil
239700990125     c                   clear                   v1snca
239800981116     c                   Endif
239900990125      *
240000981006      * metto campi in RI se segnacollo presente su questa C.A.
240100981006     c                   eval      *in11 = *off
240200990125     C                   clear                   V1Scan
240300990125     C                   clear                   V1Dcan
240400990215      *
240500990215     c                   endif
240600981006      *
240700981201     c     kdcd2b        chain     FNDCD002                           30
240800990125     c                   If        *IN30 = *OFF  and  DCDatb = *BLANKS
240900990125     c                   eval      *in11 = *on
241000990125     C                   movel     DCDcan        V1Scan
241100990215      * Aggiungo i campi relativo al ritrovamento colli
241200990215     c                   if        dcdfgs > 0
241300990215     c                   eval      *in65 = *on
241400990216     c                   eval      *in67 = *on
241500990215     c*
241600990215     c                   movel     dcdfgs        v1sfgs
241700990215     c* data ritrovamento collo
241800990215     c     *iso          move      dcddfv        Wdataeur
241900990215     c     *eur          movel     Wdataeur      v1sdfv
242000990215      *
242100990215     c                   eval      v1snps = dcdnps
242200990215      *
242300990215     c                   endif
242400990215      *
242500990215      * Aggiungo i campi relativo alla chiusura del collo
242600990215     c                   if        dcddch > 0
242700990215     c                   eval      *in66 = *on
242800990216     c                   eval      *in67 = *on
242900990215      * data chiusura
243000990215     c     *iso          move      dcddch        Wdataeur
243100990215     c     *eur          movel     Wdataeur      v1sdch
243200990215      *decodifico la causale
243300990215     c                   clear                   TIBS02DS
243400990215     C                   MOVEL     'C'           t02mod
243500990215     C                   MOVEL     knsif         t02sif
243600990215     C                   MOVEL     'CCH'         t02cod
243700990215     C                   MOVEL(P)  dcdcch        t02ke1
243800990215      *
243900990215     C                   CALL      'TIBS02R'
244000990215     C                   PARM                    KPJBA
244100990215     C                   PARM                    TIBS02DS
244200990215      *
244300990215     C                   if        t02err = *BLANKS
244400990215     C                   MOVEL     T02UNI        Dcch
244500990215     C                   MOVEL     §CCHdesc      V1scch
244600990215     c                   endif
244700990215      *
244800990125     c                   Endif
244900981116     c*
245000981029     C                   move      ARTfls        V1SFLS
245100981029     C                   move      ARTnsc        V1SNSC
245200981117      * Imposto data consegna
245300981117     c                   clear                   v1sdtc
245400981117     c                   IF        ARTdcm > *zeros
245500981117     c     *iso          move      ARTdcm        Wdataeur
245600981117     c     *eur          movel     Wdataeur      v1sdtc
245700981117     c                   ENDIF
245800990125      * Se non impostato da CA imposto anomalia da segnacollo
245900981117     c                   IF        V1Scan = *blanks
246000981029     C                   move      ARTcan        V1SCAN
246100981117     c                   ENDIF
246200981006      * Decodifico cod. anomalia
246300981006     C                   MOVEL     '3E'          KCOD
246400981117     C                   MOVEL(P)  V1SCAN        KKEY
246500981006     C     KTAB          CHAIN     TABEL00F                           31
246600981006     C  N31              MOVEL     TBLUNI        V1DCAN
246700981020      *
246800981006     C                   ADD       1             NRR1
246900981113     C                   WRITE     FI01S03
247000981006     C                   endif
247100981006      *
247200981006     C                   if        i00tpb = 'A'
247300981006     C     KBOL          READE     FNART000                               31
247400981006     C                   else
247500981006     C     KBOL          READE     FNBLT000                               31
247600981006     C                   endif
247700981006      *
247800981006     C                   ENDDO
247900981006      *
248000981006
248100981006      * memorizzo il nrr dell'ultimo record caricato
248200981006     c                   eval      *IN20 = ( nrr1 <> *zeros)
248300981006      *
248400981006     C                   ENDSR
248500980717      *****************************************************************
248600981113      *  VISUALIZZAZIONE DATI ANAGRAFICI CLIENTI/RIVALSA/INTESTATARIO
248700980717      *****************************************************************
248800980514     C     MANCLI        BEGSR
248900981021      *
249000981113     c     KDKA          CHAIN     FNDKA000                           30
249100990909     c                   IF        *IN30 = *OFF  and DKAatb = *blank
249200981113     c                   movel     DKArag        W2Crag
249300981113     c                   movel     DKAvia        W2Cvia
249400981113     c                   movel     DKAcap        W2Ccap
249500981113     c                   movel     DKAloc        W2Cloc
249600981113     c                   movel     DKAprv        W2Cprv
249700981113     c                   movel     DKAnaz        W2Cnaz
249800981113     c                   eval      W2Civa = %subst(DKAiva:1:14)
249900981113     c                   eval      W2Cive = %subst(DKAiva:15:2)
250000981113     c                   movel     DKAtel        W2Ctel
250100981113     c                   movel     DKAfax        W2Cfax
250200980717      * EMISSIONE
250300990909     c                   do        *HIVAL
250400981113     c                   exfmt     FI01W02
250500990909     c   KL              leave
250600981113     c                   enddo
250700990909      *
250800990909     c                   ENDIF
250900980717      *
251000990909     C                   ENDSR
251100050928
251200050928      *****************************************************************
251300131014      *  VISUALIZZAZIONE DISPOSIZIONI A.I.G.
251400050928      *****************************************************************
251500050928     c     SUB2_F2       BEGSR
251600050928      *
251700050928      * Richiamo pgm. FIDN8CR
251800050928      * (imposto I00mod a scanso di equivoci:
251900050928      *  da questo pgm. NON è utlizzato)
252000050928     c                   eval      I00mod = 'I'
252100050928     c                   movel(p)  FIDN00ds      KPJBU
252200050928      *
252300050928     c                   call      'FIDN8CR'
252400050928     c                   parm                    KPJBA
252500050928      *
252600050928     c                   ENDSR
252700050909      *****************************************************************
252800131014      *  VISUALIZZAZIONE DATI RICHIESTO RIMBORSO A.I.G.
252900050909      *****************************************************************
253000050909     C     SUB2_F11      BEGSR
253100050909      *
253200050909     c                   clear                   wlbdat
253300050909     c                   Z-ADD     DRAdrr        G02inv
253400050909     c                   movel     '3'           G02err
253500050909     c                   CALL      'XSRDA8'
253600050909     c                   PARM                    WLBDAT
253700050909     c                   z-add     g02dat        W22drr
253800050909
253900050909     c                   clear                   wlbdat
254000050909     c                   Z-ADD     DRAdvr        G02inv
254100050909     c                   movel     '3'           G02err
254200050909     c                   CALL      'XSRDA8'
254300050909     c                   PARM                    WLBDAT
254400050909     c                   z-add     g02dat        W22dvr
254500050909
254600050909     c                   eval      W22iri = DRAiri
254700050909      * EMISSIONE
254800050909     c                   do        *HIVAL
254900050909     c                   exfmt     FI01W22
255000050909     c   KL              leave
255100050909     c                   enddo
255200050909      *
255300050909     C                   ENDSR
255400050909      *****************************************************************
255500980615      *  REPERIMENTO DATI TIPO ANOMALIA
255600980625      *****************************************************************
255700971020     C     CHTAD         BEGSR
255800980615      *
255900981019     c                   clear                   DTAD
256000981019      *
256100981020     c                   clear                   TIBS02DS
256200980615     C                   MOVEL     'C'           t02mod
256300980615     C                   MOVEL     knsif         t02sif
256400980615     C                   MOVEL     'TAD'         t02cod
256500980615     C                   MOVEL(P)  Wkey1         t02ke1
256600980615      *
256700980615     C                   CALL      'TIBS02R'
256800980615     C                   PARM                    KPJBA
256900981020     C                   PARM                    TIBS02DS
257000980615      *
257100980615     C                   if        t02err = *BLANKS
257200980615     C                   MOVEL     T02UNI        DTAD
257300980625     C                   endif
257400980625      *
257500971020     C                   ENDSR
257600981020      *****************************************************************
257700981020      *  AGGANCIO ANAGRAFICO CLIENTE
257800981020      *****************************************************************
257900981020     C     CTR_CNACO     BEGSR
258000981020      *
258100981020     c                   IF        SIMFEL = *zeros                              SEDE
258200981020     C                   clear                   TIBS69DS
258300981022     c                   z-add     Wksc          I69kac
258400981022      *
258500981020     C                   CALL      'TIBS69R'
258600981020     C                   PARM                    tibs69DS
258700981020     C                   PARM                    DS_cnaco
258800981020     C                   PARM                    DS_cnind
258900981020     C                   PARM                    DS_cnclp
259000981020     C                   PARM                    DS_fncls
259100981022      *
259200981022IF  2C                   eval      *IN30 = (O69ERR =  '1')
259300981022     C                   eval      wtibs69r = '1'
259400981020      *
259500981020     c                   ELSE                                                   FILIALE
259600150630     c                   z-add     DUTkci        ccc
259700981022     c     KACO          chain     cnaco000                           30
259800981020     c                   ENDIF
259900981020      *
260000981020     C                   ENDSR
260100981005      *****************************************************************
260200981005      *  CONTROLLO TIPO BOLLA
260300981005      *****************************************************************
260400981005     C     CTR_tipobo    BEGSR
260500981005      *
260600981005     C                   movel     'TB'          kCOD
260700981005     C                   movel(P)  TAStbl        kKEY
260800981005     C     ktab          CHAIN     TABEL                              31
260900981005     C                   movel     TBLUNI        DSTB
261000981005     c                   if        §TBrbl <> 'R'
261100981005     C                   movel     §TBtpo        FLGTB1
261200981005     c                   endif
261300981005      *
261400981005     C                   ENDSR
261500990315      *****************************************************************
261600990315      *  DECODIFICO LA BANCA
261700990315      *****************************************************************
261800990315     C     SUB_BANCA     BEGSR
261900990315      *
262000990315     c                   move      dclabi        wabi
262100990315     c                   move      dclcab        wcab
262200990315      *
262300990315     c     kabi          chain     cnabi01l                           30
262400990315      *
262500990315     c                   if        not  *in30
262600990315     c                   eval      v2cban = %trim(abiist) + '-' + %trim(abiage)
262700990316     c                             + '-' + %trim(abiloc) + '-' + %trim(abicom)
262800990315     c*
262900990315     c                   endif
263000990315     c*
263100990315     c                   endsr
263200980722      *****************************************************************
263300980623      *  ROUTINE INIZIALE
263400980722      *****************************************************************
263500951009     C     *INZSR        BEGSR
263600980722
263700951009     C     *ENTRY        PLIST
263800951009     C                   PARM                    KPJBA
263900000309     C                   PARM                    CALLricors        1
264000980722
264100981020     C                   MOVEL     KPJBU         FIDN00DS
264200150630      *
264300150630      * -?Reperimento dati del lavoro?
264400150630     c                   exsr      DatiJob
264500981009      *
264600980611      * ORA
264700980611     C                   TIME                    W0140
264800980619      * UDATE IN GGMMAAAA
264900980611     C                   MOVE      W0140         WDTGIO
265000980625
265100981005      * APRO FILE PARTICOLARI SEDE/FILIALE
265200080715     c                   if        %subst(knsif:7:1)='P'
265300080715     c                   eval      %subst(Wlibfile:7:3)='PRD'
265400080715     c                   eval      %subst(Wlibbrv :7:3)='PRD'
265500080715     c                   else
265600080715     c                   eval      %subst(Wlibfile:7:3)='201'
265700080715     c                   eval      %subst(Wlibbrv :7:3)='201'
265800080715     c                   endif
265900080715     c
266000981005     C                   IF        SIMFEL = *zeros
266100990806     C                   open      TITAS30C
266200981005     C                   open      TNTMD01L
266300981221     C                   open      TNCSB03L
266400080715     c* Anche in sede devo aprire fnbrve
266500080715     C                   open      FNbrve1l
266600080715     C                   open      FNbrv07l
266700080715     c
266800981005     c                   ELSE
266900981006     C                   open      FNARB01L
267000981229     C                   open      FNLBL02L
267100991104     C                   open      FIAR601L
267200051114     C                   open      FiAR901L
267300981006     C                   open      FNBLP01L
267400051114     C                   open      FNART01L
267500981006     C                   open      FNBLT01L
267600981006     C                   open      CNACO00F
267700981020     C                   open      FNCLS01L
267800080715     C                   open      FNbrve1l
267900080715     C                   open      FNbrv07l
268000981006     c                   ENDIF
268100090324     c
268200090407     c* Vedo se sono EDP o qua per visualizzare terminal responsab.
268300090407     c                   if        %subst(knmus:1:3)='EDP'   or
268400140908     c                             %subst(knmus:1:3)='QUA'   or
268500140908     c                             %subst(knmus:1:3)='DLO'
268600090324     c                   seton                                        01
268700090324     c                   else
268800090324     c                   setoff                                       01
268900090324     c                   endif
269000010904
269100010904      * AGGANCIO TABELLA GED
269200010910     c                   clear                   DGEDDN
269300010904     c                   clear                   TIBS02DS
269400010904     C                   MOVEL     'C'           t02mod
269500010904     C                   MOVEL     knsif         t02sif
269600010904     C                   MOVEL     'GED'         t02cod
269700010904     C                   MOVEL(P)  'DANNI'       t02ke1
269800010904      *
269900010904     C                   CALL      'TIBS02R'
270000010904     C                   PARM                    KPJBA
270100010904     C                   PARM                    TIBS02DS
270200010904      *
270300010904     C                   if        t02err = *BLANKS
270400010910     C                   MOVEL     T02UNI        DGEDDN
270500010904     C                   endif
270600990506
270700990506      * Aggancio tabella Standard Danni
270800990506     c                   clear                   tibs02ds
270900990506     C                   MOVEL     'C'           t02mod
271000990506     C                   MOVEL     knsif         t02sif
271100990506     C                   MOVEL     'STD'         t02cod
271200990506     C                   MOVEL(P)  1             t02ke1
271300010910     C                   MOVEL(P)  §gedDBA       t02ke2
271400990506      *
271500990506     C                   CALL      'TIBS02R'
271600990506     C                   PARM                    KPJBA
271700990506     C                   PARM                    tibs02ds
271800990506      *
271900990506    1C                   if        t02err = *BLANKS
272000990506     c                   movel     T02UNI        DSTD
272100990506    1C                   endif
272200980625
272300150630     c                   z-add     1             CodUt             1 0
272400150630      *
272500971020     C     KTAB          KLIST
272600971020     C                   KFLD                    CODUT
272700971020     C                   KFLD                    KCOD
272800971020     C                   KFLD                    KKEY
272900980625      *
273000980508     c     kbol01        klist
273100980515     c                   kfld                    i00aas
273200980515     c                   kfld                    i00lnp
273300980515     c                   kfld                    i00nrs
273400980515     c                   kfld                    i00nsp
273500991116      *
273600991116     c     kar6          klist
273700991116     c                   kfld                    i00aas
273800991116     c                   kfld                    i00lnp
273900991116     c                   kfld                    i00nrs
274000991116     c                   kfld                    i00nsp
274100991116     c                   kfld                    trcar6
274200991116      *
274300991116     c     kar6lb        klist
274400991116     c                   kfld                    lblaan
274500991116     c                   kfld                    lbllpn
274600991116     c                   kfld                    lblnrn
274700991116     c                   kfld                    lblnsn
274800991116     c                   kfld                    trcar6
274900980625      *
275000971020     C     KBOL          KLIST
275100980526     C                   KFLD                    arbaas
275200971020     C                   KFLD                    VTCLNP
275300971020     C                   KFLD                    VTCNRS
275400971020     C                   KFLD                    VTCNSP
275500980625      *
275600971021     C     KDET          KLIST
275700981113     C                   KFLD                    dctaae
275800981113     C                   KFLD                    dctnev
275900980625      *
276000971021     C     KDCS          KLIST
276100971021     C                   KFLD                    KTPD
276200971021     C                   KFLD                    KNKT
276300980609     C                   KFLD                    KSTD
276400980609     C                   KFLD                    KDIM
276500980609     C                   KFLD                    KHIM
276600971021     C                   KFLD                    KNKS
276700971021     C                   KFLD                    KTRC
276800981116      *
276900981116     C     KDCSpno       KLIST
277000981116     C                   KFLD                    KTPD
277100981116     C                   KFLD                    KNKT
277200981116     C                   KFLD                    KSTD
277300981116     C                   KFLD                    KDIM
277400981116     C                   KFLD                    KHIM
277500981116     C                   KFLD                    KNKS
277600981116     C                   KFLD                    KTRC
277700981116     C                   KFLD                    Kpno
277800980625      *
277900980518     c     kdcd          klist
278000980518     c                   kfld                    dctaac
278100980518     c                   kfld                    dctfil
278200980518     c                   kfld                    dctnca
278300080715     c     kdcdE         klist
278400080715     c                   kfld                    dctaac
278500080715     c                   kfld                    dctfil
278600080715     c                   kfld                    dctnca
278700080715     c                   kfld                    Knsc
278800981113      *
278900981113     c     kdet2         klist
279000981113     c                   kfld                    dctaad
279100981113     c                   kfld                    dctpod
279200981113     c                   kfld                    dctnde
279300980625      *
279400980526     c     kdcd2b        klist
279500980526     c                   kfld                    artfls
279600980526     c                   kfld                    artlna
279700980526     c                   kfld                    artnrs
279800980526     c                   kfld                    artnsc
279900980526     c                   kfld                    i00aac
280000980526     c                   kfld                    i00fil
280100980526     c                   kfld                    i00nca
280200080715     c
280300080715     c     kbrv          klist
280400080715     c                   kfld                    dcdfls
280500080715     c                   kfld                    dcdlna
280600080715     c                   kfld                    dcdnrs
280700080715     c                   kfld                    dcdnsc
280800080715     c     kBRVE         klist
280900080715     c                   kfld                    brvnpg
281000080715     c                   kfld                    brvnfv
281100080715     c                   kfld                    brvfgs
281200080715     c                   kfld                    dcdfls
281300080715     c                   kfld                    dcdlna
281400080715     c                   kfld                    dcdnrs
281500080715     c                   kfld                    dcdnsc
281600980625      *
281700980520     c     knumca        klist
281800980520     c                   kfld                    i00aac
281900980520     c                   kfld                    i00fil
282000980520     c                   kfld                    i00nca
282100981020      *
282200981020     c     KDKA          klist
282300981113     c                   kfld                    dctaac
282400981113     c                   kfld                    dctfil
282500981113     c                   kfld                    dctnca
282600981020     c                   kfld                    Wtrc
282700980625      *
282800980513     C     KACO          KLIST
282900980513     C                   KFLD                    codut
283000980513     C                   KFLD                    CCC
283100981022     C                   KFLD                    Wksc
283200981113      *
283300981113     c     kdcc          klist
283400981113     c                   kfld                    dctaac
283500981113     c                   kfld                    dctfil
283600981113     c                   kfld                    dctnca
283700990315      *
283800990315     C     Kabi          KLIST
283900990315     c                   kfld                    wabi
284000990315     c                   kfld                    wcab
284100980721      *
284200951009     C                   ENDSR
284300150630
284400150630      *---------------------------------------------------------------*
284500150630      * Reperimento Dati del job (Utente/Operativi)                   *
284600150630      *---------------------------------------------------------------*
284700150630     c     DatiJob       BegSr
284800150630      *
284900150630     c     *dtaara       define    §azute        azuteds
285000150630     c     *dtaara       define    §datiute      ddatiute
285100150630      *
285200150630     c                   in(E)     *dtaara
285300150630     c                   IF        %ERROR or RSUT = *blanks
285400150630     c                   clear                   Tibs34Ds
285500150630     c                   call      'TIBS34R'
285600150630     c                   parm                    Tibs34Ds
285700150630     c                   in        *dtaara
285800150630     c                   ENDIF
285900150630      *
286000150630     c                   EndSr
286100981112     c*--------------------------------------------------------------------------------------------
286200981112** CMD  L78                                                                  *
286300990125F3=Fine   F12=Ritorno  F20=Storico Fasi  F15=Importi  F18=Note F24=Altri Tasti
286400990128F10=Note Stampa  F13=Documenti Richiesti F19=Visualiz.Bolla    F24=Altri Tasti
286500990128                            F14=Responsabilità  F21=Segnacolli F24=Altri Tasti
286600990125                            F7=Evento           F21=Segnacolli F24=Altri Tasti
286700990128                                                F21=Segnacolli F24=Altri Tasti
286800981117** CMD2 L25                                                                  *
286900981130F8=Benefic.
287000981117F9=Rivalsa
287100981130F8=Benefic.  F9=Rivalsa
287200981112** MSG  L78                                                                  *
287300981116F7=Evento  ERRATO!!!  Non ci sono Eventi da interrogare                        1
287400981130F8=Benefic.ERRATO!!!  Non ci sono Dati relativi al BENEFICIARIO ALTERNATIVO    2
287500981116F9=Rivalsa ERRATO!!!  Non c'è Rivalsa                                          3
287600981113F21=Segnacol.   ERRATO!!!  Non c'è dettaglio segnacolli                        4
287700981113F13=Documenti Richiesti  ERRATO!!! Non ci sono Documenti richiesti al cliente  5
287800981116F8=Intestatario  ERRATO!!! Non esistono dati relativi all'INTESTATARIO P.L.    6
287900080715Per la spunta senza anomalia, posizionarsi sul segnacollo e premere F5         7
288000150630F8=Beneficiario  ERRATO!!! Non esistono dati relativi al BENEFICIARIO          8
288100981201**  Sol Fasi solleciti
288200981201575605615630
