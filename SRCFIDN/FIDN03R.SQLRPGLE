000100120723     /*PRM  dbgview(*source)
000200120723     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
000300120723     /*CMD         ovrscope(*calllvl)
000400120723     /*CMD  ovrdbf file(TNTMD01L) tofile(GAITRAGRPS/TNTMD01L) +
000500120723     /*CMD         ovrscope(*calllvl)
000600120723     /*CMD  ovrdbf file(TNCSB03L) tofile(GAITRAGRPS/TNCSB03L) +
000700120723     /*CMD         ovrscope(*calllvl)
000800120723     /*END  dltovr file(TITAS30C TNTMD01L TNCSB03L) lvl(*)
000900120723     /*END
001000980618      *-------------------------------------------------------------------------*
001100980619      *    APERTURA C.A. - GESTIONE CONFERMA - MANUTENZIONE -                   *
001200980618      *-------------------------------------------------------------------------*
001300980831
001400980831      ****************************************************************
001500980831      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
001600980831      ****************************************************************
001700981014      * 010 = Apertura C.A.                                 WFAPE
001800981214      * 025 = Lettera per avaria                            WFLAV
001900981214      * 050 = Sollecito lettera per avaria                  WFSLA
002000981015      * 100 = Conferma C.A.                                 WFCON
002100080310      * 110 = Fittizia in filiale                           WFFFF
002200121011      *//* 215 = L.T. - Esito contatto telefonico beneficiario WFECB
002300121011      *//* 190 = L.T. - Stampa Comunicazione al Cliente        WFSCC
002400121011      *//* 400 = Preavviso di danno                            WFPAD
002500981223      * 545 = Stampa Avviso Danno                           WFSAD
002600981224      * 550 = Modifica rivalsa                              WFMRI
002700990205      * 690 = Ristampa Progetto di Liquidazione             WFRPL
002800990205      * 700 = Stampa Progetto di Liquidazione               WFSPL
002900980831      ***************************************************************
003000980715
003100980625      ****************************************************************
003200980625      *  RIEPILOGO INDICATORI
003300980625      ***************************************************************
003400980728      * 01   - APERTURA C.A.
003500980728      * 02   - CONFERMA C.A.
003600980728      * 03   - MANUTENZIONE
003700990406      * 04   - MANUTENZIONE RESPONSABILITA'
003800990121      * 05   - C.A. non confermata ACCESO - C.A. in conferma o confermata SPENTO
003900981126      * 06   - C.A CONFERMATA
004000990128      * 07   - EMETTO importo concordato
004100990510      * 08   - EMETTO responsabiltà per sped. Internazionali
004200990128      * 09   - EMETTO importo trattenuto
004300981229      * 10   - VISUALIZZO importo da assicurare
004400981229      * 11   - VISUALIZZO C/Assegno
004500981229      * 12   - VISUALIZZO Totale fattura immediata
004600990210      * 13   - VISUALIZZAZIONE data stampa copia denuncia
004700990128      * 15   - ABILITO F8 nella prima videata (Manutenzione anagrafica clienti)
004800990128      * 16   - DISABILITO F12 e F03 in video1
004900990128      * 18   - ABILITO F07 = Manutenzione Bolla
005000990128      * 19   - ABILITO F16 = Annullamento C.A.
005100980728      * 20   - COMODO Gestione subfile
005200980728      * 21   - COMODO Gestione subfile
005300980728      * 22   - COMODO Gestione subfile
005400980728      * 23   - COMODO Gestione subfile
005500020124      * 24   - F18 Lampeggiante e ad Alta intensità se ho note nuove
005600020129      * 25   - ABILITATO F5 = Visualizza Evento
005700980728      * 28   - ERRORE GENERICO DSPF
005800980728      * 29   - RIEMISSIONE FORMATO VIDEO
005900980728      * 30   - COMODO
006000980728      * 31   - COMODO
006100980728      * 32   - COMODO
006200020124      * 33   - EMETTO flag franchigia
006300990308      * 34   - EMETTO beneficiario
006400990128      * 35   - PROTEGGO rimborso a carico
006500981209      * 36   - PROTEGGO rivalsa
006600990128      * 37   - PROTEGGO l'interlocutore
006700990128      * 38   - PROTEGGO il beneficiario
006800990128      * 39   - PROTEGGO il beneficiario alternativo
006900990129      ***  Video 1
007000981021      * 40   - ERRORE tipo anomalia
007100980728      * 41   - ERRORE data apertura C.A.
007200980728      * 42   - ERRORE numero evento
007300981019      * 43   - ERRORE verifica esistenza DDT
007400981126      * 44   - ERRORE flag tipo gestione
007500990210      * 45   - ERRORE riserva
007600981127      * 46   - ERRORE beneficiario
007700981127      * 47   - ERRORE beneficiario alternativo
007800980728      * 48   - ERRORE descrizione aggiuntiva anomalia
007900980728      * 49   - ERRORE motivo del danno
008000000628      * 50   - ERRORE Bolla posta senza numero evento
008100020124      * 51   - ERRORE flag FRANCHIGIA
008200081111      ***  Prtf mail a servizi assicurativi
008300081111      * 52   - In caso di mail per mancanza ammanco
008400081111      ***  LIBERI
008500990210      * 53   -
008600990210      * 54   -
008700990210      * 55   -
008800990210      * 56   -
008900990210      * 57   -
009000990210      * 58   -
009100990210      * 59   -
009200990129      ***  Video per inserimento anagrafiche, beneficiario alternativo o rivalsa
009300980728      * 60   - ERRORE ragione sociale
009400980728      * 61   - ERRORE indirizzo
009500980728      * 62   - ERRORE C.A.P.
009600980728      * 63   - ERRORE località
009700980728      * 64   - ERRORE provincia
009800980728      * 65   - ERRORE nazione
009900980728      * 66   - ERRORE fax
010000980728      * 67   - ERRORE telefono
010100980728      * 68   - ERRORE Partita IVA
010200990128      * 69   - PROTEGGO campi window anagrafica
010300990129      ***  Video dettaglio segnacolli
010400981006      * 70   - BLOCCO tipo scelta segnacolli (control)
010500980728      * 71   - ERRORE tipo anomalia segnacolli
010600980728      * 72   - ERRORE campo scelta segnacolli
010700981229      * 73   - PROTEGGO campo scelta segnacollo se presente su altra c.a.
010800981229      * 74   - REVERSE image record di sfl dettaglio colli per colli in c.a
010900980728      * 75   -
011000990129      ***  Video 2
011100981029      * 76   - ERRORE importo trattenuto
011200981029      * 77   - ERRORE rivalsa
011300981021      * 78   - ERRORE data importo trattenuto
011400981209      * 79   - ERRORE rimborso a carico...
011500990510      * 80   - ERRORE responsabilità Internazionale
011600980728      * 81   - ERRORE colli danneggiati/mancanti
011700980728      * 82   - ERRORE Kg danneggiati/mancanti
011800981113      * 83   - ERRORE importo massimo risarcibile
011900990129      * 84   - ERRORE importo concordato
012000981020      * 85   - ERRORE disposizioni merce danneggiata
012100980918      * 86   - ERRORE pezzi danneggiati/mancanti
012200980918      * 87   - ERRORE descrizione pezzi danneggiati/mancanti
012300990129      * 88   -
012400990129      * 89   -
012500981029      * 90   -
012600990129      ***  Video responsabilità
012700990210      * 91   - PROTEGGO campi di immissione
012800990208      * 92   - ERRORE numero colli
012900990208      * 93   - ERRORE P.O. responsabile
013000990129      * 94   -
013100990129      * 95   -
013200990129      * 96   -
013300990129      * 97   -
013400990129      * 98   -
013500050217      * 99   - errore qcmdexc
013600980625      ***************************************************************
013700980618
013800080604     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
013900980625      * ---------------------------------------------------------
014000981021     fFIDN03D   CF   E             WORKSTN sfile(FI03S04:nrr1)
014100990209     f                                     sfile(FI03S05:nrr5)
014200981005     fTABEL00F  IF   E           K DISK
014300990129     fFNDCT01L  UF A E           K DISK
014400990129     fFNDCS01L  UF A E           K DISK
014500050805     fFNDET01L  UF   E           K DISK
014600981005     fFNDED01L  IF   E           K DISK
014700990209     fFNDCF01L  IF A E           k DISK
014800990209     fFNDCD01L  IF A E           k DISK
014900990209     fFNDCD02L  UF A E           k DISK    rename(fndcd000:fndcd002)
015000990209     fFNDCL01L  UF A E           k DISK
015100990209     fFNDKA01L  UF A E           k DISK
015200050915     fFNDCK01L  UF A E           K DISK
015300990407     fFNDPT01L  UF A E           k DISK
015400981005     fAZORG01L  IF   E           K DISK
015500990407     fFNDCR02L  UF A E           k DISK     usropn                                          pgm
015600060309     fTNTAM01L  IF   E           K DISK
015700131106     fazcae05l  IF   E           K DISK
015800981006      * filiale
015900010528     fFNEVB04L  IF   E           K DISK     usropn                                          pgm
016000981127     fFNDFA01L  IF   E           K DISK     usropn                                          gestione
016100040525     fFNSPE01L  IF   E           K DISK
016200981127     fFLNUF01L  UF   E           K DISK     usropn                                          apertura
016300981224     fFNLBL01L  IF   E           K DISK     usropn                                          apertura
016400990407     fFNLBL02L  IF   E           K DISK     usropn     rename(fnlbl000:fnlbl002)
016500990518     ffnanm02L  IF   E           K DISK     usropn
016600120111     F                                      INFDS(FNANM2)
016700981006     fFNBLT01L  IF   E           K DISK     usropn
016800020827     f                                      prefix(ART:3)
016900060217     fFiAR401L  IF   E           K DISK     usropn
017000991104     fFIAR601L  IF   E           K DISK     usropn
017100051114     fFiAR901L  IF   E           K DISK     usropn
017200981006     fFNART01L  IF   E           K DISK     usropn
017300990607     fFNBLP01L  UF   E           K DISK     usropn
017400990202     fFNARB01L  IF   E           K DISK     usropn
017500050331     fTigcp21l  IF   E           K DISK     usropn
017600981006      * sede
017700990806     fTITAS30C  IF   E           K DISK     usropn
017800050218     fTNTMD01L  uF a E           K DISK     usropn
017900020827     f                                      prefix(ARB:3)
018000981229     fTNCSB03L  IF   E           K DISK     usropn
018100050523      *
018200050523     fFIDN03P   o    e             printer usropn
018300980611      * ---------------------------------------------------------
018400050217     d cmd             s             60    dim(01) ctdata perrcd(1)
018500180130     D MSG             S             78    DIM(123) CTDATA PERRCD(1)            MSG VIDEO
018600050601     D CMM             S             70    DIM(1) CTDATA PERRCD(1)
018700980508     D L6              S              3  0 DIM(30)                              P.O.GESTITI
018800990217     D D7O             S              1    DIM(30)                              Con. Anom. Dirott.
018900990217     D R7O             S              1    DIM(30)                              Con. Anom. Reso
019000101007     d s2A             s              3    DIM(30)                              Eventi Riserva
019100990311     D S7C             S              3  0 DIM(30)                              Anomalia IDD
019200981006     D nsc             S              7  0 DIM(100)
019300980623     D aac             S              4  0 DIM(100)
019400980623     D fil             S              3  0 DIM(100)
019500980623     D nca             S              7  0 DIM(100)
019600990309     D lna             S              3  0 DIM(100)
019700131105     D RS1             S             22    DIM(900)                             NSC-%res-PO-TRE prim
019800131105     D RS2             S             22    DIM(900)                             NSC-%res-PO-TRE dopo
019900000307     D RSU             S             14    DIM(900)                             NSC-PO-TRE  univoche
020000000307     D sec             S              7  0 DIM(900)                             Segnacolli C.A.
020100000307     D per             S              6  2 DIM(900)                             colli x Segnacolli
020200050217     d comman          s             80
020300050217     d lenght          s             15  5 inz(80)
020400050601     d Qcomm           s            500
020500050601     d Qleng           s             15  5 inz(500)
020600050217     d wlib            s             10    inz('GAITRAGRU ')
020700050217     d wlibp           s             10    inz('GAITRAGRPS')
020800980717
020900050523      * - Parametri x Controllo profilo utenti
021000050523     d TIBS34ds      e ds                  inz
021100050523      * - Ds di riferimento al file esterno AZUTE00F
021200050523     d AZUTEds       e ds                  extname(AZUTE00F)
021300050523      * - Ds per dati organigramma
021400050523     d dDatiUte      e ds
021500130806
021600130806     d dUTE01        e ds
021700050523      *
021800981020     D FIDN00DS      E DS
021900981029     d  ds_numca               6     19
022000980717     D FIDN05DS      E DS
022100020129     D FIDN06DS      E DS                  INZ
022200980723     D FIDN07DS      E DS
022300981020     D FIDN10DS      E DS
022400981020     D FIDN11DS      E DS
022500990409     D FIDN30DS      E DS                  INZ
022600120507     d FIDN40DS      e ds
022700990408     D FIDN42DS      E DS
022800990407     D FIDN48DS      E DS
022900990216     D FIDN50DS      E DS
023000990226     D FIDN53DS      E DS
023100981020     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
023200000420     D TIBS20DS      E DS
023300000420     D TIBS02DS      E DS
023400981015     d FNLV55DS      E DS
023500160818     d*?DS PER FNLV59R - CARICAMENTO E CONTROLLO TARIFFE DI 1 CLIENTE?
023600160818     d fnlv59ds      E DS
023700990409     d TIBS69DS      E DS                  INZ
023800981005     d DS_cnaco      E DS                  extname(CNACO00F)
023900981005     d DS_cnind      E DS                  extname(CNIND00F)
024000981005     d DS_cnclp      E DS                  extname(CNCLP00F)
024100981005     d DS_fncls      E DS                  extname(FNCLS00F)
024200981020     d TISI95DS      E DS
024300981020     d TRUL42DS      E DS
024400981020     d FNLV13DS      E DS
024500981021     d FNLV14DS      E DS
024600991104     d FNLV16DS      E DS
024700120111     d fnlr33ds      E DS
024800120507
024900120507     d TrulIbanI0    e ds                  qualified
025000121011     d*// TrulIbanI1    e ds                  qualified
025100120507     d TrulIbanO0    e ds                  qualified
025200121011     d*// TrulIbanO1    e ds                  qualified
025300050523      *
025400050601      * Parametri x invio e-mail
025500050601     d dmradan       e ds
025600050601     d trtcm1ds      e ds
025700050601      *
025800130404       // -?File Clienti abilitati alla chiusura automatica C.A.
025900130404     d FNDKLDS       e ds                  extname(FNDKL00F)
026000130404       // -?Parametri per Reperimento ultimo numero C.A. chiuse
026100130404     d TRUL33DS      e ds
026200130404       // -?DS per richiamo pgm Chiusura C.A.
026300130404     d FIDN22DS      e ds
026400130404
026500050523      *
026600981020     D TRUL06DS      E DS
026700980611     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
026800170428
026900170428     D FNLV50DS      E DS
027000000427     d DSBL4A        E DS
027100981020     D KPJBA         E DS
027200981020     D DTAD          E DS
027300981020     D DTADEVE       E DS                  extname(DTAD)  prefix(E)
027400990217     D DTRE          E DS
027500981020     D DSTD          E DS
027600080208     D DLLT          E DS
027700981020     D DSTB          E DS
027800101007     d ds2A          e ds
027900981020     D DS3A          E DS
028000990602     D DS3K          E DS
028100990311     D DS7C          E DS
028200981020     D DSCV          E DS
028300981020     D DS5A          E DS
028400010910     D DGEDDN        E DS
028500010412     D DGEI          E DS
028600981020     D DS7O          E DS
028700981228     D DDCT01        E DS
028800990216     D DDCR01        E DS
028900020201     D DDCF01        E DS                  inz
029000981020     D DS_FI03W02    E DS                  extname(FIDN03D:FI03W02)
029100981022     D DS_W02CCON    E DS                  extname(FIDN03D:FI03W02)  Prefix(C)
029200981022     D DS_W02CRIV    E DS                  extname(FIDN03D:FI03W02)  Prefix(R)
029300981022     D DS_FNDKA      E DS                  extname(FNDKA00F)
029400981022     D DS_CLICON     E DS                  extname(FNDKA00F)  Prefix(C)
029500121011     D*// DS_CLIRIV     E DS                  extname(FNDKA00F)  Prefix(R)
029600121011     D*// TNSYM5CDS     E DS
029700991104     D DTASV         E DS
029800000202     D OG143         E DS
029900070402     d og148         e ds
030000070402     d dslr02        e ds
030100010904     D YEURCODS      E DS                  inz
030200010904     D   YECTAR      E                     inz('H')
030300011017     D XDECDS        E DS                  inz
030400011017     D   OCDESI      E                     inz(*OFF)
030500130404
030600130404       // -?Tabella DKL - Clienti abilitati alla chiusura automatica C.A.
030700130404     d dDKL          e ds
030800040720      *
030900040720     D FIDN12DS      E DS
031000040720     d  skKey                 26    305    dim(20)
031100050218     d Fidn58ds      e ds
031200120111     d
031300120111     D FNANM2          DS
031400120111     D  ANMNR2               397    400B 0
031500040720
031600040720     d dsKey           ds
031700040720     d  dsKaac                        4  0
031800040720     d  dsKfil                        3  0
031900040720     d  dsKnca                        7  0
032000980615
032100980520     d                 ds
032200981029     d  DS_dctkey              1     14
032300980520     d  dctaac                 1      4  0
032400980526     d  dctfil                 5      7  0
032500980526     d  dctnca                 8     14  0
032600981023
032700981029     d                 ds
032800981029     d  DS_dcdkey              1     14
032900981029     d  dcdaac                 1      4  0
033000981029     d  dcdfil                 5      7  0
033100981029     d  dcdnca                 8     14  0
033200981029
033300980610     d                 ds                  INZ
033400980727     d  DS_arbdat              1      8  0
033500980525     d  arbaas                 1      4  0
033600980525     d  arbmgs                 5      8  0
033700981009
033800981009     d                 ds                  INZ
033900981009     d  DS_lblori              1     16  0
034000981009     d  lblaao                 1      4  0
034100981009     d  lbllpo                 5      7  0
034200981009     d  lblnro                 8      9  0
034300981009     d  lblnso                10     16  0
034400981009
034500981009     d                 ds                  INZ
034600981009     d  DS_lblpre              1     16  0
034700981009     d  lblaap                 1      4  0
034800981009     d  lbllpp                 5      7  0
034900981009     d  lblnrp                 8      9  0
035000981009     d  lblnsp                10     16  0
035100990419
035200990420     d                 DS                  INZ
035300990419     d  DS_segnca              1      8  0
035400990419     d  wdcdfls                1      3  0
035500990419     d  wdcdlna                4      6  0
035600990419     d  wdcdnrs                7      8  0
035700980615
035800990420     d                 DS
035900990419     d  DS_segnart             1      8  0
036000990419     d  artfls                 1      3  0
036100990419     d  artlna                 4      6  0
036200990419     d  artnrs                 7      8  0
036300050523
036400050523     d DS_PO           ds                  inz
036500050523     d  ORGfil                             inz
036600050523     d  ORGdes                             inz
036700050523     d  ORGind                             inz
036800050523     d  ORGcpf                             inz
036900050523     d  ORGloc                             inz
037000050523     d  ORGpro                             inz
037100050523     d  ORGtel                             inz
037200050523     d  ORGfax                             inz
037300050523      *
037400050523     d Save_PO_P       s                   like(DS_PO) inz
037500050523     d Save_PO_A       s                   like(DS_PO) inz
037600990419
037700971016     D WLBDAT          DS                  INZ
037800971016     D  G02DAT                 1      8  0
037900971016     D  G02INV                 9     16  0
038000971016     D  G02ERR                17     17
038100971016     D  G02TGI                18     22  0
038200981020
038300971021     D                 DS
038400980521     D  V1CAAE                 1      4  0 inz
038500980520     D  V1CNEV                 5     11    inz
038600971021     D  CODEV                  1     11
038700980615
038800980429     D                 DS
038900980429     D  AA4                    1      4  0
039000980429     D  COMlin                 5      7  0
039100980429     D  WNUMCA                 8     14  0
039200980429     D  DSNCA                  1     14  0
039300980615
039400951009     D                SDS
039500971016     D  VTCPGM                 1     10
039600980615
039700971016     D TIT1            C                   CONST('   APERTURA   ')
039800971016     D TIT2            C                   CONST('   CONFERMA   ')
039900980511     D TIT4            C                   CONST(' MANUTENZIONE ')
040000980615
040100990113     d Datasped        S               D   DATFMT(*eur)
040200980623     D Wdataeur        S               D   DATFMT(*eur)
040300980623     D Wdataoggi       S               D   DATFMT(*eur)
040400000427     D Wsop            S                   LIKE(TASsop)
040500000427     D Wlna            S                   LIKE(ARBlna)
040600981228     D Wlnp            S                   LIKE(ARBlnp)
040700090324     D Wtfplnp         S                   LIKE(ARBlnp)
040800010912     D Wcolli          S                   LIKE(ARBncl)
040900990315     D Wv6spor         S                   LIKE(DCRpor)
041000990315     D Wv6snsc         S                   LIKE(§DCRnsc)
041100981228     D £DCTipv         S                   LIKE(DCTipv)
041200981130     D £DCLipr         S                   LIKE(DCLipr)
041300981130     D £DCLipt         S                   LIKE(DCLipt)
041400990128     D W£DCLipt        S                   LIKE(DCLipt)
041500981130     D £ARBias         S                   LIKE(ARBias)
041600990127     D wv2cipr         S                   LIKE(V2Cipr)
041700990127     D wv2cipt         S                   LIKE(V2Cipt)
041800981105     D wv2crag         S                   LIKE(V2Crag)
041900981105     D wdkarag         S                   LIKE(DKArag)
042000120712     ***D wv2cpgd         S                   LIKE(V2Cpgd)
042100981214     D Wi05fca         S                   LIKE(I05fca)
042200981214     D wisksc          S                   LIKE(ARBksc)
042300981029     D wisctr          S                   LIKE(ARBctr)
042400981029     D WDksc           S                   LIKE(V1Dksc)
042500990205     D wdclfrp         S                   LIKE(DCLfrp)
042600010912     D wdclcpr         S                   LIKE(DCLcpr)
042700010912     D wdclcpt         S                   LIKE(DCLcpt)
042800010912     D wdctcpv         S                   LIKE(DCTcpv)
042900981022     D wv2cdpt         S                   LIKE(V2Cdpt)
043000990209     D wdctcar         S                   LIKE(DCTcar)
043100981020     D Wtrc            S                   LIKE(DKAtrc)
043200981005     D Kaas2           S                   LIKE(EVBaas)
043300980625     D Kcod            S                   LIKE(TBLcod)
043400980623     D §cod            S                   LIKE(TBLcod)
043500980611     D Kkey            S                   LIKE(TBLkey)
043600980623     D §key            S                   LIKE(TBLkey)
043700980611     D ktpd            s                   LIKE(DCStpd)
043800980611     D knkt            s                   LIKE(DCSnkt)
043900980611     D kstd            s                   LIKE(DCSstd)
044000980722     D wstd            s                   LIKE(DCSstd)
044100980611     D knks            s                   LIKE(DCSnks)
044200980611     D ktrc            s                   LIKE(DCStrc)
044300980611     D kdim            s                   LIKE(DCSdim)
044400980611     D khim            s                   LIKE(DCShim)
044500981015     D kfgs            s                   LIKE(I00fgs)
044600981015     D Wv1cnev         s                   LIKE(DCTnev)
044700990303     D Wv1cksc         s                   LIKE(V1Cksc)
044800980611     D codevs          s                   LIKE(codev)
044900980611     D ultnrr1         s                   LIKE(NRR1)
045000990407     D WV2Cipv         s                   LIKE(V2Cipv)
045100981130     D WV2Cvpv         s                   LIKE(V2Cvpv)
045200981027     D WDCTfpv         s                   LIKE(DCTfpv)
045300981020     D wsvcap          s                   LIKE(W2Ccap)
045400980611     D wsvloc          s                   LIKE(W2Cloc)
045500980611     D wsvprv          s                   LIKE(W2Cprv)
045600990114     D WTcolli         s                   LIKE(ARBncl)
045700990209     D WV1Ccrc         s                   LIKE(V1Ccrc)
045800980611     D wmsg            s                   LIKE(O13msg)
045900980611     D wfzv            s                   LIKE(E13fzv)
046000980611     D wfz1            s                   LIKE(E13fz1)
046100980611     D wfz2            s                   LIKE(E13fz2)
046200980611     D wfz3            s                   LIKE(E13fz3)
046300980611     D paxccc          s                   LIKE(ACOkcc)
046400980611     D paxdmt          s                   LIKE(ACOrag)
046500981019     D wvlt            s                   LIKE(DCTvpv)
046600980625     D wfrg            s                   LIKE(GCPfrg)
046700990205     d Wv1cfpr1        s                   LIKE(V1Cfpr)
046800990205     d Wv1cfpr         s                   LIKE(V1Cfpr)
046900980709     d Wfpr            s                   LIKE(V1Cfpr)
047000980709     d Wptr            s                   LIKE(V1Cptr)
047100980720     d Wv2cncn         s                   LIKE(V2Cncn)
047200981019     d WV1Ctdt         s                   LIKE(V1Ctdt)
047300980720     d Wdataca         s                   LIKE(V1Cdca)
047400050217     d Wdtacainv       s                   LIKE(dctdch)
047500980722     d Wnot            s                   LIKE(DCSnot)
047600990407     d Wfvm            s                   LIKE(O48fvm)
047700990407     d Wvlm            s                   LIKE(O48vlm)
047800020211     d Wftc            s                   LIKE(O48ftc)
047900981230     D W§dctport       S                   LIKE(§DCTport)
048000031211     D w§STDlpf        s                   like(§STDlpf1) inz(*hival)
048100080208     D W§stdllt        S                   LIKE(§STDllt)
048200981113     D Kfls            S                   LIKE(SPEfls)  inz('L')
048300981113     D Kcli            S                   LIKE(SPEcli)
048400981113     D Kclu            S                   LIKE(SPEcod)  inz('006')             Luogo per danni
048500020419     D Wntw            S                   LIKE(§ogntw)
048600170509     D Wlnpntw         S                   LIKE(§ogntw)
048700060313     D Wtamfmp         S                   LIKE(tamfmp)
048800060313     D Ktamctr         S                   LIKE(tamctr)
048900050805     d SAVaae          s                   like(DCTaae)   inz
049000050805     d SAVnev          s                   like(DCTnev)   inz
049100120111     d kcaa            s                   like(anmcaa)   inz(115)
049200981124
049300981214     d WFape           s                   LIKE(DCTfca)   inz(010)
049400981214     D WFlav           S                   LIKE(DCTfca)   INZ(025)
049500981214     D WFsla           S                   LIKE(DCTfca)   INZ(050)
049600981124     d WFcon           s                   LIKE(DCTfca)   inz(100)
049700080310     d WFfff           s                   LIKE(DCTfca)   inz(110)
049800121011     d*// WFecb           s                   LIKE(DCTfca)   inz(215)
049900121011     d*// WFpad           s                   LIKE(DCTfca)   inz(400)
050000990127     d WFsad           s                   LIKE(DCTfca)   inz(545)
050100981224     d WFmri           s                   LIKE(DCTfca)   inz(550)
050200990205     d WFrpl           s                   LIKE(DCTfca)   inz(690)
050300990205     d WFspl           s                   LIKE(DCTfca)   inz(700)
050400121011     d*// WFscc           s                   LIKE(DCTfca)   inz(190)
050500991116     d TRCAR6          s                   LIKE(AR6TRC)
050600980625
050700990601     d Wgiace          s              1
050800990601     d Wemd02          s              1
050900981126     d Wdfagga         s              3
051000000420     d Wmsg33frz       s              1
051100990301     d Wmsg35frz       s              1
051200990301     d Wmsg46frz       s              1
051300990226     d Wmsg63frz       s              1
051400121011     d*// Wmsg73frz       s              1
051500990112     d Wmsg85frz       s              1
051600990226     d Wmsg97frz       s              1
051700050428     d Wmsg98frz       s              1
051800051019     d Wmsg113frz      s              1
051900080229     d Wmsg116frz      s              1
052000981022     D wfndcl          s              1
052100990407     D wlegami         s              1
052200110419     D wdirott         s              1
052300990115     d Wclsflo5        s              1
052400120712     ***d Wclsflo3        s              1
052500981020     d Wtadtipo        s              1
052600990311     d Widdaperto      s              1
052700990323     d Wctridd         s              1
052800981022     d Wclicon         s              1
052900981019     d Wcardcd         s              1
053000981006     d Wfprfrz         s              1
053100990205     d Wnoriv          s              1
053200990208     d Wcalresp        s              1
053300980625     D Wkey1           s             15
053400980623     D §des            S             30
053500121011     D*// des             S             25
053600990226     D wclipar         s              1
053700990408     D wfndpt          s              1
053800990408     D wmand           s              1
053900981005     D wtibs69r        s              1
054000990408     D wfidn48r        s              1
054100990217     D wreso           s              1
054200990304     D wriserva        s              1
054300121011     D*// wtipomsg        s              1
054400121011     D*// wfil            s              3
054500121011     D*// waac            s              4
054600990209     D kcap            s              9
054700990209     D werr            s              1
054800990209     D wvalvar         s              1
054900990209     D PAXdut          s             30
055000990209     D PAXdit          s              3
055100990209     D PAXflr          s             90
055200980626     D w001a           s              1
055300010910     D w002a           s              2
055400981104     D w003a           s              3
055500990218     D w005a           s              5
055600980619     D w007a           s              7
055700121011     D*// w007b           s              7
055800981104     D w010a           s             10
055900981104     D w010b           s             10
056000981104     D w011a           s             11
056100990218     D w014a           s             14
056200131105     D w022a           s             22
056300990210     D w0030           s              3  0
056400131106     D w0030p          s              3  0
056500990210     D w0033           s              3  3
056600990210     D w0040           s              4  0
056700990226     D w0070           s              7  0
056800990226     D w0080           s              8  0
056900980720     D w0133           s             13  3
057000120619     D totpunti        s             13  3
057100980720     D w0140           s             14  0
057200000308     D w0040b          s              4  0
057300000420     D wora4           s              4  0
057400990209     D §kut            s              1  0
057500990209     D PA4CNU          s              3S 0
057600990209     D wclinc          s              5  0
057700990209     D wtestca         s              1  0
057800121011     D*// NN              s              1  0
057900980708     D II              s              3  0
058000090924     D JJ              s              5  0
058100980626     D XX              s              2  0
058200981016     D Wsegnca         s              3  0
058300980611     D wdtgio          s              8  0
058400980611     D dateu           s              8  0
058500980611     D aas             s              2  0
058600980611     D cnu             s              3  0
058700980623     D nrr1            s              5  0
058800990209     D nrr5            s              5  0
058900981014     D Wvalass         s             13  3
059000981127     D Wlimris         s             13  3
059100990209     D PAXsta          s              1  0
059200000427     D Wlocale         s              1
059300000628     D lnppt           s              1
059400020910      * Flag booleani
059500020124     D $ErrF6          s              1    inz(*off)
059600121011     d*// $ChgBLP         s              1    inz(*off)
059700160729     d $EEX_Mail       s               n   inz
059800070402     d wfilgeo         s              1    inz(*off)
059900070402     d wmialna         s              1    inz(*off)
060000160729      *
060100130404     d wKSC            s                   like(DCTksc)
060200131106     d
060300131106     d wok_ter         s              1
060400131106     d vuotoTFP        s                   like(caetfp)
060500131106     d kepa            s                   like(caeepa)
060600990927      *
060700990927     D PE_TESTA1       C                   'Ristampa del progetto di liquidazio-
060800990927     D                                     ne'
060900990927     D PE_RIGA1_1      C                   'Il cliente ha trattenuto. Effettuar-
061000990927     D                                     e la ristampa del progetto di '
061100990927     D PE_RIGA1_2      C                   'liquidazione per la CA '
061200990927      *
061300990927     D PE_TESTA2       C                   'Manutenzionata rivalsa'
061400990927     D PE_RIGA2_1      C                   'Dopo la stampa del progetto di liqu-
061500990927     D                                     idazione è stata manutenzionata'
061600990927     D PE_RIGA2_2      C                   'la rivalsa per la CA '
061700990927      *
061800990927     D PE_SALUTI       C                   'Distinti Saluti.     Ufficio Danni'
061900020125      *
062000020130     d Comm_Fra_I      c                   const('Inserita Pratica in franchigi-
062100020125     d                                     a')
062200020130     d Comm_Fra_E      c                   const('Eliminata Pratica in franchig-
062300020125     d                                     ia')
062400050523
062500080310     d Comm_MTG_T      c                   const('Modifica gestione CA in Liq.T-
062600080310     d                                     rans.')
062700080310     d Comm_MTG_P      c                   const('Modifica gestione CA in Pra.A-
062800080310     d                                     ssic.')
062900080313     d Comm_MBEN_M     c                   const('Modifica Beneficiario in Mitt-
063000080313     d                                     ente.')
063100080313     d Comm_MBEN_D     c                   const('Modifica Beneficiario in Dest-
063200080313     d                                     inat.')
063300130408     d Comm_chiudi     c                   const('Chiusura Forzata dopo fase 10-
063400130408     d                                     0')
063500080313
063600050523     d C_Cmd_1         c                   const('DLTOVR  FILE(FIDN03P) +
063700050523     d                                            LVL(*JOB)')
063800120508
063900120508      // - Campi per il richiamo al TRULIBANR
064000120508     d rqsformatname...
064100120508     d                 S             10A
064200120508     d rqsData         s            256a
064300120508     d rqsdatasize...
064400120508     d                 S             10I 0
064500120508     d rqsopcode...
064600120508     d                 S             10A
064700120508     d rpyformatname...
064800120508     d                 S             10A
064900120508     d rpyData         s            256a
065000120508     d rpydatasize...
065100120508     d                 S             10I 0
065200120508     d rpyesito...
065300120508     d                 S             10I 0
065400120507
065500120507      //---------------------------------------------------------------
065600120507      //?Definizione procedure esterne.
065700120507      //---------------------------------------------------------------
065800120507      // - Coordinate bancarie
065900120507     d TrulIbanR       pr                  extpgm('TRULIBANR')
066000120507     d  rqsOpCode                    10a   const
066100120507     d  rpyEsito                     10i 0
066200120507     d  rqsFormatName                10a   const
066300120507     d  rqsData                     256a   options(*varsize)
066400120507     d  rqsDataSize                  10i 0 const
066500120507     d  rpyFormatName                10a   const
066600120507     d  rpyData                     256a   options(*varsize)
066700120507     d  rpyDataSize                  10i 0 const
066800130404
066900130404       // -?Reperimento dati tabelle
067000130404      /copy gaitrasrc/srcprotopr,tibs02r
067100130404       // -?Reperimento ultimo numero C.A. chiusa
067200130404      /copy gaitrasrc/srcprotopr,trul33r
067300130404
067400130404       // -?PGM chiusura C.A.
067500130404     d fidn22r         pr                  extpgm('FIDN22R')
067600130404     d  kpjba                              likeds(KPJBA) const
067700120507
067800980625      * ---------------------------------------------------------
067900971016     IFNBLP000
068000971020     I              BLPATB                      ARBATB
068100971016     I              BLPAAS                      ARBAAS
068200971016     I              BLPLNP                      ARBLNP
068300971016     I              BLPNRS                      ARBNRS
068400971016     I              BLPNSP                      ARBNSP
068500971016     I              BLPCBO                      ARBCBO
068600971016     I              BLPMGS                      ARBMGS
068700971016     I              BLPFNS                      ARBFNS
068800971016     I              BLPSCL                      ARBSCL
068900971016     I              BLPFBR                      ARBFBR
069000981104     I              BLPFST                      WDDT
069100971016     I              BLPFDN                      ARBFDN
069200971016     I              BLPLNA                      ARBLNA
069300971016     I              BLPZNC                      ARBZNC
069400971016     I              BLPKSC                      ARBKSC
069500971016     I              BLPRSM                      ARBRSM
069600971016     I              BLPINM                      ARBINM
069700971016     I              BLPCAM                      ARBCAM
069800971016     I              BLPLOM                      ARBLOM
069900971016     I              BLPNZM                      ARBNZM
070000971016     I              BLPPRM                      ARBPRM
070100971016     I              BLPFAP                      ARBFAP
070200971016     I              BLPCPI                      ARBCPI
070300971016     I              BLPRSD                      ARBRSD
070400971016     I              BLPIND                      ARBIND
070500971016     I              BLPCAD                      ARBCAD
070600971016     I              BLPLOD                      ARBLOD
070700971016     I              BLPNZD                      ARBNZD
070800971016     I              BLPPRD                      ARBPRD
070900971016     I              BLPFIN                      ARBFIN
071000971016     I              BLPGC1                      ARBGC1
071100971016     I              BLPGC2                      ARBGC2
071200971016     I              BLPCTR                      ARBCTR
071300971016     I              BLPCTS                      ARBCTS
071400971016     I              BLPFTM                      ARBFTM
071500971016     I              BLPTSP                      ARBTSP
071600971016     I              BLPNAS                      ARBNAS
071700971016     I              BLPNCL                      ARBNCL
071800971016     I              BLPNCP                      ARBNCP
071900971016     I              BLPPKC                      ARBPKC
072000971016     I              BLPPKB                      ARBPKB
072100971016     I              BLPPKF                      ARBPKF
072200971016     I              BLPNCR                      ARBNCR
072300971016     I              BLPVLC                      ARBVLC
072400971016     I              BLPFVB                      ARBFVB
072500971016     I              BLPVLB                      ARBVLB
072600971016     I              BLPFVF                      ARBFVF
072700971016     I              BLPVLF                      ARBVLF
072800971016     I              BLPQFT                      ARBQFT
072900971016     I              BLPVMD                      ARBVMD
073000971016     I              BLPVAD                      ARBVAD
073100971016     I              BLPIAS                      ARBIAS
073200971016     I              BLPVAS                      ARBVAS
073300971016     I              BLPCCM                      ARBCCM
073400971016     I              BLPRMN                      ARBRMN
073500971016     I              BLPRMA                      ARBRMA
073600971016     I              BLPRMO                      ARBRMO
073700971016     I              BLPFLS                      ARBFLS
073800971016     I              BLPNCD                      ARBNCD
073900971016     I              BLPNCA                      ARBNCA
074000971016     I              BLPXCO                      ARBXCO
074100971016     I              BLPCTM                      ARBCTM
074200971016     I              BLPFFD                      ARBFFD
074300971016     I              BLPTCR                      ARBTCR
074400971016     I              BLPDCR                      ARBDCR
074500971016     I              BLPHCR                      ARBHCR
074600971016     I              BLPDTI                      ARBDTI
074700971016     I              BLPHTI                      ARBHTI
074800971016     I              BLPDCE                      ARBDCE
074900971016     I              BLPNPI                      ARBNPI
075000971016     I              BLPNCI                      ARBNCI
075100971016     I              BLPNRC                      ARBNRC
075200971016     I              BLPDBR                      ARBDBR
075300971016     I              BLPNFV                      ARBNFV
075400971016     I              BLPDPC                      ARBDPC
075500971016     I              BLPDUC                      ARBDUC
075600971016     I              BLPFLP                      ARBFLP
075700971016     I              BLPDET                      ARBDET
075800971016     I              BLPDUT                      ARBDUT
075900971016     I              BLPDAM                      ARBDAM
076000971016     I              BLPNDC                      ARBNDC
076100971016     I              BLPDDC                      ARBDDC
076200971016     I              BLPDCP                      ARBDCP
076300971016     I              BLPDCM                      ARBDCM
076400971016     I              BLPHMC                      ARBHMC
076500971016     I              BLPTC1                      ARBTC1
076600971016     I              BLPTC2                      ARBTC2
076700971016     I              BLPCCA                      ARBCCA
076800971016     I              BLPTMC                      ARBTMC
076900971016     I              BLPGGA                      ARBGGA
077000971016     I              BLPGMA                      ARBGMA
077100971016     I              BLPGVA                      ARBGVA
077200971016     I              BLPFLE                      ARBFLE
077300971016     I              BLPTFP                      ARBTFP
077400971016     I              BLPFEA                      ARBFEA
077500971016     I              BLPTFA                      ARBTFA
077600971016     I              BLPFT1                      ARBFT1
077700971016     I              BLPDT1                      ARBDT1
077800971016     I              BLPFT2                      ARBFT2
077900971016     I              BLPDT2                      ARBDT2
078000981005
078100990806     ITITAS000
078200981005     I              TASAAS                      ARBAAS
078300981005     I              TASLNP                      ARBLNP
078400981005     I              TASNRS                      ARBNRS
078500981005     I              TASNSP                      ARBNSP
078600981005     I              TASMGS                      ARBMGS
078700981005     I              TASNCL                      ARBNCL
078800981005     I              TASKSC                      ARBKSC
078900981005     I              TASCCM                      ARBCCM
079000990806     I              TASPKF                      ARBPKF
079100981005     I              TASFVF                      ARBFVF
079200981005     I              TASVLF                      ARBVLF
079300981006     I              TASLNA                      ARBLNA
079400981013     I              TASIAS                      ARBIAS
079500981013     I              TASVAS                      ARBVAS
079600981013     I              TASTSP                      ARBTSP
079700981013     I              TASCTR                      ARBCTR
079800990208     I              TASTFP                      ARBTFP
079900990208     I              TASTFA                      ARBTFA
080000981104     I              TASLL1                      WDDT
080100990414     I              TASCCA                      ARBCCA
080200981005
080300990806     ITITAS010
080400981005     I              TASAAS                      ARBAAS
080500981005     I              TASLNP                      ARBLNP
080600981005     I              TASNRS                      ARBNRS
080700981005     I              TASNSP                      ARBNSP
080800981005     I              TASMGS                      ARBMGS
080900981005     I              TASNCL                      ARBNCL
081000981005     I              TASKSC                      ARBKSC
081100981005     I              TASCCM                      ARBCCM
081200990806     I              TASPKF                      ARBPKF
081300981005     I              TASFVF                      ARBFVF
081400981005     I              TASVLF                      ARBVLF
081500981006     I              TASLNA                      ARBLNA
081600981013     I              TASIAS                      ARBIAS
081700981013     I              TASVAS                      ARBVAS
081800981013     I              TASTSP                      ARBTSP
081900981013     I              TASCTR                      ARBCTR
082000990208     I              TASTFP                      ARBTFP
082100990208     I              TASTFA                      ARBTFA
082200981104     I              TASLL1                      WDDT
082300990414     I              TASCCA                      ARBCCA
082400981005
082500990806     ITITASP00
082600981005     I              TASAAS                      ARBAAS
082700981005     I              TASLNP                      ARBLNP
082800981005     I              TASNRS                      ARBNRS
082900981005     I              TASNSP                      ARBNSP
083000981005     I              TASMGS                      ARBMGS
083100981005     I              TASNCL                      ARBNCL
083200981005     I              TASKSC                      ARBKSC
083300981005     I              TASCCM                      ARBCCM
083400990806     I              TASPKF                      ARBPKF
083500981005     I              TASFVF                      ARBFVF
083600981005     I              TASVLF                      ARBVLF
083700981006     I              TASLNA                      ARBLNA
083800981013     I              TASIAS                      ARBIAS
083900981013     I              TASVAS                      ARBVAS
084000981013     I              TASTSP                      ARBTSP
084100981013     I              TASCTR                      ARBCTR
084200990208     I              TASTFP                      ARBTFP
084300990208     I              TASTFA                      ARBTFA
084400981104     I              TASLL1                      WDDT
084500990414     I              TASCCA                      ARBCCA
084600981229
084700981229     ITNCSB000
084800981229     I              CSBCAS                      AR9CAS
084900981229     I              CSBVCA                      AR9VCA
085000981229     I              CSBCMB                      AR9CMB
085100980625      * ---------------------------------------------------------
085200130404
085300130404      /free
085400130404         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
085500130404      /end-free
085600980615
085700980611      * se ho il numero spedizione chaino la bolla
085800980611     c                   IF        I00NSP > *ZEROS
085900981005      *
086000981228     c                   IF         i00tpb = 'S'                                SEDE
086100990408     c     kbol01        chain(N)  tntmd000                           30
086200990806     c     kbol01        chain     TITAS30c                           30
086300990408      *  se tipo bolla non è valido cerco altro rcd
086400981228     C                   movel     'TB'          kCOD
086500981228     C                   movel(P)  TAStbl        kKEY
086600990408     C     ktab          chain(N)  TABEL                              31
086700981228     C                   movel     TBLUNI        DSTB
086800981228     c                   if        §TBrbl = 'R'
086900990806     c     kbol01        reade     TITAS30c                               30
087000000427     c  n30              eval      Wsop = TasSop
087100981005     c                   endif
087200981005      *
087300981228     c                   ELSE
087400981005      *
087500981228     c                   If         i00tpb = 'P'
087600990202     c     kbol01        chain(N)  fnblp000                           30
087700000427     c  n30              eval      Wsop = BlpSop
087800981228     c                   Else
087900981228      *
088000990202     c     kbol01        chain(N)  fnarb000                           30
088100981104     c                   movel     'A'           WTRC
088200060217     c     KAR4          chain(N)  FiAR4000                           30
088300000427     c                   IF        %found
088400000427     c                   movel     AR4not        DsBl4a
088500060217     c                   else
088600060217     c                   clear                   dsbl4a
088700060217     c                   endif
088800060217     c
088900000427     c                   movel     §B4ABM        WDDT
089000000427     c                   movel     §B4ASO        Wsop
089100060217     c
089200981228     c                   Endif
089300000427      *
089400981228     c                   ENDIF
089500981005      *
089600990217     c                   if          *IN30 = *off
089700981126     c     *iso          move      DS_arbdat     datasped
089800990217     c                   if          ARBcca <> *blanks
089900990217     c     arbcca        lookup    R7O                                    30
090000990217     c   30              movel     'S'           Wreso
090100990217    1c                   endif
090200990217    1c                   endif
090300990217      *
090400980611     c                   ENDIF
090500980625
090600980625      * Carico i segnacolli che sono eventualmente su altre c.a. ed imposto WTESTCA
090700980625      *     WTESTCA = 0  non esistono altre CA per la spedizione
090800980625      *             = 1  esistono altre CA per la spedizione
090900990114      *             = 2  esistono altre CA per la spedizione, di cui una senza dettaglio segnacolli
091000990407     c                   EVAL      Wtestca = *Zeros
091100990407     c                   EXSR      CARNSC
091200980625
091300990407      * CONTROLLI PRELIMINARI PER APERTURA C.A.
091400981217     c                   IF        i00mod = 'A'
091500990113     c                   exsr      CTR_A
091600990113     c                   If        o00err <> *blanks
091700990113     c                   goto      fine
091800990113     c                   Endif
091900990407      *    imposto tipo porto, tipo sped.
092000990113     c                   exsr      IMP_A
092100981228     c                   ENDIF
092200980625
092300980618      * CONTROLLI PRELIMINARI PER CONFERMA C.A.:
092400980618      *   Se manca il numero spedizione non è possibile chiudere la  C.A.
092500980618     c                   IF        I00mod = 'G'  AND  I00nsp =  *zeros
092600980618     c                   movel     msg(38)       o00msg
092700980618     c                   eval      o00err = 'E'
092800980618     c                   goto      fine
092900980618     c                   ENDIF
093000981020
093100980716      *   G E S T I O N E   C A M P I   F O R M A T I   V I D E O
093200980716
093300980716      * Riempio testata
093400980716     C                   EXSR      RIETES
093500000428      *
093600000428      * Verifico se spedizione Locale (stessa L6)
093700170428      *  di fatto è lnp = lna. Quindi non mi interessa nemmeno per 196
093800000428     c     WLNP          lookup    l6                                     30
093900000428     c   30WLNA          lookup    l6                                     30
094000000428     c   30              eval      Wlocale = 'S'
094100070402
094200070402      * se filiale gestione ha la procedura telefonate GEO attiva controllo
094300070402      * anche se la LNA della bolla è in £6
094400070402     c                   if        wfilgeo = *on
094500070402     c     wlna          lookup    l6                                     30
094600170428     c                   if        *in30 and d50val_a='S'
094700070402     c                   eval      wmialna = *in30
094800070402     c                   endif
094900170428     c                   endif
095000981020      *
095100980716      * Riempio dettaglio video 1 e 2
095200980716     C                   EXSR      RIEDET
095300981020      *
095400981006      * Caricamento subfile elenco segnacolli.
095500990419      *    Carico da FNDCD se: Sede, non Apertura e non esistono rcd in ART/BLT, non Apertura ed i
095600990419      *                        segnacolli in ART/BLT non corrispondono con FNDCD (Bolle legate)
095700990420     c                   SELECT
095800990420      *
095900990420     c                   WHEN        SIMFEL = *zeros
096000990420     C                   exsr      RIES01S
096100990420      *
096200990420     c                   WHEN        I00mod = 'A'
096300990420     C                   exsr      RIES01
096400990420      *
096500990420     c                   OTHER
096600990420     c                   clear                   DS_segnart
096700990420     C                   If          I00tpb = 'A'
096800990420     C     KBOL          CHAIN(N)  FNART000                           31
096900990420     C                   Else
097000990420     C     KBOL          CHAIN(N)  FNBLT000                           31
097100990420     C                   Endif
097200990420     c                   If        *IN31 = *OFF  and  DS_segnca = DS_segnart
097300990420     C                   exsr      RIES01
097400990420     c                   Else
097500990420     C                   exsr      RIES01S
097600990420     C                   Endif
097700990420     c                   ENDSL
097800990420
097900990406      * Se manutenzione responsabilità salto a gestione responsabilità
098000990406     c   04              goto      responsab
098100990406
098200990310      * In apertura in entrata mi posiziono sul numero evento
098300990310     c                   eval      *in42 = (I00mod = 'A')
098400980626
098500980716      *            G E S T I O N E   V I D E O  1
098600971020     C     EMD01         TAG
098700990226      *
098800990301      * Gestione messaggi forzabili in video 1
098900990226     C                   If        V1CMSG  = *blanks
099000990226     c                   exsr      msgd01
099100990226     c                   Endif
099200981022      *
099300020129      * Attivo F5 su FI03D01/Z01
099400020129     c                   eval      *in25 = (*in06   = *on    and
099500020129     c                                      wV1Cnev > *zeros and
099600020129     c                                     (I00mod <> 'M' or DCTatb <> 'A'))
099700981209      * Attivo F8
099800990128     c                   eval      *IN15 = (Wclicon = 'S')
099900981022      *
100000971020     C                   WRITE     FI03T01
100100981209      * Se annullata videata a parte
100200981209     C                   if        I00MOD = 'M'  and  DCTatb = 'A'
100300981209     C                   WRITE     FI03Z03
100400981209     C                   EXFMT     FI03D03
100500981209     c                   else
100600981209     C                   WRITE     FI03Z01
100700981209     C                   EXFMT     FI03D01
100800981209     c                   endif
100900980626      *
101000971020     C                   CLEAR                   V1CMSG
101100990129     c                   eval      *in29 = *off
101200990129     c                   eval      *in28 = *off
101300980625
101400980625      * F3 = FINE
101500980623     C                   IF        *INKC = *ON
101600980626     C                   movel     'S'           O00F03
101700981020     C                   movel     FIDN00DS      KPJBU
101800980626     C                   goto      FINE
101900980623     C                   ENDIF
102000980720
102100990113      * F4 = NOTE STAMPA
102200990113     C                   IF        *INKD = *ON
102300990113     c                   exsr      SUB_F4
102400990409     C                   goto      EMD01
102500990113     C                   ENDIF
102600990113
102700980720      * F5 = RIPRISTINO
102800980720     C                   IF        *INKE = *ON
102900020129     C                             and I00MOD = 'M'  and  DCTatb = 'A'
103000980721      *
103100981016      *  Controllo se ci sono le condizioni per la riapertura
103200981016     c                   exsr      CTR_A
103300981016     c                   IF        O00err = 'E'
103400981016     c                   movel     O00msg        V1Cmsg
103500981016     c                   clear                   O00err
103600981016     c                   clear                   O00msg
103700981016     c                   eval      *in28 = *on
103800981016     c                   ELSE
103900990210      * Disalloco FNDCT000
104000990210     c                   except    FNDCTfree
104100990210      *
104200980721     c                   clear                   FIDN11DS
104300980721     c                   eval      I11mod = 'R'
104400980721     c                   move      VTCdc1        I11aac
104500980721     c                   eval      I11fil = VTCfil
104600980721     c                   eval      I11nca = VTCnca
104700980721     c                   call      'FIDN11R'
104800980721     c                   parm                    kpjba
104900980721     c                   parm                    fidn11ds
105000980721      *
105100981016     c                   IF        O11err = 'E'
105200980721     c                   movel     O11msg        V1Cmsg
105300980721     c                   eval      *in28 = *on
105400981016     C                   ENDIF
105500981016     C                   ENDIF
105600980721      *
105700981016     C   28              GOTO      EMD01
105800981016     C                   GOTO      FINE
105900980720     C                   ENDIF
106000020129
106100020129      * F5 = VISUALIZZA EVENTO
106200020129     C                   IF        *INKE = *ON  and  *in25 = *on
106300020129     C                   exsr      CallFIDN06
106400020129     C                   goto      EMD01
106500020129     C                   ENDIF
106600990409
106700990426      * F9 = MANDATO
106800990409     C                   IF        *INKI = *ON
106900990409     C                   call      'FIDN30R'
107000990409     C                   parm                    KPJBA
107100990409     C                   parm                    FIDN30DS
107200990409     C                   goto      EMD01
107300990409     C                   ENDIF
107400980625
107500980625      * F12 = RITORNO
107600980619     C                   IF        *INKL = *ON
107700980626     C                   movel     'S'           O00F12
107800981020     C                   movel     FIDN00DS      KPJBU
107900980626     C                   goto      FINE
108000980619     C                   ENDIF
108100980625
108200980625      * F14 = VARIAZIONE TIPO ANOMALIA
108300971020     C     *INKN         IFEQ      *ON
108400990209     c                   If        WDCTcar = 'R'  or  WDCTcar = 'T'
108500990209     c                   eval      *in28 = *on
108600990209     c                   movel     msg(91)       V1Cmsg
108700990209     c                   Else
108800980626     C                   exsr      VARTAD
108900990209     c                   Endif
109000980626     C                   goto      EMD01
109100971020     C                   END
109200980716
109300980716      * F16 = ANNULAMENTO
109400980716     C     *INKQ         IFEQ      *ON
109500990210      *   Disalloco FNDCT000
109600990210     c                   except    FNDCTfree
109700980721      *
109800980721     c                   clear                   FIDN11DS
109900050224     c                   eval      I11mod = '*'
110000980721     c                   move      VTCdc1        I11aac
110100980721     c                   eval      I11fil = VTCfil
110200980721     c                   eval      I11nca = VTCnca
110300980721     c                   call      'FIDN11R'
110400980721     c                   parm                    kpjba
110500980721     c                   parm                    fidn11ds
110600980721      *
110700980721     c                   if        O11err = 'E'
110800980721     c                   movel     O11msg        V1Cmsg
110900980721     c                   eval      *in28 = *on
111000990210      * Rialloco  FNDCT000
111100990210     c     knumca        CHAIN     fndct000                           30
111200980721     C                   GOTO      EMD01
111300980721     c                   else
111400060904      * se la cancellazione è andata a buon fine cancello anche TNTMD se non ci sono altre CA
111500060904      * aperte sulla stessa spedizione
111600060904      *
111700060904     c                   clear                   fidn12ds
111800060904     c                   eval      i12aas = i00aas
111900060904     c                   eval      i12lnp = i00lnp
112000060904     c                   eval      i12nrs = i00nrs
112100060904     c                   eval      i12nsp = i00nsp
112200060904     c                   eval      i12fel = 'E'
112300061003     c                   eval      i12fan = ' '
112400061003     c                   eval      i12fch = ' '
112500060904      *
112600060904     c                   call      'FIDN12R'
112700060904     c                   parm                    fidn12ds
112800060904      *
112900060904      * se trovata almeno una CA non deleto
113000060904     c                   if        o12nca = 0
113100060327
113200060327     c     kbol02        Chain     Tntmd01l
113300060327     c                   if        %found(tntmd01l)
113400060327     c                   delete    Tntmd000
113500060327     c                   endif
113600060904     c                   endif
113700060327
113800980721     C                   GOTO      FINE
113900980721     c                   endif
114000980721      *
114100981230     C                   ENDIF
114200980625
114300980625      * F18 = NOTE
114400971030     C     *INKS         IFEQ      *ON
114500980716     c                   exsr      SUB_F18
114600980623     C                   goto      EMD01
114700980623     c                   ENDIF
114800980625
114900981209      * Se C.A. annullata non eseguo i controlli
115000981209     C                   if        I00MOD = 'M'  and  DCTatb = 'A'
115100981209     C                   goto      EMD01
115200981209     C                   endif
115300981209
115400980619      * Controlli video 1
115500980615     C                   EXSR      CTRD01
115600990301      *
115700990301      * Gestione messaggi forzabili in video 1
115800990301     C                   If        V1CMSG  = *blanks
115900990301     c                   exsr      msgd01
116000990301     c                   Endif
116100981209      *
116200980623     C                   if        *IN28 = *ON  or  *IN29 = *ON
116300980623     C                   goto      EMD01
116400980623     C                   endif
116500980625
116600981126      * F08 = Manutenzione anagrafica Beneficiario alternativo
116700980625     c     *INKH         ifeq      *on
116800990128     c                   eval      *in69 = (*IN39 = *ON)
116900981022     c                   movel     DS_W02ccon    DS_FI03W02
117000981102     c                   movel     'C'           Wtrc
117100981022     c                   EXSR      mancli
117200981022     C                   GOTO      EMD01
117300980515     C                   END
117400990120
117500020129      * Attivo F5 su FI03D02/Z02
117600020129     c                   eval      *in25 = (*in06   =  *on    and
117700020129     c                                      wV1Cnev >  *zeros)
117800990120      * Dalla Conferma in poi se Pratica emetto importo trattenuto e data
117900990128     c                   eval      *IN09  =  (V1Cfpr = 'P'  and
118000990120     c                                        (*IN02 = *ON  or  *IN06 = *ON))
118100020124
118200020124     c                   eval      $ErrF6 = *off
118300980716
118400980716      *            G E S T I O N E   V I D E O  2
118500980716     C     EMD02         TAG
118600981022      *
118700980716     C                   WRITE     FI03T01
118800980716     C                   WRITE     FI03Z02
118900980716     C                   EXFMT     FI03D02
119000980716      *
119100980716     C                   CLEAR                   V1CMSG
119200990129     c                   eval      *in29 = *off
119300990129     c                   eval      *in28 = *off
119400980716
119500990113      * F4 = NOTE STAMPA
119600990113     C                   IF        *INKD = *ON
119700990113     c                   exsr      SUB_F4
119800990409     C                   goto      EMD02
119900990113     C                   ENDIF
120000020129
120100020129      * F5 = VISUALIZZA EVENTO
120200020129     C                   IF        *INKE = *ON  and  *in25 = *on
120300020129     C                   exsr      CallFIDN06
120400020129     C                   goto      EMD02
120500020129     C                   ENDIF
120600990409
120700990426      * F9 = MANDATO
120800990409     C                   IF        *INKI = *ON
120900990409     C                   call      'FIDN30R'
121000990409     C                   parm                    KPJBA
121100990409     C                   parm                    FIDN30DS
121200990409     C                   goto      EMD02
121300990409     C                   ENDIF
121400981012
121500981012      * F12 = RITORNO
121600981012     C                   IF        *INKL = *ON
121700981012     C                   goto      EMD01
121800981012     C                   ENDIF
121900980716
122000980716      * F18 = NOTE
122100980716     C     *INKS         IFEQ      *ON
122200980716     c                   exsr      SUB_F18
122300980716     C                   goto      EMD02
122400980716     c                   ENDIF
122500980716
122600980716      * Controlli video 2
122700980716     C                   EXSR      CTRD02
122800981020
122900990310      *  Per errore o riemissione forzata emetto VIDEO 2
123000990310     C                   If        *IN28 = *ON  or  *IN29 = *ON
123100990310     C                   goto      EMD02
123200990310     c                   Endif
123300990310      *
123400981126      * Per errore tipo gestione torno a VIDEO 1
123500990310     C                   If        *IN44 = *ON
123600990310     C                   goto      EMD01
123700990310     c                   Endif
123800981022
123900981022      * F08 = CLIENTE RIVALSA
124000990310     c     *INKH         ifeq      *ON
124100990128     c                   eval      *in69 = (*IN36 = *ON)
124200981105     c                   movel     V2Crag        RW2Crag
124300981022     c                   movel     DS_W02criv    DS_FI03W02
124400981102     c                   movel     'R'           Wtrc
124500981022     c                   exsr      MANCLI
124600981022     C                   GOTO      EMD02
124700981022     C                   END
124800981020
124900981020      * F21 = SEGNACOLLI
125000981020     C     *INKV         IFEQ      *ON
125100981020     C                   exsr      GESNSC
125200990210     C   KF              goto      RESPONSAB                                    Conferma -> RESPONSA
125300990210     C  NKF              goto      EMD02                                        No conferma -> VID2
125400981020     c                   ENDIF
125500980717
125600981020      * C O N T R O L L I   P E R   C O N F E R M A
125700981020
125800981020     c                   clear                   WEMD02
125900981020      *
126000981020     c                   SELECT
126100981022      *
126200981020      *  Per errore o riemissione forzata emetto video 2
126300981020     C                   WHEN      *IN28 = *ON  or  *IN29 = *ON
126400981020     C                   eval      WEMD02 = 'S'
126500981022      *
126600981104      *  Se Apertura per spedizione passo alla gestione segnacolli
126700020827     c                   WHEN      I00MOD = 'A'
126800981020     c                   exsr      GESNSC
126900981020     C   KL              eval      WEMD02 = 'S'
127000981022      *
127100981020      *  Se non ho premuto F6 emetto video 2
127200981020     c                   WHEN      *INKF = *OFF
127300981020     C                   eval      WEMD02 = 'S'
127400981022      *
127500981020      *  Se il n° colli danneggiati non è variato e corrisponde al n° segnacolli della C.A. confermo
127600990114     c                   WHEN      V2Cncn = WV2Cncn  and  V2Cncn = Wsegnca
127700990408     c                   exsr      AGGIORNA
127800981020     C   28              eval      WEMD02 = 'S'
127900981022      *
128000981020      *  Se il n° colli dann. è variato o non corrisponde al n° segnac. della C.A. passo a gest.seg.
128100990114     c                   WHEN      V2Cncn <> WV2Cncn  or  V2Cncn <> Wsegnca
128200981020     c                   exsr      GESNSC
128300981020     C   KL              eval      WEMD02 = 'S'
128400981022      *
128500981022      * Nei casi non contemplati emetto video 2   (PER SICUREZZA !!!!)
128600981020     c                   OTHER
128700981020     C   28              eval      WEMD02 = 'S'
128800981022      *
128900981020     c                   ENDSL
129000981020      *
129100981020     c                   IF        Wemd02 <> *blanks
129200981020     C                   goto      EMD02
129300981020     c                   endif
129400981020
129500990208      * Responsabilità
129600990210     C     RESPONSAB     TAG
129700990208     c                   Exsr      GESTresp
129800990208
129900990129      *------------------------
130000951009     C     FINE          TAG
130100990129      *------------------------
130200981020
130300980623      * Chiudo pgm di servizo
130400981020     c                   clear                   FIDN10DS
130500971031     c                   movel     'C'           i10tla
130600981020     c                   movel     FIDN10DS      kpjbu
130700971030     c                   call      'FIDN10R'
130800971030     c                   parm                    kpjba
130900981005      *
131000990311     C                   IF        Wtibs69r  = 'S'
131100981005     C                   EVAL      I69TLA  = 'C'
131200981005     C                   CALL      'TIBS69R'
131300981005     C                   PARM                    TIBS69DS
131400981005     C                   ENDIF
131500981029      *
131600981029     c                   clear                   tibs02ds
131700981029     c                   movel     'C'           t02tla
131800981029     c                   call      'TIBS02R'
131900981029     c                   parm                    kpjba
132000981029     c                   parm                    tibs02ds
132100981029      *
132200981029     c                   movel     FIDN00DS      kpjbu
132300981029      *
132400981005     C                   EVAL      *INLR = *ON
132500980615      ****************************************************************
132600981016      * CARICAMENTO SEGNACOLLI IN ALTRE C.A. E CONTEGGIO PROPRI
132700980615      ****************************************************************
132800981022     C     CARNSC        BEGSR
132900980625      *
133000980708     c                   clear                   II
133100990114     c                   clear                   Wsegnca
133200990114     c                   clear                   WTcolli
133300980618      *
133400040720      * cerco le CA aperte e non annulate legate alla spedizione
133500040720      *
133600040720     c                   clear                   fidn12ds
133700040720     c                   eval      i12aas = i00aas
133800040720     c                   eval      i12lnp = i00lnp
133900040720     c                   eval      i12nrs = i00nrs
134000040720     c                   eval      i12nsp = i00nsp
134100040720     c                   eval      i12fel = 'E'
134200040720     c                   eval      i12fan = 'E'
134300040720     c                   eval      i12fch = 'E'
134400040720      *
134500040720     c                   call      'FIDN12R'
134600040720     c                   parm                    fidn12ds
134700040720      *
134800040720     c                   z-add     *zeros        aa                2 0
134900040720      * se trovata almeno una CA
135000040720     c                   if        o12nca > 0
135100040720     c                   dow       aa <  o12nca
135200040720     c                   add       1             aa
135300040720     c                   movea     skkey(aa)     dskey
135400040720     C                   z-add     dsKaac        dctaac
135500040720     C                   z-add     dsKfil        dctfil
135600040720     C                   z-add     dsKnca        dctnca
135700040720      *
135800990114      * Esiste un'altra C.A.
135900990114     c                   IF         DS_DCTKEY <> DS_NUMCA
136000040720      * aggncio FNdct per controllare i colli
136100040720     c     kca           chain(N)  fndct01L
136200040720     c                   If        %found(fndct01L)
136300990114     c                   add       DCTncn        WTcolli                        Colli di altre C.A.
136400990114     c                   SELECT
136500990114     c                   WHEN       DCTNCN = *ZEROS
136600990114     c                   add       1             WTcolli
136700990114     c                   eval      wtestca = 2                                  Altra CA senza colli
136800990114     c                   WHEN       WTESTCA = *ZEROS
136900990114     c                   eval      wtestca = 1                                  Altra CA
137000990114     c                   ENDSL
137100990114     c                   ENDIF
137200040720     c                   ENDIF
137300990114      *
137400990209     c     kca           setll     fndcd000
137500990209     c     kca           reade(N)  fndcd000                               30
137600980625      *
137700990114     c     *IN30         DOWEQ     *OFF
137800990114      *
137900990114     c                   SELECT
138000990114      * Non considero annullati e chiusi
138100990114     c                   WHEN       DCDatb =  'A'  or  DCDdch <> *zeros
138200981016      * Conto i miei segnacolli
138300990114     c                   WHEN      DS_dctkey = DS_numca
138400990114     c                   add       1             Wsegnca
138500990419     c                   if         Wdcdlna = *zeros
138600990419     c                   z-add     DCDfls        wdcdfls
138700990419     c                   z-add     DCDlna        wdcdlna
138800990419     c                   z-add     DCDnrs        wdcdnrs
138900990419     c                   endif
139000981016      * Memorizzo segnacolli di altre C.A.
139100990114     c                   OTHER
139200980708     c                   add       1             II
139300981029     c                   z-add     DCDnsc        nsc(II)
139400990309     c                   z-add     DCDlna        lna(II)
139500981029     c                   z-add     DCTaac        aac(II)
139600981029     c                   z-add     DCTfil        fil(II)
139700981029     c                   z-add     DCTnca        nca(II)
139800990114     c                   ENDSL
139900981016      *
140000990209     c     kca           reade(N)  fndcd000                               30
140100981016     c                   ENDDO
140200990114      *
140300990114      * Se per la CA in esame non ho segnacolli imposto i colli danneggiati
140400990114     c                   IF        DS_dctkey = DS_numca   and   Wsegnca = *zeros
140500990114     c                   add       DCTncn        Wsegnca
140600990114     c                   ENDIF
140700990114      *
140800980518     c                   enddo
140900040720     c                   endif
141000981029      *
141100990114      * Ricavo colli selezionabili
141200990114     c     ARBncl        sub       Wtcolli       JJ
141300981029      *
141400981029     c                   clear                   FNDCT000
141500980708      *
141600980518     c                   endsr
141700980618      ****************************************************************
141800980618      * CONTROLLI PRELIMINARI PER APERTURA C.A.
141900980618      ****************************************************************
142000981217     C     CTR_A         BEGSR
142100980707
142200980625      * ------------------------------------------------------------------------------------------ *
142300980625      *     C O N D I Z I O N I   N E C E S S A R I E   P E R   L' A P E R T U R A                 *
142400980625      * ------------------------------------------------------------------------------------------ *
142500990304      *  altri controlli, relativi alla riserva, sono inseriti in CTRD01                           *
142600990304      * ------------------------------------------------------------------------------------------ *
142700990127      *   - Non è possibile aprire C.A. su bolle recupero. Controllato da FIDN00R                  *
142800990127      * 0 - Non è possibile aprire C.A. se non ho la spedizione                                    *
142900981224      * 1 - Non devono essere trascorsi (§5ABLP - 1) giorni dalla data consegna                    *
143000000420      *      ELIMINATO, reso obsoleto dalla rigenerazione bolle                                    *
143100990407      * 2 - Consegna anomala reso non apro se Mamma     ELIMINATO                                  *
143200990217      * 3 - Se figlia di reso non controllo. Consegna anomala dirottamento apro in partenza solo   *
143300990217      *      se Mamma Originale e non locale                                                       *
143400040720      *                                     e se risulta consegnata
143500990217      * 4 - Se figlia di reso non controllo. Consegna anomala dirottamento apro in arrivo solo se  *
143600981228      *      ultima Figlia. Consegna anomala cambio porto non apro se Figlia                       *
143700981224      * 5 - Almeno uno dei colli della spedizione non deve essere in un'altra C.A                  *
143800981224      * 6 - Se apro per segnacollo non deve essere in un'altra C.A                                 *
143900990114      * 7 - Se anomalia per colli non consegnati e la spedizione è consegnata non apro             *
144000990114      * 8 - Se anomalia per colli consegnati e la sped. non ha almeno consegna parziale non apro   *
144100990407      * 9 - La spedizione non deve essere in giacenza              * Anche ripristino CA annullata *
144200990407      * 10- La sped. non deve essere in distinta        ELIMINATO  * Anche ripristino CA annullata *
144300990407      * 11- Se la spedizione è in blocco non apro                  * Anche ripristino CA annullata *
144400981016      * ------------------------------------------------------------------------------------------ *
144500980707
144600110419     c                   clear                   wdirott
144700981020     C                   IF        *INKE = *OFF                                 Ripristino
144800981016
144900990127      *    Controllo 0
145000990127     c                   IF        I00nsp = *ZEROS
145100990127     c                   movel     msg(88)       o00msg
145200990127     c                   eval      o00err = 'E'
145300990127     c                   ENDIF
145400010904
145500010904      *    Controllo 1
145600010904
145700010904      *    Controllo 2
145800981217
145900981217      *    Controllo 3
146000990127     c                   IF         O00err <> 'E'  and  ARBcca <> *blanks
146100990217     c     arbcca        lookup    D7O                                    30
146200981009     c                   if        *IN30 = *ON  and  i00tpb = 'P'
146300990408     c     Kbol01        chain(N)  fnlbl002                           30
146400990407     c  N30              eval      Wlegami = 'S'
146500981009     c  N30              eval      *IN30 = DS_LBLORI <> DS_LBLPRE
146600040720      *   Se Mamma Originale di un dirottamento deve essere consegnata altrimenti aprire in arrivo
146700040720     c  N30              eval      *IN30 = arbdcm = 0
146800990107      *   Se Mamma Originale di un dirottamento memorizzo LNP
146900990107     c  N30              z-add     ARBlnp        WLNP
147000110419     c  N30              eval      Wdirott = 'S'
147100981009     c                   endif
147200990107      *
147300981009     c                   if        *IN30 = *ON
147400981009     c                   movel     msg(64)       o00msg
147500981009     c                   eval      o00err = 'E'
147600981009     c                   endif
147700981228      *
147800981009     c                   ENDIF
147900981224
148000981224      *    Controllo 4
148100981228      *  Se la sped. non è figlia di un Reso verifico eventuali legami bolla
148200990127     c                   IF         O00err <> 'E'  and  ARBfbr <> 'S'
148300981229      *   Verifico se la sped. è figlia.
148400981229      *         Se sped. locale non esiste dirottamento, se sped. non locale non esiste cambio porto
148500990408     c     kbol01        chain(N)  fnlbl000                           30
148600990407     c  N30              eval      Wlegami = 'S'
148700981224      *
148800981224     c                   SELECT
148900981224     c                   WHEN      *IN30 = *ON
149000981229      *   Se sped. locale può essere solo figlia di un cambio Porto e quindi non apro
149100981224     c                   WHEN      ARBlnp = ARBlna
149200981228     c                   movel     msg(82)       o00msg
149300981228     c                   eval      o00err = 'E'
149400981229      *   Se sped. non locale ho solamente dirottamento, se sono in partenza non è l'ultima figlia
149500981224     c                   WHEN      I00tpb = 'P'
149600981228     c                   movel     msg(64)       o00msg
149700981228     c                   eval      o00err = 'E'
149800981229      *   Se la sped. è anche Mamma di un dirottamento errore
149900981224     c                   OTHER
150000990413     c                   eval      *in30 = *off
150100981224     c                   if        ARBcca <> *blanks
150200990217     c     arbcca        lookup    D7O                                    30
150300990413     c                   endif
150400990413     c                   if        *in30 = *ON
150500990413     c                   movel     msg(64)       o00msg
150600990413     c                   eval      o00err = 'E'
150700990413     c                   else
150800990413      *   Se posso aprire memorizzo LNA perchè Ultima Figlia e LNP della Mamma originale
150900990413     c                   z-add     LBLlpo        WLNP
151000990413     c                   z-add     ARBlna        WLNA
151100110419     c                   eval      Wdirott = 'S'
151200981224     c                   endif
151300981224     c                   ENDSL
151400981228      *
151500981224     c                   ENDIF
151600981009
151700981224      *    Controllo 5
151800050113     c                   IF         O00err <> 'E'   AND  WTcolli = ARBncl
151900981009     c                   movel     msg(31)       o00msg
152000981009     c                   eval      o00err = 'E'
152100981009     c                   endif
152200981009
152300981016     c                   ENDIF
152400981016
152500981224      *    Controllo 6
152600981224
152700990114      *    Decodifico il tipo anomalia per controlli sulla consegna
152800990108     C                   MOVEL(P)  I00Tad        WKEY1
152900990108     C                   clear                   WTADtipo
153000990108     C                   EXSR      CHTAD
153100990114     C
153200990114      *    Controllo 7
153300990127     c                   IF         ARBdcm > *zeros  and  §TADtico = 'N'
153400990108     c                             and  O00ERR <> 'E'
153500990108     c                   movel     msg(83)       o00msg
153600990108     c                   eval      o00err = 'E'
153700990108     c                   ENDIF
153800990114
153900990114      *    Controllo 8
154000990127     c                   IF         ARBDCM = *zeros  and  ARBdcp = *zeros
154100990114     c                             and  §TADtico = 'C'  and  O00ERR <> 'E'
154200990114     c                   movel     msg(87)       o00msg
154300990114     c                   eval      o00err = 'E'
154400990114     c                   ENDIF
154500990108
154600981224      * Controlli da fare anche per ripristino
154700990127     c                   if         i00nsp > *zeros   AND   O00ERR <> 'E'
154800981224
154900990114      *    Controllo 9
155000990601     c                   clear                   Wgiace
155100980625      *
155200040720      *  verifico solo  le giacenze in arrivo
155300050523     c     kgiac         chain     Tigcp21l                           30
155400990601     c                   if        *in30 = *off
155500990601     c                   eval       Wgiace = 'S'
155600050331     c                   if          gcpfas < 40
155700980625     c                   movel     msg(16)       o00msg
155800980625     c                   eval      o00err = 'E'
155900980625     c                   endif
156000990601     c                   endif
156100981009
156200990114      *    Controllo 10
156300981009
156400990114      *    Controllo 11
156500990601     c                   SELECT
156600990601     c                   WHEN       O00err = 'E'
156700990601     c                   WHEN       ARBfbc =  *blanks
156800990601     c                   WHEN       ARBfbc =  '*'
156900990601     c                   WHEN       ARBfbc = 'G'  and  Wgiace = 'S'
157000990601     c                   OTHER
157100980703     c                   movel     msg(45)       o00msg
157200980703     c                   eval      o00err = 'E'
157300990601     c                   ENDSL
157400981224      *
157500981224     c                   ENDIF
157600981224
157700980625     c                   endsr
157800981228      ****************************************************************
157900981228      * IMPOSTO DATI SPEDIZIONE
158000981228      ****************************************************************
158100981228     C     IMP_A         BEGSR
158200000628     C                   CLEAR                   LNPPT
158300981228
158400981228      *  TIPO PORTO
158500981228     C                   movel     '3A'          kCOD
158600981228     C                   movel(P)  ARBcbo        kKEY
158700990408     C     ktab          chain(N)  TABEL                              31
158800981228     C                   movel     TBLUNI        DS3A
158900981228     C                   movel     §3ATB1        §DCTport
159000981230      *
159100981228      *    Se consegna anomala "cambio porto" inverto il porto
159200981228     c                   SELECT
159300981230     c                   WHEN      ARBcca <> '9'
159400981230     c                   WHEN      §DCTport = 'F'
159500981228     c                   movel     'A'           §DCTport
159600110419     c                   eval      §dctcca=arbcca
159700981228     c                   WHEN      §DCTport = 'A'
159800981228     c                   movel     'F'           §DCTport
159900110419     c                   eval      §dctcca=arbcca
160000981228     c                   ENDSL
160100981228
160200981228      *  DETERMINO P.O. DI PARTENZA E P.O. D'ARRIVO
160300981228      *     Se figlia di Reso o nessun dirottamento imposto LNP e LNA della spedizione
160400981228      *     Se apertura su mamma originale (non reso) di un dirottamento ricerco l'ultima figlia
160500981228      *     Se non impostati forzo LNP e LNA della spedizione
160600981228     c                   IF        ARBfbr <> 'S'  and
160700981228     c                             WLNP > *zeros  and  WLNA = *zeros
160800981228     c                   z-add     LBLLAN        WLNA                           Da Chain in CTR_A
160900981228     c                   DO        *hival
161000990408     c     Klbl_F        chain(N)  fnlbl002                           30
161100990407     c  N30              eval      Wlegami = 'S'
161200990407     c  N30              z-add     LBLLAN        WLNA
161300981228     c  N30              ENDDO
161400981228     c                   ELSE
161500981228      *
161600981228     c                   If        WLNP = *zeros
161700981228     c                   z-add     ARBlnp        WLNP
161800981228     c                   Endif
161900981228     c                   If        WLNA = *zeros
162000981228     c                   z-add     ARBlna        WLNA
162100981228     c                   Endif
162200981228     c                   ENDIF
162300981228
162400020419      *---------------- 15/04/2002
162500981228      *  TIPO SPEDIZIONE
162600020419      *       export - import - nazionale
162700981228      *
162800170509     c                   clear                   wntw
162900170509     c                   clear                   wlnpntw
163000020419     c     WLNA          chain     azorg
163100020419      *
163200170509     c                   IF        %found(AZORG01L)
163300020419     C                   movel     ORGDE3        OG143
163400020419     c                   IF        §OGntw = 'DPD'  or  §OGntw = 'EEX'  or
163500020419     c                             §OGntw = 'EUP'  or  §OGntw = 'FED'
163600020419     c                   movel     'E'           §DCTtisp
163700020419     c                   movel     §ogntw        Wntw
163800020419     c                   ENDIF
163900020419     c                   ENDIF
164000020419      *
164100020419     c     WLNP          chain     azorg
164200020419      *
164300170509     c                   IF        %found(AZORG01L)
164400020419     C                   movel     ORGDE3        OG143
164500020419     c                   IF        §OGntw = 'DPD'  or  §OGntw = 'EEX'  or
164600020419     c                             §OGntw = 'EUP'  or  §OGntw = 'FED'
164700170509     c
164800170509     c                   IF        §DCTtisp = *blanks
164900020419     c                   movel     'I'           §DCTtisp
165000020419     c                   movel     §ogntw        Wntw
165100170509     c                   endif
165200170509     c
165300170509     c                   movel     §ogntw        Wlnpntw
165400020419     c                   ENDIF
165500020419      *
165600020419     c                   ENDIF
165700020419
165800020419      *  NETWORK ESTERO
165900020419     C                   SELECT
166000020419     c                   WHEN      §DCTtisp = *blanks
166100020419     c                   WHEN      Wntw  = 'EEX'
166200020419     c                   movel     'X'           §DCTeuro
166300020419     c                   WHEN      Wntw  = 'EUP'
166400020419     c                   movel     'P'           §DCTeuro
166500020419     c                   WHEN      Wntw  = 'DPD'
166600020419     c                   movel     'D'           §DCTeuro
166700020419     c                   WHEN      Wntw  = 'FED'
166800020419     c                   movel     'F'           §DCTeuro
166900020419     c                   ENDSL
167000020419      *----------------
167100990408
167200990408      *  BOLLA CON LEGAMI
167300990408     c                   movel     Wlegami       §DCTlega
167400110419     c* CONSEGNA ANOMALA
167500110419     c                   select
167600110419     c                   when      §dctcca <>' '
167700110419     c                   when      wdirott='S'
167800110419     c                   eval      §dctcca='1'
167900110419     c                   other
168000110419     c                   movel     arbcca        §dctcca
168100110419     c                   endsl
168200000628     c*
168300981228
168400981228     c                   ENDSR
168500980615      ****************************************************************
168600980615      *  CARICAMENTO DATI DI TESTATA VIDEO 1
168700980615      ****************************************************************
168800980716     C     RIETES        BEGSR
168900980625
169000981229      * Se non sono in apertura aggancio CA
169100980611     C                   IF        I00MOD <> 'A'
169200980525     c                   z-add     i00fil        vtcfil
169300980525     c                   z-add     i00nca        vtcnca
169400990210     c     knumca        CHAIN     fndct000                           30
169500981228      *
169600981228     c                   movel     dctflo        DDCT01
169700990113     c                   z-add     DCTlna        Wlna
169800990113     c                   movel     §DCTlnpc      Wlnp
169900050805      * Memorizzo il "vecchio" numero evento
170000050805     c                   eval      SAVaae  = DCTaae
170100050805     c                   eval      SAVnev  = DCTnev
170200990113      *
170300981229     C                   clear                   WLBDAT
170400980611     c                   move      dctmgc        w0080
170500980526     c                   movel     dctaac        w0080
170600050217     c                   z-add     w0080         wdtacainv
170700981229     C                   z-add     w0080         G02inv
170800980525     C                   movel     '3'           G02err
170900980525     C                   CALL      'XSRDA8'
171000980525     C                   PARM                    WLBDAT
171100981229     c                   z-add     g02dat        VTCdc1
171200980720     c                   z-add     VTCdc1        Wdataca
171300980623     c                   ENDIF
171400981229
171500981229      * Imposto parametri se ho key spedizione
171600981229     c                   IF        I00nsp <> *zeros
171700981229     c                   move      datasped      VTCdsp
171800981229     C                   move      i00lnp        VTClnp
171900981229     C                   move      i00nrs        VTCnrs
172000981229     C                   move      i00nsp        VTCnsp
172100981229      *   Flag DDT
172200981229     c                   IF        WDDT = 'S'  or  WDDT = 'C'
172300031027     c                             or wddt = 'P' or wddt = 'Q'
172400981229     c                             or  (I00tpb = 'S'  and  WDDT = *blanks)
172500981229     C                   movel     'SI DDT'      VTCddt
172600981229     c                   ELSE
172700981229     C                   movel     'NO DDT'      VTCddt
172800981229     c                   ENDIF
172900020827      *   Linea d'arrivo
173000050523     c                   clear                   DS_PO
173100981229     C                   move      I00lna        VTClna
173200990408     C     I00lna        chain(N)  AZORG                              31
173300981229     C  N31              If        ORGDE5 <> *BLANKS
173400981229     C                   movel     ORGDE5        VTDlna
173500981229     C                   Else
173600981229     C                   movel     ORGDES        VTDlna
173700981229     C                   Endif
173800050523     c                   movel     DS_PO         Save_PO_A
173900981229      *   C/Assegno
174000981229     c                   SELECT
174100051114     c                   WHEN      I00tpb <>'S'
174200051114     c     kbol01        chain(N)  fiar9000                           30
174300981229     c                   WHEN      I00tpb = 'S'  and TASfca = 'S'
174400990408     c     kbol01        chain(N)  tncsb000                           30
174500981229     c                   ENDSL
174600981229      *
174700990806     c                   IF        *IN30 = *off and  CSBsta <> 9
174800981229     c                   z-add     AR9cas        VTCcas
174900981229     c                   move      AR9vca        VTCvca
175000990806     c                   ENDIF
175100990408      *   Fattura
175200981229     c                   SELECT
175300981229      *
175400981229     c                   WHEN      I00tpb = 'A'  and  §DCTport = 'A'
175500990408      *      se Mamma di cambio porto cerco la fattura della figlia
175600110421     c****               IF        ARBcca <> '9'
175700110421     c                   IF        §dctcca <> '9'
175800991116     C                   EVAL      TRCAR6 = '1'
175900991116     c     KAR6          chain(N)  fiar6000                           30
176000990408     c                   ELSE
176100990408     c     Kbol01        chain(N)  fnlbl002                           30
176200991116     C                   EVAL      TRCAR6 = '1'
176300991116     c  N30Kar6bl        chain(N)  fiar6000                           30
176400990408     c                   ENDIF
176500990408      *
176600981229     c                   If        *in30 = *off  and  AR6nft > *zeros
176700981229     c                                           and  AR6dft > *zeros
176800981229     c                   z-add     AR6ift        VTCift
176900010910     c                   movel     AR6div        VTCvif
177000981229     c                   Endif
177100981229      *
177200981229     c                   WHEN      I00tpb = 'S'  and  §DCTport = 'A'
177300991104     c                   move      arbksc        w0040
177400981229     c                   If        w0040 = 9999  and  TASnft > *zeros
177500981229     c                   exsr      CALC_iftSE
177600981229     c                   Endif
177700981229      *
177800981229     c                   ENDSL
177900981229      *
178000981229     c                   ENDIF
178100980831
178200990428      * Attivo F16 (Annulla) nel caso di Manutenzione di C.A., non annullata e mai trasmessa,
178300990428      *  in fase di Apertura
178400990428     c                   eval      *in19 =
178500990428     c                                   (I00MOD = 'M'  and  DCTfca = Wfape  and
178600050224     c                                   DCTatb <> 'A'  and  dateu = wdtacainv
178700050224     c                                   and i00fgs = dctfil)
178800981201
178900981230      *  Per recuperare Mitt/Dest utilizzo il porto sped. che coincide con quello C.A. tranne se la
179000990310      *  sped. è "Mamma di cambio porto", in questo caso inverto il porto C.A.
179100981230     c                   SELECT
179200110421     c                   WHEN      §dctcca <> '9'
179300981230     c                   movel     §DCTport      W§DCTport
179400981230     c                   WHEN      §DCTport = 'A'
179500981230     c                   movel     'F'           W§DCTport
179600981230     c                   WHEN      §DCTport = 'F'
179700981230     c                   movel     'A'           W§DCTport
179800981230     c                   ENDSL
179900990408      *
180000000420      * Imposto Codice Cliente Intestatario Spedizione
180100000420      *  Franco=Mittente - Assegnato=Destinatario
180200991115      * campi di ar6
180300991115     c                   clear                   AR6ksc
180400991115     c                   clear                   AR6ctr
180500991115      *
180600990408     c                   z-add     ARBksc        WISksc
180700990408     c                   z-add     ARBctr        WISctr
180800990408      *   Se sono in Apertura in Arrivo di una Mamma di cambio porto prendo il codice dalla figlia
180900990408     c                   IF        I00mod = 'A'  and  I00tpb = 'A'
181000990408     c                                           and  ARBcca = '9'
181100990408     c     Kbol01        chain(N)  fnlbl002                           30
181200990408     c                   If        *IN30 = *off
181300990408     c     Klbl_F        chain(N)  fnarb000                           30
181400990408     c                   If        *IN30 = *off
181500990408     c                   z-add     ARBksc        WISksc
181600990408     c                   z-add     ARBctr        WISctr
181700990408     c     kbol01        chain(N)  fnarb000                           30
181800990408     c                   Endif
181900990408     c                   Endif
182000990408     c                   ELSE
182100990408      *
182200981229      *   Se assegnato in partenza verifico il codice destinatario
182300981230     c                   IF        I00tpb = 'P'  and  w§DCTport = 'A'
182400991116     C                   EVAL      TRCAR6 = '1'
182500991116     c     Kar6          chain(N)  FIAR6000                           30
182600990408     c                   If        *in30 = *OFF
182700000406     c                   If        AR6ksc <> *zeros
182800991115     c                   movel     AR6ksc        WISksc
182900991115     c                   movel     AR6ctr        WISctr
183000991115     c                   Endif
183100990408     c                   Endif
183200990408     c                   ENDIF
183300990408      *
183400990408     c                   ENDIF
183500981201
183600080208      * recupero il limite liquidazione del cliente della spedizione
183700080208      * verifico se a blanks il campo a video
183800110923
183900110923     c                   clear                   data08            8 0
184000110923      * solo per un pò di tempo devo recuperare la data di conferma della CA se non sono
184100110923      * in apertura per impostare il limite lt = a 80 euro
184200110923     c                   if        i00mod <> 'A' and dctfpr <> ' '
184300110923     c     kca           setgt     fndcf01l
184400110923     c                   do        *hival
184500110923     c     kca           readpe(n) fndcf01l                               31
184600110923     c                   if        *in31
184700110923     c                   leave
184800110923     c                   endif
184900110923     c                   if        dcffca = 100
185000110923     c                   movel     dcfdfc        data08
185100110923     c                   leave
185200110923     c                   endif
185300110923     c                   enddo
185400110923
185500110923      * se la data è > 0 e minore di 20111003 imposto 80 euro
185600110923     c                   If        data08 > 0 and data08 < 20111003
185700110923     c                   eval      §STDLLT = 80
185800110923     c                   Endif
185900110923
186000110923     c                   Endif
186100080208
186200080208      * recupero limite liquidazione transattiva
186300080208
186400080208     c                   eval      w§stdllt = §stdllt
186500080208
186600080208     c                   clear                   dllt
186700080208      *
186800080208     c                   clear                   TIBS02DS
186900080208     c                   movel     'C'           t02mod
187000080208     c                   movel     knsif         t02sif
187100080208     c                   movel     'LLT'         t02cod
187200080208     c                   move (P)  Wisksc        t02ke1
187300080208      *
187400080208     c                   call      'TIBS02R'
187500080208     c                   parm                    KPJBA
187600080208     c                   parm                    TIBS02DS
187700080208      *
187800080208     c                   if        t02err = *BLANKS
187900080208     c                   movel     T02uni        Dllt
188000080208     c                   eval      w§stdllt = §lltimp
188100080208     c                   endif
188200080208     c
188300080208      *
188400981005      * Codici mittente/destinario
188500981230     c                   IF         w§DCTport = 'A'
188600980716     c                   move      arbccm        VTCksm
188700991115     c                   if        AR6ksc <> *zeros
188800991115     c                   movel     AR6ksc        VTCksd
188900981201     c                   else
189000981201     c                   movel     arbksc        VTCksd
189100981201     c                   endif
189200981005     c                   ELSE
189300981005     c                   move      arbksc        VTCksm
189400981201     c                   clear                   VTCksd
189500981005     c                   ENDIF
189600990304      *
189700990226      * Verifico se il mittente o il destinario sono clienti con particolarità
189800990226     c                   IF         VTCksm > *zeros
189900990226     c                   z-add     VTCksm        w0070
190000990226     c                   exsr      clipar
190100990226     c                   ENDIF
190200990226      *
190300990226     c                   IF         VTCksd > *zeros  and  Wclipar = *blanks
190400990226     c                   z-add     VTCksd        w0070
190500990226     c                   exsr      clipar
190600990226     c                   ENDIF
190700980716
190800980716      * dati bolla
190900981228     c                   IF        §DCTport = 'A'
191000981228     c                   movel(P)  'ASSEGNATO'   VTDcbo
191100981228     c                   ELSE
191200981228     c                   movel(P)  'FRANCO'      VTDcbo
191300981005     c                   ENDIF
191400980716     c                   movel     arbrsm        VTCrsm
191500980716     c                   move      arbcam        VTCcam
191600980716     c                   movel     arblom        VTClom
191700980716     c                   movel     arbprm        VTCprm
191800980716     c                   movel     arbnzm        VTCnam
191900980716     c                   movel     arbrsd        VTCrsd
192000980716     c                   move      arbcad        VTCcad
192100980716     c                   movel     arblod        VTClod
192200980716     c                   movel     arbprd        VTCprd
192300980716     c                   movel     arbnzd        VTCnad
192400980716     c                   z-add     arbncl        VTCncl
192500980716     c                   z-add     arbpkf        VTCpkf
192600980716     c                   movel     arbfvf        VTCfvf
192700980716     c                   z-add     arbvlf        VTCvlf
192800981113     c                   IF        DCTnas <> *blanks
192900981113     c                   movel     dctnas        VTCnas
193000981113     c                   ELSE
193100981113     c                   movel     arbnas        VTCnas
193200981113     c                   ENDIF
193300010918      *
193400010918     c                   If        ARBias <> 0  and  ARBvas = *blank
193500011114     c                   eval      ARBvas = 'ITL'
193600010918     c                   Endif
193700981221      *
193800990319      * Importo da assicurare solo se ho la spedizione e in sede se non impostato dalla fatturazione
193900990319     c                   SELECT
194000990319     c                   WHEN      SIMFEL <> *zeros
194100990319     C                   z-add     ARBIAS        VTCias                         Imp. da assicurare
194200990319     C                   movel     ARBVAS        VTCvas                           Valuta
194300990319     c                   WHEN      TASFCM <> 'F'
194400990319     C                   z-add     ARBIAS        VTCias                         Imp. da assicurare
194500990319     C                   movel     ARBVAS        VTCvas                           Valuta
194600990319     c                   ENDSL
194700990319      *
194800990319     c                   SELECT
194900990319     c                   WHEN        VTCias = *zeros
195000010907     C                   clear                   VTCvas
195100010910     c                   WHEN        VTCvas = §gedDBA
195200990319     C                   z-add     VTCias        £ARBias
195300990319     c                   OTHER
195400981221     C                   movel     VTCvas        WVLT
195500981221     C                   z-add     VTCias        W0133
195600010904     c                   exsr      srCMB
195700010912     C                   if        not *in28
195800010904     c                   z-add     w0133         £ARBias
195900981221     c                   Endif
196000990319     c                   ENDSL
196100980717
196200980717      * FASE C.A.
196300980717     C                   z-add     i00fca        VTCfca
196400980717     c                   CLEAR                   FIDN05DS
196500980717     c                   movel     'C'           I05MOD
196600980717     c                   z-add     VTCfca        I05fca
196700980717     C                   CALL      'FIDN05R'
196800980717     c                   PARM                    KPJBA
196900980717     c                   PARM                    FIDN05DS
197000980717     c                   IF        O05err <> 'E'
197100980717     c                   movel     O05REC        DS_FNDFA
197200980717     c                   if        i00mod = 'G' and DFAdgc <> *blanks
197300980717     c                   movel     DFAdgc        VTDfca
197400980717     c                   else
197500980717     c                   movel     DFAdes        VTDfca
197600980717     C                   endif
197700980717     c                   ENDIF
197800980717
197900980717      * P.O. gestione
198000070402     c                   clear                   og148
198100980717     C                   z-add     i00fgs        VTCgfc
198200990408     C     VTCgfc        chain(N)  AZORG                              31
198300980717     C  N31              if        ORGDE5 <> *BLANKS
198400980717     C                   movel     ORGDE5        VTDgfc
198500980717     C                   else
198600980717     C                   movel     ORGDES        VTDgfc
198700980717     C                   endif
198800070402     c  n31              eval      og148 = orgde8
198900070402      * controllo se Filiale gestione è abilitata a telefonate GEO
199000070402     c                   eval      wfilgeo = (§oggeot = 'S')
199100980716
199200990108      * TIPO ANOMALIA (per Apertura impostati nella SBR CTR_A)
199300990111     c                   if        I00mod <> 'A'
199400981229     C                   MOVEL(P)  I00Tad        WKEY1
199500981019     C                   clear                   WTADtipo
199600980716     C                   EXSR      CHTAD
199700990108     C                   endif
199800990408
199900990408      * MANDATO ASSICURATIVO
200000990408     c                   IF        I00mod <> 'A'
200100990428     c     kca           chain(N)  FNDPT000                           30
200200990408     c                   If        *IN30 = *OFF
200300990408     c                   eval      WFNDPT = 'S'
200400990408      *
200500990408     c                   If        DPTatb <> 'A'
200600990408     c                   eval      Wmand = 'S'
200700990408     c                   eval      Wfvm = DPTfvm
200800020211     c                   eval      Wftc = DPTftc
200900990408      * memorizzo se cliente assicurato a valore variabile
201000990408    2c                   IF        DPTftc = 'C'  and  DPTvlm = *zeros
201100990408     c                   move      'S'           Wvalvar
201200990408     c                   ENDIF
201300990408      * Controllo divisa
201400010911     c                   IF        DPTvlm > *zeros  and  DPTvvm <> §gedDBA
201500010906     c                   z-add     DPTvlm        W0133
201600010906     c                   movel     DPTvvm        Wvlt
201700010904     c                   exsr      srCMB
201800010912     c                   z-add     W0133         Wvlm
201900011002     c                   movel     §gedDBA       Wvlt
202000010911     c                   ELSE
202100010911     c                   z-add     DPTvlm        Wvlm
202200011003     c                   IF        DPTvlm > *zeros
202300011003     c                   movel     §gedDBA       Wvlt
202400011003     c                   ENDIF
202500990408     c                   ENDIF
202600990409      * imposto DS per richiamo pgm di visualizzazione mandato
202700990409     c                   clear                   fidn30ds
202800990409     c                   z-add     dptksc        I30ksc
202900990409     c                   z-add     dptctr        I30ctr
203000990409     c                   z-add     dptprg        I30prg
203100990409     c                   movel     dptftc        I30ftc
203200990409     c                   movel     dptfvm        I30fvm
203300990409     c                   z-add     dptvlm        I30vlm
203400990409     c                   movel     dptvvm        I30vvm
203500990408     C                   Endif
203600990408      *
203700990408     C                   ENDIF
203800990408     C                   ENDIF
203900980625
204000981229     c                   eval      *IN10 = (VTCias > *zeros)
204100981229     c                   eval      *IN11 = (VTCcas > *zeros)
204200981229     c                   eval      *IN12 = (VTCift > *zeros)
204300990217
204400990121      * Se C.A. non confermata *IN05 *ON - C.A. in conferma o confermata *IN05 *OFF
204500981102     c                   eval      *in05 = (DCTfca < WFcon  and  I00MOD <> 'G')
204600981126      * Se C.A. confermata *IN06 *ON
204700981127     c                   eval      *in06 = (DCTfca >= WFcon)
204800000628      *
204900020419      * VEDO SE LNP poste
205000050523     c                   clear                   DS_PO
205100020419     c     WLNP          chain     azorg
205200020419     c                   if        %FOUND(AZORG01L)
205300000628     C                   MOVEL     ORGDE3        OG143
205400020419     c                   if        §ogntw = 'PPT'
205500020419     C                   eval      lnppt = 'S'
205600020419     c                   endif
205700000628     C                   ENDIF
205800050523     c                   movel     DS_PO         Save_PO_P
205900981102
206000980611     C                   ENDSR
206100980615      ****************************************************************
206200980716      *  CARICAMENTO DATI DI DETTAGLIO
206300980615      ****************************************************************
206400980716     C     RIEDET        BEGSR
206500981201
206600020419      * Verifico se il cliente è assicurato:
206700020419      *    in apertura oppure se cambia il cliente e la bolla non è
206800020419      *    legata
206900990408     c                   IF        I00mod = 'A'  or  (§DCTlega <> 'S'  and
207000990408     c                               (WISksc <> DPTksc  or  WISctr <> DPTctr))
207100990408     c                   exsr      CTR_cliass
207200990408     c                   ENDIF
207300020419      * Aggancio anagrafico particolarità clienti,
207400020419      *   clienti 8888 e 9999 sono senza particolarità
207500990408     c                   move      WISksc        w0040
207600990311     c                   IF        W0040 <> 9999 and W0040 <> 8888 and W0040 > 0
207700990408     c                   z-add     WISksc        I69kcs
207800990311     c                   clear                   CLSflo
207900990311     c                   EXSR      CTR_ANAGRA
208000990311     c                   eval      WCLSflo5 = %SUBST(CLSflo:5:1)                Stampa Copia Denunci
208100120712     ***c                   eval      WCLSflo3 = %SUBST(CLSflo:3:1)                Pagamento danno
208200990311     c                   ENDIF
208300981201      * Determino se limite risarcibile da utilizzare è "CMR" o "Legge 450"
208400981201     c                   EXSR      IMPlimris
208500980625
208600020419      * Responsabilità Internazionale visualizzato se sped.
208700020419      *   EuroExpress o Europolitan
208800021108      *   oppure export DPD porto franco
208900021108      *   oppure export FedEx
209000020419     C                   IF            §DCTeuro = 'X' or  §DCTeuro = 'P'
209100020509     c                             or (§DCTeuro = 'D' and §DCTport = 'F'
209200020509     c                                                and §DCTtisp = 'E')
209300021108     c                             or (§DCTeuro = 'F' and §DCTtisp = 'E')
209400020419     c                   eval      *in08 = *on
209500020419     c                   ELSE
209600020419     c                   eval      *in08 = *off
209700020419     c                   ENDIF
209800981211
209900980716      * Inizializzo i campi delle videate in base al tipo di elaborazione
210000981021      *----------------------------
210100981020   WHC                   IF        I00mod = 'A'                                 ** Apertura **
210200981021      *----------------------------
210300980625
210400980625     C                   Z-ADD     wdtgio        V1CDCA                         Data gg = Data CA
210500980720     c                   z-add     V1Cdca        Wdataca
210600980625     C                   MOVE      WDTGIO        V1CAAE                         AA evento = anno sys
210700020125
210800020125      *   Imposto flag franchigia " "
210900020125     c                   eval      *in33 =  *on
211000020125     c                   clear                   V1Hfra
211100020125     c                   clear                   V1Cfra
211200980625
211300990304      *   Imposto riserva se esiste evento "R  "
211400990304     c                   exsr      EVEriserva
211500990304     c                   IF        Wriserva = *blanks
211600990209     C                   movel     'N'           V1Ccrc
211700990304     c                   ELSE
211800990304     C                   movel     'S'           V1Ccrc
211900990304     c                   ENDIF
212000990209     C                   movel     V1Ccrc        WV1Ccrc
212100990115
212200990115      *   Imposto Richiesta copia denuncia
212300990115     c                   movel     Wclsflo5      V1Crcd
212400981022
212500981020      *----------------------------
212600981020   WHC                   ELSE
212700981020      *----------------------------
212800981127
212900981022      * Imposto valori
213000990510     c   08              move      DCTres        V2Cres                         Respons. INTERNAZION
213100981020     c                   z-add     DCTncn        V2Cncn                         Colli danneggiati
213200981020     c                   z-add     DCTncn        WV2Cncn                        Memo Colli dannegg.
213300981020     c                   z-add     DCTpkd        V2Cpkd                         Kg danneggiati
213400981020     c                   z-add     DCTnpz        V2Cnpz                         Pezzi danneggiati
213500981020     c                   movel     DCTdsd        V2Cdsd                         Test merce dannegg.
213600981020     c                   movel     DCTtdt        V1Ctdt                         Test DDT
213700981020     c                   movel     DCTtdt        WV1Ctdt                        Memo Test DDT
213800990209     c                   movel     dctcar        Wdctcar                        Tipo calcolo resp.
213900990209     c                   movel     dctrcd        V1Crcd                         Rich. copia denuncia
214000981005     c                   IF        DCTdic > *zeros
214100981005     c     *iso          move      DCTdic        Wdataeur
214200981022     c     *eur          movel     Wdataeur      V1Cdic                         Data stampa copia
214300990210     c                   eval      *IN13 = *ON
214400981005     c                   ENDIF
214500990304     c                   movel     DCTcrc        V1Ccrc                         Riserva
214600990304     c                   movel     DCTcrc        WV1Ccrc                        Riserva
214700990304
214800990304      * In filiale verifico se esiste evento "R  " = Riserva
214900990304     c                   IF        SIMFEL <> *zeros
215000990304     c                   exsr      EVEriserva
215100990304     c                   ENDIF
215200981020
215300981113      * Importo massimo risarcibile
215400981116      *   Calcolo ed imposto i valori a video se: sono in conferma, non è obbligatorio l'inserimento
215500981116      *   manuale e non è gia stato inserito l'importo
215600981113     c                   IF        DCTipv = *zeros  and  I00mod = 'G'
215700981116     c                             and  §TADimpr <> 'M'
215800981113     c                   exsr      CALCVAS                                      Calcolo
215900981113     C                   z-add     Wvalass       V2Cipv
216000981113     C                   z-add     Wvalass       WV2Cipv
216100010911     c                   movel     Wvlt          V2Cvpv
216200010911     c                   movel     Wvlt          WV2Cvpv
216300981113     c                   movel     'C'           WDCTfpv
216400981113     c                   ELSE
216500981113     c                   z-add     DCTipv        V2Cipv
216600981113     c                   z-add     DCTipv        WV2Cipv
216700981113     c                   movel     DCTvpv        V2Cvpv                           Valuta
216800981130     c                   movel     DCTvpv        WV2Cvpv
216900981113     c                   movel     DCTfpv        WDCTfpv                          Tipo inserimento
217000981113     c                   ENDIF
217100981130      *     Tipo Calcolo
217200981113     c                   IF        WDCTfpv = 'M'
217300981020     c                   eval      V2Dfpv = 'Inserito manualmente'
217400981113     c                   ENDIF
217500981113     c                   IF        WDCTfpv = 'C'
217600981020     c                   eval      V2Dfpv = 'Calcolato'
217700981020     c                   ENDIF
217800981130      *     Calcolo controvalore
217900010918     c                   IF        V2Cvpv = §gedDBA  or  V2Cipv = 0
218000981130     c                   z-add     V2Cipv        £DCTipv
218100981130     c                   ELSE
218200010912     c                   movel     V2Cvpv        WVLT
218300010912     c                   z-add     V2Cipv        W0133
218400010912     c                   exsr      srCMB
218500010912     c                   z-add     w0133         £DCTipv
218600981130     c                   ENDIF
218700020125
218800020125      * Flag Franchigia
218900020125     c                   movel     §DCTpfra      V1Hfra
219000020125     c                   movel     §DCTpfra      V1Cfra
219100980625
219200980625      * Da file descrizioni: descrizioni aggiuntive
219300980520     c                   movel     'C'           ktpd
219400981029     c                   movel(p)  DS_numca      knkt
219500980520     c                   movel     'A'           kstd
219600980730     c                   clear                   kdim
219700980730     c                   clear                   khim
219800980730     c                   clear                   knks
219900980520     c                   movel     'D'           ktrc
220000980520     c                   exsr      chadcs
220100980520     c  n30              movel     dcsnot        v1dagg
220200980625
220300980625      * Da file descrizioni: motivo del danno
220400980520     c                   movel     'C'           ktpd
220500981029     c                   movel(P)  DS_numca      knkt
220600980609     c                   movel     'M'           kstd
220700980520     c                   clear                   knks
220800980609     c                   movel     'D'           ktrc
220900980609     c                   clear                   kdim
221000980609     c                   clear                   khim
221100980520     c                   exsr      chadcs
221200980520     c  n30              movel     dcsnot        v1dmtv
221300980625
221400981020      * Numero Evento
221500981020     c                   IF        DCTnev > *zeros
221600981020      *
221700980521     c                   move      dctnev        v1cnev
221800980520     c                   z-add     dctaae        v1caae
221900990115     C                   move      CODEV         CODEVS
222000981020      *   reperisco decodifica da file descrizioni
222100980520     c                   movel     'E'           ktpd
222200980617     c                   movel(P)  codev         knkt
222300980520     c                   clear                   kstd
222400980520     c                   clear                   knks
222500980609     c                   clear                   kdim
222600980609     c                   clear                   khim
222700980520     c                   movel     'D'           ktrc
222800980520     c                   exsr      chadcs
222900980520     c  n30              movel     dcsnot        v1dnev
223000981020      *   reperisco decodifica tipo anomalia evento
223100981104     C                   movel     V1Cnev        Wv1cnev
223200990408     C     KDET          chain(N)  FNDET000                           31
223300981019     c                   if        *IN31 = *OFF
223400981104     C                   movel(P)  DETtad        WKEY1
223500981019     C                   movel     'E'           WTADtipo
223600981019     C                   exsr      CHTAD
223700981019     c                   endif
223800981104      *
223900981104      *    Sostituisco zero non significativi con blanks
224000981104     c                   movel     V1Cnev        W007A
224100981104     c                   clear                   V1Cnev
224200981104     c     '0'           check     W007A         XX
224300981104     c                   eval      %subst(V1Cnev:xx) = %subst(W007A:xx)
224400981019      *
224500980615     c                   ENDIF
224600980625
224700980716      * Da file descrizioni: descrizione pezzi danneggiati
224800980520     c                   movel     'C'           ktpd
224900981029     c                   movel(P)  DS_numca      knkt
225000980520     c                   movel     'P'           kstd
225100980520     c                   clear                   knks
225200980520     c                   movel     'D'           ktrc
225300980520     c                   exsr      chadcs
225400980716     c  n30              movel     dcsnot        V2Dnpz
225500980625
225600980625      * Da file descrizioni: descrizione pezzi presenti
225700980609     c                   movel     'K'           kstd
225800980520     c                   clear                   knks
225900980609     c                   movel     'D'           ktrc
226000980609     c                   clear                   kdim
226100980609     c                   clear                   khim
226200980520     c                   exsr      chadcs
226300980716     c  n30              movel     dcsnot        V2Cprs
226400980625
226500981126      * BENEFICIARIO ALTERNATIVO
226600980728     c                   IF        DCTKSC > *ZEROS
226700980520     c                   z-add     dctksc        v1cksc
226800990303     c                   z-add     dctksc        Wv1cksc
226900980520     c                   move      v1cksc        w0040
227000981022      *   se codificato decodifico da p.d.c.
227100980615     C                   IF        w0040 <> 8888
227200981005     c                   clear                   ACOrag
227300990408     c                   z-add     DCTksc        I69kac
227400990311     c                   exsr      CTR_anagra
227500981022     c  N30              movel     ACOrag        V1Dksc
227600980615     c                   ELSE
227700990408      *
227800981022      *   se non codificato decodifico da anagrafica clienti c.a.
227900981022     c                   movel     'S'           wclicon
228000981022     c                   clear                   FNDKA000
228100981020     c                   movel     'C'           Wtrc
228200981022     c     KDKA          CHAIN(N)  FNDKA000                           30
228300981022     c                   movel     dkarag        v1dksc
228400981022     c                   movel     DS_FNDKA      DS_CLICON
228500981022      *          imposto campi per video
228600981022     c                   movel     DKArag        CW2Crag
228700981022     c                   movel     DKAvia        CW2Cvia
228800981022     c                   movel     DKAcap        CW2Ccap
228900981022     c                   movel     DKAloc        CW2Cloc
229000981022     c                   movel     DKAprv        CW2Cprv
229100981022     c                   movel     DKAnaz        CW2Cnaz
229200981022     c                   eval      CW2Civa = %subst(DKAiva:1:14)
229300981022     c                   eval      CW2Cive = %subst(DKAiva:15:2)
229400981022     c                   movel     DKAtel        CW2Ctel
229500981022     c                   movel     DKAfax        CW2Cfax
229600980615     c                   ENDIF
229700980728     c                   ENDIF
229800980716
229900981126      * Da file descrizioni: interlocutore
230000980716     c                   movel     'C'           ktpd
230100981029     c                   movel(P)  DS_numca      knkt
230200980716     c                   movel     'N'           kstd
230300980716     c                   clear                   knks
230400980716     c                   movel     'D'           ktrc
230500980716     c                   exsr      chadcs
230600980716     c  n30              movel     dcsnot        V1Dnoc
230700981022
230800981022      * CLIENTE RIVALSA
230900981022     c                   clear                   FNDKA000
231000981022     c                   movel     'R'           Wtrc
231100981022     c     KDKA          CHAIN(N)  FNDKA000                           30
231200981022      *
231300990909     c                   IF        *IN30 = *OFF  and  DKAatb = *blanks
231400981105     c                   movel     DKArag        V2Crag
231500981105     c                   movel     DKArag        WV2Crag
231600981105     c                   movel     DKArag        WDKArag
231700121011     c*// ???            movel     DS_FNDKA      DS_CLIRIV   - non è mai usata!
231800981022      *          imposto campi per video
231900981022     c                   movel     DKArag        RW2Crag
232000981022     c                   movel     DKAvia        RW2Cvia
232100981022     c                   movel     DKAcap        RW2Ccap
232200981022     c                   movel     DKAloc        RW2Cloc
232300981022     c                   movel     DKAprv        RW2Cprv
232400981022     c                   movel     DKAnaz        RW2Cnaz
232500981022     c                   eval      RW2Civa = %subst(DKAiva:1:14)
232600981022     c                   eval      RW2Cive = %subst(DKAiva:15:2)
232700981022     c                   movel     DKAtel        RW2Ctel
232800981022     c                   movel     DKAfax        RW2Cfax
232900981029     c                   ENDIF
233000980724
233100990308      * Flag tipo gestione
233200980724     c                   movel     dctfpr        v1cfpr
233300980724     c                   movel     dctptr        v1cptr
233400990308      * Fino alla conferma segnalo l'inserimento manuale del beneficiario
233500990308     c                   If        V1Cptr <> *blanks  and
233600990308     c                                  (*in05 = *on  or  I00MOD = 'G')
233700990308     c                   movel     'Manuale'     V1Dptr
233800990308     c                   Endif
233900990308      *
234000990308     c                   IF         I00MOD = 'G'
234100980724     c                   exsr      impPROPR
234200990308     c                   movel     Wfpr          V1Cfpr
234300990308      *   Se beneficiario non forzato lo imposto
234400990308     c                   If        V1Cptr = *blanks
234500990308     c                   movel     Wptr          V1Cptr
234600990308     c                   Endif
234700981022     c                   ENDIF
234800990308      *
234900990205     c                   movel     v1cfpr        Wv1cfpr
235000981113
235100981126      * Interlocutore
235200040520      *   In conferma se non impostato e se ho beneficiario alternativo cerco nel luogo 006
235300040520     c                   IF        I00MOD = 'G'  and  V1Dnoc = *blanks
235400040520     c                                           and  V1Cksc <> *zeros
235500040520     c                   move      v1cksc        w0040
235600040520     c                   If        w0040 <> 8888
235700040520     c                   z-add     V1cKsc        KCLI
235800040525     c     kspe          chain     fnspe000                           30
235900040520     C  N30              movel     SPEper        V1Dnoc
236000040520     c                   EndIf
236100040520     c                   ENDIF
236200981126      *   In conferma se non impostato e se non ho beneficiario alternativo cerco nel luogo 006
236300981113     c                   IF        I00MOD = 'G'  and  V1Dnoc = *blanks
236400981113     c                                           and  V1Cksc = *zeros
236500981113     c                   z-add     WISksc        KCLI
236600040525     c     kspe          chain     fnspe000                           30
236700981113     C  N30              movel     SPEper        V1Dnoc
236800981113     c                   ENDIF
236900981020
237000120712     *** * Pagamento Danno
237100120712     *** *    In gestione se ho Liq. Transattiva forzo Vettore, se ho pratica lo forzo da anagrafico
237200120712     *** *    se impostato altrimenti forzo Consuldanni
237300120712     ***c                   SELECT
237400120712     ***c                   WHEN        I00mod <> 'G'
237500120712     ***c                   movel     DCTpgd        V2Cpgd
237600120712     ***c                   WHEN        DCTfpr = 'T'
237700120712     ***c                   eval      V2cpgd = 'V'
237800120712     ***c                   WHEN        WCLSflo3 <> *blanks
237900120712     ***c                   movel     WCLSflo3      V2Cpgd
238000120712     ***c                   WHEN        WCLSflo3 = *blanks
238100120712     ***c                   eval      V2cpgd = 'C'
238200120712     ***c                   ENDSL
238300120712     ***c                   movel     V2Cpgd        WV2Cpgd
238400981020
238500981029      * Aggancio file Liquidazione C.A.
238600990209     c     kca           CHAIN     FNDCL000                           30
238700990205     c                   eval      WDCLfrp = DCLfrp
238800990127      *
238900981021     c                   IF        *IN30 = *OFF
239000981029     c                   eval      Wfndcl = 'S'
239100981029      *   Importo trattenuto
239200981021     c                   z-add     DCLipt        V2Cipt
239300981030     c                   z-add     DCLipt        WV2Cipt
239400981021     c                   movel     DCLvpt        V2Cvpt
239500990127     c                   z-add     DCLcpt        WDCLcpt
239600981021     c                   if        DCLdpt > *zeros
239700981021     c     *iso          move      DCLdpt        Wdataeur
239800981021     c     *eur          movel     Wdataeur      V2Cdpt
239900990127     c                   movel     DCLdpt        WV2Cdpt
240000981021     c                   endif
240100010906      *      Converto importo in moneta corrente
240200010918     c                   IF        V2Cvpt = §gedDBA  or  V2Cipt = 0
240300981130     c                   z-add     V2Cipt        £DCLipt
240400981130     c                   ELSE
240500010912     c                   movel     V2Cvpt        WVLT
240600010912     c                   z-add     V2Cipt        W0133
240700010912     c                   exsr      srCMB
240800010912     c                   z-add     w0133         £DCLipt
240900981130     c                   ENDIF
241000990128     c                   z-add     £DCLipt       W£DCLipt
241100990127      *   Importo concordato
241200990127     c                   z-add     DCLipr        V2Cipr
241300990127     c                   z-add     DCLipr        WV2Cipr
241400990127     c                   movel     DCLvpr        V2Cvpr
241500990127     c                   z-add     DCLcpr        WDCLcpr
241600010912      *      Converto importo in moneta corrente
241700010918     c                   IF        V2Cvpr = §gedDBA  or  V2Cipr = 0
241800010912     c                   z-add     V2Cipr        £DCLipr
241900981130     c                   ELSE
242000010912     C                   movel     V2Cvpr        WVLT
242100010912     C                   z-add     V2Cipr        W0133
242200010912     c                   exsr      srCMB
242300010912     c                   z-add     w0133         £DCLipr
242400981130     c                   ENDIF
242500981029      *
242600981022     c                   ENDIF
242700981021
242800981020      *----------------------------
242900981020    1C                   ENDIF
243000981020      *----------------------------
243100010911      *
243200010911      * Visualizzazione del codice divisa/valuta per gli importi in I/O
243300010911      *  (anche se 0)
243400010911     c                   if        V2Cvpv = *blanks
243500010911     c                   movel     §gedDBA       V2Cvpv
243600010911     c                   endif
243700010911     c                   if        V2Cvpt = *blanks
243800010911     c                   movel     §gedDBA       V2Cvpt
243900010911     c                   endif
244000010911     c                   if        V2Cvpr = *blanks
244100010911     c                   movel     §gedDBA       V2Cvpr
244200010911     c                   endif
244300990309
244400990309      *                 E M I S S I O N E   C A M P I   C O N T R O L L A T A
244500990309
244600990309      * EMETTO IMPORTO CONCORDATO se: Manutenzione, fase eseguita Esito contatto con il benficiario,
244700990309      *   sono in sede
244800080303
244900080314     c                   eval      *IN07 =  (I00MOD = 'M'  and dctfpr = 'T'
245000080303     c                                                     and  SIMFEL = *zeros)
245100990309
245200990309      * EMETTO BENEFICIARIO se si verifica una delle seguenti condizioni:
245300990309      *    - sono in conferma od oltre
245400990309      *    - prima della conferma porto assegnato segue fattura "nazionale"
245500080208     c                   move      Wisksc        W0040
245600020419     c                   eval      *IN34 = (*in05 = *off     or
245700020419     c                                      (§DCTport = 'A'  and  W0040 <> 9999
245800020419     c                                                and  §DCTtisp = *blanks))
245900020124
246000031211     c                   eval      w0040 = §STDdlpf1 / 10000
246100051121      * EMETTO il FLAG FRANCHIGIA se:
246200051121      *  1) la C.A. è in gestione alla SEDE;
246300051121      *  2) ha come tipo gestione "Pratica Assicurativa";
246400051121      *  3) l'anno della spedizione relativa è oltre il 2001 (w0040=2002)
246500051121      *  4) la C.A. è in una fase minore di 700 (progetto di liquidazione)
246600051121      * OPPURE se:
246700051121      *  1) era già stato valorizzato
246800051121     c                   eval      *in33 =     (DCTgfc =  046
246900051121     c                                     and  DCTfpr =  'P'
247000051121     c                                     and  DCTaas >= w0040
247100051121     c                                     and  DCTfca <  WFspl )
247200051121     c                                     or   V1Hfra <> *blanks
247300020129      *
247400020129      * se emetto il Flag Franchigia NON emetto la Descrizione Evento
247500020129     c                   if        *in33
247600020129     c                   clear                   V1Dnev
247700020129     c                   endif
247800990128
247900990128      *                 P R O T E Z I O N E   C A M P I
248000990128
248100990122      * Proteggo il RIMBORSO A CARICO se é impostata la data valuta pagamento o se dopo la conferma
248200990122      *   il tipo gestione è liquidazione transattiva
248300990122     c                   eval      *in35  =  (DCLvlp > *zeros  or
248400990122     c                                        (*IN06 = *ON  and  DCTfpr = 'T'))
248500981209
248600981209      * Proteggo RIVALSA se si verifica una delle seguenti condizioni:
248700990201      *  il tipo gestione è Liq. Transattiva, è impostata la data valuta pagamento, sono in filiale
248800990201      *  e il profilo non è autorizzato alla variazione.
248900990201     c                   eval      *in36 =     V1Cfpr = 'T'
249000990201     c                                     or  DCLvlp > *zeros
249100990208     c                                     or (SIMFEL <> *zeros              and
249200130806     c**************                           %SUBST(knmus:1:3) <> §STDpv1  and
249300130806     c**************                           %SUBST(knmus:1:3) <> §STDpv2)
249400130806     c                                         §UTEmresca = *blanks)
249500981209
249600990208      * Dopo la conferma proteggo il BENEFICIARIO e l'INTERLOCUTORE: sempre se il tipo gestione è
249700990128      *  tipo gestione Liquidazione Transattiva; se ho il numero Pratica o l'importo Trattenuto se
249800990128      *  tipo gestione Pratica Assicurativa.
249900121011      **** non faccio più la protezione del campo se sono in LT in quanto non avrei altro modo per
250000121011      **** modificare il beneficiario togliendo la fase contatto e esito contatto LT (200e 215)
250100080229     c                   IF        *IN06 = *ON
250200080229     c                             and (DCTprn > *zeros or  DCLipt > *zeros)
250300990128     c                   eval      *in37 = *ON
250400990128     c                   eval      *in38 = *ON
250500990128     c                   ENDIF
250600981014
250700981014     C                   ENDSR
250800020124
250900981229      ****************************************************************
251000981229      * CALCOLO TOTALE FATTURA IMMEDIATA IN SEDE
251100981229      ****************************************************************
251200981229     C     CALC_iftse    BEGSR
251300981229      *
251400991104      * calcolo il totale fattura immediata richiamando FNLV16
251500991104     C                   clear                   fnlv16ds
251600991104     C                   clear                   dtasv
251700991104     C                   eval      d17tla = 'L'
251800991104     C                   eval      d17tbo = '0'
251900991104     C                   eval      d17crb = 'V'
252000991104     C                   eval      d17aas = i00aas
252100991104     C                   eval      d17lns = i00lnp
252200991104     C                   eval      d17nrs = i00nrs
252300991104     C                   eval      d17nsp = i00nsp
252400991104     C                   eval      d17por = taspor
252500991104     C                   eval      d17tbs = tastbl
252600991104     C                   eval      d17fim = 'F'
252700991104     C                   call      'FNLV16R'
252800991104     C                   parm                    fnlv16ds
252900991104     C                   parm                    dtasv
253000991104      * se non ci sono errori carico il totale fattura
253100991104     C                   if        o17err = ' '
253200991104     c                   z-add     o17ift        vtcift
253300010905     c                   if        o17ift <> 0
253400010910     c                   movel     TASdiv        vtcvif
253500010906     c                   else
253600010910     c                   clear                   vtcvif
253700010906     c                   endif
253800010904     c                   endif
253900991104
254000981229      *
254100981229     c                   endsr
254200981014      **************************************************************************
254300981014      * VERIFICA ESISTENZA MANDATO ASSICURATIVO
254400981014      **************************************************************************
254500981020     C     CTR_CLIASS    BEGSR
254600981014
254700990407     c                   clear                   FIDN48DS
254800990407     c                   clear                   Wvlm
254900990407     c                   clear                   Wfvm
255000020211     c                   clear                   Wftc
255100990408     c                   clear                   Wmand
255200990407     c                   clear                   Wvalvar
255300990407
255400990407      *  Clienti 8888 e 9999 sono senza mandato ed elevazione
255500990408     c                   move      WISksc        w0040
255600000202     c                   moveL     WISksc        w0030
255700990407     c                   IF        i00nsp > *zeros  AND
255800990407     c                               W0040 <> 9999  AND  W0040 <> 8888
255900990408     c                   movel     'L'           I48tla
256000990408     c                   z-add     WISksc        I48ksc
256100990407     c                   z-add     WISctr        I48ctr
256200990407     c                   movel     ARBtsp        I48tsp
256300990407     c                   movel     DS_arbdat     I48dsp
256400000202     C* IMPOSTO FIE
256500020419     C                   IF        §DCTtisp = *blank
256600020419     C                   movel     'J'           I48fie
256700020419     c                   ELSE
256800020419     C                   movel     'S'           I48fie
256900020419     C                   ENDIF
257000020419      *
257100990407     c                   movel     §DCTport      I48por
257200990407     c                   movel     'S'           I48mns
257300990408     c                   movel     'S'           Wfidn48r
257400990407
257500990407     c                   CALL      'FIDN48R'
257600990407     C                   PARM                    KPJBA
257700990407     C                   PARM                    FIDN48DS
257800990407
257900990409      * imposto DS per richiamo pgm di visualizzazione mandato
258000990409     c                   clear                   fidn30ds
258100990409     c                   z-add     I48ksc        I30ksc
258200990409     c                   z-add     I48ctr        I30ctr
258300990409     c                   z-add     O48prg        I30prg
258400990409     c                   movel     O48ftc        I30ftc
258500990409     c                   movel     O48fvm        I30fvm
258600990409     c                   z-add     O48vlm        I30vlm
258700990409     c                   movel     O48vvm        I30vvm
258800990409
258900990407     c                   IF        O48mand = 'S'
259000990408     c                   eval      Wmand = O48mand
259100990407     c                   eval      Wfvm = O48fvm
259200020211     c                   eval      Wftc = O48ftc
259300990407
259400990408      * memorizzo se cliente assicurato a valore variabile
259500990407    2c                   IF        O48FTC = 'C'  and  O48vlm = *zeros
259600990407     c                   move      'S'           Wvalvar
259700990407     c                   ENDIF
259800990407
259900990407      * Controllo divisa
260000990407    2c                   IF        O48vlm > *zeros
260100010911     c                   If        O48vvm <> §gedDBA
260200990407     c                   z-add     O48vlm        W0133
260300990407     c                   movel     O48vvm        Wvlt
260400010904     c                   exsr      srCMB
260500010912     c                   z-add     W0133         Wvlm
260600011002     c                   movel     §gedDBA       Wvlt
260700010911     c                   Else
260800010911     c                   z-add     O48vlm        Wvlm
260900011003     c                   movel     §gedDBA       Wvlt
261000010906     c                   Endif
261100010907     c                   else
261200010907     c                   clear                   wVLM
261300010911     c                   clear                   wVLT
261400010906     c                   ENDIF
261500990407
261600990407     c                   ENDIF
261700990407
261800990407     c                   ENDIF
261900990407
262000990407     c                   ENDSR
262100981012      **********************************************************************
262200981012      * LETTURA RECORD FILE DESCRIZIONI
262300981012      **********************************************************************
262400981012     C     Chadcs        BEGSR
262500981012      *
262600980520     c     kdcs          setll     fndcs01l
262700980522     c     kdcs          reade(N)  fndcs01l                               30
262800980615      *
262900990105     c     *in30         doweq     *off
263000990105     c     dcsatb        andne     *blanks
263100990105     c     kdcs          reade(N)  fndcs01l                               30
263200990105     c                   enddo
263300980615      *
263400980521     c                   endsr
263500990113      **********************************************************************
263600990113      * F4 = NOTE PER STAMPA
263700990113      **********************************************************************
263800990113     C     SUB_F4        BEGSR
263900990113      *
264000990113     C                   clear                   FIDN10DS
264100990113     c                   movel     'D'           i10trc
264200990113     c                   movel     'T'           i10std
264300990113     c                   movel     'C'           i10tpd
264400990113     c                   z-add     Wdataca       i10dta
264500990119     c                   movel     'V'           i10flm
264600990113     c                   movel(P)  DS_numca      I10nkt
264700990113      *
264800990113     c                   movel(P)  fidn10ds      kpjbu
264900990113     C                   CALL      'FIDN10R'
265000990113     C                   PARM                    kpjba
265100990113      *
265200990113     C                   ENDSR
265300980716      ****************************************************************
265400981012      * F18 = GESTIONE NOTE
265500981012      ****************************************************************
265600981012     C     SUB_F18       BEGSR
265700981012      *
265800981020     C                   clear                   FIDN10DS
265900981012     c                   z-add     Wdataca       i10dta
266000981012     c                   movel     'N'           i10trc
266100981012     c                   movel     'C'           i10tpd
266200981012      *
266300990107     c                   SELECT
266400990107     c                   WHEN      I00MOD = 'A'
266500981012     c                   movel     'M'           i10flm
266600981012     c                   movel     i00fca        i10nks
266700990107     c                   eval      i10dim = dateu
266800990107      *
266900990107     c                   WHEN      I00mod = 'G'
267000990107     c                   movel     'M'           i10flm
267100990107     c                   movel     i00fca        i10nks
267200990107     c                   movel     DS_numca      i10nkt
267300990107      *
267400990107     c                   WHEN      I00mod = 'M'
267500990107     c                   movel     'V'           i10flm
267600981012     c                   movel     dctfca        i10nks
267700990107     c                   movel     DS_numca      i10nkt
267800990107     c                   ENDSL
267900981012      *
268000981020     c                   movel(P)  FIDN10DS      kpjbu
268100981012     C                   CALL      'FIDN10R'
268200981012     C                   PARM                    kpjba
268300981012      *
268400981012     c                   ENDSR
268500980615      **********************************************************************
268600990226      *  MESSAGGI FORZABILI IN VIDEO 1
268700980615      **********************************************************************
268800990226     C     MSGD01        BEGSR
268900990226
269000990226     c                   SELECT
269100000420
269200990226      * Apertura: se la spedizione è in distinta
269300990226     c                   WHEN      I00mod = 'A'  and  I00tpb = 'A'
269400990226     c                             and  ARBdcm = *zeros  and  ARBndc > *zeros
269500990226     c                             and   Wmsg35frz = *blanks
269600990226     c                   eval      *in28 = *ON
269700990303     c                   movel     msg(35)       V1Cmsg
269800990226     c                   eval      Wmsg35frz = 'S'
269900990226
270000990226      * Apertura: se per la spedizione esiste un'altra CA
270100990226     c                   WHEN      I00mod = 'A'  and  Wtestca > *zeros
270200990226     c                                           and   Wmsg46frz = *blanks
270300990226     c                   eval      Wmsg46frz = 'S'
270400990226     c                   eval      *in28 = *ON
270500990226     c                   movel     msg(46)       V1Cmsg
270600990226
270700990226      * Conferma: se Avaria con Reso confermata senza attendere giorni stabiliti dalla lettera reso
270800990226     c                   WHEN      I00mod = 'G'   and   Wmsg63frz = *blanks
270900990226     c                             and  §TADragr = 'V'  and  DCTdsd = 'R'
271000990226      *   Aggancio anagrafico fasi
271100990226     c     I00fca        chain     fndfa000                           30
271200990226     c                   If        *in30 = *off  and  DFAfra <> *zeros
271300990226     c                   movel     DFAgga        WDFAgga
271400990226     c     kca           setgt     fndcf01l
271500990226      *
271600990226     c                   DO        *HIVAL
271700990226     c     kca           readpe    fndcf01l                               30
271800990226      *
271900990226     c                   if        *in30 = *off  and  DCFfca = DFAfra
272000990226     c     *iso          movel     DCFdfc        Wdataeur
272100990226     c                   adddur    DFAgga:*d     Wdataeur
272200990226     c                   if        Wdataeur > Wdataoggi
272300990226     c                   eval      *in30 = *ON
272400990226     c                   eval      *in28 = *ON
272500990226     c                   movel     msg(63)       V1Cmsg
272600990226     c                   eval      %SUBST(V1Cmsg:47:3) = WDFAgga
272700990226     c                   eval      Wmsg63frz = 'S'
272800990226     c                   endif
272900990226     c                   endif
273000990226      *
273100990226     c  N30              eval      *in30 = (DFArud = *blanks)                   tabellata fase prec.
273200990226     c  N30              ENDDO
273300990226     c                   ENDIF
273400990226
273500990226      * Fino alla conferma per clienti con particolarità
273600990226     c                   WHEN      *in06 = *off  and  Wmsg97frz = *blanks
273700990226     c                                           and  Wclipar = 'S'
273800990226     c                   eval      Wmsg97frz = 'S'
273900990226     c                   eval      *in28 = *ON
274000990226     c                   movel     msg(97)       V1Cmsg
274100990226
274200990226     c                   ENDSL
274300990226
274400990226     c                   ENDSR
274500990226      **********************************************************************
274600990226      *  CONTROLLI VIDEATA 1
274700990226      **********************************************************************
274800990226     C     CTRD01        BEGSR
274900990226      *
275000000628     c     41            DO        50            XX
275100980626     c                   eval      *IN(XX) = *OFF
275200980626     c                   ENDDO
275300981127
275400981127      * LIMITE RISARCIBILE
275500981127     c                   IF        Wlimris = *zeros
275600981127     C                   eval      *in28 = *on
275700981127     C                   movel     MSG(23)       V1CMSG
275800981127     c                   ENDIF
275900981127     C   28              GOTO      ECTD01
276000980626
276100980626      * DATA APERTURA C.A. (solo in apertura)
276200981127     c                   IF        I00mod = 'A'
276300980619      *
276400980626     C                   IF        V1CDCA  = *zeros
276500990129     c                   eval      *in41 = *ON
276600990129     c                   eval      *in28 = *ON
276700980626     C                   movel     MSG(3)        V1CMSG
276800980626     C                   ELSE
276900980626      *
277000980626     C                   clear                   WLBDAT
277100980626     C                   z-add     V1CDCA        G02DAT
277200971020     C                   CALL      'XSRDA8'
277300971020     C                   PARM                    WLBDAT
277400990113     C                   IF        G02ERR = '1'
277500990129     c                   eval      *in41 = *ON
277600990129     c                   eval      *in28 = *ON
277700980626     C                   movel     MSG(4)        V1CMSG
277800990113     C                   ELSE
277900990113      *
278000980626     C                   move      G02INV        INVDCA            8 0
278100980626     C                   z-add     G02DAT        V1CDCA
278200990113      *   La data apertura non può essere minore della data spedizione
278300990113     C                   If        INVdca  <  DS_arbdat
278400990129     c                   eval      *in41 = *ON
278500990129     c                   eval      *in28 = *ON
278600990113     C                   movel     MSG(86)       V1CMSG
278700981127     C                   Endif
278800980626     C                   ENDIF
278900990113     C                   ENDIF
279000980626      *
279100981127     C                   ENDIF
279200980626     C   28              GOTO      ECTD01
279300981020
279400981020      * NUMERO EVENTO
279500981020     c                   exsr      CTRnev
279600990111      *    Sostituisco zero non significativi con blanks
279700981104     c                   IF        V1Cnev = *ZEROS
279800981104     c                   clear                   V1Cnev
279900981104     c                   ELSE
280000981104     c                   movel     V1Cnev        W007A
280100981104     c                   clear                   V1CNEV
280200981104     c     '0'           check     W007A         XX
280300981104     c                   eval      %subst(V1Cnev:xx) = %subst(W007A:xx)
280400981104     C                   ENDIF
280500990111      *
280600990111     c                   IF        *in28 = *on  or  *IN29 = *ON
280700990111     C                   goto      ECTD01
280800990111     C                   ENDIF
280900020419      * ERRORE se non sono in apertura e tolgo il numero evento
281000020419      *   in caso di spedizione posta errore
281100000628      *
281200020419     c                   if        i00mod <> 'A' and V1Cnev = *blanks
281300020419     c                                           and lnppt = 'S'
281400000628     C                   eval      *in50 = *on
281500000628     C                   eval      *in42 = *on
281600000628     C                   eval      *in28 = *on
281700000628     C                   movel     MSG(110)      V1CMSG
281800000628     c                   endif
281900980626
282000981020      * DESCRIZIONE ANOMALIA
282100980626     c                   IF        V1Cfag = '+'   OR
282200980626     c                               (V1Hfag = '+'  AND  V1Hagg <> *BLANKS
282300980626     c                                              AND  V1Dagg = *BLANKS)
282400980619      * Richiamo pgm gestione descrizioni
282500981020     C                   CLEAR                   FIDN10DS
282600980619      *
282700990121     c                   IF        I00mod = 'A'
282800980619     c                   movel     'M'           i10flm
282900990121     c                   ELSE
283000990121     c                   movel     'V'           i10flm
283100990120     c                   movel     DS_numca      i10nkt
283200990121     c                   ENDIF
283300990120      *
283400980720     c                   z-add     Wdataca       i10dta
283500971030     c                   movel     'C'           i10tpd
283600971030     c                   movel     'A'           i10std
283700980730     c                   clear                   i10nks
283800971030     c                   movel     'D'           i10trc
283900971030     c                   movel     v1dagg        i10not
284000981020     c                   movel(P)  FIDN10DS      kpjbu
284100980619      *
284200980619     C                   CALL      'FIDN10R'
284300971030     C                   PARM                    kpjba
284400980619      *
284500981020     c                   movel     kpjbu         FIDN10DS
284600980626      *
284700980626     C                   IF        O10ERR = *blanks
284800980626     C                   movel     O10NOT        V1DAGG
284900980626     C                   movel     O10NOT        V1hAGG
285000980626     C                   clear                   V1CFAG
285100980626     c                   if        v1hagg <> *blanks
285200980424     c                   move      '+'           v1hfag
285300980619     c                   endif
285400980626     C                   ENDIF
285500980626      *
285600980626     C                   eval      *in29 = *on
285700980626     C                   GOTO      ECTD01
285800980626     C                   ENDIF
285900980626      *
286000980619     c                   movel     v1dagg        v1hagg
286100980626      * controllo se obbligatorio
286200980626     c                   IF        §TADdagg = 'S'  and  V1Dagg = *blanks
286300990129     C                   eval      *in48 = *on
286400990129     C                   eval      *in28 = *on
286500980626     C                   movel     MSG(43)       V1CMSG
286600980626     C                   GOTO      ECTD01
286700980626     C                   ENDIF
286800980626
286900980619      * MOTIVO DEL DANNO
287000980626     c                   IF        V1Cfmt = '+'   OR
287100980626     c                               (V1Hfmt = '+'  AND  V1Hmtv <> *BLANKS
287200980626     c                                              AND  V1Dmtv = *BLANKS)
287300980623      * Richiamo pgm gestione descrizioni
287400981020     C                   CLEAR                   FIDN10DS
287500990120      *
287600990121     c                   IF        I00mod = 'A'
287700990121     c                   movel     'M'           i10flm
287800990121     c                   ELSE
287900990121     c                   movel     'V'           i10flm
288000990121     c                   movel     DS_numca      i10nkt
288100990121     c                   ENDIF
288200990120      *
288300980720     c                   z-add     Wdataca       i10dta
288400980623     c                   movel     'C'           i10tpd
288500980609     c                   movel     'M'           i10std
288600980609     c                   movel     'D'           i10trc
288700971030     c                   movel     v1dmtv        i10not
288800981020     c                   movel(P)  FIDN10DS      kpjbu
288900980626      *
289000980722     C                   CALL      'FIDN10R'
289100971030     C                   PARM                    kpjba
289200980626      *
289300981020     c                   movel     kpjbu         FIDN10DS
289400980626      *
289500980626     C                   IF        O10ERR = *blanks
289600980626     C                   movel     O10NOT        V1DMTV
289700980626     C                   movel     O10NOT        V1hmtv
289800980626     C                   clear                   V1CFMT
289900980626     c                   if        v1hmtv <> *blanks
290000980424     c                   move      '+'           v1hfmt
290100980626     c                   endif
290200980626     C                   ENDIF
290300980626      *
290400980626     C                   eval      *in29 = *on
290500971021     C                   GOTO      ECTD01
290600980626     C                   ENDIF
290700980626      * controllo se obbligatorio
290800980626     c                   IF        §TADmtvd = 'S'  and  V1Dmtv = *blanks
290900990129     C                   eval      *in49 = *on
291000990129     C                   eval      *in28 = *on
291100980626     C                   movel     MSG(44)       V1CMSG
291200980626     C                   GOTO      ECTD01
291300980626     C                   ENDIF
291400981019
291500990209      * PRESENTATA RISERVA
291600990421      *     Se presente Evento Riserva = "R  " non posso negarla, solo per sped consegnata
291700990304      *     Se C.A. in attesa di ricalcolo responsabilità non posso variare
291800990415      *     Dopo l'Apertura non è possibile togliere riserva
291900990304     c                   SELECT
292000990304      *
292100990304     c                   WHEN      V1Ccrc = 'N'  and  Wriserva = 'S'
292200990421     c                                           and  §TADtico = 'C'
292300990304     c                   movel     msg(99)       v1cmsg
292400990309      *
292500990304     C                   WHEN      V1Ccrc <> WV1Ccrc  and
292600990304     C                                      (WDCTcar = 'R'  or  WDCTcar = 'T')
292700990209     c                   movel     msg(91)       v1cmsg
292800990415      *
292900990415     c                   WHEN      I00MOD <> 'A'  and  V1Ccrc = 'N'
293000990415     c                                            and  WV1Ccrc = 'S'
293100990415     c                   movel     msg(108)      v1cmsg
293200990309      *
293300990306     c                   ENDSL
293400990309      *
293500990309     C                   If        V1Cmsg <> *blanks
293600990309     C                   eval      *in45 = *on
293700990309     c                   eval      *in28 = *on
293800990309     C                   goto      ECTD01
293900990309     c                   Endif
294000990209
294100990209      * TEST VERIFICA DDT
294200990315      *     Fino alla conferma la  Partenza può inserire " " o "P", l'Arrivo può inserire " " o "A"
294300990315      *     Dalla conferma in poi è obbligatorio, se richiesto da tabella TAD, con le stesse regole
294400990315     C                   SELECT
294500990315      *
294600990315     C                   WHEN      V1Ctdt = *blanks  and  *IN05 = *ON
294700990315      *
294800990315     C                   WHEN      V1Ctdt = *blanks  and  §tadfddt = 'S'
294900990209     c                   movel     msg(56)       v1cmsg
295000990317      *
295100990317     c                   WHEN      V1CTDT = WV1CTDT
295200990317      *
295300000428     c                   WHEN      Wlocale = 'S'
295400981019      *
295500990315     C                   WHEN      V1Ctdt = 'A'
295600000428     c     Wlnp          lookup    L6                                     30
295700000428     c                   if        *in30
295800000428     c                   movel     msg(54)       v1cmsg
295900000308     c                   endif
296000981019      *
296100990315     C                   WHEN      V1Ctdt = 'P'
296200000428     c     Wlna          lookup    L6                                     30
296300000428     c                   if        *in30
296400000428     c                   movel     msg(55)       v1cmsg
296500000308     c                   endif
296600981019      *
296700990315     c                   ENDSL
296800981019      *
296900981019     C                   if        V1Cmsg <> *blanks
297000990129     C                   eval      *in43 = *on
297100990129     C                   eval      *in28 = *on
297200981019     C                   goto      ECTD01
297300981019     c                   endif
297400980626
297500981126      * BENEFICIARIO ALTERNATIVO
297600981022     c                   move      V1Cksc        w0040
297700981022     c                   clear                   Wclicon
297800981022     c                   SELECT
297900990226      *
298000981022      *    se immessa ragione sociale e non il codice richiamo il pgm di ricerca alfabetica
298100981022     c                   WHEN      V1Cksc = *zeros  and  V1Dksc <> *blanks
298200981022     c                   movel     V1Dksc        WDksc
298300980525     c                   exsr      ricalf
298400981022     c                   eval      *in29 = *on
298500990226      *
298600981022      *    Se codice 9999 segnalazione di errore
298700981022     c                   WHEN      w0040 = 9999
298800980513     c                   movel     msg(18)       v1cmsg
298900990129     C                   eval      *in47 = *on
299000990129     C                   eval      *in28 = *on
299100990226      *
299200990311      *    se codice 8888 ed è inserito l'importo trattenuto segnalazione di errore
299300990120     c                   WHEN      w0040 = 8888  and  V2Cipt > *zeros
299400990120     c                   movel     msg(71)       v1cmsg
299500990129     C                   eval      *in47 = *on
299600990129     C                   eval      *in28 = *on
299700990226      *
299800990311      *    se codice 8888 controllo il P.O. ed emetto videata dati anagrafici se non inseriti
299900981022     c                   WHEN      w0040 = 8888
300000990311     c                   movel     V1Cksc        w0030
300100990408     C     W0030         chain(N)  AZORG                              31
300200990311     c                   If        *in31 = *on  or  ORGfva <> *blanks
300300990311     c                   eval      *in28 = *ON
300400990311     c                   movel     msg(58)       v1cmsg
300500990311     c                   Else
300600981022     c                   movel     'S'           Wclicon
300700981022     c                   if        CW2crag = *blanks
300800981022     c                   movel     DS_W02ccon    DS_FI03W02
300900981102     c                   movel     'C'           Wtrc
301000980626     c                   exsr      mancli
301100980717     c                   eval      *in29 = *ON
301200990226     c                   endif
301300990311     c                   endif
301400990226      *
301500990301      *    se codificato deve essere su P.d.C. e se ho imp. trattenuto deve essere di fatturazione
301600981022     c                   WHEN      V1Cksc <> *zeros
301700990226     c                   exsr      CTR_benalt
301800980626      *
301900080229      *      emettere un messaggio informativo se modificato il beneficiario
302000080303     c                   If        DCTksc = V1Cksc  and  V1Cksc > *zeros  and
302100080229     c                             V1Cptr <> DCTptr  and  Wmsg116frz = *blanks
302200080229     c                   eval      Wmsg116frz = 'S'
302300080229     c                   movel     msg(116)      V1cmsg
302400080229     C                   eval      *in47 = *on
302500080229     C                   eval      *in28 = *on
302600080303     c                   endif
302700110504      *
302800110504      *    in fase di conferma CA e dopo conferma se CA aperta su Spedizione dirottata e con
302900110504      *    di porto chiedo obbligatoriamente il beneficiario alternativo
303000110504     c                   WHEN      V1Cksc= *zeros and (§dctcca = '9' or
303100110504     c                             §dctcca = '1') and *in05 = *off
303200110504     c                   movel     msg(117)      v1cmsg
303300110504     C                   eval      *in47 = *on
303400110504     C                   eval      *in28 = *on
303500981022     c                   ENDSL
303600980626      *
303700990303     c                   z-add     V1Cksc        Wv1cksc
303800980626     c                   if        *in28 = *on  OR  *in29 = *on
303900980626     C                   GOTO      ECTD01
304000980626     C                   endif
304100980709
304200981126      * TIPO GESTIONE
304300020419      *        Attenzione: '*?*' sono controlli ripetuti in CTRD02,
304400020419      *                    riportare eventuali modifiche
304500990128     c                   SELECT
304600990128      *
304700990129     c                   WHEN        *IN05 = *ON                                  *?*
304800980709      *
304900990128     c                   WHEN        V1Cfpr = *blanks
305000980709     c                   movel     msg(49)       v1cmsg
305100980709      *
305200990128     c                   WHEN        V1Cfpr = 'T'
305300120712     ***c                   eval      V2cpgd = 'V'
305400990128     c                   exsr      impPROPR                                     *?*
305500080225      * se sono in gestione alla sede ed è consigliabile gestire in pratica non lo faccio
305600080225      * do errore
305700080225     c                   If        dctgfc=46 and Wfprfrz='S'
305800080211     c                   eval      Wfprfrz = 'N'
305900080211     c                   endif
306000990128     c                   if        Wfpr = 'P' and  Wfprfrz = 'S'                *?*
306100990128     c                   movel     msg(50)       v1cmsg                         *?*
306200990128     c                   endif                                                  *?*
306300990128     c                   if        Wfpr = 'P' and  Wfprfrz = 'N'                *?*
306400990205     c                             and  WV1Cfpr1 <> 'T'                         *?*
306500990205     c                   movel     'T'           WV1Cfpr1                       *?*
306600990128     c                   movel     msg(51)       v1cmsg                         *?*
306700990128     c                   endif                                                  *?*
306800080208      *
306900080208     c                   WHEN        V1Cfpr = 'P'
307000080208     c                   exsr      impPROPR                                     *?*
307100080225      * se sono in gestione alla sede ed è consigliabile gestire in lt non lo faccio
307200080225      * do errore
307300080225     c                   If        dctgfc=46 and Wfprfrz='S'
307400080211     c                   eval      Wfprfrz = 'N'
307500080211     c                   endif
307600080303      * se ho importo trattenuto non modifico il tipo gestione
307700080303     c                   If        v2cipt > 0 and wfpr = 'T'
307800080303     c                   eval      wfprfrz = 'N'
307900080303     c                   eval      wfpr    = 'P'
308000080303     c                   endif
308100080303      *
308200080208     c                   if        Wfpr = 'T' and  Wfprfrz = 'S'                *?*
308300080208     c                   movel     msg(114)      v1cmsg                         *?*
308400080208     c                   endif                                                  *?*
308500080211     c                   if        Wfpr = 'T' and  Wfprfrz = 'N'
308600080211     c                             and  WV1Cfpr1 <> 'P'
308700080211     c                   movel     'P'           WV1Cfpr1
308800080211     c                   movel     msg(115)      v1cmsg
308900080211     c                   endif
309000990128      *
309100120712     ***c                   WHEN        V2cipt > *zeros
309200120712     ***c                   eval      V2cpgd = 'V'
309300990128      *
309400120712     ***c                   WHEN        WV1Cfpr = 'T'  and  WCLSflo3 <> *blanks
309500120712     ***c                   movel     WCLSflo3      V2Cpgd
309600120712     *** *
309700120712     ***c                   WHEN        WV1Cfpr = 'T'  and  WCLSflo3 = *blanks
309800120712     ***c                   eval      V2cpgd = 'C'
309900990128      *
310000990128     c                   ENDSL
310100980709      *
310200990122     c                   IF        V1Cmsg <> *blanks
310300080310      * se sono in gestione alla filiale imposto il tipo gestione
310400080310     c                   If        dctgfc <> 46 and Wfprfrz='S'
310500080208     c                   eval      v1cfpr = wfpr
310600080208     c                   eval      wv1cfpr = wfpr
310700080208     c                   endif
310800990129     C                   eval      *in44 = *on
310900990129     C                   eval      *in28 = *on
311000980709     C                   goto      ECTD01
311100990205     c                   ELSE
311200990205     c                   movel     V1Cfpr        WV1Cfpr
311300990122     c                   ENDIF
311400981127
311500981127      * BENEFICIARIO
311600020419      *      DESTINATARIO o nulla se emesso dall'apertura alla conferma
311700020419      **NO** MITTENTE     non modificabile se spedizione export
311800020419      *      MITTENTE     non modificabile se spedizione import in Franco
311900020419      *      DESTINATARIO non modificabile se spedizione import in Assegnato
312000990308      * Fino alla conferma segnalo l'inserimento manuale del beneficiario
312100981127     c                   SELECT
312200990308      *
312300990308     c                   WHEN      *IN05 = *ON  and  *IN34 = *ON
312400990308     c                                          and  V1Cptr = 'M'
312500990308     c                   movel     msg(104)      v1cmsg
312600990308      *
312700990308     c                   WHEN      *IN05 = *ON  and  *IN34 = *ON
312800990308     c                                          and  V1Cptr = 'D'
312900990308     c                   movel     'Manuale'     V1Dptr
313000990308      *
313100990308     c                   WHEN      *IN05 = *ON  and  *IN34 = *ON
313200990308     c                   clear                   V1Dptr
313300990308      *
313400990121     c                   WHEN      *IN05 = *ON
313500990308      *
313600990121     c                   WHEN       V1Cptr = *blanks
313700990121     c                   movel     msg(49)       v1cmsg
313800990309     c                   clear                   V1Dptr
313900990415      *
314000990415     c                   WHEN       §DCTtisp = 'E'  and  V1Cptr = 'D'
314100990415     c                                              and  V1Cfpr <> 'P'
314200080604     c                   eval      v1Cfpr = 'P'
314300990415     c                   movel     msg(107)      v1cmsg
314400990308      *
314500990121     c                   WHEN       §DCTtisp = 'I'  and  §DCTport = 'F'
314600990121     c                              and  V1Cptr <> 'M'
314700981201     c                   movel     msg(79)       v1cmsg
314800990308      *
314900990121     c                   WHEN       §DCTtisp = 'I'  and  §DCTport = 'A'
315000990121     c                              and  V1Cptr <> 'D'
315100981127     c                   movel     msg(25)       v1cmsg
315200990309      *
315300990309     c                   WHEN       I00mod = 'G'  and  V1Cptr <> 'D'
315400990309     c                   clear                   V1Dptr
315500990308      *
315600981127     c                   ENDSL
315700990121      *
315800990121     c                   If        V1Cmsg <> *blanks
315900990129     C                   eval      *in46 = *on
316000990129     C                   eval      *in28 = *on
316100990121     C                   goto      ECTD01
316200990121     c                   Endif
316300990306
316400990306      * CONTROLLI APERTURA
316500990308      *     Se avaria non posso avere l'evento smarrimento autotrasportatore
316600990309      *     Se locale non effettuo controlli per la riserva
316700990306      *     Se partenza, sped consegnata, no riserva non apro PA segue fattura
316800990306      *     Se arrivo, sped consegnata, no riserva non apro PF
316900990306      *     Se partenza, sped consegnata, si riserva non apro
317000000628      *     Se partenza linea posta devo avere l'evento valorizzato
317100990306      *
317200990306     c                   SELECT
317300990309      *
317400990309     c                   WHEN      I00mod <> 'A'
317500990308      *
317600990308     c                   WHEN      §TADragr = 'V'  and  DETtad = '15'
317700990308     c                   eval      *in28 = *on
317800990308     c                   movel     msg(103)      v1cmsg
317900990306      *
318000000428     c                   WHEN      Wlocale = 'S'
318100990306      *
318200990306     c                   WHEN      V1Ccrc  = 'N'  and  §TADtico = 'C'
318300990306     c                                            and  §DCTport = 'A'
318400990306     c     Wlnp          lookup    L6                                     28
318500990306     c   28              movel     msg(100)      v1cmsg
318600990306      *
318700990306     c                   WHEN      V1Ccrc  = 'N'  and  §TADtico = 'C'
318800990306     c                                            and  §DCTport = 'F'
318900990306     c     Wlna          lookup    L6                                     28
319000990306     c   28              movel     msg(101)      v1cmsg
319100990306      *
319200990306     c                   WHEN      V1Ccrc  = 'S'  and  §TADtico = 'C'
319300990306     c     Wlnp          lookup    L6                                     28
319400170509     c* solo per la DPD è possibile aprire in partenza
319500170515     c* Verifico se il profilo può aprire in partenza la c.a.
319600170515     c                   if        (*in28 and wlnpntw= 'DPD'
319700170515     c                             and §utemdppca<>'S') or
319800170515     c
319900170515     c                             (*in28 and wlnpntw<>'DPD')
320000170509     c                   movel     msg(102)      v1cmsg
320100170509     c                   else
320200170509     c                   setoff                                       28
320300170509     c                   endif
320400000628      *
320500170509     c***                WHEN      V1Cnev = *blanks and lnppt = 'S'
320600170509     C***                eval      *in50 = *on
320700170509     C***                eval      *in28 = *on
320800170509     C***                movel     MSG(109)      V1CMSG
320900990306      *
321000990306     c                   ENDSL
321100990306     C   28              goto      ECTD01
321200980724
321300971020     C     ECTD01        ENDSR
321400980716      **********************************************************************
321500980716      *  CONTROLLI VIDEATA 2
321600980716      **********************************************************************
321700980716     C     CTRD02        BEGSR
321800980716
321900981029     c     76            DO        90            XX
322000980722     c                   eval      *IN(XX) = *OFF
322100980722     c                   ENDDO
322200980722
322300990114      * Numero Colli
322400980716     c                   SELECT
322500990209      *
322600990209      *    Campo non modificabile se CA in attesa di ricalcolo responsabilità
322700990209     c                   WHEN      V2Cncn <> WV2Cncn  and
322800990209     c                             (WDCTcar = 'R'  or  WDCTcar = 'T')
322900990209     C                   movel     MSG(91)       V1Cmsg
323000981030      *
323100990217      *    se richiesto in tabella o spedizione di 1 collo controllo che i colli siano indicati
323200990217     c                   WHEN      V2Cncn = *zeros  and
323300990217     c                              (§TADdcdn = 'S'  or  VTCncl = 1)
323400980716     C                   movel     MSG(36)       V1Cmsg
323500981030      *
323600990114      *    il n° dei colli danneggiati/mancanti non può superare il n° colli della spedizione
323700981020     c                   WHEN      V2Cncn > VTCncl  and  I00nsp > *zeros
323800980716     C                   movel     MSG(10)       V1Cmsg
323900990114      *
324000990114      *    il n° dei colli danneggiati/mancanti non può superare il n° dei colli selezionabili
324100990126     c                   WHEN      JJ < V2Cncn  and  I00nsp > *zeros
324200990114     C                   movel     MSG(48)       V1Cmsg
324300981030      *
324400990114      *    per spedizione posso avere solo una CA senza il n° dei colli danneggiati/mancanti
324500990114     c                   WHEN      Wtestca  = 2  and  V2Cncn = *zeros
324600980716     C                   movel     MSG(47)       V1Cmsg
324700990217      *
324800990217      *    se reso il numero colli deve essere maggiore di zero
324900990217     c                   WHEN      Wreso = 'S'  and  V2Cncn = *zeros
325000990217     C                   movel     MSG(93)       V1Cmsg
325100980716      *
325200980716     c                   ENDSL
325300980716      *
325400980716     c                   IF        V1Cmsg <> *blanks
325500990129     C                   eval      *in81 = *on
325600990129     C                   eval      *in28 = *on
325700980716     C                   GOTO      ECTD02
325800980716     c                   ENDIF
325900980716
326000981103      * Il peso danneggiato/mancante non può superare il peso sped. e se tabellato deve essere > 0
326100990114     c                   SELECT
326200000307     c* messaggio forzabile
326300000307     c                   WHEN      V2Cpkd > VTCpkf  and  I00nsp > *zeros  and
326400000307     c                             wmsg33frz = *blanks
326500990129     C                   eval      *in82 = *on
326600990129     C                   eval      *in28 = *on
326700980918     C                   movel     MSG(33)       V1CMSG
326800000307     c                   eval      wmsg33frz = 'S'
326900051019      *
327000051019      * messaggio forzabile
327100051019     c                   WHEN           Wmsg113frz = *blanks
327200051019      * se dal P.O.
327300051019     c                             and  DUTlpo    <> 'S'
327400051019      *    viene modificato
327500051019     c                             and  DCTpkd     > *zeros
327600051019      *    il peso danneggiato
327700051019     c                             and  V2Cpkd    <> DCTpkd
327800051019      *    di una C.A. con tipo anomalia "Avaria"
327900051019     c                             and  §TADragr   = 'V'
328000051019      *    con importo massimo risarcibile >= importo massimo per perizia
328100051019     c                             and  V2Cipv    >= §STDipa
328200051019     c                   eval      *in82 = *on
328300051019     c                   eval      *in28 = *on
328400051019     c                   movel     Msg(113)      V1Cmsg
328500051019     c                   eval      Wmsg113frz = 'S'
328600051019      *
328700990114     c                   WHEN       §TADkgdo = 'S'  AND  V2Cpkd = *zeros
328800990129     C                   eval      *in82 = *on
328900990129     C                   eval      *in28 = *on
329000980918     C                   movel     MSG(61)       V1CMSG
329100990114     c                   ENDSL
329200980918     C   28              goto      ECTD02
329300980918
329400980918      * Se il numero pezzi danneggiati/mancanti è richiesto obbligatorio in tabella devo:
329500980918      *   DALLA CONFERMA IN POI = numero pezzi > 0
329600990208      *   PRIMA DELLA CONFERMA = inserire n° pezzi oppure descrizione pezzi presenti tranne Avarie
329700980918      *
329800990114     c                   SELECT
329900990208     c                   WHEN        §TADpzdo <> 'S'  or  V2Cnpz > *zeros
330000990208     c                   WHEN        §TADragr <> 'V'
330100990208     C                   movel     MSG(37)       V1CMSG
330200990208     C                   eval      *in86 = *on
330300990208     C                   eval      *in28 = *on
330400990208     c                   WHEN        *IN05 = *off
330500990129     C                   movel     MSG(37)       V1CMSG
330600990129     C                   eval      *in86 = *on
330700990129     C                   eval      *in28 = *on
330800990208     c                   WHEN        V2Cprs = *blanks  and  V2Cfps <> '+'
330900990129     C                   movel     MSG(62)       V1CMSG
331000990129     C                   eval      *in86 = *on
331100990129     C                   eval      *in28 = *on
331200990114     c                   ENDSL
331300980918     C   28              goto      ECTD02
331400980918
331500981216      * se immesso i numero pezzi dann./manc. è obbligatorio inserire la descrizione e viceversa
331600981216     C                   IF        V2Cnpz > *ZEROS  and  V2Dnpz = *BLANKS
331700981216     C                                              and  V2Cfpz <> '+'
331800990129     C                   movel     MSG(9)        V1CMSG
331900990129     C                   eval      *in87 = *on
332000990129     C                   eval      *in28 = *on
332100980918     C                   ELSE
332200980918     C                   IF        V2Dnpz <> *BLANKS  and  V2Cnpz = *ZEROS
332300990111     C                   movel     MSG(84)       V1CMSG
332400990129     C                   eval      *in86 = *on
332500990129     C                   eval      *in28 = *on
332600980918     C                   ENDIF
332700980918     C                   ENDIF
332800981211     C   28              goto      ECTD02
332900980716
333000980716      * DESCRIZIONE PEZZI DANNEGGIATI/MANCANTI
333100980716     c                   IF        V2Cfpz = '+'   OR
333200980716     c                               (V2Hfpz = '+'  AND  V2Hnpz <> *BLANKS
333300980716     c                                              AND  V2Dnpz = *BLANKS)
333400980716      * Richiamo pgm gestione descrizioni
333500981020     C                   CLEAR                   FIDN10DS
333600990120      *
333700990121     c                   IF        I00mod = 'A'
333800990121     c                   movel     'M'           i10flm
333900990121     c                   ELSE
334000990121     c                   movel     'V'           i10flm
334100990121     c                   movel     DS_numca      i10nkt
334200990121     c                   ENDIF
334300990120      *
334400980720     c                   z-add     Wdataca       i10dta
334500980716     c                   movel     'C'           i10tpd
334600980716     c                   movel     'P'           i10std
334700980716     c                   movel     'D'           i10trc
334800980716     c                   movel     V2Dnpz        i10not
334900981020     c                   movel(P)  FIDN10DS      kpjbu
335000980716      *
335100980722     C                   CALL      'FIDN10R'
335200980716     C                   PARM                    kpjba
335300980716      *
335400981020     c                   movel     kpjbu         FIDN10DS
335500980716      *
335600980716    2C                   IF        O10ERR = *blanks
335700980716     C                   movel     O10NOT        V2Dnpz
335800980716     C                   movel     O10NOT        V2Hnpz
335900980716     C                   clear                   V2Cfpz
336000980716     c                   if        V2Hnpz <> *blanks
336100980716     c                   move      '+'           V2Hfpz
336200980716     c                   endif
336300980716    2C                   ENDIF
336400980716      *
336500980716     C                   eval      *in29 = *on
336600980716     C                   GOTO      ECTD02
336700981211    2C                   ENDIF
336800980716
336900980716      * DESCRIZIONE PEZZI PRESENTI
337000980716     c                   IF        V2Cfps = '+'   OR
337100980716     c                               (V2Hfps = '+'  AND  V2Hprs <> *BLANKS
337200980716     c                                              AND  V2Cprs = *BLANKS)
337300980716      * Richiamo pgm gestione descrizioni
337400981020     C                   CLEAR                   FIDN10DS
337500990120      *
337600990121     c                   IF        I00mod = 'A'
337700990121     c                   movel     'M'           i10flm
337800990121     c                   ELSE
337900990121     c                   movel     'V'           i10flm
338000990121     c                   movel     DS_numca      i10nkt
338100990121     c                   ENDIF
338200990120      *
338300980720     c                   z-add     Wdataca       i10dta
338400980716     c                   movel     'C'           i10tpd
338500980716     c                   movel     'K'           i10std
338600980716     c                   movel     'D'           i10trc
338700980716     c                   movel     V2Cprs        i10not
338800981020     c                   movel(P)  FIDN10DS      kpjbu
338900980716      *
339000980722     C                   CALL      'FIDN10R'
339100980716     C                   PARM                    kpjba
339200980716      *
339300981020     c                   movel     kpjbu         FIDN10DS
339400980716      *
339500980716    2C                   IF        O10ERR = *BLANKS
339600980716     C                   movel     O10NOT        V2Cprs
339700980716     C                   movel     O10NOT        V2Hprs
339800980716     C                   clear                   V2Cfps
339900980716     c                   if        V2Hprs <> *blanks
340000980716     c                   move      '+'           V2Hfps
340100980716     c                   endif
340200980716    2C                   ENDIF
340300980716      *
340400980716     C                   eval      *in29 = *on
340500980716     C                   GOTO      ECTD02
340600981211     C                   ENDIF
340700980720
340800980720      * TEST CHIUSURA MERCE DANNEGGIATA
340900980721      *     Obbligatorio se richiesto da tabella TAD.
341000980721     C                   IF        §tadfmed = 'S' and  V2Cdsd = *blanks
341100990129     C                   eval      *in85 = *on
341200990129     C                   eval      *in28 = *on
341300980720     c                   movel     msg(57)       v1cmsg
341400980720     C                   goto      ECTD02
341500980720     c                   endif
341600981211
341700990510      * RESPONSABILITA' PER INTERNAZIONALE
341800020419      *     Se gestito è obbligatorio dall'apertura  per EXPORT
341900020419      *                             e dalla conferma per IMPORT
342000981211     c                   SELECT
342100990128     c                   WHEN      *IN08 = *OFF
342200981211     c                   WHEN      V2Cres <> *blanks
342300020419     c                   WHEN      §DCTtisp = 'E'  and  I00fca >= WFape
342400981211     C                   movel     msg(80)       V1Cmsg
342500990129     C                   eval      *in80 = *on
342600990129     C                   eval      *in28 = *on
342700981228     c                   WHEN      §DCTtisp = 'I'  and  *IN05 = *OFF
342800981211     C                   movel     msg(80)       V1Cmsg
342900990129     C                   eval      *in80 = *on
343000990129     C                   eval      *in28 = *on
343100981211     c                   ENDSL
343200981211     c   28              goto      ECTD02
343300020419      *
343400021108      *  Durante la conferma, fase 100, di una c.a. aperta su
343500021108      *  spedizione export DPD o FedEx, se V2CRES = "P" - PARTNER, è
343600021108      *  obbligatorio il "tipo gestione" = "P" - Pratica assicurativa
343700020426     c                   if            not *in05
343800020426     c                             and not *in06
343900020419     c                             and §DCTtisp = 'E'
344000021108     c                             and (§DCTeuro = 'D'
344100021108     c                              or  §DCTeuro = 'F')
344200020419     c                             and V2Cres   = 'P'
344300020419     c                             and V1Cfpr  <> 'P'
344400020419     c                   eval      *in28 = *ON
344500020426     c                   eval      *in80 = *ON
344600020419     c                   movel     msg(50)       V1CMSG
344700080604      * se sono in gestione alla filiale imposto il tipo gestione
344800080604     c                   If        dctgfc <> 46
344900080604     c                   eval      v1cfpr = 'P'
345000080604     c                   endif
345100080604     c                   goto      ECTD02
345200080604     c                   endif
345300981014
345400981014      * CLIENTE CON MANDATO A VALORE VARIABILE
345500020419      *     Se il valore non è in bolla emetto un msg forzabile
345600020419      *     in APERTURA bloccante dalla conferma
345700990319     c                   IF        ARBias = *zeros  AND  Wvalvar = 'S'
345800981130     c   05              clear                   Wvalvar
345900981130     c                   eval      *in28 = *ON
346000981014     c                   movel     msg(65)       V1CMSG
346100981014     C                   goto      ECTD02
346200981014     c                   ENDIF
346300981014
346400981113      * IMPORTO MASSIMO RISARCIBILE
346500020419      *     Se è forzato l'inserimento manuale il campo è obbligatorio
346600020419      *     dalla Conferma per le C.A. che hanno Reso come disposizione
346700020419      *     merce danneggiata, dall'Apertura negli altri casi.
346800020419      *     Se l'inserimento è libero memorizzo il valore calcolato se
346900020419      *     non inserito dall'utente
347000981104      *
347100981130      *   Controllo la Valuta
347200010912     c                   clear                   Wdctcpv
347300981130     C                   movel     V2Cvpv        WVLT
347400981130     C                   z-add     V2Cipv        W0133
347500981130     C                   exsr      ctrvlt
347600981130      *
347700981130     C                   IF        *IN28 = *ON  or  *IN29 = *ON
347800010912     C                   movel     *on           *in83
347900981130     C                   goto      ECTD02
348000981130     c                   ENDIF
348100981130      *
348200981130      *   Controllo l'importo
348300981104     c                   IF        V2Cipv = *zeros
348400981104     c                   clear                   WDCTfpv
348500981104     c                   clear                   V2Dfpv
348600981130     c                   clear                   £DCTipv
348700981104     c                   ENDIF
348800981113      *
348900981130      *        Calcolo Importo Massimo Risarcibile
349000981104     c                   exsr      CALCVAS
349100981104      *
349200981104     c                   SELECT
349300981104      *
349400981104     c                   WHEN      §TADimpr = 'M'  and  V2Cdsd <> 'R'
349500981104     c                              and  V2Cipv = *zeros
349600981104     c                   eval      *in28 = *on
349700981104     c                   movel     msg(66)       V1CMSG
349800981104      *
349900981104     c                   WHEN      §TADimpr = 'M'  and  V2Cdsd = 'R'
350000981104     c                              and  V2Cipv = *zeros  and  *IN05 = *off
350100981104     c                   eval      *in28 = *on
350200981104     c                   movel     msg(66)       V1CMSG
350300981104      *
350400981104     c                   WHEN      WDCTfpv = *blanks  and  V2Cipv > *zeros
350500981104     c                   movel     'M'           WDCTfpv
350600981104     C                   z-add     V2Cipv        WV2Cipv
350700981130     c                   movel     V2Cvpv        WV2Cvpv
350800981104     c                   eval      V2Dfpv = 'Inserito manualmente'
350900981104      *
351000981130     c                   WHEN      V2Cipv <> *zeros  AND
351100981130     c                               (V2Cipv <> WV2Cipv  or  V2Cvpv <> WV2Cvpv)
351200981104     c                   movel     'M'           WDCTfpv
351300981104     C                   z-add     V2Cipv        WV2Cipv
351400981130     c                   movel     V2Cvpv        WV2Cvpv
351500981104     c                   eval      V2Dfpv = 'Inserito manualmente'
351600981104      *
351700981104     c                   WHEN      *IN05 = *OFF  and  WDCTfpv <> 'M'
351800981104     C                   z-add     Wvalass       V2Cipv
351900981104     C                   z-add     Wvalass       WV2Cipv
352000010911     c                   movel     Wvlt          V2Cvpv
352100010911     c                   movel     Wvlt          WV2Cvpv
352200010912     c                   clear                   Wdctcpv
352300981104     c                   movel     'C'           WDCTfpv
352400981104     c                   eval      V2Dfpv = 'Calcolato'
352500981104      *
352600981104     c                   ENDSL
352700981130      *
352800010912      *     Calcolo il controvalore in MONETA CORRENTE
352900981130     c                   IF        *IN28 = *off
353000010918     C                   IF        V2Cvpv = §gedDBA  or  V2Cipv = 0
353100981130     c                   z-add     V2Cipv        £DCTipv
353200010906     c                   ELSE
353300010912     C                   movel     V2Cvpv        WVLT
353400010912     C                   z-add     V2Cipv        W0133
353500010912     c                   exsr      srCMB
353600010912     c                   z-add     w0133         £DCTipv
353700010906     c                   ENDIF
353800981130     c                   ENDIF
353900981104      *
354000981104      *     In ogni caso l'importo inserito non può essere maggiore del calcolato.
354100981130     c                   IF        £DCTipv > Wvalass  and  *IN28 = *off
354200981130     c                   eval      *in28 = *on
354300981104     c                   move      Wvalass       W003a
354400981104     c                   movel     Wvalass       W010a
354500981104     c     '0'           check     w010a         xx
354600020919     c                   if        xx = 0
354700981113     c                   eval      V1Cmsg  =  %subst(MSG(77):1:57)  +  '0,'
354800981113     c                                         +  W003a  +  '  '  +  Wvlt
354900981104     c                   else
355000040317     c                   eval      W010b = %subst(W010a:xx)
355100040317     c     ' '           checkr    w010b         xx
355200981113     c                   eval      V1Cmsg  =  %subst(MSG(77):1:57)
355300981104     c                                        +  %trim(W010b)  +  ','
355400981113     c                                         +  W003a  +  '  '  +  Wvlt
355500981104     c                   endif
355600981104     c                   ENDIF
355700981020      *
355800981104     c                   IF        *IN28 = *ON  or  *IN29 = *ON
355900981104     c                   eval      *in83 = *on
356000981104     C                   goto      ECTD02
356100981104     c                   ENDIF
356200020131      *
356300020124      *
356400020124      * Importo massimo risarcibile > limite per Pratiche in Franchigia
356500020125      * (solo una volta: se $ErrF6 = *off)
356600031211      * - Prima imposto l'importo "limite" da testare in base alla data
356700031211      *   spedizione
356800031211     c                   reset                   w§STDlpf
356900031211if  1c                   if            *in33     =  *on
357000031211     c                             and V1Cfra    =  'F'
357100031211     c                             and $ErrF6    =  *off
357200031211     c                             and DS_ARBdat >= §STDdlpf1
357300031211sel 2c                   select
357400031211w   2c                   when      §STDdlpf2 <= DS_ARBdat
357500031211     c                   eval      w§STDlpf   = §STDlpf2
357600031211w   2c                   when      §STDdlpf1 <= DS_ARBdat
357700031211     c                   eval      w§STDlpf   = §STDlpf1
357800031211e   2c                   endsl
357900031217     c                   if        V2Cipv    >  w§STDlpf
358000020124     c                   eval      $ErrF6 = *on
358100020124     c                   eval      *in83  = *on
358200020124     c                   eval      *in28  = *on
358300020125     c                   movel     msg(81)       V1Cmsg
358400020124     c                   goto      ECTD02
358500020124     c                   endif
358600031217e   1c                   endif
358700981030
358800981030      * IMPORTO TRATTENUTO
358900990128      *     Se indicato l'Importo richiesto deve essere minore o uguale.
359000990226      *     Se inserito dopo il pagamento del Progetto di Liquidazione errore
359100990301      *     Se è ok il confronto con l'Imp. richiesto sproteggo il beneficiario alternativo
359200990301      *     Il beneficiario alternativo deve essere un cliente di fatturazione
359300990128      *
359400080303     c*****              IF        *IN09 = *ON
359500990120      *   Controllo la Valuta
359600981130     c                   clear                   Wdclcpt
359700981130     C                   movel     V2Cvpt        WVLT
359800981130     C                   z-add     V2Cipt        W0133
359900981130     C                   exsr      ctrvlt
360000010912     C                   eval      *in76 = (*IN28 = *ON  or  *IN29 = *ON)
360100981130      *
360200981130     c                   IF        V2Cipt > *zeros  and  *IN76 = *off
360300990120     c                   move      V1Cksc        W0040
360400981130      *
360500010906      *     Calcolo il controvalore in MONETA CORRENTE
360600010918     c                   IF        V2Cvpt = §gedDBA  or  V2Cipt = 0
360700981201     c                   z-add     V2Cipt        £DCLipt
360800010906     c                   ELSE
360900010912     C                   movel     V2Cvpt        WVLT
361000010912     C                   z-add     V2Cipt        W0133
361100010912     c                   exsr      srCMB
361200010912     c                   z-add     w0133         £DCLipt
361300010906     c                   ENDIF
361400981030      *
361500990120     c                   SELECT
361600990301     c                   WHEN        £DCLipr > *zeros  and  £DCLipr < £DCLipt
361700981030     c                   movel     msg(76)       v1cmsg
361800981030     c                   eval      *in28 = *on
361900981030     c                   eval      *in76 = *on
362000990120      *
362100990301     c                   WHEN        W0040 = *zeros  or  W0040 = 8888
362200990120     c                   movel     msg(71)       v1cmsg
362300990120     c                   eval      *in28 = *on
362400990120     c                   eval      *in76 = *on
362500990128     c                   eval      *in39 = *off
362600990128      *
362700990301     c                   WHEN        DCLvlp > *zeros  and  V2Cipt <> WV2Cipt
362800990128     c                   eval      *in28 = *on
362900990128     c                   eval      *in76 = *on
363000990128     c                   movel     msg(75)       v1cmsg
363100990128      *
363200990301     c                   OTHER
363300990128     c                   eval      *in39 = *off
363400990301     c                   exsr      ctr_benalt
363500990301     c                   eval      *in76 = (*IN28 = *ON)
363600990120     c                   ENDSL
363700981030      *
363800981030     c                   ENDIF
363900080303     c*****              ENDIF
364000981021     C   76              goto      ECTD02
364100981021
364200981021      * DATA IMPORTO TRATTENUTO
364300981021      *     Dalla Conferma in poi
364400981102     c                   IF        *IN05 = *off
364500981021      *
364600981021     c                   SELECT
364700981021      *
364800981021     C                   WHEN      V2Cipt > *zeros  and  V2Cdpt = *zeros
364900981021     c                   movel     msg(20)       v1cmsg
365000990129     C                   eval      *in78 = *on
365100990129     C                   eval      *in28 = *on
365200990205     c                   clear                   WV2Cdpt
365300981021      *
365400981021     C                   WHEN      V2Cipt = *zeros  and  V2Cdpt > *zeros
365500981021     c                   movel     msg(21)       v1cmsg
365600990129     C                   eval      *in76 = *on
365700990129     C                   eval      *in28 = *on
365800981021      *
365900981021     C                   WHEN      V2Cdpt > *zeros
366000981022     C                   move      V2Cdpt        G02dat
366100981022     C                   clear                   G02err
366200981021     C                   CALL      'XSRDA8'
366300981021     C                   PARM                    WLBDAT
366400981022     c                   if        g02err = '1'
366500990129     C                   eval      *in78 = *on
366600990129     C                   eval      *in28 = *on
366700981021     c                   movel     msg(22)       v1cmsg
366800981022     c                   else
366900981022     c                   move      G02inv        WV2Cdpt
367000981030     C                   z-add     G02DAT        V2Cdpt
367100981021     c                   endif
367200990205      *
367300990205     C                   WHEN      V2Cdpt = *zeros
367400990205     c                   clear                   WV2Cdpt
367500981021      *
367600981021     c                   ENDSL
367700981021      *
367800981021     c                   ENDIF
367900981021     C   28              goto      ECTD02
368000990127
368100990127      * IMPORTO CONCORDATO
368200990127      *     Se gestito deve essere maggiore di zero e non maggiore del massimo consentito per L.T.
368300990127      *
368400990128     c                   IF        *IN07 = *ON
368500990127      *     Controllo la Valuta
368600990127     c                   clear                   Wdclcpr
368700990127     C                   movel     V2Cvpr        WVLT
368800990127     C                   z-add     V2Cipr        W0133
368900990127     C                   exsr      ctrvlt
369000990127
369100990129     C                   eval      *in84 = (*IN28 = *ON  or  *IN29 = *ON)
369200990127      *
369300990129     c                   IF        V2Cipr > *zeros  and  *IN84 = *off
369400010906      *     Calcolo il controvalore in MONETA CORRENTE
369500010918     c                   IF        V2Cvpr = §gedDBA  or  V2Cipr = 0
369600990127     c                   z-add     V2Cipr        £DCLipr
369700010906     c                   ELSE
369800010912     C                   movel     DCLvpr        WVLT
369900010912     C                   z-add     DCLipr        W0133
370000010912     c                   exsr      srCMB
370100010912     c                   z-add     w0133         £DCLipr
370200010906     c                   ENDIF
370300060208     c                   ENDIF
370400990127      *     Controlli
370500990127     c                   SELECT
370600990127     c                   WHEN      V2Cipr = *zeros
370700990129     C                   eval      *in84 = *on
370800990129     C                   eval      *in28 = *on
370900990127     c                   movel     msg(89)       v1cmsg
371000051104     c                   WHEN      £DCLipr >
371100051104     c                             (§STDllt + (§STDllt * §STDptl)/ 100)
371200990127     c                   movel     msg(90)       v1cmsg
371300990129     C                   eval      *in84 = *on
371400990129     C                   eval      *in28 = *on
371500990127     c                   ENDSL
371600990127      *
371700990127     c                   ENDIF
371800990127     C   88              goto      ECTD02
371900981021
372000981105      * RIVALSA
372100981105      *     Dalla Conferma se immessa/modificata rag. sociale emetto la videata dati anagrafici
372200981105     c                   IF        *IN05 = *off
372300981105      *
372400981105     c                   IF        V2Crag = *blanks
372500981105     c                   clear                   RW2Crag
372600981105     c                   clear                   RW2Cvia
372700981105     c                   clear                   RW2Ccap
372800981105     c                   clear                   RW2Cloc
372900981105     c                   clear                   RW2Cprv
373000981105     c                   clear                   RW2Cnaz
373100981105     c                   clear                   RW2Civa
373200981105     c                   clear                   RW2Ctel
373300981105     c                   clear                   RW2Cfax
373400981105     c                   ELSE
373500981105      *
373600981105     c                   IF        *IN05 = *off  and  V2Crag <> WV2Crag
373700981105     c                   movel     V2Crag        RW2Crag
373800981022     c                   movel     DS_W02criv    DS_FI03W02
373900981102     c                   movel     'R'           Wtrc
374000981022     c                   exsr      mancli
374100981022     c                   eval      *in29 = *ON
374200981022     c                   ENDIF
374300981105     c                   ENDIF
374400981105      *
374500981105     c                   ENDIF
374600981105     C   29              goto      ECTD02
374700981030
374800120712     *** * FLAG RIMBORSO CLIENTE
374900120712     *** *     Dalla conferma deve essere impostato
375000120712     ***c                   IF        *IN05 = *off  and  V2Cpgd = *blanks
375100120712     *** * tolto controllo del chi paga, impostato automaticamente "V" di vettore
375200120712     ***c                   eval      v2cpgd = 'V'
375300120712     ***c******             movel     msg(74)       V1Cmsg
375400120712     ***C******             eval      *in79 = *on
375500120712     ***C******             eval      *in28 = *on
375600120712     ***c                   ENDIF
375700120712     *** *       Se liquidazione transattiva devo impostare "V" = Vettore
375800120712     ***c                   IF        *IN05 = *off   and
375900120712     ***c                              V1Cfpr = 'T'  and  V2Cpgd <> 'V'
376000120712     *** * tolto controllo del chi paga, impostato automaticamente "V" di vettore
376100120712     ***c                   eval      v2cpgd = 'V'
376200120712     ***c*****              movel     msg(78)       V1Cmsg
376300120712     ***C*****              eval      *in79 = *on
376400120712     ***C*****              eval      *in28 = *on
376500120712     ***c                   ENDIF
376600981030     C   28              GOTO      ECTD02
376700981030
376800981030      * CONTROLLI DI CONGRUITA TRA IMPORTO TRATTENUTO, RIVALSA E FLAG RIMBORSO
376900981223      *     Dalla conferma e prima del pagamento P.L.
377000981223     c                   If        *IN05 = *OFF  and  *in35 = *OFF
377100990415     c                   EXSR      CTR_IPTriv
377200981126     c                   Endif
377300981030     C   28              GOTO      ECTD02
377400981020
377500981126      * TIPO GESTIONE
377600981014      *     Controllo in video 2 per considerare Importo da assicurare e KG danneggiati se variati
377700980717      *     Se errore torno in video 1
377800980717      *
377900080211     c                   exsr      impPROPR
378000080225     c                   If        dctgfc=46 and Wfprfrz='S'
378100080211     c                   eval      Wfprfrz = 'N'
378200080211     c                   endif
378300080211      *
378400980717     c                   if        V1Cfpr = 'T'
378500980717     c                   if        Wfpr    = 'P' and  Wfprfrz = 'S'
378600980717     c                   movel     msg(50)       v1cmsg
378700980717     c                   endif
378800980717     c                   if        Wfpr = 'P' and  Wfprfrz = 'N'
378900990205     c                             and  WV1Cfpr1 <> 'T'
379000990205     c                   movel     'T'           WV1Cfpr1
379100980717     c                   movel     msg(51)       v1cmsg
379200980717     c                   endif
379300980717     c                   endif
379400080211      *
379500080211     c                   If          V1Cfpr = 'P'
379600080303      * se ho importo trattenuto non modifico il tipo gestione
379700080303     c                   If        v2cipt > 0 and wfpr = 'T'
379800080303     c                   eval      wfprfrz = 'N'
379900080303     c                   eval      wfpr    = 'P'
380000080303     c                   endif
380100080211     c                   if        Wfpr = 'T' and  Wfprfrz = 'S'                *?*
380200080211     c                   movel     msg(114)      v1cmsg                         *?*
380300080211     c                   endif                                                  *?*
380400080211     c                   if        Wfpr = 'T' and  Wfprfrz = 'N'
380500080211     c                             and  WV1Cfpr1 <> 'P'
380600080211     c                   movel     'P'           WV1Cfpr1
380700080211     c                   movel     msg(115)      v1cmsg
380800080211     c                   endif
380900080211     c                   endif                                                  *?*
381000080211      *
381100980717      *
381200980717     c                   if        V1Cmsg <> *blanks
381300080310      * se sono in gestione alla filiale imposto il tipo gestione
381400080310     c                   If        dctgfc <> 46 and Wfprfrz='S'
381500080211     c                   eval      v1cfpr = wfpr
381600080211     c                   eval      wv1cfpr = wfpr
381700080211     c                   endif
381800990129     C                   eval      *in44 = *on
381900990129     C                   eval      *in28 = *on
382000980717     C                   goto      ECTD02
382100980717     c                   endif
382200990112
382300990119      *           M E S S A G G I
382400990119
382500990226      * In Conferma per Pratica se i colli non sono tutti  consegnati emetto messaggio informativo
382600080211      * eseguo il controllo anche in liquidazione transattiva in quanto non si fa più la fase
382700080211      * di contatto beneficiario
382800080211     c*********          IF        I00mod = 'G'  and  V1Cfpr = 'P'
382900080211     c                   IF        I00mod = 'G'
383000990226     c                             and  Wmsg85frz = *blanks
383100990226     c                   movel     'S'           Wmsg85frz
383200990226     c                   clear                   FIDN53ds
383300990226     c                   z-add     DCTaas        I53aas
383400990226     c                   z-add     DCTlnp        I53lnp
383500990226     c                   z-add     DCTnrs        I53nrs
383600990226     c                   z-add     DCTnsp        I53nsp
383700990226     c                   movel(P)  fidn53ds      Kpjbu
383800990226     c                   except    FNDCTfree
383900990226     c                   CALL      'FIDN53R'
384000990226     c                   PARM                    KPJBA
384100990226     c                   movel     kpjbu         fidn53ds
384200990226     c                   If        O53ERR <> *blanks
384300990112     c                   eval      *in28 = *ON
384400990112     c                   movel     msg(85)       V1Cmsg
384500990112     C                   goto      ECTD02
384600990226     c                   Endif
384700990226     c     knumca        chain     fndct000                           30
384800990112     c                   ENDIF
384900981021
385000980716     C     ECTD02        ENDSR
385100981016      **************************************************************************
385200990430      * CONTROLLO CODICE BENEFICIARIO ALTERNATIVO
385300981016      **************************************************************************
385400990226     C     CTR_benalt    BEGSR
385500990226      *
385600050428      * se codice diverso dal precedente pulisco il flag del msg forzabile
385700050428     c                   If        wv1cksc <> v1cksc
385800050428     c                   clear                   wmsg98frz
385900050428     c                   endif
386000050428
386100990226     c                   clear                   ACOflg
386200990226      *    Aggancio anagrafico
386300990408     c                   z-add     V1Cksc        I69kac
386400990311     c                   exsr      CTR_anagra
386500990301      *    Aggancio particolarità se ho l'importo trattenuto
386600990226     c                   If          *in30 = *off  and  ACOflg = *blanks
386700990301     c                                             and  V2Cipt > *zeros
386800990408     c                   z-add     V1Cksc        I69kcp
386900990311     c                   exsr      CTR_anagra
387000990226     c                   Endif
387100040524      *    Aggancio i luoghi per recuperare l'interlocutore se non è già inserito
387200040524     c                   If          Not *In30 and AcoFlg = *Blanks and
387300040524     c                               V1dNoc = *Blanks
387400040524     c                   z-add     V1cKsc        KCLI
387500040525     c     kSpe          Chain     Fnspe01l
387600040524     c                   If        %Found(Fnspe01l)
387700040524     c                   Movel     SpePer        V1dNoc
387800040524     c                   EndIf
387900040524     c                   EndIf
388000120507
388100120507      *?Se Porto Franco Import EEX il beneficiario alternativo deve essere il codice
388200120507      *?del partner (solo se non c'è l'importo trattenuto)
388300120507     c                   IF        §DCTport = 'F' and §DCTtisp = 'I' and
388400120507     c                             §DCTeuro = 'X' and v2cipt = 0
388500120507     c                   clear                   FIDN40DS
388600120507     c                   eval      I40dsp = ds_arbdat
388700120507     c                   eval      I40fil = dctlnp
388800120507     c                   call      'FIDN40R'
388900120507     c                   parm                    fidn40ds
389000120507      *?Ho trovato il codice del partner deve essere uguale al beneficiario alternativo
389100120507     c                   IF        O40cod <> *zeros and O40cod <> V1Cksc
389200120508     c                   eval      V1Cmsg = msg(119)
389300120507     c                   eval      *in47 = *on
389400120507     c                   eval      *in28 = *on
389500120507     c                   leavesr
389600120507     c                   ENDIF
389700120507     c                   ENDIF
389800990301      *
389900990301     c                   SELECT
390000990226      *       non trovato o annullato errore
390100990301     c                   WHEN        *in30 = *on  or  ACOflg = '*'
390200990301     c                   movel     msg(19)       v1cmsg
390300990226     C                   eval      *in47 = *on
390400990226     C                   eval      *in28 = *on
390500120508
390600120508      *?Se il beneficiario alternativo è il partner non devo fare controlli sul cod.fatturazione
390700120508     c                   WHEN      §DCTport = 'F' and §DCTtisp = 'I' and
390800120508     c                             §DCTeuro = 'X' and v2cipt = 0
390900120508      *?devo verificare la presenza delle coordinate bancarie
391000120508      /free
391100120508       //?Pagamento DANNI --> DN
391200120508       //?Recupero le coordinate bancarie
391300120508         rqsOpCode = 'SEARCH';
391400120508         clear rpyEsito;
391500120508         clear TrulIbanI0;
391600120508         clear TrulIbanO0;
391700120508         TrulIbanI0.KSC = v1cksc;
391800120508         TrulIbanI0.PAG = 'DN';
391900120508         rqsFormatName = 'TRULIBANI0';
392000120508         rpyFormatName = 'TRULIBANO0';
392100120508         rqsDataSize   = %size(TrulIbanI0);
392200120508         rpyDataSize   = %size(TrulIbanO0);
392300120508         TrulIbanR (rqsOpCode:rpyEsito:
392400120508                    rqsFormatName:TrulIbanI0:rqsDataSize:
392500120508                    rpyFormatname:TrulIbanO0:rpyDataSize);
392600120508         IF  rpyEsito <> 0;
392700120508           IF  Wmsg98frz = ' ';
392800120508             V1Cmsg = msg(118);
392900120508             *in47 = *on;
393000120508             *in28 = *on;
393100120508             wmsg98frz = 'S';
393200120508             leavesr;
393300120508           ENDIF;
393400120508         ENDIF;
393500120508      /end-free
393600120508     c                   eval      *in29 = (V1Cksc <> WV1Cksc)
393700120508     c                   movel     ACOrag        V1Dksc
393800990301      *
393900990301     c                   WHEN        V2Cipt = *zeros
394000990303     c                   eval      *in29 = (V1Cksc <> WV1Cksc)
394100990301     c                   movel     ACOrag        V1Dksc
394200990301      *
394300990301     c                   OTHER
394400050428      *    Controllo che sia un codice di fatturazione
394500050428     c                   If        *in30 = *on  or  ACOflg = '*'
394600050428     c                                          or  V1Cksc <> CLPscf
394700050428     c                   movel     msg(112)      v1cmsg
394800050428     C                   eval      *in47 = *on
394900050428     C                   eval      *in28 = *on
395000050428     c                   else
395100990301      *    Controllo che sia il codice di fatturazione del beneficiario
395200050428
395300990301     c                   If          V1Cptr = 'M'
395400990408     c                   z-add     VTCksm        I69kcp
395500990301     c                   Else
395600990408     c                   z-add     VTCksd        I69kcp
395700990301     c                   Endif
395800990408     c                   movel     I69kcp        W0040
395900990301      *
396000990301     c                   If        W0040 <> 9999  AND  W0040 <> 8888
396100990311     c                   exsr      CTR_anagra
396200990226     c                   If        *in30 = *on  or  ACOflg = '*'
396300990301     c                                          or  V1Cksc <> CLPscf
396400050428     c                   if        Wmsg98frz = ' '
396500990301     c                   movel     msg(98)       v1cmsg
396600990301     C                   eval      *in47 = *on
396700990301     C                   eval      *in28 = *on
396800050428     c                   eval      Wmsg98frz = 'S'
396900050428     c                   endif
397000050428     c                   endif
397100050428
397200990301     c                   Else
397300990303     c                   eval      *in29 = (V1Cksc <> WV1Cksc)
397400990226     c                   movel     ACOrag        V1Dksc
397500990226     c                   Endif
397600990301     c                   Endif
397700990226      *
397800990301     c                   ENDSL
397900990301      *
398000990226     C                   ENDSR
398100990226      **************************************************************************
398200010906      * CALCOLO VALORE ASSICURATO - SEMPRE IN MONETA CORRENTE -
398300990226      **************************************************************************
398400990226     C     CALCVAS       BEGSR
398500990226
398600981016      * Legenda Tipo Valore: 1 = COLLI; 2 = SPEDIZIONE; 3 = PESO KG
398700981016     c                   clear                   Wvalass
398800981020     C                   clear                   Wvlt
398900981016
399000010906      * Se il valore è esposto in bolla il tipo valore è sempre a
399100010906      *  spedizione, imposto il valore in moneta corrente
399200990319     c                   IF        VTCias > *zeros
399300981130     c                   z-add     £ARBias       WVLM
399400981020     c                   movel     '2'           WFVM
399500981016     c                   ELSE
399600981127      * Se non ho mandato calcolo con il limite risarcibile a Kg stabilito per legge o CMR
399700990407     c                   IF        Wmand <> 'S'
399800981127     c                   z-add     Wlimris       WVLM
399900981016     c                   movel     '3'           WFVM
400000981016     c                   ENDIF
400100981016     c                   ENDIF
400200981020
400300981113      *  Se la quantità in funzione del Tipo Valore non è impostata considero l'importo
400400981126     c                   SELECT
400500981126      *
400600981126     c                   WHEN      WFVM  = '1'                                  ** COLLI **
400700981126     c                   If        V2Cncn > *zeros
400800010904     c                   eval      Wvalass =  V2Cncn * Wvlm
400900981126     c                   Else
401000010904     c                   eval      Wvalass =  VTCncl * Wvlm
401100981126     c                   Endif
401200981020      *
401300981126     c                   WHEN      WFVM  = '3'                                  ** PESO **
401400981126     c                   If        V2Cpkd > *zeros
401500010904     c                   eval      Wvalass =  V2Cpkd * Wvlm
401600981126     c                   Else
401700010904     c                   eval      Wvalass =  VTCpkf * Wvlm
401800981126     c                   Endif
401900981020      *
402000981126     c                   OTHER
402100010904     c                   eval      Wvalass =  Wvlm
402200981126     c                   ENDSL
402300010904      *
402400010904      * Impostazione divisa (corrente)   &
402500010904      * Arrotondamento importo al decimale previsto
402600010904      *
402700010904     C                   IF        WValAss <> 0
402800010905     C                   IF        WVlt    =  *blanks
402900010910     C                   movel     §gedDBA       WVlt
403000010905     C                   ENDIF
403100010904     C                   z-add     WValAss       W0133
403200010904     C                   exsr      srCMB
403300010904     C                   z-add     W0133         WValAss
403400010904     C                   ELSE
403500010904     C                   clear                   WVlt
403600010904     C                   ENDIF
403700981020
403800981016     C                   ENDSR
403900981015      *****************************************************************
404000981015      * CONTROLLO NUMERO EVENTO
404100981015      *****************************************************************
404200981015     C     CTRNEV        BEGSR
404300981015
404400990129     c                   eval      *in29 = *off
404500990129     c                   eval      *in30 = *off
404600981019     C                   clear                   Wv1cnev
404700981104     c                   clear                   V1Dnev
404800981104     C                   clear                   DTADEVE
404900981104     C                   clear                   V1Ctae
405000981104     C                   clear                   V1Dtae
405100981104      *
405200990108      *    Controllo anno
405300990108      *
405400990108      *   sistemo anno di due cifre
405500990108     C                   if        V1Caae < 100  and  V1Caae > 60
405600990108     C                             and  V1Caae > *zeros
405700990108     C                   ADD       1900          V1Caae
405800990108     C                   endif
405900990108     C                   if        V1Caae < 100  and  V1Caae <= 60
406000990108     C                             and  V1Caae > *zeros
406100990108     C                   ADD       2000          V1Caae
406200990108     C                   endif
406300990108      *
406400990108      *    Sostituisco blanks con zero nel numero
406500981104     c     ' ':'0'       xlate     V1Cnev        W007A
406600981104     c                   movel     W007A         V1Cnev
406700981019
406800981015      * Ricerca
406900981015     c                   clear                   fidn07ds
407000981104     C     '?'           SCAN      V1Cnev                                 31
407100981015      *
407200981015     C                   IF        *IN31 = *on
407300981103     C                   eval      *in42 = *on
407400981015     C                   movel     'R'           I07mod
407500981015     C                   CALL      'FIDN07R'
407600981015     c                   parm                    kpjba
407700981015     c                   parm                    fidn07ds
407800981015      *
407900981015     C                   IF        O07err <> 'E'
408000981015     C                   eval      *in30 = *on
408100981015     c                   z-add     O07aae        V1Caae
408200981015     c                   movel     O07nev        V1Cnev
408300981015     C                   ENDIF
408400981104      *
408500981015     C                   ENDIF
408600981104
408700981104      *    Controllo se immesso un numero valido
408800981104     C                   testn                   V1Cnev               31
408900981015     C                   IF        *IN31 = *on
409000981104     C                   move      V1Cnev        W001A
409100981015     c                   eval      *in31 = (W001A >= '0')
409200981015     C                   ENDIF
409300981015      *
409400981015      * Controlli di validità'
409500981015     c                   SELECT
409600981015
409700981015      *    Errore ricerca
409800981015     c                   WHEN      O07err = 'E'
409900981015     C                   eval      *in29 = *on
410000981015     c                   clear                   V1Cnev
410100981015
410200981019      *    Obbligatorio dall'apertura in poi    ** Controllo eliminato **
410300981015     c                   WHEN      V1Cnev = *zeros  or  V1Cnev = *blanks
410400981015
410500981015      *    Numero evento non valido
410600981015     C                   WHEN      *IN31 = *off
410700981015     C                   movel     MSG(5)        V1CMSG
410800990129     C                   eval      *in42 = *on
410900990129     C                   eval      *in28 = *on
411000981015
411100981015     c                   OTHER
411200981015
411300981015      *    Controllo esistenza evento in testata eventi
411400981015     C                   movel     V1CNEV        Wv1cnev
411500051121     C     KDET          chain(N)  FNDET000                           31
411600981015      *
411700981015     C                   IF        *IN31 = *on  OR  DETATB <> *blanks
411800981015     C                   movel     MSG(6)        V1CMSG
411900990129     C                   eval      *in42 = *on
412000990129     C                   eval      *in28 = *on
412100981015     C                   ELSE
412200981015      *
412300981015      *    Dal file descrizioni ricavo la decodifica evento
412400981015     C                   movel     'E'           KTPD
412500981015     C                   movel     V1CAAE        W011A
412600981015     C                   move      V1CNEV        W011A
412700981015     C                   movel(P)  W011A         KNKT
412800981015     C                   clear                   Khim
412900981015     C                   clear                   Kdim
413000981015     C                   clear                   KSTD
413100981015     C                   clear                   KNKS
413200981015     C                   movel     'D'           KTRC
413300981015     C     KDCS          chain(N)  FNDCS01L                           31
413400981015      *
413500020129      * se emetto il Flag Franchigia NON emetto la Descrizione Evento
413600020129     c                   if        not *in33
413700981015     C  N31              movel(P)  DCSNOT        V1DNEV
413800020129     c                   endif
413900981020      *
414000981020      *  Ricavo la decodifca Anomalia Evento
414100981104     C                   movel(P)  DETtad        WKEY1
414200981020     C                   movel     'E'           WTADtipo
414300981020     C                   exsr      CHTAD
414400981015      *
414500981015      *  Verifico se l'evento è previsto per questo PO, se non previsto emetto msg forzabile
414600981015     c                   IF        CODEV <> CODEVS
414700981015     C                   move      CODEV         CODEVS
414800990111      *   Se non trovo il dettaglio tutti i P.O. sono abilitati
414900981015     c     KDET          setll     fnded000                               31
415000981015      *
415100981015     c                   IF        *IN31 = *ON
415200990111      *   Controllo se il P.O. gestione è abilitato
415300981015     C                   z-add     I00fgs        Kfgs
415400981015     C     KDED          chain     FNDED000                           31
415500981015      *
415600981015     c                   IF        *IN31 = *ON
415700990111      *   Cerco eventuale 1o livello
415800981015     c                   clear                   FNLV55DS
415900981015     c                   movel     'P'           D55tpt
416000990111     c                   movel     I00fgs        D55lin
416100981015     c                   eval      D55drf = (detaae * 10000) + detmge
416200981015     c                   call      'FNLV55R'
416300981015     c                   parm                    FNLV55DS
416400981015      *
416500990111      *   Se sono secondo livello aggancio con il primo livello
416600981015    4c                   IF        D55err = *blanks  AND  D55tfp <> I00fgs
416700981015     C                   z-add     D55tfp        Kfgs
416800981015     C     KDED          chain     FNDED000                           31
416900990111     c                   ELSE
417000990111     c                   eval      *in31 = *ON
417100990111     C                   ENDIF
417200981015      *
4173009810156    c                   IF        *in31 = *on  OR  DEDatb <> *blanks
417400981015     C                   movel     MSG(7)        V1CMSG
417500990129     C                   eval      *in42 = *on
417600990129     C                   eval      *in28 = *on
417700981015     C                   ENDIF
417800981015      *
417900981015     C                   ENDIF
418000981015     C                   ENDIF
418100981015     C                   ENDIF
418200981015      *
418300981015     C                   ENDIF
418400981015     C                   ENDSL
418500981020      *
418600981020     c   30              eval      *in29 = *ON
418700981020
418800981015     C                   ENDSR
418900980623      *****************************************************************
419000980623      * CONTROLLO DIVISA
419100980623      *****************************************************************
419200980623     C     CTRVLT        BEGSR
419300980623      *
419400010904     C                   MOVEL     'CV'          KCOD
419500980623      * RICERCA
419600980623     C     '?'           SCAN      WVLT                                   30
419700980623      *
419800980623     C                   if        *IN30 = *ON
419900980623      *
420000980623     C                   CALL      'X§TABER'
420100980623     C                   PARM                    CODUT
420200980623     C                   PARM                    KCOD
420300980623     C                   PARM                    §KEY
420400980623     C                   PARM                    §DES
420500980623      *
420600980623     C                   MOVEL     §KEY          WVLT
420700980623     C                   eval      *in29 = *ON
420800980623     C                   GOTO      ENDVLT
420900980623     C                   endif
421000980623      *
421100980623      * CONTROLLO DIVISA
421200010904     C                   MOVEL(P)  WVLT          KKEY
421300010904     C     KTAB          CHAIN     TABEL00f                           30
421400990408      * ERRORE
421500010904     C                   if        *IN30 = *ON    OR
421600010904     C                              ( *IN30 = *OFF  and  TBLFLG <> *blanks)
421700980623     C                   MOVEL     MSG(40)       V1CMSG
421800980623     C                   eval      *IN28 = *ON
421900980623     C                   GOTO      ENDVLT
422000980623     C                   endif
422100980623      *
422200980623     C                   MOVEL     TBLUNI        DSCV
422300980623      *
422400980623      * VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
422500980623     C                   IF        §CVFDC <> 'S'
422600980623     C                   MOVE      W0133         W0033
422700980623     C                   if        W0033 <> 0
422800980623     C                   MOVEL     MSG(41)       V1CMSG
422900980623     C                   eval      *IN28 = *ON
423000980623     C                   GOTO      ENDVLT
423100980623     C                   endif
423200010904     C                   else
423300010910      * controllo numero massimo di decimali impostabili per la valuta
423400010910     C     W0133         casNE     0             srCTRDEC
423500010904     C                   ENDCS
423600010910     C                   IF        *in28
423700010910     C                   MOVEL     MSG(111)      V1CMSG
423800010910     c                   move      §cvDEC        w002a
423900010914     c                   eval      %subst(V1CMSG:29:2) = w002a
424000010910     C                   GOTO      ENDVLT
424100010910     C                   ENDIF
424200980623     C                   ENDIF
424300980623      *
424400981130     C     ENDVLT        ENDSR
424500980623      *****************************************************************
424600980623      *  Ricerca alfabetica
424700980623      *****************************************************************
424800980525     c     ricalf        begsr
424900980623      *
425000981022     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
425100981022     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
425200050523     C                   Z-ADD     dutKCI        PAXccc                         Capoconto Clienti
425300981022     c                   movel(P)  WDksc         PAXdmt
425400000907     C                   z-add     1             PAXnum
425500981022      *
425600000907     C                   CALL      'XALFA3BR'
425700980525     C                   PARM                    PAXDUT
425800980525     C                   PARM                    CODUT
425900980525     C                   PARM                    PAXDMT
426000980525     C                   PARM                    PAXCCC
426100981022     C                   PARM                    PAXSTA
426200981102     C                   PARM      *blanks       PAXFLR
426300981102     C                   PARM      *blanks       PAXDIT
426400000907     C                   PARM                    PAXNUM            2 0
426500000907     C                   PARM                    PAXKCM           80
426600000907     C                   PARM                    PAXKSM          140
426700000907     C                   PARM                    PAXKDM           60
426800980623      *
426900980623      * NON TROVATO O PREMUTO CMD12
427000980525    2C     paxsta        IFGT      -1
427100000907     C                   MOVEL     PAXKSM        V1Cksc
427200980525    2C                   ENDIF
427300980623      *
427400980525     c                   endsr
427500980720      *****************************************************************
427600980720      *  GESTIONE VIDEATA DETTAGLIO SEGNACOLLI
427700980720      *****************************************************************
427800971020     C     GESNSC        BEGSR
427900980727
428000981020      *  Blocco Tipo scelta se ho caricato segnacolli da FNDCD
428100990317     c                   EVAL      *IN70 = (wcardcd = 'S')
428200971023     c                   z-add     1             v1cpos
428300980720      *
428400990323      *  Verifico se devo controllare Anomalie aperte
428500990323     c                   SELECT
428600990323     c                   WHEN      Simfel = *zeros
428700990323     c                   WHEN      §TADcidd <> 'S'
428800990323     c                   WHEN      WV1Cnev > *zeros  and  E§TADcidd <> 'S'
428900990323     c                   OTHER
429000990323     c                   eval      Wctridd = 'S'
429100990323     c                   ENDSL
429200990323      *
429300971020     C     EMS01         TAG
429400980720      *
429500971023     c     v1cpos        ifgt      0
429600971023     c                   z-add     v1cpos        v1cnrr
429700971023     c                   else
429800971023     c                   z-add     ultnrr1       v1cnrr
429900971023     c                   end
430000980720      *
430100981021     C                   WRITE     FI03T04
430200981021     C                   WRITE     FI03Z04
430300981021     c  N20              write     FI03D04
430400981021     C                   EXFMT     FI03C04
430500980720      *
430600990129     c                   eval      *in29 = *off
430700990129     c                   eval      *in28 = *off
430800971020     C                   CLEAR                   V1CMSG
430900990114      *
431000990114     C                   EXSR      CTRS01
431100990114      *
431200990114     c   KL              movel     *blanks       vcctsl
431300990114     C   KL              goto      ENSC
431400980720      *
431500980720     c                   IF        *IN28 = *ON  or  *IN29 = *ON
431600980720     C                             or  *INKF = *OFF
431700980720     C                   goto      EMS01
431800980720     c                   ENDIF
431900980720      *
432000990408     C                   EXSR      AGGIORNA
432100971022     C   28              GOTO      EMS01
432200070402
432300070402      * se apertura C.A.
432400070402      * filiale gestione con procedura telefonate GEO attiva e LNA della spedizione
432500070403      * in £6, se bolla non in distinta e non consegna, proponga la messa in sospensione
432600070403      * della bolla
432700070402     c                   if        i00mod = 'A' and wfilgeo = *on and
432800070403     c                             wmialna = *on and
432900070403     c                             arbdcm = *zeros and arbndc = *zeros
433000070402     c                   clear                   dslr02
433100070402     c                   eval      dr2aas = i00aas
433200070402     c                   eval      dr2lnp = i00lnp
433300070402     c                   eval      dr2nrs = i00nrs
433400070402     c                   eval      dr2nsp = i00nsp
433500070402     c                   eval      dr2fgs = i00fgs
433600070402     c                   eval      dr2ric = '1'
433700070402     c                   eval      dr2bs = 'B'
433800070402     c                   eval      kpjbu = dslr02
433900070402     c                   call      'FNLR39R'
434000070402     c                   parm                    kpjba
434100070402     c                   eval      dslr02 = kpjbu
434200070402     c                   endif
434300980720      *
434400980720     c                   z-add     V2Cncn        WV2Cncn                        Memo Colli dannegg.
434500980720      *
434600971020     C     ENSC          ENDSR
434700980623      *****************************************************************
434800981006      *  RIEMPIMENTO SUBFILE SEGNACOLLI SEDE o NON PRESENTE DETTAGLIO
434900980623      *****************************************************************
435000981006     C     RIES01S       BEGSR
435100980623      *
435200990129     c                   eval      *in21 = *off
435300990129     c                   eval      *in23 = *off
435400990129      *
435500981021     C                   WRITE     FI03C04
435600990129      *
435700990129     C                   eval      *in20 = *on
435800990129     C                   eval      *in21 = *on
435900980623      *
436000980720     C                   move      *ZEROS        NRR1
436100980623     c                   movel     *blanks       vcctsl
436200980720     C                   clear                   V1SSEL
436300981006     c                   movel     'S'           wcardcd
436400981006     C                   eval      *IN71 = *OFF
436500981229     c                   eval      *IN74 = *ON
436600980626
436700990209     c     kca           setll     fndcd000
436800990209     c     kca           reade(N)  fndcd000                               31
436900981006      *
437000980626     C     *IN31         DOWEQ     *OFF
437100980626      *
437200981029     C                   IF        DCDatb = *blanks  and  DCDdch = *zeros
437300981229     c                   move      *IN74         V1Hi74
437400981029     C                   move      DCDfls        V1Sfls
437500981029     C                   move      DCDnsc        V1Snsc
437600981029     C                   move      DCDcan        V1Scan
437700981113     C                   move      DCDcan        V1Hcan
437800990309     C                   move      DCDlna        V1Hlna
437900980626      * Decodifico cod. anomalia
438000981029     C                   movel     '3E'          KCOD
438100981029     C                   movel(P)  DCDcan        KKEY
438200990408     C     KTAB          CHAIN     TABEL00F                           31
438300990408     C  N31              movel     TBLUNI        V1DCAN
438400981029     C   31              clear                   V1DCAN
438500980720      *
438600981029     C                   add       1             NRR1
438700981021     C                   WRITE     FI03S04
438800981029     C                   ENDIF
438900980626      *
439000990209     c     kca           reade(N)  fndcd000                               31
439100980626     C                   ENDDO
439200981006      *
439300981006      * Carico segnacolli contenuti in altre C.A.
439400981229     c                   eval      *IN74 = *OFF
439500981006      *
439600981006     c                   DO        100           II
439700981006     c                   IF        nca(II) = *zeros
439800981006     c                   z-add     101           II
439900981006     c                   ELSE
440000981029      *
440100981229     c                   move      *IN74         V1Hi74
440200981117     C                   z-add     nsc(II)       V1Snsc
440300981117     c                   z-add     aac(II)       V1Saac
440400981117     c                   z-add     fil(II)       V1Sfil
440500981006     c                   z-add     nca(II)       V1Snca
440600990309     c                   z-add     lna(II)       V1Hlna
440700981006     C                   clear                   V1Scan
440800981113     C                   clear                   V1Hcan
440900981113     C                   clear                   V1Dcan
441000981006     C                   add       1             NRR1
441100981021     C                   write     FI03S04
441200981006     C                   ENDIF
441300981006     c                   ENDDO
441400980720      *
441500980626     C                   eval      *in23 = *on
441600980720
441700980619      * memorizzo il nrr dell'ultimo record caricato
441800971023     c                   z-add     nrr1          ultnrr1
441900980626     c                   eval      *IN20 = ( nrr1 <> *zeros)
442000980623      *
442100971020     C                   ENDSR
442200981006      *****************************************************************
442300981006      *  RIEMPIMENTO SUBFILE SEGNACOLLI
442400981006      *****************************************************************
442500981006     C     RIES01        BEGSR
442600981006      *
442700981117      *  Pulizia subfile
442800990129     c                   eval      *in21 = *off
442900990129     c                   eval      *in23 = *off
443000990129      *
443100981021     C                   WRITE     FI03C04
443200990129      *
443300990129     C                   eval      *in20 = *on
443400990129     C                   eval      *in21 = *on
443500981006      *
443600981117     C                   clear                   NRR1
443700981117     c                   clear                   VCCtsl
443800981117     C                   clear                   V1Ssel
443900981117     c                   movel     'N'           Wcardcd
444000981006     C                   eval      *in71 = *off
444100981006      *
444200981006     C                   if        i00tpb = 'A'
444300981006     C     KBOL          SETLL     FNART000
444400981006     C     KBOL          READE     FNART000                               31
444500981006     C                   else
444600981006     C     KBOL          SETLL     FNBLT000
444700981006     C     KBOL          READE     FNBLT000                               31
444800981006     C                   endif
444900981006
445000981006     C     *IN31         DOWEQ     *OFF
445100981117     C                   IF        ARTatb = *BLANKS
445200981117      *
445300981117      *   proteggo scelta se il segnacollo è già su altra c.a.
445400981006     c                   z-add     1             II
445500981229     c     ARTnsc        lookup    nsc(II)                                73
445600981229     c                   IF        *in73 = *ON
445700981117     c                   z-add     aac(II)       v1saac
445800981117     c                   z-add     fil(II)       v1sfil
445900981006     c                   z-add     nca(II)       v1snca
446000990309     c                   z-add     lna(II)       v1Hlna
446100981117     c                   ELSE
446200981117     c                   clear                   v1saac
446300981117     c                   clear                   v1sfil
446400981117     c                   clear                   v1snca
446500981117     c                   ENDIF
446600981117      *
446700981117      * metto campi in RI e imposto anomalia se segnacollo presente su questa C.A.
446800981229     c                   eval      *in74 = *off
446900981113     C                   clear                   V1Scan
447000981113     C                   clear                   V1Dcan
447100981113      *
447200981006     c                   IF        I00MOD <> 'A'
447300981016     c     kdcd2b        chain(N)  FNDCD002                           30
447400981117     c                   IF        *IN30 = *OFF  and
447500981117     c                             DCDatb = *BLANKS  and  DCDdch = *zeros
447600981229     c                   eval      *in74 = *ON
447700981117     C                   movel     DCDcan        V1Scan
447800981006     c                   ENDIF
447900981117     c                   ENDIF
448000981006      *
448100981117     c                   clear                   V1Ssel
448200981229     c                   move      *IN74         v1hi74
448300981029     C                   move      ARTfls        V1SFLS
448400981029     C                   move      ARTnsc        V1SNSC
448500990309     c                   z-add     ARTlna        V1Hlna
448600981117      * Imposto data consegna
448700981117     c                   clear                   v1sdcm
448800981117     c                   IF        ARTdcm > *zeros
448900981117     c     *iso          move      ARTdcm        Wdataeur
449000981117     c     *eur          movel     Wdataeur      v1sdcm
449100981117     c                   ENDIF
449200981117      * Se non impostato da C.A. imposto anomalia da segnacollo
449300981117     c                   IF        V1Scan = *blanks
449400981117     C                   move      ARTcan        V1SCAN
449500981117     c                   ENDIF
449600981117     C                   move      V1Scan        V1Hcan
449700981006      * Decodifico cod. anomalia
449800981006     C                   MOVEL     '3E'          KCOD
449900981113     C                   MOVEL(P)  V1Scan        KKEY
450000990408     C     KTAB          CHAIN     TABEL00F                           31
450100990408     C  N31              MOVEL     TBLUNI        V1Dcan
450200981020      *
450300981006     C                   ADD       1             NRR1
450400981021     C                   WRITE     FI03S04
450500981006     C                   endif
450600981006      *
450700981006     C                   if        i00tpb = 'A'
450800981006     C     KBOL          READE     FNART000                               31
450900981006     C                   else
451000981006     C     KBOL          READE     FNBLT000                               31
451100981006     C                   endif
451200981006      *
451300981006     C                   ENDDO
451400981006      *
451500981006     C                   eval      *in23 = *on
451600981006
451700981006      * memorizzo il nrr dell'ultimo record caricato
451800981006     c                   z-add     nrr1          ultnrr1
451900981006     c                   eval      *IN20 = ( nrr1 <> *zeros)
452000981006      *
452100981006     C                   ENDSR
452200980626      *****************************************************************
452300980626      * CONTROLLI  SUBFILE
452400980626      *****************************************************************
452500971021     C     CTRS01        BEGSR
452600980727
452700990317     c     71            DO        75            XX
452800980727     c                   eval      *IN(XX) = *OFF
452900980727     c                   ENDDO
453000980626      *
453100980626     C                   eval      *in22 = *on
453200990129     c                   eval      *in71 = *off
453300990129     c                   eval      *in72 = *off
453400980626     C                   CLEAR                   wclinc
453500980518     c                   z-add     1             nrr1
453600980720
453700981125     C     nrr1          chain     FI03S04                            3232
453800980720
453900981029    1C     *IN32         DOWEQ     *OFF
454000990114      *
454100980720      * reimposto indicatore di RI per segnacollo di questa C.A.
454200981229     c                   move      v1hi74        *in74
454300981006      * Reimposto indicatore di blocco scelta x segnacollo di altra C.A. Se non ho caricato da FNDCD
454400981006     c                   IF        wcardcd <> 'S'
454500981229     c     v1snsc        lookup    nsc                                    73
454600981006     c                   ENDIF
454700990114      * Clearizzo scelta per F12
454800990114     c   KL              clear                   V1Ssel
454900990114     c   KL              goto      ECTS01
455000980720      *
455100981229    2c     *in73         ifeq      *off
455200980623      * reimposto indicatore di RI
455300981229     c                   move      v1hi74        *in74
455400980720
455500990209      * errore se CA in attesa di ricalcolo ed ho una selezione
455600990209     c                   IF        (WDCTcar = 'R'  or  WDCTcar = 'T')  and
455700990209     c                             (VCCtsl = 'T'  or  V1Ssel <> *blanks)
455800990209     C                   eval      *in28 = *on
455900990209     C                   movel     MSG(91)       V1CMSG
456000990209     c                   ENDIF
456100990209     C   28              goto      ECTS01
456200990209
456300990209      * errore se "TIPO SCELTA SEGNACOLLI"  = "T"  ad I/E <> blanks
456400990209     c                   IF        VCCtsl = 'T'  and V1Ssel <> *blanks
456500990129     C                   eval      *in72 = *on
456600990129     C                   eval      *in28 = *on
456700980720     C                   movel     MSG(34)       V1CMSG
456800980720     c                   ENDIF
456900981211     C   28              goto      ECTS01
457000980720
457100980720      * errore se tipo selezione 'E' e segnacollo non selezionato per la C.A.
457200981229     c                   IF        V1Ssel = 'E'  and  *in74 = *off
457300990129     C                   eval      *in72 = *on
457400990129     C                   eval      *in28 = *on
457500980720     C                   movel     MSG(8)        V1CMSG
457600980720     c                   ENDIF
457700981211     C   28              goto      ECTS01
457800981020
457900981020      * errore se Apertura per segnacollo e segnacollo ricevuto non inserito
458000981020     c                   IF        I00mod = 'A'  and  V1Snsc = I00nsc
458100981020     c                             and (V1Ssel <> 'I'  and  VCCtsl <> 'T'
458200981229     c                                  and  *IN74 = *off)
458300990129     C                   eval      *in72 = *on
458400990129     C                   eval      *in28 = *on
458500981020     C                   movel     MSG(68)       V1CMSG
458600981020     c                   ENDIF
458700981211     C   28              goto      ECTS01
458800980710
458900981211      * Se sono in Apertura e ho caricato i segnacolli da FNDCD verifico se il collo deve essere
459000981211      * consegnato o non consegnato in base alla tabella TAD
459100981211     c                   IF        wcardcd <> 'S'  and  I00mod = 'A'
459200981211     c                             and  (VCCtsl = 'T'  or  V1Ssel = 'I')
459300981006      *
459400981117     c                   IF        §TADtico = 'C'  AND  V1Sdcm = *zeros
459500980710     C                   movel     MSG(32)       V1CMSG
459600990129     C                   eval      *in72 = *on
459700990129     C                   eval      *in28 = *on
459800980720     c                   ENDIF
459900980710      *
460000981117     c                   IF        §TADtico = 'N'  and  V1Sdcm > *zeros
460100980710     C                   movel     MSG(52)       V1CMSG
460200990129     C                   eval      *in72 = *on
460300990129     C                   eval      *in28 = *on
460400980720     c                   ENDIF
460500980710      *
460600981006     c                   ENDIF
460700981211     C   28              GOTO      ECTS01
460800180130      *
460900180208      * - NON SOLO in fase di Apertura (ma anche di Manutenzione e Gestione)
461000180208      *   occorre verificare che i segnacolli qui selezionati (quindi liberi
461100180208      *   fino al caricamento del subfile S04 - vedi *in73) NON siano appena
461200180208      *   stati imputati ad un'altra C.A.
461300180208     c                   IF        VCCtsl = 'T'  or  V1Ssel = 'I'
461400180130     c                   exsr      sr_CtrlSgnCl
461500180130     c                   if        *in28
461600180130     c                   goto      EctS01
461700180130     c                   endif
461800180130     c                   ENDIF
461900990311
462000990323      * Il segnacollo non deve avere IDD aperto
462100990323     c                   IF        Wctridd = 'S'  and
462200990323     c                             (V1Ssel = 'I'  or  VCCtsl = 'T')
462300990311     c                   exsr      ctr_idd
462400990311     c                   If        Widdaperto = 'S'
462500990311     C                   eval       *in72 = *on
462600990311     C                   eval       *in28 = *on
462700990311     C                   movel     MSG(105)      V1CMSG
462800990311     c                   EndIf
462900990311     c                   ENDIF
463000990311     C   28              goto      ECTS01
463100980710
463200980720      * GESTIONE ANOMALIA SEGNACOLLO
463300980720      *   Ricerca in tabella
463400980518     C     '?'           SCAN      V1SCAN                                 31
463500980720      *
463600980720    3C                   if        *IN31 = *ON
463700990216     c                   eval       *in71 = *on
463800971021     C                   MOVEL     CODUT         §KUT
463900971021     C                   MOVEL     '3E'          §COD
464000971021     C                   MOVEL     *BLANKS       §KEY
464100971021     C                   CALL      'X§TABER'
464200980626     C                   PARM                    §KUT
464300980626     C                   PARM                    §COD
464400980623     C                   PARM                    §KEY
464500980623     C                   PARM                    §DES
464600971021     C                   MOVEL     §KEY          V1SCAN
464700980720      *
464800980720     C                   if        V1SCAN <> *BLANKS
464900971021     C                   MOVEL     §DES          V1DCAN
465000980720     C                   endif
465100980720      *
465200980720     C                   eval      *in29 = *ON
465300971021     C                   GOTO      ECTS01
465400980720     C                   ENDIF
465500980720      *
465600980720     C                   if        V1SCAN <> *BLANKS
465700971021     C                   MOVEL     '3E'          KCOD
465800971021     C                   MOVEL(P)  V1SCAN        KKEY
465900990408     C     KTAB          CHAIN     TABEL00F                           31
466000980720    4C                   if        *IN31 = *OFF  and TBLflg = *blanks
466100990408     C                   MOVEL     TBLUNI        V1DCAN
466200980720   X4C                   else
466300971021     C                   MOVE      *BLANKS       V1DCAN
466400990129     C                   eval      *in71 = *on
466500990129     C                   eval      *in28 = *on
466600971023     C                   MOVEL     MSG(13)       V1CMSG
466700971021     C                   GOTO      ECTS01
466800980720     C                   endif
466900980720     C                   endif
467000980720
467100980720      * conto numero colli selezionati
467200980720      *   compresi quelli che erano eventualualmente già in C.A. tranne gli esclusi
467300980720     c                   IF        VCCtsl = 'T'  or  V1Ssel = 'I'
467400981229     c                             or  (*IN74 = *ON  and  V1Ssel <> 'E')
467500980518     c                   add       1             wclinc
467600980720     c                   ENDIF
467700980720
467800980720    2c                   ENDIF
467900971021     C     ECTS01        TAG
468000980720
468100981021     C                   UPDATE    FI03S04
468200980720
468300990114     c                   IF        *in28 = *on  or  *in29 = *on
468400981029     C                   eval      *in32 = *on
468500980720     c                   ENDIF
468600980720      *
468700981029     c                   IF        *in32 = *off
468800980720     c                   add       1             nrr1
468900981029     C     nrr1          chain     FI03S04                            32
469000980720     c                   ENDIF
469100980720
469200971022    1C                   ENDDO
469300980720
469400990304      * Controllo la corrispondenza tra il N. segnacolli scelti e il N. colli danneggiati/mancanti
469500990217      * Non controllo se premuto F12 o se già segnalati errori
469600000427      * Non controllo se bolla rigenerata e non ho segnacolli
469700990304      *                  C O N T R O L L I   E L I M I N A T I
469800990304      *   se i colli spedizione sono uguali ai colli danneggiati/mancanti selezione obbligatoria;
469900990304      *   se la spedizione è mamma di reso selezione obbligatoria;
470000990304      *   se anomalia per colli consegnati è possibile non indicare il dettaglio segnacolli
470100990217      *
470200990114     c                   SELECT
470300990217      *
470400990114     c                   WHEN      *IN28 = *ON  or  *IN29 = *ON  or  *INKL =*ON
470500990217      *
470600990114     c                   WHEN      V2Cncn = Wclinc
470700000427      *
470800000427     c                   WHEN      Wsop = '-'  and  ultnrr1 = *zeros
470900990217      *
471000990114     c                   OTHER
471100990114     c                   eval      *in28 = *on
471200990114     C                   movel     MSG(15)       V1CMSG
471300990114     c                   ENDSL
471400990114     c
471500990114     c
471600971021     C                   ENDSR
471700180130      /free
471800180130       //--------------------------------------------------------------
471900180130       // VERIFICA SEGNACOLLI EVENTUALMENTE GIA' IMPUTATI AD ALTRA C.A.
472000180130       //--------------------------------------------------------------
472100180130       BEGSR  sr_CtrlSgnCl;
472200180130
472300180208         setll  ( V1Sfls : V1Hlna : I00nrs : V1Snsc )  FNDCD002;
472400180220         readE(N)  ( V1Sfls : V1Hlna : I00nrs : V1Snsc )  FNDCD002;
472500180130
472600180208         DoW  NOT %eof( FNDCD02L );
472700180208
472800180208           If  DCDatb = *blank     and
472900180208               DCDdch = *zero;
473000180208
473100180209             //*·If  ( I00mod = 'A'      or  I00mod = 'G'
473200180209             //*·                        or  I00mod = 'M' )  and
473300180209             If  ( I00aac <> DCDaac  or  I00fil <> DCDfil
473400180209                                     or  I00nca <> DCDnca );
473500180130
473600180208               *in72 = *on;
473700180208               *in28 = *on;
473800180208               V1Cmsg = %trimR( Msg(123) ) + ' ' +
473900180208                        %editc( DCDfil : 'X' ) + '-' +
474000180208                        %trim( %editc( DCDnca : 'Z' ) ) +
474100180208                        ' (anno ' + %editc( DCDaac : '3' ) + ')';
474200180208               leavesr;
474300180208
474400180208             EndIf;
474500180208
474600180208           EndIf;
474700180208
474800180220           readE(N)  ( V1Sfls : V1Hlna : I00nrs : V1Snsc )  FNDCD002;
474900180130
475000180208         EndDo;
475100180130
475200180130       ENDSR;
475300180130      /end-free
475400980717      *****************************************************************
475500980717      *  MANUTENZIONE/INSERIMENTO ANAGRAFICA CLIENTI
475600980717      *****************************************************************
475700980514     C     MANCLI        BEGSR
475800981021      *
475900980717      * EMISSIONE
476000980514     c     emw02         tag
476100981021     c                   exfmt     FI03W02
476200981021      *
476300980514     c                   clear                   w2cmsg
476400980717     c                   eval      *in28 = *off
476500981021      *
476600980717     c   KL              goto      Emancli
476700981021      *
476800980717      * CONTROLLI
476900980619     c                   exsr      ctrw02
477000980515     c   28              goto      emw02
477100981020      *
477200981102     c                   IF        wtrc = 'C'
477300981102     c                   movel     w2crag        v1dksc
477400981102     c                   movel     DS_FI03W02    DS_W02CCON
477500981102     c                   ELSE
477600981105     c                   movel     w2crag        V2Crag
477700981105     c                   movel     w2crag        WV2Crag
477800981102     c                   movel     DS_FI03W02    DS_W02CRIV
477900981102     c                   ENDIF
478000981102
478100980717      *
478200980717     C     EMANCLI       ENDSR
478300980717      *****************************************************************
478400980717      * CONTROLLI VIDEATA MANUTENZ./INSERIMENTO ANAGRAFICA CLIENTI
478500980717      *****************************************************************
478600980514     C     CTRW02        BEGSR
478700980722
478800980727     c     60            DO        69            XX
478900980722     c                   eval      *IN(XX) = *OFF
479000980722     c                   ENDDO
479100980722
479200981021      * Controllo Ragione Sociale, Indirizzo, Località, Provincia, CAP, Nazione e Partita IVA
479300981021     c                   CLEAR                   FNLV14DS
479400981021     c                   z-add     DS_arbdat     I14dri
479500981021     c                   movel     W2Crag        I14rsc
479600981021     c                   movel     W2Cvia        I14ind
479700981021     c                   movel     W2Cloc        E14loc
479800981021     c                   movel     W2Cprv        E14prv
479900981021     c                   movel     W2Ccap        E14cap
480000981021     c                   movel     W2Cnaz        E14nar
480100981021     c                   eval      E14pip = W2Cive + W2Civa
480200981021      *
480300981021     c                   CALL      'FNLV14R'
480400981021     c                   PARM                    KPJBA
480500981021     c                   PARM                    FNLV14DS
480600981021      *
480700981021     c                   movel     I14ind        W2Cvia
480800981021     c                   movel     E14loc        W2Cloc
480900981021     c                   movel     E14prv        W2Cprv
481000981021     c                   movel     E14cap        W2Ccap
481100981021     c                   movel     E14nar        W2Cnaz
481200981021     c                   eval      W2Cive = %subst(E14pip:1:2)
481300981021     c                   eval      W2Civa = %subst(E14pip:3:14)
481400981021      *
481500981021     c                   eval      *in60 = (O14err = '1')                       Ragione Sociale
481600981021     c                   eval      *in61 = (O14err = '2')                       Indirizzo
481700981021     c                   eval      *in62 = (O14err = '5')                       CAP
481800981021     c                   eval      *in63 = (O14err = '3')                       Località
481900981021     c                   eval      *in64 = (O14err = '4')                       Provincia
482000981021     c                   eval      *in65 = (O14err = '6')                       Nazione
482100981021     c                   eval      *in68 = (O14err = '7')                       P.IVA
482200981021      *
482300981021     c                   IF        O14msg <> *blanks
482400981021     c                   movel     O14msg        W2Cmsg
482500981021     c                   eval      *IN28 = *ON
482600981021     c                   ELSE
482700981021      * Controllo il CAP
482800981021     C                   movel     w2ccap        kcap
482900981021     C                   exsr      CAZIS0
483000981021     c                   ENDIF
483100981021     C   28              goto      ectw02
483200981021
483300981021      * CONTROLLO IL NUMERO FAX
483400981021     C                   IF        W2Cfax <> *BLANKS
483500981021     C                   clear                   TRUL42DS
483600981021     C                   movel     W2Cfax        D42FAX
483700981021     C                   CALL      'TRUL42R'
483800981021     C                   PARM                    TRUL42DS
483900981021     C                   IF        D42ERR = '1'
484000990129     C                   eval      *in66 = *on
484100990129     C                   eval      *in28 = *on
484200981021     C                   movel     D42MSG        w2cmsg
484300981021     C                   ENDIF
484400981021     C                   ENDIF
484500981021     C   28              goto      ectw02
484600981021
484700981021      * CONTROLLO IL NUMERO DI TELEFONO
484800981021     C                   IF        W2Ctel <> *BLANKS
484900981021     C                   clear                   TRUL42DS
485000981021     C                   movel     W2Ctel        D42FAX
485100981021     C                   CALL      'TRUL42R'
485200981021     C                   PARM                    TRUL42DS
485300981021     C                   IF        D42ERR = '1'
485400990129     C                   eval      *in67 = *on
485500990129     C                   eval      *in28 = *on
485600981021     C                   movel     D42MSG        w2cmsg
485700981021     C                   ENDIF
485800981021     C                   ENDIF
485900981021     C   28              goto      ectw02
486000980722
486100980722     c     ECTW02        ENDSR
486200980722      *---------------------------------------------------------------*
486300980722      * ROUTINE DI CONTROLLO ESISTENZA CAP                            *
486400980722      *---------------------------------------------------------------*
486500980514     C     CAZIS0        BEGSR
486600980722      *
486700980722      * CONTROLLO CAP PER INTERO
486800981020     C                   CLEAR                   TISI95DS
486900981020     C                   CLEAR                   FNLV13DS
487000980514     C                   MOVEL     ' '           I95TLA
487100980514     C                   MOVEL     '7'           I95TCN
487200980514     C                   MOVEL     w2cCAP        I95CAP
487300980514     C                   MOVEL     w2cNAZ        I95NAR
487400980514     C                   MOVEL     w2cvia        I95IND
487500980514     C                   MOVEL     w2cloc        I95LOC
487600980514     C                   MOVEL     w2cprv        I95PRV
487700980514     C                   MOVEL     'S'           I13AF0
487800980514     C                   MOVEL     'S'           I13AF1
487900980514     C                   MOVEL     'S'           I13SZ2
488000980514     C                   MOVEL     'S'           I13LA3
488100980514     C                   MOVEL     'S'           I13SZ3
488200980514     C                   MOVEL     'S'           I13CNV
488300980514     C                   MOVEL     'S'           I13SZV
488400980514     C* Gestione errori forzabili: se cambiato CAP/Località
488500981021      * riazzero i valori di errori forzabili
488600980514    2C     w2cCAP        IFNE      WSVCAP
488700980514     C                   CLEAR                   WFZV
488800980514     C                   CLEAR                   WFZ1
488900980514     C                   CLEAR                   WFZ2
489000980514     C                   CLEAR                   WFZ3
489100980514     C                   MOVEL     w2cCAP        WSVCAP
489200980514    2C                   ENDIF
489300981021      * Modifica località
489400980514    2C     w2cLOC        IFNE      WSVLOC
489500980514     C                   MOVEL     w2cLOC        WSVLOC
489600980514     C                   CLEAR                   WFZ1
489700980514     C                   CLEAR                   WFZ2
489800980514     C                   CLEAR                   WFZ3
489900980514    2C                   ENDIF
490000981021      * Modifica PROVINCIA
490100980515    2C     w2cPRv        IFNE      WSVPRv
490200980515     C                   MOVEL     w2cPRv        WSVPRv
490300980514     C                   CLEAR                   WFZ3
490400980514    2C                   ENDIF
490500981021      * CALL
490600980514     C                   MOVEL     WFZV          E13FZV
490700980514     C                   MOVEL     WFZ1          E13FZ1
490800980514     C                   MOVEL     WFZ2          E13FZ2
490900980514     C                   MOVEL     WFZ3          E13FZ3
491000980514     C                   CALL      'FNLV13R'
491100980514     C                   PARM                    KPJBA
491200981020     C                   PARM                    FNLV13DS
491300981020     C                   PARM                    TISI95DS
491400980514     C* Ricontrollo errori
491500980514     C                   MOVEL     O13ERR        WERR
491600980514     C                   MOVEL     O13MSG        WMSG
491700980514     C                   MOVEL     E13FZV        WFZV
491800980514     C                   MOVEL     E13FZ1        WFZ1
491900980514     C                   MOVEL     E13FZ2        WFZ2
492000980514     C                   MOVEL     E13FZ3        WFZ3
492100980514     C*
492200980514     C* Se il livello di affidabilità non è inferiore a 2 e
492300980514     C* il livello di reperimento dati non è inferiore a 3
492400981020     C* reimposto i dati da TISI95DS
492500980514     C     O13RON        IFEQ      'S'
492600980514     C     O13ROC        OREQ      'S'
492700980514     C     O13ROP        OREQ      'S'
492800980514     C     O13ROL        OREQ      'S'
492900980514     C                   MOVEL     O95LOC        w2cLOC
493000980514     C                   MOVEL     O95PRV        w2cprv
493100980514     C                   MOVEL     O95NAR        w2cnaz
493200980514     C                   MOVEL     O95CAP        w2cCAP
493300980514     C                   END
493400980514     C*
493500980514     C                   EXSR      CTRIND
493600980514     C* Se non ci sono errori proseguo
493700980514     C   28              GOTO      FINCTR
493800980514     C*
493900980514     C* Controllo se reimpostata località
494000980514     C     O13ROL        IFEQ      'S'
494100990129     c                   eval      *in63 = *on
494200990129     c                   eval      *in28 = *on
494300980515     C                   MOVEL     msg(26)       w2cmsg
494400980514     C                   GOTO      FINCTR
494500980514     C                   END
494600980514     C* Controllo se reimpostata Provincia
494700980514     C     O13ROP        IFEQ      'S'
494800990129     c                   eval      *in64 = *on
494900990129     c                   eval      *in28 = *on
495000980515     C                   MOVEL     msg(28)       w2cmsg
495100980514     C                   GOTO      FINCTR
495200980514     C                   END
495300980514     C* Controllo se reimpostato CAP
495400980514     C     O13ROC        IFEQ      'S'
495500990129     c                   eval      *in62 = *on
495600990129     c                   eval      *in28 = *on
495700980515     C                   MOVEL     msg(27)       w2cmsg
495800980514     C                   GOTO      FINCTR
495900980514     C                   END
496000980514     C* Controllo se reimpostata Nazione
496100980514     C     O13RON        IFEQ      'S'
496200990129     c                   eval      *in65 = *on
496300990129     c                   eval      *in28 = *on
496400980515     C                   MOVEL     msg(29)       w2cmsg
496500980514     C                   GOTO      FINCTR
496600980514     C                   END
496700980514     C*
496800980514     C     FINCTR        ENDSR
496900980514     C*------------------------------------------------------*
497000980514     C*  Controllo dati indirizzo
497100980514     C*------------------------------------------------------*
497200980514     C     CTRIND        BEGSR
497300981021     C*
497400981021     C* Errore !!!!
497500981021     C     WERR          IFNE      ' '
497600981021     C     WMSG          ANDNE     *BLANKS
497700981021     C* Manca la Ragione sociale
497800981021     C                   SELECT
497900981021     C* Errore su località/città
498000981021     C     WERR          WHENEQ    '3'
498100990129     c                   eval      *in63 = *on
498200990129     c                   eval      *in28 = *on
498300981021     C* Errore su Cap
498400981021     C     WERR          WHENEQ    '5'
498500990129     c                   eval      *in62 = *on
498600990129     c                   eval      *in28 = *on
498700981021     C* Errore su provincia
498800981021     C     WERR          WHENEQ    '4'
498900990129     c                   eval      *in64 = *on
499000990129     c                   eval      *in28 = *on
499100981021     C* Nazione  errata
499200981021     C     WERR          WHENEQ    '6'
499300990129     c                   eval      *in65 = *on
499400990129     c                   eval      *in28 = *on
499500981021    2C                   ENDSL
499600990129      *
499700981021     C                   MOVEL     WMSG          w2cmsg
499800981021     C                   END
499900981021     C*
500000980514     C                   ENDSR
500100980623      *****************************************************************
500200980623      *   CONFERMA DATI C. A.
500300980623      *****************************************************************
500400990408     C     AGGIORNA      BEGSR
500500980722
500600981020      * Per apertura stacco numero c.a.
500700990127     C                   if        I00MOD = 'A'
500800990127     C                   exsr      numprog
500900980623     c                   endif
501000980623      *
501100170607if  1c                   IF        *IN28 = *OFF
501200160523      *
501300981020      * Descrizioni
501400981020     C                   EXSR      WRTDCS
501500990127      * Fasi
501600990408      *     sempre ma in Modifica solo se: tolta o immessa rivalsa dopo la Stampa Avviso Danno,
501700990408      *                                    ristampa dell'avviso danno per importo trattenuto
501800990205     C                   if        I00MOD <> 'M'
501900990208     C                              OR  (DCTfca > WFsad  and  V2Crag <> WDKArag
502000990205     c                              and (V2Crag = *blanks or WDKArag = *blanks))
502100990208     C                              OR  (DCTfca <> WFRPL  and  DCLfrp = 'S'
502200990927     c                                and WDCLfrp = *blanks and DCLdds > *zeros
502300990927     c                                                     and  I00fca > WFrpl)
502400981020     C                   EXSR      WRTDCF
502500980623     c                   endif
502600080310      *     in MODIFICA tipo gestione : se cambiato tipo gestione da Pratica a LT devo anche
502700080310      *                                    impostare la fase 110 se sono già oltre la 100
502800080310     C                   if        I00MOD = 'M'
502900080310     C                             and  DCTfca >= WFcon  and  dctfpr <> v1cfpr
503000080310     C                   EXSR      WRTDCF
503100080310     c                   endif
503200981022      * Liquidazione C.A.
503300990408      *     se modificati importo concordato o importo trattenuto
503400080303     c*****              if            (*IN07 = *ON  and  DCLipr <> V2Cipr)
503500080303     c*****                        or  (*IN09 = *ON  and  DCLipt <> V2Cipt)
503600080303     c*****                        or  (*IN09 = *ON  and  DCLdpt <> WV2Cdpt)
503700080303      * tolto il controllo degli indicatori in quanto a volte cambiando da pratioca a LT si
503800080303      * perdono eventuali azzeramenti dei campi
503900080303     c                   if            DCLipr <> V2Cipr
504000080303     c                             or  DCLipt <> V2Cipt
504100080303     c                             or  DCLdpt <> WV2Cdpt
504200981022     C                   EXSR      WRTDCL
504300981022     c                   endif
504400981022      * Anagrafiche
504500990408      *     in Apertura solo se inserito negli altri casi sempre
504600981022     c                   if        I00mod <> 'A'  or  V1Cksc > *zeros
504700981105     c                              or  V2Crag <> *blanks
504800981020     C                   EXSR      WRTDKA
504900981022     c                   endif
505000981022      * Dettaglio C.A.
505100990408      *     se caricati segnacolli
505200990210     c                   IF        ultnrr1 > *zeros
505300981022     C                   EXSR      WRTDCD
505400990210     c                   ENDIF
505500120111
505600120111     c* In conferma per mancanza, chiudo eventuali anomalie 115 rimaste aperte
505700160523if  2c                   if           I00mod   = 'G' and §TADragr  =  'M'
505800120111     c     kca           setll     fndcd000
505900120111     c     kca           reade(N)  fndcd000                               31
506000120111      *
506100160523do  3C     *IN31         DOWEQ     *OFF
506200120111      *
506300120111     C                   IF        DCDatb = *blanks  and  DCDdch = *zeros
506400120112     c                             and dcdnsc>0
506500120111     c     kanm2         chain     fnanm02l
506600120111     c                   if        %found(fnanm02l) and anmdch=0
506700120111     C                   CLEAR                   fnlr33ds
506800120111     c                   eval      d33dch=dateu
506900120111     c                   eval      d33fsc='M'
507000120111     c                   eval      d33cch='AN'
507100120111     C                   MOVE      ANMNR2        D33NRR
507200120111     C                   CALL      'FNLR33R'
507300120111     C                   PARM                    fnlr33ds
507400120111     c                   endif
507500120111     c                   endif
507600120111     c
507700120111     c     kca           reade(N)  fndcd000                               31
507800160523e   3c                   enddo
507900160523e   2c                   endif
508000120111     c
508100990408      * Mandato Assicurativo
508200990408      *     se ho richiamato pgm per reperirlo
508300990408     c                   IF        Wfidn48r = 'S'
508400990408     C                   EXSR      WRTDPT
508500990408     c                   ENDIF
508600050805      * Testata Evento (se richiesto rimborso ad A.I.G.)
508700050805      *    Se aperta C.A. su evento già fleggato
508800050805      *     o modificato il numero evento su una C.A.
508900160523if  2c                   if           I00mod   = 'A'
509000050805     c                             or WV1Cnev <> SAVnev
509100050805     c                             or V1Caae  <> SAVaae
509200050805     c                   exsr      UpdDET
509300160523e   2c                   endif
509400981020      * Testata C.A.
509500981020     C                   EXSR      WRTDCT
509600050218
509700990607      * In Apertura sfleggo BLP per ritorno informazione al mittente
509800160523if  2C                   If        I00MOD = 'A'
509900990607     C                   EXSR      WRTBLP
510000050218      * scrivo il file TNTMD
510100050218     C                   EXSR      WRTTMD
510200050218      * aggiorno titas richiamando FIDN58R
510300050218     c                   clear                   fidn58ds
510400050218     c                   eval      i58aas = dctaas
510500050218     c                   eval      i58lnp = dctlnp
510600050218     c                   eval      i58nrs = dctnrs
510700050218     c                   eval      i58nsp = dctnsp
510800050218     c                   movel     fidn58ds      kpjbu
510900050218     c                   call      'FIDN58R'
511000050218     c                   parm                    kpjba
511100160523e   2c                   Endif
511200980623      *
511300160523e   1c                   ENDIF
511400050523      *
511500050523      *In Confermaper Avariase il massimo risarcibile supera?il
511600050601      *  valore tabellato => invio e-mail previste in tab. MRA
511700050523     c                   IF            I00mod    =  'G'
511800050523     c                             and §TADragr  =  'V'
511900050523     c                             and V2Cipv    >  §STDipa
512000081111     c                   eval      *in52 = *off
512100050523     c                   exsr      sr_InviaMail
512200050523     c                   ENDIF
512300081110      *
512400081110      *In Confermaper Mancanza/ammanchi  il massimo risarcibile supera?il
512500081110      *  valore tabellato => invio e-mail previste in tab. MRA
512600081110     c                   IF            I00mod    =  'G'
512700081110     c                             and (§TADragr  =  'M' OR §TADragr = 'A')
512800081110     c                             and V2Cipv    >  §STDima
512900081111     c                   eval      *in52 = *on
513000081110     c                   exsr      sr_InviaMail
513100081110     c                   ENDIF
513200080117      *
513300080117      *In Conferma C.A. relativa a FedEx con responsabilità "PARTNER"?
513400080117      *  => invio e-mail previste in tab. MRA
513500080117     c                   IF            I00mod    =  'G'
513600080117     c                             and I00fca    = WFcon
513700080117     c                             and §DCTeuro  =  'F'
513800080117     c                             and DCTres    =  'P'
513900080118     c                   exsr      sr_CallFIDN38
514000080117     c                   ENDIF
514100130802      *
514200160518      *In Conferma C.A. relativa a bolle Export (EUROEXPRESS) con?
514300160518      *  responsabilità "PARTNER"?
514400160728      *  confermata come Pratica Assicurativa?
514500160728      *  con importo massimo risarcibile > 100 € (DSTD.§STDIPDEEX)?
514600130802      *  => invio e-mail previste in tab. MRA
514700160729     c                   clear                   $EEX_Mail
514800130802     c                   IF            I00mod    =  'G'
514900130802     c                             and I00fca    = WFcon
515000130802     c                             and §DCTtisp  =  'E'
515100130802     c                             and §DCTeuro  =  'X'
515200130802     c                             and DCTres    =  'P'
515300160729     c                             and DCTfpr    =  'P'
515400160728     c                             and V2Cipv    >= §STDipdEEX
515500160518     c                   Select
515600160518      * -x lna 320 - 325?
515700160518     c                   When      DCTlna = 320  or  DCTlna = 325
515800130802     c                   exsr      sr_CallFIDN39
515900160729     c                   eval      $EEX_Mail = *on
516000160518      * -x lna 350?
516100160518     c                   When      DCTlna = 350
516200141007     c                   exsr      sr_CallFIDN41
516300160729     c                   eval      $EEX_Mail = *on
516400160523      * -x lna 311, 333, 334 e 340-345?
516500160523     c                   When      DCTlna = 311  or
516600160523     c                             DCTlna = 333  or
516700160523     c                             DCTlna = 334  or
516800160523     c                             DCTlna = 340  or  DCTlna = 345
516900160523     c                   exsr      sr_CallFIDN68
517000160729     c                   eval      $EEX_Mail = *on
517100160518     c                   EndSl
517200141007     c                   ENDIF
517300080516      *
517400080516      *In Conferma C.A. Liquidazione Transattiva invio lettera al cliente beneficiario
517500080516      *
517600080516     c                   IF            I00mod    =  'G'
517700080516     c                             and I00fca    = WFcon
517800080516     c                             and DCTfpr    =  'T'
517900130410      /free
518000130410       //?Chiusura automatica C.A. in caso di Avaria
518100130410           IF  §TADragr = 'V';
518200130410             exsr chiudiCA;
518300130410           ENDIF;
518400130410      /end-free
518500080516     c                   exsr      sr_CallFIDN67
518600080516     c                   ENDIF
518700980623      *
518800980623     C                   ENDSR
518900980623      *****************************************************************
519000980623      *   STACCO PROGRESSIVO C. A.
519100980623      *****************************************************************
519200980623     C     NUMPROG       BEGSR
519300981006      *
519400980623     C                   eval      *in30 = *on
519500980623     C                   eval      *in28 = *off
519600990113     C                   move      V1Cdca        W0040
519700990113     C                   move      W0040         AAS
519800980623     C                   Z-ADD     201           CNU
519900000224     c* invece di recuperare il numeratore del P.O. gestione lo recupero dal P.O. che sta
520000000224     c* aprendo la C.A. che potrebbe essere anche un P.O. di £6
520100000224     c                   if        i00tpb = 'P'
520200000224     c                   z-add     wlnp          COMLIN
520300000224     c                   endif
520400000224     c                   if        i00tpb = 'A'
520500000224     c                   z-add     wlna          COMLIN
520600000224     c                   endif
520700980623      *
520800980623    2C     *IN30         DOWEQ     *ON
520900980623    2C     *IN28         ANDEQ     *OFF
521000980623      *
521100980623     C     KNUF          CHAIN     FLNUF                              3031
521200980623      *
521300980623      * Se non trovato il numeratore provo a crearlo
521400980623    3C                   if        *IN30 = *ON
521500980623     C                   Z-ADD     CNU           PA4CNU
521600980623     C                   CALL      'FNLVCNR'
521700980623     C                   PARM                    PA4CNU
521800980623     C     KNUF          CHAIN     FLNUF                              3031
521900980623    3C                   endif
522000980623      *
522100980623      * Se ancora non l'ho trovato errore
522200980623     C   30              eval      *in28 = *on
522300981229     C   30              MOVEL     MSG(11)       V1CMSG
522400980623     C   31              eval      *in28 = *on
522500980623     C   31              MOVEL     MSG(12)       V1CMSG
522600980623      *
522700980904     c                   if        *IN28 = *OFF
522800980623     C                   ADD       1             NUFNUM
522900980623     C                   UPDATE    FLNUF
523000980623     C                   Z-ADD     NUFNUM        WNUMCA
523100990113     C                   move      V1Cdca        AA4
523200980904     c                   endif
523300990113      *
523400980623    2C                   ENDDO
523500980623      *
523600980623     C                   ENDSR
523700980623      *****************************************************************
523800980623      *  VARIAZIONE TIPO ANOMALIA
523900980623      *****************************************************************
524000971020     C     VARTAD        BEGSR
524100980615      *
524200980716     C                   MOVEL     VTCtad        W1CTAD
524300980716     C                   MOVEL     VTDtad        W1DTAD
524400980623      *
524500971020     C     EMW01         TAG
524600971020     C                   EXFMT     FI03W01
524700980623      *
524800990129     c                   eval      *in28 = *off
524900990129     c                   eval      *in29 = *off
525000971020     C                   CLEAR                   W1CMSG
525100980623      *
525200981019     C   KL              goto      etad
525300980623      *
525400971021     C     '?'           SCAN      W1CTAD                                 31
525500981019      *  Ricerca
525600980623    1C                   IF        *IN31 = *ON
525700981020     c                   clear                   TIBS02DS
525800980526     c                   movel     'R'           t02mod
525900980722     c                   movel     knsif         t02sif
526000980722     C                   movel     'TAD'         t02cod
526100980722     C                   CALL      'TIBS02R'
526200980722     c                   parm                    KPJBA
526300981020     c                   parm                    TIBS02DS
526400981019      *
526500981019      *  NO Ricerca
526600981019     C                   ELSE
526700981019     C                   movel(P)  W1CTAD        WKEY1
526800981026     C                   clear                   WTADtipo
526900981019     C                   exsr      CHTAD
527000981019     C                   ENDIF
527100981019      *
527200981019      *   Controlli di validità'
527300981019    2C                   IF        W1CTAD = *BLANKS
527400981019     C                   movel     MSG(1)        W1CMSG
527500990129     c                   eval      *in40 = *on
527600990129     c                   eval      *in28 = *on
527700981019     C                   ELSE
527800981019      *
527900981019     c                   IF        t02err <> *blanks
528000981019     c                   eval      *in28 = *on
528100981019     C                   movel     MSG(2)        W1CMSG
528200981019     C                   ELSE
528300981019      *
528400981019     C                   movel     t02ke1        W1CTAD
528500980526     c                   movel     t02uni        dtad
528600981019     C                   movel     §taddesc      W1DTAD
528700981019      * Se Anomalia per Eventi non è valida
528800981019     C                   IF        §TADragr = 'E'
528900990129     c                   eval      *in40 = *on
529000990129     c                   eval      *in28 = *on
529100981020     c                   movel     MSG(67)       W1CMSG
529200981019     C                   ENDIF
529300981019      *
529400981019     C                   ENDIF
529500981019     C                   ENDIF
529600981019      *
529700981026     c                   IF        *IN28 = *ON  or  *INKF = *OFF
529800981019     C                   goto      EMW01
529900981026     c                   ELSE
530000981026     C                   movel     W1Ctad        VTCtad
530100981026     C                   movel     W1Dtad        VTDtad
530200981026     c                   ENDIF
530300980623      *
530400971020     C     ETAD          ENDSR
530500980625      *****************************************************************
530600980615      *  REPERIMENTO DATI TIPO ANOMALIA
530700980625      *****************************************************************
530800971020     C     CHTAD         BEGSR
530900980615      *
531000981019     c                   if        WTADtipo = 'E'
531100981019     c                   clear                   DTADEVE
531200981019     C                   else
531300981019     c                   clear                   DTAD
531400981019     C                   endif
531500981019      *
531600981020     c                   clear                   TIBS02DS
531700980615     C                   MOVEL     'C'           t02mod
531800980615     C                   MOVEL     knsif         t02sif
531900980615     C                   MOVEL     'TAD'         t02cod
532000980615     C                   MOVEL(P)  Wkey1         t02ke1
532100980615      *
532200980615     C                   CALL      'TIBS02R'
532300980615     C                   PARM                    KPJBA
532400981020     C                   PARM                    TIBS02DS
532500980615      *
532600980615     C                   if        t02err = *BLANKS
532700981019      *
532800981019     c                   if        WTADtipo = 'E'
532900981019     C                   MOVEL     T02UNI        DTADEVE
533000981019     C                   MOVEL     WKEY1         V1Ctae
533100981019     C                   MOVEL     E§TADdesc     V1Dtae
533200981019     C                   else
533300980615     C                   MOVEL     T02UNI        DTAD
533400980716     C                   MOVEL     WKEY1         VTCtad
533500980716     C                   MOVEL     §TADdesc      VTDtad
533600980625     C                   endif
533700981019     C                   endif
533800980625      *
533900971020     C                   ENDSR
534000980623      *****************************************************************
534100981126      * IMPOSTO TIPO GESTIONE
534200980623      *****************************************************************
534300980625     C     impPROPR      BEGSR
534400980709
534500080208      *
534600980623      *---------------------------------------------------------------------------------------------
534700981127      *                  T I P O   G E S T I O N E
534800981127      *---------------------------------------------------------------------------------------------
534900060302      * _ PRATICA ASSICURATIVA non modificabile se
535000051116      *                            Evento fortuito,senza mandato o elevazione,
535100051116      *                            senza importo d'assicurare in bolla
535200051116      *                          * Porto Franco Import
535300080604      *                          * Spedizione EXPORT DPD o FedEx responsabilità PARTNER
535400080604      *                          * Spedizione EXPORT e beneficiario Destinatario
535500020419      * - LIQUID. TRANSATTIVA  modificabile se:
535600020419      *                            non rientra nei casi precedenti
535700020419      *                          e max risarcibile minore del limite
535800020419      * - PRATICA ASSICURATIVA modificabile se:
535900020419      *                            non rientra nei casi precedenti e
536000020419      *                          e (Kg danneggiati * limite risarcibile)
536100020419      *                             minore del limite in tabella
536200020419      * - LIQUID. TRANSATTIVA  modificabile se:
536300020419      *                            non rientra nei casi precedenti
536400980623      *---------------------------------------------------------------------------------------------
536500980625
536600980623     c                   SELECT
536700980623      *
536800060302     c                   WHEN           ((WV1Cnev > *zeros and                  Evento
536900051116     c                                   E§tadevfo = 'S'  and                   Fortuito
537000051116     c                                   Wmand = ' ' and                        Senza mandato/Elevaz
537100051116     c                                   VTCias = *zeros)                       Valore assicurato
537200051116     c                             or   (§DCTtisp = 'I'  and                    -import
537300080604     c                                   §DCTport = 'F')                         porto franco
537400080604     c                             or   (§DCTtisp = 'E'  and                    -export
537500080604     c                                  (§DCTeuro = 'D'   or                     DPD  o
537600080604     c                                   §DCTeuro = 'F') and                     FedEx
537700080604     c                                   V2Cres   = 'P')                         resp. Partner
537800080604     c                             or   (§DCTtisp = 'E'  and                    -export
537900080604     c                                   V1cptr   = 'D'))                        benef. destinatario
538000051116     c                   movel     'P'           Wfpr
538100051116     c                   movel     'S'           Wfprfrz
538200051116     c
538300980709      *
538400080208     c                   WHEN           V2Cipv <= W§STDllt                      Imp. Max risarcibile
538500981113     c                   movel     'T'           Wfpr
538600080208     c                   movel     'S'           Wfprfrz
538700051213      *
538800080208     c                   WHEN           V2Cipv > W§STDllt   and                 Imp. Max risarcibile
538900051213     c                                  (Wmand = 'S'  or                        Mandato/Elevazione
539000051213     c                                  VTCias > *zeros)                        Valore assicurato
539100051213     c                   movel     'P'           Wfpr
539200080208     c                   movel     'S'           Wfprfrz
539300981105      *
539400080208     c                   WHEN          (V2Cpkd * Wlimris) > W§STDllt  OR        Kg danneggiati
539500080208     c                                  V2Cipv > W§STDllt                       Imp. Max risarcibile
539600981105     c                   movel     'P'           Wfpr
539700080208     c                   movel     'S'           Wfprfrz
539800980623      *
539900980623     c                   OTHER
540000980709     c                   movel     'T'           Wfpr
540100080208     c                   movel     'S'           Wfprfrz
540200980623      *
540300980623     c                   ENDSL
540400080208      *---------------------------------------------------------------------------------------------
540500080208      *                  I N T E R L O C U T O R E
540600080208      *---------------------------------------------------------------------------------------------
540700080208      *   MITTENTE      non modificabile se spedizione export  ** NO **
540800080208      *   MITTENTE      non modificabile se spedizione import in porto
540900080208      *                      franco
541000080208      *   DESTINATARIO      modificabile se importo da assicurare in
541100080208      *                      bolla e spedizione in assegnato
541200080208      *   DESTINATARIO  non modificabile se spedizione import in porto
541300080208      *                      assegnato
541400080208      *   DESTINATARIO      modificabile se cliente con mandato e
541500080208      *                      spedizione in assegnato
541600080208      *   MITTENTE          modificabile negli altri casi
541700080208      *---------------------------------------------------------------------------------------------
541800080208
541900080208     c                   SELECT
542000080208      *
542100080208     c                   WHEN      §DCTtisp = 'I'  and  §DCTport = 'F'
542200080208     c                   movel     'M'           Wptr
542300080208      *
542400080208     c                   WHEN      §DCTport = 'A'  and  VTCias > *zeros
542500080208     c                   movel     'D'           Wptr
542600080208      *
542700080208     c                   WHEN      §DCTtisp = 'I'  and  §DCTport = 'A'
542800080208     c                   movel     'D'           Wptr
542900080208      *
543000080208     c                   WHEN      §DCTport = 'A'  and  WMAND = 'S'
543100080208     c                   movel     'D'           Wptr
543200080208      *
543300080208     c                   OTHER
543400080208     c                   movel     'M'           Wptr
543500080208      *
543600080208     c                   ENDSL
543700080208      *
543800980525     c                   endsr
543900990208      *****************************************************************
544000990208      *  CONTROLLO CONGRUITA' TRA IMPORTO TRATTENUTO E RIVALSA
544100990208      *****************************************************************
544200990208     C     CTR_IPTriv    BEGSR
544300990208
544400990208      *  IL CLIENTE TRATTIENE
544500990208      *-----------
544600990208     c                   IF        V2Cipt > *zeros
544700990208
544800990208     c                   SELECT
544900990208      *    Ho rivalsa Errore
545000990208     c                   WHEN        V2Crag <> *blanks
545100990208     c                   movel     MSG(69)       V1Cmsg
545200990208     c                   eval      *in77 = *on
545300990208     c                   eval      *in28 = *on
545400120712     *** *    Se non paga Vettore Errore
545500120712     ***C                   WHEN        V2Cpgd <> 'V'
545600120712     ***c                   movel     MSG(70)       V1Cmsg
545700120712     ***c                   eval      *in79 = *on
545800120712     ***c                   eval      *in28 = *on
545900990208      *    Se ho stampato P.L. e ho inserito l'importo trattenuto imposto flag per ristampa
546000120712     *** *     solo se chi paga era la Consuldanni
546100990927     c                   WHEN        DCLdds > *zeros   and  V2Cipt > *zeros  and
546200120712     c                               WV2Cipt = *zeros
546300120712     ***c                                                 and  WV2Cpgd = 'C'
546400990208     c                   movel     'S'           DCLfrp
546500990208     c                   ENDSL
546600990208
546700990208      *-----------
546800990208     C                   ELSE
546900990208      *-----------
547000990208
547100990208      *  HO TOLTO RIVALSA
547200990208     c                   IF        V2Crag <> WDKArag  and  V2Crag = *blanks
547300990208     c                                                and  Wnoriv = *blanks
547400990208     c                   movel     'S'           Wnoriv
547500120712     *** *       reimposto chi paga la prima volta
547600120712     ***c                   If        WCLSflo3 = *blanks
547700120712     ***c                   eval      V2cpgd = 'C'
547800120712     ***c                   Else
547900120712     ***c                   movel     Wclsflo3      V2Cpgd
548000120712     ***c                   Endif
548100990208      *    Se ho stampato P.L. imposto flag per ristampa
548200990208     c                   If        DCLdds > *zeros
548300990208     c                   movel     'S'           DCLfrp
548400990208     c                   Endif
548500990208     c                   ENDIF
548600990208
548700990208      *  HO IMMESSO RIVALSA
548800990208     c                   IF        V2Crag <> WDKArag  and  WDKArag = *blanks
548900990208     c                   clear                   Wnoriv
549000990208      *
549100990208     c                   SELECT
549200120712     *** *    Se paga Vettore Errore
549300120712     ***C                   WHEN        V2Cpgd = 'V'
549400120712     ***c                   movel     MSG(72)       V1Cmsg
549500120712     ***c                   eval      *in79 = *on
549600120712     ***c                   eval      *in28 = *on
549700990208      *    Se ho stampato P.L. imposto flag per ristampa
549800990208     c                   WHEN        DCLdds > *zeros
549900990208     c                   movel     'S'           DCLfrp
550000990208     c                   ENDSL
550100990208      *
550200990208     c                   ENDIF
550300990208
550400120712     *** *  HO MODIFICATO CHI PAGA
550500120712     ***c                   IF        *IN28 = *OFF  and  V2Cpgd <> WV2Cpgd
550600120712     *** *    Se non ho tolto rivalsa
550700120712     ***c                   IF        V2Crag = WDKArag  or  V2Crag <> *blanks
550800120712     *** *    Se ho stampato il P.L. e non è impostata la ristampa errore
550900120712     *** *****
551000120712     *** ****** Non emetto più errore sul campo del chi paga NON c'è più Consuldanni
551100120712     *** *****
551200120712     *** *
551300120712     ***c                   IF        DCLdds > *zeros  and  DCLfrp = *blanks
551400120712     ***c******             movel     MSG(74)       V1Cmsg
551500120712     ***c******             eval      *in79 = *on
551600120712     ***c******             eval      *in28 = *on
551700120712     ***c                   ELSE
551800120712     *** *    Se ho rivalsa e non paga Consuldanni errore
551900120712     ***c                   IF        V2Crag <> *blanks  and  V2Cpgd <> 'C'
552000120712     ***c******             movel     MSG(74)       V1Cmsg
552100120712     ***c******             eval      *in79 = *on
552200120712     ***c******             eval      *in28 = *on
552300120712     ***c                   ENDIF
552400120712     ***c                   ENDIF
552500120712     *** *
552600120712     ***c                   ENDIF
552700120712     ***c                   ENDIF
552800990208
552900990208      *-----------
553000990208     c                   ENDIF
553100990208
553200990208     C                   ENDSR
553300980615      *****************************************************************
553400980722      *  GESTIONE SCRITTURA DESCRIZIONI C.A.
553500980615      *****************************************************************
553600971022     C     WRTDCS        BEGSR
553700990121
553800990121      * Richiamo il pgm gestione descrizioni in modalità conferma (FLM='C') per la scrittura delle
553900990121      * note (per apertura e gestione) e delle descrizioni (per apertura).
554000990121     c                   IF        I00MOD = 'A'  or  I00MOD = 'G'
554100981020     C                   CLEAR                   FIDN10DS
554200980720     C                   movel     'C'           I10FLM
554300981216     C                   movel     'C'           I10TPD
554400981216     C                   movel     'N'           I10TRC
554500990107     c                   If        I00mod = 'A'
554600990107     c                   movel(P)  dsnca         I10nkt
554700990107     c                   Else
554800990107     c                   movel(P)  DS_numca      I10nkt
554900990107     c                   Endif
555000980618     c                   movel     i00fca        i10nks
555100981020     c                   movel(P)  FIDN10DS      kpjbu
555200980722     C                   CALL      'FIDN10R'
555300980429     C                   PARM                    kpjba
555400990121     c                   ENDIF
555500990121      *
555600990121     c                   IF        I00MOD = 'A'
555700981217     C                   CLEAR                   FIDN10DS
555800981217     C                   movel     'C'           I10FLM
555900981217     C                   movel     'C'           I10TPD
556000981217     C                   movel     'D'           I10TRC
556100990119     C                   clear                   I10STD
556200990107     c                   If        I00mod = 'A'
556300990107     c                   movel(P)  dsnca         I10nkt
556400990107     c                   Else
556500990107     c                   movel(P)  DS_numca      I10nkt
556600990107     c                   Endif
556700981217     c                   movel(P)  FIDN10DS      kpjbu
556800981217     C                   CALL      'FIDN10R'
556900981217     C                   PARM                    kpjba
557000990119     c                   ENDIF
557100990119
557200981216      * Aggiorno e/o scrivo record nel file descrizioni in base ai campi descrittivi che ho a video,
557300981216      * nel caso di aggiornamento della descrizione direttamente senza richiamo di FIDN10R.
557400981216      * E se sono in apertura richiamo il pgm gestione descrizioni in modalità conferma (FLM='C')
557500980721
557600980615      * DESCRIZIONE AGGIUNTIVA
557700980722     c                   IF        V1Dagg <> *blanks
557800980722     c                   movel     'A'           wstd
557900981216     c                   movel     v1dagg        wnot
558000980722     c                   exsr      wrtdcs_1
558100980722     c                   ENDIF
558200980721
558300980721      * MOTIVO DEL DANNO
558400980722     c                   IF        V1Dmtv <> *blanks
558500980722     c                   movel     'M'           wstd
558600980722     c                   movel     v1dmtv        wnot
558700980722     c                   exsr      wrtdcs_1
558800980722     c                   ENDIF
558900980721
559000980721      * PEZZI DANNEGGIATI/MANCANTI
559100980722     c                   IF        V2Dnpz <> *blanks
559200980722     c                   movel     'P'           wstd
559300980722     c                   movel     V2Dnpz        wnot
559400980722     c                   exsr      wrtdcs_1
559500980722     c                   ENDIF
559600980721
559700980721      * PEZZI PRESENTI
559800980722     c                   IF        V2Cprs <> *blanks
559900980722     c                   movel     'K'           wstd
560000980722     c                   movel     V2Cprs        wnot
560100980722     c                   exsr      wrtdcs_1
560200980722     c                   ENDIF
560300980721
560400981126      * INTERLOCUTORE
560500980722     c                   movel     'N'           wstd
560600980722     c                   movel     V1Dnoc        Wnot
560700980722     c                   exsr      wrtdcs_1
560800020125
560900020125      * MODIFICA FLAG FRANCHIGIA
561000020125     c                   IF            I00mod <> 'A'
561100020125     c                             and V1Cfra <> V1Hfra
561200020125     c                   clear                   wstd
561300020125     c                   IF        V1Cfra =  'F'
561400020125     c                   movel(p)  Comm_Fra_I    wnot
561500020125     c                   else
561600020125     c                   movel(p)  Comm_Fra_E    wnot
561700020125     c                   ENDIF
561800020125     c                   exsr      wrtdcs_2
561900020125     c                   ENDIF
562000080310
562100080310      * MODIFICA FLAG TIPO GESTIONE
562200080314     c                   IF            I00mod =  'M'
562300080310     c                             and V1Cfpr <> dctfpr
562400080310     c                   clear                   wstd
562500080310     c                   If        V1cfpr = 'T'
562600080310     c                   movel(p)  Comm_MTG_T    wnot
562700080310     c                   else
562800080310     c                   movel(p)  Comm_MTG_P    wnot
562900080310     c                   endif
563000080310     c                   exsr      wrtdcs_2
563100080310     c                   ENDIF
563200080313
563300080313      * MODIFICA beneficiario
563400080314     c                   IF            I00mod =  'M'
563500080313     c                             and V1Cptr <> dctptr
563600080313     c                   clear                   wstd
563700080313     c                   If        V1cptr = 'D'
563800080313     c                   movel(p)  Comm_MBEN_D   wnot
563900080313     c                   else
564000080313     c                   movel(p)  Comm_MBEN_M   wnot
564100080313     c                   endif
564200080313     c                   exsr      wrtdcs_2
564300080313     c                   ENDIF
564400981126
564500971022     C                   ENDSR
564600980722      *****************************************************************
564700981216      *  SCRITTURA DESCRIZIONI - CAMPO A VIDEO
564800980722      *****************************************************************
564900980722     C     WRTDCS_1      BEGSR
565000980722      *
565100980722     c                   IF        I00mod = 'A'
565200980722     c                   movel(P)  dsnca         knkt
565300980722     c                   ELSE
565400981029     c                   movel(P)  DS_numca      knkt
565500980722     c                   ENDIF
565600980722      *
565700980722     c                   movel     'C'           ktpd
565800980722     c                   movel     wstd          kstd
565900980722     c                   clear                   kdim
566000980722     c                   clear                   khim
566100980730     c                   clear                   knks
566200980722     c                   movel     'D'           ktrc
566300980722      *
566400980722     c     kdcs          chain     fndcs01l                           30
566500980722      *
566600980722     c     *in30         ifeq      *on
566700980722     c                   clear                   fndcs000
566800980722     c                   movel     ktpd          dcstpd
566900980722     c                   movel     knkt          dcsnkt
567000980722     c                   movel     kstd          dcsstd
567100980722     c                   movel     knks          dcsnks
567200980722     c                   movel     kdim          dcsdim
567300980722     c                   movel     khim          dcshim
567400980722     c                   movel     ktrc          dcstrc
567500980722     c                   z-add     1             dcspno
567600981005     c                   movel     I00fgs        dcspos
567700980722     c                   movel     knmus         dcspru
567800980722     c                   endif
567900980722      *
568000980722     c                   movel     Wnot          dcsnot
568100990104     c                   clear                   dcsatb
568200990104     c                   clear                   dcsft1
568300980722     c                   clear                   dcsdt1
568400980722      *
568500980722     c  n30              update    fndcs000
568600980722      *
568700981126      * INTERLOCUTORE  scrivo solo se <> blanks
568800980722     c                   if        *IN30 = *ON  and  Wstd <> 'N'
568900980722     c                   write     fndcs000
569000980722     c                   else
569100980722     c                   if        *IN30 = *ON  AND  Wstd = 'N'
569200980722     c                               AND  V1Dnoc <> *blanks
569300980722     c                   write     fndcs000
569400980722     c                   endif
569500980722     c                   endif
569600980722      *
569700980722     C                   ENDSR
569800020125      *****************************************************************
569900020125      *  SCRITTURA NOTE AGGIUTIVE
570000020125      *****************************************************************
570100020125     C     WRTDCS_2      BEGSR
570200020125      *
570300020125      * cerco l'ultimo progressivo note della fase in corso
570400020125      * - campi strandard
570500020125     c                   eval      Ktpd = 'C'
570600020125     c                   eval      Knkt = ds_numca
570700020125     c                   eval      Kstd = WSTD
570800020125     c                   eval      Kdim = dateu
570900020125     c                   eval      Khim = wora4
571000020125      * - dati relativi alla CA in manutenzione
571100020125     c                   movel(p)  I00fca        Knks
571200020125     c                   eval      Ktrc = 'N'
571300020125      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
571400020125     c     Kdcs          setgt     FNDCS01L
571500020125     c     Kdcs          readpe    FNDCS01L
571600020125      *
571700020125     c                   clear                   DCSatb
571800020125     c                   eval      DCStpd = Ktpd
571900020125     c                   eval      DCSnkt = Knkt
572000020125     c                   eval      DCSstd = Kstd
572100020125     c                   eval      DCSdim = Kdim
572200020125     c                   eval      DCShim = Khim
572300020125     c                   eval      DCSnks = Knks
572400020125     c                   eval      DCStrc = Ktrc
572500020125     c                   IF        %eof(FNDCS01L)
572600020125     c                   eval      DCSpno = 1
572700020125     c                   else
572800020125     c                   eval      DCSpno = DCSpno + 1
572900020125     c                   ENDIF
573000020125     c                   eval      DCSnot = WNOT
573100020125     c                   eval      DCSpru = KNMUS
573200020125     c                   eval      DCSpos = I00fgs
573300020125     c                   clear                   DCSstn
573400020125     c                   clear                   DCSft1
573500020125     c                   clear                   DCSdt1
573600020125      *
573700020125     c                   write     FNDCS000
573800020125      *
573900020125     C                   ENDSR
574000130409      *****************************************************************
574100130409      *  SCRITTURA NOTE AGGIUTIVE CHIUSURA CA
574200130409      *****************************************************************
574300130409     C     WRTDCS_3      BEGSR
574400130409      *
574500130409      * cerco l'ultimo progressivo note della fase 900
574600130409      * - campi strandard
574700130409     c                   eval      Ktpd = 'C'
574800130409     c                   eval      Knkt = ds_numca
574900130409     c                   eval      Kstd = WSTD
575000130409     c                   eval      Kdim = dateu
575100130409     c                   eval      Khim = wora4
575200130409      * - dati relativi alla CA in CHIUSURA
575300130409     c                   movel(p)  900           Knks
575400130409     c                   eval      Ktrc = 'N'
575500130409      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
575600130409     c     Kdcs          setgt     FNDCS01L
575700130409     c     Kdcs          readpe    FNDCS01L
575800130409      *
575900130409     c                   clear                   DCSatb
576000130409     c                   eval      DCStpd = Ktpd
576100130409     c                   eval      DCSnkt = Knkt
576200130409     c                   eval      DCSstd = Kstd
576300130409     c                   eval      DCSdim = Kdim
576400130409     c                   eval      DCShim = Khim
576500130409     c                   eval      DCSnks = Knks
576600130409     c                   eval      DCStrc = Ktrc
576700130409     c                   IF        %eof(FNDCS01L)
576800130409     c                   eval      DCSpno = 1
576900130409     c                   else
577000130409     c                   eval      DCSpno = DCSpno + 1
577100130409     c                   ENDIF
577200130409     c                   eval      DCSnot = WNOT
577300130409     c                   eval      DCSpru = KNMUS
577400130409     c                   eval      DCSpos = I00fgs
577500130409     c                   clear                   DCSstn
577600130409     c                   clear                   DCSft1
577700130409     c                   clear                   DCSdt1
577800130409      *
577900130409     c                   write     FNDCS000
578000130409      *
578100130409     C                   ENDSR
578200980623      *****************************************************************
578300980623      *  SCRITTURA FASI C.A.
578400980623      *****************************************************************
578500980623     c     WRTDCF        BEGSR
578600980623      *
578700980429     c                   clear                   fndcf000
578800980722      *
578900980623     c                   IF        I00MOD = 'A'
579000980722     c                   z-add     aa4           DCFaac
579100980722     c                   z-add     comlin        DCFfil
579200980722     c                   z-add     wnumca        DCFnca
579300980623     c                   ELSE
579400980722     c                   z-add     i00aac        DCFaac
579500980722     c                   z-add     i00fil        DCFfil
579600980722     c                   z-add     i00nca        DCFnca
579700980722     c                   movel     v1cptr        DCFptr
579800980623     c                   ENDIF
579900980722      *
580000990205      * Se Manutenzione forzo la fase
580100990205     c                   SELECT
580200990205     C                   WHEN      I00MOD = 'M'  and
580300990205     c                             DCTfca > WFsad  and  V2Crag <> WDKArag
580400990205     c                             and (V2Crag = *blanks or WDKArag = *blanks)
580500990205     c                   z-add     WFmri         DCFfca
580600990205     c                   clear                   DCFptr
580700990205      *
580800990205     C                   WHEN      I00MOD = 'M'  and
580900990205     c                             WDCLfrp = *blanks  and  DCLfrp = 'S'
581000990927     c                             and  DCLdds > *zeros  and  I00fca > WFrpl
581100990205     c                   z-add     WFrpl         DCFfca
581200990205     c                   clear                   DCFptr
581300080310      * modifica tipo gestione da P  a T
581400080310     C                   WHEN      I00MOD = 'M'  and
581500080310     c                             dctfpr <> v1cfpr
581600080310     c                   z-add     WFfff         DCFfca
581700080310     c                   clear                   DCFptr
581800990205      *
581900990205     c                   WHEN        I00mod <> 'M'
582000990205     c                   z-add     i00fca        DCFfca
582100990205     c                   ENDSL
582200981224      *
582300980722     c                   z-add     dateu         DCFdfc
582400000420     c                   movel     wora4         DCFhfc
582500980722     c                   z-add     i00fgs        DCFfev
582600980722     c                   movel     knmus         DCFpru
582700980907     c                   clear                   DCFftr
582800980907     c                   clear                   DCFdtr
582900001102     c                   clear                   DCFlet
583000020201     c                   clear                   DDCF01
583100980722      *
583200001102      * Verifico se devo tornare al cliente l'info di C.A. confermata
583300001102     c                   IF        DcfFca = WFcon
583400001102     c                   exsr      AGGT3K
583500001102     c                   IF        §3KCQ7 = 'S'
583600020201     c                   movel     'S'           §DCFriti
583700001102     c                   ENDIF
583800001102     c                   ENDIF
583900020125      *
584000020125      * Flag Franchigia - aggiornabile solo se:
584100020125      *  1) la C.A. è in gestione alla SEDE;
584200020125      *  2) ha come tipo gestione "Pratica Assicurativa";
584300020125      *  3) l'anno della spedizione relativa è oltre il 2001
584400020125      *  4) la C.A. è in una fase minore di 700 (progetto di liquidazione)
584500020125      *  5) L'importo da assicurare in bolla (TASIAS) è 0
584600020125      *  6) Non esiste un mandato per conto (/reale) nel file FNDPT00F
584700020201      * OPPURE se:
584800020201      *  1) era già stato valorizzato
584900020125    1c                   if        *in33 = *on
585000020201     c                   movel     V1Cfra        §DCFpfra
585100020201    1c                   else
585200020201     c                   movel     §DCTpfra      §DCFpfra
585300020125    1c                   endif
585400160728      *
585500160728      *?Impostazione Tipo Invio = "X" (Inviato Preavviso Danno al Partner):?
585600160728      *?SE conferma C.A.?
585700160728     c                   IF            I00mod    =  'G'
585800160728     c                             and I00fca    = WFcon
585900160728      *   ?relativa a bolle Export?
586000160728     c                             and §DCTtisp  =  'E'
586100160728      *   ?con partner EuroExpress?
586200160728     c                             and §DCTeuro  =  'X'
586300160728      *   ?con responsabilità "PARTNER"?
586400160901     c*//                          and DCTres    =  'P'
586500160901     c                             and ( *in08   =  *on
586600160901     c                             and   V2Cres  =  'P' )
586700160728      *   ?confermata come Pratica Assicurativa?
586800160901     c                             and V1Cfpr    =  'P'
586900160728      *   ?con importo massimo risarcibile > 100 € (DSTD.§STDIPDEEX)?
587000160728     c                             and V2Cipv    >= §STDipdEEX
587100160830      *   ?da inviare e-mail per una tra le LNA EuroExpress previste?
587200160830     c                             and ( DCTlna = 320  or  DCTlna = 325  or
587300160830     c                                   DCTlna = 350  or
587400160830     c                                   DCTlna = 311  or  DCTlna = 333  or
587500160830     c                                                     DCTlna = 334  or
587600160830     c                                   DCTlna = 340  or  DCTlna = 345 )
587700160728     c                   movel     'X'           §DCFinvi
587800160728     c                   endif
587900020201      *
588000020201     c                   movel     DDCF01        DcfLet
588100001102      *
588200980519     c                   write     fndcf000
588300980623      *
588400971022     C                   ENDSR
588500980623      *****************************************************************
588600980623      *  SCRITTURA/AGGIORNAMENTO DETTAGLIO C.A.
588700980623      *****************************************************************
588800971022     C     WRTDCD        BEGSR
588900980623      *
589000981020      *  Leggo SFL dettaglio segnacolli
589100980519     c                   z-add     1             nrr1
589200981029     c     nrr1          chain     FI03S04                            31
589300981029    2c     *IN31         DOWEQ     *OFF
589400980722      *
589500981020      *   Elaboro segnacolli non presenti su altre C.A.
589600981229     c     v1snsc        lookup    nsc                                    73
589700981229    3c                   IF         *IN73 = *OFF
589800980623      *
589900981113      *   Elaboro segnacolli selezionati oppure se modificata anomalia e non esclusi
590000020827    4c                   IF        VCCtsl = 'T'  or  V1Ssel = 'I'
590100981113     C                             or  (V1Scan <> V1Hcan  and  V1Ssel <> 'E')
590200981016     c     KDCD2D        chain     FNDCD002                           30
590300990317      *
590400990317      *   Se Inserimento ricalcolo responsabilità
590500990317     c                   if        Wcalresp = *blanks  and
590600990317     c                             (VCCtsl = 'T'  or  V1Ssel = 'I')
590700990208     c                   eval      Wcalresp = 'S'
590800990208     c                   endif
590900981016      *
591000981029      *   Clearizzo se: Inserimento, Apertura, Rcd annullato, Aggiorno rcd di altra C.A.
591100981029     c                   IF            *IN30  = *ON
591200981029     c                             or  I00mod = 'A'
591300981029     c                             or  DCDatb = 'A'
591400981029     c                             or  DS_numca <> DS_dcdkey
591500981016     c                   clear                   FNDCD002
591600981016     c                   ENDIF
591700981016      *
591800981020      *  Imposto i campi
591900981020     c                   IF        I00MOD = 'A'
592000981020     c                   z-add     aa4           dcdaac
592100981020     c                   z-add     comlin        dcdfil
592200981020     c                   z-add     wnumca        dcdnca
592300020827     c                   ELSE
592400981016     c                   z-add     i00aac        dcdaac
592500981016     c                   z-add     i00fil        dcdfil
592600981016     c                   z-add     i00nca        dcdnca
592700020827     c                   ENDIF
592800981029      *
592900981029     c                   z-add     v1sfls        dcdfls
593000990309     c                   z-add     v1hlna        dcdlna
593100981029     c                   z-add     i00nrs        dcdnrs
593200981016     c                   z-add     v1snsc        dcdnsc
593300981016     c                   movel     v1scan        dcdcan
593400981016     c                   clear                   dcdatb
593500981029     c                   clear                   dcddch
593600981029     c                   clear                   dcdcch
593700981016     c                   clear                   dcdftr
593800981016     c                   clear                   dcddtr
593900980720      *
594000020827    5c                   IF        *IN30 = *OFF
594100981016     c                   update    FNDCD002
594200980623     c                   ELSE
594300981016     c                   write     FNDCD002
594400980623    5c                   ENDIF
594500980623      *
594600980623    4c                   endif
594700980623      *
594800980722      * se non sono in apertura cancello eventuali records esclusi
594900020827    4c                   IF        I00MOD <> 'A'   and
595000981229     c                             V1Ssel = 'E'  and  V1Hi74 = '1'
595100981016     c     kdcd2c        chain     FNDCD002                           30
595200020827    5c                   IF        *IN30 = *OFF
595300980722     c                   movel     'A'           dcdatb
595400980907     c                   clear                   dcdftr
595500980907     c                   clear                   dcddtr
595600981016     c                   update    FNDCD002
595700990317      * ricalcolo responsabilità e non permetto variazioni manuali
595800990209     c                   if          Wcalresp = *blanks
595900990208     c                   eval      Wcalresp = 'S'
596000990317     c                   eval      *IN91 = *ON
596100990208     c                   endif
596200990208      *
596300020827    5c                   ENDIF
596400051019      * ...e già che ci sono: annullo i rec. corrispondenti in FNDCR
596500051019      *    (se in SEDE viene modificato il numero dei colli)
596600051019if  5c                   if             DUTlpo     = 'S'
596700051020     c                             and  WV2Cncn    > *zeros
596800051020     c                             and  V2Cncn     < WV2Cncn
596900051019     c                   exsr      AnnullaDCR
597000051019e   5c                   endif
597100020827    4c                   ENDIF
597200980722      *
597300020827    3c                   ENDIF
597400980623      *
597500980527     c                   add       1             nrr1
597600981029     c     nrr1          chain     FI03S04                            31
597700020827    2c                   ENDDO
597800980623      *
597900971022     C                   ENDSR
598000051019      *****************************************************************
598100051019      * ANNULLAM. RECORDS RELATIVI ALLE RESPONSABILITA' x SEGNACOLLO
598200051019      *****************************************************************
598300051019     c     AnnullaDCR    BEGSR
598400051020      *
598500051020     c                   if        NOT %open(FNDCR02L)
598600051020     c                   open      FNDCR02L
598700051020     c                   endif
598800051019      *
598900051019      * Annullo tutti rcd relativi al segnacollo in esame
599000051019     c                   movel     V1Snsc        DCRseg
599100051019     c     K04DCR02      setll     FNDCR000
599200051019     c     K04DCR02      reade     FNDCR000
599300051019      *
599400051019do  1c                   DOW       NOT %eof(FNDCR02L)
599500051019      *
599600051019if  2c                   if        DCRatb = *blanks
599700051019     c                   eval      DCRatb = 'A'
599800051019     c                   clear                   DCRftr
599900051019     c                   eval      DCRdtr = DateU
600000051019     c                   UPDATE    FNDCR000
600100051020      *   ...e (se trovo record specifici - per segnacollo)
600200051020      *   considero da NON ricalcolare la responsabilità
600300051020     c                   clear                   Wcalresp
600400051019e   2c                   endif
600500051019      *
600600051019     c     K04DCR02      reade     FNDCR000
600700051019      *
600800051019e   1c                   ENDDO
600900051019      *
601000051019     c                   ENDSR
601100981022      *****************************************************************
601200981022      *  SCRITTURA/AGGIORNAMENTO liquidazione C.A.
601300981022      *****************************************************************
601400981022     C     WRTDCL        BEGSR
601500981022      *
601600080303      * tolgo il controllo dell'indicatore in quanto non è detto che sia acceso se vengo da
601700080303      * una modifica di tipo gestione
601800080303     c*****              If        *in09 = *on
601900981022     c                   z-add     V2Cipt        DCLipt
602000981022     c                   movel     V2Cvpt        DCLvpt
602100981201     c                   z-add     WDCLcpt       DCLcpt
602200981201     c                   z-add     WV2Cdpt       DCLdpt
602300080303     c******             Endif
602400010907     c                   If        DCLipt = 0
602500010907     c                   clear                   DCLvpt
602600010907     c                   clear                   DCLcpt
602700010907     c                   clear                   DCLdpt
602800010907     c                   Endif
602900990128      *
603000080303     c                   If        *in07 = *on
603100990127     c                   z-add     V2Cipr        DCLipr
603200990127     c                   movel     V2Cvpr        DCLvpr
603300990127     c                   z-add     WDCLcpr       DCLcpr
603400990128     c                   z-add     V2Cipr        DCLipl
603500990128     c                   movel     V2Cvpr        DCLvpl
603600990128     c                   z-add     WDCLcpr       DCLcpl
603700080303     c                   Endif
603800010907     c                   If        DCLipr = 0
603900010907     c                   clear                   DCLvpr
604000010907     c                   clear                   DCLcpr
604100010907     c                   Endif
604200010907     c                   If        DCLipl = 0
604300010907     c                   clear                   DCLvpl
604400010907     c                   clear                   DCLcpl
604500010907     c                   Endif
604600990128      *
604700981022     c                   clear                   DCLftr
604800981022      *
604900981022     c                   If        Wfndcl = 'S'
605000981106     c                   UPDATE    FNDCL000
605100981022     c                   Else
605200990127      *
605300990127     c                   z-add     i00aac        DCLaac
605400990127     c                   z-add     i00fil        DCLfil
605500990127     c                   z-add     i00nca        DCLnca
605600981106     c                   WRITE     FNDCL000
605700981022     c                   Endif
605800981022      *
605900981022     C                   ENDSR
606000980722      *****************************************************************
606100981126      *  SCRITTURA/AGGIORNAMENTO anagrafica clienti per danni
606200980722      *****************************************************************
606300980520     C     WRTDKA        BEGSR
606400981022
606500981126      * BENEFICIARIO ALTERNATIVO
606600981022     c                   movel     'C'           Wtrc
606700981022     c     KDKA          chain     fndka01l                           30
606800981022     c                   move      V1Cksc        W0040
606900981022      *
607000990408      *     Se è codificato o non è impostato annullo FNDKA se esiste
607100981022     c                   IF        W0040 <> 8888
607200990408     c                   If          *IN30 = *OFF
607300990408     c                   movel     'A'           dkaatb
607400990408     c                   clear                   dkaftr
607500990408     c                   update    fndka000
607600990408    2c                   Endif
607700981022     c                   ELSE
607800981022      *
607900981022     c                   clear                   FNDKA000
608000981022     c                   If        I00mod <> 'A'
608100981022     c                   z-add     i00aac        dkaaac
608200981022     c                   z-add     i00fil        dkafil
608300981022     c                   z-add     i00nca        dkanca
608400981022     c                   Else
608500981022     c                   z-add     aa4           dkaaac
608600981022     c                   z-add     comlin        dkafil
608700981022     c                   z-add     wnumca        dkanca
608800981022     c                   Endif
608900981022      *
609000981102     c                   move      V1Cksc        DKAksc
609100981022     c                   movel     Wtrc          DKAtrc
609200050523     c                   movel     dutKCI        DKAkcc
609300981022     c                   movel     CW2Crag       DKArag
609400981022     c                   movel     CW2Cvia       DKAvia
609500981022     c                   movel     CW2Ccap       DKAcap
609600981022     c                   movel     CW2Cloc       DKAloc
609700981022     c                   movel     CW2Cprv       DKAprv
609800981022     c                   movel     CW2Cnaz       DKAnaz
609900981022     c                   eval      DKAiva = CW2Civa + CW2Cive
610000981022     c                   movel     CW2Ctel       DKAtel
610100981022     c                   movel     CW2Cfax       DKAfax
610200981022      *
610300981022      * Scrivo o aggiorno
610400981022     c                   If        *IN30 = *ON
610500981022     c                   write     fndka000
610600981022     c                   Else
610700981022     c                   update    fndka000
610800981022    2c                   Endif
610900981022      *
611000981022     c                   ENDIF
611100981022
611200981022      * RIVALSA
611300981022     c                   movel     'R'           Wtrc
611400981022     c     KDKA          chain     fndka01l                           30
611500981022      *
611600981022      *     Se non è impostato cancello FNDKA se esiste
611700981105     c                   IF        V2Crag = *blanks
611800990408     c                   If          *IN30 = *OFF
611900990408     c                   movel     'A'           dkaatb
612000990408     c                   clear                   dkaftr
612100990408     c                   update    fndka000
612200990408    2c                   Endif
612300981022     c                   ELSE
612400981022      *
612500981022     c                   clear                   FNDKA000
612600981022     c                   If        I00mod <> 'A'
612700981105     c                   z-add     I00aac        DKAaac
612800981105     c                   z-add     I00fil        DKAfil
612900981105     c                   z-add     I00nca        DKAnca
613000981022     c                   Else
613100981105     c                   z-add     aa4           DKAaac
613200981105     c                   z-add     comlin        DKAfil
613300981105     c                   z-add     wnumca        DKAnca
613400981022     c                   Endif
613500981022      *
613600981022     c                   movel     Wtrc          DKAtrc
613700050523     c                   movel     dutKCI        DKAkcc
613800981105     c                   z-add     8888          DKAksc
613900981105     c                   movel     DKAfil        DKAksc
614000981022     c                   movel     RW2Crag       DKArag
614100981022     c                   movel     RW2Cvia       DKAvia
614200981022     c                   movel     RW2Ccap       DKAcap
614300981022     c                   movel     RW2Cloc       DKAloc
614400981022     c                   movel     RW2Cprv       DKAprv
614500981022     c                   movel     RW2Cnaz       DKAnaz
614600981022     c                   eval      DKAiva = RW2Civa + RW2Cive
614700981022     c                   movel     RW2Ctel       DKAtel
614800981022     c                   movel     RW2Cfax       DKAfax
614900981022      *
615000981022      * Scrivo o aggiorno
615100981022     c                   If        *IN30 = *ON
615200981022     c                   write     fndka000
615300981022     c                   Else
615400981022     c                   update    fndka000
615500981022    2c                   Endif
615600981022      *
615700981022     c                   ENDIF
615800981022
615900981022     C                   ENDSR
616000990408      **********************************************************************
616100990408      *  SCRITTURA/AGGIORNAMENTO MANDATO ASSICURATIVO
616200990408      **********************************************************************
616300990408     C     WRTDPT        BEGSR
616400990428      *
616500990428      *  Riaggancio FNDPT se esiste
616600990428     c                   IF        Wfndpt = 'S'
616700990428     c     kca           chain     FNDPT000                           30
616800990428     c                   ENDIF
616900990408      *
617000990408     c                   SELECT
617100990408      *  Se ho mandato aggiorno
617200990408     c                   WHEN      WMAND = 'S'
617300990408     c                   clear                   fndpt000
617400990408     c                   If        I00mod = 'A'
617500990408     c                   z-add     aa4           dptaac
617600990408     c                   z-add     comlin        dptfil
617700990408     c                   z-add     wnumca        dptnca
617800990408     c                   Else
617900990408     c                   z-add     dctaac        dptaac
618000990408     c                   z-add     dctfil        dptfil
618100990408     c                   z-add     dctnca        dptnca
618200990408     c                   Endif
618300050523     c                   movel     dutKCI        dptkcc
618400990408     c                   z-add     I48ksc        dptksc
618500990408     c                   z-add     I48ctr        dptctr
618600990408     c                   z-add     O48prg        dptprg
618700990408     c                   movel     O48ftc        dptftc
618800990408     c                   movel     O48fvm        dptfvm
618900990408     c                   z-add     O48vlm        dptvlm
619000010913     c                   if        O48vlm <> 0
619100990408     c                   movel     O48vvm        dptvvm
619200010913     c                   else
619300010913     c                   clear                   dptvvm
619400010913     c                   endif
619500990408     c                   IF        Wfndpt = 'S'
619600990408     c                   update    FNDPT000
619700990408     c                   ELSE
619800990408     c                   write     FNDPT000
619900990408     c                   ENDIF
620000990428      *
620100990408      *  Se esiste il rcd lo annullo
620200990408     c                   WHEN      Wfndpt = 'S'  and  DPTatb <> 'A'
620300990408     c                   movel     'A'           DPTatb
620400990408     c                   clear                   DPTftr
620500990408     c                   update    FNDPT000
620600990408      *
620700990408     c                   ENDSL
620800990408      *
620900990408     C                   ENDSR
621000980717      **********************************************************************
621100990514      *  AGGIORNO BOLLA PARTENZA PER TRASMISSIONE AL MITTENTE
621200980717      **********************************************************************
621300990607     C     WRTBLP        BEGSR
621400990514
621500001102      * Aggancio 3K
621600001102     c                   exsr      AGGT3K
621700990602
621800990602      * Controllo se è previsto il ritorno delle consegne anomale
621900990607     C                   IF        §3KCQ4 = 'S'
622000990514
622100990514      * Sfleggo se ho FNBLP in linea
622200050310      * BOLLA MAMMA
622300050310     C     kbol02        chain     fnblp000                           30
622400020910
622500990607     C                   IF        *IN30 = *OFF
622600990607     C                   clear                   BLPft3
622700990607     C                   clear                   BLPdt3
622800990607     c                   update    fnblp000
622900990607     C                   ENDIF
623000990514      *
623100990607     C                   ENDIF
623200990514      *
623300990607     C                   ENDSR
623400050805
623500050805      **********************************************************************
623600050805      *  AGGIORNAMENTO TESTATA EVENTI
623700050805      **********************************************************************
623800050805     C     UpdDET        BEGSR
623900050805      *
624000050805      * Ripulisco il flag di RIMBORSO A.I.G. su FNDET00F
624100050805      * se  "P" = Ricezione totale del rimborso da AIG
624200050805      *  o  "N" = Evento non fortuito che ha le CA pagate ma < 10000€
624300050805      *
624400051121     c     Kdet          chain     FNDET000
624500050805      *
624600050805if  1c                   if        %found(FNDET01L)
624700050805     c                             and (DETftr = 'P'
624800050916     c                              or  DETftr = 'N' or
624900050916     c                             (DETftr = 'X' and ds_arbdat >= 20050418))
625000050805     c                   clear                   DETftr
625100050805     c                   UPDATE    FNDET000
625200050805e   1c                   endif
625300050805      *
625400050805     C                   ENDSR
625500020910
625600990514      **********************************************************************
625700990514      *  SCRITTURA/AGGIORNAMENTO TESTATA C.A.
625800990514      **********************************************************************
625900990514     C     WRTDCT        BEGSR
626000990514      *
626100980722      * se non sono in apertura chaino la testata c.a.
626200980722    2c                   IF        I00MOD <> 'A'
626300990114     c     knumca        chain     fndct000                           30
626400980722     c                   ELSE
626500980717      *  operazioni solo per i record nuovo (apertura)
626600980519     c                   clear                   fndct000
626700980519     c                   z-add     aa4           dctaac
626800980519     c                   z-add     comlin        dctfil
626900980519     c                   z-add     wnumca        dctnca
627000980519     c                   move      invdca        dctmgc
627100980519     c                   z-add     i00aas        dctaas
627200980519     c                   z-add     i00lnp        dctlnp
627300980519     c                   z-add     i00nrs        dctnrs
627400980519     c                   z-add     i00nsp        dctnsp
627500040720      *
627600040720      *   verifico la presenza di legami per valorizzare i campi del numero spedizione
627700040720      *
627800040720     c                   If        §dctlega = 'S'
627900040720      *  operazioni solo per i record nuovo (apertura)
628000040720     c                   clear                   fndck000
628100040720
628200040720      *   se ci sono legami verifico chi fa l'apertura
628300040720
628400040720      *   se ha aperto  la PARTENZA   la bolla mamma è in FIDN00DS e cerco la bolla figlia
628500040720     c                   If        i00tpb = 'P'
628600040720      * cerco l'ultima figlia in FNLBL
628700040720     c     kbol01        chain     FNLBL02L
628800040720do  3c                   dow       %found(FNLBL02L)
628900040720     c                   eval      DCKaas = LBLaan
629000040720     c                   eval      DCKlnp = LBLlpn
629100040720     c                   eval      DCKnrs = LBLnrn
629200040720     c                   eval      DCKnsp = LBLnsn
629300040721     c     klbl_f        chain     FNLBL02L
629400040720e   3c                   enddo
629500040720      *
629600040720     C                   ELSE
629700040720      * se ha aperto l'arrivo la bolla figlia è in FIDN00DS e devo cercare la bolla mamma
629800040720     c                   z-add     i00aas        dckaas
629900040720     c                   z-add     i00lnp        dcklnp
630000040720     c                   z-add     i00nrs        dcknrs
630100040720     c                   z-add     i00nsp        dcknsp
630200040720      * cerco la mamma in FNLBL
630300040720     c     kbol01        chain     FNLBL01L
630400040720     c                   If        %found(FNLBL01L)
630500040720     c                   eval      DCTaas = LBLaao
630600040720     c                   eval      DCTlnp = LBLlpo
630700040720     c                   eval      DCTnrs = LBLnro
630800040720     c                   eval      DCTnsp = LBLnso
630900040720     c                   endif
631000040720      *
631100040720     c                   endif
631200040720      * scrivo il record di FNDCK nel quale c'è sempre la figlia mentre in FNDCT c'è sempre la bolla
631300040720      * mamma
631400040720     c                   eval      DCKaac = DCTaac
631500040720     c                   eval      DCKfil = DCTfil
631600040720     c                   eval      DCKnca = DCTnca
631700040720     c                   write     FNDCK000
631800040720     c                   endif
631900040720
632000981229     c                   z-add     Wlna          dctlna
632100981229     c                   movel     Wlnp          §dctlnpc
632200981001     c                   movel     arbnas        dctnas
632300981228     c                   movel     DDCT01        dctflo
632400980722     c                   ENDIF
632500020125      *
632600020125      * Flag Franchigia - aggiornabile solo se:
632700020125      *  1) la C.A. è in gestione alla SEDE;
632800020125      *  2) ha come tipo gestione "Pratica Assicurativa";
632900020125      *  3) l'anno della spedizione relativa è oltre il 2001
633000020125      *  4) la C.A. è in una fase minore di 700 (progetto di liquidazione)
633100020125      *  5) L'importo da assicurare in bolla (TASIAS) è 0
633200020125      *  6) Non esiste un mandato per conto (/reale) nel file FNDPT00F
633300020201      * OPPURE se:
633400020201      *  1) era già stato valorizzato
633500020125    1c                   if        *in33 = *on
633600020125     c                   movel     V1Cfra        §DCTPfra
633700020125     c                   movel     DDCT01        dctflo
633800020125    1c                   endif
633900980709      *
634000990209      * Imposto valori per ricalcolo responsabilità
634100990209     c                   If        Wcalresp = *blanks
634200990209     c                             and  (I00mod = 'A'      or
634300051103     c                                  (DUTlpo  = 'S'
634400051103     c                             and   V2Cncn >= DCTncn) or
634500051103     c                                  (DUTlpo <> 'S'
634600051103     c                             and   V2Cncn <> DCTncn) or
634700990209     c                                   VTCtad <> DCTtad  or
634800000307     c                                   V1Ccrc <> DCTcrc  or
634900000307     c                                   WV1Cnev <> DCTnev)
635000990209     c                   eval      Wcalresp = 'S'
635100990209     c                   Endif
635200000307      *
635300000307     C                   IF        WV1Cnev > *zeros
635400000307     c                   z-add     v1caae        dctaae
635500000307     c                   z-add     WV1Cnev       dctnev
635600000307     c                   ELSE
635700000307     c                   clear                   dctaae
635800000307     c                   clear                   dctnev
635900000307     c                   ENDIF
636000990317      * Se ho diminuito il numero colli danneggiati impedisco modifiche alla responsabilità
636100990317     c                   If        V2Cncn < DCTncn
636200990317     c                   eval      *in91 = *ON
636300990317     c                   Endif
636400990317      *
636500990209     c                   movel     VTCtad        dcttad
636600990209     c                   z-add     V2Cncn        dctncn
636700980722     c                   movel     v1ccrc        dctcrc
636800990209      *
636900980716     c                   z-add     V2Cnpz        dctnpz
637000980716     c                   z-add     V2Cpkd        dctpkd
637100981019     c                   z-add     V2Cipv        DCTipv
637200010907     c                   IF        DCTipv <> 0
637300981019     c                   movel     V2Cvpv        DCTvpv
637400010907     c                   else
637500010907     c                   clear                   DCTvpv
637600010907     c                   ENDIF
637700010912     c                   movel     WDCTcpv       DCTcpv
637800981022     c                   movel     Wdctfpv       DCTfpv
637900981019     c                   move      V1Ctdt        dcttdt
638000981022     c                   move      V2Cdsd        dctdsd
638100080310      * verifico se modificato il tipo gestione modifico anche la fase
638200080310      *
638300080310     C                   If        I00MOD = 'M'  and
638400080310     c                             dctfpr <> v1cfpr
638500080310     C                             and  DCTfca >= WFcon
638600080310     c                   z-add     WFfff         DCTfca
638700080310     c                   else
638800981022     c                   z-add     I00fca        dctfca
638900080310     c                   endif
639000080310      *
639100981022     c                   movel     V1Cfpr        dctfpr
639200981022     c                   movel     V1Cptr        dctptr
639300120712     ***c                   movel     V2Cpgd        dctpgd
639400120712     c                   clear                   DCTpgd
639500050523     c                   z-add     dutKCI        dctkcc
639600981022     c                   z-add     V1Cksc        dctksc
639700981022     c                   move      V1Crcd        dctrcd
639800990128     c                   IF        *IN08 = *ON
639900981211     c                   move      V2Cres        dctres
640000981211     c                   ELSE
640100981211     c                   clear                   dctres
640200981211     c                   ENDIF
640300980722
640400990927      * Se C.A. in una fase superiore all'avviso danno e ho tolto o immesso rivalsa imposto come
640500990927      * fase C.A. "Modifica Rivalsa"
640600981224     c                   IF        DCTfca > WFsad  and  V2Crag <> WDKArag
640700981223     c                             and  (V2Crag = *blanks or WDKArag = *blanks)
640800990927     c                   z-add     WFmri         DCTfca
640900981223     c                   ELSE
641000990205
641100120712     *** * Se immesso l'importo trattenuto, già stampato il Progetto di Liquidazione, chi paga
641200120712     *** * era la Consuldanni e la fase attuale è maggiore di quella da impostare:
641300120712      * Se immesso l'importo trattenuto, già stampato il Progetto di Liquidazione
641400120712      * e la fase attuale è maggiore di quella da impostare:
641500990927      * forzo la fase "Ristampa Progetto di Liquidazione"
641600990205     c                   IF        WDCLfrp = *blanks  and  DCLfrp = 'S'
641700990205     c                             and  DCLdds > *zeros  and  I00fca >  WFrpl
641800120712     ***c                             and  WV2Cpgd = 'C'
641900990927     c                   z-add     WFrpl         DCTfca
642000990205     c                   ELSE
642100991001
642200991001      *  Se sono in fase "Stampa progetto liquidazione e non ricado nei casi precedenti non imposto
642300991001      *  FASE CA e PO GESTIONE
642400991001     c                   IF        I00fca <>  WFspl
642500981223
642600980717      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
642700990111      *  Se la fase ricavata è stampa lettera reso avaria o il suo sollecito la ritengo valida solo
642800990111      *   se la merce danneggiata è resa, in caso contrario ripeto la ricerca
642900981214     c                   z-add     DCTfca        WI05fca
643000981214      *
643100981214     c                   DO        *HIVAL
643200981214     c                   eval      *in30 = *on
643300981214     c                   exsr      SUB_POFASE
643400990111     c                   If        DCTdsd <> 'R'
643500981214     c                             and (DFAfca = WFlav  or  DFAfca = WFsla)
643600981214     c                   z-add     DFAfca        WI05fca
643700981214     c                   eval      *in30 = *off
643800981214     C                   Endif
643900981214     c  N30              ENDDO
644000981214
644100980722     c                   SELECT
644200980722     c                   WHEN      DFAgfs = 'P'
644300981229     c                   movel     §DCTlnpc      dctgfc
644400980722     c                   WHEN      DFAgfs = 'S'
644500980717     c                   z-add     46            dctgfc
644600981023     c                   WHEN      DFAgfs = 'A'
644700981023     c                   z-add     DCTlna        dctgfc
644800980722     c                   OTHER
644900981023     c                   z-add     DCTfil        dctgfc
645000980722     c                   ENDSL
645100981223      *
645200981223     c                   ENDIF
645300991001     c                   ENDIF
645400990205     c                   ENDIF
645500981223
645600981223      * Imposto flag per trasmissione dati
645700981223     c                   clear                   dctft1
645800981223     c                   clear                   dctft2
645900980722
646000981223      * Per Apertura imposto P.O. per trasmissione dati e scrivo
646100981223     c                   IF        I00MOD <> 'A'
646200990217     c                   UPDATE    fndct000
646300981223     c                   ELSE
646400981223      *
646500981223     c                   if         i00tpb = 'P'
646600981229     c                   z-add     DCTlna        dctfl1
646700980519     c                   else
646800981229     c                   movel     §DCTlnpc      dctfl1
646900981223     c                   endif
647000981230     c                   z-add     046           dctfl2
647100981230      *
647200981223     c                   WRITE     fndct000
647300981223     c                   ENDIF
647400980722      *
647500990217      * Richiamo per aggiornare flag visualizzazione responsabilita
647600990217     c                   clear                   kpjbu
647700990217     c                   CALL      'FIDN52R'
647800990217     c                   PARM                    KPJBA
647900990217     c                   PARM                    DS_DCTKEY
648000040705      *
648100040705      * Controllo se necessario rigenerare la spedizione in apertura CA
648200040705     c                   IF        I00MOD =  'A'
648300040705     C                   EXSR      CtrRigen
648400040705     c                   endif
648500040705      *
648600990217      *
648700971022     C                   ENDSR
648800990927      *----------------------------------------------------------------
648900000420      * VERIFICO SE INVIARE IN SEDE LA RICHIESTA DI RIGENERAZIONE SPEDIZIONE
649000990927      *----------------------------------------------------------------
649100000420     C     CtrRigen      BEGSR
649200990927      *
649300000628      * se spedizione in partenza e giorni pulizia di ARB impostati     oppure
649400000628      * se spedizione in arrivo   e giorni pulizia di BLP impostati
649500000628     c                   IF        (i00tpb = 'A'and  ARBdcm > *ZEROS  and
649600000628     c                              §5ABLP > 1)  or
649700000628     c                             (i00tpb = 'P'and  ARBdcm > *ZEROS  and
649800000628     c                              §5Aarb > 1)
649900000420     c     *iso          move      ARBdcm        Wdataeur
650000000628      * calcolo i giorni di pulizia di blp se sto aprendo in arrivo
650100000628     c                   if        i00tpb = 'A'
650200000420     c                   sub       1             §5ABLP
650300000420     c                   adddur    §5ABLP:*D     wdataeur
650400000628     c                   endif
650500000628      * calcolo i giorni di pulizia di arb se sto aprendo in partenza
650600000628     c                   if        i00tpb = 'P'
650700000628     c                   sub       1             §5AARB
650800000628     c                   adddur    §5AARB:*D     wdataeur
650900000628     c                   endif
651000000420      *
651100000420      * se sono trascorsi i gg dalla pulizia
651200000420     c                   IF        Wdataeur <= Wdataoggi
651300000420      *
651400000420     c                   If         i00tpb = 'A'
651500000420     c     kbol01        setll     fnblp000                               30
651600000420     c                   Else
651700000420     c     kbol01        setll     fnarb000                               30
651800000420     c                   Endif
651900000420      *
652000000420      * se la bolla non è in ARB o BLP
652100000420     c                   IF        *in30 = *off
652200000420      *
652300000420      * invio in sede una richiesta di rigenerazione spedizione
652400000420     c                   clear                   tibs20ds
652500000420     C                   MOVEL     'N'           i20VID
652600000420     C                   Z-ADD     arbAAS        i20AAS
652700000420     C                   Z-ADD     arbLNP        i20LNP
652800000420     C                   Z-ADD     arbNRS        i20NRS
652900000420     C                   Z-ADD     arbNSP        i20NSP
653000000420     C                   Z-ADD     arbLNA        i20LNA
653100000420     C                   MOVEL     'D'           i20CAR
653200000420     c                   If         i00tpb = 'A'
653300000420     C                   MOVEL     'P'           i20TRA
653400000420     C                   Else
653500000420     C                   MOVEL     'A'           i20TRA
653600000420     C                   Endif
653700000420     C                   Z-ADD     I00fgs        i20POR
653800000420     C                   MOVEL     KNMUS         i20PRU
653900000420     C                   Z-ADD     DATEU         i20DIM
654000000420     C                   Z-ADD     WORA4         i20HIM
654100000420      *
654200000420     C                   MOVEL(P)  tibs20ds      KPJBU
654300000420      *
654400000420     C                   CALL      'TIBS20R'
654500000420     C                   PARM                    KPJBA
654600000420      *
654700000420     c                   ENDIF
654800000420     c                   ENDIF
654900000420     c                   ENDIF
655000000420      *
655100000420     C                   ENDSR
655200050218      *----------------------------------------------------------------
655300050218      *  SCRITTURA/AGGIORNAMENTO TNTMD
655400050218      *----------------------------------------------------------------
655500050218     c     wrttmd        BegSr
655600050218
655700050310      * SEMPRE CON LA BOLLA MAMMA (campi di fndct)
655800050310     c     kbol02        Chain     Tntmd01l
655900050218
656000050310      * la cerco prima in partenza
656100050310     c     kbol02        chain(n)  fnblp000                           30
656200050310      * se non c'è in partenza la cerco in arrivo
656300050310     c   30kbol02        chain(n)  fnarb000                           30
656400050218
656500050218     c                   If        Not *In30
656600050218      * Scrivo un nuovo rcd
656700050218     c                   If        Not %Found(tntmd01l)
656800050218     c                   Write     Tntmd000
656900050310     c                   Else
657000050218      * Aggiorno rcd esistente
657100050218     c                   Update    Tntmd000
657200050218     c                   EndIf
657300050218
657400050218     c                   EndIf
657500050218
657600050218     c                   EndSr
657700981029      *****************************************************************
657800981214      *  RICAVO IL P.O. CHE GESTISCE LA FASE SUCCESSIVA
657900981029      *****************************************************************
658000981214     C     SUB_POFASE    BEGSR
658100981030      *
658200981214     c                   CLEAR                   DS_FNDFA
658300981214     c                   CLEAR                   FIDN05DS
658400981214      *
658500981214     c                   movel     'F'           I05MOD
658600981214     c                   z-add     WI05fca       I05fca
658700981214     c                   movel     DCTfpr        I05fpr
658800981214     c                   movel     DCTptr        I05tpc
658900981214     c                   if        WV1Cnev  >  *zeros
659000981214     c                   movel     'E'           I05fpe
659100981214     c                   else
659200981214     c                   movel     'N'           I05fpe
659300981214     c                   endif
659400981214     c                   if        DCTnde > *zeros
659500981214     c                   movel     'D'           I05fde
659600981214     c                   else
659700981214     c                   movel     'N'           I05fde
659800981214     c                   endif
659900981214     c                   movel     'O'           I05ffs
660000981214     c                   z-add     DCFdfc        i05dta
660100981214     c                   move      DCTtad        i05tad
660200981214      * valorizzo il numero c.a.
660300981214     c                   z-add     dctaac        i05aac
660400981214     c                   z-add     dctfil        i05fil
660500981214     c                   z-add     dctnca        i05nca
660600981214      *
660700981214     C                   CALL      'FIDN05R'
660800981214     c                   PARM                    KPJBA
660900981214     c                   PARM                    FIDN05DS
661000981214      *
661100981214     c                   movel     O05REC        DS_FNDFA
661200981214      *
661300981214     c                   ENDSR
661400990208      **********************************************************************
661500990208      *  GESTIONE RESPONSABILITA'
661600990208      **********************************************************************
661700990208     C     GESTRESP      BEGSR
661800990209     c                   clear                   FIDN50DS
661900990209      *
662000990209     c                   movel     'C'           I50mod
662100990209     c                   z-add     DCTaac        I50aac
662200990209     c                   z-add     DCTfil        I50fil
662300990209     c                   z-add     DCTnca        I50nca
662400040720      * passo il numero di spedizione sul quale ho aperto la CA (in caso di apertura su figlia
662500040720      * di dirottammento in arrivo ho la bolla figlia)
662600050915      * VISTO che il calcolo della responsabilità avviene anche in partenza e in sede Elisa
662700050915      * ha modificato il programma del calcolo della responsabilità in modo tale che riconosce
662800050915      * chi ha aperto la CA in base ai dati che vengono passati al pgm FIDN50R.
662900050915      * Quindi indipendentemente da chi richiama il calcolo della responsabilità (Partenza Arrivo
663000050915      * Sede) passiamo a FIDN50R la bolla figlia se esiste in FNDCK altrimenti la bolla di FNDCT
663100050915      * e la linea di partenza della CA del campo DCTFLO
663200050915      * AGGANCIO FNDCK
663300050915
663400050922     c     Kca           chain     fndck01l
663500050915     c                   If        %found(fndck01l)
663600050915     c                   z-add     DCKaas        I50aas
663700050915     c                   z-add     DCKlnp        I50lnp
663800050915     c                   z-add     DCKnrs        I50nrs
663900050915     c                   z-add     DCKnsp        I50nsp
664000050915     c                   else
664100050915     c                   z-add     DCTaas        I50aas
664200050915     c                   z-add     DCTlnp        I50lnp
664300050915     c                   z-add     DCTnrs        I50nrs
664400050915     c                   z-add     DCTnsp        I50nsp
664500050915     c                   endif
664600050912      *
664700050912     c                   if         i00tpb = 'S'                                SEDE
664800050915     c     kbol50        chain     titas30c                           30
664900050915      * Aggancio TITAS
665000050912     c                   else
665100050915     c     kbol50        chain(n)  fnarb000                           30
665200050329      * se non c'è in arrivo   la cerco in partenza
665300050915     c   30kbol50        chain(n)  fnblp000                           30
665400050912     c                   endif
665500040720      *
665600990209     c                   z-add     ARBlna        I50lna
665700990209     c                   z-add     ARBncl        I50ncl
665800990209     c                   z-add     ARBtfp        I50tfp
665900990209     c                   z-add     ARBtfa        I50tfa
666000990305     c                   z-add     VTCksm        I50ksm
666100990305     c                   z-add     VTCksd        I50ksd
666200050915      * passo anche la linea di partenza della CA
666300050915     c                   movel     §dctlnpc      I50lnpmam
666400090324      *  In Apertura calcolo sempre.
666500090804      *  Negli altri casi se il tipo calcolo è abilitato (A=automatico; " "=no respons.;R=ricalcolo)
666600090324      *  e se sono stati variati alcuni dati (colli, dettaglio colli, riserva, tipo anomalia,evento)
666700090324      *  se sono l'arrivo calcolo subito, se sono la partenza o la sede imposto il ricalcolo.
666800090324if  1c                   IF        Wcalresp = 'S'  and
666900090804     c                              (DCTcar = 'A'  or  DCTcar = 'R'
667000090324     c                                             or  DCTcar = *blanks)
667100990209      *
667200990209     c                   movel(P)  FIDN50DS      KPJBU
667300990209      *
667400050218     c                   Call      'FIDN50C'
667500990209     c                   Parm                    KPJBA
667600990209      *
667700990209     c                   movel     KPJBU         FIDN50DS
667800990209      *
667900990210      *  Se ho un errore imposto il flag per il ricalcolo
668000990210     c                   SELECT
668100990210     c                   WHEN        O50err = 'N'
668200990210     c                   clear                   Wcalresp
668300990210      *
668400990210     c                   WHEN        O50err = *blanks
668500990210     c                   eval      Wcalresp = O50car
668600990210      *
668700990210     c                   WHEN        O50err = 'E'
668800990210     c                   movel     O50msg        V6Cmsg
668900990210     c                   eval      Wcalresp = 'R'
669000990210     c                   ENDSL
669100990210      *
669200990210      *
669300051019x   1c                   ELSE
669400990210     c                   eval      Wcalresp = DCTcar
669500051019e   1c                   ENDIF
669600990212
669700990212      * Rialloco  FNDCT000
669800990212     c     kca           CHAIN     fndct000                           30
669900990217     c                   movel     dctflo        Ddct01
670000990217     c                   movel     §DCTlnpc      Wlnp
670100090324     c
670200090324     c                   if        wlnp= i50lnp
670300090324     c                   movel     i50tfp        wtfplnp
670400090324     c                   else
670500090324     c                   clear                   fnlv55ds
670600090324     c                   movel     'P'           d55tpt
670700090324     c                   movel     wlnp          d55lin
670800090324     c                   if        ds_arbdat>0
670900090324     c                   z-add     ds_arbdat     d55drf
671000090324     c                   else
671100090324     c                   move      dctmgc        d55drf
671200090324     c                   movel     dctaac        d55drf
671300090324     c                   endif
671400090324     c                   call      'FNLV55R'
671500090324     c                   parm                    fnlv55ds
671600090324     c                   movel     d55tfp        wtfplnp
671700090324     c                   endif
671800090324     c
671900990212
672000990406      *  Visualizzo responsabilità
672100990209     c                   SELECT
672200990210      *    se il pgm di calcolo ha riscontrato errori.
672300990210     c                   WHEN         V6Cmsg <> *blanks
672400990210     c                   exsr      CARresp
672500990210     c                   exsr      GESresp
672600990406      *    no se la responsabilità non deve essere considerata
672700990217     c                   WHEN          DCTtcr = 'S'
672800990406     c                   if        *in04 = *on
672900990406     c                   eval      O00err = 'E'
673000990406     c                   movel     msg(106)      O00msg
673100990406     c                   endif
673200990210      *    se apertura
673300990210     c                   WHEN          I00mod = 'A'
673400990210     c                   exsr      CARresp
673500990210     c                   exsr      GESresp
673600990210      *    se manutenzione in sede
673700990210     c                   WHEN          SIMFEL = *zeros
673800990210     c                   exsr      CARresp
673900990210     c                   exsr      GESresp
674000990210      *    in filiale solo per profili abilitati
674100130806     c************       WHEN          %SUBST(knmus:1:3) = §STDpv1  or
674200130806     c************                     %SUBST(knmus:1:3) = §STDpv2
674300130806     c                   WHEN          §UTEmresca = 'S'
674400990208     c                   exsr      CARresp
674500990208     c                   exsr      GESresp
674600990209     c                   ENDSL
674700990208
674800990210      *  Aggiorno FNDCT
674900990210     c                   eval      DCTcar = Wcalresp
675000990614     c                   clear                   DCTft1
675100990614     c                   clear                   DCTft2
675200990210     c                   UPDATE    FNDCT000
675300990210
675400990208     c                   ENDSR
675500990208      **********************************************************************
675600990208      *  IMPOSTO DATI VIDEO RESPONSABILITA'
675700990208      **********************************************************************
675800990209     C     CARresp       BEGSR
675900990208
676000990209      * TESTATA
676100990208     c                   IF          I00mod = 'A'
676200990215     c                   z-add     DCTfil        vtcfil
676300990215     c                   z-add     DCTnca        vtcnca
676400990209     c                   z-add     Wdataca       VTCdc1
676500990208     C                   z-add     DCTfca        VTCfca
676600990209      *
676700990208     c                   CLEAR                   FIDN05DS
676800990208     c                   movel     'C'           I05MOD
676900990208     c                   z-add     VTCfca        I05fca
677000990208     C                   CALL      'FIDN05R'
677100990208     c                   PARM                    KPJBA
677200990208     c                   PARM                    FIDN05DS
677300990208     c                   If        O05err <> 'E'
677400990208     c                   movel     O05REC        DS_FNDFA
677500990208     c                   movel     DFAdes        VTDfca
677600990208     c                   Endif
677700990208      *
677800990209     C                   z-add     DCTgfc        VTCgfc
677900990208     C     VTCgfc        chain     AZORG                              31
678000990208     C  N31              if        ORGDE5 <> *BLANKS
678100990208     C                   movel     ORGDE5        VTDgfc
678200990208     C                   else
678300990208     C                   movel     ORGDES        VTDgfc
678400990208     C                   endif
678500990208      *
678600990208     c                   ENDIF
678700990208      *
678800990210     C                   z-add     DCTncn        VTCncn
678900990208     c                   IF        VTClna = *zeros
679000990208     C                   move      DCTlna        VTClna
679100990208     C     VTClna        chain     AZORG                              31
679200990208     C  N31              If        ORGDE5 <> *BLANKS
679300990208     C                   movel     ORGDE5        VTDlna
679400990208     C                   Else
679500990208     C                   movel     ORGDES        VTDlna
679600990208     C                   Endif
679700990208     C                   ENDIF
679800990210      *
679900990210     c                   SELECT
680000990210     c                   WHEN       Wcalresp = *blanks
680100990210     c                   eval      V6Cdca = 'Nessuna responsabilità'
680200990210     c                   WHEN       Wcalresp = 'M'
680300990210     c                   eval      V6Cdca = 'Manuale'
680400990210     c                   WHEN       Wcalresp = 'A'
680500990210     c                   eval      V6Cdca = 'Automatico'
680600990210     c                   OTHER
680700990210     c                   eval      V6Cdca = 'In attesa di ricalcolo'
680800990210     c                   ENDSL
680900990210
681000990209      * SUBFILE
681100990209      *    Pulisco Subfile
681200990209     C                   eval      *IN21 = *OFF
681300990209     C                   WRITE     FI03C05
681400990210     C                   eval      *IN21 = *ON
681500990210     C                   eval      *IN20 = *ON
681600990209     C                   z-add     *ZEROS        NRR5
681700990210      *
681800990216      *  Se non proteggo la videata carico segnacolli C.A.
681900990317     c                   IF            I00mod = 'A'                             apertura
682000990317     c                             or  V6Cmsg <> *blanks                        errori calcolo resp.
682100990317     c                             or  DCTcar = 'T'  or  DCTcar = 'R'           attesa di ricalcolo
682200990317     c                             or  *in91 = *ON                              escluso segnacollo
682300990216     c                   eval      *in91 =  *on
682400990216     c                   ELSE
682500990216     c                   clear                   JJ
682600990216     c     kca           setll     fndcd000
682700990216     c     kca           reade(N)  fndcd000                               31
682800990216     C     *IN31         DOWEQ     *OFF
682900990216     c                   if        DCDatb = *blanks
683000990216     c                   add       1             JJ
683100990216     c                   z-add     DCDnsc        SEC(jj)
683200990306     c                   endif
683300990216     c     kca           reade(N)  fndcd000                               31
683400990216     c                   ENDDO
683500990216     C                   ENDIF
683600990210      *
683700990217      *   Carico Subfile
683800051020     c                   if        NOT %open(FNDCR02L)
683900990217     c                   OPEN      FNDCR02L
684000051020     c                   endif
684100990209     c     Kca           CHAIN     FNDCR000                           30
684200990209      *
684300990209     c     *in30         DOWEQ     *off
684400990315      *
684500990315     c                   IF        DCRatb = *blanks
684600990315      *
684700990216     c                   movel     DCRflo        DDCR01
684800990217      *       Controllo se segnacollo numerico
684900990217     C                   clear                   V6Snsc
685000990217     C                   testn                   §DCRnsc              31
685100990217     C                   If        *IN31 = *on
685200990217     C                   move      §DCRnsc       W001A
685300990217     c                   eval      *in31 = (W001A >= '0')
685400990217     C   31              move      §DCRnsc       V6Snsc
685500990217     C                   Endif
685600990218     C                   movel     V6Snsc        V6Hnsc
685700990217     C                   movel     V6Snsc        W007a
685800990217      *
685900990209     c                   z-add     DCRper        V6Sper
686000990217     c                   z-add     DCRper        V6Hper
686100990218     C                   movel     V6Sper        W005a
686200990219     c                   if         DCRpor > *zeros
686300990210     c                   movel     DCRpor        V6Spor
686400990217     c                   movel     DCRpor        V6Hpor
686500990219     c                   else
686600990219     c                   clear                   V6Spor
686700990219     c                   clear                   V6Hpor
686800990219     c                   endif
686900990217      *
687000990209     C     DCRpor        chain     AZORG                              31
687100990217     c                   Select
687200990217     c                   When      *in31 = *on
687300990217     C                   clear                   V6Sdpo
687400990217     c                   When      ORGde5 <> *blanks
687500990217     C                   movel(P)  ORGde5        V6Sdpo
687600990217     c                   Other
687700990217     C                   movel(P)  ORGdes        V6Sdpo
687800990217     c                   Endsl
687900131105
688000131105     C                   clear                   V6Stpr
688100131105     C                   move      §DCRter       V6Stpr
688200131105     C                   move      V6Stpr        V6Htpr
688300990217      *
688400990209     C                   movel     DCRtcr        V6Htcr
688500990209     C                   clear                   V6Sdtc
688600990209     C                   exsr      decodTCR
688700990218      *
688800990217     C                   movel     §DCRtre       V6Stre
688900990217     C                   movel     §DCRtre       V6Htre
689000990217     C                   exsr      decodTRE
689100990217     C                   movel     §TREdesc      V6Sdtr
689200990217     C                   movel     §TREcalc      V6Hcal
689300990210      *
689400990209     c                   add       1             NRR5
689500131106     c                   eval      RS1(nrr5) =W007a+W005a+V6Spor+V6Stre+v6stpr
689600990209     C                   WRITE     FI03S05
689700990315      *
689800990315     c                   ENDIF
689900990210     c     Kca           READE     FNDCR000                               30
690000990315     c                   ENDDO
690100990210      *
690200990209      *    Scrivo rcd vuoti
690300990209     c                   IF        *in91 = *off
690400990210     c                   eval      W0040 = (NRR5 + 1)
690500000308     c                   eval      W0040B = (W0040 + 20)
690600990217     c                   clear                   V6Snsc
690700990217     c                   clear                   V6Hnsc
690800990217     C                   movel(p)  V6Snsc        W007a
690900990217     c                   clear                   V6Sper
691000990217     c                   clear                   V6Hper
691100990218     C                   movel(p)  V6Sper        W005a
691200990209     c                   clear                   V6Spor
691300990217     c                   clear                   V6Hpor
691400131105     c                   clear                   V6Stpr
691500131105     c                   clear                   V6Htpr
691600990209     C                   clear                   V6Sdpo
691700990209     C                   clear                   V6Sdtc
691800990217     c                   clear                   V6Htcr
691900990217     C                   clear                   V6Stre
692000990217     c                   clear                   V6Htre
692100990217     c                   clear                   V6Hcal
692200990217     C                   clear                   V6Sdtr
692300131106     c                   eval      W022a =  W007a+W005a+V6Spor+V6Stre+v6stpr
692400000308     c     W0040         DO        W0040B        NRR5
692500131105     c                   movel     W022a         RS1(nrr5)
692600990209     C                   WRITE     FI03S05
692700990209     c                   ENDDO
692800990209     c                   ENDIF
692900990209      *
693000990210     c                   SORTA     RS1
693100990210      *
693200990209     c                   ENDSR
693300990209      **********************************************************************
693400990209      *  EMETTO VIDEATA RESPONSABILITA'
693500990209      **********************************************************************
693600131106     C     GESresp       BEGSR
693700990208      *
693800990209      * I colli per responsabilità sono quelli danneggiati o se zero i colli spedizione
693900990209     c                   IF          VTCncn = *zeros
694000990209     c                   z-add     VTCncl        Wcolli
694100990209     c                   ELSE
694200990209     c                   z-add     VTCncn        Wcolli
694300990209     c                   ENDIF
694400990209      *
694500990209     c                   eval      *in28 = *off
694600990209     c                   eval      *inKF = *off
694700990209     c                   z-add     1             V6Cnrr
694800990209      *
694900990209     C     EMD05         TAG
695000990209      *
695100990219     c                   IF        *in28 = *ON
695200990219     c                   z-add     nrr5          V6Cnrr
695300990219     c                   ELSE
695400990219     c                   z-add     1             V6Cnrr
695500990219     c                   ENDIF
695600990218      *
695700990209     c                   write     FI03T05
695800990209     c                   write     FI03Z05
695900990209     c                   EXFMT     FI03C05
696000990209      *
696100990209     c                   eval      *in28 = *off
696200990219     c                   eval      *in29 = *off
696300990210     c                   clear                   V6Cmsg
696400990209     c                   clear                   nrr5
696500990209      *
696600990317     c                   IF        *IN91 = *off
696700990317     c                   exsr      CTRD05
696800990317     c                   ENDIF
696900990209      *
697000990219     c                   If        *in28 = *on  or  *in29 = *on  or
697100990210     c                             (*in91 = *off  and  *inKf = *off)
697200990209     c                   goto      emd05
697300990209     c                   Endif
697400990210      *
697500990210      * Se la responsabilità è stata modificata aggiorno FNDCR
697600990210     c                   IF        *IN91 = *off
697700000307     c                   DO        900           jj
697800990218     c                   if        RS1(jj) <> RS2(jj)
697900990210     c                   exsr      WRTDCR
698000000307     c                   eval      jj = 900
698100990210     c                   eval      Wcalresp = 'M'
698200990210     c                   endif
698300990210     c                   ENDDO
698400990210     c                   ENDIF
698500990210      *
698600990208     c                   ENDSR
698700990209      *****************************************************************
698800990209      *  CONTROLLI VIDEO 5
698900990209      *****************************************************************
699000990209     C     CTRD05        BEGSR
699100990209      *
699200990217     c                   clear                   PER
699300990218     c                   clear                   RSU
699400990218     c                   clear                   RS2
699500990209     c                   z-add     1             nrr5
699600990209      *
699700990209     C     nrr5          chain     FI03S05                            3232
699800990209      *
699900990209    1C     *IN32         DOWEQ     *OFF
700000990209      *
700100990209     C                   eval      *in92 = *off
700200990209     C                   eval      *in93 = *off
700300990219     C                   eval      *in94 = *off
700400990219     C                   eval      *in95 = *off
700500131105     C                   eval      *in96 = *off
700600990209      *
700700990217      * clearizzo se eliminato campo
700800990217     c                   exsr      cleard05
700900990217      *
701000990218      *   Controllo che il segnacollo sia nei selezionati
701100990218     c                   z-add     1             jj
701200990218     c     V6Snsc        Lookup    SEC(jj)                                30
701300990218      *
701400990218     c                   IF          *IN30 = *OFF
701500990218     c                   eval      *in28 = *on
701600990218     c                   eval      *in92 = *on
701700990218     c                   movel     MSG(30)       V6Cmsg
701800990218     c                   ELSE
701900990218      *
702000990218      *  Memorizzo colli per segnacollo
702100990218     c                   add       V6Sper        PER(jj)
702200990218     c                   z-add     V6Snsc        V6Hnsc
702300990218     C                   ENDIF
702400990217     C   28              goto      ECTR05
702500120619     c
702600120619     c* controllo che il totale punti respsonsabilità per segnacollo non siano superiori al
702700120619     c*  massimo previsto che è 3
702800120619     c                   if        v6snsc>0 and per(jj)>§stdpermax
702900120619     c                   eval      *in28 = *on
703000120619     c                   eval      *in92 = *on
703100120619     c                   movel     MSG(120)      V6Cmsg
703200120619     c                   eval      %subst(v6cmsg:73:6)=%editc(§stdpermax:'4')
703300120619     C                   goto      ECTR05
703400120619     C                   ENDIF
703500990209      *
703600990217      * Se variato/inserito il P.O. controllo
703700990209     c                   IF        V6Spor <> V6Hpor
703800131106     c                   eval      *in29=*on
703900990217     c                   exsr      CTR_POresp
704000990209     c                   ENDIF
704100990209     C   28              goto      ECTR05
704200131105      *
704300990217      *
704400990217      * Se variato/inserito il Tipo Attribuzione controllo
704500990217     c                   IF         V6Stre <> V6Htre
704600990217     C                   exsr      decodTRE
704700990217      *
704800990217     c                   IF           §TREdesc = *blanks
704900990217     c                   eval       *in28 = *on
705000990217     c                   eval       *in95 = *on
705100990218     c                   movel     MSG(92)       V6Cmsg
705200990217     c                   ELSE
705300990217     C                   movel     V6Stre        V6Htre
705400990217     C                   movel     §TREdesc      V6Sdtr
705500990217     C                   movel     §TREcalc      V6Hcal
705600990217     c                   ENDIF
705700990217     c                   ENDIF
705800990219     c                   IF         *IN28 = *ON  or  *IN29 = *ON
705900990219     C                   goto      ECTR05
706000990219     c                   ENDIF
706100131106      * Se variato/inserito il terminal controllo
706200131107      * Se vuoto lo calcolo in base al p.o. responsabile
706300131106     c                   IF        V6Stpr <> V6Htpr  or (v6stpr=*blanks
706400131106     c                             or v6stpr='000')
706500131106     c                   exsr      CTR_TPresp
706600131106     c                   ENDIF
706700131106     c                   IF         *IN28 = *ON  or  *IN29 = *ON
706800131106     C                   goto      ECTR05
706900131106     c                   endif
707000990209      *
707100990218      * controllo: se inseriti dati parziali, se dati corretti per il tipo attribuzione,
707200990218      *            se inserimento univoco per segnacollo-P.O.-Attribuzione
707300990218     C                   exsr      ctrd05_con
707400990218     C   28              goto      ECTR05
707500990217      *
707600990217      * Memorizzo inserimenti per controllo variazioni
707700990217     C                   movel     V6Snsc        W007a
707800990218     C                   movel     V6Sper        W005a
707900131107     c                   eval      RS2(nrr5) =W007a+W005a+V6Spor+V6Stre+ v6stpr
708000990209      *
708100990209     C     ECTR05        TAG
708200990209     C                   UPDATE    FI03S05
708300990209      *
708400990212     c                   add       1             nrr5
708500990210      *
708600000308     c                   IF        *IN28 = *ON  or  *IN29 = *ON
708700990210     c                   eval        *IN32 = *ON
708800990210     c                   ELSE
708900990210     c     nrr5          chain     FI03S05                            32
709000990210     c                   ENDIF
709100990209      *
709200990209    1C                   ENDDO
709300990218      *
709400990218      * Controllo: che i colli inseriti corrispondano ai colli danneggiati,
709500990218      *            che per segnacollo sia inserito un collo.
709600990219     c                   IF         *IN28 = *OFF  and  *IN29 = *OFF
709700990218     c                   xfoot     PER           W0133
709800090303     c                   IF         Wcolli > W0133
709900990218     C                   eval      *in28 = *on
710000990219     C                   eval      nrr5 = 1
710100990218     C                   movel     MSG(60)       V6Cmsg
710200990218     c                   ELSE
710300120619     c
710400120619     c* controllo anche i punti in totale
710500120619     c                   eval      ToTpunti=Wcolli * §stdrsval
710600120619     c                   if        w0133>totpunti
710700120619     C                   eval      *in28 = *on
710800120619     C                   eval      nrr5 = 1
710900120619     C                   movel     MSG(121)      V6Cmsg
711000120619     c                   eval      %subst(v6cmsg:57:6)=%editc(§stdpermax:'4')
711100120619     c                   endif
711200990218      *
711300990209     c                   ENDIF
711400990218     c                   ENDIF
711500990210      *
711600990210     c  N28              SORTA     RS2
711700990209      *
711800990209     c                   ENDSR
711900990210      *****************************************************************
712000990217      *  CLEARIZZO SE ELIMINATI CAMPI
712100990210      *****************************************************************
712200990217     C     CLEARD05      BEGSR
712300990217      *
712400990217      * Segnacollo
712500990217     c                   IF        V6Snsc = *zeros  and  V6Hnsc <> *zeros
712600990217     c                   clear                   V6Hnsc
712700990217     c                   ENDIF
712800990217      *
712900990217      * Colli
713000990217     c                   IF        V6Sper = *zeros  and  V6Hper <> *zeros
713100990217     c                   clear                   V6Hper
713200990217     c                   ENDIF
713300990217      *
713400990217      * P.O.
713500990217     c                   IF        V6Spor = *zeros
713600990217     c                   clear                   V6Spor
713700990217     c                   ENDIF
713800990217     c                   IF        V6Spor = *blanks  and  V6Hpor <> *blanks
713900990217     c                   clear                   V6Hpor
714000990217     c                   clear                   V6Sdpo
714100990217     c                   clear                   V6Sdtc
714200990217     c                   clear                   V6Htcr
714300990217     c                   ENDIF
714400131105      * Terminal
714500131105     c                   IF        V6Stpr = *zeros
714600131105     c                   clear                   V6Stpr
714700131105     c                   ENDIF
714800131105     c                   IF        V6Stpr = *blanks  and  V6Htpr <> *blanks
714900131105     c                   clear                   V6Htpr
715000131105     c                   ENDIF
715100990217      *
715200990217      * Tipo Attribuzione
715300990217     c                   IF        V6Stre = *blanks  and  V6Htre <> *blanks
715400990217     c                   clear                   V6Htre
715500990217     c                   clear                   V6Sdtr
715600990217     c                   clear                   V6Hcal
715700990217     c                   ENDIF
715800990217      *
715900990217     c                   ENDSR
716000990217      *****************************************************************
716100990217      *  CONTROLLO P.O. PER RESPONSABILITA
716200990217      *****************************************************************
716300990217     C     CTR_PORESP    BEGSR
716400990217      *
716500990217      *    Controllo se immesso un numero valido
716600990217     c                   clear                   W0030
716700990217     C                   testn                   V6Spor               30
716800990217      *
716900990217     C                   If          *IN30 = *on
717000990217     C                   move      V6Spor        W001A
717100990217     C                   If           W001A >= '0'
717200990217     c                   movel     V6Spor        W0030
717300990217     c                   eval      *in30 = (W001A >= '0')
717400990217     C                   Endif
717500990217     C                   Endif
717600990217      *
717700990217     C     W0030         chain     AZORG                              31
717800990217      *
717900990217     c                   IF        *IN31 = *ON  or  ORGfva <> *blanks
718000990217     c                              or  (ORGfag <> 'F'  and  ORGfag <> 'A')
718100990217     C                   eval      *in94 = *on
718200990217     C                   eval      *in28 = *on
718300990217     C                   movel     MSG(58)       V6Cmsg
718400990217     c                   ELSE
718500990217      *
718600990217     c                   movel     V6Spor        V6Hpor
718700990217     c                   If          ORGde5 <> *blanks
718800990217     C                   movel(P)  ORGde5        V6Sdpo
718900990217     c                   Else
719000990217     C                   movel(P)  ORGdes        V6Sdpo
719100990217     c                   Endif
719200131106
719300131106     c* ricalcolo il terminal del p.o. responsabile
719400131106     c                   exsr      CAL_ter
719500990217      *
719600990217     C                   SELECT
719700090324     c                   WHEN      W0030 = DCTlna  or  W0030 = i50tfa
719800090324     c                             or w0030=i50lna
719900990217     c                   movel     'A'           V6Htcr
720000090324     c                   WHEN      W0030 = Wlnp    or  W0030 = i50tfp
720100090324     c                             or w0030=i50lnp or  w0030 = wtfplnp
720200990217     c                   movel     'P'           V6Htcr
720300990217     c                   OTHER
720400990217     c                   movel     'T'           V6Htcr
720500990217     c                   ENDSL
720600990217      *
720700990217     c                   clear                   V6Sdtc
720800990217     c                   Exsr      decodtcr
720900990217      *
721000990217     c                   ENDIF
721100990217      *
721200990217     c                   ENDSR
721300131105      *****************************************************************
721400131105      *  CONTROLLO Terminal del P.O. Responsabile
721500131105      *****************************************************************
721600131105     C     CTR_TPRESP    BEGSR
721700131105      *
721800131106     c* se è vuoto ed è presente il p.o responsabile
721900131106     c* calcolo il terminal in base al p.o. respoinsabile
722000131106     c                   if        v6stpr=*blanks or v6stpr=*zeros
722100131106     c                   if        v6spor>*zeros
722200131106     c                   exsr      cal_ter
722300131106     c                   endif
722400131106     c                   else
722500131105      *    Controllo se immesso un numero valido
722600131105     c                   clear                   W0030
722700131105     C                   testn                   V6Stpr               30
722800131105      *
722900131105     C                   If          *IN30 = *on
723000131105     C                   move      V6Stpr        W001A
723100131105     C                   If           W001A >= '0'
723200131105     c                   movel     V6Stpr        W0030
723300131105     c                   eval      *in30 = (W001A >= '0')
723400131105     C                   Endif
723500131105     C                   Endif
723600131105      *
723700131105     C     W0030         chain     AZORG                              31
723800131105      *
723900131105     c                   IF        *IN31 = *ON  or  ORGfva <> *blanks
724000131105     c                              or  (ORGfag <> 'F'  and  ORGfag <> 'A')
724100131105     C                   eval      *in96 = *on
724200131105     C                   eval      *in28 = *on
724300131105     C                   movel     MSG(58)       V6Cmsg
724400131105     c                   ELSE
724500131105      *
724600131105
724700131105     c* controllo che sia un terminal
724800131106
724900131106     c* provo con ter_part
725000131106     c                   clear                   wok_ter
725100131106     c                   eval      kepa='P'
725200131106     c     kcaeP         setll     azcae05l
725300131106     c     kcaeP         reade     azcae05l
725400131106     c                   dow       not%eof(azcae05l) and wok_ter=*blanks
725500131106     c                   if        caeatb=*blanks
725600131106     c                   eval      wok_ter='S'
725700131106     c                   endif
725800131106     c     kcaeP         reade     azcae05l
725900131106     c                   enddo
726000131106     c* se non trovato come ter-part cerco come ter_arr
726100131106     c                   if        wok_ter=*blanks
726200131106     c                   eval      kepa='A'
726300131106     c     kcaeA         setll     azcae05l
726400131106     c     kcaeA         reade     azcae05l
726500131106     c                   dow       not%eof(azcae05l) and wok_ter=*blanks
726600131106     c                   if        caeatb=*blanks
726700131106     c                   eval      wok_ter='S'
726800131106     c                   endif
726900131106     c     kcaeA         reade     azcae05l
727000131106     c                   enddo
727100131106     c                   endif
727200131106     c                   if        wok_ter=*blanks
727300131105     C                   eval      *in96 = *on
727400131105     C                   eval      *in28 = *on
727500131105     C                   movel     MSG(122)      V6Cmsg
727600131105     c                   endif
727700131105      *
727800131105      *
727900131105     c                   ENDIF
728000131106     c                   ENDIF
728100131107     c                   if        *in96=*off
728200131107     c                   movel     V6Stpr        V6Htpr
728300131107     c                   endif
728400131105      *
728500131105     c                   ENDSR
728600990218      *****************************************************************
728700990218      * CONTROLLI DI CONGRUITA' DEI DATI INSERITI
728800990218      *****************************************************************
728900990218     C     CTRD05_con    BEGSR
729000990218      *
729100990218     c                   SELECT
729200990218      *
729300990218     c                   WHEN        V6Hcal <> 'N'  and  V6Snsc = *zeros  and
729400990218     c                               V6Sper = *zeros  and  V6Spor = *blanks  and
729500131106     c                               V6Stre = *blanks  and v6stpr= *blanks
729600990218      *
729700990218     c                   WHEN        V6Hcal = 'N'  and  V6Snsc > *zeros
729800990218     C                   eval      *in92 = *on
729900990218     C                   eval      *in28 = *on
730000990218     C                   movel     MSG(95)       V6Cmsg
730100990218      *
730200990218     c                   WHEN        V6Hcal = 'N'  and  V6Sper = *zeros
730300990218     C                   eval      *in93 = *on
730400990218     C                   eval      *in28 = *on
730500990218     C                   movel     MSG(59)       V6Cmsg
730600120619     c
730700120802     c*****              WHEN        V6Sper > §stdpermax
730800120802     c                   WHEN        v6snsc>0 and V6Sper > §stdpermax
730900120619     C                   eval      *in93 = *on
731000120619     C                   eval      *in28 = *on
731100120619     C                   movel     MSG(120)      V6Cmsg
731200120619     c                   eval      %subst(v6cmsg:73:6)=%editc(§stdpermax:'4')
731300990218      *
731400990218     c                   WHEN        V6Hcal = 'N'  and  V6Spor <> *blanks
731500990218     C                   eval      *in94 = *on
731600990218     C                   eval      *in28 = *on
731700990218     C                   movel     MSG(96)       V6Cmsg
731800990218      *
731900990218     c                   WHEN        V6Hcal = 'N'  or
732000990218     c                               (V6Sper > *zeros  and  V6Spor <> *blanks
732100990218     c                                                 and  V6Stre <> *blanks)
732200990218     C                   movel     V6Snsc        W007a
732300990218     c                   eval      W014a =  W007a + V6Spor + V6Stre
732400990218     c     W014a         Lookup    RSU                                    28
732500990218     c                   If          *IN28 = *ON
732600990218     C                   eval      *in92 = *on
732700990218     C                   movel     MSG(94)       V6Cmsg
732800990218     c                   Else
732900990218     c                   z-add     1             JJ
733000990218     c     *blanks       Lookup    RSU(jj)                                30
733100990218     c                   movel     W014a         RSU(jj)
733200990218     c                   Endif
733300990218      *
733400990218     c                   WHEN        V6Sper = *zeros
733500990218     C                   eval      *in93 = *on
733600990218     C                   eval      *in28 = *on
733700990218     C                   movel     MSG(59)       V6Cmsg
733800120619      *
733900990218     c                   WHEN        V6Spor = *blanks
734000990218     C                   eval      *in94 = *on
734100990218     C                   eval      *in28 = *on
734200990218     C                   movel     MSG(59)       V6Cmsg
734300990218      *
734400990218     c                   WHEN        V6Stre = *blanks
734500990218     C                   eval      *in95 = *on
734600990218     C                   eval      *in28 = *on
734700990218     C                   movel     MSG(59)       V6Cmsg
734800990218      *
734900990218     c                   ENDSL
735000990218      *
735100990218     c                   ENDSR
735200990217      *****************************************************************
735300990217      *  SCRIVO FILE RESPONSABILITA'
735400990217      *****************************************************************
735500990217     C     WRTDCR        BEGSR
735600990217      *
735700990315      * Annullo rcd esistenti
735800990315     c     KCA           chain     fndcr000                           30
735900990315     c                   DOW        *IN30 = *Off
736000990317     c                   If        DCRatb = *blanks
736100990317     c                   eval      DCRatb = 'A'
736200990315     c                   clear                   dcrftr
736300051019     c                   eval      DCRdtr = DateU
736400990315     c                   update    fndcr000
736500990315     c                   Endif
736600990315     c     KCA           reade     fndcr000                               30
736700990210    1C  N30              ENDDO
736800990210      *
736900990210     c                   z-add     1             nrr5
737000990210      *
737100990210     C     nrr5          chain     FI03S05                            3232
737200990210      *
737300990210    1C     *IN32         DOWEQ     *OFF
737400990210      *
737500990210     c                   IF        V6Sper <> *zeros
737600990315      *
737700990326     c                   if        V6Snsc = *zeros
737800990326     c                   clear                   WV6Snsc
737900990326     c                   else
738000990315     c                   movel     V6Snsc        WV6Snsc
738100990326     c                   endif
738200990315     c                   movel     V6Spor        WV6Spor
738300990326      *
738400990315     c     KDCR          CHAIN     fndcr000                           30
738500990315      *
738600990315     c                   clear                   FNDCR000
738700051019     c*** già clearato:  clear                   DCRatb
738800990210     c                   z-add     DCTaac        DCRaac
738900990210     c                   z-add     DCTfil        DCRfil
739000990210     c                   z-add     DCTnca        DCRnca
739100990210     c                   movel     V6Spor        DCRpor
739200990210     c                   z-add     V6Sper        DCRper
739300990210     c                   movel     V6Htcr        DCRtcr
739400990219     c                   clear                   DDCR01
739500990219     c                   if         V6Snsc > *zeros
739600990219     c                   movel     V6Snsc        §DCRnsc
739700990219     c                   endif
739800990219     c                   movel     V6Stre        §DCRtre
739900090417     c
740000131106     C***  dcrpor        chain     AZORG                              31
740100131106     c***                movel     orgde3        og143
740200131106     c***31              clear                   og143
740300090324     c
740400090324     c* Calcolo il terminal responsabile e tipo
740500131106     c***R               select
740600090325     c* se p.o. estero memorizzo se stesso
740700131106     c***R               when      §ogntw='EEX' or §ogntw='DPD' or
740800131106     c***R                         §ogntw='FED'
740900131106     c***R               movel     dcrpor        §dcrter
741000131106     c***R               when      dcrtcr='A'
741100131106     c***R               movel     i50tfa        §dcrter
741200131106     c***R               when      dcrtcr='P' and (dcrpor=wlnp or
741300131106     c***R                                         dcrpor=wtfplnp)
741400131106     c***R               movel     wtfplnp       §dcrter
741500131106     c***R               when      dcrtcr='P' and (dcrpor=i50lnp or
741600131106     c***R                                         dcrpor=i50tfp )
741700131106     c***R               movel     i50tfp        §dcrter
741800131106     c***R
741900131106     c***R               other
742000131106     c***R               clear                   fnlv55ds
742100131106     c***R               movel     'A'           d55tpt
742200131106     c***R               movel     dcrpor        d55lin
742300131106     c***R               if        ds_arbdat>0
742400131106     c***R               z-add     ds_arbdat     d55drf
742500131106     c***R               else
742600131106     c***R               move      dctmgc        d55drf
742700131106     c***R               movel     dctaac        d55drf
742800131106     c***R               endif
742900131106     c***R               call      'FNLV55R'
743000131106     c***R               parm                    fnlv55ds
743100131106     c***R               movel     d55tfa        §dcrter
743200131106      ***R
743300131106     c***R               endsl
743400131106     c***R
743500131106     c                   movel     v6stpr        §dcrter
743600131106     c                   movel     DDCR01        DCRflo
743700051019     c*** già clearato:  clear                   DCRftr
743800051019     c*** già clearato:  clear                   DCRdtr
743900990315      *
744000990315     c                   if        *in30 = *ON
744100990210     c                   WRITE     FNDCR000
744200990315     c                   else
744300990315     c                   UPDATE    FNDCR000
744400990315     c                   endif
744500990315      *
744600990210     c                   ENDIF
744700990210      *
744800990210     c                   add       1             nrr5
744900990210     c     nrr5          chain     FI03S05                            32
745000990210    1C                   ENDDO
745100990210      *
745200990210     c                   ENDSR
745300050523      *---------------------------------------------------------------*
745400050523      * Invio e-mail x perizia per avaria                             *
745500050523      *---------------------------------------------------------------*
745600050523     c     sr_InviaMail  BEGSR
745700050601
745800081110      * Reperisco dati in tabella mra per OVRTPRTF in caso di Avaria
745900081110      *
746000081110    1c                   If        §TADragr = 'V'
746100050907     c                   CLEAR                   TIBS02ds
746200050601     c                   clear                   trtcm1ds
746300050907     c                   movel     'C'           t02mod
746400050601     c                   movel     KNSIF         T02sif
746500050601     c                   movel     'MRA'         T02cod
746600050601     c                   movel(p)  'FIDN03R'     T02ke1
746700050601     c                   call      'TIBS02R'
746800050601     c                   parm                    KPJBA
746900050601     c                   parm                    TIBS02ds
747000081110    2c                   if        T02err = *blanks
747100050601     c                   movel     T02uni        dmradan
747200050601     c                   eval      §cm1dst = §mraddest
747300050601     c                   move      dutpou        §cm1po
747400050601     c                   move      '0'           §cm1sts
747500050601     c                   movel     §mradreg      §cm1tips
747600050601     c                   movel     §mradidpro    §cm1idp
747700050601     c                   eval      §cm1mitt = §mradmitt
747800050601     c                   eval      §cm1var  =  '*OBJM*Richiesta PERIZIA +
747900050601     c                                         per avaria'
748000170607     c                                      +  ' n° C.A. ' +
748100170607     c                             %editc( DCTfil : 'X' ) + '/' +
748200170607     c                             %trim( %editc( DCTnca : '3' ) ) +
748300170607     c                             ' del ' + %editc( DCTaac : '3' )
748400081110    2c                   endif
748500081110    1c                   ENDIF
748600081110
748700081110      * Reperisco dati in tabella mra per OVRTPRTF in caso per Ammanco / Mancanza
748800081110      *
748900081110    1c                   If        §TADragr = 'A' or §TADragr = 'M'
749000081110     c                   CLEAR                   TIBS02ds
749100081110     c                   clear                   trtcm1ds
749200081110     c                   movel     'C'           t02mod
749300081110     c                   movel     KNSIF         T02sif
749400081110     c                   movel     'MRA'         T02cod
749500081110     c                   eval      T02ke1 ='FIDN03R-AMM/MAN'
749600081110     c                   call      'TIBS02R'
749700081110     c                   parm                    KPJBA
749800081110     c                   parm                    TIBS02ds
749900081110    2c                   if        T02err = *blanks
750000081110     c                   movel     T02uni        dmradan
750100081110     c                   eval      §cm1dst = §mraddest
750200081110     c                   move      dutpou        §cm1po
750300081110     c                   move      '0'           §cm1sts
750400081110     c                   movel     §mradreg      §cm1tips
750500081110     c                   movel     §mradidpro    §cm1idp
750600081110     c                   eval      §cm1mitt = §mradmitt
750700081110     c                   eval      §cm1var  =  '*OBJM*Ammanchi/mancanze +
750800081110     c                                         rilevanti'
750900170607     c                                      +  ' n° C.A. ' +
751000170607     c                             %editc( DCTfil : 'X' ) + '/' +
751100170607     c                             %trim( %editc( DCTnca : '3' ) ) +
751200170607     c                             ' del ' + %editc( DCTaac : '3' )
751300081110    2c                   endif
751400081110    1c                   ENDIF
751500050601      *
751600050601      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
751700050601      *   TRAMITE E-mail all'assicurazione
751800050601      *
751900050601
752000050601      * eseguo ovprtf
752100081110    1c                   If        T02err = *blanks
752200050601     c                   eval      %subst(cmm(1): 23: 10) = §MRADOUTQI
752300050601     c                   eval      Qcomm = %trim(cmm(1)) + trtcm1ds
752400050601     c                             + ''')'
752500050601     c                   z-add     395           Qleng
752600050601     c                   CALL      'QCMDEXC'                            90
752700050601     c                   PARM                    Qcomm
752800050601     c                   PARM                    Qleng
752900050601     c
753000050601      *
753100050523      *
753200050523      * Preparo la mail da inviare
753300050523     c                   exsr      sr_PrintMail
753400050523      * DltOvr
753500050601     c                   clear                   Comman
753600050523     c                   call      'QCMDEXC'                            99
753700050523     c                   parm      C_Cmd_1       Comman
753800050523     c                   parm                    Lenght
753900050523      *
754000050523e   1c                   ENDIF
754100050523      *
754200050523     c                   ENDSR
754300050523      *---------------------------------------------------------------*
754400050523      * Prepara e-mail x perizia per avaria                           *
754500050523      *---------------------------------------------------------------*
754600050523     c     sr_PrintMail  BEGSR
754700050523      *
754800050523     c                   open      FIDN03P
754900050523      *
755000050523     c                   clear                   DN03PT1
755100081111     c                   If        §tadragr <> 'V'
755200081111     c                   eval      *in52 = *on
755300081111     c                   endif
755400050523      *
755500050523     c                   eval      PT1fil = DCTfil
755600050523     c                   select
755700050523     c                   when      DCTfil = Wlnp
755800050523     c                   movel     Save_PO_P     DS_PO
755900050523     c                   eval      PT1pod = ORGdes
756000050523     c                   when      DCTfil = Wlna
756100050523     c                   movel     Save_PO_A     DS_PO
756200050523     c                   eval      PT1pod = ORGdes
756300050523     c                   endsl
756400050523     c                   eval      PT1nca = DCTnca
756500050907     c                   clear                   WLBdat
756600050907     c                   eval      G02inv = (DCTaac * 10000) + DCTmgc
756700050907     c                   eval      G02err = '3'
756800050907     c                   call      'XSRDA8'
756900050907     c                   parm                    WLBdat
757000050907     c                   eval      PT1dca = G02dat
757100050523     c                   movel     Save_PO_A     DS_PO
757200050523     c                   eval      PT1poa = ORGdes
757300050523     c                   eval      PT1poi = ORGind
757400050523     c                   eval      PT1poc = ORGcpf
757500050523     c                   eval      PT1pol = ORGloc
757600050523     c                   eval      PT1pop = ORGpro
757700050523     c                   eval      PT1pot = ORGtel
757800050523     c                   eval      PT1pof = ORGfax
757900081111     c                   eval      PT1ipv = DCTipv
758000050523      *
758100050523     c                   write     DN03PT1
758200050523      *
758300050523     c                   close     FIDN03P
758400050523      *
758500050523     c                   ENDSR
758600080118      *---------------------------------------------------------------*
758700080118      * Invio e-mail x avviso danno a FedEx                           *
758800080118      *---------------------------------------------------------------*
758900080118     c     sr_CallFIDN38 BEGSR
759000080118      *
759100080118     c                   movel(p)  FIDN00ds      KPJBU
759200080121     c                   call      'FIDN38R'
759300080118     c                   parm                    KPJBA
759400080118     c                   movel     KPJBU         FIDN00ds
759500080118      *
759600080118     c                   if        o00err  <> *blanks
759700080118     c                   eval      o00msg = %trim(msg(73))
759800080118     c                                    + ' ' + o00msg
759900080118     c                   eval      o00err = 'E'
760000080118e   1c                   endif
760100080118      *
760200080118     c                   ENDSR
760300130802
760400130802      *---------------------------------------------------------------*
760500130802      * Invio e-mail x avviso danno a Partner VanDuuren               *
760600130802      *---------------------------------------------------------------*
760700130802     c     sr_CallFIDN39 BEGSR
760800130802
760900130802     c                   movel(p)  FIDN00ds      KPJBU
761000130802     c                   call      'FIDN39R'
761100130802     c                   parm                    KPJBA
761200130802     c                   movel     KPJBU         FIDN00ds
761300130802
761400130802     c                   if        o00err  <> *blanks
761500130802     c                   eval      o00msg = %trim(msg(73))
761600130802     c                                    + ' ' + o00msg
761700130802     c                   eval      o00err = 'E'
761800130802     c                   endif
761900130802
762000130802     c                   ENDSR
762100141007      *
762200141007      *---------------------------------------------------------------*
762300141007      * Invio e-mail x avviso danno a Partner UK                      *
762400141007      *---------------------------------------------------------------*
762500141007     c     sr_CallFIDN41 BEGSR
762600141007      *
762700141007     c                   movel(p)  FIDN00ds      KPJBU
762800141007     c                   call      'FIDN41R'
762900141007     c                   parm                    KPJBA
763000141007     c                   movel     KPJBU         FIDN00ds
763100141007      *
763200141007     c                   if        o00err  <> *blanks
763300141007     c                   eval      o00msg = %trim(msg(73))
763400141007     c                                    + ' ' + o00msg
763500141007     c                   eval      o00err = 'E'
763600141007     c                   endif
763700141007      *
763800141007     c                   ENDSR
763900160518      *
764000160518      *---------------------------------------------------------------*
764100160518      * Invio e-mail x avviso danno a Partner (EuroExpress)           *
764200160518      * · 311     = GEL Express Logistik GmbH                         *
764300160518      * · 333     = France Express Lyon Corbas                        *
764400160518      * · 334     = Calexpress Gennevilliers                          *
764500160518      * · 340-345 = Azkar                                             *
764600160518      *---------------------------------------------------------------*
764700160518     c     sr_CallFIDN68 BEGSR
764800160518      *
764900160518     c                   movel(p)  FIDN00ds      KPJBU
765000160518     c                   call      'FIDN68R'
765100160518     c                   parm                    KPJBA
765200160518     c                   movel     KPJBU         FIDN00ds
765300160518      *
765400160518     c                   if        o00err  <> *blanks
765500160609     c*//                eval      o00msg = %trim(msg(73))
765600160609     c*//                                 + ' ' + o00msg
765700160609     c*//                eval      o00err = 'E'
765800160609      * -?Meglio NON segnalare errori (tipo "e-mail mancante") a chi?
765900160609      *  ?NON sarebbe in grado né di capire né di risolvere...?
766000160609     c                   clear                   o00err
766100160609     c                   clear                   o00msg
766200160518     c                   endif
766300160518      *
766400160518     c                   ENDSR
766500130802
766600080516      *---------------------------------------------------------------*
766700080516      * Invio lettera al cliente beneficiario della L.T. fase 100     *
766800080516      *---------------------------------------------------------------*
766900080516     c     sr_CallFIDN67 BEGSR
767000080516      *
767100080516     c                   clear                   o00err
767200080516     c                   clear                   o00msg
767300080516     c                   movel(p)  FIDN00ds      KPJBU
767400080516     c                   call      'FIDN67R'
767500080516     c                   parm                    KPJBA
767600080516     c                   movel     KPJBU         FIDN00ds
767700080516      *
767800080516     c                   if        o00err  <> *blanks
767900080516     c                   eval      o00msg = %trim(msg(73))
768000080516     c                                    + ' ' + o00msg
768100080516     c                   eval      o00err = 'E'
768200080516e   1c                   endif
768300080516      *
768400080516     c                   ENDSR
768500130404
768600130404      /free
768700130404       //--------------------------------------------------------------
768800130404       //?Chiusura automatica C.A.                                     ?
768900130404       //--------------------------------------------------------------
769000130404       BEGSR  chiudiCA;
769100130404
769200130404       //?Posso chiudere la C.A. solo se il Beneficiario è presente sul file
769300130404       //?dei clienti con chiusura automatica C.A.
769400130404         SELECT;
769500130404
769600130404       //?Codice beneficiario inserito dall'utente
769700130404           WHEN  DCTksc > 0;
769800130404             wKSC = DCTksc;
769900130404
770000130404       //?Codice beneficiario = Mittente
770100130404           WHEN  DCTptr = 'M';
770200130404             wKSC = VTCksm;
770300130404
770400130404       //?Codice beneficiario = Mittente
770500130404           WHEN  DCTptr = 'D';
770600130404             wKSC = VTCksd;
770700130404         ENDSL;
770800130404
770900130404       //?Cerco il beneificario sul file
771000130404         exec sql
771100130404         SELECT DKLcod into :DKLcod
771200130404         FROM FNDKL00F
771300130404         WHERE DKLksc = :wKSC;
771400130404       //?non trovo il rcd corrispondente vado a fine
771500130404         IF sqlcod <> 0;
771600130404           leavesr;
771700130404         ENDIF;
771800130404
771900130404       //?Recupero i dati da tabella DKL
772000130404         clear dDKL;
772100130404         clear TIBS02ds;
772200130404         T02mod = 'C';
772300130404         T02cod = 'DKL';
772400130404         T02sif = KNSIF;
772500130404         T02ke1 = %editc(DKLcod:'X');
772600130404         TNTBE_RicercaControllo  (kpjba : tibs02ds);
772700130405         IF  T02err <> *blanks;
772800130405           leavesr;
772900130404         ENDIF;
773000130405         dDKL = T02uni;
773100130404
773200130404       //?Ultimo numero prelevato dal Numeratore
773300130404         clear TRUL33DS;
773400130404         I33cnu = §DKLcnu;
773500130404         I33ope = 1;
773600130405         I33aaa = DCTaac;
773700130404         kpjbu = TRUL33DS;
773800130404         trul33r (KPJBA);
773900130404         TRUL33DS = kpjbu;
774000130404
774100130404       //?Non sono ancora arrivato al numero MAX di c.a. da chiudere
774200130405         IF  O33nrf < §DKLnca;
774300130404         //?Chiudo la C.A.
774400130404           clear FIDN22DS;
774500130404           I22tri = 'R';
774600130404           I22mod = 'C';
774700130404           I22aac = DCTaac;
774800130404           I22fil = DCTfil;
774900130404           I22nca = DCTnca;
775000130404           I22cch = §DKLcau;
775100130408           I22fgs = DCTgfc;
775200130404           kpjbu = FIDN22DS;
775300130404           fidn22r (kpjba);
775400130404           IF  O22err <> *blanks;
775500130404             leavesr;
775600130404           ENDIF;
775700130404         //?Avanzo il numeratore
775800130404           clear I33ope;
775900130405           I33aaa = DCTaac;
776000130404           I33num = 1;
776100130404           kpjbu = TRUL33DS;
776200130404           trul33r (KPJBA);
776300130408
776400130408         //?Scrivo nota
776500130408           clear wstd ;
776600130408           wnot = Comm_chiudi;
776700130409           exsr wrtdcs_3;
776800130404         ENDIF;
776900130404
777000130404       ENDSR;
777100130404
777200130404      /end-free
777300130404
777400990209      *****************************************************************
777500990311      *  CONTROLLO SE CI SONO IDD APERTI
777600990209      *****************************************************************
777700990311     C     CTR_IDD       BEGSR
777800990209      *
777900990311     c                   clear                   WIDDaperto
778000990311      *
778100990518     c     kanm          setll     fnanm02L
778200990518     c     kanm          reade     fnanm02L                               30
778300990301      *
778400990311     C                   DOW       *in30 = *off
778500990311     C     anmcaa        lookup    S7C                                    31
778600990312     C                   If        *in31 = *on  and  anmdch = *zeros
778700990311     c                   eval        *in30 = *on
778800990311     c                   eval        WIDDaperto = 'S'
778900990311     c                   Else
779000990518     c     kanm          reade     fnanm02L                               30
779100990311     c                   Endif
779200990311     c  N30              ENDDO
779300990209      *
779400990209     c                   ENDSR
779500001102      **********************************************************************
779600001102      *  AGGANCIO TABELLA 3K
779700001102      **********************************************************************
779800001102     C     AGGT3K        BEGSR
779900001102
780000001102     c                   clear                   DS3K
780100001102
780200001102      * Elaboro solo mittenti codificati
780300001102     c                   z-add     VTCksm        w0040
780400001102     C                   IF        VTCksm > *zeros  and  W0040 <> 8888
780500001102     C                                              and  W0040 <> 9999
780600001102
780700001102     C                   eval      KCOD = '3K'
780800001102     C                   movel(P)  VTCksm        KKEY
780900001102     C     KTAB          CHAIN     TABEL
781000001102     C                   IF        %found  and  TBLflg = *blanks
781100001102     C                   movel     TBLuni        DS3K
781200001102     C                   ENDIF
781300001102     C                   ENDIF
781400001102      *
781500001102     C                   ENDSR
781600990311      *****************************************************************
781700990311      *  DECODIFICO TABELLA CALCOLO RESPONSABILITA'
781800990311      *****************************************************************
781900990311     C     DECODtcr      BEGSR
782000990311      *
782100990311     c                   clear                   TIBS02DS
782200990311      *
782300990311     C                   MOVEL     'C'           t02mod
782400990311     C                   MOVEL     knsif         t02sif
782500990311     C                   MOVEL     'TCR'         t02cod
782600990311     C                   MOVEL(P)  V6Htcr        t02ke1
782700990311      *
782800990311     C                   CALL      'TIBS02R'
782900990311     C                   PARM                    KPJBA
783000990311     C                   PARM                    TIBS02DS
783100990311      *
783200990311     C                   IF        t02err = *BLANKS
783300990311     C                   movel(P)  T02uni        V6Sdtc
783400990311     C                   ENDIF
783500990311      *
783600990311     c                   ENDSR
783700990217      *****************************************************************
783800990217      *  DECODIFICO TABELLA TIPO ATTRIBUZIONE RESPONSABILITA'
783900990217      *****************************************************************
784000990217     C     DECODtre      BEGSR
784100990219      *
784200990219     C     '?'           SCAN      V6Stre                                 31
784300990219      *  Ricerca
784400990219    1C                   IF        *IN31 = *ON
784500990219     c                   eval       *in95 = *on
784600990219     c                   clear                   TIBS02DS
784700990219     c                   movel     'R'           t02mod
784800990219     c                   movel     knsif         t02sif
784900990219     C                   movel     'TRE'         t02cod
785000990219     C                   CALL      'TIBS02R'
785100990219     c                   parm                    KPJBA
785200990219     c                   parm                    TIBS02DS
785300990219      *
785400990219     c                   If          T02err = *blanks
785500990219     C                   movel     T02ke1        V6Stre
785600990219     c                   Endif
785700990219     c                   ENDIF
785800990217      *
785900990217     C                   clear                   Dtre
786000990219     c                   clear                   TIBS02DS
786100990217      *
786200990217     C                   MOVEL     'C'           t02mod
786300990217     C                   MOVEL     knsif         t02sif
786400990217     C                   MOVEL     'TRE'         t02cod
786500990218     C                   MOVEL(P)  V6Stre        t02ke1
786600990217      *
786700990217     C                   CALL      'TIBS02R'
786800990217     C                   PARM                    KPJBA
786900990217     C                   PARM                    TIBS02DS
787000990217      *
787100990217     C                   IF        t02err = *BLANKS
787200990217     C                   movel(P)  T02uni        DTRE
787300990217     C                   ENDIF
787400990217      *
787500990217     c                   ENDSR
787600981020      *****************************************************************
787700990311      *  AGGANCIO ANAGRAFICHE
787800981020      *****************************************************************
787900990311     C     CTR_ANAGRA    BEGSR
788000981022      *
788100981020     C                   CALL      'TIBS69R'
788200981020     C                   PARM                    tibs69DS
788300981020     C                   PARM                    DS_cnaco
788400981020     C                   PARM                    DS_cnind
788500981020     C                   PARM                    DS_cnclp
788600981020     C                   PARM                    DS_fncls
788700981022      *
788800990226     C                   eval      *IN30 = (O69ERR =  '1')
788900990311      *
789000990311     C                   eval      wtibs69r = 'S'
789100990408      * per il 1° giro è inizializzata nelle specifiche "D"
789200990311     C                   clear                   TIBS69DS
789300981020      *
789400981020     C                   ENDSR
789500981005      *****************************************************************
789600981127      *  DETERMINO LIMITE RISARCIBILE DA UTILIZZARE
789700981005      *****************************************************************
789800981127     C     IMPlimris     BEGSR
789900990311      *
790000020419      * se la spedizione è EEX o EUP oppure DPD export
790100021108      *             oppure FedEx export
790200021108     c                   IF            §DCTeuro = 'X'
790300021108     c                             or  §DCTeuro = 'P'
790400021108     c                             or (§DCTeuro = 'D' and
790500021108     c                                 §DCTtisp = 'E')
790600021108     c                             or (§DCTeuro = 'F' and
790700021108     c                                 §DCTtisp = 'E')
790800010412      * importo in moneta corrente controvalore CMR
790900010412     c                   z-add     §GEccm        wlimris
791000981127      *
791100990222      *    LEGGE 450
791200981127     c                   ELSE
791300060309      * verifico che valore di limite risarcibile devo applicare
791400060309      *------------------------------------------------------------------
791500060309      * RICERCA LIMITE RISARCIBILE CON FLAG IN TESTATA CA
791600060309      *
791700060309      * - se il flag in testata CA non è valorizzato
791800060309      * - o se sono in modalità inserimento
791900060309      *------------------------------------------------------------------
792000060309     c                   If        I00MOD = 'A' or §dcttlrp = ' '
792100060309     c                   exsr      Rec_lrp
792200060309     c                   endif
792300060309
792400060313     c                   If        §dcttlrp = ' ' or §dcttlrp = '6'
792500060309     c                   z-add     6,2           §gelrp
792600060309     c                   else
792700060309     c                   z-add     1             §gelrp
792800060309     c                   endif
792900060309
793000060313      * importo in moneta corrente limite risarcibile al KG
793100010412     c                   z-add     §GElrp        wlimris
793200981127     c                   ENDIF
793300981127      *
793400981127     C                   ENDSR
793500060309      *
793600060309      *****************************************************************
793700060309      *  RECUPERO IMPORTO LIMITE RISARCIBILE
793800060309      *****************************************************************
793900060309     C     REC_LRP       BEGSR
794000060309
794100060309      * Imposto il flag in base a DSP = Data spedizione
794200060309      *- flag = " " DSP minore di 01/03/06   prende sempre la vecchia legge 450 6,2 eur
794300060309      *- flag = "1" DSP maggiore di 30/04/06 prende sempre la nuova legge 286 1 eur
794400060309      *- flag = "1" DSP tra 01/03/06 e 30/04/06 e flag in tariffa rinuncia AC BASE = 'S'
794500060309      *                                      prende la nuova legge 286 1 eur
794600060309      *- flag = "6" DSP tra 01/03/06 e 30/04/06 e flag in tariffa rinuncia AC BASE = ' '
794700060313      *             e non ha tariffa AC BASE prende  6,2 eur senza nessuna legge
794800060313      *- flag = "1" DSP tra 01/03/06 e 30/04/06 e flag in tariffa rinuncia AC BASE = ' '
794900060313      *             ed  ha tariffa AC BASE   prende  la nuova legge 286 1 eur
795000060309
795100060309     c                   SELECT
795200060309
795300060309      *- flag = " " DSP minore di 01/03/06   prende sempre la vecchia legge 450 6,2 eur
795400060309
795500060309     c                   When      DS_ARBDAT < 20060301
795600060313     c                   eval      §dcttlrp = ' '
795700060309
795800060407      *- flag = "1" DSP maggiore di 14/05/06 prende sempre la nuova legge 286 1 eur
795900060309
796000060407     c                   When      DS_ARBDAT > 20060514
796100060313     c                   eval      §dcttlrp = '1'
796200060309
796300060309     c                   OTHER
796400060309
796500060309      * recupero la tariffa
796600060309      *
796700160818     c                   clear                   FNLV59ds
796800160818     c                   z-add     dutkci        iLV59kci
796900160818     c                   z-add     WISksc        iLV59ksc
797000160818     c                   movel     DS_arbdat     iLV59dsp                       Dta Sped x Rif. Tar.
797100160818     C                   IF        §DCTtisp = *blank
797200160818     C                   movel     'J'           iLV59fie
797300160818     c                   ELSE
797400160818     C                   movel     'S'           iLV59fie
797500160818     C                   ENDIF
797600160818     c                   eval      iLV59ta2 = 'B'
797700160818     c                   eval      iLV59blo = 'S'
797800160818     c                   movel     ARBtsp        iLV59tsp
797900160818     c                   movel     'S'           iLV59con
798000160818     c                   move      WISctr        iLV59ctr
798100060309      *
798200160818     c                   call      'FNLV59R'
798300160818     c                   parm                    FNLV59DS
798400060309
798500060309      * Se trovato progressivo aggancio le tariffe
798600160818    2c                   IF        oLV59ERR = *BLANKS  and  oLV59TKS = 'S'
798700160818     c                   move      oLV59ctr      ktamctr
798800060309     c     Ktam          chain     Tntam01l
798900060309     c                   If        %found(tntam01l) and tamatb = ' '
799000060309     c                   eval      wtamfmp  = tamfmp
799100060309     c                   else
799200060309      * se non trovata tariffa al cliente imposto S = Rifiuto AC BASE
799300060309     c                   eval      wtamfmp = 'S'
799400060309     c                   endif
799500060309     c                   else
799600060309      * se non trovata tariffa al cliente imposto S = Rifiuto AC BASE
799700060309     c                   eval      wtamfmp = 'S'
799800060309     c                   endif
799900060309
800000060313     c                   If        wtamfmp = 'S' or (wmand = 'S' and Wftc = 'd')
800100060313     c                   eval      §dcttlrp = '1'
800200060313     c                   else
800300060313     c                   eval      §dcttlrp = '6'
800400060309     c                   endif
800500060309
800600060309     c                   ENDSL
800700060309
800800060309     c                   endsr
800900060309      *
801000990216      *****************************************************************
801100990216      *  VERIFICO SE CI SONO NOTE NUOVE
801200990216      *****************************************************************
801300990216     C     CTR_NOTE      BEGSR
801400990216      *
801500990216     C                   clear                   FIDN42DS
801600990216     c                   movel     'N'           I42trc
801700990216     c                   movel     'C'           I42tpd
801800990216     c                   movel     ds_numca      I42nkt
801900990216     c                   movel(P)  fidn42ds      kpjbu
802000990216      *
802100990216     C                   CALL      'FIDN42R'
802200990216     C                   PARM                    kpjba
802300990216      *
802400990216     c                   movel     kpjbu         fidn42ds
802500020124     c                   eval      *in24 =  (o42err = *blanks)
802600990216      *
802700990216     C                   ENDSR
802800990304      *****************************************************************
802900990304      *  VERIFICO SE ESISTE EVENTO RISERVA
803000990304      *****************************************************************
803100990304     C     EVERISERVA    BEGSR
803200990304      *
803300010528     C                   open      FNEVB04L
803400101007
803500101007      * controllo in base agli eventi caricati in schiera
803600990304      *
803700101007     c                   eval      xx = 1
803800101007     c                   FOR       xx by 1 to %elem(s2A)
803900101007     c                   IF        s2A(xx) <> *blanks
804000990304     C                   move      i00aas        Kaas2
804100101007     c                   eval      EVBcev = s2A(xx)
804200990811     c     Kevb          chain     fnevb000                           31
804300990304      *
804400010528     c     *in31         ifeq      *off
804500990304     C                   movel     'S'           Wriserva
804600101007     c                   leave
804700010528     c                   endif
804800101007     c                   ENDIF
804900101007     c                   ENDFOR
805000990304      *
805100010528     C                   close     FNEVB04L
805200990304      *
805300990304     C                   ENDSR
805400990226      *****************************************************************
805500990226      *  VERIFICO SE CLIENTE PARTICOLARE
805600990226      *****************************************************************
805700990226     C     CLIPAR        BEGSR
805800990226      *
805900990226     c                   clear                   TIBS02DS
806000990226      *
806100990226     C                   MOVEL     'C'           t02mod
806200990226     C                   MOVEL     knsif         t02sif
806300990226     C                   MOVEL     'DPC'         t02cod
806400990226     C                   MOVEL(P)  W0070         t02ke1
806500990226      *
806600990226     C                   CALL      'TIBS02R'
806700990226     C                   PARM                    KPJBA
806800990226     C                   PARM                    TIBS02DS
806900990226      *
807000990226     C                   if        t02err = *BLANKS
807100990226     C                   eval      Wclipar = 'S'
807200990226     c                   endif
807300990226      *
807400990226     C                   ENDSR
807500010904
807600010904      *****************************************************************
807700010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
807800010904      *****************************************************************
807900010910     C     srCTRDEC      BEGSR
808000010904      *
808100011017     C                   reset                   XDECDS
808200010904      *
808300011017     C                   z-add     W0133         ICDnum
808400011017     C                   z-add     §cvDEC        ICDnrd
808500010904      *
808600011017     C                   call      'XDEC'
808700011017     C                   parm                    XDECDS
808800010904      *
808900011017     C                   IF        OCDesi  <> *off
809000010910     C                   eval      *IN28 = *ON
809100010904     C                   ENDIF
809200010904      *
809300010904     C                   ENDSR
809400010904
809500010904      *****************************************************************
809600010904      * CONVERSIONE IMPORTI IN MONETA CORRENTE
809700010904      *****************************************************************
809800010904     C     srCMB         BEGSR
809900010904      *
810000010904     C                   reset                   YEURCODS
810100010904      *
810200010904     C                   movel     WVLT          yecDVI
810300010904     C                   z-add     W0133         yecIMI
810400010910     C                   movel     §gedDBA       yecDVO
810500010904      *
810600010904     C                   call      'YEURCO'
810700010904     C                   parm                    YEURCODS
810800010904      *
810900010904     C                   IF        yecESI  = ' '
811000010904     C                   z-add     yecIMO        W0133
811100010912     C                   else
811200010912     C                   movel     *on           *in28
811300010904     C                   ENDIF
811400010904      *
811500010904     C                   ENDSR
811600020129
811700020129      *****************************************************************
811800020129      * VISUALIZZAZIONE EVENTO
811900020129      *****************************************************************
812000020129     C     CallFIDN06    BEGSR
812100020129      *
812200020129     C                   clear                   FIDN06DS
812300020129     C                   movel     'I'           I06mod
812400020129     C                   z-add     WV1Cnev       I06nev
812500020129     C                   z-add     V1Caae        I06aae
812600020129     C                   movel(P)  FIDN06DS      KPJBU
812700020129      *
812800020129     C                   call      'FIDN06R'
812900020129     C                   parm                    KPJBA
813000020129      *
813100020129     C                   ENDSR
813200131106     c*
813300131106      *****************************************************************
813400131106      * Calcola terminal responsabile in base al p.o.
813500131106      *****************************************************************
813600131106     c     CAL_ter       begsr
813700131106     c                   move      v6spor        w0030p
813800131106     C     w0030p        chain     AZORG                              31
813900131106     c                   movel     orgde3        og143
814000131106     c   31              clear                   og143
814100131106     c
814200131106     c* Calcolo il terminal responsabile e tipo
814300131106     c                   select
814400131106     c* se p.o. estero memorizzo se stesso
814500131106     c                   when      §ogntw='EEX' or §ogntw='DPD' or
814600131106     c                             §ogntw='FED'
814700131106     c                   movel     v6spor        v6stpr
814800131106     c                   when      v6htcr='A'
814900131106     c                   movel     i50tfa        v6stpr
815000131106     c                   when      v6htcr='P' and (w0030p =wlnp or
815100131106     c                                             w0030p =wtfplnp)
815200131106     c                   movel     wtfplnp       v6stpr
815300131106     c                   when      v6htcr='P' and (w0030p =i50lnp or
815400131106     c                                             w0030p =i50tfp )
815500131106     c                   movel     i50tfp        v6stpr
815600131106     c
815700131106     c                   other
815800131106     c                   clear                   fnlv55ds
815900131106     c                   movel     'A'           d55tpt
816000131106     c                   movel     w0030p        d55lin
816100131106     c                   if        ds_arbdat>0
816200131106     c                   z-add     ds_arbdat     d55drf
816300131106     c                   else
816400131106     c                   move      dctmgc        d55drf
816500131106     c                   movel     dctaac        d55drf
816600131106     c                   endif
816700131106     c                   call      'FNLV55R'
816800131106     c                   parm                    fnlv55ds
816900131106     c                   movel     d55tfa        v6stpr
817000131106
817100131106     c                   endsl
817200131106     c                   eval      v6htpr=v6stpr
817300131106     c                   endsr
817400980722      *****************************************************************
817500980623      *  ROUTINE INIZIALE
817600980722      *****************************************************************
817700951009     C     *INZSR        BEGSR
817800980722
817900951009     C     *ENTRY        PLIST
818000951009     C                   PARM                    KPJBA
818100980722
818200981020     C                   MOVEL     KPJBU         FIDN00DS
818300980625
818400980625      *  Verifico il tipo di funzione da eseguire
818500980625     C                   SELECT
818600980625      *
818700980625     C                   WHEN      I00MOD = 'A'                                 APERTURA C.A.
818800980625     c                   eval      *in01 = *on
818900980625     C                   movel(P)  tit1          VTCMOD
819000980625      *
819100980625     C                   WHEN      I00MOD = 'G'                                 CONFERMA C.A.
819200980625     C                   movel(P)  tit2          VTCMOD
819300980625     c                   eval      *in02 = *on
819400981211      *
819500981211     C                   WHEN      I00MOD = 'M'                                 MANUTENZIONE C.A.
819600981211     C                   movel(P)  tit4          VTCMOD
819700981211     c                   eval      *in03 = *on
819800981028      *
819900981028     C                   WHEN      I00MOD = 'C'                                 MANUTENZIONE C.A.
820000981028     c                   eval      *in16 = *on                                  RICHIAMATA DA PGM
820100990406     c                   eval      *in03 = *on                                  "COLLI RITROVATI"
820200981211     C                   movel(P)  tit4          VTCMOD
820300990406     c                   eval      I00mod = 'M'
820400990406      *
820500990406     C                   WHEN      I00MOD = 'E'                                 MANUTENZIONE C.A.
820600990406     c                   eval      *in04 = *on                                  SOLO RESPONSABILITA'
820700990406     c                   eval      *in03 = *on                                  "COLLI RITROVATI"
820800990406     C                   movel(P)  tit4          VTCMOD
820900990406     c                   eval      I00mod = 'M'
821000980625      *
821100980625     c                   OTHER
821200980625     c                   movel     msg(42)       o00msg
821300980625     c                   eval      o00err = 'E'
821400980625     c                   goto      fine
821500980625      *
821600980625     C                   ENDSL
821700980625
821800981126      * Dati società
821900050523     C                   Z-ADD     1             CODUT             1 0
822000050523     c                   exsr      DatiJob
822100130806      * Imposto le abilitazioni dell'utente
822200130806     c                   eval      dute01 = UTEfaf
822300980619      ***
822400980619      * CARICO TABELLA £6
822500980619      ***
822600980508     C                   CLEAR                   L6
822700981020     C                   CLEAR                   TRUL06DS
822800980508     C                   MOVE      '£6'          D06COD
822900980513     C                   MOVEL     i00fgs        D06key
823000981020     C                   MOVEL     TRUL06DS      KPJBU
823100980508     C                   CALL      'TRUL06R'
823200980508     C                   PARM                    KPJBA
823300981020     C                   MOVEL     KPJBU         TRUL06DS
823400980508     C                   MOVEA     LIN           L6
823500170428     c* Verifico le filiali gestione per avere i flag di validtà
823600170428     C                   clear                   FNLV50DS
823700170428     C                   MOVEL     i00fgs        D50FGS
823800170428     c                   eval      d50tcn='E'
823900170428     C                   CALL      'FNLV50R'
824000170428     C                   PARM                    FNLV50DS
824100981127      ***
824200010412      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
824300981127      ***
824400010412      *
824500010412      * recupero la moneta corrente
824600010412     C                   CLEAR                   tibs02ds
824700010412      *
824800010412     C                   MOVEL     'L'           T02TLA
824900010412     C                   MOVEL     'C'           T02MOD
825000010412     C                   MOVEL     KNSIF         T02SIF
825100010412     C                   MOVEL     'GED'         T02COD
825200010904     C                   MOVEL     'DANNI'       T02KE1
825300010412      *
825400010412     C                   CALL      'TIBS02R'
825500010412     C                   PARM                    KPJBA
825600010412     C                   PARM                    tibs02ds
825700010412      *
825800010910     c                   movel     T02UNI        DGEDDN
825900010412      * recupero i valori nella moneta corrente
826000010412     C                   CLEAR                   tibs02ds
826100010412      *
826200010412     C                   MOVEL     'L'           T02TLA
826300010412     C                   MOVEL     'C'           T02MOD
826400010412     C                   MOVEL     KNSIF         T02SIF
826500010412     C                   MOVEL     'GEI'         T02COD
826600010910     C                   MOVEL     §GEDDBA       T02KE1
826700010412      *
826800010412     C                   CALL      'TIBS02R'
826900010412     C                   PARM                    KPJBA
827000010412     C                   PARM                    tibs02ds
827100010412      *
827200010412     c                   movel     t02uni        DGEI
827300010904      ***
827400010904      * leggo tabella standards danni
827500010904      ***
827600010904     c                   clear                   TIBS02DS
827700010904     C                   MOVEL     'C'           t02mod
827800010904     C                   MOVEL     knsif         t02sif
827900010904     C                   MOVEL     'STD'         t02cod
828000010904     C                   MOVEL(P)  1             t02ke1
828100010910     C                   MOVEL(P)  §gedDBA       t02ke2
828200010904      *
828300010904     C                   CALL      'TIBS02R'
828400010904     C                   PARM                    KPJBA
828500010904     C                   PARM                    TIBS02DS
828600010904      *
828700010904     C                   if        t02err = *BLANKS
828800010904     C                   MOVEL     T02UNI        DSTD
828900010904     C                   endif
829000990311      *
829100990312      *        Causali anomalia IDD
829200990311     C                   clear                   XX
829300990311     C                   movel     '7C'          KCOD
829400990312     C     ktabe         SETLL     TABEL00F
829500990311     C                   DO        *HIVAL
829600990312     C     ktabe         READE     TABEL00F                               31
829700990311     C   31              leave
829800990311     c                   movel     tbluni        DS7C
829900990311     c                   IF        §7Cdan = 'S'
830000990311     C                   add       1             XX
830100990311     C                   movel     TBLKEY        S7C(xx)
830200990311     c                   ENDIF
830300990311     C                   ENDDO
830400101007
830500101007      * carico EVENTI che hanno RISERVA
830600101007     c                   clear                   xx
830700101007     c                   eval      kcod = '2A'
830800101007     c     ktabe         SETLL     tabel00f
830900101007     c                   do        *hival
831000101007     c     ktabe         READE     tabel00f
831100101007     c                   IF        %eof(tabel00f)
831200101007     c                   leave
831300101007     c                   ENDIF
831400101007     c                   eval      ds2A = TBLuni
831500101007     c                   IF        §2Atip = 'R'
831600101007     c                   eval      xx = xx + 1
831700101007     c                   eval      s2A(xx) = TBLkey
831800101007     c                   ENDIF
831900101007     c                   ENDDO
832000981127
832100981127      * TABELLE UTILIZZATE IN APERTURA
832200980623     c                   IF        I00MOD = 'A'
832300981127      *
832400981009      *        Date pulizia in filiale
832500980623     C                   clear                   ds5a
832600980623     C                   MOVEL     '5A'          kCOD
832700980623     C                   MOVEL(P)  1             kkEY
832800990408     C     ktab          CHAIN(N)  TABEL                              31
832900980623     C                   MOVEL     TBLUNI        ds5a
833000981009      *
833100981009      *        Causali consegna anomala solo Dirottamento
833200981009     C                   clear                   XX
833300990217     C                   clear                   JJ
833400981009     C                   MOVE      '7O'          KCOD
833500990312     C     ktabe         SETLL     TABEL00F
833600981009     C                   DO        *HIVAL
833700990312     C     ktabe         READE     TABEL00F                               31
833800981029     C   31              leave
833900981009     c                   movel     tbluni        DS7O
834000981009     c                   IF        §7Ofdi =  'D'
834100981009     C                   ADD       1             XX
834200990217     C                   MOVEL     TBLKEY        D7O(xx)
834300981009     c                   ENDIF
834400990217     c                   IF        §7Ores =  'S'
834500990217     C                   ADD       1             jj
834600990217     C                   MOVEL     TBLKEY        R7O(jj)
834700990217     c                   ENDIF
834800990217
834900981009     C                   ENDDO
835000981009      *
835100980623     C                   ENDIF
835200990216
835300990216      *  Verifico se ci sono note nuove, in caso positivo evidenzio F18
835400990216     C                   exsr      CTR_NOTE
835500981009
835600980611      * ORA
835700980611     C                   TIME                    W0140
835800000420      * ORA
835900000420     C                   MOVEL     W0140         WORA4
836000980619      * UDATE IN GGMMAAAA
836100980611     C                   MOVE      W0140         WDTGIO
836200980619      * UDATE IN AAAAMMGG
836300971016     C                   Z-ADD     WDTGIO        G02DAT
836400971016     C                   MOVEL     *BLANK        G02ERR
836500971016     C                   CALL      'XSRDA8'
836600971016     C                   PARM                    WLBDAT
836700980611     C                   MOVEL     G02INV        DATEU
836800980623     C     *iso          MOVEL     DATEU         Wdataoggi
836900050217
837000050217      * Apertura del file TNTMD
837100050217     c                   Clear                   comman
837200050217     c                   Eval      comman = cmd(1)
837300050217     c                   Eval      %Subst(comman:13:8) = 'TNTMD01L'
837400050217      * test sistema informativo di prova
837500050217     c                   If        %subst(knsif:7:1) = 'P'
837600050217     c                   Eval      %Subst(comman:30:10) = wlibp
837700050217     c                   Else
837800050217     c                   Eval      %Subst(comman:30:10) = wlib
837900050217     c                   EndIf
838000050217
838100050217     c                   Eval      comman =
838200050217     c                             %trim(comman) + '/TNTMD01L)'
838300050217     c                   Call      'QCMDEXC'                            99
838400050217     c                   Parm                    comman
838500050217     c                   Parm                    lenght
838600050217     c   99              Goto      fine
838700050217     c  n99              Open      Tntmd01l
838800980625
838900981126      * APRO FILE SPECIFICI PER SEDE/FILIALE/MODALITA' PGM
839000981005     C                   IF        SIMFEL = *zeros
839100990806     C                   open      TITAS30C
839200981229     C                   open      TNCSB03L
839300981005     c                   ELSE
839400981126      *
839500990311     c                   If        I00mod = 'A'
839600981006     C                   open      FLNUF01L
839700981224     C                   open      FNLBL01L
839800990311     c                   Endif
839900990311     c                   If        I00mod = 'G'
840000981126     C                   open      FNDFA01L
840100990311     c                   Endif
840200990105     C                   open      FNLBL02L
840300981006     C                   open      FNARB01L
840400981006     C                   open      FNBLP01L
840500981006     C                   open      FNART01L
840600981006     C                   open      FNBLT01L
840700060217     C                   open      FiAR401L
840800991104     C                   open      FIAR601L
840900051114     C                   open      FiAR901L
841000050331     C                   open      Tigcp21l
841100990518     C                   open      fnanm02L
841200981006     c                   ENDIF
841300980625
841400990312     C     KTABE         KLIST
841500990312     C                   KFLD                    CODUT
841600990312     C                   KFLD                    KCOD
841700990312      *
841800971020     C     KTAB          KLIST
841900971020     C                   KFLD                    CODUT
842000971020     C                   KFLD                    KCOD
842100971020     C                   KFLD                    KKEY
842200980625      *
842300980508     c     kbol01        klist
842400980515     c                   kfld                    i00aas
842500980515     c                   kfld                    i00lnp
842600980515     c                   kfld                    i00nrs
842700980515     c                   kfld                    i00nsp
842800050310      *
842900050310     c     kbol02        klist
843000050310     c                   kfld                    dctaas
843100050310     c                   kfld                    dctlnp
843200050310     c                   kfld                    dctnrs
843300050310     c                   kfld                    dctnsp
843400050915      *
843500050915     c     kbol50        klist
843600050915     c                   kfld                    i50aas
843700050915     c                   kfld                    i50lnp
843800050915     c                   kfld                    i50nrs
843900050915     c                   kfld                    i50nsp
844000991116      *
844100991116     c     kar6          klist
844200991116     c                   kfld                    i00aas
844300991116     c                   kfld                    i00lnp
844400991116     c                   kfld                    i00nrs
844500991116     c                   kfld                    i00nsp
844600991116     c                   kfld                    trcar6
844700991116      *
844800991116     c     kar6bl        klist
844900991116     c                   kfld                    LBLaan
845000991116     c                   kfld                    LBLlpn
845100991116     c                   kfld                    LBLnrn
845200991116     c                   kfld                    LBLnsn
845300991116     c                   kfld                    trcar6
845400981023      *
845500981104     c     kar4          klist
845600981023     c                   kfld                    i00aas
845700981023     c                   kfld                    i00lnp
845800981023     c                   kfld                    i00nrs
845900981023     c                   kfld                    i00nsp
846000981023     c                   kfld                    WTRC
846100980625      *
846200971020     C     KBOL          KLIST
846300980526     C                   KFLD                    arbaas
846400971020     C                   KFLD                    VTCLNP
846500971020     C                   KFLD                    VTCNRS
846600971020     C                   KFLD                    VTCNSP
846700980625      *
846800980625     c     kgiac         klist
846900980625     c                   kfld                    i00aas
847000980625     c                   kfld                    i00lnp
847100980625     c                   kfld                    i00nrs
847200980625     c                   kfld                    i00nsp
847300980625     c                   kfld                    Wfrg
847400980625      *
847500971021     C     KDET          KLIST
847600971021     C                   KFLD                    V1CAAE
847700981015     C                   KFLD                    Wv1cnev
847800050805     c     KDETold       klist
847900050805     c                   kfld                    DCTaae
848000050805     c                   kfld                    DCTnev
848100980625      *
848200971021     C     KDED          KLIST
848300971021     C                   KFLD                    V1CAAE
848400981015     C                   KFLD                    Wv1cnev
848500981015     C                   KFLD                    kfgs
848600980625      *
848700980625     C     KEVB          KLIST
848800980625     C                   KFLD                    KAAS2
848900980625     c                   kfld                    i00lnp
849000980625     c                   kfld                    i00nrs
849100980625     c                   kfld                    i00nsp
849200010528     c                   kfld                    evbcev
849300980625      *
849400971021     C     KDCS          KLIST
849500971021     C                   KFLD                    KTPD
849600971021     C                   KFLD                    KNKT
849700980609     C                   KFLD                    KSTD
849800980609     C                   KFLD                    KDIM
849900980609     C                   KFLD                    KHIM
850000971021     C                   KFLD                    KNKS
850100971021     C                   KFLD                    KTRC
850200980625      *
850300971022     C     KNUF          KLIST
850400980611     C                   KFLD                    AAS
850500980611     C                   KFLD                    CNU
850600980429     C                   KFLD                    COMLIN
850700980625      *
850800990209     c     kca           klist
850900980518     c                   kfld                    dctaac
851000980518     c                   kfld                    dctfil
851100980518     c                   kfld                    dctnca
851200980625      *
851300980526     c     kdcd2b        klist
851400980526     c                   kfld                    artfls
851500980526     c                   kfld                    artlna
851600980526     c                   kfld                    artnrs
851700980526     c                   kfld                    artnsc
851800980526     c                   kfld                    i00aac
851900980526     c                   kfld                    i00fil
852000980526     c                   kfld                    i00nca
852100981009      *
852200980527     c     kdcd2c        klist
852300980527     c                   kfld                    v1sfls
852400990309     c                   kfld                    v1hlna
852500980527     c                   kfld                    i00nrs
852600980527     c                   kfld                    v1snsc
852700980527     c                   kfld                    i00aac
852800980527     c                   kfld                    i00fil
852900980527     c                   kfld                    i00nca
853000981016      *
853100981016     c     kdcd2d        klist
853200981016     c                   kfld                    v1sfls
853300990309     c                   kfld                    v1hlna
853400981016     c                   kfld                    i00nrs
853500981016     c                   kfld                    v1snsc
853600051019      *
853700051019     c     K04DCR02      klist
853800051019     c                   kfld                    DCDaac
853900051019     c                   kfld                    DCDfil
854000051019     c                   kfld                    DCDnca
854100051019     c                   kfld                    DCRseg
854200980625      *
854300980520     c     knumca        klist
854400980520     c                   kfld                    i00aac
854500980520     c                   kfld                    i00fil
854600980520     c                   kfld                    i00nca
854700981020      *
854800981020     c     KDKA          klist
854900981020     c                   kfld                    i00aac
855000981020     c                   kfld                    i00fil
855100981020     c                   kfld                    i00nca
855200981020     c                   kfld                    Wtrc
855300981113      *
855400981113     C     Kspe          klist
855500981113     C                   KFLD                    KFLS
855600981113     C                   KFLD                    KCLI
855700981113     C                   KFLD                    KCLU
855800980721      *
855900981228     c     klbl_F        klist
856000981228     c                   kfld                    LBLaan
856100981228     c                   kfld                    LBLlpn
856200981228     c                   kfld                    LBLnrn
856300981228     c                   kfld                    LBLnsn
856400990308      *
856500990308     c     kanm          klist
856600990308     c                   kfld                    v1sfls
856700990311     c                   kfld                    v1hlna
856800990311     c                   kfld                    I00nrs
856900990308     c                   kfld                    v1snsc
857000990315      *
857100990315     c     kdcr          klist
857200990315     c                   kfld                    dctaac
857300990315     c                   kfld                    dctfil
857400990315     c                   kfld                    dctnca
857500990315     c                   kfld                    Wv6snsc
857600990315     c                   kfld                    Wv6spor
857700060309
857800060309     c     KTam          Klist
857900160818     c                   Kfld                    iLV59KSC
858000060313     c                   Kfld                    Ktamctr
858100160818     c                   Kfld                    oLV59PRG
858200120111     c     kANM2         klist
858300120111     c                   kfld                    dcdfls
858400120111     c                   kfld                    dcdlna
858500120111     c                   kfld                    dcdnrs
858600120111     c                   kfld                    dcdnsc
858700120111     c                   kfld                    kcaa
858800131106     c     kcaeP         klist
858900131106     c                   kfld                    kepa
859000131106     c                   kfld                    w0030
859100131106     c                   kfld                    vuotoTFP
859200131106     c     kcaeA         klist
859300131106     c                   kfld                    kepa
859400131106     c                   kfld                    w0030
859500981224      *
859600951009     C                   ENDSR
859700050523      *---------------------------------------------------------------*
859800050523      *?Reperimento Dati del job (Utente/Operativi)                  ?*
859900050523      *---------------------------------------------------------------*
860000050523     c     DatiJob       BegSr
860100050523      *
860200050523     c     *dtaara       define    §azute        AZUTEds
860300050523     c     *dtaara       define    §datiute      dDatiUte
860400050523      *
860500050523     c                   in(E)     *dtaara
860600050523     c                   IF        %ERROR or RSUT = *blanks
860700050523     c                   clear                   Tibs34Ds
860800050523     c                   call      'TIBS34R'
860900050523     c                   parm                    Tibs34Ds
861000050523     c                   in        *dtaara
861100050523     c                   ENDIF
861200050523      *
861300050523     c                   EndSr
861400980611      *------------------------------------------------------------
861500990210     OFNDCT000  E            FNDCTfree
861600980731      *------------------------------------------------------------
861700050217** cmd
861800050217OVRDBF FILE(        ) TOFILE(
861900981014** MSG  L78                                                                  *
862000980709Immettere il Tipo Anomalia                                                     1
862100980709Tipo Anomalia inesistente o annullato                                          2
862200980709Immettere la data apertura                                                     3
862300980709Data apertura errata                                                           4
862400980709Immettere Numero Evento valido                                                 5
862500980709Numero Evento inesistente o annullato                                          6
862600980709P.O. non interessato all'evento immesso. ENTER PER FORZARE                     7
862700980720Non è possibile escludere segnacolli non inclusi in C.A.                       8
862800990111La descrizione pezzi dannegg./mancanti è obbligatoria se inserito il n. pezzi  9
862900981015I colli danneggiati/mancanti non possono superare il colli in spedizione       10
863000980709MANCA NUMERATORE C.A.: telefonare in Sede !!!!!!!                              11
863100980709NUMERATORE C.A. non disponibile: RITENTARE  NUOVAMENTE !!!                     12
863200980709Anomalia segnacollo inesistente o annullata                                    13
863300980709Bolla non manutenzionabile                                                     14
863400981015Il numero colli selezionati è diverso dal numero colli Danneggiati/Mancanti    15
863500981009Impossibile aprire C.A. : pratica giacenza aperta                              16
863600010911LIBERO - ex Impossibile aprire C.A. : superata la data pulizia spedizioni      17
863700981022Codice 9999 non utilizzabile. Inserire 8888 o un codice esistente su P.d.C.    18
863800981022Codice errato o annullato                                                      19
863900981021La data importo trattenuto è obbligatoria                                      20
864000981021Se immessa la data immettere anche l'importo trattenuto o eliminare entrambi   21
864100981021Data importo trattenuto errata                                                 22
864200981127MANCA LIMITE RISARCIBILE: telefonare in Sede !!!!!!                            23
864300020125Mancano le caratteristiche per mantenere il flag Franchigia valorizzato ("F")  24
864400981201Per le spedizioni Import in porto assegnato il beneficiario è il DESTINATARIO  25
864500980709Attenzione !! Modificata Località                                              26
864600980709Attenzione !! Modificato CAP                                                   27
864700980709Attenzione !! Modificata Provincia                                             28
864800980709Attenzione !! Modificata Nazione                                               29
864900990218Inserito segnacollo non appartenente alla C.A.                                 30
865000981009Impossibile aprire C.A. : tutti i colli sono su altre C.A.                     31
865100980710Il collo non è consegnato: selezione non possibile per tipo anomalia scelto    32
865200000307I Kg danneggiati/mancanti non possono superare i Kg in bolla ENTER PER FORZARE 33
865300980720Se scelti Tutti i segnacolli non è ammesso Inclusione/Esclusione               34
865400990226ATTENZIONE !! La spedizione risulta in distinta                                35
865500990217Indicare il numero dei colli danneggiati/mancanti                              36
865600990111Il numero pezzi danneggiati/mancanti è obbligatorio                            37
865700980710Non è possibile confermare la C. A.: manca numero spedizione                   38
865800980709Se immessa la divisa immettere anche l'importo o eliminare entrambi            39
865900980709Divisa errata                                                                  40
866000980709Non ammessi importi con decimali per la divisa indicata                        41
866100980709Opzione non gestibile: telefonare in Sede !!!                                  42
866200980721La descrizione anomalia è obbligatoria                                         43
866300980710Il motivo del danno è obbligatorio                                             44
866400981009Impossibile aprire C.A. : spedizione con blocco                                45
866500980709Per la spedizione risultano aperte altre C.A.                                  46
866600980720Selezionare colli Dannegg./mancanti: già aperta C.A. senza dettaglio colli     47
866700990114Il numero colli danneggiati/mancanti è maggiore dei colli selezionabili        48
866800981126Tipo gestione: impostare correttamente i test                                  49
866900981126Tipo gestione: è OBBLIGATORIO procedere con pratica assicurativa               50
867000981126Tipo gestione: è anche possibile procedere con pratica assicurativa            51
867100980710Il collo è consegnato: selezione non possibile per tipo anomalia scelto        52
867200090303Per il segnacollo  inserire almeno 1 punto di responsabilità                   53
867300980720In partenza non è possibile impostare l'arrivo come P.O verifica DDT           54
867400980720In arrivo non è possibile impostare la partenza come P.O. verifica DDT         55
867500980720Impostare P.O. verifica DDT                                                    56
867600981020Indicare le disposizioni merce in danno                                        57
867700990209P.O. inesistente o annullato                                                   58
867800120619Inseriti dati non validi o incompleti                                          59
867900090303I punti di responsabilità devono essere maggiori/uguali ai colli danneg/mancan
868000981015Il peso danneggiato/mancante è obbligatorio                                    61
868100980918Inserire i pezzi danneggiati/mancanti o la descrizione dei pezzi presenti      62
868200981126Conferma C.A. forzata prima dello scadere dei xxx giorni previsti              63
868300981009Impossibile aprire C.A. : spedizione con consegna anomala dirottamento         64
868400981014Importo da Assicurare non indicato per cliente con Mandato a Valore Variabile  65
868500990127Inserire l'Importo Massimo Risarcibile                                         66
868600981020Tipo anomalia valido solo per EVENTI                                           67
868700981020Segnacollo utilizzato per l'apertura non incluso in C.A.                       68
868800981029Respingere Rivalsa: il cliente ha trattenuto                                   69
868900120723Rimborso a carico del vettore: il cliente ha trattenuto                        70 LIBERO
869000990120Il cliente trattiene, indicarne il codice nel beneficiario alternativo         71
869100120723Rimborso a carico di CONSULDANNI: pervenuta rivalsa                            72 LIBERO
869200080118E-MAIL non inviata:                                                            73
869300120723Tipo rimborso cliente non corretto                                             74 LIBERO
869400990226Progetto di Liquidazione rimborsato, l'Importo Trattenuto non si può inserire  75
869500990127L'Importo Trattenuto non può essere maggiore dell'Importo concordato           76
869600981113Il Massimo Risarcibile inserito è maggiore del calcolato                       77
869700120723Rimborso a carico del vettore: il tipo gestione è Liquidazione Transattiva     78 LIBERO
869800981217Per le spedizioni Import in porto franco il beneficiario è il MITTENTE         79
869900990510Inserire la Responsabilità Internazionale                                      80
870000020125Il Mass.Risarcib. è > del limite per Pratiche in Franchigia. ENTER per forzare 81
870100981228Impossibile aprire C.A. : spedizione figlia di un cambio porto                 82
870200990108Impossibile aprire C.A. : tipo anomalia non valido per spedizione consegnata   83
870300990111Il numero pezzi danneggiati/mancanti è obbligatorio se inserita la descrizione 84
870400990111ATTENZIONE !! Ci sono dei colli non consegnati per questa spedizione           85
870500990113Data apertura minore della data spedizione                                     86
870600990114Impossibile aprire C.A. : tipo anomalia non valido per sped. non consegnata    87
870700990127Impossibile aprire C.A. : non esiste spedizione per il segnacollo indicato     88
870800990127Inserire l'Importo Concordato                                                  89
870900990127L'Importo Concordato è superiore all'Importo Limite per la Liq. Transattiva    90
871000990209Modifica non consentita C.A. in attesa di ricalcolo responsabilità             91
871100990218Tipo Attribuzione inesistente o annullato                                      92
871200990217I colli danneggiati/mancanti sono obbligatori: spedizione di Reso              93
871300990306P.O. e Tipo Attribuzione già inseriti per il segnacollo                        94
871400990218Per il Tipo Attribuzione inserito non indicare il segnacollo                   95
871500990218Per il Tipo Attribuzione inserito non indicare il Punto Operativo              96
871600990226ATTENZIONE !! Inserire oportuna descrizione dei Pezzi Danneggiati/Mancanti     97
871700050428Beneficiario alternativo non è cod.fatturaz.del beneficiario.ENTER per forzare 98
871800990306Non è possibile modificare la Riserva: esiste un'evento di Riserva             99
871900990308Impossibile aprire C.A.: porto assegnato in partenza consegnato senza riserva  100
872000990308Impossibile aprire C.A.: porto franco in arrivo consegnato senza riserva       101
872100990308Impossibile aprire C.A.: spedizione in partenza consegnata con riserva         102
872200990308Impossibile aprire C.A.: avaria non consentita con evento smarrimento autotras 103
872300990308Prima della conferma per gli assegnati non è possibile indicare il mittente    104
872400990311Impossibile aprire C.A.: il segnacollo selezionato non é in IDD3               105
872500990406Manutenzione non consentita: responsabilità non considerata nella reportistica 106
872600990415Se Export con beneficiario il DESTINATARIO tipo gestione deve essere PRATICA   107
872700990415Non è possibile togliere la Riserva.                                           108
872800000628Impossibile aprire C.A.: spedizione posta. (Ammesso solo in caso di evento)    109
872900000628Impossibile togliere Numero Evento per una C.A. aperta su spedizione Posta     110
873000010910Ammessi importi con massimo __ decimali per la divisa indicata                 111
873100050428Beneficiario Alternativo deve essere un codice di fatturazione                 112
873200051019Avvisare la SEDE della modifica del peso e dell'importo massimo risarcibile    113
873300080208Tipo gestione: è OBBLIGATORIO procedere con liquidazione transattiva           114
873400080211Tipo gestione: è anche possibile procedere con liquidazione transattiva        115
873500080229Il Beneficiario è stato modificato, verificare il Beneficiario Alternativo     116
873600110504Beneficiario Alternativo obbligatorio in caso di Cambio Porto o Dirottamento   117
873700120508Mancano Coordinate bancarie Bonifico Danni: Avvisare Uff.Gestione Pagamenti    118
873800120508Porto Franco Import, il beneficiario deve essere il Partner                    119
873900120619Per ogni collo i punti di responsabilità non possono essere superiori a xxxxx  120
874000120619I punti di responsabilità in totale non devono superare xxxxxx per ogni collo  121
874100131105Il codice inserito non è un terminal                                           122
874200180130Il segnacollo risulta già imputato alla C.A.                                   123
874300050601**
874400050601OVRPRTF FIDN03P  OUTQ(          ) USRDFNDTA('
