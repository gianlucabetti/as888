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
018500131105     D MSG             S             78    DIM(122) CTDATA PERRCD(1)            MSG VIDEO
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
460800990311
460900990323      * Il segnacollo non deve avere IDD aperto
461000990323     c                   IF        Wctridd = 'S'  and
461100990323     c                             (V1Ssel = 'I'  or  VCCtsl = 'T')
461200990311     c                   exsr      ctr_idd
461300990311     c                   If        Widdaperto = 'S'
461400990311     C                   eval       *in72 = *on
461500990311     C                   eval       *in28 = *on
461600990311     C                   movel     MSG(105)      V1CMSG
461700990311     c                   EndIf
461800990311     c                   ENDIF
461900990311     C   28              goto      ECTS01
462000980710
462100980720      * GESTIONE ANOMALIA SEGNACOLLO
462200980720      *   Ricerca in tabella
462300980518     C     '?'           SCAN      V1SCAN                                 31
462400980720      *
462500980720    3C                   if        *IN31 = *ON
462600990216     c                   eval       *in71 = *on
462700971021     C                   MOVEL     CODUT         §KUT
462800971021     C                   MOVEL     '3E'          §COD
462900971021     C                   MOVEL     *BLANKS       §KEY
463000971021     C                   CALL      'X§TABER'
463100980626     C                   PARM                    §KUT
463200980626     C                   PARM                    §COD
463300980623     C                   PARM                    §KEY
463400980623     C                   PARM                    §DES
463500971021     C                   MOVEL     §KEY          V1SCAN
463600980720      *
463700980720     C                   if        V1SCAN <> *BLANKS
463800971021     C                   MOVEL     §DES          V1DCAN
463900980720     C                   endif
464000980720      *
464100980720     C                   eval      *in29 = *ON
464200971021     C                   GOTO      ECTS01
464300980720     C                   ENDIF
464400980720      *
464500980720     C                   if        V1SCAN <> *BLANKS
464600971021     C                   MOVEL     '3E'          KCOD
464700971021     C                   MOVEL(P)  V1SCAN        KKEY
464800990408     C     KTAB          CHAIN     TABEL00F                           31
464900980720    4C                   if        *IN31 = *OFF  and TBLflg = *blanks
465000990408     C                   MOVEL     TBLUNI        V1DCAN
465100980720   X4C                   else
465200971021     C                   MOVE      *BLANKS       V1DCAN
465300990129     C                   eval      *in71 = *on
465400990129     C                   eval      *in28 = *on
465500971023     C                   MOVEL     MSG(13)       V1CMSG
465600971021     C                   GOTO      ECTS01
465700980720     C                   endif
465800980720     C                   endif
465900980720
466000980720      * conto numero colli selezionati
466100980720      *   compresi quelli che erano eventualualmente già in C.A. tranne gli esclusi
466200980720     c                   IF        VCCtsl = 'T'  or  V1Ssel = 'I'
466300981229     c                             or  (*IN74 = *ON  and  V1Ssel <> 'E')
466400980518     c                   add       1             wclinc
466500980720     c                   ENDIF
466600980720
466700980720    2c                   ENDIF
466800971021     C     ECTS01        TAG
466900980720
467000981021     C                   UPDATE    FI03S04
467100980720
467200990114     c                   IF        *in28 = *on  or  *in29 = *on
467300981029     C                   eval      *in32 = *on
467400980720     c                   ENDIF
467500980720      *
467600981029     c                   IF        *in32 = *off
467700980720     c                   add       1             nrr1
467800981029     C     nrr1          chain     FI03S04                            32
467900980720     c                   ENDIF
468000980720
468100971022    1C                   ENDDO
468200980720
468300990304      * Controllo la corrispondenza tra il N. segnacolli scelti e il N. colli danneggiati/mancanti
468400990217      * Non controllo se premuto F12 o se già segnalati errori
468500000427      * Non controllo se bolla rigenerata e non ho segnacolli
468600990304      *                  C O N T R O L L I   E L I M I N A T I
468700990304      *   se i colli spedizione sono uguali ai colli danneggiati/mancanti selezione obbligatoria;
468800990304      *   se la spedizione è mamma di reso selezione obbligatoria;
468900990304      *   se anomalia per colli consegnati è possibile non indicare il dettaglio segnacolli
469000990217      *
469100990114     c                   SELECT
469200990217      *
469300990114     c                   WHEN      *IN28 = *ON  or  *IN29 = *ON  or  *INKL =*ON
469400990217      *
469500990114     c                   WHEN      V2Cncn = Wclinc
469600000427      *
469700000427     c                   WHEN      Wsop = '-'  and  ultnrr1 = *zeros
469800990217      *
469900990114     c                   OTHER
470000990114     c                   eval      *in28 = *on
470100990114     C                   movel     MSG(15)       V1CMSG
470200990114     c                   ENDSL
470300990114     c
470400990114     c
470500971021     C                   ENDSR
470600980717      *****************************************************************
470700980717      *  MANUTENZIONE/INSERIMENTO ANAGRAFICA CLIENTI
470800980717      *****************************************************************
470900980514     C     MANCLI        BEGSR
471000981021      *
471100980717      * EMISSIONE
471200980514     c     emw02         tag
471300981021     c                   exfmt     FI03W02
471400981021      *
471500980514     c                   clear                   w2cmsg
471600980717     c                   eval      *in28 = *off
471700981021      *
471800980717     c   KL              goto      Emancli
471900981021      *
472000980717      * CONTROLLI
472100980619     c                   exsr      ctrw02
472200980515     c   28              goto      emw02
472300981020      *
472400981102     c                   IF        wtrc = 'C'
472500981102     c                   movel     w2crag        v1dksc
472600981102     c                   movel     DS_FI03W02    DS_W02CCON
472700981102     c                   ELSE
472800981105     c                   movel     w2crag        V2Crag
472900981105     c                   movel     w2crag        WV2Crag
473000981102     c                   movel     DS_FI03W02    DS_W02CRIV
473100981102     c                   ENDIF
473200981102
473300980717      *
473400980717     C     EMANCLI       ENDSR
473500980717      *****************************************************************
473600980717      * CONTROLLI VIDEATA MANUTENZ./INSERIMENTO ANAGRAFICA CLIENTI
473700980717      *****************************************************************
473800980514     C     CTRW02        BEGSR
473900980722
474000980727     c     60            DO        69            XX
474100980722     c                   eval      *IN(XX) = *OFF
474200980722     c                   ENDDO
474300980722
474400981021      * Controllo Ragione Sociale, Indirizzo, Località, Provincia, CAP, Nazione e Partita IVA
474500981021     c                   CLEAR                   FNLV14DS
474600981021     c                   z-add     DS_arbdat     I14dri
474700981021     c                   movel     W2Crag        I14rsc
474800981021     c                   movel     W2Cvia        I14ind
474900981021     c                   movel     W2Cloc        E14loc
475000981021     c                   movel     W2Cprv        E14prv
475100981021     c                   movel     W2Ccap        E14cap
475200981021     c                   movel     W2Cnaz        E14nar
475300981021     c                   eval      E14pip = W2Cive + W2Civa
475400981021      *
475500981021     c                   CALL      'FNLV14R'
475600981021     c                   PARM                    KPJBA
475700981021     c                   PARM                    FNLV14DS
475800981021      *
475900981021     c                   movel     I14ind        W2Cvia
476000981021     c                   movel     E14loc        W2Cloc
476100981021     c                   movel     E14prv        W2Cprv
476200981021     c                   movel     E14cap        W2Ccap
476300981021     c                   movel     E14nar        W2Cnaz
476400981021     c                   eval      W2Cive = %subst(E14pip:1:2)
476500981021     c                   eval      W2Civa = %subst(E14pip:3:14)
476600981021      *
476700981021     c                   eval      *in60 = (O14err = '1')                       Ragione Sociale
476800981021     c                   eval      *in61 = (O14err = '2')                       Indirizzo
476900981021     c                   eval      *in62 = (O14err = '5')                       CAP
477000981021     c                   eval      *in63 = (O14err = '3')                       Località
477100981021     c                   eval      *in64 = (O14err = '4')                       Provincia
477200981021     c                   eval      *in65 = (O14err = '6')                       Nazione
477300981021     c                   eval      *in68 = (O14err = '7')                       P.IVA
477400981021      *
477500981021     c                   IF        O14msg <> *blanks
477600981021     c                   movel     O14msg        W2Cmsg
477700981021     c                   eval      *IN28 = *ON
477800981021     c                   ELSE
477900981021      * Controllo il CAP
478000981021     C                   movel     w2ccap        kcap
478100981021     C                   exsr      CAZIS0
478200981021     c                   ENDIF
478300981021     C   28              goto      ectw02
478400981021
478500981021      * CONTROLLO IL NUMERO FAX
478600981021     C                   IF        W2Cfax <> *BLANKS
478700981021     C                   clear                   TRUL42DS
478800981021     C                   movel     W2Cfax        D42FAX
478900981021     C                   CALL      'TRUL42R'
479000981021     C                   PARM                    TRUL42DS
479100981021     C                   IF        D42ERR = '1'
479200990129     C                   eval      *in66 = *on
479300990129     C                   eval      *in28 = *on
479400981021     C                   movel     D42MSG        w2cmsg
479500981021     C                   ENDIF
479600981021     C                   ENDIF
479700981021     C   28              goto      ectw02
479800981021
479900981021      * CONTROLLO IL NUMERO DI TELEFONO
480000981021     C                   IF        W2Ctel <> *BLANKS
480100981021     C                   clear                   TRUL42DS
480200981021     C                   movel     W2Ctel        D42FAX
480300981021     C                   CALL      'TRUL42R'
480400981021     C                   PARM                    TRUL42DS
480500981021     C                   IF        D42ERR = '1'
480600990129     C                   eval      *in67 = *on
480700990129     C                   eval      *in28 = *on
480800981021     C                   movel     D42MSG        w2cmsg
480900981021     C                   ENDIF
481000981021     C                   ENDIF
481100981021     C   28              goto      ectw02
481200980722
481300980722     c     ECTW02        ENDSR
481400980722      *---------------------------------------------------------------*
481500980722      * ROUTINE DI CONTROLLO ESISTENZA CAP                            *
481600980722      *---------------------------------------------------------------*
481700980514     C     CAZIS0        BEGSR
481800980722      *
481900980722      * CONTROLLO CAP PER INTERO
482000981020     C                   CLEAR                   TISI95DS
482100981020     C                   CLEAR                   FNLV13DS
482200980514     C                   MOVEL     ' '           I95TLA
482300980514     C                   MOVEL     '7'           I95TCN
482400980514     C                   MOVEL     w2cCAP        I95CAP
482500980514     C                   MOVEL     w2cNAZ        I95NAR
482600980514     C                   MOVEL     w2cvia        I95IND
482700980514     C                   MOVEL     w2cloc        I95LOC
482800980514     C                   MOVEL     w2cprv        I95PRV
482900980514     C                   MOVEL     'S'           I13AF0
483000980514     C                   MOVEL     'S'           I13AF1
483100980514     C                   MOVEL     'S'           I13SZ2
483200980514     C                   MOVEL     'S'           I13LA3
483300980514     C                   MOVEL     'S'           I13SZ3
483400980514     C                   MOVEL     'S'           I13CNV
483500980514     C                   MOVEL     'S'           I13SZV
483600980514     C* Gestione errori forzabili: se cambiato CAP/Località
483700981021      * riazzero i valori di errori forzabili
483800980514    2C     w2cCAP        IFNE      WSVCAP
483900980514     C                   CLEAR                   WFZV
484000980514     C                   CLEAR                   WFZ1
484100980514     C                   CLEAR                   WFZ2
484200980514     C                   CLEAR                   WFZ3
484300980514     C                   MOVEL     w2cCAP        WSVCAP
484400980514    2C                   ENDIF
484500981021      * Modifica località
484600980514    2C     w2cLOC        IFNE      WSVLOC
484700980514     C                   MOVEL     w2cLOC        WSVLOC
484800980514     C                   CLEAR                   WFZ1
484900980514     C                   CLEAR                   WFZ2
485000980514     C                   CLEAR                   WFZ3
485100980514    2C                   ENDIF
485200981021      * Modifica PROVINCIA
485300980515    2C     w2cPRv        IFNE      WSVPRv
485400980515     C                   MOVEL     w2cPRv        WSVPRv
485500980514     C                   CLEAR                   WFZ3
485600980514    2C                   ENDIF
485700981021      * CALL
485800980514     C                   MOVEL     WFZV          E13FZV
485900980514     C                   MOVEL     WFZ1          E13FZ1
486000980514     C                   MOVEL     WFZ2          E13FZ2
486100980514     C                   MOVEL     WFZ3          E13FZ3
486200980514     C                   CALL      'FNLV13R'
486300980514     C                   PARM                    KPJBA
486400981020     C                   PARM                    FNLV13DS
486500981020     C                   PARM                    TISI95DS
486600980514     C* Ricontrollo errori
486700980514     C                   MOVEL     O13ERR        WERR
486800980514     C                   MOVEL     O13MSG        WMSG
486900980514     C                   MOVEL     E13FZV        WFZV
487000980514     C                   MOVEL     E13FZ1        WFZ1
487100980514     C                   MOVEL     E13FZ2        WFZ2
487200980514     C                   MOVEL     E13FZ3        WFZ3
487300980514     C*
487400980514     C* Se il livello di affidabilità non è inferiore a 2 e
487500980514     C* il livello di reperimento dati non è inferiore a 3
487600981020     C* reimposto i dati da TISI95DS
487700980514     C     O13RON        IFEQ      'S'
487800980514     C     O13ROC        OREQ      'S'
487900980514     C     O13ROP        OREQ      'S'
488000980514     C     O13ROL        OREQ      'S'
488100980514     C                   MOVEL     O95LOC        w2cLOC
488200980514     C                   MOVEL     O95PRV        w2cprv
488300980514     C                   MOVEL     O95NAR        w2cnaz
488400980514     C                   MOVEL     O95CAP        w2cCAP
488500980514     C                   END
488600980514     C*
488700980514     C                   EXSR      CTRIND
488800980514     C* Se non ci sono errori proseguo
488900980514     C   28              GOTO      FINCTR
489000980514     C*
489100980514     C* Controllo se reimpostata località
489200980514     C     O13ROL        IFEQ      'S'
489300990129     c                   eval      *in63 = *on
489400990129     c                   eval      *in28 = *on
489500980515     C                   MOVEL     msg(26)       w2cmsg
489600980514     C                   GOTO      FINCTR
489700980514     C                   END
489800980514     C* Controllo se reimpostata Provincia
489900980514     C     O13ROP        IFEQ      'S'
490000990129     c                   eval      *in64 = *on
490100990129     c                   eval      *in28 = *on
490200980515     C                   MOVEL     msg(28)       w2cmsg
490300980514     C                   GOTO      FINCTR
490400980514     C                   END
490500980514     C* Controllo se reimpostato CAP
490600980514     C     O13ROC        IFEQ      'S'
490700990129     c                   eval      *in62 = *on
490800990129     c                   eval      *in28 = *on
490900980515     C                   MOVEL     msg(27)       w2cmsg
491000980514     C                   GOTO      FINCTR
491100980514     C                   END
491200980514     C* Controllo se reimpostata Nazione
491300980514     C     O13RON        IFEQ      'S'
491400990129     c                   eval      *in65 = *on
491500990129     c                   eval      *in28 = *on
491600980515     C                   MOVEL     msg(29)       w2cmsg
491700980514     C                   GOTO      FINCTR
491800980514     C                   END
491900980514     C*
492000980514     C     FINCTR        ENDSR
492100980514     C*------------------------------------------------------*
492200980514     C*  Controllo dati indirizzo
492300980514     C*------------------------------------------------------*
492400980514     C     CTRIND        BEGSR
492500981021     C*
492600981021     C* Errore !!!!
492700981021     C     WERR          IFNE      ' '
492800981021     C     WMSG          ANDNE     *BLANKS
492900981021     C* Manca la Ragione sociale
493000981021     C                   SELECT
493100981021     C* Errore su località/città
493200981021     C     WERR          WHENEQ    '3'
493300990129     c                   eval      *in63 = *on
493400990129     c                   eval      *in28 = *on
493500981021     C* Errore su Cap
493600981021     C     WERR          WHENEQ    '5'
493700990129     c                   eval      *in62 = *on
493800990129     c                   eval      *in28 = *on
493900981021     C* Errore su provincia
494000981021     C     WERR          WHENEQ    '4'
494100990129     c                   eval      *in64 = *on
494200990129     c                   eval      *in28 = *on
494300981021     C* Nazione  errata
494400981021     C     WERR          WHENEQ    '6'
494500990129     c                   eval      *in65 = *on
494600990129     c                   eval      *in28 = *on
494700981021    2C                   ENDSL
494800990129      *
494900981021     C                   MOVEL     WMSG          w2cmsg
495000981021     C                   END
495100981021     C*
495200980514     C                   ENDSR
495300980623      *****************************************************************
495400980623      *   CONFERMA DATI C. A.
495500980623      *****************************************************************
495600990408     C     AGGIORNA      BEGSR
495700980722
495800981020      * Per apertura stacco numero c.a.
495900990127     C                   if        I00MOD = 'A'
496000990127     C                   exsr      numprog
496100980623     c                   endif
496200980623      *
496300170607if  1c                   IF        *IN28 = *OFF
496400160523      *
496500981020      * Descrizioni
496600981020     C                   EXSR      WRTDCS
496700990127      * Fasi
496800990408      *     sempre ma in Modifica solo se: tolta o immessa rivalsa dopo la Stampa Avviso Danno,
496900990408      *                                    ristampa dell'avviso danno per importo trattenuto
497000990205     C                   if        I00MOD <> 'M'
497100990208     C                              OR  (DCTfca > WFsad  and  V2Crag <> WDKArag
497200990205     c                              and (V2Crag = *blanks or WDKArag = *blanks))
497300990208     C                              OR  (DCTfca <> WFRPL  and  DCLfrp = 'S'
497400990927     c                                and WDCLfrp = *blanks and DCLdds > *zeros
497500990927     c                                                     and  I00fca > WFrpl)
497600981020     C                   EXSR      WRTDCF
497700980623     c                   endif
497800080310      *     in MODIFICA tipo gestione : se cambiato tipo gestione da Pratica a LT devo anche
497900080310      *                                    impostare la fase 110 se sono già oltre la 100
498000080310     C                   if        I00MOD = 'M'
498100080310     C                             and  DCTfca >= WFcon  and  dctfpr <> v1cfpr
498200080310     C                   EXSR      WRTDCF
498300080310     c                   endif
498400981022      * Liquidazione C.A.
498500990408      *     se modificati importo concordato o importo trattenuto
498600080303     c*****              if            (*IN07 = *ON  and  DCLipr <> V2Cipr)
498700080303     c*****                        or  (*IN09 = *ON  and  DCLipt <> V2Cipt)
498800080303     c*****                        or  (*IN09 = *ON  and  DCLdpt <> WV2Cdpt)
498900080303      * tolto il controllo degli indicatori in quanto a volte cambiando da pratioca a LT si
499000080303      * perdono eventuali azzeramenti dei campi
499100080303     c                   if            DCLipr <> V2Cipr
499200080303     c                             or  DCLipt <> V2Cipt
499300080303     c                             or  DCLdpt <> WV2Cdpt
499400981022     C                   EXSR      WRTDCL
499500981022     c                   endif
499600981022      * Anagrafiche
499700990408      *     in Apertura solo se inserito negli altri casi sempre
499800981022     c                   if        I00mod <> 'A'  or  V1Cksc > *zeros
499900981105     c                              or  V2Crag <> *blanks
500000981020     C                   EXSR      WRTDKA
500100981022     c                   endif
500200981022      * Dettaglio C.A.
500300990408      *     se caricati segnacolli
500400990210     c                   IF        ultnrr1 > *zeros
500500981022     C                   EXSR      WRTDCD
500600990210     c                   ENDIF
500700120111
500800120111     c* In conferma per mancanza, chiudo eventuali anomalie 115 rimaste aperte
500900160523if  2c                   if           I00mod   = 'G' and §TADragr  =  'M'
501000120111     c     kca           setll     fndcd000
501100120111     c     kca           reade(N)  fndcd000                               31
501200120111      *
501300160523do  3C     *IN31         DOWEQ     *OFF
501400120111      *
501500120111     C                   IF        DCDatb = *blanks  and  DCDdch = *zeros
501600120112     c                             and dcdnsc>0
501700120111     c     kanm2         chain     fnanm02l
501800120111     c                   if        %found(fnanm02l) and anmdch=0
501900120111     C                   CLEAR                   fnlr33ds
502000120111     c                   eval      d33dch=dateu
502100120111     c                   eval      d33fsc='M'
502200120111     c                   eval      d33cch='AN'
502300120111     C                   MOVE      ANMNR2        D33NRR
502400120111     C                   CALL      'FNLR33R'
502500120111     C                   PARM                    fnlr33ds
502600120111     c                   endif
502700120111     c                   endif
502800120111     c
502900120111     c     kca           reade(N)  fndcd000                               31
503000160523e   3c                   enddo
503100160523e   2c                   endif
503200120111     c
503300990408      * Mandato Assicurativo
503400990408      *     se ho richiamato pgm per reperirlo
503500990408     c                   IF        Wfidn48r = 'S'
503600990408     C                   EXSR      WRTDPT
503700990408     c                   ENDIF
503800050805      * Testata Evento (se richiesto rimborso ad A.I.G.)
503900050805      *    Se aperta C.A. su evento già fleggato
504000050805      *     o modificato il numero evento su una C.A.
504100160523if  2c                   if           I00mod   = 'A'
504200050805     c                             or WV1Cnev <> SAVnev
504300050805     c                             or V1Caae  <> SAVaae
504400050805     c                   exsr      UpdDET
504500160523e   2c                   endif
504600981020      * Testata C.A.
504700981020     C                   EXSR      WRTDCT
504800050218
504900990607      * In Apertura sfleggo BLP per ritorno informazione al mittente
505000160523if  2C                   If        I00MOD = 'A'
505100990607     C                   EXSR      WRTBLP
505200050218      * scrivo il file TNTMD
505300050218     C                   EXSR      WRTTMD
505400050218      * aggiorno titas richiamando FIDN58R
505500050218     c                   clear                   fidn58ds
505600050218     c                   eval      i58aas = dctaas
505700050218     c                   eval      i58lnp = dctlnp
505800050218     c                   eval      i58nrs = dctnrs
505900050218     c                   eval      i58nsp = dctnsp
506000050218     c                   movel     fidn58ds      kpjbu
506100050218     c                   call      'FIDN58R'
506200050218     c                   parm                    kpjba
506300160523e   2c                   Endif
506400980623      *
506500160523e   1c                   ENDIF
506600050523      *
506700050523      *In Confermaper Avariase il massimo risarcibile supera?il
506800050601      *  valore tabellato => invio e-mail previste in tab. MRA
506900050523     c                   IF            I00mod    =  'G'
507000050523     c                             and §TADragr  =  'V'
507100050523     c                             and V2Cipv    >  §STDipa
507200081111     c                   eval      *in52 = *off
507300050523     c                   exsr      sr_InviaMail
507400050523     c                   ENDIF
507500081110      *
507600081110      *In Confermaper Mancanza/ammanchi  il massimo risarcibile supera?il
507700081110      *  valore tabellato => invio e-mail previste in tab. MRA
507800081110     c                   IF            I00mod    =  'G'
507900081110     c                             and (§TADragr  =  'M' OR §TADragr = 'A')
508000081110     c                             and V2Cipv    >  §STDima
508100081111     c                   eval      *in52 = *on
508200081110     c                   exsr      sr_InviaMail
508300081110     c                   ENDIF
508400080117      *
508500080117      *In Conferma C.A. relativa a FedEx con responsabilità "PARTNER"?
508600080117      *  => invio e-mail previste in tab. MRA
508700080117     c                   IF            I00mod    =  'G'
508800080117     c                             and I00fca    = WFcon
508900080117     c                             and §DCTeuro  =  'F'
509000080117     c                             and DCTres    =  'P'
509100080118     c                   exsr      sr_CallFIDN38
509200080117     c                   ENDIF
509300130802      *
509400160518      *In Conferma C.A. relativa a bolle Export (EUROEXPRESS) con?
509500160518      *  responsabilità "PARTNER"?
509600160728      *  confermata come Pratica Assicurativa?
509700160728      *  con importo massimo risarcibile > 100 € (DSTD.§STDIPDEEX)?
509800130802      *  => invio e-mail previste in tab. MRA
509900160729     c                   clear                   $EEX_Mail
510000130802     c                   IF            I00mod    =  'G'
510100130802     c                             and I00fca    = WFcon
510200130802     c                             and §DCTtisp  =  'E'
510300130802     c                             and §DCTeuro  =  'X'
510400130802     c                             and DCTres    =  'P'
510500160729     c                             and DCTfpr    =  'P'
510600160728     c                             and V2Cipv    >= §STDipdEEX
510700160518     c                   Select
510800160518      * -x lna 320 - 325?
510900160518     c                   When      DCTlna = 320  or  DCTlna = 325
511000130802     c                   exsr      sr_CallFIDN39
511100160729     c                   eval      $EEX_Mail = *on
511200160518      * -x lna 350?
511300160518     c                   When      DCTlna = 350
511400141007     c                   exsr      sr_CallFIDN41
511500160729     c                   eval      $EEX_Mail = *on
511600160523      * -x lna 311, 333, 334 e 340-345?
511700160523     c                   When      DCTlna = 311  or
511800160523     c                             DCTlna = 333  or
511900160523     c                             DCTlna = 334  or
512000160523     c                             DCTlna = 340  or  DCTlna = 345
512100160523     c                   exsr      sr_CallFIDN68
512200160729     c                   eval      $EEX_Mail = *on
512300160518     c                   EndSl
512400141007     c                   ENDIF
512500080516      *
512600080516      *In Conferma C.A. Liquidazione Transattiva invio lettera al cliente beneficiario
512700080516      *
512800080516     c                   IF            I00mod    =  'G'
512900080516     c                             and I00fca    = WFcon
513000080516     c                             and DCTfpr    =  'T'
513100130410      /free
513200130410       //?Chiusura automatica C.A. in caso di Avaria
513300130410           IF  §TADragr = 'V';
513400130410             exsr chiudiCA;
513500130410           ENDIF;
513600130410      /end-free
513700080516     c                   exsr      sr_CallFIDN67
513800080516     c                   ENDIF
513900980623      *
514000980623     C                   ENDSR
514100980623      *****************************************************************
514200980623      *   STACCO PROGRESSIVO C. A.
514300980623      *****************************************************************
514400980623     C     NUMPROG       BEGSR
514500981006      *
514600980623     C                   eval      *in30 = *on
514700980623     C                   eval      *in28 = *off
514800990113     C                   move      V1Cdca        W0040
514900990113     C                   move      W0040         AAS
515000980623     C                   Z-ADD     201           CNU
515100000224     c* invece di recuperare il numeratore del P.O. gestione lo recupero dal P.O. che sta
515200000224     c* aprendo la C.A. che potrebbe essere anche un P.O. di £6
515300000224     c                   if        i00tpb = 'P'
515400000224     c                   z-add     wlnp          COMLIN
515500000224     c                   endif
515600000224     c                   if        i00tpb = 'A'
515700000224     c                   z-add     wlna          COMLIN
515800000224     c                   endif
515900980623      *
516000980623    2C     *IN30         DOWEQ     *ON
516100980623    2C     *IN28         ANDEQ     *OFF
516200980623      *
516300980623     C     KNUF          CHAIN     FLNUF                              3031
516400980623      *
516500980623      * Se non trovato il numeratore provo a crearlo
516600980623    3C                   if        *IN30 = *ON
516700980623     C                   Z-ADD     CNU           PA4CNU
516800980623     C                   CALL      'FNLVCNR'
516900980623     C                   PARM                    PA4CNU
517000980623     C     KNUF          CHAIN     FLNUF                              3031
517100980623    3C                   endif
517200980623      *
517300980623      * Se ancora non l'ho trovato errore
517400980623     C   30              eval      *in28 = *on
517500981229     C   30              MOVEL     MSG(11)       V1CMSG
517600980623     C   31              eval      *in28 = *on
517700980623     C   31              MOVEL     MSG(12)       V1CMSG
517800980623      *
517900980904     c                   if        *IN28 = *OFF
518000980623     C                   ADD       1             NUFNUM
518100980623     C                   UPDATE    FLNUF
518200980623     C                   Z-ADD     NUFNUM        WNUMCA
518300990113     C                   move      V1Cdca        AA4
518400980904     c                   endif
518500990113      *
518600980623    2C                   ENDDO
518700980623      *
518800980623     C                   ENDSR
518900980623      *****************************************************************
519000980623      *  VARIAZIONE TIPO ANOMALIA
519100980623      *****************************************************************
519200971020     C     VARTAD        BEGSR
519300980615      *
519400980716     C                   MOVEL     VTCtad        W1CTAD
519500980716     C                   MOVEL     VTDtad        W1DTAD
519600980623      *
519700971020     C     EMW01         TAG
519800971020     C                   EXFMT     FI03W01
519900980623      *
520000990129     c                   eval      *in28 = *off
520100990129     c                   eval      *in29 = *off
520200971020     C                   CLEAR                   W1CMSG
520300980623      *
520400981019     C   KL              goto      etad
520500980623      *
520600971021     C     '?'           SCAN      W1CTAD                                 31
520700981019      *  Ricerca
520800980623    1C                   IF        *IN31 = *ON
520900981020     c                   clear                   TIBS02DS
521000980526     c                   movel     'R'           t02mod
521100980722     c                   movel     knsif         t02sif
521200980722     C                   movel     'TAD'         t02cod
521300980722     C                   CALL      'TIBS02R'
521400980722     c                   parm                    KPJBA
521500981020     c                   parm                    TIBS02DS
521600981019      *
521700981019      *  NO Ricerca
521800981019     C                   ELSE
521900981019     C                   movel(P)  W1CTAD        WKEY1
522000981026     C                   clear                   WTADtipo
522100981019     C                   exsr      CHTAD
522200981019     C                   ENDIF
522300981019      *
522400981019      *   Controlli di validità'
522500981019    2C                   IF        W1CTAD = *BLANKS
522600981019     C                   movel     MSG(1)        W1CMSG
522700990129     c                   eval      *in40 = *on
522800990129     c                   eval      *in28 = *on
522900981019     C                   ELSE
523000981019      *
523100981019     c                   IF        t02err <> *blanks
523200981019     c                   eval      *in28 = *on
523300981019     C                   movel     MSG(2)        W1CMSG
523400981019     C                   ELSE
523500981019      *
523600981019     C                   movel     t02ke1        W1CTAD
523700980526     c                   movel     t02uni        dtad
523800981019     C                   movel     §taddesc      W1DTAD
523900981019      * Se Anomalia per Eventi non è valida
524000981019     C                   IF        §TADragr = 'E'
524100990129     c                   eval      *in40 = *on
524200990129     c                   eval      *in28 = *on
524300981020     c                   movel     MSG(67)       W1CMSG
524400981019     C                   ENDIF
524500981019      *
524600981019     C                   ENDIF
524700981019     C                   ENDIF
524800981019      *
524900981026     c                   IF        *IN28 = *ON  or  *INKF = *OFF
525000981019     C                   goto      EMW01
525100981026     c                   ELSE
525200981026     C                   movel     W1Ctad        VTCtad
525300981026     C                   movel     W1Dtad        VTDtad
525400981026     c                   ENDIF
525500980623      *
525600971020     C     ETAD          ENDSR
525700980625      *****************************************************************
525800980615      *  REPERIMENTO DATI TIPO ANOMALIA
525900980625      *****************************************************************
526000971020     C     CHTAD         BEGSR
526100980615      *
526200981019     c                   if        WTADtipo = 'E'
526300981019     c                   clear                   DTADEVE
526400981019     C                   else
526500981019     c                   clear                   DTAD
526600981019     C                   endif
526700981019      *
526800981020     c                   clear                   TIBS02DS
526900980615     C                   MOVEL     'C'           t02mod
527000980615     C                   MOVEL     knsif         t02sif
527100980615     C                   MOVEL     'TAD'         t02cod
527200980615     C                   MOVEL(P)  Wkey1         t02ke1
527300980615      *
527400980615     C                   CALL      'TIBS02R'
527500980615     C                   PARM                    KPJBA
527600981020     C                   PARM                    TIBS02DS
527700980615      *
527800980615     C                   if        t02err = *BLANKS
527900981019      *
528000981019     c                   if        WTADtipo = 'E'
528100981019     C                   MOVEL     T02UNI        DTADEVE
528200981019     C                   MOVEL     WKEY1         V1Ctae
528300981019     C                   MOVEL     E§TADdesc     V1Dtae
528400981019     C                   else
528500980615     C                   MOVEL     T02UNI        DTAD
528600980716     C                   MOVEL     WKEY1         VTCtad
528700980716     C                   MOVEL     §TADdesc      VTDtad
528800980625     C                   endif
528900981019     C                   endif
529000980625      *
529100971020     C                   ENDSR
529200980623      *****************************************************************
529300981126      * IMPOSTO TIPO GESTIONE
529400980623      *****************************************************************
529500980625     C     impPROPR      BEGSR
529600980709
529700080208      *
529800980623      *---------------------------------------------------------------------------------------------
529900981127      *                  T I P O   G E S T I O N E
530000981127      *---------------------------------------------------------------------------------------------
530100060302      * _ PRATICA ASSICURATIVA non modificabile se
530200051116      *                            Evento fortuito,senza mandato o elevazione,
530300051116      *                            senza importo d'assicurare in bolla
530400051116      *                          * Porto Franco Import
530500080604      *                          * Spedizione EXPORT DPD o FedEx responsabilità PARTNER
530600080604      *                          * Spedizione EXPORT e beneficiario Destinatario
530700020419      * - LIQUID. TRANSATTIVA  modificabile se:
530800020419      *                            non rientra nei casi precedenti
530900020419      *                          e max risarcibile minore del limite
531000020419      * - PRATICA ASSICURATIVA modificabile se:
531100020419      *                            non rientra nei casi precedenti e
531200020419      *                          e (Kg danneggiati * limite risarcibile)
531300020419      *                             minore del limite in tabella
531400020419      * - LIQUID. TRANSATTIVA  modificabile se:
531500020419      *                            non rientra nei casi precedenti
531600980623      *---------------------------------------------------------------------------------------------
531700980625
531800980623     c                   SELECT
531900980623      *
532000060302     c                   WHEN           ((WV1Cnev > *zeros and                  Evento
532100051116     c                                   E§tadevfo = 'S'  and                   Fortuito
532200051116     c                                   Wmand = ' ' and                        Senza mandato/Elevaz
532300051116     c                                   VTCias = *zeros)                       Valore assicurato
532400051116     c                             or   (§DCTtisp = 'I'  and                    -import
532500080604     c                                   §DCTport = 'F')                         porto franco
532600080604     c                             or   (§DCTtisp = 'E'  and                    -export
532700080604     c                                  (§DCTeuro = 'D'   or                     DPD  o
532800080604     c                                   §DCTeuro = 'F') and                     FedEx
532900080604     c                                   V2Cres   = 'P')                         resp. Partner
533000080604     c                             or   (§DCTtisp = 'E'  and                    -export
533100080604     c                                   V1cptr   = 'D'))                        benef. destinatario
533200051116     c                   movel     'P'           Wfpr
533300051116     c                   movel     'S'           Wfprfrz
533400051116     c
533500980709      *
533600080208     c                   WHEN           V2Cipv <= W§STDllt                      Imp. Max risarcibile
533700981113     c                   movel     'T'           Wfpr
533800080208     c                   movel     'S'           Wfprfrz
533900051213      *
534000080208     c                   WHEN           V2Cipv > W§STDllt   and                 Imp. Max risarcibile
534100051213     c                                  (Wmand = 'S'  or                        Mandato/Elevazione
534200051213     c                                  VTCias > *zeros)                        Valore assicurato
534300051213     c                   movel     'P'           Wfpr
534400080208     c                   movel     'S'           Wfprfrz
534500981105      *
534600080208     c                   WHEN          (V2Cpkd * Wlimris) > W§STDllt  OR        Kg danneggiati
534700080208     c                                  V2Cipv > W§STDllt                       Imp. Max risarcibile
534800981105     c                   movel     'P'           Wfpr
534900080208     c                   movel     'S'           Wfprfrz
535000980623      *
535100980623     c                   OTHER
535200980709     c                   movel     'T'           Wfpr
535300080208     c                   movel     'S'           Wfprfrz
535400980623      *
535500980623     c                   ENDSL
535600080208      *---------------------------------------------------------------------------------------------
535700080208      *                  I N T E R L O C U T O R E
535800080208      *---------------------------------------------------------------------------------------------
535900080208      *   MITTENTE      non modificabile se spedizione export  ** NO **
536000080208      *   MITTENTE      non modificabile se spedizione import in porto
536100080208      *                      franco
536200080208      *   DESTINATARIO      modificabile se importo da assicurare in
536300080208      *                      bolla e spedizione in assegnato
536400080208      *   DESTINATARIO  non modificabile se spedizione import in porto
536500080208      *                      assegnato
536600080208      *   DESTINATARIO      modificabile se cliente con mandato e
536700080208      *                      spedizione in assegnato
536800080208      *   MITTENTE          modificabile negli altri casi
536900080208      *---------------------------------------------------------------------------------------------
537000080208
537100080208     c                   SELECT
537200080208      *
537300080208     c                   WHEN      §DCTtisp = 'I'  and  §DCTport = 'F'
537400080208     c                   movel     'M'           Wptr
537500080208      *
537600080208     c                   WHEN      §DCTport = 'A'  and  VTCias > *zeros
537700080208     c                   movel     'D'           Wptr
537800080208      *
537900080208     c                   WHEN      §DCTtisp = 'I'  and  §DCTport = 'A'
538000080208     c                   movel     'D'           Wptr
538100080208      *
538200080208     c                   WHEN      §DCTport = 'A'  and  WMAND = 'S'
538300080208     c                   movel     'D'           Wptr
538400080208      *
538500080208     c                   OTHER
538600080208     c                   movel     'M'           Wptr
538700080208      *
538800080208     c                   ENDSL
538900080208      *
539000980525     c                   endsr
539100990208      *****************************************************************
539200990208      *  CONTROLLO CONGRUITA' TRA IMPORTO TRATTENUTO E RIVALSA
539300990208      *****************************************************************
539400990208     C     CTR_IPTriv    BEGSR
539500990208
539600990208      *  IL CLIENTE TRATTIENE
539700990208      *-----------
539800990208     c                   IF        V2Cipt > *zeros
539900990208
540000990208     c                   SELECT
540100990208      *    Ho rivalsa Errore
540200990208     c                   WHEN        V2Crag <> *blanks
540300990208     c                   movel     MSG(69)       V1Cmsg
540400990208     c                   eval      *in77 = *on
540500990208     c                   eval      *in28 = *on
540600120712     *** *    Se non paga Vettore Errore
540700120712     ***C                   WHEN        V2Cpgd <> 'V'
540800120712     ***c                   movel     MSG(70)       V1Cmsg
540900120712     ***c                   eval      *in79 = *on
541000120712     ***c                   eval      *in28 = *on
541100990208      *    Se ho stampato P.L. e ho inserito l'importo trattenuto imposto flag per ristampa
541200120712     *** *     solo se chi paga era la Consuldanni
541300990927     c                   WHEN        DCLdds > *zeros   and  V2Cipt > *zeros  and
541400120712     c                               WV2Cipt = *zeros
541500120712     ***c                                                 and  WV2Cpgd = 'C'
541600990208     c                   movel     'S'           DCLfrp
541700990208     c                   ENDSL
541800990208
541900990208      *-----------
542000990208     C                   ELSE
542100990208      *-----------
542200990208
542300990208      *  HO TOLTO RIVALSA
542400990208     c                   IF        V2Crag <> WDKArag  and  V2Crag = *blanks
542500990208     c                                                and  Wnoriv = *blanks
542600990208     c                   movel     'S'           Wnoriv
542700120712     *** *       reimposto chi paga la prima volta
542800120712     ***c                   If        WCLSflo3 = *blanks
542900120712     ***c                   eval      V2cpgd = 'C'
543000120712     ***c                   Else
543100120712     ***c                   movel     Wclsflo3      V2Cpgd
543200120712     ***c                   Endif
543300990208      *    Se ho stampato P.L. imposto flag per ristampa
543400990208     c                   If        DCLdds > *zeros
543500990208     c                   movel     'S'           DCLfrp
543600990208     c                   Endif
543700990208     c                   ENDIF
543800990208
543900990208      *  HO IMMESSO RIVALSA
544000990208     c                   IF        V2Crag <> WDKArag  and  WDKArag = *blanks
544100990208     c                   clear                   Wnoriv
544200990208      *
544300990208     c                   SELECT
544400120712     *** *    Se paga Vettore Errore
544500120712     ***C                   WHEN        V2Cpgd = 'V'
544600120712     ***c                   movel     MSG(72)       V1Cmsg
544700120712     ***c                   eval      *in79 = *on
544800120712     ***c                   eval      *in28 = *on
544900990208      *    Se ho stampato P.L. imposto flag per ristampa
545000990208     c                   WHEN        DCLdds > *zeros
545100990208     c                   movel     'S'           DCLfrp
545200990208     c                   ENDSL
545300990208      *
545400990208     c                   ENDIF
545500990208
545600120712     *** *  HO MODIFICATO CHI PAGA
545700120712     ***c                   IF        *IN28 = *OFF  and  V2Cpgd <> WV2Cpgd
545800120712     *** *    Se non ho tolto rivalsa
545900120712     ***c                   IF        V2Crag = WDKArag  or  V2Crag <> *blanks
546000120712     *** *    Se ho stampato il P.L. e non è impostata la ristampa errore
546100120712     *** *****
546200120712     *** ****** Non emetto più errore sul campo del chi paga NON c'è più Consuldanni
546300120712     *** *****
546400120712     *** *
546500120712     ***c                   IF        DCLdds > *zeros  and  DCLfrp = *blanks
546600120712     ***c******             movel     MSG(74)       V1Cmsg
546700120712     ***c******             eval      *in79 = *on
546800120712     ***c******             eval      *in28 = *on
546900120712     ***c                   ELSE
547000120712     *** *    Se ho rivalsa e non paga Consuldanni errore
547100120712     ***c                   IF        V2Crag <> *blanks  and  V2Cpgd <> 'C'
547200120712     ***c******             movel     MSG(74)       V1Cmsg
547300120712     ***c******             eval      *in79 = *on
547400120712     ***c******             eval      *in28 = *on
547500120712     ***c                   ENDIF
547600120712     ***c                   ENDIF
547700120712     *** *
547800120712     ***c                   ENDIF
547900120712     ***c                   ENDIF
548000990208
548100990208      *-----------
548200990208     c                   ENDIF
548300990208
548400990208     C                   ENDSR
548500980615      *****************************************************************
548600980722      *  GESTIONE SCRITTURA DESCRIZIONI C.A.
548700980615      *****************************************************************
548800971022     C     WRTDCS        BEGSR
548900990121
549000990121      * Richiamo il pgm gestione descrizioni in modalità conferma (FLM='C') per la scrittura delle
549100990121      * note (per apertura e gestione) e delle descrizioni (per apertura).
549200990121     c                   IF        I00MOD = 'A'  or  I00MOD = 'G'
549300981020     C                   CLEAR                   FIDN10DS
549400980720     C                   movel     'C'           I10FLM
549500981216     C                   movel     'C'           I10TPD
549600981216     C                   movel     'N'           I10TRC
549700990107     c                   If        I00mod = 'A'
549800990107     c                   movel(P)  dsnca         I10nkt
549900990107     c                   Else
550000990107     c                   movel(P)  DS_numca      I10nkt
550100990107     c                   Endif
550200980618     c                   movel     i00fca        i10nks
550300981020     c                   movel(P)  FIDN10DS      kpjbu
550400980722     C                   CALL      'FIDN10R'
550500980429     C                   PARM                    kpjba
550600990121     c                   ENDIF
550700990121      *
550800990121     c                   IF        I00MOD = 'A'
550900981217     C                   CLEAR                   FIDN10DS
551000981217     C                   movel     'C'           I10FLM
551100981217     C                   movel     'C'           I10TPD
551200981217     C                   movel     'D'           I10TRC
551300990119     C                   clear                   I10STD
551400990107     c                   If        I00mod = 'A'
551500990107     c                   movel(P)  dsnca         I10nkt
551600990107     c                   Else
551700990107     c                   movel(P)  DS_numca      I10nkt
551800990107     c                   Endif
551900981217     c                   movel(P)  FIDN10DS      kpjbu
552000981217     C                   CALL      'FIDN10R'
552100981217     C                   PARM                    kpjba
552200990119     c                   ENDIF
552300990119
552400981216      * Aggiorno e/o scrivo record nel file descrizioni in base ai campi descrittivi che ho a video,
552500981216      * nel caso di aggiornamento della descrizione direttamente senza richiamo di FIDN10R.
552600981216      * E se sono in apertura richiamo il pgm gestione descrizioni in modalità conferma (FLM='C')
552700980721
552800980615      * DESCRIZIONE AGGIUNTIVA
552900980722     c                   IF        V1Dagg <> *blanks
553000980722     c                   movel     'A'           wstd
553100981216     c                   movel     v1dagg        wnot
553200980722     c                   exsr      wrtdcs_1
553300980722     c                   ENDIF
553400980721
553500980721      * MOTIVO DEL DANNO
553600980722     c                   IF        V1Dmtv <> *blanks
553700980722     c                   movel     'M'           wstd
553800980722     c                   movel     v1dmtv        wnot
553900980722     c                   exsr      wrtdcs_1
554000980722     c                   ENDIF
554100980721
554200980721      * PEZZI DANNEGGIATI/MANCANTI
554300980722     c                   IF        V2Dnpz <> *blanks
554400980722     c                   movel     'P'           wstd
554500980722     c                   movel     V2Dnpz        wnot
554600980722     c                   exsr      wrtdcs_1
554700980722     c                   ENDIF
554800980721
554900980721      * PEZZI PRESENTI
555000980722     c                   IF        V2Cprs <> *blanks
555100980722     c                   movel     'K'           wstd
555200980722     c                   movel     V2Cprs        wnot
555300980722     c                   exsr      wrtdcs_1
555400980722     c                   ENDIF
555500980721
555600981126      * INTERLOCUTORE
555700980722     c                   movel     'N'           wstd
555800980722     c                   movel     V1Dnoc        Wnot
555900980722     c                   exsr      wrtdcs_1
556000020125
556100020125      * MODIFICA FLAG FRANCHIGIA
556200020125     c                   IF            I00mod <> 'A'
556300020125     c                             and V1Cfra <> V1Hfra
556400020125     c                   clear                   wstd
556500020125     c                   IF        V1Cfra =  'F'
556600020125     c                   movel(p)  Comm_Fra_I    wnot
556700020125     c                   else
556800020125     c                   movel(p)  Comm_Fra_E    wnot
556900020125     c                   ENDIF
557000020125     c                   exsr      wrtdcs_2
557100020125     c                   ENDIF
557200080310
557300080310      * MODIFICA FLAG TIPO GESTIONE
557400080314     c                   IF            I00mod =  'M'
557500080310     c                             and V1Cfpr <> dctfpr
557600080310     c                   clear                   wstd
557700080310     c                   If        V1cfpr = 'T'
557800080310     c                   movel(p)  Comm_MTG_T    wnot
557900080310     c                   else
558000080310     c                   movel(p)  Comm_MTG_P    wnot
558100080310     c                   endif
558200080310     c                   exsr      wrtdcs_2
558300080310     c                   ENDIF
558400080313
558500080313      * MODIFICA beneficiario
558600080314     c                   IF            I00mod =  'M'
558700080313     c                             and V1Cptr <> dctptr
558800080313     c                   clear                   wstd
558900080313     c                   If        V1cptr = 'D'
559000080313     c                   movel(p)  Comm_MBEN_D   wnot
559100080313     c                   else
559200080313     c                   movel(p)  Comm_MBEN_M   wnot
559300080313     c                   endif
559400080313     c                   exsr      wrtdcs_2
559500080313     c                   ENDIF
559600981126
559700971022     C                   ENDSR
559800980722      *****************************************************************
559900981216      *  SCRITTURA DESCRIZIONI - CAMPO A VIDEO
560000980722      *****************************************************************
560100980722     C     WRTDCS_1      BEGSR
560200980722      *
560300980722     c                   IF        I00mod = 'A'
560400980722     c                   movel(P)  dsnca         knkt
560500980722     c                   ELSE
560600981029     c                   movel(P)  DS_numca      knkt
560700980722     c                   ENDIF
560800980722      *
560900980722     c                   movel     'C'           ktpd
561000980722     c                   movel     wstd          kstd
561100980722     c                   clear                   kdim
561200980722     c                   clear                   khim
561300980730     c                   clear                   knks
561400980722     c                   movel     'D'           ktrc
561500980722      *
561600980722     c     kdcs          chain     fndcs01l                           30
561700980722      *
561800980722     c     *in30         ifeq      *on
561900980722     c                   clear                   fndcs000
562000980722     c                   movel     ktpd          dcstpd
562100980722     c                   movel     knkt          dcsnkt
562200980722     c                   movel     kstd          dcsstd
562300980722     c                   movel     knks          dcsnks
562400980722     c                   movel     kdim          dcsdim
562500980722     c                   movel     khim          dcshim
562600980722     c                   movel     ktrc          dcstrc
562700980722     c                   z-add     1             dcspno
562800981005     c                   movel     I00fgs        dcspos
562900980722     c                   movel     knmus         dcspru
563000980722     c                   endif
563100980722      *
563200980722     c                   movel     Wnot          dcsnot
563300990104     c                   clear                   dcsatb
563400990104     c                   clear                   dcsft1
563500980722     c                   clear                   dcsdt1
563600980722      *
563700980722     c  n30              update    fndcs000
563800980722      *
563900981126      * INTERLOCUTORE  scrivo solo se <> blanks
564000980722     c                   if        *IN30 = *ON  and  Wstd <> 'N'
564100980722     c                   write     fndcs000
564200980722     c                   else
564300980722     c                   if        *IN30 = *ON  AND  Wstd = 'N'
564400980722     c                               AND  V1Dnoc <> *blanks
564500980722     c                   write     fndcs000
564600980722     c                   endif
564700980722     c                   endif
564800980722      *
564900980722     C                   ENDSR
565000020125      *****************************************************************
565100020125      *  SCRITTURA NOTE AGGIUTIVE
565200020125      *****************************************************************
565300020125     C     WRTDCS_2      BEGSR
565400020125      *
565500020125      * cerco l'ultimo progressivo note della fase in corso
565600020125      * - campi strandard
565700020125     c                   eval      Ktpd = 'C'
565800020125     c                   eval      Knkt = ds_numca
565900020125     c                   eval      Kstd = WSTD
566000020125     c                   eval      Kdim = dateu
566100020125     c                   eval      Khim = wora4
566200020125      * - dati relativi alla CA in manutenzione
566300020125     c                   movel(p)  I00fca        Knks
566400020125     c                   eval      Ktrc = 'N'
566500020125      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
566600020125     c     Kdcs          setgt     FNDCS01L
566700020125     c     Kdcs          readpe    FNDCS01L
566800020125      *
566900020125     c                   clear                   DCSatb
567000020125     c                   eval      DCStpd = Ktpd
567100020125     c                   eval      DCSnkt = Knkt
567200020125     c                   eval      DCSstd = Kstd
567300020125     c                   eval      DCSdim = Kdim
567400020125     c                   eval      DCShim = Khim
567500020125     c                   eval      DCSnks = Knks
567600020125     c                   eval      DCStrc = Ktrc
567700020125     c                   IF        %eof(FNDCS01L)
567800020125     c                   eval      DCSpno = 1
567900020125     c                   else
568000020125     c                   eval      DCSpno = DCSpno + 1
568100020125     c                   ENDIF
568200020125     c                   eval      DCSnot = WNOT
568300020125     c                   eval      DCSpru = KNMUS
568400020125     c                   eval      DCSpos = I00fgs
568500020125     c                   clear                   DCSstn
568600020125     c                   clear                   DCSft1
568700020125     c                   clear                   DCSdt1
568800020125      *
568900020125     c                   write     FNDCS000
569000020125      *
569100020125     C                   ENDSR
569200130409      *****************************************************************
569300130409      *  SCRITTURA NOTE AGGIUTIVE CHIUSURA CA
569400130409      *****************************************************************
569500130409     C     WRTDCS_3      BEGSR
569600130409      *
569700130409      * cerco l'ultimo progressivo note della fase 900
569800130409      * - campi strandard
569900130409     c                   eval      Ktpd = 'C'
570000130409     c                   eval      Knkt = ds_numca
570100130409     c                   eval      Kstd = WSTD
570200130409     c                   eval      Kdim = dateu
570300130409     c                   eval      Khim = wora4
570400130409      * - dati relativi alla CA in CHIUSURA
570500130409     c                   movel(p)  900           Knks
570600130409     c                   eval      Ktrc = 'N'
570700130409      * cerco l'ultimo progressivo riga relative alla fase attuale CA se esiste
570800130409     c     Kdcs          setgt     FNDCS01L
570900130409     c     Kdcs          readpe    FNDCS01L
571000130409      *
571100130409     c                   clear                   DCSatb
571200130409     c                   eval      DCStpd = Ktpd
571300130409     c                   eval      DCSnkt = Knkt
571400130409     c                   eval      DCSstd = Kstd
571500130409     c                   eval      DCSdim = Kdim
571600130409     c                   eval      DCShim = Khim
571700130409     c                   eval      DCSnks = Knks
571800130409     c                   eval      DCStrc = Ktrc
571900130409     c                   IF        %eof(FNDCS01L)
572000130409     c                   eval      DCSpno = 1
572100130409     c                   else
572200130409     c                   eval      DCSpno = DCSpno + 1
572300130409     c                   ENDIF
572400130409     c                   eval      DCSnot = WNOT
572500130409     c                   eval      DCSpru = KNMUS
572600130409     c                   eval      DCSpos = I00fgs
572700130409     c                   clear                   DCSstn
572800130409     c                   clear                   DCSft1
572900130409     c                   clear                   DCSdt1
573000130409      *
573100130409     c                   write     FNDCS000
573200130409      *
573300130409     C                   ENDSR
573400980623      *****************************************************************
573500980623      *  SCRITTURA FASI C.A.
573600980623      *****************************************************************
573700980623     c     WRTDCF        BEGSR
573800980623      *
573900980429     c                   clear                   fndcf000
574000980722      *
574100980623     c                   IF        I00MOD = 'A'
574200980722     c                   z-add     aa4           DCFaac
574300980722     c                   z-add     comlin        DCFfil
574400980722     c                   z-add     wnumca        DCFnca
574500980623     c                   ELSE
574600980722     c                   z-add     i00aac        DCFaac
574700980722     c                   z-add     i00fil        DCFfil
574800980722     c                   z-add     i00nca        DCFnca
574900980722     c                   movel     v1cptr        DCFptr
575000980623     c                   ENDIF
575100980722      *
575200990205      * Se Manutenzione forzo la fase
575300990205     c                   SELECT
575400990205     C                   WHEN      I00MOD = 'M'  and
575500990205     c                             DCTfca > WFsad  and  V2Crag <> WDKArag
575600990205     c                             and (V2Crag = *blanks or WDKArag = *blanks)
575700990205     c                   z-add     WFmri         DCFfca
575800990205     c                   clear                   DCFptr
575900990205      *
576000990205     C                   WHEN      I00MOD = 'M'  and
576100990205     c                             WDCLfrp = *blanks  and  DCLfrp = 'S'
576200990927     c                             and  DCLdds > *zeros  and  I00fca > WFrpl
576300990205     c                   z-add     WFrpl         DCFfca
576400990205     c                   clear                   DCFptr
576500080310      * modifica tipo gestione da P  a T
576600080310     C                   WHEN      I00MOD = 'M'  and
576700080310     c                             dctfpr <> v1cfpr
576800080310     c                   z-add     WFfff         DCFfca
576900080310     c                   clear                   DCFptr
577000990205      *
577100990205     c                   WHEN        I00mod <> 'M'
577200990205     c                   z-add     i00fca        DCFfca
577300990205     c                   ENDSL
577400981224      *
577500980722     c                   z-add     dateu         DCFdfc
577600000420     c                   movel     wora4         DCFhfc
577700980722     c                   z-add     i00fgs        DCFfev
577800980722     c                   movel     knmus         DCFpru
577900980907     c                   clear                   DCFftr
578000980907     c                   clear                   DCFdtr
578100001102     c                   clear                   DCFlet
578200020201     c                   clear                   DDCF01
578300980722      *
578400001102      * Verifico se devo tornare al cliente l'info di C.A. confermata
578500001102     c                   IF        DcfFca = WFcon
578600001102     c                   exsr      AGGT3K
578700001102     c                   IF        §3KCQ7 = 'S'
578800020201     c                   movel     'S'           §DCFriti
578900001102     c                   ENDIF
579000001102     c                   ENDIF
579100020125      *
579200020125      * Flag Franchigia - aggiornabile solo se:
579300020125      *  1) la C.A. è in gestione alla SEDE;
579400020125      *  2) ha come tipo gestione "Pratica Assicurativa";
579500020125      *  3) l'anno della spedizione relativa è oltre il 2001
579600020125      *  4) la C.A. è in una fase minore di 700 (progetto di liquidazione)
579700020125      *  5) L'importo da assicurare in bolla (TASIAS) è 0
579800020125      *  6) Non esiste un mandato per conto (/reale) nel file FNDPT00F
579900020201      * OPPURE se:
580000020201      *  1) era già stato valorizzato
580100020125    1c                   if        *in33 = *on
580200020201     c                   movel     V1Cfra        §DCFpfra
580300020201    1c                   else
580400020201     c                   movel     §DCTpfra      §DCFpfra
580500020125    1c                   endif
580600160728      *
580700160728      *?Impostazione Tipo Invio = "X" (Inviato Preavviso Danno al Partner):?
580800160728      *?SE conferma C.A.?
580900160728     c                   IF            I00mod    =  'G'
581000160728     c                             and I00fca    = WFcon
581100160728      *   ?relativa a bolle Export?
581200160728     c                             and §DCTtisp  =  'E'
581300160728      *   ?con partner EuroExpress?
581400160728     c                             and §DCTeuro  =  'X'
581500160728      *   ?con responsabilità "PARTNER"?
581600160901     c*//                          and DCTres    =  'P'
581700160901     c                             and ( *in08   =  *on
581800160901     c                             and   V2Cres  =  'P' )
581900160728      *   ?confermata come Pratica Assicurativa?
582000160901     c                             and V1Cfpr    =  'P'
582100160728      *   ?con importo massimo risarcibile > 100 € (DSTD.§STDIPDEEX)?
582200160728     c                             and V2Cipv    >= §STDipdEEX
582300160830      *   ?da inviare e-mail per una tra le LNA EuroExpress previste?
582400160830     c                             and ( DCTlna = 320  or  DCTlna = 325  or
582500160830     c                                   DCTlna = 350  or
582600160830     c                                   DCTlna = 311  or  DCTlna = 333  or
582700160830     c                                                     DCTlna = 334  or
582800160830     c                                   DCTlna = 340  or  DCTlna = 345 )
582900160728     c                   movel     'X'           §DCFinvi
583000160728     c                   endif
583100020201      *
583200020201     c                   movel     DDCF01        DcfLet
583300001102      *
583400980519     c                   write     fndcf000
583500980623      *
583600971022     C                   ENDSR
583700980623      *****************************************************************
583800980623      *  SCRITTURA/AGGIORNAMENTO DETTAGLIO C.A.
583900980623      *****************************************************************
584000971022     C     WRTDCD        BEGSR
584100980623      *
584200981020      *  Leggo SFL dettaglio segnacolli
584300980519     c                   z-add     1             nrr1
584400981029     c     nrr1          chain     FI03S04                            31
584500981029    2c     *IN31         DOWEQ     *OFF
584600980722      *
584700981020      *   Elaboro segnacolli non presenti su altre C.A.
584800981229     c     v1snsc        lookup    nsc                                    73
584900981229    3c                   IF         *IN73 = *OFF
585000980623      *
585100981113      *   Elaboro segnacolli selezionati oppure se modificata anomalia e non esclusi
585200020827    4c                   IF        VCCtsl = 'T'  or  V1Ssel = 'I'
585300981113     C                             or  (V1Scan <> V1Hcan  and  V1Ssel <> 'E')
585400981016     c     KDCD2D        chain     FNDCD002                           30
585500990317      *
585600990317      *   Se Inserimento ricalcolo responsabilità
585700990317     c                   if        Wcalresp = *blanks  and
585800990317     c                             (VCCtsl = 'T'  or  V1Ssel = 'I')
585900990208     c                   eval      Wcalresp = 'S'
586000990208     c                   endif
586100981016      *
586200981029      *   Clearizzo se: Inserimento, Apertura, Rcd annullato, Aggiorno rcd di altra C.A.
586300981029     c                   IF            *IN30  = *ON
586400981029     c                             or  I00mod = 'A'
586500981029     c                             or  DCDatb = 'A'
586600981029     c                             or  DS_numca <> DS_dcdkey
586700981016     c                   clear                   FNDCD002
586800981016     c                   ENDIF
586900981016      *
587000981020      *  Imposto i campi
587100981020     c                   IF        I00MOD = 'A'
587200981020     c                   z-add     aa4           dcdaac
587300981020     c                   z-add     comlin        dcdfil
587400981020     c                   z-add     wnumca        dcdnca
587500020827     c                   ELSE
587600981016     c                   z-add     i00aac        dcdaac
587700981016     c                   z-add     i00fil        dcdfil
587800981016     c                   z-add     i00nca        dcdnca
587900020827     c                   ENDIF
588000981029      *
588100981029     c                   z-add     v1sfls        dcdfls
588200990309     c                   z-add     v1hlna        dcdlna
588300981029     c                   z-add     i00nrs        dcdnrs
588400981016     c                   z-add     v1snsc        dcdnsc
588500981016     c                   movel     v1scan        dcdcan
588600981016     c                   clear                   dcdatb
588700981029     c                   clear                   dcddch
588800981029     c                   clear                   dcdcch
588900981016     c                   clear                   dcdftr
589000981016     c                   clear                   dcddtr
589100980720      *
589200020827    5c                   IF        *IN30 = *OFF
589300981016     c                   update    FNDCD002
589400980623     c                   ELSE
589500981016     c                   write     FNDCD002
589600980623    5c                   ENDIF
589700980623      *
589800980623    4c                   endif
589900980623      *
590000980722      * se non sono in apertura cancello eventuali records esclusi
590100020827    4c                   IF        I00MOD <> 'A'   and
590200981229     c                             V1Ssel = 'E'  and  V1Hi74 = '1'
590300981016     c     kdcd2c        chain     FNDCD002                           30
590400020827    5c                   IF        *IN30 = *OFF
590500980722     c                   movel     'A'           dcdatb
590600980907     c                   clear                   dcdftr
590700980907     c                   clear                   dcddtr
590800981016     c                   update    FNDCD002
590900990317      * ricalcolo responsabilità e non permetto variazioni manuali
591000990209     c                   if          Wcalresp = *blanks
591100990208     c                   eval      Wcalresp = 'S'
591200990317     c                   eval      *IN91 = *ON
591300990208     c                   endif
591400990208      *
591500020827    5c                   ENDIF
591600051019      * ...e già che ci sono: annullo i rec. corrispondenti in FNDCR
591700051019      *    (se in SEDE viene modificato il numero dei colli)
591800051019if  5c                   if             DUTlpo     = 'S'
591900051020     c                             and  WV2Cncn    > *zeros
592000051020     c                             and  V2Cncn     < WV2Cncn
592100051019     c                   exsr      AnnullaDCR
592200051019e   5c                   endif
592300020827    4c                   ENDIF
592400980722      *
592500020827    3c                   ENDIF
592600980623      *
592700980527     c                   add       1             nrr1
592800981029     c     nrr1          chain     FI03S04                            31
592900020827    2c                   ENDDO
593000980623      *
593100971022     C                   ENDSR
593200051019      *****************************************************************
593300051019      * ANNULLAM. RECORDS RELATIVI ALLE RESPONSABILITA' x SEGNACOLLO
593400051019      *****************************************************************
593500051019     c     AnnullaDCR    BEGSR
593600051020      *
593700051020     c                   if        NOT %open(FNDCR02L)
593800051020     c                   open      FNDCR02L
593900051020     c                   endif
594000051019      *
594100051019      * Annullo tutti rcd relativi al segnacollo in esame
594200051019     c                   movel     V1Snsc        DCRseg
594300051019     c     K04DCR02      setll     FNDCR000
594400051019     c     K04DCR02      reade     FNDCR000
594500051019      *
594600051019do  1c                   DOW       NOT %eof(FNDCR02L)
594700051019      *
594800051019if  2c                   if        DCRatb = *blanks
594900051019     c                   eval      DCRatb = 'A'
595000051019     c                   clear                   DCRftr
595100051019     c                   eval      DCRdtr = DateU
595200051019     c                   UPDATE    FNDCR000
595300051020      *   ...e (se trovo record specifici - per segnacollo)
595400051020      *   considero da NON ricalcolare la responsabilità
595500051020     c                   clear                   Wcalresp
595600051019e   2c                   endif
595700051019      *
595800051019     c     K04DCR02      reade     FNDCR000
595900051019      *
596000051019e   1c                   ENDDO
596100051019      *
596200051019     c                   ENDSR
596300981022      *****************************************************************
596400981022      *  SCRITTURA/AGGIORNAMENTO liquidazione C.A.
596500981022      *****************************************************************
596600981022     C     WRTDCL        BEGSR
596700981022      *
596800080303      * tolgo il controllo dell'indicatore in quanto non è detto che sia acceso se vengo da
596900080303      * una modifica di tipo gestione
597000080303     c*****              If        *in09 = *on
597100981022     c                   z-add     V2Cipt        DCLipt
597200981022     c                   movel     V2Cvpt        DCLvpt
597300981201     c                   z-add     WDCLcpt       DCLcpt
597400981201     c                   z-add     WV2Cdpt       DCLdpt
597500080303     c******             Endif
597600010907     c                   If        DCLipt = 0
597700010907     c                   clear                   DCLvpt
597800010907     c                   clear                   DCLcpt
597900010907     c                   clear                   DCLdpt
598000010907     c                   Endif
598100990128      *
598200080303     c                   If        *in07 = *on
598300990127     c                   z-add     V2Cipr        DCLipr
598400990127     c                   movel     V2Cvpr        DCLvpr
598500990127     c                   z-add     WDCLcpr       DCLcpr
598600990128     c                   z-add     V2Cipr        DCLipl
598700990128     c                   movel     V2Cvpr        DCLvpl
598800990128     c                   z-add     WDCLcpr       DCLcpl
598900080303     c                   Endif
599000010907     c                   If        DCLipr = 0
599100010907     c                   clear                   DCLvpr
599200010907     c                   clear                   DCLcpr
599300010907     c                   Endif
599400010907     c                   If        DCLipl = 0
599500010907     c                   clear                   DCLvpl
599600010907     c                   clear                   DCLcpl
599700010907     c                   Endif
599800990128      *
599900981022     c                   clear                   DCLftr
600000981022      *
600100981022     c                   If        Wfndcl = 'S'
600200981106     c                   UPDATE    FNDCL000
600300981022     c                   Else
600400990127      *
600500990127     c                   z-add     i00aac        DCLaac
600600990127     c                   z-add     i00fil        DCLfil
600700990127     c                   z-add     i00nca        DCLnca
600800981106     c                   WRITE     FNDCL000
600900981022     c                   Endif
601000981022      *
601100981022     C                   ENDSR
601200980722      *****************************************************************
601300981126      *  SCRITTURA/AGGIORNAMENTO anagrafica clienti per danni
601400980722      *****************************************************************
601500980520     C     WRTDKA        BEGSR
601600981022
601700981126      * BENEFICIARIO ALTERNATIVO
601800981022     c                   movel     'C'           Wtrc
601900981022     c     KDKA          chain     fndka01l                           30
602000981022     c                   move      V1Cksc        W0040
602100981022      *
602200990408      *     Se è codificato o non è impostato annullo FNDKA se esiste
602300981022     c                   IF        W0040 <> 8888
602400990408     c                   If          *IN30 = *OFF
602500990408     c                   movel     'A'           dkaatb
602600990408     c                   clear                   dkaftr
602700990408     c                   update    fndka000
602800990408    2c                   Endif
602900981022     c                   ELSE
603000981022      *
603100981022     c                   clear                   FNDKA000
603200981022     c                   If        I00mod <> 'A'
603300981022     c                   z-add     i00aac        dkaaac
603400981022     c                   z-add     i00fil        dkafil
603500981022     c                   z-add     i00nca        dkanca
603600981022     c                   Else
603700981022     c                   z-add     aa4           dkaaac
603800981022     c                   z-add     comlin        dkafil
603900981022     c                   z-add     wnumca        dkanca
604000981022     c                   Endif
604100981022      *
604200981102     c                   move      V1Cksc        DKAksc
604300981022     c                   movel     Wtrc          DKAtrc
604400050523     c                   movel     dutKCI        DKAkcc
604500981022     c                   movel     CW2Crag       DKArag
604600981022     c                   movel     CW2Cvia       DKAvia
604700981022     c                   movel     CW2Ccap       DKAcap
604800981022     c                   movel     CW2Cloc       DKAloc
604900981022     c                   movel     CW2Cprv       DKAprv
605000981022     c                   movel     CW2Cnaz       DKAnaz
605100981022     c                   eval      DKAiva = CW2Civa + CW2Cive
605200981022     c                   movel     CW2Ctel       DKAtel
605300981022     c                   movel     CW2Cfax       DKAfax
605400981022      *
605500981022      * Scrivo o aggiorno
605600981022     c                   If        *IN30 = *ON
605700981022     c                   write     fndka000
605800981022     c                   Else
605900981022     c                   update    fndka000
606000981022    2c                   Endif
606100981022      *
606200981022     c                   ENDIF
606300981022
606400981022      * RIVALSA
606500981022     c                   movel     'R'           Wtrc
606600981022     c     KDKA          chain     fndka01l                           30
606700981022      *
606800981022      *     Se non è impostato cancello FNDKA se esiste
606900981105     c                   IF        V2Crag = *blanks
607000990408     c                   If          *IN30 = *OFF
607100990408     c                   movel     'A'           dkaatb
607200990408     c                   clear                   dkaftr
607300990408     c                   update    fndka000
607400990408    2c                   Endif
607500981022     c                   ELSE
607600981022      *
607700981022     c                   clear                   FNDKA000
607800981022     c                   If        I00mod <> 'A'
607900981105     c                   z-add     I00aac        DKAaac
608000981105     c                   z-add     I00fil        DKAfil
608100981105     c                   z-add     I00nca        DKAnca
608200981022     c                   Else
608300981105     c                   z-add     aa4           DKAaac
608400981105     c                   z-add     comlin        DKAfil
608500981105     c                   z-add     wnumca        DKAnca
608600981022     c                   Endif
608700981022      *
608800981022     c                   movel     Wtrc          DKAtrc
608900050523     c                   movel     dutKCI        DKAkcc
609000981105     c                   z-add     8888          DKAksc
609100981105     c                   movel     DKAfil        DKAksc
609200981022     c                   movel     RW2Crag       DKArag
609300981022     c                   movel     RW2Cvia       DKAvia
609400981022     c                   movel     RW2Ccap       DKAcap
609500981022     c                   movel     RW2Cloc       DKAloc
609600981022     c                   movel     RW2Cprv       DKAprv
609700981022     c                   movel     RW2Cnaz       DKAnaz
609800981022     c                   eval      DKAiva = RW2Civa + RW2Cive
609900981022     c                   movel     RW2Ctel       DKAtel
610000981022     c                   movel     RW2Cfax       DKAfax
610100981022      *
610200981022      * Scrivo o aggiorno
610300981022     c                   If        *IN30 = *ON
610400981022     c                   write     fndka000
610500981022     c                   Else
610600981022     c                   update    fndka000
610700981022    2c                   Endif
610800981022      *
610900981022     c                   ENDIF
611000981022
611100981022     C                   ENDSR
611200990408      **********************************************************************
611300990408      *  SCRITTURA/AGGIORNAMENTO MANDATO ASSICURATIVO
611400990408      **********************************************************************
611500990408     C     WRTDPT        BEGSR
611600990428      *
611700990428      *  Riaggancio FNDPT se esiste
611800990428     c                   IF        Wfndpt = 'S'
611900990428     c     kca           chain     FNDPT000                           30
612000990428     c                   ENDIF
612100990408      *
612200990408     c                   SELECT
612300990408      *  Se ho mandato aggiorno
612400990408     c                   WHEN      WMAND = 'S'
612500990408     c                   clear                   fndpt000
612600990408     c                   If        I00mod = 'A'
612700990408     c                   z-add     aa4           dptaac
612800990408     c                   z-add     comlin        dptfil
612900990408     c                   z-add     wnumca        dptnca
613000990408     c                   Else
613100990408     c                   z-add     dctaac        dptaac
613200990408     c                   z-add     dctfil        dptfil
613300990408     c                   z-add     dctnca        dptnca
613400990408     c                   Endif
613500050523     c                   movel     dutKCI        dptkcc
613600990408     c                   z-add     I48ksc        dptksc
613700990408     c                   z-add     I48ctr        dptctr
613800990408     c                   z-add     O48prg        dptprg
613900990408     c                   movel     O48ftc        dptftc
614000990408     c                   movel     O48fvm        dptfvm
614100990408     c                   z-add     O48vlm        dptvlm
614200010913     c                   if        O48vlm <> 0
614300990408     c                   movel     O48vvm        dptvvm
614400010913     c                   else
614500010913     c                   clear                   dptvvm
614600010913     c                   endif
614700990408     c                   IF        Wfndpt = 'S'
614800990408     c                   update    FNDPT000
614900990408     c                   ELSE
615000990408     c                   write     FNDPT000
615100990408     c                   ENDIF
615200990428      *
615300990408      *  Se esiste il rcd lo annullo
615400990408     c                   WHEN      Wfndpt = 'S'  and  DPTatb <> 'A'
615500990408     c                   movel     'A'           DPTatb
615600990408     c                   clear                   DPTftr
615700990408     c                   update    FNDPT000
615800990408      *
615900990408     c                   ENDSL
616000990408      *
616100990408     C                   ENDSR
616200980717      **********************************************************************
616300990514      *  AGGIORNO BOLLA PARTENZA PER TRASMISSIONE AL MITTENTE
616400980717      **********************************************************************
616500990607     C     WRTBLP        BEGSR
616600990514
616700001102      * Aggancio 3K
616800001102     c                   exsr      AGGT3K
616900990602
617000990602      * Controllo se è previsto il ritorno delle consegne anomale
617100990607     C                   IF        §3KCQ4 = 'S'
617200990514
617300990514      * Sfleggo se ho FNBLP in linea
617400050310      * BOLLA MAMMA
617500050310     C     kbol02        chain     fnblp000                           30
617600020910
617700990607     C                   IF        *IN30 = *OFF
617800990607     C                   clear                   BLPft3
617900990607     C                   clear                   BLPdt3
618000990607     c                   update    fnblp000
618100990607     C                   ENDIF
618200990514      *
618300990607     C                   ENDIF
618400990514      *
618500990607     C                   ENDSR
618600050805
618700050805      **********************************************************************
618800050805      *  AGGIORNAMENTO TESTATA EVENTI
618900050805      **********************************************************************
619000050805     C     UpdDET        BEGSR
619100050805      *
619200050805      * Ripulisco il flag di RIMBORSO A.I.G. su FNDET00F
619300050805      * se  "P" = Ricezione totale del rimborso da AIG
619400050805      *  o  "N" = Evento non fortuito che ha le CA pagate ma < 10000€
619500050805      *
619600051121     c     Kdet          chain     FNDET000
619700050805      *
619800050805if  1c                   if        %found(FNDET01L)
619900050805     c                             and (DETftr = 'P'
620000050916     c                              or  DETftr = 'N' or
620100050916     c                             (DETftr = 'X' and ds_arbdat >= 20050418))
620200050805     c                   clear                   DETftr
620300050805     c                   UPDATE    FNDET000
620400050805e   1c                   endif
620500050805      *
620600050805     C                   ENDSR
620700020910
620800990514      **********************************************************************
620900990514      *  SCRITTURA/AGGIORNAMENTO TESTATA C.A.
621000990514      **********************************************************************
621100990514     C     WRTDCT        BEGSR
621200990514      *
621300980722      * se non sono in apertura chaino la testata c.a.
621400980722    2c                   IF        I00MOD <> 'A'
621500990114     c     knumca        chain     fndct000                           30
621600980722     c                   ELSE
621700980717      *  operazioni solo per i record nuovo (apertura)
621800980519     c                   clear                   fndct000
621900980519     c                   z-add     aa4           dctaac
622000980519     c                   z-add     comlin        dctfil
622100980519     c                   z-add     wnumca        dctnca
622200980519     c                   move      invdca        dctmgc
622300980519     c                   z-add     i00aas        dctaas
622400980519     c                   z-add     i00lnp        dctlnp
622500980519     c                   z-add     i00nrs        dctnrs
622600980519     c                   z-add     i00nsp        dctnsp
622700040720      *
622800040720      *   verifico la presenza di legami per valorizzare i campi del numero spedizione
622900040720      *
623000040720     c                   If        §dctlega = 'S'
623100040720      *  operazioni solo per i record nuovo (apertura)
623200040720     c                   clear                   fndck000
623300040720
623400040720      *   se ci sono legami verifico chi fa l'apertura
623500040720
623600040720      *   se ha aperto  la PARTENZA   la bolla mamma è in FIDN00DS e cerco la bolla figlia
623700040720     c                   If        i00tpb = 'P'
623800040720      * cerco l'ultima figlia in FNLBL
623900040720     c     kbol01        chain     FNLBL02L
624000040720do  3c                   dow       %found(FNLBL02L)
624100040720     c                   eval      DCKaas = LBLaan
624200040720     c                   eval      DCKlnp = LBLlpn
624300040720     c                   eval      DCKnrs = LBLnrn
624400040720     c                   eval      DCKnsp = LBLnsn
624500040721     c     klbl_f        chain     FNLBL02L
624600040720e   3c                   enddo
624700040720      *
624800040720     C                   ELSE
624900040720      * se ha aperto l'arrivo la bolla figlia è in FIDN00DS e devo cercare la bolla mamma
625000040720     c                   z-add     i00aas        dckaas
625100040720     c                   z-add     i00lnp        dcklnp
625200040720     c                   z-add     i00nrs        dcknrs
625300040720     c                   z-add     i00nsp        dcknsp
625400040720      * cerco la mamma in FNLBL
625500040720     c     kbol01        chain     FNLBL01L
625600040720     c                   If        %found(FNLBL01L)
625700040720     c                   eval      DCTaas = LBLaao
625800040720     c                   eval      DCTlnp = LBLlpo
625900040720     c                   eval      DCTnrs = LBLnro
626000040720     c                   eval      DCTnsp = LBLnso
626100040720     c                   endif
626200040720      *
626300040720     c                   endif
626400040720      * scrivo il record di FNDCK nel quale c'è sempre la figlia mentre in FNDCT c'è sempre la bolla
626500040720      * mamma
626600040720     c                   eval      DCKaac = DCTaac
626700040720     c                   eval      DCKfil = DCTfil
626800040720     c                   eval      DCKnca = DCTnca
626900040720     c                   write     FNDCK000
627000040720     c                   endif
627100040720
627200981229     c                   z-add     Wlna          dctlna
627300981229     c                   movel     Wlnp          §dctlnpc
627400981001     c                   movel     arbnas        dctnas
627500981228     c                   movel     DDCT01        dctflo
627600980722     c                   ENDIF
627700020125      *
627800020125      * Flag Franchigia - aggiornabile solo se:
627900020125      *  1) la C.A. è in gestione alla SEDE;
628000020125      *  2) ha come tipo gestione "Pratica Assicurativa";
628100020125      *  3) l'anno della spedizione relativa è oltre il 2001
628200020125      *  4) la C.A. è in una fase minore di 700 (progetto di liquidazione)
628300020125      *  5) L'importo da assicurare in bolla (TASIAS) è 0
628400020125      *  6) Non esiste un mandato per conto (/reale) nel file FNDPT00F
628500020201      * OPPURE se:
628600020201      *  1) era già stato valorizzato
628700020125    1c                   if        *in33 = *on
628800020125     c                   movel     V1Cfra        §DCTPfra
628900020125     c                   movel     DDCT01        dctflo
629000020125    1c                   endif
629100980709      *
629200990209      * Imposto valori per ricalcolo responsabilità
629300990209     c                   If        Wcalresp = *blanks
629400990209     c                             and  (I00mod = 'A'      or
629500051103     c                                  (DUTlpo  = 'S'
629600051103     c                             and   V2Cncn >= DCTncn) or
629700051103     c                                  (DUTlpo <> 'S'
629800051103     c                             and   V2Cncn <> DCTncn) or
629900990209     c                                   VTCtad <> DCTtad  or
630000000307     c                                   V1Ccrc <> DCTcrc  or
630100000307     c                                   WV1Cnev <> DCTnev)
630200990209     c                   eval      Wcalresp = 'S'
630300990209     c                   Endif
630400000307      *
630500000307     C                   IF        WV1Cnev > *zeros
630600000307     c                   z-add     v1caae        dctaae
630700000307     c                   z-add     WV1Cnev       dctnev
630800000307     c                   ELSE
630900000307     c                   clear                   dctaae
631000000307     c                   clear                   dctnev
631100000307     c                   ENDIF
631200990317      * Se ho diminuito il numero colli danneggiati impedisco modifiche alla responsabilità
631300990317     c                   If        V2Cncn < DCTncn
631400990317     c                   eval      *in91 = *ON
631500990317     c                   Endif
631600990317      *
631700990209     c                   movel     VTCtad        dcttad
631800990209     c                   z-add     V2Cncn        dctncn
631900980722     c                   movel     v1ccrc        dctcrc
632000990209      *
632100980716     c                   z-add     V2Cnpz        dctnpz
632200980716     c                   z-add     V2Cpkd        dctpkd
632300981019     c                   z-add     V2Cipv        DCTipv
632400010907     c                   IF        DCTipv <> 0
632500981019     c                   movel     V2Cvpv        DCTvpv
632600010907     c                   else
632700010907     c                   clear                   DCTvpv
632800010907     c                   ENDIF
632900010912     c                   movel     WDCTcpv       DCTcpv
633000981022     c                   movel     Wdctfpv       DCTfpv
633100981019     c                   move      V1Ctdt        dcttdt
633200981022     c                   move      V2Cdsd        dctdsd
633300080310      * verifico se modificato il tipo gestione modifico anche la fase
633400080310      *
633500080310     C                   If        I00MOD = 'M'  and
633600080310     c                             dctfpr <> v1cfpr
633700080310     C                             and  DCTfca >= WFcon
633800080310     c                   z-add     WFfff         DCTfca
633900080310     c                   else
634000981022     c                   z-add     I00fca        dctfca
634100080310     c                   endif
634200080310      *
634300981022     c                   movel     V1Cfpr        dctfpr
634400981022     c                   movel     V1Cptr        dctptr
634500120712     ***c                   movel     V2Cpgd        dctpgd
634600120712     c                   clear                   DCTpgd
634700050523     c                   z-add     dutKCI        dctkcc
634800981022     c                   z-add     V1Cksc        dctksc
634900981022     c                   move      V1Crcd        dctrcd
635000990128     c                   IF        *IN08 = *ON
635100981211     c                   move      V2Cres        dctres
635200981211     c                   ELSE
635300981211     c                   clear                   dctres
635400981211     c                   ENDIF
635500980722
635600990927      * Se C.A. in una fase superiore all'avviso danno e ho tolto o immesso rivalsa imposto come
635700990927      * fase C.A. "Modifica Rivalsa"
635800981224     c                   IF        DCTfca > WFsad  and  V2Crag <> WDKArag
635900981223     c                             and  (V2Crag = *blanks or WDKArag = *blanks)
636000990927     c                   z-add     WFmri         DCTfca
636100981223     c                   ELSE
636200990205
636300120712     *** * Se immesso l'importo trattenuto, già stampato il Progetto di Liquidazione, chi paga
636400120712     *** * era la Consuldanni e la fase attuale è maggiore di quella da impostare:
636500120712      * Se immesso l'importo trattenuto, già stampato il Progetto di Liquidazione
636600120712      * e la fase attuale è maggiore di quella da impostare:
636700990927      * forzo la fase "Ristampa Progetto di Liquidazione"
636800990205     c                   IF        WDCLfrp = *blanks  and  DCLfrp = 'S'
636900990205     c                             and  DCLdds > *zeros  and  I00fca >  WFrpl
637000120712     ***c                             and  WV2Cpgd = 'C'
637100990927     c                   z-add     WFrpl         DCTfca
637200990205     c                   ELSE
637300991001
637400991001      *  Se sono in fase "Stampa progetto liquidazione e non ricado nei casi precedenti non imposto
637500991001      *  FASE CA e PO GESTIONE
637600991001     c                   IF        I00fca <>  WFspl
637700981223
637800980717      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
637900990111      *  Se la fase ricavata è stampa lettera reso avaria o il suo sollecito la ritengo valida solo
638000990111      *   se la merce danneggiata è resa, in caso contrario ripeto la ricerca
638100981214     c                   z-add     DCTfca        WI05fca
638200981214      *
638300981214     c                   DO        *HIVAL
638400981214     c                   eval      *in30 = *on
638500981214     c                   exsr      SUB_POFASE
638600990111     c                   If        DCTdsd <> 'R'
638700981214     c                             and (DFAfca = WFlav  or  DFAfca = WFsla)
638800981214     c                   z-add     DFAfca        WI05fca
638900981214     c                   eval      *in30 = *off
639000981214     C                   Endif
639100981214     c  N30              ENDDO
639200981214
639300980722     c                   SELECT
639400980722     c                   WHEN      DFAgfs = 'P'
639500981229     c                   movel     §DCTlnpc      dctgfc
639600980722     c                   WHEN      DFAgfs = 'S'
639700980717     c                   z-add     46            dctgfc
639800981023     c                   WHEN      DFAgfs = 'A'
639900981023     c                   z-add     DCTlna        dctgfc
640000980722     c                   OTHER
640100981023     c                   z-add     DCTfil        dctgfc
640200980722     c                   ENDSL
640300981223      *
640400981223     c                   ENDIF
640500991001     c                   ENDIF
640600990205     c                   ENDIF
640700981223
640800981223      * Imposto flag per trasmissione dati
640900981223     c                   clear                   dctft1
641000981223     c                   clear                   dctft2
641100980722
641200981223      * Per Apertura imposto P.O. per trasmissione dati e scrivo
641300981223     c                   IF        I00MOD <> 'A'
641400990217     c                   UPDATE    fndct000
641500981223     c                   ELSE
641600981223      *
641700981223     c                   if         i00tpb = 'P'
641800981229     c                   z-add     DCTlna        dctfl1
641900980519     c                   else
642000981229     c                   movel     §DCTlnpc      dctfl1
642100981223     c                   endif
642200981230     c                   z-add     046           dctfl2
642300981230      *
642400981223     c                   WRITE     fndct000
642500981223     c                   ENDIF
642600980722      *
642700990217      * Richiamo per aggiornare flag visualizzazione responsabilita
642800990217     c                   clear                   kpjbu
642900990217     c                   CALL      'FIDN52R'
643000990217     c                   PARM                    KPJBA
643100990217     c                   PARM                    DS_DCTKEY
643200040705      *
643300040705      * Controllo se necessario rigenerare la spedizione in apertura CA
643400040705     c                   IF        I00MOD =  'A'
643500040705     C                   EXSR      CtrRigen
643600040705     c                   endif
643700040705      *
643800990217      *
643900971022     C                   ENDSR
644000990927      *----------------------------------------------------------------
644100000420      * VERIFICO SE INVIARE IN SEDE LA RICHIESTA DI RIGENERAZIONE SPEDIZIONE
644200990927      *----------------------------------------------------------------
644300000420     C     CtrRigen      BEGSR
644400990927      *
644500000628      * se spedizione in partenza e giorni pulizia di ARB impostati     oppure
644600000628      * se spedizione in arrivo   e giorni pulizia di BLP impostati
644700000628     c                   IF        (i00tpb = 'A'and  ARBdcm > *ZEROS  and
644800000628     c                              §5ABLP > 1)  or
644900000628     c                             (i00tpb = 'P'and  ARBdcm > *ZEROS  and
645000000628     c                              §5Aarb > 1)
645100000420     c     *iso          move      ARBdcm        Wdataeur
645200000628      * calcolo i giorni di pulizia di blp se sto aprendo in arrivo
645300000628     c                   if        i00tpb = 'A'
645400000420     c                   sub       1             §5ABLP
645500000420     c                   adddur    §5ABLP:*D     wdataeur
645600000628     c                   endif
645700000628      * calcolo i giorni di pulizia di arb se sto aprendo in partenza
645800000628     c                   if        i00tpb = 'P'
645900000628     c                   sub       1             §5AARB
646000000628     c                   adddur    §5AARB:*D     wdataeur
646100000628     c                   endif
646200000420      *
646300000420      * se sono trascorsi i gg dalla pulizia
646400000420     c                   IF        Wdataeur <= Wdataoggi
646500000420      *
646600000420     c                   If         i00tpb = 'A'
646700000420     c     kbol01        setll     fnblp000                               30
646800000420     c                   Else
646900000420     c     kbol01        setll     fnarb000                               30
647000000420     c                   Endif
647100000420      *
647200000420      * se la bolla non è in ARB o BLP
647300000420     c                   IF        *in30 = *off
647400000420      *
647500000420      * invio in sede una richiesta di rigenerazione spedizione
647600000420     c                   clear                   tibs20ds
647700000420     C                   MOVEL     'N'           i20VID
647800000420     C                   Z-ADD     arbAAS        i20AAS
647900000420     C                   Z-ADD     arbLNP        i20LNP
648000000420     C                   Z-ADD     arbNRS        i20NRS
648100000420     C                   Z-ADD     arbNSP        i20NSP
648200000420     C                   Z-ADD     arbLNA        i20LNA
648300000420     C                   MOVEL     'D'           i20CAR
648400000420     c                   If         i00tpb = 'A'
648500000420     C                   MOVEL     'P'           i20TRA
648600000420     C                   Else
648700000420     C                   MOVEL     'A'           i20TRA
648800000420     C                   Endif
648900000420     C                   Z-ADD     I00fgs        i20POR
649000000420     C                   MOVEL     KNMUS         i20PRU
649100000420     C                   Z-ADD     DATEU         i20DIM
649200000420     C                   Z-ADD     WORA4         i20HIM
649300000420      *
649400000420     C                   MOVEL(P)  tibs20ds      KPJBU
649500000420      *
649600000420     C                   CALL      'TIBS20R'
649700000420     C                   PARM                    KPJBA
649800000420      *
649900000420     c                   ENDIF
650000000420     c                   ENDIF
650100000420     c                   ENDIF
650200000420      *
650300000420     C                   ENDSR
650400050218      *----------------------------------------------------------------
650500050218      *  SCRITTURA/AGGIORNAMENTO TNTMD
650600050218      *----------------------------------------------------------------
650700050218     c     wrttmd        BegSr
650800050218
650900050310      * SEMPRE CON LA BOLLA MAMMA (campi di fndct)
651000050310     c     kbol02        Chain     Tntmd01l
651100050218
651200050310      * la cerco prima in partenza
651300050310     c     kbol02        chain(n)  fnblp000                           30
651400050310      * se non c'è in partenza la cerco in arrivo
651500050310     c   30kbol02        chain(n)  fnarb000                           30
651600050218
651700050218     c                   If        Not *In30
651800050218      * Scrivo un nuovo rcd
651900050218     c                   If        Not %Found(tntmd01l)
652000050218     c                   Write     Tntmd000
652100050310     c                   Else
652200050218      * Aggiorno rcd esistente
652300050218     c                   Update    Tntmd000
652400050218     c                   EndIf
652500050218
652600050218     c                   EndIf
652700050218
652800050218     c                   EndSr
652900981029      *****************************************************************
653000981214      *  RICAVO IL P.O. CHE GESTISCE LA FASE SUCCESSIVA
653100981029      *****************************************************************
653200981214     C     SUB_POFASE    BEGSR
653300981030      *
653400981214     c                   CLEAR                   DS_FNDFA
653500981214     c                   CLEAR                   FIDN05DS
653600981214      *
653700981214     c                   movel     'F'           I05MOD
653800981214     c                   z-add     WI05fca       I05fca
653900981214     c                   movel     DCTfpr        I05fpr
654000981214     c                   movel     DCTptr        I05tpc
654100981214     c                   if        WV1Cnev  >  *zeros
654200981214     c                   movel     'E'           I05fpe
654300981214     c                   else
654400981214     c                   movel     'N'           I05fpe
654500981214     c                   endif
654600981214     c                   if        DCTnde > *zeros
654700981214     c                   movel     'D'           I05fde
654800981214     c                   else
654900981214     c                   movel     'N'           I05fde
655000981214     c                   endif
655100981214     c                   movel     'O'           I05ffs
655200981214     c                   z-add     DCFdfc        i05dta
655300981214     c                   move      DCTtad        i05tad
655400981214      * valorizzo il numero c.a.
655500981214     c                   z-add     dctaac        i05aac
655600981214     c                   z-add     dctfil        i05fil
655700981214     c                   z-add     dctnca        i05nca
655800981214      *
655900981214     C                   CALL      'FIDN05R'
656000981214     c                   PARM                    KPJBA
656100981214     c                   PARM                    FIDN05DS
656200981214      *
656300981214     c                   movel     O05REC        DS_FNDFA
656400981214      *
656500981214     c                   ENDSR
656600990208      **********************************************************************
656700990208      *  GESTIONE RESPONSABILITA'
656800990208      **********************************************************************
656900990208     C     GESTRESP      BEGSR
657000990209     c                   clear                   FIDN50DS
657100990209      *
657200990209     c                   movel     'C'           I50mod
657300990209     c                   z-add     DCTaac        I50aac
657400990209     c                   z-add     DCTfil        I50fil
657500990209     c                   z-add     DCTnca        I50nca
657600040720      * passo il numero di spedizione sul quale ho aperto la CA (in caso di apertura su figlia
657700040720      * di dirottammento in arrivo ho la bolla figlia)
657800050915      * VISTO che il calcolo della responsabilità avviene anche in partenza e in sede Elisa
657900050915      * ha modificato il programma del calcolo della responsabilità in modo tale che riconosce
658000050915      * chi ha aperto la CA in base ai dati che vengono passati al pgm FIDN50R.
658100050915      * Quindi indipendentemente da chi richiama il calcolo della responsabilità (Partenza Arrivo
658200050915      * Sede) passiamo a FIDN50R la bolla figlia se esiste in FNDCK altrimenti la bolla di FNDCT
658300050915      * e la linea di partenza della CA del campo DCTFLO
658400050915      * AGGANCIO FNDCK
658500050915
658600050922     c     Kca           chain     fndck01l
658700050915     c                   If        %found(fndck01l)
658800050915     c                   z-add     DCKaas        I50aas
658900050915     c                   z-add     DCKlnp        I50lnp
659000050915     c                   z-add     DCKnrs        I50nrs
659100050915     c                   z-add     DCKnsp        I50nsp
659200050915     c                   else
659300050915     c                   z-add     DCTaas        I50aas
659400050915     c                   z-add     DCTlnp        I50lnp
659500050915     c                   z-add     DCTnrs        I50nrs
659600050915     c                   z-add     DCTnsp        I50nsp
659700050915     c                   endif
659800050912      *
659900050912     c                   if         i00tpb = 'S'                                SEDE
660000050915     c     kbol50        chain     titas30c                           30
660100050915      * Aggancio TITAS
660200050912     c                   else
660300050915     c     kbol50        chain(n)  fnarb000                           30
660400050329      * se non c'è in arrivo   la cerco in partenza
660500050915     c   30kbol50        chain(n)  fnblp000                           30
660600050912     c                   endif
660700040720      *
660800990209     c                   z-add     ARBlna        I50lna
660900990209     c                   z-add     ARBncl        I50ncl
661000990209     c                   z-add     ARBtfp        I50tfp
661100990209     c                   z-add     ARBtfa        I50tfa
661200990305     c                   z-add     VTCksm        I50ksm
661300990305     c                   z-add     VTCksd        I50ksd
661400050915      * passo anche la linea di partenza della CA
661500050915     c                   movel     §dctlnpc      I50lnpmam
661600090324      *  In Apertura calcolo sempre.
661700090804      *  Negli altri casi se il tipo calcolo è abilitato (A=automatico; " "=no respons.;R=ricalcolo)
661800090324      *  e se sono stati variati alcuni dati (colli, dettaglio colli, riserva, tipo anomalia,evento)
661900090324      *  se sono l'arrivo calcolo subito, se sono la partenza o la sede imposto il ricalcolo.
662000090324if  1c                   IF        Wcalresp = 'S'  and
662100090804     c                              (DCTcar = 'A'  or  DCTcar = 'R'
662200090324     c                                             or  DCTcar = *blanks)
662300990209      *
662400990209     c                   movel(P)  FIDN50DS      KPJBU
662500990209      *
662600050218     c                   Call      'FIDN50C'
662700990209     c                   Parm                    KPJBA
662800990209      *
662900990209     c                   movel     KPJBU         FIDN50DS
663000990209      *
663100990210      *  Se ho un errore imposto il flag per il ricalcolo
663200990210     c                   SELECT
663300990210     c                   WHEN        O50err = 'N'
663400990210     c                   clear                   Wcalresp
663500990210      *
663600990210     c                   WHEN        O50err = *blanks
663700990210     c                   eval      Wcalresp = O50car
663800990210      *
663900990210     c                   WHEN        O50err = 'E'
664000990210     c                   movel     O50msg        V6Cmsg
664100990210     c                   eval      Wcalresp = 'R'
664200990210     c                   ENDSL
664300990210      *
664400990210      *
664500051019x   1c                   ELSE
664600990210     c                   eval      Wcalresp = DCTcar
664700051019e   1c                   ENDIF
664800990212
664900990212      * Rialloco  FNDCT000
665000990212     c     kca           CHAIN     fndct000                           30
665100990217     c                   movel     dctflo        Ddct01
665200990217     c                   movel     §DCTlnpc      Wlnp
665300090324     c
665400090324     c                   if        wlnp= i50lnp
665500090324     c                   movel     i50tfp        wtfplnp
665600090324     c                   else
665700090324     c                   clear                   fnlv55ds
665800090324     c                   movel     'P'           d55tpt
665900090324     c                   movel     wlnp          d55lin
666000090324     c                   if        ds_arbdat>0
666100090324     c                   z-add     ds_arbdat     d55drf
666200090324     c                   else
666300090324     c                   move      dctmgc        d55drf
666400090324     c                   movel     dctaac        d55drf
666500090324     c                   endif
666600090324     c                   call      'FNLV55R'
666700090324     c                   parm                    fnlv55ds
666800090324     c                   movel     d55tfp        wtfplnp
666900090324     c                   endif
667000090324     c
667100990212
667200990406      *  Visualizzo responsabilità
667300990209     c                   SELECT
667400990210      *    se il pgm di calcolo ha riscontrato errori.
667500990210     c                   WHEN         V6Cmsg <> *blanks
667600990210     c                   exsr      CARresp
667700990210     c                   exsr      GESresp
667800990406      *    no se la responsabilità non deve essere considerata
667900990217     c                   WHEN          DCTtcr = 'S'
668000990406     c                   if        *in04 = *on
668100990406     c                   eval      O00err = 'E'
668200990406     c                   movel     msg(106)      O00msg
668300990406     c                   endif
668400990210      *    se apertura
668500990210     c                   WHEN          I00mod = 'A'
668600990210     c                   exsr      CARresp
668700990210     c                   exsr      GESresp
668800990210      *    se manutenzione in sede
668900990210     c                   WHEN          SIMFEL = *zeros
669000990210     c                   exsr      CARresp
669100990210     c                   exsr      GESresp
669200990210      *    in filiale solo per profili abilitati
669300130806     c************       WHEN          %SUBST(knmus:1:3) = §STDpv1  or
669400130806     c************                     %SUBST(knmus:1:3) = §STDpv2
669500130806     c                   WHEN          §UTEmresca = 'S'
669600990208     c                   exsr      CARresp
669700990208     c                   exsr      GESresp
669800990209     c                   ENDSL
669900990208
670000990210      *  Aggiorno FNDCT
670100990210     c                   eval      DCTcar = Wcalresp
670200990614     c                   clear                   DCTft1
670300990614     c                   clear                   DCTft2
670400990210     c                   UPDATE    FNDCT000
670500990210
670600990208     c                   ENDSR
670700990208      **********************************************************************
670800990208      *  IMPOSTO DATI VIDEO RESPONSABILITA'
670900990208      **********************************************************************
671000990209     C     CARresp       BEGSR
671100990208
671200990209      * TESTATA
671300990208     c                   IF          I00mod = 'A'
671400990215     c                   z-add     DCTfil        vtcfil
671500990215     c                   z-add     DCTnca        vtcnca
671600990209     c                   z-add     Wdataca       VTCdc1
671700990208     C                   z-add     DCTfca        VTCfca
671800990209      *
671900990208     c                   CLEAR                   FIDN05DS
672000990208     c                   movel     'C'           I05MOD
672100990208     c                   z-add     VTCfca        I05fca
672200990208     C                   CALL      'FIDN05R'
672300990208     c                   PARM                    KPJBA
672400990208     c                   PARM                    FIDN05DS
672500990208     c                   If        O05err <> 'E'
672600990208     c                   movel     O05REC        DS_FNDFA
672700990208     c                   movel     DFAdes        VTDfca
672800990208     c                   Endif
672900990208      *
673000990209     C                   z-add     DCTgfc        VTCgfc
673100990208     C     VTCgfc        chain     AZORG                              31
673200990208     C  N31              if        ORGDE5 <> *BLANKS
673300990208     C                   movel     ORGDE5        VTDgfc
673400990208     C                   else
673500990208     C                   movel     ORGDES        VTDgfc
673600990208     C                   endif
673700990208      *
673800990208     c                   ENDIF
673900990208      *
674000990210     C                   z-add     DCTncn        VTCncn
674100990208     c                   IF        VTClna = *zeros
674200990208     C                   move      DCTlna        VTClna
674300990208     C     VTClna        chain     AZORG                              31
674400990208     C  N31              If        ORGDE5 <> *BLANKS
674500990208     C                   movel     ORGDE5        VTDlna
674600990208     C                   Else
674700990208     C                   movel     ORGDES        VTDlna
674800990208     C                   Endif
674900990208     C                   ENDIF
675000990210      *
675100990210     c                   SELECT
675200990210     c                   WHEN       Wcalresp = *blanks
675300990210     c                   eval      V6Cdca = 'Nessuna responsabilità'
675400990210     c                   WHEN       Wcalresp = 'M'
675500990210     c                   eval      V6Cdca = 'Manuale'
675600990210     c                   WHEN       Wcalresp = 'A'
675700990210     c                   eval      V6Cdca = 'Automatico'
675800990210     c                   OTHER
675900990210     c                   eval      V6Cdca = 'In attesa di ricalcolo'
676000990210     c                   ENDSL
676100990210
676200990209      * SUBFILE
676300990209      *    Pulisco Subfile
676400990209     C                   eval      *IN21 = *OFF
676500990209     C                   WRITE     FI03C05
676600990210     C                   eval      *IN21 = *ON
676700990210     C                   eval      *IN20 = *ON
676800990209     C                   z-add     *ZEROS        NRR5
676900990210      *
677000990216      *  Se non proteggo la videata carico segnacolli C.A.
677100990317     c                   IF            I00mod = 'A'                             apertura
677200990317     c                             or  V6Cmsg <> *blanks                        errori calcolo resp.
677300990317     c                             or  DCTcar = 'T'  or  DCTcar = 'R'           attesa di ricalcolo
677400990317     c                             or  *in91 = *ON                              escluso segnacollo
677500990216     c                   eval      *in91 =  *on
677600990216     c                   ELSE
677700990216     c                   clear                   JJ
677800990216     c     kca           setll     fndcd000
677900990216     c     kca           reade(N)  fndcd000                               31
678000990216     C     *IN31         DOWEQ     *OFF
678100990216     c                   if        DCDatb = *blanks
678200990216     c                   add       1             JJ
678300990216     c                   z-add     DCDnsc        SEC(jj)
678400990306     c                   endif
678500990216     c     kca           reade(N)  fndcd000                               31
678600990216     c                   ENDDO
678700990216     C                   ENDIF
678800990210      *
678900990217      *   Carico Subfile
679000051020     c                   if        NOT %open(FNDCR02L)
679100990217     c                   OPEN      FNDCR02L
679200051020     c                   endif
679300990209     c     Kca           CHAIN     FNDCR000                           30
679400990209      *
679500990209     c     *in30         DOWEQ     *off
679600990315      *
679700990315     c                   IF        DCRatb = *blanks
679800990315      *
679900990216     c                   movel     DCRflo        DDCR01
680000990217      *       Controllo se segnacollo numerico
680100990217     C                   clear                   V6Snsc
680200990217     C                   testn                   §DCRnsc              31
680300990217     C                   If        *IN31 = *on
680400990217     C                   move      §DCRnsc       W001A
680500990217     c                   eval      *in31 = (W001A >= '0')
680600990217     C   31              move      §DCRnsc       V6Snsc
680700990217     C                   Endif
680800990218     C                   movel     V6Snsc        V6Hnsc
680900990217     C                   movel     V6Snsc        W007a
681000990217      *
681100990209     c                   z-add     DCRper        V6Sper
681200990217     c                   z-add     DCRper        V6Hper
681300990218     C                   movel     V6Sper        W005a
681400990219     c                   if         DCRpor > *zeros
681500990210     c                   movel     DCRpor        V6Spor
681600990217     c                   movel     DCRpor        V6Hpor
681700990219     c                   else
681800990219     c                   clear                   V6Spor
681900990219     c                   clear                   V6Hpor
682000990219     c                   endif
682100990217      *
682200990209     C     DCRpor        chain     AZORG                              31
682300990217     c                   Select
682400990217     c                   When      *in31 = *on
682500990217     C                   clear                   V6Sdpo
682600990217     c                   When      ORGde5 <> *blanks
682700990217     C                   movel(P)  ORGde5        V6Sdpo
682800990217     c                   Other
682900990217     C                   movel(P)  ORGdes        V6Sdpo
683000990217     c                   Endsl
683100131105
683200131105     C                   clear                   V6Stpr
683300131105     C                   move      §DCRter       V6Stpr
683400131105     C                   move      V6Stpr        V6Htpr
683500990217      *
683600990209     C                   movel     DCRtcr        V6Htcr
683700990209     C                   clear                   V6Sdtc
683800990209     C                   exsr      decodTCR
683900990218      *
684000990217     C                   movel     §DCRtre       V6Stre
684100990217     C                   movel     §DCRtre       V6Htre
684200990217     C                   exsr      decodTRE
684300990217     C                   movel     §TREdesc      V6Sdtr
684400990217     C                   movel     §TREcalc      V6Hcal
684500990210      *
684600990209     c                   add       1             NRR5
684700131106     c                   eval      RS1(nrr5) =W007a+W005a+V6Spor+V6Stre+v6stpr
684800990209     C                   WRITE     FI03S05
684900990315      *
685000990315     c                   ENDIF
685100990210     c     Kca           READE     FNDCR000                               30
685200990315     c                   ENDDO
685300990210      *
685400990209      *    Scrivo rcd vuoti
685500990209     c                   IF        *in91 = *off
685600990210     c                   eval      W0040 = (NRR5 + 1)
685700000308     c                   eval      W0040B = (W0040 + 20)
685800990217     c                   clear                   V6Snsc
685900990217     c                   clear                   V6Hnsc
686000990217     C                   movel(p)  V6Snsc        W007a
686100990217     c                   clear                   V6Sper
686200990217     c                   clear                   V6Hper
686300990218     C                   movel(p)  V6Sper        W005a
686400990209     c                   clear                   V6Spor
686500990217     c                   clear                   V6Hpor
686600131105     c                   clear                   V6Stpr
686700131105     c                   clear                   V6Htpr
686800990209     C                   clear                   V6Sdpo
686900990209     C                   clear                   V6Sdtc
687000990217     c                   clear                   V6Htcr
687100990217     C                   clear                   V6Stre
687200990217     c                   clear                   V6Htre
687300990217     c                   clear                   V6Hcal
687400990217     C                   clear                   V6Sdtr
687500131106     c                   eval      W022a =  W007a+W005a+V6Spor+V6Stre+v6stpr
687600000308     c     W0040         DO        W0040B        NRR5
687700131105     c                   movel     W022a         RS1(nrr5)
687800990209     C                   WRITE     FI03S05
687900990209     c                   ENDDO
688000990209     c                   ENDIF
688100990209      *
688200990210     c                   SORTA     RS1
688300990210      *
688400990209     c                   ENDSR
688500990209      **********************************************************************
688600990209      *  EMETTO VIDEATA RESPONSABILITA'
688700990209      **********************************************************************
688800131106     C     GESresp       BEGSR
688900990208      *
689000990209      * I colli per responsabilità sono quelli danneggiati o se zero i colli spedizione
689100990209     c                   IF          VTCncn = *zeros
689200990209     c                   z-add     VTCncl        Wcolli
689300990209     c                   ELSE
689400990209     c                   z-add     VTCncn        Wcolli
689500990209     c                   ENDIF
689600990209      *
689700990209     c                   eval      *in28 = *off
689800990209     c                   eval      *inKF = *off
689900990209     c                   z-add     1             V6Cnrr
690000990209      *
690100990209     C     EMD05         TAG
690200990209      *
690300990219     c                   IF        *in28 = *ON
690400990219     c                   z-add     nrr5          V6Cnrr
690500990219     c                   ELSE
690600990219     c                   z-add     1             V6Cnrr
690700990219     c                   ENDIF
690800990218      *
690900990209     c                   write     FI03T05
691000990209     c                   write     FI03Z05
691100990209     c                   EXFMT     FI03C05
691200990209      *
691300990209     c                   eval      *in28 = *off
691400990219     c                   eval      *in29 = *off
691500990210     c                   clear                   V6Cmsg
691600990209     c                   clear                   nrr5
691700990209      *
691800990317     c                   IF        *IN91 = *off
691900990317     c                   exsr      CTRD05
692000990317     c                   ENDIF
692100990209      *
692200990219     c                   If        *in28 = *on  or  *in29 = *on  or
692300990210     c                             (*in91 = *off  and  *inKf = *off)
692400990209     c                   goto      emd05
692500990209     c                   Endif
692600990210      *
692700990210      * Se la responsabilità è stata modificata aggiorno FNDCR
692800990210     c                   IF        *IN91 = *off
692900000307     c                   DO        900           jj
693000990218     c                   if        RS1(jj) <> RS2(jj)
693100990210     c                   exsr      WRTDCR
693200000307     c                   eval      jj = 900
693300990210     c                   eval      Wcalresp = 'M'
693400990210     c                   endif
693500990210     c                   ENDDO
693600990210     c                   ENDIF
693700990210      *
693800990208     c                   ENDSR
693900990209      *****************************************************************
694000990209      *  CONTROLLI VIDEO 5
694100990209      *****************************************************************
694200990209     C     CTRD05        BEGSR
694300990209      *
694400990217     c                   clear                   PER
694500990218     c                   clear                   RSU
694600990218     c                   clear                   RS2
694700990209     c                   z-add     1             nrr5
694800990209      *
694900990209     C     nrr5          chain     FI03S05                            3232
695000990209      *
695100990209    1C     *IN32         DOWEQ     *OFF
695200990209      *
695300990209     C                   eval      *in92 = *off
695400990209     C                   eval      *in93 = *off
695500990219     C                   eval      *in94 = *off
695600990219     C                   eval      *in95 = *off
695700131105     C                   eval      *in96 = *off
695800990209      *
695900990217      * clearizzo se eliminato campo
696000990217     c                   exsr      cleard05
696100990217      *
696200990218      *   Controllo che il segnacollo sia nei selezionati
696300990218     c                   z-add     1             jj
696400990218     c     V6Snsc        Lookup    SEC(jj)                                30
696500990218      *
696600990218     c                   IF          *IN30 = *OFF
696700990218     c                   eval      *in28 = *on
696800990218     c                   eval      *in92 = *on
696900990218     c                   movel     MSG(30)       V6Cmsg
697000990218     c                   ELSE
697100990218      *
697200990218      *  Memorizzo colli per segnacollo
697300990218     c                   add       V6Sper        PER(jj)
697400990218     c                   z-add     V6Snsc        V6Hnsc
697500990218     C                   ENDIF
697600990217     C   28              goto      ECTR05
697700120619     c
697800120619     c* controllo che il totale punti respsonsabilità per segnacollo non siano superiori al
697900120619     c*  massimo previsto che è 3
698000120619     c                   if        v6snsc>0 and per(jj)>§stdpermax
698100120619     c                   eval      *in28 = *on
698200120619     c                   eval      *in92 = *on
698300120619     c                   movel     MSG(120)      V6Cmsg
698400120619     c                   eval      %subst(v6cmsg:73:6)=%editc(§stdpermax:'4')
698500120619     C                   goto      ECTR05
698600120619     C                   ENDIF
698700990209      *
698800990217      * Se variato/inserito il P.O. controllo
698900990209     c                   IF        V6Spor <> V6Hpor
699000131106     c                   eval      *in29=*on
699100990217     c                   exsr      CTR_POresp
699200990209     c                   ENDIF
699300990209     C   28              goto      ECTR05
699400131105      *
699500990217      *
699600990217      * Se variato/inserito il Tipo Attribuzione controllo
699700990217     c                   IF         V6Stre <> V6Htre
699800990217     C                   exsr      decodTRE
699900990217      *
700000990217     c                   IF           §TREdesc = *blanks
700100990217     c                   eval       *in28 = *on
700200990217     c                   eval       *in95 = *on
700300990218     c                   movel     MSG(92)       V6Cmsg
700400990217     c                   ELSE
700500990217     C                   movel     V6Stre        V6Htre
700600990217     C                   movel     §TREdesc      V6Sdtr
700700990217     C                   movel     §TREcalc      V6Hcal
700800990217     c                   ENDIF
700900990217     c                   ENDIF
701000990219     c                   IF         *IN28 = *ON  or  *IN29 = *ON
701100990219     C                   goto      ECTR05
701200990219     c                   ENDIF
701300131106      * Se variato/inserito il terminal controllo
701400131107      * Se vuoto lo calcolo in base al p.o. responsabile
701500131106     c                   IF        V6Stpr <> V6Htpr  or (v6stpr=*blanks
701600131106     c                             or v6stpr='000')
701700131106     c                   exsr      CTR_TPresp
701800131106     c                   ENDIF
701900131106     c                   IF         *IN28 = *ON  or  *IN29 = *ON
702000131106     C                   goto      ECTR05
702100131106     c                   endif
702200990209      *
702300990218      * controllo: se inseriti dati parziali, se dati corretti per il tipo attribuzione,
702400990218      *            se inserimento univoco per segnacollo-P.O.-Attribuzione
702500990218     C                   exsr      ctrd05_con
702600990218     C   28              goto      ECTR05
702700990217      *
702800990217      * Memorizzo inserimenti per controllo variazioni
702900990217     C                   movel     V6Snsc        W007a
703000990218     C                   movel     V6Sper        W005a
703100131107     c                   eval      RS2(nrr5) =W007a+W005a+V6Spor+V6Stre+ v6stpr
703200990209      *
703300990209     C     ECTR05        TAG
703400990209     C                   UPDATE    FI03S05
703500990209      *
703600990212     c                   add       1             nrr5
703700990210      *
703800000308     c                   IF        *IN28 = *ON  or  *IN29 = *ON
703900990210     c                   eval        *IN32 = *ON
704000990210     c                   ELSE
704100990210     c     nrr5          chain     FI03S05                            32
704200990210     c                   ENDIF
704300990209      *
704400990209    1C                   ENDDO
704500990218      *
704600990218      * Controllo: che i colli inseriti corrispondano ai colli danneggiati,
704700990218      *            che per segnacollo sia inserito un collo.
704800990219     c                   IF         *IN28 = *OFF  and  *IN29 = *OFF
704900990218     c                   xfoot     PER           W0133
705000090303     c                   IF         Wcolli > W0133
705100990218     C                   eval      *in28 = *on
705200990219     C                   eval      nrr5 = 1
705300990218     C                   movel     MSG(60)       V6Cmsg
705400990218     c                   ELSE
705500120619     c
705600120619     c* controllo anche i punti in totale
705700120619     c                   eval      ToTpunti=Wcolli * §stdrsval
705800120619     c                   if        w0133>totpunti
705900120619     C                   eval      *in28 = *on
706000120619     C                   eval      nrr5 = 1
706100120619     C                   movel     MSG(121)      V6Cmsg
706200120619     c                   eval      %subst(v6cmsg:57:6)=%editc(§stdpermax:'4')
706300120619     c                   endif
706400990218      *
706500990209     c                   ENDIF
706600990218     c                   ENDIF
706700990210      *
706800990210     c  N28              SORTA     RS2
706900990209      *
707000990209     c                   ENDSR
707100990210      *****************************************************************
707200990217      *  CLEARIZZO SE ELIMINATI CAMPI
707300990210      *****************************************************************
707400990217     C     CLEARD05      BEGSR
707500990217      *
707600990217      * Segnacollo
707700990217     c                   IF        V6Snsc = *zeros  and  V6Hnsc <> *zeros
707800990217     c                   clear                   V6Hnsc
707900990217     c                   ENDIF
708000990217      *
708100990217      * Colli
708200990217     c                   IF        V6Sper = *zeros  and  V6Hper <> *zeros
708300990217     c                   clear                   V6Hper
708400990217     c                   ENDIF
708500990217      *
708600990217      * P.O.
708700990217     c                   IF        V6Spor = *zeros
708800990217     c                   clear                   V6Spor
708900990217     c                   ENDIF
709000990217     c                   IF        V6Spor = *blanks  and  V6Hpor <> *blanks
709100990217     c                   clear                   V6Hpor
709200990217     c                   clear                   V6Sdpo
709300990217     c                   clear                   V6Sdtc
709400990217     c                   clear                   V6Htcr
709500990217     c                   ENDIF
709600131105      * Terminal
709700131105     c                   IF        V6Stpr = *zeros
709800131105     c                   clear                   V6Stpr
709900131105     c                   ENDIF
710000131105     c                   IF        V6Stpr = *blanks  and  V6Htpr <> *blanks
710100131105     c                   clear                   V6Htpr
710200131105     c                   ENDIF
710300990217      *
710400990217      * Tipo Attribuzione
710500990217     c                   IF        V6Stre = *blanks  and  V6Htre <> *blanks
710600990217     c                   clear                   V6Htre
710700990217     c                   clear                   V6Sdtr
710800990217     c                   clear                   V6Hcal
710900990217     c                   ENDIF
711000990217      *
711100990217     c                   ENDSR
711200990217      *****************************************************************
711300990217      *  CONTROLLO P.O. PER RESPONSABILITA
711400990217      *****************************************************************
711500990217     C     CTR_PORESP    BEGSR
711600990217      *
711700990217      *    Controllo se immesso un numero valido
711800990217     c                   clear                   W0030
711900990217     C                   testn                   V6Spor               30
712000990217      *
712100990217     C                   If          *IN30 = *on
712200990217     C                   move      V6Spor        W001A
712300990217     C                   If           W001A >= '0'
712400990217     c                   movel     V6Spor        W0030
712500990217     c                   eval      *in30 = (W001A >= '0')
712600990217     C                   Endif
712700990217     C                   Endif
712800990217      *
712900990217     C     W0030         chain     AZORG                              31
713000990217      *
713100990217     c                   IF        *IN31 = *ON  or  ORGfva <> *blanks
713200990217     c                              or  (ORGfag <> 'F'  and  ORGfag <> 'A')
713300990217     C                   eval      *in94 = *on
713400990217     C                   eval      *in28 = *on
713500990217     C                   movel     MSG(58)       V6Cmsg
713600990217     c                   ELSE
713700990217      *
713800990217     c                   movel     V6Spor        V6Hpor
713900990217     c                   If          ORGde5 <> *blanks
714000990217     C                   movel(P)  ORGde5        V6Sdpo
714100990217     c                   Else
714200990217     C                   movel(P)  ORGdes        V6Sdpo
714300990217     c                   Endif
714400131106
714500131106     c* ricalcolo il terminal del p.o. responsabile
714600131106     c                   exsr      CAL_ter
714700990217      *
714800990217     C                   SELECT
714900090324     c                   WHEN      W0030 = DCTlna  or  W0030 = i50tfa
715000090324     c                             or w0030=i50lna
715100990217     c                   movel     'A'           V6Htcr
715200090324     c                   WHEN      W0030 = Wlnp    or  W0030 = i50tfp
715300090324     c                             or w0030=i50lnp or  w0030 = wtfplnp
715400990217     c                   movel     'P'           V6Htcr
715500990217     c                   OTHER
715600990217     c                   movel     'T'           V6Htcr
715700990217     c                   ENDSL
715800990217      *
715900990217     c                   clear                   V6Sdtc
716000990217     c                   Exsr      decodtcr
716100990217      *
716200990217     c                   ENDIF
716300990217      *
716400990217     c                   ENDSR
716500131105      *****************************************************************
716600131105      *  CONTROLLO Terminal del P.O. Responsabile
716700131105      *****************************************************************
716800131105     C     CTR_TPRESP    BEGSR
716900131105      *
717000131106     c* se è vuoto ed è presente il p.o responsabile
717100131106     c* calcolo il terminal in base al p.o. respoinsabile
717200131106     c                   if        v6stpr=*blanks or v6stpr=*zeros
717300131106     c                   if        v6spor>*zeros
717400131106     c                   exsr      cal_ter
717500131106     c                   endif
717600131106     c                   else
717700131105      *    Controllo se immesso un numero valido
717800131105     c                   clear                   W0030
717900131105     C                   testn                   V6Stpr               30
718000131105      *
718100131105     C                   If          *IN30 = *on
718200131105     C                   move      V6Stpr        W001A
718300131105     C                   If           W001A >= '0'
718400131105     c                   movel     V6Stpr        W0030
718500131105     c                   eval      *in30 = (W001A >= '0')
718600131105     C                   Endif
718700131105     C                   Endif
718800131105      *
718900131105     C     W0030         chain     AZORG                              31
719000131105      *
719100131105     c                   IF        *IN31 = *ON  or  ORGfva <> *blanks
719200131105     c                              or  (ORGfag <> 'F'  and  ORGfag <> 'A')
719300131105     C                   eval      *in96 = *on
719400131105     C                   eval      *in28 = *on
719500131105     C                   movel     MSG(58)       V6Cmsg
719600131105     c                   ELSE
719700131105      *
719800131105
719900131105     c* controllo che sia un terminal
720000131106
720100131106     c* provo con ter_part
720200131106     c                   clear                   wok_ter
720300131106     c                   eval      kepa='P'
720400131106     c     kcaeP         setll     azcae05l
720500131106     c     kcaeP         reade     azcae05l
720600131106     c                   dow       not%eof(azcae05l) and wok_ter=*blanks
720700131106     c                   if        caeatb=*blanks
720800131106     c                   eval      wok_ter='S'
720900131106     c                   endif
721000131106     c     kcaeP         reade     azcae05l
721100131106     c                   enddo
721200131106     c* se non trovato come ter-part cerco come ter_arr
721300131106     c                   if        wok_ter=*blanks
721400131106     c                   eval      kepa='A'
721500131106     c     kcaeA         setll     azcae05l
721600131106     c     kcaeA         reade     azcae05l
721700131106     c                   dow       not%eof(azcae05l) and wok_ter=*blanks
721800131106     c                   if        caeatb=*blanks
721900131106     c                   eval      wok_ter='S'
722000131106     c                   endif
722100131106     c     kcaeA         reade     azcae05l
722200131106     c                   enddo
722300131106     c                   endif
722400131106     c                   if        wok_ter=*blanks
722500131105     C                   eval      *in96 = *on
722600131105     C                   eval      *in28 = *on
722700131105     C                   movel     MSG(122)      V6Cmsg
722800131105     c                   endif
722900131105      *
723000131105      *
723100131105     c                   ENDIF
723200131106     c                   ENDIF
723300131107     c                   if        *in96=*off
723400131107     c                   movel     V6Stpr        V6Htpr
723500131107     c                   endif
723600131105      *
723700131105     c                   ENDSR
723800990218      *****************************************************************
723900990218      * CONTROLLI DI CONGRUITA' DEI DATI INSERITI
724000990218      *****************************************************************
724100990218     C     CTRD05_con    BEGSR
724200990218      *
724300990218     c                   SELECT
724400990218      *
724500990218     c                   WHEN        V6Hcal <> 'N'  and  V6Snsc = *zeros  and
724600990218     c                               V6Sper = *zeros  and  V6Spor = *blanks  and
724700131106     c                               V6Stre = *blanks  and v6stpr= *blanks
724800990218      *
724900990218     c                   WHEN        V6Hcal = 'N'  and  V6Snsc > *zeros
725000990218     C                   eval      *in92 = *on
725100990218     C                   eval      *in28 = *on
725200990218     C                   movel     MSG(95)       V6Cmsg
725300990218      *
725400990218     c                   WHEN        V6Hcal = 'N'  and  V6Sper = *zeros
725500990218     C                   eval      *in93 = *on
725600990218     C                   eval      *in28 = *on
725700990218     C                   movel     MSG(59)       V6Cmsg
725800120619     c
725900120802     c*****              WHEN        V6Sper > §stdpermax
726000120802     c                   WHEN        v6snsc>0 and V6Sper > §stdpermax
726100120619     C                   eval      *in93 = *on
726200120619     C                   eval      *in28 = *on
726300120619     C                   movel     MSG(120)      V6Cmsg
726400120619     c                   eval      %subst(v6cmsg:73:6)=%editc(§stdpermax:'4')
726500990218      *
726600990218     c                   WHEN        V6Hcal = 'N'  and  V6Spor <> *blanks
726700990218     C                   eval      *in94 = *on
726800990218     C                   eval      *in28 = *on
726900990218     C                   movel     MSG(96)       V6Cmsg
727000990218      *
727100990218     c                   WHEN        V6Hcal = 'N'  or
727200990218     c                               (V6Sper > *zeros  and  V6Spor <> *blanks
727300990218     c                                                 and  V6Stre <> *blanks)
727400990218     C                   movel     V6Snsc        W007a
727500990218     c                   eval      W014a =  W007a + V6Spor + V6Stre
727600990218     c     W014a         Lookup    RSU                                    28
727700990218     c                   If          *IN28 = *ON
727800990218     C                   eval      *in92 = *on
727900990218     C                   movel     MSG(94)       V6Cmsg
728000990218     c                   Else
728100990218     c                   z-add     1             JJ
728200990218     c     *blanks       Lookup    RSU(jj)                                30
728300990218     c                   movel     W014a         RSU(jj)
728400990218     c                   Endif
728500990218      *
728600990218     c                   WHEN        V6Sper = *zeros
728700990218     C                   eval      *in93 = *on
728800990218     C                   eval      *in28 = *on
728900990218     C                   movel     MSG(59)       V6Cmsg
729000120619      *
729100990218     c                   WHEN        V6Spor = *blanks
729200990218     C                   eval      *in94 = *on
729300990218     C                   eval      *in28 = *on
729400990218     C                   movel     MSG(59)       V6Cmsg
729500990218      *
729600990218     c                   WHEN        V6Stre = *blanks
729700990218     C                   eval      *in95 = *on
729800990218     C                   eval      *in28 = *on
729900990218     C                   movel     MSG(59)       V6Cmsg
730000990218      *
730100990218     c                   ENDSL
730200990218      *
730300990218     c                   ENDSR
730400990217      *****************************************************************
730500990217      *  SCRIVO FILE RESPONSABILITA'
730600990217      *****************************************************************
730700990217     C     WRTDCR        BEGSR
730800990217      *
730900990315      * Annullo rcd esistenti
731000990315     c     KCA           chain     fndcr000                           30
731100990315     c                   DOW        *IN30 = *Off
731200990317     c                   If        DCRatb = *blanks
731300990317     c                   eval      DCRatb = 'A'
731400990315     c                   clear                   dcrftr
731500051019     c                   eval      DCRdtr = DateU
731600990315     c                   update    fndcr000
731700990315     c                   Endif
731800990315     c     KCA           reade     fndcr000                               30
731900990210    1C  N30              ENDDO
732000990210      *
732100990210     c                   z-add     1             nrr5
732200990210      *
732300990210     C     nrr5          chain     FI03S05                            3232
732400990210      *
732500990210    1C     *IN32         DOWEQ     *OFF
732600990210      *
732700990210     c                   IF        V6Sper <> *zeros
732800990315      *
732900990326     c                   if        V6Snsc = *zeros
733000990326     c                   clear                   WV6Snsc
733100990326     c                   else
733200990315     c                   movel     V6Snsc        WV6Snsc
733300990326     c                   endif
733400990315     c                   movel     V6Spor        WV6Spor
733500990326      *
733600990315     c     KDCR          CHAIN     fndcr000                           30
733700990315      *
733800990315     c                   clear                   FNDCR000
733900051019     c*** già clearato:  clear                   DCRatb
734000990210     c                   z-add     DCTaac        DCRaac
734100990210     c                   z-add     DCTfil        DCRfil
734200990210     c                   z-add     DCTnca        DCRnca
734300990210     c                   movel     V6Spor        DCRpor
734400990210     c                   z-add     V6Sper        DCRper
734500990210     c                   movel     V6Htcr        DCRtcr
734600990219     c                   clear                   DDCR01
734700990219     c                   if         V6Snsc > *zeros
734800990219     c                   movel     V6Snsc        §DCRnsc
734900990219     c                   endif
735000990219     c                   movel     V6Stre        §DCRtre
735100090417     c
735200131106     C***  dcrpor        chain     AZORG                              31
735300131106     c***                movel     orgde3        og143
735400131106     c***31              clear                   og143
735500090324     c
735600090324     c* Calcolo il terminal responsabile e tipo
735700131106     c***R               select
735800090325     c* se p.o. estero memorizzo se stesso
735900131106     c***R               when      §ogntw='EEX' or §ogntw='DPD' or
736000131106     c***R                         §ogntw='FED'
736100131106     c***R               movel     dcrpor        §dcrter
736200131106     c***R               when      dcrtcr='A'
736300131106     c***R               movel     i50tfa        §dcrter
736400131106     c***R               when      dcrtcr='P' and (dcrpor=wlnp or
736500131106     c***R                                         dcrpor=wtfplnp)
736600131106     c***R               movel     wtfplnp       §dcrter
736700131106     c***R               when      dcrtcr='P' and (dcrpor=i50lnp or
736800131106     c***R                                         dcrpor=i50tfp )
736900131106     c***R               movel     i50tfp        §dcrter
737000131106     c***R
737100131106     c***R               other
737200131106     c***R               clear                   fnlv55ds
737300131106     c***R               movel     'A'           d55tpt
737400131106     c***R               movel     dcrpor        d55lin
737500131106     c***R               if        ds_arbdat>0
737600131106     c***R               z-add     ds_arbdat     d55drf
737700131106     c***R               else
737800131106     c***R               move      dctmgc        d55drf
737900131106     c***R               movel     dctaac        d55drf
738000131106     c***R               endif
738100131106     c***R               call      'FNLV55R'
738200131106     c***R               parm                    fnlv55ds
738300131106     c***R               movel     d55tfa        §dcrter
738400131106      ***R
738500131106     c***R               endsl
738600131106     c***R
738700131106     c                   movel     v6stpr        §dcrter
738800131106     c                   movel     DDCR01        DCRflo
738900051019     c*** già clearato:  clear                   DCRftr
739000051019     c*** già clearato:  clear                   DCRdtr
739100990315      *
739200990315     c                   if        *in30 = *ON
739300990210     c                   WRITE     FNDCR000
739400990315     c                   else
739500990315     c                   UPDATE    FNDCR000
739600990315     c                   endif
739700990315      *
739800990210     c                   ENDIF
739900990210      *
740000990210     c                   add       1             nrr5
740100990210     c     nrr5          chain     FI03S05                            32
740200990210    1C                   ENDDO
740300990210      *
740400990210     c                   ENDSR
740500050523      *---------------------------------------------------------------*
740600050523      * Invio e-mail x perizia per avaria                             *
740700050523      *---------------------------------------------------------------*
740800050523     c     sr_InviaMail  BEGSR
740900050601
741000081110      * Reperisco dati in tabella mra per OVRTPRTF in caso di Avaria
741100081110      *
741200081110    1c                   If        §TADragr = 'V'
741300050907     c                   CLEAR                   TIBS02ds
741400050601     c                   clear                   trtcm1ds
741500050907     c                   movel     'C'           t02mod
741600050601     c                   movel     KNSIF         T02sif
741700050601     c                   movel     'MRA'         T02cod
741800050601     c                   movel(p)  'FIDN03R'     T02ke1
741900050601     c                   call      'TIBS02R'
742000050601     c                   parm                    KPJBA
742100050601     c                   parm                    TIBS02ds
742200081110    2c                   if        T02err = *blanks
742300050601     c                   movel     T02uni        dmradan
742400050601     c                   eval      §cm1dst = §mraddest
742500050601     c                   move      dutpou        §cm1po
742600050601     c                   move      '0'           §cm1sts
742700050601     c                   movel     §mradreg      §cm1tips
742800050601     c                   movel     §mradidpro    §cm1idp
742900050601     c                   eval      §cm1mitt = §mradmitt
743000050601     c                   eval      §cm1var  =  '*OBJM*Richiesta PERIZIA +
743100050601     c                                         per avaria'
743200170607     c                                      +  ' n° C.A. ' +
743300170607     c                             %editc( DCTfil : 'X' ) + '/' +
743400170607     c                             %trim( %editc( DCTnca : '3' ) ) +
743500170607     c                             ' del ' + %editc( DCTaac : '3' )
743600081110    2c                   endif
743700081110    1c                   ENDIF
743800081110
743900081110      * Reperisco dati in tabella mra per OVRTPRTF in caso per Ammanco / Mancanza
744000081110      *
744100081110    1c                   If        §TADragr = 'A' or §TADragr = 'M'
744200081110     c                   CLEAR                   TIBS02ds
744300081110     c                   clear                   trtcm1ds
744400081110     c                   movel     'C'           t02mod
744500081110     c                   movel     KNSIF         T02sif
744600081110     c                   movel     'MRA'         T02cod
744700081110     c                   eval      T02ke1 ='FIDN03R-AMM/MAN'
744800081110     c                   call      'TIBS02R'
744900081110     c                   parm                    KPJBA
745000081110     c                   parm                    TIBS02ds
745100081110    2c                   if        T02err = *blanks
745200081110     c                   movel     T02uni        dmradan
745300081110     c                   eval      §cm1dst = §mraddest
745400081110     c                   move      dutpou        §cm1po
745500081110     c                   move      '0'           §cm1sts
745600081110     c                   movel     §mradreg      §cm1tips
745700081110     c                   movel     §mradidpro    §cm1idp
745800081110     c                   eval      §cm1mitt = §mradmitt
745900081110     c                   eval      §cm1var  =  '*OBJM*Ammanchi/mancanze +
746000081110     c                                         rilevanti'
746100170607     c                                      +  ' n° C.A. ' +
746200170607     c                             %editc( DCTfil : 'X' ) + '/' +
746300170607     c                             %trim( %editc( DCTnca : '3' ) ) +
746400170607     c                             ' del ' + %editc( DCTaac : '3' )
746500081110    2c                   endif
746600081110    1c                   ENDIF
746700050601      *
746800050601      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
746900050601      *   TRAMITE E-mail all'assicurazione
747000050601      *
747100050601
747200050601      * eseguo ovprtf
747300081110    1c                   If        T02err = *blanks
747400050601     c                   eval      %subst(cmm(1): 23: 10) = §MRADOUTQI
747500050601     c                   eval      Qcomm = %trim(cmm(1)) + trtcm1ds
747600050601     c                             + ''')'
747700050601     c                   z-add     395           Qleng
747800050601     c                   CALL      'QCMDEXC'                            90
747900050601     c                   PARM                    Qcomm
748000050601     c                   PARM                    Qleng
748100050601     c
748200050601      *
748300050523      *
748400050523      * Preparo la mail da inviare
748500050523     c                   exsr      sr_PrintMail
748600050523      * DltOvr
748700050601     c                   clear                   Comman
748800050523     c                   call      'QCMDEXC'                            99
748900050523     c                   parm      C_Cmd_1       Comman
749000050523     c                   parm                    Lenght
749100050523      *
749200050523e   1c                   ENDIF
749300050523      *
749400050523     c                   ENDSR
749500050523      *---------------------------------------------------------------*
749600050523      * Prepara e-mail x perizia per avaria                           *
749700050523      *---------------------------------------------------------------*
749800050523     c     sr_PrintMail  BEGSR
749900050523      *
750000050523     c                   open      FIDN03P
750100050523      *
750200050523     c                   clear                   DN03PT1
750300081111     c                   If        §tadragr <> 'V'
750400081111     c                   eval      *in52 = *on
750500081111     c                   endif
750600050523      *
750700050523     c                   eval      PT1fil = DCTfil
750800050523     c                   select
750900050523     c                   when      DCTfil = Wlnp
751000050523     c                   movel     Save_PO_P     DS_PO
751100050523     c                   eval      PT1pod = ORGdes
751200050523     c                   when      DCTfil = Wlna
751300050523     c                   movel     Save_PO_A     DS_PO
751400050523     c                   eval      PT1pod = ORGdes
751500050523     c                   endsl
751600050523     c                   eval      PT1nca = DCTnca
751700050907     c                   clear                   WLBdat
751800050907     c                   eval      G02inv = (DCTaac * 10000) + DCTmgc
751900050907     c                   eval      G02err = '3'
752000050907     c                   call      'XSRDA8'
752100050907     c                   parm                    WLBdat
752200050907     c                   eval      PT1dca = G02dat
752300050523     c                   movel     Save_PO_A     DS_PO
752400050523     c                   eval      PT1poa = ORGdes
752500050523     c                   eval      PT1poi = ORGind
752600050523     c                   eval      PT1poc = ORGcpf
752700050523     c                   eval      PT1pol = ORGloc
752800050523     c                   eval      PT1pop = ORGpro
752900050523     c                   eval      PT1pot = ORGtel
753000050523     c                   eval      PT1pof = ORGfax
753100081111     c                   eval      PT1ipv = DCTipv
753200050523      *
753300050523     c                   write     DN03PT1
753400050523      *
753500050523     c                   close     FIDN03P
753600050523      *
753700050523     c                   ENDSR
753800080118      *---------------------------------------------------------------*
753900080118      * Invio e-mail x avviso danno a FedEx                           *
754000080118      *---------------------------------------------------------------*
754100080118     c     sr_CallFIDN38 BEGSR
754200080118      *
754300080118     c                   movel(p)  FIDN00ds      KPJBU
754400080121     c                   call      'FIDN38R'
754500080118     c                   parm                    KPJBA
754600080118     c                   movel     KPJBU         FIDN00ds
754700080118      *
754800080118     c                   if        o00err  <> *blanks
754900080118     c                   eval      o00msg = %trim(msg(73))
755000080118     c                                    + ' ' + o00msg
755100080118     c                   eval      o00err = 'E'
755200080118e   1c                   endif
755300080118      *
755400080118     c                   ENDSR
755500130802
755600130802      *---------------------------------------------------------------*
755700130802      * Invio e-mail x avviso danno a Partner VanDuuren               *
755800130802      *---------------------------------------------------------------*
755900130802     c     sr_CallFIDN39 BEGSR
756000130802
756100130802     c                   movel(p)  FIDN00ds      KPJBU
756200130802     c                   call      'FIDN39R'
756300130802     c                   parm                    KPJBA
756400130802     c                   movel     KPJBU         FIDN00ds
756500130802
756600130802     c                   if        o00err  <> *blanks
756700130802     c                   eval      o00msg = %trim(msg(73))
756800130802     c                                    + ' ' + o00msg
756900130802     c                   eval      o00err = 'E'
757000130802     c                   endif
757100130802
757200130802     c                   ENDSR
757300141007      *
757400141007      *---------------------------------------------------------------*
757500141007      * Invio e-mail x avviso danno a Partner UK                      *
757600141007      *---------------------------------------------------------------*
757700141007     c     sr_CallFIDN41 BEGSR
757800141007      *
757900141007     c                   movel(p)  FIDN00ds      KPJBU
758000141007     c                   call      'FIDN41R'
758100141007     c                   parm                    KPJBA
758200141007     c                   movel     KPJBU         FIDN00ds
758300141007      *
758400141007     c                   if        o00err  <> *blanks
758500141007     c                   eval      o00msg = %trim(msg(73))
758600141007     c                                    + ' ' + o00msg
758700141007     c                   eval      o00err = 'E'
758800141007     c                   endif
758900141007      *
759000141007     c                   ENDSR
759100160518      *
759200160518      *---------------------------------------------------------------*
759300160518      * Invio e-mail x avviso danno a Partner (EuroExpress)           *
759400160518      * · 311     = GEL Express Logistik GmbH                         *
759500160518      * · 333     = France Express Lyon Corbas                        *
759600160518      * · 334     = Calexpress Gennevilliers                          *
759700160518      * · 340-345 = Azkar                                             *
759800160518      *---------------------------------------------------------------*
759900160518     c     sr_CallFIDN68 BEGSR
760000160518      *
760100160518     c                   movel(p)  FIDN00ds      KPJBU
760200160518     c                   call      'FIDN68R'
760300160518     c                   parm                    KPJBA
760400160518     c                   movel     KPJBU         FIDN00ds
760500160518      *
760600160518     c                   if        o00err  <> *blanks
760700160609     c*//                eval      o00msg = %trim(msg(73))
760800160609     c*//                                 + ' ' + o00msg
760900160609     c*//                eval      o00err = 'E'
761000160609      * -?Meglio NON segnalare errori (tipo "e-mail mancante") a chi?
761100160609      *  ?NON sarebbe in grado né di capire né di risolvere...?
761200160609     c                   clear                   o00err
761300160609     c                   clear                   o00msg
761400160518     c                   endif
761500160518      *
761600160518     c                   ENDSR
761700130802
761800080516      *---------------------------------------------------------------*
761900080516      * Invio lettera al cliente beneficiario della L.T. fase 100     *
762000080516      *---------------------------------------------------------------*
762100080516     c     sr_CallFIDN67 BEGSR
762200080516      *
762300080516     c                   clear                   o00err
762400080516     c                   clear                   o00msg
762500080516     c                   movel(p)  FIDN00ds      KPJBU
762600080516     c                   call      'FIDN67R'
762700080516     c                   parm                    KPJBA
762800080516     c                   movel     KPJBU         FIDN00ds
762900080516      *
763000080516     c                   if        o00err  <> *blanks
763100080516     c                   eval      o00msg = %trim(msg(73))
763200080516     c                                    + ' ' + o00msg
763300080516     c                   eval      o00err = 'E'
763400080516e   1c                   endif
763500080516      *
763600080516     c                   ENDSR
763700130404
763800130404      /free
763900130404       //--------------------------------------------------------------
764000130404       //?Chiusura automatica C.A.                                     ?
764100130404       //--------------------------------------------------------------
764200130404       BEGSR  chiudiCA;
764300130404
764400130404       //?Posso chiudere la C.A. solo se il Beneficiario è presente sul file
764500130404       //?dei clienti con chiusura automatica C.A.
764600130404         SELECT;
764700130404
764800130404       //?Codice beneficiario inserito dall'utente
764900130404           WHEN  DCTksc > 0;
765000130404             wKSC = DCTksc;
765100130404
765200130404       //?Codice beneficiario = Mittente
765300130404           WHEN  DCTptr = 'M';
765400130404             wKSC = VTCksm;
765500130404
765600130404       //?Codice beneficiario = Mittente
765700130404           WHEN  DCTptr = 'D';
765800130404             wKSC = VTCksd;
765900130404         ENDSL;
766000130404
766100130404       //?Cerco il beneificario sul file
766200130404         exec sql
766300130404         SELECT DKLcod into :DKLcod
766400130404         FROM FNDKL00F
766500130404         WHERE DKLksc = :wKSC;
766600130404       //?non trovo il rcd corrispondente vado a fine
766700130404         IF sqlcod <> 0;
766800130404           leavesr;
766900130404         ENDIF;
767000130404
767100130404       //?Recupero i dati da tabella DKL
767200130404         clear dDKL;
767300130404         clear TIBS02ds;
767400130404         T02mod = 'C';
767500130404         T02cod = 'DKL';
767600130404         T02sif = KNSIF;
767700130404         T02ke1 = %editc(DKLcod:'X');
767800130404         TNTBE_RicercaControllo  (kpjba : tibs02ds);
767900130405         IF  T02err <> *blanks;
768000130405           leavesr;
768100130404         ENDIF;
768200130405         dDKL = T02uni;
768300130404
768400130404       //?Ultimo numero prelevato dal Numeratore
768500130404         clear TRUL33DS;
768600130404         I33cnu = §DKLcnu;
768700130404         I33ope = 1;
768800130405         I33aaa = DCTaac;
768900130404         kpjbu = TRUL33DS;
769000130404         trul33r (KPJBA);
769100130404         TRUL33DS = kpjbu;
769200130404
769300130404       //?Non sono ancora arrivato al numero MAX di c.a. da chiudere
769400130405         IF  O33nrf < §DKLnca;
769500130404         //?Chiudo la C.A.
769600130404           clear FIDN22DS;
769700130404           I22tri = 'R';
769800130404           I22mod = 'C';
769900130404           I22aac = DCTaac;
770000130404           I22fil = DCTfil;
770100130404           I22nca = DCTnca;
770200130404           I22cch = §DKLcau;
770300130408           I22fgs = DCTgfc;
770400130404           kpjbu = FIDN22DS;
770500130404           fidn22r (kpjba);
770600130404           IF  O22err <> *blanks;
770700130404             leavesr;
770800130404           ENDIF;
770900130404         //?Avanzo il numeratore
771000130404           clear I33ope;
771100130405           I33aaa = DCTaac;
771200130404           I33num = 1;
771300130404           kpjbu = TRUL33DS;
771400130404           trul33r (KPJBA);
771500130408
771600130408         //?Scrivo nota
771700130408           clear wstd ;
771800130408           wnot = Comm_chiudi;
771900130409           exsr wrtdcs_3;
772000130404         ENDIF;
772100130404
772200130404       ENDSR;
772300130404
772400130404      /end-free
772500130404
772600990209      *****************************************************************
772700990311      *  CONTROLLO SE CI SONO IDD APERTI
772800990209      *****************************************************************
772900990311     C     CTR_IDD       BEGSR
773000990209      *
773100990311     c                   clear                   WIDDaperto
773200990311      *
773300990518     c     kanm          setll     fnanm02L
773400990518     c     kanm          reade     fnanm02L                               30
773500990301      *
773600990311     C                   DOW       *in30 = *off
773700990311     C     anmcaa        lookup    S7C                                    31
773800990312     C                   If        *in31 = *on  and  anmdch = *zeros
773900990311     c                   eval        *in30 = *on
774000990311     c                   eval        WIDDaperto = 'S'
774100990311     c                   Else
774200990518     c     kanm          reade     fnanm02L                               30
774300990311     c                   Endif
774400990311     c  N30              ENDDO
774500990209      *
774600990209     c                   ENDSR
774700001102      **********************************************************************
774800001102      *  AGGANCIO TABELLA 3K
774900001102      **********************************************************************
775000001102     C     AGGT3K        BEGSR
775100001102
775200001102     c                   clear                   DS3K
775300001102
775400001102      * Elaboro solo mittenti codificati
775500001102     c                   z-add     VTCksm        w0040
775600001102     C                   IF        VTCksm > *zeros  and  W0040 <> 8888
775700001102     C                                              and  W0040 <> 9999
775800001102
775900001102     C                   eval      KCOD = '3K'
776000001102     C                   movel(P)  VTCksm        KKEY
776100001102     C     KTAB          CHAIN     TABEL
776200001102     C                   IF        %found  and  TBLflg = *blanks
776300001102     C                   movel     TBLuni        DS3K
776400001102     C                   ENDIF
776500001102     C                   ENDIF
776600001102      *
776700001102     C                   ENDSR
776800990311      *****************************************************************
776900990311      *  DECODIFICO TABELLA CALCOLO RESPONSABILITA'
777000990311      *****************************************************************
777100990311     C     DECODtcr      BEGSR
777200990311      *
777300990311     c                   clear                   TIBS02DS
777400990311      *
777500990311     C                   MOVEL     'C'           t02mod
777600990311     C                   MOVEL     knsif         t02sif
777700990311     C                   MOVEL     'TCR'         t02cod
777800990311     C                   MOVEL(P)  V6Htcr        t02ke1
777900990311      *
778000990311     C                   CALL      'TIBS02R'
778100990311     C                   PARM                    KPJBA
778200990311     C                   PARM                    TIBS02DS
778300990311      *
778400990311     C                   IF        t02err = *BLANKS
778500990311     C                   movel(P)  T02uni        V6Sdtc
778600990311     C                   ENDIF
778700990311      *
778800990311     c                   ENDSR
778900990217      *****************************************************************
779000990217      *  DECODIFICO TABELLA TIPO ATTRIBUZIONE RESPONSABILITA'
779100990217      *****************************************************************
779200990217     C     DECODtre      BEGSR
779300990219      *
779400990219     C     '?'           SCAN      V6Stre                                 31
779500990219      *  Ricerca
779600990219    1C                   IF        *IN31 = *ON
779700990219     c                   eval       *in95 = *on
779800990219     c                   clear                   TIBS02DS
779900990219     c                   movel     'R'           t02mod
780000990219     c                   movel     knsif         t02sif
780100990219     C                   movel     'TRE'         t02cod
780200990219     C                   CALL      'TIBS02R'
780300990219     c                   parm                    KPJBA
780400990219     c                   parm                    TIBS02DS
780500990219      *
780600990219     c                   If          T02err = *blanks
780700990219     C                   movel     T02ke1        V6Stre
780800990219     c                   Endif
780900990219     c                   ENDIF
781000990217      *
781100990217     C                   clear                   Dtre
781200990219     c                   clear                   TIBS02DS
781300990217      *
781400990217     C                   MOVEL     'C'           t02mod
781500990217     C                   MOVEL     knsif         t02sif
781600990217     C                   MOVEL     'TRE'         t02cod
781700990218     C                   MOVEL(P)  V6Stre        t02ke1
781800990217      *
781900990217     C                   CALL      'TIBS02R'
782000990217     C                   PARM                    KPJBA
782100990217     C                   PARM                    TIBS02DS
782200990217      *
782300990217     C                   IF        t02err = *BLANKS
782400990217     C                   movel(P)  T02uni        DTRE
782500990217     C                   ENDIF
782600990217      *
782700990217     c                   ENDSR
782800981020      *****************************************************************
782900990311      *  AGGANCIO ANAGRAFICHE
783000981020      *****************************************************************
783100990311     C     CTR_ANAGRA    BEGSR
783200981022      *
783300981020     C                   CALL      'TIBS69R'
783400981020     C                   PARM                    tibs69DS
783500981020     C                   PARM                    DS_cnaco
783600981020     C                   PARM                    DS_cnind
783700981020     C                   PARM                    DS_cnclp
783800981020     C                   PARM                    DS_fncls
783900981022      *
784000990226     C                   eval      *IN30 = (O69ERR =  '1')
784100990311      *
784200990311     C                   eval      wtibs69r = 'S'
784300990408      * per il 1° giro è inizializzata nelle specifiche "D"
784400990311     C                   clear                   TIBS69DS
784500981020      *
784600981020     C                   ENDSR
784700981005      *****************************************************************
784800981127      *  DETERMINO LIMITE RISARCIBILE DA UTILIZZARE
784900981005      *****************************************************************
785000981127     C     IMPlimris     BEGSR
785100990311      *
785200020419      * se la spedizione è EEX o EUP oppure DPD export
785300021108      *             oppure FedEx export
785400021108     c                   IF            §DCTeuro = 'X'
785500021108     c                             or  §DCTeuro = 'P'
785600021108     c                             or (§DCTeuro = 'D' and
785700021108     c                                 §DCTtisp = 'E')
785800021108     c                             or (§DCTeuro = 'F' and
785900021108     c                                 §DCTtisp = 'E')
786000010412      * importo in moneta corrente controvalore CMR
786100010412     c                   z-add     §GEccm        wlimris
786200981127      *
786300990222      *    LEGGE 450
786400981127     c                   ELSE
786500060309      * verifico che valore di limite risarcibile devo applicare
786600060309      *------------------------------------------------------------------
786700060309      * RICERCA LIMITE RISARCIBILE CON FLAG IN TESTATA CA
786800060309      *
786900060309      * - se il flag in testata CA non è valorizzato
787000060309      * - o se sono in modalità inserimento
787100060309      *------------------------------------------------------------------
787200060309     c                   If        I00MOD = 'A' or §dcttlrp = ' '
787300060309     c                   exsr      Rec_lrp
787400060309     c                   endif
787500060309
787600060313     c                   If        §dcttlrp = ' ' or §dcttlrp = '6'
787700060309     c                   z-add     6,2           §gelrp
787800060309     c                   else
787900060309     c                   z-add     1             §gelrp
788000060309     c                   endif
788100060309
788200060313      * importo in moneta corrente limite risarcibile al KG
788300010412     c                   z-add     §GElrp        wlimris
788400981127     c                   ENDIF
788500981127      *
788600981127     C                   ENDSR
788700060309      *
788800060309      *****************************************************************
788900060309      *  RECUPERO IMPORTO LIMITE RISARCIBILE
789000060309      *****************************************************************
789100060309     C     REC_LRP       BEGSR
789200060309
789300060309      * Imposto il flag in base a DSP = Data spedizione
789400060309      *- flag = " " DSP minore di 01/03/06   prende sempre la vecchia legge 450 6,2 eur
789500060309      *- flag = "1" DSP maggiore di 30/04/06 prende sempre la nuova legge 286 1 eur
789600060309      *- flag = "1" DSP tra 01/03/06 e 30/04/06 e flag in tariffa rinuncia AC BASE = 'S'
789700060309      *                                      prende la nuova legge 286 1 eur
789800060309      *- flag = "6" DSP tra 01/03/06 e 30/04/06 e flag in tariffa rinuncia AC BASE = ' '
789900060313      *             e non ha tariffa AC BASE prende  6,2 eur senza nessuna legge
790000060313      *- flag = "1" DSP tra 01/03/06 e 30/04/06 e flag in tariffa rinuncia AC BASE = ' '
790100060313      *             ed  ha tariffa AC BASE   prende  la nuova legge 286 1 eur
790200060309
790300060309     c                   SELECT
790400060309
790500060309      *- flag = " " DSP minore di 01/03/06   prende sempre la vecchia legge 450 6,2 eur
790600060309
790700060309     c                   When      DS_ARBDAT < 20060301
790800060313     c                   eval      §dcttlrp = ' '
790900060309
791000060407      *- flag = "1" DSP maggiore di 14/05/06 prende sempre la nuova legge 286 1 eur
791100060309
791200060407     c                   When      DS_ARBDAT > 20060514
791300060313     c                   eval      §dcttlrp = '1'
791400060309
791500060309     c                   OTHER
791600060309
791700060309      * recupero la tariffa
791800060309      *
791900160818     c                   clear                   FNLV59ds
792000160818     c                   z-add     dutkci        iLV59kci
792100160818     c                   z-add     WISksc        iLV59ksc
792200160818     c                   movel     DS_arbdat     iLV59dsp                       Dta Sped x Rif. Tar.
792300160818     C                   IF        §DCTtisp = *blank
792400160818     C                   movel     'J'           iLV59fie
792500160818     c                   ELSE
792600160818     C                   movel     'S'           iLV59fie
792700160818     C                   ENDIF
792800160818     c                   eval      iLV59ta2 = 'B'
792900160818     c                   eval      iLV59blo = 'S'
793000160818     c                   movel     ARBtsp        iLV59tsp
793100160818     c                   movel     'S'           iLV59con
793200160818     c                   move      WISctr        iLV59ctr
793300060309      *
793400160818     c                   call      'FNLV59R'
793500160818     c                   parm                    FNLV59DS
793600060309
793700060309      * Se trovato progressivo aggancio le tariffe
793800160818    2c                   IF        oLV59ERR = *BLANKS  and  oLV59TKS = 'S'
793900160818     c                   move      oLV59ctr      ktamctr
794000060309     c     Ktam          chain     Tntam01l
794100060309     c                   If        %found(tntam01l) and tamatb = ' '
794200060309     c                   eval      wtamfmp  = tamfmp
794300060309     c                   else
794400060309      * se non trovata tariffa al cliente imposto S = Rifiuto AC BASE
794500060309     c                   eval      wtamfmp = 'S'
794600060309     c                   endif
794700060309     c                   else
794800060309      * se non trovata tariffa al cliente imposto S = Rifiuto AC BASE
794900060309     c                   eval      wtamfmp = 'S'
795000060309     c                   endif
795100060309
795200060313     c                   If        wtamfmp = 'S' or (wmand = 'S' and Wftc = 'd')
795300060313     c                   eval      §dcttlrp = '1'
795400060313     c                   else
795500060313     c                   eval      §dcttlrp = '6'
795600060309     c                   endif
795700060309
795800060309     c                   ENDSL
795900060309
796000060309     c                   endsr
796100060309      *
796200990216      *****************************************************************
796300990216      *  VERIFICO SE CI SONO NOTE NUOVE
796400990216      *****************************************************************
796500990216     C     CTR_NOTE      BEGSR
796600990216      *
796700990216     C                   clear                   FIDN42DS
796800990216     c                   movel     'N'           I42trc
796900990216     c                   movel     'C'           I42tpd
797000990216     c                   movel     ds_numca      I42nkt
797100990216     c                   movel(P)  fidn42ds      kpjbu
797200990216      *
797300990216     C                   CALL      'FIDN42R'
797400990216     C                   PARM                    kpjba
797500990216      *
797600990216     c                   movel     kpjbu         fidn42ds
797700020124     c                   eval      *in24 =  (o42err = *blanks)
797800990216      *
797900990216     C                   ENDSR
798000990304      *****************************************************************
798100990304      *  VERIFICO SE ESISTE EVENTO RISERVA
798200990304      *****************************************************************
798300990304     C     EVERISERVA    BEGSR
798400990304      *
798500010528     C                   open      FNEVB04L
798600101007
798700101007      * controllo in base agli eventi caricati in schiera
798800990304      *
798900101007     c                   eval      xx = 1
799000101007     c                   FOR       xx by 1 to %elem(s2A)
799100101007     c                   IF        s2A(xx) <> *blanks
799200990304     C                   move      i00aas        Kaas2
799300101007     c                   eval      EVBcev = s2A(xx)
799400990811     c     Kevb          chain     fnevb000                           31
799500990304      *
799600010528     c     *in31         ifeq      *off
799700990304     C                   movel     'S'           Wriserva
799800101007     c                   leave
799900010528     c                   endif
800000101007     c                   ENDIF
800100101007     c                   ENDFOR
800200990304      *
800300010528     C                   close     FNEVB04L
800400990304      *
800500990304     C                   ENDSR
800600990226      *****************************************************************
800700990226      *  VERIFICO SE CLIENTE PARTICOLARE
800800990226      *****************************************************************
800900990226     C     CLIPAR        BEGSR
801000990226      *
801100990226     c                   clear                   TIBS02DS
801200990226      *
801300990226     C                   MOVEL     'C'           t02mod
801400990226     C                   MOVEL     knsif         t02sif
801500990226     C                   MOVEL     'DPC'         t02cod
801600990226     C                   MOVEL(P)  W0070         t02ke1
801700990226      *
801800990226     C                   CALL      'TIBS02R'
801900990226     C                   PARM                    KPJBA
802000990226     C                   PARM                    TIBS02DS
802100990226      *
802200990226     C                   if        t02err = *BLANKS
802300990226     C                   eval      Wclipar = 'S'
802400990226     c                   endif
802500990226      *
802600990226     C                   ENDSR
802700010904
802800010904      *****************************************************************
802900010910      * VERIFICA NUMERO DECIMALI IMPOSTATI
803000010904      *****************************************************************
803100010910     C     srCTRDEC      BEGSR
803200010904      *
803300011017     C                   reset                   XDECDS
803400010904      *
803500011017     C                   z-add     W0133         ICDnum
803600011017     C                   z-add     §cvDEC        ICDnrd
803700010904      *
803800011017     C                   call      'XDEC'
803900011017     C                   parm                    XDECDS
804000010904      *
804100011017     C                   IF        OCDesi  <> *off
804200010910     C                   eval      *IN28 = *ON
804300010904     C                   ENDIF
804400010904      *
804500010904     C                   ENDSR
804600010904
804700010904      *****************************************************************
804800010904      * CONVERSIONE IMPORTI IN MONETA CORRENTE
804900010904      *****************************************************************
805000010904     C     srCMB         BEGSR
805100010904      *
805200010904     C                   reset                   YEURCODS
805300010904      *
805400010904     C                   movel     WVLT          yecDVI
805500010904     C                   z-add     W0133         yecIMI
805600010910     C                   movel     §gedDBA       yecDVO
805700010904      *
805800010904     C                   call      'YEURCO'
805900010904     C                   parm                    YEURCODS
806000010904      *
806100010904     C                   IF        yecESI  = ' '
806200010904     C                   z-add     yecIMO        W0133
806300010912     C                   else
806400010912     C                   movel     *on           *in28
806500010904     C                   ENDIF
806600010904      *
806700010904     C                   ENDSR
806800020129
806900020129      *****************************************************************
807000020129      * VISUALIZZAZIONE EVENTO
807100020129      *****************************************************************
807200020129     C     CallFIDN06    BEGSR
807300020129      *
807400020129     C                   clear                   FIDN06DS
807500020129     C                   movel     'I'           I06mod
807600020129     C                   z-add     WV1Cnev       I06nev
807700020129     C                   z-add     V1Caae        I06aae
807800020129     C                   movel(P)  FIDN06DS      KPJBU
807900020129      *
808000020129     C                   call      'FIDN06R'
808100020129     C                   parm                    KPJBA
808200020129      *
808300020129     C                   ENDSR
808400131106     c*
808500131106      *****************************************************************
808600131106      * Calcola terminal responsabile in base al p.o.
808700131106      *****************************************************************
808800131106     c     CAL_ter       begsr
808900131106     c                   move      v6spor        w0030p
809000131106     C     w0030p        chain     AZORG                              31
809100131106     c                   movel     orgde3        og143
809200131106     c   31              clear                   og143
809300131106     c
809400131106     c* Calcolo il terminal responsabile e tipo
809500131106     c                   select
809600131106     c* se p.o. estero memorizzo se stesso
809700131106     c                   when      §ogntw='EEX' or §ogntw='DPD' or
809800131106     c                             §ogntw='FED'
809900131106     c                   movel     v6spor        v6stpr
810000131106     c                   when      v6htcr='A'
810100131106     c                   movel     i50tfa        v6stpr
810200131106     c                   when      v6htcr='P' and (w0030p =wlnp or
810300131106     c                                             w0030p =wtfplnp)
810400131106     c                   movel     wtfplnp       v6stpr
810500131106     c                   when      v6htcr='P' and (w0030p =i50lnp or
810600131106     c                                             w0030p =i50tfp )
810700131106     c                   movel     i50tfp        v6stpr
810800131106     c
810900131106     c                   other
811000131106     c                   clear                   fnlv55ds
811100131106     c                   movel     'A'           d55tpt
811200131106     c                   movel     w0030p        d55lin
811300131106     c                   if        ds_arbdat>0
811400131106     c                   z-add     ds_arbdat     d55drf
811500131106     c                   else
811600131106     c                   move      dctmgc        d55drf
811700131106     c                   movel     dctaac        d55drf
811800131106     c                   endif
811900131106     c                   call      'FNLV55R'
812000131106     c                   parm                    fnlv55ds
812100131106     c                   movel     d55tfa        v6stpr
812200131106
812300131106     c                   endsl
812400131106     c                   eval      v6htpr=v6stpr
812500131106     c                   endsr
812600980722      *****************************************************************
812700980623      *  ROUTINE INIZIALE
812800980722      *****************************************************************
812900951009     C     *INZSR        BEGSR
813000980722
813100951009     C     *ENTRY        PLIST
813200951009     C                   PARM                    KPJBA
813300980722
813400981020     C                   MOVEL     KPJBU         FIDN00DS
813500980625
813600980625      *  Verifico il tipo di funzione da eseguire
813700980625     C                   SELECT
813800980625      *
813900980625     C                   WHEN      I00MOD = 'A'                                 APERTURA C.A.
814000980625     c                   eval      *in01 = *on
814100980625     C                   movel(P)  tit1          VTCMOD
814200980625      *
814300980625     C                   WHEN      I00MOD = 'G'                                 CONFERMA C.A.
814400980625     C                   movel(P)  tit2          VTCMOD
814500980625     c                   eval      *in02 = *on
814600981211      *
814700981211     C                   WHEN      I00MOD = 'M'                                 MANUTENZIONE C.A.
814800981211     C                   movel(P)  tit4          VTCMOD
814900981211     c                   eval      *in03 = *on
815000981028      *
815100981028     C                   WHEN      I00MOD = 'C'                                 MANUTENZIONE C.A.
815200981028     c                   eval      *in16 = *on                                  RICHIAMATA DA PGM
815300990406     c                   eval      *in03 = *on                                  "COLLI RITROVATI"
815400981211     C                   movel(P)  tit4          VTCMOD
815500990406     c                   eval      I00mod = 'M'
815600990406      *
815700990406     C                   WHEN      I00MOD = 'E'                                 MANUTENZIONE C.A.
815800990406     c                   eval      *in04 = *on                                  SOLO RESPONSABILITA'
815900990406     c                   eval      *in03 = *on                                  "COLLI RITROVATI"
816000990406     C                   movel(P)  tit4          VTCMOD
816100990406     c                   eval      I00mod = 'M'
816200980625      *
816300980625     c                   OTHER
816400980625     c                   movel     msg(42)       o00msg
816500980625     c                   eval      o00err = 'E'
816600980625     c                   goto      fine
816700980625      *
816800980625     C                   ENDSL
816900980625
817000981126      * Dati società
817100050523     C                   Z-ADD     1             CODUT             1 0
817200050523     c                   exsr      DatiJob
817300130806      * Imposto le abilitazioni dell'utente
817400130806     c                   eval      dute01 = UTEfaf
817500980619      ***
817600980619      * CARICO TABELLA £6
817700980619      ***
817800980508     C                   CLEAR                   L6
817900981020     C                   CLEAR                   TRUL06DS
818000980508     C                   MOVE      '£6'          D06COD
818100980513     C                   MOVEL     i00fgs        D06key
818200981020     C                   MOVEL     TRUL06DS      KPJBU
818300980508     C                   CALL      'TRUL06R'
818400980508     C                   PARM                    KPJBA
818500981020     C                   MOVEL     KPJBU         TRUL06DS
818600980508     C                   MOVEA     LIN           L6
818700170428     c* Verifico le filiali gestione per avere i flag di validtà
818800170428     C                   clear                   FNLV50DS
818900170428     C                   MOVEL     i00fgs        D50FGS
819000170428     c                   eval      d50tcn='E'
819100170428     C                   CALL      'FNLV50R'
819200170428     C                   PARM                    FNLV50DS
819300981127      ***
819400010412      * leggo tabella variabili tariffe per limite risarcibile  nella moneta corrente
819500981127      ***
819600010412      *
819700010412      * recupero la moneta corrente
819800010412     C                   CLEAR                   tibs02ds
819900010412      *
820000010412     C                   MOVEL     'L'           T02TLA
820100010412     C                   MOVEL     'C'           T02MOD
820200010412     C                   MOVEL     KNSIF         T02SIF
820300010412     C                   MOVEL     'GED'         T02COD
820400010904     C                   MOVEL     'DANNI'       T02KE1
820500010412      *
820600010412     C                   CALL      'TIBS02R'
820700010412     C                   PARM                    KPJBA
820800010412     C                   PARM                    tibs02ds
820900010412      *
821000010910     c                   movel     T02UNI        DGEDDN
821100010412      * recupero i valori nella moneta corrente
821200010412     C                   CLEAR                   tibs02ds
821300010412      *
821400010412     C                   MOVEL     'L'           T02TLA
821500010412     C                   MOVEL     'C'           T02MOD
821600010412     C                   MOVEL     KNSIF         T02SIF
821700010412     C                   MOVEL     'GEI'         T02COD
821800010910     C                   MOVEL     §GEDDBA       T02KE1
821900010412      *
822000010412     C                   CALL      'TIBS02R'
822100010412     C                   PARM                    KPJBA
822200010412     C                   PARM                    tibs02ds
822300010412      *
822400010412     c                   movel     t02uni        DGEI
822500010904      ***
822600010904      * leggo tabella standards danni
822700010904      ***
822800010904     c                   clear                   TIBS02DS
822900010904     C                   MOVEL     'C'           t02mod
823000010904     C                   MOVEL     knsif         t02sif
823100010904     C                   MOVEL     'STD'         t02cod
823200010904     C                   MOVEL(P)  1             t02ke1
823300010910     C                   MOVEL(P)  §gedDBA       t02ke2
823400010904      *
823500010904     C                   CALL      'TIBS02R'
823600010904     C                   PARM                    KPJBA
823700010904     C                   PARM                    TIBS02DS
823800010904      *
823900010904     C                   if        t02err = *BLANKS
824000010904     C                   MOVEL     T02UNI        DSTD
824100010904     C                   endif
824200990311      *
824300990312      *        Causali anomalia IDD
824400990311     C                   clear                   XX
824500990311     C                   movel     '7C'          KCOD
824600990312     C     ktabe         SETLL     TABEL00F
824700990311     C                   DO        *HIVAL
824800990312     C     ktabe         READE     TABEL00F                               31
824900990311     C   31              leave
825000990311     c                   movel     tbluni        DS7C
825100990311     c                   IF        §7Cdan = 'S'
825200990311     C                   add       1             XX
825300990311     C                   movel     TBLKEY        S7C(xx)
825400990311     c                   ENDIF
825500990311     C                   ENDDO
825600101007
825700101007      * carico EVENTI che hanno RISERVA
825800101007     c                   clear                   xx
825900101007     c                   eval      kcod = '2A'
826000101007     c     ktabe         SETLL     tabel00f
826100101007     c                   do        *hival
826200101007     c     ktabe         READE     tabel00f
826300101007     c                   IF        %eof(tabel00f)
826400101007     c                   leave
826500101007     c                   ENDIF
826600101007     c                   eval      ds2A = TBLuni
826700101007     c                   IF        §2Atip = 'R'
826800101007     c                   eval      xx = xx + 1
826900101007     c                   eval      s2A(xx) = TBLkey
827000101007     c                   ENDIF
827100101007     c                   ENDDO
827200981127
827300981127      * TABELLE UTILIZZATE IN APERTURA
827400980623     c                   IF        I00MOD = 'A'
827500981127      *
827600981009      *        Date pulizia in filiale
827700980623     C                   clear                   ds5a
827800980623     C                   MOVEL     '5A'          kCOD
827900980623     C                   MOVEL(P)  1             kkEY
828000990408     C     ktab          CHAIN(N)  TABEL                              31
828100980623     C                   MOVEL     TBLUNI        ds5a
828200981009      *
828300981009      *        Causali consegna anomala solo Dirottamento
828400981009     C                   clear                   XX
828500990217     C                   clear                   JJ
828600981009     C                   MOVE      '7O'          KCOD
828700990312     C     ktabe         SETLL     TABEL00F
828800981009     C                   DO        *HIVAL
828900990312     C     ktabe         READE     TABEL00F                               31
829000981029     C   31              leave
829100981009     c                   movel     tbluni        DS7O
829200981009     c                   IF        §7Ofdi =  'D'
829300981009     C                   ADD       1             XX
829400990217     C                   MOVEL     TBLKEY        D7O(xx)
829500981009     c                   ENDIF
829600990217     c                   IF        §7Ores =  'S'
829700990217     C                   ADD       1             jj
829800990217     C                   MOVEL     TBLKEY        R7O(jj)
829900990217     c                   ENDIF
830000990217
830100981009     C                   ENDDO
830200981009      *
830300980623     C                   ENDIF
830400990216
830500990216      *  Verifico se ci sono note nuove, in caso positivo evidenzio F18
830600990216     C                   exsr      CTR_NOTE
830700981009
830800980611      * ORA
830900980611     C                   TIME                    W0140
831000000420      * ORA
831100000420     C                   MOVEL     W0140         WORA4
831200980619      * UDATE IN GGMMAAAA
831300980611     C                   MOVE      W0140         WDTGIO
831400980619      * UDATE IN AAAAMMGG
831500971016     C                   Z-ADD     WDTGIO        G02DAT
831600971016     C                   MOVEL     *BLANK        G02ERR
831700971016     C                   CALL      'XSRDA8'
831800971016     C                   PARM                    WLBDAT
831900980611     C                   MOVEL     G02INV        DATEU
832000980623     C     *iso          MOVEL     DATEU         Wdataoggi
832100050217
832200050217      * Apertura del file TNTMD
832300050217     c                   Clear                   comman
832400050217     c                   Eval      comman = cmd(1)
832500050217     c                   Eval      %Subst(comman:13:8) = 'TNTMD01L'
832600050217      * test sistema informativo di prova
832700050217     c                   If        %subst(knsif:7:1) = 'P'
832800050217     c                   Eval      %Subst(comman:30:10) = wlibp
832900050217     c                   Else
833000050217     c                   Eval      %Subst(comman:30:10) = wlib
833100050217     c                   EndIf
833200050217
833300050217     c                   Eval      comman =
833400050217     c                             %trim(comman) + '/TNTMD01L)'
833500050217     c                   Call      'QCMDEXC'                            99
833600050217     c                   Parm                    comman
833700050217     c                   Parm                    lenght
833800050217     c   99              Goto      fine
833900050217     c  n99              Open      Tntmd01l
834000980625
834100981126      * APRO FILE SPECIFICI PER SEDE/FILIALE/MODALITA' PGM
834200981005     C                   IF        SIMFEL = *zeros
834300990806     C                   open      TITAS30C
834400981229     C                   open      TNCSB03L
834500981005     c                   ELSE
834600981126      *
834700990311     c                   If        I00mod = 'A'
834800981006     C                   open      FLNUF01L
834900981224     C                   open      FNLBL01L
835000990311     c                   Endif
835100990311     c                   If        I00mod = 'G'
835200981126     C                   open      FNDFA01L
835300990311     c                   Endif
835400990105     C                   open      FNLBL02L
835500981006     C                   open      FNARB01L
835600981006     C                   open      FNBLP01L
835700981006     C                   open      FNART01L
835800981006     C                   open      FNBLT01L
835900060217     C                   open      FiAR401L
836000991104     C                   open      FIAR601L
836100051114     C                   open      FiAR901L
836200050331     C                   open      Tigcp21l
836300990518     C                   open      fnanm02L
836400981006     c                   ENDIF
836500980625
836600990312     C     KTABE         KLIST
836700990312     C                   KFLD                    CODUT
836800990312     C                   KFLD                    KCOD
836900990312      *
837000971020     C     KTAB          KLIST
837100971020     C                   KFLD                    CODUT
837200971020     C                   KFLD                    KCOD
837300971020     C                   KFLD                    KKEY
837400980625      *
837500980508     c     kbol01        klist
837600980515     c                   kfld                    i00aas
837700980515     c                   kfld                    i00lnp
837800980515     c                   kfld                    i00nrs
837900980515     c                   kfld                    i00nsp
838000050310      *
838100050310     c     kbol02        klist
838200050310     c                   kfld                    dctaas
838300050310     c                   kfld                    dctlnp
838400050310     c                   kfld                    dctnrs
838500050310     c                   kfld                    dctnsp
838600050915      *
838700050915     c     kbol50        klist
838800050915     c                   kfld                    i50aas
838900050915     c                   kfld                    i50lnp
839000050915     c                   kfld                    i50nrs
839100050915     c                   kfld                    i50nsp
839200991116      *
839300991116     c     kar6          klist
839400991116     c                   kfld                    i00aas
839500991116     c                   kfld                    i00lnp
839600991116     c                   kfld                    i00nrs
839700991116     c                   kfld                    i00nsp
839800991116     c                   kfld                    trcar6
839900991116      *
840000991116     c     kar6bl        klist
840100991116     c                   kfld                    LBLaan
840200991116     c                   kfld                    LBLlpn
840300991116     c                   kfld                    LBLnrn
840400991116     c                   kfld                    LBLnsn
840500991116     c                   kfld                    trcar6
840600981023      *
840700981104     c     kar4          klist
840800981023     c                   kfld                    i00aas
840900981023     c                   kfld                    i00lnp
841000981023     c                   kfld                    i00nrs
841100981023     c                   kfld                    i00nsp
841200981023     c                   kfld                    WTRC
841300980625      *
841400971020     C     KBOL          KLIST
841500980526     C                   KFLD                    arbaas
841600971020     C                   KFLD                    VTCLNP
841700971020     C                   KFLD                    VTCNRS
841800971020     C                   KFLD                    VTCNSP
841900980625      *
842000980625     c     kgiac         klist
842100980625     c                   kfld                    i00aas
842200980625     c                   kfld                    i00lnp
842300980625     c                   kfld                    i00nrs
842400980625     c                   kfld                    i00nsp
842500980625     c                   kfld                    Wfrg
842600980625      *
842700971021     C     KDET          KLIST
842800971021     C                   KFLD                    V1CAAE
842900981015     C                   KFLD                    Wv1cnev
843000050805     c     KDETold       klist
843100050805     c                   kfld                    DCTaae
843200050805     c                   kfld                    DCTnev
843300980625      *
843400971021     C     KDED          KLIST
843500971021     C                   KFLD                    V1CAAE
843600981015     C                   KFLD                    Wv1cnev
843700981015     C                   KFLD                    kfgs
843800980625      *
843900980625     C     KEVB          KLIST
844000980625     C                   KFLD                    KAAS2
844100980625     c                   kfld                    i00lnp
844200980625     c                   kfld                    i00nrs
844300980625     c                   kfld                    i00nsp
844400010528     c                   kfld                    evbcev
844500980625      *
844600971021     C     KDCS          KLIST
844700971021     C                   KFLD                    KTPD
844800971021     C                   KFLD                    KNKT
844900980609     C                   KFLD                    KSTD
845000980609     C                   KFLD                    KDIM
845100980609     C                   KFLD                    KHIM
845200971021     C                   KFLD                    KNKS
845300971021     C                   KFLD                    KTRC
845400980625      *
845500971022     C     KNUF          KLIST
845600980611     C                   KFLD                    AAS
845700980611     C                   KFLD                    CNU
845800980429     C                   KFLD                    COMLIN
845900980625      *
846000990209     c     kca           klist
846100980518     c                   kfld                    dctaac
846200980518     c                   kfld                    dctfil
846300980518     c                   kfld                    dctnca
846400980625      *
846500980526     c     kdcd2b        klist
846600980526     c                   kfld                    artfls
846700980526     c                   kfld                    artlna
846800980526     c                   kfld                    artnrs
846900980526     c                   kfld                    artnsc
847000980526     c                   kfld                    i00aac
847100980526     c                   kfld                    i00fil
847200980526     c                   kfld                    i00nca
847300981009      *
847400980527     c     kdcd2c        klist
847500980527     c                   kfld                    v1sfls
847600990309     c                   kfld                    v1hlna
847700980527     c                   kfld                    i00nrs
847800980527     c                   kfld                    v1snsc
847900980527     c                   kfld                    i00aac
848000980527     c                   kfld                    i00fil
848100980527     c                   kfld                    i00nca
848200981016      *
848300981016     c     kdcd2d        klist
848400981016     c                   kfld                    v1sfls
848500990309     c                   kfld                    v1hlna
848600981016     c                   kfld                    i00nrs
848700981016     c                   kfld                    v1snsc
848800051019      *
848900051019     c     K04DCR02      klist
849000051019     c                   kfld                    DCDaac
849100051019     c                   kfld                    DCDfil
849200051019     c                   kfld                    DCDnca
849300051019     c                   kfld                    DCRseg
849400980625      *
849500980520     c     knumca        klist
849600980520     c                   kfld                    i00aac
849700980520     c                   kfld                    i00fil
849800980520     c                   kfld                    i00nca
849900981020      *
850000981020     c     KDKA          klist
850100981020     c                   kfld                    i00aac
850200981020     c                   kfld                    i00fil
850300981020     c                   kfld                    i00nca
850400981020     c                   kfld                    Wtrc
850500981113      *
850600981113     C     Kspe          klist
850700981113     C                   KFLD                    KFLS
850800981113     C                   KFLD                    KCLI
850900981113     C                   KFLD                    KCLU
851000980721      *
851100981228     c     klbl_F        klist
851200981228     c                   kfld                    LBLaan
851300981228     c                   kfld                    LBLlpn
851400981228     c                   kfld                    LBLnrn
851500981228     c                   kfld                    LBLnsn
851600990308      *
851700990308     c     kanm          klist
851800990308     c                   kfld                    v1sfls
851900990311     c                   kfld                    v1hlna
852000990311     c                   kfld                    I00nrs
852100990308     c                   kfld                    v1snsc
852200990315      *
852300990315     c     kdcr          klist
852400990315     c                   kfld                    dctaac
852500990315     c                   kfld                    dctfil
852600990315     c                   kfld                    dctnca
852700990315     c                   kfld                    Wv6snsc
852800990315     c                   kfld                    Wv6spor
852900060309
853000060309     c     KTam          Klist
853100160818     c                   Kfld                    iLV59KSC
853200060313     c                   Kfld                    Ktamctr
853300160818     c                   Kfld                    oLV59PRG
853400120111     c     kANM2         klist
853500120111     c                   kfld                    dcdfls
853600120111     c                   kfld                    dcdlna
853700120111     c                   kfld                    dcdnrs
853800120111     c                   kfld                    dcdnsc
853900120111     c                   kfld                    kcaa
854000131106     c     kcaeP         klist
854100131106     c                   kfld                    kepa
854200131106     c                   kfld                    w0030
854300131106     c                   kfld                    vuotoTFP
854400131106     c     kcaeA         klist
854500131106     c                   kfld                    kepa
854600131106     c                   kfld                    w0030
854700981224      *
854800951009     C                   ENDSR
854900050523      *---------------------------------------------------------------*
855000050523      *?Reperimento Dati del job (Utente/Operativi)                  ?*
855100050523      *---------------------------------------------------------------*
855200050523     c     DatiJob       BegSr
855300050523      *
855400050523     c     *dtaara       define    §azute        AZUTEds
855500050523     c     *dtaara       define    §datiute      dDatiUte
855600050523      *
855700050523     c                   in(E)     *dtaara
855800050523     c                   IF        %ERROR or RSUT = *blanks
855900050523     c                   clear                   Tibs34Ds
856000050523     c                   call      'TIBS34R'
856100050523     c                   parm                    Tibs34Ds
856200050523     c                   in        *dtaara
856300050523     c                   ENDIF
856400050523      *
856500050523     c                   EndSr
856600980611      *------------------------------------------------------------
856700990210     OFNDCT000  E            FNDCTfree
856800980731      *------------------------------------------------------------
856900050217** cmd
857000050217OVRDBF FILE(        ) TOFILE(
857100981014** MSG  L78                                                                  *
857200980709Immettere il Tipo Anomalia                                                     1
857300980709Tipo Anomalia inesistente o annullato                                          2
857400980709Immettere la data apertura                                                     3
857500980709Data apertura errata                                                           4
857600980709Immettere Numero Evento valido                                                 5
857700980709Numero Evento inesistente o annullato                                          6
857800980709P.O. non interessato all'evento immesso. ENTER PER FORZARE                     7
857900980720Non è possibile escludere segnacolli non inclusi in C.A.                       8
858000990111La descrizione pezzi dannegg./mancanti è obbligatoria se inserito il n. pezzi  9
858100981015I colli danneggiati/mancanti non possono superare il colli in spedizione       10
858200980709MANCA NUMERATORE C.A.: telefonare in Sede !!!!!!!                              11
858300980709NUMERATORE C.A. non disponibile: RITENTARE  NUOVAMENTE !!!                     12
858400980709Anomalia segnacollo inesistente o annullata                                    13
858500980709Bolla non manutenzionabile                                                     14
858600981015Il numero colli selezionati è diverso dal numero colli Danneggiati/Mancanti    15
858700981009Impossibile aprire C.A. : pratica giacenza aperta                              16
858800010911LIBERO - ex Impossibile aprire C.A. : superata la data pulizia spedizioni      17
858900981022Codice 9999 non utilizzabile. Inserire 8888 o un codice esistente su P.d.C.    18
859000981022Codice errato o annullato                                                      19
859100981021La data importo trattenuto è obbligatoria                                      20
859200981021Se immessa la data immettere anche l'importo trattenuto o eliminare entrambi   21
859300981021Data importo trattenuto errata                                                 22
859400981127MANCA LIMITE RISARCIBILE: telefonare in Sede !!!!!!                            23
859500020125Mancano le caratteristiche per mantenere il flag Franchigia valorizzato ("F")  24
859600981201Per le spedizioni Import in porto assegnato il beneficiario è il DESTINATARIO  25
859700980709Attenzione !! Modificata Località                                              26
859800980709Attenzione !! Modificato CAP                                                   27
859900980709Attenzione !! Modificata Provincia                                             28
860000980709Attenzione !! Modificata Nazione                                               29
860100990218Inserito segnacollo non appartenente alla C.A.                                 30
860200981009Impossibile aprire C.A. : tutti i colli sono su altre C.A.                     31
860300980710Il collo non è consegnato: selezione non possibile per tipo anomalia scelto    32
860400000307I Kg danneggiati/mancanti non possono superare i Kg in bolla ENTER PER FORZARE 33
860500980720Se scelti Tutti i segnacolli non è ammesso Inclusione/Esclusione               34
860600990226ATTENZIONE !! La spedizione risulta in distinta                                35
860700990217Indicare il numero dei colli danneggiati/mancanti                              36
860800990111Il numero pezzi danneggiati/mancanti è obbligatorio                            37
860900980710Non è possibile confermare la C. A.: manca numero spedizione                   38
861000980709Se immessa la divisa immettere anche l'importo o eliminare entrambi            39
861100980709Divisa errata                                                                  40
861200980709Non ammessi importi con decimali per la divisa indicata                        41
861300980709Opzione non gestibile: telefonare in Sede !!!                                  42
861400980721La descrizione anomalia è obbligatoria                                         43
861500980710Il motivo del danno è obbligatorio                                             44
861600981009Impossibile aprire C.A. : spedizione con blocco                                45
861700980709Per la spedizione risultano aperte altre C.A.                                  46
861800980720Selezionare colli Dannegg./mancanti: già aperta C.A. senza dettaglio colli     47
861900990114Il numero colli danneggiati/mancanti è maggiore dei colli selezionabili        48
862000981126Tipo gestione: impostare correttamente i test                                  49
862100981126Tipo gestione: è OBBLIGATORIO procedere con pratica assicurativa               50
862200981126Tipo gestione: è anche possibile procedere con pratica assicurativa            51
862300980710Il collo è consegnato: selezione non possibile per tipo anomalia scelto        52
862400090303Per il segnacollo  inserire almeno 1 punto di responsabilità                   53
862500980720In partenza non è possibile impostare l'arrivo come P.O verifica DDT           54
862600980720In arrivo non è possibile impostare la partenza come P.O. verifica DDT         55
862700980720Impostare P.O. verifica DDT                                                    56
862800981020Indicare le disposizioni merce in danno                                        57
862900990209P.O. inesistente o annullato                                                   58
863000120619Inseriti dati non validi o incompleti                                          59
863100090303I punti di responsabilità devono essere maggiori/uguali ai colli danneg/mancan
863200981015Il peso danneggiato/mancante è obbligatorio                                    61
863300980918Inserire i pezzi danneggiati/mancanti o la descrizione dei pezzi presenti      62
863400981126Conferma C.A. forzata prima dello scadere dei xxx giorni previsti              63
863500981009Impossibile aprire C.A. : spedizione con consegna anomala dirottamento         64
863600981014Importo da Assicurare non indicato per cliente con Mandato a Valore Variabile  65
863700990127Inserire l'Importo Massimo Risarcibile                                         66
863800981020Tipo anomalia valido solo per EVENTI                                           67
863900981020Segnacollo utilizzato per l'apertura non incluso in C.A.                       68
864000981029Respingere Rivalsa: il cliente ha trattenuto                                   69
864100120723Rimborso a carico del vettore: il cliente ha trattenuto                        70 LIBERO
864200990120Il cliente trattiene, indicarne il codice nel beneficiario alternativo         71
864300120723Rimborso a carico di CONSULDANNI: pervenuta rivalsa                            72 LIBERO
864400080118E-MAIL non inviata:                                                            73
864500120723Tipo rimborso cliente non corretto                                             74 LIBERO
864600990226Progetto di Liquidazione rimborsato, l'Importo Trattenuto non si può inserire  75
864700990127L'Importo Trattenuto non può essere maggiore dell'Importo concordato           76
864800981113Il Massimo Risarcibile inserito è maggiore del calcolato                       77
864900120723Rimborso a carico del vettore: il tipo gestione è Liquidazione Transattiva     78 LIBERO
865000981217Per le spedizioni Import in porto franco il beneficiario è il MITTENTE         79
865100990510Inserire la Responsabilità Internazionale                                      80
865200020125Il Mass.Risarcib. è > del limite per Pratiche in Franchigia. ENTER per forzare 81
865300981228Impossibile aprire C.A. : spedizione figlia di un cambio porto                 82
865400990108Impossibile aprire C.A. : tipo anomalia non valido per spedizione consegnata   83
865500990111Il numero pezzi danneggiati/mancanti è obbligatorio se inserita la descrizione 84
865600990111ATTENZIONE !! Ci sono dei colli non consegnati per questa spedizione           85
865700990113Data apertura minore della data spedizione                                     86
865800990114Impossibile aprire C.A. : tipo anomalia non valido per sped. non consegnata    87
865900990127Impossibile aprire C.A. : non esiste spedizione per il segnacollo indicato     88
866000990127Inserire l'Importo Concordato                                                  89
866100990127L'Importo Concordato è superiore all'Importo Limite per la Liq. Transattiva    90
866200990209Modifica non consentita C.A. in attesa di ricalcolo responsabilità             91
866300990218Tipo Attribuzione inesistente o annullato                                      92
866400990217I colli danneggiati/mancanti sono obbligatori: spedizione di Reso              93
866500990306P.O. e Tipo Attribuzione già inseriti per il segnacollo                        94
866600990218Per il Tipo Attribuzione inserito non indicare il segnacollo                   95
866700990218Per il Tipo Attribuzione inserito non indicare il Punto Operativo              96
866800990226ATTENZIONE !! Inserire oportuna descrizione dei Pezzi Danneggiati/Mancanti     97
866900050428Beneficiario alternativo non è cod.fatturaz.del beneficiario.ENTER per forzare 98
867000990306Non è possibile modificare la Riserva: esiste un'evento di Riserva             99
867100990308Impossibile aprire C.A.: porto assegnato in partenza consegnato senza riserva  100
867200990308Impossibile aprire C.A.: porto franco in arrivo consegnato senza riserva       101
867300990308Impossibile aprire C.A.: spedizione in partenza consegnata con riserva         102
867400990308Impossibile aprire C.A.: avaria non consentita con evento smarrimento autotras 103
867500990308Prima della conferma per gli assegnati non è possibile indicare il mittente    104
867600990311Impossibile aprire C.A.: il segnacollo selezionato non é in IDD3               105
867700990406Manutenzione non consentita: responsabilità non considerata nella reportistica 106
867800990415Se Export con beneficiario il DESTINATARIO tipo gestione deve essere PRATICA   107
867900990415Non è possibile togliere la Riserva.                                           108
868000000628Impossibile aprire C.A.: spedizione posta. (Ammesso solo in caso di evento)    109
868100000628Impossibile togliere Numero Evento per una C.A. aperta su spedizione Posta     110
868200010910Ammessi importi con massimo __ decimali per la divisa indicata                 111
868300050428Beneficiario Alternativo deve essere un codice di fatturazione                 112
868400051019Avvisare la SEDE della modifica del peso e dell'importo massimo risarcibile    113
868500080208Tipo gestione: è OBBLIGATORIO procedere con liquidazione transattiva           114
868600080211Tipo gestione: è anche possibile procedere con liquidazione transattiva        115
868700080229Il Beneficiario è stato modificato, verificare il Beneficiario Alternativo     116
868800110504Beneficiario Alternativo obbligatorio in caso di Cambio Porto o Dirottamento   117
868900120508Mancano Coordinate bancarie Bonifico Danni: Avvisare Uff.Gestione Pagamenti    118
869000120508Porto Franco Import, il beneficiario deve essere il Partner                    119
869100120619Per ogni collo i punti di responsabilità non possono essere superiori a xxxxx  120
869200120619I punti di responsabilità in totale non devono superare xxxxxx per ogni collo  121
869300131105Il codice inserito non è un terminal                                           122
869400050601**
869500050601OVRPRTF FIDN03P  OUTQ(          ) USRDFNDTA('
