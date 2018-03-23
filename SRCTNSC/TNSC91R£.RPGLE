000100910802     H DECEDIT('0,') DATEDIT(*DMY.)
000200000223     H*--------------------------------------------------------------*
000300000223     H* ATTENZIONE !!!                                               *
000400000223     H*--------------------------------------------------------------*
000500000223     H* Se vengono modificate le logiche del pagamento così come è   *
000600000223     H* già stato fatto (sostituito il codice cliente nell'attribuz. *
000700000223     H* del ramo aziendale di pagamento nel giugno 1999 per i C/Ass. *
000800000223     H* intestati a Bartolini, gestita la lnp della bolla originale  *
000900000223     H* per l'attribuzione della ramo della lnp nel febbraio 2000)   *
001000000223     H* occorre completare per intero il giro dei pagamenti infragr. *
001100000223     H* altrimenti la stampa dei C/Assegni incassati non pagati non  *
001200000223     H* quadrerà mai con la contabilità. I pagamenti potrebbereo     *
001300000223     H* non funzionare correttamente in quanto un C/Assegno prima    *
001400010308     H* considerato infragruppo (per cui è già stato scritto il TNCSG*
001500000223     H* potrebbe non esserlo più se esaminato con nuove logiche o    *
001600000223     H* viceversa)                                                   *
001700000223     H*--------------------------------------------------------------*
001800941102     H* TNSC91R  *---------------------------------------------------*
001900910320     H*----------*                                                   *
002000910902     H*           - CREAZIONE FILE DI WORK                           *
002100910320     H*--------------------------------------------------------------*
002200010220     FTNCSB04L  IF   E           K DISK    USROPN
002300010308      *FTNCSB03LIFE           K        DISK
002400971218     F                                     RENAME(TNCSB000:TNCSBSPE)
002500010315      *FTNCSG02LIF  E           K        DISK
002600010315      *F            TNCSG000                          KRENAMETNCSG002
002700010220     FAZORG01L  IF   E           K DISK
002800010220     FTNCSA01L  IF   E           K DISK    USROPN
002900990728     FTITAS30C  IF   E           K DISK
003000941102     FTNCSBWRK  O    E             DISK
003100971217     D*--------------------------------------------------------------*
003200910904     D FIL             S              3    DIM(29)
003300910904     D L3              S              3    DIM(999)
003400910904     D §L3             S              3    DIM(999)
003500910909     D §DI             S              1  0 DIM(999)
003600910909     D §CA             S              3  0 DIM(999)
003700971217     D*--------------------------------------------------------------*
003800910802     D KPJBA         E DS
003900910902     D PARAM           DS
004000910902     D* RICH. C/A SCOPERTI
004100910902     D  VSEL1                  1      1
004200910902     D* RICH. C/A INCASSATI E NON PAGATI
004300910902     D  VSEL2                  2      2
004400911003     D* RICH. C/A PAGATI
004500911003     D  VSEL3                  3      3
004600911003     D  VORD                   4      4
004700911003     D  VDET                   5      6
004800941102     D  DASPE                  7     14  0
004900941102     D  ASPE                  15     22  0
005000941102     D  DAINC                 23     30  0
005100941102     D  AINC                  31     38  0
005200941103     D  LIBERO                39     40
005300941102     D  DAPAG                 41     48  0
005400941102     D  APAG                  49     56  0
005500941102     D  VDAFIL                57     59  0
005600941102     D  VAFIL                 60     62  0
005700960909     D* RICH. C/A PAGATI E NON INCASSATI
005800960909     D  VSEL4                 63     63
005900010105     D* SCOPERTI PARZIALIZZAZZIONE B= BARTOLINI P=POSTE ' ' TUTTI
006000010105     D  VPARZ                 64     64
006001040309     D* FILE ESTERO
006002040309     D  VFILE1                65     65
006003040309     D* C.A. S/N/SOLO C.A
006004040913     D  Vscop1                66     66
006005040913     D* FILE italia
006006040913     D  VFILi1                67     67
006009050711     D  vclida                68     74  0
006010050711     D  vclia                 75     81  0
006100910902     D*
006200910902     D* KEY4 DATA + NR.SPEDIZ.
006300910902     D DTNSP           DS
006400941102     D  CSBAAS                 1      3P 0
006500941102     D  CSBMGS                 4      6P 0
006600941102     D  CSBNRS                 7      8P 0
006700941102     D  CSBNSP                 9     12P 0
006800911009     D DTRAG           DS
006900911009     D  RAGIO                  1     13
007000911009     D  NSP                   14     20  0
007100010306     D*FTNSC96    E DSTNSC96DS
007200971218     D FNLV55        E DS                  EXTNAME(FNLV55DS)
007300980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
007400980127     D CNACO         E DS                  EXTNAME(CNACO00F)
007500980127     D CNIND         E DS                  EXTNAME(CNIND00F)
007600980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
007700980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
007800010105     D OG143         E DS
007900990611     D*  Ds per ricezione dati di incasso
008000990611     D WSVINC          DS            75
008100990617     D  TPI                   30     30
008200990611     D  DATINC                47     54  0
008300010306     D*FCSBINC      DS
008400010306     D*F                                      1  10 CSBNRA
008500010306     D*F                                     11  180CSBDTE
008600010306     D*F                                     19  230CSBABI
008700010306     D*F                                     24  280CSBCAB
008800010306     D*F                                     29  29 CSBTPA
008900010306     D*F                                     30  30 CSBTPI
009000010306     D*F                                     31  380CSBUDA
009100010306     D*F                                     39  460CSBDTA
009200010306     D*F                                     47  540CSBDDC
009300010306     D*F                                     55  610CSBNDT
009400010306     D*F                                     62  670CSBNRI
009500010306     D*F                                     68  750CSBDRI
009600990611     D*  Ds per ricezione dati di incasso
009700990611     D WSVPAG          DS            46
009800990611     D  DATPAG                16     23  0
009900010306     I*FCSBPAG      DS
010000010306     I*F                                      1   1 CSBFPC
010100010306     I*F                                      2   80CSBBNA
010200010306     I*F                                      9  150CSBNDP
010300010306     I*F                                     16  230CSBDDP
010400010306     I*F                                     24  310CSBDRG
010500010306     I*F                                     32  370CSBNRG
010600010306     I*F                                     38  464CSBCMP
010700910321     C***------------------------------------------***
010800971218     C*  Operazioni inizio lettura TNCSG
010900971218     C                   EXSR      SETCSB
011000910902     C*
011100910902     C     *IN90         DOUEQ     '1'
011200960909     C  N03
011300960909     CANN02KBN6          READE     TNCSB04L                               90
011400950623     C   03              READ      TNCSB04L                               90
011500960909     C   02              READ      TNCSB04L                               90
011600910902     C*
011700910902     C  N90              DO
011800971218     C*-------------------------------------------------*
011900971218     C* Controllo stato C/Assegno per estrazione
012000971218     C                   EXSR      CTRSTA
012100971218     C     SELSTA        CABEQ     'N'           NOWRT
012200971218     C*-------------------------------------------------*
012300971218     C* Eseguo controlli pagamenti infragruppo
012400010306     C*F                   EXSR INFRAG
012500010306     C*F         SELPIG    CABEQ'N'       NOWRT
012600971217     C*-------------------------------------------------*
012700010308     C* SCOPERTI X LNA:
012800971217     C     *IN04         IFEQ      '1'
012900971217     C     VORD          ANDEQ     'A'
013000971218     C                   EXSR      SCOPOK
013100971218     C     SELSCO        CABEQ     'N'           NOWRT
013200971217     C                   END
013300971219     C* SCOPERTI X LNP:
013400971219     C     *IN04         IFEQ      '1'
013500971219     C     VORD          ANDEQ     'P'
013600971219     C                   EXSR      NOPGOK
013700971219     C     SELSCO        CABEQ     'N'           NOWRT
013800971219     C                   END
013900971217     C*-------------------------------------------------*
014000971219     C* INCASSATI DA PAGARE
014100971217     C     *IN01         IFEQ      '1'
014200971218     C                   EXSR      INCAOK
014300971218     C     SELINC        CABEQ     'N'           NOWRT
014400971217     C                   END
014500971217     C*-------------------------------------------------*
014600971218     C* PAGATI
014700971217     C     *IN03         IFEQ      '1'
014800971218     C                   EXSR      PAGAOK
014900971218     C     SELPAG        CABEQ     'N'           NOWRT
015000971217     C                   END
015100971217     C*-------------------------------------------------*
015200971217     C* PAGATI NON INCASSATI
015300971218     C     *IN02         IFEQ      '1'
015400971218     C                   EXSR      PAGANT
015500971218     C     SELANT        CABEQ     'N'           NOWRT
015600971218     C                   END
015700971217     C*-------------------------------------------------*
015800971218     C* PARZIALIZZAZIONI SU DATE
015900971218     C                   EXSR      PRZDAT
016000971218     C     SELDAT        CABEQ     'N'           NOWRT
016100971217     C*-------------------------------------------------*
016200971218     C* SCRIVO DATI SU FILE DI WORK
016300971218     C                   EXSR      WRTCSB
016400910902     C                   END
016500971217     C*
016600971218     C     NOWRT         TAG
016700910902     C                   END
016800971218     C*-------------------------------------------------*
016900971218     C* Se richiesta estrazione pagati eseguo lettura tncsg:
017000971218     C* considero anche i C/Ass.rimessi dal mio ramo aziendale
017100971218     C* ad un altro ramo ma da questo non ancora pagato
017200971218     C     *IN02         IFEQ      '1'
017300971218     C     *IN03         OREQ      '1'
017400010308     C*F                   EXSR SETCSG
017500971218     C* loop lettura TNCSG
017600010308     C*F         *IN91     DOWEQ'0'
017700971218     C*-------------------------------------------------*
017800971218     C* Aggancio TNCSB e verifico di non avere già estratto
017900971218     C* il C/Assegno
018000010306     C*F                   EXSR CHNCSB
018100010306     C*F         SELCSB    CABEQ'N'       NOWRTG
018200971218     C*-------------------------------------------------*
018300971218     C* Controllo stato C/Assegno per estrazione
018400971218     C                   EXSR      CTRSTA
018500010306     C*F                   EXSR CALL96
018600971218     C     SELSTA        CABEQ     'N'           NOWRTG
018700990616     C* Se lnA NON è del mio ramo non lo considero
018800990616     C* (QUINDI IL RAMO PAGAMENTO SI)
018900010308     C*F         D96RSI    CABNED96RLA    NOWRTG
019000971218     C*-------------------------------------------------*
019100971218     C* Pagati
019200971218     C     *IN03         IFEQ      '1'
019300971218     C                   EXSR      PAGAOK
019400971218     C     SELPAG        CABEQ     'N'           NOWRTG
019500971218     C                   END
019600971218     C*-------------------------------------------------*
019700971218     C* Pagati non incassati
019800971218     C     *IN02         IFEQ      '1'
019900971218     C                   EXSR      PAGANT
020000971218     C     SELANT        CABEQ     'N'           NOWRTG
020100971218     C                   END
020200971218     C*-------------------------------------------------*
020300971218     C* Parzializzazione su date
020400971218     C                   EXSR      PRZDAT
020500971218     C     SELDAT        CABEQ     'N'           NOWRT
020600971218     C*-------------------------------------------------*
020700971218     C* Scrivo dati su file di work
020800971218     C                   EXSR      WRTCSB
020900971218     C*
021000971218     C     NOWRTG        TAG
021100010315     C*F                   READ TNCSG02L                 91
021200971218     C                   END
021300010308     C*F                   END
021400971218     C*          -------------
021500971218     C* RICHIAMO PGM X CHIUSURA IN LR
021600010306     C*F                   CLEARTNSC96
021700010306     C*F                   MOVEL'C'       D96TLA
021800010306     C*F                   CALL 'TNSC96R'
021900010306     C*F                   PARM           KPJBA
022000010306     C*F                   PARM           TNSC96
022100010306     C*F                   PARM           WSVINC
022200010306     C*F                   PARM           WSVPAG
022300971218     C* RICHIAMO PGM X CHIUSURA IN LR
022400971218     C                   CLEAR                   FNLV55
022500990611     C                   MOVEL     'C'           D55TLA
022600971218     C                   CALL      'FNLV55R'
022700990611     C                   PARM                    FNLV55
022800980127     C*
022900980127     C                   CLEAR                   TIBS69
023000980127     C                   MOVEL     'C'           I69TLA
023100980127     C                   CALL      'TIBS69R'
023200980127     C                   PARM                    TIBS69
023300980127     C                   PARM                    CNACO
023400980127     C                   PARM                    CNIND
023500980127     C                   PARM                    CNCLP
023600980127     C                   PARM                    FNCLS
023700971218     C*
023800990617     C                   MOVEL     PARAM         KPJBU
023900910902     C                   SETON                                        LR
024000971218     C*-------------------------------------------------*
024100971218     C* SETCSB: posizionamento su TNCSB
024200971218     C*-------------------------------------------------*
024300971218     C     SETCSB        BEGSR
024400971218     C*-------------------------------------------------*
024500010220     C                   OPEN      TNCSB04L
024600010220     C                   OPEN      TNCSA01L
024700971218     C*-------------------------------------------------*
024800971218     C* Posizionamento in base all'opzione richiesta
024900971218     C                   Z-ADD     0             KBN6
025000971218     C     VSEL1         IFEQ      'X'
025100971218     C     KBN6          SETLL     TNCSB04L
025200971218     C                   SETON                                        04
025300971218     C                   END
025400971218     C  N04VSEL2         IFEQ      'X'
025500971218     C                   Z-ADD     DAPAG         KBN6
025600971218     C     KBN6          SETLL     TNCSB04L
025700971218     C                   SETON                                        01
025800971218     C                   END
025900971218     C  N04
026000971218     CANN01VSEL3         IFEQ      'X'
026100971218     C                   Z-ADD     DAPAG         KBN6
026200971218     C     KBN6          SETLL     TNCSB04L
026300971218     C                   SETON                                        03
026400971218     C                   END
026500971218     C  N04
026600971218     CANN01
026700971218     CANN03VSEL4         IFEQ      'X'
026800971218     C                   Z-ADD     DAPAG         KBN6
026900971218     C     KBN6          SETLL     TNCSB04L
027000971218     C                   SETON                                        02
027100971218     C                   END
027200971218     C*
027300971218     C                   ENDSR
027400971218     C*-------------------------------------------------*
027500010306     C*FSETCSG: posizionamento su TNCSG
027600971218     C*-------------------------------------------------*
027700010306     C*F         SETCSG    BEGSR
027800971218     C*-------------------------------------------------*
027900010306     C*F                   SETOF                     23
028000010306     C*F                   OPEN TNCSG02L
028100010306     C*F                   OPEN TNCSB03L
028200971218     C* Posizionamento in base all'opzione richiesta
028300010306     C*F                   Z-ADDDAPAG     KDDT
028400010306     C*F         KDDT      SETLLTNCSG02L
028500010306     C*F                   READ TNCSG02L                 91
028600971218     C*
028700010306     C*F                   ENDSR
028800971218     C*-------------------------------------------------*
028900971218     C* CHNCSB: posizionamento su TNCSB
029000971218     C*-------------------------------------------------*
029100010306     C*F         CHNCSB    BEGSR
029200971218     C* Aggancio TNCSB
029300010306     C*F                   Z-ADDCSGAAS    KAAS
029400010306     C*F                   Z-ADDCSGLNP    KLNP
029500010306     C*F                   Z-ADDCSGNRS    KNRS
029600010306     C*F                   Z-ADDCSGNSP    KNSP
029700010306     C*F         KCSB      CHAINTNCSB03L             90
029800971218     C* Se non esiste TNCSG oppure è già stata impostata
029900971218     C* la data pagam.su di esso (C/Ass.in prec.estraz.)
030000971218     C* non lo considero
030100010306     C*F         *IN90     IFEQ '1'
030200010306     C*F         CSBDDP    ORGE DAPAG
030300010306     C*F         CSBDDP    ANDLEAPAG
030400010306     C*F                   MOVEL'N'       SELCSB  1
030500010306     C*F                   ELSE
030600010306     C*F                   MOVEL'S'       SELCSB  1
030700010306     C*F                   END
030800971218     C*
030900010306     C*F                   ENDSR
031000971218     C*-------------------------------------------------*
031100971218     C* CRTSTA: Controllo stato C/Assegno x estrazione
031200971218     C*-------------------------------------------------*
031300971218     C     CTRSTA        BEGSR
031400971218     C*
031500971218     C                   MOVEL     'N'           SELSTA
031600971218     C* ESCLUDO ANNULLATI
031700971218     C     CSBSTA        CABEQ     9             FISTA
031800971218     C* ESCLUDO CONTENZIOSO !!!
031900971218     C     CSBSTA        CABEQ     3             FISTA
032000971218     C* ESCLUDO C/A STORNO PAGAMENTO
032100971218     C     CSBSTA        CABEQ     6             FISTA
032200971218     C* ESCLUDO C/A ERRATO PAGAMENTO
032300971218     C     CSBSTA        CABEQ     7             FISTA
032400971218     C* ESCLUDO C/A OMESSI
032500971218     C     CSBSTA        CABEQ     8             FISTA
032600971218     C*-------------------------------------------------*
032700971218     C* ESCLUDO QUELLI FUORI LIMITE FILIALE
032800971218     C     VORD          IFEQ      'P'
032900971218     C     CSBLNP        CABLT     VDAFIL        FISTA
033000971218     C     CSBLNP        CABGT     VAFIL         FISTA
033100971218     C                   ELSE
033200971218     C     CSBLNA        CABLT     VDAFIL        FISTA
033300971218     C     CSBLNA        CABGT     VAFIL         FISTA
033400971218     C                   END
033401050707     C*-------------------------------------------------*
033402050707     C* ESCLUDO i clienti FUORI LIMITE
033403050707     C                   IF        vclida <> 0 or vclia <> 0
033404050707     C     CSBksc        CABLT     Vclida        FISTA
033405050707     C     CSBksc        CABGT     Vclia         FISTA
033406050707     c                   end
033500971218     C*
033600971218     C                   MOVEL     'S'           SELSTA            1
033700971218     C*
033800971218     C     FISTA         ENDSR
033900971218     C*-------------------------------------------------*
034000010306     C*FINFRAG: Controlli per pagamenti infragruppo
034100971218     C*-------------------------------------------------*
034200010306     C*F         INFRAG    BEGSR
034300971218     C*
034400990611     C* controllo di quale ramo sono lnp e la lna della spedizione
034500010306     C*F                   CLEARTNSC96
034600010306     C*F                   CLEARWSVINC
034700010306     C*F                   CLEARWSVPAG
034800010306     C*F                   MOVEL*BLANKS   D96TLA
034900010306     C*F                   MOVELCSBTPI    TPI
035000010306     C*F                   MOVEL'E'       D96TPD
035100010306     C*F                   MOVELKNSIF     D96NSI
035200010306     C*F                   Z-ADDCSBAAS    D96AAS
035300010306     C*F                   Z-ADDCSBLNP    D96LNP
035400010306     C*F                   Z-ADDCSBLNA    D96LNA
035500010306     C*F                   Z-ADDCSBNRS    D96NRS
035600010306     C*F                   Z-ADDCSBNSP    D96NSP
035700010306     C*FControllo il codice cliente da passare
035800010306     C*F         CSBCDI    IFNE 0
035900010306     C*F                   MOVELCSBCDI    D96CDI
036000010306     C*F                   ELSE
036100010306     C*F                   MOVELCSBKSC    D96CDI
036200010306     C*F                   END
036300010306     C*FRichiamo pgm
036400010306     C*F                   CALL 'TNSC96R'
036500010306     C*F                   PARM           KPJBA
036600010306     C*F                   PARM           TNSC96
036700010306     C*F                   PARM           WSVINC
036800010306     C*F                   PARM           WSVPAG
036900010306     C*F-------------------------------------------------------- *
037000010306     C*FINFRAGRUPPO:
037100010306     C*F .... Se sto esaminando gli scoperti per lnp prendo in
037200010306     C*F      considerazione il C/Assegno se la lnp della sped.
037300010306     C*F      (lnp originale nel caso di dirottamento) è del mio
037400010306     C*F      ramo aziendale
037500010306     C*F .... Se sto esaminando gli scoperti per lna prendo in
037600010306     C*F      considerazione il C/Assegno se la lna della sped.
037700010306     C*F      è del mio ramo aziendale
037800010306     C*F .... Se sto esaminando gli incassati/non pagati o pagati
037900010306     C*F      non incassati prendo
038000010306     C*F      in considerazione il C/Assegno :
038100010306     C*F      - se la lnp è del mio ramo aziendale per i mittenti
038200010306     C*F      - se il cliente è del mio ramo se intestato a Bartolini
038300010306     C*F      - se la lna è del mio ramo aziendale
038400010306     C*F-------------------------------------------------------- *
038500010306     C*F      (Possiamo ragionare tranquillamente con il ramo di
038600010306     C*F      pagamento per i C/Assegni INCASSATI NON PAGATI xchè
038700010306     C*F      in fase di attivazione di una nuova logica di attri-
038800010306     C*F      buzione del ramo di pagamento ocorrerà concludere
038900010306     C*F      il giro dei pagamenti, compresi quelli infragruppo,
039000010306     C*F      per quanto riguarda i PAGATI NON INCASSATI non sono
039100010306     C*F      ancora stati attivati quindi per il momento non han-
039200010306     C*F      no problemi di passaggio da una logica ad un'altra)
039300010306     C*F-------------------------------------------------------- *
039400010306     C*F         *IN04     IFEQ '1'
039500010306     C*F         VORD      ANDEQ'P'
039600010306     C*F         D96RSI    ANDNED96RLP
039700010306     C*F         *IN04     OREQ '1'
039800010306     C*F         VORD      ANDEQ'A'
039900010306     C*F         D96RSI    ANDNED96RLA
040000010306     C*F         *IN03     ORNE '1'
040100010306     C*F         *IN04     ANDNE'1'
040200010306     C*F         D96RSI    ANDNED96RLA
040300010306     C*F         D96RSI    ANDNED96RMP
040400010306     C*F      per i C/Assegni pagati prendo
040500010306     C*F      in considerazione il C/Assegno se è del mio ramo
040600010306     C*F      aziendale la lna oppure se esiste il pagamento infregruppo
040700010306     C*F         *IN03     OREQ '1'
040800010306     C*F         *IN04     ANDNE'1'
040900010306     C*F         D96RSI    ANDNED96RLA
041000010306     C*F         D96TRV    ANDNE'S'
041100010306     C*F                   MOVEL'N'       SELPIG
041200010306     C*F                   ELSE
041300010306     C*F                   MOVEL'S'       SELPIG  1
041400010306     C*F                   END
041500990615     C*--------------------------------------------------------- *
041600010306     C*F         NOPIG     ENDSR
041700990611     C*-------------------------------------------------*
041800010306     C*FCALL96: Controlli per pagamenti infragruppo
041900010306     C*F------------------------------------------------*
042000010306     C*F         CALL96    BEGSR
042100010306     C*F
042200010306     C*Fcontrollo di quale ramo sono lnp e la lna della spedizione
042300010306     C*F                   CLEARTNSC96
042400010306     C*F                   CLEARWSVINC
042500010306     C*F                   CLEARWSVPAG
042600010306     C*F                   MOVEL*BLANKS   D96TLA
042700010306     C*F                   MOVEL'E'       D96TPD
042800010306     C*F                   MOVELCSBTPI    TPI
042900010306     C*F                   MOVELKNSIF     D96NSI
043000010306     C*F                   Z-ADDCSBAAS    D96AAS
043100010306     C*F                   Z-ADDCSBLNP    D96LNP
043200010306     C*F                   Z-ADDCSBLNA    D96LNA
043300010306     C*F                   Z-ADDCSBNRS    D96NRS
043400010306     C*F                   Z-ADDCSBNSP    D96NSP
043500010306     C*FControllo il codice cliente da passare
043600010306     C*F         CSBCDI    IFNE 0
043700010306     C*F                   MOVELCSBCDI    D96CDI
043800010306     C*F                   ELSE
043900010306     C*F                   MOVELCSBKSC    D96CDI
044000010306     C*F                   END
044100010306     C*F                   MOVEL'S'       D96CSG
044200010306     C*F                   Z-ADDCSGDDT    D96DDT
044300010306     C*FRichiamo pgm
044400010306     C*F                   CALL 'TNSC96R'
044500010306     C*F                   PARM           KPJBA
044600010306     C*F                   PARM           TNSC96
044700010306     C*F                   PARM           WSVINC
044800010306     C*F                   PARM           WSVPAG
044900010306     C*F
045000010306     C*F                   ENDSR
045100971218     C*-------------------------------------------------*
045200971218     C* SCOPOK: OK Estrazione scoperti x lna
045300971218     C*-------------------------------------------------*
045400971218     C     SCOPOK        BEGSR
045500971218     C*
045600971218     C                   MOVEL     'N'           SELSCO
045700971218     C*
045800000223     C* Per quanto riguarda i C/Assegni scoperti non ha
045900000223     C* alcuna importanza l'infragruppo, controllo i dati
046000000223     C* di incasso su TNCSB: la data distinta di incasso
046100000223     C* deve essere uguale a zero  !!!
046200971218     C     CSBDDC        CABNE     *ZEROS        NOSCO
046300971218     C*
046400010105      *VERIFICO PARZIALIZZAZIONE POSTE O BARTOLINI
046500010105     C     VPARZ         IFNE      *BLANK
046600010105     C     CSBLNP        CHAIN     AZORG01L                           98
046700010105     C     *IN98         IFEQ      '0'
046800010105     C                   MOVEL     ORGDE3        OG143
046900010105     C                   SELECT
047000010105      * SOLO POSTE
047100020717      * sostituito con il network
047200010105     C     VPARZ         WHENEQ    'P'
047300020717     C**** §OGPT         CABNE     'S'           NOSCO
047400020717     C     §OgNTW        CABNE     'PPT'         NOSCO
047500010105      * SOLO BARTOLINI
047600010105     C     VPARZ         WHENEQ    'B'
047700020717     C**** §OGPT         CABNE     ' '           NOSCO
047800020717     C     §OgNTW        CABEQ     'PPT'         NOSCO
047900010105      *
048000010105     C                   END
048100010105     C                   END
048200010105     C                   END
048300010105      *
048400971218     C                   MOVEL     'S'           SELSCO            1
048500971218     C*
048600971218     C     NOSCO         ENDSR
048700971219     C*-------------------------------------------------*
048800971219     C* NOPGOK:OK Estrazione SCOPERTI X LNP: NO PAGATI
048900971219     C*-------------------------------------------------*
049000971219     C     NOPGOK        BEGSR
049100971219     C*
049200971219     C                   MOVEL     'N'           SELSCO
049300000223     C* Per quanto riguarda i C/Assegni scoperti x lnp non
049400000223     C* ha alcuna importanza l'infragruppo, controllo i dati
049500000223     C* di incasso/mancato pagamento su TNCSB: il C/Assegno
049600000223     C* deve essere considerato sia che risulti non incassato,
049700000223     C* sia che risulti incassato ma non pagato
049800000223     C*
049900971219     C* SE LA DATA DI INCASSO SUL C/ASSEGNO E' <> DA ZERO
050000971219     C* CONTROLLO CHE NON SIA STATO PAGATO
050100971219     C     CSBDDC        IFNE      0
050200971219     C* SE IL RAMO DELLA LNP E' UGUALE A QUELLO IN CUI STO
050300971219     C* LAVORANDO CONFRONTO LIMITE PAGAMENTO
050400971219     C     CSBDDP        CABNE     0             FINOPG               90
050500971219     C                   END
050600971219     C*
050700971219     C                   MOVEL     'S'           SELSCO            1
050800971219     C*
050900971219     C     FINOPG        ENDSR
051000000419     C*------------------------------------------------*
051100971218     C* INCAOK: OK Estrazione incassati
051200971218     C*-------------------------------------------------*
051300971218     C     INCAOK        BEGSR
051400971218     C*
051500971218     C                   MOVEL     'N'           SELINC
051600971218     C* CONTROLLO SEMPRE CHE LA DATA DISTINTA DI INCASSO
051700971218     C* E LA DATA CONFERMA INCASSO SIANO MAGGIORI DI 0
051800971218     C     CSBDTA        CABEQ     *ZEROS        FIINCA
051801030117     c* basta la data conferma incasso. x i c/assegni omessi anticipati
051802030117     c* la data distinta è 0 quindi li ometterebbe
051900030117     C*    CSBDDC        CABEQ     *ZEROS        FIINCA
052000971218     C* PAGAMENTO INFRAGRUPPO !!!!!
052100010308     C*F         D96RMP    IFNE D96RSI
052200010308     C*F         D96RLA    ORNE D96RSI
052300971218     C* SE IL RAMO DELLA LNA E' DIVERSO DA QUELLO IN CUI STO
052400000419     C* LAVORANDO CONSIDERO I C/ASS.CONTROLLANDO  CHE CI SIA TNCSG
052500010308     C*F         D96RLA    IFNE D96RSI
052600010308     C*F         D96TRV    CABNE'S'       FIINCA
052700010308     C*F                   ELSE
052800971218     C* SE IL RAMO DELLA LNA E' UGUALE A QUELLO IN CUI STO
052900000419     C* LAVORANDO CONSIDERO I C/ASS. controllando che non ci sia TNCSG
053000010308     C*F         D96TRV    CABEQ'S'       FIINCA
053100010308     C*F                   END
053200010308     C*F                   END                             INFRAG.
053300971218     C*
053400971218     C                   MOVEL     'S'           SELINC            1
053500971218     C*
053600971218     C     FIINCA        ENDSR
053700971218     C*-------------------------------------------------*
053800971218     C* PAGAOK: OK  C/Assegno pagato
053900971218     C*-------------------------------------------------*
054000971218     C     PAGAOK        BEGSR
054100971218     C*
054200971218     C                   MOVEL     'N'           SELPAG
054300010308     C* CONTROLLO SEMPRE CHE SIA IMPOSTATA LA DATA INCASSO
054400971218     C     CSBDDC        CABEQ     *ZEROS        NOPAG
054500971218     C* SE IL RAMO DELLA LNA E' UGUALE A QUELLO IN CUI STO
054600010308     C*F         D96RLA    IFEQ D96RSI
054700990616     C* CONTROLLO SE ESISTE IL PAGAMENTO INFRAGRUPPO
054800010308     C*F         D96TRV    IFEQ 'S'
054900990616     C* ... SE SI CONTROLLO LE DATE LIMITE PAGAMENTO CON TNCSG
055000010308     C*F         DATPAG    CABLTDAPAG     NOPAG
055100010308     C*F         DATPAG    CABGTAPAG      NOPAG      91
055200010308     C*F         CSBDDP    COMP APAG                 90
055300990616     C* ALTRIMENTI CONTROLLO CHE CI SIANO I DATI PAGAMENTO IN
055400990616     C* CSB (NO PAGAMENTO INFRAGRUPPO)
055500010308     C*F                   ELSE
055600971218     C* SE IL RAMO DELLA LNP E' UGUALE A QUELLO IN CUI STO
055700971218     C* LAVORANDO CONFRONTO LIMITE PAGAMENTO
055800011105     C     CSBDDP        CABGT     APAG          NOPAG                90
055900010308     C*F                   END
056000010308     C*F                   END
056100971218     C*
056200971218     C                   MOVEL     'S'           SELPAG            1
056300971218     C*
056400971218     C     NOPAG         ENDSR
056500971218     C*-------------------------------------------------*
056600971218     C* PAGANT: OK C/ASS.ANTICIPATO (PAGATO E NON INC.)
056700971218     C*-------------------------------------------------*
056800971218     C     PAGANT        BEGSR
056900971218     C*
057000971218     C                   MOVEL     'N'           SELANT
057100971218     C* SE IL RAMO DELLA LNP E' UGUALE A QUELLO IN CUI STO
057200971218     C* LAVORANDO CONTROLLO CHE CSBDDP > LIMITE IMPOSTO
057300971218     C*              (DATA PAG.C/ASS.)
057400010308     C*F         D96RMP    IFEQ D96RSI
057500011105     C     CSBDDP        CABGT     APAG          NOANT                90
057600010308     C*F                   ELSE
057700990616     C* SE IL RAMO DI LNP E' DIVERSO CONTROLLO CHE CI SIA
057800990616     C* TNCSG e che la data sia inferiore al limite imposto
057900010308     C*F         D96TRV    IFNE 'S'
058000010308     C*F                   GOTO NOANT
058100010308     C*F                   ELSE
058200010308     C*F         DATPAG    CABLTDAPAG     NOANT
058300010308     C*F         DATPAG    CABGTAPAG      NOANT      91
058400010308     C*F                   END
058500010308     C*F                   END
058600990616     C*
058700990616     C* SE IL RAMO DELLA LNA E' UGUALE A QUELLO IN CUI STO
058800990616     C* LAVORANDO CONTROLLO CHE LA DATA INCASSO = 0
058900010308     C*F         D96RLA    IFEQ D96RSI
059000010308     C     CSBDDC        CABNE     *ZEROS        NOANT
059100010308     C*F                   ELSE
059200990616     C* SE IL RAMO DI LNA E' DIVERSO CONTROLLO CHE NON CI
059300990616     C* SIA TNCSG
059400010308     C*F         D96TRV    IFEQ 'S'
059500010308     C*F                   GOTO NOANT
059600010308     C*F                   END
059700010308     C*F                   END
059800971218     C*
059900971218     C                   MOVEL     'S'           SELANT            1
060000971218     C*
060100971218     C     NOANT         ENDSR
060200971218     C*-------------------------------------------------*
060300971218     C* PRZDAT: PARZIALIZZAZIONE PER DATE
060400971218     C*-------------------------------------------------*
060500971218     C     PRZDAT        BEGSR
060600971218     C*
060700971218     C                   MOVEL     'N'           SELDAT
060800971218     C* PARZIALIZZAZ. X DATA SPEDIZ.
060900971218     C     DASPE         IFNE      *ZEROS
061000971218     C                   MOVEL     CSBAAS        DSP8              8 0
061100971218     C                   MOVE      CSBMGS        DSP8
061200971218     C     DSP8          CABLT     DASPE         NODAT
061300971218     C     DSP8          CABGT     ASPE          NODAT
061400971218     C                   END
061500971218     C*-------------------------------------------------*
061600971218     C* PARZIALIZZAZ. X DATA INCASSO:
061700971218     C     DAINC         IFNE      *ZEROS
061800971218     C* SE LA LNA E' UGUALE A QUELLA DEL MIO RAMO CONFRONTO
061900971218     C* I LIMITI DATI CON LA DATA CONFERMA INCASSSO IN SEDE
062000010308     C*F         D96RLA    IFEQ D96RSI
062100971218     C     CSBDTA        CABLT     DAINC         NODAT
062200971218     C     CSBDTA        CABGT     AINC          NODAT
062300971218     C* ALTRIMENTI CONFRONTO I LIMITI DATI CON LA DATA VALUTA
062400971218     C* PAGAMENTI INFRAGRUPPO
062500010308     C*F                   ELSE
062600010308     C*F         DATINC    CABLTDAINC     NODAT
062700010308     C*F         DATINC    CABGTAINC      NODAT
062800010308     C*F                   END
062900971218     C                   END
063000971218     C*
063100971218     C                   MOVEL     'S'           SELDAT            1
063200971218     C*
063300971218     C     NODAT         ENDSR
063400971218     C*-------------------------------------------------*
063500971218     C* WRTCSB: SCRITTURA FILE TNCSBWRK
063600971218     C*-------------------------------------------------*
063700971218     C     WRTCSB        BEGSR
063800971218     C*
063900971218     C* DECODIFICA RAGIONE SOCIALE
064000971218     C   04
064100971218     COR 02
064200971218     COR 03              DO
064300971218     C* SE LA LNP NON E' DEL MIO RAMO IMPOSTO IL CODICE INFRAGR.
064400971218     C* PER LA DECODIFICA
064500010308     C*F         D96RMP    IFNE D96RSI
064600010308     C*F         *IN04     ANDNE'1'
064700010308     C*F         *IN03     ANDNE'3'
064800010308     C*F         D96TRV    OREQ 'S'
064900010308     C*F         D96RLA    ANDEQD96RSI
065000010308     C*F         *IN03     ANDEQ'1'
065100010308     C*F                   Z-ADD1         X       40
065200010308     C*F                   MOVE D96KSC    CSBKSC
065300010308     C*F                   END
065400971218     C*
065500971218     C                   MOVE      CSBKSC        CODI              4 0
065600971218     C     CODI          IFEQ      8888
065700971218     C     CODI          OREQ      9999
065800971218     C     KSPE          CHAIN     TNCSA01L                           50
065900971218     C  N50              MOVEL     CSARSD        RAGIO
066000971218     C   50              MOVEL     *BLANKS       RAGIO
066100971218     C                   ELSE
066200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
066300980127     C                   CLEAR                   TIBS69
066400980127     C                   CLEAR                   CNACO
066500980127     C                   CLEAR                   CNIND
066600980127     C                   CLEAR                   CNCLP
066700980127     C                   CLEAR                   FNCLS
066800980127     C                   MOVEL     ' '           I69TLA
066900980127     C                   MOVEL     KNSIF         I69SIF
067000980127     C                   Z-ADD     CSBCDI        I69KAC
067100980127     C                   CALL      'TIBS69R'
067200980127     C                   PARM                    TIBS69
067300980127     C                   PARM                    CNACO
067400980127     C                   PARM                    CNIND
067500980127     C                   PARM                    CNCLP
067600980127     C                   PARM                    FNCLS
067700980127     C     O69ERR        IFEQ      *BLANK
067800980127     C                   MOVEL     ACORAG        RAGIO
067900980127     C                   END
068000971218     C                   END
068100971218     C                   Z-ADD     CSBNSP        NSP
068200971218     C                   END
068300971218     C*
068400971218     C     VORD          IFEQ      'P'
068500971218     C                   Z-ADD     1             §C                4 0
068600971218     C                   MOVE      CSBLNP        §FIL              3
068700971218     C     §FIL          LOOKUP    §L3(§C)                                22
068800971218     C* ESCLUSIONE FIL. MECC. IN ARRIVO
068900971218     C   22              MOVE      §DI(§C)       CSBDIV
069000971218     C   22              MOVE      §CA(§C)       CSBCAR
069100971218     C   22              MOVE      L3(§C)        CSBKE1
069200971218     C                   MOVE      CSBLNP        CSBKE2
069300971218     C                   MOVE      CSBLNA        CSBKE3
069400971218     C  N03
069500971218     CANN04
069600971218     CANN02              MOVEL     DTNSP         CSBKE4
069700971218     C   02              MOVEL     DTRAG         CSBKE4
069800971218     C   02              MOVEL     *ZEROS        CSBKE3
069900971218     C   03              MOVEL     DTRAG         CSBKE4
070000971218     C   03              MOVEL     *ZEROS        CSBKE3
070100971218     C   04              MOVEL     DTRAG         CSBKE4
070200971218     C   04              MOVEL     *ZEROS        CSBKE3
070300971218     C                   ELSE
070400971218     C*                    ----
070500971218     C                   Z-ADD     1             §C
070600971218     C                   MOVE      CSBLNA        §FIL
070700971218     C     §FIL          LOOKUP    §L3(§C)                                22
070800971218     C   22              MOVE      §DI(§C)       CSBDIV
070900971218     C   22              MOVE      §CA(§C)       CSBCAR
071000971218     C   22              MOVE      L3(§C)        CSBKE1
071100971218     C                   MOVE      CSBLNA        CSBKE2
071200971218     C                   MOVE      *ZEROS        CSBKE3
071300971218     C  N03
071400971218     CANN02              MOVEL     DTNSP         CSBKE4
071500971218     C   03              MOVEL     DTRAG         CSBKE4
071600971218     C   02              MOVEL     DTRAG         CSBKE4
071700971218     C                   END
071800971218     C*
071900971218     C* RILEVO DAL FILE DELLE BOLLE IL COD.CONSEGNA ANOMALA
072000990728     C     KTAS          CHAIN     TITAS30C                           99
072100971218     C  N99              MOVEL     TASCCA        CSBCCA
072200971218     C   99              CLEAR                   CSBCCA
072300971218     C* se lnp non è del mio ramo assumo come data pagamento
072400971218     C* la data distinta infragruppo
072500010308     C*F         D96RMP    IFNE D96RSI
072600010308     C*F         *IN01     ANDEQ'1'
072700010308     C*F         D96TRV    ANDEQ'S'
072800010308     C*F         D96TRV    OREQ 'S'
072900010308     C*F         D96RLA    ANDEQD96RSI
073000010308     C*F         *IN03     ANDEQ'1'
073100010308     C*F                   MOVE WSVPAG    CSBPAG
073200010308     C*F                   END
073300971218     C* se lna non è del mio ramo assumo come data incasso
073400971218     C* la data distinta infragruppo
073500010308     C*F         D96RSI    IFNE D96RLA
073600010308     C*F         D96TRV    ANDEQ'S'
073700010308     C*F                   MOVE WSVINC    CSBINC
073800010308     C*F                   END
073900971218     C*
074000991025     C   04              MOVEL     *BLANKS       CSBFUS
074100971218     C                   WRITE     BLCSWRK
074200971218     C*
074300971218     C                   ENDSR
074400971217     C*-------------------------------------------------*
074500971217     C* *INZSR: OPERAZIONI INIZIALI
074600971217     C*-------------------------------------------------*
074700971217     C     *INZSR        BEGSR
074800971217     C*
074900971217     C     *ENTRY        PLIST
075000971217     C                   PARM                    KPJBA
075100971217     C                   MOVEL     KPJBU         PARAM
075200971217     C*-------------------------------------------------*
075300971217     C* TNCSA01L                                           *
075400971217     C     KSPE          KLIST
075500971217     C                   KFLD                    CSBAAS
075600971217     C                   KFLD                    CSBLNP
075700971217     C                   KFLD                    CSBNRS
075800971217     C                   KFLD                    CSBNSP
075900990728     C* TITAS30C
076000971217     C     KTAS          KLIST
076100971217     C                   KFLD                    CSBAAS
076200971217     C                   KFLD                    CSBLNP
076300971217     C                   KFLD                    CSBNRS
076400971217     C                   KFLD                    CSBNSP
076500971217     C                   KFLD                    CSBTBL
076600971218     C* TNCSB03L                                           *
076700971218     C     KCSB          KLIST
076800971218     C                   KFLD                    KAAS
076900971218     C                   KFLD                    KLNP
077000971218     C                   KFLD                    KNRS
077100971218     C                   KFLD                    KNSP
077200971218     C* TNCSG01L                                           *
077300971218     C     KCSG          KLIST
077400971218     C                   KFLD                    KAAS
077500971218     C                   KFLD                    KLNP
077600971218     C                   KFLD                    KNRS
077700971218     C                   KFLD                    KNSP
077800971217     C*-------------------------------------------------*
077900971217     C     *LIKE         DEFINE    CSBBNA        KBNA
078000971217     C     *LIKE         DEFINE    CSBDDP        KBN6
078100971218     C     *LIKE         DEFINE    CSBAAS        KAAS
078200971218     C     *LIKE         DEFINE    CSBLNP        KLNP
078300971218     C     *LIKE         DEFINE    CSBNRS        KNRS
078400971218     C     *LIKE         DEFINE    CSBNSP        KNSP
078500010315     C*F         *LIKE     DEFN CSGDDT    KDDT             *
078600971217     C*-------------------------------------------------*
078700971217     C*  GUARDO SE LA FIL.è MECCANIZZATA CON GLI ARRIVI
078800971219     C                   Z-ADD     0             §C
078900010220     C****                 OPEN AZORG01L
079000971217     C     *IN21         DOUEQ     '1'
079100971217     C                   READ      AZORG01L                               21
079200971217     C  N21ORGFVA        IFEQ      *BLANK
079300971217     C     VORD          ANDEQ     'P'
079400971217     C     VORD          OREQ      'A'
079500971219     C                   ADD       1             §C
079600971217     C                   MOVE      ORGFIL        §L3(§C)           3
079700971217     C                   MOVEL     ORGFL3        §DI(§C)
079800971217     C                   MOVEL     ORGCAR        §CA(§C)
079900971217     C* AGGANCIO FILIALE PRIMO LIVELLO RELATIVA
080000971218     C                   CLEAR                   FNLV55
080100971217     C                   MOVEL     *BLANKS       D55TLA
080200971217     C                   MOVEL     'P'           D55TPT
080300971217     C                   MOVEL     ORGFIL        D55LIN
080400971217     C                   MOVEL     ORGFIL        D55LNP
080500971217     C                   CALL      'FNLV55R'
080600971217     C                   PARM                    FNLV55
080700971217     C                   MOVE      D55TFP        L3(§C)
080800971217     C                   END
080900971217     C                   END
081000010220      *****                CLOSEAZORG01L
081100971217     C*-------------------------------------------------*
081200971217     C                   ENDSR
