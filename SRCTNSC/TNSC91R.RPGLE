000100910802     H DECEDIT('0,') DATEDIT(*DMY.)
000200000223     H*--------------------------------------------------------------*
000300000223     H* ATTENZIONE !!!                                               *
000400000223     H*--------------------------------------------------------------*
000500941102     H* TNSC91R  *---------------------------------------------------*
000600910320     H*----------*                                                   *
000700910902     H*           - CREAZIONE FILE DI WORK                           *
000800910320     H*--------------------------------------------------------------*
000900010220     FTNCSB04L  IF   E           K DISK    USROPN
001000971218     F                                     RENAME(TNCSB000:TNCSBSPE)
001100010220     FTNCSA01L  IF   E           K DISK    USROPN
001200120216     FAZORG01L  IF   E           K DISK
001300990728     FTITAS30C  IF   E           K DISK
001400941102     FTNCSBWRK  O    E             DISK
001500971217     D*--------------------------------------------------------------*
001600910904     D FIL             S              3    DIM(29)
001700910904     D L3              S              3    DIM(999)
001800910904     D §L3             S              3    DIM(999)
001900910909     D §DI             S              1  0 DIM(999)
002000910909     D §CA             S              3  0 DIM(999)
002100971217     D*--------------------------------------------------------------*
002200910802     D KPJBA         E DS
002300910902     D PARAM           DS
002400910902     D* RICH. C/A SCOPERTI
002500910902     D  VSEL1                  1      1
002600910902     D* RICH. C/A INCASSATI E NON PAGATI
002700910902     D  VSEL2                  2      2
002800911003     D* RICH. C/A PAGATI
002900911003     D  VSEL3                  3      3
003000911003     D  VORD                   4      4
003100911003     D  VDET                   5      6
003200941102     D  DASPE                  7     14  0
003300941102     D  ASPE                  15     22  0
003400941102     D  DAINC                 23     30  0
003500941102     D  AINC                  31     38  0
003600941103     D  LIBERO                39     40
003700941102     D  DAPAG                 41     48  0
003800941102     D  APAG                  49     56  0
003900941102     D  VDAFIL                57     59  0
004000941102     D  VAFIL                 60     62  0
004100960909     D* RICH. C/A PAGATI E NON INCASSATI
004200960909     D  VSEL4                 63     63
004300040309     D* FILE ESTERO
004400040309     D  VFILE1                65     65
004500040309     D* C.A. S/N/SOLO C.A
004600040913     D  Vscop1                66     66
004700040913     D* FILE italia
004800040913     D  VFILi1                67     67
004900050711     D  vclida                68     74  0
005000050711     D  vclia                 75     81  0
005100910902     D*
005200910902     D* KEY4 DATA + NR.SPEDIZ.
005300910902     D DTNSP           DS
005400941102     D  CSBAAS                 1      3P 0
005500941102     D  CSBMGS                 4      6P 0
005600941102     D  CSBNRS                 7      8P 0
005700941102     D  CSBNSP                 9     12P 0
005800911009     D DTRAG           DS
005900911009     D  RAGIO                  1     13
006000911009     D  NSP                   14     20  0
006100971218     D FNLV55        E DS                  EXTNAME(FNLV55DS)
006200980127     D TIBS69        E DS                  EXTNAME(TIBS69DS)
006300980127     D CNACO         E DS                  EXTNAME(CNACO00F)
006400980127     D CNIND         E DS                  EXTNAME(CNIND00F)
006500980127     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006600980127     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006700990611     D*  Ds per ricezione dati di incasso
006800990611     D WSVINC          DS            75
006900990617     D  TPI                   30     30
007000990611     D  DATINC                47     54  0
007100990611     D*  Ds per ricezione dati di incasso
007200990611     D WSVPAG          DS            46
007300990611     D  DATPAG                16     23  0
007400910321     C***------------------------------------------***
007500971218     C                   EXSR      SETCSB
007600910902     C*
007700910902     C     *IN90         DOUEQ     '1'
007800960909     C  N03
007900960909     CANN02KBN6          READE     TNCSB04L                               90
008000950623     C   03              READ      TNCSB04L                               90
008100960909     C   02              READ      TNCSB04L                               90
008200910902     C*
008300910902     C  N90              DO
008400971218     C*-------------------------------------------------*
008500971218     C* Controllo stato C/Assegno per estrazione
008600971218     C                   EXSR      CTRSTA
008700971218     C     SELSTA        CABEQ     'N'           NOWRT
008800971217     C*-------------------------------------------------*
008900010308     C* SCOPERTI X LNA:
009000971217     C     *IN04         IFEQ      '1'
009100971217     C     VORD          ANDEQ     'A'
009200971218     C                   EXSR      SCOPOK
009300971218     C     SELSCO        CABEQ     'N'           NOWRT
009400971217     C                   END
009500971219     C* SCOPERTI X LNP:
009600971219     C     *IN04         IFEQ      '1'
009700971219     C     VORD          ANDEQ     'P'
009800971219     C                   EXSR      NOPGOK
009900971219     C     SELSCO        CABEQ     'N'           NOWRT
010000971219     C                   END
010100971217     C*-------------------------------------------------*
010200971219     C* INCASSATI DA PAGARE
010300971217     C     *IN01         IFEQ      '1'
010400971218     C                   EXSR      INCAOK
010500971218     C     SELINC        CABEQ     'N'           NOWRT
010600971217     C                   END
010700971217     C*-------------------------------------------------*
010800971218     C* PAGATI
010900971217     C     *IN03         IFEQ      '1'
011000971218     C                   EXSR      PAGAOK
011100971218     C     SELPAG        CABEQ     'N'           NOWRT
011200971217     C                   END
011300971217     C*-------------------------------------------------*
011400971217     C* PAGATI NON INCASSATI
011500971218     C     *IN02         IFEQ      '1'
011600971218     C                   EXSR      PAGANT
011700971218     C     SELANT        CABEQ     'N'           NOWRT
011800971218     C                   END
011900971217     C*-------------------------------------------------*
012000971218     C* PARZIALIZZAZIONI SU DATE
012100971218     C                   EXSR      PRZDAT
012200971218     C     SELDAT        CABEQ     'N'           NOWRT
012300971217     C*-------------------------------------------------*
012400971218     C* SCRIVO DATI SU FILE DI WORK
012500971218     C                   EXSR      WRTCSB
012600910902     C                   END
012700971217     C*
012800971218     C     NOWRT         TAG
012900910902     C                   END
013000971218     C*-------------------------------------------------*
013100971218     C     *IN02         IFEQ      '1'
013200971218     C     *IN03         OREQ      '1'
013300971218     C*-------------------------------------------------*
013400971218     C* Controllo stato C/Assegno per estrazione
013500971218     C                   EXSR      CTRSTA
013600971218     C     SELSTA        CABEQ     'N'           NOWRTG
013700971218     C* Pagati
013800971218     C     *IN03         IFEQ      '1'
013900971218     C                   EXSR      PAGAOK
014000971218     C     SELPAG        CABEQ     'N'           NOWRTG
014100971218     C                   END
014200971218     C*-------------------------------------------------*
014300971218     C* Pagati non incassati
014400971218     C     *IN02         IFEQ      '1'
014500971218     C                   EXSR      PAGANT
014600971218     C     SELANT        CABEQ     'N'           NOWRTG
014700971218     C                   END
014800971218     C*-------------------------------------------------*
014900971218     C* Parzializzazione su date
015000971218     C                   EXSR      PRZDAT
015100971218     C     SELDAT        CABEQ     'N'           NOWRT
015200971218     C*-------------------------------------------------*
015300971218     C* Scrivo dati su file di work
015400971218     C                   EXSR      WRTCSB
015500971218     C*
015600971218     C     NOWRTG        TAG
015700971218     C                   END
015800971218     C* RICHIAMO PGM X CHIUSURA IN LR
015900971218     C                   CLEAR                   FNLV55
016000990611     C                   MOVEL     'C'           D55TLA
016100971218     C                   CALL      'FNLV55R'
016200990611     C                   PARM                    FNLV55
016300980127     C*
016400980127     C                   CLEAR                   TIBS69
016500980127     C                   MOVEL     'C'           I69TLA
016600980127     C                   CALL      'TIBS69R'
016700980127     C                   PARM                    TIBS69
016800980127     C                   PARM                    CNACO
016900980127     C                   PARM                    CNIND
017000980127     C                   PARM                    CNCLP
017100980127     C                   PARM                    FNCLS
017200971218     C*
017300990617     C                   MOVEL     PARAM         KPJBU
017400910902     C                   SETON                                        LR
017500971218     C*-------------------------------------------------*
017600971218     C* SETCSB: posizionamento su TNCSB
017700971218     C*-------------------------------------------------*
017800971218     C     SETCSB        BEGSR
017900971218     C*-------------------------------------------------*
018000010220     C                   OPEN      TNCSB04L
018100010220     C                   OPEN      TNCSA01L
018200971218     C*-------------------------------------------------*
018300971218     C* Posizionamento in base all'opzione richiesta
018400971218     C                   Z-ADD     0             KBN6
018500971218     C     VSEL1         IFEQ      'X'
018600971218     C     KBN6          SETLL     TNCSB04L
018700971218     C                   SETON                                        04
018800971218     C                   END
018900971218     C  N04VSEL2         IFEQ      'X'
019000971218     C                   Z-ADD     DAPAG         KBN6
019100971218     C     KBN6          SETLL     TNCSB04L
019200971218     C                   SETON                                        01
019300971218     C                   END
019400971218     C  N04
019500971218     CANN01VSEL3         IFEQ      'X'
019600971218     C                   Z-ADD     DAPAG         KBN6
019700971218     C     KBN6          SETLL     TNCSB04L
019800971218     C                   SETON                                        03
019900971218     C                   END
020000971218     C  N04
020100971218     CANN01
020200971218     CANN03VSEL4         IFEQ      'X'
020300971218     C                   Z-ADD     DAPAG         KBN6
020400971218     C     KBN6          SETLL     TNCSB04L
020500971218     C                   SETON                                        02
020600971218     C                   END
020700971218     C*
020800971218     C                   ENDSR
020900971218     C*-------------------------------------------------*
021000971218     C* CRTSTA: Controllo stato C/Assegno x estrazione
021100971218     C*-------------------------------------------------*
021200971218     C     CTRSTA        BEGSR
021300971218     C*
021400971218     C                   MOVEL     'N'           SELSTA
021500971218     C* ESCLUDO ANNULLATI
021600971218     C     CSBSTA        CABEQ     9             FISTA
021700971218     C* ESCLUDO CONTENZIOSO !!!
021800971218     C     CSBSTA        CABEQ     3             FISTA
021900971218     C* ESCLUDO C/A STORNO PAGAMENTO
022000971218     C     CSBSTA        CABEQ     6             FISTA
022100971218     C* ESCLUDO C/A ERRATO PAGAMENTO
022200971218     C     CSBSTA        CABEQ     7             FISTA
022300971218     C* ESCLUDO C/A OMESSI
022400971218     C     CSBSTA        CABEQ     8             FISTA
022500971218     C*-------------------------------------------------*
022600971218     C* ESCLUDO QUELLI FUORI LIMITE FILIALE
022700971218     C     VORD          IFEQ      'P'
022800971218     C     CSBLNP        CABLT     VDAFIL        FISTA
022900971218     C     CSBLNP        CABGT     VAFIL         FISTA
023000971218     C                   ELSE
023100971218     C     CSBLNA        CABLT     VDAFIL        FISTA
023200971218     C     CSBLNA        CABGT     VAFIL         FISTA
023300971218     C                   END
023400050707     C*-------------------------------------------------*
023500050707     C* ESCLUDO i clienti FUORI LIMITE
023600050707     C                   IF        vclida <> 0 or vclia <> 0
023700050707     C     CSBksc        CABLT     Vclida        FISTA
023800050707     C     CSBksc        CABGT     Vclia         FISTA
023900050707     c                   end
024000971218     C*
024100971218     C                   MOVEL     'S'           SELSTA            1
024200971218     C*
024300971218     C     FISTA         ENDSR
024400971218     C*-------------------------------------------------*
024500971218     C* SCOPOK: OK Estrazione scoperti x lna
024600971218     C*-------------------------------------------------*
024700971218     C     SCOPOK        BEGSR
024800971218     C*
024900971218     C                   MOVEL     'N'           SELSCO
025000971218     C*
025100000223     C* Per quanto riguarda i C/Assegni scoperti non ha
025200000223     C* alcuna importanza l'infragruppo, controllo i dati
025300000223     C* di incasso su TNCSB: la data distinta di incasso
025400000223     C* deve essere uguale a zero  !!!
025500971218     C     CSBDDC        CABNE     *ZEROS        NOSCO
025600971218     C                   MOVEL     'S'           SELSCO            1
025700971218     C*
025800971218     C     NOSCO         ENDSR
025900971219     C*-------------------------------------------------*
026000971219     C* NOPGOK:OK Estrazione SCOPERTI X LNP: NO PAGATI
026100971219     C*-------------------------------------------------*
026200971219     C     NOPGOK        BEGSR
026300971219     C*
026400971219     C                   MOVEL     'N'           SELSCO
026500000223     C* Per quanto riguarda i C/Assegni scoperti x lnp non
026600000223     C* ha alcuna importanza l'infragruppo, controllo i dati
026700000223     C* di incasso/mancato pagamento su TNCSB: il C/Assegno
026800000223     C* deve essere considerato sia che risulti non incassato,
026900000223     C* sia che risulti incassato ma non pagato
027000000223     C*
027100971219     C* SE LA DATA DI INCASSO SUL C/ASSEGNO E' <> DA ZERO
027200971219     C* CONTROLLO CHE NON SIA STATO PAGATO
027300971219     C     CSBDDC        IFNE      0
027400971219     C* SE IL RAMO DELLA LNP E' UGUALE A QUELLO IN CUI STO
027500971219     C* LAVORANDO CONFRONTO LIMITE PAGAMENTO
027600971219     C     CSBDDP        CABNE     0             FINOPG               90
027700971219     C                   END
027800971219     C*
027900971219     C                   MOVEL     'S'           SELSCO            1
028000971219     C*
028100971219     C     FINOPG        ENDSR
028200000419     C*------------------------------------------------*
028300971218     C* INCAOK: OK Estrazione incassati
028400971218     C*-------------------------------------------------*
028500971218     C     INCAOK        BEGSR
028600971218     C*
028700971218     C                   MOVEL     'N'           SELINC
028800971218     C* CONTROLLO SEMPRE CHE LA DATA DISTINTA DI INCASSO
028900971218     C* E LA DATA CONFERMA INCASSO SIANO MAGGIORI DI 0
029000971218     C     CSBDTA        CABEQ     *ZEROS        FIINCA
029100971218     C                   MOVEL     'S'           SELINC            1
029200971218     C*
029300971218     C     FIINCA        ENDSR
029400971218     C*-------------------------------------------------*
029500971218     C* PAGAOK: OK  C/Assegno pagato
029600971218     C*-------------------------------------------------*
029700971218     C     PAGAOK        BEGSR
029800971218     C*
029900971218     C                   MOVEL     'N'           SELPAG
030000010308     C* CONTROLLO SEMPRE CHE SIA IMPOSTATA LA DATA INCASSO
030100971218     C     CSBDDC        CABEQ     *ZEROS        NOPAG
030200971218     C* SE IL RAMO DELLA LNP E' UGUALE A QUELLO IN CUI STO
030300971218     C* LAVORANDO CONFRONTO LIMITE PAGAMENTO
030400011105     C     CSBDDP        CABGT     APAG          NOPAG                90
030500971218     C                   MOVEL     'S'           SELPAG            1
030600971218     C*
030700971218     C     NOPAG         ENDSR
030800971218     C*-------------------------------------------------*
030900971218     C* PAGANT: OK C/ASS.ANTICIPATO (PAGATO E NON INC.)
031000971218     C*-------------------------------------------------*
031100971218     C     PAGANT        BEGSR
031200971218     C*
031300971218     C                   MOVEL     'N'           SELANT
031400011105     C     CSBDDP        CABGT     APAG          NOANT                90
031500010308     C     CSBDDC        CABNE     *ZEROS        NOANT
031600971218     C*
031700971218     C                   MOVEL     'S'           SELANT            1
031800971218     C*
031900971218     C     NOANT         ENDSR
032000971218     C*-------------------------------------------------*
032100971218     C* PRZDAT: PARZIALIZZAZIONE PER DATE
032200971218     C*-------------------------------------------------*
032300971218     C     PRZDAT        BEGSR
032400971218     C*
032500971218     C                   MOVEL     'N'           SELDAT
032600971218     C* PARZIALIZZAZ. X DATA SPEDIZ.
032700971218     C     DASPE         IFNE      *ZEROS
032800971218     C                   MOVEL     CSBAAS        DSP8              8 0
032900971218     C                   MOVE      CSBMGS        DSP8
033000971218     C     DSP8          CABLT     DASPE         NODAT
033100971218     C     DSP8          CABGT     ASPE          NODAT
033200971218     C                   END
033300971218     C*-------------------------------------------------*
033400971218     C* PARZIALIZZAZ. X DATA INCASSO:
033500971218     C     DAINC         IFNE      *ZEROS
033600971218     C* SE LA LNA E' UGUALE A QUELLA DEL MIO RAMO CONFRONTO
033700971218     C* I LIMITI DATI CON LA DATA CONFERMA INCASSSO IN SEDE
033800971218     C     CSBDTA        CABLT     DAINC         NODAT
033900971218     C     CSBDTA        CABGT     AINC          NODAT
034000971218     C                   END
034100971218     C*
034200971218     C                   MOVEL     'S'           SELDAT            1
034300971218     C*
034400971218     C     NODAT         ENDSR
034500971218     C*-------------------------------------------------*
034600971218     C* WRTCSB: SCRITTURA FILE TNCSBWRK
034700971218     C*-------------------------------------------------*
034800971218     C     WRTCSB        BEGSR
034900971218     C*
035000971218     C* DECODIFICA RAGIONE SOCIALE
035100971218     C   04
035200971218     COR 02
035300971218     COR 03              DO
035400971218     C*
035500971218     C                   MOVE      CSBKSC        CODI              4 0
035600971218     C     CODI          IFEQ      8888
035700971218     C     CODI          OREQ      9999
035800971218     C     KSPE          CHAIN     TNCSA01L                           50
035900971218     C  N50              MOVEL     CSARSD        RAGIO
036000971218     C   50              MOVEL     *BLANKS       RAGIO
036100971218     C                   ELSE
036200980127      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
036300980127     C                   CLEAR                   TIBS69
036400980127     C                   CLEAR                   CNACO
036500980127     C                   CLEAR                   CNIND
036600980127     C                   CLEAR                   CNCLP
036700980127     C                   CLEAR                   FNCLS
036800980127     C                   MOVEL     ' '           I69TLA
036900980127     C                   MOVEL     KNSIF         I69SIF
037000980127     C                   Z-ADD     CSBCDI        I69KAC
037100980127     C                   CALL      'TIBS69R'
037200980127     C                   PARM                    TIBS69
037300980127     C                   PARM                    CNACO
037400980127     C                   PARM                    CNIND
037500980127     C                   PARM                    CNCLP
037600980127     C                   PARM                    FNCLS
037700980127     C     O69ERR        IFEQ      *BLANK
037800980127     C                   MOVEL     ACORAG        RAGIO
037900980127     C                   END
038000971218     C                   END
038100971218     C                   Z-ADD     CSBNSP        NSP
038200971218     C                   END
038300971218     C*
038400971218     C     VORD          IFEQ      'P'
038500971218     C                   Z-ADD     1             §C                4 0
038600971218     C                   MOVE      CSBLNP        §FIL              3
038700971218     C     §FIL          LOOKUP    §L3(§C)                                22
038800971218     C* ESCLUSIONE FIL. MECC. IN ARRIVO
038900971218     C   22              MOVE      §DI(§C)       CSBDIV
039000971218     C   22              MOVE      §CA(§C)       CSBCAR
039100971218     C   22              MOVE      L3(§C)        CSBKE1
039200971218     C                   MOVE      CSBLNP        CSBKE2
039300971218     C                   MOVE      CSBLNA        CSBKE3
039400971218     C  N03
039500971218     CANN04
039600971218     CANN02              MOVEL     DTNSP         CSBKE4
039700971218     C   02              MOVEL     DTRAG         CSBKE4
039800971218     C   02              MOVEL     *ZEROS        CSBKE3
039900971218     C   03              MOVEL     DTRAG         CSBKE4
040000971218     C   03              MOVEL     *ZEROS        CSBKE3
040100971218     C   04              MOVEL     DTRAG         CSBKE4
040200971218     C   04              MOVEL     *ZEROS        CSBKE3
040300971218     C                   ELSE
040400971218     C*                    ----
040500971218     C                   Z-ADD     1             §C
040600971218     C                   MOVE      CSBLNA        §FIL
040700971218     C     §FIL          LOOKUP    §L3(§C)                                22
040800971218     C   22              MOVE      §DI(§C)       CSBDIV
040900971218     C   22              MOVE      §CA(§C)       CSBCAR
041000971218     C   22              MOVE      L3(§C)        CSBKE1
041100971218     C                   MOVE      CSBLNA        CSBKE2
041200971218     C                   MOVE      *ZEROS        CSBKE3
041300971218     C  N03
041400971218     CANN02              MOVEL     DTNSP         CSBKE4
041500971218     C   03              MOVEL     DTRAG         CSBKE4
041600971218     C   02              MOVEL     DTRAG         CSBKE4
041700971218     C                   END
041800971218     C*
041900971218     C* RILEVO DAL FILE DELLE BOLLE IL COD.CONSEGNA ANOMALA
042000990728     C     KTAS          CHAIN     TITAS30C                           99
042100971218     C  N99              MOVEL     TASCCA        CSBCCA
042200971218     C   99              CLEAR                   CSBCCA
042300971218     C*
042400991025     C   04              MOVEL     *BLANKS       CSBFUS
042500971218     C                   WRITE     BLCSWRK
042600971218     C*
042700971218     C                   ENDSR
042800971217     C*-------------------------------------------------*
042900971217     C* *INZSR: OPERAZIONI INIZIALI
043000971217     C*-------------------------------------------------*
043100971217     C     *INZSR        BEGSR
043200971217     C*
043300971217     C     *ENTRY        PLIST
043400971217     C                   PARM                    KPJBA
043500971217     C                   MOVEL     KPJBU         PARAM
043600971217     C*-------------------------------------------------*
043700971217     C* TNCSA01L                                           *
043800971217     C     KSPE          KLIST
043900971217     C                   KFLD                    CSBAAS
044000971217     C                   KFLD                    CSBLNP
044100971217     C                   KFLD                    CSBNRS
044200971217     C                   KFLD                    CSBNSP
044300990728     C* TITAS30C
044400971217     C     KTAS          KLIST
044500971217     C                   KFLD                    CSBAAS
044600971217     C                   KFLD                    CSBLNP
044700971217     C                   KFLD                    CSBNRS
044800971217     C                   KFLD                    CSBNSP
044900971217     C                   KFLD                    CSBTBL
045000971218     C* TNCSB03L                                           *
045100971218     C     KCSB          KLIST
045200971218     C                   KFLD                    KAAS
045300971218     C                   KFLD                    KLNP
045400971218     C                   KFLD                    KNRS
045500971218     C                   KFLD                    KNSP
045600971217     C*-------------------------------------------------*
045700971217     C     *LIKE         DEFINE    CSBBNA        KBNA
045800971217     C     *LIKE         DEFINE    CSBDDP        KBN6
045900971218     C     *LIKE         DEFINE    CSBAAS        KAAS
046000971218     C     *LIKE         DEFINE    CSBLNP        KLNP
046100971218     C     *LIKE         DEFINE    CSBNRS        KNRS
046200971218     C     *LIKE         DEFINE    CSBNSP        KNSP
046300971217     C*-------------------------------------------------*
046400971217     C*  GUARDO SE LA FIL.è MECCANIZZATA CON GLI ARRIVI
046500971219     C                   Z-ADD     0             §C
046600971217     C     *IN21         DOUEQ     '1'
046700971217     C                   READ      AZORG01L                               21
046800971217     C  N21ORGFVA        IFEQ      *BLANK
046900971217     C     VORD          ANDEQ     'P'
047000971217     C     VORD          OREQ      'A'
047100971219     C                   ADD       1             §C
047200971217     C                   MOVE      ORGFIL        §L3(§C)           3
047300971217     C                   MOVEL     ORGFL3        §DI(§C)
047400971217     C                   MOVEL     ORGCAR        §CA(§C)
047500971217     C* AGGANCIO FILIALE PRIMO LIVELLO RELATIVA
047600971218     C                   CLEAR                   FNLV55
047700971217     C                   MOVEL     *BLANKS       D55TLA
047800971217     C                   MOVEL     'P'           D55TPT
047900971217     C                   MOVEL     ORGFIL        D55LIN
048000971217     C                   MOVEL     ORGFIL        D55LNP
048100971217     C                   CALL      'FNLV55R'
048200971217     C                   PARM                    FNLV55
048300971217     C                   MOVE      D55TFP        L3(§C)
048400971217     C                   END
048500971217     C                   END
048600971217     C                   ENDSR
