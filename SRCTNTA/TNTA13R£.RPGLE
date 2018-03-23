000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200950805     F* TNTA13R *-----------------------------------------------------*
000300950805     F*                 STAMPA TARIFFE GIACENZA                       *
000400921201     F*---------------------------------------------------------------*
000500950805     FCNACO04L  IF   E           K DISK
000600040920     FFNCLS01L  IF   E           K DISK
000700990714     FTITGC01L  IF   E           K DISK
000800991019     FTNTAM01L  IF   E           K DISK
000900991019     FTABEL00F  IF   E           K DISK
001000950805     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001100041012
001200950805     D SK              S             66    DIM(13) CTDATA PERRCD(1)
001300041012
001400041012      * P.O. gestibili dall'utente
001500041012     d fig             s              3    dim(250) inz(*Zeros)
001600041011
001700991019     D DSTA01        E DS
001800991019     D DSCV          E DS
001900950805     D KPJBA         E DS
002000040920     d tnta23ds      e ds
002100041012     d Trul31ds      e ds
002200041011
002300950805     C*****************************************************************
002400950805     C* RIEPILOGO INDICATORI
002500950805     C*****************************************************************
002600950805     C* 15    - SI TRATTA DI TARIFFA CHE PUO' ESSERE GESTITA DALLA
002700950805     C*         FILIALE COLLEGATA
002800950805     C* 30/31 - DI COMODO
002900950805     C*****************************************************************
003000950805     C*
003100950805     C     KACO          SETLL     CNACO000
003200950805     C     KACO          READE     CNACO000                               30
003300950805     C*
003400950805    1C     *IN30         DOWEQ     *OFF
003500950805     C*
003600950805     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DEL
003700950805     C*   CODICE CLIENTE CHE STO ELABORANDO
003800041011     C                   MOVEL     ACOKSC        W0030             3
003900950805     C     W0030         LOOKUP    FIG                                    15
004000950805     C*
004100950805    2C     *IN15         IFEQ      *ON
004200950805     C*
004300990714     C     ACOKSC        SETLL     TITGC000
004400990714     C     ACOKSC        READE     TITGC000                               31
004500950805     C*
004600950805    3C     *IN31         DOWEQ     *OFF
004700970729     C* ESCLUDO LE ANNULLATE
004800970729     C     TGCATB        IFEQ      ' '
004900040920      *
005000040920     C                   SETOFF                                       1011
005100040920     C* VERIFICO I TIPI COMINUCAZIONI SE QUELLI SELEZIONATI
005200040920     C                   IF        TGCTCM = ' ' OR TGCTFG = ' '
005300040920      * AGGANCIO  ANAGRAFICA AGGIUNTIVA
005400040920     C     ACOKSC        CHAIN     FNCLS01L
005500040920     C                   IF        %FOUND(FNCLS01L)
005600040920     C                   IF        TGCTCM = ' '
005700041022     C                   MOVE      CLSFLO        TGCTCM
005800040920     C                   SETON                                        10
005900040920     C                   ENDIF
006000950805     C*
006100040920     C                   IF        TGCTFG = ' '
006200041022     C                   MOVEl     CLSFLO        TGCTFG
006300040920     C                   SETON                                        11
006400040920     C                   ENDIF
006500040920     C                   ENDIF
006600040920      *
006700040920     C                   ENDIF
006800040920      * VERIFICO LE SELEZIONI FATTE  A VIDEO
006900040920     C                   IF        D23TCM <> ' ' AND TGCTCM <> D23TCM
007000040920     C                   GOTO      LEGGI
007100040920     C                   ENDIF
007200040920      *
007300040920     C                   IF        D23TFG <> ' ' AND TGCTFG <> D23TFG
007400040920     C                   GOTO      LEGGI
007500040920     C                   ENDIF
007600040920      *
007700950805     C* STAMPA TETSTA
007800950805     C     *INOF         IFEQ      *ON
007900950805     C                   EXCEPT    TES
008000040920     C                   EXCEPT    TES1
008100950805     C                   SETOFF                                       OF
008200950805     C                   ENDIF
008300991019     C*
008400991019     C* RECUPERO LA DIVISA DELLA TARIFFA DALLA TESTATA TAROFFA
008500991019     C     KTAM          CHAIN     TNTAM01L                           32
008600991019     C*
008700991019     C     *IN32         IFEQ      *OFF
008800991019     C                   MOVEL     TAMFLO        DSTA01
008900991019     C* VERIFICO LA DIVISA SE HA DECIMALI OPPURE NO
009000991019     C* CONTROLLO DELLA DIVISA
009100991019     C                   MOVEL     'CV'          COD               2
009200991019     C                   MOVEL     *BLANKS       KEY               8
009300991019     C                   MOVEL     §TADIV        KEY
009400991019     C     KTAB          CHAIN     TABEL                              32
009500991019     C   32              MOVEL     *BLANKS       TBLUNI
009600991019     C                   MOVEL     TBLUNI        DSCV
009700991019     C     §CVFDC        COMP      'S'                                    95
009800991019     C* PREPARO IL CAMPO DI STAMPA
009900991019     C                   MOVEL     §TADIV        WDIV              3
010000950805     C*
010100950805     C* STAMPA DETTAGLIO
010200990714     C* ADEGUO GLI IMPORTI ALLE LUNGHEZZE DI STAMPA
010300991019     C     *IN95         IFEQ      *OFF
010400990714     C                   Z-ADD     TGCSGV        STPSGV            7 0
010500990714     C                   Z-ADD     TGCSGR        STPSGR            7 0
010600990714     C                   Z-ADD     TGCSGP        STPSGP            7 0
010700990714     C                   Z-ADD     TGCSGD        STPSGD            7 0
010800990714     C                   Z-ADD     TGCST1        STPST1            7 0
010900990714     C                   Z-ADD     TGCST2        STPST2            7 0
011000990714     C                   Z-ADD     TGCST3        STPST3            7 0
011100990714     C                   Z-ADD     TGCSTM        STPSTM            7 0
011200991019     C                   ELSE
011300991019     C                   Z-ADD     TGCSGV        STDSGV            7 3
011400991019     C                   Z-ADD     TGCSGR        STDSGR            7 3
011500991019     C                   Z-ADD     TGCSGP        STDSGP            7 3
011600991019     C                   Z-ADD     TGCSGD        STDSGD            7 3
011700991019     C                   Z-ADD     TGCST1        STDST1            7 3
011800991019     C                   Z-ADD     TGCST2        STDST2            7 3
011900991019     C                   Z-ADD     TGCST3        STDST3            7 3
012000991019     C                   Z-ADD     TGCSTM        STDSTM            7 3
012100991019     C                   ENDIF
012200991019     C                   ENDIF
012300990714     C*
012400950805     C                   EXCEPT    DET
012500970729     C                   ENDIF
012600950805     C*
012700040920     C     LEGGI         TAG
012800990714     C     ACOKSC        READE     TITGC000                               31
012900950805    3C                   ENDDO
013000950805    2C                   ENDIF
013100950805     C*
013200950805     C     KACO          READE     CNACO000                               30
013300950805    1C                   ENDDO
013400950805     C*
013500950805     C                   SETON                                        LR
013600950805     C*
013700950805     C*--- INZSR INIZIALE --------------------------------------------*
013800950805     C     *INZSR        BEGSR
013900950805     C*
014000950805     C     *ENTRY        PLIST
014100950805     C                   PARM                    KPJBA
014200041012     c                   Movel     kpjbu         Tnta23ds
014300040920      *
014400991019     C* CHIAVE DI TNTAM
014500991019     C     KTAM          KLIST
014600991019     C                   KFLD                    TGCKSC
014700991019     C                   KFLD                    TGCCTR
014800991019     C                   KFLD                    TGCPRG
014900991019     C* CHIAVE DI TABEL
015000991019     C*
015100991019     C     KTAB          KLIST
015200991019     C                   KFLD                    CODUT
015300991019     C                   KFLD                    COD
015400991019     C                   KFLD                    KEY
015500991019     C*
015600991019     C                   Z-ADD     1             CODUT             1 0
015700950805     C*
015800950805     C* LA DATA DEL GIORNO LA PRENDO DA TIME
015900950805     C                   TIME                    W0140            14 0
016000950805     C                   MOVE      W0140         UDATE8            8 0
016100950805     C                   MOVEL     W0140         UTIME             6 0
016200950805     C*
016300950805     C                   SETON                                        OF
016400041012
016500041012      * se è stato richiesto un p.o. solo imposto il p.o. richiesto nella sk
016600041012      * fig
016700041012     c                   If        d23fil > *Zeros
016800041012     c                   Move      d23fil        fig(1)
016900041012     c                   else
017000041012      * Reperimento dei P.O. gestibili dall'utente
017100041012     c                   clear                   TRUL31DS
017200041012     c                   eval      I31abi = d23abi
017300041012     c                   eval      I31cdi = d23cdi
017400041012     c                   eval      I31car = d23car
017500041012     c                   eval      I31cpo = d23cpo
017600041012     c                   call      'TRUL31R'
017700041012     c                   parm                    KPJBA
017800041012     c                   parm                    TRUL31DS
017900041012     c                   if        O31pog > *zeros
018000041012     c                   movea     O31pog        fig
018100041012     c                   endif
018200041012     c                   endif
018300040920      *
018400040920      * stampo le parazializzazioni fatte a video
018500040920      *
018600040920     c                   Except    tes
018700040920     c                   Except    tes0
018800950805     C*
018900950805     C****  KLIST  ****
019000950805     C* ACCESSO CNACO04L
019100950805     C     KACO          KLIST
019200950805     C                   KFLD                    CODUT
019300041011     C                   KFLD                    d23KCI
019400950805     C*
019500950805     C                   ENDSR
019600950805     C*
019700950805     C*---------------------------------------------------------------*
019800950805     OPRTF198   E            TES              02
019900950805     O                       SK(1)              132
020000041011     O                       d23rsu              20
020100950805     O                                           21 '/'
020200041011     O                       d23fel              24
020300950805     O                                          170 'TNTA13R'
020400950805     O                       UDATE8             183 '  /  /    '
020500950805     O                                          193 'PAG.'
020600950805     O                       PAGE          Z    198
020700950805     O          E            TES         1
020800950805     O                       KNSIF               10
020900950805     O                       KNMUS               21
021000950805     O                       UTIME              181 '  :  :  '
021100040921     O          E            TES0       10
021200040921     O                                        +   2 '--------------  Selezioni '
021300040921     O                                              '--------------'
021400040921     O          E            TES0        3
021500040920     O                                        +   2 'Punto Operativo'
021600041012     O                       d23fil        z  +  24
021700041012     O                       d23dpo           +   3
021800040920     O          E            TES0        3
021900040920     O                                        +   2 'Tipo Comunicazione al'
022000040921     O                                              ' Mittente'
022100040921     O                       D23TCM           +  11
022200040920     O                       D23DCM           +   3
022300040920     O          E            TES0        3
022400040920     O                                        +   2 'Tipo Comunicazione di'
022500040921     O                                              ' Fine Giacenza'
022600040921     O                       D23TFG           +   6
022700040920     O                       D23DFG           +   3
022800040920     O          E            TES1        2
022900950805     O                       SK(2)               66
023000950805     O                       SK(6)              132
023100950805     O                       SK(10)             198
023200040920     O          E            TES1        1
023300950805     O                       SK(3)               66
023400950805     O                       SK(7)              132
023500950805     O                       SK(11)             198
023600040920     O          E            TES1        1  1
023700950805     O                       SK(4)               66
023800950805     O                       SK(8)              132
023900950805     O                       SK(12)             198
024000950805     O* DETTAGLIO
024100950805     O          E            DET         1
024200950805     O                       ACORAG              48
024300991019     O                       ACOKSC        Z     56
024400991019     O                       TGCCTR              60
024500991019     O                       TGCPRG        Z     64
024600991019     O                       WDIV                68
024700991019     O              N95      STPSGV        Z     76
024800991019     O              N95      STPSGR        Z     85
024900991019     O              N95      STPSGP        Z     94
025000991019     O              N95      STPSGD        Z    103
025100991019     O               95      STDSGV        4     76
025200991019     O               95      STDSGR        4     85
025300991019     O               95      STDSGP        4     94
025400991019     O               95      STDSGD        4    103
025500950805     O                       TGCSG1        Z    108
025600950805     O                       TGCSG2        Z    115
025700950805     O                       TGCSG3        Z    122
025800991019     O              N95      STPST1        Z    133
025900991019     O              N95      STPST2        Z    142
026000991019     O              N95      STPST3        Z    151
026100991019     O               95      STDST1        4    133
026200991019     O               95      STDST2        4    142
026300991019     O               95      STDST3        4    151
026400950805     O                       TGCSGF        Z    156
026500950805     O                       TGCFAF             162
026600950805     O                       TGCGGR        Z    168
026700991019     O              N95      STPSTM        Z    178
026800991019     O               95      STDSTM        4    178
026900950805     O                       TGCRPV        4    184
027000040920     O               10                         187 '('
027100950805     O                       TGCTCM             188
027200040920     O               10                         189 ')'
027300040920     O               11                         192 '('
027400950805     O                       TGCTFG             193
027500040920     O               11                         194 ')'
027600950805     O*
027700950805     O*---------------------------------------------------------------*
027800970729**
027900950805     ****    T A R I F F E    D I    G I A C E N Z A    ****         1
028000991019RAGIONE SOCIALE                                  CODICE  COD PRG D   2
028100991019                                                 CLIENTE TAR TAR T   3
028200950805                                                                     4
028300991019XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXX XXX XXX X   5
028400991019IV   SPESE  SPESE    SPESE    SPESE    GG.    GG.    GG.      SPES   6
028500991019AR   VARIE  RICONS.  RICONS.  DOSSIER  SOSTA  SOSTA  SOSTA    SOST   7
028600950805            CITTA'   PROVINC.          1°sc.  2°sc.  3°sc.    1°sc   8
028700991019XX XXXXXXX  XXXXXXX  XXXXXXX  XXXXXXX  XXX    XXX    XXX    XXXXXX   9
028800970729E    SPESE    SPESE  FRANCHIGIA   GG.   SPESE   RAP.  T.C. T.C.     10
028900950805A    SOSTA    SOSTA  GG.   FLAG  RIEN.  SOSTA   PESO  APE. FINE     11
029000950805.    2°sc.    3°sc.        APPL.        MINIMO  VOL.  MIT. GIAC.    12
029100970729X  XXXXXXX  XXXXXXX  XXX     X    XX   XXXXXXX  XX,X   X    X       13
