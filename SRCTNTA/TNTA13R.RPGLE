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
001200041029     D SK              S             66    DIM(15) CTDATA PERRCD(1)
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
002600041029     C* 10/11 - condizionano la stampa
002700041029     C* 13/14 - condizionano la stampa
002800950805     C* 15    - SI TRATTA DI TARIFFA CHE PUO' ESSERE GESTITA DALLA
002900950805     C*         FILIALE COLLEGATA
003000950805     C* 30/31 - DI COMODO
003100950805     C*****************************************************************
003200950805     C*
003300950805     C     KACO          SETLL     CNACO000
003400950805     C     KACO          READE     CNACO000                               30
003500950805     C*
003600950805    1C     *IN30         DOWEQ     *OFF
003700950805     C*
003800950805     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DEL
003900950805     C*   CODICE CLIENTE CHE STO ELABORANDO
004000041011     C                   MOVEL     ACOKSC        W0030             3
004100950805     C     W0030         LOOKUP    FIG                                    15
004200950805     C*
004300950805    2C     *IN15         IFEQ      *ON
004400950805     C*
004500990714     C     ACOKSC        SETLL     TITGC000
004600990714     C     ACOKSC        READE     TITGC000                               31
004700950805     C*
004800950805    3C     *IN31         DOWEQ     *OFF
004900970729     C* ESCLUDO LE ANNULLATE
005000970729     C     TGCATB        IFEQ      ' '
005100041029      *
005200041029     c                   clear                   clstcm
005300041029     c                   clear                   clstfg
005400041029      * AGGANCIO  ANAGRAFICA AGGIUNTIVA
005500041029     C     ACOKSC        CHAIN     FNCLS01L
005600041029     C                   IF        %FOUND(FNCLS01L)
005700041029     C                   MOVE      CLSFLO        CLSTCM            1
005800041029     C*
005900041029     C                   MOVEl     CLSFLO        CLSTFG            1
006000040920      *
006100041029     C                   ENDIF
006200041029      *
006300041029      * se richieste le sole differenze  da anagrafiche  aggancio sempre
006400041029      * fncls e recupero i flag
006500041029      *
006600041029      *
006700041029     c                   If        d23dpc = 'S'
006800041029      *
006900041029     c                   if        tgctcm = ' ' and tgctfg = ' '
007000041029     c                   goto      leggi
007100041029     c                   endif
007200041029      *
007300041029      *
007400041029      * verifico se ci sono delle differenze
007500041029      *
007600041029     c                   if        (clstcm = tgctcm  or  tgctcm =  ' ') and
007700041029     c                             (clstfg = tgctfg  or  tgctfg =  ' ')
007800041029     c                   goto      Leggi
007900041029     c                   endif
008000041029
008100041029
008200041029      *
008300041029      * parzializzazione per tipo comunicazione
008400041029      *
008500041029
008600041029     c                   ELSE
008700041029
008800041029
008900041029     C**                 SETOFF                                       1011
009000040920     C* VERIFICO I TIPI COMINUCAZIONI SE QUELLI SELEZIONATI
009100041029     C***                IF        TGCTCM = ' ' OR TGCTFG = ' '
009200040920      * AGGANCIO  ANAGRAFICA AGGIUNTIVA
009300041029     C***  ACOKSC        CHAIN     FNCLS01L
009400041029     C***                IF        %FOUND(FNCLS01L)
009500041029     C***                IF        TGCTCM = ' '
009600041029     C***                MOVE      CLSFLO        TGCTCM
009700041029     C***                SETON                                        10
009800041029     C***                ENDIF
009900950805     C*
010000041029     C***                IF        TGCTFG = ' '
010100041029     C***                MOVEl     CLSFLO        TGCTFG
010200041029     C***                SETON                                        11
010300041029     C***                ENDIF
010400041029     C***                ENDIF
010500040920      *
010600041029     C***                ENDIF
010700041029      *
010800041029     C***                ENDIF
010900040920      * VERIFICO LE SELEZIONI FATTE  A VIDEO
011000041029     C                   IF        D23TCM <> ' ' AND
011100041029     C                             ((tgctcm <> ' ' and tgctcm <> d23tcm) or
011200041029     c                              (tgctcm =  ' ' and clstcm <> d23tcm))
011300040920     C                   GOTO      LEGGI
011400040920     C                   ENDIF
011500040920      *
011600041029     C                   IF        D23TFG <> ' ' AND
011700041029     C                             ((tgctfg <> ' ' and tgctfg <> d23tfg) or
011800041029     c                              (tgctfg =  ' ' and clstfg <> d23tfg))
011900040920     C                   GOTO      LEGGI
012000040920     C                   ENDIF
012100041029      *
012200041029     c                   endif
012300040920      *
012400950805     C* STAMPA TETSTA
012500950805     C     *INOF         IFEQ      *ON
012600041029      *
012700950805     C                   EXCEPT    TES
012800040920     C                   EXCEPT    TES1
012900950805     C                   SETOFF                                       OF
013000950805     C                   ENDIF
013100991019     C*
013200991019     C* RECUPERO LA DIVISA DELLA TARIFFA DALLA TESTATA TAROFFA
013300991019     C     KTAM          CHAIN     TNTAM01L                           32
013400991019     C*
013500991019     C     *IN32         IFEQ      *OFF
013600991019     C                   MOVEL     TAMFLO        DSTA01
013700991019     C* VERIFICO LA DIVISA SE HA DECIMALI OPPURE NO
013800991019     C* CONTROLLO DELLA DIVISA
013900991019     C                   MOVEL     'CV'          COD               2
014000991019     C                   MOVEL     *BLANKS       KEY               8
014100991019     C                   MOVEL     §TADIV        KEY
014200991019     C     KTAB          CHAIN     TABEL                              32
014300991019     C   32              MOVEL     *BLANKS       TBLUNI
014400991019     C                   MOVEL     TBLUNI        DSCV
014500991019     C     §CVFDC        COMP      'S'                                    95
014600991019     C* PREPARO IL CAMPO DI STAMPA
014700991019     C                   MOVEL     §TADIV        WDIV              3
014800950805     C*
014900950805     C* STAMPA DETTAGLIO
015000990714     C* ADEGUO GLI IMPORTI ALLE LUNGHEZZE DI STAMPA
015100991019     C     *IN95         IFEQ      *OFF
015200990714     C                   Z-ADD     TGCSGV        STPSGV            7 0
015300990714     C                   Z-ADD     TGCSGR        STPSGR            7 0
015400990714     C                   Z-ADD     TGCSGP        STPSGP            7 0
015500990714     C                   Z-ADD     TGCSGD        STPSGD            7 0
015600990714     C                   Z-ADD     TGCST1        STPST1            7 0
015700990714     C                   Z-ADD     TGCST2        STPST2            7 0
015800990714     C                   Z-ADD     TGCST3        STPST3            7 0
015900990714     C                   Z-ADD     TGCSTM        STPSTM            7 0
016000991019     C                   ELSE
016100991019     C                   Z-ADD     TGCSGV        STDSGV            7 3
016200991019     C                   Z-ADD     TGCSGR        STDSGR            7 3
016300991019     C                   Z-ADD     TGCSGP        STDSGP            7 3
016400991019     C                   Z-ADD     TGCSGD        STDSGD            7 3
016500991019     C                   Z-ADD     TGCST1        STDST1            7 3
016600991019     C                   Z-ADD     TGCST2        STDST2            7 3
016700991019     C                   Z-ADD     TGCST3        STDST3            7 3
016800991019     C                   Z-ADD     TGCSTM        STDSTM            7 3
016900991019     C                   ENDIF
017000991019     C                   ENDIF
017100990714     C*
017200950805     C                   EXCEPT    DET
017300041029     c                   if        prima = ' ' and *inof
017400041029     c                   except    fine
017500041029     c                   movel     'N'           prima             1
017600041029     c                   endif
017700970729     C                   ENDIF
017800950805     C*
017900040920     C     LEGGI         TAG
018000990714     C     ACOKSC        READE     TITGC000                               31
018100950805    3C                   ENDDO
018200950805    2C                   ENDIF
018300950805     C*
018400950805     C     KACO          READE     CNACO000                               30
018500950805    1C                   ENDDO
018600041029     c
018700041029     c                   if        prima = ' '
018800041029     c                   except    fine
018900041029     c                   movel     'N'           prima             1
019000041029     c                   endif
019100950805     C*
019200950805     C                   SETON                                        LR
019300950805     C*
019400950805     C*--- INZSR INIZIALE --------------------------------------------*
019500950805     C     *INZSR        BEGSR
019600950805     C*
019700950805     C     *ENTRY        PLIST
019800950805     C                   PARM                    KPJBA
019900041012     c                   Movel     kpjbu         Tnta23ds
020000040920      *
020100991019     C* CHIAVE DI TNTAM
020200991019     C     KTAM          KLIST
020300991019     C                   KFLD                    TGCKSC
020400991019     C                   KFLD                    TGCCTR
020500991019     C                   KFLD                    TGCPRG
020600991019     C* CHIAVE DI TABEL
020700991019     C*
020800991019     C     KTAB          KLIST
020900991019     C                   KFLD                    CODUT
021000991019     C                   KFLD                    COD
021100991019     C                   KFLD                    KEY
021200991019     C*
021300991019     C                   Z-ADD     1             CODUT             1 0
021400950805     C*
021500950805     C* LA DATA DEL GIORNO LA PRENDO DA TIME
021600950805     C                   TIME                    W0140            14 0
021700950805     C                   MOVE      W0140         UDATE8            8 0
021800950805     C                   MOVEL     W0140         UTIME             6 0
021900950805     C*
022000950805     C                   SETON                                        OF
022100041012
022200041012      * se è stato richiesto un p.o. solo imposto il p.o. richiesto nella sk
022300041012      * fig
022400041012     c                   If        d23fil > *Zeros
022500041012     c                   Move      d23fil        fig(1)
022600041012     c                   else
022700041012      * Reperimento dei P.O. gestibili dall'utente
022800041012     c                   clear                   TRUL31DS
022900041012     c                   eval      I31abi = d23abi
023000041012     c                   eval      I31cdi = d23cdi
023100041012     c                   eval      I31car = d23car
023200041012     c                   eval      I31cpo = d23cpo
023300041012     c                   call      'TRUL31R'
023400041012     c                   parm                    KPJBA
023500041012     c                   parm                    TRUL31DS
023600041012     c                   if        O31pog > *zeros
023700041012     c                   movea     O31pog        fig
023800041012     c                   endif
023900041012     c                   endif
024000040920      *
024100040920      * stampo le parazializzazioni fatte a video
024200040920      *
024300041029     c                   If        d23dpc = 'S'
024400041029     c                   movel     'SI'          stpdpc            2
024500041029     c                   endif
024600040920     c                   Except    tes
024700040920     c                   Except    tes0
024800950805     C*
024900950805     C****  KLIST  ****
025000950805     C* ACCESSO CNACO04L
025100950805     C     KACO          KLIST
025200950805     C                   KFLD                    CODUT
025300041011     C                   KFLD                    d23KCI
025400950805     C*
025500950805     C                   ENDSR
025600950805     C*
025700950805     C*---------------------------------------------------------------*
025800950805     OPRTF198   E            TES              02
025900950805     O                       SK(1)              132
026000041011     O                       d23rsu              20
026100950805     O                                           21 '/'
026200041011     O                       d23fel              24
026300950805     O                                          170 'TNTA13R'
026400950805     O                       UDATE8             183 '  /  /    '
026500950805     O                                          193 'PAG.'
026600950805     O                       PAGE          Z    198
026700950805     O          E            TES         1
026800950805     O                       KNSIF               10
026900950805     O                       KNMUS               21
027000950805     O                       UTIME              181 '  :  :  '
027100040921     O          E            TES0       10
027200040921     O                                        +   2 '--------------  Selezioni '
027300040921     O                                              '--------------'
027400040921     O          E            TES0        3
027500040920     O                                        +   2 'Punto Operativo'
027600041029     O                       d23fil        z  +  43
027700041012     O                       d23dpo           +   3
027800040920     O          E            TES0        3
027900040920     O                                        +   2 'Tipo Comunicazione al'
028000040921     O                                              ' Mittente'
028100041029     O                       D23TCM           +  30
028200040920     O                       D23DCM           +   3
028300040920     O          E            TES0        3
028400040920     O                                        +   2 'Tipo Comunicazione di'
028500040921     O                                              ' Fine Giacenza'
028600041029     O                       D23TFG           +  25
028700040920     O                       D23DFG           +   3
028800041029     O          E            TES0        3
028900041029     O                                        +   2 'Lista Tariffe con Tip'
029000041029     O                                              'o Comunicazioni diffe'
029100041029     O                                              'renti da Anag.Clienti'
029200041029     O                       STPDPC           +   1
029300040920     O          E            TES1        2
029400950805     O                       SK(2)               66
029500950805     O                       SK(6)              132
029600950805     O                       SK(10)             198
029700040920     O          E            TES1        1
029800950805     O                       SK(3)               66
029900950805     O                       SK(7)              132
030000950805     O                       SK(11)             198
030100041029     O          E            TES1        1
030200950805     O                       SK(4)               66
030300950805     O                       SK(8)              132
030400950805     O                       SK(12)             198
030500041029     O          E            TES1        1  1
030600041029     O                       SK(14)             198
030700950805     O* DETTAGLIO
030800950805     O          E            DET         1
030900950805     O                       ACORAG              48
031000991019     O                       ACOKSC        Z     56
031100991019     O                       TGCCTR              60
031200991019     O                       TGCPRG        Z     64
031300991019     O                       WDIV                68
031400991019     O              N95      STPSGV        Z     76
031500991019     O              N95      STPSGR        Z     85
031600991019     O              N95      STPSGP        Z     94
031700991019     O              N95      STPSGD        Z    103
031800991019     O               95      STDSGV        4     76
031900991019     O               95      STDSGR        4     85
032000991019     O               95      STDSGP        4     94
032100991019     O               95      STDSGD        4    103
032200950805     O                       TGCSG1        Z    108
032300950805     O                       TGCSG2        Z    115
032400950805     O                       TGCSG3        Z    122
032500991019     O              N95      STPST1        Z    133
032600991019     O              N95      STPST2        Z    142
032700991019     O              N95      STPST3        Z    151
032800991019     O               95      STDST1        4    133
032900991019     O               95      STDST2        4    142
033000991019     O               95      STDST3        4    151
033100950805     O                       TGCSGF        Z    156
033200950805     O                       TGCFAF             162
033300950805     O                       TGCGGR        Z    168
033400991019     O              N95      STPSTM        Z    178
033500991019     O               95      STDSTM        4    178
033600950805     O                       TGCRPV        4    184
033700041029     O                       TGCTCM             187
033800041029     O                                          188 '('
033900041029     O                       CLSTCM             189
034000041029     O                                          190 ')'
034100041029     O                       TGCTFG             192
034200041029     O                                          193 '('
034300041029     O                       CLSTFG             194
034400041029     O                                          195 ')'
034500041029     O          E            fine             64
034600041029     O                       sk(15)              66
034700950805     O*
034800950805     O*---------------------------------------------------------------*
034900970729**
035000950805     ****    T A R I F F E    D I    G I A C E N Z A    ****         1
035100991019RAGIONE SOCIALE                                  CODICE  COD PRG D   2
035200991019                                                 CLIENTE TAR TAR T   3
035300950805                                                                     4
035400991019XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXX XXX XXX X   5
035500991019IV   SPESE  SPESE    SPESE    SPESE    GG.    GG.    GG.      SPES   6
035600991019AR   VARIE  RICONS.  RICONS.  DOSSIER  SOSTA  SOSTA  SOSTA    SOST   7
035700950805            CITTA'   PROVINC.          1°sc.  2°sc.  3°sc.    1°sc   8
035800991019XX XXXXXXX  XXXXXXX  XXXXXXX  XXXXXXX  XXX    XXX    XXX    XXXXXX   9
035900970729E    SPESE    SPESE  FRANCHIGIA   GG.   SPESE   RAP.  T.C. T.C.     10
036000950805A    SOSTA    SOSTA  GG.   FLAG  RIEN.  SOSTA   PESO  APE. FINE     11
036100950805.    2°sc.    3°sc.        APPL.        MINIMO  VOL.  MIT. GIAC.    12
036200970729X  XXXXXXX  XXXXXXX  XXX     X    XX   XXXXXXX  XX,X   X    X       13
036300041029                                                       (*)  (*)     14
036400041029(*)=valori in Anagrafica Clienti                                    15
