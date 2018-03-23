000100051121     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200940720     H* TNTA36R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400940720     H*              RICERCA PROGRESSIVI TARIFFE/OFFERTE             *
000500000000     H*--------------------------------------------------------------*
000600940720     FTNTA36D   CF   E             WORKSTN
000700940720     F                                     SFILE(TA36S01:NRR)
000800940720     FTNTAM01L  IF   E           K DISK
000900991022     FTITPT01L  IF   E           K DISK
001000991022     FTITGC01L  IF   E           K DISK
001100900329     FCNACO00F  IF   E           K DISK
001200941103     FTABEL00F  IF   E           K DISK
001300051121      *------------------------------------------------------------------------*
001400051121     d conta           s              3  0
001500051121     d dataoggi        s              8  0
001600051121     d savdst          s                   like(tamdst)
001700051121     d wksc            s                   like(tamksc)
001800051121     d wctr            s                   like(tamctr)
001900051121     d wprg            s                   like(tamprg)
002000061220     d wdatarif        s              8  0
002100940825     D*
002200900329     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA DA COPIARE
002300900329     D PARAM4          DS
002400940825     D* COD4 = CODICE CLIENTE DELLA TARIFFA DA CERCARE
002500900329     D  COD4                   6     12  0
002600940825     D* CTR4 = RESTITUISCE IL CODICE TARIFFA SELEZIONATO
002700940825     D*        SE NON SELEZIONATO RESTITUISCE A BLANKS
002800900329     D  CTR4                  13     15
002900060515     D* FLG4 ='2' CERCA NELLE OFFERTE '1'= CERCA NELLE TARIFFE
003000900329     D  FLG4                  16     16
003100940825     D* VPR4 =' ' VISUALIZZA SOLO L'ULTIMO PROGRESSIVO PER OGNI TARIFFA
003200940825     D* VPR4 ='1' VISUALIZZA TUTTI I PROGRESSIVI DI TUTTE LE TARIFFE
003300051121     D* VPR4 ='2' VISUALIZZA il progressivo in essere + i sucessivi
003400940825     D  VPR4                  17     17
003500940825     D* CCC= CAPOCONTO CLIENTI
003600940825     D  CCC4                  18     21  0
003700940825     D* VIDDCV =  RESTITUISCE LA DESCRIZIONE VARIABILE DELLA TARIFFA
003800900530     D  VIDDCV                22     36
003900961111     D* PARPRG =  RESTITUISCE IL PROGRESSIVO DELLA TARIFFA SELEZIONATA
004000961111     D*           SE NON SELEZIONATO RESTITUISCE A BLANKS
004100961111     D  PARPRG                37     39
004200061220      * data riferimento (non viene passata da tutti i chiamanti)
004300061220     d  pardtr                40     47
004400090608     d ds5e          e ds
004500000000     D KPJBA         E DS
004600900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004700991022     D DSTA01        E DS
004800940720     D                 DS
004900940720     D  AA                     1      4  0
005000940720     D  MM                     5      6  0
005100940720     D  GG                     7      8  0
005200940720     D  DATA                   1      8  0
005300000000     I/SPACE 3
005400000000     C*---------------------------------------------------------------*
005500000000     C     *ENTRY        PLIST
005600000000     C                   PARM                    KPJBA
005700900329     C                   MOVEL     KPJBU         PARAM4
005800961111     C                   CLEAR                   PARPRG
005900000000     C                   Z-ADD     1             CODUT
006000900320     C                   CALL      'X§PARUT'
006100900320     C                   PARM                    UT§DSE
006200000000     C                   MOVEL     RAGUT         RSUT             20
006300940720     C*
006400000000     C*---------------------------------------------------------------*
006500900329     C     KACO          KLIST
006600900329     C                   KFLD                    CODUT
006700940825     C                   KFLD                    CCC4
006800900329     C                   KFLD                    COD4
006900941103     C     KTAB          KLIST
007000941103     C                   KFLD                    CODUT
007100941103     C                   KFLD                    COD
007200941103     C                   KFLD                    KEY
007300991022     C* ACCESSO TITPT01L
007400991022     C     KTPT          KLIST
007500991022     C                   KFLD                    TAMKSC
007600991022     C                   KFLD                    TAMCTR
007700991022     C                   KFLD                    TAMPRG
007800051121
007900051121     c     ktam          klist
008000051121     C                   KFLD                    TAMKSC
008100051121     C                   KFLD                    TAMCTR
008200051121
008300051121     c     ktam01        klist
008400051121     C                   KFLD                    wKSC
008500051121     C                   KFLD                    wCTR
008600051121     C                   KFLD                    wPRG
008700941103     C**
008800941103     C* DEFINIZIONE CAMPI
008900941103     C**
009000941103     C     *LIKE         DEFINE    TBLKEY        KEY
009100941103     C     *LIKE         DEFINE    TBLCOD        COD
009200051121     c                   Z-add     *date         dataoggi
009300061220
009400061220      * se data riferimento a blank passo data del giorno
009500061220     c                   if        pardtr = *blanks or
009600061220     c                             pardtr = *zeros
009700061220     c                   eval      wdatarif = dataoggi
009800061220     c                   else
009900061220     c                   move      pardtr        wdatarif
010000061220     c                   endif
010100941103     C*---------------------------------------------------------------*
010200940720     C*
010300930210     C                   Z-ADD     0             NRR               5 0
010400900402     C* SE FLG4 = 2 COPIO DA OFFERTA
010500900329     C     FLG4          IFEQ      '2'
010600941103     C                   SETON                                        1020
010700900330     C                   Z-ADD     COD4          NRV4
010800900329     C                   END
010900940927     C*
011000940927     C* VPR4 = '1' ---> VISUALIZZO TUTTI I PRG DI TUTTE LE TARIFFE
011100940927     C*                 QUINDI ACCENDO *IN10 (COME PER LE OFFERTE)
011200940927     C*                 PERCHE' DEVO ESEGUIRE LE STESSE ISTRUZIONI
011300940927     C     VPR4          IFEQ      '1'
011400940927     C                   SETON                                        10
011500940927     C                   ENDIF
011600051121     C*
011700051121     C* VPR4 = '2' ---> VISUALIZZO TUTTI I PRG Da quello in vigore
011800051121     C*                 in poi (solo per tariffe)
011900051121     C     VPR4          IFEQ      '2'
012000051121     C                   SETON                                        12
012100051121     C                   ENDIF
012200940927     C*
012300900323     C** PULIZIA SUBFILE
012400000000     C                   EXSR      PULSF
012500941103     C*
012600900329     C* SE TARIFFA DECODIFICA CODICE CLIENTE
012700941103     C     FLG4          IFNE      '2'
012800941103     C     KACO          CHAIN     CNACO000                           30
012900941103     C   30              GOTO      FINE
013000941103     C                   MOVEL     ACORAG        RAGDES
013100941103     C                   ENDIF
013200900323     C*
013300940720     C     COD4          SETLL     TNTAM000
013400900329     C     LEGGI         TAG
013500940720     C     COD4          READE     TNTAM000                               30
013600900329     C* FINE CARICAMENTO
013700900330     C* SE TARIFFA EMETTO ULTIMA LETTA
013800900402     C  N10
013900051121     CanN12
014000900402     CAN 30              EXSR      SCRIVI
014100941103     C*
014200900329     C  N30              DO
014300900329     C* SE ANNULLATO TORNO A LEGGERE
014400900329     C     TAMATB        CABEQ     'A'           LEGGI
014500051121     c                   eval      wksc = tamksc
014600051121     c                   eval      wctr = tamctr
014700051121     c                   eval      wprg = tamprg
014800051121     c                   move      tamctr        wctr3             3
014900051121     c   12              If        wctr3  <> savctr
015000051121     c                   ExSr      cercadst
015100051121     c                   EndIf
015200900329     C* SALVO SOLO LA PRIMA VOLTA
015300051121     C  N11              move      TAMCTR        SAVCTR            3
015400900329     C  N11              SETON                                        11
015500900329     C*
015600900329     C* SE TARIFFA EMETTO SOLO ULTIMO PRG
015700051121     C  n10              IF        wctr3  <> SAVCTR
015800051121     C  n12              EXSR      SCRIVI
015900051121     C                   move      TAMCTR        SAVCTR
016000900329     C                   END
016100900329     C*
016200900329     C                   MOVE      ' '           VIDSCE
016300900329     C                   Z-ADD     TAMPRG        VIDPRG
016400900329     C                   Z-ADD     TAMCTR        VIDCTR
016500991022     C* CERCO LA DATA ULTIMA VARIAZIONE PIU' ALTA DI TUTTI
016600991022     C                   EXSR      CERDUV
016700991022     C                   Z-ADD     TAMDUV        VIDDUV
016800991022     C* DIVISA
016900991022     C                   MOVEL     TAMFLO        DSTA01
017000991022     C                   MOVEL     §TADIV        VIDDIV
017100991022     C*
017200900329     C                   Z-ADD     TAMDDT        VIDDDT
017300900329     C                   Z-ADD     TAMDST        VIDDST
017400900529     C                   MOVEL     TAMDCV        VIDDCV
017500020321      *
017600020321     C* DPD/FEDEX/ITALIA/ESTERO
017700020321     C                   SELECT
017800020321  1A C     §TADPD        WHENEQ    'S'
017900020321     C                   MOVE      'D'           VIDFIE
018000020321     C*
018100020321  1A C     §TAFED        WHENEQ    'S'
018200020321     C                   MOVE      'F'           VIDFIE
018300020321     C                   OTHER
018400020321     C                   MOVEL     TAMFIE        VIDFIE
018500020321     C                   ENDSL
018600020321      *
018700940720     C                   MOVEL     TAMFTP        VIDFTP
018800941103     C*
018900941103     C* TIPO SERVIZIO
019000941103     C                   MOVEL     '5E'          COD
019100941103     C                   MOVEL(P)  TAMTSP        KEY
019200941103     C     KTAB          CHAIN     TABEL                              30
019300090608     C  N30              MOVEL     TBLUNI        ds5e
019400090608     C  N30              MOVEL     §5ed08        VIDTSP
019500941103     C   30              CLEAR                   VIDTSP
019600941103     C*
019700941103     C* BLOCCATA /ARRIVI /PARTENZE
019800941103     C                   CLEAR                   VIDBAP
019900941103     C     TAMBAP        IFEQ      'B'
020000941103     C                   MOVEL     'BLOC'        VIDBAP
020100941103     C                   ENDIF
020200941103     C     TAMBAP        IFEQ      'P'
020300941103     C                   MOVEL     'PART'        VIDBAP
020400941103     C                   ENDIF
020500941103     C     TAMBAP        IFEQ      'A'
020600941103     C                   MOVEL     'ARR '        VIDBAP
020700941103     C                   ENDIF
020800900329     C* COPIA DA OFFERTA
020900940927     C   10              EXSR      SCRIVI
021000051121
021100051121     c                   If        *in12 and wctr3  = savctr
021200051121     c                   If        conta = 0 and tamdst >= savdst
021300051121     c                   Exsr      scrivi
021400051121     c                   endif
021500061220     c**!!!              If        conta <> 0 and savdst <= dataoggi and
021600061220     c**!!!                        tamdst >= dataoggi
021700061220     c                   If        conta <> 0 and savdst <= wdatarif and
021800061220     c                             tamdst >= wdatarif
021900051121     c                   Exsr      scrivi
022000051121     c                   endif
022100051121     c                   endif
022200900330     C*
022300900329     C                   GOTO      LEGGI
022400900329     C                   END
022500900529     C* NON ESISTONO PROGRESSIVI PER QUESTA TARIFFA/OFFERTA
022600900529     C     NRR           IFEQ      0
022700900529     C                   GOTO      FINE
022800900529     C                   END
022900900323     C*
023000940720     C                   WRITE     TA36Z02
023100000000     C** COMANDI SUBFILE
023200000000     C     SU            TAG
023300940720     C                   EXFMT     TA36C01
023400900323     C** FINE LAVORO
023500900323     C   KL              GOTO      FINE
023600900323     C** CONTROLLO SE EFFETTUATA UNA SCELTA
023700940720     C                   READC     TA36S01                                30
023800900329     C   30              SETON                                        99
023900900329     C   30              GOTO      SU
024000900323     C*
024100900329     C                   MOVE      VIDCTR        CTR4
024200961111     C                   MOVE      VIDPRG        PARPRG
024300900329     C                   MOVEL     PARAM4        KPJBU
024400900323     C*
024500000000     C     FINE          TAG
024600000000     C                   SETON                                        LR
024700900323     C* PULIZIA SUBFILE ---------------------------------------------
024800900323     C     PULSF         BEGSR
024900000000     C                   SETON                                        84
025000940720     C                   WRITE     TA36C01
025100000000     C                   SETOFF                                       84
025200900319     C                   ENDSR
025300900330     C*
025400940720     C*--- SCRIVO SUBFILE --------------------------------------------*
025500900330     C     SCRIVI        BEGSR
025600940720     C* GIRO DATA DECORRENZA
025700940720     C                   Z-ADD     VIDDDT        DATA
025800940720     C                   Z-ADD     MM            GGMM              4 0
025900940720     C                   MOVEL     GG            GGMM
026000940720     C                   Z-ADD     AA            VIDDDT
026100940720     C                   MOVEL     GGMM          VIDDDT
026200940720     C* GIRO DATA SCADENZA
026300940720     C                   Z-ADD     VIDDST        DATA
026400940720     C                   Z-ADD     MM            GGMM
026500940720     C                   MOVEL     GG            GGMM
026600940720     C                   Z-ADD     AA            VIDDST
026700940720     C                   MOVEL     GGMM          VIDDST
026800940720     C* GIRO DATA ULTIMA VARIAZIONE
026900940720     C                   Z-ADD     VIDDUV        DATA
027000940720     C                   Z-ADD     MM            GGMM
027100940720     C                   MOVEL     GG            GGMM
027200940720     C                   Z-ADD     AA            VIDDUV
027300940720     C                   MOVEL     GGMM          VIDDUV
027400940720     C*
027500900330     C                   ADD       1             NRR
027600940720     C                   WRITE     TA36S01
027700940720     C*
027800900330     C                   ENDSR
027900991022     C*--- RICERCA ULTIMA DATA VARIAZIONE TARIFFA --------------------*
028000991022     C     CERDUV        BEGSR
028100991022     C*
028200991022     C* LEGGO TUTTE LE TESTATE TARIFFE PARTICOLARI  CERCANDO LA DATA
028300991022     C* MAGGIORE DI ULTIMA MODIFICA
028400991022     C     KTPT          SETLL     TITPT01L
028500991022     C     KTPT          READE     TITPT01L                               33
028600991022     C*
028700991022     C     *IN33         DOWEQ     *OFF
028800991022     C* VERIFICO SE LA DATA ULTIMA VARIAZIONE E' MAGGIORE DI QUELLA
028900991022     C* DELLA TESTATA
029000991022     C     TPTDUV        IFGT      TAMDUV
029100991022     C     TPTATB        ANDEQ     ' '
029200991022     C                   Z-ADD     TPTDUV        TAMDUV
029300991022     C                   ENDIF
029400991022     C*
029500991022     C     KTPT          READE     TITPT01L                               33
029600991022     C*
029700991022     C                   ENDDO
029800991022     C* VERIFICO ANCHE L'ULTIMA DATA VARIAZIONE TARIFFA CHE C'E' IN
029900991022     C* TITGC TARIFFE GIACENZE  (SE ESISTE)
030000991022     C     KTPT          CHAIN     TITGC01L                           33
030100991022     C     *IN33         IFEQ      *OFF
030200991022     C     TGCATB        ANDEQ     ' '
030300991022     C     TGCDUV        ANDGT     TAMDUV
030400991022     C                   Z-ADD     TGCDUV        TAMDUV
030500991022     C                   ENDIF
030600991022     C*
030700991022     C                   ENDSR
030800991022     C*
030900051121
031000051121      *------------------------------------------------------------------------*
031100051121      * Cerco il progrssivo in essere
031200051121      *------------------------------------------------------------------------*
031300051121     c     Cercadst      BegSr
031400051121
031500051121     c                   Clear                   savdst
031600051121     c                   Clear                   conta
031700051121
031800051121     c     ktam          Setll     Tntam01l
031900051121     c                   Do        *Hival
032000051121     c     ktam          Reade     Tntam01l
032100051121     c                   If        %Eof(tntam01l)
032200051121     c                   Leave
032300051121     c                   EndIf
032400051121     c                   If        tamatb <> *Blanks
032500051121     c                   Iter
032600051121     c                   EndIf
032700061220     c**!!!              If        tamdst <= dataoggi
032800061220     c                   If        tamdst <= wdatarif
032900051121     c                   Eval      savdst = tamdst
033000051121     c                   Else
033100051121     c                   add       1             conta
033200051121     c                   Leave
033300051121     c                   EndIf
033400051121
033500051121     c                   EndDo
033600051121
033700051121     c     ktam01        chain     tntam01l
033800051121
033900051121     c                   EndSr
034000991022     C*---------------------------------------------------------------*
