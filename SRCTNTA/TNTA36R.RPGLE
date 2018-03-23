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
001300091120     ftfntc01l  if   e           k disk
001400051121      *------------------------------------------------------------------------*
001500051121     d conta           s              3  0
001600051121     d dataoggi        s              8  0
001700051121     d savdst          s                   like(tamdst)
001800051121     d wksc            s                   like(tamksc)
001900051121     d wctr            s                   like(tamctr)
002000051121     d wprg            s                   like(tamprg)
002100061220     d wdatarif        s              8  0
002200091120
002300091120     d kntcapl         s                   like(NtcApl)
002400091120     d kntcnk1         s                   like(NtcNk1)
002500091120     d kntcnk2         s                   like(NtcNk2)
002600091120     d kntctnt         s                   like(NtcTnt)
002700940825     D*
002800900329     D* PASSO IN SCELTA COD TARIFFA DI TARIFFA/OFFERTA DA COPIARE
002900900329     D PARAM4          DS
003000940825     D* COD4 = CODICE CLIENTE DELLA TARIFFA DA CERCARE
003100900329     D  COD4                   6     12  0
003200940825     D* CTR4 = RESTITUISCE IL CODICE TARIFFA SELEZIONATO
003300940825     D*        SE NON SELEZIONATO RESTITUISCE A BLANKS
003400900329     D  CTR4                  13     15
003500060515     D* FLG4 ='2' CERCA NELLE OFFERTE '1'= CERCA NELLE TARIFFE
003600091120      *       '3' cerca nelle offerte da trattativa
003700900329     D  FLG4                  16     16
003800940825     D* VPR4 =' ' VISUALIZZA SOLO L'ULTIMO PROGRESSIVO PER OGNI TARIFFA
003900940825     D* VPR4 ='1' VISUALIZZA TUTTI I PROGRESSIVI DI TUTTE LE TARIFFE
004000051121     D* VPR4 ='2' VISUALIZZA il progressivo in essere + i sucessivi
004100940825     D  VPR4                  17     17
004200940825     D* CCC= CAPOCONTO CLIENTI
004300940825     D  CCC4                  18     21  0
004400940825     D* VIDDCV =  RESTITUISCE LA DESCRIZIONE VARIABILE DELLA TARIFFA
004500900530     D  VIDDCV                22     36
004600961111     D* PARPRG =  RESTITUISCE IL PROGRESSIVO DELLA TARIFFA SELEZIONATA
004700961111     D*           SE NON SELEZIONATO RESTITUISCE A BLANKS
004800961111     D  PARPRG                37     39
004900061220      * data riferimento (non viene passata da tutti i chiamanti)
005000061220     d  pardtr                40     47
005100090608     d ds5e          e ds
005200000000     D KPJBA         E DS
005300900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005400991022     D DSTA01        E DS
005500940720     D                 DS
005600940720     D  AA                     1      4  0
005700091120     d  aa2                    3      4  0
005800940720     D  MM                     5      6  0
005900940720     D  GG                     7      8  0
006000940720     D  DATA                   1      8  0
006100000000     I/SPACE 3
006200000000     C*---------------------------------------------------------------*
006300000000     C     *ENTRY        PLIST
006400000000     C                   PARM                    KPJBA
006500900329     C                   MOVEL     KPJBU         PARAM4
006600961111     C                   CLEAR                   PARPRG
006700000000     C                   Z-ADD     1             CODUT
006800900320     C                   CALL      'X§PARUT'
006900900320     C                   PARM                    UT§DSE
007000000000     C                   MOVEL     RAGUT         RSUT             20
007100940720     C*
007200000000     C*---------------------------------------------------------------*
007300900329     C     KACO          KLIST
007400900329     C                   KFLD                    CODUT
007500940825     C                   KFLD                    CCC4
007600900329     C                   KFLD                    COD4
007700941103     C     KTAB          KLIST
007800941103     C                   KFLD                    CODUT
007900941103     C                   KFLD                    COD
008000941103     C                   KFLD                    KEY
008100991022     C* ACCESSO TITPT01L
008200991022     C     KTPT          KLIST
008300991022     C                   KFLD                    TAMKSC
008400991022     C                   KFLD                    TAMCTR
008500991022     C                   KFLD                    TAMPRG
008600051121
008700051121     c     ktam          klist
008800051121     C                   KFLD                    TAMKSC
008900051121     C                   KFLD                    TAMCTR
009000051121
009100051121     c     ktam01        klist
009200051121     C                   KFLD                    wKSC
009300051121     C                   KFLD                    wCTR
009400051121     C                   KFLD                    wPRG
009500091120      *
009600091120     c     ktfntc        klist
009700091120     c                   kfld                    kntcapl
009800091120     c                   kfld                    kntcnk1
009900091120     c                   kfld                    kntcnk2
010000091120     c                   kfld                    kntctnt
010100941103     C**
010200941103     C* DEFINIZIONE CAMPI
010300941103     C**
010400941103     C     *LIKE         DEFINE    TBLKEY        KEY
010500941103     C     *LIKE         DEFINE    TBLCOD        COD
010600051121     c                   Z-add     *date         dataoggi
010700061220
010800061220      * se data riferimento a blank passo data del giorno
010900061220     c                   if        pardtr = *blanks or
011000061220     c                             pardtr = *zeros
011100061220     c                   eval      wdatarif = dataoggi
011200061220     c                   else
011300061220     c                   move      pardtr        wdatarif
011400061220     c                   endif
011500941103     C*---------------------------------------------------------------*
011600940720     C*
011700930210     C                   Z-ADD     0             NRR               5 0
011800900402     C* SE FLG4 = 2 COPIO DA OFFERTA
011900900329     C     FLG4          IFEQ      '2'
012000091120     c     flg4          oreq      '3'
012100941103     C                   SETON                                        1020
012200900330     C                   Z-ADD     COD4          NRV4
012300091120     c                   if        flg4 = '3'
012400091120     c                   eval      *in21 = *on
012500091120     c                   eval      nrvt = cod4
012600091120     c                   endif
012700900329     C                   END
012800940927     C*
012900940927     C* VPR4 = '1' ---> VISUALIZZO TUTTI I PRG DI TUTTE LE TARIFFE
013000940927     C*                 QUINDI ACCENDO *IN10 (COME PER LE OFFERTE)
013100940927     C*                 PERCHE' DEVO ESEGUIRE LE STESSE ISTRUZIONI
013200940927     C     VPR4          IFEQ      '1'
013300940927     C                   SETON                                        10
013400940927     C                   ENDIF
013500051121     C*
013600051121     C* VPR4 = '2' ---> VISUALIZZO TUTTI I PRG Da quello in vigore
013700051121     C*                 in poi (solo per tariffe)
013800051121     C     VPR4          IFEQ      '2'
013900051121     C                   SETON                                        12
014000051121     C                   ENDIF
014100940927     C*
014200900323     C** PULIZIA SUBFILE
014300000000     C                   EXSR      PULSF
014400941103     C*
014500900329     C* SE TARIFFA DECODIFICA CODICE CLIENTE
014600941103     C     FLG4          IFNE      '2'
014700091120     c     flg4          andne     '3'
014800941103     C     KACO          CHAIN     CNACO000                           30
014900941103     C   30              GOTO      FINE
015000941103     C                   MOVEL     ACORAG        RAGDES
015100941103     C                   ENDIF
015200900323     C*
015300940720     C     COD4          SETLL     TNTAM000
015400900329     C     LEGGI         TAG
015500940720     C     COD4          READE     TNTAM000                               30
015600900329     C* FINE CARICAMENTO
015700900330     C* SE TARIFFA EMETTO ULTIMA LETTA
015800900402     C  N10
015900051121     CanN12
016000900402     CAN 30              EXSR      SCRIVI
016100941103     C*
016200900329     C  N30              DO
016300900329     C* SE ANNULLATO TORNO A LEGGERE
016400900329     C     TAMATB        CABEQ     'A'           LEGGI
016500051121     c                   eval      wksc = tamksc
016600051121     c                   eval      wctr = tamctr
016700051121     c                   eval      wprg = tamprg
016800051121     c                   move      tamctr        wctr3             3
016900051121     c   12              If        wctr3  <> savctr
017000051121     c                   ExSr      cercadst
017100051121     c                   EndIf
017200900329     C* SALVO SOLO LA PRIMA VOLTA
017300051121     C  N11              move      TAMCTR        SAVCTR            3
017400900329     C  N11              SETON                                        11
017500900329     C*
017600900329     C* SE TARIFFA EMETTO SOLO ULTIMO PRG
017700051121     C  n10              IF        wctr3  <> SAVCTR
017800051121     C  n12              EXSR      SCRIVI
017900051121     C                   move      TAMCTR        SAVCTR
018000900329     C                   END
018100900329     C*
018200900329     C                   MOVE      ' '           VIDSCE
018300900329     C                   Z-ADD     TAMPRG        VIDPRG
018400900329     C                   Z-ADD     TAMCTR        VIDCTR
018500991022     C* CERCO LA DATA ULTIMA VARIAZIONE PIU' ALTA DI TUTTI
018600991022     C                   EXSR      CERDUV
018700991022     C                   Z-ADD     TAMDUV        VIDDUV
018800991022     C* DIVISA
018900991022     C                   MOVEL     TAMFLO        DSTA01
019000991022     C                   MOVEL     §TADIV        VIDDIV
019100991022     C*
019200900529     C                   MOVEL     TAMDCV        VIDDCV
019300020321      *
019400020321     C* DPD/FEDEX/ITALIA/ESTERO
019500020321     C                   SELECT
019600020321  1A C     §TADPD        WHENEQ    'S'
019700020321     C                   MOVE      'D'           VIDFIE
019800020321     C*
019900020321  1A C     §TAFED        WHENEQ    'S'
020000020321     C                   MOVE      'F'           VIDFIE
020100020321     C                   OTHER
020200020321     C                   MOVEL     TAMFIE        VIDFIE
020300020321     C                   ENDSL
020400020321      *
020500940720     C                   MOVEL     TAMFTP        VIDFTP
020600941103     C*
020700941103     C* TIPO SERVIZIO
020800941103     C                   MOVEL     '5E'          COD
020900941103     C                   MOVEL(P)  TAMTSP        KEY
021000941103     C     KTAB          CHAIN     TABEL                              30
021100090608     C  N30              MOVEL     TBLUNI        ds5e
021200090608     C  N30              MOVEL     §5ed08        VIDTSP
021300941103     C   30              CLEAR                   VIDTSP
021400941103     C*
021500941103     C* BLOCCATA /ARRIVI /PARTENZE
021600941103     C                   CLEAR                   VIDBAP
021700941103     C     TAMBAP        IFEQ      'B'
021800941103     C                   MOVEL     'BLOC'        VIDBAP
021900941103     C                   ENDIF
022000941103     C     TAMBAP        IFEQ      'P'
022100941103     C                   MOVEL     'PART'        VIDBAP
022200941103     C                   ENDIF
022300941103     C     TAMBAP        IFEQ      'A'
022400941103     C                   MOVEL     'ARR '        VIDBAP
022500941103     C                   ENDIF
022600900329     C* COPIA DA OFFERTA
022700940927     C   10              EXSR      SCRIVI
022800051121
022900051121     c                   If        *in12 and wctr3  = savctr
023000051121     c                   If        conta = 0 and tamdst >= savdst
023100051121     c                   Exsr      scrivi
023200051121     c                   endif
023300061220     c**!!!              If        conta <> 0 and savdst <= dataoggi and
023400061220     c**!!!                        tamdst >= dataoggi
023500061220     c                   If        conta <> 0 and savdst <= wdatarif and
023600061220     c                             tamdst >= wdatarif
023700051121     c                   Exsr      scrivi
023800051121     c                   endif
023900051121     c                   endif
024000900330     C*
024100900329     C                   GOTO      LEGGI
024200900329     C                   END
024300900529     C* NON ESISTONO PROGRESSIVI PER QUESTA TARIFFA/OFFERTA
024400900529     C     NRR           IFEQ      0
024500900529     C                   GOTO      FINE
024600900529     C                   END
024700900323     C*
024800940720     C                   WRITE     TA36Z02
024900000000     C** COMANDI SUBFILE
025000000000     C     SU            TAG
025100940720     C                   EXFMT     TA36C01
025200900323     C** FINE LAVORO
025300900323     C   KL              GOTO      FINE
025400900323     C** CONTROLLO SE EFFETTUATA UNA SCELTA
025500940720     C                   READC     TA36S01                                30
025600900329     C   30              SETON                                        99
025700900329     C   30              GOTO      SU
025800900323     C*
025900900329     C                   MOVE      VIDCTR        CTR4
026000961111     C                   MOVE      VIDPRG        PARPRG
026100900329     C                   MOVEL     PARAM4        KPJBU
026200900323     C*
026300000000     C     FINE          TAG
026400000000     C                   SETON                                        LR
026500900323     C* PULIZIA SUBFILE ---------------------------------------------
026600900323     C     PULSF         BEGSR
026700000000     C                   SETON                                        84
026800940720     C                   WRITE     TA36C01
026900000000     C                   SETOFF                                       84
027000900319     C                   ENDSR
027100900330     C*
027200940720     C*--- SCRIVO SUBFILE --------------------------------------------*
027300900330     C     SCRIVI        BEGSR
027400940720     C* GIRO DATA DECORRENZA
027500091120     C                   Z-ADD     tamDDT        DATA
027600940720     C                   Z-ADD     MM            GGMM              4 0
027700940720     C                   MOVEL     GG            GGMM
027800091120     C                   Z-ADD     AA2           VIDDDT
027900940720     C                   MOVEL     GGMM          VIDDDT
028000940720     C* GIRO DATA SCADENZA
028100091120     C                   Z-ADD     tamDST        DATA
028200940720     C                   Z-ADD     MM            GGMM
028300940720     C                   MOVEL     GG            GGMM
028400091120     C                   Z-ADD     AA2           VIDDST
028500940720     C                   MOVEL     GGMM          VIDDST
028600940720     C* GIRO DATA ULTIMA VARIAZIONE
028700940720     C                   Z-ADD     VIDDUV        DATA
028800940720     C                   Z-ADD     MM            GGMM
028900940720     C                   MOVEL     GG            GGMM
029000940720     C                   Z-ADD     AA            VIDDUV
029100940720     C                   MOVEL     GGMM          VIDDUV
029200091120
029300091120      * Cerco se su cliente-offerta/trattativa-tariffa esiste la nota 10
029400091120     c                   eval      *in50 = *off
029500091120     c                   clear                   vidnota
029600091120     c                   select
029700091120     c                   when      flg4 = '3'
029800091120     c                   eval      kntcapl = 'T'
029900091120     c                   when      flg4 = '2'
030000091120     c                   eval      kntcapl = 'V'
030100091120     c                   other
030200091120     c                   eval      kntcapl = 'C'
030300091120     c                   endsl
030400091120     c                   movel(p)  ccc4          kntcnk1
030500091120     c                   move      cod4          kntcnk1
030600091120     c                   movel     tamctr        kntcnk2
030700091120     c                   movel     tamctr        kntcnk2
030800091120     c                   movel     '10'          kntctnt
030900091120     c     ktfntc        setll     tfntc01l
031000091120     c                   if        %equal(tfntc01l)
031100091120     c                   eval      vidnota = 'SI'
031200091120     c                   eval      *in50 = *on
031300091120     c                   else
031400091120     c                   clear                   kntcnk2
031500091120     c     ktfntc        setll     tfntc01l
031600091120     c                   if        %equal(tfntc01l)
031700091120     c                   eval      vidnota = 'SI'
031800091120     c                   eval      *in50 = *off
031900091120     c                   endif
032000091120     c                   endif
032100940720     C*
032200900330     C                   ADD       1             NRR
032300940720     C                   WRITE     TA36S01
032400940720     C*
032500900330     C                   ENDSR
032600991022     C*--- RICERCA ULTIMA DATA VARIAZIONE TARIFFA --------------------*
032700991022     C     CERDUV        BEGSR
032800991022     C*
032900991022     C* LEGGO TUTTE LE TESTATE TARIFFE PARTICOLARI  CERCANDO LA DATA
033000991022     C* MAGGIORE DI ULTIMA MODIFICA
033100991022     C     KTPT          SETLL     TITPT01L
033200991022     C     KTPT          READE     TITPT01L                               33
033300991022     C*
033400991022     C     *IN33         DOWEQ     *OFF
033500991022     C* VERIFICO SE LA DATA ULTIMA VARIAZIONE E' MAGGIORE DI QUELLA
033600991022     C* DELLA TESTATA
033700991022     C     TPTDUV        IFGT      TAMDUV
033800991022     C     TPTATB        ANDEQ     ' '
033900991022     C                   Z-ADD     TPTDUV        TAMDUV
034000991022     C                   ENDIF
034100991022     C*
034200991022     C     KTPT          READE     TITPT01L                               33
034300991022     C*
034400991022     C                   ENDDO
034500991022     C* VERIFICO ANCHE L'ULTIMA DATA VARIAZIONE TARIFFA CHE C'E' IN
034600991022     C* TITGC TARIFFE GIACENZE  (SE ESISTE)
034700991022     C     KTPT          CHAIN     TITGC01L                           33
034800991022     C     *IN33         IFEQ      *OFF
034900991022     C     TGCATB        ANDEQ     ' '
035000991022     C     TGCDUV        ANDGT     TAMDUV
035100991022     C                   Z-ADD     TGCDUV        TAMDUV
035200991022     C                   ENDIF
035300991022     C*
035400991022     C                   ENDSR
035500991022     C*
035600051121
035700051121      *------------------------------------------------------------------------*
035800051121      * Cerco il progrssivo in essere
035900051121      *------------------------------------------------------------------------*
036000051121     c     Cercadst      BegSr
036100051121
036200051121     c                   Clear                   savdst
036300051121     c                   Clear                   conta
036400051121
036500051121     c     ktam          Setll     Tntam01l
036600051121     c                   Do        *Hival
036700051121     c     ktam          Reade     Tntam01l
036800051121     c                   If        %Eof(tntam01l)
036900051121     c                   Leave
037000051121     c                   EndIf
037100051121     c                   If        tamatb <> *Blanks
037200051121     c                   Iter
037300051121     c                   EndIf
037400061220     c**!!!              If        tamdst <= dataoggi
037500061220     c                   If        tamdst <= wdatarif
037600051121     c                   Eval      savdst = tamdst
037700051121     c                   Else
037800051121     c                   add       1             conta
037900051121     c                   Leave
038000051121     c                   EndIf
038100051121
038200051121     c                   EndDo
038300051121
038400051121     c     ktam01        chain     tntam01l
038500051121
038600051121     c                   EndSr
038700991022     C*---------------------------------------------------------------*
