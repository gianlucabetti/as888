000100950110     H* TRTR47R *-----------------------------------------------------*
000200111110     H*  Ricezione volumi mamcanti in sede
000300941124     H*---------------------------------------------------------------*
000400930422     H* INDICATORI USATI                                              *
000500930422     H*---------------------------------------------------------------*
000600930806     H* 01 - MANCA SECONDA BOLLA
000700930806     H* 02 - MANCA PRIMA   BOLLA
000800930806     H* 05 - BOLLA TROVATA IN ARCHIVIO CORRENTE
000900930426     H* 06 - BOLLA TROVATA NELLO STORICO
001000930426     H* 07 - BOLLA TROVATA IN ARCHIVIO BOLLE ANNO PRECEDENTE
001100990702     H* 11 - LETTURA DI TITAS000
001200990702     H* 12 - LETTURA DI TITAS010
001300990702     H* 13 - LETTURA DI TITASP00
001400930423     H* 30 - LETTURA TABEL00F
001500930427     H* 31 - RICERCA FILIALE IN TABELLA £1
001600930423     H* 32 - CALL 'QCMDEXC'
001700950110     H* 33 - LETTURA FNARBV0R
001800990702     H* 38 - LETTURA DI TNBLA000
001900930806     H* 39/40 COMOD
002000930422     H*---------------------------------------------------------------*
002100930422     H* DATA BASE                                                     *
002200930422     H*---------------------------------------------------------------*
002300930422     H DECEDIT('0,') DATEDIT(*DMY.)
002400111110     Ffnarbv0f  iF   E           K DISK    prefix(M_)
002500941124     FTNBLA01L  IF   E           K DISK
002600111110     Ffnarb01l  IF   E           K DISK
002700111110     FTITAS30C  IF   E           K DISK
002800930423     FTABEL00F  IF   E           K DISK
002900070302     Fazorg01l  IF   E           K DISK
003000070202     Ffnblp01l  IF   E           K DISK    usropn
003100111110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003200930422     D*---------------------------------------------------------------*
003300930422     D* SCHIERE                                                       *
003400930422     D*---------------------------------------------------------------*
003500930426     D* DESCRIZIONE ERRORE
003600930528     D ERR             S             35    DIM(4) CTDATA PERRCD(1)
003700070202     d cmdt            s             60    dim(01) ctdata perrcd(1)
003800930422     D*---------------------------------------------------------------*
003900930422     D* DS                                                            *
004000930422     D*---------------------------------------------------------------*
004100930422     D KPJBA         E DS
004200930423     D  LEGFIL               483    492
004300930423     D  MBRFIL               493    502
004400960208     D***
004500960208     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004600960208     D***
004700960208     D WLBDAT          DS                  INZ
004800960208     D  G02DAT                 1      8  0
004900960208     D  G02INV                 9     16  0
005000960208     D  G02ERR                17     17
005100960208     D  G02TGI                18     22  0
005200960208     D***
005300960208     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
005400960208     D***
005500960208     D WGIDAT          DS                  INZ
005600960208     D  GIODAT                 1      8  0
005700960208     D  GIOINV                 9     16  0
005800960208     D  GIOTGI                17     21  0
005900930528     D DS3A          E DS
006000070302     D og143         E DS
006100070202     D tibs55ds      E DS
006200950111     D*
006300950111     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
006400950111     D DSLV20        E DS                  EXTNAME(FNLV20DS)
006500990702     D*
006600990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006700111110     d
006800111110     d commt           s             80
006900111110     d lenght          s             15  5 inz(80)
007000070202     i
007100990701     ITITAS000      11
007200990701     ITITAS010      12
007300990701     ITITASP00      13
007400930422     C*---------------------------------------------------------------*
007500930422     C*                    MAIN                                       *
007600930423     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI VOLUME              *
007700930422     C*---------------------------------------------------------------*
007800930528     C*
007900930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008000930528     C                   EXSR      DEFVAR
008100930528     C*
008200930528     C* APERTURA RIUSCITA
008300930423     C*
008400930423     C* LEGGO VARIAZIONE VOLUME
008500111110     C                   READ      fnarbv0f                               33
008600930423DO  2C     *IN33         DOWEQ     '0'
008700111110     c* Leggo le variazioni dal 4/11 al 5/11 alle ore 00,30
008800111110    3c                   if        m_arbdtv=20111104
008900111110     c                             or (m_arbdtv=20111105 and
009000111110     c                                 m_arborv<003000)
009100930423     C*
009200111110     c     kvdl          chain     fnarb01l
009300111110    4c                   if        %found(fnarb01l)
009400111110     c
009500930423     C* GESTIONE VARIAZIONI DA APPLICARE
009600930423     C                   EXSR      GESMOD
009700111110    4c                   endif
009800111110    3c                   endif
009900111110    c
010000111110     C                   READ      fnarbv0f                               33
010100930423E   2C                   ENDDO
010200990702     C*
010300990702     C                   MOVEL     'C'           D55TLA
010400990702     C                   CALL      'FNLV55R'
010500990702     C                   PARM                    DSLV55
010600930422     C*
010700111110     C                   EXCEPT    FINE
010800930423     C                   SETON                                        LR
010900930423     C*---------------------------------------------------------------*
011000930423     C*                    GESMOD                                     *
011100930423     C* ROUTINE DI GESTIONE MODIFICHE VOLUME DA APPLICARE             *
011200930423     C*---------------------------------------------------------------*
011300930423     C     GESMOD        BEGSR
011400930426     C*
011500930426     C                   SETOFF                                       050607
011600930528     C                   SETOFF                                       3108
011700930806     C                   SETOFF                                       0102
011800930806     C*
011900930806     C* LETTURA TIPO BOLLA
012000930528     C                   MOVEL     '3A'          COD
012100950111     C                   MOVEL(P)  ARBCBO        KEY
012200930528     C     KTAB          CHAIN     TABEL                              30
012300930528     C*
012400930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
012500930806IF  1C     *IN30         IFEQ      '1'
012600930528     C*
012700930806X   1C                   ELSE
012800930528     C*
012900930528     C                   MOVEL     TBLUNI        DS3A
013000950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
013100950905     C* CANCELLO SENZA DARE ERRORE
013200950905   1AC     §3ABSD        IFEQ      ' '
013300930806     C*
013400930806     C* 2   B O L L A
013500930806     C*
013600930806IF  2C     §3ATB2        IFNE      '  '
013700930806     C                   MOVEL     §3ATB2        TBL
013800930528     C*
013900930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
014000941129     C                   SETOFF                                       111213
014100990701     C     KTAS          CHAIN     TITAS30C                           37
014200930426     C*
014300930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
014400941129     C   37KTAS2         CHAIN     TNBLA01L                           38
014500941129     C   37
014600941129     CAN 38              SETON                                        01
014700941129    2C                   ENDIF
014800930426     C*
014900930806     C* 1   B O L L A
015000930806     C*
015100930806     C                   MOVEL     §3ATB1        TBL
015200930528     C*
015300930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
015400941129     C                   SETOFF                                       111213
015500990701     C     KTAS          CHAIN     TITAS30C                           37
015600930528     C*
015700930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
015800941129IF  2C     *IN37         IFEQ      '1'
015900941129     C     KTAS2         CHAIN     TNBLA01L                           38
016000941129     C   38              SETON                                        02
016100930528     C*
016200941129X   2C                   ELSE
016300941129     C* BOLLA TROVATA
016400941129     C   11              SETON                                        05
016500941129     C   12              SETON                                        06
016600941129     C   13              SETON                                        07
016700941129E   2C                   ENDIF
016800930806     C*
016900930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
017000930806     C   01
017100930806     COR 02              EXSR      MANBOL
017200930806     C*
017300930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
017400930806IF  2C     *IN08         IFEQ      '0'
017500070202     c
017600070202   2ac     wChiusainBLP  ifeq      ' '
017700930806     C*
017800930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
017900930528IF  3C     *IN05         IFEQ      '1'
018000930427     C     *IN06         OREQ      '1'
018100930427     C     *IN07         OREQ      '1'
018200111110     c* Aggiorno solo se volume diverso da quello di filiale
018300111110   3ac                   if        arbvlc<>tasvlc
018400930528     C                   EXSR      AGGIOR
018500930528     C*
018600930806IF  4C     *IN05         IFEQ      *ON
018700930806     C*
018800930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
018900930706     C*
019000111110     C**                 UPDATE    TITAS000
019100930806E   4C                   ENDIF
019200930706     C*
019300111110     C*+ 06              UPDATE    TITAS010
019400940208     C*
019500111110     C** 07              UPDATE    TITASP00
019600111110E  3aC                   ENDIF
019700111110E   3C                   ENDIF
019800930806     C*
019900941129     C* 2 BOLLA TROVATA IN ARCHIVIO
020000941129IF  3C     §3ATB2        IFNE      *BLANKS
020100941129     C     *IN01         ANDEQ     *OFF
020200941129     C*
020300930528     C                   MOVEL     §3ATB2        TBL
020400941129     C                   SETOFF                                       111213
020500990701     C     KTAS          CHAIN     TITAS30C                           30
020600930528     C*
020700941129    4C     *IN30         IFEQ      *OFF
020800930528     C                   EXSR      AGGIOR
020900930528     C*
021000111110     C** 11              UPDATE    TITAS000
021100111110     C** 12              UPDATE    TITAS010
021200111110     C** 13              UPDATE    TITASP00
021300941129E   4C                   ENDIF
021400941129E   3C                   ENDIF
021500070202E  2aC                   ENDIF
021600930528     C*
021700070202     C*
021800930806E   2C                   ENDIF
021900950905     C*
022000950905X  1AC                   ELSE
022100950905E  1AC                   ENDIF
022200950905E   1C                   ENDIF
022300930806     C*
022400930528     C                   ENDSR
022500930528     C*---------------------------------------------------------------*
022600930528     C*                    AGGIOR                                     *
022700930806     C* ROUTINE DI AGGIORNAMENTO VOLUME SULLA BOLLA                   *
022800930528     C*---------------------------------------------------------------*
022900930528     C     AGGIOR        BEGSR
023000930426     C*
023100930426     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO VOLUME UGUALE A 'T'
023200970625    1C******     TASFVF    IFNE 'T'
023300930528     C*
023400930806     C                   MOVEL     TASTBL        BOL1              1
023500930806     C                   MOVE      TASTBL        BOL2              1
023600930426     C                   MOVE      TASKSC        CODCLI            4 0
023700930806     C*
023800950111     C                   CLEAR                   DSLV20
023900950111     C*
024000950111     C* SE E' LA PRIMA BOLLA
024100950111    2C     TBL           IFEQ      §3ATB1
024200950111     C*
024300950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
024400950111    3C     BOL2          IFEQ      '7'
024500950111     C     BOL2          OREQ      'C'
024600950111     C     BOL2          OREQ      'S'
024700950111     C     BOL1          OREQ      'A'
024800950111     C     CODCLI        ANDEQ     9999
024900950111     C*
025000950111     C* NON PASSO LA DATA FATTURA
025100950111     C                   CLEAR                   D20DFT
025200950111     C*
025300950111   X3C                   ELSE
025400950111     C                   MOVEL     TASDFT        D20DFT
025500950111    3C                   ENDIF
025600930623     C*
025700950111   X2C                   ELSE
025800950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
025900950111     C                   CLEAR                   D20DFT
026000950111    2C                   ENDIF
026100930623     C*
026200950111     C                   MOVEL     'C'           D20TVL
026300950111     C                   MOVEL     ARBNCR        D20NCR
026400950111     C                   MOVEL     ARBVLC        D20VLU
026500950111     C                   MOVEL     TASFVF        D20FVF
026600950111     C                   MOVEL     TASVLF        D20VLF
026700950111     C                   MOVEL     TASFVB        D20FVB
026800950111     C                   MOVEL     TASVLB        D20VLB
026900950111     C                   MOVEL     TASAAS        D20AAS
027000950111     C                   MOVEL     TASLNP        D20LNP
027100950111     C                   MOVEL     TASNRS        D20NRS
027200950111     C                   MOVEL     TASNSP        D20NSP
027300950111     C                   MOVEL     ARBCBO        D20CBO
027400950111     C                   MOVEL     TASNCL        D20NCL
027500950111     C                   MOVEL     TASLNA        D20LNA
027600950111     C                   MOVEL     NUM3          D20MBR
027700960927     C                   MOVE      TASMGS        D20DTE
027800960927     C                   MOVEL     TASAAS        D20DTE
027900970904     C* SE IL VOLUME CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
028000970904     C*  COLLI RILEVATI, LI METTO UGUALI
028100111110     C**   ARBFVC        IFEQ      'T'
028200111110     C**   ARBNCR        ANDNE     TASNCL
028300111110     C**                 Z-ADD     TASNCL        D20NCR
028400111110     C**                 ENDIF
028500950111     C*
028600950111     C                   MOVEL     DSLV20        KPJBU
028700950111     C*
028800950111     C                   CALL      'FNLV20R'
028900950111     C                   PARM                    KPJBA
029000950111     C                   MOVEL     KPJBU         DSLV20
029100950111     C*
029200930623     C*
029300990702     C* SE ERRORE SCRIVO FILE ERRORI
029400950111    2C     D20ERR        IFEQ      '2'
029500950111     C*
029600930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
029700950111    3C     BOL1          IFEQ      'F'
029800950111    3C                   ELSE
029900930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
030000990702     C                   CLEAR                   DSLV55
030100990702     C                   MOVEL     'P'           D55TPT
030200990702     C                   Z-ADD     TASLNA        D55LIN
030300990702     C                   Z-ADD     DATEU         D55DRF
030400990702     C                   CALL      'FNLV55R'
030500990702     C                   PARM                    DSLV55
030600950111    3C                   ENDIF
030700111110     c                   except    err2
030800111110     c                   add       1             toterr2           9 0
030900930623     C*
031000950111    2C                   ENDIF
031100990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
031200990702     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV20R
031300111110     c                   except    variaz
031400111110     c                   add       1             totvar            9 0
031500111110     c
031600950111     C                   MOVEL     D20NCR        TASNCR
031700950111     C                   MOVEL     D20VLU        TASVLC
031800950111     C                   MOVEL     D20FVF        TASFVF
031900950111     C                   MOVEL     D20VLF        TASVLF
032000970625    1C***********          ENDIF
032100930426     C*
032200930423     C                   ENDSR
032300930806     C*---------------------------------------------------------------*
032400930806     C*                    MANBOL                                     *
032500930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
032600930806     C*---------------------------------------------------------------*
032700930806     C     MANBOL        BEGSR
032800930806     C*
032900930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
033000930806IF  1C     *IN01         IFEQ      *ON
033100930806     C     *IN02         ANDEQ     *ON
033200070202     c                   exsr      CERBLP
033300070202     c
033400070202     c* Non trovata nemmeno in blp come merce mai affidata
033500070202   1ac                   if        wChiusainBLP=' '
033600960208     C                   SETON                                        08
033700960208     C*
033800960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
033900930806     C*
034000111110     c                   add       1             toterr1           9 0
034100111110     C                   EXcept    err1
034200070202   1aC                   ENDIF
034300930806     C*
034400930806X   1C                   ELSE
034500930806     C*
034600930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
034700930806IF  2C     *IN01         IFEQ      *ON
034800930806     C     *IN02         OREQ      *ON
034900930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
035000930806IF  3C     *IN01         IFEQ      *ON
035100930806     C                   MOVEL     §3ATB1        ALF2
035200930806     C                   EXSR      RICBOL
035300930806     C*
035400930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
035500960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
035600930806IF  4C     TIBOL         IFEQ      *BLANKS
035700111110     C                   EXcept    err4
035800111110     c                   add       1             toterr4           9 0
035900930806X   4C                   ELSE
036000930806     C                   MOVEL     TIBOL         §3ATB2
036100990702     C                   SETOFF                                       01
036200930806E   4C                   ENDIF
036300990702     C* RICHAINO LA PRIMA BOLLA
036400990702     C                   MOVEL     §3ATB1        TBL
036500990702     C                   SETOFF                                       111213
036600990702     C     KTAS          CHAIN     TITAS30C                           37
036700930806     C*
036800930806X   3C                   ELSE
036900930806     C* N O N   T R O V AT A   P R I M A   B O L L A
037000930806     C                   MOVEL     §3ATB2        ALF2              2
037100930806     C                   EXSR      RICBOL
037200930806     C*
037300930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
037400930806IF  4C     TIBOL         IFEQ      *BLANKS
037500070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
037600070202     c*  cancello senza dare erorre
037700070202     c                   exsr      CERBLP
037800070202     c
037900070202     c* Non trovata nemmeno in blp come merce mai affidata
038000070202    5c                   if        wChiusainBLP=' '
038100070202     c
038200960208     C                   SETON                                        08
038300111110     C                   EXcept    err1
038400111110     c                   add       1             toterr1           9 0
038500070202E   5C                   ENDIF
038600070202     c
038700930806X   4C                   ELSE
038800941129     C   11              SETON                                        05
038900941129     C   12              SETON                                        06
039000941129     C   13              SETON                                        07
039100930806     C                   MOVEL     TIBOL         §3ATB1
039200070202E   4C                   ENDIF
039300930806E   3C                   ENDIF
039400930806E   2C                   ENDIF
039500930806E   1C                   ENDIF
039600930806     C*
039700930806     C                   ENDSR
039800070202     C*---------------------------------------------------------------*
039900070202     C*                    CERBLP                                     *
040000070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
040100070202     c* merce mai affdata
040200070202     C*---------------------------------------------------------------*
040300070202     C     CERBLP        BEGSR
040400070202     c                   clear                   wChiusainBLP      1
040500070202     c     ktas2         chain     fnblp01l
040600070202    5c                   if        %found(fnblp01l) and blpft1='N'
040700070302     c                   if        blpcca='7'
040800070202     c                   eval      wChiusainBLP='S'
040900070302     c                   else
041000070302     c* Verifico se si tratta di import
041100070302     c     blplnp        chain     azorg01l
041200070302If   c                   If        %Found(Azorg01l)
041300070302     c                   Movel     OrgDe3        og143
041400070302     c                   Else
041500070302     c                   Clear                   og143
041600070302     c                   EndIf
041700070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
041800070302     c                   eval      wChiusainBLP='S'
041900070302     c                   EndIf
042000070302     C
042100070302     c                   EndIf
042200070302     c                   EndIf
042300070202     c                   ENDSR
042400930806     C*---------------------------------------------------------------*
042500930806     C*                    RICBOL                                     *
042600930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
042700930806     C*---------------------------------------------------------------*
042800930806     C     RICBOL        BEGSR
042900930806     C*
043000930806     C                   SETOFF                                       3940
043100941129     C                   SETOFF                                       111213
043200930806     C                   CLEAR                   TIBOL
043300930806     C*
043400990701     C     KTAS2         SETLL     TITAS30C
043500990701     C     KTAS2         READE     TITAS30C                               39
043600930806     C*
043700930806DO  1C     *IN39         DOWEQ     *OFF
043800930806     C*
043900930806IF  2C     TASTBL        IFNE      ALF2
044000930806     C                   SETON                                        39
044100930806     C                   MOVEL     TASTBL        TIBOL             2
044200930806E   2C                   ENDIF
044300930806     C*
044400990701     C  N39KTAS2         READE     TITAS30C                               39
044500930806E   1C                   ENDDO
044600930806     C*
044700930806     C*
044800930806     C                   ENDSR
044900930422     C*---------------------------------------------------------------*
045000930422     C*                    DEFVAR                                     *
045100930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
045200930422     C*---------------------------------------------------------------*
045300930422     C     DEFVAR        BEGSR
045400930422     C*
045500930422     C     *ENTRY        PLIST
045600930422     C                   PARM                    KPJBA
045700930422     C*
045800950111     C                   Z-ADD     1             CODUT             1 0
045900930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
046000930423     C                   MOVEL     MBRFIL        ALF4              4
046100930423     C                   MOVE      ALF4          NUM3              3 0
046200960208     C* UDATE - 3
046300960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
046400960208     C                   TIME                    W0140            14 0
046500960208     C* UDATE IN GGMMAAAA
046600960208     C                   MOVE      W0140         WDTGIO            8 0
046700960208     C*
046800960208     C* UDATE IN AAAAMMGG
046900960208     C                   Z-ADD     WDTGIO        G02DAT
047000960208     C                   MOVEL     *BLANK        G02ERR
047100960208     C                   CALL      'XSRDA8'
047200960208     C                   PARM                    WLBDAT
047300960208     C                   MOVEL     G02INV        DATEU             8 0
047400960208     C* SOTTRAGGO  3
047500960208     C     G02TGI        SUB       3             GIOTGI
047600960208     C*
047700960208     C                   CALL      'XSRGI8'
047800960208     C                   PARM                    WGIDAT
047900960208     C*
048000960208     C                   Z-ADD     GIOINV        DATAM             8 0
048100070202     c* Apro file bolle di filiale
048200070202     c                   clear                   tibs55ds
048300070202     c                   eval      i50tla='L'
048400070202     c                   eval      i50apo=001
048500070202     c                   call      'TIBS55R'
048600070202     c                   parm                    tibs55ds
048700070202     c*
048800070202     c                   Clear                   commt
048900070202     c                   Movel(p)  cmdt(1)       commt
049000070202     c                   Eval      %Subst(commt:30:10) = o50ala
049100070202     c                   Eval      commt =
049200070202     c                             %trim(commt) + '/FNBLP01L)'
049300070202     c                   Call      'QCMDEXC'
049400070202     c                   Parm                    commt
049500070202     c                   Parm                    lenght
049600070202     c
049700070202     c                   Open      FNBLP01L
049800950111     C*
049900950111     C* DEFINIZIONE CAMPI
050000950111     C     *LIKE         DEFINE    TBLCOD        COD
050100950111     C     *LIKE         DEFINE    TBLKEY        KEY
050200930422     C*
050300950111     C* ACCESSO TABEL00F
050400930423     C     KTAB          KLIST
050500950111     C                   KFLD                    CODUT
050600950111     C                   KFLD                    COD
050700950111     C                   KFLD                    KEY
050800990701     C* ACCESSO TITAS30C
050900930426     C     KTAS          KLIST
051000950111     C                   KFLD                    ARBAAS
051100950111     C                   KFLD                    ARBLNP
051200950111     C                   KFLD                    ARBNRS
051300950111     C                   KFLD                    ARBNSP
051400930528     C                   KFLD                    TBL               2
051500990701     C* ACCESSO TITAS30C/TNBLA01L
051600930528     C     KTAS2         KLIST
051700950111     C                   KFLD                    ARBAAS
051800950111     C                   KFLD                    ARBLNP
051900950111     C                   KFLD                    ARBNRS
052000950111     C                   KFLD                    ARBNSP
052100111110     C     kvdl          KLIST
052200111110     C                   KFLD                    M_arbAAS
052300111110     C                   KFLD                    M_arbLNP
052400111110     C                   KFLD                    M_arbNRS
052500111110     C                   KFLD                    M_arbNSP
052600930423     C*
052700930423     C                   ENDSR
052800930423     C*---------------------------------------------------------------*
052900930423     C*                    QCMDEXC                                    *
053000930423     C*---------------------------------------------------------------*
053100111110     OQSYSPRT   E            err1        1
053200111110     O                                         +  1 'bolla insistente1'
053300111110     O                       arbaas        z   +  1
053400111110     O                       arblnp        z   +  1
053500111110     O                       arbnrs        z   +  1
053600111110     O                       arbnsp        z   +  1
053700111110     OQSYSPRT   E            err4        1
053800111110     O                                         +  1 'bolla insistente2'
053900111110     O                       arbaas        z   +  1
054000111110     O                       arblnp        z   +  1
054100111110     O                       arbnrs        z   +  1
054200111110     O                       arbnsp        z   +  1
054300111110     OQSYSPRT   E            err2        1
054400111110     O                                         +  1 'bolla già tassata'
054500111110     O                       arbaas        z   +  1
054600111110     O                       arblnp        z   +  1
054700111110     O                       arbnrs        z   +  1
054800111110     O                       arbnsp        z   +  1
054900111110     OQSYSPRT   E            variaz      1
055000111110     O                                         +  1 'bolla da variare'
055100111110     O                       arbaas        z   +  1
055200111110     O                       arblnp        z   +  1
055300111110     O                       arbnrs        z   +  1
055400111110     O                       arbnsp        z   +  1
055500111110     o                                         +  2 'colli'
055600111110     o                       tasncl        2   +  1
055700111110     o                                         +  2 'TASVLF'
055800111110     o                       tasfvf            +  1
055900111110     o                       tasvlf        2   +  1
056000111110     o                                         +  2 'VDL da mettere'
056100111110     o                       d20ncr        2   +  1
056200111110     o                       d20vlu        2   +  1
056300111110     o                                         +  2 'NEW fatt'
056400111110     o                       d20fvf            +  1
056500111110     o                       d20vlf        2   +  1
056600111110     OQSYSPRT   E            FINE        1
056700111110     O                                         +  1 'TOT DA VARIARE'
056800111110     o                       TOTVAR        2   +  1
056900111110     O                                         +  1 'TOT bolla 1 ines'
057000111110     o                       TOTerr1       2   +  1
057100111110     O                                         +  1 'TOT bolla 2 ines'
057200111110     o                       TOTerr4       2   +  1
057300111110     O                                         +  1 'Bolla tassata'
057400111110     o                       TOTerr2       2   +  1
057500940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
057600930426**         ERR
0577009305281 SPEDIZIONE INESISTENTE
057800950222SPEDIZIONE GIA' TASSATA
057900930528CODICE BOLLA INESISTENTE
0580009305282 SPEDIZIONE INESISTENTE
058100070202** cmdt
058200070202OVRDBF FILE(FNBLP01L) TOFILE(
