000100990805     H* TRTR51R *-----------------------------------------------------*
000200990805     H*            - RICEZIONE VARIAZIONI PESO   IN SEDE -            *
000300930422     H* INDICATORI USATI                                              *
000400930422     H*---------------------------------------------------------------*
000500930806     H* 01 - MANCA SECONDA BOLLA
000600930806     H* 02 - MANCA PRIMA   BOLLA
000700930806     H* 05 - BOLLA TROVATA IN ARCHIVIO CORRENTE
000800930426     H* 06 - BOLLA TROVATA NELLO STORICO
000900930426     H* 07 - BOLLA TROVATA IN ARCHIVIO BOLLE ANNO PRECEDENTE
001000990702     H* 11 - LETTURA DI TITAS000
001100990702     H* 12 - LETTURA DI TITAS010
001200990702     H* 13 - LETTURA DI TITASP00
001300930423     H* 30 - LETTURA TABEL00F
001400930427     H* 31 - RICERCA FILIALE IN TABELLA £1
001500930423     H* 32 - CALL 'QCMDEXC'
001600990805     H* 33 - LETTURA FNARBP0R
001700990702     H* 38 - LETTURA DI TNBLA000
001800930806     H* 39/40 COMOD
001900930422     H*---------------------------------------------------------------*
002000930422     H* DATA BASE                                                     *
002100930422     H*---------------------------------------------------------------*
002200930422     H DECEDIT('0,') DATEDIT(*DMY.)
002300941124     FTNBLA01L  IF   E           K DISK
002400111114     FTITAS30C  uF   E           K DISK
002500111114     FFIARBE2C  iF   E           K DISK
002600990805     F                                     RENAME(FNARBP00:FNARBP0E)
002700930423     FTABEL00F  IF   E           K DISK
002800070302     Fazorg01l  IF   E           K DISK
002900070202     Ffnblp01l  IF   E           K DISK    usropn
003000111111     Ffnarb01l  IF   E           K DISK
003100111114     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
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
006300990805     D* DS PER FNLV39R - DS DEI CAMPI PER CALCOLO PESO   DA FATTURARE
006400990805     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006500990702     D*
006600990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006700070202     d commt           s             80
006800070202     d lenght          s             15  5 inz(80)
006900111123     d
007000111123     d Dataiso         s               d   datfmt(*iso)
007100111123     d Datasys         s               d   datfmt(*iso)  inz(*sys)
007200111123     i
007300070202     i
007400990701     ITITAS000      11
007500990701     ITITAS010      12
007600990701     ITITASP00      13
007700930422     C*---------------------------------------------------------------*
007800930422     C*                    MAIN                                       *
007900990805     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI PESO                *
008000930422     C*---------------------------------------------------------------*
008100930528     C*
008200930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008300930528     C                   EXSR      DEFVAR
008400930528     C*
008500930528     C*
008600930528     C* APERTURA RIUSCITA
008700930423     C*
008800990805     C* LEGGO VARIAZIONE PESO
008900111111     c                   z-add     2011          kaas              4 0
009000111123     c                   z-add     2011          kaas              4 0
009100111123     c                   eval      dataiso=datasys
009200111123     c                   subdur    2:*d          dataiso
009300111123     c                   movel     dataiso       w0080             8 0
009400111123     c                   move      w0080         kmgfine           4 0
009500111123     c
009600111111     C*
009700111111     C* APERTURA RIUSCITA
009800111111     C*
009900111111     C* LEGGO VARIAZIONE VOLUME
010000111111     C     kaas          setll     fnarb01l
010100111111     C     kaas          READe     fnarb01l                               33
010200930423DO  2C     *IN33         DOWEQ     '0'
010300930423     C*
010400930423     C* GESTIONE VARIAZIONI DA APPLICARE
010500111111     c
010600111123    3c                   if        arbmgs>=1110 and arbmgs<=kmgfine
010700930423     C                   EXSR      GESMOD
010800111111     c                   endif
010900111111    c
011000111111     C     kaas          READe     fnarb01l                               33
011100111111     c
011200930423E   2C                   ENDDO
011300930423     C*
011400990702     C*
011500990702     C                   MOVEL     'C'           D55TLA
011600990702     C                   CALL      'FNLV55R'
011700990702     C                   PARM                    DSLV55
011800930422     C*
011900111114     C                   EXCEPT    FINE
012000930423     C                   SETON                                        LR
012100930423     C*---------------------------------------------------------------*
012200930423     C*                    GESMOD                                     *
012300990805     C* ROUTINE DI GESTIONE MODIFICHE PESO   DA APPLICARE             *
012400930423     C*---------------------------------------------------------------*
012500930423     C     GESMOD        BEGSR
012600930426     C*
012700930426     C                   SETOFF                                       050607
012800930528     C                   SETOFF                                       3108
012900930806     C                   SETOFF                                       0102
013000111123     c                   clear                   Wchiusainblp
013100930806     C*
013200930806     C* LETTURA TIPO BOLLA
013300930528     C                   MOVEL     '3A'          COD
013400950111     C                   MOVEL(P)  ARBCBO        KEY
013500930528     C     KTAB          CHAIN     TABEL                              30
013600930528     C*
013700930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
013800930806IF  1C     *IN30         IFEQ      '1'
013900111111     C                   Z-ADD     102           FEV               3 0
014000111114     C                   MOVEL     ERR(3)        DER
014100930528     C*
014200930806X   1C                   ELSE
014300930528     C*
014400930528     C                   MOVEL     TBLUNI        DS3A
014500950905     C* SE SI TRATTA DI UNA BOLLA NON TRASMESSA IN SEDE
014600950905     C* CANCELLO SENZA DARE ERRORE
014700950905   1AC     §3ABSD        IFEQ      ' '
014800930806     C*
014900930806     C* 2   B O L L A
015000930806     C*
015100930806IF  2C     §3ATB2        IFNE      '  '
015200930806     C                   MOVEL     §3ATB2        TBL
015300930528     C*
015400930426     C* CERCO BOLLA IN ARCHIVIO CORRENTE
015500941129     C                   SETOFF                                       111213
015600111124     c                   if        Aggiorna='S'
015700990701     C     KTAS          CHAIN     TITAS30C                           37
015800111124     c                   else
015900111124     C     KTAS          CHAIN(N)  TITAS30C                           37
016000111124     c                   endif
016100930426     C*
016200930426     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
016300941129     C   37KTAS2         CHAIN     TNBLA01L                           38
016400941129     C   37
016500941129     CAN 38              SETON                                        01
016600941129    2C                   ENDIF
016700930426     C*
016800930806     C* 1   B O L L A
016900930806     C*
017000930806     C                   MOVEL     §3ATB1        TBL
017100930528     C*
017200930528     C* CERCO BOLLA IN ARCHIVIO CORRENTE
017300941129     C                   SETOFF                                       111213
017400111124     c                   if        Aggiorna='S'
017500990701     C     KTAS          CHAIN     TITAS30C                           37
017600111124     c                   else
017700111124     C     KTAS          CHAIN(n)  TITAS30C                           37
017800111124     c                   endif
017900930528     C*
018000930528     C* SE NON LA TROVO CERCO NELLE BOLLE ANNULLATE
018100941129IF  2C     *IN37         IFEQ      '1'
018200941129     C     KTAS2         CHAIN     TNBLA01L                           38
018300941129     C   38              SETON                                        02
018400930528     C*
018500941129X   2C                   ELSE
018600941129     C* BOLLA TROVATA
018700941129     C   11              SETON                                        05
018800941129     C   12              SETON                                        06
018900941129     C   13              SETON                                        07
019000941129E   2C                   ENDIF
019100930806     C*
019200930806     C* CONTROLLO SE MANCANO VERAMENTE O E' CAMBIATO IL TIPO BOLLA
019300930806     C   01
019400930806     COR 02              EXSR      MANBOL
019500930806     C*
019600930806     C* SE PRIMA BOLLA INESISTENTE ESCO E NON AGGIORNO
019700930806IF  2C     *IN08         IFEQ      '0'
019800070202     c
019900070202     c* Se chiusa in blp non stampo ma cancello in fnarbv0r
020000070202   2ac     wChiusainBLP  ifeq      ' '
020100930806     C*
020200930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
020300930528IF  3C     *IN05         IFEQ      '1'
020400111124   3ac                   if        arbpkc >taspkc and tasfic=' '
020500930528     C                   EXSR      AGGIOR
020600930528     C*
020700930806IF  4C     *IN05         IFEQ      *ON
020800111118     c                   if        Aggiorna='S'
020900111114     C                   UPDATE    TITAS000
021000111114E  3aC                   ENDIF
021100111118E  3aC                   ENDIF
021200111114E   4C                   ENDIF
021300930806E   3C                   ENDIF
021400930806     C*
021500941129     C* 2 BOLLA TROVATA IN ARCHIVIO
021600941129IF  3C     §3ATB2        IFNE      *BLANKS
021700941129     C     *IN01         ANDEQ     *OFF
021800941129     C*
021900930528     C                   MOVEL     §3ATB2        TBL
022000941129     C                   SETOFF                                       111213
022100111124     c                   if        Aggiorna='S'
022200990701     C     KTAS          CHAIN     TITAS30C                           30
022300111124     c                   else
022400111124     C     KTAS          CHAIN(n)  TITAS30C                           30
022500111124     c                   endif
022600930528     C*
022700941129    4C     *IN30         IFEQ      *OFF
022800111124   3ac                   if        arbpkc >taspkc and tasfic=' '
022900930528     C                   EXSR      AGGIOR
023000930528     C*
023100111118     c                   if        Aggiorna='S'
023200111114     C   11              UPDATE    TITAS000
023300941129E   4C                   ENDIF
023400111118E   4C                   ENDIF
023500111118     c
023600111118E   4C                   ENDIF
023700941129E   3C                   ENDIF
023800070202E  2aC                   ENDIF
023900111114E   2C                   ENDIF
024000930528     C*
024100950905E  1AC                   ENDIF
024200950905E   1C                   ENDIF
024300930806     C*
024400930528     C                   ENDSR
024500930528     C*---------------------------------------------------------------*
024600930528     C*                    AGGIOR                                     *
024700990805     C* ROUTINE DI AGGIORNAMENTO PESO   SULLA BOLLA                   *
024800930528     C*---------------------------------------------------------------*
024900930528     C     AGGIOR        BEGSR
025000930426     C*
025100990805     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO PESO   UGUALE A 'T'
025200970625    1C******     TASFVF    IFNE 'T'
025300930528     C*
025400930806     C                   MOVEL     TASTBL        BOL1              1
025500930806     C                   MOVE      TASTBL        BOL2              1
025600930426     C                   MOVE      TASKSC        CODCLI            4 0
025700930806     C*
025800990805     C                   CLEAR                   DSLV39
025900950111     C*
026000950111     C* SE E' LA PRIMA BOLLA
026100950111    2C     TBL           IFEQ      §3ATB1
026200950111     C*
026300950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
026400950111    3C     BOL2          IFEQ      '7'
026500950111     C     BOL2          OREQ      'C'
026600950111     C     BOL2          OREQ      'S'
026700950111     C     BOL1          OREQ      'A'
026800950111     C     CODCLI        ANDEQ     9999
026900950111     C*
027000950111     C* NON PASSO LA DATA FATTURA
027100990805     C                   CLEAR                   D39DFT
027200950111     C*
027300950111   X3C                   ELSE
027400990805     C                   MOVEL     TASDFT        D39DFT
027500950111    3C                   ENDIF
027600930623     C*
027700950111   X2C                   ELSE
027800950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
027900990805     C                   CLEAR                   D39DFT
028000950111    2C                   ENDIF
028100930623     C*
028200990805     C                   MOVEL     'C'           D39TPP
028300990921     C                   MOVEL     ARBNCP        D39NCP
028400990805     C                   MOVEL     ARBPKC        D39PLU
028500990805     C                   MOVEL     TASFPF        D39FPF
028600990805     C                   MOVEL     TASPKF        D39PLF
028700990805     C                   MOVEL     TASFPB        D39FPB
028800990805     C                   MOVEL     TASPKB        D39PLB
028900990805     C                   MOVEL     TASAAS        D39AAS
029000990805     C                   MOVEL     TASLNP        D39LNP
029100990805     C                   MOVEL     TASNRS        D39NRS
029200990805     C                   MOVEL     TASNSP        D39NSP
029300990805     C                   MOVEL     ARBCBO        D39CBO
029400990805     C                   MOVEL     TASNCL        D39NCL
029500990805     C                   MOVEL     TASLNA        D39LNA
029600990805     C                   MOVEL     NUM3          D39MBR
029700990805     C                   MOVE      TASMGS        D39DTE
029800990805     C                   MOVEL     TASAAS        D39DTE
029900990805     C* SE IL PESO   CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030000970904     C*  COLLI RILEVATI, LI METTO UGUALI
030100111111     C**   ARBFPC        IFEQ      'T'
030200111111     C*+   ARBNCP        ANDNE     TASNCL
030300111111     C*+                 Z-ADD     TASNCL        D39NCP
030400111111     C**                 ENDIF
030500950111     C*
030600990805     C                   MOVEL     DSLV39        KPJBU
030700950111     C*
030800990805     C                   CALL      'FNLV39R'
030900950111     C                   PARM                    KPJBA
031000990805     C                   MOVEL     KPJBU         DSLV39
031100950111     C*
031200930623     C*
031300990702     C* SE ERRORE SCRIVO FILE ERRORI
031400990805    2C     D39ERR        IFEQ      '2'
031500950111     C*
031600930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
031700950111    3C     BOL1          IFEQ      'F'
031800990701     C                   Z-ADD     TASTFP        FEV
031900950111    3C                   ELSE
032000930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032100990702     C                   CLEAR                   DSLV55
032200990702     C                   MOVEL     'P'           D55TPT
032300990702     C                   Z-ADD     TASLNA        D55LIN
032400990702     C                   Z-ADD     DATEU         D55DRF
032500990702     C                   CALL      'FNLV55R'
032600990702     C                   PARM                    DSLV55
032700990702     C                   Z-ADD     D55TFP        FEV
032800950111    3C                   ENDIF
032900950111     C                   MOVEL     ERR(2)        DER
033000111114     c                   except    err2
033100111114     c                   add       1             toterr2           9 0
033200930623     C*
033300930623     C* SCRIVO IL FILE DEGLI ERRORI
033400950111    2C                   ENDIF
033500111114
033600111114     c                   except    variaz
033700111114     c                   add       1             totvarNO          9 0
033800111114     c
033900990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
034000990805     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV39R
034100990805     C                   MOVEL     D39NCP        TASNCP
034200990805     C                   MOVEL     D39PLU        TASPKC
034300111114     C**                 MOVEL     D39FPF        TASFPF
034400111114     C**                 MOVEL     D39PLF        TASPKF
034500970625    1C***********          ENDIF
034600930426     C*
034700930706     C                   ENDSR
034800930806     C*---------------------------------------------------------------*
034900930806     C*                    MANBOL                                     *
035000930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
035100930806     C*---------------------------------------------------------------*
035200930806     C     MANBOL        BEGSR
035300930806     C*
035400930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
035500930806IF  1C     *IN01         IFEQ      *ON
035600930806     C     *IN02         ANDEQ     *ON
035700070202     c                   exsr      CERBLP
035800070202     c
035900070202     c* Non trovata nemmeno in blp come merce mai affidata
036000070202   1ac                   if        wChiusainBLP=' '
036100960208     C                   SETON                                        08
036200111114     c                   add       1             toterr1           9 0
036300111114     C                   EXcept    err1
036400960208     C*
036500070202   1aC                   ENDIF
036600930806     C*
036700930806X   1C                   ELSE
036800930806     C*
036900930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
037000930806IF  2C     *IN01         IFEQ      *ON
037100930806     C     *IN02         OREQ      *ON
037200930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
037300930806IF  3C     *IN01         IFEQ      *ON
037400930806     C                   MOVEL     §3ATB1        ALF2
037500930806     C                   EXSR      RICBOL
037600930806     C*
037700930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
037800960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
037900930806IF  4C     TIBOL         IFEQ      *BLANKS
038000111114     C                   EXcept    err4
038100111114     c                   add       1             toterr4           9 0
038200930806X   4C                   ELSE
038300930806     C                   MOVEL     TIBOL         §3ATB2
038400990702     C                   SETOFF                                       01
038500930806E   4C                   ENDIF
038600990805     C* RICHIAMO LA PRIMA BOLLA
038700990702     C                   MOVEL     §3ATB1        TBL
038800990702     C                   SETOFF                                       111213
038900111124     c                   if        Aggiorna='S'
039000990702     C     KTAS          CHAIN     TITAS30C                           37
039100111124     c                   else
039200111124     c     KTAS          CHAIN(n)  TITAS30C                           37
039300111124     c                   endif
039400930806     C*
039500930806X   3C                   ELSE
039600990805     C* N O N   T R O V A T A   P R I M A   B O L L A
039700930806     C                   MOVEL     §3ATB2        ALF2              2
039800930806     C                   EXSR      RICBOL
039900930806     C*
040000930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
040100930806IF  4C     TIBOL         IFEQ      *BLANKS
040200070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
040300070202     c*  cancello senza dare erorre
040400070202     c                   exsr      CERBLP
040500070202     c
040600070202     c* Non trovata nemmeno in blp come merce mai affidata
040700070202    5c                   if        wChiusainBLP=' '
040800111114     C                   SETON                                        08
040900111114     C                   EXcept    err1
041000111114     c                   add       1             toterr1           9 0
041100070202     c
041200070202    5C                   ENDIF
041300070202     c
041400930806X   4C                   ELSE
041500941129     C   11              SETON                                        05
041600941129     C   12              SETON                                        06
041700941129     C   13              SETON                                        07
041800930806     C                   MOVEL     TIBOL         §3ATB1
041900930806E   4C                   ENDIF
042000930806E   3C                   ENDIF
042100930806E   2C                   ENDIF
042200930806E   1C                   ENDIF
042300930806     C*
042400930806     C                   ENDSR
042500930806     C*---------------------------------------------------------------*
042600930806     C*                    RICBOL                                     *
042700930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
042800930806     C*---------------------------------------------------------------*
042900930806     C     RICBOL        BEGSR
043000930806     C*
043100930806     C                   SETOFF                                       3940
043200941129     C                   SETOFF                                       111213
043300930806     C                   CLEAR                   TIBOL
043400930806     C*
043500990701     C     KTAS2         SETLL     TITAS30C
043600990701     C     KTAS2         READE     TITAS30C                               39
043700930806     C*
043800930806DO  1C     *IN39         DOWEQ     *OFF
043900930806     C*
044000930806IF  2C     TASTBL        IFNE      ALF2
044100930806     C                   SETON                                        39
044200930806     C                   MOVEL     TASTBL        TIBOL             2
044300930806E   2C                   ENDIF
044400930806     C*
044500990701     C  N39KTAS2         READE     TITAS30C                               39
044600930806E   1C                   ENDDO
044700930806     C*
044800930806     C                   ENDSR
044900070202     C*---------------------------------------------------------------*
045000070202     C*                    CERBLP                                     *
045100070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
045200070202     c* merce mai affdata
045300070202     C*---------------------------------------------------------------*
045400070202     C     CERBLP        BEGSR
045500070202     c                   clear                   wChiusainBLP      1
045600070202     c     ktas2         chain     fnblp01l
045700070202    5c                   if        %found(fnblp01l) and blpft1='N'
045800070302     c                   if        blpcca='7'
045900070202     c                   eval      wChiusainBLP='S'
046000070302     c                   else
046100070302     c* Verifico se si tratta di import
046200070302     c     blplnp        chain     azorg01l
046300070302If   c                   If        %Found(Azorg01l)
046400070302     c                   Movel     OrgDe3        og143
046500070302     c                   Else
046600070302     c                   Clear                   og143
046700070302     c                   EndIf
046800070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
046900070302     c                   eval      wChiusainBLP='S'
047000070302     c                   EndIf
047100070302     C
047200070302     c                   EndIf
047300070302     c                   EndIf
047400070202     c                   ENDSR
047500930422     C*---------------------------------------------------------------*
047600930422     C*                    DEFVAR                                     *
047700930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
047800930422     C*---------------------------------------------------------------*
047900930422     C     DEFVAR        BEGSR
048000930422     C*
048100930422     C     *ENTRY        PLIST
048200930422     C                   PARM                    KPJBA
048300111118     c                   movel     kpjbu         aggiorna          1
048400930422     C*
048500950111     C                   Z-ADD     1             CODUT             1 0
048600930806     C                   Z-ADD     0             DAT
048700930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
048800930423     C                   MOVEL     MBRFIL        ALF4              4
048900930423     C                   MOVE      ALF4          NUM3              3 0
049000960208     C* UDATE - 3
049100960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
049200960208     C                   TIME                    W0140            14 0
049300960208     C* UDATE IN GGMMAAAA
049400960208     C                   MOVE      W0140         WDTGIO            8 0
049500960208     C*
049600960208     C* UDATE IN AAAAMMGG
049700960208     C                   Z-ADD     WDTGIO        G02DAT
049800960208     C                   MOVEL     *BLANK        G02ERR
049900960208     C                   CALL      'XSRDA8'
050000960208     C                   PARM                    WLBDAT
050100960208     C                   MOVEL     G02INV        DATEU             8 0
050200960208     C* SOTTRAGGO  3
050300960208     C     G02TGI        SUB       3             GIOTGI
050400960208     C*
050500960208     C                   CALL      'XSRGI8'
050600960208     C                   PARM                    WGIDAT
050700960208     C*
050800960208     C                   Z-ADD     GIOINV        DATAM             8 0
050900070202     c
051000070202     c* Apro file bolle di filiale
051100070202     c                   clear                   tibs55ds
051200070202     c                   eval      i50tla='L'
051300070202     c                   eval      i50apo=001
051400070202     c                   call      'TIBS55R'
051500070202     c                   parm                    tibs55ds
051600070202     c*
051700070202     c                   Clear                   commt
051800070202     c                   Movel(p)  cmdt(1)       commt
051900070202     c                   Eval      %Subst(commt:30:10) = o50ala
052000070202     c                   Eval      commt =
052100070202     c                             %trim(commt) + '/FNBLP01L)'
052200070202     c                   Call      'QCMDEXC'
052300070202     c                   Parm                    commt
052400070202     c                   Parm                    lenght
052500070202     c
052600070202     c                   Open      FNBLP01L
052700950111     C*
052800950111     C* DEFINIZIONE CAMPI
052900950111     C     *LIKE         DEFINE    ARBFEV        FEV
053000950111     C     *LIKE         DEFINE    ARBDER        DER
053100950111     C     *LIKE         DEFINE    ARBDAT        DAT
053200950111     C     *LIKE         DEFINE    TBLCOD        COD
053300950111     C     *LIKE         DEFINE    TBLKEY        KEY
053400930422     C*
053500950111     C* ACCESSO TABEL00F
053600930423     C     KTAB          KLIST
053700950111     C                   KFLD                    CODUT
053800950111     C                   KFLD                    COD
053900950111     C                   KFLD                    KEY
054000990701     C* ACCESSO TITAS30C
054100930426     C     KTAS          KLIST
054200950111     C                   KFLD                    ARBAAS
054300950111     C                   KFLD                    ARBLNP
054400950111     C                   KFLD                    ARBNRS
054500950111     C                   KFLD                    ARBNSP
054600930528     C                   KFLD                    TBL               2
054700990701     C* ACCESSO TITAS30C/TNBLA01L
054800930528     C     KTAS2         KLIST
054900950111     C                   KFLD                    ARBAAS
055000950111     C                   KFLD                    ARBLNP
055100950111     C                   KFLD                    ARBNRS
055200950111     C                   KFLD                    ARBNSP
055300991028     C* ACCESSO FIARBE2C
055400950111     C     KARB          KLIST
055500950111     C                   KFLD                    ARBAAS
055600950111     C                   KFLD                    ARBLNP
055700950111     C                   KFLD                    ARBNRS
055800950111     C                   KFLD                    ARBNSP
055900950111     C                   KFLD                    ARBDER
056000950111     C                   KFLD                    DAT
056100950111     C     KARB2         KLIST
056200950111     C                   KFLD                    ARBAAS
056300950111     C                   KFLD                    ARBLNP
056400950111     C                   KFLD                    ARBNRS
056500950111     C                   KFLD                    ARBNSP
056600930423     C*
056700930423     C                   ENDSR
056800930423     C*---------------------------------------------------------------*
056900930423     C*                    QCMDEXC                                    *
057000930423     C*---------------------------------------------------------------*
057100111114     OQSYSPRT   E            err1        1
057200111114     O                                         +  1 'bolla insistente1'
057300111114     O                       arbaas        z   +  1
057400111114     O                       arblnp        z   +  1
057500111114     O                       arbnrs        z   +  1
057600111114     O                       arbnsp        z   +  1
057700111114     OQSYSPRT   E            err4        1
057800111114     O                                         +  1 'bolla insistente2'
057900111114     O                       arbaas        z   +  1
058000111114     O                       arblnp        z   +  1
058100111114     O                       arbnrs        z   +  1
058200111114     O                       arbnsp        z   +  1
058300111114     OQSYSPRT   E            err2        1
058400111114     O                                         +  1 'bolla già tassata'
058500111114     O                       arbaas        z   +  1
058600111114     O                       arblnp        z   +  1
058700111114     O                       arbnrs        z   +  1
058800111114     O                       arbnsp        z   +  1
058900111114     OQSYSPRT   E            variaz      1
059000111114     O                                         +  1 'bolla da variare'
059100111114     O                       arbaas        z   +  1
059200111114     O                       arblnp        z   +  1
059300111114     O                       arbnrs        z   +  1
059400111114     O                       arbnsp        z   +  1
059500111114     O                       arbcbo            +  1
059600111114     O                       arbmgs            +  1 '  /  '
059700111114     o                                         +  2 'colli'
059800111114     o                       tasncl        2   +  1
059900111114     o                                         +  2 'TASPKF'
060000111114     o                       taspkf        2   +  1
060100111114     o                                         +  2 'VDL da mettere'
060200111114     o                       d39ncp        2   +  1
060300111114     o                       d39plu        2   +  1
060400111114     OQSYSPRT   E            FINE        1
060500111114     O**                                       +  1 'TOT DA VARIARE OTT'
060600111114     o**                     TOTVARot      2   +  1
060700111114     O                                         +  1 'TOT DA VARIARE NOV'
060800111114     o                       TOTVARno      2   +  1
060900111114     OQSYSPRT   E            FINE        1
061000111114     O                                         +  1 'TOT 1°bolla ines'
061100111114     o                       TOTerr1       2   +  1
061200111114     O                                         +  1 'TOT 2°bolla ines'
061300111114     o                       TOTerr4       2   +  1
061400111114     O                                         +  1 'Bolla tassata'
061500111114     o                       TOTerr2       2   +  1
061600940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
061700930426**         ERR
0618009305281 SPEDIZIONE INESISTENTE
061900950222SPEDIZIONE GIA' TASSATA
062000930528CODICE BOLLA INESISTENTE
0621009305282 SPEDIZIONE INESISTENTE
062200070202** cmdt
062300070202OVRDBF FILE(FNBLP01L) TOFILE(
