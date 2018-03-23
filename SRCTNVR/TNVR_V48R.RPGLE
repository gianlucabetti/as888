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
002400941124     FTNBLA01L  IF   E           K DISK
002500111110     Ffnarb01l  IF   E           K DISK
002600111121     Ffiarbf1c  IF   E           K DISK    ignore(fnarbd00)
002700111121     f                                     ignore(fnarbk00)
002800111121     f                                     ignore(fiarbt00)
002900111121     f                                     ignore(fnarbg00)
003000111121     f                                     ignore(fnarbm00)
003100111121     f                                     ignore(fnarbp00)
003200111121     f                                     prefix(V_)
003300111111     FTITAS30C  uF   E           K DISK
003400930423     FTABEL00F  IF   E           K DISK
003500070302     Fazorg01l  IF   E           K DISK
003600070202     Ffnblp01l  IF   E           K DISK    usropn
003700111110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003800930422     D*---------------------------------------------------------------*
003900930422     D* SCHIERE                                                       *
004000930422     D*---------------------------------------------------------------*
004100930426     D* DESCRIZIONE ERRORE
004200930528     D ERR             S             35    DIM(4) CTDATA PERRCD(1)
004300070202     d cmdt            s             60    dim(01) ctdata perrcd(1)
004400930422     D*---------------------------------------------------------------*
004500930422     D* DS                                                            *
004600930422     D*---------------------------------------------------------------*
004700930422     D KPJBA         E DS
004800930423     D  LEGFIL               483    492
004900930423     D  MBRFIL               493    502
005000960208     D***
005100960208     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005200960208     D***
005300960208     D WLBDAT          DS                  INZ
005400960208     D  G02DAT                 1      8  0
005500960208     D  G02INV                 9     16  0
005600960208     D  G02ERR                17     17
005700960208     D  G02TGI                18     22  0
005800960208     D***
005900960208     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
006000960208     D***
006100960208     D WGIDAT          DS                  INZ
006200960208     D  GIODAT                 1      8  0
006300960208     D  GIOINV                 9     16  0
006400960208     D  GIOTGI                17     21  0
006500930528     D DS3A          E DS
006600070302     D og143         E DS
006700070202     D tibs55ds      E DS
006800950111     D*
006900950111     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
007000950111     D DSLV20        E DS                  EXTNAME(FNLV20DS)
007100990702     D*
007200990702     D DSLV55        E DS                  EXTNAME(FNLV55DS)
007300111110     d
007400111110     d commt           s             80
007500111110     d lenght          s             15  5 inz(80)
007600111121     d
007700111121     d Dataiso         s               d   datfmt(*iso)
007800111121     d Datasys         s               d   datfmt(*iso)  inz(*sys)
007900070202     i
008000990701     ITITAS000      11
008100990701     ITITAS010      12
008200990701     ITITASP00      13
008300930422     C*---------------------------------------------------------------*
008400930422     C*                    MAIN                                       *
008500930423     C* PROGRAMMA DI RICEZIONE IN SEDE VARIAZIONI VOLUME              *
008600930422     C*---------------------------------------------------------------*
008700930528     C*
008800930528     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008900930528     C                   EXSR      DEFVAR
009000111124     c
009100111111     c                   z-add     2011          kaas              4 0
009200111121     c                   eval      dataiso=datasys
009300111124     c                   movel     dataiso       dateu             8 0
009400111124     c
009500111202     c                   subdur    3:*d          dataiso
009600120119     c                   movel     dataiso       datafine          8 0
009700120119     c                   move      datafine      kmgfine           4 0
009800111121     c
009900930528     C*
010000930528     C* APERTURA RIUSCITA
010100930423     C*
010200930423     C* LEGGO VARIAZIONE VOLUME
010300111111     C     kaas          setll     fnarb01l
010400111111     C     kaas          READe     fnarb01l                               33
010500930423DO  2C     *IN33         DOWEQ     '0'
010600111111     c
010700120119     c                   if        ((arbaas*10000)+arbmgs)<=datafine
010800120119     c                             and arbaas=2012
010900930423     C*
011000930423     C* GESTIONE VARIAZIONI DA APPLICARE
011100930423     C                   EXSR      GESMOD
011200111110    3c                   endif
011300111110    c
011400111111     C     kaas          READe     fnarb01l                               33
011500930423E   2C                   ENDDO
011600990702     C*
011700990702     C                   MOVEL     'C'           D55TLA
011800990702     C                   CALL      'FNLV55R'
011900990702     C                   PARM                    DSLV55
012000930422     C*
012100111110     C                   EXCEPT    FINE
012200930423     C                   SETON                                        LR
012300930423     C*---------------------------------------------------------------*
012400930423     C*                    GESMOD                                     *
012500930423     C* ROUTINE DI GESTIONE MODIFICHE VOLUME DA APPLICARE             *
012600930423     C*---------------------------------------------------------------*
012700930423     C     GESMOD        BEGSR
012800930426     C*
012900930426     C                   SETOFF                                       050607
013000930528     C                   SETOFF                                       3108
013100930806     C                   SETOFF                                       0102
013200111123     c                   clear                   wchiusainblp
013300930806     C*
013400930806     C* LETTURA TIPO BOLLA
013500930528     C                   MOVEL     '3A'          COD
013600950111     C                   MOVEL(P)  ARBCBO        KEY
013700930528     C     KTAB          CHAIN     TABEL                              30
013800930528     C*
013900930528     C* NON TROVATO CODICE BOLLA: SCRIVO NEL FILE DEGLI ERRORI
014000930806IF  1C     *IN30         IFEQ      '1'
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
015600111121     c                   if        Aggiorna='N'
015700111121     C     KTAS          CHAIN(N)  TITAS30C                           37
015800111121     c                   else
015900111121     C     KTAS          CHAIN     TITAS30C                           37
016000111121     c                   endif
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
017400111121     c                   if        Aggiorna='N'
017500111121     C     KTAS          CHAIN(N)  TITAS30C                           37
017600111121     c                   else
017700990701     C     KTAS          CHAIN     TITAS30C                           37
017800111121     c                   endif
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
019900070202   2ac     wChiusainBLP  ifeq      ' '
020000930806     C*
020100930528     C* 1 BOLLA TROVATA IN ARCHIVIO (CORRENTE, STORICO O PRECEDENTE)
020200930528IF  3C     *IN05         IFEQ      '1'
020300111110     c* Aggiorno solo se volume diverso da quello di filiale
020400111124   3ac                   if        arbvlc >tasvlc and tasfic=' '
020500930528     C                   EXSR      AGGIOR
020600930528     C*
020700930806IF  4C     *IN05         IFEQ      *ON
020800930806     C*
020900930706     C* SE ESISTE ERRORE NON STAMPATO CANCELLO
021000111118     c                   if        Aggiorna ='S'
021100111111     C                   UPDATE    TITAS000
021200111118     c                   endif
021300111118     c
021400930806E   4C                   ENDIF
021500930706     C*
021600111110E  3aC                   ENDIF
021700111110E   3C                   ENDIF
021800930806     C*
021900941129     C* 2 BOLLA TROVATA IN ARCHIVIO
022000941129IF  3C     §3ATB2        IFNE      *BLANKS
022100941129     C     *IN01         ANDEQ     *OFF
022200941129     C*
022300930528     C                   MOVEL     §3ATB2        TBL
022400941129     C                   SETOFF                                       111213
022500111121     c                   if        Aggiorna='N'
022600111121     C     KTAS          CHAIN(N)  TITAS30C                           30
022700111121     c                   else
022800990701     C     KTAS          CHAIN     TITAS30C                           30
022900111121     c                   endif
023000930528     C*
023100941129    4C     *IN30         IFEQ      *OFF
023200111124   3ac                   if        arbvlc >tasvlc and tasfic=' '
023300930528     C                   EXSR      AGGIOR
023400930528     C*
023500111118     c                   if        Aggiorna ='S'
023600111111     C   11              UPDATE    TITAS000
023700941129E   4C                   ENDIF
023800111118E   4C                   ENDIF
023900111118     c
024000111118E   4C                   ENDIF
024100941129E   3C                   ENDIF
024200070202E  2aC                   ENDIF
024300930528     C*
024400070202     C*
024500930806E   2C                   ENDIF
024600950905     C*
024700950905X  1AC                   ELSE
024800950905E  1AC                   ENDIF
024900950905E   1C                   ENDIF
025000930806     C*
025100930528     C                   ENDSR
025200930528     C*---------------------------------------------------------------*
025300930528     C*                    AGGIOR                                     *
025400930806     C* ROUTINE DI AGGIORNAMENTO VOLUME SULLA BOLLA                   *
025500930528     C*---------------------------------------------------------------*
025600930528     C     AGGIOR        BEGSR
025700930426     C*
025800930426     C* ESCLUDO QUALSIASI AGGIORNAMENTO SE TIPO VOLUME UGUALE A 'T'
025900970625    1C******     TASFVF    IFNE 'T'
026000930528     C*
026100930806     C                   MOVEL     TASTBL        BOL1              1
026200930806     C                   MOVE      TASTBL        BOL2              1
026300930426     C                   MOVE      TASKSC        CODCLI            4 0
026400930806     C*
026500950111     C                   CLEAR                   DSLV20
026600950111     C*
026700950111     C* SE E' LA PRIMA BOLLA
026800950111    2C     TBL           IFEQ      §3ATB1
026900950111     C*
027000950111     C* SE C/SERVIZIO O STORNO O ASSEGNATO 9999
027100950111    3C     BOL2          IFEQ      '7'
027200950111     C     BOL2          OREQ      'C'
027300950111     C     BOL2          OREQ      'S'
027400950111     C     BOL1          OREQ      'A'
027500950111     C     CODCLI        ANDEQ     9999
027600950111     C*
027700950111     C* NON PASSO LA DATA FATTURA
027800950111     C                   CLEAR                   D20DFT
027900950111     C*
028000950111   X3C                   ELSE
028100950111     C                   MOVEL     TASDFT        D20DFT
028200950111    3C                   ENDIF
028300930623     C*
028400950111   X2C                   ELSE
028500950111     C* SE E' LA SECONDA BOLLA NON PASSO MAI LA DATA FATTURA
028600950111     C                   CLEAR                   D20DFT
028700950111    2C                   ENDIF
028800930623     C*
028900950111     C                   MOVEL     'C'           D20TVL
029000950111     C                   MOVEL     ARBNCR        D20NCR
029100950111     C                   MOVEL     ARBVLC        D20VLU
029200950111     C                   MOVEL     TASFVF        D20FVF
029300950111     C                   MOVEL     TASVLF        D20VLF
029400950111     C                   MOVEL     TASFVB        D20FVB
029500950111     C                   MOVEL     TASVLB        D20VLB
029600950111     C                   MOVEL     TASAAS        D20AAS
029700950111     C                   MOVEL     TASLNP        D20LNP
029800950111     C                   MOVEL     TASNRS        D20NRS
029900950111     C                   MOVEL     TASNSP        D20NSP
030000950111     C                   MOVEL     ARBCBO        D20CBO
030100950111     C                   MOVEL     TASNCL        D20NCL
030200950111     C                   MOVEL     TASLNA        D20LNA
030300950111     C                   MOVEL     NUM3          D20MBR
030400960927     C                   MOVE      TASMGS        D20DTE
030500960927     C                   MOVEL     TASAAS        D20DTE
030600970904     C* SE IL VOLUME CHE RICEVO E' T MA IL NUMERO COLLI E' <> DA
030700970904     C*  COLLI RILEVATI, LI METTO UGUALI
030800111110     C**   ARBFVC        IFEQ      'T'
030900111110     C**   ARBNCR        ANDNE     TASNCL
031000111110     C**                 Z-ADD     TASNCL        D20NCR
031100111110     C**                 ENDIF
031200950111     C*
031300950111     C                   MOVEL     DSLV20        KPJBU
031400950111     C*
031500950111     C                   CALL      'FNLV20R'
031600950111     C                   PARM                    KPJBA
031700950111     C                   MOVEL     KPJBU         DSLV20
031800950111     C*
031900930623     C*
032000990702     C* SE ERRORE SCRIVO FILE ERRORI
032100950111    2C     D20ERR        IFEQ      '2'
032200950111     C*
032300930623     C* FRANCO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNP
032400950111    3C     BOL1          IFEQ      'F'
032500950111    3C                   ELSE
032600930623     C* ASSEGNATO: FILIALE CHE ESEGUE LA VARIAZIONE  =  AS DELLA LNA
032700990702     C                   CLEAR                   DSLV55
032800990702     C                   MOVEL     'P'           D55TPT
032900990702     C                   Z-ADD     TASLNA        D55LIN
033000990702     C                   Z-ADD     DATEU         D55DRF
033100990702     C                   CALL      'FNLV55R'
033200990702     C                   PARM                    DSLV55
033300950111    3C                   ENDIF
033400111110     c                   except    err2
033500111110     c                   add       1             toterr2           9 0
033600930623     C*
033700950111    2C                   ENDIF
033800990702     C* AGGIORNO IN OGNI CASO I CAMPI DI TITAS (ANCHE SE C'E' ERRORE)
033900990702     C* PERCHE' LA LORO IMPOSTAZIONE E' A CURA DELL'FNLV20R
034000111124     c                   clear                   v_arbcvb
034100111124     c                   clear                   v_arbdtv
034200111124     c                   clear                   v_arborv
034300111124     c
034400111124     c     ktas2         chain     fiarbf1c                           98
034500111124     c                   dow       not *in98
034600111124     c     ktas2         reade     fiarbf1c                               98
034700111124     c                   enddo
034800111121     c
034900111124     c* se la data è di oggi non stampo e non vario la bolla
035000111124     c                   if        v_arbdtv<dateu
035100111111     c                   except    variaz
035200111111     c                   add       1             totvarNO          9 0
035300111110     c
035400950111     C                   MOVEL     D20NCR        TASNCR
035500950111     C                   MOVEL     D20VLU        TASVLC
035600950111     C                   MOVEL     D20FVF        TASFVF
035700950111     C                   MOVEL     D20VLF        TASVLF
035800111124     c                   endif
035900111124     c
036000970625    1C***********          ENDIF
036100930426     C*
036200930423     C                   ENDSR
036300930806     C*---------------------------------------------------------------*
036400930806     C*                    MANBOL                                     *
036500930806     C* ROUTINE DI CONTROLLO SE MANCA LA BOLLA O E' CAMBIATO IL TIPO  *
036600930806     C*---------------------------------------------------------------*
036700930806     C     MANBOL        BEGSR
036800930806     C*
036900930806     C* 01 02 ON - MANCANO ENTRAMBE SCRIVO FILE DEGLI ERRORI
037000930806IF  1C     *IN01         IFEQ      *ON
037100930806     C     *IN02         ANDEQ     *ON
037200070202     c                   exsr      CERBLP
037300070202     c
037400070202     c* Non trovata nemmeno in blp come merce mai affidata
037500070202   1ac                   if        wChiusainBLP=' '
037600960208     C                   SETON                                        08
037700960208     C*
037800960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
037900930806     C*
038000111110     c                   add       1             toterr1           9 0
038100111110     C                   EXcept    err1
038200070202   1aC                   ENDIF
038300930806     C*
038400930806X   1C                   ELSE
038500930806     C*
038600930806     C* 01 O 02 OFF - MANCA UNA DELLE DUE VEDO SE CAMBIATO TIPO BOLLA
038700930806IF  2C     *IN01         IFEQ      *ON
038800930806     C     *IN02         OREQ      *ON
038900930806     C* N O N   T R O V A T A   S E C O N D A   B O L L A
039000930806IF  3C     *IN01         IFEQ      *ON
039100930806     C                   MOVEL     §3ATB1        ALF2
039200930806     C                   EXSR      RICBOL
039300930806     C*
039400930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
039500960208     C* DO ERRORE SOLO SE PASSATI 3 GG DALLA DATA VARIAZIONE
039600930806IF  4C     TIBOL         IFEQ      *BLANKS
039700111110     C                   EXcept    err4
039800111110     c                   add       1             toterr4           9 0
039900930806X   4C                   ELSE
040000930806     C                   MOVEL     TIBOL         §3ATB2
040100990702     C                   SETOFF                                       01
040200930806E   4C                   ENDIF
040300990702     C* RICHAINO LA PRIMA BOLLA
040400990702     C                   MOVEL     §3ATB1        TBL
040500990702     C                   SETOFF                                       111213
040600111121     c                   if        Aggiorna='N'
040700111121     C     KTAS          CHAIN(N)  TITAS30C                           37
040800111121     c                   else
040900990702     C     KTAS          CHAIN     TITAS30C                           37
041000111121     c                   endif
041100930806     C*
041200930806X   3C                   ELSE
041300930806     C* N O N   T R O V AT A   P R I M A   B O L L A
041400930806     C                   MOVEL     §3ATB2        ALF2              2
041500930806     C                   EXSR      RICBOL
041600930806     C*
041700930806     C* NON TROVATO ALTRO TIPO BOLLA: ERRORE
041800930806IF  4C     TIBOL         IFEQ      *BLANKS
041900070202     c* Se la bolla è chiusa in partenza con cca=/-merce mai affidadata
042000070202     c*  cancello senza dare erorre
042100070202     c                   exsr      CERBLP
042200070202     c
042300070202     c* Non trovata nemmeno in blp come merce mai affidata
042400070202    5c                   if        wChiusainBLP=' '
042500070202     c
042600960208     C                   SETON                                        08
042700111110     C                   EXcept    err1
042800111110     c                   add       1             toterr1           9 0
042900070202E   5C                   ENDIF
043000070202     c
043100930806X   4C                   ELSE
043200941129     C   11              SETON                                        05
043300941129     C   12              SETON                                        06
043400941129     C   13              SETON                                        07
043500930806     C                   MOVEL     TIBOL         §3ATB1
043600070202E   4C                   ENDIF
043700930806E   3C                   ENDIF
043800930806E   2C                   ENDIF
043900930806E   1C                   ENDIF
044000930806     C*
044100930806     C                   ENDSR
044200070202     C*---------------------------------------------------------------*
044300070202     C*                    CERBLP                                     *
044400070202     C* cerco bolla in blp peer vedere se chiusa in partenza con      *
044500070202     c* merce mai affdata
044600070202     C*---------------------------------------------------------------*
044700070202     C     CERBLP        BEGSR
044800070202     c                   clear                   wChiusainBLP      1
044900070202     c     ktas2         chain     fnblp01l
045000070202    5c                   if        %found(fnblp01l) and blpft1='N'
045100070302     c                   if        blpcca='7'
045200070202     c                   eval      wChiusainBLP='S'
045300070302     c                   else
045400070302     c* Verifico se si tratta di import
045500070302     c     blplnp        chain     azorg01l
045600070302If   c                   If        %Found(Azorg01l)
045700070302     c                   Movel     OrgDe3        og143
045800070302     c                   Else
045900070302     c                   Clear                   og143
046000070302     c                   EndIf
046100070302    5c                   if        §ogntw='EEX'  or §ogntw='DPD'
046200070302     c                   eval      wChiusainBLP='S'
046300070302     c                   EndIf
046400070302     C
046500070302     c                   EndIf
046600070302     c                   EndIf
046700070202     c                   ENDSR
046800930806     C*---------------------------------------------------------------*
046900930806     C*                    RICBOL                                     *
047000930806     C* ROUTINE DI RICERCA BOLLA SU ARCHIVIO STORICO O CORRENTE       *
047100930806     C*---------------------------------------------------------------*
047200930806     C     RICBOL        BEGSR
047300930806     C*
047400930806     C                   SETOFF                                       3940
047500941129     C                   SETOFF                                       111213
047600930806     C                   CLEAR                   TIBOL
047700930806     C*
047800990701     C     KTAS2         SETLL     TITAS30C
047900990701     C     KTAS2         READE     TITAS30C                               39
048000930806     C*
048100930806DO  1C     *IN39         DOWEQ     *OFF
048200930806     C*
048300930806IF  2C     TASTBL        IFNE      ALF2
048400930806     C                   SETON                                        39
048500930806     C                   MOVEL     TASTBL        TIBOL             2
048600930806E   2C                   ENDIF
048700930806     C*
048800990701     C  N39KTAS2         READE     TITAS30C                               39
048900930806E   1C                   ENDDO
049000930806     C*
049100930806     C*
049200930806     C                   ENDSR
049300930422     C*---------------------------------------------------------------*
049400930422     C*                    DEFVAR                                     *
049500930422     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
049600930422     C*---------------------------------------------------------------*
049700930422     C     DEFVAR        BEGSR
049800930422     C*
049900930422     C     *ENTRY        PLIST
050000930422     C                   PARM                    KPJBA
050100111118     c                   movel     kpjbu         aggiorna          1
050200930422     C*
050300950111     C                   Z-ADD     1             CODUT             1 0
050400930423     C* FILIALI IN GESTIONE DEL MEMBRO DI RICEZIONE (AS DI INVIO)
050500930423     C                   MOVEL     MBRFIL        ALF4              4
050600930423     C                   MOVE      ALF4          NUM3              3 0
050700960208     C* UDATE - 3
050800960208     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
050900960208     C                   TIME                    W0140            14 0
051000960208     C* UDATE IN GGMMAAAA
051100960208     C                   MOVE      W0140         WDTGIO            8 0
051200960208     C*
051300960208     C* UDATE IN AAAAMMGG
051400960208     C                   Z-ADD     WDTGIO        G02DAT
051500960208     C                   MOVEL     *BLANK        G02ERR
051600960208     C                   CALL      'XSRDA8'
051700960208     C                   PARM                    WLBDAT
051800960208     C                   MOVEL     G02INV        DATEU             8 0
051900960208     C* SOTTRAGGO  3
052000960208     C     G02TGI        SUB       3             GIOTGI
052100960208     C*
052200960208     C                   CALL      'XSRGI8'
052300960208     C                   PARM                    WGIDAT
052400960208     C*
052500960208     C                   Z-ADD     GIOINV        DATAM             8 0
052600070202     c* Apro file bolle di filiale
052700070202     c                   clear                   tibs55ds
052800070202     c                   eval      i50tla='L'
052900070202     c                   eval      i50apo=001
053000070202     c                   call      'TIBS55R'
053100070202     c                   parm                    tibs55ds
053200070202     c*
053300070202     c                   Clear                   commt
053400070202     c                   Movel(p)  cmdt(1)       commt
053500070202     c                   Eval      %Subst(commt:30:10) = o50ala
053600070202     c                   Eval      commt =
053700070202     c                             %trim(commt) + '/FNBLP01L)'
053800070202     c                   Call      'QCMDEXC'
053900070202     c                   Parm                    commt
054000070202     c                   Parm                    lenght
054100070202     c
054200070202     c                   Open      FNBLP01L
054300950111     C*
054400950111     C* DEFINIZIONE CAMPI
054500950111     C     *LIKE         DEFINE    TBLCOD        COD
054600950111     C     *LIKE         DEFINE    TBLKEY        KEY
054700930422     C*
054800950111     C* ACCESSO TABEL00F
054900930423     C     KTAB          KLIST
055000950111     C                   KFLD                    CODUT
055100950111     C                   KFLD                    COD
055200950111     C                   KFLD                    KEY
055300990701     C* ACCESSO TITAS30C
055400930426     C     KTAS          KLIST
055500950111     C                   KFLD                    ARBAAS
055600950111     C                   KFLD                    ARBLNP
055700950111     C                   KFLD                    ARBNRS
055800950111     C                   KFLD                    ARBNSP
055900930528     C                   KFLD                    TBL               2
056000990701     C* ACCESSO TITAS30C/TNBLA01L
056100930528     C     KTAS2         KLIST
056200950111     C                   KFLD                    ARBAAS
056300950111     C                   KFLD                    ARBLNP
056400950111     C                   KFLD                    ARBNRS
056500950111     C                   KFLD                    ARBNSP
056600930423     C*
056700930423     C                   ENDSR
056800930423     C*---------------------------------------------------------------*
056900930423     C*                    QCMDEXC                                    *
057000930423     C*---------------------------------------------------------------*
057100111110     OQSYSPRT   E            err1        1
057200111110     O                                         +  1 'bolla insistente1'
057300111110     O                       arbaas        z   +  1
057400111110     O                       arblnp        z   +  1
057500111110     O                       arbnrs        z   +  1
057600111110     O                       arbnsp        z   +  1
057700111110     OQSYSPRT   E            err4        1
057800111110     O                                         +  1 'bolla insistente2'
057900111110     O                       arbaas        z   +  1
058000111110     O                       arblnp        z   +  1
058100111110     O                       arbnrs        z   +  1
058200111110     O                       arbnsp        z   +  1
058300111110     OQSYSPRT   E            err2        1
058400111110     O                                         +  1 'bolla già tassata'
058500111110     O                       arbaas        z   +  1
058600111110     O                       arblnp        z   +  1
058700111110     O                       arbnrs        z   +  1
058800111110     O                       arbnsp        z   +  1
058900111110     OQSYSPRT   E            variaz      1
059000111121     O                                         +  1 'Da variare'
059100111110     O                       arbaas        z   +  1
059200111110     O                       arblnp        z   +  1
059300111110     O                       arbnrs        z   +  1
059400111110     O                       arbnsp        z   +  1
059500111121     O                       arblna        z   +  1
059600111111     O                       arbmgs            +  1 '  /  '
059700111121     O                       v_arbcvb          +  1
059800111121     O                       v_arbdtv          +  1 '    /  /  '
059900111121     O                       v_arborv          +  1 '  :  :  '
060000111121     OQSYSPRT   E            variaz      1
060100111121     o                                         + 20 'colli'
060200111110     o                       tasncl        2   +  1
060300111110     o                                         +  2 'TASVLF'
060400111110     o                       tasfvf            +  1
060500111110     o                       tasvlf        2   +  1
060600111110     o                                         +  2 'VDL da mettere'
060700111110     o                       d20ncr        2   +  1
060800111110     o                       d20vlu        2   +  1
060900111110     o                                         +  2 'NEW fatt'
061000111110     o                       d20fvf            +  1
061100111110     o                       d20vlf        2   +  1
061200111110     OQSYSPRT   E            FINE        1
061300111111     O**                                       +  1 'TOT DA VARIARE OTT'
061400111111     o**                     TOTVARot      2   +  1
061500111111     O                                         +  1 'TOT DA VARIARE NOV'
061600111111     o                       TOTVARno      2   +  1
061700111111     OQSYSPRT   E            FINE        1
061800111111     O                                         +  1 'TOT 1°bolla ines'
061900111110     o                       TOTerr1       2   +  1
062000111111     O                                         +  1 'TOT 2°bolla ines'
062100111110     o                       TOTerr4       2   +  1
062200111110     O                                         +  1 'Bolla tassata'
062300111110     o                       TOTerr2       2   +  1
062400940412£$    */COPY GAITRASRC/SRCTRST,TRST49R
062500930426**         ERR
0626009305281 SPEDIZIONE INESISTENTE
062700950222SPEDIZIONE GIA' TASSATA
062800930528CODICE BOLLA INESISTENTE
0629009305282 SPEDIZIONE INESISTENTE
063000070202** cmdt
063100070202OVRDBF FILE(FNBLP01L) TOFILE(
