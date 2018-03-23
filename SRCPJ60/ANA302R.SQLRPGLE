000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*caller)
000200000530     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE)
000300000530     H*PARMS DYNUSRPRF(*OWNER)
000400941130     H DECEDIT('0,') DATEDIT(*DMY.)
000500941130      *
000600060515G1495X*  MEMBRO PULITO IN CHIUSURA RELEASE 5.0
000700060515      *
000800941212      *            REPERIMENTO DATI DA ANCLN00F+ANSOG00F+ANIND00F
000900941130      *
001000941130      * input
001100941229      * KSC302          CLIENTE
001200941229      * DES302          DESCRIZIONE BREVE
001300941229      * RSO302          RAGIONE SOCIALE
001400941229      * ST1302          RICERCA PER STRINGA SULLA RAGIONE SOCIALE
001500941229      * CIT302          LOCALITA'
001600941229      * ST2302          RICERCA PER STRINGA SULLA LOCALITA'
001700941229      * STA302          STATO
001800941229      * PRV302          PROVINCIA
001900941229      * CAP302          CAP
002000941229      * IVA302          PARTITA IVA
002100941229      * CDF302          CODICE FISCALE
002200941229      * LDV302          LINEA DI VENDITA
002300941229      * SOG302          SOGGETTO
002400941229      * FSN302          FILIALI SI/NO
002500941229      * ATT302          SOLO ATTIVI/SOLO NON  ATTIVI/TUTTI
002600950103      * ANN302          ANNULLATI SI/NO
002700941229      * ORD302          ORDINAMENTO (NUMERO ORDER BY)
002800990927      * POS302          INIZIO ELENCO DA
002900980105      * CST302          DATI STATISTICI
003000980105      * CLF302          Cliente di Fatturazione
003100941130     F*----------------------------------------------------*
003200950224     F*------------
003300040226     FANCLN02L  IF   E           K DISK    USROPN
003400040226     F                                     RENAME(ANCLN000:ANCLN02)
003500950228     FANCLN03L  IF   E           K DISK    USROPN
003600950224     F                                     RENAME(ANCLN000:ANCLN03)
003700950224     F*------------
003800950228     FANCLN04L  IF   E           K DISK    USROPN
003900950228     F                                     RENAME(ANCLN000:ANCLN04)
004000950224     F*------------
004100950228     FANCLN05L  IF   E           K DISK    USROPN
004200950228     F                                     RENAME(ANCLN000:ANCLN05)
004300950224     F*------------
004400950303     FANCLN09L  IF   E           K DISK    USROPN
004500950303     F                                     RENAME(ANCLN000:ANCLN09)
004600951003     F*------------
004700951003     FANCLN08L  IF   E           K DISK    USROPN
004800951003     F                                     RENAME(ANCLN000:ANCLN08)
004900950224     F*------------
005000951003     FANCLN10L  IF   E           K DISK    USROPN
005100951003     F                                     RENAME(ANCLN000:ANCLN10)
005200950224     F*------------
005300950320     FANSOG01L  IF   E           K DISK    USROPN
005400950320     F                                     RENAME(ANSOG000:ANSOG01)
005500950224     F*------------
005600950320     FANSOG02J  IF   E           K DISK    USROPN
005700950320     F                                     RENAME(ANSOGJ00:ANSOG02)
005800950224     F*------------
005900950320     FANSOG03J  IF   E           K DISK    USROPN
006000950320     F                                     RENAME(ANSOGJ00:ANSOG03)
006100950224     F*------------
006200950320     FANSOG04J  IF   E           K DISK    USROPN
006300950320     F                                     RENAME(ANSOGJ00:ANSOG04)
006400950224     F*------------
006500950320     FANIND01L  IF   E           K DISK    USROPN
006600950320     F                                     RENAME(ANIND000:ANIND01)
006700950224     F*------------
006800950320     FANIND02J  IF   E           K DISK    USROPN
006900950320     F                                     RENAME(ANINDJ00:ANIND02)
007000950224     F*------------
007100950320     FANIND03J  IF   E           K DISK    USROPN
007200950320     F                                     RENAME(ANINDJ00:ANIND03)
007300950224     F*------------
007400950320     FANIND04J  IF   E           K DISK    USROPN
007500950320     F                                     RENAME(ANINDJ00:ANIND04)
007600950224     F*------------
007700950320     FANIND05J  IF   E           K DISK    USROPN
007800950320     F                                     RENAME(ANINDJ00:ANIND05)
007900970924     F*------------
008000980105      * file dati statistici
008100980105     FANCLS01L  IF   E           K DISK    USROPN
008200980105     F                                     RENAME(ANCLS000:ANCLS01)
008300950224     F*------------
008400960606     D*----------------------------------------------------*
008500960606     D STR             S              1    DIM(50)
008600970924      *
008700980105      * Schiera per dati statistici da parameri STA302
008800990928     D $S              S              4    DIM(30)
008900980105      * Schiera per  dati statistici da file ANACLS00F
009000980105     D $A              S              4    DIM(30)
009100990927     D*
009200941130     D*----------------------------------------------------*
009300941130     D* Passaggio Parametri
009400941130     D KPJBA         E DS
009500941130     D*-------------
009600020321     D SOC001        E DS                  EXTNAME(XSOC001DS)
009700020321     D    XscGest                     1    overlay(xscSkp:3)
009800020321     D* DS Interna per dati di output di XSOC
009900020321     D XSOCDS          DS          1000
010000020321
010100991014     D* Parametri setup generale
010200991014     D SICOM         E DS
010300991014     D*
010400990930     D* Parametri gestione dati statistici
010500990923     D P009DS        E DS                  EXTNAME(ANP009DS)
010600990927     D*
010700990927     D XPAOUT          DS          2000
010800990928     D*-------------
010900990930     D*-------------
011000941130     D* Parametri in ricezione
011100941229     D A302DS        E DS                  EXTNAME(ANA302DS)
011200960607     D*-------------
011300991014     Dancln00f       E DS
011400020213     Dancls00f       E DS
011500020213     D sccodsta               22    141    dim(30)
011600960607     D*-------------
011700960607     DSOGDS          E DS                  extname(ANSOG00F)
011800960607     D*-------------
011900960607     DINDDS          E DS                  extname(ANIND00F)
012000941130     D*-------------
012100960613     D TpSogg          S              1
012200960613     D PrimaVolta      S              1  0
012300960613     D TpRicerca       S              1
012400960613     D Descr           S             50
012500960613     D Risultato       S              1  0
012600000103     D FlgSog          S              1
012700000103     D LenOut2         S              9B 0
012800000103     D LenOut1         S              9B 0
012900960613     D LenOut          S              9B 0
013000950103     D* Costanti
013100991203     D* costanti per la conversione minuscole/maiuscole:
013200991203     D UPP             C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
013300991203     D LOW             C                   'abcdefghijklmnopqrstuvwxyz'
013400991203     D*
013500950103     D* data a 0
013600960606     D $DT0            s               d   inz(*loval)
013700991014     D* parametri per X004VALCLN
013800991014     D soc004          s                   like(clnsocieta)
013900991014     D unita004        s                   like(clnunita)
014000991014     D esito           S              1
014100991014     D* prototipazione
014200991014     D/COPY *LIBL/SRCCPY,PJX004PR
014300991014     D*-----------------
014400991014     D*
014500980105      *
014600000114     D ZDateISO        S               D
014700050125B0771D ZDateALFA       S             10A
014800980105     IANCLS01
014900980105     I              CLSSOCIETA                  CLSSOC
015000980105     I              CLSUNITA                    CLSUNI
015100980105     I              CLSFILIALE                  CLSFIL
015200980105     I              CLSCDSTA01                  $A(1)
015300980105     I              CLSCDSTA02                  $A(2)
015400980105     I              CLSCDSTA03                  $A(3)
015500980105     I              CLSCDSTA04                  $A(4)
015600980105     I              CLSCDSTA05                  $A(5)
015700980105     I              CLSCDSTA06                  $A(6)
015800980105     I              CLSCDSTA07                  $A(7)
015900980105     I              CLSCDSTA08                  $A(8)
016000980105     I              CLSCDSTA09                  $A(9)
016100980105     I              CLSCDSTA10                  $A(10)
016200980105     I              CLSCDSTA11                  $A(11)
016300980105     I              CLSCDSTA12                  $A(12)
016400980105     I              CLSCDSTA13                  $A(13)
016500980105     I              CLSCDSTA14                  $A(14)
016600980105     I              CLSCDSTA15                  $A(15)
016700980105     I              CLSCDSTA16                  $A(16)
016800980105     I              CLSCDSTA17                  $A(17)
016900980105     I              CLSCDSTA18                  $A(18)
017000980105     I              CLSCDSTA19                  $A(19)
017100980105     I              CLSCDSTA20                  $A(20)
017200980105     I              CLSCDSTA21                  $A(21)
017300980105     I              CLSCDSTA22                  $A(22)
017400980105     I              CLSCDSTA23                  $A(23)
017500980105     I              CLSCDSTA24                  $A(24)
017600980105     I              CLSCDSTA25                  $A(25)
017700980105     I              CLSCDSTA26                  $A(26)
017800980105     I              CLSCDSTA27                  $A(27)
017900980105     I              CLSCDSTA28                  $A(28)
018000980105     I              CLSCDSTA29                  $A(29)
018100980105     I              CLSCDSTA30                  $A(30)
018200980105     I              CLSLINEAV                   CLSLDV
018300941130     C*----------------------------------------------------*
018400941130     C*                MAIN LINE PROGRAM
018500941130     C*----------------------------------------------------*
018600941130     C*
018700941130     C* inizializzazione variabili
018800941130     C                   EXSR      INZVAR
018900941130     C*
019000950227     C* SETLL
019100950227     C     RIC302        IFEQ      '0'
019200950227     C                   EXSR      SETLL
019300950227     C                   ENDIF
019400950227     C*
019500950227     C* READ
019600950227     C                   EXSR      READ
019700941130     C*
019800941130     C* fine programma
019900941130     C                   EXSR      ENDPGM
020000941130     C*
020100941130     C/EJECT
020200941130     C************************************************************
020300941130     C* FINE PROGRAMMA
020400941130     C************************************************************
020500941130     C     ENDPGM        BEGSR
020600941130     C*
020700941130     C* passaggio dati al pgm chiamante
020800941229     C                   MOVEL     A302DS        KPJBU
020900941130     C*
021000941229     C     END302        IFEQ      *OFF
021100941130     C                   SETON                                            RT
021200941130     C                   ELSE
021300960606     C                   EXSR      CLOCUR
021400941130     C                   SETON                                            LR
021500941130     C                   ENDIF
021600941130     C*
021700941130     C                   RETURN
021800941130     C*
021900941130     C                   ENDSR
022000941130     C/EJECT
022100941130     C************************************************************
022200950227     C* SELEZIONE SETLL IN BASE ALL'ORDINAMENTO
022300941130     C************************************************************
022400950227     C     SETLL         BEGSR
022500941130     C*
022600950227     C     $GEST         CASEQ     '01'          SET01
022700950227     C     $GEST         CASEQ     '02'          SET02
022800950227     C     $GEST         CASEQ     '03'          SET03
022900960613     C     $GEST         CASEQ     '04'          SET04
023000950227     C     $GEST         CASEQ     '05'          SET05
023100960607     C**** $GEST         CASEQ     '06'          SET06
023200950227     C     $GEST         CASEQ     '07'          SET07
023300950227     C     $GEST         CASEQ     '08'          SET08
023400950227     C     $GEST         CASEQ     '09'          SET09
023500950227     C     $GEST         CASEQ     '10'          SET10
023600950227     C     $GEST         CASEQ     '11'          SET11
023700950227     C     $GEST         CASEQ     '12'          SET12
023800950227     C     $GEST         CASEQ     '13'          SET13
023900951003     C     $GEST         CASEQ     '14'          SET14
024000950227     C                   ENDCS
024100950228     C*
024200950227     C                   ENDSR
024300950227     C/EJECT
024400950227     C************************************************************
024500950227     C* SETLL CON ORDINAMENTO PER CODICE CLIENTE
024600950227     C************************************************************
024700950227     C     SET01         BEGSR
024800950227     C*
024900950227     C                   SELECT
025000950227     C* NO LINEE DI VENDITA
025100950227     C* NO FILIALI
025200950227     C     LDV302        WHENEQ    *BLANK
025300950227     C     FSN302        ANDEQ     *OFF
025400960607     C                   MOVE      SOC302        CLNSocieta
025500950224     C                   MOVE      KSC302        CLNKSC
025600960607     C                   MOVE      *BLANK        CLNLineaV
025700960607     C                   Z-ADD     0             CLNFiliale
025800950228     C*
025900950228     C* apro il file se non lo è mai stato
026000950228     C     $CLN05        IFEQ      *BLANK
026100950228     C                   OPEN      ANCLN05L
026200950228     C                   MOVE      *ON           $CLN05
026300950228     C                   ENDIF
026400950224     C*
026500950224     C     K4CLN5        SETLL     ANCLN05
026600950227     C*
026700950227     C* SI LINEE DI VENDITA
026800950227     C* NO FILIALI
026900950227     C     LDV302        WHENNE    *BLANK
027000950227     C     FSN302        ANDEQ     *OFF
027100960607     C                   MOVE      SOC302        CLNSocieta
027200950227     C                   MOVE      KSC302        CLNKSC
027300960607     C                   MOVE      LDV302        CLNLineaV
027400960607     C                   Z-ADD     0             CLNFiliale
027500950228     C*
027600950228     C* apro il file se non lo è mai stato
027700950228     C     $CLN04        IFEQ      *BLANK
027800950228     C                   OPEN      ANCLN04L
027900950228     C                   MOVE      *ON           $CLN04
028000950228     C                   ENDIF
028100950227     C*
028200950227     C     K4CLN4        SETLL     ANCLN04
028300950227     C*
028400950227     C* NO LINEE DI VENDITA
028500950227     C* SI FILIALI
028600950227     C     LDV302        WHENEQ    *BLANK
028700950227     C     FSN302        ANDEQ     *ON
028800960607     C                   MOVE      SOC302        CLNSocieta
028900950227     C                   MOVE      KSC302        CLNKSC
029000960607     C                   MOVE      LDV302        CLNLineaV
029100960607     C                   Z-ADD     0             CLNFiliale
029200950228     C*
029300950228     C* apro il file se non lo è mai stato
029400950228     C     $CLN03        IFEQ      *BLANK
029500950228     C                   OPEN      ANCLN03L
029600950228     C                   MOVE      *ON           $CLN03
029700950228     C                   ENDIF
029800950227     C*
029900950227     C     K4CLN3        SETLL     ANCLN03
030000950227     C*
030100950227     C* SI LINEE DI VENDITA
030200950227     C* SI FILIALI
030300950227     C     LDV302        WHENNE    *BLANK
030400950227     C     FSN302        ANDEQ     *ON
030500960607     C                   MOVE      SOC302        CLNSocieta
030600950227     C                   MOVE      KSC302        CLNKSC
030700960607     C                   MOVE      LDV302        CLNLineaV
030800960607     C                   Z-ADD     0             CLNFiliale
030900950228     C*
031000950228     C* apro il file se non lo è mai stato
031100950228     C     $CLN03        IFEQ      *BLANK
031200950228     C                   OPEN      ANCLN03L
031300950228     C                   MOVE      *ON           $CLN03
031400950228     C                   ENDIF
031500950227     C*
031600950227     C     K4CLN3        SETLL     ANCLN03
031700950224     C*
031800950227     C                   ENDSL
031900950227     C*
032000950227     C                   ENDSR
032100950227     C/EJECT
032200950227     C************************************************************
032300950227     C* SETLL CON ORDINAMENTO PER DESCRIZIONE BREVE CLIENTE
032400950227     C************************************************************
032500950227     C     SET02         BEGSR
032600950227     C*
032700960607     C                   MOVE      SOC302        CLNSocieta
032800960607     C                   MOVE      DES302        CLNDesBrev
032900950228     C*
033000950228     C* apro il file se non lo è mai stato
033100950228     C     $CLN08        IFEQ      *BLANK
033200950228     C                   OPEN      ANCLN08L
033300950228     C                   MOVE      *ON           $CLN08
033400950228     C                   ENDIF
033500950227     C*
033600950227     C     K2CLN8        SETLL     ANCLN08
033700950227     C*
033800950227     C                   ENDSR
033900950227     C/EJECT
034000950227     C************************************************************
034100950227     C* SETLL CON ORDINAMENTO PER RAGIONE SOCIALE SOGGETTO
034200950227     C************************************************************
034300950227     C     SET03         BEGSR
034400950227     C*
034500950227     C                   MOVE      RSO302        SOGDES
034600950228     C*
034700950228     C* apro il file se non lo è mai stato
034800950228     C     $SOG02        IFEQ      *BLANK
034900950320     C                   OPEN      ANSOG02J
035000950228     C                   MOVE      *ON           $SOG02
035100950228     C                   ENDIF
035200950227     C*
035300950227     C     K1SOG2        SETLL     ANSOG02
035400950227     C*
035500950227     C                   ENDSR
035600950227     C/EJECT
035700950227     C************************************************************
035800960613     C* RICERCA PER STRINGA PER RAGIONE SOCIALE
035900950227     C************************************************************
036000960613     C     SET04         BEGSR
036100950227     C*
036200960613     C                   EVAL      PrimaVolta = 1
036300960613     C                   EVAL      Descr = *blank
036400960613     C                   MOVEL     RSo302        Descr
036500960613     C*
036600960613     C                   ENDSR
036700960613     C/EJECT
036800960613     C************************************************************
036900960613     C* SETLL CON ORDINAMENTO PER INDIRIZZO SOGGETTO
037000960613     C************************************************************
037100960613     C     SET05         BEGSR
037200960613     C*
037300960607     C                   MOVE      CIT302        INDLocalit
037400950228     C*
037500950228     C* apro il file se non lo è mai stato
037600950228     C     $IND02        IFEQ      *BLANK
037700950320     C                   OPEN      ANIND02J
037800950228     C                   MOVE      *ON           $IND02
037900950228     C                   ENDIF
038000950227     C*
038100950227     C     K1IND2        SETLL     ANIND02
038200950227     C*
038300950227     C                   ENDSR
038400960607     C/EJECT
038500960607     C************************************************************
038600960607     C* SQL
038700960607     C* RICERCA PER STRINGA PER INDIRIZZO SOGGETTO
038800960607     C************************************************************
038900960607     C     SET06         BEGSR
039000960607     C*
039100960607     C/EXEC SQL
039200960607     C+   DECLARE A2 CURSOR FOR
039300960607     C+     SELECT *
039400960610     C+      FROM  ANCLN00F , ANIND00F
039500960610     C+      WHERE
039600960610     C+                  CLNSOGG  = INDSOGG
039700960607     C+      AND         INDTPIND  = CLNTPIND
039800960607     C+      AND         INDCDIND  = CLNCDIND
039900960607     C+      AND         CLNSOCIETA  = :SOC302
040000960607     C+      AND         CLNKSC  BETWEEN :KSC302 AND :KSC999
040100960607     C+      AND         CLNDESBREV  BETWEEN :DES302 AND :DES999
040200960607     C+      AND         INDLOCALIT  LIKE :STRING
040300960607     C+      AND         INDSTATO  BETWEEN :STA302 AND :STA999
040400960607     C+      AND         INDPROV  BETWEEN :PRV302 AND :PRV999
040500960607     C+      AND         INDCAP  BETWEEN :CAP302 AND :CAP999
040600960607     C+      AND         CLNLINEAV  BETWEEN :LDV302 AND :LDV999
040700960607     C+      AND         CLNSOGG  BETWEEN :SOG302 AND :SOG999
040800960607     C+      AND         CLNFILIALE  BETWEEN :FIL000 AND :FIL999
040900960607     C+      AND         CLNKCC  BETWEEN :KCC000 AND :KCC999
041000960607     C+      AND       ( CLNDTFIRPC > :OGGI
041100960607     C+       OR         CLNDTFIRPC = '0001-01-01' )
041200960607     C+      FOR FETCH ONLY
041300960607     C/END-EXEC
041400960607     C*
041500960607     C                   ENDSR
041600941130     C/EJECT
041700990930     C************************************************************
041800990927     C* SQL : ricerca con parzializzazione per solo dati statistici:
041900990927     C* join tra ANCLN, ANIND, ANCLS.
042000990927     C************************************************************
042100990927     C     SET15         BEGSR
042200990927     C*
042300990927     C*  preparo la stringa per eseguire l'SQL.
042400990927     C*
042500991005     C                   movel     *blank        SQ             1200
042600990928     C                   movel     'SELECT'      sq
042700990928     C     sq            cat       'ANCLN00F':1  sq
042800990928     C     sq            cat       '.*,':0       sq
042900990928     C     sq            cat       'ANSOG00F':1  sq
043000990928     C     sq            cat       '.*,':0       sq
043100990928     C     sq            cat       'ANIND00F':1  sq
043200991005     C     sq            cat       '.*':0        sq
043300990928     C     sq            cat       'FROM':1      sq
043400990928     C     sq            cat       'ANCLN00F':1  sq
043500990928     C* join con ANIND
043600990928     C     sq            cat       'INNER JOIN':1sq
043700990928     C     sq            cat       'ANIND00F':1  sq
043800990928     C     sq            cat       'ON':1        sq
043900990928     C* predicati di join con ANIND
044000990928     C     sq            cat       'CLNSOGG=':1  sq
044100990928     C     sq            cat       'INDSOGG':0   sq
044200990928     C     sq            cat       'AND':1       sq
044300990928     C     sq            cat       'CLNTPIND=':1 sq
044400990928     C     sq            cat       'INDTPIND':0  sq
044500990928     C     sq            cat       'AND':1       sq
044600990928     C     sq            cat       'CLNCDIND=':1 sq
044700990928     C     sq            cat       'INDCDIND':0  sq
044800990928     C* join con ANSOG00F
044900990928     C     sq            cat       'INNER JOIN':1sq
045000990928     C     sq            cat       'ANSOG00F':1  sq
045100990928     C     sq            cat       'ON':1        sq
045200991005     C* predicati di join con ANSOG
045300990928     C     sq            cat       'CLNSOGG=':1  sq
045400990929     C     sq            cat       'SOGSOGG':0   sq
045500990928     C* where conditions: dipendono dai parametri moduli.
045600990928     C     sq            cat       'WHERE':1     sq
045700991005     C     sq            cat       'CLNSOCIETA':1sq
045800990929     C     sq            cat       '=''':0       sq
045900990928     C     sq            cat       soc302:0      sq
046000990929     C     sq            cat       '''':0        sq
046100991005     C*  LDV = '   ' come da selezioni
046200991005     C     sq            cat       'AND':1       sq
046300991005     C     sq            cat       'CLNLINEAV':1 sq
046400991005     C     sq            cat       '=''   ''':0  sq
046500991005     C* lista comprensiva delle filiali
046600990929     C                   if        FSN302 = *off
046700990928     C     sq            cat       'AND':1       sq
046800990928     C     sq            cat       'CLNFILIALE':1sq
046900990928     C     sq            cat       '= 0':1       sq
047000050125     C                   endif
047100050125B0771C* solo attivi in contabilità
047200150424>>>>>C*                  if        ATT302 = '2'
047300150424>>>>>C*    sq            cat       'AND':1       sq
047400150424>>>>>C*    sq            cat       'CLNKCC':1    sq
047500150424>>>>>C*    sq            cat       '<>'' ''':0   sq
047600150424>>>>>C*                  endif
047700050125  "  C* solo non attivi in contabilità
047800150424>>>>>C*                  if        ATT302 = '3'
047900150424>>>>>C*    sq            cat       'AND':1       sq
048000150424>>>>>C*    sq            cat       'CLNKCC':1    sq
048100150424>>>>>C*    sq            cat       '='' ''':0    sq
048200150424>>>>>C*                  endif
048300050125  "  C* lista NON comprensiva annullati
048400050125  "  C                   if        ANN302 <> *On
048500050125  "  C                   move      ZDateISO      ZDateALFA
048600050125  "  C     sq            cat       'AND (':1     sq
048700050125  "  C     sq            cat       'CLNDTFIRPC':0sq
048800050125  "  C     sq            cat       '>''':0       sq
048900050125  "  C     sq            cat       ZDateALFA:0   sq
049000050125  "  C     sq            cat       '''':0        sq
049100050125  "  C     sq            cat       'OR':1        sq
049200050125  "  C     sq            cat       'CLNDTFIRPC':1sq
049300050125  "  C     sq            cat       '=''':0       sq
049400050125  "  C     sq            cat       '0001-01-01':0sq
049500050125  "  C     sq            cat       ''')':0       sq
049600050125B0771C                   endif
049700050125     C* KSC deve essere contenuto nella sub query
049800991005     C     sq            cat       'AND':1       sq
049900991006     C     sq            cat       'CLNKSC':1    sq
050000991006     C     sq            cat       'CONCAT':1    sq
050100991006     C     sq            cat       'DIGITS(CLN':1sq
050200991006     C     sq            cat       'FILIALE)':0  sq
050300991006     C     sq            cat       'IN':1        sq
050400991005     C* subquery
050500991005     C     sq            cat       '(SELECT':1   sq
050600991005     C     sq            cat       'DISTINCT':1  sq
050700991005     C     sq            cat       'CLSKSC':1    sq
050800991006     C     sq            cat       'CONCAT':1    sq
050900991006     C     sq            cat       'DIGITS(CLS':1sq
051000991006     C     sq            cat       'FILIALE)':0  sq
051100991005     C     sq            cat       'FROM':1      sq
051200991005     C     sq            cat       'ANCLS00F':1  sq
051300041122     C     sq            cat       'WHERE':1     sq
051400041122B0758C     sq            cat       'CLSSOCIETA':1sq
051500041122  "  C     sq            cat       '=''':0       sq
051600041122  "  C     sq            cat       soc302:0      sq
051700041122  "  C     sq            cat       '''':0        sq
051800041122B0758C     sq            cat       'AND':1       sq
051900041122     C* dati statistici per linea
052000991005     C                   if        dsl009 = '1'
052100990928     C     sq            cat       'CLSLINEAV':1 sq
052200991005     C     sq            cat       '<>''   ''':0 sq
052300991005     C                   else
052400991005     C     sq            cat       'CLSLINEAV':1 sq
052500991005     C     sq            cat       '=''   ''':0  sq
052600990928     C                   endif
052700990928     C* where conditions su attributi statistici.
052800990928     C                   movea     cst302        $s
052900990928     C     1             do        30            $cdsta            2 0
053000990929     C                   if        $s($cdsta) <> *blank
053100990928     C     sq            cat       'AND':1       sq
053200990928     C     sq            cat       'CLSCDSTA':1  sq
053300990928     C                   movel     $cdsta        $Xcdsta           2
053400990928     C     sq            cat       $Xcdsta:0     sq
053500990929     C     sq            cat       '=''':0       sq
053600990929     C     sq            cat       $s($cdsta):0  sq
053700990929     C     sq            cat       '''':0        sq
053800990929     C                   endif
053900990928     C                   enddo
054000991005     C*
054100991005     C     sq            cat       'AND':1       sq
054200991005     C     sq            cat       'ANCLN00F.':1 sq
054300991005     C     sq            cat       'CLNKSC=':0   sq
054400991005     C     sq            cat       'CLSKSC':0    sq
054500991005     C     sq            cat       'AND':1       sq
054600991005     C     sq            cat       'ANCLN00F.':1 sq
054700991005     C     sq            cat       'CLNFILIALE':0sq
054800991005     C     sq            cat       '=CLS':0      sq
054900991005     C     sq            cat       'FILIALE)':0  sq
055000991005     C*  order by
055100990928     C     sq            cat       'ORDER BY':1  sq
055200990928     C                   SELECT
055300990928     C* Ord.1 = cliente
055400990928     C     ORD302        WHENEQ    1
055500991012     C     sq            cat       'CLNKSC,':1   sq
055600991012     C     sq            cat       'CLNFILIALE':0sq
055700991012     C     sq            cat       ',CLNLINEAV':0sq
055800990928     C* Ord.2 = descrizione breve
055900990928     C     ORD302        WHENEQ    2
056000991006     C     sq            cat       'CLNDESBREV':1sq
056100990928     C* Ord.3 = ragione sociale
056200990928     C     ORD302        WHENEQ    3
056300990928     C     sq            cat       'SOGDES':1    sq
056400990928     C* Ord.4 = località
056500990928     C     ORD302        WHENEQ    4
056600990928     C     sq            cat       'INDLOCALIT':1sq
056700990928     C* Ord.5 = stato
056800990928     C     ORD302        WHENEQ    5
056900990928     C     sq            cat       'INDSTATO':1  sq
057000990928     C* Ord.6 = provincia
057100990928     C     ORD302        WHENEQ    6
057200990928     C     sq            cat       'INDPROV':1   sq
057300990928     C* Ord.7 = CAP
057400990928     C     ORD302        WHENEQ    7
057500990928     C     sq            cat       'INDCAP':1    sq
057600990928     C* Ord.8 = partita iva
057700990928     C     ORD302        WHENEQ    8
057800990928     C     sq            cat       'SOGPARTIVA':1sq
057900990928     C* Ord.9 = codice fiscale
058000990928     C     ORD302        WHENEQ    9
058100990928     C     sq            cat       'SOGCDFISC':1 sq
058200990928     C* Ord.10= linea di vendita
058300990928     C     ORD302        WHENEQ    10
058400991012     C     sq            cat       'CLNLINEAV,':1sq
058500991012     C     sq            cat       'CLNFILIALE':0sq
058600990928     C* Ord.20= soggetto
058700990928     C     ORD302        WHENEQ    20
058800990928     C     sq            cat       'CLNSOGG':1   sq
058900990928     C* Ord.21= capoconto
059000990928     C     ORD302        WHENEQ    21
059100990928     C     sq            cat       'CLNKCC':1    sq
059200990928     C                   ENDSL
059300990928     C*
059400990928     C     sq            cat       'FOR FETCH':1 sq
059500990928     C     sq            cat       'ONLY':1      sq
059600990928     C*
059700990928     C/EXEC SQL
059800990928     C+   PREPARE S1 FROM :SQ
059900990928     C/END-EXEC
060000990927     C*
060100990928     C/EXEC SQL
060200990928     C+   DECLARE A1 CURSOR FOR S1
060300990928     C/END-EXEC
060400990927     C*
060500990927     C                   ENDSR
060600990930     C/EJECT
060700950227     C************************************************************
060800950227     C* SETLL CON ORDINAMENTO PER STATO SOGGETTO
060900950227     C************************************************************
061000950227     C     SET07         BEGSR
061100950227     C*
061200960607     C                   MOVE      STA302        INDStato
061300950228     C*
061400950228     C* apro il file se non lo è mai stato
061500950228     C     $IND04        IFEQ      *BLANK
061600950320     C                   OPEN      ANIND04J
061700950228     C                   MOVE      *ON           $IND04
061800950228     C                   ENDIF
061900950227     C*
062000950227     C     K1IND4        SETLL     ANIND04
062100950227     C*
062200950227     C                   ENDSR
062300950227     C/EJECT
062400950227     C************************************************************
062500950227     C* SETLL CON ORDINAMENTO PER PROVINCIA SOGGETTO
062600950227     C************************************************************
062700950227     C     SET08         BEGSR
062800950227     C*
062900960607     C                   MOVE      PRV302        INDProv
063000950228     C*
063100950228     C* apro il file se non lo è mai stato
063200950228     C     $IND03        IFEQ      *BLANK
063300950320     C                   OPEN      ANIND03J
063400950228     C                   MOVE      *ON           $IND03
063500950228     C                   ENDIF
063600950227     C*
063700950227     C     K1IND3        SETLL     ANIND03
063800950227     C*
063900950227     C                   ENDSR
064000950227     C/EJECT
064100950227     C************************************************************
064200950227     C* SETLL CON ORDINAMENTO PER CAP SOGGETTO
064300950227     C************************************************************
064400950227     C     SET09         BEGSR
064500950227     C*
064600950228     C                   MOVE      CAP302        INDCAP
064700950228     C*
064800950228     C* apro il file se non lo è mai stato
064900950228     C     $IND05        IFEQ      *BLANK
065000950320     C                   OPEN      ANIND05J
065100950228     C                   MOVE      *ON           $IND05
065200950228     C                   ENDIF
065300950227     C*
065400950227     C     K1IND5        SETLL     ANIND05
065500950227     C*
065600950227     C                   ENDSR
065700950227     C/EJECT
065800950227     C************************************************************
065900950227     C* SETLL CON ORDINAMENTO PER PARTITA IVA SOGGETTO
066000950227     C************************************************************
066100950228     C     SET10         BEGSR
066200950227     C*
066300960607     C                   MOVE      IVA302        SOGPartIVA
066400950228     C*
066500950228     C* apro il file se non lo è mai stato
066600950228     C     $SOG04        IFEQ      *BLANK
066700950320     C                   OPEN      ANSOG04J
066800950228     C                   MOVE      *ON           $SOG04
066900950228     C                   ENDIF
067000950227     C*
067100950227     C     K1SOG4        SETLL     ANSOG04
067200950227     C*
067300950227     C                   ENDSR
067400950227     C/EJECT
067500950227     C************************************************************
067600950227     C* SETLL CON ORDINAMENTO PER CODICE FISCALE SOGGETTO
067700950227     C************************************************************
067800950228     C     SET11         BEGSR
067900950227     C*
068000960607     C                   MOVE      CDF302        SOGCdFisc
068100950228     C*
068200950228     C* apro il file se non lo è mai stato
068300950228     C     $SOG03        IFEQ      *BLANK
068400950320     C                   OPEN      ANSOG03J
068500950228     C                   MOVE      *ON           $SOG03
068600950228     C                   ENDIF
068700950227     C*
068800950227     C     K1SOG3        SETLL     ANSOG03
068900950227     C*
069000950227     C                   ENDSR
069100950228     C/EJECT
069200950228     C************************************************************
069300950228     C* SETLL CON ORDINAMENTO PER LINEA DI VENDITA
069400950228     C************************************************************
069500950228     C     SET12         BEGSR
069600950228     C*
069700960607     C                   MOVE      SOC302        CLNSocieta
069800960607     C                   MOVE      LDV302        CLNLineaV
069900950228     C*
070000950228     C                   SELECT
070100950228     C* NO FILIALI
070200950228     C     FSN302        WHENEQ    *OFF
070300960607     C                   MOVE      *ZERO         CLNFiliale
070400950228     C* apro il file se non lo è mai stato
070500950228     C     $CLN05        IFEQ      *BLANK
070600950228     C                   OPEN      ANCLN05L
070700950228     C                   MOVE      *ON           $CLN05
070800950228     C                   ENDIF
070900950228     C     K3CLN5        SETLL     ANCLN05
071000950228     C* SI FILIALI
071100950228     C     FSN302        WHENEQ    *ON
071200950228     C* apro il file se non lo è mai stato
071300950228     C     $CLN03        IFEQ      *BLANK
071400950228     C                   OPEN      ANCLN03L
071500950228     C                   MOVE      *ON           $CLN03
071600950228     C                   ENDIF
071700950228     C     K2CLN3        SETLL     ANCLN03
071800950228     C*
071900950228     C                   ENDSL
072000950228     C*
072100950228     C                   ENDSR
072200950227     C/EJECT
072300950227     C************************************************************
072400950227     C* SETLL CON ORDINAMENTO PER SOGGETTO
072500950227     C************************************************************
072600950227     C     SET13         BEGSR
072700950227     C*
072800960607     C                   MOVE      SOC302        CLNSocieta
072900960607     C                   MOVE      SOG302        CLNSogg
073000950228     C*
073100950303     C* apro il file se non lo è mai stato
073200950303     C     $CLN07        IFEQ      *BLANK
073300950303     C                   OPEN      ANCLN09L
073400950303     C                   MOVE      *ON           $CLN07
073500950303     C                   ENDIF
073600970324     C* apro il file se non lo è mai stato(serve anche questo, altrimenti
073700970324     c* si rompe)
073800970324     C     $CLN10        IFEQ      *BLANK
073900970324     C                   OPEN      ANCLN10L
074000970324     C                   MOVE      *ON           $CLN10
074100970324     C                   ENDIF
074200950303     C*
074300950303     C     K2CLN9        SETLL     ANCLN09
074400950227     C*
074500950227     C                   ENDSR
074600951003     C/EJECT
074700951003     C************************************************************
074800951003     C* SETLL CON ORDINAMENTO PER KCC
074900951003     C************************************************************
075000951003     C     SET14         BEGSR
075100951003     C*
075200960607     C                   MOVE      SOC302        CLNSocieta
075300951003     C                   MOVE      KCC302        CLNKCC
075400951003     C*
075500951003     C* apro il file se non lo è mai stato
075600951003     C     $CLN10        IFEQ      *BLANK
075700951003     C                   OPEN      ANCLN10L
075800951003     C                   MOVE      *ON           $CLN10
075900951003     C                   ENDIF
076000951003     C*
076100951003     C     K2CLN0        SETLL     ANCLN10
076200951003     C*
076300951003     C                   ENDSR
076400950227     C/EJECT
076500950227     C************************************************************
076600960607     C* Apertura cursori
076700950227     C************************************************************
076800960607     C     OpnCur        BEGSR
076900950228     C*
077000960607     C                   SELECT
077100960607     C*
077200960607     C     $GEST         WHENEQ    '06'
077300960607     C*
077400960607     C/EXEC SQL
077500960607     C+     OPEN A2
077600960607     C/END-EXEC
077700990930     C*
077800990927     C     $GEST         WHENEQ    '15'
077900990927     C*
078000990928     C/EXEC SQL
078100990928     C+     OPEN A1
078200990930     C/END-EXEC
078300960607     C*
078400960607     C                   ENDSL
078500960607     C*
078600960607     C                   ENDSR
078700960607     C/EJECT
078800960607     C************************************************************
078900960607     C* SELEZIONE READ  IN BASE ALL'ORDINAMENTO
079000960607     C************************************************************
079100960607     C     READ          BEGSR
079200960607     C*
079300950228     C                   MOVEL     *OFF          $EFILE
079400950228     C                   MOVEL     *OFF          $RCDOK
079500950228     C*
079600950228     C     $EFILE        DOUEQ     *ON
079700950228     C     $RCDOK        OREQ      *ON
079800950227     C*
079900950228     C* LETTURE CHE HANNO PER ORDINAMENTO UN CAMPO DI ANCLN00F
080000950228     C*
080100950228     C                   SELECT
080200950228     C     $GEST         WHENEQ    '01'
080300950228     C     $GEST         OREQ      '02'
080400950228     C     $GEST         OREQ      '12'
080500950310     C     $GEST         OREQ      '13'
080600951003     C     $GEST         OREQ      '14'
080700950228     C*
080800950228     C     $GEST         CASEQ     '01'          RED01
080900950228     C     $GEST         CASEQ     '02'          RED02
081000950228     C     $GEST         CASEQ     '12'          RED12
081100950310     C     $GEST         CASEQ     '13'          RED13
081200951003     C     $GEST         CASEQ     '14'          RED14
081300950228     C                   ENDCS
081400950228     C*
081500950228     C                   MOVE      *IN21         $EFILE
081600950228     C*
081700950228     C* se trovato rcd
081800950228     C     $EFILE        IFEQ      *OFF
081900950228     C* viene ctrl la validità del rcd
082000950228     C                   EXSR      SELCLN
082100950228     C* se rcd valido
082200950228     C     $RCDOK        IFEQ      *ON
082300950228     C* reperisco e ctrl la validità del soggetto relativo
082400960607     C                   MOVE      CLNSogg       SOGSogg
082500950228     C                   EXSR      REPSOG
082600950228     C* se rcd valido
082700950228     C     $RCDOK        IFEQ      *ON
082800950228     C* reperisco e ctrl la validità dell'indirizzo relativo
082900960607     C                   MOVE      CLNSogg       INDSogg
083000960607     C                   MOVE      CLNTpInd      INDTpInd
083100960607     C                   MOVE      CLNCdInd      INDCdInd
083200950228     C                   EXSR      REPIND
083300950228     C                   ENDIF
083400950228     C                   ENDIF
083500950228     C                   ENDIF
083600950228     C*
083700950228     C* LETTURE CHE HANNO PER ORDINAMENTO UN CAMPO DI ANSOG00F
083800950228     C*
083900950228     C     $GEST         WHENEQ    '03'
084000950228     C     $GEST         OREQ      '10'
084100950228     C     $GEST         OREQ      '11'
084200950228     C*
084300950227     C     $GEST         CASEQ     '03'          RED03
084400950228     C     $GEST         CASEQ     '10'          RED10
084500950228     C     $GEST         CASEQ     '11'          RED11
084600950228     C                   ENDCS
084700950228     C*
084800950228     C                   MOVE      *IN21         $EFILE
084900950228     C*
085000950228     C* se trovato rcd
085100950228     C     $EFILE        IFEQ      *OFF
085200960607     C* ctrl la validità del soggetto
085300950228     C                   EXSR      SELSOG
085400950228     C* se rcd valido
085500950228     C     $RCDOK        IFEQ      *ON
085600960607     C* ctrl la validità del cliente relativo
085700960606     C                   EXSR      SELCLN
085800950228     C* se rcd valido
085900950228     C     $RCDOK        IFEQ      *ON
086000950228     C* reperisco e ctrl la validità dell'indirizzo relativo
086100960607     C                   MOVE      SOGSogg       INDSogg
086200960607     C                   MOVE      CLNTpInd      INDTpInd
086300960607     C                   MOVE      CLNCdInd      INDCdInd
086400960606     C                   EXSR      REPIND
086500950228     C                   ENDIF
086600950228     C                   ENDIF
086700960607     C                   ENDIF
086800950228     C*
086900950228     C* LETTURE CHE HANNO PER ORDINAMENTO UN CAMPO DI ANIND00F
087000950228     C*
087100950228     C     $GEST         WHENEQ    '05'
087200950228     C     $GEST         OREQ      '07'
087300950228     C     $GEST         OREQ      '08'
087400950228     C     $GEST         OREQ      '09'
087500950228     C*
087600950228     C     $GEST         CASEQ     '05'          RED05
087700950228     C     $GEST         CASEQ     '07'          RED07
087800950228     C     $GEST         CASEQ     '08'          RED08
087900950228     C     $GEST         CASEQ     '09'          RED09
088000950228     C                   ENDCS
088100950228     C*
088200950228     C                   MOVE      *IN21         $EFILE
088300950228     C*
088400950228     C* se trovato rcd
088500950228     C     $EFILE        IFEQ      *OFF
088600960607     C* ctrl la validità del rcd
088700950228     C                   EXSR      SELIND
088800950228     C* se rcd valido
088900950228     C     $RCDOK        IFEQ      *ON
089000960607     C* ctrl la validità del cliente
089100950320     C                   EXSR      SELCLN
089200950228     C* se rcd valido
089300950228     C     $RCDOK        IFEQ      *ON
089400950228     C* reperisco e ctrl la validità del soggetto relativo
089500960607     C                   MOVE      INDSogg       SOGSogg
089600950228     C                   EXSR      REPSOG
089700950228     C                   ENDIF
089800950228     C                   ENDIF
089900950228     C                   ENDIF
090000960607     C*
090100960613     C* CHAIMATE ALLA RICERCA ESTESA
090200960607     C*
090300960613     C     $GEST         WHENEQ    '04'
090400960613     C                   EXSR      RED04
090500960613     C*
090600960613     C* LETTURE CHE PREVEDONO L'SQL
090700960613     C*
090800960613     C     $GEST         WHENEQ    '06'
090900960613     C                   EXSR      RED06
091000020321
091100020321     c                   IF        xscGest = '2'
091200020321
091300020321     c                   if        $rcdok = *on
091400020321     c                   exsr      selPjnWeb
091500020321     c                   endIf
091600020321
091700020321     c                   else
091800020321
091900991014     C* devo anche controllare selezioni su ANGUSR
092000991014     C                   if        $rcdok = *on
092100991014     C                   exsr      selangusr
092200991014     C                   endif
092300020321
092400020321     c                   EndIf
092500990930     C*
092600990928     C     $GEST         WHENEQ    '15'
092700991014     C                   EXSR      RED15
092800020321
092900020321     c                   IF        xscGest = '2'
093000020321
093100020321     c                   if        $rcdok = *on
093200020321     c                   exsr      selPjnWeb
093300020321     c                   endIf
093400020321
093500020321     c                   else
093600020321
093700991014     C* devo anche controllare selezioni su ANGUSR
093800991014     C                   if        $rcdok = *on
093900991014     C                   exsr      selangusr
094000991014     C                   endif
094100020321
094200020321     c                   EndIf
094300950228     C*
094400950228     C                   ENDSL
094500950228     C*
094600950228     C                   ENDDO
094700950228     C*
094800950228     C                   MOVE      $EFILE        END302
094900950227     C*
095000950227     C                   ENDSR
095100950227     C/EJECT
095200950227     C************************************************************
095300950227     C* READ  CON ORDINAMENTO PER CODICE CLIENTE
095400950227     C************************************************************
095500950227     C     RED01         BEGSR
095600950227     C*
095700950227     C                   SELECT
095800950227     C* NO LINEE DI VENDITA
095900950227     C* NO FILIALI
096000950227     C     LDV302        WHENEQ    *BLANK
096100950227     C     FSN302        ANDEQ     *OFF
096200950227     C*
096300950227     C     K3CLN5        READE     ANCLN05                                21
096400950227     C*
096500950227     C* SI LINEE DI VENDITA
096600950227     C* NO FILIALI
096700950227     C     LDV302        WHENNE    *BLANK
096800950227     C     FSN302        ANDEQ     *OFF
096900950227     C*
097000950227     C     K2CLN4        READE     ANCLN04                                21
097100950227     C*
097200950227     C* NO LINEE DI VENDITA
097300950227     C* SI FILIALI
097400950227     C     LDV302        WHENEQ    *BLANK
097500950227     C     FSN302        ANDEQ     *ON
097600950227     C*
097700950227     C     K2CLN3        READE     ANCLN03                                21
097800950227     C*
097900950227     C* SI LINEE DI VENDITA
098000950227     C* SI FILIALI
098100950227     C     LDV302        WHENNE    *BLANK
098200950227     C     FSN302        ANDEQ     *ON
098300950227     C*
098400950227     C     K1CLN3        READE     ANCLN03                                21
098500950227     C*
098600950227     C                   ENDSL
098700950227     C*
098800950227     C                   ENDSR
098900950227     C/EJECT
099000950227     C************************************************************
099100950227     C* READ  CON ORDINAMENTO PER DESCRIZIONE BREVE CLIENTE
099200950227     C************************************************************
099300950227     C     RED02         BEGSR
099400950227     C*
099500950227     C     K1CLN8        READE     ANCLN08                                21
099600950227     C*
099700950227     C                   ENDSR
099800950228     C/EJECT
099900950228     C************************************************************
100000950228     C* READ  CON ORDINAMENTO PER RAGIONE SOCIALE SOGGETTO
100100950228     C************************************************************
100200950228     C     RED03         BEGSR
100300950228     C*
100400950228     C                   READ      ANSOG02                                21
100500950228     C*
100600950228     C                   ENDSR
100700941130     C/EJECT
100800941130     C************************************************************
100900950228     C* RICERCA PER STRINGA PER RAGIONE SOCIALE SOGGETTO
101000941130     C************************************************************
101100950228     C     RED04         BEGSR
101200960613     C*
101300960613     C                   EVAL      LenOut1 = %Size(SOGDS)
101400991014     C                   EVAL      LenOut = %Size(ancln00f)
101500000103     C                   EVAL      LenOut2 = %Size(INDDS)
101600000103     C                   eval      FlgSog = *Off
101700960613     C                   EVAL      Risultato = 0
101800960613     C*
101900960613     C                   CALL      'ANA908R1'
102000960613     C                   PARM      'C'           TpSogg
102100960613     C                   PARM                    Soc302
102200960613     C                   PARM                    PrimaVolta
102300960613     C                   PARM      '1'           TpRicerca
102400960613     C                   PARM                    Descr
102500960613     C                   PARM                    Risultato
102600960613     C                   PARM                    LenOut1
102700000103     C                   PARM                    SOGDS
102800960613     C                   PARM                    LenOut
102900991014     C                   PARM                    ancln00f
103000000103     C                   PARM                    LenOut2
103100000103     C                   PARM                    INDDS
103200000103     C                   PARM                    FlgSog
103300960606     C*
103400960613     C                   EVAL      PrimaVolta = 0
103500960613     C*
103600960613     C                   IF        Risultato = 1
103700960613     C                   EVAL      $EFile = *on
103800960610     C*
103900960606     C                   ELSE
104000960607     C                   EVAL      $EFile = *off
104100960607     C                   EVAL      $RcdOK = *on
104200960617     C* ctrl la validità del soggetto
104300000103     C                   EXSR      SELSOG
104400960617     C* se rcd valido
104500960617     C     $RCDOK        IFEQ      *ON
104600960617     C* ctrl la validità del cliente
104700960617     C                   EXSR      SELCLN
104800960617     C* se rcd valido
104900960617     C     $RCDOK        IFEQ      *ON
105000960610     C* reperisco e ctrl la validità dell'indirizzo relativo
105100991230     C                   MOVE      INDSogg       SOGSogg
105200960610     C                   MOVE      CLNTpInd      INDTpInd
105300960610     C                   MOVE      CLNCdInd      INDCdInd
105400960610     C                   EXSR      REPIND
105500960610     C                   ENDIF
105600960617     C                   ENDIF
105700960617     C                   ENDIF
105800960610     C*
105900941130     C                   ENDSR
106000960606     C*
106100950228     C/EJECT
106200950228     C************************************************************
106300950228     C* READ  CON ORDINAMENTO PER INDIRIZZO
106400950228     C************************************************************
106500950228     C     RED05         BEGSR
106600950228     C*
106700950228     C                   READ      ANIND02                                21
106800950228     C*
106900950228     C                   ENDSR
107000950228     C/EJECT
107100950228     C************************************************************
107200950228     C* RICERCA PER STRINGA PER INDIRIZZO
107300950228     C************************************************************
107400950228     C     RED06         BEGSR
107500950228     C*
107600960607     C/EXEC SQL
107700991014     C+     FETCH A2 INTO :ANCLN00F , :INDDS
107800960607     C/END-EXEC
107900960607     C*
108000960607     C                   IF        SqlCod = 100
108100960607     C                   EVAL      $EFile = *on
108200960610     C*
108300960607     C                   ELSE
108400960607     C                   EVAL      $EFile = *off
108500960607     C                   EVAL      $RcdOK = *on
108600970723     C* reperisco dati di ansog
108700960610     C                   MOVE      INDSogg       SOGSogg
108800960610     C                   EXSR      REPSOG
108900960610     C*
109000960610     C                   ENDIF
109100950228     C*
109200950228     C                   ENDSR
109300950228     C/EJECT
109400950228     C************************************************************
109500950228     C* READ  CON ORDINAMENTO PER STATO
109600950228     C************************************************************
109700950228     C     RED07         BEGSR
109800950228     C*
109900950228     C                   READ      ANIND04                                21
110000950228     C*
110100950228     C                   ENDSR
110200950228     C/EJECT
110300950228     C************************************************************
110400950228     C* READ  CON ORDINAMENTO PER PROVINCIA
110500950228     C************************************************************
110600950228     C     RED08         BEGSR
110700950228     C*
110800950228     C                   READ      ANIND03                                21
110900950228     C*
111000950228     C                   ENDSR
111100950228     C/EJECT
111200950228     C************************************************************
111300950228     C* READ  CON ORDINAMENTO PER CAP
111400950228     C************************************************************
111500950228     C     RED09         BEGSR
111600950228     C*
111700950228     C                   READ      ANIND05                                21
111800950228     C*
111900950228     C                   ENDSR
112000950228     C/EJECT
112100950228     C************************************************************
112200950228     C* READ  CON ORDINAMENTO PER PARTITA IVA SOGGETTO
112300950228     C************************************************************
112400950228     C     RED10         BEGSR
112500950228     C*
112600950228     C                   READ      ANSOG04                                21
112700950228     C*
112800950228     C                   ENDSR
112900950228     C/EJECT
113000950228     C************************************************************
113100950228     C* READ  CON ORDINAMENTO PER CODICE FISCALE SOGGETTO
113200950228     C************************************************************
113300950228     C     RED11         BEGSR
113400950228     C*
113500950228     C                   READ      ANSOG03                                21
113600950228     C*
113700950228     C                   ENDSR
113800950228     C/EJECT
113900950228     C************************************************************
114000950228     C* READ  CON ORDINAMENTO PER LINEA DI VENDITA
114100950228     C************************************************************
114200950228     C     RED12         BEGSR
114300950228     C*
114400950228     C                   SELECT
114500950228     C* NO FILIALI
114600950228     C     FSN302        WHENEQ    *OFF
114700950310     C     K2CLN5        READE     ANCLN05                                21
114800950228     C* SI FILIALI
114900950228     C     FSN302        WHENEQ    *ON
115000950228     C     K1CLN3        READE     ANCLN03                                21
115100950228     C*
115200950228     C                   ENDSL
115300950228     C*
115400950228     C                   ENDSR
115500950228     C/EJECT
115600950228     C************************************************************
115700950228     C* READ  CON ORDINAMENTO PER SOGGETTO
115800950228     C************************************************************
115900950228     C     RED13         BEGSR
116000950228     C*
116100950310     C     K1CLN9        READE     ANCLN09                                21
116200950228     C*
116300950228     C                   ENDSR
116400951003     C/EJECT
116500951003     C************************************************************
116600951003     C* READ  CON ORDINAMENTO PER KCC
116700951003     C************************************************************
116800951003     C     RED14         BEGSR
116900951003     C*
117000951003     C     K1CLN0        READE     ANCLN10                                21
117100951003     C*
117200951003     C                   ENDSR
117300950227     C/EJECT
117400990930     C************************************************************
117500990928     C* RICERCA per soli dati statistici
117600990928     C************************************************************
117700990928     C     RED15         BEGSR
117800990928     C*
117900991014     C/EXEC SQL
118000991014     C+     FETCH A1 INTO :ANCLN00F , :SOGDS , :INDDS
118100991014     C/END-EXEC
118200990928     C*
118300990928     C                   IF        SqlCod = 100
118400990928     C                   EVAL      $EFile = *on
118500990928     C*
118600990928     C                   ELSE
118700990928     C                   EVAL      $EFile = *off
118800990928     C                   EVAL      $RcdOK = *on
118900990928     C                   ENDIF
119000990928     C*
119100990928     C                   ENDSR
119200990930     C/EJECT
119300950227     C************************************************************
119400950228     C* REPERIMENTO SOGGETTO (SE ORDIN. PER CLIENTE O INDIRIZZO)
119500950227     C************************************************************
119600950227     C     REPSOG        BEGSR
119700950227     C*
119800950301     C* apro il file se non lo è mai stato
119900950301     C     $SOG01        IFEQ      *BLANK
120000950320     C                   OPEN      ANSOG01L
120100950301     C                   MOVE      *ON           $SOG01
120200950301     C                   ENDIF
120300950301     C*
120400960607     C     SOGSogg       CHAIN     ANSOG01                            21
120500950227     C*
120600950227     C     *IN21         IFEQ      *OFF
120700950227     C                   EXSR      SELSOG
120800950227     C                   ELSE
120900950227     C                   MOVE      *OFF          $RCDOK
121000950227     C                   ENDIF
121100950227     C*
121200950227     C                   ENDSR
121300950228     C/EJECT
121400950228     C************************************************************
121500950228     C* REPERIMENTO INDIRIZZO (SE ORDIN. PER CLIENTE O SOGGETTO)
121600950228     C************************************************************
121700950228     C     REPIND        BEGSR
121800950301     C*
121900950301     C* apro il file se non lo è mai stato
122000950301     C     $IND01        IFEQ      *BLANK
122100950320     C                   OPEN      ANIND01L
122200950301     C                   MOVE      *ON           $IND01
122300950301     C                   ENDIF
122400950228     C*
122500950228     C     K3IND1        CHAIN     ANIND01                            21
122600950228     C*
122700950228     C     *IN21         IFEQ      *OFF
122800950228     C                   EXSR      SELIND
122900950228     C                   ELSE
123000950228     C                   MOVE      *OFF          $RCDOK
123100950228     C                   ENDIF
123200950228     C*
123300950228     C                   ENDSR
123400950227     C/EJECT
123500950227     C************************************************************
123600950227     C* SELEZIONI SU ANCLN00F
123700950227     C************************************************************
123800950227     C     SELCLN        BEGSR
123900950227     C*
124000950227     C                   MOVEL     *ON           $RCDOK
124100040226     C*
124200040226     C* se ordinamento per descrizione estesa
124300040226     C     $gest         ifeq      '04'
124400040226     c* e se chiesta linea di vendita
124500040226     C     ldv302        ifne      *blanks
124600040226     C                   exsr      CercaRec
124700040226     C                   endIF
124800040302     C                   endIF
124801150424>>>>>** Selezione sui codici di bollettazione e/o fatturazione.
124802150424>>>>>C                   SELECT
124803150424>>>>>C                   WHEN      att302 = '1'                                 Tutti
124804150424>>>>>C                   WHEN      att302 = '2'                                 Solo fatturazione
124805150424>>>>>C                   IF        clnCliFatt <> *BLANK AND clnCliFatt <> clnKsc
124806150424>>>>>C                   EVAL      $rcdOk = *OFF
124807150424>>>>>C                   ENDIF
124808150424>>>>>C                   WHEN      att302 = '3'                                 Solo bollettazione
124809150424>>>>>C                   IF        clnCliFatt = *BLANK OR clnCliFatt = clnKsc
124810150424>>>>>C                   EVAL      $rcdOk = *OFF
124811150424>>>>>C                   ENDIF
124813150424>>>>>C                   ENDSL
124900950227     C*
125000980105     C* se richiesto cli. di fatturazione lo controllo uguale
125100980105     C     CLF302        ifNE      *blanks
125200980105     C     clnCLIFATT    andNE     CLF302
125300980105     C                   movel     *off          $RCDOK
125400980105     C                   endIF
125500950227     C*
125600950227     C* SELEZIONI RISPETTO AI VALORI RCD
125700950320     C* societa non compresa
125800960607     C     CLNSocieta    IFNE      SOC302
125900950320     C                   MOVEL     *OFF          $RCDOK
126000950320     C                   ENDIF
126100950227     C* cliente annullato
126200950310     C     $RCDOK        IFEQ      *ON
126300960607     C     CLNDtFiRpC    IFGT      $DT0
126400960607     C     CLNDtFiRpC    ANDLT     OGGI
126500950227     C                   MOVEL     *OFF          $RCDOK
126600950310     C                   ENDIF
126700950310     C                   ENDIF
126800950227     C*
126900950227     C* cliente presente in contabilità
127000950310     C     $RCDOK        IFEQ      *ON
127100950310     C     CLNKCC        IFGT      KCC999
127200950310     C     CLNKCC        ORLT      KCC000
127300950227     C                   MOVEL     *OFF          $RCDOK
127400950310     C                   ENDIF
127500950310     C                   ENDIF
127600950227     C*
127700950227     C* filiale cliente
127800950310     C     $RCDOK        IFEQ      *ON
127900960607     C     CLNFiliale    IFGT      FIL999
128000960607     C     CLNFiliale    ORLT      FIL000
128100950227     C                   MOVEL     *OFF          $RCDOK
128200950310     C                   ENDIF
128300950310     C                   ENDIF
128400950310     C*
128500950227     C*
128600950227     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
128700950227     C*
128800951003     C* codice capoconto non compreso nella scelta
128900951003     C     CLNKCC        IFLT      KCC302
129000951003     C     CLNKCC        ORGT      KCC999
129100950310     C                   MOVEL     *OFF          $RCDOK
129200951003     C     CLNKCC        IFGT      KCC999
129300950310     C     ORD302        ANDEQ     20
129400951003     C     *HIVAL        SETLL     ANCLN10
129500950310     C                   ENDIF
129600950310     C                   ENDIF
129700951003     C* codice soggetto non compreso nella scelta
129800960607     C     CLNSogg       IFLT      SOG302
129900960607     C     CLNSogg       ORGT      SOG999
130000951003     C                   MOVEL     *OFF          $RCDOK
130100960607     C     CLNSogg       IFGT      SOG999
130200951003     C     ORD302        ANDEQ     20
130300951003     C     *HIVAL        SETLL     ANCLN09
130400951003     C                   ENDIF
130500951003     C                   ENDIF
130600950227     C* codice cliente non compreso nella scelta
130700950310     C     CLNKSC        IFLT      KSC302
130800950227     C     CLNKSC        ORGT      KSC999
130900950227     C                   MOVEL     *OFF          $RCDOK
131000950227     C     CLNKSC        IFGT      KSC999
131100950227     C     ORD302        ANDEQ     1
131200950227     C                   SELECT
131300950227     C* NO LINEE DI VENDITA
131400950227     C* NO FILIALI
131500950227     C     LDV302        WHENEQ    *BLANK
131600950227     C     FSN302        ANDEQ     *OFF
131700950227     C*
131800950227     C     *HIVAL        SETLL     ANCLN05
131900950227     C*
132000950227     C* SI LINEE DI VENDITA
132100950227     C* NO FILIALI
132200950227     C     LDV302        WHENNE    *BLANK
132300950227     C     FSN302        ANDEQ     *OFF
132400950227     C*
132500950227     C     *HIVAL        SETLL     ANCLN04
132600950227     C*
132700950227     C* NO LINEE DI VENDITA
132800950227     C* SI FILIALI
132900950227     C     LDV302        WHENEQ    *BLANK
133000950227     C     FSN302        ANDEQ     *ON
133100950227     C*
133200950227     C     *HIVAL        SETLL     ANCLN03
133300950227     C*
133400950227     C* SI LINEE DI VENDITA
133500950227     C* SI FILIALI
133600950227     C     LDV302        WHENNE    *BLANK
133700950227     C     FSN302        ANDEQ     *ON
133800950227     C*
133900000905     C* devo riposizionarmi:
134000000905     C* - se ClnLdv > LDV999 --> fine lettura
134100000905     C*   altrimenti devo posizionarmi alla prossima rottura di LDV
134200000905     C*
134300000905     C                   if        ClnLineav > Ldv999
134400950227     C     *HIVAL        SETLL     ANCLN03
134500000905     C                   else
134600000905     C* se Ksc > Ksc999 mi riposiziono alla prossima rottura di LDV
134700000905     C                   if        ClnKsc > Ksc999
134800000905     C     K2Cln3        Setgt     AnCln03
134900000905     C     K1Cln3        reade     Ancln03                                21
135000000905     C                   if        *in21 = *off
135100000905     C                   eval      ClnKsc = Ksc302
135200000905     C     K4Cln3        setll     Ancln03
135300000905     C                   endif
135400000905     C                   endif
135500000905     C                   endif
135600950227     C*
135700950227     C                   ENDSL
135800950227     C                   ENDIF
135900950310     C                   ENDIF
136000950227     C*
136100950227     C* descrizione breve non compresa nella scelta
136200960607     C     CLNDesBrev    IFLT      DES302
136300960607     C     CLNDesBrev    ORGT      DES999
136400950227     C                   MOVEL     *OFF          $RCDOK
136500960607     C     CLNDesBrev    IFGT      DES999
136600950227     C     ORD302        ANDEQ     2
136700950227     C     *HIVAL        SETLL     ANCLN08
136800950227     C                   ENDIF
136900950310     C                   ENDIF
137000950227     C*
137100950227     C* linea di vendita non compresa nella scelta
137200960607     C     CLNLineaV     IFLT      LDV302
137300960607     C     CLNLineaV     ORGT      LDV999
137400950227     C                   MOVEL     *OFF          $RCDOK
137500960607     C     CLNLineaV     IFGT      LDV999
137600950227     C     ORD302        ANDEQ     10
137700950315     C                   SELECT
137800950315     C* NO FILIALI
137900950315     C     FSN302        WHENEQ    *OFF
138000950315     C     *HIVAL        SETLL     ANCLN05
138100950315     C* SI FILIALI
138200950315     C     FSN302        WHENEQ    *ON
138300950227     C     *HIVAL        SETLL     ANCLN03
138400950315     C                   ENDSL
138500950227     C                   ENDIF
138600950310     C                   ENDIF
138700980105      *
138800980105      * Controllo dati statistici per selezione clienti
138900980105      * se deve essere effettuato questo controllo perchè sono stati
139000980105      * richiesti dati statistici, reimposto il flag di record valido $rcdoK
139100980105     C     cst302        IFNE      *blank
139200990930     C     $rcdok        andeq     *on
139300050125     X*** ex A0240       move      *off          $rcdok
139400990930     C* REDCLS solo se record valido
139500980105     C                   EXSR      REDCLS
139600980105     C                   endif
139700020321
139800020321     c                   IF        xscGest = '2'
139900020321
140000020321     c                   if        $rcdok = *on
140100020321     c                   exsr      selPjnWeb
140200020321     c                   endIf
140300020321
140400020321     c                   else
140500020321
140600991230     C*
140700991014     C* inserisco la chiamata alla procedura per la validazione
140800991014     C* del record se richiesto da Setup generale SICOM.
140900991014     C*
141000991014     C                   if        $rcdok = *on
141100991014     C                   exsr      selangusr
141200991014     C                   endif
141300991014     C*
141400991014     C*
141500020321     c                   EndIf
141600020321
141700950227     C                   ENDSR
141800950227     C/EJECT
141900991014     C************************************************************
142000991014     C* SELEZIONI SU ANGUSR ( se richiesto da SICOM)
142100991014     C************************************************************
142200991014     C     selangusr     begsr
142300020617     C*
142400020617     C                   if        jbusin <>'N'
142500020617     C*
142600991014     C                   eval      soc004 = soc302
142700991014     C                   eval      unita004 = uni302
142800020125     C                   eval      esito = *off
142900020125     C*
143000020125     C* ricerca campi statistici del cliente
143100020125     C* apro il file se non lo è mai stato
143200020125     C                   clear                   ancls00f
143300020125     C                   if        $cls01=*blank
143400020125     C                   open      ancls01l
143500020125     C                   move      *on           $cls01
143600020125     C                   endif
143700020125     C*
143800020125     C                   move      clnsocieta    clssoc
143900020125     C                   move      clnksc        clsksc
144000020125     C                   z-add     clnfiliale    clsfil
144100020125     C* dati statistici per LDV
144200020125     C                   if        dsl009 = '1'
144300020125     C                   if        clnlineav <> *blank
144400020125     C                   movel     clnlineav     clsldv
144500020125     C     K04CLS01      setll     ancls01l
144600020125     C     K04CLS01      reade     ancls01l                               21
144700020125     C                   else
144800020125     C     K03CLS01      setll     ancls01l
144900020125     C     *in21         doweq     *off
145000020125     C     K03CLS01      reade     ancls01l                               21
145100020125     C   21              leave
145200020125     C                   enddo
145300020125     C                   endif
145400020125     C*
145500020125     C                   else
145600020125     C*
145700020125     C                   move      '   '         clsldv
145800020125     C     K04CLS01      setll     ancls01l
145900020125     C     K04CLS01      reade     ancls01l                               21
146000020125     C                   endif
146100020125     C*
146200020125     C                   if        *in21=*off
146300020213     C                   movea     $a            sccodsta
146400020213     C                   eval      clssocieta=   clssoc
146500020213     C                   eval      clsunita  =   clsuni
146600020213     C                   eval      clslineav =   clsldv
146700020213     C                   eval      clsfiliale=   clsfil
146800020125     C                   endif
146900020125     C                   callp     X004VALCLN(soc004 :
147000991014     C                                        unita004 :
147100991014     C                                        kpjba :
147200020125     C                                        ancln00f :
147300020125     C                                        ancls00f :
147400020125     C                                        esito)
147500991014     C*
147600991014     C                   if        esito = '1'
147700991014     C                   movel     *off          $RCDOK
147800991014     C                   endif
147900991014     C
148000991014     C                   endif
148100991014     C*
148200991014     C                   endsr
148300991014     C/EJECT
148400020321      ************************************************************
148500020321      * SELEZIONI SU PJNWEB ( se richiesto da SICOM)
148600020321      ************************************************************
148700020321     c     selPjnWeb     begsr
148800020617
148900020617     C                   if        jbusin <>'N'
149000020617     C
149100020321     c                   Call      'PJNWA00R'
149200020321     c                   parm                    soc302
149300020321     c                   parm                    uni302
149400020321     c                   parm                    kpjba
149500020321     c                   parm                    ancln00f
149600020321     c                   parm      *off          esito
149700020321
149800020321     c                   if        esito = '1'
149900020321     c                   movel     *off          $RCDOK
150000020321     c                   endif
150100020321     c
150200020321     c                   endif
150300020321
150400020321     c                   endsr
150500020321     C/EJECT
150600040226     C************************************************************
150700040226     C* Cerca record di ancln
150800040226     C************************************************************
150900040226     C     CercaRec      BEGSR
151000040226     C* apro il file se non lo è mai stato
151100040226     C     $CLN02        IFEQ      *BLANK
151200040226     C                   OPEN      ANCLN02L
151300040226     C                   MOVE      *ON           $CLN02            1
151400040226     C                   ENDIF
151500040226     c                   eval      clnlineav =  LDV302
151600040226     C*
151700040226     c     k5cln2        CHAIN     ancln02L
151800040226     C                   if        not %found (ancln02l)
151900040226     c                   eval      $rcdok = *off
152000040226     c                   endif
152100040226     C*
152200040226     c                   endsr
152300950227     C************************************************************
152400950227     C* SELEZIONI SU ANSOG00F
152500950227     C************************************************************
152600950227     C     SELSOG        BEGSR
152700950227     C*
152800950227     C                   MOVEL     *ON           $RCDOK
152900950227     C*
153000950227     C* SELEZIONI RISPETTO AI VALORI RCD
153100950227     C*
153200950227     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
153300950227     C*
153400950227     C* codice soggetto non compreso nella scelta
153500960607     C     SOGSogg       IFLT      SOG302
153600960607     C     SOGSogg       ORGT      SOG999
153700950227     C                   MOVEL     *OFF          $RCDOK
153800960607     C     SOGSogg       IFGT      SOG999
153900950227     C     ORD302        ANDEQ     20
154000950227     C     *HIVAL        SETLL     ANSOG01
154100950227     C                   ENDIF
154200950310     C                   ENDIF
154300950227     C*
154400950227     C* ragione sociale non compresa nella scelta
154500950310     C* se non richiesta ricerca per scansione
154600950310     C     SOGDES        IFLT      RSO302
154700950310     C     ST1302        ANDEQ     *OFF
154800950227     C     SOGDES        ORGT      RSO999
154900950310     C     ST1302        ANDEQ     *OFF
155000950227     C                   MOVEL     *OFF          $RCDOK
155100950227     C     SOGDES        IFGT      RSO999
155200950227     C     ORD302        ANDEQ     3
155300950227     C     *HIVAL        SETLL     ANSOG02
155400950227     C                   ENDIF
155500950310     C                   ENDIF
155600950227     C*
155700950227     C* partita iva non compresa nella scelta
155800960607     C     SOGPartIVA    IFLT      IVA302
155900960607     C     SOGPartIVA    ORGT      IVA999
156000950227     C                   MOVEL     *OFF          $RCDOK
156100960607     C     SOGPartIVA    IFGT      IVA999
156200950227     C     ORD302        ANDEQ     8
156300950227     C     *HIVAL        SETLL     ANSOG04
156400950227     C                   ENDIF
156500950310     C                   ENDIF
156600950227     C*
156700950227     C* codice fiscale non compreso nella scelta
156800960607     C     SOGCdFisc     IFLT      CDF302
156900960607     C     SOGCdFisc     ORGT      CDF999
157000950227     C                   MOVEL     *OFF          $RCDOK
157100960607     C     SOGCdFisc     IFGT      CDF999
157200950227     C     ORD302        ANDEQ     9
157300950227     C     *HIVAL        SETLL     ANSOG03
157400950227     C                   ENDIF
157500950310     C                   ENDIF
157600950227     C*
157700950227     C                   ENDSR
157800950227     C/EJECT
157900950227     C************************************************************
158000950227     C* SELEZIONI SU ANIND00F
158100950227     C************************************************************
158200950227     C     SELIND        BEGSR
158300950227     C*
158400950227     C                   MOVEL     *ON           $RCDOK
158500950227     C*
158600950227     C*
158700950227     C* SELEZIONI RISPETTO AI VALORI RCD
158800950227     C*
158900950227     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
159000950227     C*
159100950227     C* indirizzo non compreso nella scelta
159200950310     C* e se non richiesta scansione per indirizzo
159300960607     C     INDLocalit    IFLT      CIT302
159400950310     C     ST2302        ANDEQ     *OFF
159500960607     C     INDLocalit    ORGT      CIT999
159600950310     C     ST2302        ANDEQ     *OFF
159700950227     C                   MOVEL     *OFF          $RCDOK
159800960607     C     INDLocalit    IFGT      CIT999
159900950227     C     ORD302        ANDEQ     4
160000950227     C     *HIVAL        SETLL     ANIND02
160100950227     C                   ENDIF
160200950310     C                   ENDIF
160300950227     C*
160400950227     C* stato non compreso nella scelta
160500960607     C     INDStato      IFLT      STA302
160600960607     C     INDStato      ORGT      STA999
160700950227     C                   MOVEL     *OFF          $RCDOK
160800960607     C     INDStato      IFGT      STA999
160900950227     C     ORD302        ANDEQ     5
161000950227     C     *HIVAL        SETLL     ANIND04
161100950227     C                   ENDIF
161200950310     C                   ENDIF
161300950227     C*
161400950227     C* provincia non compresa nella scelta
161500960607     C     INDProv       IFLT      PRV302
161600960607     C     INDProv       ORGT      PRV999
161700950227     C                   MOVEL     *OFF          $RCDOK
161800960607     C     INDProv       IFGT      PRV999
161900950227     C     ORD302        ANDEQ     6
162000950227     C     *HIVAL        SETLL     ANIND03
162100950227     C                   ENDIF
162200950310     C                   ENDIF
162300950227     C*
162400950227     C* CAP non compreso nella scelta
162500950310     C     INDCAP        IFLT      CAP302
162600950227     C     INDCAP        ORGT      CAP999
162700950227     C                   MOVEL     *OFF          $RCDOK
162800950227     C     INDCAP        IFGT      CAP999
162900950227     C     ORD302        ANDEQ     7
163000950227     C     *HIVAL        SETLL     ANIND05
163100950227     C                   ENDIF
163200950310     C                   ENDIF
163300950227     C*
163400950227     C                   ENDSR
163500960606     C/EJECT
163600960606     C************************************************************
163700960606     C* CHIUSURA CURSORE
163800960606     C************************************************************
163900960606     C     CLOCUR        BEGSR
164000960606     C*
164100960613     C                   SELECT
164200960607     C*
164300960607     C     $GEST         WHENEQ    '06'
164400960607     C*
164500960607     C/EXEC SQL
164600960607     C+     CLOSE A2
164700960607     C/END-EXEC
164800960606     C*
164900990930     C     $GEST         WHENEQ    '15'
165000990929     C*
165100990929     C/EXEC SQL
165200990929     C+     CLOSE A1
165300990929     C/END-EXEC
165400990930     C*
165500960606     C                   ENDSL
165600960607     C*
165700960607     C                   MOVE      *OFF          $OPEN
165800960606     C*
165900960606     C                   ENDSR
166000941130     C/EJECT
166100941130     C************************************************************
166200941130     C* OPERAZIONI INIZIALI
166300941130     C************************************************************
166400941130     C     *INZSR        BEGSR
166500000114     C     *DMY          Move      Udate         ZDateISO
166600000114     C     *JobRun       Move      ZDateISO      ZDate             6 0
166700941130     C*
166800941130     C* Reperimento parametri
166900941130     C*
167000941130     C     *ENTRY        PLIST
167100991014     C                   PARM                    KPJBA
167200941229     C                   PARM                    A302DS
167300991014     C                   PARM                    ancln00f
167400941212     C                   PARM                    SOGDS
167500941130     C                   PARM                    INDDS
167600000714     C*
167700991203     C* converto le minuscole in maiuscole per la ricerca
167800991203     C*
167900991203     C     LOW:UPP       xlate     RSO302        RSO302
168000000714     C*
168100941130     C                   ENDSR
168200941130     C/EJECT
168300941130     C************************************************************
168400941130     C* INIZIALIZZAZIONE VARIABILI
168500941130     C************************************************************
168600941130     C     INZVAR        BEGSR
168700950228     C*
168800950228     C* Se è la prima chiamata eseguo l'inizializzazione variabili
168900950228     C*
169000950228     C     RIC302        IFEQ      '0'
169100990930     C* l'aggancio dell'anagrafica dipende dall'impostazione del
169200990923     C* parametro modulo ANCLS00F.
169300990923     C*
169400990923     C                   movel     *blank        XPARIC
169500990923     C                   movel     SOC302        XPASOC
169600990923     C                   movel     'ANCLS00F'    XPAPAR
169700990923     C                   movel     *blank        XPAOUT
169800990923     C                   callb     'XPAR'
169900990923     C                   parm                    XPASOC            3
170000990923     C                   parm                    XPAPAR            8
170100990923     C                   parm                    XPAOUT
170200990923     C                   parm                    XPAERR            1
170300990923     C                   parm                    XPARIC            1
170400990923     C     XPAERR        ifeq      '0'
170500990923     C                   movel     XPAOUT        P009DS
170600990923     C                   else
170700990923     C                   movel     *blanks       P009DS
170800990923     C                   endif
170900990928     C*
171000990930     C*
171100991014     C* leggo SICOM per Jbusiness.
171200991014     C                   movel     *blank        XPARIC
171300991014     C                   movel     *blank        XPAPAR
171400991014     C                   movel     SOC302        XPASOC
171500991014     C                   movel     'SICOM'       XPAPAR
171600991014     C                   movel     *blank        XPAOUT
171700991014     C                   callb     'XPAR'
171800991014     C                   parm                    XPASOC            3
171900991014     C                   parm                    XPAPAR            8
172000991014     C                   parm                    XPAOUT
172100991014     C                   parm                    XPAERR            1
172200991014     C                   parm                    XPARIC            1
172300991014     C     XPAERR        ifeq      '0'
172400991014     C                   movel     XPAOUT        SICOM
172500991014     C                   else
172600991014     C                   movel     *blanks       SICOM
172700991014     C                   endif
172800991014     C*
172900020321
173000040302      * reperimento dati società
173100020508     c* chiamata per reperire solo i dati di ansif, altrimenti viene
173200020508     c* emessa la finestra di richiesta società se si parte da una
173300020508     c* societa diversa dalla capofila e si chiede 'C' di capofila da
173400020508     c* ANA300R
173500020508     c                   MOVEL     'SOC002'      TIPXSC
173600040302     c                   MOVEL     Soc302        SOCXSC
173700020321     c                   CALLb     'XSOC'
173800020321     c                   parm                    TIPXSC            6
173900020321     c                   parm                    SOCXSC            3
174000020321     c                   parm                    CDSXSC            9 0
174100020321     c                   parm                    MODXSC            3
174200020321     c                   parm      *BLANKS       RTNXSC            1
174300020321     c                   parm                    XSOCDS
174400020321     c                   parm                    KPJBA
174500020321     c                   If        RtnXsc <> '1'
174600020321     c                   movel     XSOCDS        SOC001
174700020321     c                   Else
174800020321     c                   ExSr      ENDPGM
174900020321     c                   Endif
175000960606     C*
175100960607     C* Chiudo il cursore se era già aperto (solo per ricerche di stringa)
175200960606     C*
175300960606     C     $OPEN         IFEQ      *ON
175400960606     C                   EXSR      CLOCUR
175500960606     C                   ENDIF
175600941201     C*
175700941201     C* Variabili gestione pgm
175800941201     C*
175900990930     C* se nessuna selezione a parte i dati statistici imposto $gest=15
176000990927     C* ed eseguo un SQL su ANCLN, ANIND, ANCLS.
176100990927     C*
176200990927     C                   if        sog302 = *blank
176300990927     C                             and kcc302 = *blank
176400990927     C                             and ksc302 = *blank
176500990927     C                             and des302 = *blank
176600990927     C                             and rso302 = *blank
176700990927     C                             and cit302 = *blank
176800990927     C                             and sta302 = *blank
176900990927     C                             and prv302 = *blank
177000990927     C                             and cap302 = *blank
177100990927     C                             and iva302 = *blank
177200990927     C                             and cdf302 = *blank
177300990927     C                             and ldv302 = *blank
177400060720B0887C                             and clf302 = *blank
177500990928     C                             and cst302 <> *blank
177600990928     C                   move      '15'          $gest
177700990927     C                   else
177800990930     C*
177900941130     C                   SELECT
178000941229     C     ORD302        WHENEQ    1
178100941130     C                   MOVE      '01'          $GEST
178200941229     C     ORD302        WHENEQ    2
178300941212     C                   MOVE      '02'          $GEST
178400941229     C     ORD302        WHENEQ    3
178500941229     C     ST1302        ANDEQ     *OFF
178600941212     C                   MOVE      '03'          $GEST
178700941229     C     ORD302        WHENEQ    3
178800941229     C     ST1302        ANDEQ     *ON
178900941212     C                   MOVE      '04'          $GEST
179000941229     C     ORD302        WHENEQ    4
179100941229     C     ST2302        ANDEQ     *OFF
179200941212     C                   MOVE      '05'          $GEST
179300941229     C     ORD302        WHENEQ    4
179400941229     C     ST2302        ANDEQ     *ON
179500941212     C                   MOVE      '06'          $GEST
179600941229     C     ORD302        WHENEQ    5
179700941212     C                   MOVE      '07'          $GEST
179800941229     C     ORD302        WHENEQ    6
179900941212     C                   MOVE      '08'          $GEST
180000941229     C     ORD302        WHENEQ    7
180100941130     C                   MOVE      '09'          $GEST
180200941229     C     ORD302        WHENEQ    8
180300941212     C                   MOVE      '10'          $GEST
180400941229     C     ORD302        WHENEQ    9
180500941212     C                   MOVE      '11'          $GEST
180600941229     C     ORD302        WHENEQ    10
180700941212     C                   MOVE      '12'          $GEST
180800951003     C     ORD302        WHENEQ    20
180900951003     C                   MOVE      '13'          $GEST
181000951003     C     ORD302        WHENEQ    21
181100951003     C                   MOVE      '14'          $GEST
181200950321     C     ORD302        WHENEQ    99
181300950321     C                   MOVE      '01'          $GEST
181400950321     C                   Z-ADD     1             ORD302
181500941130     C                   ENDSL
181600990930     C*
181700990930     C                   endif
181800941130     C*
181900941130     C* Variabili appoggio
182000941130     C*
182100950228     C                   MOVE      *OFF          $EFILE
182200950228     C                   MOVE      *OFF          $RCDOK
182300950228     C                   MOVE      *BLANK        $TIP
182400960606     C*
182500960606     C                   MOVE      *BLANK        STRING           50
182600960613     C     $GEST         IFEQ      '06'
182700960606     C                   MOVEA     *ALL' '       STR
182800960606     C                   MOVEA     CIT302        STR(2)
182900960606     C                   MOVE      '%'           STR(1)
183000960606     C                   Z-ADD     50            X
183100960606     C                   MOVE      '%'           STR(X)
183200960606     C     X             DOUEQ     1
183300960606     C                   SUB       1             X
183400960606     C                   SETOFF                                       21
183500960606     C     STR(X)        IFEQ      *BLANK
183600960606     C                   MOVE      '%'           STR(X)
183700960606     C                   SETON                                        21
183800960606     C                   ENDIF
183900960606     C   21              ENDDO
184000960606     C                   ENDIF
184100960606     C                   MOVEA     STR           STRING
184200950103     C*
184300980105     C                   CALL      'XDT4000'
184400950103     C                   PARM                    XDTAMG           10
184500000114     C                   PARM      ZDate         XDTGMA            6 0
184600950103     C                   PARM      4             XDTSTA            1 0
184700950103     C                   MOVEL     XDTAMG        WDATE
184800941205     C*
184900941205     C* eseguo CAT per creare limite superiore delle BETWEEN
185000941205A1211X*                  MOVEL     *ALL'9'       ALL9             75
185100941205A1211C                   MOVEL     *hival        ALL9             75
185200941229     C     DES302        CAT       ALL9:0        DES999
185300941229     C     RSO302        CAT       ALL9:0        RSO999
185400941229     C     CIT302        CAT       ALL9:0        CIT999
185500941229     C     STA302        CAT       ALL9:0        STA999
185600941229     C     PRV302        CAT       ALL9:0        PRV999
185700941229     C     CAP302        CAT       ALL9:0        CAP999
185800941229     C     IVA302        CAT       ALL9:0        IVA999
185900941229     C     CDF302        CAT       ALL9:0        CDF999
186000990930     C* ATTENZIONE: se nelle parzializzazioni non ho indicato nessuna
186100990923     C* LDV devo restituire solo i sottoconti con linea *blank;
186200990923     C* devo quindi abblencare il limite superiore della LDV (LDV999).
186300990923     C* In questo caso quindi non ha senso richiedere un ordinamento
186400990930     C* per linea di vendita (ottengo un SFL vuoto).
186500991014     C     LDV302        IFEQ      *BLANK
186600991014     C                   MOVEL     *BLANK        LDV999
186700991014     C                   ELSE
186800941229     C     LDV302        CAT       ALL9:0        LDV999
186900991014     C                   END
187000941229     C     KSC302        CAT       ALL9:0        KSC999
187100951003     C     SOG302        CAT       ALL9:0        SOG999
187200951003     C     KCC302        CAT       ALL9:0        KCC999
187300941212     C* anche le filiali
187400941229     C     FSN302        IFEQ      *ON
187500941212     C                   MOVE      *ZERO         FIL000
187600941212A1211X*                  MOVE      *ALL'9'       FIL999
187700941212A1211C                   MOVE      *hival        FIL999
187800941212     C* senza le filiali
187900941212     C                   ELSE
188000941212     C                   MOVE      *ZERO         FIL000
188100941212     C                   MOVE      *ZERO         FIL999
188200941212     C                   ENDIF
188300150424>>>>>C*                  SELECT
188400950111     C* tutti
188500150424>>>>>C*    ATT302        WHENEQ    '1'
188600950111     C                   MOVE      *BLANK        KCC000
188700150424>>>>>C*    KCC302        IFEQ      *BLANK
188800950111A1211X*                  MOVE      *ALL'9'       KCC999
188900950111A1211C                   MOVE      *hival        KCC999
189000150424>>>>>C*                  ENDIF
189100150424>>>>>C* solo i clienti presenti in contabilità
189200150424>>>>>C* (ovvero quelli che hanno già associato il capoconto)
189300150424>>>>>C*    ATT302        WHENEQ    '2'
189400150424>>>>>C*                  MOVE      *BLANK        KCC000
189500150424>>>>>C* l'& è il primo carattere dopo il blank
189600150424>>>>>C*                  MOVE      '&'           KCC000
189700150424>>>>>C*    KCC302        IFEQ      *BLANK
189800150424A1211X*                  MOVE      *ALL'9'       KCC999
189900150424A1211C*                  MOVE      *hival        KCC999
190000150424>>>>>C*                  ENDIF
190100150424>>>>>C* solo i clienti NON presenti in contabilità
190200150424>>>>>C* (ovvero quelli che NON hanno già associato il capoconto)
190300150424>>>>>C*    ATT302        WHENEQ    '3'
190400150424>>>>>C*                  MOVE      *BLANK        KCC000
190500150424>>>>>C*                  MOVE      *BLANK        KCC999
190600150424>>>>>C*                  ENDSL
190700950103     C*
190800950103     C* no annullati
190900950103     C     ANN302        IFEQ      *OFF
191000950103     C                   MOVE      WDATE         OGGI
191100950103     C* si annullati
191200950103     C                   ELSE
191300950103     C                   MOVE      $DT0          OGGI
191400950103     C                   ENDIF
191500941213     C*
191600941213     C* richiesta Inizio elenco da...
191700950228     C*
191800950228     C     POS302        IFNE      *BLANK
191900950228     C                   EXSR      POSFIL
192000950228     C                   ENDIF
192100960607     C*
192200960607     C* eseguo le DECLARE solo per i caricamenti SQL
192300960607     C     $GEST         CASEQ     '06'          SET06
192400990930     C* preparo il cursore per l'SQL su ANCLN,ANIND,ANCLS.
192500990930     C     $gest         caseq     '15'          set15
192600960607     C                   ENDCS
192700960607     C*
192800960607     C* apro il cursore se non lo è (solo per i caricamenti SQL)
192900960607     C     $OPEN         IFNE      *on
193000960607     C                   EXSR      OpnCur
193100960607     C                   MOVE      *ON           $OPEN
193200960607     C                   ENDIF
193300941130     C*
193400941201     C                   ENDIF
193500941201     C*
193600941201     C                   ENDSR
193700941213     C/EJECT
193800941213     C************************************************************
193900941213     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
194000941213     C************************************************************
194100950228     C     POSFIL        BEGSR
194200941213     C*
194300941213     C* Gestione posizionamento
194400941213     C                   SELECT
194500941213     C* Ord.1 = cliente
194600941229     C     ORD302        WHENEQ    1
194700941229     C     POS302        ANDNE     *BLANK
194800941229     C                   MOVEL     POS302        KSC302
194900941213     C* Ord.2 = descrizione breve
195000941229     C     ORD302        WHENEQ    2
195100941229     C     POS302        ANDNE     *BLANK
195200941229     C                   MOVEL     POS302        DES302
195300941213     C* Ord.3 = ragione sociale
195400941229     C     ORD302        WHENEQ    3
195500941229     C     POS302        ANDNE     *BLANK
195600941229     C                   MOVEL     POS302        RSO302
195700941213     C* Ord.4 = località
195800941229     C     ORD302        WHENEQ    4
195900941229     C     POS302        ANDNE     *BLANK
196000941229     C                   MOVEL     POS302        CIT302
196100941213     C* Ord.5 = stato
196200941229     C     ORD302        WHENEQ    5
196300941229     C     POS302        ANDNE     *BLANK
196400941229     C                   MOVEL     POS302        STA302
196500941213     C* Ord.6 = provincia
196600941229     C     ORD302        WHENEQ    6
196700941229     C     POS302        ANDNE     *BLANK
196800941229     C                   MOVEL     POS302        PRV302
196900941213     C* Ord.7 = CAP
197000941229     C     ORD302        WHENEQ    7
197100941229     C     POS302        ANDNE     *BLANK
197200941229     C                   MOVEL     POS302        CAP302
197300941213     C* Ord.8 = partita iva
197400941229     C     ORD302        WHENEQ    8
197500941229     C     POS302        ANDNE     *BLANK
197600941229     C                   MOVEL     POS302        IVA302
197700941213     C* Ord.9 = codice fiscale
197800941229     C     ORD302        WHENEQ    9
197900941229     C     POS302        ANDNE     *BLANK
198000941229     C                   MOVEL     POS302        CDF302
198100941213     C* Ord.10= linea di vendita
198200941229     C     ORD302        WHENEQ    10
198300941229     C     POS302        ANDNE     *BLANK
198400941229     C                   MOVEL     POS302        LDV302
198500941213     C* Ord.20= soggetto
198600941229     C     ORD302        WHENEQ    20
198700941229     C     POS302        ANDNE     *BLANK
198800941229     C                   MOVEL     POS302        SOG302
198900951003     C* Ord.20= capoconto
199000951003     C     ORD302        WHENEQ    21
199100951003     C     POS302        ANDNE     *BLANK
199200951003     C                   MOVEL     POS302        KCC302
199300951003     C                   ENDSL
199400941213     C*
199500941213     C                   ENDSR
199600990929     C/EJECT
199700990930     C************************************************************
199800990929     C* confronto attributi statistici
199900990929     C************************************************************
200000990929     C     COMPSTA       BEGSR
200100990929     C*
200200990929     C* confronta 1 a 1 i dati  statistici valorizzati nelle
200300990929     C* parzializzazioni con i dati reperiti da ANCLS00F.
200400990929     C* In55=*on ---> rcd da scartare
200500990929     C                   eval      *in55 = *off
200600990929     C     1             do        30            $cdsta
200700990929     C                   if        $s($cdsta) <> *blank
200800990929     C                             and $a($cdsta) <> $s($cdsta)
200900990929     C                   eval      *in55 = *on
201000990929     C                   leave
201100990929     C                   endif
201200990929     C                   enddo
201300990929     C*
201400990929     C                   ENDSR
201500990930     C/EJECT
201600980105     C************************************************************
201700980105     C* CONTROLLO DATI STATISTICI SU FILE ANCLS00F
201800980105     C************************************************************
201900980105     C     REDCLS        BEGSR
202000980105      *
202100990929     C                   movea     cst302        $s
202200980105      *
202300980105     C* apro il file se non lo è mai stato
202400980105     C     $CLS01        IFEQ      *BLANK
202500980105     C                   OPEN      ANCLS01L
202600980105     C                   MOVE      *ON           $CLS01
202700980105     C                   ENDIF
202800980105      * imposto chiave
202900991005     C* pulisco tracciato campi statistici
203000991005     C                   movea     *blanks       $a
203100980105     C                   move      clnsocieta    clssoc
203200980105     C                   move      clnksc        clsksc
203300990930     C*
203400990923     C* l'aggancio di ANCLS00F dipende dalla configurazione del
203500990923     C* relativo parametro modulo e dalle parzializzazioni ricevute:
203600990923     C* 1) è indipendente dal fatto che sia richiesta una lista
203700990923     C*    comprensiva o meno delle filiali (fa fede sempre la filiale
203800990923     C*    del record di ANCLN corrente);
203900990923     C* 2) se dati statistici a livello di linea di vendita:
204000990923     C*   2.1) se non richiesta nessuna LDV (CLNLDV = *blank e non
204100990923     C*        LDV302) devo luppare su ANCLS per tutte le LDV con
204200990923     C*        filiale 0;
204300990923     C*   2.2) altrimenti aggancio con chiave a massimo dettaglio;
204400990923     C* 3) se dati statistici non a livello di LDV,indipendentemente
204500990923     C*    dal fatto che sia richiesta o meno una LDV, devo agganciare
204600990923     C*    ANCLS con in chiave la filiale (da ANCLN) e non la LDV.
204700990923     C*
204800990923     C                   z-add     clnfiliale    clsfil
204900990923     C* dati statistici per LDV
205000990927     C                   if        DSL009 = '1'
205100990927     C                   if        clnlineav <> *blank
205200990923     C                   movel     clnlineav     clsldv
205300990923     C     K04CLS01      setll     ancls01l
205400990923     C     K04CLS01      reade     ancls01l                               21
205500990929     C  N21              exsr      compsta
205600990929     C   55              move      *off          $rcdok
205700990929     C   21              move      *off          $rcdok
205800990927     C                   else
205900990923     C*          loop
206000990927     C                   move      *off          $rcdok
206100990927     C     K03CLS01      setll     ancls01l
206200990927     C     *in21         doweq     *off
206300990927     C     K03CLS01      reade     ancls01l                               21
206400991005     C   21              move      *off          $rcdok
206500991005     C   21              leave
206600990929     C  N21              exsr      compsta
206700990929     C  N55              eval      $rcdok = *on
206800990929     C  N55              seton                                        21
206900990927     C                   enddo
207000990927     C                   endif
207100990927     C*
207200990927     C                   else
207300990923     C*
207400990928     C                   move      '   '         clsldv
207500990928     C     K04CLS01      setll     ancls01l
207600990928     C     K04CLS01      reade     ancls01l                               21
207700990929     C  N21              exsr      compsta
207800990929     C   55              move      *off          $rcdok
207900990929     C   21              move      *off          $rcdok
208000990927     C*
208100990927     C                   endif
208200990930     C*
208300980105     C     finecls       ENDSR
208400980320     C/EJECT
208500050125B0096X/EJECT
208600941130     C************************************************************
208700941130     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
208800941130     C************************************************************
208900941130     C     DEFCAM        BEGSR
209000941130     C*
209100941130     C* klist
209200040226     C*
209300040226     C* ANCLN02L
209400040226     C     K5CLN2        KLIST
209500040226     C                   KFLD                    CLNSocieta
209600040226     C                   KFLD                    CLNSogg
209700040226     C                   KFLD                    CLNKSC
209800040226     C                   KFLD                    CLNfiliale
209900040226     C                   KFLD                    CLNlineav
210000941130     C*
210100950228     C* ANCLN03L
210200950228     C     K4CLN3        KLIST
210300960607     C                   KFLD                    CLNSocieta
210400960607     C                   KFLD                    CLNLineaV
210500950228     C                   KFLD                    CLNKSC
210600960607     C                   KFLD                    CLNFiliale
210700950228     C     K2CLN3        KLIST
210800960607     C                   KFLD                    CLNSocieta
210900960607     C                   KFLD                    CLNLineaV
211000950228     C     K1CLN3        KLIST
211100960607     C                   KFLD                    CLNSocieta
211200950228     C* ANCLN04L
211300950228     C     K4CLN4        KLIST
211400960607     C                   KFLD                    CLNSocieta
211500960607     C                   KFLD                    CLNFiliale
211600950228     C                   KFLD                    CLNKSC
211700960607     C                   KFLD                    CLNLineaV
211800950228     C     K2CLN4        KLIST
211900960607     C                   KFLD                    CLNSocieta
212000960607     C                   KFLD                    CLNFiliale
212100950228     C* ANCLN05L
212200950228     C     K4CLN5        KLIST
212300960607     C                   KFLD                    CLNSocieta
212400960607     C                   KFLD                    CLNFiliale
212500960607     C                   KFLD                    CLNLineaV
212600950228     C                   KFLD                    CLNKSC
212700950228     C     K3CLN5        KLIST
212800960607     C                   KFLD                    CLNSocieta
212900960607     C                   KFLD                    CLNFiliale
213000960607     C                   KFLD                    CLNLineaV
213100950228     C     K2CLN5        KLIST
213200960607     C                   KFLD                    CLNSocieta
213300960607     C                   KFLD                    CLNFiliale
213400950303     C* ANCLN09L
213500950303     C     K4CLN9        KLIST
213600960607     C                   KFLD                    CLNSocieta
213700960607     C                   KFLD                    CLNSogg
213800960607     C                   KFLD                    CLNTpInd
213900960607     C                   KFLD                    CLNCdInd
214000950303     C     K2CLN9        KLIST
214100960607     C                   KFLD                    CLNSocieta
214200960607     C                   KFLD                    CLNSogg
214300950303     C     K1CLN9        KLIST
214400960607     C                   KFLD                    CLNSocieta
214500951003     C* ANCLN10L
214600951003     C     K5CLN0        KLIST
214700960607     C                   KFLD                    CLNSocieta
214800951003     C                   KFLD                    CLNKCC
214900951003     C                   KFLD                    CLNKSC
215000960607     C                   KFLD                    CLNLineaV
215100960607     C                   KFLD                    CLNFiliale
215200951003     C     K2CLN0        KLIST
215300960607     C                   KFLD                    CLNSocieta
215400951003     C                   KFLD                    CLNKCC
215500951003     C     K1CLN0        KLIST
215600960607     C                   KFLD                    CLNSocieta
215700950228     C* ANCLN08L
215800950228     C     K2CLN8        KLIST
215900960607     C                   KFLD                    CLNSocieta
216000960607     C                   KFLD                    CLNDesBrev
216100950228     C     K1CLN8        KLIST
216200960607     C                   KFLD                    CLNSocieta
216300950320     C* ANSOG01L
216400950228     C     K1SOG1        KLIST
216500960607     C                   KFLD                    SOGSogg
216600950320     C* ANSOG02J
216700950228     C     K1SOG2        KLIST
216800950228     C                   KFLD                    SOGDES
216900950320     C* ANSOG03J
217000950228     C     K1SOG3        KLIST
217100960607     C                   KFLD                    SOGCdFisc
217200950320     C* ANSOG04J
217300950228     C     K1SOG4        KLIST
217400960607     C                   KFLD                    SOGPartIVA
217500950320     C* ANIND01L
217600950228     C     K3IND1        KLIST
217700960607     C                   KFLD                    INDSogg
217800960607     C                   KFLD                    INDTpInd
217900960607     C                   KFLD                    INDCdInd
218000950320     C* ANIND02J
218100950228     C     K1IND2        KLIST
218200960607     C                   KFLD                    INDLocalit
218300950320     C* ANIND03J
218400950228     C     K1IND3        KLIST
218500960607     C                   KFLD                    INDProv
218600950320     C* ANIND04J
218700950228     C     K1IND4        KLIST
218800960607     C                   KFLD                    INDStato
218900950320     C* ANIND05J
219000950228     C     K1IND5        KLIST
219100950228     C                   KFLD                    INDCAP
219200970924      *
219300980105      * ANCLS01L
219400980105     C     K2CLS1        KLIST
219500980105     C                   KFLD                    CLSSOC
219600980105     C                   KFLD                    CLSKSC
219700990930     C*
219800990923     C     K04CLS01      KLIST
219900990923     C                   KFLD                    CLSSOC
220000990923     C                   KFLD                    CLSKSC
220100990923     C                   KFLD                    CLSFIL
220200990923     C                   KFLD                    CLSLDV
220300990923     C*
220400990923     C     K03CLS01      KLIST
220500990923     C                   KFLD                    CLSSOC
220600990923     C                   KFLD                    CLSKSC
220700990930     C                   KFLD                    CLSFIL
220800990923     C*
220900941130     C* Variabili per gestione pgm
221000941130     C*
221100941130     C                   MOVE      *BLANK        $GEST             2
221200950228     C                   MOVE      *BLANK        $EFILE            1
221300950228     C                   MOVE      *BLANK        $RCDOK            1
221400950228     C                   MOVE      *BLANK        $TIP              1
221500960606     C                   MOVE      *BLANK        $OPEN             1
221600941201     C*
221700941130     C*
221800941130     C* Variabili appoggio
221900941130     C*
222000950228     C                   MOVE      *BLANK        $CLN03            1
222100950228     C                   MOVE      *BLANK        $CLN04            1
222200950228     C                   MOVE      *BLANK        $CLN05            1
222300950228     C                   MOVE      *BLANK        $CLN07            1
222400950228     C                   MOVE      *BLANK        $CLN08            1
222500951003     C                   MOVE      *BLANK        $CLN10            1
222600950228     C                   MOVE      *BLANK        $SOG01            1
222700950228     C                   MOVE      *BLANK        $SOG02            1
222800950228     C                   MOVE      *BLANK        $SOG03            1
222900950228     C                   MOVE      *BLANK        $SOG04            1
223000950228     C                   MOVE      *BLANK        $IND01            1
223100950228     C                   MOVE      *BLANK        $IND02            1
223200950228     C                   MOVE      *BLANK        $IND03            1
223300950228     C                   MOVE      *BLANK        $IND04            1
223400950228     C                   MOVE      *BLANK        $IND05            1
223500980105     C                   MOVE      *BLANK        $CLS01            1
223600941130     C* campi per parzializzazioni su rcd (solo alfanumerici)
223700960607     C     *LIKE         DEFINE    CLNDesBrev    DES999
223800941212     C     *LIKE         DEFINE    SOGDES        RSO999
223900960607     C     *LIKE         DEFINE    INDLocalit    CIT999
224000960607     C     *LIKE         DEFINE    INDStato      STA999
224100960607     C     *LIKE         DEFINE    INDProv       PRV999
224200941212     C     *LIKE         DEFINE    INDCAP        CAP999
224300960607     C     *LIKE         DEFINE    SOGPartIVA    IVA999
224400960607     C     *LIKE         DEFINE    SOGCdFisc     CDF999
224500960607     C     *LIKE         DEFINE    CLNLineaV     LDV999
224600941212     C     *LIKE         DEFINE    CLNKSC        KSC999
224700990927     C     *LIKE         DEFINE    CLNSogg       SOG999
224800960607     C     *LIKE         DEFINE    CLNFiliale    FIL000
224900960607     C     *LIKE         DEFINE    CLNFiliale    FIL999
225000941212     C     *LIKE         DEFINE    CLNKCC        KCC000
225100941212     C     *LIKE         DEFINE    CLNKCC        KCC999
225200960607     C     *LIKE         DEFINE    CLNDtFiRpC    WDATE
225300960607     C     *LIKE         DEFINE    CLNDtFiRpC    OGGI
225400990930     C     *LIKE         DEFINE    CST302        WCLS
225500941130     C*
225600941130     C* Indici
225700941130     C*
225800941130     C                   Z-ADD     0             X                 3 0
225900941130     C                   Z-ADD     0             Y                 3 0
226000980105     C                   Z-ADD     0             $X                3 0
226100941130     C*
226200941130     C                   ENDSR
