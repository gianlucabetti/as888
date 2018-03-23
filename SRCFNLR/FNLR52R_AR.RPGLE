000100920914     H DECEDIT('0,') DATEDIT(*DMY.)
000200941214      * FNLR52R *----------------------------------------------------*
000300911024      *     - ELABORAZIONE PRELISTA CONSEGNA -                       *
000400920227      * -------------------------------------------------------------*
000500990128     F* CARATTERISCITE VALIDE PER I 3 CASI AB/NO AB/ AB E N AB:
000600990128     F*
000700990128     F*     . E' ARRIVATO IL F.V.PARTENZE
000800990128     F*     . IL FV PARTENZA E' ABBINATO AD UN FOGLIO ARRIVI CON
000900990128     F*        DATA FOGLIO <= ALLA DATA RICHIESTA
001000990128     F*     . NON E' BLOCCATA (IN NESSUN MODO)
001100990128     F*     . NON HA UNA C.A. APERTA
001200920227      *-                                                             *
001300920227     F*    ABILITATE:
001400920227     F*     . L'ABILITAZIONE ARRIVI E' MAGGIORE DI ZERO
001500920227     F*     . IL C/A E L'ASS.TO E' ABILITATO
001600920227     F*     . NON E' IN DISTINTA
001700920227     F*     . LA DATA PREVISTA CONSEGNA E' <= ALLA RICHIESTA
001800920227     F*
001900920227     F*    NON ABILITATE:
002000920227     F*     . L'ABILITAZIONE ARRIVI E' ZERO
002100920227     F*     . IL C/A E L'ASS.TO (NON E' TESTATO)
002200920227     F*     . IN DISTINTA (NON E' POSSIBILE)
002300920227     F*     . LA DATA PREVISTA CONSEGNA (NON E TESTATA)
002400920227     F*-                                                             *
002500920227     F*    ABILITATE E NON ABILITATE
002600920227     F*     . ABILITAZIONE C/A E L'ASS.TO  (NON TESTATO MA EVIDENZIATO)
002700920227     F*     . NON E' IN DISTINTA
002800920227     F*     . LA DATA PREVISTA CONSEGNA (NON E' TESTATA)
002900920227     F*
003000950228     F*--------------------------------------------------------------*
003100950228     F*  IL PROGRAMMA VIENE LANCIATO DA:
003200950228     F*    . STAMPA PRELISTA DI CONSEGNA (FILTRO) (FNLR51R)
003300950228     F*    . ABBINAMENTO FOGLI VIAGGIO (FNLR25R)
003400950228     F*    . ELENCO SPEDIZIONI NON ABILITATE (FNLR64R)
003500950228     F*--------------------------------------------------------------*
003600990324     FTNTBE02L  IF   E           K DISK
003700990324     FAZORG01L  IF   E           K DISK
003800941215     FFNARB01L  IF   E           K DISK
003900941215     F                                     RENAME(FNARB000:FNARB001)
004000960517     FFNARB08L  IF   E           K DISK
004100941215     FFNARB05L  IF   E           K DISK
004200941215     F                                     RENAME(FNARB000:FNARB005)
004300941215     FFNARB06L  IF   E           K DISK
004400941215     F                                     RENAME(FNARB000:FNARB006)
004500060216     FFiAR401L  IF   E           K DISK
004600991104     FFIAR601L  IF   E           K DISK
004700051114     FFiAR901L  IF   E           K DISK
004800090309     FFiART01L  IF   E           K DISK
004900991105     FFIARI01L  IF   E           K DISK
005000941214     FFNFVA01L  IF   E           K DISK
005100950621     FFNFVV02L  IF   E           K DISK
005200941214     FFNLBL01L  IF   E           K DISK
005300911106     FTABEL00F  IF   E           K DISK
005400040809     F**""*FNDCT02L  IF   E           K DISK
005500990324     FFNDCT01L  IF   E           K DISK
005600990324     F                                     RENAME(FNDCT000:FNDCT001)
005700990304     FFNDCD02L  IF   E           K DISK
005800070118     FFNBRV03L  IF   E           K DISK
005900070118     FFNBRV01L  IF   E           K DISK
006000070118     F                                     RENAME(FNBRV000:FNBRV001)
006100030925     fWFCBB01L  uf a e           k disk    usropn
006200960516     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
006300960516     F                                     USROPN
006400960516     F                                     FORMLEN(066)
006500960516     F                                     FORMOFL(062)
006600911209     F*----------------------------------------------------------------
006700911209     F*--
006800911209     F*- INDICATORI DI UTILIZZO GESTIONALE
006900950621     F*--
007000990127     F* 05 stampo c.a. sulla spedizione
007100990324     F* 06 C'E' DARA CHIUSURA C.A.
007200990127     F* 10 INDICARE DI ALLOCAZIONE RECORD SU FNFVV
007300960119     F* 11 DI COMODO
007400960517     F* 16 ESISTONO LE CONSEGNE PARTICOLARI DA STAMPARE
007500960516     F* 17 PRESENTE DATA DOGLIO ARRIVI
007600960516     F* 18 PRESENTE DATA CONSEGNA RICHIESTA
007700960516     F* 19 INDICA RICHIESTA DELLE SPEDIZIONI PRESENTI IN IMA
007800950621     F* 20 INDICA RICHIESTA DELLE SPEDIZIONI SOLO ABILITATE
007900911209     F* 30 INDICA RICHIESTA DELLE SPEDIZIONI NON ABILITATE
008000911209     F* 40 INDICA RICHIESTA DI TUTTE LE SPEDIZIONI
008100950315RM*  F* 45 INDICA RICHIESTA DI SPEDIZIONI SOLO INCASSO
008200950315RM*  F* 46 INDICA RICHIESTA DI SPEDIZIONI SOLO NON INCASSO
008300960321RM*  F* 77 ELENCO SPEDIZIONI NON PARTITE SU UN FV
008400911209     F*----------------------------------------------------------------
008500960321     D L1              S              3  0 DIM(30)                              FIL GESTITE
008600960321     D*
008700020522     D LIN             S              3  0 DIM(29)                              LINEE ARRIVI
008800960516     D STT             S             66    DIM(15) CTDATA PERRCD(1)
008900941215     D NSC             S              7  0 DIM(13)                              SEGNACOLLI
009000920527     D SP1             S              7  0 DIM(12)                              NO ABIL. PARTENZA
009100920527     D SP2             S              7  0 DIM(12)                              NO ABIL. PARTENZA
009200920527     D SA1             S              7  0 DIM(12)                              NO ABIL. ARRIVO
009300920527     D SA2             S              7  0 DIM(12)                              NO ABIL. ARRIVO
009400920907     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
009500920907     D FIL             S              3    DIM(29)                              FIL. COMODO
009600990324     D CCH             S              2    DIM(50)                              CAUS.CHIUS.C.A
009700921130     D*
009800941215     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
009900950808     D MSG             S             66    DIM(2) CTDATA PERRCD(1)              MESSAGI IN STAMPA
010000950321     D*
010100950322     D NRS             S              7  0 DIM(999)                             N.SEGNACOLLO
010200950322     D FGS             S              3  0 DIM(999)                             FILIALE GEST.
010300000203     D NFV             S              6  0 DIM(999)                             FOGLIO VIAGGIO
010400950322     D DFV             S              8  0 DIM(999)                             DATA F.V.
010500911024     D KPJBA         E DS
010600911028     D PARAM           DS
010700911031     D  LIP                    1     87
010800911031     D                                     DIM(29)                              LINEE ARRIVI
010900911104     D  ZNC                   88    107
011000911104     D                                     DIM(10)                              ZONE VIDEO BUO
011100941216     D  PARDAT               108    115  0
011200941216     D  PARABI               116    117
011300950621     D  PARLNP               118    120  0
011400941216     D  PARNFV               121    125
011500941216     D  PARGES               126    128  0
011600950215     D  PAROUT               129    129
011700950621     D  PARINC               130    131
011800950621     D  PARIMA               132    133
011900960320     D* DATA F. ARRIVI DI ABBINAMENTO IN AAAAMMGG
012000960320     D  PARDFA               134    141  0
012100970516     D* NUMERO FIL GESTIONE FOGLIO IMA
012200970516     D  PARFIA               142    146  0
012300970516     D  PARFGS               147    149  0
012400000626     D* SI/NO/SOLO POSTE   S/N/P
012500000626     D  PARSNP               150    150
012600000809     D* TUTTE, SOLO SI, SOLO NO ASSEGNATO
012700000803     D  PARASS               151    152
012800000803     D* TUTTE, SOLO SI, SOLO NO RESI
012900000803     D  PARRES               153    154
013000021205     D* Tutte, solo SI, solo NO - Bolle Bloccate
013100021205     D  PARblo               155    156
013200030826     d* Funzione richiesta: *blank = stampa, "WRKF " = creaz. file di lavoro
013300030826     d  PARfr                252    256
013400911024      *
013500911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013600950119     D DSCV          E DS
013700950119     D DS3A          E DS
013800920311     D DS1P          E DS
013900920527     D DS3I          E DS
014000960321     D DS5A          E DS
014100970707     D DS5P          E DS
014200991104     D DGED          E DS
014300971224     D DSLV53        E DS                  EXTNAME(FNLV53DS)
014400920318     D CNCR80        E DS
014500000323     D DSBL4A        E DS
014600000626     D OG143         E DS
014700040826     d dvpoppt       e ds
014800990304     D DCCH            DS
014900990304     D  §CHDES                 1     50
015000990304     D  §CHR52                54     54
015100990324     D DTAD            DS
015200990324     D  §TARGR                26     26
015300990324     D  §TARIT                82     82
015400960321     D* DS PER TRUL06R - CARICAMENTO £X
015500960321     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015600960321     D  LNE                    1     90  0
015700960321     D                                     DIM(30)                              FIL. COMODO
015800990304     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015900051214     D fnlv80ds      E DS
016000991104     D YEUR          E DS                  EXTNAME(YEURCODS)
016100990127     D                 DS
016200990127     D  DCTDCA                 1      8  0
016300990127     D  DCTAAC                 1      4  0
016400990127     D  DCTMGC                 5      8  0
016500941216     D* DATA SPEDIZIONE BOLLA
016600960321     D                 DS
016700941216     D  ARBDSP                 1      8  0
016800941216     D  ARBAAS                 1      4  0
016900941216     D  ARBMGS                 5      8  0
017000941216     D WLBDAT          DS
017100941216     D  G02DAT                 1      8  0
017200941216     D  G02INV                 9     16  0
017300941216     D  G02ERR                17     17
017400941216     D  G02TGI                18     22  0
017500960321     D WGIDAT          DS
017600960321     D  GIODAT                 1      8  0
017700960321     D  GIOINV                 9     16  0
017800960321     D  GIOTGI                17     21  0
017900040809
018000040809      * ds per il controllo della presenza di CA per la spedizione richiesta
018100040809     d FIDN12DS      e ds
018200040809     d  skKey                 26    305    dim(20)
018300040809     d  skAnn                306    325    dim(20)
018400040809     d  skDca                326    485  0 dim(20)
018500040809     d  skFca                486    545  0 dim(20)
018600040809     d  skDch                546    705  0 dim(20)
018700040809     d  skCch                706    745    dim(20)
018800040809     d*
018900040809     d dsKey           ds
019000040809     d  dsaac                         4  0
019100040809     d  dsfil                         3  0
019200040809     d  dsnca                         7  0
019300040809
019400030826      *
019500030826      * Definizione variabili
019600030826      *
019700030826     d WCAS            s                   like(AR9CAS) inz
019800030826     d WIFT            s                   like(AR6IFT) inz
019900030826     d WIPP            s                   like(ARIIPP) inz
020000030826     d WCONVE          s                   like(ARIIPP) inz
020100030826     d KTIP            s                   like(ARITIP) inz
020200030826     d STPDIV          s                   like(AR6DIV) inz
020300030826     d WLNP            s                   like(FVVFGS) inz
020400030826     d KTRC            s                   like(AR4TRC) inz
020500030826     d STPTIC          s                   like(AR9TIC) inz
020600030826     d STPCAS          s                   like(AR9CAS) inz
020700030826     d STPVCA          s                   like(AR9VCA) inz
020800030826     d STPCMB          s                   like(AR9CMB) inz
020900030826     d WNPG            s                   like(BRVNPG) inz
021000030826     d IMANPG          s                   like(BRVNPG) inz
021100030826     d WNFA            s                   like(FVANFA) inz
021200030826     d WDFA            s                   like(FVADFA) inz
021300030826     d STPHCR          s                   like(ARBHCR) inz
021400030826     d STPDFA          s                   like(PARDFA) inz
021500030826     d STPDAT          s                   like(PARDAT) inz
021600030826     d KCOD            s                   like(TBECOD) inz
021700030826     d KLIN            s                   like(TBELIN) inz
021800030826     d KSIF            s                   like(TBESIF) inz
021900030826     d KKE1            s                   like(TBEKE1) inz
022000030826     d KKE2            s                   like(TBEKE2) inz
022100030826     d WNFV            s                   like(FVANFV) inz
022200030826     d WNFVBR          s                   like(BRVNFV) inz
022300030826     d wErr            s              3  0 inz
022400960516     D*
022500960516     D* DEFINIZIONE COSTANTI
022600960516     D CINC            C                   CONST('-SPEDIZIONI SOLO INC-
022700960516     D                                     ASSO-')
022800960516     D CNINC           C                   CONST('-SPEDIZIONI SOLO NON-
022900960516     D                                      INCASSO-')
023000960516     D CIMA            C                   CONST('-SPEDIZIONI PRESENTI-
023100960516     D                                      IN IMA-')
023200000803     D ALT1            C                   CONST('SOLO NON ASSEGNATI')
023300000803     D ALT2            C                   CONST('SOLO ASSEGNATI    ')
023400000803     D ALT3            C                   CONST('SOLO NON RESI')
023500000803     D ALT4            C                   CONST('SOLO RESI')
023600021205     D Alt5            c                   const('SOLO BOLLE NON BLOCCATE')
023700021205     D Alt6            c                   const('SOLO BOLLE BLOCCATE')
023800950626     C*---------------------------------------------------------------
023900941215     C*
024000960321     C* Lettura dei files FNARB00 per linea e zona inserite
024100960321     C* nelle tabelle già impostate
024200960321     C* elaborando soltanto le bolle di FV abbinati.
024300940620     C*
024400911209     C* CONTROLLO SE VIENE RICHIESTA LA STAMPA DELLE SPEDIZIONI ABILITA
024500911209     C* TE O NON OPPURE TUTTE
024600911209     C*
024700940620     C* 20 ON - SOLO BOLLE ABILITATE
024800950626    0C     *IN77         IFEQ      *OFF
024900950626     C*
025000950626    1C     PARABI        IFEQ      'SI'
025100911209     C                   SETON                                        20
025200911209     C*
025300911209     C                   ELSE
025400940620     C* 30 ON - SOLO NON ABILITATE
025500941216    2C     PARABI        IFEQ      'NO'
025600911209     C                   SETON                                        30
025700911209     C                   ELSE
025800940620     C* 40 ON - TUTTE
025900911209     C                   SETON                                        40
026000940620    2C                   ENDIF
026100940620    1C                   ENDIF
026200950315RM*  C*
026300950315RM*  C     PARINC        COMP      'SI'                                   45
026400950315RM*  C  N45PARINC        COMP      'NO'                                   46
026500000626     C* SI/NO/SOLO POSTE (PARAMETRO IN STAMPA)
026600000626     C     PARSNP        IFEQ      'N'
026700000626     C                   MOVE      'NO POSTE'    POSTE$           10
026800000626     C                   ENDIF
026900000626     C     PARSNP        IFEQ      'S'
027000000626     C                   MOVE      'SI POSTE'    POSTE$
027100000626     C                   ENDIF
027200000626     C     PARSNP        IFEQ      'P'
027300000626     C     'SOLO'        CAT       'POSTE':1     POSTE$
027400000626     C                   ENDIF
027500000803     C* ASSEGNATI/RESI
027600000803     C     PARASS        IFEQ      'SI'
027700000803     C                   MOVEL     ALT2          ALTRP1           18
027800000803     C                   ENDIF
027900000803     C     PARASS        IFEQ      'NO'
028000000803     C                   MOVEL     ALT1          ALTRP1
028100000803     C                   ENDIF
028200000803     C     PARRES        IFEQ      'SI'
028300000803     C                   MOVEL     ALT4          ALTRP2           13
028400000803     C                   ENDIF
028500000803     C     PARRES        IFEQ      'NO'
028600000803     C                   MOVEL     ALT3          ALTRP2
028700000803     C                   ENDIF
028800021205      * Bloccati
028900021205     C                   select
029000021205     C                   when      PARblo = 'NO'
029100021205     C                   movel     Alt5          AltRp3           23
029200021205     C                   when      PARblo = 'SI'
029300021205     C                   movel     Alt6          AltRp3
029400021205     C                   endsl
029500000803     C*
029600950621     C* SOLO SPEDIZIONE MANCANTI IN IMA
029700960321    1C     PARIMA        IFEQ      'SI'
029800971224     C* SE E' UN PO ESTERO FORZO IL SUO TERMINAL DI PARTENZA
029900971229     C     PARLNP        IFNE      300
030000971224     C     PARLNP        CHAIN     AZORG                              11
030100020220     c                   eval      og143 = orgde3
030200971229     C                   END
030300971229     C     PARLNP        IFEQ      300
030400020220     c     §ogntw        oreq      'EEX'
030500020220     c     §ogntw        oreq      'EUP'
030600020220     c     §ogntw        oreq      'FED'
030700020220     c     §ogntw        oreq      'DPD'
030800971224     C                   Z-ADD     SIMFEL        WLNP
030900970513     C                   ELSE
031000970513     C                   Z-ADD     PARLNP        WLNP
031100970513     C                   ENDIF
031200970516     C**
031300970516     C* SE NUMERO FOGLIO IMA PASSATO PRENDO QUELLO ALTRIMENTI LO
031400970516     C*  DETERMINO
031500970516    2C     PARFIA        IFEQ      0
031600970516     C**
031700950621     C* CERCO IL FOGLIO IMA DI OGGI
031800950622     C     KFVV          CHAIN     FNFVV000                           10
031900960517     C* SE NON TROVATO FOGLIO SONO TUTTE PRESENTI IN IMA PER
032000950621     C*  CUI NON ACCENDO NEMMENO L'INDICATORE
032100970516    3C     *IN10         DOWEQ     *OFF
032200970516    4C     FVVSPG        IFEQ      'A'
032300950622     C     FVVATB        ANDEQ     ' '
032400950622     C                   SETON                                        1910
032500970516   x4C                   ELSE
032600950622     C     KFVV          READE     FNFVV000                               10
032700970516    4C                   ENDIF
032800970516    3C                   ENDDO
032900970516     C*
033000970516   X2C                   ELSE
033100970516     C                   SETON                                        19
033200970516     C                   MOVEL     PARFIA        FVVNFV
033300970516     C                   MOVEL     PARFGS        FVVFGS
033400970516     C                   MOVEL     IMANPG        FVVNPG
033500970516    2C                   ENDIF
033600970516     C*
033700960321    1C                   ENDIF
033800940620     C*
033900911024     C                   Z-ADD     1             I                 2 0
034000911024     C                   Z-ADD     1             F                 2 0
034100950626    1C     I             DOWLE     29                                           *
034200911024     C*
034300911024     C* SE LA LINEA E' A BLANKS FINISCO IL CARICAMENTO
034400911024     C*
034500940620    2C     LIN(I)        IFNE      *ZEROS
034600911024     C*
034700940620    3C     F             DOWLE     10
034800911024     C*
034900911024     C                   SETOFF                                           53
035000940620    4C     ZNC(F)        IFEQ      *BLANKS
035100911024     C     F             ANDNE     1
035200911024     C                   SETON                                            53
035300911024     C                   Z-ADD     10            F
035400940620    4C                   END
035500960321     C*
035600911104     C                   MOVE      ZNC(F)        VIDZNC
035700960517     C  N53
035800960517     CAN 20KARB          SETLL     FNARB08L
035900941215     C  N53
036000941215     CANN20KARB          SETLL     FNARB05L
036100940620     C*
036200911024     C* LEGGO IL FILE IN BASE AL FATTO CHE LA ZONA SIA A ZERO OPPURE NO
036300940620    4C  N53              DO        *HIVAL
036400940620    5C     ZNC(F)        IFEQ      *BLANKS
036500960517     C   20LIN(I)        READE     FNARB08L                               53
036600941215     C  N20LIN(I)        READE     FNARB05L                               53
036700940620   X5C                   ELSE
036800960517     C   20KARB          READE     FNARB08L                               53
036900941215     C  N20KARB          READE     FNARB05L                               53
037000940620    5C                   END
037100940620     C*
037200911024     C* CONTROLLO LA VALIDITA' DEL RECORD
037300940620    5C  N53              DO
037400960321     C                   SETOFF                                           70
037500960321     C*
037600960321     C* CONTROLLO SE E' ABBINATO E A QUALE FOGLIO ARRIVI
037700960321     C                   EXSR      ABFVA
037800960321     C*
037900960516     C* SE IL FOGLIO RELATIVO NON E' ABBINATO O LA DATA FOGLIO ARRIVI
038000960520     C* E' > DELLA RICHIESTA : NON ELABORO
038100960321    6C     WNFA          IFGT      0
038200960321     C     WDFA          ANDLE     PARDFA
038300960321     C**
038400960321     C**
038500010330     C* SE BOLLA NON CONSEGNATA LA DEVO STAMPARE LO STESSO ANCHE SE
038600010330     C* RICHIESTI IN NON INC. SE SI TRATTA DI BOLLA DI RECUPERO NON
038700010330     C* LEGATA
038800010330     C                   CLEAR                   WINCPT
038900010330     C     ARBDCM        IFEQ      *ZEROS
039000010320     C     ARBNDC        ANDEQ     *ZEROS
039100010320     C                   MOVEL     '3A'          TBLCOD
039200010320     C                   MOVEL     *BLANKS       TBLKEY
039300010320     C                   MOVEL     ARBCBO        TBLKEY
039400010320     C*
039500010320     C     KTAB          CHAIN     TABEL00F                           13
039600010320     C  N13              MOVEL     TBLUNI        DS3A
039700010320     C   13              CLEAR                   DS3A
039800010320    8C     §3ARBL        IFEQ      'R'
039900010320     C     KARB1         SETLL     FNLBL01L                               13
040000010320     C* Se non trovati legami memorizzo che è da elaborare
040100010320     C  N13              MOVEL     'S'           WINCPT            1
040200010320    8C                   ENDIF
040300010320    7C                   ENDIF
040400960321     C* DATA CONSEGNA = 0   DA CONSEGNARE
040500960321IF  7C     ARBDCM        IFEQ      *ZERO                                                    E
040600960520     C* ESCLUDO SE  PARINC = 'SI' => Richiesti solo incasso
040700960520     C     PARINC        ANDNE     'SI'
040800960520     C* ESCLUDO LE BOLLE IN DISTINTA
040900960520     C     ARBNDC        ANDEQ     0
041000010320     C* OPPURE SE SOLO INCASSO DI BOLLA DI RECUPERO NON LEGATA
041100010320IF  7C     ARBDCM        OREQ      *ZERO                                                    E
041200010320     C     PARINC        ANDEQ     'SI'
041300010320     C     WINCPT        ANDEQ     'S'
041400010320     C     ARBNDC        ANDEQ     0
041500920227     C*
041600990304     C* ESCLUDO LE SPEDIZIONI CON C.A. APERTA
041700990128     C                   CLEAR                   WCA
041800040809     C**""*KARB1         CHAIN     FNDCT02L                           89
041900040809    8C**""**IN89         DOWEQ     *OFF
042000990128     C* C.A. APERTA E NON ANNULLATA --> C.A. OK
042100040809    9C**""*DCTATB        IFEQ      ' '
042200040809     C**""*DCTDCH        ANDEQ     0
042300040809     C**""*              MOVEL     'S'           WCA               1
042400040809     C**""*              SETON                                        89
042500040809   X9C**""*              ELSE
042600990304     C*
042700990324     C* MEMORIZZO SE E' PRESENTE UNA C.A. CHIUSA CON CAUSALE DA NON
042800990324     C*  STAMPARE
042900040809     C**""*DCTATB        IFEQ      ' '
043000040809     C**""*              MOVEL     'C'           WCA               1
043100040809     C**""*              ENDIF
043200990324     C*
043300040809     C**""*KARB1         READE     FNDCT02L                               89
043400040809    9C**""*              ENDIF
043500040809    8C**""*              ENDDO
043600040809      *
043700040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
043800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
043900040809      * legate alla spedizione che passo
044000040809     c                   clear                   fidn12ds
044100040809     c                   eval      i12aas = ARBAAS
044200040809     c                   eval      i12lnp = ARBLNP
044300040809     c                   eval      i12nrs = ARBNRS
044400040809     c                   eval      i12nsp = ARBNSP
044500040809     c                   eval      i12fel = 'E'
044600040809     c                   eval      i12fan = 'E'
044700040809     c*
044800040809     c                   call      'FIDN12R'
044900040809     c                   parm                    fidn12ds
045000040809     c*
045100040809     c                   setoff                                       89
045200040809     c*
045300040809     c* se non ci sono errori
045400040809     c                   if        o12err <> ' '
045500040809     c                   seton                                        89
045600040809     c                   else
045700040809     c* se numero di CA trovate maggiore di zero
045800040809     c                   if        o12nca = 0
045900040809     c                   seton                                        89
046000040809     c                   else
046100040809     c                   z-add     1             jj                2 0
046200040809     c                   dow       jj <= o12nca and *in89 = *off
046300040809     C* C.A. APERTA E NON ANNULLATA --> C.A. OK
046400040809     C     skdch(jj)     IFEQ      0
046500040809     C                   MOVEL     'S'           WCA               1
046600040809     C                   SETON                                        89
046700040809     C                   ELSE
046800040809     C*
046900040809     C* MEMORIZZO SE E' PRESENTE UNA C.A. CHIUSA CON CAUSALE DA NON STAMPARE
047000040809     C                   MOVEL     'C'           WCA               1
047100040809     C                   ENDIF
047200040809     c*
047300040809     c                   add       1             jj
047400040809     c                   enddo
047500040809     c*
047600040809     c                   seton                                        89
047700040809     c*
047800040809     c                   endif
047900040809     c                   endif
048000990304     C**
048100990304     C** SE C.A. APERTA ESCLUDO SEMPRE
048200990304   7BC     WCA           IFNE      'S'
048300990324     C*
048400990304     C** SE C'E' C.A. CHIUSA, CONTROLLO DALLA CAUSALE CHIUSURA E DALLO
048500990304     C**  STATO DEGLI ALTRI COLLI SE DEVO STAMPARE O MENO
048600990304    8C     WCA           IFEQ      'C'
048700990304     C                   EXSR      CACHIU
048800990304     C                   ENDIF
048900990304     C*
049000990304     C* ELABORO SE :
049100990304     C* 1) NON C'E' C.A.
049200990304   7CC     WCA           IFEQ      ' '
049300990304     C* 3) C'E' C.A. CHIUSA MA UNO DEI COLLI NON IN C.A. NON E' CONS.
049400990304     C     WCA           OREQ      'C'
049500990304     C     WNOCON        ANDEQ     'S'
049600960516     C*
049700000626     C* ESCLUDO BOLLE IN BASE A PARAMETRO SI/NO/SOLO POSTE
049800000626     C* .... SOLAMENTE SE NON SONO RITORNI ALL'INCASSO
049900000626     C*
050000000626     C     ARBDCM        IFEQ      0
050100000626     C     ARBLNP        CHAIN     AZORG01L                           83
050200000626     C     *IN83         IFEQ      *OFF
050300000626     C                   MOVEL     ORGDE3        OG143
050400000626      *
050500040826     c* se pgm richiamato da quadratura (PARfr='WRKF ') assumo che parsnp='S
050600040826     c* ') ed in questo caso le bolle poste le includo solo se data spedizio
050700040826     c* >= data in tabella VPO rk 'PPT'
050800040826     c     §ogntw        ifeq      'PPT'
050900040826     c     parfr         andeq     'WRKF '
051000040826     c     arbdsp        andlt     §vpoptl
051100040826     c                   seton                                        70
051200040826     c                   endif
051300040826     c*
051400000626     C     PARSNP        IFEQ      'N'
051500040826     c     parfr         andne     'WRKF '
051600020220     c     §ogntw        andeq     'PPT'
051700000626     C                   SETON                                        70
051800000626     C                   ENDIF
051900000626      *
052000000626     C     PARSNP        IFEQ      'P'
052100040826     c     parfr         andne     'WRKF '
052200020220     c     §ogntw        andne     'PPT'
052300000626     C                   SETON                                        70
052400000626     C                   ENDIF
052500000626      *
052600000626     C                   ENDIF
052700000626     C                   ENDIF
052800000803     C*
052900000803     C* ESCLUDO BOLLE IN BASE A PARAMETRO "ASSEGNATO"
053000000803     C*
053100000803     C                   MOVEL     '3A'          TBLCOD
053200000803     C                   MOVEL     *BLANKS       TBLKEY
053300000803     C                   MOVEL     ARBCBO        TBLKEY
053400000803     C                   CLEAR                   COM01
053500000803     C*
053600000803     C     KTAB          CHAIN     TABEL00F                           13
053700000803     C  N13              MOVEL     TBLUNI        DS3A
053800000803     C  N13              MOVEL     §3ATB1        COM01             1
053900000803     C*
054000000803     C     PARASS        IFEQ      'SI'
054100000803     C     COM01         ANDNE     'A'
054200000803     C                   SETON                                        70
054300000803     C                   ENDIF
054400000803     C*
054500000803     C     PARASS        IFEQ      'NO'
054600000803     C     COM01         ANDEQ     'A'
054700000803     C                   SETON                                        70
054800000803     C                   ENDIF
054900000803     C*
055000000803     C* ESCLUDO BOLLE IN BASE A PARAMETRO "RESI"
055100000626     C*
055200000803     C     PARRES        IFEQ      'SI'
055300000803     C     ARBFBR        ANDNE     'S'
055400000803     C                   SETON                                        70
055500000803     C                   ENDIF
055600000803     C*
055700000803     C     PARRES        IFEQ      'NO'
055800000803     C     ARBFBR        ANDEQ     'S'
055900000803     C                   SETON                                        70
056000000803     C                   ENDIF
056100021205      *
056200021205      * Escludo Bolle in base a parametro "BOLLE BLOCCATE"
056300021205      *
056400021205     c                   if            (PARblo =  'SI'
056500021205     c                             and  ARBfbc =  *blanks)
056600021205     c                             or  (PARblo =  'NO'
056700021205     c                             and  ARBfbc <> *blanks)
056800021205     C                   eval      *in70 = *on
056900021205     C                   endif
057000000803     C*
057100000626     C* S O L O    A B I L I T A T E
057200000626     C*
057300960321    8C     *IN20         IFEQ      '1'
057400911115     C*
057500940620     C* SE LA DATA CONSEGNA RICHIESTA E' MINORE DELLA DATA
057600940620     C* DISTINTA IMPOSTATA A VIDEO NON STAMPO (70 ON)
057700960321    9C     ARBTCR        IFEQ      ' '
057800960321     C     ARBDCR        ANDGT     PARDAT
057900911115     C                   SETON                                            70
058000960321    9C                   END
058100940620     C*
058200940620     C* SE CONSEGNA RICHIESTA HA COME TIPO "D-DOPO" ESCLUDO ANCHE
058300941216     C*  ARBDCR = PARDAT (70 ON)
058400960321    9C     ARBTCR        IFEQ      'D'
058500941216     C     ARBDCR        ANDGE     PARDAT
058600911115     C                   SETON                                            70
058700960321    9C                   END
058800911209     C*
058900960321    8C                   END
059000000626     C*
059100920227     C*
059200940620     C* S O L O    N O N    A B I L I T A T E
059300920227     C*
059400960321    8C     *IN30         IFEQ      '1'
059500911209     C*
059600960417     C* ARBAMA =0  NON ABILITATO ASSEGNATO O NON ABILITATO C/ASS-->
059700960417     C*            ELABORO
059800960516    9C     ARBAMA        IFGT      *ZEROS
059900960417     C     ARBACC        ANDEQ     'S'
060000960417     C     ARBACA        ANDEQ     'S'
060100960417     C                   SETON                                        70
060200960417    9C                   END
060300911209     C*
060400940620     C* CONTROLLO DETTAGLIO COLLI
060500960321     C  N70              EXSR      DETCOL
060600920527     C*
060700960321    8C                   ENDIF
060800920227     C*
060900940620     C* A B I L I T A T E  E   N O N   A B I L I T A T E
061000920227     C*
061100960321    8C     *IN40         IFEQ      '1'
061200911209     C*
061300920227     C* EVIDENZIO SE ABILITATA O MENO
061400911209     C                   SETOFF                                       31
061500960516    9C     ARBAMA        IFNE      *ZEROS
061600911209     C     ARBACC        ANDEQ     'S'
061700911209     C     ARBACA        ANDEQ     'S'
061800911209     C     ARBFBC        ANDEQ     ' '
061900911209     C                   SETON                                        31
062000960321    9C                   END
062100940620     C*
062200940620     C* CONTROLLO DETTAGLIO COLLI
062300940620     C  N70              EXSR      DETCOL
062400960321    8C                   ENDIF
062500950621     C*
062600960517     C* SE RICHIESTE LE BOLLE PRESENTI IN IMA CONTROLLO CHE ABBIANO
062700960517     C*   ALMENO UN COLLO IN IMA
062800960517    8C     *IN19         IFEQ      *ON
062900960517     C     *IN70         ANDEQ     *OFF
063000960517     C                   EXSR      CTRIMA
063100911115     C*
063200960516     C* SE RICHIESTE LE BOLLE PRESENTI IN IMA ESCLUDO I RECUPERI
063300960517     C     *IN70         IFEQ      *OFF
063400960516     C                   MOVEL     '3A'          TBLCOD
063500960516     C                   MOVEL(P)  ARBCBO        TBLKEY
063600960516     C     KTAB          CHAIN     TABEL00F                           33
063700960516     C  N33              MOVEL     TBLUNI        DS3A
063800960516     C   33              CLEAR                   DS3A
063900960516     C     §3ARBL        IFEQ      'R'
064000960516     C                   SETON                                        70
064100960516     C                   ENDIF
064200960517     C                   ENDIF
064300960517    8C                   ENDIF
064400960516     C*
064500030826     ***C  N70              EXSR      STAMPA
064600030826sel 8c                   select
064700030826w   8c                   when      *in70
064800030826w   8c                   when      PARfr = 'WRKF '
064900030826     c                   clear                   wErr
065000030826     c                   exsr      WriteWFCBB
065100030826x   8c                   other
065200030826     c                   exsr      Stampa
065300030826e   8c                   endsl
065400990304   7CC                   ENDIF
065500990304   7BC                   ENDIF
065600941215     C*
065700960321   X7C                   ELSE
065800960321     C* DATA CONSEGNA > 0  CONSEGNATA
065900960520    8C     ARBDCM        IFNE      *ZEROS
066000960520     C* ESCLUDO SE  PARINC = 'NO' => Richiesto solo non incasso
066100960520     C     PARINC        ANDNE     'NO'
066200960520     C* ESCLUDO SE SOLO BOLLE PRESENTI IN IMA
066300960520     C     PARIMA        ANDNE     'SI'
066400941215     C                   EXSR      STAMP1
066500941215     C*
066600960520    8C                   ENDIF
066700960321    7C                   ENDIF
066800911024     C*
066900960321    6C                   ENDIF
067000960321    5C                   ENDDO
067100960321    4C  N53              ENDDO
067200911024     C*
067300911024     C                   ADD       1             F
067400960321    3C                   ENDDO
067500911024     C                   Z-ADD     1             F
067600911024     C*
067700940620   X2C                   ELSE
067800911024     C                   Z-ADD     29            I
067900960321    2C                   ENDIF
068000911024     C*
068100911024     C                   ADD       1             I
068200911024     C*
068300950626    1C                   ENDDO
068400950626    0C                   ENDIF
068500911024     C*
068600940620     C**
068700940620     C* S T A M P A   E L E N C O   B O L L E   N O N   P A R T I T E
068800940620     C**
068900940620    1C     *IN77         IFEQ      *ON
069000940620     C*
069100941215     C     KARB6         SETLL     FNARB06L                           78
069200940620     C*
069300940620    2C     *IN78         DOWEQ     '0'
069400941215     C     KARB6         READE     FNARB06L                               78
069500940620     C*
069600940620    3C  N78ARBAMP        IFEQ      0
069700920908     C     ARBLNA        LOOKUP    L6                                     91
069800940620     C*
069900940620    4C     *IN91         IFEQ      *ON
070000000322     C* IGNORO SE SI TRATTA DI BOLLA CON FLAG DDT='N' PERCHE' SIGNIFICA
070100000322     C* CHE NON E' ANCORA STATA STAMPATA E QUINDI NON HO BISOGNO DI
070200000322     C* TOGLIERLA DAL GIRO
070300000322     C                   MOVEL(P)  'A'           KTRC
070400060216     C     KAR4          CHAIN     FiAR401L                           11
070500000322     C  N11              MOVEL     AR4NOT        DSBL4A
070600000322   4AC     *IN11         IFEQ      *ON
070700000322     C     §B4ABM        ORNE      'N'
070800940620     C* CONTROLLO IL DETTAGLIO COLLI
070900940620     C                   EXSR      DETCOL
071000960119     C* SE SPEDIZIONE CON TUTTI I COLLI PARTITI MA NON ABILITATA
071100970117     C* --> NON STAMPO
071200960119     C                   SETOFF                                       14
071300960119    5C     SP1(1)        IFEQ      *ZERO
071400970811     C     NRS(1)        ANDEQ     *ZEROS
071500960119     C                   SETON                                        14
071600970811    5C                   ENDIF
071700970807     C*
071800970807     C* SE SPEDIZIONE IMPORT CON COLLI PARZIALMENTE A TERRA E NON HA
071900970807     C* IL C/A E NON PREVEDE FATTURA IMMEDIATA IN ASSEGNATO
072000970807     C* --> NON STAMPO
072100971224   5AC     *IN14         IFEQ      *OFF
072200971224     C     *IN34         ANDEQ     *OFF
072300971224     C     ARBLNP        CHAIN     AZORG                              11
072400020220     c                   eval      og143 = orgde3
072500020220     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP'or
072600020220     c                             §ogntw = 'FED' or §ogntw = 'DPD'
072700970807     C* DECODIFICO IL TIPO BOLLA
072800970807     C                   MOVEL     '3A'          TBLCOD
072900970807     C                   MOVEL     *BLANKS       TBLKEY
073000970807     C                   MOVEL     ARBCBO        TBLKEY
073100970807     C     KTAB          CHAIN     TABEL00F                           13
073200970807     C   13              CLEAR                   DS3A
073300970807     C  N13              MOVEL     TBLUNI        DS3A
073400970807     C*
073500970811    6C     §3AFCA        IFEQ      0
073600970807     C                   MOVE      ARBKSC        W0040             4 0
073700970807     C* SE ASSEGNATO VERIFICO SE C'E' LA FATTURA
073800970811     C                   SETON                                        13
073900970811    7C     W0040         IFEQ      9999
074000991104     C                   MOVEL     1             KTRC
074100991104     C     KARB1         CHAIN     FIAR601L                           13
074200970811    7C                   END
074300970811    7C     W0040         IFNE      9999
074400970811     C     *IN13         OREQ      *ON
074500970811     C     AR6DFT        OREQ      0
074600970807     C                   SETON                                        14
074700970811    7C                   END
074800970811    6C                   END
074900970811    5C                   END
075000971224   5AC                   END
075100960119     C*
075200940620     C* STAMPO LA SPEDIZIONE NON PARTITA
075300030826     ***C  N14              EXSR      STAMPA
075400030826sel 8c                   select
075500030826w   8c                   when      *in14
075600030826w   8c                   when      PARfr = 'WRKF '
075700030826     c                   clear                   wErr
075800030826     c                   exsr      WriteWFCBB
075900030826x   8c                   other
076000030826     c                   exsr      Stampa
076100030826e   8c                   endsl
076200941219     C* SE LA BOLLA NON E' ABILITATA, MA NON C'E' NESSUN COLLO NON
076300941219     C* ABILITATO ALLA PARTENZA, EMETTO MESSAGGIO
076400960119    5C  N14SP1(1)        IFEQ      *ZERO
076500950322     C     ARBAMA        ANDEQ     0
076600030826if  6c                   if        PARfr = 'WRKF '
076700030826     c                   eval      wErr  = 1
076800030826     c                   exsr      WriteWFCBB
076900030826x   6c                   else
077000950808     C                   MOVEL     MSG(1)        STPMSG           66
077100941219     C                   EXCEPT    MESSAG                                       EMETTI MESSAGIO
077200030826e   6c                   endif
077300950808     C*
077400950808    6C     NRS(1)        IFEQ      *ZEROS
077500030826if  7c                   if        PARfr <> 'WRKF '
077600950808     C                   EXCEPT    VUOTA
077700030826e   7c                   endif
077800950808   X6C                   ELSE
077900030826if  7c                   if        PARfr =  'WRKF '
078000030826     c                   eval      wErr  =  2
078100030826     c                   exsr      WriteWFCBB
078200030826x   7c                   else
078300950808     C                   MOVEL     MSG(2)        STPMSG
078400950808     C                   EXCEPT    MESSG2
078500030826e   7c                   endif
078600950321     C* Stampo segnacolli che si trovano su F.V. diverso e non ancora
078700950321     C* abbinato
078800030826if 6ac                   if        PARfr <> 'WRKF '
078900950321     C                   Z-ADD     1             II
079000950322     C                   MOVE      ' '           W001A             1
079100950808    7C     NRS(II)       DOWNE     *ZEROS
079200950808    8C     W001A         IFEQ      ' '
079300950321     C                   EXCEPT    DETSC
079400950322     C                   MOVE      '1'           W001A
079500950808   X8C                   ELSE
079600950322     C                   EXCEPT    DETSC1
079700950322     C                   MOVE      ' '           W001A
079800950808    8C                   END
079900950321     C                   ADD       1             II
080000950808    7C                   ENDDO
080100950808     C                   EXCEPT    VUOTA
080200030826e  6ac                   endif
080300950808    6C                   END
080400950321     C*
080500950808    5C                   ENDIF
080600941219     C*
080700940620    4C                   ENDIF
080800000322   4AC                   ENDIF
080900940620    3C                   ENDIF
081000950808    2C                   ENDDO
081100950808    1C                   ENDIF
081200911104     C*
081300030826     ***C   99              EXCEPT    TOTALE
081400030826if  1c                   if        PARfr <> 'WRKF '
081500030826     c                             and   *in99
081600030826     c                   except    TOTALE
081700030826e   1c                   endif
081800911104     C*
081900990304     C                   CLEAR                   DSBS02
082000990304     C                   MOVEL     'C'           T02TLA
082100990304     C                   CALL      'TIBS02R'
082200990304     C                   PARM                    KPJBA
082300990304     C                   PARM                    DSBS02
082400990304     C                   CLEAR                   DSBS02
082500911028     C                   SETON                                        LR
082600941214     C**************************************************************************
082700941214     C* CONTROLLO SE BOLLA DI FV ABBINATO
082800941214     C**************************************************************************
082900940620     C     ABFVA         BEGSR
083000960321     C                   CLEAR                   WNFA
083100960321     C                   CLEAR                   WDFA
083200960321     C* CONTROLLO SE SPEDIZIONE LOCALE
083300051019     C***  ARBLNP        LOOKUP    L1                                     61
083400051019     C     ARBtfp        comp      arbtfa                                 61
083500941214     C*
083600940620     C* CONTROLLO SE IL FOGLIO VIAGGIO E'GIA' ABBINATO
083700960321    0C     ARBNFV        IFEQ      99999
083800960321     C     *IN61         OREQ      *ON
083900960321     C                   Z-ADD     *ALL'9'       WNFA
084000960321     C* CHAIN TIPO BOLLA
084100960321     C                   MOVEL     '3A'          TBLCOD
084200960321     C                   MOVEL(P)  ARBCBO        TBLKEY
084300960321     C     KTAB          CHAIN     TABEL00F                           13
084400960321     C  N13              MOVEL     TBLUNI        DS3A
084500960321     C   13              CLEAR                   DS3A
084600960321     C*
084700960321     C* 1) SE SI TRATTA DI BOLLA DI RECUPERO. STAMPO SOLO SE ABILITATO
084800960321     C*    L'ASSEGNATO   (LASCIO DATA ARRIVO = 0)
084900960321    1C     §3ARBL        IFEQ      'R'
085000960321    2C     ARBACA        IFNE      'S'
085100960321     C                   CLEAR                   WNFA
085200960321    2C                   ENDIF
085300960321     C*
085400960321   X1C                   ELSE
085500960321     C* 2) SE LNP DELLA &6 --> STAMPO SEMPRE LASCIO DATA ARRIVO =0
085600960322     C                   MOVEL     ARBLNP        W003A             3
085700960322     C     W003A         LOOKUP    LIP                                    13
085800960321    2C     *IN13         IFEQ      *OFF
085900960321     C*
086000960321     C* 3) SE LA DATA ARRIVO NON C'E', PERCHE' BOLLA SENZA SEGNACOLLI
086100960321     C*  CONFRONTO CON LA DATA SPEDIZIONE
086200960321    3C     ARBDAM        IFEQ      0
086300960321     C                   MOVEL     ARBDSP        WDFA
086400960321   X3C                   ELSE
086500960321     C*
086600960321     C* 4) SE LA BOLLA HA LA DATA DI ARRIVO LA USO
086700960321     C                   MOVEL     ARBDAM        WDFA
086800960321    3C                   ENDIF
086900960321    2C                   ENDIF
087000960321    1C                   ENDIF
087100960321     C*
087200960321   X0C                   ELSE
087300960321     C*
087400971224     C* RICHIAMO PGM PER CERCARE FOGLIO VIAGGIO DELLA BOLLA
087500971224     C                   CLEAR                   DSLV53
087600971224     C                   MOVEL     'A'           D53TFO
087700971224     C                   MOVE      SIMFEL        D53FEL
087800971224     C                   Z-ADD     ARBNFV        D53NFV
087900981214     C                   MOVE      ARBTFP        D53FGS
088000031211     c* controllo la bolla se relativa a foglio abbinato
088100031211     C                   MOVEL     'B'           D53ABB
088200971224     C                   MOVE      ARBLNA        D53LNA
088300971224     C                   MOVEL     ARBTFA        D53TFA
088400971224     C                   CALL      'FNLV53R'
088500971224     C                   PARM                    DSLV53
088600960321     C*
088700960321     C* SE NON HO TROVATO IL FOGLIO ARRIVI
088800960321     C* VERIFICO SE E' GIA' STATO PULITO
088900971224    1C     D53ERR        IFNE      *BLANKS
089000960321    2C     ARBDSP        IFLE      COMFVA
089100021212     C* - se non trovato foglio arrivi:
089200021212     C*   escludo se PARINC = 'NO' (NON richiesti SOLO incasso)
089300021212     C     D53err        OReq      '2'
089400021212     C     PARinc        andne     'NO'
089500960321     C* FOGLIO PARTENZA GIA' PULITO: COME SE FOSSE ABBINATO
089600960321     C                   Z-ADD     *ALL'9'       WNFA
089700960321     C                   MOVEL     ARBDAM        WDFA
089800960321   X2C                   ELSE
089900960321     C                   CLEAR                   WNFA
090000960321     C                   CLEAR                   WDFA
090100960321    2C                   ENDIF
090200960321     C**
090300960321   X1C                   ELSE
090400971224     C                   MOVEL     D53NFA        WNFA
090500971224     C                   MOVEL     D53DFA        WDFA
090600960321    1C                   ENDIF
090700960321    0C                   ENDIF
090800941214     C*
090900940620     C                   ENDSR
091000941214     C**************************************************************************
091100941214     C* CONTROLLA DETTAGLIO COLLI
091200941214     C**************************************************************************
091300940620     C     DETCOL        BEGSR
091400941214     C*
091500940620     C                   EXSR      DETS                                         DETT. COLLI
091600940620     C     H             ADD       K             ZZ                2 0
091700940620     C*
091800940620    1C     ZZ            IFGE      ARBNCL
091900940620     C                   SETON                                        34
092000940620     C                   ELSE
092100940620     C                   SETOFF                                       34
092200940620    1C                   END
092300940620     C*
092400940620     C     T             ADD       W             YY                2 0
092500940620     C*
092600940620    1C     YY            IFGE      ARBNCL
092700940620     C                   SETON                                        35
092800940620     C                   ELSE
092900940620     C                   SETOFF                                       35
093000940620    1C                   END
093100941214     C*
093200940620     C                   ENDSR
093300941214     C**************************************************************************
093400941215     C* STAMPA DISTINTA USCITA CONSEGNE PER FNARB01L
093500941214     C**************************************************************************
093600911025     C     STAMPA        BEGSR
093700991104     C* IMPOSTAZIONI DI STAMPA COMUNI
093800991104     C                   EXSR      IMPCOM
093900010330     C     WINCPT        IFEQ      *BLANKS
094000991104     C                   ADD       ARBNCL        STPTCO            5 0
094100911206     C*
094200941215     C                   ADD       ARBPKF        V06PKF            8 1
094300941215     C                   ADD       ARBVLF        V06VOL            7 3
094400010330     C                   ENDIF
094500911025     C                   ADD       1             V06TSP            4 0
094600051214     c
094700911025     C                   MOVE      ARBDCR        MMGG              4 0
094800911025     C                   MOVEL     MMGG          MM                2 0
094900911025     C                   MOVE      MMGG          GG                2 0
095000911025     C                   MOVE      MM            STPGMC            4 0
095100911025     C                   MOVEL     GG            STPGMC
095200960516     C                   MOVEL     ARBHCR        STPHCR
095300051214     c* Verifico se è tassativa
095400051214     c                   movel     arbtcr        stptcr            1
095500051214     c                   if        stptcr=' ' and arbdcr>0
095600051214     c                   clear                   fnlv80ds
095700051214     c                   eval      ilv80aas=arbaas
095800051214     c                   eval      ilv80lnp=arblnp
095900051214     c                   eval      ilv80nrs=arbnrs
096000051214     c                   eval      ilv80nsp=arbnsp
096100051214     c                   eval      ilv80mgs=arbmgs
096200051214     c                   eval      ilv80tc1=arbtc1
096300051214     c                   eval      ilv80tc2=arbtc2
096400051214     c                   eval      ilv80dcr=arbdcr
096500051214     c                   eval      ilv80hcr=arbhcr
096600051214     c                   eval      ilv80tcr=arbtcr
096700051214     c                   call      'FNLV80R'
096800051214     c                   parm                    kpjba
096900051214     c                   parm                    fnlv80ds
097000051214     c
097100051214     c                   if        olv80tat='S'
097200051214     c                   movel     'T'           stptcr
097300051214     c                   endif
097400051214     c                   endif
097500051214     c
097600950119     C*
097700911104     C     COMFNS        IFEQ      'S'                                          *B003
097800911025     C                   SETON                                        95
097900911025     C                   ELSE
098000911025     C                   SETOFF                                       95
098100911025     C                   END                                                    *E003
098200950119     C*
098300911106     C* CONTROLLO SE ESISTONO ANOMALIE
098400911106     C     ARBFAN        IFEQ      'S'
098500911106     C                   SETON                                        96
098600911106     C                   ELSE
098700911106     C                   SETOFF                                       96
098800911106     C                   END
098900950119     C*
099000950119     C     ARBVLF        IFGT      §3IVLF
099100950119     C     ARBPKF        ORGT      §3IPKF
099200920527     C                   SETON                                        44
099300920527     C                   ELSE
099400920527     C                   SETOFF                                       44
099500920527     C                   END
099600911025     C*
099700930426     C* INDICATORE DI CONSEGNA PARZIALE
099800930426     C     ARBDCP        COMP      0                                  94
099900941215     C*
100000941215     C                   EXSR      SCRIVI                                       STAMPA RIGA
100100941215     C*
100200960517     C     *IN77         IFEQ      *ON
100300960517     C*
100400950322     C     SP1(1)        IFGT      0
100500960517     C     ARBAMA        IFEQ      0
100600920527     C                   EXCEPT    NOAP1
100700960517     C                   ELSE
100800950322     C                   EXCEPT    NOAP1X
100900960517     C                   ENDIF
101000950322     C                   END
101100950322     C*
101200920527     C     SP2(1)        IFGT      0
101300920527     C                   EXCEPT    NOAP2
101400920527     C                   END
101500920527     C     SA1(1)        IFGT      0
101600920527     C                   EXCEPT    NOAA1
101700920527     C                   END
101800920527     C     SA2(1)        IFGT      0
101900920527     C                   EXCEPT    NOAA2
102000920527     C                   END
102100960517     C*
102200960517     C                   ENDIF
102300950119     C**
102400950119     C** SEGNACOLLI NON SEQUENZIALI
102500911025     C   95              DO                                                     *B003
102600950119     C*
102700090309     C  N90KARB1         SETLL     FiART01L
102800090309     C   90KLBL          SETLL     FiART01L
102900911025     C                   Z-ADD     1             S                 2 0
103000941215     C                   Z-ADD     *ZEROS        NSC
103100911025     C                   DO        *HIVAL                                       *B004
103200090309     C  N90KARB1         READE     FiART01L                               63
103300090309     C   90KLBL          READE     FiART01L                               63
103400911025     C  N63S             DOWLE     13                                           *B005
103500941215     C                   MOVE      ARTNSC        NSC(S)
103600911025     C                   ADD       1             S
103700090309     C  N90KARB1         READE     FiART01L                               63
103800090309     C   90KLBL          READE     FiART01L                               63
103900911025     C  N63              END                                                    *E005
104000941215     C                   EXCEPT    DETNSC
104100911025     C                   Z-ADD     1             S
104200941215     C                   Z-ADD     *ZEROS        NSC
104300911025     C  N63              END                                                    *E004
104400911025     C                   END                                                    *E003
104500941214     C*
104600911025     C                   ENDSR
104700991104     C**************************************************************************
104800991104     C* IMPOSTAZIONI COMUNI PER LA STAMPA
104900991104     C**************************************************************************
105000991104     C     IMPCOM        BEGSR
105100991105     C                   CLEAR                   CONSE1
105200991105     C                   CLEAR                   CONSE2
105300991104     C                   MOVEL     *ZEROS        STPGMC
105400991104     C                   MOVEL     *ZEROS        STPHCR
105500991104     C                   CLEAR                   AR9TIC                         TIPO INCASSO CONTRAS
105600991104     C                   CLEAR                   AR9CAS                         CONTRASSEGNO
105700991104     C                   CLEAR                   AR9VCA                         VALUTA
105800991104     C                   CLEAR                   AR9CMB                         CAMBIO
105900991104     C                   CLEAR                   COMFNS
106000991104     C                   CLEAR                   WCONVE
106100991105     C                   CLEAR                   W0133
106200991105     C                   CLEAR                   WIFT
106300991105     C                   Z-ADD     0             STPIM0            9 0
106400991105     C                   Z-ADD     0             STPIMD            9 3
106500991104     C                   MOVEL     *BLANKS       STPTIC                         TIPO INCASSO CONTRAS
106600991104     C                   Z-ADD     0             STPCAS                         CONTRASSEGNO
106700991104     C                   CLEAR                   STPDIV                         VALUTA C/A
106800991104     C                   CLEAR                   STPVCA                         VALUTA C/A
106900991104     C                   CLEAR                   STPCMB                         CAMBIO C/A
107000991104     C                   SETOFF                                       5590
107100991104     C*
107200991104     C* CONTROLLO SE LA BOLLA IN QUESTIONE HA LEGAME CON SECONDA BOLLA
107300991104     C     KARB1         CHAIN     FNLBL01L                           54
107400991104     C*
107500991104     C  N54LBLLAN        COMP      LBLLAP                             5454
107600991104     C*
107700991104    1C     *IN54         IFEQ      *OFF
107800991104     C                   SETON                                        90
107900991104     C*
108000991104     C     KLBL          CHAIN     FNARB01L                           55
108100051114     C  N55KLBL          CHAIN     FiAR901L                           89
108200991104     C*
108300991104     C  N55
108400991104    2CANN89ARBICC        IFEQ      ' '
108500991104     C     ARBICC        OREQ      'R'
108600991104     C                   MOVEL     AR9VCA        STPVCA                         VALUTA C/A   CONTRAS
108700991104     C                   MOVEL     AR9TIC        STPTIC                         TIPO INCASSO CONTRAS
108800991104     C                   Z-ADD     AR9CAS        STPCAS                         CONTRASSEGNO
108900991104     C                   Z-ADD     AR9CMB        STPCMB                         CAMBIO
109000991104    2C                   END
109100991104     C  N55              MOVE      ARBFNS        COMFNS            1
109200991104     C*
109300991104     C* RIAGGANCIO DI NUOVO LA FIGLIA PER RECUPERARE GLI ALTRI DATI
109400991104     C  N54KARB2         CHAIN     FNARB01L                           55
109500991104     C*
109600991104   X1C                   ELSE
109700991104     C                   MOVE      ARBFNS        COMFNS
109800991104     C*
109900991104     C* SE NON C'E' LEGAME CONTROLLO IL C/A DELLA BOLLA CHE STO LEGGEND
110000051114     C     KARB1         CHAIN     FiAR901L                           89
110100991104    2C  N89ARBICC        IFEQ      ' '
110200991104     C     ARBICC        OREQ      'R'
110300991104     C                   MOVEL     AR9VCA        STPVCA                         VALUTA C/A   CONTRAS
110400991104     C                   MOVEL     AR9TIC        STPTIC                         TIPO INCASSO CONTRAS
110500991104     C                   Z-ADD     AR9CAS        STPCAS                         CONTRASSEGNO
110600991104     C                   Z-ADD     AR9CMB        STPCMB                         CAMBIO
110700991104    2C                   ENDIF
110800991104    1C                   ENDIF
110900991104     C*
111000991104     C* DECODIFICO IL TIPO BOLLA
111100991104     C                   MOVEL     '3A'          TBLCOD
111200991104     C                   MOVEL     *BLANKS       TBLKEY
111300991104     C                   MOVEL     ARBCBO        TBLKEY
111400991104     C*
111500991104     C     KTAB          CHAIN     TABEL00F                           13
111600991104     C  N13              DO
111700991104     C                   MOVEL     TBLUNI        DS3A
111800991104     C                   MOVEL     §3ADES        VIDCBL            1
111900991104     C                   MOVEL     §3ATB1        COM01             1
112000991104     C                   END
112100991104     C*
112200991104     C                   Z-ADD     ARBDSP        G02INV                         AAAA/MM/GG
112300991104     C                   Z-ADD     *ZERO         G02DAT
112400991104     C                   MOVEL     '3'           G02ERR
112500991104     C                   CALL      'XSRDA8'
112600991104     C                   PARM                    WLBDAT
112700991104     C                   MOVE      G02DAT        W0020             2 0
112800991104     C                   MOVEL     G02DAT        STPDSP            6 0          GG/MM/AA
112900991104     C                   MOVE      W0020         STPDSP
113000991104     C*
113100991104     C* AGGANCIO I DATI DI TASSAZIONE BOLLA
113200991104     C                   CLEAR                   AR6IFT                         IMPORTO FATTURA
113300991104     C                   CLEAR                   AR6DIV                         IMPORTO FATTURA
113400991104    1C     §3ATB2        IFEQ      ' '
113500991104     C     COM01         ANDEQ     'A'
113600991104     C*
113700991104     C                   MOVEL     1             KTRC
113800991105     C     KAR6          CHAIN     FIAR601L                           89
113900991104   X1C                   ELSE
114000991104     C*
114100991104     C* PER 2 BOLLA
114200991104    2C     §3ATB2        IFNE      *BLANKS
114300991104     C                   MOVEL     2             KTRC
114400991105     C     KAR6          CHAIN     FIAR601L                           89
114500991104    2C                   ENDIF
114600991104    1C                   ENDIF
114700991104     C**
114800991104    1C     AR6IFT        IFGT      0
114900991104    2C     ARBFIP        IFEQ      'I'
115000991104     C     ARBICA        OREQ      ' '
115100991104     C     ARBICA        OREQ      'R'
115200991104     C* LEGGO DA ARI GLI INCASSI PARZIALI FATTI
115300991104     C     KARI          CHAIN     FIARI01L                           89
115400991104    3C     *IN89         DOWEQ     *OFF
115500991104     C*
115600991104     C     ARIIMP        ADD       ARIIPP        WIPP
115700991104    4C     WIPP          IFGT      0
115800011025    5C     ARIDIV        IFNE      §GEDCN
115900011025      *
116000991104     C**
116100991104     C                   CLEAR                   YEUR
116200991104     C                   MOVEL     ARIDIV        YECDVI
116300011025     C                   MOVEL     §GEDCN        YECDVO
116400011025     C                   MOVE      §CVDEC        YECDCO
116500991104     C                   Z-ADD     WIPP          YECIMI
116600991104     C*
116700991104     C                   CALL      'YEURCO'
116800991104     C                   PARM                    YEUR
116900991104     C*
117000991104    6C     YECESI        IFEQ      ' '
117100991104     C                   ADD       YECIMO        WCONVE
117200991104    6C                   ENDIF
117300991104   X5C                   ELSE
117400991104     C                   ADD       WIPP          WCONVE
117500991104    5C                   ENDIF
117600991104    4C                   ENDIF
117700991104     C**
117800991104     C     KARI          READE     FIARI01L                               89
117900991104    3C                   ENDDO
118000991105     C**
118100991105     C** TRASFORMO LA FATTURA NELLA DIVISA DELLA MONETA DI CONTO
118200011025     C     AR6DIV        IFNE      §GEDCN
118300011025      *
118400991105     C                   CLEAR                   YEUR
118500991105     C                   MOVEL     AR6DIV        YECDVI
118600011025     C                   MOVEL     §GEDCN        YECDVO
118700011025     C                   MOVE      §CVDEC        YECDCO
118800991105     C                   Z-ADD     AR6IFT        YECIMI
118900991105     C*
119000991105     C                   CALL      'YEURCO'
119100991105     C                   PARM                    YEUR
119200991105     C*
119300991105    6C     YECESI        IFEQ      ' '
119400991105     C                   Z-ADD     YECIMO        WIFT
119500991105     C                   ENDIF
119600991105     C                   ELSE
119700991105     C                   Z-ADD     AR6IFT        WIFT
119800991105     C                   ENDIF
119900991105     C**
120000991105     C     WIFT          SUB       WCONVE        W0133            13 3
120100991105    3C     W0133         IFGT      0
120200011025     C                   MOVEL     §GEDCN        STPDIV
120300991105     C                   MOVE      W0133         W0030             3 0
120400991105     C* IMPORTO CON DECIMALI
120500991105     C     W0030         IFGT      0
120600991105     C                   Z-ADD     W0133         STPIMD
120700991105     C                   SETON                                        01
120800991105     C                   ELSE
120900991105     C* IMPORTO SENZA DECIMALI
121000991105     C                   Z-ADD     W0133         STPIM0
121100991105     C                   SETOFF                                       01
121200991105     C                   ENDIF
121300991105     C**
121400991104    3C                   END
121500991104    2C                   END
121600991104    1C                   END
121700991104     C*
121800991104     C* CALCOLO IL TOTALE DA INCASSARE
121900991104     C* PER IL C/A SE E' IN DIVISA LO CAMBIO
122000991104     C                   EXSR      CTRCAS
122100991104     C*
122200991105     C* TOTALE DA INCASSAE
122300991105     C                   ADD       W0133         STPTOC           17 3
122400991105     C                   ADD       WCAS          STPTOC
122500991105     C*
122600991105     C* CONSEGNE PARTICOLARI
122700991105    1C     ARBTC1        IFNE      *BLANK
122800991105     C                   MOVEL     '1P'          TBLCOD
122900991105     C                   MOVEL     *BLANKS       TBLKEY
123000991105     C                   MOVEL     ARBTC1        TBLKEY
123100991105     C     KTAB          CHAIN     TABEL00F                           13
123200991105     C  N13              MOVEL     TBLUNI        DS1P
123300991105     C  N13              MOVEL     §1PDES        CONSE1           25
123400991105    1C                   END
123500991105     C* DECODIFICO 2° TIPO CONSEGNA PARTICOLARE
123600991105    1C     ARBTC2        IFNE      *BLANK
123700991105     C                   MOVEL     '1P'          TBLCOD
123800991105     C                   MOVEL     *BLANKS       TBLKEY
123900991105     C                   MOVEL     ARBTC2        TBLKEY
124000991105     C     KTAB          CHAIN     TABEL00F                           13
124100991105     C  N13              MOVEL     TBLUNI        DS1P
124200991105     C  N13              MOVEL     §1PDES        CONSE2           25
124300991105     C*
124400991105     C* SE C'E' SOLO LA 2° CONS.PARTICOLARE, METTO LA
124500991105     C* DECODIFICA DELLA 2° AL POSTO DI QUELLA DELLA 1°
124600991105    2C     CONSE1        IFEQ      *BLANKS
124700991105     C     CONSE2        ANDNE     *BLANKS
124800991105     C                   MOVEL     CONSE2        CONSE1
124900991105     C                   CLEAR                   CONSE2
125000991105    2C                   ENDIF
125100991105    1C                   ENDIF
125200991105     C*
125300991104     C                   ENDSR
125400941214     C**************************************************************************
125500941215     C* STAMPA DISTINTA USCITA CONSEGNE PER FNARB
125600941214     C**************************************************************************
125700911105     C     STAMP1        BEGSR
125800991104     C* IMPOSTAZIONI COMUNI DI STAMPA
125900991104     C                   EXSR      IMPCOM
126000950119     C*
126100920123     C                   SETON                                        9598
126200911106     C                   SETOFF                                       96
126300991104     C**
126400991105     C* NON MI SERVE IL FLAG NON SEQ.
126500991104     C                   CLEAR                   COMFNS
126600911105     C*
126700920123     C* STAMPO SOLO SE DEVO INCASSARE
126800950119    1C     STPCAS        IFGT      *ZEROS
126900991105     C     W0133         ORGT      *ZEROS
127000030925if  2c                   if        PARfr = 'WRKF '
127100030925     c                   clear                   wErr
127200030925     c                   exsr      WriteWFCBB
127300030925x   2c                   else
127400941215     C                   EXSR      SCRIVI                                       STAMPA RIGA
127500030925e   2c                   endif
127600920211     C                   ADD       1             V06TSP            4 0
127700950119    1C                   END
127800991105     C*
127900920123     C                   SETOFF                                       98
128000941214     C*
128100911105     C                   ENDSR
128200941215     C**************************************************************************
128300941215     C* STAMPA DELLA RIGA DI DETTAGLIO
128400941215     C**************************************************************************
128500941215     C     SCRIVI        BEGSR
128600960517     C                   SETOFF                                       16
128700941215     C*
128800941215     C* PER OVERFLOW STAMPO LA TESTATA
128900941215     C  N99
129000941215     COR OF              EXCEPT    TESTA
129100941215     C  N99              SETON                                        99
129200941215     C*
129300941215     C* PRIMA DI STAMPARE AGGANCIO, SE CI SONO, LE NOTE
129400941215     C                   MOVEL     *BLANKS       NOTE1
129500941215     C                   MOVEL     '8'           KTRC                           TIPO RECORD
129600060216     C     KAR4          CHAIN     FiAR401L                           89
129700941215     C  N89              MOVEL     AR4NOT        NOTE1            35
129800950119     C*
129900941215     C                   MOVEL     *BLANKS       NOTE2
130000941215     C                   MOVEL     '9'           KTRC                           TIPO RECORD
130100060216     C     KAR4          CHAIN     FiAR401L                           89
130200941215     C  N89              MOVEL     AR4NOT        NOTE2            35
130300941215     C*
130400941215     C* SE NOTE1 E' VUOTO E NOTE2 E' PIENO, METTO NOTE2 IN NOTE1
130500941215     C     NOTE1         IFEQ      *BLANKS
130600941215     C     NOTE2         ANDNE     *BLANKS
130700941215     C                   MOVEL     NOTE2         NOTE1
130800941215     C                   CLEAR                   NOTE2
130900941215     C                   ENDIF
131000030320
131100030320      * Controllo se bolla mai stampata
131200030320     c                   Eval      *In12 = *Off
131300030320     c                   Clear                   Dsbl4a
131400030320     c                   Movel     'A'           Ktrc
131500060216     c     Kar4          Chain     Fiar401l
131600060216     c                   If        %Found(Fiar401l)
131700030320     c                   Movel     Ar4Not        Dsbl4a
131800030320     c                   EndIf
131900030320     c                   If        §b4abm = 'N'
132000030320     c                   Eval      *In12 = *On
132100030320     c                   EndIf
132200941216     C*
132300960515     C                   MOVEL     ARBRSD        STPRSD           15            DESTINATARIO
132400960515     C                   MOVEL     ARBRSM        STPRSM           15            MITTENTE
132500010330     C     WINCPT        IFEQ      'S'
132600010330     C                   SETON                                        97
132700010330     C                   ENDIF
132800941215     C*
132900941215     C* RIGA DI DETTAGLIO DELLA BOLLA
133000941215     C                   EXCEPT    DETT
133100010330     C*
133200010330     C                   SETOFF                                       97
133300990127     C**
133400990127     C* VEDO SE ESISTONO C.A. SULLA SPEDIZIONE
133500990127     C                   SETOFF                                       05
133600040809     C**""*KARB1         CHAIN     FNDCT02L                           89
133700990127     C**
133800040809    1C**""**IN89         DOWEQ     *OFF
133900040809     C**""*DCTATB        ANDNE     ' '
134000040809     C**""*KARB1         READE     FNDCT02L                               89
134100040809    1C**""*              ENDDO
134200040809      *
134300040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
134400040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
134500040809      * legate alla spedizione che passo
134600040809     c                   clear                   fidn12ds
134700040809     c                   eval      i12aas = ARBAAS
134800040809     c                   eval      i12lnp = ARBLNP
134900040809     c                   eval      i12nrs = ARBNRS
135000040809     c                   eval      i12nsp = ARBNSP
135100040809     c                   eval      i12fel = 'E'
135200040809     c                   eval      i12fan = 'E'
135300040809     c*
135400040809     c                   call      'FIDN12R'
135500040809     c                   parm                    fidn12ds
135600040809     c*
135700040809     c                   setoff                                       89
135800040809     c*
135900040809     c* se non ci sono errori
136000040809     c                   if        o12err <> ' '
136100040809     c                   seton                                        89
136200040809     c                   else
136300040809     c* se numero di CA trovate maggiore di zero
136400040809     c                   if        o12nca = 0
136500040809     c                   seton                                        89
136600040809     c                   else
136700040809     c                   eval      jj = 1
136800040809     c                   exsr      PREDN12R
136900040809     c                   endif
137000040809     c                   endif
137100040809     C**
137200990127     C  N89              SETON                                        05
137300941215     C*
137400960515     C* SE NECESSARIO, EMETTO UN'ALTRA RIGA DI DETTAGLIO
137500990127    1C     NOTE1         IFNE      *BLANKS                                      1° RIGA NOTE
137600960515     C     CONSE1        ORNE      *BLANKS                                      1° CONS.PARTIC.
137700960515     C     *IN96         OREQ      *ON                                          ANOMALIE
137800960515     C     *IN94         OREQ      *ON                                          CONS.PARZIALE
137900960515     C     *IN98         ANDEQ     *OFF
138000990127     C     *IN05         OREQ      *ON                                          CONS.PARZIALE
138100990127     C* PREPARO I DATI DELLA C.A.
138200990127     C   05              EXSR      PRECA
138300960517     C*
138400990127    2C     CONSE1        IFNE      *BLANKS
138500960517     C                   SETON                                        16
138600990127    2C                   ENDIF
138700040809     C*
138800941215     C                   EXCEPT    DETT1
138900990127     C*
139000990127     C* SE CI SONO ALTE C.A. STAMPO ANCORA
139100040809     C**""*KARB1         READE     FNDCT02L                               89
139200040809    2C**""**IN89         DOWEQ     *OFF
139300040809    3C**""*DCTATB        IFEQ      ' '
139400040809     C**""*              EXSR      PRECA
139500990127     C** STAMPO TESTATA E DETT C.A.
139600040809     C**""*   OF              EXCEPT    TESTA
139700040809     C**""*              EXCEPT    DETCA
139800040809    3C**""*              ENDIF
139900040809     C**""*KARB1         READE     FNDCT02L                               89
140000040809    2C**""*              ENDDO
140100040809      *
140200040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
140300040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
140400040809      * legate alla spedizione che passo
140500040809     c                   clear                   fidn12ds
140600040809     c                   eval      i12aas = ARBAAS
140700040809     c                   eval      i12lnp = ARBLNP
140800040809     c                   eval      i12nrs = ARBNRS
140900040809     c                   eval      i12nsp = ARBNSP
141000040809     c                   eval      i12fel = 'E'
141100040809     c                   eval      i12fan = 'E'
141200040809     c*
141300040809     c                   call      'FIDN12R'
141400040809     c                   parm                    fidn12ds
141500040809     c*
141600040809     c                   setoff                                       89
141700040809     c*
141800040809     c* se non ci sono errori
141900040809     c                   if        o12err <> ' '
142000040809     c                   seton                                        89
142100040809     c                   else
142200040809     c* se numero di CA trovate maggiore di zero
142300040809     c                   if        o12nca = 0
142400040809     c                   seton                                        89
142500040809     c                   else
142600040810     c                   add       1             jj
142700040809     c                   dow       jj <= o12nca and *in89 = *off
142800040809     c                   exsr      PREDN12R
142900040809     C                   EXSR      PRECA
143000040809     C** STAMPO TESTATA E DETT C.A.
143100040809     C   OF              EXCEPT    TESTA
143200040809     C                   EXCEPT    DETCA
143300040809     c*
143400040809     c                   add       1             jj
143500040809     c                   enddo
143600040809     c*
143700040809     c                   seton                                        89
143800040809     c*
143900040809     c                   endif
144000040809     c                   endif
144100040809     C**
144200990127    1C                   ENDIF
144300941215     C*
144400941215     C                   ENDSR
144500040809     C**************************************************************************
144600040809     C* PREPARO I DATI DELLA C.A. REPERENDOLI DALLA DS DEL *PGM UTILITA' FIDN12R
144700040809     C**************************************************************************
144800040809     C     PREDN12R      BEGSR
144900040809     C*
145000040809     C                   movea     skkey(jj)     dskey
145100040809     C                   eval      DCTFIL = dsfil
145200040809     C                   eval      DCTNCA = dsnca
145300040809     C                   eval      DCTDCH = skdch(jj)
145400040809     C                   eval      DCTCCH = skcch(jj)
145500040809     C*
145600040809     C* Aggancio il file FNDCT01L x reperire i dati relativi alla data data apertura CA
145700040809     C     KDCT1_C       chain     FNDCT01L
145800040809     C                   if        not %found(FNDCT01L)
145900040809     C                   eval      DCTAAC = *all'9'
146000040809     C                   eval      DCTMGC = *all'9'
146100040809     C                   endif
146200040809     C*
146300040809     C                   ENDSR
146400941214     C**************************************************************************
146500990127     C* PREPARO I DATI DELLA C.A.
146600941214     C**************************************************************************
146700990127     C     PRECA         BEGSR
146800990127     C                   SETOFF                                       06
146900990304     C                   CLEAR                   STACCH
147000990324     C                   CLEAR                   STACC2
147100990127     C* DATA APERTURA C.A.
147200990127     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
147300990127     C                   Z-ADD     *ZERO         G02DAT
147400990127     C                   MOVEL     '3'           G02ERR
147500990127     C                   CALL      'XSRDA8'
147600990127     C                   PARM                    WLBDAT
147700990127     C                   MOVE      G02DAT        W0020             2 0
147800990127     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
147900990127     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
148000990127     C** DATA CNHIUSUA C.A.
148100990127     C                   CLEAR                   STACHC
148200990127     C     DCTDCH        IFGT      0
148300990127     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
148400990127     C                   Z-ADD     *ZERO         G02DAT
148500990127     C                   MOVEL     '3'           G02ERR
148600990127     C                   CALL      'XSRDA8'
148700990127     C                   PARM                    WLBDAT
148800990127     C                   MOVE      G02DAT        W0020             2 0
148900990127     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
149000990127     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
149100990127     C                   SETON                                        06
149200990304     C* CAUSALE CHIUSURA
149300990304    2C     DCTCCH        IFNE      *BLANKS
149400990304     C                   CLEAR                   DCCH
149500990304     C                   CLEAR                   DSBS02
149600990304     C                   MOVEL     'C'           T02MOD
149700990304     C                   MOVEL     KNSIF         T02SIF
149800990304     C                   MOVEL     'CCH'         T02COD
149900990304     C                   MOVEL     DCTCCH        T02KE1
150000990304     C                   CALL      'TIBS02R'
150100990304     C                   PARM                    KPJBA
150200990304     C                   PARM                    DSBS02
150300990304     C*
150400990304    3C     T02ERR        IFEQ      ' '
150500990304     C                   MOVEL     T02UNI        DCCH
150600990304    3C                   ENDIF
150700990304     C                   MOVEL     §CHDES        STACCH           25
150800990324     C                   MOVEL     §CHDES        STACC2           15
150900990304    2C                   ENDIF
151000990127     C                   ENDIF
151100990127     C                   ENDSR
151200990127     C**************************************************************************
151300990127     C* ANALISI DETTAGLIO SEGNACOLLI
151400990127     C**************************************************************************
151500990127     C     DETS          BEGSR
151600941214     C*
151700920527     C                   Z-ADD     0             H                 2 0
151800920527     C                   Z-ADD     0             K                 2 0
151900920527     C                   Z-ADD     0             W                 2 0
152000920527     C                   Z-ADD     0             T                 2 0
152100950322     C                   Z-ADD     0             II                3 0
152200920527     C                   MOVEA     *ZEROS        SP1
152300920527     C                   MOVEA     *ZEROS        SP2
152400920527     C                   MOVEA     *ZEROS        SA1
152500920527     C                   MOVEA     *ZEROS        SA2
152600950321     C                   MOVEA     *ZEROS        NRS
152700950321     C                   MOVEA     *ZEROS        FGS
152800950321     C                   MOVEA     *ZEROS        NFV
152900950321     C                   MOVEA     *ZEROS        DFV
153000950322     C     *IN77         IFEQ      *OFF
153100950322    1C     ARBAMA        ANDEQ     0
153200950322     C     *IN77         OREQ      *ON
153300090309     C     KARB1         SETLL     FiART01L                           10
153400090309     C     KARB1         READE     FiART01L                               10
153500950321    2C     *IN10         DOWEQ     '0'
153600950322    3C     ARBAMP        IFEQ      0
153700950321    4C     ARTFLP        IFNE      *ZERO                                        FIL.TRANSITO
153800950321    5C     ARTDUT        IFEQ      0
153900920527     C                   ADD       1             H
154000950321    6C     H             IFGT      12
154100920527     C     H             SUB       12            K
154200950321    7C     K             IFGT      12
154300920527     C                   Z-ADD     12            K
154400920527     C                   Z-ADD     9999999       SP2(K)
154500920527     C                   SETON                                        10
154600920527     C                   ELSE
154700941215     C                   Z-ADD     ARTNSC        SP2(K)
154800950321    7C                   END
154900920527     C                   ELSE
155000941215     C                   Z-ADD     ARTNSC        SP1(H)
155100950321    6C                   END
155200950321    5C                   END
155300920527     C                   ELSE
155400950321    5C     ARTDFV        IFEQ      0
155500920527     C                   ADD       1             H
155600950321    6C     H             IFGT      12
155700920527     C     H             SUB       12            K
155800950321    7C     K             IFGT      12
155900920527     C                   Z-ADD     12            K
156000920527     C                   Z-ADD     9999999       SP2(K)
156100920527     C                   SETON                                        10
156200920527     C                   ELSE
156300941215     C                   Z-ADD     ARTNSC        SP2(K)
156400950321    7C                   END
156500920527     C                   ELSE
156600941215     C                   Z-ADD     ARTNSC        SP1(H)
156700950321    6C                   END
156800950321    5C                   END
156900950321    4C                   END
157000920527     C                   ELSE
157100950321    4C     ARTDAM        IFEQ      0
157200920527     C                   ADD       1             W
157300950321    5C     W             IFGT      12
157400920527     C     W             SUB       12            T
157500950321    6C     T             IFGT      12
157600920527     C                   Z-ADD     12            T
157700920527     C                   Z-ADD     9999999       SA2(T)
157800920527     C                   SETON                                        10
157900920527     C                   ELSE
158000941215     C                   Z-ADD     ARTNSC        SA2(T)
158100950321    6C                   END
158200920527     C                   ELSE
158300941215     C                   Z-ADD     ARTNSC        SA1(W)
158400950321    5C                   END
158500950321    4C                   END
158600950807    3C                   END
158700950321     C* SOLO SE RICHIESTA STAMPA SPEDIZIONI NON PARTITE verifico se
158800950321     C* il collo si trova in un altro F.V. non ancora abbinato ed in
158900950321     C* questo caso memorizzo nr.segnacollo, LNP/numero e data F.V. per
159000950321     C* poi stamparli nel caso di "COLLI PARTITI MA SPEDIZIONE NON ABI-
159100950321     C* LITATA".
159200950807    3C     *IN77         IFEQ      *ON
159300950322     C* Solo se colli partiti
159400950322     C     SP1(1)        ANDEQ     *ZEROS
159500950321     C     ARTABN        ANDNE     'S'
159600950321     C* Prendo l'ultima spunta (Potrei averne più di una in caso di
159700950321     C* disguido).
159800070118     C     KBRV3         SETLL     FNBRV03L
159900070118     C     KBRV3         READE     FNBRV03L                               89
160000950807     C*
160100950807    4C     *IN89         DOWEQ     *OFF
160200950807     C*
160300031215     C**                 Z-ADD     BRVNFV        WNFV
160400031215     C**   KFVA1         CHAIN     FNFVA01L                           89
160500031215     C** 89              MOVE      99991231      FVADFV
160600031215     C** 89              MOVE      *ZEROS        FVANFA
160700031215     C* RICHIAMO PGM PER CERCARE FOGLIO VIAGGIO DELLA BOLLA
160800031215     C                   CLEAR                   DSLV53
160900031215     C                   MOVEL     'A'           D53TFO
161000031215     C                   MOVE      SIMFEL        D53FEL
161100031215     C                   Z-ADD     brvNFV        D53NFV
161200031215     C                   MOVE      brvfgs        D53FGS
161300031215     C                   MOVEL     'A'           D53ABB
161400031215     C                   MOVE      brvLNA        D53LNA
161500031215     C**                 MOVEL     ARBTFA        D53TFA
161600031215     C                   CALL      'FNLV53R'
161700031215     C                   PARM                    DSLV53
161800031215     C*
161900031215     C* SE NON ci sono errori e foglio non abbinato memorizzo
162000031215     C* VERIFICO SE E' GIA' STATO PULITO
162100031215    1C     D53ERR        IFeq      *BLANKS
162200031216    6C     d53NFA        andeq     0
162300950321     C                   ADD       1             II
162400950321     C                   Z-ADD     ARTNSC        NRS(II)
162500950321     C                   Z-ADD     BRVFGS        FGS(II)
162600950321     C                   Z-ADD     BRVNFV        NFV(II)
162700031215     C                   MOVE      d53DFV        G02INV
162800950321     C                   MOVE      *ZEROS        G02DAT
162900950321     C                   MOVEL     3             G02ERR
163000950321     C                   CALL      'XSRDA8'
163100950321     C                   PARM                    WLBDAT
163200950321     C                   MOVEL     G02DAT        DFV(II)
163300950807     C                   SETON                                        89
163400950807     C*
163500950807   X6C                   ELSE
163600070118     C     KBRV3         READE     FNBRV03L                               89
163700950807    6C                   ENDIF
163800950807     C*
163900950807    4C                   ENDDO
164000950807    3C                   ENDIF
164100950321     C*
164200090309     C  N10KARB1         READE     FiART01L                               10
164300950807    2C                   ENDDO
164400950807    1C                   END
164500920527     C*
164600920527     C                   ENDSR
164700941214     C**************************************************************************
164800941214     C* CARICO L6
164900941214     C**************************************************************************
165000920907     C     CARL6         BEGSR
165100941214     C*
165200020422     c                   clear                   dsul06
165300020422     c                   eval      d06cod = '£6'
165400020422     c                   movel     parges        d06key
165500020422     c                   movel     dsul06        kpjbu
165600020422     c                   call      'TRUL06R'
165700020422     c                   parm                    kpjba
165800020422     c                   movel     kpjbu         dsul06
165900020422     c                   movel     lne           l6
166000941214     C*
166100920907     C                   ENDSR
166200950119     C*
166300991105     C* CONVERTO IL C/A NELLA DIVISA CORRENTE -----------------------*
166400950119     C     CTRCAS        BEGSR
166500991105     C                   CLEAR                   WCAS
166600950119     C*
166700950119    1C     STPCAS        IFGT      0
166800991104     C*
166900011025    2C     STPVCA        IFNE      §GEDCN
167000011025      *
167100991105     C                   CLEAR                   YEUR
167200991105     C                   MOVEL     STPVCA        YECDVI
167300011025     C                   MOVEL     §GEDCN        YECDVO
167400011025     C                   MOVE      §CVDEC        YECDCO
167500991105     C                   Z-ADD     STPCAS        YECIMI
167600991105     C*
167700991105     C                   CALL      'YEURCO'
167800991105     C                   PARM                    YEUR
167900991105     C*
168000991105    3C     YECESI        IFEQ      ' '
168100991105     C                   Z-ADD     YECIMO        WCAS
168200991105    3C                   ENDIF
168300991105     C*
168400991105   X2C                   ELSE
168500991105     C                   Z-ADD     STPCAS        WCAS
168600991105    2C                   ENDIF
168700991105    1C                   ENDIF
168800950119     C*
168900950119     C                   ENDSR
169000950621     C*
169100960517     C* CONTROLLO SE BOLLA PRESENTE IN IMA ---------------------------*
169200950621     C     CTRIMA        BEGSR
169300960517     C                   SETON                                        70
169400960517     C*
169500090309     C     KARB1         CHAIN     FiART000                           10
169600960517     C*
169700950621     C     *IN10         DOWEQ     *OFF
169800960517     C* SE PRESENTE ALMENO UN COLLO IN IMA, STAMPO LA BOLLA
169900000203     C                   Z-ADD     FVVNFV        WNFVBR
170000070118     C     KBRV          SETLL     FNBRV001                               33
170100960517     C*
170200960517     C     *IN33         IFEQ      *ON
170300960517     C                   SETON                                        10
170400960517     C                   SETOFF                                       70
170500960517     C                   ELSE
170600090309     C     KARB1         READE     FiART000                               10
170700960517     C                   ENDIF
170800950621     C                   ENDDO
170900950621     C*
171000950621     C                   ENDSR
171100960321     C**************************************************************************
171200960321     C     *INZSR        BEGSR
171300960321     C**************************************************************************
171400960321     C*
171500960321     C     *ENTRY        PLIST
171600960321     C                   PARM                    KPJBA
171700960321     C                   MOVEL     KPJBU         PARAM
171800960321     C                   Z-ADD     1             CODUT
171900960321     C*---------------------------------------------------------------
172000960321     C     KARB          KLIST
172100960321     C                   KFLD                    LIN(I)
172200960321     C                   KFLD                    VIDZNC            2 0
172300960321     C*
172400960321     C     KARB1         KLIST
172500960321     C                   KFLD                    ARBAAS
172600960321     C                   KFLD                    ARBLNP
172700960321     C                   KFLD                    ARBNRS
172800960321     C                   KFLD                    ARBNSP
172900991105     C     KAR6          KLIST
173000991104     C                   KFLD                    ARBAAS
173100991104     C                   KFLD                    ARBLNP
173200991104     C                   KFLD                    ARBNRS
173300991104     C                   KFLD                    ARBNSP
173400991104     C                   KFLD                    KTRC
173500991104     C     KARI          KLIST
173600991104     C                   KFLD                    ARBAAS
173700991104     C                   KFLD                    ARBLNP
173800991104     C                   KFLD                    ARBNRS
173900991104     C                   KFLD                    ARBNSP
174000991104     C                   KFLD                    KTIP
174100960321     C*
174200960321     C     KARB6         KLIST
174300960321     C                   KFLD                    $LNP
174400960321     C                   KFLD                    $NFV
174500960321     C*
174600960321     C     KLBL          KLIST
174700960321     C                   KFLD                    LBLAAP
174800960321     C                   KFLD                    LBLLPP
174900960321     C                   KFLD                    LBLNRP
175000960321     C                   KFLD                    LBLNSP
175100960321     C*
175200960321     C     KARB2         KLIST
175300960321     C                   KFLD                    LBLAAN
175400960321     C                   KFLD                    LBLLPN
175500960321     C                   KFLD                    LBLNRN
175600960321     C                   KFLD                    LBLNSN
175700960321     C*
175800960321     C     KTAB          KLIST
175900960321     C                   KFLD                    CODUT
176000960321     C                   KFLD                    TBLCOD
176100960321     C                   KFLD                    TBLKEY
176200960321     C*
176300960321     C     KFVA1         KLIST
176400960321     C                   KFLD                    BRVFGS
176500000203     C                   KFLD                    WNFV
176600990121     C                   KFLD                    PARGES
176700960321     C*
176800960321     C     KFVV          KLIST
176900960321     C                   KFLD                    IMANPG
177000960321     C                   KFLD                    DATEU
177100970513     C                   KFLD                    WLNP
177200960321     C*
177300960321     C     KAR4          KLIST
177400960321     C                   KFLD                    ARBAAS
177500960321     C                   KFLD                    ARBLNP
177600960321     C                   KFLD                    ARBNRS
177700960321     C                   KFLD                    ARBNSP
177800960321     C                   KFLD                    KTRC
177900960321     C*
178000960321     C     KBRV          KLIST
178100960321     C                   KFLD                    FVVNPG
178200000203     C                   KFLD                    WNFVBR
178300960321     C                   KFLD                    FVVFGS
178400960321     C                   KFLD                    ARTFLS
178500960321     C                   KFLD                    ARTLNA
178600960321     C                   KFLD                    ARTNRS
178700960321     C                   KFLD                    ARTNSC
178800960321     C     KBRV3         KLIST
178900960321     C                   KFLD                    WNPG
179000960321     C                   KFLD                    ARTFLS
179100960321     C                   KFLD                    ARTLNA
179200960321     C                   KFLD                    ARTNRS
179300960321     C                   KFLD                    ARTNSC
179400990304     C     KART          KLIST
179500990304     C                   KFLD                    ARTFLS
179600990304     C                   KFLD                    ARTLNA
179700990304     C                   KFLD                    ARTNRS
179800990304     C                   KFLD                    ARTNSC
179900990324     C     KTBE2         KLIST
180000990324     C                   KFLD                    KCOD
180100990324     C                   KFLD                    KLIN
180200990324     C                   KFLD                    KSIF
180300990324     C                   KFLD                    KKE1
180400990324     C                   KFLD                    KKE2
180500990324     C     KTBE22        KLIST
180600990324     C                   KFLD                    KCOD
180700990324     C                   KFLD                    KLIN
180800990324     C                   KFLD                    KSIF
180900990324     C     KDCT          KLIST
181000990324     C                   KFLD                    DCDAAC
181100990324     C                   KFLD                    DCDFIL
181200990324     C                   KFLD                    DCDNCA
181300030826      *
181400030826     c     K04cbb01      klist
181500030826     c                   kfld                    ARBaas
181600030826     c                   kfld                    ARBlnp
181700030826     c                   kfld                    ARBnrs
181800030826     c                   kfld                    ARBnsp
181900040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
182000040809     C     KDCT1_C       KLIST
182100040809     C                   KFLD                    dsaac
182200040809     C                   KFLD                    dsfil
182300040809     C                   KFLD                    dsnca
182400960321     C*
182500960321     C                   Z-ADD     1             WNPG
182600960321     C                   Z-ADD     3             IMANPG
182700991104     C                   MOVEL     'A'           KTIP
182800960321     C*
182900960321     C*---------------------------------------------------------------
183000960321     C* CARICO LE VARIABILI PARTENZE PER EVIDENZIALE I PESI O VOLUMI
183100960321     C* GRANDI
183200960321     C                   MOVEL     '3I'          TBLCOD
183300960321     C                   MOVEL(P)  '1'           TBLKEY
183400960321     C     KTAB          CHAIN     TABEL                              11
183500960321     C  N11              MOVEL     TBLUNI        DS3I
183600960321     C   11              CLEAR                   DS3I
183700960321     C*
183800960321     C* PRENDO GG PULIZIA FILE FNFVA
183900960321     C                   MOVEL     '5A'          TBLCOD
184000960321     C                   MOVEL     '1       '    TBLKEY
184100960321     C     KTAB          CHAIN     TABEL00F                           30
184200960321     C  N30              MOVEL     TBLUNI        DS5A
184300960321     C   30              CLEAR                   DS5A
184400960321     C*---------------------------------------------------------------
184500960321     C* VERIFICO SE E' STATA RICHIESTA LA STAMPA DELLE SPEDIZIONI NON
184600960321     C* PARTITE
184700960321     C     PARNFV        IFGT      *ZEROS
184800960321     C                   SETON                                        77
184900960321     C                   MOVE      PARLNP        $LNP              3 0
185000960321     C                   MOVE      PARNFV        $NFV              5 0
185100960321     C* CARICO L6 DELLE FILIALI GESTITE
185200960321     C                   EXSR      CARL6
185300960321     C                   ELSE
185400960321     C                   MOVEL     LIP           LIN
185500960321     C                   END
185600960321      *
185700960321     C                   CALL      'X§PARUT'
185800960321     C                   PARM                    UT§DSE
185900960321     C                   MOVEL     RAGUT         RSUT             20
186000960321     C                   MOVEL     REC80         CNCR80
186100960321     C*  UDATE
186200960321     C                   TIME                    W0140            14 0
186300960321     C* UDATE IN GGMMAAAA
186400960321     C                   MOVE      W0140         WDTGIO            8 0
186500960321     C* ORA
186600960321     C                   MOVEL     W0140         TIMES             6 0
186700960321     C*
186800960321     C* UDATE IN AAAAMMGG
186900960321     C                   Z-ADD     WDTGIO        G02DAT
187000960321     C                   MOVEL     *BLANK        G02ERR
187100960321     C                   CALL      'XSRDA8'
187200960321     C                   PARM                    WLBDAT
187300960321     C                   MOVEL     G02INV        DATEU             8 0
187400960321     C*
187500960321     C*---------------------------------------------------------------
187600960321     C*
187700960321     C* IMPOSTO E APRO IL FILE DI STAMPA
187800960321     C                   MOVEL     KQOTD         PRINT
187900960730     C*
188000960730     C                   CLEAR                   W0030
188100020522     c     simtpp        ifeq      '2'
188200020522     c     simtpp        oreq      *blanks
188300020522     c                   MOVEL     simpou        W0030             3 0
188400960730     C                   ENDIF
188500960730     C*
188600960730     C* FACCIO L'OVRDBF SE RICHIESTO
188700960321     C     PAROUT        IFNE      *BLANKS
188800960730     C*
188900960730     C* SE NON SONO UN REM E NON STO LANCIANDO SIMFEL
189000020522     c     simtpp        ifne      '2'
189100020522     c     simtpp        andne     *blanks
189200960730     C     PARGES        ANDNE     SIMFEL
189300960730     C*
189400960730     C* SE     SONO UN REM E     STO LANCIANDO SIMFEL
189500020522     c     simtpp        oreq      '2'
189600960730     C     PARGES        ANDEQ     SIMFEL
189700020522     c     simtpp        oreq      *blanks
189800020522     C     PARGES        ANDEQ     SIMFEL
189900960730     C*
190000960730     C* SE     SONO UN REM MA NON STO LANCIANDO ME STESSO
190100020522     c     simtpp        oreq      '2'
190200960730     C     PARGES        ANDNE     W0030
190300020522     c     simtpp        oreq      *blanks
190400020522     C     PARGES        ANDNE     W0030
190500960730     C*
190600970707     C                   MOVEL     'T'           W004A             4
190700970707     C                   MOVE      PARGES        W004A
190800970707     C                   MOVEL(P)  W004A         TBLKEY
190900970707     C                   MOVEL     '5P'          TBLCOD
191000970707     C     KTAB          CHAIN     TABEL00F                           89
191100970707     C     *IN89         IFEQ      *OFF
191200970707     C                   MOVEL     TBLUNI        DS5P
191300970707     C                   MOVEL(P)  §5POTQ        PRINT            10            STAMPANTE
191400970707     C                   END
191500960321     C                   ENDIF
191600960730     C                   ENDIF
191700030826      *
191800030826if  1c                   if        PARfr = 'WRKF '
191900030826      *
192000030826     c                   open      WFCBB01L
192100030826      *
192200030826x   1c                   else
192300960730     C*
192400960321     C                   MOVEA     PRINT         CMD(49)                        OUTQ
192500960321     C*
192600960321     C     PARGES        IFNE      SIMFEL
192700960321     C     PARGES        ANDNE     *ZERO
192800971224     C                   MOVEL     PARGES        ALF3              3
192900960321     C                   MOVEA     ALF3          CMD(38)                        FORMTYPE
193000960321     C                   ENDIF
193100960321     C                   MOVEL     *BLANKS       COMMAN           80
193200960321     C                   MOVEA     CMD           COMMAN
193300960321     C                   Z-ADD     80            LUNG             15 5
193400960321     C                   CALL      'QCMDEXC'
193500960321     C                   PARM                    COMMAN
193600960321     C                   PARM                    LUNG
193700960510     C                   OPEN      PRTF198                                      PRINTER FILE
193800030826      *
193900030826e   1c                   endif
194000960321     C**
194100960321     C* CARICO LA £1
194200960321     C**
194300960321     C                   CLEAR                   DSUL06
194400960321     C                   MOVE      '£1'          D06COD
194500960321     C                   MOVEL     SIMFEL        D06KEY
194600960321     C                   MOVEL     'L'           D06TLA
194700960321     C                   MOVEL     DSUL06        KPJBU
194800960321     C*
194900960321     C                   CALL      'TRUL06R'
195000960321     C                   PARM                    KPJBA
195100960321     C                   MOVEL     KPJBU         DSUL06
195200960321     C                   MOVEA     LNE           L1
195300960321     C**
195400960321     C* CALCOLO UDATE - GG PULIZIA FVA
195500960321     C*
195600960321     C* PRENDO LA DATA DEL GIORNO DA TIME
195700960321     C                   TIME                    W0140            14 0
195800960321     C                   MOVE      W0140         UDATE8            8 0
195900960321SC   C* CON XSRDA8 TRASFORMO LA DATA ODIERNA IN NR.GIORNI (G02TGI)
196000960321     C                   MOVE      UDATE8        G02DAT
196100960321     C                   MOVEL     *BLANK        G02ERR
196200960321     C                   CALL      'XSRDA8'
196300960321     C                   PARM                    WLBDAT
196400960321     C*
196500960321      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
196600960321     C                   MOVE      *BLANKS       $DAT
196700960321     C                   MOVE      *BLANKS       $ERR
196800960321     C                   MOVE      *BLANKS       $PGM             10
196900960321     C                   MOVEL     'FNLRA7R'     $PGM                           BLP-FS61
197000960321     C                   CALL      'TRUL49C'                            99
197100960321     C                   PARM                    $PGM
197200000103     C                   PARM                    $DAT              8
197300960321     C                   PARM                    $ERR              1
197400960321      *
197500960321      * PROGRAMMA IN USO DATA ULTIMO UTILIZZO = DATA DEL GIORNO
197600960321     C     $ERR          IFNE      '1'
197700960321     C     *IN99         ANDLE     '1'
197800960321     C     $DAT          ANDNE     *BLANKS
197900960321     C                   MOVE      $DAT          G02INV
198000960321     C                   ENDIF
198100960321     C*
198200960321     C                   Z-ADD     *ZEROS        G02DAT
198300960321     C                   Z-ADD     *ZEROS        G02TGI
198400960321     C                   MOVEL     '3'           G02ERR
198500960321     C                   CALL      'XSRDA8'
198600960321     C                   PARM                    WLBDAT
198700960321      *
198800960321     C*
198900960321     C* SOTTRAGGO I GIORNI PULIZIA FOGLI VIAGGIO PARTENZA IN ARRIVO
199000960321     C     G02TGI        SUB       §5AFVA        GIOTGI
199100960321     C*
199200960321     C                   CALL      'XSRGI8'
199300960321     C                   PARM                    WGIDAT
199400960321     C*
199500960321     C                   Z-ADD     GIOINV        COMFVA            8 0
199600960321     C*
199700960516     C* GIRO LA DATA CONS.RICHIESTA E LA DATA FOGLIO ARRIVI RICHIESTE
199800960516     C*  RICHIESTE A VIDEO
199900960516     C     PARDFA        IFGT      0
200000960516     C                   SETON                                        17
200100960516     C                   Z-ADD     PARDFA        G02INV                         AAAA/MM/GG
200200960516     C                   Z-ADD     *ZERO         G02DAT
200300960516     C                   MOVEL     '3'           G02ERR
200400960516     C                   CALL      'XSRDA8'
200500960516     C                   PARM                    WLBDAT
200600960516     C                   Z-ADD     G02DAT        STPDFA                         GG/MM/AAAA
200700960516     C                   ENDIF
200800960516     C*
200900960516     C     PARDAT        IFGT      0
201000960517     C     PARDAT        ANDNE     99991231
201100960516     C                   SETON                                        18
201200960516     C                   Z-ADD     PARDAT        G02INV                         AAAA/MM/GG
201300960516     C                   Z-ADD     *ZERO         G02DAT
201400960516     C                   MOVEL     '3'           G02ERR
201500960516     C                   CALL      'XSRDA8'
201600960516     C                   PARM                    WLBDAT
201700960516     C                   Z-ADD     G02DAT        STPDAT                         GG/MM/AAAA
201800960516     C                   ENDIF
201900990324     C* SE ELENCO BOLLE DA CONSEGNARE CARICO LE CAUS.CHIUSURE C.A. CHE
202000990324     C*  CHE NON DEVO STAMPARE SE SOLO LORO NON CONSEGNATI
202100990324     C     *IN77         IFEQ      *OFF
202200990324     C                   CLEAR                   KCOD
202300990324     C                   CLEAR                   KLIN
202400990324     C                   MOVEL     KNSIF         KSIF
202500990324     C                   MOVEL     *ALL'0'       KKE1
202600990324     C                   MOVE      'CCH'         KKE1
202700990324     C                   CLEAR                   KKE2
202800990324     C     KTBE2         CHAIN     TNTBE02L                           89
202900990324     C     *IN89         IFEQ      *ON
203000990324     C     TBEATB        ORNE      ' '
203100990324     C                   CLEAR                   KSIF
203200990324     C                   ENDIF
203300990324     C                   MOVEL     'CCH'         KCOD
203400990324     C                   Z-ADD     1             XX                3 0
203500990324     C     KTBE22        CHAIN     TNTBE02L                           89
203600990324     C     *IN89         DOWEQ     *OFF
203700990324     C     TBEATB        IFEQ      ' '
203800990324     C                   MOVEL     TBEUNI        DCCH
203900990324     C     §CHR52        IFEQ      'S'
204000990324     C                   MOVEL     TBEKE1        CCH(XX)
204100990324     C                   ADD       1             XX
204200990324     C                   ENDIF
204300990324     C                   ENDIF
204400990324     C     KTBE22        READE     TNTBE02L                               89
204500990324     C                   ENDDO
204600990324     C                   ENDIF
204700991104     C**
204800991104     C* CARICO DIVISA CORRENTE
204900991104     C**
205000991104     C                   CLEAR                   DSBS02
205100991104     C                   MOVEL     'C'           T02MOD
205200991104     C                   MOVEL     KNSIF         T02SIF
205300991104     C                   MOVEL     'GED'         T02COD
205400991104     C                   MOVEL(P)  1             T02KE1
205500991104     C                   CALL      'TIBS02R'
205600991104     C                   PARM                    KPJBA
205700991104     C                   PARM                    DSBS02
205800991104    2C     T02ERR        IFEQ      ' '
205900991104     C                   MOVEL     T02UNI        DGED
206000991104     C                   ELSE
206100991104     C                   CLEAR                   DGED
206200991104     C                   ENDIF
206300040826     c**
206400040826     c* carico data partenza ultima fase poste
206500040826     c**
206600040826     C                   CLEAR                   DSBS02
206700040826     c                   clear                   dvpoppt
206800040826     C                   MOVEL     'C'           T02MOD
206900040826     C                   MOVEL     KNSIF         T02SIF
207000040826     C                   MOVEL     'VPO'         T02COD
207100040826     c                   movel(P)  'PPT'         t02ke1
207200040826     C                   CALL      'TIBS02R'
207300040826     C                   PARM                    KPJBA
207400040826     C                   PARM                    DSBS02
207500040826    2C                   IF        T02ERR = *BLANKS
207600040826     C                   MOVEL     T02uni        dvpoppt
207700040826    2C                   ENDIF
207800011029      *
207900011029      * Recupero il numero di decimali di §GEDCN
208000011029     C                   MOVEL     'CV'          TBLCOD
208100011029     C                   MOVEL     *BLANKS       TBLKEY
208200011029     C                   MOVEL     §GEDCN        TBLKEY
208300011029     C     KTAB          CHAIN     TABEL                              31
208400011029     C  N31              MOVEL     TBLUNI        DSCV
208500011029     C   31              CLEAR                   DSCV
208600960321     C                   ENDSR
208700990304     C**************************************************************************
208800990304     C* ANALIZZO LA C.A. CHIUSA
208900990304     C**************************************************************************
209000990304     C     CACHIU        BEGSR
209100990304     C                   CLEAR                   DCCH
209200990304     C                   CLEAR                   WNOCON
209300990324     C*
209400090309     C     KARB1         SETLL     FiART01L
209500090309     C     KARB1         READE     FiART01L                               10
209600990324    1C     *IN10         DOWEQ     *OFF
209700990324    2C     ARTDCM        IFEQ      0
209800990304     C*
209900990304     C* VERIFICO SE E' SU C.A.
210000990304     C     KART          CHAIN     FNDCD02L                           32
210100990304     C*
210200990304     C* SE DETT.CA ANNULLATO O APERTO, IGNORO COME SE FOSSE CHIUSO
210300990304     C*  ALTRIMENTI GUARDO LA SUA CAUSALE CHIUSURA
210400990324    3C     DCDDCH        IFGT      0
210500990304     C     DCDATB        ANDEQ     ' '
210600990304     C                   CLEAR                   DCCH
210700990304     C*
210800990324     C                   SETOFF                                       89
210900990324    4C     DCDCCH        IFNE      *BLANKS
211000990324     C     DCDCCH        LOOKUP    CCH                                    89
211100990324    4C                   ENDIF
211200990304     C**
211300990304     C** COME SE NON FOSSE CONSEGNATO
211400990324    4C     *IN89         IFEQ      *OFF
211500990304     C                   MOVEL     'S'           WNOCON            1
211600990304     C                   SETON                                        10
211700990324   X4C                   ELSE
211800990324     C* VERIFICO IL TIPO ANOMALIA SE E' MANCANZA O AVARIE RESE O DISTR
211900990324     C* OK --> ALTRIMENTI STAMPO COME NON CONSEGNATA
212000990324     C     KDCT          CHAIN     FNDCT01L                           89
212100050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
212200050221     c                   If        Not *In89 and dctdt2 > 0
212300050221     c                   Eval      *In89 = *On
212400050221     c                   EndIf
212500990324    5C     *IN89         IFEQ      *OFF
212600990324     C     DCTATB        ANDEQ     ' '
212700990324     C                   CLEAR                   DSBS02
212800990324     C                   MOVEL     'C'           T02MOD
212900990324     C                   MOVEL     KNSIF         T02SIF
213000990324     C                   MOVEL     'TAD'         T02COD
213100990324     C                   MOVEL     DCTTAD        T02KE1
213200990324     C                   CALL      'TIBS02R'
213300990324     C                   PARM                    KPJBA
213400990324     C                   PARM                    DSBS02
213500990324     C**
213600990324    6C     T02ERR        IFEQ      ' '
213700990324     C                   MOVEL     T02UNI        DTAD
213800990324     C                   ELSE
213900990324     C                   MOVEL     'A'           §TARGR
214000990324    6C                   ENDIF
214100990324     C*
214200990324     C* SE LA C.A. E' AVARIA NON RESA NE DISTRUTTA O AMMANCO
214300990324     C*  DEVO STAMPARE COME NON CONSEGNATA
214400990324    6C     §TARGR        IFEQ      'V'
214500990324     C     DCTDSD        ANDNE     'R'
214600990324     C     DCTDSD        ANDNE     'D'
214700990324     C     §TARGR        OREQ      'A'
214800990324     C                   MOVEL     'S'           WNOCON            1
214900990324     C                   SETON                                        10
215000990324    6C                   ENDIF
215100990324     C*
215200990324     C* NON TROVATA TESTATA --> STAMPO
215300990324   X5C                   ELSE
215400990324     C                   MOVEL     'S'           WNOCON
215500990324     C                   SETON                                        10
215600990324    5C                   ENDIF
215700990324    4C                   ENDIF
215800990304     C**
215900990324   X3C                   ELSE
216000990304     C                   MOVEL     'S'           WNOCON
216100990324     C                   SETON                                        10
216200990324    3C                   ENDIF
216300990324    2C                   ENDIF
216400990304     C*
216500090309     C  N10KARB1         READE     FiART01L                               10
216600990324    1C                   ENDDO
216700990304     C                   ENDSR
216800030826      *****************************************************************
216900030826      * WriteWFCBB - Scrittura del record nel file di lavoro WFCBB    *
217000030826      *****************************************************************
217100030826     c     WriteWFCBB    BegSr
217200030826      *
217300030826     c     K04CBB01      chain     WFCBB000
217400030826      *
217500030826if  1c                   if        NOT %found(WFCBB01L)
217600030826     c                   clear                   WFCBB000
217700030826     c                   eval      CBBaas = ARBaas
217800030826     c                   eval      CBBlnp = ARBlnp
217900030826     c                   eval      CBBnrs = ARBnrs
218000030826     c                   eval      CBBnsp = ARBnsp
218100030917     c                   eval      CBBmgs = ARBmgs
218200030917     c                   eval      CBBncl = ARBncl
218300030917     c                   eval      CBBpkf = ARBpkf
218400030917     c                   eval      CBBvlf = ARBvlf
218500030917     c                   eval      CBBrsd = ARBrsd
218600030917     c                   eval      CBBind = ARBind
218700030917     c                   eval      CBBcad = ARBcad
218800030917     c                   eval      CBBlod = ARBlod
218900030917     c                   eval      CBBprd = ARBprd
219000030917     c                   eval      CBBnzd = ARBnzd
219100031001     c                   eval      CBBtcr = ARBtcr
219200031001     c                   eval      CBBdcr = ARBdcr
219300030826e   1c                   endif
219400030826      *
219500030826if  1c                   if        wErr   > *zeros
219600030826     c                   move      wErr          CBBerr
219700030826     c                   movel(p)  Msg(wErr)     CBBmsg
219800030826e   1c                   endif
219900030826      *
220000030826if  1c                   if        NOT %found(WFCBB01L)
220100030826     c                   WRITE     WFCBB000
220200030826x   1c                   else
220300030826     c                   UPDATE    WFCBB000
220400030826e   1c                   endif
220500030826      *
220600030826     c                   EndSr
220700030826      *****************************************************************
220800960510     OPRTF198   E            TESTA            02
220900960515     O              N77      STT(1)             132
221000960515     O              N77      STT(2)             198
221100960515     O               77      STT(14)            132
221200960515     O               77      STT(15)            198
221300911025     O                       RSUT                20
221400920318     O                       SIMFEL              30
221500960515     O                       WDTGIO             180 '  /  /    '
221600960515     O                       PAGE          Z    198
221700960510     O          E            TESTA            03
221800941215     O                       KNSIF               10
221900941215     O                       KNMUS               21
222000960515     O               77                         102 'FOGLIO VIAGGIO:'
222100920318     O               77      $LNP              +  1
222200920318     O               77                        +  0 '/'
222300920318     O               77      $NFV          Z   +  0
222400960515     O                       TIMES              178 '  :  :  '
222500960510     O          E            TESTA            04
222600911209     O               20      STT(11)             66
222700911209     O               30      STT(12)             66
222800911209     O               40      STT(13)             66
222900960516RM*  O               45      CINC                84
223000960516RM*  O               46      CNINC               84
223100960516     O               19      CIMA               114
223200960516     O               17                         140 'FINO A DATA F.ARRIVI:'
223300960516     O               17      STPDFA             150 '  /  /    '
223400960516     O               18                         177 'FINO A DATA CONS.RICH.:'
223500960516     O               18      STPDAT             187 '  /  /    '
223600000626     O                       POSTE$             198
223700000803     O          E            TESTA            05
223800000803     O                       ALTRP1              18
223900000803     O                       ALTRP2              37
224000021205     o                       AltRp3             + 6
224100960516     O*
224200000803     O          E            TESTA            07
224300911025     O                       STT(3)              66
224400911025     O                       STT(4)             132
224500960515     O                       STT(5)             198
224600000803     O          E            TESTA            08
224700960515     O                       STT(6)              66
224800960515     O                       STT(7)             132
224900960515     O                       STT(8)             198
225000000803     O          E            TESTA            09
225100911025     O                       STT(9)              66
225200911025     O                       STT(10)            132
225300960515     O                       STT(10)            198
225400941215     O*-------------------------------------------------------------------------
225500960515     O* 1° RIGA
225600911025     O          E            DETT        1
225700941216     O                       ARBLNA               3
225800960515     O                                            4 '/'
225900960515     O                       ARBZNC               7
226000960515     O                       ARBNRS        Z     11
226100010330     O              N95N97   ARBNCD        Z     19
226200010330     O              N95N97                       20 '/'
226300010330     O              N95N97   ARBNCA        Z     27
226400960515     O               98                          27 'SOLO INCASSO'
226500010330     O               97                          27 'RECUPERO    '
226600960515     O                       ARBNCL        Z     33
226700960515     O                       ARBPKF        4     42
226800960515     O               44                          43 '*'
226900960515     O                       ARBVLF        4     50
227000960515     O               44                          51 '*'
227100960515     O                       STPRSD              67
227200030320     O                       ARBCAD              77
227300030320     O                       ARBPRD              80
227400030320     O                       ARBNZD              84
227500030320     O                       ARBGC1              87
227600030320     O                       ARBGC2              90
227700030320     O                       VIDCBL              93
227800051214     O                       STPtcr              95
227900051214     O                       STPGMC             101 '  /  '
228000051214     O                       STPHCR             107 '  :  '
228100051214     o               12                         109 'N'
228200960515     O                       ARBTSP             111
228300991105     O                       STPRSM             128
228400991105     O                       ARBLNP             133
228500991105     O                       ARBNRS        Z    136
228600991105     O                       ARBNSP        Z    144
228700991105     O                       STPDSP             153 '  /  /  '
228800991105     O               31                         156 'AB'
228900991105     O                       STPTIC         B   160
229000991105     O                       STPCAS        2B   178
229100991105     O                       STPVCA         B   182
229200991105     O                  01   STPIMD        2B   194
229300991105     O                 N01   STPIM0        2B   194
229400991105     O                       STPDIV             198
229500960515     O* RIBATTITURA RIGA
229600960515     O          E    44      DETT        0
229700960515     O                       ARBPKF        4     42
229800960515     O                                           43 '*'
229900960515     O                       ARBVLF        4     50
230000960515     O                                           51 '*'
230100960515     O* 2° RIGA
230200960515     O          E            DETT1       1
230300990324     O               05                             'C.A.'
230400990324     O               05      DCTFIL               8
230500990324     O               05      DCTNCA        Z     15
230600990324     O               05                          19 'del'
230700990324     O               05      STADCA              28 '  /  /  '
230800990324     O               05    06                    35 'chiusa'
230900990324     O               05    06STACHC              44 '  /  /  '
231000990324     O               05N96 06DCTCCH            +  1
231100990324     O               05N96 06                  +  0 '-'
231200990324     O               05N96 06STACC2            +  0
231300990324     O               96                          54 'ANOMALIE'
231400990324     O              N98 94                       63 ' PARZIALE'
231500990127     O                     16                    74 'Cons.Part.'
231600990127     O                       CONSE1             100
231700990127     O                       CONSE2             126
231800990127     O                       NOTE1              162
231900960517     O                       NOTE2              198
232000990127     O          E            DETCA       1
232100990127     O                                              'C.A.'
232200990127     O                       DCTFIL               8
232300990127     O                       DCTNCA        Z     15
232400990127     O                                           19 'del'
232500990127     O                       STADCA              28 '  /  /  '
232600990127     O                     06                    35 'chiusa'
232700990127     O                     06STACHC              44 '  /  /  '
232800990304     O                     06DCTCCH            +  1
232900990304     O                     06                  +  0 '-'
233000990304     O                     06STACCH            +  0
233100990127     O*-------------------------------------------------------------------------
233200941215     O          E            DETNSC      1
233300941215     O                       NSC                127 '       /'
233400941219     O*-------------------------------------------------------------------------
233500941219     O          E            MESSAG      1
233600950808     O                       STPMSG              66
233700941221     O          E            MESSAG      0  0
233800950808     O                       STPMSG              66
233900950808     O          E            MESSG2      0  0
234000950808     O                       STPMSG             132
234100950808     O          E            MESSG2      0  0
234200950808     O                       STPMSG             132
234300950321     O          E            DETSC       1
234400950322     O                                           13 'Segnacollo n.'
234500950321     O                       NRS(II)       Z   +  1
234600950321     O                                         +  2 'F.V.P.'
234700950321     O                       FGS(II)           +  1
234800950321     O                                         +  0 '/'
234900950321     O                       NFV(II)       Z   +  0
235000950322     O                                         +  2 'del'
235100950321     O                       DFV(II)       8   +  1
235200950322     O          E            DETSC1      0
235300950322     O                                           75 'Segnacollo n.'
235400950322     O                       NRS(II)       Z   +  1
235500950322     O                                         +  2 'F.V.P.'
235600950322     O                       FGS(II)           +  1
235700950322     O                                         +  0 '/'
235800950322     O                       NFV(II)       Z   +  0
235900950322     O                                         +  2 'del'
236000950322     O                       DFV(II)       8   +  1
236100950808     O          E            VUOTA       1
236200941215     O*-------------------------------------------------------------------------
236300911212     O          E            TOTALE      2
236400911025     O                       STT(9)              66
236500911025     O                       STT(10)            132
236600960515     O                       STT(10)            198
236700911212     O          E            TOTALE      1
236800941216     O                                           19 'TOTALI SPEDIZIONI :'
236900941216     O                       V06TSP        Z     25
237000991105     O                                           40 'COLLI :'
237100991105     O                       STPTCO        Z     47
237200991105     O                                           60 'PESO :'
237300991105     O                       V06PKF        4     70
237400991105     O                                           84 'VOLUME :'
237500991105     O                       V06VOL        4     93
237600991105     O                                          112 'DA INCASSARE :'
237700991105     O                       STPTOC        2    134
237800011025     O                       §GEDCN             139
237900941215     O*-------------------------------------------------------------------------
238000920527     O          E            NOAP1       1
238100950322     O                                           19 'Colli non abilitati'
238200920527     O                                         +  1 'dalla partenza:'
238300950322     O              N34      SP1                132 '        '
238400950322     O               34                          42 'TUTTI'
238500950322     O*-------------------------------------------------------------------------
238600950322     O          E            NOAP1X      1
238700950322     O                                           19 'Abilitaz.forz.ma co'
238800950322     O                                         +  0 'lli non partiti:'
238900950322     O              N34      SP1                132 '        '
239000950322     O               34                          42 'TUTTI'
239100941215     O*-------------------------------------------------------------------------
239200920527     O          E            NOAP2       1
239300930302     O              N34      SP2                132 '        '
239400941215     O*-------------------------------------------------------------------------
239500920527     O          E            NOAA1       1
239600950322     O                                           19 'Colli non abilitati'
239700920528     O                                         +  1 'all'' arrivo:'
239800950322     O              N35      SA1                132 '        '
239900950322     O               35                          42 'TUTTI'
240000941215     O*-------------------------------------------------------------------------
240100920527     O          E            NOAA2       1
240200930302     O              N35      SA2                132 '        '
240300941215     O*-------------------------------------------------------------------------
240400950808** STT
240500960515***   E L E N C O    B O L L E    D A    C O N S E G N A R E   ***
240600960515                        FNLR52R                           PAG.
240700030320LNA/ZONA SER   SEGNACOLLI   COLLI     PESO  VOLUME  DESTINATARIO
240800051214  C.A.P.    PR NAZ  GG   TBO  CONSEGNA  STA TP MITTENTE          S
240900991105 P E D I Z I O N E        TIPO / IMP.CONTRASSEGNO   IMP. FATTURA
241000991105
241100051214                   CHIUS      RICHIESTA DDT SER                 LN
241200991105P SER NUMERO   DATA            DA  INCASSARE        DA INCASSARE
241300911025------------------------------------------------------------------
241400911025------------------------------------------------------------------
241500960516RICHIESTE LE SPEDIZIONI ABILITATE ALLA CONSEGNA
241600960516RICHIESTE LE SPEDIZIONI NON ABILITATE ALLA CONSEGNA
241700960516RICHIESTE LE SPEDIZIONI ABILITATE E NON ALLA CONSEGNA
241800960515***  L I S T A   S P E D I Z I O N I   N O N   P A R T I T E  ***
241900960515                        FNLR52R                          PAG.
242000941215**  5678901234567890123456789012345678901234567890123456789012345678901234567890
242100960510OVRPRTF FILE(PRTF198) FORMTYPE('*STD    ') OUTQ(          ) USRDTA('Lista cons')
242200941219**
242300950322Colli partiti ma spedizione non abilitata
242400950808Colli partiti SU F.V.P. non abbinati
