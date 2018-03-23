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
004800941215     FFNART01L  IF   E           K DISK
004900991105     FFIARI01L  IF   E           K DISK
005000941214     FFNFVA01L  IF   E           K DISK
005100950621     FFNFVV02L  IF   E           K DISK
005200941214     FFNLBL01L  IF   E           K DISK
005300911106     FTABEL00F  IF   E           K DISK
005400040809     F**""*FNDCT02L  IF   E           K DISK
005500990324     FFNDCT01L  IF   E           K DISK
005600990324     F                                     RENAME(FNDCT000:FNDCT001)
005700990304     FFNDCD02L  IF   E           K DISK
005800140618     FFNBRV05L  IF   E           K DISK
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
008500960321     D*
008600020522     D LIN             S              3  0 DIM(29)                              LINEE ARRIVI
008700960516     D STT             S             66    DIM(15) CTDATA PERRCD(1)
008800941215     D NSC             S              7  0 DIM(13)                              SEGNACOLLI
008900920527     D SP1             S              7  0 DIM(12)                              NO ABIL. PARTENZA
009000920527     D SP2             S              7  0 DIM(12)                              NO ABIL. PARTENZA
009100920527     D SA1             S              7  0 DIM(12)                              NO ABIL. ARRIVO
009200920527     D SA2             S              7  0 DIM(12)                              NO ABIL. ARRIVO
009300920907     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
009400920907     D FIL             S              3    DIM(29)                              FIL. COMODO
009500990324     D CCH             S              2    DIM(50)                              CAUS.CHIUS.C.A
009600921130     D*
009700941215     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
009800950808     D MSG             S             66    DIM(2) CTDATA PERRCD(1)              MESSAGI IN STAMPA
009900950321     D*
010000950322     D NRS             S              7  0 DIM(999)                             N.SEGNACOLLO
010100950322     D FGS             S              3  0 DIM(999)                             FILIALE GEST.
010200000203     D NFV             S              6  0 DIM(999)                             FOGLIO VIAGGIO
010300950322     D DFV             S              8  0 DIM(999)                             DATA F.V.
010400911024     D KPJBA         E DS
010500911028     D PARAM           DS
010600911031     D  LIP                    1     87
010700911031     D                                     DIM(29)                              LINEE ARRIVI
010800911104     D  ZNC                   88    107
010900911104     D                                     DIM(10)                              ZONE VIDEO BUO
011000941216     D  PARDAT               108    115  0
011100941216     D  PARABI               116    117
011200950621     D  PARLNP               118    120  0
011300941216     D  PARNFV               121    125
011400941216     D  PARGES               126    128  0
011500950215     D  PAROUT               129    129
011600950621     D  PARINC               130    131
011700950621     D  PARIMA               132    133
011800960320     D* DATA F. ARRIVI DI ABBINAMENTO IN AAAAMMGG
011900960320     D  PARDFA               134    141  0
012000970516     D* NUMERO FIL GESTIONE FOGLIO IMA
012100970516     D  PARFIA               142    146  0
012200970516     D  PARFGS               147    149  0
012300000626     D* SI/NO/SOLO POSTE   S/N/P
012400121211     D**  PARSNP               150    150
012500000809     D* TUTTE, SOLO SI, SOLO NO ASSEGNATO
012600000803     D  PARASS               151    152
012700000803     D* TUTTE, SOLO SI, SOLO NO RESI
012800000803     D  PARRES               153    154
012900021205     D* Tutte, solo SI, solo NO - Bolle Bloccate
013000021205     D  PARblo               155    156
013100030826     d* Funzione richiesta: *blank = stampa, "WRKF " = creaz. file di lavoro
013200030826     d  PARfr                252    256
013300911024      *
013400911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013500950119     D DSCV          E DS
013600950119     D DS3A          E DS
013700920311     D DS1P          E DS
013800920527     D DS3I          E DS
013900960321     D DS5A          E DS
014000970707     D DS5P          E DS
014100991104     D DGED          E DS
014200971224     D DSLV53        E DS                  EXTNAME(FNLV53DS)
014300920318     D CNCR80        E DS
014400000323     D DSBL4A        E DS
014500000626     D OG143         E DS
014600990304     D DCCH            DS
014700990304     D  §CHDES                 1     50
014800990304     D  §CHR52                54     54
014900990324     D DTAD            DS
015000990324     D  §TARGR                26     26
015100990324     D  §TARIT                82     82
015200960321     D* DS PER TRUL06R - CARICAMENTO £X
015300960321     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015400960321     D  LNE                    1     90  0
015500960321     D                                     DIM(30)                              FIL. COMODO
015600990304     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015700051214     D fnlv80ds      E DS
015800991104     D YEUR          E DS                  EXTNAME(YEURCODS)
015900990127     D                 DS
016000990127     D  DCTDCA                 1      8  0
016100990127     D  DCTAAC                 1      4  0
016200990127     D  DCTMGC                 5      8  0
016300941216     D* DATA SPEDIZIONE BOLLA
016400960321     D                 DS
016500941216     D  ARBDSP                 1      8  0
016600941216     D  ARBAAS                 1      4  0
016700941216     D  ARBMGS                 5      8  0
016800941216     D WLBDAT          DS
016900941216     D  G02DAT                 1      8  0
017000941216     D  G02INV                 9     16  0
017100941216     D  G02ERR                17     17
017200941216     D  G02TGI                18     22  0
017300960321     D WGIDAT          DS
017400960321     D  GIODAT                 1      8  0
017500960321     D  GIOINV                 9     16  0
017600960321     D  GIOTGI                17     21  0
017700040809
017800040809      * ds per il controllo della presenza di CA per la spedizione richiesta
017900040809     d FIDN12DS      e ds
018000040809     d  skKey                 26    305    dim(20)
018100040809     d  skAnn                306    325    dim(20)
018200040809     d  skDca                326    485  0 dim(20)
018300040809     d  skFca                486    545  0 dim(20)
018400040809     d  skDch                546    705  0 dim(20)
018500040809     d  skCch                706    745    dim(20)
018600040809     d*
018700040809     d dsKey           ds
018800040809     d  dsaac                         4  0
018900040809     d  dsfil                         3  0
019000040809     d  dsnca                         7  0
019100040809
019200030826      *
019300030826      * Definizione variabili
019400030826      *
019500030826     d WCAS            s                   like(AR9CAS) inz
019600030826     d WIFT            s                   like(AR6IFT) inz
019700030826     d WIPP            s                   like(ARIIPP) inz
019800030826     d WCONVE          s                   like(ARIIPP) inz
019900030826     d KTIP            s                   like(ARITIP) inz
020000030826     d STPDIV          s                   like(AR6DIV) inz
020100030826     d WLNP            s                   like(FVVFGS) inz
020200030826     d KTRC            s                   like(AR4TRC) inz
020300030826     d STPTIC          s                   like(AR9TIC) inz
020400030826     d STPCAS          s                   like(AR9CAS) inz
020500030826     d STPVCA          s                   like(AR9VCA) inz
020600030826     d STPCMB          s                   like(AR9CMB) inz
020700030826     d WNPG            s                   like(BRVNPG) inz
020800030826     d IMANPG          s                   like(BRVNPG) inz
020900030826     d WNFA            s                   like(FVANFA) inz
021000030826     d WDFA            s                   like(FVADFA) inz
021100030826     d STPHCR          s                   like(ARBHCR) inz
021200030826     d STPDFA          s                   like(PARDFA) inz
021300030826     d STPDAT          s                   like(PARDAT) inz
021400030826     d KCOD            s                   like(TBECOD) inz
021500030826     d KLIN            s                   like(TBELIN) inz
021600030826     d KSIF            s                   like(TBESIF) inz
021700030826     d KKE1            s                   like(TBEKE1) inz
021800030826     d KKE2            s                   like(TBEKE2) inz
021900030826     d WNFV            s                   like(FVANFV) inz
022000030826     d WNFVBR          s                   like(BRVNFV) inz
022100030826     d wErr            s              3  0 inz
022200960516     D*
022300960516     D* DEFINIZIONE COSTANTI
022400960516     D CINC            C                   CONST('-SPEDIZIONI SOLO INC-
022500960516     D                                     ASSO-')
022600960516     D CNINC           C                   CONST('-SPEDIZIONI SOLO NON-
022700960516     D                                      INCASSO-')
022800960516     D CIMA            C                   CONST('-SPEDIZIONI PRESENTI-
022900960516     D                                      IN IMA-')
023000000803     D ALT1            C                   CONST('SOLO NON ASSEGNATI')
023100000803     D ALT2            C                   CONST('SOLO ASSEGNATI    ')
023200000803     D ALT3            C                   CONST('SOLO NON RESI')
023300000803     D ALT4            C                   CONST('SOLO RESI')
023400021205     D Alt5            c                   const('SOLO BOLLE NON BLOCCATE')
023500021205     D Alt6            c                   const('SOLO BOLLE BLOCCATE')
023600950626     C*---------------------------------------------------------------
023700941215     C*
023800960321     C* Lettura dei files FNARB00 per linea e zona inserite
023900960321     C* nelle tabelle già impostate
024000960321     C* elaborando soltanto le bolle di FV abbinati.
024100940620     C*
024200911209     C* CONTROLLO SE VIENE RICHIESTA LA STAMPA DELLE SPEDIZIONI ABILITA
024300911209     C* TE O NON OPPURE TUTTE
024400911209     C*
024500940620     C* 20 ON - SOLO BOLLE ABILITATE
024600950626    0C     *IN77         IFEQ      *OFF
024700950626     C*
024800950626    1C     PARABI        IFEQ      'SI'
024900911209     C                   SETON                                        20
025000911209     C*
025100911209     C                   ELSE
025200940620     C* 30 ON - SOLO NON ABILITATE
025300941216    2C     PARABI        IFEQ      'NO'
025400911209     C                   SETON                                        30
025500911209     C                   ELSE
025600940620     C* 40 ON - TUTTE
025700911209     C                   SETON                                        40
025800940620    2C                   ENDIF
025900940620    1C                   ENDIF
026000950315RM*  C*
026100950315RM*  C     PARINC        COMP      'SI'                                   45
026200950315RM*  C  N45PARINC        COMP      'NO'                                   46
026300000626     C* SI/NO/SOLO POSTE (PARAMETRO IN STAMPA)
026400121211     C**   PARSNP        IFEQ      'N'
026500121211     C**                 MOVE      'NO POSTE'    POSTE$           10
026600121211     C**                 ENDIF
026700121211     C**   PARSNP        IFEQ      'S'
026800121211     C**                 MOVE      'SI POSTE'    POSTE$
026900121211     C**                 ENDIF
027000121211     C**   PARSNP        IFEQ      'P'
027100121211     C**   'SOLO'        CAT       'POSTE':1     POSTE$
027200121211     C**                 ENDIF
027300000803     C* ASSEGNATI/RESI
027400000803     C     PARASS        IFEQ      'SI'
027500000803     C                   MOVEL     ALT2          ALTRP1           18
027600000803     C                   ENDIF
027700000803     C     PARASS        IFEQ      'NO'
027800000803     C                   MOVEL     ALT1          ALTRP1
027900000803     C                   ENDIF
028000000803     C     PARRES        IFEQ      'SI'
028100000803     C                   MOVEL     ALT4          ALTRP2           13
028200000803     C                   ENDIF
028300000803     C     PARRES        IFEQ      'NO'
028400000803     C                   MOVEL     ALT3          ALTRP2
028500000803     C                   ENDIF
028600021205      * Bloccati
028700021205     C                   select
028800021205     C                   when      PARblo = 'NO'
028900021205     C                   movel     Alt5          AltRp3           23
029000021205     C                   when      PARblo = 'SI'
029100021205     C                   movel     Alt6          AltRp3
029200021205     C                   endsl
029300000803     C*
029400950621     C* SOLO SPEDIZIONE MANCANTI IN IMA
029500960321    1C     PARIMA        IFEQ      'SI'
029600971224     C* SE E' UN PO ESTERO FORZO IL SUO TERMINAL DI PARTENZA
029700971229     C     PARLNP        IFNE      300
029800971224     C     PARLNP        CHAIN     AZORG                              11
029900020220     c                   eval      og143 = orgde3
030000971229     C                   END
030100971229     C     PARLNP        IFEQ      300
030200020220     c     §ogntw        oreq      'EEX'
030300020220     c     §ogntw        oreq      'EUP'
030400020220     c     §ogntw        oreq      'FED'
030500020220     c     §ogntw        oreq      'DPD'
030600971224     C                   Z-ADD     SIMFEL        WLNP
030700970513     C                   ELSE
030800970513     C                   Z-ADD     PARLNP        WLNP
030900970513     C                   ENDIF
031000970516     C**
031100970516     C* SE NUMERO FOGLIO IMA PASSATO PRENDO QUELLO ALTRIMENTI LO
031200970516     C*  DETERMINO
031300970516    2C     PARFIA        IFEQ      0
031400970516     C**
031500950621     C* CERCO IL FOGLIO IMA DI OGGI
031600950622     C     KFVV          CHAIN     FNFVV000                           10
031700960517     C* SE NON TROVATO FOGLIO SONO TUTTE PRESENTI IN IMA PER
031800950621     C*  CUI NON ACCENDO NEMMENO L'INDICATORE
031900970516    3C     *IN10         DOWEQ     *OFF
032000970516    4C     FVVSPG        IFEQ      'A'
032100950622     C     FVVATB        ANDEQ     ' '
032200950622     C                   SETON                                        1910
032300970516   x4C                   ELSE
032400950622     C     KFVV          READE     FNFVV000                               10
032500970516    4C                   ENDIF
032600970516    3C                   ENDDO
032700970516     C*
032800970516   X2C                   ELSE
032900970516     C                   SETON                                        19
033000970516     C                   MOVEL     PARFIA        FVVNFV
033100970516     C                   MOVEL     PARFGS        FVVFGS
033200970516     C                   MOVEL     IMANPG        FVVNPG
033300970516    2C                   ENDIF
033400970516     C*
033500960321    1C                   ENDIF
033600940620     C*
033700911024     C                   Z-ADD     1             I                 2 0
033800911024     C                   Z-ADD     1             F                 2 0
033900950626    1C     I             DOWLE     29                                           *
034000911024     C*
034100911024     C* SE LA LINEA E' A BLANKS FINISCO IL CARICAMENTO
034200911024     C*
034300940620    2C     LIN(I)        IFNE      *ZEROS
034400911024     C*
034500940620    3C     F             DOWLE     10
034600911024     C*
034700911024     C                   SETOFF                                           53
034800940620    4C     ZNC(F)        IFEQ      *BLANKS
034900911024     C     F             ANDNE     1
035000911024     C                   SETON                                            53
035100911024     C                   Z-ADD     10            F
035200940620    4C                   END
035300960321     C*
035400911104     C                   MOVE      ZNC(F)        VIDZNC
035500960517     C  N53
035600960517     CAN 20KARB          SETLL     FNARB08L
035700941215     C  N53
035800941215     CANN20KARB          SETLL     FNARB05L
035900940620     C*
036000911024     C* LEGGO IL FILE IN BASE AL FATTO CHE LA ZONA SIA A ZERO OPPURE NO
036100940620    4C  N53              DO        *HIVAL
036200940620    5C     ZNC(F)        IFEQ      *BLANKS
036300960517     C   20LIN(I)        READE     FNARB08L                               53
036400941215     C  N20LIN(I)        READE     FNARB05L                               53
036500940620   X5C                   ELSE
036600960517     C   20KARB          READE     FNARB08L                               53
036700941215     C  N20KARB          READE     FNARB05L                               53
036800940620    5C                   END
036900940620     C*
037000911024     C* CONTROLLO LA VALIDITA' DEL RECORD
037100940620    5C  N53              DO
037200960321     C                   SETOFF                                           70
037300960321     C*
037400960321     C* CONTROLLO SE E' ABBINATO E A QUALE FOGLIO ARRIVI
037500960321     C                   EXSR      ABFVA
037600960321     C*
037700960516     C* SE IL FOGLIO RELATIVO NON E' ABBINATO O LA DATA FOGLIO ARRIVI
037800960520     C* E' > DELLA RICHIESTA : NON ELABORO
037900960321    6C     WNFA          IFGT      0
038000960321     C     WDFA          ANDLE     PARDFA
038100960321     C**
038200960321     C**
038300010330     C* SE BOLLA NON CONSEGNATA LA DEVO STAMPARE LO STESSO ANCHE SE
038400010330     C* RICHIESTI IN NON INC. SE SI TRATTA DI BOLLA DI RECUPERO NON
038500010330     C* LEGATA
038600010330     C                   CLEAR                   WINCPT
038700010330     C     ARBDCM        IFEQ      *ZEROS
038800010320     C     ARBNDC        ANDEQ     *ZEROS
038900010320     C                   MOVEL     '3A'          TBLCOD
039000010320     C                   MOVEL     *BLANKS       TBLKEY
039100010320     C                   MOVEL     ARBCBO        TBLKEY
039200010320     C*
039300010320     C     KTAB          CHAIN     TABEL00F                           13
039400010320     C  N13              MOVEL     TBLUNI        DS3A
039500010320     C   13              CLEAR                   DS3A
039600010320    8C     §3ARBL        IFEQ      'R'
039700010320     C     KARB1         SETLL     FNLBL01L                               13
039800010320     C* Se non trovati legami memorizzo che è da elaborare
039900010320     C  N13              MOVEL     'S'           WINCPT            1
040000010320    8C                   ENDIF
040100010320    7C                   ENDIF
040200960321     C* DATA CONSEGNA = 0   DA CONSEGNARE
040300960321IF  7C     ARBDCM        IFEQ      *ZERO                                                    E
040400960520     C* ESCLUDO SE  PARINC = 'SI' => Richiesti solo incasso
040500960520     C     PARINC        ANDNE     'SI'
040600960520     C* ESCLUDO LE BOLLE IN DISTINTA
040700960520     C     ARBNDC        ANDEQ     0
040800010320     C* OPPURE SE SOLO INCASSO DI BOLLA DI RECUPERO NON LEGATA
040900010320IF  7C     ARBDCM        OREQ      *ZERO                                                    E
041000010320     C     PARINC        ANDEQ     'SI'
041100010320     C     WINCPT        ANDEQ     'S'
041200010320     C     ARBNDC        ANDEQ     0
041300920227     C*
041400990304     C* ESCLUDO LE SPEDIZIONI CON C.A. APERTA
041500990128     C                   CLEAR                   WCA
041600040809     C**""*KARB1         CHAIN     FNDCT02L                           89
041700040809    8C**""**IN89         DOWEQ     *OFF
041800990128     C* C.A. APERTA E NON ANNULLATA --> C.A. OK
041900040809    9C**""*DCTATB        IFEQ      ' '
042000040809     C**""*DCTDCH        ANDEQ     0
042100040809     C**""*              MOVEL     'S'           WCA               1
042200040809     C**""*              SETON                                        89
042300040809   X9C**""*              ELSE
042400990304     C*
042500990324     C* MEMORIZZO SE E' PRESENTE UNA C.A. CHIUSA CON CAUSALE DA NON
042600990324     C*  STAMPARE
042700040809     C**""*DCTATB        IFEQ      ' '
042800040809     C**""*              MOVEL     'C'           WCA               1
042900040809     C**""*              ENDIF
043000990324     C*
043100040809     C**""*KARB1         READE     FNDCT02L                               89
043200040809    9C**""*              ENDIF
043300040809    8C**""*              ENDDO
043400040809      *
043500040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
043600040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
043700040809      * legate alla spedizione che passo
043800040809     c                   clear                   fidn12ds
043900040809     c                   eval      i12aas = ARBAAS
044000040809     c                   eval      i12lnp = ARBLNP
044100040809     c                   eval      i12nrs = ARBNRS
044200040809     c                   eval      i12nsp = ARBNSP
044300040809     c                   eval      i12fel = 'E'
044400040809     c                   eval      i12fan = 'E'
044500040809     c*
044600040809     c                   call      'FIDN12R'
044700040809     c                   parm                    fidn12ds
044800040809     c*
044900040809     c                   setoff                                       89
045000040809     c*
045100040809     c* se non ci sono errori
045200040809     c                   if        o12err <> ' '
045300040809     c                   seton                                        89
045400040809     c                   else
045500040809     c* se numero di CA trovate maggiore di zero
045600040809     c                   if        o12nca = 0
045700040809     c                   seton                                        89
045800040809     c                   else
045900040809     c                   z-add     1             jj                2 0
046000040809     c                   dow       jj <= o12nca and *in89 = *off
046100040809     C* C.A. APERTA E NON ANNULLATA --> C.A. OK
046200040809     C     skdch(jj)     IFEQ      0
046300040809     C                   MOVEL     'S'           WCA               1
046400040809     C                   SETON                                        89
046500040809     C                   ELSE
046600040809     C*
046700040809     C* MEMORIZZO SE E' PRESENTE UNA C.A. CHIUSA CON CAUSALE DA NON STAMPARE
046800040809     C                   MOVEL     'C'           WCA               1
046900040809     C                   ENDIF
047000040809     c*
047100040809     c                   add       1             jj
047200040809     c                   enddo
047300040809     c*
047400040809     c                   seton                                        89
047500040809     c*
047600040809     c                   endif
047700040809     c                   endif
047800990304     C**
047900990304     C** SE C.A. APERTA ESCLUDO SEMPRE
048000990304   7BC     WCA           IFNE      'S'
048100990324     C*
048200990304     C** SE C'E' C.A. CHIUSA, CONTROLLO DALLA CAUSALE CHIUSURA E DALLO
048300990304     C**  STATO DEGLI ALTRI COLLI SE DEVO STAMPARE O MENO
048400990304    8C     WCA           IFEQ      'C'
048500990304     C                   EXSR      CACHIU
048600990304     C                   ENDIF
048700990304     C*
048800990304     C* ELABORO SE :
048900990304     C* 1) NON C'E' C.A.
049000990304   7CC     WCA           IFEQ      ' '
049100990304     C* 3) C'E' C.A. CHIUSA MA UNO DEI COLLI NON IN C.A. NON E' CONS.
049200990304     C     WCA           OREQ      'C'
049300990304     C     WNOCON        ANDEQ     'S'
049400960516     C*
049500000626     C* ESCLUDO BOLLE IN BASE A PARAMETRO SI/NO/SOLO POSTE
049600000626     C* .... SOLAMENTE SE NON SONO RITORNI ALL'INCASSO
049700000626     C*
049800000626     C     ARBDCM        IFEQ      0
049900000626     C     ARBLNP        CHAIN     AZORG01L                           83
050000000626     C     *IN83         IFEQ      *OFF
050100000626     C                   MOVEL     ORGDE3        OG143
050200000626      *
050300040826     c* se pgm richiamato da quadratura (PARfr='WRKF ') assumo che parsnp='S
050400040826     c* ') ed in questo caso le bolle poste le includo solo se data spedizio
050500040826     c* >= data in tabella VPO rk 'PPT'
050600121211     c**   §ogntw        ifeq      'PPT'
050700121211     c**   parfr         andeq     'WRKF '
050800121211     c**   arbdsp        andlt     §vpoptl
050900121211     c**                 seton                                        70
051000121211     c**                 endif
051100040826     c*
051200121211     C**   PARSNP        IFEQ      'N'
051300121211     c**   parfr         andne     'WRKF '
051400121211     c**   §ogntw        andeq     'PPT'
051500121211     C**                 SETON                                        70
051600121211     C**                 ENDIF
051700000626      *
051800121211     C**   PARSNP        IFEQ      'P'
051900121211     c**   parfr         andne     'WRKF '
052000121211     c**   §ogntw        andne     'PPT'
052100121211     C**                 SETON                                        70
052200121211     C***                ENDIF
052300000626      *
052400000626     C                   ENDIF
052500000626     C                   ENDIF
052600000803     C*
052700000803     C* ESCLUDO BOLLE IN BASE A PARAMETRO "ASSEGNATO"
052800000803     C*
052900000803     C                   MOVEL     '3A'          TBLCOD
053000000803     C                   MOVEL     *BLANKS       TBLKEY
053100000803     C                   MOVEL     ARBCBO        TBLKEY
053200000803     C                   CLEAR                   COM01
053300000803     C*
053400000803     C     KTAB          CHAIN     TABEL00F                           13
053500000803     C  N13              MOVEL     TBLUNI        DS3A
053600000803     C  N13              MOVEL     §3ATB1        COM01             1
053700000803     C*
053800000803     C     PARASS        IFEQ      'SI'
053900000803     C     COM01         ANDNE     'A'
054000000803     C                   SETON                                        70
054100000803     C                   ENDIF
054200000803     C*
054300000803     C     PARASS        IFEQ      'NO'
054400000803     C     COM01         ANDEQ     'A'
054500000803     C                   SETON                                        70
054600000803     C                   ENDIF
054700000803     C*
054800000803     C* ESCLUDO BOLLE IN BASE A PARAMETRO "RESI"
054900000626     C*
055000000803     C     PARRES        IFEQ      'SI'
055100000803     C     ARBFBR        ANDNE     'S'
055200000803     C                   SETON                                        70
055300000803     C                   ENDIF
055400000803     C*
055500000803     C     PARRES        IFEQ      'NO'
055600000803     C     ARBFBR        ANDEQ     'S'
055700000803     C                   SETON                                        70
055800000803     C                   ENDIF
055900021205      *
056000021205      * Escludo Bolle in base a parametro "BOLLE BLOCCATE"
056100021205      *
056200021205     c                   if            (PARblo =  'SI'
056300021205     c                             and  ARBfbc =  *blanks)
056400021205     c                             or  (PARblo =  'NO'
056500021205     c                             and  ARBfbc <> *blanks)
056600021205     C                   eval      *in70 = *on
056700021205     C                   endif
056800000803     C*
056900000626     C* S O L O    A B I L I T A T E
057000000626     C*
057100960321    8C     *IN20         IFEQ      '1'
057200911115     C*
057300940620     C* SE LA DATA CONSEGNA RICHIESTA E' MINORE DELLA DATA
057400940620     C* DISTINTA IMPOSTATA A VIDEO NON STAMPO (70 ON)
057500960321    9C     ARBTCR        IFEQ      ' '
057600960321     C     ARBDCR        ANDGT     PARDAT
057700911115     C                   SETON                                            70
057800960321    9C                   END
057900940620     C*
058000940620     C* SE CONSEGNA RICHIESTA HA COME TIPO "D-DOPO" ESCLUDO ANCHE
058100941216     C*  ARBDCR = PARDAT (70 ON)
058200960321    9C     ARBTCR        IFEQ      'D'
058300941216     C     ARBDCR        ANDGE     PARDAT
058400911115     C                   SETON                                            70
058500960321    9C                   END
058600911209     C*
058700960321    8C                   END
058800000626     C*
058900920227     C*
059000940620     C* S O L O    N O N    A B I L I T A T E
059100920227     C*
059200960321    8C     *IN30         IFEQ      '1'
059300911209     C*
059400960417     C* ARBAMA =0  NON ABILITATO ASSEGNATO O NON ABILITATO C/ASS-->
059500960417     C*            ELABORO
059600960516    9C     ARBAMA        IFGT      *ZEROS
059700960417     C     ARBACC        ANDEQ     'S'
059800960417     C     ARBACA        ANDEQ     'S'
059900960417     C                   SETON                                        70
060000960417    9C                   END
060100911209     C*
060200940620     C* CONTROLLO DETTAGLIO COLLI
060300960321     C  N70              EXSR      DETCOL
060400920527     C*
060500960321    8C                   ENDIF
060600920227     C*
060700940620     C* A B I L I T A T E  E   N O N   A B I L I T A T E
060800920227     C*
060900960321    8C     *IN40         IFEQ      '1'
061000911209     C*
061100920227     C* EVIDENZIO SE ABILITATA O MENO
061200911209     C                   SETOFF                                       31
061300960516    9C     ARBAMA        IFNE      *ZEROS
061400911209     C     ARBACC        ANDEQ     'S'
061500911209     C     ARBACA        ANDEQ     'S'
061600911209     C     ARBFBC        ANDEQ     ' '
061700911209     C                   SETON                                        31
061800960321    9C                   END
061900940620     C*
062000940620     C* CONTROLLO DETTAGLIO COLLI
062100940620     C  N70              EXSR      DETCOL
062200960321    8C                   ENDIF
062300950621     C*
062400960517     C* SE RICHIESTE LE BOLLE PRESENTI IN IMA CONTROLLO CHE ABBIANO
062500960517     C*   ALMENO UN COLLO IN IMA
062600960517    8C     *IN19         IFEQ      *ON
062700960517     C     *IN70         ANDEQ     *OFF
062800960517     C                   EXSR      CTRIMA
062900911115     C*
063000960516     C* SE RICHIESTE LE BOLLE PRESENTI IN IMA ESCLUDO I RECUPERI
063100960517     C     *IN70         IFEQ      *OFF
063200960516     C                   MOVEL     '3A'          TBLCOD
063300960516     C                   MOVEL(P)  ARBCBO        TBLKEY
063400960516     C     KTAB          CHAIN     TABEL00F                           33
063500960516     C  N33              MOVEL     TBLUNI        DS3A
063600960516     C   33              CLEAR                   DS3A
063700960516     C     §3ARBL        IFEQ      'R'
063800960516     C                   SETON                                        70
063900960516     C                   ENDIF
064000960517     C                   ENDIF
064100960517    8C                   ENDIF
064200960516     C*
064300030826     ***C  N70              EXSR      STAMPA
064400030826sel 8c                   select
064500030826w   8c                   when      *in70
064600030826w   8c                   when      PARfr = 'WRKF '
064700030826     c                   clear                   wErr
064800030826     c                   exsr      WriteWFCBB
064900030826x   8c                   other
065000030826     c                   exsr      Stampa
065100030826e   8c                   endsl
065200990304   7CC                   ENDIF
065300990304   7BC                   ENDIF
065400941215     C*
065500960321   X7C                   ELSE
065600960321     C* DATA CONSEGNA > 0  CONSEGNATA
065700960520    8C     ARBDCM        IFNE      *ZEROS
065800960520     C* ESCLUDO SE  PARINC = 'NO' => Richiesto solo non incasso
065900960520     C     PARINC        ANDNE     'NO'
066000960520     C* ESCLUDO SE SOLO BOLLE PRESENTI IN IMA
066100960520     C     PARIMA        ANDNE     'SI'
066200941215     C                   EXSR      STAMP1
066300941215     C*
066400960520    8C                   ENDIF
066500960321    7C                   ENDIF
066600911024     C*
066700960321    6C                   ENDIF
066800960321    5C                   ENDDO
066900960321    4C  N53              ENDDO
067000911024     C*
067100911024     C                   ADD       1             F
067200960321    3C                   ENDDO
067300911024     C                   Z-ADD     1             F
067400911024     C*
067500940620   X2C                   ELSE
067600911024     C                   Z-ADD     29            I
067700960321    2C                   ENDIF
067800911024     C*
067900911024     C                   ADD       1             I
068000911024     C*
068100950626    1C                   ENDDO
068200950626    0C                   ENDIF
068300911024     C*
068400940620     C**
068500940620     C* S T A M P A   E L E N C O   B O L L E   N O N   P A R T I T E
068600940620     C**
068700940620    1C     *IN77         IFEQ      *ON
068800940620     C*
068900941215     C     KARB6         SETLL     FNARB06L                           78
069000940620     C*
069100940620    2C     *IN78         DOWEQ     '0'
069200941215     C     KARB6         READE     FNARB06L                               78
069300940620     C*
069400940620    3C  N78ARBAMP        IFEQ      0
069500920908     C     ARBLNA        LOOKUP    L6                                     91
069600940620     C*
069700940620    4C     *IN91         IFEQ      *ON
069800000322     C* IGNORO SE SI TRATTA DI BOLLA CON FLAG DDT='N' PERCHE' SIGNIFICA
069900000322     C* CHE NON E' ANCORA STATA STAMPATA E QUINDI NON HO BISOGNO DI
070000000322     C* TOGLIERLA DAL GIRO
070100000322     C                   MOVEL(P)  'A'           KTRC
070200060216     C     KAR4          CHAIN     FiAR401L                           11
070300000322     C  N11              MOVEL     AR4NOT        DSBL4A
070400000322   4AC     *IN11         IFEQ      *ON
070500000322     C     §B4ABM        ORNE      'N'
070600940620     C* CONTROLLO IL DETTAGLIO COLLI
070700940620     C                   EXSR      DETCOL
070800960119     C* SE SPEDIZIONE CON TUTTI I COLLI PARTITI MA NON ABILITATA
070900970117     C* --> NON STAMPO
071000960119     C                   SETOFF                                       14
071100960119    5C     SP1(1)        IFEQ      *ZERO
071200970811     C     NRS(1)        ANDEQ     *ZEROS
071300960119     C                   SETON                                        14
071400970811    5C                   ENDIF
071500970807     C*
071600970807     C* SE SPEDIZIONE IMPORT CON COLLI PARZIALMENTE A TERRA E NON HA
071700970807     C* IL C/A E NON PREVEDE FATTURA IMMEDIATA IN ASSEGNATO
071800970807     C* --> NON STAMPO
071900971224   5AC     *IN14         IFEQ      *OFF
072000971224     C     *IN34         ANDEQ     *OFF
072100971224     C     ARBLNP        CHAIN     AZORG                              11
072200020220     c                   eval      og143 = orgde3
072300020220     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP'or
072400020220     c                             §ogntw = 'FED' or §ogntw = 'DPD'
072500970807     C* DECODIFICO IL TIPO BOLLA
072600970807     C                   MOVEL     '3A'          TBLCOD
072700970807     C                   MOVEL     *BLANKS       TBLKEY
072800970807     C                   MOVEL     ARBCBO        TBLKEY
072900970807     C     KTAB          CHAIN     TABEL00F                           13
073000970807     C   13              CLEAR                   DS3A
073100970807     C  N13              MOVEL     TBLUNI        DS3A
073200970807     C*
073300970811    6C     §3AFCA        IFEQ      0
073400970807     C                   MOVE      ARBKSC        W0040             4 0
073500970807     C* SE ASSEGNATO VERIFICO SE C'E' LA FATTURA
073600970811     C                   SETON                                        13
073700970811    7C     W0040         IFEQ      9999
073800991104     C                   MOVEL     1             KTRC
073900991104     C     KARB1         CHAIN     FIAR601L                           13
074000970811    7C                   END
074100970811    7C     W0040         IFNE      9999
074200970811     C     *IN13         OREQ      *ON
074300970811     C     AR6DFT        OREQ      0
074400970807     C                   SETON                                        14
074500970811    7C                   END
074600970811    6C                   END
074700970811    5C                   END
074800971224   5AC                   END
074900960119     C*
075000940620     C* STAMPO LA SPEDIZIONE NON PARTITA
075100030826     ***C  N14              EXSR      STAMPA
075200030826sel 8c                   select
075300030826w   8c                   when      *in14
075400030826w   8c                   when      PARfr = 'WRKF '
075500030826     c                   clear                   wErr
075600030826     c                   exsr      WriteWFCBB
075700030826x   8c                   other
075800030826     c                   exsr      Stampa
075900030826e   8c                   endsl
076000941219     C* SE LA BOLLA NON E' ABILITATA, MA NON C'E' NESSUN COLLO NON
076100941219     C* ABILITATO ALLA PARTENZA, EMETTO MESSAGGIO
076200960119    5C  N14SP1(1)        IFEQ      *ZERO
076300950322     C     ARBAMA        ANDEQ     0
076400030826if  6c                   if        PARfr = 'WRKF '
076500030826     c                   eval      wErr  = 1
076600030826     c                   exsr      WriteWFCBB
076700030826x   6c                   else
076800950808     C                   MOVEL     MSG(1)        STPMSG           66
076900941219     C                   EXCEPT    MESSAG                                       EMETTI MESSAGIO
077000030826e   6c                   endif
077100950808     C*
077200950808    6C     NRS(1)        IFEQ      *ZEROS
077300030826if  7c                   if        PARfr <> 'WRKF '
077400950808     C                   EXCEPT    VUOTA
077500030826e   7c                   endif
077600950808   X6C                   ELSE
077700030826if  7c                   if        PARfr =  'WRKF '
077800030826     c                   eval      wErr  =  2
077900030826     c                   exsr      WriteWFCBB
078000030826x   7c                   else
078100950808     C                   MOVEL     MSG(2)        STPMSG
078200950808     C                   EXCEPT    MESSG2
078300030826e   7c                   endif
078400950321     C* Stampo segnacolli che si trovano su F.V. diverso e non ancora
078500950321     C* abbinato
078600030826if 6ac                   if        PARfr <> 'WRKF '
078700950321     C                   Z-ADD     1             II
078800950322     C                   MOVE      ' '           W001A             1
078900950808    7C     NRS(II)       DOWNE     *ZEROS
079000950808    8C     W001A         IFEQ      ' '
079100950321     C                   EXCEPT    DETSC
079200950322     C                   MOVE      '1'           W001A
079300950808   X8C                   ELSE
079400950322     C                   EXCEPT    DETSC1
079500950322     C                   MOVE      ' '           W001A
079600950808    8C                   END
079700950321     C                   ADD       1             II
079800950808    7C                   ENDDO
079900950808     C                   EXCEPT    VUOTA
080000030826e  6ac                   endif
080100950808    6C                   END
080200950321     C*
080300950808    5C                   ENDIF
080400941219     C*
080500940620    4C                   ENDIF
080600000322   4AC                   ENDIF
080700940620    3C                   ENDIF
080800950808    2C                   ENDDO
080900950808    1C                   ENDIF
081000911104     C*
081100030826     ***C   99              EXCEPT    TOTALE
081200030826if  1c                   if        PARfr <> 'WRKF '
081300030826     c                             and   *in99
081400030826     c                   except    TOTALE
081500030826e   1c                   endif
081600911104     C*
081700990304     C                   CLEAR                   DSBS02
081800990304     C                   MOVEL     'C'           T02TLA
081900990304     C                   CALL      'TIBS02R'
082000990304     C                   PARM                    KPJBA
082100990304     C                   PARM                    DSBS02
082200990304     C                   CLEAR                   DSBS02
082300911028     C                   SETON                                        LR
082400941214     C**************************************************************************
082500941214     C* CONTROLLO SE BOLLA DI FV ABBINATO
082600941214     C**************************************************************************
082700940620     C     ABFVA         BEGSR
082800960321     C                   CLEAR                   WNFA
082900960321     C                   CLEAR                   WDFA
083000960321     C* CONTROLLO SE SPEDIZIONE LOCALE
083100051019     C     ARBtfp        comp      arbtfa                                 61
083200941214     C*
083300940620     C* CONTROLLO SE IL FOGLIO VIAGGIO E'GIA' ABBINATO
083400960321    0C     ARBNFV        IFEQ      99999
083500960321     C     *IN61         OREQ      *ON
083600960321     C                   Z-ADD     *ALL'9'       WNFA
083700960321     C* CHAIN TIPO BOLLA
083800960321     C                   MOVEL     '3A'          TBLCOD
083900960321     C                   MOVEL(P)  ARBCBO        TBLKEY
084000960321     C     KTAB          CHAIN     TABEL00F                           13
084100960321     C  N13              MOVEL     TBLUNI        DS3A
084200960321     C   13              CLEAR                   DS3A
084300960321     C*
084400960321     C* 1) SE SI TRATTA DI BOLLA DI RECUPERO. STAMPO SOLO SE ABILITATO
084500960321     C*    L'ASSEGNATO   (LASCIO DATA ARRIVO = 0)
084600960321    1C     §3ARBL        IFEQ      'R'
084700960321    2C     ARBACA        IFNE      'S'
084800960321     C                   CLEAR                   WNFA
084900960321    2C                   ENDIF
085000960321     C*
085100960321   X1C                   ELSE
085200960321     C* 2) SE LNP DELLA &6 --> STAMPO SEMPRE LASCIO DATA ARRIVO =0
085300960322     C                   MOVEL     ARBLNP        W003A             3
085400960322     C     W003A         LOOKUP    LIP                                    13
085500960321    2C     *IN13         IFEQ      *OFF
085600960321     C*
085700960321     C* 3) SE LA DATA ARRIVO NON C'E', PERCHE' BOLLA SENZA SEGNACOLLI
085800960321     C*  CONFRONTO CON LA DATA SPEDIZIONE
085900960321    3C     ARBDAM        IFEQ      0
086000960321     C                   MOVEL     ARBDSP        WDFA
086100960321   X3C                   ELSE
086200960321     C*
086300960321     C* 4) SE LA BOLLA HA LA DATA DI ARRIVO LA USO
086400960321     C                   MOVEL     ARBDAM        WDFA
086500960321    3C                   ENDIF
086600960321    2C                   ENDIF
086700960321    1C                   ENDIF
086800960321     C*
086900960321   X0C                   ELSE
087000960321     C*
087100971224     C* RICHIAMO PGM PER CERCARE FOGLIO VIAGGIO DELLA BOLLA
087200971224     C                   CLEAR                   DSLV53
087300971224     C                   MOVEL     'A'           D53TFO
087400971224     C                   Z-ADD     ARBNFV        D53NFV
087500981214     C                   MOVE      ARBTFP        D53FGS
087600031211     c* controllo la bolla se relativa a foglio abbinato
087700031211     C                   MOVEL     'B'           D53ABB
087800971224     C                   MOVE      ARBLNA        D53LNA
087900971224     C                   MOVEL     ARBTFA        D53TFA
088000971224     C                   CALL      'FNLV53R'
088100971224     C                   PARM                    DSLV53
088200960321     C*
088300960321     C* SE NON HO TROVATO IL FOGLIO ARRIVI
088400960321     C* VERIFICO SE E' GIA' STATO PULITO
088500971224    1C     D53ERR        IFNE      *BLANKS
088600960321    2C     ARBDSP        IFLE      COMFVA
088700021212     C* - se non trovato foglio arrivi:
088800021212     C*   escludo se PARINC = 'NO' (NON richiesti SOLO incasso)
088900021212     C     D53err        OReq      '2'
089000021212     C     PARinc        andne     'NO'
089100960321     C* FOGLIO PARTENZA GIA' PULITO: COME SE FOSSE ABBINATO
089200960321     C                   Z-ADD     *ALL'9'       WNFA
089300960321     C                   MOVEL     ARBDAM        WDFA
089400960321   X2C                   ELSE
089500960321     C                   CLEAR                   WNFA
089600960321     C                   CLEAR                   WDFA
089700960321    2C                   ENDIF
089800960321     C**
089900960321   X1C                   ELSE
090000971224     C                   MOVEL     D53NFA        WNFA
090100971224     C                   MOVEL     D53DFA        WDFA
090200960321    1C                   ENDIF
090300960321    0C                   ENDIF
090400941214     C*
090500940620     C                   ENDSR
090600941214     C**************************************************************************
090700941214     C* CONTROLLA DETTAGLIO COLLI
090800941214     C**************************************************************************
090900940620     C     DETCOL        BEGSR
091000941214     C*
091100940620     C                   EXSR      DETS                                         DETT. COLLI
091200940620     C     H             ADD       K             ZZ                2 0
091300940620     C*
091400940620    1C     ZZ            IFGE      ARBNCL
091500940620     C                   SETON                                        34
091600940620     C                   ELSE
091700940620     C                   SETOFF                                       34
091800940620    1C                   END
091900940620     C*
092000940620     C     T             ADD       W             YY                2 0
092100940620     C*
092200940620    1C     YY            IFGE      ARBNCL
092300940620     C                   SETON                                        35
092400940620     C                   ELSE
092500940620     C                   SETOFF                                       35
092600940620    1C                   END
092700941214     C*
092800940620     C                   ENDSR
092900941214     C**************************************************************************
093000941215     C* STAMPA DISTINTA USCITA CONSEGNE PER FNARB01L
093100941214     C**************************************************************************
093200911025     C     STAMPA        BEGSR
093300991104     C* IMPOSTAZIONI DI STAMPA COMUNI
093400991104     C                   EXSR      IMPCOM
093500010330     C     WINCPT        IFEQ      *BLANKS
093600991104     C                   ADD       ARBNCL        STPTCO            5 0
093700911206     C*
093800941215     C                   ADD       ARBPKF        V06PKF            8 1
093900941215     C                   ADD       ARBVLF        V06VOL            7 3
094000010330     C                   ENDIF
094100911025     C                   ADD       1             V06TSP            4 0
094200051214     c
094300911025     C                   MOVE      ARBDCR        MMGG              4 0
094400911025     C                   MOVEL     MMGG          MM                2 0
094500911025     C                   MOVE      MMGG          GG                2 0
094600911025     C                   MOVE      MM            STPGMC            4 0
094700911025     C                   MOVEL     GG            STPGMC
094800960516     C                   MOVEL     ARBHCR        STPHCR
094900051214     c* Verifico se è tassativa
095000051214     c                   movel     arbtcr        stptcr            1
095100051214     c                   if        stptcr=' ' and arbdcr>0
095200051214     c                   clear                   fnlv80ds
095300051214     c                   eval      ilv80aas=arbaas
095400051214     c                   eval      ilv80lnp=arblnp
095500051214     c                   eval      ilv80nrs=arbnrs
095600051214     c                   eval      ilv80nsp=arbnsp
095700051214     c                   eval      ilv80mgs=arbmgs
095800051214     c                   eval      ilv80tc1=arbtc1
095900051214     c                   eval      ilv80tc2=arbtc2
096000051214     c                   eval      ilv80dcr=arbdcr
096100051214     c                   eval      ilv80hcr=arbhcr
096200051214     c                   eval      ilv80tcr=arbtcr
096300051214     c                   call      'FNLV80R'
096400051214     c                   parm                    kpjba
096500051214     c                   parm                    fnlv80ds
096600051214     c
096700051214     c                   if        olv80tat='S'
096800051214     c                   movel     'T'           stptcr
096900051214     c                   endif
097000051214     c                   endif
097100051214     c
097200950119     C*
097300911104     C     COMFNS        IFEQ      'S'                                          *B003
097400911025     C                   SETON                                        95
097500911025     C                   ELSE
097600911025     C                   SETOFF                                       95
097700911025     C                   END                                                    *E003
097800950119     C*
097900911106     C* CONTROLLO SE ESISTONO ANOMALIE
098000911106     C     ARBFAN        IFEQ      'S'
098100911106     C                   SETON                                        96
098200911106     C                   ELSE
098300911106     C                   SETOFF                                       96
098400911106     C                   END
098500950119     C*
098600950119     C     ARBVLF        IFGT      §3IVLF
098700950119     C     ARBPKF        ORGT      §3IPKF
098800920527     C                   SETON                                        44
098900920527     C                   ELSE
099000920527     C                   SETOFF                                       44
099100920527     C                   END
099200911025     C*
099300930426     C* INDICATORE DI CONSEGNA PARZIALE
099400930426     C     ARBDCP        COMP      0                                  94
099500941215     C*
099600941215     C                   EXSR      SCRIVI                                       STAMPA RIGA
099700941215     C*
099800960517     C     *IN77         IFEQ      *ON
099900960517     C*
100000950322     C     SP1(1)        IFGT      0
100100960517     C     ARBAMA        IFEQ      0
100200920527     C                   EXCEPT    NOAP1
100300960517     C                   ELSE
100400950322     C                   EXCEPT    NOAP1X
100500960517     C                   ENDIF
100600950322     C                   END
100700950322     C*
100800920527     C     SP2(1)        IFGT      0
100900920527     C                   EXCEPT    NOAP2
101000920527     C                   END
101100920527     C     SA1(1)        IFGT      0
101200920527     C                   EXCEPT    NOAA1
101300920527     C                   END
101400920527     C     SA2(1)        IFGT      0
101500920527     C                   EXCEPT    NOAA2
101600920527     C                   END
101700960517     C*
101800960517     C                   ENDIF
101900950119     C**
102000950119     C** SEGNACOLLI NON SEQUENZIALI
102100911025     C   95              DO                                                     *B003
102200950119     C*
102300941214     C  N90KARB1         SETLL     FNART01L
102400941214     C   90KLBL          SETLL     FNART01L
102500911025     C                   Z-ADD     1             S                 2 0
102600941215     C                   Z-ADD     *ZEROS        NSC
102700911025     C                   DO        *HIVAL                                       *B004
102800941214     C  N90KARB1         READE     FNART01L                               63
102900941214     C   90KLBL          READE     FNART01L                               63
103000911025     C  N63S             DOWLE     13                                           *B005
103100941215     C                   MOVE      ARTNSC        NSC(S)
103200911025     C                   ADD       1             S
103300941214     C  N90KARB1         READE     FNART01L                               63
103400941214     C   90KLBL          READE     FNART01L                               63
103500911025     C  N63              END                                                    *E005
103600941215     C                   EXCEPT    DETNSC
103700911025     C                   Z-ADD     1             S
103800941215     C                   Z-ADD     *ZEROS        NSC
103900911025     C  N63              END                                                    *E004
104000911025     C                   END                                                    *E003
104100941214     C*
104200911025     C                   ENDSR
104300991104     C**************************************************************************
104400991104     C* IMPOSTAZIONI COMUNI PER LA STAMPA
104500991104     C**************************************************************************
104600991104     C     IMPCOM        BEGSR
104700991105     C                   CLEAR                   CONSE1
104800991105     C                   CLEAR                   CONSE2
104900991104     C                   MOVEL     *ZEROS        STPGMC
105000991104     C                   MOVEL     *ZEROS        STPHCR
105100991104     C                   CLEAR                   AR9TIC                         TIPO INCASSO CONTRAS
105200991104     C                   CLEAR                   AR9CAS                         CONTRASSEGNO
105300991104     C                   CLEAR                   AR9VCA                         VALUTA
105400991104     C                   CLEAR                   AR9CMB                         CAMBIO
105500991104     C                   CLEAR                   COMFNS
105600991104     C                   CLEAR                   WCONVE
105700991105     C                   CLEAR                   W0133
105800991105     C                   CLEAR                   WIFT
105900991105     C                   Z-ADD     0             STPIM0            9 0
106000991105     C                   Z-ADD     0             STPIMD            9 3
106100991104     C                   MOVEL     *BLANKS       STPTIC                         TIPO INCASSO CONTRAS
106200991104     C                   Z-ADD     0             STPCAS                         CONTRASSEGNO
106300991104     C                   CLEAR                   STPDIV                         VALUTA C/A
106400991104     C                   CLEAR                   STPVCA                         VALUTA C/A
106500991104     C                   CLEAR                   STPCMB                         CAMBIO C/A
106600991104     C                   SETOFF                                       5590
106700991104     C*
106800991104     C* CONTROLLO SE LA BOLLA IN QUESTIONE HA LEGAME CON SECONDA BOLLA
106900991104     C     KARB1         CHAIN     FNLBL01L                           54
107000991104     C*
107100991104     C  N54LBLLAN        COMP      LBLLAP                             5454
107200991104     C*
107300991104    1C     *IN54         IFEQ      *OFF
107400991104     C                   SETON                                        90
107500991104     C*
107600991104     C     KLBL          CHAIN     FNARB01L                           55
107700051114     C  N55KLBL          CHAIN     FiAR901L                           89
107800991104     C*
107900991104     C  N55
108000991104    2CANN89ARBICC        IFEQ      ' '
108100991104     C     ARBICC        OREQ      'R'
108200991104     C                   MOVEL     AR9VCA        STPVCA                         VALUTA C/A   CONTRAS
108300991104     C                   MOVEL     AR9TIC        STPTIC                         TIPO INCASSO CONTRAS
108400991104     C                   Z-ADD     AR9CAS        STPCAS                         CONTRASSEGNO
108500991104     C                   Z-ADD     AR9CMB        STPCMB                         CAMBIO
108600991104    2C                   END
108700991104     C  N55              MOVE      ARBFNS        COMFNS            1
108800991104     C*
108900991104     C* RIAGGANCIO DI NUOVO LA FIGLIA PER RECUPERARE GLI ALTRI DATI
109000991104     C  N54KARB2         CHAIN     FNARB01L                           55
109100991104     C*
109200991104   X1C                   ELSE
109300991104     C                   MOVE      ARBFNS        COMFNS
109400991104     C*
109500991104     C* SE NON C'E' LEGAME CONTROLLO IL C/A DELLA BOLLA CHE STO LEGGEND
109600051114     C     KARB1         CHAIN     FiAR901L                           89
109700991104    2C  N89ARBICC        IFEQ      ' '
109800991104     C     ARBICC        OREQ      'R'
109900991104     C                   MOVEL     AR9VCA        STPVCA                         VALUTA C/A   CONTRAS
110000991104     C                   MOVEL     AR9TIC        STPTIC                         TIPO INCASSO CONTRAS
110100991104     C                   Z-ADD     AR9CAS        STPCAS                         CONTRASSEGNO
110200991104     C                   Z-ADD     AR9CMB        STPCMB                         CAMBIO
110300991104    2C                   ENDIF
110400991104    1C                   ENDIF
110500991104     C*
110600991104     C* DECODIFICO IL TIPO BOLLA
110700991104     C                   MOVEL     '3A'          TBLCOD
110800991104     C                   MOVEL     *BLANKS       TBLKEY
110900991104     C                   MOVEL     ARBCBO        TBLKEY
111000991104     C*
111100991104     C     KTAB          CHAIN     TABEL00F                           13
111200991104     C  N13              DO
111300991104     C                   MOVEL     TBLUNI        DS3A
111400991104     C                   MOVEL     §3ADES        VIDCBL            1
111500991104     C                   MOVEL     §3ATB1        COM01             1
111600991104     C                   END
111700991104     C*
111800991104     C                   Z-ADD     ARBDSP        G02INV                         AAAA/MM/GG
111900991104     C                   Z-ADD     *ZERO         G02DAT
112000991104     C                   MOVEL     '3'           G02ERR
112100991104     C                   CALL      'XSRDA8'
112200991104     C                   PARM                    WLBDAT
112300991104     C                   MOVE      G02DAT        W0020             2 0
112400991104     C                   MOVEL     G02DAT        STPDSP            6 0          GG/MM/AA
112500991104     C                   MOVE      W0020         STPDSP
112600991104     C*
112700991104     C* AGGANCIO I DATI DI TASSAZIONE BOLLA
112800991104     C                   CLEAR                   AR6IFT                         IMPORTO FATTURA
112900991104     C                   CLEAR                   AR6DIV                         IMPORTO FATTURA
113000991104    1C     §3ATB2        IFEQ      ' '
113100991104     C     COM01         ANDEQ     'A'
113200991104     C*
113300991104     C                   MOVEL     1             KTRC
113400991105     C     KAR6          CHAIN     FIAR601L                           89
113500991104   X1C                   ELSE
113600991104     C*
113700991104     C* PER 2 BOLLA
113800991104    2C     §3ATB2        IFNE      *BLANKS
113900991104     C                   MOVEL     2             KTRC
114000991105     C     KAR6          CHAIN     FIAR601L                           89
114100991104    2C                   ENDIF
114200991104    1C                   ENDIF
114300991104     C**
114400991104    1C     AR6IFT        IFGT      0
114500991104    2C     ARBFIP        IFEQ      'I'
114600991104     C     ARBICA        OREQ      ' '
114700991104     C     ARBICA        OREQ      'R'
114800991104     C* LEGGO DA ARI GLI INCASSI PARZIALI FATTI
114900991104     C     KARI          CHAIN     FIARI01L                           89
115000991104    3C     *IN89         DOWEQ     *OFF
115100991104     C*
115200991104     C     ARIIMP        ADD       ARIIPP        WIPP
115300991104    4C     WIPP          IFGT      0
115400011025    5C     ARIDIV        IFNE      §GEDCN
115500011025      *
115600991104     C**
115700991104     C                   CLEAR                   YEUR
115800991104     C                   MOVEL     ARIDIV        YECDVI
115900011025     C                   MOVEL     §GEDCN        YECDVO
116000011025     C                   MOVE      §CVDEC        YECDCO
116100991104     C                   Z-ADD     WIPP          YECIMI
116200991104     C*
116300991104     C                   CALL      'YEURCO'
116400991104     C                   PARM                    YEUR
116500991104     C*
116600991104    6C     YECESI        IFEQ      ' '
116700991104     C                   ADD       YECIMO        WCONVE
116800991104    6C                   ENDIF
116900991104   X5C                   ELSE
117000991104     C                   ADD       WIPP          WCONVE
117100991104    5C                   ENDIF
117200991104    4C                   ENDIF
117300991104     C**
117400991104     C     KARI          READE     FIARI01L                               89
117500991104    3C                   ENDDO
117600991105     C**
117700991105     C** TRASFORMO LA FATTURA NELLA DIVISA DELLA MONETA DI CONTO
117800011025     C     AR6DIV        IFNE      §GEDCN
117900011025      *
118000991105     C                   CLEAR                   YEUR
118100991105     C                   MOVEL     AR6DIV        YECDVI
118200011025     C                   MOVEL     §GEDCN        YECDVO
118300011025     C                   MOVE      §CVDEC        YECDCO
118400991105     C                   Z-ADD     AR6IFT        YECIMI
118500991105     C*
118600991105     C                   CALL      'YEURCO'
118700991105     C                   PARM                    YEUR
118800991105     C*
118900991105    6C     YECESI        IFEQ      ' '
119000991105     C                   Z-ADD     YECIMO        WIFT
119100991105     C                   ENDIF
119200991105     C                   ELSE
119300991105     C                   Z-ADD     AR6IFT        WIFT
119400991105     C                   ENDIF
119500991105     C**
119600991105     C     WIFT          SUB       WCONVE        W0133            13 3
119700991105    3C     W0133         IFGT      0
119800011025     C                   MOVEL     §GEDCN        STPDIV
119900991105     C                   MOVE      W0133         W0030             3 0
120000991105     C* IMPORTO CON DECIMALI
120100991105     C     W0030         IFGT      0
120200991105     C                   Z-ADD     W0133         STPIMD
120300991105     C                   SETON                                        01
120400991105     C                   ELSE
120500991105     C* IMPORTO SENZA DECIMALI
120600991105     C                   Z-ADD     W0133         STPIM0
120700991105     C                   SETOFF                                       01
120800991105     C                   ENDIF
120900991105     C**
121000991104    3C                   END
121100991104    2C                   END
121200991104    1C                   END
121300991104     C*
121400991104     C* CALCOLO IL TOTALE DA INCASSARE
121500991104     C* PER IL C/A SE E' IN DIVISA LO CAMBIO
121600991104     C                   EXSR      CTRCAS
121700991104     C*
121800991105     C* TOTALE DA INCASSAE
121900991105     C                   ADD       W0133         STPTOC           17 3
122000991105     C                   ADD       WCAS          STPTOC
122100991105     C*
122200991105     C* CONSEGNE PARTICOLARI
122300991105    1C     ARBTC1        IFNE      *BLANK
122400991105     C                   MOVEL     '1P'          TBLCOD
122500991105     C                   MOVEL     *BLANKS       TBLKEY
122600991105     C                   MOVEL     ARBTC1        TBLKEY
122700991105     C     KTAB          CHAIN     TABEL00F                           13
122800991105     C  N13              MOVEL     TBLUNI        DS1P
122900991105     C  N13              MOVEL     §1PDES        CONSE1           25
123000991105    1C                   END
123100991105     C* DECODIFICO 2° TIPO CONSEGNA PARTICOLARE
123200991105    1C     ARBTC2        IFNE      *BLANK
123300991105     C                   MOVEL     '1P'          TBLCOD
123400991105     C                   MOVEL     *BLANKS       TBLKEY
123500991105     C                   MOVEL     ARBTC2        TBLKEY
123600991105     C     KTAB          CHAIN     TABEL00F                           13
123700991105     C  N13              MOVEL     TBLUNI        DS1P
123800991105     C  N13              MOVEL     §1PDES        CONSE2           25
123900991105     C*
124000991105     C* SE C'E' SOLO LA 2° CONS.PARTICOLARE, METTO LA
124100991105     C* DECODIFICA DELLA 2° AL POSTO DI QUELLA DELLA 1°
124200991105    2C     CONSE1        IFEQ      *BLANKS
124300991105     C     CONSE2        ANDNE     *BLANKS
124400991105     C                   MOVEL     CONSE2        CONSE1
124500991105     C                   CLEAR                   CONSE2
124600991105    2C                   ENDIF
124700991105    1C                   ENDIF
124800991105     C*
124900991104     C                   ENDSR
125000941214     C**************************************************************************
125100941215     C* STAMPA DISTINTA USCITA CONSEGNE PER FNARB
125200941214     C**************************************************************************
125300911105     C     STAMP1        BEGSR
125400991104     C* IMPOSTAZIONI COMUNI DI STAMPA
125500991104     C                   EXSR      IMPCOM
125600950119     C*
125700920123     C                   SETON                                        9598
125800911106     C                   SETOFF                                       96
125900991104     C**
126000991105     C* NON MI SERVE IL FLAG NON SEQ.
126100991104     C                   CLEAR                   COMFNS
126200911105     C*
126300920123     C* STAMPO SOLO SE DEVO INCASSARE
126400950119    1C     STPCAS        IFGT      *ZEROS
126500991105     C     W0133         ORGT      *ZEROS
126600030925if  2c                   if        PARfr = 'WRKF '
126700030925     c                   clear                   wErr
126800030925     c                   exsr      WriteWFCBB
126900030925x   2c                   else
127000941215     C                   EXSR      SCRIVI                                       STAMPA RIGA
127100030925e   2c                   endif
127200920211     C                   ADD       1             V06TSP            4 0
127300950119    1C                   END
127400991105     C*
127500920123     C                   SETOFF                                       98
127600941214     C*
127700911105     C                   ENDSR
127800941215     C**************************************************************************
127900941215     C* STAMPA DELLA RIGA DI DETTAGLIO
128000941215     C**************************************************************************
128100941215     C     SCRIVI        BEGSR
128200960517     C                   SETOFF                                       16
128300941215     C*
128400941215     C* PER OVERFLOW STAMPO LA TESTATA
128500941215     C  N99
128600941215     COR OF              EXCEPT    TESTA
128700941215     C  N99              SETON                                        99
128800941215     C*
128900941215     C* PRIMA DI STAMPARE AGGANCIO, SE CI SONO, LE NOTE
129000941215     C                   MOVEL     *BLANKS       NOTE1
129100941215     C                   MOVEL     '8'           KTRC                           TIPO RECORD
129200060216     C     KAR4          CHAIN     FiAR401L                           89
129300941215     C  N89              MOVEL     AR4NOT        NOTE1            35
129400950119     C*
129500941215     C                   MOVEL     *BLANKS       NOTE2
129600941215     C                   MOVEL     '9'           KTRC                           TIPO RECORD
129700060216     C     KAR4          CHAIN     FiAR401L                           89
129800941215     C  N89              MOVEL     AR4NOT        NOTE2            35
129900941215     C*
130000941215     C* SE NOTE1 E' VUOTO E NOTE2 E' PIENO, METTO NOTE2 IN NOTE1
130100941215     C     NOTE1         IFEQ      *BLANKS
130200941215     C     NOTE2         ANDNE     *BLANKS
130300941215     C                   MOVEL     NOTE2         NOTE1
130400941215     C                   CLEAR                   NOTE2
130500941215     C                   ENDIF
130600030320
130700030320      * Controllo se bolla mai stampata
130800030320     c                   Eval      *In12 = *Off
130900030320     c                   Clear                   Dsbl4a
131000030320     c                   Movel     'A'           Ktrc
131100060216     c     Kar4          Chain     Fiar401l
131200060216     c                   If        %Found(Fiar401l)
131300030320     c                   Movel     Ar4Not        Dsbl4a
131400030320     c                   EndIf
131500030320     c                   If        §b4abm = 'N'
131600030320     c                   Eval      *In12 = *On
131700030320     c                   EndIf
131800941216     C*
131900960515     C                   MOVEL     ARBRSD        STPRSD           15            DESTINATARIO
132000960515     C                   MOVEL     ARBRSM        STPRSM           15            MITTENTE
132100010330     C     WINCPT        IFEQ      'S'
132200010330     C                   SETON                                        97
132300010330     C                   ENDIF
132400941215     C*
132500941215     C* RIGA DI DETTAGLIO DELLA BOLLA
132600941215     C                   EXCEPT    DETT
132700010330     C*
132800010330     C                   SETOFF                                       97
132900990127     C**
133000990127     C* VEDO SE ESISTONO C.A. SULLA SPEDIZIONE
133100990127     C                   SETOFF                                       05
133200040809     C**""*KARB1         CHAIN     FNDCT02L                           89
133300990127     C**
133400040809    1C**""**IN89         DOWEQ     *OFF
133500040809     C**""*DCTATB        ANDNE     ' '
133600040809     C**""*KARB1         READE     FNDCT02L                               89
133700040809    1C**""*              ENDDO
133800040809      *
133900040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
134000040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
134100040809      * legate alla spedizione che passo
134200040809     c                   clear                   fidn12ds
134300040809     c                   eval      i12aas = ARBAAS
134400040809     c                   eval      i12lnp = ARBLNP
134500040809     c                   eval      i12nrs = ARBNRS
134600040809     c                   eval      i12nsp = ARBNSP
134700040809     c                   eval      i12fel = 'E'
134800040809     c                   eval      i12fan = 'E'
134900040809     c*
135000040809     c                   call      'FIDN12R'
135100040809     c                   parm                    fidn12ds
135200040809     c*
135300040809     c                   setoff                                       89
135400040809     c*
135500040809     c* se non ci sono errori
135600040809     c                   if        o12err <> ' '
135700040809     c                   seton                                        89
135800040809     c                   else
135900040809     c* se numero di CA trovate maggiore di zero
136000040809     c                   if        o12nca = 0
136100040809     c                   seton                                        89
136200040809     c                   else
136300040809     c                   eval      jj = 1
136400040809     c                   exsr      PREDN12R
136500040809     c                   endif
136600040809     c                   endif
136700040809     C**
136800990127     C  N89              SETON                                        05
136900941215     C*
137000960515     C* SE NECESSARIO, EMETTO UN'ALTRA RIGA DI DETTAGLIO
137100990127    1C     NOTE1         IFNE      *BLANKS                                      1° RIGA NOTE
137200960515     C     CONSE1        ORNE      *BLANKS                                      1° CONS.PARTIC.
137300960515     C     *IN96         OREQ      *ON                                          ANOMALIE
137400960515     C     *IN94         OREQ      *ON                                          CONS.PARZIALE
137500960515     C     *IN98         ANDEQ     *OFF
137600990127     C     *IN05         OREQ      *ON                                          CONS.PARZIALE
137700990127     C* PREPARO I DATI DELLA C.A.
137800990127     C   05              EXSR      PRECA
137900960517     C*
138000990127    2C     CONSE1        IFNE      *BLANKS
138100960517     C                   SETON                                        16
138200990127    2C                   ENDIF
138300040809     C*
138400941215     C                   EXCEPT    DETT1
138500990127     C*
138600990127     C* SE CI SONO ALTE C.A. STAMPO ANCORA
138700040809     C**""*KARB1         READE     FNDCT02L                               89
138800040809    2C**""**IN89         DOWEQ     *OFF
138900040809    3C**""*DCTATB        IFEQ      ' '
139000040809     C**""*              EXSR      PRECA
139100990127     C** STAMPO TESTATA E DETT C.A.
139200040809     C**""*   OF              EXCEPT    TESTA
139300040809     C**""*              EXCEPT    DETCA
139400040809    3C**""*              ENDIF
139500040809     C**""*KARB1         READE     FNDCT02L                               89
139600040809    2C**""*              ENDDO
139700040809      *
139800040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
139900040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
140000040809      * legate alla spedizione che passo
140100040809     c                   clear                   fidn12ds
140200040809     c                   eval      i12aas = ARBAAS
140300040809     c                   eval      i12lnp = ARBLNP
140400040809     c                   eval      i12nrs = ARBNRS
140500040809     c                   eval      i12nsp = ARBNSP
140600040809     c                   eval      i12fel = 'E'
140700040809     c                   eval      i12fan = 'E'
140800040809     c*
140900040809     c                   call      'FIDN12R'
141000040809     c                   parm                    fidn12ds
141100040809     c*
141200040809     c                   setoff                                       89
141300040809     c*
141400040809     c* se non ci sono errori
141500040809     c                   if        o12err <> ' '
141600040809     c                   seton                                        89
141700040809     c                   else
141800040809     c* se numero di CA trovate maggiore di zero
141900040809     c                   if        o12nca = 0
142000040809     c                   seton                                        89
142100040809     c                   else
142200040810     c                   add       1             jj
142300040809     c                   dow       jj <= o12nca and *in89 = *off
142400040809     c                   exsr      PREDN12R
142500040809     C                   EXSR      PRECA
142600040809     C** STAMPO TESTATA E DETT C.A.
142700040809     C   OF              EXCEPT    TESTA
142800040809     C                   EXCEPT    DETCA
142900040809     c*
143000040809     c                   add       1             jj
143100040809     c                   enddo
143200040809     c*
143300040809     c                   seton                                        89
143400040809     c*
143500040809     c                   endif
143600040809     c                   endif
143700040809     C**
143800990127    1C                   ENDIF
143900941215     C*
144000941215     C                   ENDSR
144100040809     C**************************************************************************
144200040809     C* PREPARO I DATI DELLA C.A. REPERENDOLI DALLA DS DEL *PGM UTILITA' FIDN12R
144300040809     C**************************************************************************
144400040809     C     PREDN12R      BEGSR
144500040809     C*
144600040809     C                   movea     skkey(jj)     dskey
144700040809     C                   eval      DCTFIL = dsfil
144800040809     C                   eval      DCTNCA = dsnca
144900040809     C                   eval      DCTDCH = skdch(jj)
145000040809     C                   eval      DCTCCH = skcch(jj)
145100040809     C*
145200040809     C* Aggancio il file FNDCT01L x reperire i dati relativi alla data data apertura CA
145300040809     C     KDCT1_C       chain     FNDCT01L
145400040809     C                   if        not %found(FNDCT01L)
145500040809     C                   eval      DCTAAC = *all'9'
145600040809     C                   eval      DCTMGC = *all'9'
145700040809     C                   endif
145800040809     C*
145900040809     C                   ENDSR
146000941214     C**************************************************************************
146100990127     C* PREPARO I DATI DELLA C.A.
146200941214     C**************************************************************************
146300990127     C     PRECA         BEGSR
146400990127     C                   SETOFF                                       06
146500990304     C                   CLEAR                   STACCH
146600990324     C                   CLEAR                   STACC2
146700990127     C* DATA APERTURA C.A.
146800990127     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
146900990127     C                   Z-ADD     *ZERO         G02DAT
147000990127     C                   MOVEL     '3'           G02ERR
147100990127     C                   CALL      'XSRDA8'
147200990127     C                   PARM                    WLBDAT
147300990127     C                   MOVE      G02DAT        W0020             2 0
147400990127     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
147500990127     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
147600990127     C** DATA CNHIUSUA C.A.
147700990127     C                   CLEAR                   STACHC
147800990127     C     DCTDCH        IFGT      0
147900990127     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
148000990127     C                   Z-ADD     *ZERO         G02DAT
148100990127     C                   MOVEL     '3'           G02ERR
148200990127     C                   CALL      'XSRDA8'
148300990127     C                   PARM                    WLBDAT
148400990127     C                   MOVE      G02DAT        W0020             2 0
148500990127     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
148600990127     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
148700990127     C                   SETON                                        06
148800990304     C* CAUSALE CHIUSURA
148900990304    2C     DCTCCH        IFNE      *BLANKS
149000990304     C                   CLEAR                   DCCH
149100990304     C                   CLEAR                   DSBS02
149200990304     C                   MOVEL     'C'           T02MOD
149300990304     C                   MOVEL     KNSIF         T02SIF
149400990304     C                   MOVEL     'CCH'         T02COD
149500990304     C                   MOVEL     DCTCCH        T02KE1
149600990304     C                   CALL      'TIBS02R'
149700990304     C                   PARM                    KPJBA
149800990304     C                   PARM                    DSBS02
149900990304     C*
150000990304    3C     T02ERR        IFEQ      ' '
150100990304     C                   MOVEL     T02UNI        DCCH
150200990304    3C                   ENDIF
150300990304     C                   MOVEL     §CHDES        STACCH           25
150400990324     C                   MOVEL     §CHDES        STACC2           15
150500990304    2C                   ENDIF
150600990127     C                   ENDIF
150700990127     C                   ENDSR
150800990127     C**************************************************************************
150900990127     C* ANALISI DETTAGLIO SEGNACOLLI
151000990127     C**************************************************************************
151100990127     C     DETS          BEGSR
151200941214     C*
151300920527     C                   Z-ADD     0             H                 2 0
151400920527     C                   Z-ADD     0             K                 2 0
151500920527     C                   Z-ADD     0             W                 2 0
151600920527     C                   Z-ADD     0             T                 2 0
151700950322     C                   Z-ADD     0             II                3 0
151800920527     C                   MOVEA     *ZEROS        SP1
151900920527     C                   MOVEA     *ZEROS        SP2
152000920527     C                   MOVEA     *ZEROS        SA1
152100920527     C                   MOVEA     *ZEROS        SA2
152200950321     C                   MOVEA     *ZEROS        NRS
152300950321     C                   MOVEA     *ZEROS        FGS
152400950321     C                   MOVEA     *ZEROS        NFV
152500950321     C                   MOVEA     *ZEROS        DFV
152600950322     C     *IN77         IFEQ      *OFF
152700950322    1C     ARBAMA        ANDEQ     0
152800950322     C     *IN77         OREQ      *ON
152900941214     C     KARB1         SETLL     FNART01L                           10
153000950322     C     KARB1         READE     FNART01L                               10
153100950321    2C     *IN10         DOWEQ     '0'
153200950322    3C     ARBAMP        IFEQ      0
153300950321    4C     ARTFLP        IFNE      *ZERO                                        FIL.TRANSITO
153400950321    5C     ARTDUT        IFEQ      0
153500920527     C                   ADD       1             H
153600950321    6C     H             IFGT      12
153700920527     C     H             SUB       12            K
153800950321    7C     K             IFGT      12
153900920527     C                   Z-ADD     12            K
154000920527     C                   Z-ADD     9999999       SP2(K)
154100920527     C                   SETON                                        10
154200920527     C                   ELSE
154300941215     C                   Z-ADD     ARTNSC        SP2(K)
154400950321    7C                   END
154500920527     C                   ELSE
154600941215     C                   Z-ADD     ARTNSC        SP1(H)
154700950321    6C                   END
154800950321    5C                   END
154900920527     C                   ELSE
155000950321    5C     ARTDFV        IFEQ      0
155100920527     C                   ADD       1             H
155200950321    6C     H             IFGT      12
155300920527     C     H             SUB       12            K
155400950321    7C     K             IFGT      12
155500920527     C                   Z-ADD     12            K
155600920527     C                   Z-ADD     9999999       SP2(K)
155700920527     C                   SETON                                        10
155800920527     C                   ELSE
155900941215     C                   Z-ADD     ARTNSC        SP2(K)
156000950321    7C                   END
156100920527     C                   ELSE
156200941215     C                   Z-ADD     ARTNSC        SP1(H)
156300950321    6C                   END
156400950321    5C                   END
156500950321    4C                   END
156600920527     C                   ELSE
156700950321    4C     ARTDAM        IFEQ      0
156800920527     C                   ADD       1             W
156900950321    5C     W             IFGT      12
157000920527     C     W             SUB       12            T
157100950321    6C     T             IFGT      12
157200920527     C                   Z-ADD     12            T
157300920527     C                   Z-ADD     9999999       SA2(T)
157400920527     C                   SETON                                        10
157500920527     C                   ELSE
157600941215     C                   Z-ADD     ARTNSC        SA2(T)
157700950321    6C                   END
157800920527     C                   ELSE
157900941215     C                   Z-ADD     ARTNSC        SA1(W)
158000950321    5C                   END
158100950321    4C                   END
158200950807    3C                   END
158300950321     C* SOLO SE RICHIESTA STAMPA SPEDIZIONI NON PARTITE verifico se
158400950321     C* il collo si trova in un altro F.V. non ancora abbinato ed in
158500950321     C* questo caso memorizzo nr.segnacollo, LNP/numero e data F.V. per
158600950321     C* poi stamparli nel caso di "COLLI PARTITI MA SPEDIZIONE NON ABI-
158700950321     C* LITATA".
158800950807    3C     *IN77         IFEQ      *ON
158900950322     C* Solo se colli partiti
159000950322     C     SP1(1)        ANDEQ     *ZEROS
159100950321     C     ARTABN        ANDNE     'S'
159200950321     C* Prendo l'ultima spunta (Potrei averne più di una in caso di
159300950321     C* disguido).
159400140618     C     KBRV3         SETLL     FNBRV05L
159500140618     C     KBRV3         READE     FNBRV05L                               89
159600950807     C*
159700950807    4C     *IN89         DOWEQ     *OFF
159800950807     C*
159900031215     C* RICHIAMO PGM PER CERCARE FOGLIO VIAGGIO DELLA BOLLA
160000031215     C                   CLEAR                   DSLV53
160100031215     C                   MOVEL     'A'           D53TFO
160200031215     C                   Z-ADD     brvNFV        D53NFV
160300031215     C                   MOVE      brvfgs        D53FGS
160400031215     C                   MOVEL     'A'           D53ABB
160500031215     C                   MOVE      brvLNA        D53LNA
160600031215     C**                 MOVEL     ARBTFA        D53TFA
160700031215     C                   CALL      'FNLV53R'
160800031215     C                   PARM                    DSLV53
160900031215     C*
161000031215     C* SE NON ci sono errori e foglio non abbinato memorizzo
161100031215     C* VERIFICO SE E' GIA' STATO PULITO
161200031215    1C     D53ERR        IFeq      *BLANKS
161300031216    6C     d53NFA        andeq     0
161400950321     C                   ADD       1             II
161500950321     C                   Z-ADD     ARTNSC        NRS(II)
161600950321     C                   Z-ADD     BRVFGS        FGS(II)
161700950321     C                   Z-ADD     BRVNFV        NFV(II)
161800031215     C                   MOVE      d53DFV        G02INV
161900950321     C                   MOVE      *ZEROS        G02DAT
162000950321     C                   MOVEL     3             G02ERR
162100950321     C                   CALL      'XSRDA8'
162200950321     C                   PARM                    WLBDAT
162300950321     C                   MOVEL     G02DAT        DFV(II)
162400950807     C                   SETON                                        89
162500950807     C*
162600950807   X6C                   ELSE
162700140618     C     KBRV3         READE     FNBRV05L                               89
162800950807    6C                   ENDIF
162900950807     C*
163000950807    4C                   ENDDO
163100950807    3C                   ENDIF
163200950321     C*
163300950322     C  N10KARB1         READE     FNART01L                               10
163400950807    2C                   ENDDO
163500950807    1C                   END
163600920527     C*
163700920527     C                   ENDSR
163800941214     C**************************************************************************
163900941214     C* CARICO L6
164000941214     C**************************************************************************
164100920907     C     CARL6         BEGSR
164200941214     C*
164300020422     c                   clear                   dsul06
164400020422     c                   eval      d06cod = '£6'
164500020422     c                   movel     parges        d06key
164600020422     c                   movel     dsul06        kpjbu
164700020422     c                   call      'TRUL06R'
164800020422     c                   parm                    kpjba
164900020422     c                   movel     kpjbu         dsul06
165000020422     c                   movel     lne           l6
165100941214     C*
165200920907     C                   ENDSR
165300950119     C*
165400991105     C* CONVERTO IL C/A NELLA DIVISA CORRENTE -----------------------*
165500950119     C     CTRCAS        BEGSR
165600991105     C                   CLEAR                   WCAS
165700950119     C*
165800950119    1C     STPCAS        IFGT      0
165900991104     C*
166000011025    2C     STPVCA        IFNE      §GEDCN
166100011025      *
166200991105     C                   CLEAR                   YEUR
166300991105     C                   MOVEL     STPVCA        YECDVI
166400011025     C                   MOVEL     §GEDCN        YECDVO
166500011025     C                   MOVE      §CVDEC        YECDCO
166600991105     C                   Z-ADD     STPCAS        YECIMI
166700991105     C*
166800991105     C                   CALL      'YEURCO'
166900991105     C                   PARM                    YEUR
167000991105     C*
167100991105    3C     YECESI        IFEQ      ' '
167200991105     C                   Z-ADD     YECIMO        WCAS
167300991105    3C                   ENDIF
167400991105     C*
167500991105   X2C                   ELSE
167600991105     C                   Z-ADD     STPCAS        WCAS
167700991105    2C                   ENDIF
167800991105    1C                   ENDIF
167900950119     C*
168000950119     C                   ENDSR
168100950621     C*
168200960517     C* CONTROLLO SE BOLLA PRESENTE IN IMA ---------------------------*
168300950621     C     CTRIMA        BEGSR
168400960517     C                   SETON                                        70
168500960517     C*
168600950621     C     KARB1         CHAIN     FNART000                           10
168700960517     C*
168800950621     C     *IN10         DOWEQ     *OFF
168900960517     C* SE PRESENTE ALMENO UN COLLO IN IMA, STAMPO LA BOLLA
169000000203     C                   Z-ADD     FVVNFV        WNFVBR
169100070118     C     KBRV          SETLL     FNBRV001                               33
169200960517     C*
169300960517     C     *IN33         IFEQ      *ON
169400960517     C                   SETON                                        10
169500960517     C                   SETOFF                                       70
169600960517     C                   ELSE
169700960517     C     KARB1         READE     FNART000                               10
169800960517     C                   ENDIF
169900950621     C                   ENDDO
170000950621     C*
170100950621     C                   ENDSR
170200960321     C**************************************************************************
170300960321     C     *INZSR        BEGSR
170400960321     C**************************************************************************
170500960321     C*
170600960321     C     *ENTRY        PLIST
170700960321     C                   PARM                    KPJBA
170800960321     C                   MOVEL     KPJBU         PARAM
170900960321     C                   Z-ADD     1             CODUT
171000960321     C*---------------------------------------------------------------
171100960321     C     KARB          KLIST
171200960321     C                   KFLD                    LIN(I)
171300960321     C                   KFLD                    VIDZNC            2 0
171400960321     C*
171500960321     C     KARB1         KLIST
171600960321     C                   KFLD                    ARBAAS
171700960321     C                   KFLD                    ARBLNP
171800960321     C                   KFLD                    ARBNRS
171900960321     C                   KFLD                    ARBNSP
172000991105     C     KAR6          KLIST
172100991104     C                   KFLD                    ARBAAS
172200991104     C                   KFLD                    ARBLNP
172300991104     C                   KFLD                    ARBNRS
172400991104     C                   KFLD                    ARBNSP
172500991104     C                   KFLD                    KTRC
172600991104     C     KARI          KLIST
172700991104     C                   KFLD                    ARBAAS
172800991104     C                   KFLD                    ARBLNP
172900991104     C                   KFLD                    ARBNRS
173000991104     C                   KFLD                    ARBNSP
173100991104     C                   KFLD                    KTIP
173200960321     C*
173300960321     C     KARB6         KLIST
173400960321     C                   KFLD                    $LNP
173500960321     C                   KFLD                    $NFV
173600960321     C*
173700960321     C     KLBL          KLIST
173800960321     C                   KFLD                    LBLAAP
173900960321     C                   KFLD                    LBLLPP
174000960321     C                   KFLD                    LBLNRP
174100960321     C                   KFLD                    LBLNSP
174200960321     C*
174300960321     C     KARB2         KLIST
174400960321     C                   KFLD                    LBLAAN
174500960321     C                   KFLD                    LBLLPN
174600960321     C                   KFLD                    LBLNRN
174700960321     C                   KFLD                    LBLNSN
174800960321     C*
174900960321     C     KTAB          KLIST
175000960321     C                   KFLD                    CODUT
175100960321     C                   KFLD                    TBLCOD
175200960321     C                   KFLD                    TBLKEY
175300960321     C*
175400960321     C     KFVA1         KLIST
175500960321     C                   KFLD                    BRVFGS
175600000203     C                   KFLD                    WNFV
175700990121     C                   KFLD                    PARGES
175800960321     C*
175900960321     C     KFVV          KLIST
176000960321     C                   KFLD                    IMANPG
176100960321     C                   KFLD                    DATEU
176200970513     C                   KFLD                    WLNP
176300960321     C*
176400960321     C     KAR4          KLIST
176500960321     C                   KFLD                    ARBAAS
176600960321     C                   KFLD                    ARBLNP
176700960321     C                   KFLD                    ARBNRS
176800960321     C                   KFLD                    ARBNSP
176900960321     C                   KFLD                    KTRC
177000960321     C*
177100960321     C     KBRV          KLIST
177200960321     C                   KFLD                    FVVNPG
177300000203     C                   KFLD                    WNFVBR
177400960321     C                   KFLD                    FVVFGS
177500960321     C                   KFLD                    ARTFLS
177600960321     C                   KFLD                    ARTLNA
177700960321     C                   KFLD                    ARTNRS
177800960321     C                   KFLD                    ARTNSC
177900960321     C     KBRV3         KLIST
178000960321     C                   KFLD                    WNPG
178100960321     C                   KFLD                    ARTFLS
178200960321     C                   KFLD                    ARTLNA
178300960321     C                   KFLD                    ARTNRS
178400960321     C                   KFLD                    ARTNSC
178500990304     C     KART          KLIST
178600990304     C                   KFLD                    ARTFLS
178700990304     C                   KFLD                    ARTLNA
178800990304     C                   KFLD                    ARTNRS
178900990304     C                   KFLD                    ARTNSC
179000990324     C     KTBE2         KLIST
179100990324     C                   KFLD                    KCOD
179200990324     C                   KFLD                    KLIN
179300990324     C                   KFLD                    KSIF
179400990324     C                   KFLD                    KKE1
179500990324     C                   KFLD                    KKE2
179600990324     C     KTBE22        KLIST
179700990324     C                   KFLD                    KCOD
179800990324     C                   KFLD                    KLIN
179900990324     C                   KFLD                    KSIF
180000990324     C     KDCT          KLIST
180100990324     C                   KFLD                    DCDAAC
180200990324     C                   KFLD                    DCDFIL
180300990324     C                   KFLD                    DCDNCA
180400030826      *
180500030826     c     K04cbb01      klist
180600030826     c                   kfld                    ARBaas
180700030826     c                   kfld                    ARBlnp
180800030826     c                   kfld                    ARBnrs
180900030826     c                   kfld                    ARBnsp
181000040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
181100040809     C     KDCT1_C       KLIST
181200040809     C                   KFLD                    dsaac
181300040809     C                   KFLD                    dsfil
181400040809     C                   KFLD                    dsnca
181500960321     C*
181600960321     C                   Z-ADD     1             WNPG
181700960321     C                   Z-ADD     3             IMANPG
181800991104     C                   MOVEL     'A'           KTIP
181900960321     C*
182000960321     C*---------------------------------------------------------------
182100960321     C* CARICO LE VARIABILI PARTENZE PER EVIDENZIALE I PESI O VOLUMI
182200960321     C* GRANDI
182300960321     C                   MOVEL     '3I'          TBLCOD
182400960321     C                   MOVEL(P)  '1'           TBLKEY
182500960321     C     KTAB          CHAIN     TABEL                              11
182600960321     C  N11              MOVEL     TBLUNI        DS3I
182700960321     C   11              CLEAR                   DS3I
182800960321     C*
182900960321     C* PRENDO GG PULIZIA FILE FNFVA
183000960321     C                   MOVEL     '5A'          TBLCOD
183100960321     C                   MOVEL     '1       '    TBLKEY
183200960321     C     KTAB          CHAIN     TABEL00F                           30
183300960321     C  N30              MOVEL     TBLUNI        DS5A
183400960321     C   30              CLEAR                   DS5A
183500960321     C*---------------------------------------------------------------
183600960321     C* VERIFICO SE E' STATA RICHIESTA LA STAMPA DELLE SPEDIZIONI NON
183700960321     C* PARTITE
183800960321     C     PARNFV        IFGT      *ZEROS
183900960321     C                   SETON                                        77
184000960321     C                   MOVE      PARLNP        $LNP              3 0
184100960321     C                   MOVE      PARNFV        $NFV              5 0
184200960321     C* CARICO L6 DELLE FILIALI GESTITE
184300960321     C                   EXSR      CARL6
184400960321     C                   ELSE
184500960321     C                   MOVEL     LIP           LIN
184600960321     C                   END
184700960321      *
184800960321     C                   CALL      'X§PARUT'
184900960321     C                   PARM                    UT§DSE
185000960321     C                   MOVEL     RAGUT         RSUT             20
185100960321     C                   MOVEL     REC80         CNCR80
185200960321     C*  UDATE
185300960321     C                   TIME                    W0140            14 0
185400960321     C* UDATE IN GGMMAAAA
185500960321     C                   MOVE      W0140         WDTGIO            8 0
185600960321     C* ORA
185700960321     C                   MOVEL     W0140         TIMES             6 0
185800960321     C*
185900960321     C* UDATE IN AAAAMMGG
186000960321     C                   Z-ADD     WDTGIO        G02DAT
186100960321     C                   MOVEL     *BLANK        G02ERR
186200960321     C                   CALL      'XSRDA8'
186300960321     C                   PARM                    WLBDAT
186400960321     C                   MOVEL     G02INV        DATEU             8 0
186500960321     C*
186600960321     C*---------------------------------------------------------------
186700960321     C*
186800960321     C* IMPOSTO E APRO IL FILE DI STAMPA
186900960321     C                   MOVEL     KQOTD         PRINT
187000960730     C*
187100960730     C                   CLEAR                   W0030
187200020522     c     simtpp        ifeq      '2'
187300020522     c     simtpp        oreq      *blanks
187400020522     c                   MOVEL     simpou        W0030             3 0
187500960730     C                   ENDIF
187600960730     C*
187700960730     C* FACCIO L'OVRDBF SE RICHIESTO
187800960321     C     PAROUT        IFNE      *BLANKS
187900960730     C*
188000121211     C* SE NON SONO UN 2°liv E NON STO LANCIANDO SIMFEL
188100020522     c     simtpp        ifne      '2'
188200020522     c     simtpp        andne     *blanks
188300960730     C     PARGES        ANDNE     SIMFEL
188400960730     C*
188500121211     C* SE     SONO UN 2°liv E     STO LANCIANDO SIMFEL
188600020522     c     simtpp        oreq      '2'
188700960730     C     PARGES        ANDEQ     SIMFEL
188800020522     c     simtpp        oreq      *blanks
188900020522     C     PARGES        ANDEQ     SIMFEL
189000960730     C*
189100121211     C* SE     SONO UN 2°liv MA NON STO LANCIANDO ME STESSO
189200020522     c     simtpp        oreq      '2'
189300960730     C     PARGES        ANDNE     W0030
189400020522     c     simtpp        oreq      *blanks
189500020522     C     PARGES        ANDNE     W0030
189600960730     C*
189700970707     C                   MOVEL     'T'           W004A             4
189800970707     C                   MOVE      PARGES        W004A
189900970707     C                   MOVEL(P)  W004A         TBLKEY
190000970707     C                   MOVEL     '5P'          TBLCOD
190100970707     C     KTAB          CHAIN     TABEL00F                           89
190200970707     C     *IN89         IFEQ      *OFF
190300970707     C                   MOVEL     TBLUNI        DS5P
190400970707     C                   MOVEL(P)  §5POTQ        PRINT            10            STAMPANTE
190500970707     C                   END
190600960321     C                   ENDIF
190700960730     C                   ENDIF
190800030826      *
190900030826if  1c                   if        PARfr = 'WRKF '
191000030826      *
191100030826     c                   open      WFCBB01L
191200030826      *
191300030826x   1c                   else
191400960730     C*
191500960321     C                   MOVEA     PRINT         CMD(49)                        OUTQ
191600960321     C*
191700960321     C     PARGES        IFNE      SIMFEL
191800960321     C     PARGES        ANDNE     *ZERO
191900971224     C                   MOVEL     PARGES        ALF3              3
192000960321     C                   MOVEA     ALF3          CMD(38)                        FORMTYPE
192100960321     C                   ENDIF
192200960321     C                   MOVEL     *BLANKS       COMMAN           80
192300960321     C                   MOVEA     CMD           COMMAN
192400960321     C                   Z-ADD     80            LUNG             15 5
192500960321     C                   CALL      'QCMDEXC'
192600960321     C                   PARM                    COMMAN
192700960321     C                   PARM                    LUNG
192800960510     C                   OPEN      PRTF198                                      PRINTER FILE
192900030826      *
193000030826e   1c                   endif
193100960321     C**
193200960321     C* CALCOLO UDATE - GG PULIZIA FVA
193300960321     C*
193400960321     C* PRENDO LA DATA DEL GIORNO DA TIME
193500960321     C                   TIME                    W0140            14 0
193600960321     C                   MOVE      W0140         UDATE8            8 0
193700960321SC   C* CON XSRDA8 TRASFORMO LA DATA ODIERNA IN NR.GIORNI (G02TGI)
193800960321     C                   MOVE      UDATE8        G02DAT
193900960321     C                   MOVEL     *BLANK        G02ERR
194000960321     C                   CALL      'XSRDA8'
194100960321     C                   PARM                    WLBDAT
194200960321     C*
194300960321      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
194400960321     C                   MOVE      *BLANKS       $DAT
194500960321     C                   MOVE      *BLANKS       $ERR
194600960321     C                   MOVE      *BLANKS       $PGM             10
194700960321     C                   MOVEL     'FNLRA7R'     $PGM                           BLP-FS61
194800960321     C                   CALL      'TRUL49C'                            99
194900960321     C                   PARM                    $PGM
195000000103     C                   PARM                    $DAT              8
195100960321     C                   PARM                    $ERR              1
195200960321      *
195300960321      * PROGRAMMA IN USO DATA ULTIMO UTILIZZO = DATA DEL GIORNO
195400960321     C     $ERR          IFNE      '1'
195500960321     C     *IN99         ANDLE     '1'
195600960321     C     $DAT          ANDNE     *BLANKS
195700960321     C                   MOVE      $DAT          G02INV
195800960321     C                   ENDIF
195900960321     C*
196000960321     C                   Z-ADD     *ZEROS        G02DAT
196100960321     C                   Z-ADD     *ZEROS        G02TGI
196200960321     C                   MOVEL     '3'           G02ERR
196300960321     C                   CALL      'XSRDA8'
196400960321     C                   PARM                    WLBDAT
196500960321      *
196600960321     C*
196700960321     C* SOTTRAGGO I GIORNI PULIZIA FOGLI VIAGGIO PARTENZA IN ARRIVO
196800960321     C     G02TGI        SUB       §5AFVA        GIOTGI
196900960321     C*
197000960321     C                   CALL      'XSRGI8'
197100960321     C                   PARM                    WGIDAT
197200960321     C*
197300960321     C                   Z-ADD     GIOINV        COMFVA            8 0
197400960321     C*
197500960516     C* GIRO LA DATA CONS.RICHIESTA E LA DATA FOGLIO ARRIVI RICHIESTE
197600960516     C*  RICHIESTE A VIDEO
197700960516     C     PARDFA        IFGT      0
197800960516     C                   SETON                                        17
197900960516     C                   Z-ADD     PARDFA        G02INV                         AAAA/MM/GG
198000960516     C                   Z-ADD     *ZERO         G02DAT
198100960516     C                   MOVEL     '3'           G02ERR
198200960516     C                   CALL      'XSRDA8'
198300960516     C                   PARM                    WLBDAT
198400960516     C                   Z-ADD     G02DAT        STPDFA                         GG/MM/AAAA
198500960516     C                   ENDIF
198600960516     C*
198700960516     C     PARDAT        IFGT      0
198800960517     C     PARDAT        ANDNE     99991231
198900960516     C                   SETON                                        18
199000960516     C                   Z-ADD     PARDAT        G02INV                         AAAA/MM/GG
199100960516     C                   Z-ADD     *ZERO         G02DAT
199200960516     C                   MOVEL     '3'           G02ERR
199300960516     C                   CALL      'XSRDA8'
199400960516     C                   PARM                    WLBDAT
199500960516     C                   Z-ADD     G02DAT        STPDAT                         GG/MM/AAAA
199600960516     C                   ENDIF
199700990324     C* SE ELENCO BOLLE DA CONSEGNARE CARICO LE CAUS.CHIUSURE C.A. CHE
199800990324     C*  CHE NON DEVO STAMPARE SE SOLO LORO NON CONSEGNATI
199900990324     C     *IN77         IFEQ      *OFF
200000990324     C                   CLEAR                   KCOD
200100990324     C                   CLEAR                   KLIN
200200990324     C                   MOVEL     KNSIF         KSIF
200300990324     C                   MOVEL     *ALL'0'       KKE1
200400990324     C                   MOVE      'CCH'         KKE1
200500990324     C                   CLEAR                   KKE2
200600990324     C     KTBE2         CHAIN     TNTBE02L                           89
200700990324     C     *IN89         IFEQ      *ON
200800990324     C     TBEATB        ORNE      ' '
200900990324     C                   CLEAR                   KSIF
201000990324     C                   ENDIF
201100990324     C                   MOVEL     'CCH'         KCOD
201200990324     C                   Z-ADD     1             XX                3 0
201300990324     C     KTBE22        CHAIN     TNTBE02L                           89
201400990324     C     *IN89         DOWEQ     *OFF
201500990324     C     TBEATB        IFEQ      ' '
201600990324     C                   MOVEL     TBEUNI        DCCH
201700990324     C     §CHR52        IFEQ      'S'
201800990324     C                   MOVEL     TBEKE1        CCH(XX)
201900990324     C                   ADD       1             XX
202000990324     C                   ENDIF
202100990324     C                   ENDIF
202200990324     C     KTBE22        READE     TNTBE02L                               89
202300990324     C                   ENDDO
202400990324     C                   ENDIF
202500991104     C**
202600991104     C* CARICO DIVISA CORRENTE
202700991104     C**
202800991104     C                   CLEAR                   DSBS02
202900991104     C                   MOVEL     'C'           T02MOD
203000991104     C                   MOVEL     KNSIF         T02SIF
203100991104     C                   MOVEL     'GED'         T02COD
203200991104     C                   MOVEL(P)  1             T02KE1
203300991104     C                   CALL      'TIBS02R'
203400991104     C                   PARM                    KPJBA
203500991104     C                   PARM                    DSBS02
203600991104    2C     T02ERR        IFEQ      ' '
203700991104     C                   MOVEL     T02UNI        DGED
203800991104     C                   ELSE
203900991104     C                   CLEAR                   DGED
204000991104     C                   ENDIF
204100011029      *
204200011029      * Recupero il numero di decimali di §GEDCN
204300011029     C                   MOVEL     'CV'          TBLCOD
204400011029     C                   MOVEL     *BLANKS       TBLKEY
204500011029     C                   MOVEL     §GEDCN        TBLKEY
204600011029     C     KTAB          CHAIN     TABEL                              31
204700011029     C  N31              MOVEL     TBLUNI        DSCV
204800011029     C   31              CLEAR                   DSCV
204900960321     C                   ENDSR
205000990304     C**************************************************************************
205100990304     C* ANALIZZO LA C.A. CHIUSA
205200990304     C**************************************************************************
205300990304     C     CACHIU        BEGSR
205400990304     C                   CLEAR                   DCCH
205500990304     C                   CLEAR                   WNOCON
205600990324     C*
205700990304     C     KARB1         SETLL     FNART01L
205800990304     C     KARB1         READE     FNART01L                               10
205900990324    1C     *IN10         DOWEQ     *OFF
206000990324    2C     ARTDCM        IFEQ      0
206100990304     C*
206200990304     C* VERIFICO SE E' SU C.A.
206300990304     C     KART          CHAIN     FNDCD02L                           32
206400990304     C*
206500990304     C* SE DETT.CA ANNULLATO O APERTO, IGNORO COME SE FOSSE CHIUSO
206600990304     C*  ALTRIMENTI GUARDO LA SUA CAUSALE CHIUSURA
206700990324    3C     DCDDCH        IFGT      0
206800990304     C     DCDATB        ANDEQ     ' '
206900990304     C                   CLEAR                   DCCH
207000990304     C*
207100990324     C                   SETOFF                                       89
207200990324    4C     DCDCCH        IFNE      *BLANKS
207300990324     C     DCDCCH        LOOKUP    CCH                                    89
207400990324    4C                   ENDIF
207500990304     C**
207600990304     C** COME SE NON FOSSE CONSEGNATO
207700990324    4C     *IN89         IFEQ      *OFF
207800990304     C                   MOVEL     'S'           WNOCON            1
207900990304     C                   SETON                                        10
208000990324   X4C                   ELSE
208100990324     C* VERIFICO IL TIPO ANOMALIA SE E' MANCANZA O AVARIE RESE O DISTR
208200990324     C* OK --> ALTRIMENTI STAMPO COME NON CONSEGNATA
208300990324     C     KDCT          CHAIN     FNDCT01L                           89
208400050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
208500050221     c                   If        Not *In89 and dctdt2 > 0
208600050221     c                   Eval      *In89 = *On
208700050221     c                   EndIf
208800990324    5C     *IN89         IFEQ      *OFF
208900990324     C     DCTATB        ANDEQ     ' '
209000990324     C                   CLEAR                   DSBS02
209100990324     C                   MOVEL     'C'           T02MOD
209200990324     C                   MOVEL     KNSIF         T02SIF
209300990324     C                   MOVEL     'TAD'         T02COD
209400990324     C                   MOVEL     DCTTAD        T02KE1
209500990324     C                   CALL      'TIBS02R'
209600990324     C                   PARM                    KPJBA
209700990324     C                   PARM                    DSBS02
209800990324     C**
209900990324    6C     T02ERR        IFEQ      ' '
210000990324     C                   MOVEL     T02UNI        DTAD
210100990324     C                   ELSE
210200990324     C                   MOVEL     'A'           §TARGR
210300990324    6C                   ENDIF
210400990324     C*
210500990324     C* SE LA C.A. E' AVARIA NON RESA NE DISTRUTTA O AMMANCO
210600990324     C*  DEVO STAMPARE COME NON CONSEGNATA
210700990324    6C     §TARGR        IFEQ      'V'
210800990324     C     DCTDSD        ANDNE     'R'
210900990324     C     DCTDSD        ANDNE     'D'
211000990324     C     §TARGR        OREQ      'A'
211100990324     C                   MOVEL     'S'           WNOCON            1
211200990324     C                   SETON                                        10
211300990324    6C                   ENDIF
211400990324     C*
211500990324     C* NON TROVATA TESTATA --> STAMPO
211600990324   X5C                   ELSE
211700990324     C                   MOVEL     'S'           WNOCON
211800990324     C                   SETON                                        10
211900990324    5C                   ENDIF
212000990324    4C                   ENDIF
212100990304     C**
212200990324   X3C                   ELSE
212300990304     C                   MOVEL     'S'           WNOCON
212400990324     C                   SETON                                        10
212500990324    3C                   ENDIF
212600990324    2C                   ENDIF
212700990304     C*
212800990304     C  N10KARB1         READE     FNART01L                               10
212900990324    1C                   ENDDO
213000990304     C                   ENDSR
213100030826      *****************************************************************
213200030826      * WriteWFCBB - Scrittura del record nel file di lavoro WFCBB    *
213300030826      *****************************************************************
213400030826     c     WriteWFCBB    BegSr
213500030826      *
213600030826     c     K04CBB01      chain     WFCBB000
213700030826      *
213800030826if  1c                   if        NOT %found(WFCBB01L)
213900030826     c                   clear                   WFCBB000
214000030826     c                   eval      CBBaas = ARBaas
214100030826     c                   eval      CBBlnp = ARBlnp
214200030826     c                   eval      CBBnrs = ARBnrs
214300030826     c                   eval      CBBnsp = ARBnsp
214400030917     c                   eval      CBBmgs = ARBmgs
214500030917     c                   eval      CBBncl = ARBncl
214600030917     c                   eval      CBBpkf = ARBpkf
214700030917     c                   eval      CBBvlf = ARBvlf
214800030917     c                   eval      CBBrsd = ARBrsd
214900030917     c                   eval      CBBind = ARBind
215000030917     c                   eval      CBBcad = ARBcad
215100030917     c                   eval      CBBlod = ARBlod
215200030917     c                   eval      CBBprd = ARBprd
215300030917     c                   eval      CBBnzd = ARBnzd
215400031001     c                   eval      CBBtcr = ARBtcr
215500031001     c                   eval      CBBdcr = ARBdcr
215600030826e   1c                   endif
215700030826      *
215800030826if  1c                   if        wErr   > *zeros
215900030826     c                   move      wErr          CBBerr
216000030826     c                   movel(p)  Msg(wErr)     CBBmsg
216100030826e   1c                   endif
216200030826      *
216300030826if  1c                   if        NOT %found(WFCBB01L)
216400030826     c                   WRITE     WFCBB000
216500030826x   1c                   else
216600030826     c                   UPDATE    WFCBB000
216700030826e   1c                   endif
216800030826      *
216900030826     c                   EndSr
217000030826      *****************************************************************
217100960510     OPRTF198   E            TESTA            02
217200960515     O              N77      STT(1)             132
217300960515     O              N77      STT(2)             198
217400960515     O               77      STT(14)            132
217500960515     O               77      STT(15)            198
217600911025     O                       RSUT                20
217700121211     O***                    SIMFEL              30
217800121211     O                       parlnp              30
217900960515     O                       WDTGIO             180 '  /  /    '
218000960515     O                       PAGE          Z    198
218100960510     O          E            TESTA            03
218200941215     O                       KNSIF               10
218300941215     O                       KNMUS               21
218400960515     O               77                         102 'FOGLIO VIAGGIO:'
218500920318     O               77      $LNP              +  1
218600920318     O               77                        +  0 '/'
218700920318     O               77      $NFV          Z   +  0
218800960515     O                       TIMES              178 '  :  :  '
218900960510     O          E            TESTA            04
219000911209     O               20      STT(11)             66
219100911209     O               30      STT(12)             66
219200911209     O               40      STT(13)             66
219300960516RM*  O               45      CINC                84
219400960516RM*  O               46      CNINC               84
219500960516     O               19      CIMA               114
219600960516     O               17                         140 'FINO A DATA F.ARRIVI:'
219700960516     O               17      STPDFA             150 '  /  /    '
219800960516     O               18                         177 'FINO A DATA CONS.RICH.:'
219900960516     O               18      STPDAT             187 '  /  /    '
220000121211     O****                   POSTE$             198
220100000803     O          E            TESTA            05
220200000803     O                       ALTRP1              18
220300000803     O                       ALTRP2              37
220400021205     o                       AltRp3             + 6
220500960516     O*
220600000803     O          E            TESTA            07
220700911025     O                       STT(3)              66
220800911025     O                       STT(4)             132
220900960515     O                       STT(5)             198
221000000803     O          E            TESTA            08
221100960515     O                       STT(6)              66
221200960515     O                       STT(7)             132
221300960515     O                       STT(8)             198
221400000803     O          E            TESTA            09
221500911025     O                       STT(9)              66
221600911025     O                       STT(10)            132
221700960515     O                       STT(10)            198
221800941215     O*-------------------------------------------------------------------------
221900960515     O* 1° RIGA
222000911025     O          E            DETT        1
222100941216     O                       ARBLNA               3
222200960515     O                                            4 '/'
222300960515     O                       ARBZNC               7
222400960515     O                       ARBNRS        Z     11
222500010330     O              N95N97   ARBNCD        Z     19
222600010330     O              N95N97                       20 '/'
222700010330     O              N95N97   ARBNCA        Z     27
222800960515     O               98                          27 'SOLO INCASSO'
222900010330     O               97                          27 'RECUPERO    '
223000960515     O                       ARBNCL        Z     33
223100960515     O                       ARBPKF        4     42
223200960515     O               44                          43 '*'
223300960515     O                       ARBVLF        4     50
223400960515     O               44                          51 '*'
223500960515     O                       STPRSD              67
223600030320     O                       ARBCAD              77
223700030320     O                       ARBPRD              80
223800030320     O                       ARBNZD              84
223900030320     O                       ARBGC1              87
224000030320     O                       ARBGC2              90
224100030320     O                       VIDCBL              93
224200051214     O                       STPtcr              95
224300051214     O                       STPGMC             101 '  /  '
224400051214     O                       STPHCR             107 '  :  '
224500051214     o               12                         109 'N'
224600960515     O                       ARBTSP             111
224700991105     O                       STPRSM             128
224800991105     O                       ARBLNP             133
224900991105     O                       ARBNRS        Z    136
225000991105     O                       ARBNSP        Z    144
225100991105     O                       STPDSP             153 '  /  /  '
225200991105     O               31                         156 'AB'
225300991105     O                       STPTIC         B   160
225400991105     O                       STPCAS        2B   178
225500991105     O                       STPVCA         B   182
225600991105     O                  01   STPIMD        2B   194
225700991105     O                 N01   STPIM0        2B   194
225800991105     O                       STPDIV             198
225900960515     O* RIBATTITURA RIGA
226000960515     O          E    44      DETT        0
226100960515     O                       ARBPKF        4     42
226200960515     O                                           43 '*'
226300960515     O                       ARBVLF        4     50
226400960515     O                                           51 '*'
226500960515     O* 2° RIGA
226600960515     O          E            DETT1       1
226700990324     O               05                             'C.A.'
226800990324     O               05      DCTFIL               8
226900990324     O               05      DCTNCA        Z     15
227000990324     O               05                          19 'del'
227100990324     O               05      STADCA              28 '  /  /  '
227200990324     O               05    06                    35 'chiusa'
227300990324     O               05    06STACHC              44 '  /  /  '
227400990324     O               05N96 06DCTCCH            +  1
227500990324     O               05N96 06                  +  0 '-'
227600990324     O               05N96 06STACC2            +  0
227700990324     O               96                          54 'ANOMALIE'
227800990324     O              N98 94                       63 ' PARZIALE'
227900990127     O                     16                    74 'Cons.Part.'
228000990127     O                       CONSE1             100
228100990127     O                       CONSE2             126
228200990127     O                       NOTE1              162
228300960517     O                       NOTE2              198
228400990127     O          E            DETCA       1
228500990127     O                                              'C.A.'
228600990127     O                       DCTFIL               8
228700990127     O                       DCTNCA        Z     15
228800990127     O                                           19 'del'
228900990127     O                       STADCA              28 '  /  /  '
229000990127     O                     06                    35 'chiusa'
229100990127     O                     06STACHC              44 '  /  /  '
229200990304     O                     06DCTCCH            +  1
229300990304     O                     06                  +  0 '-'
229400990304     O                     06STACCH            +  0
229500990127     O*-------------------------------------------------------------------------
229600941215     O          E            DETNSC      1
229700941215     O                       NSC                127 '       /'
229800941219     O*-------------------------------------------------------------------------
229900941219     O          E            MESSAG      1
230000950808     O                       STPMSG              66
230100941221     O          E            MESSAG      0  0
230200950808     O                       STPMSG              66
230300950808     O          E            MESSG2      0  0
230400950808     O                       STPMSG             132
230500950808     O          E            MESSG2      0  0
230600950808     O                       STPMSG             132
230700950321     O          E            DETSC       1
230800950322     O                                           13 'Segnacollo n.'
230900950321     O                       NRS(II)       Z   +  1
231000950321     O                                         +  2 'F.V.P.'
231100950321     O                       FGS(II)           +  1
231200950321     O                                         +  0 '/'
231300950321     O                       NFV(II)       Z   +  0
231400950322     O                                         +  2 'del'
231500950321     O                       DFV(II)       8   +  1
231600950322     O          E            DETSC1      0
231700950322     O                                           75 'Segnacollo n.'
231800950322     O                       NRS(II)       Z   +  1
231900950322     O                                         +  2 'F.V.P.'
232000950322     O                       FGS(II)           +  1
232100950322     O                                         +  0 '/'
232200950322     O                       NFV(II)       Z   +  0
232300950322     O                                         +  2 'del'
232400950322     O                       DFV(II)       8   +  1
232500950808     O          E            VUOTA       1
232600941215     O*-------------------------------------------------------------------------
232700911212     O          E            TOTALE      2
232800911025     O                       STT(9)              66
232900911025     O                       STT(10)            132
233000960515     O                       STT(10)            198
233100911212     O          E            TOTALE      1
233200941216     O                                           19 'TOTALI SPEDIZIONI :'
233300941216     O                       V06TSP        Z     25
233400991105     O                                           40 'COLLI :'
233500991105     O                       STPTCO        Z     47
233600991105     O                                           60 'PESO :'
233700991105     O                       V06PKF        4     70
233800991105     O                                           84 'VOLUME :'
233900991105     O                       V06VOL        4     93
234000991105     O                                          112 'DA INCASSARE :'
234100991105     O                       STPTOC        2    134
234200011025     O                       §GEDCN             139
234300941215     O*-------------------------------------------------------------------------
234400920527     O          E            NOAP1       1
234500950322     O                                           19 'Colli non abilitati'
234600920527     O                                         +  1 'dalla partenza:'
234700950322     O              N34      SP1                132 '        '
234800950322     O               34                          42 'TUTTI'
234900950322     O*-------------------------------------------------------------------------
235000950322     O          E            NOAP1X      1
235100950322     O                                           19 'Abilitaz.forz.ma co'
235200950322     O                                         +  0 'lli non partiti:'
235300950322     O              N34      SP1                132 '        '
235400950322     O               34                          42 'TUTTI'
235500941215     O*-------------------------------------------------------------------------
235600920527     O          E            NOAP2       1
235700930302     O              N34      SP2                132 '        '
235800941215     O*-------------------------------------------------------------------------
235900920527     O          E            NOAA1       1
236000950322     O                                           19 'Colli non abilitati'
236100920528     O                                         +  1 'all'' arrivo:'
236200950322     O              N35      SA1                132 '        '
236300950322     O               35                          42 'TUTTI'
236400941215     O*-------------------------------------------------------------------------
236500920527     O          E            NOAA2       1
236600930302     O              N35      SA2                132 '        '
236700941215     O*-------------------------------------------------------------------------
236800950808** STT
236900960515***   E L E N C O    B O L L E    D A    C O N S E G N A R E   ***
237000960515                        FNLR52R                           PAG.
237100030320LNA/ZONA SER   SEGNACOLLI   COLLI     PESO  VOLUME  DESTINATARIO
237200051214  C.A.P.    PR NAZ  GG   TBO  CONSEGNA  STA TP MITTENTE          S
237300991105 P E D I Z I O N E        TIPO / IMP.CONTRASSEGNO   IMP. FATTURA
237400991105
237500051214                   CHIUS      RICHIESTA DDT SER                 LN
237600991105P SER NUMERO   DATA            DA  INCASSARE        DA INCASSARE
237700911025------------------------------------------------------------------
237800911025------------------------------------------------------------------
237900960516RICHIESTE LE SPEDIZIONI ABILITATE ALLA CONSEGNA
238000960516RICHIESTE LE SPEDIZIONI NON ABILITATE ALLA CONSEGNA
238100960516RICHIESTE LE SPEDIZIONI ABILITATE E NON ALLA CONSEGNA
238200960515***  L I S T A   S P E D I Z I O N I   N O N   P A R T I T E  ***
238300960515                        FNLR52R                          PAG.
238400941215**  5678901234567890123456789012345678901234567890123456789012345678901234567890
238500960510OVRPRTF FILE(PRTF198) FORMTYPE('*STD    ') OUTQ(          ) USRDTA('Lista cons')
238600941219**
238700950322Colli partiti ma spedizione non abilitata
238800950808Colli partiti SU F.V.P. non abbinati
