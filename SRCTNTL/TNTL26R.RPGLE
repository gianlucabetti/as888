000100951212     H*------------------------------------------------------------------------*
000200951213     H* AGGIORNAMENTO DATE/ORE ARRIVO REALI SU GRIGLIA                         *
000300951212     H*------------------------------------------------------------------------*
000400951212     H DECEDIT('0,') DATEDIT(*DMY.)
000500951212     F*------------------------------------------------------------------------*
000600951212     FFNFV202L  UF   E           K DISK
000700951212     FTNTSR04L  UF   E           K DISK
000800040127     FTNTSR01L  UF   E           K DISK    RENAME(TNTSR000:TNTSR001)
000900040127     FTNTSR03L  IF   E           K DISK    RENAME(TNTSR000:TNTSR003)
001000960206     FAZORG01L  IF   E           K DISK
001100960206     FTABEL00F  IF   E           K DISK
001200040127     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)  USROPN
001300040127
001400960206     D PRT             S             66    DIM(15) CTDATA PERRCD(1)
001500120904     D CMD             S              1    DIM(71) CTDATA PERRCD(71)
001600960627     D MSG             S             48    DIM(1) CTDATA PERRCD(1)
001700960206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800960206     D KPJBA         E DS
001900960206     D WLBDA8          DS
002000960206     D  G08DAT                 1      8  0
002100960206     D  G08INV                 9     16  0
002200960206     D  G08ERR                17     17
002300960206     D  G08TGI                18     22  0
002400960206     D                 DS
002500960206     D  DSDATA                 1      8  0
002600960206     D  DSAA                   1      4  0
002700960206     D  DSMM                   5      6  0
002800960206     D  DSGG                   7      8  0
002900960320     ITNTSR003
003000960320     I              TSRATB                      SRATB3
003100960320     I              TSRTRN                      SRTRN3
003200960320     I              TSRDTN                      SRDTN3
003300960320     I              TSRDTT                      SRDTT3
003400960320     I              TSRTFM                      SRTFM3
003500960320     I              TSRNFP                      SRNFP3
003600960320     I              TSRFFP                      SRFFP3
003700960320     I              TSRNFA                      SRNFA3
003800960320     I              TSRFFA                      SRFFA3
003900960320     I              TSRDTP                      SRDTP3
004000960320     I              TSRHTP                      SRHTP3
004100960320     I              TSRDRP                      SRDRP3
004200960320     I              TSRHRP                      SRHRP3
004300960320     I              TSRRTP                      SRRTP3
004400960320     I              TSRDTA                      SRDTA3
004500960320     I              TSRHTA                      SRHTA3
004600960320     I              TSRDRA                      SRDRA3
004700960320     I              TSRHRA                      SRHRA3
004800960320     I              TSRRTA                      SRRTA3
004900960320     I              TSRDKP                      SRDKP3
005000960320     I              TSRFKP                      SRFKP3
005100960320     I              TSRDKA                      SRDKA3
005200960320     I              TSRFKA                      SRFKA3
005300960320     I              TSRDKI                      SRDKI3
005400960320     I              TSRFKI                      SRFKI3
005500960320     I              TSRLNP                      SRLNP3
005600960320     I              TSRNFV                      SRNFV3
005700960320     I              TSRDFV                      SRDFV3
005800951212     C*****************************************************************
005900951212     C*  RIEPILOGO INDICATORI
006000951212     C*****************************************************************
006100960206     C* 18    - DATA/ORA ARRIVO REALE RICEVUTA = DATA/ORA ARRIVO TEOR.
006200960206     C* 30    - COMODO
006300960320     C* 31    - COMODO
006400951212     C*****************************************************************
006500951212     C* ELABORO SOLO RECORD SFLAGGATI
006600951212     C     KFV2          SETLL     FNFV202L
006700951212     C     KFV2          READE     FNFV202L                               30
006800951212    1C     *IN30         DOWEQ     *OFF
006900960301     C                   CLEAR                   KTRN
007000960301     C                   CLEAR                   KDTN
007100960301     C                   CLEAR                   KTFM
007200951212     C*
007300951212     C     KTSR4         SETLL     TNTSR04L
007400951212     C     KTSR4         READE     TNTSR04L                               30
007500951212     C* FLAGGO IN OGNI CASO FV2 SE RECORD RELATIVO A FOGLIO VIAGGIO
007600951212     C* GIA' ABBINATO
007700951212     C  N30              MOVE      'S'           FV2ABN
007800951212     C  N30              UPDATE    FNFV2000
007900960301     C*
008000951212     C*
008100951212    2C     *IN30         DOWEQ     *OFF
008200960301     C                   CLEAR                   WAPP
008300960301     C*
008400960301     C                   MOVEL     TSRTRN        KTRN                           NUM TRAINO
008500960301     C                   MOVEL     TSRDTN        KDTN                           DATA TRAINO
008600960301     C                   MOVEL     TSRTFM        KTFM                           TIPO FERMATA
008700960301     C*
008800951212    3C     TSRFFA        IFEQ      FV2LAI
008900970328     C* Memorizzo se data/ora arrivo del FV = data/ora arrivo teorico
009000970328     C     FV2DPA        COMP      TSRDTA                                 18
009100970328     C   18FV2HPA        COMP      TSRHTA                                 18
009200960206     C* Controlli già effettuati: memorizzo se ci sono variazioni per
009300960206     C* poi stamparle e in questo caso azzero anche data controllo
009400960301    4C     TSRFKA        IFNE      ' '
009500970328     C* Non c'è ritardo e non c'era neanche prima --> non faccio niente
009600970328    5C     *IN18         IFEQ      *ON
009700970328     C     TSRDKA        ANDEQ     99999999
009800970328     C     TSRRTA        ANDEQ     *BLANKS
009900970328     C     FV2RTC        ANDEQ     *BLANKS
010000970328     C                   ELSE
010100960301    6C     FV2DPA        IFNE      TSRDRA
010200960226     C     FV2HPA        ORNE      TSRHRA
010300960226     C     FV2RTC        ORNE      TSRRTA
010400960206     C                   EXSR      PRTVAR
010500960206     C                   MOVE      *ZEROS        TSRDKA
010600960301    6C                   END
010700970328    5C                   END
010800960301    4C                   END
010900960229     C*§Se non c'è ritardo: imposto data controllo = 99999999 affinchè
011000960229     C*§questo record non venga elaborato dal pgm controllo traini per
011100960229     C*§la determinazione ritardo arrivo.
011200960229     C*§ 18                MOVE *HIVAL    TSRDKA
011300951212     C* AGGIORNO DATA/ORA ARRIVO SU GRIGLIA
011400951212     C                   MOVE      FV2DPA        TSRDRA
011500951212     C                   MOVE      FV2HPA        TSRHRA
011600960626     C* SE DALLA SEDE E' STATA IMMESSA, MEDIANTE MANUTENZIONE GRIGLIA,
011700960626     C* UNA CAUSALE NON VALIDA COME RITARDO (K) MANTENGO QUEST'ULTIMA
011800960626     C     TSRRTA        IFNE      'K'
011900951212     C                   MOVEL     FV2RTC        TSRRTA
012000960626     C                   END
012100951212     C                   UPDATE    TNTSR000
012200951212     C                   SETON                                        30
012300960301     C                   MOVEL     'S'           WAPP              1
012400951212   X3C                   ELSE
012500951212     C     KTSR4         READE     TNTSR04L                               30
012600951212    3C                   END
012700951212    2C                   ENDDO
012800960301     C**
012900960301     C* SE NON HO APPOGGIATO LA DATA, PROVO DI NUOVO LEGGENDO COL
013000960301     C*  NUMERO TRAINO
013100960301     C     WAPP          IFEQ      ' '
013200960301     C     KTRN          ANDGT     0
013300960301     C                   EXSR      RIPAPP
013400960301     C                   ENDIF
013500951212     C*
013600951212     C     KFV2          READE     FNFV202L                               30
013700951212    1C                   ENDDO
013800951212     C*
013900960206     C     WPRT          IFEQ      '1'
014000960206     C                   EXCEPT    FINES
014100960206     C                   END
014200951212     C                   SETON                                        LR
014300960301     C*
014400960301     C***************** RIPROVO AD APPOGGIARE *************************
014500960301     C     RIPAPP        BEGSR
014600960301     C     KTSR          SETLL     TNTSR01L
014700960301     C     KTSR          READE     TNTSR01L                               30
014800960301     C     *IN30         DOWEQ     *OFF
014900960301     C*
015000960301     C     TSRTFM        IFEQ      KTFM
015100960301     C     TSRFFA        ANDEQ     FV2LAI
015200960301     C* TROVATO!!!! --> LO APPOGGIO
015300970328     C     FV2DPA        COMP      TSRDTA                                 18
015400970328     C   18FV2HPA        COMP      TSRHTA                                 18
015500960301     C* Controlli già effettuati: memorizzo se ci sono variazioni per
015600960301     C* poi stamparle e in questo caso azzero anche data controllo
015700960301    4C     TSRFKA        IFNE      ' '
015800970328     C* Non c'è ritardo e non c'era neanche prima --> non faccio niente
015900970328    5C     *IN18         IFEQ      *ON
016000970328     C     TSRDKA        ANDEQ     99999999
016100970328     C     FV2RTC        ANDEQ     *BLANKS
016200970328     C     TSRRTA        ANDEQ     *BLANKS
016300970328     C                   ELSE
016400960301    6C     FV2DPA        IFNE      TSRDRA
016500960301     C     FV2HPA        ORNE      TSRHRA
016600960301     C     FV2RTC        ORNE      TSRRTA
016700960301     C                   EXSR      PRTVAR
016800960301     C                   MOVE      *ZEROS        TSRDKA
016900960301    6C                   END
017000970328    5C                   END
017100960301    4C                   END
017200960301     C*
017300960301     C                   MOVE      FV2DPA        TSRDRA
017400960301     C                   MOVE      FV2HPA        TSRHRA
017500960627     C* SE DALLA SEDE E' STATA IMMESSA, MEDIANTE MANUTENZIONE GRIGLIA,
017600960627     C* UNA CAUSALE NON VALIDA COME RITARDO (K) MANTENGO QUEST'ULTIMA
017700960626     C     TSRRTA        IFNE      'K'
017800960301     C                   MOVEL     FV2RTC        TSRRTA
017900960626     C                   END
018000960301     C                   UPDATE    TNTSR001
018100960301     C*
018200960301     C                   SETON                                        30
018300960301     C*
018400960301     C                   ELSE
018500960301     C*
018600960301     C     KTSR          READE     TNTSR01L                               30
018700960301     C                   ENDIF
018800960301     C                   ENDDO
018900960301     C                   ENDSR
019000951212     C*
019100960206     C*******************  STAMPA VARIAZIONE    ***********************
019200960206     C     PRTVAR        BEGSR
019300960320     C* STAMPO LA VARIAZIONE SOLO SE TUTTE LE TRATTE SONO STATE CONTROL
019400960320     C* LATE (FKA DIVERSI DA BLANKS)
019500960320     C                   EXSR      CHKFKA
019600960320    1C     WFKA          IFEQ      *BLANKS
019700960206     C* Rovescio data traino
019800960206     C                   CLEAR                   WLBDA8
019900960206     C                   MOVE      TSRDTN        G08INV
020000960206     C                   MOVEL     '3'           G08ERR
020100960206     C                   CALL      'XSRDA8'
020200960206     C                   PARM                    WLBDA8
020300960206     C                   MOVE      G08DAT        PRTDTN
020400960206     C* Decodifico filiale fermata partenza
020500960206     C     TSRFFP        CHAIN     AZORG01L                           30
020600960206     C  N30              MOVEL     ORGDES        PRTFFP           10
020700960206     C   30              CLEAR                   PRTFFP
020800960206     C* Decodifico filiale fermata arrivo
020900960206     C     TSRFFA        CHAIN     AZORG01L                           30
021000960206     C  N30              MOVEL     ORGDES        PRTFFA           10
021100960206     C   30              CLEAR                   PRTFFA
021200960206     C* Rovescio data f.v. teorica
021300960206     C                   CLEAR                   WLBDA8
021400960206     C                   MOVE      TSRDTT        G08INV
021500960206     C                   MOVEL     '3'           G08ERR
021600960206     C                   CALL      'XSRDA8'
021700960206     C                   PARM                    WLBDA8
021800960206     C                   MOVE      G08DAT        PRTDTT
021900960226     C* Data reale arrivo
022000960206     C                   CLEAR                   PRTDPR
022100960206     C                   CLEAR                   PRTDNW
022200960206     C     TSRDRA        IFGT      *ZEROS
022300960206     C                   MOVE      TSRDRA        DSDATA
022400960206     C                   MOVEL     DSGG          PRTDPR            4 0
022500960206     C                   MOVE      DSMM          PRTDPR
022600960206     C                   END
022700960206     C     FV2DPA        IFGT      *ZEROS
022800960206     C                   MOVE      FV2DPA        DSDATA
022900960206     C                   MOVEL     DSGG          PRTDNW            4 0
023000960206     C                   MOVE      DSMM          PRTDNW
023100960206     C                   END
023200960226     C* Causale ritardo arrivo
023300960206     C                   MOVE      *BLANKS       PRTCPR           25
023400960206     C                   MOVE      *BLANKS       PRTCNW           25
023500960206     C     TSRRTA        IFNE      *BLANKS
023600960206     C                   MOVEL     'TC'          KCOD
023700960206     C                   MOVEL(P)  TSRRTA        KKEY
023800960206     C     KTAB          CHAIN     TABEL00F                           30
023900960206     C  N30              MOVEL     TBLUNI        PRTCPR
024000960206     C                   END
024100960206     C     FV2RTC        IFNE      *BLANKS
024200960206     C                   MOVEL     'TC'          KCOD
024300960206     C                   MOVEL(P)  FV2RTC        KKEY
024400960206     C     KTAB          CHAIN     TABEL00F                           30
024500960206     C  N30              MOVEL     TBLUNI        PRTCNW
024600960206     C                   END
024700960627     C* STAMPO MESSAGGIO DI NON AGGIORNAMENTO CAUSALE RITARDO SE IN
024800960627     C* GRIGLIA C'ERA CAUSALE =  K-NON PRESENTE RITARDO PER SEDE
024900960627     C     TSRRTA        IFEQ      'K'
025000960627     C                   MOVEL     MSG(1)        PRTMSG           48
025100960627     C                   ELSE
025200960627     C                   CLEAR                   PRTMSG
025300960627     C                   END
025400960206     C     WPRT          IFEQ      ' '
025500960206     C                   MOVE      '1'           WPRT              1
025600960206     C                   END
025700960206     C   OF              EXCEPT    TESTA
025800960206     C                   EXCEPT    RIGVAR
025900960320    1C                   END
026000960206     C                   ENDSR
026100960320     C***************  CONTROLLO SE VARIAZIONE DA STAMPARE  ***********
026200960320     C     CHKFKA        BEGSR
026300960320     C                   MOVE      *BLANKS       WFKA              1
026400960320     C     KTSR3         SETLL     TNTSR03L
026500960320     C     KTSR3         READE     TNTSR03L                               31
026600960320     C     *IN31         DOWEQ     *OFF
026700960320     C     SRFKA3        IFEQ      *BLANKS
026800960320     C                   MOVE      '1'           WFKA
026900960320     C                   SETON                                        31
027000960320     C                   ELSE
027100960320     C     KTSR3         READE     TNTSR03L                               31
027200960320     C                   END
027300960320     C                   ENDDO
027400960320     C                   ENDSR
027500951212     C*******************  OPERAZIONI INIZIALI  ***********************
027600951212     C     *INZSR        BEGSR
027700960206     C     *ENTRY        PLIST
027800960206     C                   PARM                    KPJBA
027900960326     C* DETERMINO CODA PER INDIRIZZAMENTO STAMPE
028000960326     C                   MOVE      KNSIF         W004A             4
028100971217     C                   MOVEL     W004A         W001A             1
028200971217     C     W001A         IFEQ      'P'
028300040127     C                   MOVEL     'QPRINTS)'    WOUTQ            11
028400960326     C                   ELSE
028500140924     C                   MOVEL     'P046A156TR)' WOUTQ
028600960326     C                   END
028700960213     C* OVRPRTF SAVE(*YES)
028800960213     C                   CLEAR                   COMMAN
028900960326     C                   MOVEA     WOUTQ         CMD(28)
029000960326     C                   MOVEA     CMD           COMMAN           80
029100120904     C                   Z-ADD     71            LUNG             15 5
029200960213     C                   CALL      'QCMDEXC'                                                A
029300960213     C                   PARM                    COMMAN
029400960213     C                   PARM                    LUNG
029500960213     C                   OPEN      PRTF198
029600960206     C*
029700960206     C                   Z-ADD     1             CODUT
029800960206     C                   CALL      'X§PARUT'
029900960206     C                   PARM                    UT§DSE
030000960206     C                   MOVEL     RAGUT         RSUT             20
030100960206     C* Rilevo la data del giorno
030200960206     C                   TIME                    WTIME            14 0          ORA E DATA
030300960206     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
030400960206     C                   MOVEL     WTIME         WORA              6 0
030500960206     C*
030600951212     C     KFV2          KLIST
030700951212     C                   KFLD                    KABN
030800951212     C                   KFLD                    KEPA
030900960122     C                   KFLD                    KTDH
031000951212     C     KTSR4         KLIST
031100951212     C                   KFLD                    FV2LNP
031200951212     C                   KFLD                    FV2NFV
031300960320     C     KTSR3         KLIST
031400960320     C                   KFLD                    TSRTRN
031500960320     C                   KFLD                    TSRDTN
031600960206     C     KTAB          KLIST
031700960206     C                   KFLD                    CODUT
031800960206     C                   KFLD                    KCOD
031900960206     C                   KFLD                    KKEY
032000960301     C     KTSR          KLIST
032100960301     C                   KFLD                    KTRN
032200960301     C                   KFLD                    KDTN
032300951212     C*
032400951212     C     *LIKE         DEFINE    FV2ABN        KABN
032500951212     C     *LIKE         DEFINE    FV2EPA        KEPA
032600960122     C     *LIKE         DEFINE    FV2TDH        KTDH
032700960206     C     *LIKE         DEFINE    TSRDTN        PRTDTN
032800960206     C     *LIKE         DEFINE    TSRDTT        PRTDTT
032900960301     C     *LIKE         DEFINE    TSRDTN        KDTN
033000960301     C     *LIKE         DEFINE    TSRTRN        KTRN
033100960301     C     *LIKE         DEFINE    TSRTFM        KTFM
033200960206     C     *LIKE         DEFINE    TBLCOD        KCOD
033300960206     C     *LIKE         DEFINE    TBLKEY        KKEY
033400951212     C*
033500960206     C                   SETON                                        OF
033600951212     C                   CLEAR                   KABN
033700960206     C                   MOVEL     'A'           KEPA                           A=Arrivo
033800960206     C                   MOVEL     'R'           KTDH                           R=Reale
033900951212     C                   ENDSR
034000960206     OPRTF198   E            TESTA          1  1
034100960206     O                       PRT(1)              66
034200960206     O                       PRT(2)             132
034300960206     O                       PRT(3)             198
034400960206     O                       RSUT                20
034500960206     O                       WDATE         8    185
034600960206     O                       PAGE          Z    198
034700960206     O          E            TESTA          2
034800960206     O                       KNSIF               10
034900960206     O                       KNMUS               21
035000960206     O                       WORA               185 '  :  :  '
035100960206     O          E            TESTA          1
035200960206     O                       PRT(6)              66
035300960206     O                       PRT(9)             132
035400960206     O                       PRT(12)            198
035500960206     O          E            TESTA          2
035600960206     O                       PRT(7)              66
035700960206     O                       PRT(10)            132
035800960206     O                       PRT(13)            198
035900960206     O          E            RIGVAR         1
036000960206     O                       TSRTRN        Z      7
036100960206     O                       PRTDTN        8     18
036200960206     O                       TSRFFP              23
036300960206     O                       PRTFFP              34
036400960206     O                       TSRFFA              39
036500960206     O                       PRTFFA              50
036600960206     O                       PRTDTT        8     61
036700960226     O                       PRTDPR              68 '  / 0'
036800960226     O                       TSRHRA              74 '  : 0'
036900960226     O                       TSRRTA              77
037000960226     O                       PRTCPR             103
037100960226     O                       PRTDNW             113 '  / 0'
037200960226     O                       FV2HPA             119 '  : 0'
037300960226     O                       FV2RTC             122
037400960226     O                       PRTCNW             148
037500960627     O                       PRTMSG             198
037600960206     O          E            FINES       1  1
037700960206     O                       PRT(15)            132
037800960627**
037900960206XXXXXXXXXXXXXXXXXXXX                                         ** EL   1
038000960206ENCO VARIAZIONI DATI ARRIVO SU FOGLI VIAGGIO TEORICI GIA' CONTROLL   2
038100960206ATI **                         TNTL26R     XX/XX/XXXX    PAG. XXXX   3
038200960206XXXXXXXXXX XXXXXXXXXX                                                4
038300960206                                             XX:XX:XX                5
038400960206--- T R A I N O ---|F O G L I O  V I A G G I O  T E O R I C O|A R    6
038500961022 NUMERO    DATA    |P.O. PARTENZA   P.O. ARRIVO     DATA     |  DA   7
038600960206XXXXXXX XX/XX/XXXX  XXX-1234567890  XXX-1234567890 XX/XX/XXXX  XX/   8
038700960206R I V O  R E A L E  P R E C E D E N T E | --- A R R I V O  R E A L   9
038800960206TA  ORA   CAUSALE RITARDO/ANTICIPO      |  DATA  ORA   CAUSALE RIT  10
038900960206XX XX:XX  X 1234567890123456789012345     XX/XX XX:XX  X 123456789  11
039000960206 E  N U O V O --                                                    12
039100960206ARDO/ANTICIPO                                                       13
0392009602060123456789012345                                                    14
039300960206              ***   FINE   STAMPA   ***                             15
039400960213**
039500120904OVRPRTF FILE(PRTF198) OUTQ(             SAVE(*YES) USRDTA('Ric.Variaz')
039600960627**  MSG
039700960627NON AGGIORNATA CAUSALE RITARDO/ANTICIPO                              1
