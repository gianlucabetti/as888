000100000000     H**************************************************************************
000200000000     H*  Nome programma:  FNLR22R
000300041019     H*  Descrizione   :  Ricezione spunte partenze
000400000000     H*  Data creazione:  25 FEB 1993
000500000000     H*  2' genesi     :  19 GEN 1995
000600000000     H**************************************************************************
000700000000     FTABEL00F  IF   E           K DISK
000800070205     FFNBRV01L  UF   E           K DISK
000900070124     FFNBRV03L  UF   E           K DISK
001000070124     F                                     RENAME(FNBRV000:FNBRV003)
001100991231     FFISPP00R  UF   E           K DISK    USROPN
001200991231     F                                     RENAME(FISPP000:FISPP0RR)
001300000000     D*
001400021008     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)              FISPP00R
001500021008     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001600021008     D CMD4            S             55    DIM(1) CTDATA PERRCD(1)
001700000000     D CAT             S              1  0 DIM(20)                              CATEGORIE
001800000000     D NST             S              1    DIM(20)                              NR.SPUNTE GG.
001900000000     D WLBDAT          DS
002000000000     D  G02DAT                 1      8  0
002100000000     D  G02INV                 9     16  0
002200000000     D  G02ERR                17     17
002300000000     D  G02TGI                18     22  0
002400000000     D WGIDAT          DS
002500000000     D  GIODAT                 1      8  0
002600000000     D  GIOINV                 9     16  0
002700000000     D  GIOTGI                17     21  0
002800970509     D PARAM           DS
002900970509     D  D21TLA                 1      1
003000991231     D  D21REC                 2     68
003100991231     D  D21ERR                69     69
003200970819     D* CAMPO DI OUTPUT: SPUNTA DA NON CARICARE
003300991231     D  D21NOS                70     70
003400970819     D* CAMPO DI OUTPUT: S=LANCIARE PMG DI ABILITAIZONE BOLLE
003500991231     D  D21ABI                71     71
003600991231     D DSSPP         E DS                  EXTNAME(FISPP00R)
003700000000     D KPJBA         E DS
003800000000     D  LEGFIL               483    492
003900000000     D  MBRFIL               493    502
004000000000     D                 DS
004100040428     D* PER RIESPLOSIONE SPUNTE IN FILE DI RICEZIONE
004200040428     D PARAM5          DS
004300040428     D  PA5FIL               237    246    INZ('FXXX999999')
004400040428     D  PA5MBR4              247    250    INZ
004500040428     D  PA5MBR               247    256    Inz
004600960430     D                 DS
004700000000     D CNCR80        E DS
004800020220     D DS5A          E DS
004900000000     D DS7N          E DS
005000000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100040303     D KNFV            S                   LIKE(SPPNFV)
005200040303     D KFGS            S                   LIKE(SPPFGS)
005300000000     C**************************************************************************
005400041019     C*  20 22    DI COMODO
005500000000     C*  30
005600000000     C*  31       CICLO PRINCIPALE DI LETTURA ARCHIVI
005700000000     C*  32       DI COMODO
005800000000     C*  33
005900960527     C*  34       DI COMODO
006000070124     C*  40  ON   SCRITTURA NUOVA SPUNTA IN FNBRV
006100000000     C*  40  OFF  AGGIORNAMENTO SPUNTA GIA' ESISTENTE IN FNBRV
006200000000     C*  99       DI COMODO, UTILIZZABILE A PIACERE
006300000000     C**************************************************************************
006400000000     C     *ENTRY        PLIST
006500000000     C                   PARM                    KPJBA
006600000000     C*
006700000000     C                   Z-ADD     1             CODUT
006800020220     C                   CALL      'X§PARUT'
006900000000     C                   PARM                    UT§DSE
007000000000     C                   MOVEL     REC80         CNCR80
007100000000     C*---------------------------------------------------------------*
007200000000     C*
007300000000     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
007400000000     C* ASSUMO IL PRIMO MEMBRO DEL FILE
007500000000     C     MBRFIL        IFEQ      *BLANK
007600021008     C                   MOVEL     '*FIRST'      MBRFIL
007700000000     C                   END
007800000000     C* MEMBRO LUNGO 3
007900000000     C                   MOVEL     MBRFIL        COMODO            4
008000000000     C                   MOVE      COMODO        MBR               3 0
008100000000     C*
008200000000     C* ALLOCO MEMBRI
008300000000     C                   EXSR      CAEX
008400000000     C   20              GOTO      FINE
008500970509     C* CARICO TABELLE
008600000000     C                   EXSR      OPERAZ
008700000000     C*
008800000000     C* LEGGO TUTTE LE SPUNTE NEL FILE DI RICEZIONE
008900991231     C                   READ      FISPP0RR                               31
009000000000     C     *IN31         DOWEQ     '0'
009100000000     C*
009200000000     C****  SPPFLG = A-TERMINAL ARRIVO
009300041018     c* come spunta defluenza o terminal su altro AS non ricevo più niente
009400041018     c* quindi elimino routine
009500041018     C***  SPPFLG        IFEQ      'A'
009600960917     C* Le spunte di defluenza le tratto come spunte arrivi
009700041018     C***  SPPNPG        OREQ      1
009800041018     C***  SPPATR        ANDEQ     'F'
009900041018     C***                EXSR      ARRIVI
010000041018     C***                ELSE
010100000000     C*
010200041115     C     SPPFLG        IFne      'T'
010300041115     C**                 EXSR      TRANSI
010400041115     C**                 ELSE
010500000000     C*
010600000000     C                   EXSR      PARTEN
010700041115     C***                ENDIF
010800041115     C                   ENDIF
010900000000     C*
011000991231     C                   READ      FISPP0RR                               31
011100000000     C                   ENDDO
011200000000     C*
011300040428     c                   setoff                                       2022
011400000000     C*
011500000000     C     FINE          TAG
011600040428     c* Se allocazione, risottometto ricezione
011700040428     c                   if        not *in22  and *in20
011800040428     c
011900040428     c                   Eval      Pa5Mbr = MbrFil
012000040428     c                   Movel     Pa5Mbr4       PA5FIL
012100040428     C                   MOVEL     'K'           PA5FIL
012200040428     C                   MOVE      PARAM5        KPJBU
012300040428     c
012400040428     C                   MOVEL     'LR22'        KCOAZ
012500040428     C                   CALL      'BCH10'
012600040428     C                   PARM                    KPJBA
012700040428     c                   endif
012800040428     c
012900970509     C* CHIUDO PGM
013000970509     C     WFC21         IFEQ      'S'
013100970509     C                   MOVEL     'C'           D21TLA
013200970509     C                   MOVEL     PARAM         KPJBU
013300970509     C                   CALL      'FNLR21R'
013400970509     C                   PARM                    KPJBA
013500970509     C                   ENDIF
013600000000     C*
013700000000     C                   SETON                                        LR
013800000000     C**************************************************************************
013900000000     C* ?    S  P  U  N  T  E      P  A  R  T  E  N  Z  E    ?
014000000000     C**************************************************************************
014100000000     C     PARTEN        BEGSR
014200950128     C*
014300950128     C* APPOGGIO SUBITO TUTTE LE SPUNTE
014400950128     C*  SE SPPATR=A (ATTESA) DELETO SUBITO
014500950128     C*  SE NO IN ARCHIVIO DI APPOGGIO
014600970509     C                   MOVEL     DSSPP         D21REC
014700970509     C                   MOVEL     ' '           D21TLA
014800970819     C                   MOVEL     ' '           D21NOS
014900970819     C                   MOVEL     ' '           D21ABI
015000970509     C                   MOVEL     PARAM         KPJBU
015100970509     C                   CALL      'FNLR21R'
015200970509     C                   PARM                    KPJBA
015300970509     C                   MOVEL     KPJBU         PARAM
015400970509     C                   MOVEL     'S'           WFC21             1
015500000000     C*
015600970819     C* NON CARICO SE SPUNTA ATTESA TRANSITO OO SPUNTA DA NON CARICARE
015700970819     C*  PERCHE' ESISTE GIA' UN'ALTRA SPUNTA IN STESSA DATA CON ORDINAM
015800970819     C*  SPUNTA MINORE
015900950613    1C     SPPATR        IFNE      'A'
016000970819     C     D21NOS        ANDEQ     ' '
016100000000     C* LA SPUNTA E' GIA ESISTENTE ?
016200000000     C* SE SI, POSSO AVERE SPUNTE DOPPIE IN ARCHIVIO ?
016300000000     C                   EXSR      DOPPIE
016400000000     C                   EXSR      RIEMPI
016500950128     C*
016600070205     C** 40              WRITE     FNBRV000
016700040303     C**N40              UPDATE    FIBRV000
016800040303     C*
016900040303    2C  N40*IN41         IFEQ      *OFF
017000040303     C     NST(WA)       ORNE      '1'                                          NR.SPUNTE GG.
017100070124     C                   UPDATE    FNBRV000
017200040303     C                   ELSE
017300070124     C                   UPDATE    FNBRV003
017400040303    2C                   ENDIF
017500950613    1C                   ENDIF
017600000000     C*
017700000000     C* TENGO ANCHE LA SPUNTA A SE NON HO TROVATO LA RELATIVA BOLLA ARR
017800950128     C* NEL FILE DI RICEZIONE
017900970509     C* BOLLA TRANSITO --> CANCELLO SEMPRE
018000050322    1C     D21ERR        IFeq      ' '
018100070205     c     *in40         oreq      *on
018200991231     C                   DELETE    FISPP0RR
018300950613     C*
018400950613   X1C                   ELSE
018500950613     C*
018600950613     C* DOPO 15 GIORNO CANCELLO LO STESSO LA SPUNTA
018700950613    2C     SPPDFV        IFLE      DATPUL
018800991231     C                   DELETE    FISPP0RR
018900950613    2C                   ENDIF
019000950613    1C                   ENDIF
019100000000     C*
019200000000     C                   ENDSR
019300040303     C**************************************************************************
019400040303     C* CERCO SPUNTA DOPPIA PER P.O., SE CAT 4 , COME DA TABELLA 7N
019500070124     C*  CON LF FNBRV01L
019600040303     C* CERCO SPUNTA DOPPIA PER FOGLIO (NPG+NFV+FGS)
019700070124     C*  CON LF FNBRV03L
019800040303     C**************************************************************************
019900040303     C     DOPPIE        BEGSR
020000040303     C*
020100040303     C                   Z-ADD     SPPNPG        KNPG                           CATEGORIA
020200040303     C                   Z-ADD     SPPLNP        KLNP
020300040303     C                   Z-ADD     SPPLNA        KLNA
020400040303     C                   Z-ADD     SPPNRS        KNRS
020500040303     C                   Z-ADD     SPPNSC        KNSC
020600040303     C                   Z-ADD     SPPFGS        KFGS
020700040303     C                   Z-ADD     SPPNFV        KNFV
020800040303     C*
020900040303     C* VERIFICO IL FLAG NELLA DS7N
021000040303     C                   Z-ADD     1             WA                3 0
021100040303     C     SPPNPG        LOOKUP    CAT(WA)                                41     ON=TROVATO
021200040303     C     *IN41         IFEQ      *OFF
021300040303    7C     NST(WA)       ORNE      '1'                                          NR.SPUNTE GG.
021400070124     C     KBRV1         CHAIN     FNBRV01L                           40
021500040303     C                   ELSE
021600070124     C     KBRV3         CHAIN     FNBRV03L                           40
021700040303     C                   ENDIF
021800040303     c                   endsr
021900000000     C**************************************************************************
022000070124     C* MOVE I CAMPI DA FISPP00R A FNBRV09L
022100000000     C* SIA PER AGGIORNAMENTO CHE PER SCRITTURA
022200000000     C**************************************************************************
022300000000     C     RIEMPI        BEGSR
022400000000     C*
022500070124     C****               CLEAR                   FIBRV000
022600070124     C****               Z-ADD     SPPPES        BRVPES
022700070124     C****               Z-ADD     SPPFGS        BRVFGS
022800070124     C****               Z-ADD     SPPNPG        BRVNPG                         CATEGORIA
022900070124     C****               Z-ADD     SPPNFV        BRVNFV
023000070124     C****               Z-ADD     SPPLNP        BRVLNP
023100070124     C****               Z-ADD     SPPLNA        BRVLNA
023200070124     C****               Z-ADD     SPPNRS        BRVNRS
023300070124     C****               Z-ADD     SPPNSC        BRVNSC
023400070124     C****               Z-ADD     SPPZNC        BRVZNC
023500070124     C****               Z-ADD     SPPNPS        BRVNPS
023600070124     C****               MOVEL     SPPCAN        BRVCAN                         ANOMALIA
023700070124     C****               Z-ADD     SPPVUC        BRVVUC
023800070124     C****               Z-ADD     SPPPUC        BRVPUC
023900070124     C****               Z-ADD     SPPDCS        BRVDCS
024000070124     C****               Z-ADD     SPPHCS        BRVHCS
024100070124     C****               Z-ADD     SPPDFS        BRVDFS
024200070124     C****               Z-ADD     SPPHMS        BRVHMS
024300070206     C                   MOVEL     SPPATR        BRVATR
024400000000     C                   MOVEL     'R'           BRVFTR                         'R'=RICEZIONE
024500000000     C                   Z-ADD     DATEU         BRVDTR                         DATA RICEZIONE
024600991231     C                   MOVEL     WTIME         BRVHTR                         ORA  RICEZIONE
024700070124     C*****              Z-ADD     SPPFLE        BRVFLE
024800000000     C*
024900000000     C                   ENDSR
025000000000     C**************************************************************************
025100000000     C*  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI
025200000000     C**************************************************************************
025300000000     C     CAEX          BEGSR
025400040428     c                   setoff                                       22
025500000000     C*
025600021008     C                   MOVE      *BLANK        VAR11            11
025700021008     C                   MOVE      *BLANK        VAR12            12
025800000000     C                   MOVE      ')'           VAR11
025900021008     C                   MOVEL     MBRfil        VAR11
026000000000     C                   MOVE      '))'          VAR12
026100021008     C                   MOVEL     MBRfil        VAR12
026200021008     C                   Z-ADD     55            LUNG             15 5
026300000000     C*
026400000000     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
026500000000     C                   SETOFF                                       20
026600000000     C                   MOVEL     *BLANKS       COMMAN
026700991231     C                   MOVE      VAR11         CMD1                            FISPP00R
026800000000     C                   MOVEA     CMD1(1)       COMMAN           80
026900000000     C                   CALL      'QCMDEXC'                            20
027000000000     C                   PARM                    COMMAN
027100000000     C                   PARM                    LUNG
027200040428     c* Se non esiste membro non risottometto pgm
027300040428     c                   if        *in20
027400040428     c                   seton                                        22
027500040428     c                   endif
027600000000     C*
027700000000     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
027800000000     C  N20              DO
027900961216     C                   MOVEL     *BLANKS       COMMAN
028000991231     C                   MOVE      VAR12         CMD2                           FISPP00R
028100961216     C                   MOVEA     CMD2(1)       COMMAN
028200000000     C                   CALL      'QCMDEXC'                            20
028300000000     C                   PARM                    COMMAN
028400000000     C                   PARM                    LUNG
028500000000     C* OVRDBF
028600041018     C  N20              do
028700961216     C                   MOVEL     *BLANKS       COMMAN
028800991231     C                   MOVE      VAR11         CMD4                           FISPP00R
028900961216     C                   MOVEA     CMD4(1)       COMMAN
029000000000     C                   CALL      'QCMDEXC'                            20
029100000000     C                   PARM                    COMMAN
029200000000     C                   PARM                    LUNG
029300041018     C                   ENDdo
029400000000     C*
029500000104     C  N20              OPEN      FISPP00R
029600000000     C*
029700000000     C                   END
029800000000     C*
029900000000     C                   ENDSR
030000000000     C**************************************************************************
030100000000     C*  O P E R A Z I O N I    I N I Z I A L I
030200000000     C**************************************************************************
030300000000     C     OPERAZ        BEGSR
030400000000     C*
030500000000     C* GIRO DATA DEL GIORNO
030600000000     C                   TIME                    WTIME            14 0          ORA & DATA
030700000000     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
030800000000     C                   Z-ADD     WDATE         G02DAT
030900000000     C                   MOVEL     *BLANK        G02ERR
031000000000     C                   CALL      'XSRDA8'
031100000000     C                   PARM                    WLBDAT
031200000000     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
031300000000     C                   Z-ADD     G02TGI        WGIORN
031400000000     C*
031500000000     C* SERVE SAPERE SE NELLE VARIE CATEGORIE FOGLI POSSO
031600000000     C* AVERE SPUNTE DOPPIE OPPURE NO
031700000000     C                   Z-ADD     *ZERO         WA                             CONTATORE
031800000000     C                   MOVEL     '7N'          COD
031900000000     C     KTAB2         SETLL     TABEL00F
032000000000     C     KTAB2         READE     TABEL00F                               30
032100000000     C     *IN30         DOWEQ     *OFF
032200000000     C     TBLFLG        IFEQ      *BLANKS
032300000000     C                   ADD       1             WA
032400000000     C                   MOVEL     TBLUNI        DS7N
032500000000     C                   MOVEL     TBLKEY        CAT(WA)
032600000000     C                   MOVEL     §7NNST        NST(WA)
032700000000     C                   ENDIF
032800000000     C     KTAB2         READE     TABEL00F                               30
032900000000     C                   ENDDO
033000000000     C*
033100000000     C* CARICO LA DS5A (NR. GIORNI DI PULIZIA ARCHIVI)
033200000000     C                   MOVEL     '5A'          COD
033300000000     C                   MOVEL     '1       '    KEY
033400000000     C     KTAB          CHAIN     TABEL00F                           30
033500000000     C  N30              MOVEL     TBLUNI        DS5A
033600000000     C   30              CLEAR                   DS5A
033700000000     C*
033800950328     C* SOTRAGGO I GIORNI PULIZIA SPUNTE
033900000000     C     WGIORN        SUB       §5ASPP        GIOTGI
034000000000     C                   CALL      'XSRGI8'
034100000000     C                   PARM                    WGIDAT
034200000000     C                   Z-ADD     GIOINV        DATPUL            8 0          AAAA/MM/GG
034300000000     C*
034400000000     C                   ENDSR
034500000000     C**************************************************************************
034600000000     C* R O U T I N E    I N I Z I A L E
034700000000     C**************************************************************************
034800000000     C     *INZSR        BEGSR
034900000000     C*
035000070124     C     KBRV1         KLIST                                                  FNBRV09L
035100000000     C                   KFLD                    KNPG                           ..CATEGORIA
035200040303     C                   KFLD                    KNFV                           ..CATEGORIA
035300040303     C                   KFLD                    KFGS                           ..FIL.GESTIONE
035400000000     C                   KFLD                    KLNP                           ..LNP
035500000000     C                   KFLD                    KLNA                           ..LNA
035600000000     C                   KFLD                    KNRS                           ..SERIE
035700000000     C                   KFLD                    KNSC                           ..SEGNACOLLO
035800070124     C     KBRV3         KLIST                                                  FNBRV09L
035900040303     C                   KFLD                    KNPG                           ..CATEGORIA
036000040303     C                   KFLD                    KLNP                           ..LNP
036100040303     C                   KFLD                    KLNA                           ..LNA
036200040303     C                   KFLD                    KNRS                           ..SERIE
036300040303     C                   KFLD                    KNSC                           ..SEGNACOLLO
036400040303     C                   KFLD                    KFGS                           ..FIL.GESTIONE
036500000000     C     KTAB          KLIST
036600000000     C                   KFLD                    CODUT
036700000000     C                   KFLD                    COD
036800000000     C                   KFLD                    KEY
036900000000     C     KTAB2         KLIST
037000000000     C                   KFLD                    CODUT
037100000000     C                   KFLD                    COD
037200000000     C*
037300020220     C     *LIKE         DEFINE    G02TGI        WGIORN
037400950122     C     *LIKE         DEFINE    TBLCOD        COD
037500000000     C     *LIKE         DEFINE    TBLKEY        KEY
037600000000     C     *LIKE         DEFINE    BRVNPG        KNPG                           CATEGORIA FOGLI
037700000000     C     *LIKE         DEFINE    BRVLNP        KLNP                           L N P
037800000000     C     *LIKE         DEFINE    BRVLNA        KLNA                           L N A
037900000000     C     *LIKE         DEFINE    BRVNRS        KNRS                           SERIE
038000000000     C     *LIKE         DEFINE    BRVNSC        KNSC                           SEGNACOLLO
038100000000     C     *LIKE         DEFINE    BRVFLE        KFLE                           FIL.ELABORATORE
038200990623     C                   Z-ADD     0             COMDFS            8 0
038300990623     C                   Z-ADD     0             COMHMS            4 0
038400000000     C*
038500000000     C                   ENDSR
038600000000     O**************************************************************************
038700961216**         CMD1
038800021008CHKOBJ OBJ(FISPP00R) OBJTYPE(*FILE)     MBR(M234567890)
038900000000**         CMD2
039000021008ALCOBJ OBJ((FISPP00R *FILE *EXCL           M234567890))
039100000000**         CMD4
039200021008OVRDBF FILE(FISPP00R)                   MBR(M234567890)
