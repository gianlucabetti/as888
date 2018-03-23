000100000000     H**************************************************************************
000200000000     H*  Nome programma:  FNLR22R
000300041019     H*  Descrizione   :  Ricezione spunte partenze
000400000000     H*  Data creazione:  25 FEB 1993
000500000000     H*  2' genesi     :  19 GEN 1995
000600000000     H**************************************************************************
000700000000     FTABEL00F  IF   E           K DISK
000800070205     FFNBRV01L  UF   E           K DISK
000900140618     FFNBRV05L  UF   E           K DISK
001000140618     F                                     RENAME(FNBRV000:FNBRV005)
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
016500040303     C*
016600040303    2C  N40*IN41         IFEQ      *OFF
016700040303     C     NST(WA)       ORNE      '1'                                          NR.SPUNTE GG.
016800070124     C                   UPDATE    FNBRV000
016900040303     C                   ELSE
017000140618     C                   UPDATE    FNBRV005
017100040303    2C                   ENDIF
017200950613    1C                   ENDIF
017300000000     C*
017400000000     C* TENGO ANCHE LA SPUNTA A SE NON HO TROVATO LA RELATIVA BOLLA ARR
017500950128     C* NEL FILE DI RICEZIONE
017600970509     C* BOLLA TRANSITO --> CANCELLO SEMPRE
017700050322    1C     D21ERR        IFeq      ' '
017800070205     c     *in40         oreq      *on
017900991231     C                   DELETE    FISPP0RR
018000950613     C*
018100950613   X1C                   ELSE
018200950613     C*
018300950613     C* DOPO 15 GIORNO CANCELLO LO STESSO LA SPUNTA
018400950613    2C     SPPDFV        IFLE      DATPUL
018500991231     C                   DELETE    FISPP0RR
018600950613    2C                   ENDIF
018700950613    1C                   ENDIF
018800000000     C*
018900000000     C                   ENDSR
019000040303     C**************************************************************************
019100040303     C* CERCO SPUNTA DOPPIA PER P.O., SE CAT 4 , COME DA TABELLA 7N
019200070124     C*  CON LF FNBRV01L
019300040303     C* CERCO SPUNTA DOPPIA PER FOGLIO (NPG+NFV+FGS)
019400140618     C*  CON LF FNBRV05L
019500040303     C**************************************************************************
019600040303     C     DOPPIE        BEGSR
019700040303     C*
019800040303     C                   Z-ADD     SPPNPG        KNPG                           CATEGORIA
019900040303     C                   Z-ADD     SPPLNP        KLNP
020000040303     C                   Z-ADD     SPPLNA        KLNA
020100040303     C                   Z-ADD     SPPNRS        KNRS
020200040303     C                   Z-ADD     SPPNSC        KNSC
020300040303     C                   Z-ADD     SPPFGS        KFGS
020400040303     C                   Z-ADD     SPPNFV        KNFV
020500040303     C*
020600040303     C* VERIFICO IL FLAG NELLA DS7N
020700040303     C                   Z-ADD     1             WA                3 0
020800040303     C     SPPNPG        LOOKUP    CAT(WA)                                41     ON=TROVATO
020900040303     C     *IN41         IFEQ      *OFF
021000040303    7C     NST(WA)       ORNE      '1'                                          NR.SPUNTE GG.
021100070124     C     KBRV1         CHAIN     FNBRV01L                           40
021200040303     C                   ELSE
021300140618     C     KBRV3         CHAIN     FNBRV05L                           40
021400040303     C                   ENDIF
021500040303     c                   endsr
021600000000     C**************************************************************************
021700070124     C* MOVE I CAMPI DA FISPP00R A FNBRV09L
021800000000     C* SIA PER AGGIORNAMENTO CHE PER SCRITTURA
021900000000     C**************************************************************************
022000000000     C     RIEMPI        BEGSR
022100000000     C*
022200070124     C****               CLEAR                   FIBRV000
022300070124     C****               Z-ADD     SPPPES        BRVPES
022400070124     C****               Z-ADD     SPPFGS        BRVFGS
022500070124     C****               Z-ADD     SPPNPG        BRVNPG                         CATEGORIA
022600070124     C****               Z-ADD     SPPNFV        BRVNFV
022700070124     C****               Z-ADD     SPPLNP        BRVLNP
022800070124     C****               Z-ADD     SPPLNA        BRVLNA
022900070124     C****               Z-ADD     SPPNRS        BRVNRS
023000070124     C****               Z-ADD     SPPNSC        BRVNSC
023100070124     C****               Z-ADD     SPPZNC        BRVZNC
023200070124     C****               Z-ADD     SPPNPS        BRVNPS
023300070124     C****               MOVEL     SPPCAN        BRVCAN                         ANOMALIA
023400070124     C****               Z-ADD     SPPVUC        BRVVUC
023500070124     C****               Z-ADD     SPPPUC        BRVPUC
023600070124     C****               Z-ADD     SPPDCS        BRVDCS
023700070124     C****               Z-ADD     SPPHCS        BRVHCS
023800070124     C****               Z-ADD     SPPDFS        BRVDFS
023900070124     C****               Z-ADD     SPPHMS        BRVHMS
024000070206     C                   MOVEL     SPPATR        BRVATR
024100000000     C                   MOVEL     'R'           BRVFTR                         'R'=RICEZIONE
024200000000     C                   Z-ADD     DATEU         BRVDTR                         DATA RICEZIONE
024300991231     C                   MOVEL     WTIME         BRVHTR                         ORA  RICEZIONE
024400070124     C*****              Z-ADD     SPPFLE        BRVFLE
024500000000     C*
024600000000     C                   ENDSR
024700000000     C**************************************************************************
024800000000     C*  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI
024900000000     C**************************************************************************
025000000000     C     CAEX          BEGSR
025100040428     c                   setoff                                       22
025200000000     C*
025300021008     C                   MOVE      *BLANK        VAR11            11
025400021008     C                   MOVE      *BLANK        VAR12            12
025500000000     C                   MOVE      ')'           VAR11
025600021008     C                   MOVEL     MBRfil        VAR11
025700000000     C                   MOVE      '))'          VAR12
025800021008     C                   MOVEL     MBRfil        VAR12
025900021008     C                   Z-ADD     55            LUNG             15 5
026000000000     C*
026100000000     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
026200000000     C                   SETOFF                                       20
026300000000     C                   MOVEL     *BLANKS       COMMAN
026400991231     C                   MOVE      VAR11         CMD1                            FISPP00R
026500000000     C                   MOVEA     CMD1(1)       COMMAN           80
026600000000     C                   CALL      'QCMDEXC'                            20
026700000000     C                   PARM                    COMMAN
026800000000     C                   PARM                    LUNG
026900040428     c* Se non esiste membro non risottometto pgm
027000040428     c                   if        *in20
027100040428     c                   seton                                        22
027200040428     c                   endif
027300000000     C*
027400000000     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
027500000000     C  N20              DO
027600961216     C                   MOVEL     *BLANKS       COMMAN
027700991231     C                   MOVE      VAR12         CMD2                           FISPP00R
027800961216     C                   MOVEA     CMD2(1)       COMMAN
027900000000     C                   CALL      'QCMDEXC'                            20
028000000000     C                   PARM                    COMMAN
028100000000     C                   PARM                    LUNG
028200000000     C* OVRDBF
028300041018     C  N20              do
028400961216     C                   MOVEL     *BLANKS       COMMAN
028500991231     C                   MOVE      VAR11         CMD4                           FISPP00R
028600961216     C                   MOVEA     CMD4(1)       COMMAN
028700000000     C                   CALL      'QCMDEXC'                            20
028800000000     C                   PARM                    COMMAN
028900000000     C                   PARM                    LUNG
029000041018     C                   ENDdo
029100000000     C*
029200000104     C  N20              OPEN      FISPP00R
029300000000     C*
029400000000     C                   END
029500000000     C*
029600000000     C                   ENDSR
029700000000     C**************************************************************************
029800000000     C*  O P E R A Z I O N I    I N I Z I A L I
029900000000     C**************************************************************************
030000000000     C     OPERAZ        BEGSR
030100000000     C*
030200000000     C* GIRO DATA DEL GIORNO
030300000000     C                   TIME                    WTIME            14 0          ORA & DATA
030400000000     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
030500000000     C                   Z-ADD     WDATE         G02DAT
030600000000     C                   MOVEL     *BLANK        G02ERR
030700000000     C                   CALL      'XSRDA8'
030800000000     C                   PARM                    WLBDAT
030900000000     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
031000000000     C                   Z-ADD     G02TGI        WGIORN
031100000000     C*
031200000000     C* SERVE SAPERE SE NELLE VARIE CATEGORIE FOGLI POSSO
031300000000     C* AVERE SPUNTE DOPPIE OPPURE NO
031400000000     C                   Z-ADD     *ZERO         WA                             CONTATORE
031500000000     C                   MOVEL     '7N'          COD
031600000000     C     KTAB2         SETLL     TABEL00F
031700000000     C     KTAB2         READE     TABEL00F                               30
031800000000     C     *IN30         DOWEQ     *OFF
031900000000     C     TBLFLG        IFEQ      *BLANKS
032000000000     C                   ADD       1             WA
032100000000     C                   MOVEL     TBLUNI        DS7N
032200000000     C                   MOVEL     TBLKEY        CAT(WA)
032300000000     C                   MOVEL     §7NNST        NST(WA)
032400000000     C                   ENDIF
032500000000     C     KTAB2         READE     TABEL00F                               30
032600000000     C                   ENDDO
032700000000     C*
032800000000     C* CARICO LA DS5A (NR. GIORNI DI PULIZIA ARCHIVI)
032900000000     C                   MOVEL     '5A'          COD
033000000000     C                   MOVEL     '1       '    KEY
033100000000     C     KTAB          CHAIN     TABEL00F                           30
033200000000     C  N30              MOVEL     TBLUNI        DS5A
033300000000     C   30              CLEAR                   DS5A
033400000000     C*
033500950328     C* SOTRAGGO I GIORNI PULIZIA SPUNTE
033600000000     C     WGIORN        SUB       §5ASPP        GIOTGI
033700000000     C                   CALL      'XSRGI8'
033800000000     C                   PARM                    WGIDAT
033900000000     C                   Z-ADD     GIOINV        DATPUL            8 0          AAAA/MM/GG
034000000000     C*
034100000000     C                   ENDSR
034200000000     C**************************************************************************
034300000000     C* R O U T I N E    I N I Z I A L E
034400000000     C**************************************************************************
034500000000     C     *INZSR        BEGSR
034600000000     C*
034700070124     C     KBRV1         KLIST                                                  FNBRV09L
034800000000     C                   KFLD                    KNPG                           ..CATEGORIA
034900040303     C                   KFLD                    KNFV                           ..CATEGORIA
035000040303     C                   KFLD                    KFGS                           ..FIL.GESTIONE
035100000000     C                   KFLD                    KLNP                           ..LNP
035200000000     C                   KFLD                    KLNA                           ..LNA
035300000000     C                   KFLD                    KNRS                           ..SERIE
035400000000     C                   KFLD                    KNSC                           ..SEGNACOLLO
035500070124     C     KBRV3         KLIST                                                  FNBRV09L
035600040303     C                   KFLD                    KNPG                           ..CATEGORIA
035700040303     C                   KFLD                    KLNP                           ..LNP
035800040303     C                   KFLD                    KLNA                           ..LNA
035900040303     C                   KFLD                    KNRS                           ..SERIE
036000040303     C                   KFLD                    KNSC                           ..SEGNACOLLO
036100040303     C                   KFLD                    KFGS                           ..FIL.GESTIONE
036200000000     C     KTAB          KLIST
036300000000     C                   KFLD                    CODUT
036400000000     C                   KFLD                    COD
036500000000     C                   KFLD                    KEY
036600000000     C     KTAB2         KLIST
036700000000     C                   KFLD                    CODUT
036800000000     C                   KFLD                    COD
036900000000     C*
037000020220     C     *LIKE         DEFINE    G02TGI        WGIORN
037100950122     C     *LIKE         DEFINE    TBLCOD        COD
037200000000     C     *LIKE         DEFINE    TBLKEY        KEY
037300000000     C     *LIKE         DEFINE    BRVNPG        KNPG                           CATEGORIA FOGLI
037400000000     C     *LIKE         DEFINE    BRVLNP        KLNP                           L N P
037500000000     C     *LIKE         DEFINE    BRVLNA        KLNA                           L N A
037600000000     C     *LIKE         DEFINE    BRVNRS        KNRS                           SERIE
037700000000     C     *LIKE         DEFINE    BRVNSC        KNSC                           SEGNACOLLO
037800000000     C     *LIKE         DEFINE    BRVFLE        KFLE                           FIL.ELABORATORE
037900990623     C                   Z-ADD     0             COMDFS            8 0
038000990623     C                   Z-ADD     0             COMHMS            4 0
038100000000     C*
038200000000     C                   ENDSR
038300000000     O**************************************************************************
038400961216**         CMD1
038500021008CHKOBJ OBJ(FISPP00R) OBJTYPE(*FILE)     MBR(M234567890)
038600000000**         CMD2
038700021008ALCOBJ OBJ((FISPP00R *FILE *EXCL           M234567890))
038800000000**         CMD4
038900021008OVRDBF FILE(FISPP00R)                   MBR(M234567890)
