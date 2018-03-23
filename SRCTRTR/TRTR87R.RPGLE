000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941019     F* TRTR87R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400941019     F* - RICEZIONE INCASSO CONTRASSEGNI RLS 11/94                   *
000500000000     F*--------------------------------------------------------------*
000600941019     FFNCAS00R  UF   E             DISK    USROPN
000700941019     FTNCSB03L  UF   E           K DISK
000800900604     FCMRIC01L  UF   E           K DISK
000900930927     FTABEL00F  IF   E           K DISK    USROPN
001000991021     FTNTBE01L  IF   E           K DISK    USROPN
001100900604     D*
001200951019     D BB              S              7  0 DIM(200)                             COD.CLI. ABILIT.BB
001300131010     D cca             S              7  0 DIM(200)                             COD.CLI. ABILIT.BB
001400131010     D ksctm           S              7  0 DIM(200)                             COD.CLI. ABILIT.BB
001500900604     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001600900604     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001700900604     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001800900604     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001900941019     D WLBDA8          DS
002000941019     D  G08DAT                 1      8  0
002100941019     D  G08INV                 9     16  0
002200941019     D  G08ERR                17     17
002300941019     D  G08TGI                18     22  0
002400131010     D Dcca          E DS
002500131010     D DGED          E DS
002600991021     D DSTM          E DS
002700950327     D DSCV          E DS
002800900604     D KPJBA         E DS
002900900604     D  LIBSYS                92    101
003000900604     D  LEGFIL               483    492
003100900604     D  MBRFIL               493    502
003200131010     d xx              s              2  0
003300131010     C*---------------------------------------------------------------*
003400941019     C                   READ      FNCAS00R                               80
003500941019     C     *IN80         DOWEQ     '0'
003600911111     C*
003700991021     C                   ADD       1             NUMREC            7 0
003800941019     C     KCSB          CHAIN     TNCSB000                           21
003900991021     C* TROVATO CONTROLLO SE DATI CONGRUENTI
004000991021     C     *IN21         IFEQ      '0'
004100941019     C* CONTROLLO SE ANNULLATO
004200941019     C     CSBSTA        IFEQ      9
004300900604     C* CONTROLLO IMPORTO
004400991021     C     CSBCAS        ORNE      CASCAS
004500941019     C* SE GIA' INCASSATO
004600991021     C     CSBDDC        ORGT      *ZEROS
004700911112     C     CSBNDT        ORGT      *ZEROS
004800941019      *
004900941019     C                   ELSE
005000900604     C* SE TUTTO BENE AGGIORNO
005100900604     C                   EXSR      AGGIOR
005200941019     C                   DELETE    FNCAS000
005300941019     C                   ENDIF                                                  <- *IN24
005400941019     C                   ENDIF                                                  <- *IN21
005500941019      *
005600941019     C                   READ      FNCAS00R                               80
005700941019     C                   ENDDO
005800900604     C*
005900900604     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
006000900604     C*
006100900604     C                   EXSR      CAEXB
006200900604     C*
006300900604     C* AGGIORNA ARCHIVIO DI LOG
006400900604     C*
006500900604     C     LEGFIL        CHAIN     CMRIC01L                           20
006600900604     C  N20              DO
006700991021     C                   Z-ADD     NUMREC        RICNR1
006800900604     C                   MOVE      '1'           RICFL2                          AGG.TO
006900900604     C                   UPDATE    CMRIC000
007000900604     C                   MOVEL     RICPRR        LEGFIL
007100900604     C                   END
007200900604     C*
007300900605     C     STOP          TAG
007400900605     C                   SETON                                        LR
007500900605     C*
007600900604     C/SPACE 005
007700900604     C*----------------------------------------------
007800941019     C* AGGIORNO FILE TNCSB00F ----------------------
007900900604     C*----------------------------------------------
008000900604     C     AGGIOR        BEGSR
008100900604     C*
008200941019     C                   MOVE      CASFLE        CSBRGF
008300941019     C                   MOVE      CASDTE        CSBDTE
008400941019     C                   MOVEL     CASTPI        CSBTPI                         T.INT.=M
008500950127     C                   MOVEL     CASTPI        CSBFUS                         T.INT.=M
008600941019     C                   MOVEL     CASTPA        CSBTPA
008700941019     C                   MOVE      CASABI        CSBABI
008800941019     C                   MOVE      CASCAB        CSBCAB
008900941019     C                   MOVE      CASNRA        CSBNRA
009000941019     C                   MOVE      CASDDC        CSBDDC
009100941019     C                   MOVE      CASNDT        CSBNDT
009200941019     C                   MOVE      AMGUDT        CSBUDA
009300941019     C                   MOVE      CASVCA        CSBVCA
009400941019     C                   MOVE      CASNRG        CSBNRI
009500941019     C                   MOVE      CASDRE        CSBDRI
009600911112     C*
009700911112     C* SE NON E DIRETTO E' CONFERMATO AUTOMATIC.
009800941019     C     CASTPI        IFNE      'M'
009900941019     C                   MOVE      AMGUDT        CSBDTA
010000930927     C                   ELSE
010100131010     c* controllo se assegno postdatato e cliente è presente nella
010200131010     c* tabella CCA "assegni da versare in banca perr c/ cliente"
010300131010     c* sostituisco il codice cliente con il relativo per TM
010400131010     c                   if        csbdte > 0 and csbdte > csbddc
010500131010     c                   eval      xx = 1
010600131010     c     csbcdi        lookup    cca(xx)                                88
010700131010     c                   if        *in88 and ksctm(xx) > 0
010800131010     c                   eval      csbksc = ksctm(xx)
010900131010     c                   eval      csbcdi = ksctm(xx)
011000131010     c                   end
011100131010     c                   end
011200930927     C*
011300950327     C* CONTROLLO SE CONTRASSEGNO ESTERO
011400930927     C* SE E'DIRETTO CONTROLLO SE IL CLIENTE è ABILITATO ALL'INCASSO "BB"
011500930927     C* IN QUESTO CASO TOLGO IN TPI IL FLAG=M , QUINDI RISULTA INCASSATO
011600930927     C* CON ASSEGNO BARTOLINI CON DISTINTA DA CONFERMARE  (VECCHIA MANIERA)
011700991021     C     CSBVCA        IFEQ      *BLANKS
011800991021     C     CSBVCA        OREQ      §GEDCN
011900991021     C     CSBVCA        OREQ      DIVALT
012000930927     C                   Z-ADD     1             X
012100930927     C     CSBCDI        LOOKUP    BB(X)                                  30
012200930927     C   30              MOVEL     *BLANK        CSBTPI
012300950127     C   30              MOVEL     *BLANK        CSBFUS
012400950327     C                   END
012500930927     C*
012600930927     C                   END
012700900604     C*
012800910913     C     CSBSTA        IFEQ      2
012900021122     C     CSBSTA        orEQ      3
013000910913     C                   Z-ADD     0             CSBSTA
013100900604     C                   END
013200941019     C                   UPDATE    TNCSB000
013300900604     C                   ADD       1             CONNBL            7 0
013400900604     C     ENDAGG        ENDSR
013500900604     C/SPACE
013600900604     C**---------------------------------------------------------
013700941019     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FNCAS00R      -
013800900604     C**---------------------------------------------------------
013900900604     C     CAEXA         BEGSR
014000900604     C                   MOVE      ')'           VAR11            11
014100900604     C                   MOVEL     MBRFIL        VAR11
014200900604     C                   MOVE      '))'          VAR12            12
014300900604     C                   MOVEL     MBRFIL        VAR12
014400900604     C                   Z-ADD     55            LUNG             15 5
014500900604     C*
014600900604     C                   SETOFF                                       20
014700900604     C                   MOVE      VAR11         CMD1
014800900604     C                   MOVEL     *BLANKS       COMMAN
014900900604     C                   MOVEA     CMD1(1)       COMMAN           80
015000900604     C                   CALL      'QCMDEXC'                            20
015100900604     C                   PARM                    COMMAN
015200900604     C                   PARM                    LUNG
015300900604     C   20              GOTO      STOP
015400900604     C** CONTROLLO SE ESISTE IL MEMBRO
015500900604     C** SE NON ESISTE CHIUDO IL PROGRAMMA
015600900604     C                   SETOFF                                       20
015700900604     C                   MOVE      VAR12         CMD2
015800900604     C                   MOVEL     *BLANKS       COMMAN
015900900604     C                   MOVEA     CMD2(1)       COMMAN
016000900604     C                   CALL      'QCMDEXC'                            20
016100900604     C                   PARM                    COMMAN
016200900604     C                   PARM                    LUNG
016300900604     C   20              GOTO      STOP
016400900604     C** ALLOCO MEMBRO
016500900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
016600900604     C                   SETOFF                                       20
016700900604     C                   MOVE      VAR11         CMD3
016800900604     C                   MOVEL     *BLANKS       COMMAN
016900900604     C                   MOVEA     CMD3(1)       COMMAN
017000900604     C                   CALL      'QCMDEXC'                            20
017100900604     C                   PARM                    COMMAN
017200900604     C                   PARM                    LUNG
017300900604     C   20              GOTO      STOP
017400900604     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
017500900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
017600941019     C                   OPEN      FNCAS00R
017700900604     C                   ENDSR
017800900604     C/SPACE 005
017900900604     C**---------------------------------------------------------
018000941019     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FNCAS00R      -
018100900604     C**---------------------------------------------------------
018200900604     C     CAEXB         BEGSR
018300900604     C                   MOVE      ')'           VAR11            11
018400900604     C                   MOVEL     MBRFIL        VAR11
018500900604     C                   MOVE      '))'          VAR12            12
018600900604     C                   MOVEL     MBRFIL        VAR12
018700900604     C                   Z-ADD     55            LUNG             15 5
018800900604     C*
018900941019     C                   CLOSE     FNCAS00R                             20
019000940825     C** RIORGANIZZA IL MEMBRO ELIMINATE SPECIFICHE
019100940826     C  N20              DO
019200940826     C                   END
019300900604     C                   MOVE      VAR12         CMD5
019400900604     C                   MOVEL     *BLANKS       COMMAN
019500900604     C                   MOVEA     CMD5(1)       COMMAN
019600900604     C                   CALL      'QCMDEXC'
019700900604     C                   PARM                    COMMAN
019800900604     C                   PARM                    LUNG
019900900604     C** DISALLOCA IL MEMBRO AGG.TO
020000900604     C                   ENDSR
020100941019     C*---------------------------------------------------------------*
020200941019     C     *INZSR        BEGSR
020300941019     C*---------------------------------------------------------------*
020400941019     C     *ENTRY        PLIST
020500941019     C                   PARM                    KPJBA
020600941019     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
020700941019     C* ASSUMO IL PRIMO MEMBRO DEL FILE
020800941019     C     MBRFIL        IFEQ      *BLANK
020900941019     C                   MOVEL     '*FIRST'      MBRFIL
021000941019     C                   END
021100941019     C                   Z-ADD     66            RIG               2 0
021200941019     C*§§
021300941019     C                   TIME                    WHHUDT           14 0
021400941019     C                   MOVEL     WHHUDT        ORA               6 0
021500941019     C                   MOVE      WHHUDT        G08DAT
021600941019     C                   MOVE      *ZEROS        G08INV
021700941019     C                   MOVEL     *BLANK        G08ERR
021800941019     C                   CALL      'XSRDA8'
021900941019     C                   PARM                    WLBDA8
022000941019     C                   Z-ADD     G08INV        AMGUDT            8 0
022100941019      *
022200941019      * CARICO TUTTI I CLIENTI ABILITATI ALL' INCASSO 'BB'
022300941019      * CON AUTORIZZ. A VERSARE ASSEGNI INT. MITTENTE
022400941019      * PER TRASFORMARLI IN ASS.BARTOLINI
022500941019     C                   OPEN      TABEL00F
022600991021     C                   OPEN      TNTBE01L
022700991215     C                   Z-ADD     1             CODUT
022800941019     C                   MOVEL     'TM'          TBLCOD
022900941019     C                   MOVE      *BLANKS       TBLKEY
023000941019     C                   MOVE      *ZEROS        X                 3 0
023100941019     C     KTAB          SETLL     TABEL00F
023200941019     C     *IN21         DOUEQ     '1'
023300941019     C     KCOD          READE     TABEL00F                               21
023400941019     C  N21              MOVEL     TBLUNI        DSTM
023500941019     C  N21§TMFBB        IFEQ      'S'
023600941019     C     §TMFVE        ANDNE     'N'                                           AUT.VERS.MITT
023700941019     C                   ADD       1             X
023800941019     C                   MOVEL     TBLKEY        BB(X)
023900941019     C                   ENDIF
024000941019     C                   ENDDO
024100131010     c* carico tabella clienti abilitati all'incasso BM per c/ loro
024200131010     C                   MOVEL     'CCA'         TBECOD
024300131010     C                   MOVE      *ZEROS        X
024400131010     C     tbecod        setll     TNTBE01L
024500131010     c                   do        *hival
024600131010     C     tbecod        reade     TNTBE01L
024700131010     c                   if        %eof(tntbe01l)
024800131010     c                   leave
024900131010     c                   end
025000131011     c                   if        tbeatb <> ' '
025100131011     c                   iter
025200131011     c                   end
025300131010     c                   movel     tbeuni        dcca
025400131010     c                   if        §ccaksctm > 0
025500131010     C                   ADD       1             X
025600131010     C                   MOVEL     TBeKE1        cca(X)
025700131010     C                   eval      ksctm(x) = §ccaksctm
025800131010     c                   end
025900131010     C                   ENDDO
026000991021     C* CARICO DIVISA MONETA DI CONTO
026100131010     C                   MOVEL     'GED'         TBECOD
026200131010     C                   MOVEL     *BLANKS       TBEKE1
026300131010     C                   MOVEL     '1'           TBEKE1
026400131010     C     KTBE          CHAIN     TNTBE01L                           21
026500131010     C  N21              MOVEL     TBEUNI        DGED
026600131010     C* ... E MONETA ALTERNATIVA
026700131010     C                   MOVE      *BLANKS       TBLKEY
026800131010     C                   MOVE      *ZEROS        X                 3 0
026900131010     C     KTAB          SETLL     TABEL00F
027000131010     C     *IN21         DOUEQ     '1'
027100131010     C     KCOD          READE     TABEL00F                               21
027200131010     C     *IN21         IFEQ      '0'
027300131010     C     TBLFLG        ANDEQ     *BLANKS
027400131010     C                   MOVEL     TBLUNI        DSCV
027500131010     C                   MOVEL     TBLKEY        WDIV              3
027600131010     C     §CVDVA        IFEQ      'S'
027700131010     C     WDIV          ANDNE     §GEDCN
027800131010     C     WDIV          ANDNE     *BLANK
027900131010     C                   MOVEL     WDIV          DIVALT            3
028000131010     C                   END
028100131010     C                   ENDIF
028200131010     C                   ENDDO
028300941019     C                   CLOSE     TABEL00F
028400991021     C                   CLOSE     TNTBE01L
028500941019     C*---------------------------------------------------------------*
028600941019     C* APERTURA MEMBRO DA TRASCODIFICARE
028700941019     C*
028800941019     C                   EXSR      CAEXA
028900941019     C*---------------------------------------------------------------*
029000941019     C*---------------------------------------------------------------*
029100941019     C* DEFINIZIONE CHIAVI
029200941019     C*
029300941019     C     KCSB          KLIST
029400941019     C                   KFLD                    CASAAS
029500941025     C                   KFLD                    CASLNP
029600941019     C                   KFLD                    CASNRS
029700941019     C                   KFLD                    CASNSP
029800941019     C*
029900941019     C     KTAB          KLIST
030000991021     C                   KFLD                    CODUT             1 0
030100941019     C                   KFLD                    TBLCOD
030200941019     C                   KFLD                    TBLKEY
030300941019     C*
030400941019     C     KCOD          KLIST
030500941019     C                   KFLD                    CODUT
030600991021     C                   KFLD                    TBLCOD
030700991021     C     KTBE          KLIST
030800991021     C                   KFLD                    TBECOD
030900991021     C                   KFLD                    TBEKE1
031000941019      *
031100941019     C     *LIKE         DEFINE    CSBCAS        TOTASS
031200941019     C     *LIKE         DEFINE    CSBCAS        TOTBB
031300941019     C                   ENDSR
031400941019**         CMD1
031500941019CHKOBJ OBJ(FNCAS00R) OBJTYPE(*FILE)     MBR(M234567890)
031600900514**         CMD2
031700941019ALCOBJ OBJ((FNCAS00R *FILE *EXCL           M234567890))
031800900514**         CMD3
031900941019OVRDBF FILE(FNCAS00R)                   MBR(M234567890)
032000900514**         CMD5
032100941019DLCOBJ OBJ((FNCAS00R *FILE *EXCL           M234567890))
