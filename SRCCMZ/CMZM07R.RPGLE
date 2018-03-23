000100921104     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000200921110?    F* CMZM07R *----------------------------------------------------*
000300921103?    F*---------*                                                    *
000400921110?    F*               - RICEZIONE DOCUMENTO -                        *
000500921110?    F*               -----------------------                        *
000600921103?    F*                                                              *
000700921103?    F*                                                 AZ.          *
000800921103?    F*                                                              *
000900921103?    F*--------------------------------------------------------------*
001000921103?    F* I N D I C A T O R I                                          *
001100921103?    F*--------------------------------------------------------------*
001200921103     F*                                                              *
001300921104     F* -01- FLAG    PER GESTIONE LOCK AREA DATI                     *
001400921103     F*                                                              *
001500921110     F* -30- LETTURA CMRCM00F                                        *
001600921112     F* -31- LETTURA CMTSM06L                                        *
001700921113     F* -32- LETTURA CMTSM04L                                        *
001800921109     F*                                                              *
001900921109     F*  90 - ERRORE QCMDEXC                                         *
002000921103?    F*
002100921103?    F*--------------------------------------------------------------*
002200921103?    F*  DATA BASE                                                   *
002300921103?    F*--------------------------------------------------------------*
002400921103     F*
002500921103?    F*------------
002600051207     fCMTSM03L  if   e           k disk    rename(CMTSM000:CMTSM003)
002700921113     FCMTSM04L  UF   E           K DISK
002800921111?    F*------------
002900921112     FCMTSM06L  UF A E           K DISK
003000921112     F                                     RENAME(CMTSM000:TSM0006)
003100921109?    F*------------
003200921112     FCMRGM00F  O    E             DISK
003300921109?    F*------------
003400921110     FCMRCM00F  IF   E             DISK    USROPN
003500930301?    F*------------
003600921109     D*
003700921109?    D*------------------------------------------------------------
003800921109?    D*  SCHIERE
003900921109?    D*------------------------------------------------------------
004000921104     D*
004100921104?    D*------------------------------------------------------------
004200921104?    D*  DS
004300921104?    D*------------------------------------------------------------
004400921110?     *-- Scomposizione del record di testata.
004500921110     D                 DS
004600921110     D  $FCFC                  1      1
004700921110     D  $TIP                   2      2
004800921110     D  $FIR                   3      5  0
004900921110     D  $DSR                   6      8  0
005000921110     D  $UTR                   9     18
005100921110     D  $UTL                  19     28
005200921110     D  $DOR                  29     35  0
005300921110     D  $DOL                  36     42  0
005400921110     D  $DOC                  43     49  0
005500921110     D  $TXT                  50    119
005600921110     D  $DTR                 120    126  0
005700921110     D  $ORR                 127    132  0
005800921110     D  $DTI                 133    139  0
005900921110     D  $ORI                 140    145  0
006000921110     D  $MSG                 146    146
006100921110     D  $STP                 147    147
006200921110     D  $RIS                 148    148
006300921110     D  $MSR                 149    149
006400921110     D  $DTL                 150    156  0
006500921110     D  $DTS                 157    163  0
006600921110     D  $DTP                 163    170  0
006700921110     D  $URG                 171    171
006800921110     D  $URR                 172    172
006900921110     D  $DOS                 173    179  0
007000921110     D  $STS                 180    180
007100921110     D  $TRA                 181    190
007200921112     D  $FIL                 191    193  0
007300921110     D  TXT                    1    199
007400921110?     *-- Definizione dei campi di CMD1
007500921110     D                 DS
007600921110     D  $FILE                 11     32
007700921111     D  $MBR                  38     48
007800921110     D  CMD1                   1     70
007900921109     D                                     DIM(1) CTDATA PERRCD(1)
008000921110?     *-- Definizione dei campi di CMD2
008100921110     D                 DS
008200921111     D  $FIL2                 13     23
008300921111     D  $LIB2                 32     53
008400921111     D  $MBR2                 59     69
008500921110     D  CMD2                   1     70
008600921110     D                                     DIM(1) CTDATA PERRCD(1)
008700921112      *---
008800921112     D CMZM01        E DS                  EXTNAME(CMZM01PF)
008900921109      *---
009000921109     D CMZLDA        E DS                  EXTNAME(CMZLDAPF)
009100921111      *---
009200921111     D FILE            C                   CONST('CMRCM00F')
009300921103?     *
009400921103?     *------------------------------------------------------------
009500921103?     *  FLUSSO PRINCIPALE
009600921103?     *------------------------------------------------------------
009700921103?     *
009800921103?    C                   EXSR      INIZIO
009900921110     C                   READ      CMRCM00F                               30
0100009211101    C     *IN30         DOWEQ     '0'
010100921103     C*
010200921110     C*
0103009211102    C     RCMFCC        IFEQ      '*'
010400921110?    C                   EXSR      GESTSM
0105009211102E   C                   ELSE
010600921112?    C                   EXSR      GESRGM
0107009211102-   C                   END
010800921109     C*
010900921110     C                   READ      CMRCM00F                               30
0110009211101-   C                   ENDDO
011100921112     C*
011200921112     C* Invio messaggio
011300921112     C*
011400921112?    C                   EXSR      GESMSG
011500921110     C*
011600921110     C* Pulizia file CMRCM00F
011700921110     C*
011800921110?    C                   EXSR      GESCLR
011900921113     C     *LOVAL        SETLL     CMTSM04L
012000921113     C     *LOVAL        SETLL     CMTSM06L
012100921103     C                   SETON                                            LR
012200921103?     *
012300921103?     *------------------------------------------------------------
012400921103?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
012500921103?     *------------------------------------------------------------
012600921103?     *
012700921103     C*
012800921103?    C     INIZIO        BEGSR
012900921103     C*
013000921109     C*
013100921103     C                   MOVE      '0'           *IN
013200921112     C     *DTAARA       DEFINE                  CMZM01
013300921110     C     *DTAARA       DEFINE    *LDA          CMZLDA
013400921109     C*
013500921110     C* CMTSM00F
013600921110     C     *LIKE         DEFINE    TSMTIP        KTIP
013700921110     C     *LIKE         DEFINE    TSMDOL        KDOL
013800921110     C     KTSM02        KLIST
013900921110     C                   KFLD                    KTIP
014000921110     C                   KFLD                    KDOL
014100921112     C*
014200921112     C* CMTSM06L
014300921112     C     *LIKE         DEFINE    TSMFIR        KFIR
014400921112     C     *LIKE         DEFINE    TSMDOR        KDOR
014500921112     C     KTSM06        KLIST
014600921112     C                   KFLD                    KFIR
014700921112     C                   KFLD                    KDOR
014800921110     C*
014900921110     C* Prelevo parametri da lcldtaara.
015000921109     C*
015100921109     C                   SETON                                        01
0152009211101    C     *IN01         DOWEQ     '1'
015300921110     C                   IN        CMZLDA                               01
0154009211101-   C                   END
015500921110     C*
015600921110     C* Apro file CMRCM00F
015700921110     C*
015800921111     C     LDAFIL        CAT       ')':0         $FIL2
015900921110     C*
016000921111     C     LDALIB        CAT       '/':0         $LIB2
016100921111     C     $LIB2         CAT       FILE:0        $LIB2
016200921111     C     $LIB2         CAT       ')':0         $LIB2
016300921103     C*
016400921111     C     LDAMBR        CAT       ')':0         $MBR2
016500921110     C*
016600921110     C                   CALL      'QCMDEXC'
016700921110     C                   PARM                    CMD2
016800921110     C                   PARM      70            WLUCMD           15 5
016900921110     C                   OPEN      CMRCM00F
017000921110     C*
017100921103     C                   ENDSR
017200921109?     *
017300921109?     *------------------------------------------------------------
017400921109?     *  G E S T S M   - GESTIONE TESTATA
017500921109?     *------------------------------------------------------------
017600921109?     *
017700921109?    C     GESTSM        BEGSR
017800930423     C*
017900930423     C* Creo documento, prelevo Numeratore dall dataarea CMZM01.
018000930423     C*
018100930423     C                   SETON                                        01
0182009304231    C     *IN01         DOWEQ     '1'
018300930423     C     *LOCK         IN        CMZM01                               01
0184009304231-   C                   END
018500930423     C                   MOVE      M01NRR        NUMNRR            7 0
018600051207     ***C                   ADD       1             NUMNRR
018700051207     c                   exsr      Chk_Free
018800930423     C                   MOVE      NUMNRR        M01NRR
018900930423     C                   OUT       CMZM01
019000930423     C     SAVNRR        IFEQ      *ZERO
019100930423     C                   MOVE      NUMNRR        SAVNRR            7 0
019200930423     C                   END
019300921112     C*---
019400921112     C                   MOVEL     RCMFCC        TXT
019500921112     C                   MOVE      RCMRIG        TXT
019600921112     C*---
019700921112     C                   MOVE      $FIL          KFIR
019800921112     C                   MOVE      $DOL          KDOR
019900921112     C     KTSM06        CHAIN     CMTSM06L                           31
020000921110     C*
020100921111     C                   MOVE      'A'           TSMTIP
020200921112     C                   MOVE      $FIL          TSMFIR
020300921112     C                   MOVE      $FIR          TSMFIL
020400921110     C                   MOVE      $DSR          TSMDSR
020500921110     C                   MOVE      $UTL          TSMUTR
020600921110     C                   MOVE      $UTR          TSMUTL
020700921113     C                   MOVE      $DOL          TSMDOR
020800930602     c*
020900930602     c* se è un sollecito di un documento già ricevuto
021000930602     c* non modifica i riferimenti documenti e imposta il testo
021100930602     c* come SOLL:
021200930602     C*
021300930602     C     *IN31         IFEQ      *ON
021400930602     C                   MOVE      M01NRR        TSMDOL
021500930602     C                   MOVE      SAVNRR        TSMDOC
021600921110     C                   MOVE      $TXT          TSMTXT
021700930602     C                   ELSE
021800930602     C     'SOLL: '      CAT       $TXT          TSMTXT
021900930602     C                   END
022000930322C    C                   EXSR      GETDAT
022100930322     C                   MOVE      $CVT2         TSMDTR
022200921110     C                   TIME                    TSMORR
022300930322     C                   MOVE      $CVT2         TSMDTI
022400921118     C                   TIME                    TSMORI
022500921110     C                   MOVE      $MSG          TSMMSG
022600930301     C                   MOVE      $STP          TSMSTP
022700921110     C                   MOVE      $RIS          TSMRIS
022800921110     C                   MOVE      $MSR          TSMMSR
022900921110     C                   MOVE      $DTL          TSMDTL
023000930322C    C                   EXSR      GETDAT
023100980227     C                   MOVE      *ZERO         TSMDTS
023200980227     C     $STP          IFNE      'S'
023300930322     C                   MOVE      $CVT2         TSMDTS
023400980227     C                   ENDIF
023500921110     C                   MOVE      $DTP          TSMDTP
023600921110     C                   MOVE      $URG          TSMURG
023700921110     C                   MOVE      $URR          TSMURR
023800921110     C                   MOVE      $DOS          TSMDOS
023900921110     C                   MOVE      '1'           TSMSTS
024000921110     C                   MOVE      $TRA          TSMTRA
024100921110     C*
024200921112     C  N31              UPDATE    TSM0006
024300921112     C   31              WRITE     TSM0006
024400921113     C*
024500921113     C* Controllo se è documento di risposta.
024600921113     C*
0247009211131    C     $DOS          IFNE      *ZERO
024800921113     C                   MOVE      $DOS          KDOL
024900921113     C     KDOL          CHAIN     CMTSM04L                           32
025000921113     C  N32              MOVE      '5'           TSMSTS
025100921113     C  N32              UPDATE    CMTSM000
0252009211131-   C                   END
025300921104     C*
025400921104     C                   ENDSR
025500051207      *
025600051207?     *------------------------------------------------------------
025700051207?     * CHK_FREE - Ricerca primo progressivo libero
025800051207?     *------------------------------------------------------------
025900051207      *
026000051207     c     Chk_Free      BEGSR
026100051207      *
026200051207do  1c                   DO        *hival
026300051207      *
026400051207do  2c                   DOU       NOT %equal(CMTSM04L)
026500051207     c                   add       1             NumNRR
026600051207if  3c                   if        NumNRR <= *zeros
026700051207     c                   eval      NumNRR =  1
026800051207e   3c                   endif
026900051207     c     NumNRR        setll     CMTSM000
027000051207e   2c                   ENDDO
027100051207      *
027200051207     c     NumNRR        setll     CMTSM003
027300051207if  2c                   if        NOT %equal(CMTSM03L)
027400051207     c                   leave
027500051207e   2c                   endif
027600051207      *
027700051207e   1c                   ENDDO
027800051207      *
027900051207     c                   ENDSR
028000921109?     *
028100930322?    C     GETDAT        BEGSR
028200930322     C                   TIME                    W012             12 0
028300930322     C                   MOVE      W012          DATA              6
028400930322     C                   MOVEL     DATA          $CVT1
028500930322     C                   CALL      'CVTDAT'
028600930322     C                   PARM                    $CVT1             8
028700930322     C                   PARM                    $CVT8             8
028800930322     C                   PARM      '*SYSVAL'     $FMT1             8
028900930322     C                   PARM      '*YMD'        $FMT2             8
029000930322     C                   PARM      '*NONE'       $SEP              8
029100930322     C                   MOVEL     $CVT8         $CVT2             6
029200930322     C                   ENDSR
029300930322?     *
029400921109?     *------------------------------------------------------------
029500921109?     *  G E S R G M   - GESTIONE RIGHE
029600921109?     *------------------------------------------------------------
029700921109?     *
029800921109?    C     GESRGM        BEGSR
029900921110     C*
030000921110     C*  Aggiorno righe di documento.
030100921109     C*
030200921112     C                   MOVE      SAVNRR        RGMDOC
030300921110     C                   MOVE      RCMFCC        RGMFCC
030400921110     C                   MOVE      RCMRIG        RGMRIG
030500921112     C                   WRITE     CMRGM000
030600921109     C*
030700921110     C*
030800921109     C                   ENDSR
030900921109?     *
031000921109?     *------------------------------------------------------------
031100921110?     *  G E S M S G   - GESTIONE INVIO MSG
031200921109?     *------------------------------------------------------------
031300921109?     *
031400921110?    C     GESMSG        BEGSR
031500921109     C*
0316009211101    C     $MSG          IFEQ      'S'
031700921110     C     $MSR          OREQ      'S'
031800921218     C                   MOVE      $FIL          $FIRC             3
031900921110     C                   CALL      'CMZM07C'
032000921110     C                   PARM                    $TXT
032100921111     C                   PARM                    $UTR
032200921218     C                   PARM                    $UTL
032300921218     C                   PARM                    $FIRC
0324009211101-   C                   END
032500921110     C*
032600930301     C* STAMPA DOCUMENTO
032700930301     C*
032800930322     C*          $STP      IFEQ 'S'
032900930322     C*          TSMFIL    CHAINAZORG01L             21
033000930322     C*          ORGFIL    IFEQ ORGFEL
033100930322     C*                    MOVEL'PRT01'   M03OUT 10
033200930322     C*                    ELSE
033300930322     C*                    MOVE TSMFIL    W003    3
033400930322     C*          'REM'     CAT  W003:0    M03OUT    P
033500930322     C*          M03OUT    CAT  'P':0     M03OUT
033600930322     C*                    END
033700930322     C*                    CALL 'CMZM03C'
033800930322     C*                    PARM           TSMDOL
033900930322     C*                    PARM           M03OUT
034000930322     C*                    END
034100921110     C                   ENDSR
034200921110?     *
034300921110?     *------------------------------------------------------------
034400921110?     *  G E S C L R   - GESTIONE PULIZIA FILE CMRCM00F
034500921110?     *------------------------------------------------------------
034600921110?     *
034700921110?    C     GESCLR        BEGSR
034800921109     C*
034900921110     C                   CLOSE     CMRCM00F
035000921110     C*
035100921111     C     LDALIB        CAT       '/':0         $FILE
035200921111     C     $FILE         CAT       LDAFIL:0      $FILE
035300921111     C     $FILE         CAT       ')':0         $FILE
035400921110     C*
035500921111     C     LDAMBR        CAT       ')':0         $MBR
035600921110     C*
035700940321     C                   CALL      'QCMDEXC'                            21
035800921111     C                   PARM                    CMD1
035900921111     C                   PARM      70            WLUCMD
036000940321     C     *IN21         IFEQ      *ON
036100940321     C                   CALL      'QCMDEXC'                            21
036200940321     C                   PARM                    CMD1
036300940321     C                   PARM      70            WLUCMD
036400940321     C                   ENDIF
036500921109     C*
036600921109     C                   ENDSR
036700940321     C*C************************************************************
036800921109     C*..1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
036900921111**CTDATA CMD1
037000921111RMVM FILE(                       MBR(
037100921110**CTDATA CMD2
037200921111OVRDBF FILE(            TOFILE(                       MBR(
