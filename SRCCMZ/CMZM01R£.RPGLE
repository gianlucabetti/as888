000100921102     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200921102?    F* CMZM01R *----------------------------------------------------*
000300921102?    F*---------*                                                    *
000400921102?    F*               - GESTIONE  POSTA -                            *
000500921102?    F*               -------------------                            *
000600040302?    F* RIMESSO TEST SU EDP,OPR, SEG, AREA, DIR, DIV, INT            *
000601021019?    F*                           (SENZA AZUTE NON SO CHI SONO)      *
000700921102?    F*                                                 AZ.          *
000800921102?    F*                                                              *
000900921102?    F*--------------------------------------------------------------*
001000921102?    F* I N D I C A T O R I                                          *
001100921102?    F*--------------------------------------------------------------*
001200921102     F*                                                              *
001300921102     F* -01- FLAG PER POSIZIONAMENTO SU SFL SUL I° REC ERRATO        *
001400921102     F* -02- FLAG PER POSIZIONAMENTO SU SFL SUL I° REC ERRATO        *
001500921113     F* -03- STATO NUOVO
001600921102     F* -04- FLAG PER GESTIONE ARRIVO / PARTENZA                     *
001700921102     F* -05- FLAG PER GESTIONE F12                                   *
001800921105     F* -06- FLAG GESTIONE CURSORE SFL SU PRIMA RIGA DI OGNI PAG.    *
001900000925     F* -07- OFF = tasto ROLLUP attivo.                              *
002000000925     F* -07- ON  = tasto ROLLUP non attivo.                          *
002100000925     F* -09- CONDIZIONAMENTO SFLDROP.                                *
002200921102     F* -20- SFLCLR              S1                                  *
002300921102     F* -N20-SFLDSPCTL           S1                                  *
002400921102     F* -21- SFLDSP/SFLEND       S1                                  *
002500921102     F* -22- SFLNXTCHG           S1                                  *
002600921102     F*                                                              *
002700921102     F* -30- LETTURA CMTSM01L                                        *
002800921102     F* -31- LETTURA CMLOG01L                                        *
002900921102     F* -32- LETTURA M001S1                                          *
003000921102     F* -33- LETTURA CMTSM04L                                        *
003100921102     F* -34- LETTURA CMRGM01L                                        *
003200921103     F* -35- LOKUP                                                   *
003300921112     F* -36- LETTURA CMTSM04L                                        *
003400921112     F* -37- SCAN '?'                                                *
003500921102     F*                                                              *
003600921102     F* -50- ROLLUP                                                  *
003700921102     F*                                                              *
003800921102     F* -70- ERR. M001S1 *- OPZIONE ERRATA                          -*
003900921102     F* -71- ERR. M001S1 *- STAMPARE IL DOC. PRIMA DI CANCELLARLO   -*
004000921102     F* -72- ERR. M001S1 *- RISPONDERE PRIMA DI CANCELLARLO         -*
004100921102     F*                                                              *
004200921103     F* -73- ERR. M001D2 *- FILIALE ERRATA                          -*
004300921103     F* -74- ERR. M001D2 *- DATA    ERRATA                          -*
004400921118     F* -77- ERR. M001D2 *- DATA    ERRATA                          -*
004500921123     F* -78- FILIALE LOCALE ERRATA
004600930406     F* -88- RICERCA SU KFTOD10L
004700921103     F*                                                              *
004800921102     F*  90 - FILTRO PER CARICAMENTO SUBFILE S1                      *
004900921112     F*  91 - ERRORE FILIALE                                         *
005000921102     F*  99 - ERRORE GENERICO                                        *
005100921102?    F*
005200921102?    F*--------------------------------------------------------------*
005300921102?    F*  DATA BASE                                                   *
005400921102?    F*--------------------------------------------------------------*
005500921102     F*
005600921102?    F*------------
005700921118     FCMTSM01L  IF   E           K DISK
005800921102?    F*------------
005900921102     FCMTSM03L  IF   E           K DISK
006000921102     F                                     RENAME(CMTSM000:TSM0003)
006100921102?    F*------------
006200921103     FCMTSM04L  UF   E           K DISK
006300921102     F                                     RENAME(CMTSM000:TSM0004)
006400921102?    F*------------
006500921103     FCMRGM01L  UF   E           K DISK
006600921102?    F*------------
006700921102     FCMLOG01L  IF   E           K DISK
006800921102?    F*------------
006900921112     FAZORG01L  IF   E           K DISK
007000940624     F**KFTOD10LIF  E           K        DISK
007100921102?    F*------------
007200921102     FCMZM01D   CF   E             WORKSTN
007300921102     F                                     SFILE(M001S1:NRR1)
007400921102     F                                     INFDS(INFO)
007500921103     D*
007600921103?    D*------------------------------------------------------------
007700921103?    D*  SCHIERE
007800921103?    D*------------------------------------------------------------
007801021004      * P.O. gestibili dall'utente
007802021004     D $POG            s              3    dim(250) inz(*zeros)
007900921103     D SK              S              1    DIM(10)
008000921103     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
008100921113     D STS             S             10    DIM(9) CTDATA PERRCD(1)
008200921102     D*
008300921102?    D*------------------------------------------------------------
008400921102?    D*  DS
008500921102?    D*------------------------------------------------------------
008600921102      *---
008700921102     D                SDS
008800930406     D  C1VIDE               244    253
008900020430     D*                                     254 263 C1NMUS
009000021015     D  C1NPRE               254    256
009100020430     D*                                     257 259 C1NFIL
009200921102      *---
009300921102     D INFO            DS
009400921102     D  NRRULT               378    379B 0
009500921102      *---
009600921102     D                 DS
009700921102     D  AA                     1      2  0
009800921102     D  MM                     3      4  0
009900921102     D  GG                     5      6  0
010000921102     D  AMG                    1      6  0
010100921123      *---
010101021004     D KPJBA         E DS
010200921123     D CMZ001        E DS                  EXTNAME(CMZ001DS)
010201021004     D TRUL31ds      E DS                  inz
010300020430     D tibs34ds      E DS                  inz
010400020430     D ddatiute      e ds
010500020430     D azuteds       e ds                  extname(AZUTE00F)
010600020430     D dLAT          e ds
010700921102      *---
010800921102     D TXT1            C                   CONST('Posta in Arrivo')
010900921102     D TXT2            C                   CONST('Posta in Partenza')
011000921102      *---
011100921102?     *
011200921102?     *------------------------------------------------------------
011300921102?     *  FLUSSO PRINCIPALE
011400921102?     *------------------------------------------------------------
011500921102?     *
011600921102?    C                   EXSR      INIZIO
0117009211021    C     EMETTI        DOWEQ     'S'
011800921102     C*
011900921102?    C                   EXSR      D2INZ
012000921102?    C                   EXSR      S1CAR
012100921102     C*
0122009211022    C     S1STAI        DOWEQ     'S'
012300921102     C*
012400921102     C                   SETOFF                                       50
012500000925     C                   MOVEL     C1MODE        *IN09
012600000925     C     C1SRN         IFLE      0
012700000925     C     C1SRN         ORGT      NRR1
012800000925     C                   Z-ADD     1             C1SRN
012900000925     C                   ENDIF
013000921102     C                   EXFMT     M001C1
013100921102?    C***
013200921102?    C* F3 = FINE
013300921102?    C***
013400921102     C   KC              MOVE      'N'           EMETTI
013500921102     C   KC              MOVE      'N'           S1STAI
013600921102?    C***
013700921102?    C* F5 = RIVISUALIZZA
013800921102?    C***
013900921102     C   KE              MOVE      'N'           S1STAI
014000921102?    C***
014100921102?    C* F6 = CREA DOCUMENTO
014200921102?    C***
0143009211023    C   KF              DO
014400921102     C                   CALL      'CMZM02R'
014500921102     C                   PARM      *ZERO         M02DCR
014600921102     C                   PARM      *ZERO         M02DCA
014700921102     C                   PARM      *ZERO         M02FIL
014800921102     C                   PARM      *BLANK        M02UTN
014900921102     C                   PARM      *BLANK        M02URG
015000921113     C                   PARM      'N'           M02MSG
015100921110     C                   PARM      *ZERO         M02DOS
015200921112     C                   PARM      'N'           M02FLG
015300921118     C                   PARM      ' '           M02BLO
015400921102     C                   MOVE      'N'           S1STAI
0155009211023-   C                   END
015600921102?    C***
015700921102?    C* F17= PARZIALIZZAZIONE
015800921102?    C***
015900921105?    C   KR              EXSR      GESD2
016000921102?    C***
016100921102?    C* F20= GESTIONE POSTA IN ARRIVO/PARTENZA
016200921102?    C***
016300921105     C   KU              MOVE      'N'           S1STAI
016400921103?    C***
016500921103?    C* F12= RITORNO
016600921103?    C***
016700921105     C   KL              MOVE      'N'           S1STAI
016800921102     C*
016900921105     C*------------
017000921102?    C***
017100921102?    C* ROLL-UP
017200921102?    C***
0173009211033    C     NRR1          IFGT      *ZERO
0174009211034    C     *IN50         IFEQ      '1'
017500921105     C*
017600921102?    C                   EXSR      CARICA
017700921102     C*
017800921102     C                   Z-ADD     NRR1          NUMSF1
017900921105     C                   Z-ADD     WZ9           C1SRN
018000921102     C*
018100921102     C*
0182009211034E   C                   ELSE
018300921103     C  NKC
018400921103     CANNKE
0185009211035    CANNKF
018600921103     CANNKR
018700921103     CANNKU
018800921103     CANNKL              DO
018900921105     C                   Z-ADD     NRRULT        SRNSAV            4 0
019000921102?    C                   EXSR      S1CTL
019100921102     C  N99              Z-ADD     SRNSAV        C1SRN
0192009211035-   C                   END
019300921102     C*
019400921105     C*
0195009211034-   C                   END
0196009211033-   C                   END
019700921105     C*------------
019800921102     C*
0199009211022-   C                   END
0200009211021-   C                   END
020100921102     C*
020200921102     C                   SETON                                            LR
020300921102?     *
020400921102?     *------------------------------------------------------------
020500921102?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
020600921102?     *------------------------------------------------------------
020700921102?     *
020800921102     C*
020900921102?    C     INIZIO        BEGSR
021000921102     C*
021100921102     C* CMTSM01L
021200921102     C     *LIKE         DEFINE    TSMTIP        KTIP
021300921103     C     *LIKE         DEFINE    TSMDTR        KDTA
021400921123     C     *DTAARA       DEFINE                  CMZ001
021500921102     C     KTSM01        KLIST
021600921102     C                   KFLD                    KTIP
021700921102     C                   KFLD                    KDTA
021800921102     C*
021900921102     C* CMLOG01L
022000921102     C     *LIKE         DEFINE    LOGOPE        KOPE
022100921102     C     *LIKE         DEFINE    LOGTRA        KTRA
022200921102     C     KLOG01        KLIST
022300921102     C                   KFLD                    KTRA
022400921103     C                   KFLD                    KOPE
022500921102     C*
022600921123     C                   IN        CMZ001
022700921102     C                   MOVE      'S'           EMETTI            1
022800921102     C                   MOVE      'S'           S1STAI            1
022900921102     C                   MOVE      'S'           D2STAI            1
023000921103     C                   MOVE      '0'           *IN
023100020430     C*
023200020430     c     *dtaara       define    §azute        azuteds
023300020430     c     *dtaara       define    §datiute      ddatiute
023400020509     C                   in        azuteds                              38
023500020509     C                   in        ddatiute                             39
023600020509     C                   IF        *in38 = '1' or *in39 = '1'
023700020509     c                             or RSUT = *blanks
023800020430     C                   CLEAR                   tibs34ds
023900020430     C                   CALL      'TIBS34R'
024000020430     C                   PARM                    tibs34ds
024100020509     C                   in        azuteds
024200020509     C                   in        ddatiute
024300020430     c                   endif
024400921105     C*
024500921105     C* IMPOSTO I PARAMETRI DI DEFAULT
024600921105     C*
024700921105     C                   MOVE      'A'           D2TIP
024800921111     C                   MOVE      *BLANK        D2UTR
024900921112     C                   MOVE      *BLANK        D2FIR
025000020430     C*          C1NPRE    IFEQ 'REM'
025100020430     C*          C1NPRE    OREQ 'SDI'
025200020430     C*                    MOVELC1NFIL    D2FIL
025300020430     C*                    ELSE
025400020430     C*                    MOVE £DST      D2FIL
025500020430     C*                    END
025600020430     C                   MOVE      dutpou        D2FIL
025700020430     C*          C1NPRE    IFEQ 'EDP'
025800020430     C*                    MOVEL'ED*     'D2UTL
025900020430     C*                    ELSE
026000020430     C*                    MOVELC1NMUS    D2UTL
026100020430     C*                    END
026200020430     C*          C1NPRE    IFEQ 'REM'
026300020430     C*          C1NPRE    OREQ 'SDI'
026400020430     C*                    MOVEL'        'D2UTL
026500020430     C*                    ENDIF
026600020509     c* Profili EDP
026601021015     c     UTEFUN        ifEQ      'EDPSE'
026602021015     C     C1NPRE        OREQ      'EDP'
026800020509     c                   movel     'ED*'         D2UTL
026900020509     c                   else
027000020430     C* SECONDO LIVELLO
027100020430     C     dutlpo        IFEQ      '2'
027200020430     C     dutlpo        OREQ      *BLANKS
027300020430     C                   MOVEL     '        '    D2UTL
027400020430     C                   ELSE
027500020430     C* PRIMO LIVELLO O SEDE
027600020430     C                   MOVEL     dutute        D2UTL
027700020430     C                   ENDIF
027800020509     C                   ENDIF
027900020430     C                   MOVEL     UDATE         D2DAT
028000020430     C***                  MOVEL311299    D2DAT
028100921118     C                   Z-ADD     0             D2DA2
028200921105     C                   MOVE      *ZERO         D2DOC
028300921105     C                   MOVE      'N'           D2RIS
028400921218     C                   MOVE      'N'           D2RSP
028500921113     C                   MOVE      'S'           D2NEW
028600921105     C                   SETOFF                                       04
028700000925     C                   MOVEL     *ON           C1MODE
028800921102     C*
028900921102     C                   ENDSR
029000930322?    C     GETDAT        BEGSR
029100930322     C                   TIME                    W012             12 0
029200930322     C                   MOVE      W012          DATA              6
029300930322     C                   MOVEL     DATA          $CVT1
029400930322     C                   CALL      'CVTDAT'
029500930322     C                   PARM                    $CVT1             8
029600930322     C                   PARM                    $CVT8             8
029700930322     C                   PARM      '*SYSVAL'     $FMT1             8
029800930322     C                   PARM      '*YMD'        $FMT2             8
029900930322     C                   PARM      '*NONE'       $SEP              8
030000930322     C                   MOVEL     $CVT8         $CVT2             6
030100930322     C                   ENDSR
030200921102?     *
030300921102?     *------------------------------------------------------------
030400921102?     *  INIZIALIZZO I CAMPI DEL M001D2
030500921102?     *------------------------------------------------------------
030600921102?     *
030700921102?    C     D2INZ         BEGSR
030800921105     C*-
030900921118     C* ESEGUO PER : -
031000921105     C*              -  F12 DALLA VIDEATA DELL'OPZ. 11
031100921105     C*              -  F20
031200921105     C*
031300921105     C*
031400921105     C* PER F12 SPENGO *IN05
031500921105     C*
031600921105     C   KL              SETOFF                                       05
031700921105     C*----
031800921105     C* PER F20 INVERTO IL TIPO DI VISUALIZZ.  ARRIVO/PARTENZA
031900921105     C*
032000921113     C   KUD2TIP         IFEQ      'A'
032100921102     C                   MOVE      'P'           D2TIP
032200921103     C                   ELSE
032300921102     C                   MOVE      'A'           D2TIP
032400921102     C                   END
032500921104     C*
032600921104     C*----
032700921105     C*
032800921105     C*
032900921105     C* PER F12 DOPO OPZ 11 REIMPOSTO I PARAMETRI PRECEDENTI
033000921105     C*
0331009211053    C   KL              DO
033200921105     C                   MOVE      SAVTIP        D2TIP
033300921105     C                   MOVE      SAVUTR        D2UTR
033400921105     C                   MOVE      SAVFIR        D2FIR
033500921123     C                   MOVE      SAVFIL        D2FIL
033600921105     C                   MOVE      SAVUTL        D2UTL
033700921105     C                   MOVE      SAVDAT        D2DAT
033800921105     C                   MOVE      SAVDOC        D2DOC
033900921105     C                   MOVE      SAVRIS        D2RIS
034000921103     C*----
0341009211051-   C                   END
034200921102     C*
034300921113     C     D2TIP         IFEQ      'A'
034400921113     C                   MOVE      *BLANK        D2TIPD
034500921113     C                   MOVEL     TXT1          D2TIPD
034600921113     C                   SETOFF                                       04
034700921113     C                   ELSE
034800921113     C                   MOVE      *BLANK        D2TIPD
034900921113     C                   MOVEL     TXT2          D2TIPD
035000921113     C                   SETON                                        04
035100921113     C                   END
035200921102     C                   ENDSR
035300921102?     *
035400921102?     *------------------------------------------------------------
035500921102?     *  CARICO SUBFILE M001S1
035600921102?     *------------------------------------------------------------
035700921102?     *
035800921102?    C     S1CAR         BEGSR
035900921102     C*
036000921102?    C*
036100921102?    C*PULISCO SUBFILE
036200921102?    C*
036300921102     C                   SETON                                            20
036400921102     C                   SETOFF                                           21
036500921102     C                   WRITE     M001C1
036600921102     C                   SETOFF                                         2022
036700921102     C*
036800921118?    C*
036900921118     C                   Z-ADD     D2DAT         XDTGMA
037000921118     C                   Z-ADD     3             XDTSTA
037100921118?    C                   EXSR      XDATA
037200921118     C                   Z-ADD     XDTAMG        DATAMG            7 0
037300921118?    C*
037400921118     C                   Z-ADD     0             DA2AMG
037500921118     C     D2DA2         IFNE      0
037600921118     C                   Z-ADD     D2DA2         XDTGMA
037700921118     C                   Z-ADD     3             XDTSTA
037800921118?    C                   EXSR      XDATA
037900921118     C                   Z-ADD     XDTAMG        DA2AMG            7 0
038000921118     C                   END
038100921118     C*
038200921102     C                   MOVE      'S'           S1STAI
038300921102     C                   Z-ADD     0             NRR1              5 0
038400921102     C                   Z-ADD     0             NUMSF1            5 0
038500921102?    C*
038600921102?    C*ESEGUO IL CARICAMENTO
038700921102?    C*
038800921102     C                   MOVE      D2TIP         KTIP
038900921118     C                   Z-ADD     DATAMG        KDTA
039000921105     C     KTSM01        SETLL     CMTSM01L
039100921113     C     KTIP          READE     CMTSM01L                               30
039200921102?    C*
039300921102?    C                   EXSR      CARICA
039400921102     C*
039500921102     C*
039600921102?    C*
039700921102?    C*MEMORIZZA IL NUMERO DI RECORDS SCRITTI
039800921102?    C*
039900921102     C                   Z-ADD     NRR1          NUMSF1
040000921103     C     NRR1          COMP      0                                  21
040100921102     C*
040200921102     C                   WRITE     M001D1
040300921102     C*
040400921102     C                   ENDSR
040500921102?     *
040600921102?     *----------------------------------------------------
040700921102?     * C A R I C A - CARICAMENTO SFL
040800921102?     *----------------------------------------------------
040900921102?     *
041000921102?    C     CARICA        BEGSR
041100921106     C*
041200921106     C* Gestione nome parzialiale per '*'  a video.
041300921103     C*
041400921106?    C                   EXSR      GESPVD
041500921105     C*
041600921105     C*----------
041700921105     C*
041800921102     C                   Z-ADD     NUMSF1        NRR1
041900921105     C                   MOVE      *ZERO         X                 2 0
042000921105     C  N30              SETOFF                                       06
042100921105     C*
0422009211181    C     X             DOWLT     12
042300921105     C     *IN30         ANDEQ     '0'
042400921118     C     TSMDTR        ANDGE     DA2AMG
042500921102     C                   SETOFF                                           90
042600921102     C*---------
042700921106     C* Gestione nome parzialiale per '*'  sul file CMTSM.
042800921105     C*
042900921106?    C                   EXSR      GESPDB
043000921106     C*
043100921106     C* Gestione controlli di validità
043200921106     C*
043300921106?    C                   EXSR      GESCHK
043400921102     C*---------
0435009211052    C  N90              DO
043600921105     C*
043700921106     C* Salvo il primo rec. per posizionare il cursore sulla prima
043800921106     C* riga della pagina sucessiva.
043900921106     C*
044000921106     C  N06NRR1          ADD       1             WZ9               9 0
044100921105     C  N06              SETON                                        06
044200921106     C*
044300921106     C* Aggiornamento sfl M001S1.
044400921106     C*
044500921106?    C                   EXSR      AGGSFL
044600921102     C*
044700921102     C                   SETOFF                                           22
044800921105     C                   ADD       1             X
044900921105     C                   ADD       1             NRR1
045000921102     C                   Z-ADD     NRR1          S1NRR
045100921102     C                   WRITE     M001S1
045200921102     C*
0453009211052-   C                   END
045400921113     C     KTIP          READE     CMTSM01L                               30
045500000925     C* Disattivo tasto ROLLUP.
045600000925     C                   MOVEL     *IN30         *IN07
0457009211051-   C                   END
045800921105     C*
045900930322     C**N06                Z-ADD1         WZ9     90
046000921102     C*
046100921102     C                   ENDSR
046200921106?     *
046300921112?    C     GESPVD        BEGSR
046400921112     C*
046500921112     C                   MOVEA     *BLANK        SK
046600921112     C     D2UTR         IFNE      *BLANK
046700921112     C                   MOVEA     D2UTR         SK
046800921112     C                   Z-ADD     1             R                 3 0
046900921112     C     '*'           LOOKUP    SK(R)                                  35
047000921112     C   35              SUB       1             R                        36
047100921112     C  N35              Z-ADD     10            R
047200921112     C                   ELSE
047300921112     C                   Z-ADD     0             R
047400921112     C                   END
047500921112     C*
047600921112     C                   MOVEA     *BLANK        SK
047700921112     C     D2UTL         IFNE      *BLANK
047800921112     C                   MOVEA     D2UTL         SK
047900921112     C                   Z-ADD     1             L                 3 0
048000921112     C     '*'           LOOKUP    SK(L)                                  35
048100921112     C   35              SUB       1             L                        36
048200921112     C  N35              Z-ADD     10            L
048300921112     C                   ELSE
048400921112     C                   Z-ADD     0             L
048500921112     C                   END
048600921112     C*
048700921112     C                   ENDSR
048800921112?     *
048900921112?     *----------------------------------------------------
049000921112?     * G E S P D B - GESTIONE NOME PARZIALE  FDB
049100921112?     *----------------------------------------------------
049200921112?     *
049300921112?    C     GESPDB        BEGSR
049400921112     C*-----
049500921112     C                   MOVEA     *BLANK        SK
049600921112     C     TSMUTR        IFNE      *BLANK
049700921112     C                   MOVEA     TSMUTR        SK
049800921112     C                   Z-ADD     1             RD                3 0
049900921112     C     '*'           LOOKUP    SK(RD)                                 35
050000921112     C   35              SUB       1             RD                       36
050100921112     C  N35              Z-ADD     10            RD
050200921112     C                   ELSE
050300921112     C                   Z-ADD     10            RD
050400921112     C                   END
050500921112     C     R             IFGT      RD
050600921112     C                   Z-ADD     RD            R1                3 0
050700921112     C                   ELSE
050800921112     C                   Z-ADD     R             R1
050900921112     C                   END
051000921112     C     R1            IFNE      0
051100921112     C     R1            SUBST(P)  D2UTR:1       WK1UTR           10
051200921112     C     R1            SUBST(P)  TSMUTR:1      WK2UTR           10
051300921112     C                   ELSE
051400921112     C                   MOVE      *BLANK        WK1UTR
051500921112     C                   MOVE      *BLANK        WK2UTR
051600921112     C                   END
051700921112     C*
051800921112     C*
051900921112     C*-----
052000921112     C                   MOVEA     *BLANK        SK
052100921112     C     TSMUTL        IFNE      *BLANK
052200921112     C                   MOVEA     TSMUTL        SK
052300921112     C                   Z-ADD     1             LD                3 0
052400921112     C     '*'           LOOKUP    SK(LD)                                 35
052500921112     C   35              SUB       1             LD                       36
052600921112     C  N35              Z-ADD     10            LD
052700921112     C                   ELSE
052800921112     C                   Z-ADD     10            LD
052900921112     C                   END
053000921112     C     L             IFGT      LD
053100921112     C                   Z-ADD     LD            L1                3 0
053200921112     C                   ELSE
053300921112     C                   Z-ADD     L             L1
053400921112     C                   END
053500921112     C     L1            IFNE      0
053600921112     C     L1            SUBST(P)  D2UTL:1       WK1UTL           10
053700921112     C     L1            SUBST(P)  TSMUTL:1      WK2UTL           10
053800921112     C                   ELSE
053900921112     C                   MOVE      *BLANK        WK1UTL
054000921112     C                   MOVE      *BLANK        WK2UTL
054100921112     C                   END
054200921112     C*
054300921112     C*
054400921112     C                   ENDSR
054500921106?     *----------------------------------------------------
054600921106?     * G E S C H K - GESTIONE CONTROLLI DI VALIDITA'
054700921106?     *----------------------------------------------------
054800921106?     *
054900921106?    C     GESCHK        BEGSR
055000921106     C*
055100921106     C* Gestione nome parziale
055200921106     C*
055300921112     C     WK1UTR        IFNE      WK2UTR
055400921112     C     WK1UTL        ORNE      WK2UTL
055500921112     C                   SETON                                        90
0556009211121-   C                   END
055700921106     C*
055800921106     C* Gestione filiale
055900921106     C*
0560009211061    C  N90D2FIR         IFNE      *ZERO
056100921112     C     D2FIR         ANDNE     *BLANK
056200921112     C                   MOVE      D2FIR         WZ3               3 0
056300921112     C     TSMFIR        COMP      WZ3                                9090
0564009211061-   C                   END
0565009211231    C  N90D2FIL         IFNE      *ZERO
056600921123     C     D2FIL         ANDNE     *BLANK
056700921123     C                   MOVE      D2FIL         WZ3               3 0
056800921123     C     TSMFIL        COMP      WZ3                                9090
0569009211231-   C                   END
057000921106     C*
057100921106     C* Gestione doc. da caricare.
057200921106     C* Carica solo i documenti di risposta al doc. selezionato D2DOC.
057300921106     C*
0574009211061    C  N90D2DOC         IFNE      *ZERO
057500921106     C     TSMDOS        COMP      D2DOC                              9090
057600921106     C  N90TSMTIP        COMP      'A'                                9090
0577009211061-   C                   END
057800921106     C*
057900921218     C* Gestione doc. CHE PREVEDONO  risposta
058000921106     C*
0581009212181    C  N90D2RSP         IFEQ      'S'
058200921106     C     TSMRIS        COMP      'S'                                9090
0583009211061-   C                   END
058400921106     C*
058500921218     C*
058600921218     C* Gestione doc. CHE ATTENDONO  risposta
058700921218     C*
0588009212181    C  N90D2RIS         IFEQ      'S'
058900921218     C     TSMRIS        IFNE      'S'
059000921218     C     TSMSTS        OREQ      '5'
059100921218     C                   SETON                                        90
059200921218     C                   END
0593009212181-   C                   END
059400921113     C*
059500970410     C* Gestione solo nuovi + NON RISPOSTI QUELLI CHE LO PREVEDONO
059600921113     C*
0597009211131    C  N90D2NEW         IFEQ      'S'
0598009211131    C     D2TIP         ANDEQ     'A'
059900970410     C                   MOVE      *BLANK        §LBFL             1
060000970410     C     TSMSTS        IFEQ      '1'
060100970410     C     TSMRIS        OREQ      'S'
060200970410     C     TSMSTS        ANDNE     '5'
060300970410     C                   MOVE      'Y'           §LBFL
0604009704101-   C                   END
060500970410     C     §LBFL         COMP      'Y'                                9090
060600970410     C****       TSMSTS    COMP '1'                  9090
0607009211131-   C                   END
060800921113     C*
060900921106     C                   ENDSR
061000921106?     *
061100921106?     *----------------------------------------------------
061200921106?     * A G G S F L - AGGIORNAMENTO SFL M001S1
061300921106?     *----------------------------------------------------
061400921106?     *
061500921106?    C     AGGSFL        BEGSR
061600921106     C*
061700921106     C* Per documenti INVIATI (STS=3) controllo se è stato
061800921106     C* consegnato nel caso aggiorno.
061900921106     C*
062000921113     C     TSMSTS        IFEQ      '3'
062100921106     C                   MOVE      'I'           KOPE
062200921106     C                   MOVE      TSMTRA        KTRA
062300921118     C     KLOG01        CHAIN     CMLOG01L                           31
062400921118     C     *IN31         DOWEQ     *OFF
062500921118     C     LOGTXT        ANDNE     *BLANK
062600921118     C     KLOG01        READE     CMLOG01L                               31
062700921118     C                   END
0628009211061    C  N31              DO
062900921118C    C     TSMDOL        CHAIN     CMTSM04L                           31
063000921106     C                   MOVE      '4'           TSMSTS
063100921118     C                   Z-ADD     LOGDAT        TSMDTI
063200921118     C                   Z-ADD     LOGORA        TSMORI
063300921118     C                   UPDATE    TSM0004
0634009211061-   C                   END
0635009211131-   C                   END
063600921106     C*
063700921106     C*
063800921106     C                   MOVE      *ZERO         S1OPZ
063900921106     C                   MOVEL     TSMTXT        S1TXT
064000921106     C                   MOVE      TSMSTS        S1STS
064100921113     C                   MOVE      S1STS         I                 3 0
064200921113     C                   MOVE      STS(I)        S1STSD
064300921113     C                   MOVE      TSMDOR        S1DOR
064400921113     C     S1STS         COMP      '1'                                    03
064500921106     C                   Z-ADD     TSMDTR        S1DAT
064600921106     C                   Z-ADD     TSMDTR        AMG
064700921106     C                   MOVE      AA            S1DAT
064800921106     C                   MOVEL     GG            S1DAT
064900921112     C*
065000921112     C                   Z-ADD     TSMORI        S1ORI
065100921112     C                   Z-ADD     TSMDTI        S1DTI
065200921112     C                   Z-ADD     TSMDTI        AMG
065300921112     C                   MOVE      AA            S1DTI
065400921112     C                   MOVEL     GG            S1DTI
065500921112     C*
065600921106     C                   Z-ADD     TSMFIR        S1FIL
065700921111     C                   MOVE      *BLANK        S1FILD
065800930401     C                   MOVEL     TSMUTL        S1FILD
065900941024     C                   MOVEL     TSMUTL        S1UTNH
066000930401     C*                    MOVE S1FIL     £FIL
066100930401?    C*                    EXSR CHKFIL
066200930401     C*          £STS      COMP 'ERR'                    35
066300930401     C* N35                MOVEL£DES      S1FILD
066400930401     C*  35                MOVEL*ALL'?'   S1FILD
066500921112     C*
066600921112     C                   MOVE      TSMSTP        S1STP
066700921106     C                   Z-ADD     TSMDTS        H1DTS
066800921112     C                   MOVE      TSMRIS        S1RIS
066900921112     C                   Z-ADD     TSMDTP        H1DTP
067000921106     C                   Z-ADD     TSMDOC        H1DOC
067100921110     C                   Z-ADD     TSMDOR        H1DOR
067200921113     C                   MOVE      TSMURR        H1URG
067300921106     C                   MOVE      TSMMSR        H1MSR
067400921106     C                   Z-ADD     TSMDOL        S1DOL
067500921110     C                   Z-ADD     TSMDOR        H1DOR
067600921106     C                   MOVEL     TSMUTR        S1UTN
067700921106     C*
067800921106     C                   ENDSR
067900921102?     *
068000921102?     *----------------------------------------------------
068100921102?     * S 1 C T L   - CONTROLLI SUL SFL M001S1
068200921102?     *----------------------------------------------------
068300921102?     *
068400921102?    C     S1CTL         BEGSR
068500921102     C*
068600921102     C*
068700921102     C                   READC     M001S1                                 32
068800921102?    C*
068900921102?    C* SPENGO GLI IND. D'ERRORE DEL SUBFILE
069000921102?    C*
069100921102     C                   SETOFF                                       010299
0692009211021    C     *IN32         DOWEQ     '0'
069300921112     C                   SETOFF                                       707172
069400921102?    C*
069500921102     C                   SETON                                        22
069600921102?    C*
069700921102?    C*  CONTROLLI
069800921102?    C*
0699009211032    C     S1OPZ         IFNE      *ZERO
070000921102?    C****
070100921102?    C* OPZ=2 MODIFICA
070200921102?    C****
0703009211033    C     S1OPZ         IFEQ      2
070400921118     C                   MOVE      ' '           M02BLO
070500921118     c* permetto la modifica solo per doc in partenza non spediti
070600921118     c*
070700921118     C     S1STS         IFNE      '9'
070800921118     C                   MOVE      'S'           M02BLO
070900921118     C                   END
071000921102     C                   CALL      'CMZM02R'
071100921106     C                   PARM      S1DOL         M02DCR            7 0
071200921106     C                   PARM      S1DOL         M02DCA            7 0
071300921102     C                   PARM      *ZERO         M02FIL            3 0
071400921102     C                   PARM      *BLANK        M02UTN           10
071500921102     C                   PARM      *BLANK        M02URG            1
071600921113     C                   PARM      'N'           M02MSG            1
071700921110     C                   PARM      *ZERO         M02DOS            7 0
071800921112     C                   PARM      'N'           M02FLG            1
071900921118     C                   PARM                    M02BLO
072000000929     C                   MOVE      'N'           S1STAI
072100921111     C*
072200921111     C* Porto lo stato del doc. da 1(Nuovo) a 2(Aperto) per posta
072300921112     C* in arrivo valorizz. anche la data lettura con udate.
072400921111     C*
0725009211114    C  N04S1STS         IFEQ      '1'
072600921112     C     S1DOL         CHAIN     CMTSM04L                           36
072700921112     C*
072800921111     C                   MOVE      '2'           TSMSTS
0729009211125    C     TSMDTL        IFEQ      *ZERO
073000930322C    C                   EXSR      GETDAT
073100930322     C                   MOVE      $CVT2         TSMDTL
0732009211125-   C                   END
073300921112     C*
073400921112     C                   UPDATE    TSM0004
0735009211124-   C                   END
073600921111     C*
073700980122     c* SE posta in arrivo e richiesta stampa stampo
073800980122     c*    SE MAI STAMPATA
073900930322     C*
074000930322     C     S1STP         IFEQ      'S'
074100980122     C     H1DTS         ANDEQ     *ZERO
074200980122     C*
074300930406     C                   MOVE      *BLANK        M03OUT
074400930322     C                   CALL      'CMZM03C'
074500930322     C                   PARM                    S1DOL
074600930322     C                   PARM                    M03OUT           10
074700930322     C                   END
0748009211023-   C                   END
074900921218?    C****
075000921218?    C* OPZ=5 VISUALIZZA
075100921218?    C****
0752009212183    C     S1OPZ         IFEQ      5
075300921218     c*
075400921218     c*
075500921218     C                   MOVE      'S'           M02BLO
075600921218     C                   CALL      'CMZM02R'
075700921218     C                   PARM      S1DOL         M02DCR            7 0
075800921218     C                   PARM      S1DOL         M02DCA            7 0
075900921218     C                   PARM      *ZERO         M02FIL            3 0
076000921218     C                   PARM      *BLANK        M02UTN           10
076100921218     C                   PARM      *BLANK        M02URG            1
076200921218     C                   PARM      'N'           M02MSG            1
076300921218     C                   PARM      *ZERO         M02DOS            7 0
076400921218     C                   PARM      'N'           M02FLG            1
076500921218     C                   PARM                    M02BLO
076600000925     C**                   MOVE 'N'       S1STAI
076700921218     C*
076800921218     C*
0769009212183-   C                   END
077000921102?    C****
077100921102?    C* OPZ=3 COPIA
077200921102?    C****
0773009211033    C     S1OPZ         IFEQ      3
077400921103     C                   CALL      'CMZM02R'
077500921103     C                   PARM      *ZERO         M02DCR
077600921106     C                   PARM      S1DOL         M02DCA
077700921103     C                   PARM      *ZERO         M02FIL
077800921103     C                   PARM      *BLANK        M02UTN
077900921103     C                   PARM      *BLANK        M02URG
078000921113     C                   PARM      'N'           M02MSG
078100921110     C                   PARM      *ZERO         M02DOS
078200921112     C                   PARM      'N'           M02FLG
078300921118     C                   PARM      ' '           M02BLO
078400000925     C**                   MOVE 'N'       S1STAI
0785009211023-   C                   END
078600921102?    C****
078700921102?    C* OPZ=4 ANNULLA
078800921102?    C****
0789009211033    C     S1OPZ         IFEQ      4
079000921118     C     *INKQ         ANDEQ     *ON
079100921102?    C                   EXSR      GESANN
079200000929     C  N71
079300000929     CANN72              MOVE      'N'           S1STAI
0794009211023-   C                   END
079500921102?    C****
079600921102?    C* OPZ=6 STAMPA
079700921102?    C****
0798009211033    C     S1OPZ         IFEQ      6
079900930406     C*****
080000940624     C***        C1VIDE    CHAINKFTOD10L             88
080100930406     C                   MOVE      *BLANK        M03OUT
080200940624     C**N88                MOVELKKQOT     M03OUT
080300930406     C*****
080400921106     C                   CALL      'CMZM03C'
080500921106     C                   PARM      S1DOL         M03DOL            7 0
080600930301     C                   PARM                    M03OUT           10
080700000925     C**                   MOVE 'N'       S1STAI
080800921112     C*
080900921112     C* Aggiorno la data di stampa.
081000921112     C*
0811009211124    C     H1DTS         IFEQ      *ZERO
081200921112     C     S1DOL         CHAIN     CMTSM04L                           36
081300921112     C*
081400930322     C                   EXSR      GETDAT
081500930322     C                   MOVE      $CVT2         TSMDTS
081600921112     C*
081700921112     C                   UPDATE    TSM0004
0818009211124-   C                   END
0819009211123-   C                   END
082000921102?    C****
082100921102?    C* OPZ=10 INVIO
082200921102?    C****
0823009211033    C     S1OPZ         IFEQ      10
082400921102     C                   CALL      'CMZM04R'
082500921106     C                   PARM      S1DOL         M04DOL            7 0
082600921102     C                   PARM      'I'           M04FLG            1
082700921103     C                   PARM      *ZERO         M04FIR            3 0
082800921102     C                   PARM      *BLANK        M04UTN           10
082900921102     C                   PARM      *BLANK        M04URG            1
083000921102     C                   PARM      *BLANK        M04MSG            1
083100921113     C                   PARM      'N'           M04SPE            1
083200000925     C**                   MOVE 'N'       S1STAI
0833009211023-   C                   END
083400921102?    C****
083500921102?    C* OPZ=11 RISPOSTA
083600921102?    C****
0837009211033    C     S1OPZ         IFEQ      11
083800921103     C*
083900921103     C* Per posta in arrivo invio una risposta
084000921103     C*
0841009211034    C     KTIP          IFEQ      'A'
084200921103     C                   CALL      'CMZM02R'
084300921103     C                   PARM      *ZERO         M02DCR
084400921118     C                   PARM      S1DOL         M02DCA
084500921103     C                   PARM      S1FIL         M02FIL
084600941024     C                   PARM      S1UTN         M02UTN
084700921103     C                   PARM      H1URG         M02URG
084800921103     C                   PARM      H1MSR         M02MSG
084900921110     C                   PARM      H1DOR         M02DOS
085000921112     C                   PARM      'N'           M02FLG
085100921118     C                   PARM      ' '           M02BLO            1
085200921112     C*
085300921112     C* Aggiorno la data di risposta.
085400921112     C*
0855009211125    C     M02FLG        IFEQ      'S'
085600921112     C     S1DOL         CHAIN     CMTSM04L                           36
0857009705226    C****       TSMDTP    IFEQ *ZERO
0858009705226    C     TSMSTS        IFNE      '5'
085900921112     C*
086000930322     C                   EXSR      GETDAT
086100930322     C                   MOVE      $CVT2         TSMDTP
086200921112     C                   MOVE      '5'           TSMSTS
086300921112     C*
086400921112     C                   UPDATE    TSM0004
0865009211126-   C                   END
0866009211125-   C                   END
086700921103     C*
086800921103     C* Per posta in partenza visualizzo le risposte ricevute
086900921103     C*
0870009211034E   C                   ELSE
087100921103     C*
087200921103     C* Salvo i parametri
087300921103     C                   MOVE      D2TIP         SAVTIP            1
087400921103     C                   MOVE      D2UTR         SAVUTR           10
087500921112     C                   MOVE      D2FIR         SAVFIR            3
087600921123     C                   MOVE      D2FIL         SAVFIL            3
087700921103     C                   MOVE      D2UTL         SAVUTL           10
087800921103     C                   MOVE      D2DAT         SAVDAT            6 0
087900921118     C                   MOVE      D2DA2         SAVDA2            6 0
088000921103     C                   MOVE      D2DOC         SAVDOC            7 0
088100921103     C                   MOVE      D2RIS         SAVRIS            1
088200921103     C*
088300921103     C* Imposto i nuovi
088400921118     C                   MOVE      'A'           D2TIP
088500921118     C                   MOVE      *BLANK        D2UTR
088600921118     C                   MOVE      *BLANK        D2FIR
088700921123     C                   MOVE      *BLANK        D2FIL
088800921118     C                   MOVEL     *BLANK        D2UTL
088900921118     C                   MOVE      UDATE         D2DAT
089000921118     C                   Z-ADD     0             D2DA2
089100921118     C                   MOVE      'N'           D2RIS
089200921118     C                   MOVE      'N'           D2NEW
089300921118     C                   MOVE      S1DOL         D2DOC
089400921103     C                   SETON                                        05
0895009211034-   C                   END
089600921103     C*
089700000925     C                   MOVE      'N'           S1STAI
0898009211023-   C                   END
089900921102?    C****
090000921102?    C* OPZ=15 SOLLECITO
090100921102?    C****
090200921103     C*
090300921103     C* OPZ. SOLLECITI (15) NON ABILITATA SE SONO IN RISPOSTA (*IN05='1')
090400921103     C*
0905009211033    C     S1OPZ         IFEQ      15
090600921103     C     *IN05         ANDEQ     '1'
090700921103     C                   SETON                                        70
0908009211033-   C                   END
090900921103     C*
0910009211033    C  N70S1OPZ         IFEQ      15
091100921113     C                   MOVE      S1FIL         M04FIR
091200921102     C                   CALL      'CMZM04R'
091300921106     C                   PARM      S1DOL         M04DOL
091400921102     C                   PARM      'S'           M04FLG
091500921112     C                   PARM                    M04FIR
091600941024     C                   PARM      S1UTN         M04UTN
091700921102     C                   PARM      H1URG         M04URG
091800921102     C                   PARM      'S'           M04MSG
091900921113     C                   PARM      'N'           M04SPE            1
092000921102     C                   MOVE      'N'           S1STAI
0921009211023-   C                   END
092200921102     C*---
092300921102     C   01
092400921102     CAN 02              SETOFF                                         01
092500921102     C   70
092600921102     CANN02
092700921102     COR 71
092800921102     CANN02
092900921102     COR 72
093000921102     CANN02              SETON                                        010299
093100921102     C   01
093200921102     CAN 02              Z-ADD     NRR1          C1SRN
093300921102     C*
0934009211022E   C                   ELSE
093500941024     C* reindirizzamento posta ricevuta
093600941024     C*
093700941024     C     S1FILD        IFNE      S1UTNH
093800941024     C     S1DOL         CHAIN     CMTSM04L                           36
093900941024     C*
094000941024     C                   MOVE      S1FILD        TSMUTL
094100941024     C                   MOVE      '1'           TSMSTS
094200941024     C*
094300941024     C                   UPDATE    TSM0004
094400941024     C                   ENDIF
094500921102     C                   SETOFF                                       22
0946009211022-   C                   END
094700921102     C*
094800000925     C                   CLEAR                   S1OPZ
094900921102     C                   UPDATE    M001S1
095000921102     C                   READC     M001S1                                 32
0951009211021-   C                   END
095200921102     C*
095300921102     C                   ENDSR
095400921102?     *
095500921102?     *------------------------------------------------------
095600921102?     * G E S A N N - GESTIONE OPZIONE DI ANNULLAMENTO
095700921102?     *------------------------------------------------------
095800921102?     *
095900921102?    C     GESANN        BEGSR
096000921105     C*----
096100921102     C* SE POSTA IN ARRIVO
0962009211021    C     *IN04         IFEQ      '0'
096300921105     C*
096400921102     C* Stampare il doc. prima di cancellarlo
096500921102     C*
0966009211122    C     S1STP         IFEQ      'S'
096700921102     C     H1DTS         ANDEQ     *ZERO
096800921102     C                   SETON                                        71
0969009211022-   C                   END
097000921102     C*
097100921102     C* Rispondere prima di cancellarlo
097200921102     C*
0973009211122    C     S1RIS         IFEQ      'S'
097400921112     C     H1DTP         ANDEQ     *ZERO
097500921102     C                   SETON                                        72
0976009211022-   C                   END
097700921105     C*
0978009211021-   C                   END
097900921105     C*----
098000921105     C*
098100921105     C  N71
0982009211051    CANN72              DO
098300921102     C*
098400921102     C* Cancello record di testata posta
098500921102     C*
098600921112     C     S1DOL         CHAIN     CMTSM04L                           33
098700921106     C     S1DOL         DELETE    CMTSM04L                           33
098800921102     C*
098900921102     C* Controllo se ci sono atri documenti con lo stesso cod. riga
099000921102     C* in caso negativo cancello le righe del doc.
099100921102     C*
099200921102     C     H1DOC         CHAIN     CMTSM03L                           33
099300921112     C   33H1DOC         CHAIN     CMRGM01L                           34
0994009211052    C   33              DO        *HIVAL
099500921102     C     H1DOC         DELETE    CMRGM01L                           34
0996009211022-   C  N34              END
099700921102     C*
0998009211051-   C                   END
099900921105     C*
100000921102     C*
100100921102     C                   ENDSR
100200921103?     *
100300921103?     *------------------------------------------------------
100400921103?     * G E S D 2   - GESTIONE VIDEATE PARAMETRIZZ.
100500921103?     *------------------------------------------------------
100600921103?     *
100700921103?    C     GESD2         BEGSR
100800921103     C*
100900921103     C* Salvo i parametri
101000921105     C                   MOVE      D2TIP         SV2TIP            1
101100921105     C                   MOVE      D2UTR         SV2UTR           10
101200921112     C                   MOVE      D2FIR         SV2FIR            3
101300921123     C                   MOVE      D2FIL         SV2FIL            3
101400921105     C                   MOVE      D2UTL         SV2UTL           10
101500921105     C                   MOVE      D2DAT         SV2DAT            6 0
101600921118     C                   MOVE      D2DA2         SV2DA2            6 0
101700921105     C                   MOVE      D2DOC         SV2DOC            7 0
101800921105     C                   MOVE      D2RIS         SV2RIS            1
101900930121     C*                    WRITEM001W1
102000921103     C                   MOVE      'S'           D2STAI
102100921103     C*
1022009211031    C     D2STAI        DOWEQ     'S'
102300921103     C*
102400921103     C                   EXFMT     M001D2
102500921103?    C***
102600921103?    C* F12= RITORNO
102700921103?    C***
1028009211032    C   KL              DO
102900921103     C*
103000921103     C* In caso di uscita con errori ripristino i parametri precedenti
103100921103     C*
103200921104     C                   MOVE      'N'           D2STAI
1033009211133    C*          *IN99     IFEQ '1'
103400921105     C                   MOVE      SV2TIP        D2TIP
103500921105     C                   MOVE      SV2UTR        D2UTR
103600921105     C                   MOVE      SV2FIR        D2FIR
103700921123     C                   MOVE      SV2FIL        D2FIL
103800921105     C                   MOVE      SV2UTL        D2UTL
103900921105     C                   MOVE      SV2DAT        D2DAT
104000921118     C                   MOVE      SV2DA2        D2DA2
104100921105     C                   MOVE      SV2DOC        D2DOC
104200921105     C                   MOVE      SV2RIS        D2RIS
1043009211133E   C*                    ELSE
1044009211133-   C*                    END
1045009211032-   C                   END
104600921103     C*
104700921103?    C  NKL              EXSR      D2CTL
104800921113C    C  N99              MOVE      'N'           D2STAI
104900921103     C*
1050009211031-   C                   END
105100921113     C     IN04          COMP      '1'                                    04
105200921113?    C                   EXSR      S1CAR
105300921104     C                   MOVE      '0'           *INKL
105400921105     C                   MOVE      '1'           *INKR
105500921103     C*
105600921103     C                   ENDSR
105700921103?     *
105800921103?     *----------------------------------------------------
105900921103?     * D 2 C T L   - CONTROLLI SUL FMT M001D2
106000921103?     *----------------------------------------------------
106100921103?     *
106200921103?    C     D2CTL         BEGSR
106300921103     C*
106400921103     C                   SETOFF                                       99
106500921103?    C*
106600921103?    C*                **  TIPO RECORD     **
106700921103?    C*
106800921103     C                   MOVE      *BLANK        D2TIPD
1069009211121    C     D2TIP         IFEQ      'A'
107000921104     C                   MOVE      '0'           IN04              1
107100921103     C                   MOVEL     TXT1          D2TIPD
1072009211121E   C                   ELSE
107300921104     C                   MOVE      '1'           IN04
107400921103     C                   MOVEL     TXT2          D2TIPD
1075009211121-   C                   END
107600921103?    C*
107700921103?    C*                **  FILIALE REMOTA  **
107800921103?    C*
1079009211111    C     D2FIR         IFNE      *ZERO
108000921112     C     D2FIR         ANDNE     *BLANK
108100921112     C                   MOVE      D2FIR         £FIL
108200921112?    C                   EXSR      CHKFIL
108300921112     C     £STS          COMP      'ERR'                                  73
108400921112     C                   MOVE      £FIL          D2FIR
1085009211111-   C                   END
108600921123?    C*
108601021004     C* controllo filiale  LOCALE
108602021004     C*     deve essere valida
108603021004     C*
108605021019     C*     se l'utente non è POC REA RED la filiale può essere
108606021004     C*     solo quella dell'utente
108607021019     c*     se sei POC REA RED una dell'area (succevvisamente Capo Area)
108608021004     c*     se sei edp vedi tutto
108609021019     c*
108610021019     c* se senza AZUTE testo ancora il profilo
108611021019     c*
108612021004     c                   movel     DUTPOU        WWpoUA            3
1087009211231    C     D2FIL         IFNE      *ZERO
108800921123     C     D2FIL         ANDNE     *BLANK
108900921123     C                   MOVE      D2FIL         £FIL
109000921123?    C                   EXSR      CHKFIL
109100921123     C     £STS          COMP      'ERR'                                  78
109200921123     C                   MOVE      £FIL          D2FIL
1093000210041-   C                   ENDIF
109301021004     c  n78              do
109302021004     c* EDP
109303021021     c***                if        UTEFUN  ='     ' AND C1NPRE <> 'EDP' OR
109304021021     c**                           UTEFUN  <>'EDPSE' AND UTEFUN <> '     '
109305021021     c* per adesso sugli altri AS vedono tutto
109306021021     c                   if        UTEFUN  ='     ' AND C1NPRE <> 'OPR' and
109307021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'SEG' and
109308021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'EDP' and
109309021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'ARE' and
109310021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIS' and
109311021023     c                             UTEFUN  ='     ' AND C1NPRE <> 'ASC' and
109312021025     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIR' and
109313030213     c                             UTEFUN  ='     ' AND C1NPRE <> 'ISP' and
109314040302     c                             UTEFUN  ='     ' AND C1NPRE <> 'INT' and
109315021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIV' or
109316021021     c                             UTEFUN  <>'EDPSE' AND UTEFUN <> '     '
109317021004     c* ALTRI
109318021019     c*
109319021019     c                   if        UTEFUN  ='     ' AND C1NPRE <> 'OPR' and
109320021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'SEG' and
109321021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'ARE' and
109322021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIS' and
109323030213     c                             UTEFUN  ='     ' AND C1NPRE <> 'ISP' and
109324040302     c                             UTEFUN  ='     ' AND C1NPRE <> 'INT' and
109325021023     c                             UTEFUN  ='     ' AND C1NPRE <> 'ASC' and
109326021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIV' or
109327021019     c                             UTEFUN  <>'     ' and
109328021019     c                             UTEFUN  <>'POC  ' AND
109329021023     c                             UTEFUN  <>'REA  ' AND
109330030213     c                             UTEFUN  <>'ISP  ' AND
109331040302     c                             UTEFUN  <>'INT  ' AND
109332021023     c                             UTEFUN  <>'RED  ' AND
109333021023     c                             UTEFUN  <>'RAC  '
109334021004     c     wwpoua        ifne      D2fil
109335021004     c                   seton                                        78
109336021004     c                   endif
109337021004     c                   else
109338021019     c* POC e abilitati
109339021004     c                   reset                   $POG
109340021004     c                   clear                   TRUL31DS
109341021004     c                   movel     'AR'          I31ABI
109342021004     c                   movel     DUTPOU        I31CPO
109343021004     c                   call      'TRUL31R'
109344021004     c                   parm                    KPJBA
109345021004     c                   parm                    TRUL31ds
109346021004if  3c                   if        O31pog > *zeros
109347021004     c                   movea     O31pog        $POG
109348021004e   3c                   endif
109349021005     c     D2FIL         LOoKUP    $pog                                   58
109350021004     c  n58              seton                                        78
109351021005     c     D2FIL         ifle      *zero
109352021005     c                   seton                                        78
109353021005     c                   endif
109354021004     c*
109355021004     C                   endif
109356021004     C                   endif
109357021004     c                   enddo
109400921103?    C*
109500921118?    C*                **  DATA    1       **
109600921103?    C*
109700921103     C                   Z-ADD     D2DAT         XDTGMA
109800921103     C                   Z-ADD     3             XDTSTA
109900921103?    C                   EXSR      XDATA
110000921103     C     XDTSTA        COMP      0                                    74
110100921118     C                   Z-ADD     XDTAMG        DATAMG            7 0
110200921118?    C*                **  DATA    2       **
110300921118?    C*
110400921118     C                   Z-ADD     0             DA2AMG
110500921118     C     D2DA2         IFNE      0
110600921118     C                   Z-ADD     D2DA2         XDTGMA
110700921118     C                   Z-ADD     3             XDTSTA
110800921118?    C                   EXSR      XDATA
110900921118     C     XDTSTA        COMP      0                                    77
111000921118     C  N77              Z-ADD     XDTAMG        DA2AMG            7 0
111100921118     C                   END
111200921103     C*
111300921103     C   73
111400921118     COR 74
111401021004     COR 78
111500921118     COR 77              SETON                                        99
111600921103     C*
111700921103     C                   ENDSR
111800921112?     *
111900921112?     *----------------------------------------------------
112000921112?     * C H K F I L - CONTROLLI FILIALE
112100921112?     *----------------------------------------------------
112200921112?     *
112300921112?    C     CHKFIL        BEGSR
112400921112     C*
112500921112     C                   MOVE      *BLANK        £DES             25
112600921112     C                   MOVE      *BLANK        £STS              3
112700921112     C                   SETOFF                                           91
112800921112     C*
1129009211121    C     £FIL          IFNE      *BLANKS
113000921112     C     £FIL          ANDNE     *ZEROS
113100921112     C*
113200921112     C     '?'           SCAN      £FIL                                   37
1133009211122    C     *IN37         IFEQ      '1'
113400921112     C                   MOVE      *BLANK        £FIL              3
113500941125     C                   CALL      'TNSD19R'
113600921112     C                   PARM      *BLANK        §COD1             3
113700921112     C                   PARM      *BLANK        §TIP              1
113800921112     C                   PARM      *BLANK        §DES1            25
113900921112     C                   MOVEL     §COD1         £FIL
114000921112     C                   MOVEL     §DES1         £DES
1141009211122E   C                   ELSE
114200921112     C*
1143009211123    C     £FIL          IFGT      *ZEROS
114400921112     C                   MOVEL     £FIL          ORGFIL
114500921112     C     ORGFIL        CHAIN     AZORG01L                           91
1146009211123E   C                   ELSE
114700921112     C                   SETON                                        91
1148009211123-   C                   END
114900921112     C*
1150009211122-   C                   END
115100921112     C*
115200921112     C* CONTROLLO SE ANNULLATO
115300921112     C*
115400921112     C  N91ORGFVA        COMP      *BLANK                             9191
115500921112     C*
115600921112     C   91              MOVE      'ERR'         £STS
115700921112     C  N91              MOVEL     ORGDES        £DES
1158009211121-   C                   END
115900921112     C*
116000921112     C                   ENDSR
116100921103?     *
116200921103?     *----------------------------------------------------*
116300921103?     * XDATA    INVERSIONE E CONTROLLO DATE               *
116400921103?     *----------------------------------------------------*
116500921103?     *
116600921103     C*
116700921103?    C     XDATA         BEGSR
116800921103     C*
116900921103     C     XDTSTA        CABGT     4             XDTERR
117000921103     C     XDTSTA        CABLT     1             XDTERR
117100921103     C     XDTSTA        CABGT     2             XDTEI
117200921103     C*
117300921103     C* -- GIRA DATA INTERNA IN ESTERNA
117400921103     C*
117500921103     C     XDTAMG        DIV       100           XDTMM             2 0
117600921103     C     XDTAMG        DIV       10000         XDTAA             2 0
117700921103     C                   MOVE      XDTAMG        XDTGG             2 0
117800921103     C                   MOVE      XDTAMG        XDTGMA            6 0
117900921103     C                   MOVEL     XDTGG         XDTGMA
118000921103     C                   MOVE      XDTAA         XDTGMA
118100921103     C                   GOTO      XDTCON
118200921103     C*
118300921103     C     XDTEI         TAG
118400921103     C*
118500921103     C* -- GIRA DATA ESTERNA IN INTERNA
118600921103     C*
118700921103     C     XDTGMA        DIV       100           XDTMM
118800921103     C     XDTGMA        DIV       10000         XDTGG
118900921103     C                   MOVE      XDTGMA        XDTAA
119000921103     C                   MOVE      XDTGMA        XDTAMG            6 0
119100921103     C                   MOVEL     XDTAA         XDTAMG
119200921103     C                   MOVE      XDTGG         XDTAMG
119300921103     C                   SUB       2             XDTSTA            1 0
119400921103     C*                                                    * PER TEST CONT
119500921103     C     XDTCON        TAG
119600921103     C     XDTSTA        CABNE     1             XDTOK
119700921103     C*
119800921103     C* -- CONTROLLA LA VALIDITA* DELLA DATA
119900921103     C*
120000921103     C     XDTGG         CABLT     1             XDTERR
120100921103     C     XDTMM         CABLT     1             XDTERR
120200921103     C     XDTMM         CABGT     12            XDTERR
120300921103     C     XDTAA         DIV       4             XDTRES            2 0
120400921103     C                   MVR                     XDTRES
120500921103     C*
120600921103     C     XDTRES        IFEQ      0
120700921103     C                   Z-ADD     29            XDTF(2)
120800921103     C                   ELSE
120900921103     C                   Z-ADD     28            XDTF(2)
121000921103     C                   END
121100921103     C*
121200921103     C     XDTGG         CABGT     XDTF(XDTMM)   XDTERR
121300921103     C*                                                    * FINE MESE
121400921103     C     XDTOK         TAG
121500921103     C*                                                    * FINE MESE
121600921103     C* -- FINE NORMALE
121700921103     C*                                                    * FINE MESE
121800921103     C                   SUB       XDTSTA        XDTSTA
121900921103     C                   GOTO      XDTFIN
122000921103     C*                                                    * FINE MESE
122100921103     C     XDTERR        TAG
122200921103     C*                                                    * FINE MESE
122300921103     C* -- DATA O RICHIESTA ERRATA
122400921103     C*                                                    * FINE MESE
122500921103     C                   Z-SUB     1             XDTSTA
122600921103     C*                                                    * FINE MESE
122700921103     C     XDTFIN        TAG
122800921103     C*                                                    * FINE MESE
122900921103     C* -- FINE ISTRUZIONI ROUTINE
123000921103     C*
123100921103     C                   ENDSR
123200921102     C********************************************************************
123300921103  ************************************************************************
123400921113**  XDTF  X XDATA
123500921103312831303130313130313031
123600921113**
123700921113NUOVO
123800921113APERTO
123900921113INVIATO
124000921113CONSEGNATO
124100921113RISPOSTO
124200921113
124300921113
124400921113
124500921113NON INV.
