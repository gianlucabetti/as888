000100921102     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200921102?    F* CMZM01R *----------------------------------------------------*
000300921102?    F*---------*                                                    *
000400921102?    F*               - GESTIONE  POSTA -                            *
000500921102?    F*               -------------------                            *
000600040302?    F* RIMESSO TEST SU EDP,OPR, SEG, AREA, DIR, DIV, INT            *
000700021019?    F*                           (SENZA AZUTE NON SO CHI SONO)      *
000800921102?    F*                                                 AZ.          *
000900921102?    F*                                                              *
001000921102?    F*--------------------------------------------------------------*
001100921102?    F* I N D I C A T O R I                                          *
001200921102?    F*--------------------------------------------------------------*
001300921102     F*                                                              *
001400921102     F* -01- FLAG PER POSIZIONAMENTO SU SFL SUL I° REC ERRATO        *
001500921102     F* -02- FLAG PER POSIZIONAMENTO SU SFL SUL I° REC ERRATO        *
001600921113     F* -03- STATO NUOVO
001700921102     F* -04- FLAG PER GESTIONE ARRIVO / PARTENZA                     *
001800921102     F* -05- FLAG PER GESTIONE F12                                   *
001900921105     F* -06- FLAG GESTIONE CURSORE SFL SU PRIMA RIGA DI OGNI PAG.    *
002000000925     F* -07- OFF = tasto ROLLUP attivo.                              *
002100000925     F* -07- ON  = tasto ROLLUP non attivo.                          *
002200000925     F* -09- CONDIZIONAMENTO SFLDROP.                                *
002300921102     F* -20- SFLCLR              S1                                  *
002400921102     F* -N20-SFLDSPCTL           S1                                  *
002500921102     F* -21- SFLDSP/SFLEND       S1                                  *
002600921102     F* -22- SFLNXTCHG           S1                                  *
002700921102     F*                                                              *
002800921102     F* -30- LETTURA CMTSM01L                                        *
002900921102     F* -31- LETTURA CMLOG01L                                        *
003000921102     F* -32- LETTURA M001S1                                          *
003100921102     F* -33- LETTURA CMTSM04L                                        *
003200921102     F* -34- LETTURA CMRGM01L                                        *
003300921103     F* -35- LOKUP                                                   *
003400921112     F* -36- LETTURA CMTSM04L                                        *
003500921112     F* -37- SCAN '?'                                                *
003600921102     F*                                                              *
003700921102     F* -50- ROLLUP                                                  *
003800921102     F*                                                              *
003900921102     F* -70- ERR. M001S1 *- OPZIONE ERRATA                          -*
004000921102     F* -71- ERR. M001S1 *- STAMPARE IL DOC. PRIMA DI CANCELLARLO   -*
004100921102     F* -72- ERR. M001S1 *- RISPONDERE PRIMA DI CANCELLARLO         -*
004200921102     F*                                                              *
004300921103     F* -73- ERR. M001D2 *- FILIALE ERRATA                          -*
004400921103     F* -74- ERR. M001D2 *- DATA    ERRATA                          -*
004500921118     F* -77- ERR. M001D2 *- DATA    ERRATA                          -*
004600921123     F* -78- FILIALE LOCALE ERRATA
004700930406     F* -88- RICERCA SU KFTOD10L
004800921103     F*                                                              *
004900921102     F*  90 - FILTRO PER CARICAMENTO SUBFILE S1                      *
005000921112     F*  91 - ERRORE FILIALE                                         *
005100921102     F*  99 - ERRORE GENERICO                                        *
005200921102?    F*
005300921102?    F*--------------------------------------------------------------*
005400921102?    F*  DATA BASE                                                   *
005500921102?    F*--------------------------------------------------------------*
005600921102     F*
005700921102?    F*------------
005800921118     FCMTSM01L  IF   E           K DISK
005900921102?    F*------------
006000921102     FCMTSM03L  IF   E           K DISK
006100921102     F                                     RENAME(CMTSM000:TSM0003)
006200921102?    F*------------
006300921103     FCMTSM04L  UF   E           K DISK
006400921102     F                                     RENAME(CMTSM000:TSM0004)
006500921102?    F*------------
006600921103     FCMRGM01L  UF   E           K DISK
006700921102?    F*------------
006800921102     FCMLOG01L  IF   E           K DISK
006900921102?    F*------------
007000921112     FAZORG01L  IF   E           K DISK
007100940624     F**KFTOD10LIF  E           K        DISK
007200921102?    F*------------
007300921102     FCMZM01D   CF   E             WORKSTN
007400921102     F                                     SFILE(M001S1:NRR1)
007500921102     F                                     INFDS(INFO)
007600921103     D*
007700921103?    D*------------------------------------------------------------
007800921103?    D*  SCHIERE
007900921103?    D*------------------------------------------------------------
008000021004      * P.O. gestibili dall'utente
008100021004     D $POG            s              3    dim(250) inz(*zeros)
008200921103     D SK              S              1    DIM(10)
008300921103     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
008400921113     D STS             S             10    DIM(9) CTDATA PERRCD(1)
008500921102     D*
008600921102?    D*------------------------------------------------------------
008700921102?    D*  DS
008800921102?    D*------------------------------------------------------------
008900921102      *---
009000921102     D                SDS
009100930406     D  C1VIDE               244    253
009200020430     D*                                     254 263 C1NMUS
009300021015     D  C1NPRE               254    256
009400020430     D*                                     257 259 C1NFIL
009500921102      *---
009600921102     D INFO            DS
009700921102     D  NRRULT               378    379B 0
009800921102      *---
009900921102     D                 DS
010000921102     D  AA                     1      2  0
010100921102     D  MM                     3      4  0
010200921102     D  GG                     5      6  0
010300921102     D  AMG                    1      6  0
010400921123      *---
010500021004     D KPJBA         E DS
010600921123     D CMZ001        E DS                  EXTNAME(CMZ001DS)
010700021004     D TRUL31ds      E DS                  inz
010800020430     D tibs34ds      E DS                  inz
010900020430     D ddatiute      e ds
011000020430     D azuteds       e ds                  extname(AZUTE00F)
011100020430     D dLAT          e ds
011200921102      *---
011300921102     D TXT1            C                   CONST('Posta in Arrivo')
011400921102     D TXT2            C                   CONST('Posta in Partenza')
011500921102      *---
011600921102?     *
011700921102?     *------------------------------------------------------------
011800921102?     *  FLUSSO PRINCIPALE
011900921102?     *------------------------------------------------------------
012000921102?     *
012100921102?    C                   EXSR      INIZIO
0122009211021    C     EMETTI        DOWEQ     'S'
012300921102     C*
012400921102?    C                   EXSR      D2INZ
012500921102?    C                   EXSR      S1CAR
012600921102     C*
0127009211022    C     S1STAI        DOWEQ     'S'
012800921102     C*
012900921102     C                   SETOFF                                       50
013000000925     C                   MOVEL     C1MODE        *IN09
013100000925     C     C1SRN         IFLE      0
013200000925     C     C1SRN         ORGT      NRR1
013300000925     C                   Z-ADD     1             C1SRN
013400000925     C                   ENDIF
013500921102     C                   EXFMT     M001C1
013600921102?    C***
013700921102?    C* F3 = FINE
013800921102?    C***
013900921102     C   KC              MOVE      'N'           EMETTI
014000921102     C   KC              MOVE      'N'           S1STAI
014100921102?    C***
014200921102?    C* F5 = RIVISUALIZZA
014300921102?    C***
014400921102     C   KE              MOVE      'N'           S1STAI
014500921102?    C***
014600921102?    C* F6 = CREA DOCUMENTO
014700921102?    C***
0148009211023    C   KF              DO
014900921102     C                   CALL      'CMZM02R'
015000921102     C                   PARM      *ZERO         M02DCR
015100921102     C                   PARM      *ZERO         M02DCA
015200921102     C                   PARM      *ZERO         M02FIL
015300921102     C                   PARM      *BLANK        M02UTN
015400921102     C                   PARM      *BLANK        M02URG
015500921113     C                   PARM      'N'           M02MSG
015600921110     C                   PARM      *ZERO         M02DOS
015700921112     C                   PARM      'N'           M02FLG
015800921118     C                   PARM      ' '           M02BLO
015900921102     C                   MOVE      'N'           S1STAI
0160009211023-   C                   END
016100921102?    C***
016200921102?    C* F17= PARZIALIZZAZIONE
016300921102?    C***
016400921105?    C   KR              EXSR      GESD2
016500921102?    C***
016600921102?    C* F20= GESTIONE POSTA IN ARRIVO/PARTENZA
016700921102?    C***
016800921105     C   KU              MOVE      'N'           S1STAI
016900921103?    C***
017000921103?    C* F12= RITORNO
017100921103?    C***
017200921105     C   KL              MOVE      'N'           S1STAI
017300921102     C*
017400921105     C*------------
017500921102?    C***
017600921102?    C* ROLL-UP
017700921102?    C***
0178009211033    C     NRR1          IFGT      *ZERO
0179009211034    C     *IN50         IFEQ      '1'
018000921105     C*
018100921102?    C                   EXSR      CARICA
018200921102     C*
018300921102     C                   Z-ADD     NRR1          NUMSF1
018400921105     C                   Z-ADD     WZ9           C1SRN
018500921102     C*
018600921102     C*
0187009211034E   C                   ELSE
018800921103     C  NKC
018900921103     CANNKE
0190009211035    CANNKF
019100921103     CANNKR
019200921103     CANNKU
019300921103     CANNKL              DO
019400921105     C                   Z-ADD     NRRULT        SRNSAV            4 0
019500921102?    C                   EXSR      S1CTL
019600921102     C  N99              Z-ADD     SRNSAV        C1SRN
0197009211035-   C                   END
019800921102     C*
019900921105     C*
0200009211034-   C                   END
0201009211033-   C                   END
020200921105     C*------------
020300921102     C*
0204009211022-   C                   END
0205009211021-   C                   END
020600921102     C*
020700921102     C                   SETON                                            LR
020800921102?     *
020900921102?     *------------------------------------------------------------
021000921102?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
021100921102?     *------------------------------------------------------------
021200921102?     *
021300921102     C*
021400921102?    C     INIZIO        BEGSR
021500921102     C*
021600921102     C* CMTSM01L
021700921102     C     *LIKE         DEFINE    TSMTIP        KTIP
021800921103     C     *LIKE         DEFINE    TSMDTR        KDTA
021900921123     C     *DTAARA       DEFINE                  CMZ001
022000921102     C     KTSM01        KLIST
022100921102     C                   KFLD                    KTIP
022200921102     C                   KFLD                    KDTA
022300921102     C*
022400921102     C* CMLOG01L
022500921102     C     *LIKE         DEFINE    LOGOPE        KOPE
022600921102     C     *LIKE         DEFINE    LOGTRA        KTRA
022700921102     C     KLOG01        KLIST
022800921102     C                   KFLD                    KTRA
022900921103     C                   KFLD                    KOPE
023000921102     C*
023100921123     C                   IN        CMZ001
023200921102     C                   MOVE      'S'           EMETTI            1
023300921102     C                   MOVE      'S'           S1STAI            1
023400921102     C                   MOVE      'S'           D2STAI            1
023500921103     C                   MOVE      '0'           *IN
023600020430     C*
023700020430     c     *dtaara       define    §azute        azuteds
023800020430     c     *dtaara       define    §datiute      ddatiute
023900020509     C                   in        azuteds                              38
024000020509     C                   in        ddatiute                             39
024100020509     C                   IF        *in38 = '1' or *in39 = '1'
024200020509     c                             or RSUT = *blanks
024300020430     C                   CLEAR                   tibs34ds
024400020430     C                   CALL      'TIBS34R'
024500020430     C                   PARM                    tibs34ds
024600020509     C                   in        azuteds
024700020509     C                   in        ddatiute
024800020430     c                   endif
024900921105     C*
025000921105     C* IMPOSTO I PARAMETRI DI DEFAULT
025100921105     C*
025200921105     C                   MOVE      'A'           D2TIP
025300921111     C                   MOVE      *BLANK        D2UTR
025400921112     C                   MOVE      *BLANK        D2FIR
025500020430     C*          C1NPRE    IFEQ 'REM'
025600020430     C*          C1NPRE    OREQ 'SDI'
025700020430     C*                    MOVELC1NFIL    D2FIL
025800020430     C*                    ELSE
025900020430     C*                    MOVE £DST      D2FIL
026000020430     C*                    END
026100020430     C                   MOVE      dutpou        D2FIL
026200020430     C*          C1NPRE    IFEQ 'EDP'
026300020430     C*                    MOVEL'ED*     'D2UTL
026400020430     C*                    ELSE
026500020430     C*                    MOVELC1NMUS    D2UTL
026600020430     C*                    END
026700020430     C*          C1NPRE    IFEQ 'REM'
026800020430     C*          C1NPRE    OREQ 'SDI'
026900020430     C*                    MOVEL'        'D2UTL
027000020430     C*                    ENDIF
027100020509     c* Profili EDP
027200021015     c     UTEFUN        ifEQ      'EDPSE'
027300021015     C     C1NPRE        OREQ      'EDP'
027400020509     c                   movel     'ED*'         D2UTL
027500020509     c                   else
027600020430     C* SECONDO LIVELLO
027700020430     C     dutlpo        IFEQ      '2'
027800020430     C     dutlpo        OREQ      *BLANKS
027900020430     C                   MOVEL     '        '    D2UTL
028000020430     C                   ELSE
028100020430     C* PRIMO LIVELLO O SEDE
028200020430     C                   MOVEL     dutute        D2UTL
028300020430     C                   ENDIF
028400020509     C                   ENDIF
028500020430     C                   MOVEL     UDATE         D2DAT
028600020430     C***                  MOVEL311299    D2DAT
028700921118     C                   Z-ADD     0             D2DA2
028800921105     C                   MOVE      *ZERO         D2DOC
028900921105     C                   MOVE      'N'           D2RIS
029000921218     C                   MOVE      'N'           D2RSP
029100921113     C                   MOVE      'S'           D2NEW
029200921105     C                   SETOFF                                       04
029300000925     C                   MOVEL     *ON           C1MODE
029400921102     C*
029500921102     C                   ENDSR
029600930322?    C     GETDAT        BEGSR
029700930322     C                   TIME                    W012             12 0
029800930322     C                   MOVE      W012          DATA              6
029900930322     C                   MOVEL     DATA          $CVT1
030000930322     C                   CALL      'CVTDAT'
030100930322     C                   PARM                    $CVT1             8
030200930322     C                   PARM                    $CVT8             8
030300930322     C                   PARM      '*SYSVAL'     $FMT1             8
030400930322     C                   PARM      '*YMD'        $FMT2             8
030500930322     C                   PARM      '*NONE'       $SEP              8
030600930322     C                   MOVEL     $CVT8         $CVT2             6
030700930322     C                   ENDSR
030800921102?     *
030900921102?     *------------------------------------------------------------
031000921102?     *  INIZIALIZZO I CAMPI DEL M001D2
031100921102?     *------------------------------------------------------------
031200921102?     *
031300921102?    C     D2INZ         BEGSR
031400921105     C*-
031500921118     C* ESEGUO PER : -
031600921105     C*              -  F12 DALLA VIDEATA DELL'OPZ. 11
031700921105     C*              -  F20
031800921105     C*
031900921105     C*
032000921105     C* PER F12 SPENGO *IN05
032100921105     C*
032200921105     C   KL              SETOFF                                       05
032300921105     C*----
032400921105     C* PER F20 INVERTO IL TIPO DI VISUALIZZ.  ARRIVO/PARTENZA
032500921105     C*
032600921113     C   KUD2TIP         IFEQ      'A'
032700921102     C                   MOVE      'P'           D2TIP
032800921103     C                   ELSE
032900921102     C                   MOVE      'A'           D2TIP
033000921102     C                   END
033100921104     C*
033200921104     C*----
033300921105     C*
033400921105     C*
033500921105     C* PER F12 DOPO OPZ 11 REIMPOSTO I PARAMETRI PRECEDENTI
033600921105     C*
0337009211053    C   KL              DO
033800921105     C                   MOVE      SAVTIP        D2TIP
033900921105     C                   MOVE      SAVUTR        D2UTR
034000921105     C                   MOVE      SAVFIR        D2FIR
034100921123     C                   MOVE      SAVFIL        D2FIL
034200921105     C                   MOVE      SAVUTL        D2UTL
034300921105     C                   MOVE      SAVDAT        D2DAT
034400921105     C                   MOVE      SAVDOC        D2DOC
034500921105     C                   MOVE      SAVRIS        D2RIS
034600921103     C*----
0347009211051-   C                   END
034800921102     C*
034900921113     C     D2TIP         IFEQ      'A'
035000921113     C                   MOVE      *BLANK        D2TIPD
035100921113     C                   MOVEL     TXT1          D2TIPD
035200921113     C                   SETOFF                                       04
035300921113     C                   ELSE
035400921113     C                   MOVE      *BLANK        D2TIPD
035500921113     C                   MOVEL     TXT2          D2TIPD
035600921113     C                   SETON                                        04
035700921113     C                   END
035800921102     C                   ENDSR
035900921102?     *
036000921102?     *------------------------------------------------------------
036100921102?     *  CARICO SUBFILE M001S1
036200921102?     *------------------------------------------------------------
036300921102?     *
036400921102?    C     S1CAR         BEGSR
036500921102     C*
036600921102?    C*
036700921102?    C*PULISCO SUBFILE
036800921102?    C*
036900921102     C                   SETON                                            20
037000921102     C                   SETOFF                                           21
037100921102     C                   WRITE     M001C1
037200921102     C                   SETOFF                                         2022
037300921102     C*
037400921118?    C*
037500921118     C                   Z-ADD     D2DAT         XDTGMA
037600921118     C                   Z-ADD     3             XDTSTA
037700921118?    C                   EXSR      XDATA
037800921118     C                   Z-ADD     XDTAMG        DATAMG            7 0
037900921118?    C*
038000921118     C                   Z-ADD     0             DA2AMG
038100921118     C     D2DA2         IFNE      0
038200921118     C                   Z-ADD     D2DA2         XDTGMA
038300921118     C                   Z-ADD     3             XDTSTA
038400921118?    C                   EXSR      XDATA
038500921118     C                   Z-ADD     XDTAMG        DA2AMG            7 0
038600921118     C                   END
038700921118     C*
038800921102     C                   MOVE      'S'           S1STAI
038900921102     C                   Z-ADD     0             NRR1              5 0
039000921102     C                   Z-ADD     0             NUMSF1            5 0
039100921102?    C*
039200921102?    C*ESEGUO IL CARICAMENTO
039300921102?    C*
039400921102     C                   MOVE      D2TIP         KTIP
039500921118     C                   Z-ADD     DATAMG        KDTA
039600921105     C     KTSM01        SETLL     CMTSM01L
039700921113     C     KTIP          READE     CMTSM01L                               30
039800921102?    C*
039900921102?    C                   EXSR      CARICA
040000921102     C*
040100921102     C*
040200921102?    C*
040300921102?    C*MEMORIZZA IL NUMERO DI RECORDS SCRITTI
040400921102?    C*
040500921102     C                   Z-ADD     NRR1          NUMSF1
040600921103     C     NRR1          COMP      0                                  21
040700921102     C*
040800921102     C                   WRITE     M001D1
040900921102     C*
041000921102     C                   ENDSR
041100921102?     *
041200921102?     *----------------------------------------------------
041300921102?     * C A R I C A - CARICAMENTO SFL
041400921102?     *----------------------------------------------------
041500921102?     *
041600921102?    C     CARICA        BEGSR
041700921106     C*
041800921106     C* Gestione nome parzialiale per '*'  a video.
041900921103     C*
042000921106?    C                   EXSR      GESPVD
042100921105     C*
042200921105     C*----------
042300921105     C*
042400921102     C                   Z-ADD     NUMSF1        NRR1
042500921105     C                   MOVE      *ZERO         X                 2 0
042600921105     C  N30              SETOFF                                       06
042700921105     C*
0428009211181    C     X             DOWLT     12
042900921105     C     *IN30         ANDEQ     '0'
043000921118     C     TSMDTR        ANDGE     DA2AMG
043100921102     C                   SETOFF                                           90
043200921102     C*---------
043300921106     C* Gestione nome parzialiale per '*'  sul file CMTSM.
043400921105     C*
043500921106?    C                   EXSR      GESPDB
043600921106     C*
043700921106     C* Gestione controlli di validità
043800921106     C*
043900921106?    C                   EXSR      GESCHK
044000921102     C*---------
0441009211052    C  N90              DO
044200921105     C*
044300921106     C* Salvo il primo rec. per posizionare il cursore sulla prima
044400921106     C* riga della pagina sucessiva.
044500921106     C*
044600921106     C  N06NRR1          ADD       1             WZ9               9 0
044700921105     C  N06              SETON                                        06
044800921106     C*
044900921106     C* Aggiornamento sfl M001S1.
045000921106     C*
045100921106?    C                   EXSR      AGGSFL
045200921102     C*
045300921102     C                   SETOFF                                           22
045400921105     C                   ADD       1             X
045500921105     C                   ADD       1             NRR1
045600921102     C                   Z-ADD     NRR1          S1NRR
045700921102     C                   WRITE     M001S1
045800921102     C*
0459009211052-   C                   END
046000921113     C     KTIP          READE     CMTSM01L                               30
046100000925     C* Disattivo tasto ROLLUP.
046200000925     C                   MOVEL     *IN30         *IN07
0463009211051-   C                   END
046400921105     C*
046500930322     C**N06                Z-ADD1         WZ9     90
046600921102     C*
046700921102     C                   ENDSR
046800921106?     *
046900921112?    C     GESPVD        BEGSR
047000921112     C*
047100921112     C                   MOVEA     *BLANK        SK
047200921112     C     D2UTR         IFNE      *BLANK
047300921112     C                   MOVEA     D2UTR         SK
047400921112     C                   Z-ADD     1             R                 3 0
047500921112     C     '*'           LOOKUP    SK(R)                                  35
047600921112     C   35              SUB       1             R                        36
047700921112     C  N35              Z-ADD     10            R
047800921112     C                   ELSE
047900921112     C                   Z-ADD     0             R
048000921112     C                   END
048100921112     C*
048200921112     C                   MOVEA     *BLANK        SK
048300921112     C     D2UTL         IFNE      *BLANK
048400921112     C                   MOVEA     D2UTL         SK
048500921112     C                   Z-ADD     1             L                 3 0
048600921112     C     '*'           LOOKUP    SK(L)                                  35
048700921112     C   35              SUB       1             L                        36
048800921112     C  N35              Z-ADD     10            L
048900921112     C                   ELSE
049000921112     C                   Z-ADD     0             L
049100921112     C                   END
049200921112     C*
049300921112     C                   ENDSR
049400921112?     *
049500921112?     *----------------------------------------------------
049600921112?     * G E S P D B - GESTIONE NOME PARZIALE  FDB
049700921112?     *----------------------------------------------------
049800921112?     *
049900921112?    C     GESPDB        BEGSR
050000921112     C*-----
050100921112     C                   MOVEA     *BLANK        SK
050200921112     C     TSMUTR        IFNE      *BLANK
050300921112     C                   MOVEA     TSMUTR        SK
050400921112     C                   Z-ADD     1             RD                3 0
050500921112     C     '*'           LOOKUP    SK(RD)                                 35
050600921112     C   35              SUB       1             RD                       36
050700921112     C  N35              Z-ADD     10            RD
050800921112     C                   ELSE
050900921112     C                   Z-ADD     10            RD
051000921112     C                   END
051100921112     C     R             IFGT      RD
051200921112     C                   Z-ADD     RD            R1                3 0
051300921112     C                   ELSE
051400921112     C                   Z-ADD     R             R1
051500921112     C                   END
051600921112     C     R1            IFNE      0
051700921112     C     R1            SUBST(P)  D2UTR:1       WK1UTR           10
051800921112     C     R1            SUBST(P)  TSMUTR:1      WK2UTR           10
051900921112     C                   ELSE
052000921112     C                   MOVE      *BLANK        WK1UTR
052100921112     C                   MOVE      *BLANK        WK2UTR
052200921112     C                   END
052300921112     C*
052400921112     C*
052500921112     C*-----
052600921112     C                   MOVEA     *BLANK        SK
052700921112     C     TSMUTL        IFNE      *BLANK
052800921112     C                   MOVEA     TSMUTL        SK
052900921112     C                   Z-ADD     1             LD                3 0
053000921112     C     '*'           LOOKUP    SK(LD)                                 35
053100921112     C   35              SUB       1             LD                       36
053200921112     C  N35              Z-ADD     10            LD
053300921112     C                   ELSE
053400921112     C                   Z-ADD     10            LD
053500921112     C                   END
053600921112     C     L             IFGT      LD
053700921112     C                   Z-ADD     LD            L1                3 0
053800921112     C                   ELSE
053900921112     C                   Z-ADD     L             L1
054000921112     C                   END
054100921112     C     L1            IFNE      0
054200921112     C     L1            SUBST(P)  D2UTL:1       WK1UTL           10
054300921112     C     L1            SUBST(P)  TSMUTL:1      WK2UTL           10
054400921112     C                   ELSE
054500921112     C                   MOVE      *BLANK        WK1UTL
054600921112     C                   MOVE      *BLANK        WK2UTL
054700921112     C                   END
054800921112     C*
054900921112     C*
055000921112     C                   ENDSR
055100921106?     *----------------------------------------------------
055200921106?     * G E S C H K - GESTIONE CONTROLLI DI VALIDITA'
055300921106?     *----------------------------------------------------
055400921106?     *
055500921106?    C     GESCHK        BEGSR
055600921106     C*
055700921106     C* Gestione nome parziale
055800921106     C*
055900921112     C     WK1UTR        IFNE      WK2UTR
056000921112     C     WK1UTL        ORNE      WK2UTL
056100921112     C                   SETON                                        90
0562009211121-   C                   END
056300921106     C*
056400921106     C* Gestione filiale
056500921106     C*
0566009211061    C  N90D2FIR         IFNE      *ZERO
056700921112     C     D2FIR         ANDNE     *BLANK
056800921112     C                   MOVE      D2FIR         WZ3               3 0
056900921112     C     TSMFIR        COMP      WZ3                                9090
0570009211061-   C                   END
0571009211231    C  N90D2FIL         IFNE      *ZERO
057200921123     C     D2FIL         ANDNE     *BLANK
057300921123     C                   MOVE      D2FIL         WZ3               3 0
057400921123     C     TSMFIL        COMP      WZ3                                9090
0575009211231-   C                   END
057600921106     C*
057700921106     C* Gestione doc. da caricare.
057800921106     C* Carica solo i documenti di risposta al doc. selezionato D2DOC.
057900921106     C*
0580009211061    C  N90D2DOC         IFNE      *ZERO
058100921106     C     TSMDOS        COMP      D2DOC                              9090
058200921106     C  N90TSMTIP        COMP      'A'                                9090
0583009211061-   C                   END
058400921106     C*
058500921218     C* Gestione doc. CHE PREVEDONO  risposta
058600921106     C*
0587009212181    C  N90D2RSP         IFEQ      'S'
058800921106     C     TSMRIS        COMP      'S'                                9090
0589009211061-   C                   END
059000921106     C*
059100921218     C*
059200921218     C* Gestione doc. CHE ATTENDONO  risposta
059300921218     C*
0594009212181    C  N90D2RIS         IFEQ      'S'
059500921218     C     TSMRIS        IFNE      'S'
059600921218     C     TSMSTS        OREQ      '5'
059700921218     C                   SETON                                        90
059800921218     C                   END
0599009212181-   C                   END
060000921113     C*
060100970410     C* Gestione solo nuovi + NON RISPOSTI QUELLI CHE LO PREVEDONO
060200921113     C*
0603009211131    C  N90D2NEW         IFEQ      'S'
0604009211131    C     D2TIP         ANDEQ     'A'
060500970410     C                   MOVE      *BLANK        §LBFL             1
060600970410     C     TSMSTS        IFEQ      '1'
060700970410     C     TSMRIS        OREQ      'S'
060800970410     C     TSMSTS        ANDNE     '5'
060900970410     C                   MOVE      'Y'           §LBFL
0610009704101-   C                   END
061100970410     C     §LBFL         COMP      'Y'                                9090
061200970410     C****       TSMSTS    COMP '1'                  9090
0613009211131-   C                   END
061400921113     C*
061500921106     C                   ENDSR
061600921106?     *
061700921106?     *----------------------------------------------------
061800921106?     * A G G S F L - AGGIORNAMENTO SFL M001S1
061900921106?     *----------------------------------------------------
062000921106?     *
062100921106?    C     AGGSFL        BEGSR
062200921106     C*
062300921106     C* Per documenti INVIATI (STS=3) controllo se è stato
062400921106     C* consegnato nel caso aggiorno.
062500921106     C*
062600921113     C     TSMSTS        IFEQ      '3'
062700921106     C                   MOVE      'I'           KOPE
062800921106     C                   MOVE      TSMTRA        KTRA
062900921118     C     KLOG01        CHAIN     CMLOG01L                           31
063000921118     C     *IN31         DOWEQ     *OFF
063100921118     C     LOGTXT        ANDNE     *BLANK
063200921118     C     KLOG01        READE     CMLOG01L                               31
063300921118     C                   END
0634009211061    C  N31              DO
063500921118C    C     TSMDOL        CHAIN     CMTSM04L                           31
063600921106     C                   MOVE      '4'           TSMSTS
063700921118     C                   Z-ADD     LOGDAT        TSMDTI
063800921118     C                   Z-ADD     LOGORA        TSMORI
063900921118     C                   UPDATE    TSM0004
0640009211061-   C                   END
0641009211131-   C                   END
064200921106     C*
064300921106     C*
064400921106     C                   MOVE      *ZERO         S1OPZ
064500921106     C                   MOVEL     TSMTXT        S1TXT
064600921106     C                   MOVE      TSMSTS        S1STS
064700921113     C                   MOVE      S1STS         I                 3 0
064800921113     C                   MOVE      STS(I)        S1STSD
064900921113     C                   MOVE      TSMDOR        S1DOR
065000921113     C     S1STS         COMP      '1'                                    03
065100921106     C                   Z-ADD     TSMDTR        S1DAT
065200921106     C                   Z-ADD     TSMDTR        AMG
065300921106     C                   MOVE      AA            S1DAT
065400921106     C                   MOVEL     GG            S1DAT
065500921112     C*
065600921112     C                   Z-ADD     TSMORI        S1ORI
065700921112     C                   Z-ADD     TSMDTI        S1DTI
065800921112     C                   Z-ADD     TSMDTI        AMG
065900921112     C                   MOVE      AA            S1DTI
066000921112     C                   MOVEL     GG            S1DTI
066100921112     C*
066200921106     C                   Z-ADD     TSMFIR        S1FIL
066300921111     C                   MOVE      *BLANK        S1FILD
066400930401     C                   MOVEL     TSMUTL        S1FILD
066500941024     C                   MOVEL     TSMUTL        S1UTNH
066600930401     C*                    MOVE S1FIL     £FIL
066700930401?    C*                    EXSR CHKFIL
066800930401     C*          £STS      COMP 'ERR'                    35
066900930401     C* N35                MOVEL£DES      S1FILD
067000930401     C*  35                MOVEL*ALL'?'   S1FILD
067100921112     C*
067200921112     C                   MOVE      TSMSTP        S1STP
067300921106     C                   Z-ADD     TSMDTS        H1DTS
067400921112     C                   MOVE      TSMRIS        S1RIS
067500921112     C                   Z-ADD     TSMDTP        H1DTP
067600921106     C                   Z-ADD     TSMDOC        H1DOC
067700921110     C                   Z-ADD     TSMDOR        H1DOR
067800921113     C                   MOVE      TSMURR        H1URG
067900921106     C                   MOVE      TSMMSR        H1MSR
068000921106     C                   Z-ADD     TSMDOL        S1DOL
068100921110     C                   Z-ADD     TSMDOR        H1DOR
068200921106     C                   MOVEL     TSMUTR        S1UTN
068300921106     C*
068400921106     C                   ENDSR
068500921102?     *
068600921102?     *----------------------------------------------------
068700921102?     * S 1 C T L   - CONTROLLI SUL SFL M001S1
068800921102?     *----------------------------------------------------
068900921102?     *
069000921102?    C     S1CTL         BEGSR
069100921102     C*
069200921102     C*
069300921102     C                   READC     M001S1                                 32
069400921102?    C*
069500921102?    C* SPENGO GLI IND. D'ERRORE DEL SUBFILE
069600921102?    C*
069700921102     C                   SETOFF                                       010299
0698009211021    C     *IN32         DOWEQ     '0'
069900921112     C                   SETOFF                                       707172
070000921102?    C*
070100921102     C                   SETON                                        22
070200921102?    C*
070300921102?    C*  CONTROLLI
070400921102?    C*
0705009211032    C     S1OPZ         IFNE      *ZERO
070600921102?    C****
070700921102?    C* OPZ=2 MODIFICA
070800921102?    C****
0709009211033    C     S1OPZ         IFEQ      2
071000921118     C                   MOVE      ' '           M02BLO
071100921118     c* permetto la modifica solo per doc in partenza non spediti
071200921118     c*
071300921118     C     S1STS         IFNE      '9'
071400921118     C                   MOVE      'S'           M02BLO
071500921118     C                   END
071600921102     C                   CALL      'CMZM02R'
071700921106     C                   PARM      S1DOL         M02DCR            7 0
071800921106     C                   PARM      S1DOL         M02DCA            7 0
071900921102     C                   PARM      *ZERO         M02FIL            3 0
072000921102     C                   PARM      *BLANK        M02UTN           10
072100921102     C                   PARM      *BLANK        M02URG            1
072200921113     C                   PARM      'N'           M02MSG            1
072300921110     C                   PARM      *ZERO         M02DOS            7 0
072400921112     C                   PARM      'N'           M02FLG            1
072500921118     C                   PARM                    M02BLO
072600000929     C                   MOVE      'N'           S1STAI
072700921111     C*
072800921111     C* Porto lo stato del doc. da 1(Nuovo) a 2(Aperto) per posta
072900921112     C* in arrivo valorizz. anche la data lettura con udate.
073000921111     C*
0731009211114    C  N04S1STS         IFEQ      '1'
073200921112     C     S1DOL         CHAIN     CMTSM04L                           36
073300921112     C*
073400921111     C                   MOVE      '2'           TSMSTS
0735009211125    C     TSMDTL        IFEQ      *ZERO
073600930322C    C                   EXSR      GETDAT
073700930322     C                   MOVE      $CVT2         TSMDTL
0738009211125-   C                   END
073900921112     C*
074000921112     C                   UPDATE    TSM0004
0741009211124-   C                   END
074200921111     C*
074300980122     c* SE posta in arrivo e richiesta stampa stampo
074400980122     c*    SE MAI STAMPATA
074500930322     C*
074600930322     C     S1STP         IFEQ      'S'
074700980122     C     H1DTS         ANDEQ     *ZERO
074800980122     C*
074900930406     C                   MOVE      *BLANK        M03OUT
075000930322     C                   CALL      'CMZM03C'
075100930322     C                   PARM                    S1DOL
075200930322     C                   PARM                    M03OUT           10
075300930322     C                   END
0754009211023-   C                   END
075500921218?    C****
075600921218?    C* OPZ=5 VISUALIZZA
075700921218?    C****
0758009212183    C     S1OPZ         IFEQ      5
075900921218     c*
076000921218     c*
076100921218     C                   MOVE      'S'           M02BLO
076200921218     C                   CALL      'CMZM02R'
076300921218     C                   PARM      S1DOL         M02DCR            7 0
076400921218     C                   PARM      S1DOL         M02DCA            7 0
076500921218     C                   PARM      *ZERO         M02FIL            3 0
076600921218     C                   PARM      *BLANK        M02UTN           10
076700921218     C                   PARM      *BLANK        M02URG            1
076800921218     C                   PARM      'N'           M02MSG            1
076900921218     C                   PARM      *ZERO         M02DOS            7 0
077000921218     C                   PARM      'N'           M02FLG            1
077100921218     C                   PARM                    M02BLO
077200000925     C**                   MOVE 'N'       S1STAI
077300921218     C*
077400921218     C*
0775009212183-   C                   END
077600921102?    C****
077700921102?    C* OPZ=3 COPIA
077800921102?    C****
0779009211033    C     S1OPZ         IFEQ      3
078000921103     C                   CALL      'CMZM02R'
078100921103     C                   PARM      *ZERO         M02DCR
078200921106     C                   PARM      S1DOL         M02DCA
078300921103     C                   PARM      *ZERO         M02FIL
078400921103     C                   PARM      *BLANK        M02UTN
078500921103     C                   PARM      *BLANK        M02URG
078600921113     C                   PARM      'N'           M02MSG
078700921110     C                   PARM      *ZERO         M02DOS
078800921112     C                   PARM      'N'           M02FLG
078900921118     C                   PARM      ' '           M02BLO
079000000925     C**                   MOVE 'N'       S1STAI
0791009211023-   C                   END
079200921102?    C****
079300921102?    C* OPZ=4 ANNULLA
079400921102?    C****
0795009211033    C     S1OPZ         IFEQ      4
079600921118     C     *INKQ         ANDEQ     *ON
079700921102?    C                   EXSR      GESANN
079800000929     C  N71
079900000929     CANN72              MOVE      'N'           S1STAI
0800009211023-   C                   END
080100921102?    C****
080200921102?    C* OPZ=6 STAMPA
080300921102?    C****
0804009211033    C     S1OPZ         IFEQ      6
080500930406     C*****
080600940624     C***        C1VIDE    CHAINKFTOD10L             88
080700930406     C                   MOVE      *BLANK        M03OUT
080800940624     C**N88                MOVELKKQOT     M03OUT
080900930406     C*****
081000921106     C                   CALL      'CMZM03C'
081100921106     C                   PARM      S1DOL         M03DOL            7 0
081200930301     C                   PARM                    M03OUT           10
081300000925     C**                   MOVE 'N'       S1STAI
081400921112     C*
081500921112     C* Aggiorno la data di stampa.
081600921112     C*
0817009211124    C     H1DTS         IFEQ      *ZERO
081800921112     C     S1DOL         CHAIN     CMTSM04L                           36
081900921112     C*
082000930322     C                   EXSR      GETDAT
082100930322     C                   MOVE      $CVT2         TSMDTS
082200921112     C*
082300921112     C                   UPDATE    TSM0004
0824009211124-   C                   END
0825009211123-   C                   END
082600921102?    C****
082700921102?    C* OPZ=10 INVIO
082800921102?    C****
0829009211033    C     S1OPZ         IFEQ      10
083000921102     C                   CALL      'CMZM04R'
083100921106     C                   PARM      S1DOL         M04DOL            7 0
083200921102     C                   PARM      'I'           M04FLG            1
083300921103     C                   PARM      *ZERO         M04FIR            3 0
083400921102     C                   PARM      *BLANK        M04UTN           10
083500921102     C                   PARM      *BLANK        M04URG            1
083600921102     C                   PARM      *BLANK        M04MSG            1
083700921113     C                   PARM      'N'           M04SPE            1
083800000925     C**                   MOVE 'N'       S1STAI
0839009211023-   C                   END
084000921102?    C****
084100921102?    C* OPZ=11 RISPOSTA
084200921102?    C****
0843009211033    C     S1OPZ         IFEQ      11
084400921103     C*
084500921103     C* Per posta in arrivo invio una risposta
084600921103     C*
0847009211034    C     KTIP          IFEQ      'A'
084800921103     C                   CALL      'CMZM02R'
084900921103     C                   PARM      *ZERO         M02DCR
085000921118     C                   PARM      S1DOL         M02DCA
085100921103     C                   PARM      S1FIL         M02FIL
085200941024     C                   PARM      S1UTN         M02UTN
085300921103     C                   PARM      H1URG         M02URG
085400921103     C                   PARM      H1MSR         M02MSG
085500921110     C                   PARM      H1DOR         M02DOS
085600921112     C                   PARM      'N'           M02FLG
085700921118     C                   PARM      ' '           M02BLO            1
085800921112     C*
085900921112     C* Aggiorno la data di risposta.
086000921112     C*
0861009211125    C     M02FLG        IFEQ      'S'
086200921112     C     S1DOL         CHAIN     CMTSM04L                           36
0863009705226    C****       TSMDTP    IFEQ *ZERO
0864009705226    C     TSMSTS        IFNE      '5'
086500921112     C*
086600930322     C                   EXSR      GETDAT
086700930322     C                   MOVE      $CVT2         TSMDTP
086800921112     C                   MOVE      '5'           TSMSTS
086900921112     C*
087000921112     C                   UPDATE    TSM0004
0871009211126-   C                   END
0872009211125-   C                   END
087300921103     C*
087400921103     C* Per posta in partenza visualizzo le risposte ricevute
087500921103     C*
0876009211034E   C                   ELSE
087700921103     C*
087800921103     C* Salvo i parametri
087900921103     C                   MOVE      D2TIP         SAVTIP            1
088000921103     C                   MOVE      D2UTR         SAVUTR           10
088100921112     C                   MOVE      D2FIR         SAVFIR            3
088200921123     C                   MOVE      D2FIL         SAVFIL            3
088300921103     C                   MOVE      D2UTL         SAVUTL           10
088400921103     C                   MOVE      D2DAT         SAVDAT            6 0
088500921118     C                   MOVE      D2DA2         SAVDA2            6 0
088600921103     C                   MOVE      D2DOC         SAVDOC            7 0
088700921103     C                   MOVE      D2RIS         SAVRIS            1
088800921103     C*
088900921103     C* Imposto i nuovi
089000921118     C                   MOVE      'A'           D2TIP
089100921118     C                   MOVE      *BLANK        D2UTR
089200921118     C                   MOVE      *BLANK        D2FIR
089300921123     C                   MOVE      *BLANK        D2FIL
089400921118     C                   MOVEL     *BLANK        D2UTL
089500921118     C                   MOVE      UDATE         D2DAT
089600921118     C                   Z-ADD     0             D2DA2
089700921118     C                   MOVE      'N'           D2RIS
089800921118     C                   MOVE      'N'           D2NEW
089900921118     C                   MOVE      S1DOL         D2DOC
090000921103     C                   SETON                                        05
0901009211034-   C                   END
090200921103     C*
090300000925     C                   MOVE      'N'           S1STAI
0904009211023-   C                   END
090500921102?    C****
090600921102?    C* OPZ=15 SOLLECITO
090700921102?    C****
090800921103     C*
090900921103     C* OPZ. SOLLECITI (15) NON ABILITATA SE SONO IN RISPOSTA (*IN05='1')
091000921103     C*
0911009211033    C     S1OPZ         IFEQ      15
091200921103     C     *IN05         ANDEQ     '1'
091300921103     C                   SETON                                        70
0914009211033-   C                   END
091500921103     C*
0916009211033    C  N70S1OPZ         IFEQ      15
091700921113     C                   MOVE      S1FIL         M04FIR
091800921102     C                   CALL      'CMZM04R'
091900921106     C                   PARM      S1DOL         M04DOL
092000921102     C                   PARM      'S'           M04FLG
092100921112     C                   PARM                    M04FIR
092200941024     C                   PARM      S1UTN         M04UTN
092300921102     C                   PARM      H1URG         M04URG
092400921102     C                   PARM      'S'           M04MSG
092500921113     C                   PARM      'N'           M04SPE            1
092600921102     C                   MOVE      'N'           S1STAI
0927009211023-   C                   END
092800921102     C*---
092900921102     C   01
093000921102     CAN 02              SETOFF                                         01
093100921102     C   70
093200921102     CANN02
093300921102     COR 71
093400921102     CANN02
093500921102     COR 72
093600921102     CANN02              SETON                                        010299
093700921102     C   01
093800921102     CAN 02              Z-ADD     NRR1          C1SRN
093900921102     C*
0940009211022E   C                   ELSE
094100941024     C* reindirizzamento posta ricevuta
094200941024     C*
094300941024     C     S1FILD        IFNE      S1UTNH
094400941024     C     S1DOL         CHAIN     CMTSM04L                           36
094500941024     C*
094600941024     C                   MOVE      S1FILD        TSMUTL
094700941024     C                   MOVE      '1'           TSMSTS
094800941024     C*
094900941024     C                   UPDATE    TSM0004
095000941024     C                   ENDIF
095100921102     C                   SETOFF                                       22
0952009211022-   C                   END
095300921102     C*
095400000925     C                   CLEAR                   S1OPZ
095500921102     C                   UPDATE    M001S1
095600921102     C                   READC     M001S1                                 32
0957009211021-   C                   END
095800921102     C*
095900921102     C                   ENDSR
096000921102?     *
096100921102?     *------------------------------------------------------
096200921102?     * G E S A N N - GESTIONE OPZIONE DI ANNULLAMENTO
096300921102?     *------------------------------------------------------
096400921102?     *
096500921102?    C     GESANN        BEGSR
096600921105     C*----
096700921102     C* SE POSTA IN ARRIVO
0968009211021    C     *IN04         IFEQ      '0'
096900921105     C*
097000921102     C* Stampare il doc. prima di cancellarlo
097100921102     C*
0972009211122    C     S1STP         IFEQ      'S'
097300921102     C     H1DTS         ANDEQ     *ZERO
097400921102     C                   SETON                                        71
0975009211022-   C                   END
097600921102     C*
097700921102     C* Rispondere prima di cancellarlo
097800921102     C*
0979009211122    C     S1RIS         IFEQ      'S'
098000921112     C     H1DTP         ANDEQ     *ZERO
098100921102     C                   SETON                                        72
0982009211022-   C                   END
098300921105     C*
0984009211021-   C                   END
098500921105     C*----
098600921105     C*
098700921105     C  N71
0988009211051    CANN72              DO
098900921102     C*
099000921102     C* Cancello record di testata posta
099100921102     C*
099200921112     C     S1DOL         CHAIN     CMTSM04L                           33
099300921106     C     S1DOL         DELETE    CMTSM04L                           33
099400921102     C*
099500921102     C* Controllo se ci sono atri documenti con lo stesso cod. riga
099600921102     C* in caso negativo cancello le righe del doc.
099700921102     C*
099800921102     C     H1DOC         CHAIN     CMTSM03L                           33
099900921112     C   33H1DOC         CHAIN     CMRGM01L                           34
1000009211052    C   33              DO        *HIVAL
100100921102     C     H1DOC         DELETE    CMRGM01L                           34
1002009211022-   C  N34              END
100300921102     C*
1004009211051-   C                   END
100500921105     C*
100600921102     C*
100700921102     C                   ENDSR
100800921103?     *
100900921103?     *------------------------------------------------------
101000921103?     * G E S D 2   - GESTIONE VIDEATE PARAMETRIZZ.
101100921103?     *------------------------------------------------------
101200921103?     *
101300921103?    C     GESD2         BEGSR
101400921103     C*
101500921103     C* Salvo i parametri
101600921105     C                   MOVE      D2TIP         SV2TIP            1
101700921105     C                   MOVE      D2UTR         SV2UTR           10
101800921112     C                   MOVE      D2FIR         SV2FIR            3
101900921123     C                   MOVE      D2FIL         SV2FIL            3
102000921105     C                   MOVE      D2UTL         SV2UTL           10
102100921105     C                   MOVE      D2DAT         SV2DAT            6 0
102200921118     C                   MOVE      D2DA2         SV2DA2            6 0
102300921105     C                   MOVE      D2DOC         SV2DOC            7 0
102400921105     C                   MOVE      D2RIS         SV2RIS            1
102500930121     C*                    WRITEM001W1
102600921103     C                   MOVE      'S'           D2STAI
102700921103     C*
1028009211031    C     D2STAI        DOWEQ     'S'
102900921103     C*
103000921103     C                   EXFMT     M001D2
103100921103?    C***
103200921103?    C* F12= RITORNO
103300921103?    C***
1034009211032    C   KL              DO
103500921103     C*
103600921103     C* In caso di uscita con errori ripristino i parametri precedenti
103700921103     C*
103800921104     C                   MOVE      'N'           D2STAI
1039009211133    C*          *IN99     IFEQ '1'
104000921105     C                   MOVE      SV2TIP        D2TIP
104100921105     C                   MOVE      SV2UTR        D2UTR
104200921105     C                   MOVE      SV2FIR        D2FIR
104300921123     C                   MOVE      SV2FIL        D2FIL
104400921105     C                   MOVE      SV2UTL        D2UTL
104500921105     C                   MOVE      SV2DAT        D2DAT
104600921118     C                   MOVE      SV2DA2        D2DA2
104700921105     C                   MOVE      SV2DOC        D2DOC
104800921105     C                   MOVE      SV2RIS        D2RIS
1049009211133E   C*                    ELSE
1050009211133-   C*                    END
1051009211032-   C                   END
105200921103     C*
105300921103?    C  NKL              EXSR      D2CTL
105400921113C    C  N99              MOVE      'N'           D2STAI
105500921103     C*
1056009211031-   C                   END
105700921113     C     IN04          COMP      '1'                                    04
105800921113?    C                   EXSR      S1CAR
105900921104     C                   MOVE      '0'           *INKL
106000921105     C                   MOVE      '1'           *INKR
106100921103     C*
106200921103     C                   ENDSR
106300921103?     *
106400921103?     *----------------------------------------------------
106500921103?     * D 2 C T L   - CONTROLLI SUL FMT M001D2
106600921103?     *----------------------------------------------------
106700921103?     *
106800921103?    C     D2CTL         BEGSR
106900921103     C*
107000921103     C                   SETOFF                                       99
107100921103?    C*
107200921103?    C*                **  TIPO RECORD     **
107300921103?    C*
107400921103     C                   MOVE      *BLANK        D2TIPD
1075009211121    C     D2TIP         IFEQ      'A'
107600921104     C                   MOVE      '0'           IN04              1
107700921103     C                   MOVEL     TXT1          D2TIPD
1078009211121E   C                   ELSE
107900921104     C                   MOVE      '1'           IN04
108000921103     C                   MOVEL     TXT2          D2TIPD
1081009211121-   C                   END
108200921103?    C*
108300921103?    C*                **  FILIALE REMOTA  **
108400921103?    C*
1085009211111    C     D2FIR         IFNE      *ZERO
108600921112     C     D2FIR         ANDNE     *BLANK
108700921112     C                   MOVE      D2FIR         £FIL
108800921112?    C                   EXSR      CHKFIL
108900921112     C     £STS          COMP      'ERR'                                  73
109000921112     C                   MOVE      £FIL          D2FIR
1091009211111-   C                   END
109200921123?    C*
109300021004     C* controllo filiale  LOCALE
109400021004     C*     deve essere valida
109500021004     C*
109600021019     C*     se l'utente non è POC REA RED la filiale può essere
109700021004     C*     solo quella dell'utente
109800021019     c*     se sei POC REA RED una dell'area (succevvisamente Capo Area)
109900021004     c*     se sei edp vedi tutto
110000021019     c*
110100021019     c* se senza AZUTE testo ancora il profilo
110200021019     c*
110300021004     c                   movel     DUTPOU        WWpoUA            3
1104009211231    C     D2FIL         IFNE      *ZERO
110500921123     C     D2FIL         ANDNE     *BLANK
110600921123     C                   MOVE      D2FIL         £FIL
110700921123?    C                   EXSR      CHKFIL
110800921123     C     £STS          COMP      'ERR'                                  78
110900921123     C                   MOVE      £FIL          D2FIL
1110000210041-   C                   ENDIF
111100021004     c  n78              do
111200021004     c* EDP
111300021021     c***                if        UTEFUN  ='     ' AND C1NPRE <> 'EDP' OR
111400021021     c**                           UTEFUN  <>'EDPSE' AND UTEFUN <> '     '
111500021021     c* per adesso sugli altri AS vedono tutto
111600021021     c                   if        UTEFUN  ='     ' AND C1NPRE <> 'OPR' and
111700021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'SEG' and
111800021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'EDP' and
111900021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'ARE' and
112000021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIS' and
112100021023     c                             UTEFUN  ='     ' AND C1NPRE <> 'ASC' and
112200021025     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIR' and
112300030213     c                             UTEFUN  ='     ' AND C1NPRE <> 'ISP' and
112400040302     c                             UTEFUN  ='     ' AND C1NPRE <> 'INT' and
112500021021     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIV' or
112600021021     c                             UTEFUN  <>'EDPSE' AND UTEFUN <> '     '
112700021004     c* ALTRI
112800021019     c*
112900021019     c                   if        UTEFUN  ='     ' AND C1NPRE <> 'OPR' and
113000021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'SEG' and
113100021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'ARE' and
113200021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIS' and
113300030213     c                             UTEFUN  ='     ' AND C1NPRE <> 'ISP' and
113400040302     c                             UTEFUN  ='     ' AND C1NPRE <> 'INT' and
113500021023     c                             UTEFUN  ='     ' AND C1NPRE <> 'ASC' and
113600021019     c                             UTEFUN  ='     ' AND C1NPRE <> 'DIV' or
113700021019     c                             UTEFUN  <>'     ' and
113800021019     c                             UTEFUN  <>'POC  ' AND
113900021023     c                             UTEFUN  <>'REA  ' AND
114000030213     c                             UTEFUN  <>'ISP  ' AND
114100040302     c                             UTEFUN  <>'INT  ' AND
114200021023     c                             UTEFUN  <>'RED  ' AND
114300021023     c                             UTEFUN  <>'RAC  '
114400021004     c     wwpoua        ifne      D2fil
114500021004     c                   seton                                        78
114600021004     c                   endif
114700021004     c                   else
114800021019     c* POC e abilitati
114900021004     c                   reset                   $POG
115000021004     c                   clear                   TRUL31DS
115100110216     c                   movel     'RA'          I31ABI
115200021004     c                   movel     DUTPOU        I31CPO
115300021004     c                   call      'TRUL31R'
115400021004     c                   parm                    KPJBA
115500021004     c                   parm                    TRUL31ds
115600021004if  3c                   if        O31pog > *zeros
115700021004     c                   movea     O31pog        $POG
115800021004e   3c                   endif
115900021005     c     D2FIL         LOoKUP    $pog                                   58
116000021004     c  n58              seton                                        78
116100021005     c     D2FIL         ifle      *zero
116200021005     c                   seton                                        78
116300021005     c                   endif
116400021004     c*
116500021004     C                   endif
116600021004     C                   endif
116700021004     c                   enddo
116800921103?    C*
116900921118?    C*                **  DATA    1       **
117000921103?    C*
117100921103     C                   Z-ADD     D2DAT         XDTGMA
117200921103     C                   Z-ADD     3             XDTSTA
117300921103?    C                   EXSR      XDATA
117400921103     C     XDTSTA        COMP      0                                    74
117500921118     C                   Z-ADD     XDTAMG        DATAMG            7 0
117600921118?    C*                **  DATA    2       **
117700921118?    C*
117800921118     C                   Z-ADD     0             DA2AMG
117900921118     C     D2DA2         IFNE      0
118000921118     C                   Z-ADD     D2DA2         XDTGMA
118100921118     C                   Z-ADD     3             XDTSTA
118200921118?    C                   EXSR      XDATA
118300921118     C     XDTSTA        COMP      0                                    77
118400921118     C  N77              Z-ADD     XDTAMG        DA2AMG            7 0
118500921118     C                   END
118600921103     C*
118700921103     C   73
118800921118     COR 74
118900021004     COR 78
119000921118     COR 77              SETON                                        99
119100921103     C*
119200921103     C                   ENDSR
119300921112?     *
119400921112?     *----------------------------------------------------
119500921112?     * C H K F I L - CONTROLLI FILIALE
119600921112?     *----------------------------------------------------
119700921112?     *
119800921112?    C     CHKFIL        BEGSR
119900921112     C*
120000921112     C                   MOVE      *BLANK        £DES             25
120100921112     C                   MOVE      *BLANK        £STS              3
120200921112     C                   SETOFF                                           91
120300921112     C*
1204009211121    C     £FIL          IFNE      *BLANKS
120500921112     C     £FIL          ANDNE     *ZEROS
120600921112     C*
120700921112     C     '?'           SCAN      £FIL                                   37
1208009211122    C     *IN37         IFEQ      '1'
120900921112     C                   MOVE      *BLANK        £FIL              3
121000941125     C                   CALL      'TNSD19R'
121100921112     C                   PARM      *BLANK        §COD1             3
121200921112     C                   PARM      *BLANK        §TIP              1
121300921112     C                   PARM      *BLANK        §DES1            25
121400921112     C                   MOVEL     §COD1         £FIL
121500921112     C                   MOVEL     §DES1         £DES
1216009211122E   C                   ELSE
121700921112     C*
1218009211123    C     £FIL          IFGT      *ZEROS
121900921112     C                   MOVEL     £FIL          ORGFIL
122000921112     C     ORGFIL        CHAIN     AZORG01L                           91
1221009211123E   C                   ELSE
122200921112     C                   SETON                                        91
1223009211123-   C                   END
122400921112     C*
1225009211122-   C                   END
122600921112     C*
122700921112     C* CONTROLLO SE ANNULLATO
122800921112     C*
122900921112     C  N91ORGFVA        COMP      *BLANK                             9191
123000921112     C*
123100921112     C   91              MOVE      'ERR'         £STS
123200921112     C  N91              MOVEL     ORGDES        £DES
1233009211121-   C                   END
123400921112     C*
123500921112     C                   ENDSR
123600921103?     *
123700921103?     *----------------------------------------------------*
123800921103?     * XDATA    INVERSIONE E CONTROLLO DATE               *
123900921103?     *----------------------------------------------------*
124000921103?     *
124100921103     C*
124200921103?    C     XDATA         BEGSR
124300921103     C*
124400921103     C     XDTSTA        CABGT     4             XDTERR
124500921103     C     XDTSTA        CABLT     1             XDTERR
124600921103     C     XDTSTA        CABGT     2             XDTEI
124700921103     C*
124800921103     C* -- GIRA DATA INTERNA IN ESTERNA
124900921103     C*
125000921103     C     XDTAMG        DIV       100           XDTMM             2 0
125100921103     C     XDTAMG        DIV       10000         XDTAA             2 0
125200921103     C                   MOVE      XDTAMG        XDTGG             2 0
125300921103     C                   MOVE      XDTAMG        XDTGMA            6 0
125400921103     C                   MOVEL     XDTGG         XDTGMA
125500921103     C                   MOVE      XDTAA         XDTGMA
125600921103     C                   GOTO      XDTCON
125700921103     C*
125800921103     C     XDTEI         TAG
125900921103     C*
126000921103     C* -- GIRA DATA ESTERNA IN INTERNA
126100921103     C*
126200921103     C     XDTGMA        DIV       100           XDTMM
126300921103     C     XDTGMA        DIV       10000         XDTGG
126400921103     C                   MOVE      XDTGMA        XDTAA
126500921103     C                   MOVE      XDTGMA        XDTAMG            6 0
126600921103     C                   MOVEL     XDTAA         XDTAMG
126700921103     C                   MOVE      XDTGG         XDTAMG
126800921103     C                   SUB       2             XDTSTA            1 0
126900921103     C*                                                    * PER TEST CONT
127000921103     C     XDTCON        TAG
127100921103     C     XDTSTA        CABNE     1             XDTOK
127200921103     C*
127300921103     C* -- CONTROLLA LA VALIDITA* DELLA DATA
127400921103     C*
127500921103     C     XDTGG         CABLT     1             XDTERR
127600921103     C     XDTMM         CABLT     1             XDTERR
127700921103     C     XDTMM         CABGT     12            XDTERR
127800921103     C     XDTAA         DIV       4             XDTRES            2 0
127900921103     C                   MVR                     XDTRES
128000921103     C*
128100921103     C     XDTRES        IFEQ      0
128200921103     C                   Z-ADD     29            XDTF(2)
128300921103     C                   ELSE
128400921103     C                   Z-ADD     28            XDTF(2)
128500921103     C                   END
128600921103     C*
128700921103     C     XDTGG         CABGT     XDTF(XDTMM)   XDTERR
128800921103     C*                                                    * FINE MESE
128900921103     C     XDTOK         TAG
129000921103     C*                                                    * FINE MESE
129100921103     C* -- FINE NORMALE
129200921103     C*                                                    * FINE MESE
129300921103     C                   SUB       XDTSTA        XDTSTA
129400921103     C                   GOTO      XDTFIN
129500921103     C*                                                    * FINE MESE
129600921103     C     XDTERR        TAG
129700921103     C*                                                    * FINE MESE
129800921103     C* -- DATA O RICHIESTA ERRATA
129900921103     C*                                                    * FINE MESE
130000921103     C                   Z-SUB     1             XDTSTA
130100921103     C*                                                    * FINE MESE
130200921103     C     XDTFIN        TAG
130300921103     C*                                                    * FINE MESE
130400921103     C* -- FINE ISTRUZIONI ROUTINE
130500921103     C*
130600921103     C                   ENDSR
130700921102     C********************************************************************
130800921103  ************************************************************************
130900921113**  XDTF  X XDATA
131000921103312831303130313130313031
131100921113**
131200921113NUOVO
131300921113APERTO
131400921113INVIATO
131500921113CONSEGNATO
131600921113RISPOSTO
131700921113
131800921113
131900921113
132000921113NON INV.
