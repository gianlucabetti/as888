000100990118     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990118     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850304     F* ANG8A0R  *-AN/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500850304     F*       - INTERROGAZIONE LISTINI MATERIE PRIME -               *
000600000000     F*       ----------------------------------------               *
000700850304     F*                                                   AZN.: G8A0 *
000800990317     F*         FILE : tmlmp01L ANTAB00F                             *
000900990118     F*                tmamp01L tmamp03L tmamp04L tmamp05L           *
001000000000     F*                                                              *
001100000000     F*         PGM. : RIC001R   - RICERCA DITTA                     *
001200990317     F*                tii104R   - RICERCA ANAGRAFICA MATERIE PRIME  *
001300990317     F*                TII110R   - RICERCA CLASSI MERCEOLOGICHE      *
001400990317     F*                TII111R   - RICERCA SOTTOCLASSI MERCEOLOG.    *
001500990317     F*                TII118R   - RICERCA CATEGORIE FISCALI         *
001600990317     F*                TII116R   - RICERCA LISTINI MATERIE PRIME     *
001700000000     F*                                                              *
001800000000     F*--------------------------------------------------------------*
001900990118     Ftmlmp01L  IF   E           K DISK
002000000000     F*---------
002100990118     Ftmamp01L  IF   E           K DISK    USROPN
002200990118     F                                     RENAME(tmamp000:tmamp1)
002300000000     F*---------
002400990118     Ftmamp03L  IF   E           K DISK    USROPN
002500990118     F                                     RENAME(tmamp000:tmamp3)
002600000000     F*---------
002700990118     Ftmamp04L  IF   E           K DISK    USROPN
002800990118     F                                     RENAME(tmamp000:tmamp4)
002900000000     F*---------
003000990118     Ftmamp05L  IF   E           K DISK    USROPN
003100990118     F                                     RENAME(tmamp000:tmamp5)
003200000000     F*---------
003300000000     FANTAB00F  IF   E           K DISK
003400000000     F*---------
003500990317     Ftii005D   CF   E             WORKSTN
003600850304     F                                     SFILE(ANG8ASF1:REC1)
003700000000     D KPJBA         E DS
003800000000     D                 DS
003900000000     D*-----DS COSTRUZIONE ARTICOLO + VERSIONE
004000871009     D  AMPMAT                 1     12
004100871009     D  PRODOT                 1     12
004200000000     D DES75           DS
004300000000     D*-----DS DESCRIZIONI LISTINO
004400000000     D  DESLIS                 1     25
004500000000     D  DESPR1                26     35
004600000000     D  DESPR2                36     45
004700000000     D  DESPR3                46     55
004800000000     D  DESPR4                56     65
004900000000     D  DESPR5                66     75
005000990118     D soc001        e ds                  extname(xsoc001ds)
005100990118     D xsocds          DS          1000
005200000000     C*----------------------------------------------------*
005300000000     C     *ENTRY        PLIST                                                  *
005400000000     C                   PARM                    KPJBA                          *
005500990118     C*---------- RICERCA DITTA :
005600990118     C                   MOVEL     'SOC001'      TIPXSC
005700990118     C                   MOVEL     *blank        SOCXSC
005800990118     C                   EXSR      REPSOC
005900990118     C     RTNXSC        IFNE      '1'
006000990118     C                   MOVEL     XSOCDS        SOC001
006100990118     C                   MOVEL     xscrgs        desdit
006200990118     c                   end
006300871009     C                   MOVEL     '?'           INTERR           12
006400990118     C                   MOVEL     *blank        richia            1
006500000000     C*----------                                          *
006600000000     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
006700000000     C     KEYANT        KLIST                                                  *
006800000000     C                   KFLD                    GRUTAB            3            *
006900000000     C                   KFLD                    CODTAB            7            *
007000990118     C*---------- KEY PER RICERCA ARTICOLO SU tmlmp01L :   *
007100841023     C     KEYLIS        KLIST                                                  *
007200841023     C                   KFLD                    CODLIS                         *
007300841023     C                   KFLD                    AMPMAT                         *
007400000000     C*----------EMISSIONE TESTATA :                       *
007500000000     C*----------------------------------------------------*
007600850304     C                   WRITE     ANG8A01                                      *
007700850304     C     RICHIA        CABEQ     'R'           SOPRA                    10    *
007800850304     C                   MOVE      *BLANKS       CODLIS                         *
007900000000     C     INIZIO        TAG                                                    *
008000000000     C                   MOVE      *ALL'X'       CLAMER                         *
008100000000     C                   MOVE      *ALL'X'       SOTCLA                         *
008200000000     C                   MOVE      *ALL'X'       CATFIS                         *
008300000000     C                   MOVE      *ALL'X'       CODART                         *
008400000000     C     SOPRA         TAG                                                    *
008500000000     C*----------EMISSIONE ERRORI SU TESTATA :             *
008600000000     C   81                                                                     *
008700000000     COR 82                                                                     *
008800000000     COR 83                                                                     *
008900000000     COR 84                                                                     *
009000000000     COR 85              SETON                                        90        *
009100000000     C*----------EMISSIONE TESTATA :                       *
009200850304     C                   EXFMT     ANG8A02                                      *
009300990409     C   Kc              GOTO      FINE                                         *
009400000000     C*----------CONTROLLO DATI PROMPT :                   *
009500000000     C                   MOVEA     *ZEROS        *IN(80)                        *
009600000000     C*                                                    *
009700000000     C*----------RICERCA LISTINO :                         *
009800000000     C     CODLIS        IFEQ      '?  '                                        *
009900000000     C                   MOVE      *BLANKS       CODLIS                         *
010000000000     C                   MOVE      *BLANKS       DES75            75            *
010100990317     C                   CALL      'TII116R'                                    *
010200000000     C                   PARM                    CODLIS                         *
010300000000     C                   PARM                    DES75                          *
010400000000     C                   GOTO      SOPRA                                        *
010500000000     C                   END                                                    *
010600000000     C*----------CONTROLLO LISTINO :                       *
010700000000     C     CODLIS        CABEQ     *BLANKS       SOPRA                    81    *
010800000000     C*-----                                               *
010900000000     C                   MOVE      '161'         GRUTAB                         *
011000000000     C                   MOVE      *ZEROS        CODTAB                         *
011100000000     C                   MOVE      CODLIS        CODTAB                         *
011200000000     C     KEYANT        CHAIN     ANTAB00F                           81        *
011300000000     C     *IN81         CABEQ     '1'           SOPRA                          *
011400841024     C                   MOVEL     TABDES        DES75                          *
011500000000      /SPACE 2
011600000000     C*----------RICERCA CLASSE MERCEOLOGICA :             *
011700000000     C     CLAMER        IFNE      *ALL'X'                                      *
011800000000     C     CLAMER        IFEQ      '?  '                                        *
011900000000     C                   MOVE      *BLANKS       CLAMER                         *
012000000000     C                   MOVE      *BLANKS       DESCLA           25            *
012100990317     C                   CALL      'TII110R'                                    *
012200000000     C                   PARM                    CLAMER                         *
012300000000     C                   PARM                    DESCLA                         *
012400000000     C                   GOTO      SOPRA                                        *
012500000000     C                   ELSE                                                   *
012600000000     C*----------CONTROLLO CLASSE MERCEOLOGICA :           *
012700000000     C                   MOVE      '155'         GRUTAB                         *
012800000000     C                   MOVE      *ZEROS        CODTAB                         *
012900000000     C                   MOVE      CLAMER        CODTAB                         *
013000000000     C     KEYANT        CHAIN     ANTAB00F                           82        *
013100000000     C     *IN82         CABEQ     '1'           SOPRA                          *
013200000000     C                   END                                                    *
013300000000     C                   END                                                    *
013400000000      /SPACE 2
013500000000     C*----------RICERCA SOTTOCLASSE MERCEOLOGICA :        *
013600000000     C     SOTCLA        IFNE      *ALL'X'                                      *
013700000000     C     SOTCLA        IFEQ      '?    '                                      *
013800000000     C                   MOVE      *BLANKS       SOTCLA                         *
013900000000     C                   MOVE      *BLANKS       DESSOT           25            *
014000990317     C                   CALL      'TII111R'                                    *
014100000000     C                   PARM                    SOTCLA                         *
014200000000     C                   PARM                    DESSOT                         *
014300000000     C                   GOTO      SOPRA                                        *
014400000000     C                   ELSE                                                   *
014500000000     C*----------CONTROLLO SOTTOCLASSE MERCEOLOGICA :      *
014600000000     C                   MOVE      '156'         GRUTAB                         *
014700000000     C                   MOVE      *ZEROS        CODTAB                         *
014800000000     C                   MOVE      SOTCLA        CODTAB                         *
014900000000     C     KEYANT        CHAIN     ANTAB00F                           83        *
015000000000     C     *IN83         CABEQ     '1'           SOPRA                          *
015100000000     C                   END                                                    *
015200000000     C                   END                                                    *
015300000000     C*----------RICERCA CATEGORIA FISCALE :               *
015400841024     C     CATFIS        IFNE      *ALL'X'                                      *
015500000000     C     CATFIS        IFEQ      '? '                                         *
015600000000     C                   MOVE      *BLANKS       CATFIS                         *
015700000000     C                   MOVE      *BLANKS       DECAFI           25            *
015800990317     C                   CALL      'TII118R'                                    *
015900841024     C                   PARM                    CATFIS                         *
016000000000     C                   PARM                    DECAFI                         *
016100000000     C                   GOTO      SOPRA                                        *
016200000000     C                   ELSE                                                   *
016300000000     C*----------CONTROLLO CATEGORIA FISCALE :             *
016400000000     C                   MOVE      '163'         GRUTAB                         *
016500000000     C                   MOVE      *ZEROS        CODTAB                         *
016600000000     C                   MOVE      CATFIS        CODTAB                         *
016700000000     C     KEYANT        CHAIN     ANTAB00F                           84        *
016800000000     C     *IN84         CABEQ     '1'           SOPRA                          *
016900000000     C                   END                                                    *
017000000000     C                   END                                                    *
017100000000     C*----------RICERCA ALFABETICA ARTICOLO :             *
017200000000     C     CODART        IFNE      *ALL'X'                                      *
017300861212     C     CODART        IFEQ      INTERR                                       *
017400000000     C                   MOVE      *BLANKS       CODART                         *
017500000000     C                   MOVE      *BLANKS       DESART                         *
017600990317     C                   CALL      'TII104R'                                    *
017700000000     C                   PARM                    CODART                         *
017800841024     C                   PARM                    DESART           30            *
017900000000     C                   GOTO      SOPRA                                        *
018000000000     C                   ELSE                                                   *
018100000000     C                   END                                                    *
018200000000     C                   END                                                    *
018300000000     C*----------CONTROLLI OK :                            *
018400000000     C*----------------------------------------------------*
018500841024     C*     GESTIONE SUBFILE SOLO CODICE LISTINO           *
018600000000     C*----------------------------------------------------*
018700841024     C     CLAMER        CABNE     *ALL'X'       NOAMP1                         *
018800841024     C     SOTCLA        CABNE     *ALL'X'       NOAMP1                         *
018900841024     C     CATFIS        CABNE     *ALL'X'       NOAMP1                         *
019000841024     C     CODART        CABNE     *ALL'X'       NOAMP1                         *
019100990118     C  N01              OPEN      tmamp01L                                     *
019200841024     C                   SETON                                        01        *
019300841024     C*---------- EMISSIONE TESTATA SFL :                  *
019400850304     C                   WRITE     ANG8ATS1                                     *
019500841024     C*----------                                          *
019600841024     C     SUBLIS        TAG                                                    *
019700990118     C     *LOVAL        SETLL     tmamp01L                                     *
019800000000     C                   Z-ADD     1             REC1              4 0          *
019900000000     C*---------- PULIZIA SLFCT :                          *
020000841024     C     SUBLI1        TAG                                                    *
020100000000     C                   SETON                                            70    *
020200850304     C                   WRITE     ANG8ASC1                                     *
020300841024     C                   SETOFF                                         7071    *
020400000000     C*---------- CARICAMENTO SFL :                        *
020500841026     C     1             DO        39            REC1                           *
020600990118     C                   READ      tmamp1                                 72    *
020700841025     C  N72              EXSR      CARICA                                       *
020800000000     C  N72              END                                                    *
020900000000     C*---------- EMISSIONE SFL :                          *
021000850304     C     SUBLI2        TAG                                                    *
021100850304     C                   EXFMT     ANG8ASC1                                     *
021200841024     C   KG              GOTO      SUBLIS                                       *
021300990118     C   Kl
021400850304     CANN10              GOTO      INIZIO                                       *
021500990118     C   Kl
021600850304     CAN 10              GOTO      FINE                                         *
021700850304     C  N72              GOTO      SUBLI1                                       *
021800850304     C   72
021900850304     CANN10              GOTO      SUBLI2                                       *
022000850304     C   72
022100850304     CAN 10              GOTO      FINE                                         *
022200841024     C     NOAMP1        TAG                                                    *
022300000000     C*----------------------------------------------------*
022400841024     C*     GESTIONE SUBFILE LISTINO + CLASSE MERC.        *
022500841024     C*----------------------------------------------------*
022600841024     C     SOTCLA        CABNE     *ALL'X'       NOAMP3                         *
022700841024     C     CATFIS        CABNE     *ALL'X'       NOAMP3                         *
022800841024     C     CODART        CABNE     *ALL'X'       NOAMP3                         *
022900990118     C  N03              OPEN      tmamp03L                                     *
023000841024     C                   SETON                                        03        *
023100841024     C*---------- EMISSIONE TESTATA SFL :                  *
023200850304     C                   WRITE     ANG8ATS1                                     *
023300841024     C*----------                                          *
023400841024     C     SUBCLA        TAG                                                    *
023500990118     C     CLAMER        SETLL     tmamp03L                                     *
023600841024     C                   Z-ADD     1             REC1              4 0          *
023700841024     C*---------- PULIZIA SLFCT :                          *
023800841024     C     SUBCL1        TAG                                                    *
023900841024     C                   SETON                                            70    *
024000850304     C                   WRITE     ANG8ASC1                                     *
024100841024     C                   SETOFF                                         7071    *
024200841024     C*---------- CARICAMENTO SFL :                        *
024300841026     C     1             DO        39            REC1                           *
024400990118     C     CLAMER        READE     tmamp3                                 72    *
024500841025     C  N72              EXSR      CARICA                                       *
024600841024     C  N72              END                                                    *
024700841024     C*---------- EMISSIONE SFL :                          *
024800841024     C     SUBCL2        TAG                                                    *
024900850304     C                   EXFMT     ANG8ASC1                                     *
025000841024     C   KG              GOTO      SUBCLA                                       *
025100990118     C   Kl
025200850304     CANN10              GOTO      INIZIO                                       *
025300990118     C   Kl
025400850304     CAN 10              GOTO      FINE                                         *
025500850304     C  N72              GOTO      SUBCL1                                       *
025600850304     C   72
025700850304     CANN10              GOTO      SUBCL2                                       *
025800850304     C   72
025900850304     CAN 10              GOTO      FINE                                         *
026000841024     C     NOAMP3        TAG                                                    *
026100841024     C*----------------------------------------------------*
026200841024      /EJECT
026300841024     C*----------------------------------------------------*
026400841024     C*     GESTIONE SUBFILE LISTINO + SOTTOCLASSE MERC.   *
026500841024     C*----------------------------------------------------*
026600841024     C     CATFIS        CABNE     *ALL'X'       NOAMP4                         *
026700841024     C     CODART        CABNE     *ALL'X'       NOAMP4                         *
026800990118     C  N04              OPEN      tmamp04L                                     *
026900841024     C                   SETON                                        04        *
027000841024     C*---------- EMISSIONE TESTATA SFL :                  *
027100850304     C                   WRITE     ANG8ATS1                                     *
027200841024     C*----------                                          *
027300841024     C     SUBSOT        TAG                                                    *
027400990118     C     SOTCLA        SETLL     tmamp04L                                     *
027500841024     C                   Z-ADD     1             REC1              4 0          *
027600841024     C*---------- PULIZIA SLFCT :                          *
027700841024     C     SUBSO1        TAG                                                    *
027800841024     C                   SETON                                            70    *
027900850304     C                   WRITE     ANG8ASC1                                     *
028000841024     C                   SETOFF                                         7071    *
028100841024     C*---------- CARICAMENTO SFL :                        *
028200841026     C     1             DO        39            REC1                           *
028300990118     C     SOTCLA        READE     tmamp4                                 72    *
028400841024     C     CLAMER        IFNE      *ALL'X'                                      *
028500841024     C     AMPCLM        CABNE     CLAMER        NONCAR                         *
028600841024     C                   END                                                    *
028700841025     C  N72              EXSR      CARICA                                       *
028800841024     C     NONCAR        TAG                                                    *
028900841024     C  N72              END                                                    *
029000841024     C*---------- EMISSIONE SFL :                          *
029100841024     C     SUBSO2        TAG                                                    *
029200850304     C                   EXFMT     ANG8ASC1                                     *
029300841024     C   KG              GOTO      SUBSOT                                       *
029400990118     C   Kl
029500850304     CANN10              GOTO      INIZIO                                       *
029600990118     C   Kl
029700850304     CAN 10              GOTO      FINE                                         *
029800850304     C  N72              GOTO      SUBSO1                                       *
029900850304     C   72
030000850304     CANN10              GOTO      SUBSO2                                       *
030100850304     C   72
030200850304     CAN 10              GOTO      FINE                                         *
030300841024     C     NOAMP4        TAG                                                    *
030400841024     C*----------------------------------------------------*
030500841024      /EJECT
030600841024     C*----------------------------------------------------*
030700841024     C*     GESTIONE SUBFILE LISTINO + CATEGORIA FISCALE   *
030800841024     C*----------------------------------------------------*
030900841024     C     CODART        CABNE     *ALL'X'       NOAMP5                         *
031000990118     C  N05              OPEN      tmamp05L                                     *
031100841024     C                   SETON                                        05        *
031200841024     C*---------- EMISSIONE TESTATA SFL :                  *
031300850304     C                   WRITE     ANG8ATS1                                     *
031400841024     C*----------                                          *
031500841024     C     SUBCAT        TAG                                                    *
031600990118     C     CATFIS        SETLL     tmamp05L                                     *
031700841024     C                   Z-ADD     1             REC1              4 0          *
031800841024     C*---------- PULIZIA SLFCT :                          *
031900841024     C     SUBCA1        TAG                                                    *
032000841024     C                   SETON                                            70    *
032100850304     C                   WRITE     ANG8ASC1                                     *
032200841024     C                   SETOFF                                         7071    *
032300841024     C*---------- CARICAMENTO SFL :                        *
032400841026     C     1             DO        39            REC1                           *
032500990118     C     CATFIS        READE     tmamp5                                 72    *
032600841025     C  N72              EXSR      CARICA                                       *
032700841024     C  N72              END                                                    *
032800841024     C*---------- EMISSIONE SFL :                          *
032900841024     C     SUBCA2        TAG                                                    *
033000850304     C                   EXFMT     ANG8ASC1                                     *
033100841024     C   KG              GOTO      SUBCAT                                       *
033200990118     C   Kl
033300850304     CANN10              GOTO      INIZIO                                       *
033400990118     C   Kl
033500850304     CAN 10              GOTO      FINE                                         *
033600850304     C  N72              GOTO      SUBCA1                                       *
033700850304     C   72
033800850304     CANN10              GOTO      SUBCA2                                       *
033900850304     C   72
034000850304     CAN 10              GOTO      FINE                                         *
034100841024     C     NOAMP5        TAG                                                    *
034200841024     C*----------------------------------------------------*
034300841024      /EJECT
034400841024     C*----------------------------------------------------*
034500841024     C*     GESTIONE SUBFILE LISTINO + CODICE ARTICOLO     *
034600841024     C*----------------------------------------------------*
034700841024     C     CLAMER        CABNE     *ALL'X'       NOART1                         *
034800841024     C     SOTCLA        CABNE     *ALL'X'       NOART1                         *
034900841024     C     CATFIS        CABNE     *ALL'X'       NOART1                         *
035000990118     C  N01              OPEN      tmamp01L                                     *
035100841024     C                   SETON                                        01        *
035200841024     C*---------- EMISSIONE TESTATA SFL :                  *
035300850304     C                   WRITE     ANG8ATS1                                     *
035400841024     C*----------                                          *
035500841024     C     SUBART        TAG                                                    *
035600990118     C     CODART        SETLL     tmamp01L                                     *
035700841024     C                   Z-ADD     1             REC1              4 0          *
035800841024     C*---------- PULIZIA SLFCT :                          *
035900841024     C     SUBAR1        TAG                                                    *
036000841024     C                   SETON                                            70    *
036100850304     C                   WRITE     ANG8ASC1                                     *
036200841024     C                   SETOFF                                         7071    *
036300841024     C*---------- CARICAMENTO SFL :                        *
036400841026     C     1             DO        39            REC1                           *
036500990118     C     CODART        READE     tmamp1                                 72    *
036600841025     C  N72              EXSR      CARICA                                       *
036700841024     C  N72              END                                                    *
036800841024     C*---------- EMISSIONE SFL :                          *
036900841024     C     SUBAR2        TAG                                                    *
037000850304     C                   EXFMT     ANG8ASC1                                     *
037100841024     C   KG              GOTO      SUBART                                       *
037200990118     C   Kl
037300850304     CANN10              GOTO      INIZIO                                       *
037400990118     C   Kl
037500850304     CAN 10              GOTO      FINE                                         *
037600850304     C  N72              GOTO      SUBAR1                                       *
037700850304     C   72
037800850304     CANN10              GOTO      SUBAR2                                       *
037900850304     C   72
038000850304     CAN 10              GOTO      FINE                                         *
038100841024     C     NOART1        TAG                                                    *
038200000000     C*----------------------------------------------------*
038300000000     C*     USCITA PROGRAMMA                               *
038400000000     C*----------------------------------------------------*
038500841024     C     FINE          TAG                                                    *
038600850304     C   99              MOVEL     'KPJB206'     KMSMN                          *
038700841024     C                   SETON                                            LR    *
038800841025     C*----------------------------------------------------*
038900841025     C*     CARICAMENTO SUBFILE                            *
039000841025     C*----------------------------------------------------*
039100841025     CSR   CARICA        BEGSR                                                  *
039200990118     C     KEYLIS        CHAIN     tmlmp000                           80        *
039300850304     C   80REC1          IFGE      1                                            *
039400850304     C                   SUB       1             REC1                           *
039500850304     C                   END                                                    *
039600841025     C  N80              DO                                                     *
039700841025     C                   Z-ADD     LMPVP1        PREZZ1                         *
039800841025     C                   Z-ADD     LMPVP2        PREZZ2                         *
039900841025     C                   Z-ADD     LMPVP3        PREZZ3                         *
040000841025     C                   Z-ADD     LMPVP4        PREZZ4                         *
040100841025     C                   Z-ADD     LMPVP5        PREZZ5                         *
040200850304     C                   WRITE     ANG8ASF1                                     *
040300850304     C                   SETON                                        71        *
040400841025     C                   END                                                    *
040500841025     CSR                 ENDSR                                                  *
040600990118     C*----------------------------------------------------*
040700990118     C* Reperimento dati società
040800990118     C*----------------------------------------------------*
040900990118     C     REPSOC        BEGSR
041000990118     C*
041100990118     C                   CALLB     'XSOC'
041200990118     C                   PARM                    TIPXSC            6
041300990118     C                   PARM                    SOCXSC            3
041400990118     C                   PARM                    CDSXSC            9 0
041500990118     C                   PARM                    MODXSC            3
041600990118     C                   PARM      *blanks       RTNXSC            1
041700990118     C                   PARM                    XSOCDS
041800990118     C                   PARM                    KPJBA
041900990118     C*
042000990118     C                   ENDSR
042100841025     C*----------------------------------------------------*
