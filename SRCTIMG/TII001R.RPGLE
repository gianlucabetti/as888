000100990519     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990519     H*PARMS ACTGRP(QILE)
000300990118     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400861211     F* ANGA00R  *-AN/38-*-------------------------------------------*
000500861211     F*------------------*                                           *
000600861211     F*       - MANUTENZIONE LISTINI PRODOTTI FINITI -               *
000700861211     F*       ----------------------------------------               *
000800861211     F*                                                   AZN.: GA00 *
000900990118     F*         FILE : tmlmp01L ANTAB00F ANGA00D                     *
001000990118     F*                tmamp01L tmamp03L tmamp04L tmamp05L           *
001100861211     F*                                                              *
001200861211     F*         PGM. : RIC001R   - RICERCA DITTA                     *
001300990317     F*                tii104R   - RICERCA ANAGRAFICA MATERIE PRIME  *
001400990317     F*                TII110R   - RICERCA CLASSI MERCEOLOGICHE      *
001500990317     F*                TII111R   - RICERCA SOTTOCLASSI MERCEOLOG.    *
001600990317     F*                TII118R   - RICERCA CATEGORIE FISCALI         *
001700990317     F*                TII116R   - RICERCA LISTINI MATERIE PRIME     *
001800861211     F*                                                              *
001900861211     F*--------------------------------------------------------------*
002000990118     Ftmlmp01L  UF A E           K DISK
002100861211     F*---------
002200990118     Ftmamp01L  IF   E           K DISK    USROPN
002300990118     F                                     RENAME(tmamp000:tmamp1)
002400861211     F*---------
002500990118     Ftmamp03L  IF   E           K DISK    USROPN
002600990118     F                                     RENAME(tmamp000:tmamp3)
002700861211     F*---------
002800990118     Ftmamp04L  IF   E           K DISK    USROPN
002900990118     F                                     RENAME(tmamp000:tmamp4)
003000861211     F*---------
003100990118     Ftmamp05L  IF   E           K DISK    USROPN
003200990118     F                                     RENAME(tmamp000:tmamp5)
003300990309     F*---------
003400861211     FANTAB00F  IF   E           K DISK
003500010725     f*
003600010725     FTNTBE01l  IF   E           k disk
003700861211     F*---------
003800990317     Ftii001D   CF   E             WORKSTN
003900861211     F                                     SFILE(ANGA0SF1:REC1)
004000861211     D KPJBA         E DS
004100861211     D*-----PASSAGGIO PARAMETRI KPJBA
004200861211     D  MSGERR               453    502
004300861211     D                 DS
004400861211     D*-----DS COSTRUZIONE ARTICOLO + VERSIONE
004500871009     D  AMPMAT                 1     12
004600871009     D  PRODOT                 1     12
004700861211     D DES75           DS
004800861211     D*-----DS DESCRIZIONI LISTINO
004900861211     D  DESLIS                 1     25
005000861211     D  DESPR1                26     35
005100861211     D  DESPR2                36     45
005200861211     D  DESPR3                46     55
005300861211     D  DESPR4                56     65
005400861211     D  DESPR5                66     75
005500990118     d dataiso         s               d   datfmt(*iso)
005600990118     d dataeur         s               d   datfmt(*eur)
005700990118     D soc001        e ds                  extname(xsoc001ds)
005800990118     D xsocds          DS          1000
005900990309     D* Ricerca divise
006000990309     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
006100010725    >D DGED          E DS
006200990519     D*-------------
006300990519     D*  Prototipazione delle procedure
006400990519     D/COPY *LIBL/SRCCPY,PJX002PR
006500861211     C*----------------------------------------------------*
006600861211     C     *ENTRY        PLIST                                                  *
006700861211     C                   PARM                    KPJBA                          *
006800990118     C*---------- RICERCA DITTA :
006900990118     C                   MOVEL     'SOC001'      TIPXSC
007000990118     C                   MOVEL     *blank        SOCXSC
007100990118     C                   EXSR      REPSOC
007200990118     C     RTNXSC        IFNE      '1'
007300990118     C                   MOVEL     XSOCDS        SOC001
007400990118     C                   MOVEL     xscrgs        desdit
007500990118     c                   end
007600861211     C*----------                                          *
007700990118     c                   time                    como14           14 0
007800990118     c                   move      como14        woggi             8 0
007900990118     c                   move      woggi         dataeur
008000990118     c                   move      dataeur       dataiso
008100990118     c                   move      dataiso       woggig            8 0
008200871009     C                   MOVEL     '?'           INTERR           12
008300010725     C*--- Reperisco la divisa corrente ------------------------------*
008400010725     C     ktbe          KLIST
008500010725     C                   KFLD                    kcod
008600010725     C                   KFLD                    kke1
008700010725     C     *like         Define    tbecod        kcod
008800010725     C     *like         Define    tbeke1        kke1
008900010725     C                   move      'GED'         kcod
009000010725     C                   move      *blanks       kke1
009100010725     C                   movel     'MAGAZZINO'   kke1
009200010725     c                   clear                   dged
009300010725     C     ktbe          CHAIN     tntbe01l                           54
009400010725     C  n54              movel     tbeuni        dged
009500861211     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
009600861211     C     KEYANT        KLIST                                                  *
009700861211     C                   KFLD                    GRUTAB            3            *
009800861211     C                   KFLD                    CODTAB            7            *
009900990118     C*---------- KEY PER RICERCA ARTICOLO SU tmlmp01L :   *
010000861211     C     KEYLIS        KLIST                                                  *
010100861211     C                   KFLD                    CODLIS                         *
010200861211     C                   KFLD                    AMPMAT                         *
010300990309     C                   KFLD                    codval                         *
010400861211     C*----------EMISSIONE TESTATA :                       *
010500861211     C*----------------------------------------------------*
010600861211     C                   WRITE     ANGA001                                      *
010700861211     C                   MOVE      *BLANKS       CODLIS                         *
010800010725     c                   if        §gedcn <> *blank
010900010725     C                   MOVEl     §gedcn        CODval                         *
011000010725     c                   else
011100010725     c                   movel     xscdiv        codval
011200010725     c                   end
011300861211     C     INIZIO        TAG                                                    *
011400861211     C                   MOVE      *ALL'X'       CLAMER                         *
011500861211     C                   MOVE      *ALL'X'       SOTCLA                         *
011600861211     C                   MOVE      *ALL'X'       CATFIS                         *
011700861211     C                   MOVE      *ALL'X'       CODART                         *
011800861211     C     SOPRA         TAG                                                    *
011900861211     C*----------EMISSIONE ERRORI SU TESTATA :             *
012000861211     C   81                                                                     *
012100861211     COR 82                                                                     *
012200861211     COR 83                                                                     *
012300861211     COR 84                                                                     *
012400990309     COR 85                                                                     *
012500990309     COR 86              SETON                                        90        *
012600861211     C*----------EMISSIONE TESTATA :                       *
012700861211     C                   EXFMT     ANGA002                                      *
012800990408     C   Kc              GOTO      FINE                                         *
012900861211     C*----------CONTROLLO DATI PROMPT :                   *
013000861211     C                   MOVEA     *ZEROS        *IN(80)                        *
013100861211     C*                                                    *
013200990309     C*----------RICERCA LISTINO :                         *
013300990309     C     CODval        IFEQ      '?   '                                       *
013400990309     C                   MOVEL     '01'          OPZ015
013500990309     C                   MOVEL     *blank        Div015
013600990309     C                   MOVEL     A015DS        KPJBU
013700990309     C                   CALL      'ANA015R'
013800990309     C                   PARM                    KPJBA
013900990309     C                   MOVEL     KPJBU         A015DS
014000990309     C     Div015        IFNE      *BLANK
014100990309     C                   MOVEL     Div015        codval
014200990309     C                   GOTO      SOPRA                                        *
014300990309     C                   ENDIF
014400990309     C                   ENDIF
014500990309     C     CODval        CABEQ     *BLANKS       SOPRA                    86    *
014600990519     C* controllo divisa valida
014700990519     C                   if        not *in86
014800990519     C                   clear                   Errore            3 0
014900990519     C                   move      codval        divisa0202
015000990519     C                   clear                   Dtrif0202
015100990519     C                   eval      Errore = X0201RpDVS(Divisa0202:Dtrif0202:
015200990519     C                                      ANDVS00F: %size(ANDVS00F) :
015300990519     C                                      ANDVX00F: %size(ANDVX00F))
015400990519     c                   if        Errore = 1 or Errore = 3
015500990519     C                   seton                                        86
015600990519     C                   end
015700990519     C                   end
015800990519     c  n86              movel     dvsdesbrev    codvald
015900990309     c   86              goto      sopra
016000861211     C*----------RICERCA LISTINO :                         *
016100861211     C     CODLIS        IFEQ      '?  '                                        *
016200861211     C                   MOVE      *BLANKS       CODLIS                         *
016300861211     C                   MOVE      *BLANKS       DES75            75            *
016400990317     C                   CALL      'TII116R'                                    *
016500861211     C                   PARM                    CODLIS                         *
016600861211     C                   PARM                    DES75                          *
016700861211     C                   GOTO      SOPRA                                        *
016800861211     C                   END                                                    *
016900861211     C*----------CONTROLLO LISTINO :                       *
017000861211     C     CODLIS        CABEQ     *BLANKS       SOPRA                    81    *
017100861211     C*-----                                               *
017200861211     C                   MOVE      '161'         GRUTAB                         *
017300861211     C                   MOVE      *ZEROS        CODTAB                         *
017400861211     C                   MOVE      CODLIS        CODTAB                         *
017500861211     C     KEYANT        CHAIN     ANTAB00F                           81        *
017600861211     C     *IN81         CABEQ     '1'           SOPRA                          *
017700861211     C                   MOVEL     TABDES        DES75                          *
017800861211     C*----------RICERCA CLASSE MERCEOLOGICA :             *
017900861211     C     CLAMER        IFNE      *ALL'X'                                      *
018000861211     C     CLAMER        IFEQ      '?  '                                        *
018100861211     C                   MOVE      *BLANKS       CLAMER                         *
018200861211     C                   MOVE      *BLANKS       DESCLA           25            *
018300990317     C                   CALL      'TII110R'                                    *
018400861211     C                   PARM                    CLAMER                         *
018500861211     C                   PARM                    DESCLA                         *
018600861211     C                   GOTO      SOPRA                                        *
018700861211     C                   ELSE                                                   *
018800861211     C*----------CONTROLLO CLASSE MERCEOLOGICA :           *
018900861211     C                   MOVE      '155'         GRUTAB                         *
019000861211     C                   MOVE      *ZEROS        CODTAB                         *
019100861211     C                   MOVE      CLAMER        CODTAB                         *
019200861211     C     KEYANT        CHAIN     ANTAB00F                           82        *
019300861211     C     *IN82         CABEQ     '1'           SOPRA                          *
019400861211     C                   END                                                    *
019500861211     C                   END                                                    *
019600861211     C*----------RICERCA SOTTOCLASSE MERCEOLOGICA :        *
019700861211     C     SOTCLA        IFNE      *ALL'X'                                      *
019800861211     C     SOTCLA        IFEQ      '?    '                                      *
019900861211     C                   MOVE      *BLANKS       SOTCLA                         *
020000861211     C                   MOVE      *BLANKS       DESSOT           25            *
020100990317     C                   CALL      'TII111R'                                    *
020200861211     C                   PARM                    SOTCLA                         *
020300861211     C                   PARM                    DESSOT                         *
020400861211     C                   GOTO      SOPRA                                        *
020500861211     C                   ELSE                                                   *
020600861211     C*----------CONTROLLO SOTTOCLASSE MERCEOLOGICA :      *
020700861211     C                   MOVE      '156'         GRUTAB                         *
020800861211     C                   MOVE      *ZEROS        CODTAB                         *
020900861211     C                   MOVE      SOTCLA        CODTAB                         *
021000861211     C     KEYANT        CHAIN     ANTAB00F                           83        *
021100861211     C     *IN83         CABEQ     '1'           SOPRA                          *
021200861211     C                   END                                                    *
021300861211     C                   END                                                    *
021400861211     C*----------RICERCA CATEGORIA FISCALE :               *
021500861211     C     CATFIS        IFNE      *ALL'X'                                      *
021600861211     C     CATFIS        IFEQ      '? '                                         *
021700861211     C                   MOVE      *BLANKS       CATFIS                         *
021800861211     C                   MOVE      *BLANKS       DECAFI           25            *
021900990317     C                   CALL      'TII118R'                                    *
022000861211     C                   PARM                    CATFIS                         *
022100861211     C                   PARM                    DECAFI                         *
022200861211     C                   GOTO      SOPRA                                        *
022300861211     C                   ELSE                                                   *
022400861211     C*----------CONTROLLO CATEGORIA FISCALE :             *
022500861211     C                   MOVE      '163'         GRUTAB                         *
022600861211     C                   MOVE      *ZEROS        CODTAB                         *
022700861211     C                   MOVE      CATFIS        CODTAB                         *
022800861211     C     KEYANT        CHAIN     ANTAB00F                           84        *
022900861211     C     *IN84         CABEQ     '1'           SOPRA                          *
023000861211     C                   END                                                    *
023100861211     C                   END                                                    *
023200861211     C*----------RICERCA ALFABETICA ARTICOLO :             *
023300861211     C     CODART        IFNE      *ALL'X'                                      *
023400861211     C     CODART        IFEQ      INTERR                                       *
023500861211     C                   MOVE      *BLANKS       CODART                         *
023600861211     C                   MOVE      *BLANKS       DESART                         *
023700990317     C                   CALL      'TII104R'                                    *
023800861211     C                   PARM                    CODART                         *
023900861211     C                   PARM                    DESART           30            *
024000861211     C                   GOTO      SOPRA                                        *
024100861211     C                   END                                                    *
024200861211     C                   END                                                    *
024300861211     C*----------CONTROLLI OK :                            *
024400861211     C*----------------------------------------------------*
024500861211     C*     GESTIONE SUBFILE SOLO CODICE LISTINO           *
024600861211     C*----------------------------------------------------*
024700861211     C     CLAMER        CABNE     *ALL'X'       NOAMP1                         *
024800861211     C     SOTCLA        CABNE     *ALL'X'       NOAMP1                         *
024900861211     C     CATFIS        CABNE     *ALL'X'       NOAMP1                         *
025000861211     C     CODART        CABNE     *ALL'X'       NOAMP1                         *
025100990118     C  N01              OPEN      tmamp01L                                     *
025200861211     C                   SETON                                        01        *
025300861211     C*---------- EMISSIONE TESTATA SFL :                  *
025400861211     C                   WRITE     ANGA0TS1                                     *
025500861211     C*----------                                          *
025600861211     C     SUBLIS        TAG                                                    *
025700990118     C     *LOVAL        SETLL     tmamp01L                                     *
025800861211     C                   Z-ADD     1             REC1              4 0          *
025900861211     C*---------- PULIZIA SLFCT :                          *
026000861211     C     SUBLI1        TAG                                                    *
026100861211     C                   SETON                                            70    *
026200861211     C                   WRITE     ANGA0SC1                                     *
026300861211     C                   SETOFF                                         7071    *
026400861211     C*---------- CARICAMENTO SFL :                        *
026500861211     C     1             DO        39            REC1                           *
026600990118     C                   READ      tmamp1                                 72    *
026700861211     C  N72              EXSR      CARICA                                       *
026800861211     C  N72              END                                                    *
026900861211     C*---------- EMISSIONE SFL :                          *
027000861211     C     SUBLI2        TAG                                                    *
027100861211     C                   EXFMT     ANGA0SC1                                     *
027200861211     C   KG              GOTO      SUBLIS                                       *
027300990118     C   Kl              GOTO      INIZIO                                       *
027400861211     C*---------- LETTURA RECORD VARIATI :                 *
027500861211     C   71              DO        *HIVAL                                       *
027600861211     C                   READC     ANGA0SF1                               91    *
027700861211     C  N91              EXSR      SCRIVI                                       *
027800861211     C  N91              END                                                    *
027900861211     C  N71                                                                     *
028000861211     COR 91
028100861211     CANN72              GOTO      SUBLI1                                       *
028200861211     C   91
028300861211     CAN 72              GOTO      SUBLI2                                       *
028400861211     C     NOAMP1        TAG                                                    *
028500861211     C*----------------------------------------------------*
028600861211     C*     GESTIONE SUBFILE LISTINO + CLASSE MERC.        *
028700861211     C*----------------------------------------------------*
028800861211     C     SOTCLA        CABNE     *ALL'X'       NOAMP3                         *
028900861211     C     CATFIS        CABNE     *ALL'X'       NOAMP3                         *
029000861211     C     CODART        CABNE     *ALL'X'       NOAMP3                         *
029100990118     C  N03              OPEN      tmamp03L                                     *
029200861211     C                   SETON                                        03        *
029300861211     C*---------- EMISSIONE TESTATA SFL :                  *
029400861211     C                   WRITE     ANGA0TS1                                     *
029500861211     C*----------                                          *
029600861211     C     SUBCLA        TAG                                                    *
029700990118     C     CLAMER        SETLL     tmamp03L                                     *
029800861211     C                   Z-ADD     1             REC1              4 0          *
029900861211     C*---------- PULIZIA SLFCT :                          *
030000861211     C     SUBCL1        TAG                                                    *
030100861211     C                   SETON                                            70    *
030200861211     C                   WRITE     ANGA0SC1                                     *
030300861211     C                   SETOFF                                         7071    *
030400861211     C*---------- CARICAMENTO SFL :                        *
030500861211     C     1             DO        39            REC1                           *
030600990118     C     CLAMER        READE     tmamp3                                 72    *
030700861211     C  N72              EXSR      CARICA                                       *
030800861211     C  N72              END                                                    *
030900861211     C*---------- EMISSIONE SFL :                          *
031000861211     C     SUBCL2        TAG                                                    *
031100861211     C                   EXFMT     ANGA0SC1                                     *
031200861211     C   KG              GOTO      SUBCLA                                       *
031300990118     C   Kl              GOTO      INIZIO                                       *
031400861211     C*---------- LETTURA RECORD VARIATI :                 *
031500861211     C   71              DO        *HIVAL                                       *
031600861211     C                   READC     ANGA0SF1                               91    *
031700861211     C  N91              EXSR      SCRIVI                                       *
031800861211     C  N91              END                                                    *
031900861211     C  N71                                                                     *
032000861211     COR 91
032100861211     CANN72              GOTO      SUBCL1                                       *
032200861211     C   91
032300861211     CAN 72              GOTO      SUBCL2                                       *
032400861211     C     NOAMP3        TAG                                                    *
032500861211     C*----------------------------------------------------*
032600861211     C*     GESTIONE SUBFILE LISTINO + SOTTOCLASSE MERC.   *
032700861211     C*----------------------------------------------------*
032800861211     C     CATFIS        CABNE     *ALL'X'       NOAMP4                         *
032900861211     C     CODART        CABNE     *ALL'X'       NOAMP4                         *
033000990118     C  N04              OPEN      tmamp04L                                     *
033100861211     C                   SETON                                        04        *
033200861211     C*---------- EMISSIONE TESTATA SFL :                  *
033300861211     C                   WRITE     ANGA0TS1                                     *
033400861211     C*----------                                          *
033500861211     C     SUBSOT        TAG                                                    *
033600990118     C     SOTCLA        SETLL     tmamp04L                                     *
033700861211     C                   Z-ADD     1             REC1              4 0          *
033800861211     C*---------- PULIZIA SLFCT :                          *
033900861211     C     SUBSO1        TAG                                                    *
034000861211     C                   SETON                                            70    *
034100861211     C                   WRITE     ANGA0SC1                                     *
034200861211     C                   SETOFF                                         7071    *
034300861211     C*---------- CARICAMENTO SFL :                        *
034400861211     C     1             DO        39            REC1                           *
034500990118     C     SOTCLA        READE     tmamp4                                 72    *
034600861211     C     CLAMER        IFNE      *ALL'X'                                      *
034700861211     C     AMPCLM        CABNE     CLAMER        NONCAR                         *
034800861211     C                   END                                                    *
034900861211     C  N72              EXSR      CARICA                                       *
035000861211     C     NONCAR        TAG                                                    *
035100861211     C  N72              END                                                    *
035200861211     C*---------- EMISSIONE SFL :                          *
035300861211     C     SUBSO2        TAG                                                    *
035400861211     C                   EXFMT     ANGA0SC1                                     *
035500861211     C   KG              GOTO      SUBSOT                                       *
035600990118     C   Kl              GOTO      INIZIO                                       *
035700861211     C*---------- LETTURA RECORD VARIATI :                 *
035800861211     C   71              DO        *HIVAL                                       *
035900861211     C                   READC     ANGA0SF1                               91    *
036000861211     C  N91              EXSR      SCRIVI                                       *
036100861211     C  N91              END                                                    *
036200861211     C  N71                                                                     *
036300861211     COR 91
036400861211     CANN72              GOTO      SUBSO1                                       *
036500861211     C   91
036600861211     CAN 72              GOTO      SUBSO2                                       *
036700861211     C     NOAMP4        TAG                                                    *
036800861211     C*----------------------------------------------------*
036900861211     C*     GESTIONE SUBFILE LISTINO + CATEGORIA FISCALE   *
037000861211     C*----------------------------------------------------*
037100861211     C     CODART        CABNE     *ALL'X'       NOAMP5                         *
037200990118     C  N05              OPEN      tmamp05L                                     *
037300861211     C                   SETON                                        05        *
037400861211     C*---------- EMISSIONE TESTATA SFL :                  *
037500861211     C                   WRITE     ANGA0TS1                                     *
037600861211     C*----------                                          *
037700861211     C     SUBCAT        TAG                                                    *
037800990118     C     CATFIS        SETLL     tmamp05L                                     *
037900861211     C                   Z-ADD     1             REC1              4 0          *
038000861211     C*---------- PULIZIA SLFCT :                          *
038100861211     C     SUBCA1        TAG                                                    *
038200861211     C                   SETON                                            70    *
038300861211     C                   WRITE     ANGA0SC1                                     *
038400861211     C                   SETOFF                                         7071    *
038500861211     C*---------- CARICAMENTO SFL :                        *
038600861211     C     1             DO        39            REC1                           *
038700990118     C     CATFIS        READE     tmamp5                                 72    *
038800861211     C  N72              EXSR      CARICA                                       *
038900861211     C  N72              END                                                    *
039000861211     C*---------- EMISSIONE SFL :                          *
039100861211     C     SUBCA2        TAG                                                    *
039200861211     C                   EXFMT     ANGA0SC1                                     *
039300861211     C   KG              GOTO      SUBCAT                                       *
039400990118     C   Kl              GOTO      INIZIO                                       *
039500861211     C*---------- LETTURA RECORD VARIATI :                 *
039600861211     C   71              DO        *HIVAL                                       *
039700861211     C                   READC     ANGA0SF1                               91    *
039800861211     C  N91              EXSR      SCRIVI                                       *
039900861211     C  N91              END                                                    *
040000861211     C  N71                                                                     *
040100861211     COR 91
040200861211     CANN72              GOTO      SUBCA1                                       *
040300861211     C   91
040400861211     CAN 72              GOTO      SUBCA2                                       *
040500861211     C     NOAMP5        TAG                                                    *
040600861211     C*----------------------------------------------------*
040700861211     C*     GESTIONE SUBFILE LISTINO + CODICE ARTICOLO     *
040800861211     C*----------------------------------------------------*
040900990118     C  N01              OPEN      tmamp01L                                     *
041000861211     C                   SETON                                        01        *
041100861211     C*---------- EMISSIONE TESTATA SFL :                  *
041200861211     C                   WRITE     ANGA0TS1                                     *
041300861211     C*----------                                          *
041400861211     C     SUBART        TAG                                                    *
041500990118     C     CODART        SETLL     tmamp01L                                     *
041600861211     C                   Z-ADD     1             REC1              4 0          *
041700861211     C*---------- PULIZIA SLFCT :                          *
041800861211     C     SUBAR1        TAG                                                    *
041900861211     C                   SETON                                            70    *
042000861211     C                   WRITE     ANGA0SC1                                     *
042100861211     C                   SETOFF                                         7071    *
042200861211     C*---------- CARICAMENTO SFL :                        *
042300861211     C     1             DO        39            REC1                           *
042400990118     C     CODART        READE     tmamp1                                 72    *
042500861211     C  N72              EXSR      CARICA                                       *
042600861211     C  N72              END                                                    *
042700861211     C*---------- EMISSIONE SFL :                          *
042800861211     C     SUBAR2        TAG                                                    *
042900861211     C                   EXFMT     ANGA0SC1                                     *
043000861211     C   KG              GOTO      SUBART                                       *
043100990118     C   Kl              GOTO      INIZIO                                       *
043200861211     C*---------- LETTURA RECORD VARIATI :                 *
043300861211     C   71              DO        *HIVAL                                       *
043400861211     C                   READC     ANGA0SF1                               91    *
043500861211     C  N91              EXSR      SCRIVI                                       *
043600861211     C  N91              END                                                    *
043700861211     C  N71                                                                     *
043800861211     COR 91
043900861211     CANN72              GOTO      SUBAR1                                       *
044000861211     C   91
044100861211     CAN 72              GOTO      SUBAR2                                       *
044200861211     C*----------------------------------------------------*
044300861211     C*     USCITA PROGRAMMA                               *
044400861211     C*----------------------------------------------------*
044500861211     C     FINE          TAG                                                    *
044600861211     C                   SETON                                            LR    *
044700861211     C*----------------------------------------------------*
044800861211     C*     CARICAMENTO SUBFILE                            *
044900861211     C*----------------------------------------------------*
045000861211     CSR   CARICA        BEGSR                                                  *
045100990118     C     KEYLIS        CHAIN     tmlmp000                           80        *
045200861211     C  N80              DO                                                     *
045300861211     C                   Z-ADD     LMPVP1        PREZZ1                         *
045400861211     C                   Z-ADD     LMPVP2        PREZZ2                         *
045500861211     C                   Z-ADD     LMPVP3        PREZZ3                         *
045600861211     C                   Z-ADD     LMPVP4        PREZZ4                         *
045700861211     C                   Z-ADD     LMPVP5        PREZZ5                         *
045800861211     C                   END                                                    *
045900861211     C   80              DO                                                     *
046000861211     C                   Z-ADD     0             PREZZ1                         *
046100861211     C                   Z-ADD     0             PREZZ2                         *
046200861211     C                   Z-ADD     0             PREZZ3                         *
046300861211     C                   Z-ADD     0             PREZZ4                         *
046400861211     C                   Z-ADD     0             PREZZ5                         *
046500861211     C                   END                                                    *
046600861211     C                   WRITE     ANGA0SF1                                     *
046700861211     C                   SETON                                        71        *
046800861211     CSR                 ENDSR                                                  *
046900861211     C*----------------------------------------------------*
047000861211     C*     SCRITTURA SUL FILE "LISTINI"                   *
047100861211     C*----------------------------------------------------*
047200861211     CSR   SCRIVI        BEGSR                                                  *
047300990118     C     KEYLIS        CHAIN     tmlmp000                           80        *
047400861211     C   80              DO                                                     *
047500861211     C                   MOVE      ' '           LMPATT                         *
047600990118     C                   MOVE      xsccut        LMPUTE                         *
047700990118     c                   move      woggig        ampdig
047800861211     C                   MOVE      *ZEROS        LMPDFG                         *
047900990309     C                   MOVE      CODval        LMPval                         *
048000990309     C                   MOVE      CODLIS        LMPLIS                         *
048100861211     C                   MOVE      AMPMAT        LMPMAT                         *
048200861211     C                   Z-ADD     PREZZ1        LMPVP1                         *
048300861211     C                   Z-ADD     PREZZ2        LMPVP2                         *
048400861211     C                   Z-ADD     PREZZ3        LMPVP3                         *
048500861211     C                   Z-ADD     PREZZ4        LMPVP4                         *
048600861211     C                   Z-ADD     PREZZ5        LMPVP5                         *
048700861211     C                   MOVE      *BLANKS       LMPFLG                         *
048800990118     C                   WRITE     tmlmp000                                     *
048900861211     C                   END                                                    *
049000861211     C  N80              DO                                                     *
049100861211     C                   MOVE      ANNULL        LMPATT                         *
049200990118     C                   MOVE      xsccut        LMPUTE                         *
049300861211     C                   Z-ADD     PREZZ1        LMPVP1                         *
049400861211     C                   Z-ADD     PREZZ2        LMPVP2                         *
049500861211     C                   Z-ADD     PREZZ3        LMPVP3                         *
049600861211     C                   Z-ADD     PREZZ4        LMPVP4                         *
049700861211     C                   Z-ADD     PREZZ5        LMPVP5                         *
049800990118     C                   UPDATE    tmlmp000                                     *
049900861211     C                   END                                                    *
050000861211     CSR                 ENDSR                                                  *
050100990118     C*----------------------------------------------------*
050200990118     C* Reperimento dati società
050300990118     C*----------------------------------------------------*
050400990118     C     REPSOC        BEGSR
050500990118     C*
050600990118     C                   CALLB     'XSOC'
050700990118     C                   PARM                    TIPXSC            6
050800990118     C                   PARM                    SOCXSC            3
050900990118     C                   PARM                    CDSXSC            9 0
051000990118     C                   PARM                    MODXSC            3
051100990118     C                   PARM      *blanks       RTNXSC            1
051200990118     C                   PARM                    XSOCDS
051300990118     C                   PARM                    KPJBA
051400990118     C*
051500990118     C                   ENDSR
051600861211     C***------------------------------------------------***
