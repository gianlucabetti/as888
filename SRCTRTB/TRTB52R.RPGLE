000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*--------------------------------------------------------------*
000300100118     F* GESTIONE TABELLA 4I - codici iso per partita iva
000400891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000500931209     F*  KC        FINE                                              *
000600940228     F*  KE        RIPRISTINO                                        *
000700940228     F*  KF        CONFERMA                                          *
000800931209     F*  KL        RITORNO                                           *
000900931209     F*  KQ        ANNULLAMENTO                                      *
001000931209     F*--------------------------------------------------------------*
001100940228     F*  02        VARIAZIONE                                        *
001200940228     F*  04        ANNULLAMENTO                                      *
001300940228     F*  40        SCAN  SU VCO4I                                    *
001400100118     F*  41        Errore forzabile su codice ISO della UE           *
001500940228     F*  80        CHAIN SU TABEL                                    *
001600931210     F*  98        CHAIN SU TABEL X ANNULLAMENTO                     *
001700931209     F*--------------------------------------------------------------*
001800940228     F*  21        ERRMSG: IMMETERE CODICE ISO (CODICE 1° VIDEATA)   *
001900940228     F*  22        ERRMSG: IMMETERE CODICE NUMERO CARATTERI          *
002000931209     F*  99        ERRORE GENERICO                                   *
002100891030     F*--------------------------------------------------------------*
002200940228     FTRTB52D   CF   E             WORKSTN
002300891030     FTABEL00F  UF A E           K DISK
002400891030     D*--------------------------------------------------------------*
002500891030     D*                        I N P U T                             *
002600891030     D*--------------------------------------------------------------*
002700100118     D ds15          E DS
002800100118     D DS4I          E DS
002900900608      *
003000971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
003100891030     D KPJBA         E DS
003200911118      *
003300911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003400940228      *
003500940228     D                SDS
003600940228     D  NOMPGM                 1     10
003700100118
003800100118     d kcod            s                   like(tblcod) inz('15')
003900100118     d forzaUE         s              1
004000931209     C*--------------------------------------------------------------*
004100931209     C*    C I C L O    P R I N C I P A L E                          *
004200931209     C*--------------------------------------------------------------*
004300931209     C*  Inizializzo prima videata impostando rich. ricerca tabella
004400940228     C                   MOVEL     ' ?'          VCOD4I
004500931209     C*
004600931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
004700931209     C     £FINE         DOUEQ     'S'
004800931209     C*
004900931209     C     TIPVID        CASEQ     '1'           GESVD1
005000931209     C     TIPVID        CASEQ     '2'           GESVD2
005100931209     C                   END
005200931209     C*
005300931209     C                   END
005400931209     C* Fine programma
005500931209     C     FINE          TAG
005600931209     C                   SETON                                        LR
005700931209     C*--------------------------------------------------------------*
005800931209     C*  GESVD1: Gestione prima videata                              *
005900931209     C*--------------------------------------------------------------*
006000931209     C     GESVD1        BEGSR
006100100118     c                   clear                   forzaUE
006200100118     C*
006300931209     C*  Emissione videata
006400940228     C                   EXFMT     TB52D02
006500931209     C                   SETOFF                                       99
006600100118     C*
006700931209     C*  Fine elaborazione
006800931209     C     *INKC         IFEQ      '1'
006900931209     C                   MOVEL     'S'           £FINE
007000931209     C                   GOTO      FINVD1
007100931209     C                   END
007200100118     C*  F7- tabella nazioni
007300100118     C     *INKg         IFEQ      '1'
007400100118     C                   call      'TRTB86R'
007500100118     c                   parm                    kpjba
007600100118     c
007700100118     C                   WRITE     TB52D01
007800100118     C                   GOTO      FINVD1
007900100118     C                   END
008000100118     C*
008100931209     C*  ERROORE : obbligatorio immettere codice
008200100118     C**   VCOD4I        IFEQ      *BLANKS
008300100118     C**                 SETON                                        2199
008400100118     C**                 GOTO      FINVD1
008500100118     C**                 END
008600100118     C*
008700100118     C*  Ricerca
008800940228     C     '?'           SCAN      VCOD4I                                 40
008900940228     C     *IN40         IFEQ      '1'
009000100118     C*
009100931209     C                   MOVEL     CODUT         TBWKUT
009200940228     C                   MOVEL     '4I'          TBWCOD
009300940228     C                   MOVE      *BLANK        VCOD4I
009400100118     C                   MOVE      *BLANK        tbwkey
009500931209     C                   CALL      'X§TABER'
009600931209     C                   PARM                    TBWKUT
009700931209     C                   PARM                    TBWCOD
009800931209     C                   PARM                    TBWKEY
009900931209     C                   PARM                    §DES             20
010000940228     C                   MOVEL     TBWKEY        VCOD4I
010100931209     C                   MOVEL     RAGUT         RSUT
010200100118     c
010300100118     C                   WRITE     TB52D01
010400100118     C*
010500931209     C                   ELSE
010600100118     C*
010700931209     C*  Reperisco dati codice
010800940228     C                   CLEAR                   DS4I
010900931209     C                   MOVEL     *BLANKS       TBWKEY
011000940228     C                   MOVEL     VCOD4I        TBWKEY
011100931209     C                   SETOFF                                       010204
011200931209     C     KEYTAB        CHAIN     TABEL                              80
011300940228     C     *IN80         IFEQ      '0'
011400940228     C                   MOVEL     TBLUNI        DS4I
011500931209     C     TBLFLG        IFEQ      ' '
011600931209     C                   SETON                                        02
011700931209     C                   ELSE
011800931209     C                   SETON                                        04
011900931209     C                   END
012000931209     C                   END
012100100118     C*
012200931209     C*  Inizializzo seconda videata
012300931209     C                   EXSR      INZVD2
012400100118     C*
012500931209     C                   END
012600100118     C*
012700931209     C     FINVD1        ENDSR
012800100118     c
012900931209     C*--------------------------------------------------------------*
013000100118     C*  GESVD2: Gestione seconda videata                            *
013100931209     C*--------------------------------------------------------------*
013200931209     C     GESVD2        BEGSR
013300100118     C*
013400931209     C*  Emissione videata
013500940228     C                   EXFMT     TB52D03
013600931209     C                   SETOFF                                       99
013700100118     C*
013800931209     C*  Fine elaborazione
013900931209     C     *INKC         IFEQ      '1'
014000931209     C                   MOVEL     'S'           £FINE
014100931209     C                   GOTO      FINVD2
014200931209     C                   END
014300100118     C*
014400931209     C*  Ritorno prima videata
014500931209     C     *INKL         IFEQ      '1'
014600931209     C                   MOVEL     '1'           TIPVID
014700940228     C                   MOVEL     ' ?'          VCOD4I
014800931209     C                   GOTO      FINVD2
014900931209     C                   END
015000100118     C*
015100931209     C*  Annullamento
015200931209     C     *INKQ         IFEQ      '1'
015300931209     C                   EXSR      ANNUL
015400931209     C                   GOTO      FINVD2
015500931209     C                   END
015600100118     C*
015700100118     C*  ERRORE : obbligatorio immettere numero chr.
015800100407     c*           oppure minimo e massimo
015900940228     C     VCHR4I        IFEQ      0
016000100407     C     §4ichmi       andeq     0
016100100407     C     §4ichmax      andeq     0
016200940228     C                   SETON                                        2299
016300940228     C                   GOTO      FINVD2
016400940228     C                   END
016500100118     c* Per Unione Europea, controllo inccrociatocon tabella azioni
016600100118     c                   if        forzaUE=' '
016700100118     c                   exsr      CtrNAZ
016800100118     c                   if        *in99
016900100118     C                   GOTO      FINVD2
017000100118     c                   endif
017100100118     c                   endif
017200100407     c* Se immesso minimo, immettere massimo  e viceversa
017300100407     c                   if        (§4ichmi>0 and §4ichmax=0) or
017400100407     c                             (§4ichmi=0 and §4ichmax>0)
017500100407     C                   SETON                                        4399
017600100407     C                   GOTO      FINVD2
017700100407     C                   END
017800100407     c* il massimo deve essere più grande del minimo
017900100407     c                   if        (§4ichmax>0 and §4ichmax<§4ichmi)
018000100407     C                   SETON                                        4499
018100100407     C                   GOTO      FINVD2
018200100407     C                   END
018300100118     C*
018400931209     C*  Rirpristino
018500940228     C     *INKE         IFEQ      '1'
018600931209     C                   EXSR      RIPRIS
018700931209     C                   GOTO      FINVD2
018800931209     C                   END
018900100118     C*
019000931209     C*  Conferma
019100940228     C     *INKF         IFEQ      '1'
019200931209     C                   EXSR      AGGIOR
019300100118     C*
019400931209     C                   END
019500100118     C*
019600931209     C     FINVD2        ENDSR
019700931209     C*--------------------------------------------------------------*
019800931209     C*  INZVD2: Inizializzazione seconda videata                    *
019900931209     C*--------------------------------------------------------------*
020000931209     C     INZVD2        BEGSR
020100100118     C*
020200940228     C*  Imposto parametri a video da DS 4I
020300940228     C                   MOVEL     §4IDES        VDES4I
020400940228     C                   MOVEL     §4ICHR        VCHR4I
020500931209     C                   MOVEL     '2'           TIPVID
020600100118     C*
020700931209     C                   ENDSR
020800931209     C*--------------------------------------------------------------*
020900100118     C*  AGGIOR: Aggiornamento parametri da video a file              *
021000931209     C*--------------------------------------------------------------*
021100931209     C     AGGIOR        BEGSR
021200100118     C*
021300940228     C                   MOVEL     VDES4I        §4IDES
021400940228     C                   MOVEL     VCHR4I        §4ICHR
021500100118     C*
021600100118     C*  Eseguo aggiornamento
021700940228     C                   MOVEL     DS4I          TBLUNI
021800940301     C                   MOVEL     *BLANKS       TBLFTR
021900940301     C                   Z-ADD     0             TBLDTR
022000931209     C     *IN80         IFEQ      '0'
022100931209     C                   UPDATE    TABEL
022200931209     C                   ELSE
022300940301     C                   MOVEL     '1'           TBLFTT
022400931209     C                   Z-ADD     1             TBLKUT
022500940228     C                   MOVEL     '4I'          TBLCOD
022600931209     C                   MOVEL     *BLANKS       TBLKEY
022700940228     C                   MOVEL     VCOD4I        TBLKEY
022800931209     C                   WRITE     TABEL
022900931209     C                   END
023000100118     C*
023100931209     C*  Ritorno alla prima videata
023200931209     C                   MOVEL     '1'           TIPVID
023300940228     C                   MOVEL     ' ?'          VCOD4I
023400100118     C*
023500931209     C                   ENDSR
023600931209     C*--------------------------------------------------------------*
023700931209     C*  ANNUL: Annullamento                                         *
023800931209     C*--------------------------------------------------------------*
023900931209     C     ANNUL         BEGSR
024000100118     C*
024100931209     C     *IN80         IFEQ      '0'
024200931209     C                   MOVEL     '*'           TBLFLG
024300931209     C                   EXCEPT    ANNTBL
024400931209     C                   END
024500100118     C*
024600931209     C*  Ritorno alla prima videata
024700931209     C                   MOVEL     '1'           TIPVID
024800940228     C                   MOVEL     ' ?'          VCOD4I
024900100118     C*
025000931209     C                   ENDSR
025100931209     C*--------------------------------------------------------------*
025200931209     C*  RIPRI: Ripristino                                           *
025300931209     C*--------------------------------------------------------------*
025400931209     C     RIPRIS        BEGSR
025500100118     C*
025600931209     C     *IN80         IFEQ      '0'
025700931209     C                   MOVEL     ' '           TBLFLG
025800931209     C                   EXCEPT    ANNTBL
025900931209     C                   END
026000100118     C*
026100931209     C*  Ritorno alla prima videata
026200931209     C                   MOVEL     '1'           TIPVID
026300940228     C                   MOVEL     ' ?'          VCOD4I
026400100118     C*
026500931209     C                   ENDSR
026600100118     C*--------------------------------------------------------------*
026700100118     C*  controllo codice iso nazione europea con relativa nazione   *
026800100118     C*--------------------------------------------------------------*
026900100118     C     CtrNAZ        BEGSR
027000100118     c     ktab2         setll     tabel00f
027100100118     c     ktab2         reade(n)  tabel00f
027200100118     c
027300100118     c                   dow       not %eof(tabel00f)
027400100118     c                   if        tblflg=' '
027500100118     c                   movel     tbluni        ds15
027600100118     c
027700100118     c                   if        §15iso=vcod4i
027800100118     c* Se i 2 codici sono incongruenti, errore forzabile
027900100118     c                   if        §15ue<>§4iue
028000100118     c                   if        §4iue='S'
028100100118     c                   seton                                        4199
028200100118     c                   else
028300100118     c                   seton                                        4299
028400100118     c                   endif
028500100118     c
028600100118     c                   eval      ForzaUE='S'
028700100118     c                   endif
028800100118
028900100118     c* Se ho trovato la nazione che mi interessa --> esco
029000100118     c                   leave
029100100118     c                   endif
029200100118     c
029300100118     c                   endif
029400100118     c     ktab2         reade(n)  tabel00f
029500100118     c                   enddo
029600100118     c
029700100118     c                   if        not *in80
029800100118     c     keytab        chain     tabel00f
029900100118     c                   endif
030000100118     c
030100100118     C                   ENDSR
030200931209     C*--------------------------------------------------------------*
030300931209     C*  *INZSR: Operazioni iniziali                                 *
030400931209     C*--------------------------------------------------------------*
030500931209     C     *INZSR        BEGSR
030600931209     C*
030700931209     C     *ENTRY        PLIST
030800931209     C                   PARM                    KPJBA
030900100118     C*
031000931209     C*  Richiamo XPARUT
031100931209     C                   Z-ADD     1             CODUT
031200931209     C                   CALL      'XPARUT'
031300931209     C                   PARM                    UTEDSE
031400931209     C                   MOVEL     RAGUT         RSUT
031500100118     C*
031600931209     C* Definizione chiavi                                 *
031700931209     C     KEYTAB        KLIST
031800931209     C                   KFLD                    TBWKUT
031900931209     C                   KFLD                    TBWCOD
032000931209     C                   KFLD                    TBWKEY
032100100118     C     Ktab2         KLIST
032200100118     C                   KFLD                    TBWKUT
032300100118     C                   KFLD                    kcod
032400100118     C*
032500931209     C* Definizione variabili                              *
032600931209     C     *LIKE         DEFINE    TBLKUT        TBWKUT
032700931209     C     *LIKE         DEFINE    TBLCOD        TBWCOD
032800931209     C     *LIKE         DEFINE    TBLKEY        TBWKEY
032900971210     C******************************************************
033000971210     C* Richiamo pgm per controllo modificabilità tabella
033100081028     C***                CLEAR                   TRUL12
033200081028     C***                MOVEL     'L'           I12TLA
033300081028     C***                MOVEL     KNSIF         I12NSI
033400081028     C***                MOVEL     '4I'          I12COD
033500081028     C***                CALL      'TRUL12R'
033600081028     C***                PARM                    TRUL12
033700081028     C**+  O12ERR        IFEQ      '1'
033800081028     C**+                GOTO      FINE
033900081028     C***                END
034000971210     C******************************************************
034100931209     C*  Inizializzazione variabili
034200931209     C                   MOVEL     'N'           £FINE             1
034300931209     C                   MOVEL     '1'           TIPVID            1
034400931209     C                   Z-ADD     1             TBWKUT
034500940228     C                   MOVE      '4I'          TBWCOD
034600100118     C*
034700940228     C                   WRITE     TB52D01
034800100118     C*
034900931209     C                   ENDSR
035000931209     O*-----------------------------------------------------*
035100931209     OTABEL     E            ANNTBL
035200931209     O                       TBLFLG
