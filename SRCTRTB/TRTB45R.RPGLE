000100000000     H DATEDIT(*YMD)
000200000000      *****************************************************************
000300000000      *
000400000000      *  Nome programma:  TRTB45R
000500000000      *  Descrizione   :  Gestione tabella 8B: Causali eventi da
000600000000      *                   tassare
000700000000      *  Autore        :  STEFANO (SC)
000800000000      *  Data creazione:  05 NOV 1993
000900000000      *
001000000000      *****************************************************************
001100000000     FTRTB45D   CF   E             WORKSTN
001200000000     F                                     SFILE(TB45S1:NRR1)
001300000000     F                                     SFILE(TB45S2:NRR2)
001400000000     FAZORG01L  IF   E           K DISK
001500000000     FTABEL00F  UF A E           K DISK
001600000000      *
001700000000     D L1              S              3  0 DIM(30)
001800020507     D*FIL             S              3    DIM(29)
001900000000      *
002000000000     D KPJBA         E DS
002100000000     D DS2A          E DS
002200000000     D CNCR80        E DS
002201131008     D uteautds      E DS
002202131008     d tibs42ds      e ds
002203090605     d $Cmd            s                   like(Qcmd)
002204090605     d                                     dim( 1) ctdata perrcd(1)
002300020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
002400020507     D  LIN                    1     90  0 dim(30)
002500000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002501090605      * - Parametri per QCMDEXC
002502090605     d Qcmd            s             80    inz
002503090605     d Qlen            s             15  5 inz(80)
002600000000      *
002700000000     C     *ENTRY        PLIST
002800000000     C                   PARM                    KPJBA
002900000000      *
003000000000      *---------------------------------------------------------------*
003100000000      *                   M A I N      L I N E                        *
003200000000      *---------------------------------------------------------------*
003300000000      *
003400000000      * Prelevo i dati dell'utente e della filiale
003500000000     C                   Z-ADD     1             CODUT
003600000000     C                   CALL      'X§PARUT'
003700000000     C                   PARM                    UT§DSE
003800000000     C                   MOVEL     RAGUT         RSUT             20
003900000000     C                   MOVEL     REC80         CNCR80
004000000000      *
004100000000     C                   EXSR      DEFCAM
004200000000      *
004300000000     C     INIZIO        TAG
004400000000     C                   MOVE      '1'           $VIDEO            1
004500000000     C                   MOVE      *BLANK        TUTTIV
004600000000     C                   MOVE      *BLANK        MANCAV
004700000000     C                   Z-ADD     *ZERO         ORGFIV
004800000000      *
004900000000      * Pulisco il SFL
005000000000     C                   SETOFF                                       3435
005100000000     C                   WRITE     TB45C1
005200000000     C                   SETON                                        34
005300000000      *
005400000000      * Riempo il SFL
005500000000     C                   EXSR      RIEMPI
005600000000      *
005700000000      * Emetto il primo formato video
005800000000     C                   SETON                                        91
005900000000     C     EMETTO        TAG
006000000000     C   91              WRITE     TB45D1
006100000000     C  N35
006200000000     CAN 91              WRITE     TB45D3
006300000000     C                   EXFMT     TB45C1
006400000000      *
006500000000      * Azzero gli indicatori di errore
006600000000     C                   SETOFF                                       404142
006700000000     C                   SETOFF                                         4445
006800000000     C                   SETOFF                                       464749
006900000000     C                   SETOFF                                         9091
007000000000      *
007100000000     C   KC              GOTO      FINE
007200000000     C   KE              GOTO      INIZIO
007300000000      *
007400000000      * Eseguo i controlli per il formato video
007500000000     C                   EXSR      CONTR
007600000000      *
007700000000      * Se tutto e' ok ed e' stato premuto F6 di conferma, registro
007800000000     C   KF
007900000000     CANN90MANCAV        IFNE      *BLANKS
008000000000     C                   EXSR      REGIS
008100000000     C                   ENDIF
008200000000      *
008300000000      * Se richiesta visualizz. altre mancate consegne emetto l'altro formato
008400000000     C     $VIDEO        IFEQ      '2'
008500000000     C                   EXSR      SECOND
008600000000     C   KC              GOTO      FINE
008700000000     C                   GOTO      INIZIO
008800000000     C                   ENDIF
008900000000      *
009000000000      * Se non vi sono errori e non e' stato premuto F6 riemetto il formato
009100000000     C  N90
009200000000     CANNKFMANCAV        IFNE      *BLANKS
009300000000     C                   SETON                                        40  90
009400000000     C                   ENDIF
009500000000      *
009600000000      * Per errore generico riemetto il formato
009700000000     C                   GOTO      EMETTO
009800000000      *
009900000000     C     FINE          TAG
010000000000     C                   SETON                                        LR
010100000000      *---------------------------------------------------------------*
010200000000      * Definizioni/dichiarazioni iniziali                            *
010300000000      *---------------------------------------------------------------*
010400000000     C     DEFCAM        BEGSR
010500000000      *
010600000000     C     TABE1K        KLIST
010700000000     C                   KFLD                    TBLKUT
010800000000     C                   KFLD                    TBLCOD
010900000000     C     TABELK        KLIST
011000000000     C                   KFLD                    TBLKUT
011100000000     C                   KFLD                    TBLCOD
011200000000     C                   KFLD                    TBLKEY
011300000000      *
011400000000      * Carico la tabella £1 per sapere quali filiali posso utilizzare
011500020507     C                   CLEAR                   TR06DS
011600020507     C                   MOVEL     '£1'          D06COD
011700020507     C                   MOVEL     simfel        D06KEY
011800020507     C                   MOVEL     TR06DS        KPJBU
011900020507     C                   CALL      'TRUL06R'
012000020507     C                   PARM                    KPJBA
012100020507     C                   MOVEL     KPJBU         TR06DS
012200020507     C                   MOVEA     LIN           l1
012400000000     C                   Z-ADD     1             TBLKUT
014500000000      *
014600000000     C     *LIKE         DEFINE    NRR1          $NRR1
014700000000     C     *LIKE         DEFINE    NRR2          $NRR2
014800000000      *
014900000000     C                   ENDSR
015000000000      *---------------------------------------------------------------*
015100000000      * Riempi il SFL con la tabella 8B                               *
015200000000      *---------------------------------------------------------------*
015300000000     C     RIEMPI        BEGSR
015400000000      *
015500000000     C                   SETON                                        30
015600000000     C                   SETOFF                                       01
015700000000     C                   Z-ADD     *ZERO         NRR1
015800000000     C                   Z-ADD     *ZERO         $NRR1
015900000000      *
016000000000      * Leggo tutta la tabella 8B
016100000000     C                   Z-ADD     1             TBLKUT
016200000000     C                   MOVE      '8B'          TBLCOD
016300000000     C     TABE1K        SETLL     TABEL00F
016400000000     C                   DO        *HIVAL
016500000000     C     TABE1K        READE     TABEL00F                               31
016600000000     C  N31TBLFLG        IFEQ      *BLANK
016700000000     C                   SETOFF                                       3033
016800000000     C                   MOVE      TBLFLG        TBLFGV
016900000000     C                   MOVEL     TBLKEY        CDMANV
017000000000     C                   MOVE      TBLKEY        ORGF1V
017100000000      *
017200040504      * Carica solo quelle linee legate al profilo
017300040504     c                   z-add     1             l1f               3 0
017400040504     C     ORGF1V        lookup    L1(L1f)                                95
017500040504     c                   if        ORGF1V = *zero
017600040504     c                   seton                                        95
017700040504     c                   end
017800040504      *
017900040504     c                   if        *in95
018000040504     C  N35              SETON                                        35
018100040504      *
018200000000      * Decodifico la filiale
018300000000     C     ORGF1V        CHAIN     AZORG01L                           99
018400000000     C                   MOVE      *BLANKS       ORGD1V
018500000000     C  N99              MOVEL     ORGDES        ORGD1V
018600000000      *
018700000000     C                   ADD       1             NRR1
018800000000     C                   ADD       1             $NRR1
018900000000     C                   WRITE     TB45S1
019000040504      *
019100040504     c                   endIF
019200000000      *
019300000000     C                   ENDIF
019400000000     C  N31              ENDDO
019500000000      *
019600000000      * Per decodificare gli elementi del SFL sulla tabella "2A" devo rileggermi
019700000000      * tutto il SFL
019800000000     C                   Z-ADD     1             TBLKUT
019900000000     C                   MOVE      '2A'          TBLCOD
020000000000     C                   DO        $NRR1         $B                4 0
020100000000     C     $B            CHAIN     TB45S1                             31
020200000000     C  N31              DO
020300000000      *
020400000000      * Decodifico l'elemento 2A
020500000000     C                   MOVE      *BLANKS       TBLKEY
020600000000     C                   MOVEL     CDMANV        TBLKEY
020700000000     C     TABELK        CHAIN     TABEL00F                           99
020800000000     C                   MOVE      *BLANKS       §2ADEV
020900000000     C  N99              MOVEL     TBLUNI        DS2A
021000000000     C  N99              MOVEL     §2ADES        §2ADEV
021100000000     C  N99§2AUGP        COMP      'S'                                3333
021200000000     C                   UPDATE    TB45S1
021300000000      *
021400000000     C                   ENDDO
021500000000     C                   ENDDO
021600000000      *
021700000000     C                   Z-ADD     1             NRR1
021800000000      *
021900000000     C                   ENDSR
022000000000      *---------------------------------------------------------------*
022100000000      * Eseguo i  controlli sui campi emessi a video                  *
022200000000      *---------------------------------------------------------------*
022300000000     C     CONTR         BEGSR
022400000000      *
022500000000      * Per prima cosa verifico che non vi siano state scelte nel SFL
022600000000      * L'unica cosa che l'utente puo' fare e' l'annullamento; se un record
022700000000      * viene annullato deve essere cancellato fisicamente dall'archivio
022800000000     C   35              DO        *HIVAL        $B
022900000000     C     $B            CHAIN     TB45S1                             31
023000000000     C  N31TBLFGV        IFEQ      'A'
023100000000      *
023200000000      * Se non e' stato premuto F6 di conferma non posso fare nulla
023300000000     C  NKF
023400000000     CANN33              SETON                                        40  90
023500000000     C  NKF
023600000000     CANN33              GOTO      ENDCTL
023700000000      *
023800000000      * Proteggo il campo dell'annullamento e aggiorno il SFL
023900000000     C                   SETON                                        33
024000000000     C                   UPDATE    TB45S1
024100000000      *
024200000000      * Cancello fisicamente il record dall'archivio
024300000000     C                   Z-ADD     1             TBLKUT
024400000000     C                   MOVE      '8B'          TBLCOD
024500000000     C                   MOVE      *BLANKS       TBLKEY
024600000000     C                   MOVEL     CDMANV        TBLKEY
024700000000     C                   MOVE      ORGF1V        TBLKEY
024800000000     C     TABELK        CHAIN     TABEL00F                           99
024900000000     C  N99              DELETE    TABEL
025000000000      *
025100000000     C                   ENDIF
025200000000     C  N31              ENDDO
025300000000     C                   Z-ADD     1             NRR1
025400000000      *
025500000000      * Se immesso 'I' non bisogna immettere gli altri campi
025600000000     C     TUTTIV        IFEQ      'I'
025700000000     C     MANCAV        IFNE      *BLANKS
025800000000     C     ORGFIV        ORNE      *ZERO
025900000000     C                   SETON                                        41  90
026000000000     C                   GOTO      ENDCTL
026100000000     C                   ENDIF
026200000000     C                   ENDIF
026300000000      *
026400000000      * Se immesso 'I' emetto il formato con tutte le mancate consegne
026500000000     C     TUTTIV        IFEQ      'I'
026600000000     C                   MOVE      '2'           $VIDEO
026700000000     C                   GOTO      ENDCTL
026800000000     C                   ENDIF
026900000000      *
027000000000      * Verifico la filiale (nel SFLCTL)
027100000000     C                   MOVE      *BLANKS       ORGDEV
027200000000     C     ORGFIV        IFNE      *ZERO
027300130910     C*    ORGFIV        LOOKUP    L1                                     99
027301130910     c* controllo se utente abilitato alla gestione
027302131008     c                   clear                   tibs42ds
027303131008     c                   movel     orgfiv        i42pge
027305131008     c                   call      'TIBS42R'
027306131008     c                   parm                    tibs42ds
027307131008     c                   movel     o42uni        uteautds
027308130910     c                   if        §autmtc <>'S'
027309131008     C                   SETON                                        44  90
027310130910     C   44              GOTO      ENDCTL
027315130910     c                   endif
027600000000      *
027700000000      * Decodifico il codice filiale
027800000000     C     ORGFIV        CHAIN     AZORG01L                           44
027900000000     C  N44              MOVE      ORGDES        ORGDEV
028000000000     C   44              SETON                                        90
028100000000     C   44              GOTO      ENDCTL
028200000000     C                   ENDIF
028300000000      *
028400000000      * Verifico la mancata consegna (nel SFLCTL)
028500000000      * Funzione di '?'
028600000000     C                   MOVEL     MANCAV        $UNO              1
028700000000     C     $UNO          IFEQ      '?'
028800000000     C                   Z-ADD     1             §KUT
028900000000     C                   MOVE      '2A'          §COD
029000000000     C                   MOVEL     *BLANKS       §KEY
029100000000     C                   CALL      'X§TABER'                            99
029200000000     C                   PARM                    §KUT              1 0
029300000000     C                   PARM                    §COD              2
029400000000     C                   PARM                    §KEY              8
029500000000     C                   PARM                    §DES             30
029600000000     C                   MOVE      *BLANKS       MANCAV
029700000000     C  N99              MOVEL     §KEY          MANCAV
029800000000     C                   SETON                                        90  91
029900000000     C                   GOTO      ENDCTL
030000000000     C                   ENDIF
030100000000      *
030200000000      * Il codice mancata consegna deve esistere nella tabella 2A
030300000000     C                   MOVE      *BLANKS       DEMANV
030400000000     C     MANCAV        IFNE      *BLANKS
030500000000     C     ORGFIV        ORNE      *ZERO
030600000000     C                   Z-ADD     1             TBLKUT
030700000000     C                   MOVE      '2A'          TBLCOD
030800000000     C                   MOVE      *BLANKS       TBLKEY
030900000000     C                   MOVEL     MANCAV        TBLKEY
031000000000     C     TABELK        CHAIN     TABEL00F                           42
031100000000     C  N42TBLFLG        COMP      *BLANKS                            4242
031200000000     C  N42              MOVEL     TBLUNI        DS2A
031300000000     C  N42              MOVEL     §2ADES        DEMANV
031400000000     C  N42§2AUGP        COMP      'S'                                4242
031500000000     C   42              SETON                                        90
031600000000     C   42              GOTO      ENDCTL
031700000000      *
031800000000      * Se tento di immettere il codice con filiale generica (=0) devo essere
031900000000      * certo che nella tabella 8B non vi sia lo stesso codice con una filiale
032000000000     C     ORGFIV        IFEQ      *ZERO
032100000000     C                   Z-ADD     1             TBLKUT
032200000000     C                   MOVE      '8B'          TBLCOD
032300000000     C                   MOVE      *BLANKS       TBLKEY
032400000000     C                   MOVEL     MANCAV        TBLKEY
032500000000     C     TABELK        SETLL     TABEL00F
032600000000     C     TABE1K        READE     TABEL00F                               99
032700000000     C                   MOVEL     TBLKEY        $TRE              3
032800000000     C  N99$TRE          IFEQ      MANCAV
032900000000     C                   SETON                                        47  90
033000000000     C                   GOTO      ENDCTL
033100000000     C                   ENDIF
033200000000     C                   ENDIF
033300000000      *
033400000000      * Verifico che nella 8B non vi sia gia' il codice che si vuole inserire
033500000000     C                   Z-ADD     1             TBLKUT
033600000000     C                   MOVE      '8B'          TBLCOD
033700000000     C                   MOVE      *BLANKS       TBLKEY
033800000000     C                   MOVEL     MANCAV        TBLKEY
033900000000     C                   MOVE      ORGFIV        TBLKEY
034000000000     C     TABELK        CHAIN     TABEL00F                           99
034100000000     C  N99              SETON                                        45  90
034200000000     C  N99              GOTO      ENDCTL
034300000000      *
034400000000      * Verifico che nella 8B non vi sia gia' il codice con filiale generico (0)
034500000000     C                   MOVE      *BLANKS       TBLKEY
034600000000     C                   MOVEL     MANCAV        TBLKEY
034700000000     C                   MOVE      '000'         TBLKEY
034800000000     C     TABELK        CHAIN     TABEL00F                           99
034900000000     C  N99              SETON                                        46  90
035000000000     C  N99              GOTO      ENDCTL
035100000000      *
035200000000     C                   ENDIF
035300000000      *
035400000000     C     ENDCTL        ENDSR
035500000000      *---------------------------------------------------------------*
035600000000      * Registrazione record nel SFL e nell'archivio tabelle          *
035700000000      *---------------------------------------------------------------*
035800000000     C     REGIS         BEGSR
035900000000      *
036000000000      * Scrivo nel SFL
036100000000     C                   MOVE      *BLANK        TBLFGV
036200000000     C                   MOVE      MANCAV        CDMANV
036300000000     C                   MOVEL     DEMANV        §2ADEV
036400000000     C                   Z-ADD     ORGFIV        ORGF1V
036500000000     C                   MOVEL     ORGDEV        ORGD1V
036600000000     C                   ADD       1             $NRR1
036700000000     C                   Z-ADD     $NRR1         NRR1
036800000000     C                   SETOFF                                       333041
036900000000     C                   SETOFF                                       4347
037000000000     C                   WRITE     TB45S1
037100000000     C  N35              SETON                                        35
037200000000      *
037300000000      * Scrivo nell'archivio
037400000000     C                   Z-ADD     1             TBLKUT
037500000000     C                   MOVE      '8B'          TBLCOD
037600000000     C                   MOVE      *BLANKS       TBLKEY
037700000000     C                   MOVEL     MANCAV        TBLKEY
037800000000     C                   MOVE      ORGFIV        TBLKEY
037900000000     C                   MOVE      *BLANKS       TBLUNI
038000000000     C                   MOVE      *BLANK        TBLFTT
038100000000     C                   Z-ADD     *ZERO         TBLFLT
038200000000     C                   MOVE      *BLANK        TBLFTR
038300000000     C                   Z-ADD     *ZERO         TBLDTR
038400000000     C                   WRITE     TABEL
038500000000      *
038600000000      * Pulisco i campi a video
038700000000     C                   MOVE      *BLANK        TUTTIV
038800000000     C                   MOVE      *BLANKS       MANCAV
038900000000     C                   MOVE      *ZERO         ORGFIV
039000000000     C                   MOVE      *BLANKS       ORGDEV
039100000000     C                   MOVE      *BLANKS       DEMANV
039200000000      *
039300000000     C                   ENDSR
039400000000      *---------------------------------------------------------------*
039500000000      * Gestione del secondo formato video                            *
039600000000      *---------------------------------------------------------------*
039700000000     C     SECOND        BEGSR
039800000000      *
039900000000      * Pulisco il SFL
040000000000     C                   SETOFF                                       3435
040100000000     C                   WRITE     TB45C2
040200000000     C                   SETON                                        34
040300000000     C                   Z-ADD     *ZERO         NRR2
040400000000      *
040500000000      * Leggo tutta la tabella 8B
040600000000     C                   Z-ADD     1             TBLKUT
040700000000     C                   MOVE      '2A'          TBLCOD
040800000000     C     TABE1K        SETLL     TABEL00F
040900000000     C                   DO        *HIVAL
041000000000     C     TABE1K        READE     TABEL00F                               31
041100000000     C  N31              MOVEL     TBLUNI        DS2A
041200000000     C  N31TBLFLG        IFEQ      *BLANK
041300000000     C     §2AUGP        ANDEQ     'S'
041400000000      *
041500000000      * Verifico che l'elemento in esame non sia gia' presente nel SFL di prima
041600000000     C                   MOVEL     TBLKEY        CDMANW
041700000000     C                   EXSR      CERCA
041800000000      *
041900000000     C     $SINO         IFEQ      *ZERO
042000000000     C  N35              SETON                                        35
042100000000     C                   MOVE      *BLANK        SCEL2V
042200000000     C                   MOVEL     TBLUNI        §2ADEW
042300000000     C                   Z-ADD     *ZERO         ORGFIW
042400000000     C                   ADD       1             NRR2
042500000000     C                   SETOFF                                       414347
042600000000     C                   WRITE     TB45S2
042700000000     C                   ENDIF
042800000000     C                   ENDIF
042900000000     C  N31              ENDDO
043000000000      *
043100000000      *
043200000000     C                   Z-ADD     1             NRR2
043300000000     C                   SETOFF                                       01
043400000000     C                   SETON                                        91
043500000000     C     EMETT2        TAG
043600000000     C   91              WRITE     TB45D2
043700000000     C  N35
043800000000     CAN 91              WRITE     TB45D3
043900000000     C                   EXFMT     TB45C2
044000000000      *
044100000000     C                   SETOFF                                       404244
044200000000     C                   SETOFF                                       454648
044300000000     C                   SETOFF                                       909102
044400000000      *
044500000000     C   KL
044600000000     COR KC              GOTO      ENDSEC
044700000000      *
044800000000      * Eseguo i  controlli sul formato video
044900000000     C                   EXSR      CONTR2
045000000000      *
045100000000      * Se premuto F6 ma nessuna scelta fatta riemetto il formato
045200000000     C  N90
045300000000     CANNKF
045400000000     CAN 02              SETON                                        40
045500000000     C  N90
045600000000     CANNKF              SETON                                        90
045700000000      *
045800000000      * Per errore generico riemetto il formato video
045900000000     C   90              GOTO      EMETT2
046000000000      *
046100000000      * Se tutto e' ok aggiorno l'archivio
046200000000     C   35              EXSR      REGIS2
046300000000      *
046400000000      * Imposto l'indicatore generico di errore per riemettere il primo formato
046500000000     C                   SETON                                        90
046600000000     C                   MOVE      '1'           $VIDEO
046700000000      *
046800000000     C     ENDSEC        ENDSR
046900000000      *---------------------------------------------------------------*
047000000000      * Verifica dei campi delsecondo formato video                   *
047100000000      *---------------------------------------------------------------*
047200000000     C     CONTR2        BEGSR
047300000000      *
047400000000     C                   Z-ADD     *ZERO         $B
047500000000     C                   Z-ADD     *ZERO         $NRR2
047600000000      *
047700000000      * Leggo gli elementi variati del SFl
047800000000     C   35              DO        *HIVAL        $NRR2
047900000000     C     $NRR2         CHAIN     TB45S2                             31
048000000000     C  N31              SETOFF                                       414347
048100000000     C  N31SCEL2V        IFNE      *BLANK
048200000000     C     ORGFIW        ORNE      *ZERO
048300000000      *
048400000000      * Se non selezionata la riga ma immessa la filiale e' errore
048500000000     C     SCEL2V        IFEQ      *BLANK
048600000000     C     ORGFIW        ANDNE     *ZERO
048700000000     C                   SETON                                        414290
048800000000     C                   Z-ADD     $NRR1         $B
048900000000     C                   UPDATE    TB45S2
049000000000     C                   GOTO      ENDCT2
049100000000     C                   ENDIF
049200000000      *
049300000000     C     SCEL2V        IFEQ      '1'
049400000000     C                   SETON                                        02
049500000000      *
049600000000      * La filiale deve esistere nella schiera
049700130910     C*    ORGFIW        LOOKUP    L1                                     99
049701130910     c* controllo se utente abilitato alla gestione
049702131008     c                   clear                   tibs42ds
049703131008     c                   movel     orgfiv        i42pge
049705131008     c                   call      'TIBS42R'
049706131008     c                   parm                    tibs42ds
049707131008     c                   movel     o42uni        uteautds
049708130910     c                   if        §autmtc <>'S'
049709130910     C* N99              SETON                                        474890
049710130910     C                   SETON                                        474890
049711130910     C   47              UPDATE    TB45S2
049712130910     C   47              Z-ADD     $NRR2         $B
049713130910     C   47              GOTO      ENDCT2
049714130910     c                   endif
050200000000      *
050300000000      * Se tento di immettere il codice con filiale generica (=0) devo essere
050400000000      * certo che nella tabella 8B non vi sia lo stesso codice con una filiale
050500000000     C     ORGFIW        IFEQ      *ZERO
050600000000     C                   Z-ADD     1             TBLKUT
050700000000     C                   MOVE      '8B'          TBLCOD
050800000000     C                   MOVE      *BLANKS       TBLKEY
050900000000     C                   MOVEL     CDMANW        TBLKEY
051000000000     C     TABELK        SETLL     TABEL00F
051100000000     C     TABE1K        READE     TABEL00F                               99
051200000000     C                   MOVEL     TBLKEY        $TRE
051300000000     C  N99$TRE          IFEQ      CDMANW
051400000000     C                   SETON                                        434590
051500000000     C                   Z-ADD     $NRR2         $B
051600000000     C                   UPDATE    TB45S2
051700000000     C                   GOTO      ENDCT2
051800000000     C                   ENDIF
051900000000     C                   ENDIF
052000000000      *
052100000000      * Verifico che nella 8B non vi sia il cod.manc.consegna selezionato
052200000000     C                   Z-ADD     1             TBLKUT
052300000000     C                   MOVE      '8B'          TBLCOD
052400000000     C                   MOVEL     *BLANKS       TBLKEY
052500000000     C                   MOVEL     CDMANW        TBLKEY
052600000000     C                   MOVE      ORGFIW        TBLKEY
052700000000     C     TABELK        CHAIN     TABEL00F                           99
052800000000     C  N99              SETON                                        434490
052900000000     C   44              Z-ADD     $NRR2         $B
053000000000     C   44              UPDATE    TB45S2
053100000000     C   44              GOTO      ENDCT2
053200000000      *
053300000000      * Verifico che nella 8B non vi sia il cod.manc.consegna con filiale 0
053400000000     C                   MOVEL     *BLANKS       TBLKEY
053500000000     C                   MOVEL     CDMANW        TBLKEY
053600000000     C                   MOVE      '000'         TBLKEY
053700000000     C     TABELK        CHAIN     TABEL00F                           99
053800000000     C  N99              SETON                                        434690
053900000000     C   46              Z-ADD     $NRR2         $B
054000000000     C   46              UPDATE    TB45S2
054100000000     C   46              GOTO      ENDCT2
054200000000      *
054300000000     C                   ENDIF
054400000000      *
054500000000     C                   ENDIF
054600000000     C  N31              UPDATE    TB45S2
054700000000     C  N31              ENDDO
054800000000      *
054900000000     C     ENDCT2        TAG
055000000000     C     $B            IFEQ      *ZERO
055100000000     C                   Z-ADD     1             NRR2
055200000000     C                   ELSE
055300000000     C                   Z-ADD     $B            NRR2
055400000000     C                   ENDIF
055500000000      *
055600000000     C                   ENDSR
055700000000      *---------------------------------------------------------------*
055800000000      * Aggiorno la tabella 8B con le scelte fatte nel SFL            *
055900000000      *---------------------------------------------------------------*
056000000000     C     REGIS2        BEGSR
056100000000      *
056200000000     C                   DO        *HIVAL        $B
056300000000     C     $B            CHAIN     TB45S2                             31
056400000000     C  N31SCEL2V        IFEQ      '1'
056500000000      *
056600000000     C                   Z-ADD     1             TBLKUT
056700000000     C                   MOVE      '8B'          TBLCOD
056800000000     C                   MOVE      *BLANKS       TBLKEY
056900000000     C                   MOVEL     CDMANW        TBLKEY
057000000000     C                   MOVE      ORGFIW        TBLKEY
057100000000     C                   MOVE      *BLANKS       TBLUNI
057200000000     C                   MOVE      *BLANK        TBLFTT
057300000000     C                   Z-ADD     *ZERO         TBLFLT
057400000000     C                   MOVE      *BLANK        TBLFTR
057500000000     C                   Z-ADD     *ZERO         TBLDTR
057600000000     C                   WRITE     TABEL
057700000000      *
057800000000     C                   ENDIF
057900000000     C  N31              ENDDO
058000000000      *
058100000000     C                   ENDSR
058200000000      *---------------------------------------------------------------*
058300000000      * Ricerco nel primo SFL l'elemento che leggo dall'archivio      *
058400000000      *---------------------------------------------------------------*
058500000000     C     CERCA         BEGSR
058600000000      *
058700000000     C                   Z-ADD     *ZERO         $SINO             1 0
058800000000      *
058900000000      * Proseguo solo se il SFL di prima e' pieno
059000000000     C     $NRR1         IFGT      *ZERO
059100000000      *
059200000000      * Escludo eventuali elementi gia' presenti nel primo SFL ma solo se il
059300000000      * cd.filiale e' 0
059400000000     C                   DO        *HIVAL        $NRR2
059500000000     C     $NRR2         CHAIN     TB45S1                             99
059600000000     C  N99TBLFGV        IFEQ      *BLANKS
059700000000     C     CDMANV        IFEQ      CDMANW
059800000000     C     ORGF1V        ANDEQ     *ZERO
059900000000     C                   Z-ADD     1             $SINO
060000000000     C                   SETON                                        99
060100000000     C                   ENDIF
060200000000     C                   ENDIF
060300000000     C  N99              ENDDO
060400000000      *
060500000000     C                   ENDIF
060600000000      *
060700000000     C                   ENDSR
060800090605** msg
060900090605SNDBRKMSG MSG('UTENTE NON AUTORIZZATO') TOMSGQ(          )
