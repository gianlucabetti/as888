000100000000     H DATEDIT(*YMD)
000200000000      *****************************************************************
000300000000      *
000400000000      *  Nome programma:  TRTB46R
000500000000      *  Descrizione   :  Gestione tabella 8C: Codici bolla da non
000600000000      *                   tassare per i padroncini
000700000000      *  Autore        :  STEFANO (SC)
000800000000      *  Data creazione:  05 NOV 1993
000900000000      *
001000000000      *****************************************************************
001100000000     FTRTB46D   CF   E             WORKSTN
001200000000     F                                     SFILE(TB46S1:NRR1)
001300000000     F                                     SFILE(TB46S2:NRR2)
001400000000     FAZORG01L  IF   E           K DISK
001500000000     FTABEL00F  UF A E           K DISK
001600000000      *
001700000000     D L1              S              3  0 DIM(30)
001800020507     D*FIL             S              3    DIM(29)
001900000000      *
002000000000     D DS3A          E DS
002200000000     D DS1Z          E DS
002300000000     D KPJBA         E DS
002400131008     d tibs42ds      e ds
002401131008     D uteautds      E DS
002500090605     d $Cmd            s                   like(Qcmd)
002600090605     d                                     dim( 1) ctdata perrcd(1)
002700020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
002800020507     D  LIN                    1     90  0 dim(30)
002900000000     D CNCR80        E DS
003000000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003100090605      * - Parametri per QCMDEXC
003200090605     d Qcmd            s             80    inz
003300090605     d Qlen            s             15  5 inz(80)
003400000000      * DS per suddivisione campo codice per la tabella 8C
003500000000     D                 DS
003600000000     D  $8CSER                 1      1
003700020725     D  $8CBOL                 2      3
003800020725     D  $8CDUE                 1      3
003900000000     D  $8CFIL                 6      8  0
004000000000     D  $DS8C                  1      8
004100000000      *
004200000000     C     *ENTRY        PLIST
004300000000     C                   PARM                    KPJBA
004400000000      *
004500000000     C     TABE1K        KLIST
004600000000     C                   KFLD                    TBLKUT
004700000000     C                   KFLD                    TBLCOD
004800000000     C     TABELK        KLIST
004900000000     C                   KFLD                    TBLKUT
005000000000     C                   KFLD                    TBLCOD
005100000000     C                   KFLD                    TBLKEY
005200000000      *---------------------------------------------------------------*
005300000000      *                   M A I N      L I N E                        *
005400000000      *---------------------------------------------------------------*
005500000000      *
005600100608     c                   clear                   Utente_EDP        1
005700100608      *
005800000000      * Prelevo i dati dell'utente e della filiale
005900000000     C                   Z-ADD     1             CODUT
006000000000     C                   CALL      'X§PARUT'
006100000000     C                   PARM                    UT§DSE
006200000000     C                   MOVEL     RAGUT         RSUT             20
006300000000     C                   MOVEL     REC80         CNCR80
007900100608      *
008000100608      *  Per controllare se utente EDP
008100100608     c                   if        %subst(knmus:1:3) = 'EDP'
008200100608     c                   movel     'S'           Utente_EDP
008300100608     c                   endif
008400000000      *
008500000000     C                   EXSR      DEFCAM
008600000000      *
008700000000     C     INIZIO        TAG
008800000000     C                   MOVE      '1'           $VIDEO            1
008900000000     C                   MOVE      *BLANK        TUTTIV
009000000000     C                   MOVE      *BLANK        CDBOLV
009100000000     C                   MOVE      *BLANK        TPSERV
009200000000     C                   Z-ADD     *ZERO         ORGFIV
009300000000     C                   MOVE      *BLANK        §3ADEV
009400000000     C                   MOVE      *BLANK        §1ZDEV
009500000000     C                   MOVE      *BLANK        ORGDEV
009600000000      *
009700000000      * Pulisco il SFL
009800000000     C                   SETOFF                                       3435
009900000000     C                   WRITE     TB46C1
010000000000     C                   SETON                                        34
010100000000      *
010200000000      * Riempo il SFL
010300000000     C                   EXSR      RIEMPI
010400000000      *
010500000000      * Emetto il primo formato video
010600000000     C                   SETON                                        91
010700000000     C     EMETTO        TAG
010800000000     C   91              WRITE     TB46D1
010900000000     C  N35
011000000000     CAN 91              WRITE     TB46D3
011100000000     C                   EXFMT     TB46C1
011200000000      *
011300000000      * Azzero gli indicatori di errore
011400000000     C                   SETOFF                                       404142
011500000000     C                   SETOFF                                       434445
011600000000     C                   SETOFF                                       4647
011700000000     C                   SETOFF                                       029091
011800000000      *
011900000000     C   KC              GOTO      FINE
012000000000     C   KE              GOTO      INIZIO
012100000000      *
012200000000      * Eseguo i controlli per il formato video
012300000000     C                   EXSR      CONTR
012400000000      *
012500000000      * Se tutto e' ok ed e' stato premuto F6 di conferma, registro
012600000000     C   KF
012700000000     CANN90CDBOLV        IFNE      *BLANKS
012800000000     C     TPSERV        ANDNE     *BLANKS
012900000000     C                   EXSR      REGIS
013000000000     C                   ENDIF
013100000000      *
013200000000      * Se richiesta visualizz. altre mancate consegne emetto l'altro formato
013300000000     C     $VIDEO        IFEQ      '2'
013400000000     C                   EXSR      SECOND
013500000000     C   KC              GOTO      FINE
013600000000     C                   GOTO      INIZIO
013700000000     C                   ENDIF
013800000000      *
013900000000      * Se non vi sono errori e non e' stato premuto F6 riemetto il formato
014000000000     C  N90
014100000000     CANNKFCDBOLV        IFNE      *BLANK
014200000000     C     *IN02         OREQ      *ON
014300000000     C                   SETON                                        40  90
014400000000     C                   ENDIF
014500000000      *
014600000000      * Per errore generico riemetto il formato
014700000000     C                   GOTO      EMETTO
014800000000      *
014900000000     C     FINE          TAG
015000000000     C                   SETON                                        LR
015100000000      *---------------------------------------------------------------*
015200000000      * Definizioni/dichiarazioni iniziali                            *
015300000000      *---------------------------------------------------------------*
015400000000     C     DEFCAM        BEGSR
015500000000      *
015600000000      * Carico la tabella £1 per sapere quali filiali posso utilizzare
015700020507     C                   CLEAR                   TR06DS
015800020507     C                   MOVEL     '£1'          D06COD
015900020507     C                   MOVEL     simfel        D06KEY
016000020507     C                   MOVEL     TR06DS        KPJBU
016100020507     C                   CALL      'TRUL06R'
016200020507     C                   PARM                    KPJBA
016300020507     C                   MOVEL     KPJBU         TR06DS
016400020507     C                   MOVEA     LIN           l1
016500000000     C                   Z-ADD     1             TBLKUT
016600020507     C*                  MOVE      '£1'          TBLCOD
016700020507     C*                  MOVE      *BLANKS       TBLKEY
016800020507     C*                  MOVEL     SIMFEL        TBLKEY
016900020507     C*                  Z-ADD     SIMFEL        L1(1)
017000020507     C*                  Z-ADD     2             $B
017100000000      *
017200020507     C*    TABELK        CHAIN     TABEL00F                           99
017300020507     C* N99TBLFLG        IFEQ      *BLANKS
017400020507     C*                  MOVEA     TBLUNI        FIL
017500000000      *
017600020507     C*                  DO        29            $A                2 0
017700020507     C*    FIL($A)       IFNE      *ZEROS
017800020507     C*    FIL($A)       ANDNE     *BLANKS
017900020507     C*                  MOVEL     FIL($A)       WNUM3             3 0
018000020507     C*    WNUM3         LOOKUP    L1                                     99
018100020507     C* N99              ADD       1             $B
018200020507     C* N99              Z-ADD     WNUM3         L1($B)
018300020507     C*                  END
018400020507     C*                  END
018500020507     C*                  END
018600000000      *
018700000000     C     *LIKE         DEFINE    NRR1          $NRR1
018800000000     C     *LIKE         DEFINE    NRR2          $NRR2
018900000000      *
019000000000     C                   ENDSR
019100000000      *---------------------------------------------------------------*
019200000000      * Riempi il SFL con la tabella 8B                               *
019300000000      *---------------------------------------------------------------*
019400000000     C     RIEMPI        BEGSR
019500000000      *
019600100607      *  i codici decisi da sede non devono essere toccati dalla filiale
019700100607      *    sono quelli che valgono per tutta Italia che hanno sulla destra
019800100607      *    lo "000" al posto della filiale.
019900100607      *
020000000000     C                   SETON                                        30
020100000000     C                   SETOFF                                       01
020200000000     C                   Z-ADD     *ZERO         NRR1
020300000000     C                   Z-ADD     *ZERO         $NRR1
020400000000      *
020500000000      * Leggo tutta la tabella 8C
020600000000     C                   Z-ADD     1             TBLKUT
020700000000     C                   MOVE      '8C'          TBLCOD
020800000000     C     TABE1K        SETLL     TABEL00F
020900000000     C                   DO        *HIVAL
021000000000     C     TABE1K        READE     TABEL00F                               31
021100100607      *
021200100607     c                   move      *blank        da_sede           1
021300100607      * sono quelli per tutta Italia
021400100607     c  n31              if        %subst(TBLKEY:6:3)='000'
021500100607     c                   move      'S'           da_sede
021600100607     c                   end
021700100607      *
021800000000     C  N31TBLFLG        IFEQ      *BLANK
021900000000     C                   SETOFF                                       3033
022000000000     C                   MOVE      TBLFLG        TBLFGV
022100000000     C                   MOVE      TBLKEY        $DS8C
022200000000     C                   MOVE      $8CSER        TPSE1V
022300000000     C                   MOVE      $8CBOL        CDBO1V
022400000000     C                   MOVE      $8CFIL        ORGF1V
022500040504      *
022600040504      * Carica solo quelle linee legate al profilo
022700040504     c                   z-add     1             l1f               3 0
022800040504     C     ORGF1V        lookup    L1(L1f)                                95
022900040504     c                   if        ORGF1V = *zero
023000040504     c                   seton                                        95
023100040504     c                   end
023200040504      *
023300040504     c                   if        *in95
023400040504     C  N35              SETON                                        35
023500000000      *
023600000000      * Decodifico la filiale
023700000000     C     ORGF1V        CHAIN     AZORG01L                           99
023800000000     C                   MOVE      *BLANKS       ORGD1V
023900000000     C  N99              MOVEL     ORGDES        ORGD1V
024000000000      *
024100100607      * Se da Sede deve proteggere
024200100607     c                   move      '0'           hin33
024300100608     c                   If        da_sede = 'S'
024400100608     c                             and Utente_EDP = 'S'
024500100607     c                   SETON                                        33
024600100607     c                   move      '1'           hin33
024700100607     c                   end
024800100607      *
024900000000     C                   ADD       1             NRR1
025000000000     C                   ADD       1             $NRR1
025100000000     C                   WRITE     TB46S1
025200100607      *
025300100607      *  quindi resetta la situazione da testare al prossimo giro
025400100607     c                   move      '0'           hin33
025500100607     c                   SETOFF                                       33
025600000000      *
025700040504     c                   endIF
025800040504      *
025900000000     C                   ENDIF
026000000000     C  N31              ENDDO
026100000000      *
026200000000      * Per decodificare gli elementi del SFL sulla tabella "2A" devo rileggermi
026300000000      * tutto il SFL
026400000000     C                   Z-ADD     1             TBLKUT
026500000000     C                   DO        $NRR1         $B                4 0
026600000000     C     $B            CHAIN     TB46S1                             31
026700000000     C  N31              DO
026800000000     C                   SETOFF                                       33
026900000000      *
027000000000      * Decodifico l'elemento 3A
027100000000     C                   MOVE      '3A'          TBLCOD
027200000000     C                   MOVE      *BLANKS       TBLKEY
027300000000     C                   MOVEL     CDBO1V        TBLKEY
027400000000     C     TABELK        CHAIN     TABEL00F                           99
027500000000     C                   MOVE      *BLANKS       §3AD1V
027600000000     C  N99              MOVEL     TBLUNI        DS3A
027700000000     C  N99              MOVEL     §3ADES        §3AD1V
027800000000     C  N99§3AUGP        COMP      'S'                                3333
027900000000      *
028000000000      * Decodifico l'elemento 1Z
028100000000     C                   MOVE      '1Z'          TBLCOD
028200000000     C                   MOVE      *BLANKS       TBLKEY
028300000000     C                   MOVEL     TPSE1V        TBLKEY
028400000000     C     TABELK        CHAIN     TABEL00F                           99
028500000000     C                   MOVE      *BLANKS       §1ZD1V
028600000000     C  N99              MOVEL     TBLUNI        §1ZD1V
028700100607      *
028800100607      * se era protetto deve riproteggerlo in aggiornamento
028900100607     c                   If        hin33 = '1'
029000100607     c                   seton                                        33
029100100607     c                   end
029200000000      *
029300000000     C                   UPDATE    TB46S1
029400100607      *
029500100607      *  quindi resetta la situazione da testare al prossimo giro
029600100607     c                   move      '0'           hin33
029700100607     c                   SETOFF                                       33
029800100607      *
029900000000     C                   ENDDO
030000000000     C                   ENDDO
030100000000      *
030200000000     C                   Z-ADD     1             NRR1
030300000000      *
030400000000     C                   ENDSR
030500000000      *---------------------------------------------------------------*
030600000000      * Eseguo i  controlli sui campi emessi a video                  *
030700000000      *---------------------------------------------------------------*
030800000000     C     CONTR         BEGSR
030900000000      *
031000000000      * Per prima cosa verifico che non vi siano state scelte nel SFL
031100000000      * L'unica cosa che l'utente puo' fare e' l'annullamento; se un record
031200000000      * viene annullato deve essere cancellato fisicamente dall'archivio
031300000000     C   35              DO        *HIVAL        $B
031400000000     C     $B            CHAIN     TB46S1                             31
031500000000     C  N31TBLFGV        IFEQ      'A'
031600000000     C                   SETON                                        02
031700000000      *
031800000000      * Se non e' stato premuto F6 di conferma non posso fare nulla
031900000000     C  NKF              SETON                                        40  90
032000000000     C  NKF              GOTO      ENDCTL
032100000000      *
032200000000      * Proteggo il campo dell'annullamento e aggiorno il SFL
032300000000     C                   SETON                                        33
032400100607     c                   move      '1'           hin33
032500000000     C                   UPDATE    TB46S1
032600100607      *
032700100607      *  quindi resetta la situazione da testare al prossimo giro
032800100607     c                   move      '0'           hin33
032900100607     c                   SETOFF                                       33
033000000000      *
033100000000      * Cancello fisicamente il record dall'archivio
033200000000     C                   Z-ADD     1             TBLKUT
033300000000     C                   MOVE      '8C'          TBLCOD
033400000000     C                   MOVE      *BLANKS       $DS8C
033500000000     C                   MOVE      CDBO1V        $8CBOL
033600000000     C                   MOVE      TPSE1V        $8CSER
033700000000     C                   MOVE      ORGF1V        $8CFIL
033800000000     C                   MOVE      $DS8C         TBLKEY
033900000000     C     TABELK        CHAIN     TABEL00F                           99
034000000000     C  N99              DELETE    TABEL
034100000000      *
034200000000     C                   ENDIF
034300000000     C  N31              ENDDO
034400000000     C                   Z-ADD     1             NRR1
034500000000      *
034600000000      * Se immesso 'I' non bisogna immettere gli altri campi
034700000000     C     TUTTIV        IFEQ      'I'
034800000000     C     CDBOLV        IFNE      *BLANKS
034900000000     C     TPSERV        ORNE      *BLANKS
035000000000     C     ORGFIV        ORNE      *ZERO
035100000000     C                   SETON                                        41  90
035200000000     C                   GOTO      ENDCTL
035300000000     C                   ENDIF
035400000000     C                   ENDIF
035500000000      *
035600000000      * Se immesso 'I' emetto il formato con tutte i codici bolla possibili
035700000000     C     TUTTIV        IFEQ      'I'
035800000000     C                   MOVE      '2'           $VIDEO
035900000000     C                   GOTO      ENDCTL
036000000000     C                   ENDIF
036100000000      *
036200000000      * Verifico la filiale (nel SFLCTL)
036300000000     C                   MOVE      *BLANKS       ORGDEV
036400000000     C     ORGFIV        IFNE      *ZERO
036500130910     C*    ORGFIV        LOOKUP    L1                                     99
036501130910     c* controllo se utente abilitato alla gestione delle tariffe aut
036502131008     c                   clear                   tibs42ds
036503131008     c                   movel     orgfiv        i42pge
036505131008     c                   call      'TIBS42R'
036506131008     c                   parm                    TIBS42ds
036507131008     c                   movel     o42uni        uteautds
036508130910     c                   if        §autmtc <>'S'
036515130910     C                   SETON                                        44  90
036516130910     C   44              GOTO      ENDCTL
036517130910     c                   endif
036800000000      *
036900000000      * Decodifico il codice filiale
037000000000     C     ORGFIV        CHAIN     AZORG01L                           44
037100000000     C  N44              MOVE      ORGDES        ORGDEV
037200000000     C   44              SETON                                        90
037300000000     C   44              GOTO      ENDCTL
037400000000     C                   ENDIF
037500000000      *
037600000000      * Funzione di '?' sul codice bolla
037700020725     c     '?'           scan      cdbolv                                 21
037800020725     C**** CDBOLV        IFEQ      '?'
037900020725     C     *in21         IFEQ      *on
038000000000     C                   Z-ADD     1             §KUT
038100000000     C                   MOVE      '3A'          §COD
038200000000     C                   MOVEL     *BLANKS       §KEY
038300000000     C                   CALL      'X§TABER'                            99
038400000000     C                   PARM                    §KUT              1 0
038500000000     C                   PARM                    §COD              2
038600000000     C                   PARM                    §KEY              8
038700000000     C                   PARM                    §DES             30
038800000000     C                   MOVE      *BLANKS       CDBOLV
038900000000     C  N99              MOVEL     §KEY          CDBOLV
039000000000     C                   SETON                                        90  91
039100000000     C                   ENDIF
039200000000      *
039300000000      * Funzione di '?' sul codice prestazione
039400000000     C     TPSERV        IFEQ      '?'
039500000000     C                   Z-ADD     1             §KUT
039600000000     C                   MOVE      '1Z'          §COD
039700000000     C                   MOVEL     *BLANKS       §KEY
039800000000     C                   CALL      'X§TABER'                            99
039900000000     C                   PARM                    §KUT              1 0
040000000000     C                   PARM                    §COD              2
040100000000     C                   PARM                    §KEY              8
040200000000     C                   PARM                    §DES             30
040300000000     C                   MOVE      *BLANKS       TPSERV
040400000000     C  N99              MOVEL     §KEY          TPSERV
040500000000     C                   SETON                                        90  91
040600000000     C                   ENDIF
040700000000     C   90              GOTO      ENDCTL
040800000000      *
040900000000      * Il codice bolla deve esistere nella tabella 3A
041000000000     C                   MOVE      *BLANKS       §3ADEV
041100000000     C                   MOVE      *BLANKS       §1ZDEV
041200000000     C     CDBOLV        IFNE      *BLANK
041300000000     C     TPSERV        ORNE      *BLANK
041400000000     C     ORGFIV        ORNE      *ZERO
041500000000     C                   Z-ADD     1             TBLKUT
041600000000     C                   MOVE      '3A'          TBLCOD
041700000000     C                   MOVE      *BLANKS       TBLKEY
041800000000     C                   MOVEL     CDBOLV        TBLKEY
041900000000     C     TABELK        CHAIN     TABEL00F                           42
042000000000     C  N42TBLFLG        COMP      *BLANKS                            4242
042100000000     C  N42              MOVEL     TBLUNI        DS3A
042200000000     C  N42§3AUGP        COMP      'S'                                4242
042300000000     C   42              SETON                                        90
042400000000     C   42              GOTO      ENDCTL
042500000000     C                   MOVEL     TBLUNI        §3ADEV
042600000000      *
042700000000      * Il tipo servizio deve esistere nella tabella 1Z
042800000000      * e deve avere il flag "gestito per padroncini"=SI
042900000000     C                   Z-ADD     1             TBLKUT
043000000000     C                   MOVE      '1Z'          TBLCOD
043100000000     C                   MOVE      *BLANKS       TBLKEY
043200000000     C                   MOVEL     TPSERV        TBLKEY
043300000000     C     TABELK        CHAIN     TABEL00F                           43
043400000000     C  N43TBLFLG        COMP      *BLANKS                            4343
043500000000     C  N43              MOVEL     TBLUNI        DS1Z
043600000000     C  N43§1ZUPD        COMP      'S'                                4343
043700000000     C   43              SETON                                        90
043800000000     C   43              GOTO      ENDCTL
043900000000     C                   MOVEL     §1ZDES        §1ZDEV
044000000000      *
044100000000      * Se tento di immettere il codice con filiale generica (=0) devo essere
044200000000      * certo che nella tabella 8C non vi sia lo stesso codice con una filiale
044300100607      *****
044400100607      *****  Attenzione la filiale "000" non deve essere gestita da questo pgm
044500100607      *****  come prima quando esistevano più AS
044600100607      *****
044700100607      *****
044800100607      *****
044900000000     C     ORGFIV        IFEQ      *ZERO
045000100607      *****
045100100607      *****  Deve segnalare come errore
045200100607      *****
045300100608     c                   if        Utente_EDP <>'S'
045400100607     C                   SETON                                        44  90
045500100607     C                   GOTO      ENDCTL
045600100608     c                   else
045700100607      *****
045800100608     C                   Z-ADD     1             TBLKUT
045900100608     C                   MOVE      '8C'          TBLCOD
046000100608     C                   MOVE      *BLANKS       $DS8C
046100100608     C                   MOVE      CDBOLV        $8CBOL
046200100608     C                   MOVE      TPSERV        $8CSER
046300100608     C                   MOVE      *ZERO         $8CFIL
046400100608     C                   MOVE      $DS8C         TBLKEY
046500100608     C     TABELK        SETLL     TABEL00F
046600100608     C     TABE1K        READE     TABEL00F                               99
046700100608     C                   MOVEL     TBLKEY        $DS8C
046800100608     C  N99$8CBOL        IFEQ      CDBOLV
046900100608     C     $8CSER        ANDEQ     TPSERV
047000100608     C                   SETON                                        47  90
047100100608     C                   GOTO      ENDCTL
047200100608     C                   ENDIF
047300100608      *
047400100608     c                   end
047500100608      *
047600100607     C                   ENDIF
047700100607      *****
047800100607      *****
047900000000      * Verifico che nella 8C non vi sia il cod.manc.consegna con una filiale
048000000000     C                   Z-ADD     1             TBLKUT
048100000000     C                   MOVE      '8C'          TBLCOD
048200000000     C                   MOVE      *BLANKS       $DS8C
048300000000     C                   MOVE      CDBOLV        $8CBOL
048400000000     C                   MOVE      TPSERV        $8CSER
048500000000     C                   MOVE      ORGFIV        $8CFIL
048600000000     C                   MOVE      $DS8C         TBLKEY
048700000000     C     TABELK        CHAIN     TABEL00F                           99
048800000000     C  N99              SETON                                        45  90
048900000000     C   45              GOTO      ENDCTL
049000000000      *
049100000000      * Eseguo sempre il comtrollo sulla 8B pero' con cd.filiale=0
049200000000     C                   MOVE      *ZERO         $8CFIL
049300000000     C                   MOVE      $DS8C         TBLKEY
049400000000     C     TABELK        CHAIN     TABEL00F                           99
049500000000     C  N99              SETON                                        46  90
049600000000     C   46              GOTO      ENDCTL
049700000000      *
049800000000     C                   ENDIF
049900000000      *
050000000000     C     ENDCTL        ENDSR
050100000000      *---------------------------------------------------------------*
050200000000      * Registrazione record nel SFL e nell'archivio tabelle          *
050300000000      *---------------------------------------------------------------*
050400000000     C     REGIS         BEGSR
050500000000      *
050600000000      * Scrivo nel SFL
050700000000     C                   MOVE      *BLANK        TBLFGV
050800000000     C                   MOVE      CDBOLV        CDBO1V
050900000000     C                   MOVEL     §3ADEV        §3AD1V
051000000000     C                   MOVE      TPSERV        TPSE1V
051100000000     C                   MOVEL     §1ZDEV        §1ZD1V
051200000000     C                   Z-ADD     ORGFIV        ORGF1V
051300000000     C                   MOVEL     ORGDEV        ORGD1V
051400000000     C                   ADD       1             $NRR1
051500000000     C                   Z-ADD     $NRR1         NRR1
051600000000     C                   SETOFF                                       33
051700000000     C                   WRITE     TB46S1
051800100607      *
051900100607      *  quindi resetta la situazione da testare al prossimo giro
052000100607     c                   move      '0'           hin33
052100100607     c                   SETOFF                                       33
052200100607      *
052300000000     C  N35              SETON                                        35
052400000000      *
052500000000      * Scrivo nell'archivio
052600000000     C                   Z-ADD     1             TBLKUT
052700000000     C                   MOVE      '8C'          TBLCOD
052800000000     C                   MOVE      *BLANKS       $DS8C
052900000000     C                   MOVE      CDBOLV        $8CBOL
053000000000     C                   MOVE      TPSERV        $8CSER
053100000000     C                   Z-ADD     ORGFIV        $8CFIL
053200000000     C                   MOVE      $DS8C         TBLKEY
053300000000     C                   MOVE      *BLANKS       TBLUNI
053400000000     C                   MOVE      *BLANK        TBLFTT
053500000000     C                   Z-ADD     *ZERO         TBLFLT
053600000000     C                   MOVE      *BLANK        TBLFTR
053700000000     C                   Z-ADD     *ZERO         TBLDTR
053800000000     C                   WRITE     TABEL
053900000000      *
054000000000      * Pulisco i campi a video
054100000000     C                   MOVE      *BLANK        TUTTIV
054200000000     C                   MOVE      *BLANKS       CDBOLV
054300000000     C                   MOVE      *BLANKS       TPSERV
054400000000     C                   MOVE      *ZERO         ORGFIV
054500000000     C                   MOVE      *BLANKS       §3ADEV
054600000000     C                   MOVE      *BLANKS       §1ZDEV
054700000000     C                   MOVE      *BLANKS       ORGDEV
054800000000      *
054900000000     C                   ENDSR
055000000000      *---------------------------------------------------------------*
055100000000      * Gestione del secondo formato video                            *
055200000000      *---------------------------------------------------------------*
055300000000     C     SECOND        BEGSR
055400000000      *
055500000000      * Pulisco il SFL
055600000000     C                   SETOFF                                       3435
055700000000     C                   WRITE     TB46C2
055800000000     C                   SETON                                        34
055900000000     C                   Z-ADD     *ZERO         NRR2
056000000000      *
056100000000      * Leggo tutta la tabella 3A
056200000000     C                   Z-ADD     1             TBLKUT
056300000000     C                   MOVE      '3A'          TBLCOD
056400000000     C     TABE1K        SETLL     TABEL00F
056500000000     C                   DO        *HIVAL
056600000000     C     TABE1K        READE     TABEL00F                               31
056700000000     C  N31TBLFLG        IFEQ      *BLANK
056800000000     C                   MOVEL     TBLUNI        DS3A
056900000000      *
057000000000      * Devo considerare solo quelli utilizzati per i padroncini
057100000000     C     §3AUGP        IFEQ      'S'
057200000000      *
057300000000      * Verifico che l'elemento in esame non sia gia' presente nel SFL di prima
057400000000     C                   MOVEL     TBLKEY        CDBOLW
057500000000      *
057600000000     C  N35              SETON                                        35
057700000000     C                   MOVE      *BLANK        SCEL2V
057800000000     C                   MOVEL     TBLUNI        §3ADEW
057900000000     C                   MOVE      *BLANK        TPSERW
058000000000     C                   Z-ADD     *ZERO         ORGFIW
058100000000     C                   SETOFF                                       414345
058200000000     C                   SETOFF                                       4749
058300000000     C                   ADD       1             NRR2
058400000000     C                   WRITE     TB46S2
058500000000     C                   ENDIF
058600000000     C                   ENDIF
058700000000     C  N31              ENDDO
058800000000      *
058900000000      *
059000000000     C                   Z-ADD     1             NRR2
059100000000     C                   SETOFF                                       01
059200000000     C                   SETON                                        91
059300000000     C     EMETT2        TAG
059400000000     C   91              WRITE     TB46D2
059500000000     C  N35
059600000000     CAN 91              WRITE     TB46D3
059700000000     C                   EXFMT     TB46C2
059800000000      *
059900000000     C                   SETOFF                                       404244
060000000000     C                   SETOFF                                       454648
060100000000     C                   SETOFF                                       50
060200000000     C                   SETOFF                                       029091
060300000000      *
060400000000     C   KL
060500000000     COR KC              GOTO      ENDSEC
060600000000      *
060700000000      * Eseguo i controlli sul formato video
060800000000     C                   EXSR      CONTR2
060900000000      *
061000000000      * Se premuto F6 ma nessuna scelta fatta riemetto il formato
061100000000     C  N90
061200000000     CANNKF
061300000000     CAN 02              SETON                                        40  90
061400000000      *
061500000000      * Per errore generico riemetto il formato video
061600000000     C   90              GOTO      EMETT2
061700000000      *
061800000000      * Se tutto e' ok aggiorno l'archivio
061900000000     C   35              EXSR      REGIS2
062000000000      *
062100000000      * Imposto l'indicatore generico di errore per riemettere il primo formato
062200000000     C                   SETON                                        90
062300000000     C                   MOVE      '1'           $VIDEO
062400000000      *
062500000000     C     ENDSEC        ENDSR
062600000000      *---------------------------------------------------------------*
062700000000      * Verifica dei campi delsecondo formato video                   *
062800000000      *---------------------------------------------------------------*
062900000000     C     CONTR2        BEGSR
063000000000      *
063100000000     C                   Z-ADD     *ZERO         $B
063200000000     C                   Z-ADD     *ZERO         $NRR2
063300000000      *
063400000000      * Leggo gli elementi variati del SFl
063500000000     C   35              DO        *HIVAL        $NRR2
063600000000     C     $NRR2         CHAIN     TB46S2                             31
063700000000     C  N31              SETOFF                                       414347
063800000000     C  N31              SETOFF                                       49
063900000000     C  N31SCEL2V        IFNE      *BLANK
064000000000     C     TPSERW        ORNE      *BLANK
064100000000     C     ORGFIW        ORNE      *ZERO
064200000000     C                   SETON                                        02
064300000000      *
064400000000      * Se non selezionata la riga ma immessa la filiale e' errore
064500000000     C     SCEL2V        IFEQ      *BLANK
064600000000     C     TPSERW        IFNE      *BLANK
064700000000     C     ORGFIW        ORNE      *ZERO
064800000000     C                   SETON                                        414290
064900000000     C                   Z-ADD     $NRR2         $B
065000000000     C                   UPDATE    TB46S2
065100000000     C                   GOTO      ENDCT2
065200000000     C                   ENDIF
065300000000     C                   ENDIF
065400000000      *
065500000000     C     SCEL2V        IFEQ      '1'
065600000000      *
065700000000      * La filiale deve esistere nella schiera
065800100608     c                   setoff                                       99
065900130910     c                   if        orgFIW <> 0
066000130910     C*    ORGFIW        LOOKUP    L1                                     99
066001130910     c* controllo se utente abilitato alla gestione delle tariffe aut
066002131008     c                   clear                   tibs42ds
066003131008     c                   movel     simpou        i42pge
066005131008     c                   call      'TIBS42R'
066006131008     c                   parm                    tibs42ds
066007131008     c                   movel     o42uni        uteautds
066008130910     c                   if        §autmtc <>'S'
066009130910     C                   SETON                                        474890
066010130910     C   47              UPDATE    TB46S2
066011130910     C   47              Z-ADD     $NRR2         $B
066012130910     C   47              GOTO      ENDCT2
066015130910     c                   endif
066100100608     c                   end
066200100608      *
066700000000      *
066800000000      * Funzione '?' sul codice prestazione
066900000000     C     TPSERW        IFEQ      '?'
067000000000     C                   Z-ADD     1             §KUT
067100000000     C                   MOVE      '1Z'          §COD
067200000000     C                   MOVEL     *BLANKS       §KEY
067300000000     C                   CALL      'X§TABER'                            99
067400000000     C                   PARM                    §KUT              1 0
067500000000     C                   PARM                    §COD              2
067600000000     C                   PARM                    §KEY              8
067700000000     C                   PARM                    §DES             30
067800000000     C                   MOVE      *BLANKS       TPSERW
067900000000     C  N99              MOVEL     §KEY          TPSERW
068000000000     C                   SETON                                        90  91
068100000000     C                   UPDATE    TB46S2
068200000000     C                   GOTO      ENDCT2
068300000000     C                   ENDIF
068400000000      *
068500000000      * Il codice prestazione deve esistere nella tabella 1Z
068600000000      * e deve avere il flag "gestito per padroncini"=SI
068700000000     C                   Z-ADD     1             TBLKUT
068800000000     C                   MOVE      '1Z'          TBLCOD
068900000000     C                   MOVE      *BLANKS       TBLKEY
069000000000     C                   MOVEL     TPSERW        TBLKEY
069100000000     C     TABELK        CHAIN     TABEL00F                           49
069200000000     C  N49TBLFLG        COMP      *BLANKS                            4949
069300000000     C  N49              MOVEL     TBLUNI        DS1Z
069400000000     C  N49§1ZUPD        COMP      'S'                                4949
069500000000     C   49              SETON                                        50  90
069600000000     C   49              UPDATE    TB46S2
069700000000     C   49              GOTO      ENDCT2
069800000000      *
069900000000      * Se tento di immettere il codice con filiale generica (=0) devo essere
070000000000      * certo che nella tabella 8C non vi sia lo stesso codice con una filiale
070100000000     C     ORGFIW        IFEQ      *ZERO
070200000000     C                   Z-ADD     1             TBLKUT
070300000000     C                   MOVE      '8C'          TBLCOD
070400000000     C                   MOVE      *BLANKS       $DS8C
070500931115     C                   MOVE      CDBOLW        $8CBOL
070600931115     C                   MOVE      TPSERW        $8CSER
070700000000     C                   MOVE      *ZERO         $8CFIL
070800000000     C                   MOVE      $DS8C         TBLKEY
070900000000     C     TABELK        SETLL     TABEL00F
071000000000     C     TABE1K        READE     TABEL00F                               99
071100000000     C  N99              MOVEL     TBLKEY        $DS8C
071200000000     C  N99$8CBOL        IFEQ      CDBOLW
071300000000     C     $8CSER        ANDEQ     TPSERW
071400000000     C                   SETON                                        434590
071500000000     C                   Z-ADD     $NRR2         $B
071600000000     C                   UPDATE    TB46S2
071700000000     C                   GOTO      ENDCT2
071800000000     C                   ENDIF
071900000000     C                   ENDIF
072000000000      *
072100000000      * Verifico che nella 8C non vi sia il cod.manc.consegna con una filiale
072200000000     C                   Z-ADD     1             TBLKUT
072300000000     C                   MOVE      '8C'          TBLCOD
072400000000     C                   MOVE      *BLANKS       $DS8C
072500000000     C                   MOVE      CDBOLW        $8CBOL
072600000000     C                   MOVE      TPSERW        $8CSER
072700000000     C                   Z-ADD     ORGFIW        $8CFIL
072800000000     C                   MOVE      $DS8C         TBLKEY
072900000000     C     TABELK        CHAIN     TABEL00F                           99
073000000000     C  N99              SETON                                        434490
073100000000     C   44              Z-ADD     $NRR2         $B
073200000000     C   44              UPDATE    TB46S2
073300000000     C   44              GOTO      ENDCT2
073400000000      *
073500000000      * Eseguo sempre il comtrollo sulla 8B pero' con cd.filiale=0
073600000000     C                   MOVE      *BLANKS       $DS8C
073700000000     C                   MOVE      CDBOLW        $8CBOL
073800000000     C                   MOVE      TPSERW        $8CSER
073900000000     C                   Z-ADD     *ZERO         $8CFIL
074000000000     C                   MOVE      $DS8C         TBLKEY
074100000000     C     TABELK        CHAIN     TABEL00F                           99
074200000000     C  N99              SETON                                        434690
074300000000     C   46              Z-ADD     $NRR2         $B
074400000000     C   46              UPDATE    TB46S2
074500000000     C   46              GOTO      ENDCT2
074600000000      *
074700000000     C                   ENDIF
074800000000      *
074900000000     C                   ENDIF
075000000000     C  N31              UPDATE    TB46S2
075100000000     C  N31              ENDDO
075200000000      *
075300000000     C     ENDCT2        TAG
075400000000     C     $B            IFEQ      *ZERO
075500000000     C                   Z-ADD     1             NRR2
075600000000     C                   ELSE
075700000000     C                   Z-ADD     $B            NRR2
075800000000     C                   ENDIF
075900000000      *
076000000000     C                   ENDSR
076100000000      *---------------------------------------------------------------*
076200000000      * Aggiorno la tabella 8B con le scelte fatte nel SFL            *
076300000000      *---------------------------------------------------------------*
076400000000     C     REGIS2        BEGSR
076500000000      *
076600000000     C                   DO        *HIVAL        $B
076700000000     C     $B            CHAIN     TB46S2                             31
076800000000     C  N31SCEL2V        IFEQ      '1'
076900000000      *
077000000000     C                   Z-ADD     1             TBLKUT
077100000000     C                   MOVE      '8C'          TBLCOD
077200000000     C                   MOVE      *BLANKS       $DS8C
077300000000     C                   MOVE      CDBOLW        $8CBOL
077400000000     C                   MOVE      TPSERW        $8CSER
077500000000     C                   Z-ADD     ORGFIW        $8CFIL
077600000000     C                   MOVE      $DS8C         TBLKEY
077700000000     C                   MOVE      *BLANKS       TBLUNI
077800000000     C                   MOVE      *BLANK        TBLFTT
077900000000     C                   Z-ADD     *ZERO         TBLFLT
078000000000     C                   MOVE      *BLANK        TBLFTR
078100000000     C                   Z-ADD     *ZERO         TBLDTR
078200000000     C                   WRITE     TABEL
078300000000      *
078400000000     C                   ENDIF
078500000000     C  N31              ENDDO
078600000000      *
078700000000     C                   ENDSR
078800090605** msg
078900090605SNDBRKMSG MSG('UTENTE NON AUTORIZZATO') TOMSGQ(          )
