000100970911     h*--------------------------------------------------------------------------------------------*
000200970911     h* Allineamento cappario con località per clienti - Richiesta parametri
000300970911     h*--------------------------------------------------------------------------------------------*
000400970911     h DECEDIT('0,') DATEDIT(*DMY.)
000500970911     f*--------------------------------------------------------------------------------------------*
000600970911     f* Data base
000700970911     f*--------------------------------------------------------------------------------------------*
000800970911     ftisi54d   CF   E             WORKSTN
000900970911     d*--------------------------------------------------------------------------------------------*
001000970911     d* Data structure
001100970911     d*--------------------------------------------------------------------------------------------*
001200970911     d*----------------
001300970911     d* schiere
001400970911     d*----------------
001500970911     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001600970911     D*----------------
001700970911     D* Variabili riferite al data base
001800970911     D*----------------
001900970911     d*----------------
002000970911     d* Variabili di controllo
002100970911     d*----------------
002200970911     d $fine           S              1    INZ('N')                             *Gestione
002300970911     d $tesvd          S              1    INZ('1')                             *riemettere testata
002400970911     d $tipvid         S              1    INZ('1')                             *gestione video
002500970911     d $err            S              1    INZ('0')                             *errore generico
002600970911     d*----------------
002700970911     d* Variabili a programma
002800970911     d*----------------
002900970911     d n3              S              3  0                                      *numerico 2
003000970911     d n8              S              8  0                                      *numerico 8
003100970911     d n14             S             14  0                                      *numerico 14
003200970911     d a1              S              1                                         *alfanumerico 1
003300970911     d datcor          S              8  0                                      *data corrente a/m/g
003400970911     d wda8            S              8  0                                      *data di 8 in g/m/a
003500970911     d da8w            S              8  0                                      *data di 8 in a/m/g
003600970911     D driv1c          S                   LIKE(v1cdri)                         *data video aa/m/g
003700970911     d*----------------
003800970911     d* Ds
003900970911     d*----------------
004000020919     D TIBS34DS      E DS                                                       *Profili utente
004100020919     D DDATIUTE      E DS                                                       *Dati utente
004200020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004300970911     d wlbda8          DS                  INZ                                  *controlla data
004400970911     d  g08dat                        8  0
004500970911     d  g08inv                        8  0
004600970911     d  g08err                        1
004700970911     d  g08tgi                        5  0
004800970911     d dspar54         DS                  INZ                                  *ds parametri lancio
004900970911     d  p54dri                        8                                          -data riferimento
005000970911     d  p54src                        1                                          -sorgenti
005100010918     d  p54azz                        1                                          -azzeramento linee
005200061020     d  p54obs                        1                                          -esclusione obsolet
005300970911     d kpjba         E DS                                                       *architettura
005400970911     c*--------------------------------------------------------------------------------------------*
005500970911     c* Main lines
005600970911     c*--------------------------------------------------------------------------------------------*
005700970911     C*
005800970911     C* impostazioni testata
005900970911     C                   EXSR      impvd0
006000970911     C*
006100970911     C* impostazioni prima videata
006200970911     C                   EXSR      impvd1
006300970911     C*
006400970911     C* ciclo fino a richiesta di fine elaborazione
006500970911do  1C                   DOU       $fine='S'
006600970911     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
006700970911     C                   ENDCS
006800970911e   1C                   ENDDO
006900970911     C*
007000970911     C                   SETON                                        lr
007100970911     c*--------------------------------------------------------------------------------------------*
007200970911     c* impvd1 - impostazioni prima videata
007300970911     c*--------------------------------------------------------------------------------------------*
007400970911     C     impvd1        BEGSR
007500970911     C*
007600970911     C* reimposta formato video e indicatori
007700970911     C                   Z-ADD     datcor        v1cdri                         data ultima versione
007800970911     C                   MOVEL     'S'           v1csrc                         srorgenti
007900010918     C                   MOVEL     'N'           v1cazz                         azzeramento linee
008000061206     C                   MOVEL     'N'           v1cobs                         esclusione obsoleti
008100970911     C                   EXSR      setvd1
008200970911     C                   MOVEL     '1'           $tipvid                        *gestione video
008300970911     C*
008400970911     C                   ENDSR
008500970911     c*--------------------------------------------------------------------------------------------*
008600970911     c* setvd1 - impostazioni indicatori prima videata
008700970911     c*--------------------------------------------------------------------------------------------*
008800970911     C     setvd1        BEGSR
008900970911     C*
009000970911     C                   MOVEL     *blanks       $msg                            *messaggio
009100970911     C                   SETOFF                                       90         *generico di errore
009200970911     C                   SETOFF                                       202122     *errori
009300020919if  1c                   IF        DUTLPO <> 'S'                                 *in filaile
009400970911     C                   SETON                                        01         *si condizionamenti
009500970911x   1c                   ELSE                                                    *in sede
009600970911     C                   SETOFF                                       01         *no condizionamenti
009700970911e   1c                   ENDIF
009800970911     C*
009900970911     C                   ENDSR
010000970911     c*--------------------------------------------------------------------------------------------*
010100970911     C* gesvd1 - gestione prima videata
010200970911     c*--------------------------------------------------------------------------------------------*
010300970911     C     gesvd1        BEGSR
010400970911     C*
010500970911     C* emissione videata
010600970911if  1C                   IF        $tesvd='1'
010700970911     C                   WRITE     si54d00                                      *testata
010800970911     C                   MOVEL     '0'           $tesvd                         *no testata
010900970911e   1C                   ENDIF
011000970911     C                   EXFMT     si54d01                                      *video
011100970911     C                   EXSR      setvd1
011200970911     C* F03-Fine
011300970911if  1C                   IF        *inkc
011400970911     C                   EXSR      f03ges
011500970911     C                   GOTO      finvd1
011600970911e   1C                   ENDIF
011700970911     C* F12-Ritorno
011800970911if  1C                   IF        *inkl
011900970911     C                   EXSR      f12ges
012000970911     C                   GOTO      finvd1
012100970911e   1C                   ENDIF
012200970911     C* Controlla video
012300970911     C                   EXSR      ctrvd1
012400970911if  1C                   IF        *in90                                        *errori
012500970911     C                   GOTO      finvd1
012600970911e   1C                   ENDIF
012700970911     C* F06-Conferma
012800970911if  1C                   IF        *inkf
012900970911     C                   EXSR      f06ges
013000970911     C                   GOTO      finvd1
013100970911e   1C                   ENDIF
013200970911     C*
013300970911     C     finvd1        ENDSR
013400970911     c*--------------------------------------------------------------------------------------------*
013500970911     C* ctrvd1 - controlla prima videata
013600970911     c*--------------------------------------------------------------------------------------------*
013700970911     C     ctrvd1        BEGSR
013800970911     C*
013900970911     C* reimposta le variabili di lavoro
014000970911     C                   MOVEL     '0'           $err
014100970911     C*---
014200970911     C* data di riferimento
014300970911     C*---
014400970911     C                   Z-ADD     *zeros        driv1c
014500970911     C                   Z-ADD     v1cdri        wda8
014600970911     C                   Z-ADD     *zeros        da8w
014700970911     C                   EXSR      chkda8
014800970911     C                   Z-ADD     wda8          v1cdri
014900970911     C                   Z-ADD     da8w          driv1c
015000970911if  1C                   IF        $err='1'
015100970911     C                   SETON                                        2090
015200970911     C                   MOVEL     msg(1)        $msg
015300970911     C                   GOTO      finct1
015400970911e   1C                   ENDIF
015500970911     C*---
015600970911     C* scarico sorgenti
015700970911     C*---
015800020919if  1c                   IF        DUTLPO = 'S'                                 *solo in sede
015900970911if  2c                   IF        v1csrc <> 'S' AND
016000970911     c                             v1csrc <> 'N'
016100970911     C                   SETON                                        2290
016200970911     C                   MOVEL     msg(4)        $msg
016300970911     C                   MOVEL     '1'           $err
016400970911     C                   GOTO      finct1
016500970911e   2C                   ENDIF
016600970911x   1c                   ELSE                                                   *in filiale
016700970911     C                   MOVEL     'N'           v1csrc
016800970911e   1C                   ENDIF
016900970911     C*
017000970911     C     finct1        ENDSR
017100970911     c*--------------------------------------------------------------------------------------------*
017200970911     c* chkda8 - controlla la data (8)
017300970911     c*--------------------------------------------------------------------------------------------*
017400970911     C     chkda8        BEGSR
017500970911     C*
017600970911     C                   Z-ADD     wda8          g08dat
017700970911     C                   Z-ADD     *zeros        g08inv
017800970911     C                   MOVEL     '0'           g08err
017900970911     C                   CALL      'XSRDA8'
018000970911     C                   PARM                    wlbda8
018100970911     C                   Z-ADD     g08dat        wda8
018200971008     C                   Z-ADD     g08inv        da8w
018300970911IF  1C                   IF        g08err='1'
018400970911     C                   MOVEL     '1'           $err
018500970911E   1C                   ENDIF
018600970911     C*
018700970911     C                   ENDSR
018800970911     c*--------------------------------------------------------------------------------------------*
018900970911     C* f03ges - gestione tasto funzionale f03 ==> Fine
019000970911     c*--------------------------------------------------------------------------------------------*
019100970911     C     f03ges        BEGSR
019200970911     C*
019300970911     C                   MOVEL     'S'           $fine                          *fine programma
019400970911     C*
019500970911     C                   ENDSR
019600970911     c*--------------------------------------------------------------------------------------------*
019700970911     C* f06ges - gestione tasto funzionale f06 ==> Conferma
019800970911     c*--------------------------------------------------------------------------------------------*
019900970911     C     f06ges        BEGSR
020000970911     c*
020100970911     c* imposta i parametri di lancio
020200970911     c                   MOVEL     driv1c        p54dri                         *data riferimento
020300970911     c                   MOVEL     v1csrc        p54src                         *sorgenti
020400010918     c                   MOVEL     v1cazz        p54azz                         *azzeramento linee
020500061020     c                   MOVEL     v1cobs        p54obs                         *esclusione obsoleti
020600970911     c*
020700970911     c* lancia il programma
020800970911     c                   MOVEL     dspar54       kpjbu                          *parametri di lancio
020900971007     c                   MOVEL     'SI55'        kcoaz                          *codice azione
021000061019     c                   CALL      'BCH10'
021100061019 MOM c***                CALL      'TISI55C'
021200970911     c                   PARM                    kpjba
021300970911     c                   MOVEL     'S'           $fine                          *fine programma
021400970911     c*
021500970911     C                   ENDSR
021600970911     c*--------------------------------------------------------------------------------------------*
021700970911     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
021800970911     c*--------------------------------------------------------------------------------------------*
021900970911     C     f12ges        BEGSR
022000970911     C*
022100970911     C                   MOVEL     'S'           $fine                          *fine programma
022200970911     C*
022300970911     C                   ENDSR
022400970911     c*--------------------------------------------------------------------------------------------*
022500970911     C* impvd0 - Impostazioni testata
022600970911     c*--------------------------------------------------------------------------------------------*
022700970911     C     impvd0        BEGSR
022800970911     C*
022900970911     C                   MOVEL     '1'           $tesvd                         *riemettere testata
023000970911     C*
023100970911     C                   ENDSR
023200020919     C*--------------------------------------------------------------------------------------------*
023300020919     C* REPERISCE I DATI UTENTE
023400020919     C*--------------------------------------------------------------------------------------------*
023500020919     C     REPDATIUTE    BEGSR
023600020919     C*
023700020919     C* INIZIALIZZA VARIABILI DI WRK
023800020919     C                   CLEAR                   TIBS34DS
023900020919     C                   CLEAR                   AZUTEDS
024000020919     C                   CLEAR                   DDATIUTE
024100020919     C*
024200020919     C     *DTAARA       DEFINE    §azute        azuteds
024300020919     C     *DTAARA       DEFINE    §datiute      ddatiute
024400020919     C                   IN(E)     *DTAARA
024500020919if  1C                   IF        %Error
024600020919     c                   EVAL      I34Tla = 'L'
024700020919     C                   CALL      'TIBS34R'
024800020919     C                   PARM                    Tibs34Ds
024900020919     C                   IN        *DTAARA
025000020919e   1C                   ENDIF
025100020919     C*
025200020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
025300020919     C                   MOVEL(P)  RSUT          DSFIRS           20
025400020919     C*
025500020919     C                   ENDSR
025600970911     c*--------------------------------------------------------------------------------------------*
025700970911     C* *inzsr - operazioni iniziali
025800970911     c*--------------------------------------------------------------------------------------------*
025900970911     C     *inzsr        BEGSR
026000970911     c*
026100970911     c* ricevimento parametri
026200970911     c     *ENTRY        PLIST
026300970911     c                   PARM                    kpjba
026400970911     C*
026500970911     C* reperimento dati utente
026600020919     C                   EXSR      REPDATIUTE
026700970911     C*
026800970911     C* reperisce la data corrente
026900970911     C                   TIME                    n14                            *ora (6)+ data (8)
027000970911     C                   MOVE      n14           n8                             *data (8) in g/m/aa
027100970911     C                   Z-ADD     n8            datcor                         *Data corrente
027200970911     C*
027300970911     C                   ENDSR
027400970911     O*--------------------------------------------------------------------------------------------*
027500970911** -msg-
027600970911TISI54R- Data errata                                                          1
027700971007TISI54R- 2                                                                    2
027800970911TISI54R- Immettere il codice della filiale elaboratore                        3
027900970911TISI54R- Immettere uno dei valori possibili                                   4
028000970911TISI54R- 5
028100970911TISI54R- 6
028200970911TISI54R- 7
028300970911TISI54R- 8
028400970911TISI54R- 9
028500970911TISI54R- 10
