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
004000970911     d dsfil         E DS                                                       *reperimento dati
004100970911     d ut§dse        E DS                  EXTNAME(ut§dse0f)                    *reper.dati utente
004200970911     d cncr80        E DS
004300970911     d wlbda8          DS                  INZ                                  *controlla data
004400970911     d  g08dat                        8  0
004500970911     d  g08inv                        8  0
004600970911     d  g08err                        1
004700970911     d  g08tgi                        5  0
004800970911     d dspar54         DS                  INZ                                  *ds parametri lancio
004900970911     d  p54dri                        8                                          -data riferimento
005000970911     d  p54src                        1                                          -sorgenti
005100970911     d kpjba         E DS                                                       *architettura
005200970911     c*--------------------------------------------------------------------------------------------*
005300970911     c* Main lines
005400970911     c*--------------------------------------------------------------------------------------------*
005500970911     C*
005600970911     C* impostazioni testata
005700970911     C                   EXSR      impvd0
005800970911     C*
005900970911     C* impostazioni prima videata
006000970911     C                   EXSR      impvd1
006100970911     C*
006200970911     C* ciclo fino a richiesta di fine elaborazione
006300970911do  1C                   DOU       $fine='S'
006400970911     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
006500970911     C                   ENDCS
006600970911e   1C                   ENDDO
006700970911     C*
006800970911     C                   SETON                                        lr
006900970911     c*--------------------------------------------------------------------------------------------*
007000970911     c* impvd1 - impostazioni prima videata
007100970911     c*--------------------------------------------------------------------------------------------*
007200970911     C     impvd1        BEGSR
007300970911     C*
007400970911     C* reimposta formato video e indicatori
007500970911     C                   Z-ADD     datcor        v1cdri                         data ultima versione
007600970911     C                   MOVEL     'S'           v1csrc                         srorgenti
007700970911     C                   EXSR      setvd1
007800970911     C                   MOVEL     '1'           $tipvid                        *gestione video
007900970911     C*
008000970911     C                   ENDSR
008100970911     c*--------------------------------------------------------------------------------------------*
008200970911     c* setvd1 - impostazioni indicatori prima videata
008300970911     c*--------------------------------------------------------------------------------------------*
008400970911     C     setvd1        BEGSR
008500970911     C*
008600970911     C                   MOVEL     *blanks       $msg                            *messaggio
008700970911     C                   SETOFF                                       90         *generico di errore
008800970911     C                   SETOFF                                       202122     *errori
008900970911if  1c                   IF        dsfiti <> 'S'                                 *in filaile
009000970911     C                   SETON                                        01         *si condizionamenti
009100970911x   1c                   ELSE                                                    *in sede
009200970911     C                   SETOFF                                       01         *no condizionamenti
009300970911e   1c                   ENDIF
009400970911     C*
009500970911     C                   ENDSR
009600970911     c*--------------------------------------------------------------------------------------------*
009700970911     C* gesvd1 - gestione prima videata
009800970911     c*--------------------------------------------------------------------------------------------*
009900970911     C     gesvd1        BEGSR
010000970911     C*
010100970911     C* emissione videata
010200970911if  1C                   IF        $tesvd='1'
010300970911     C                   WRITE     si54d00                                      *testata
010400970911     C                   MOVEL     '0'           $tesvd                         *no testata
010500970911e   1C                   ENDIF
010600970911     C                   EXFMT     si54d01                                      *video
010700970911     C                   EXSR      setvd1
010800970911     C* F03-Fine
010900970911if  1C                   IF        *inkc
011000970911     C                   EXSR      f03ges
011100970911     C                   GOTO      finvd1
011200970911e   1C                   ENDIF
011300970911     C* F12-Ritorno
011400970911if  1C                   IF        *inkl
011500970911     C                   EXSR      f12ges
011600970911     C                   GOTO      finvd1
011700970911e   1C                   ENDIF
011800970911     C* Controlla video
011900970911     C                   EXSR      ctrvd1
012000970911if  1C                   IF        *in90                                        *errori
012100970911     C                   GOTO      finvd1
012200970911e   1C                   ENDIF
012300970911     C* F06-Conferma
012400970911if  1C                   IF        *inkf
012500970911     C                   EXSR      f06ges
012600970911     C                   GOTO      finvd1
012700970911e   1C                   ENDIF
012800970911     C*
012900970911     C     finvd1        ENDSR
013000970911     c*--------------------------------------------------------------------------------------------*
013100970911     C* ctrvd1 - controlla prima videata
013200970911     c*--------------------------------------------------------------------------------------------*
013300970911     C     ctrvd1        BEGSR
013400970911     C*
013500970911     C* reimposta le variabili di lavoro
013600970911     C                   MOVEL     '0'           $err
013700970911     C*---
013800970911     C* data di riferimento
013900970911     C*---
014000970911     C                   Z-ADD     *zeros        driv1c
014100970911     C                   Z-ADD     v1cdri        wda8
014200970911     C                   Z-ADD     *zeros        da8w
014300970911     C                   EXSR      chkda8
014400970911     C                   Z-ADD     wda8          v1cdri
014500970911     C                   Z-ADD     da8w          driv1c
014600970911if  1C                   IF        $err='1'
014700970911     C                   SETON                                        2090
014800970911     C                   MOVEL     msg(1)        $msg
014900970911     C                   GOTO      finct1
015000970911e   1C                   ENDIF
015100970911     C*---
015200970911     C* scarico sorgenti
015300970911     C*---
015400970911if  1c                   IF        dsfiti = 'S'                                 *solo in sede
015500970911if  2c                   IF        v1csrc <> 'S' AND
015600970911     c                             v1csrc <> 'N'
015700970911     C                   SETON                                        2290
015800970911     C                   MOVEL     msg(4)        $msg
015900970911     C                   MOVEL     '1'           $err
016000970911     C                   GOTO      finct1
016100970911e   2C                   ENDIF
016200970911x   1c                   ELSE                                                   *in filiale
016300970911     C                   MOVEL     'N'           v1csrc
016400970911e   1C                   ENDIF
016500970911     C*
016600970911     C     finct1        ENDSR
016700970911     c*--------------------------------------------------------------------------------------------*
016800970911     c* chkda8 - controlla la data (8)
016900970911     c*--------------------------------------------------------------------------------------------*
017000970911     C     chkda8        BEGSR
017100970911     C*
017200970911     C                   Z-ADD     wda8          g08dat
017300970911     C                   Z-ADD     *zeros        g08inv
017400970911     C                   MOVEL     '0'           g08err
017500970911     C                   CALL      'XSRDA8'
017600970911     C                   PARM                    wlbda8
017700970911     C                   Z-ADD     g08dat        wda8
017800971008     C                   Z-ADD     g08inv        da8w
017900970911IF  1C                   IF        g08err='1'
018000970911     C                   MOVEL     '1'           $err
018100970911E   1C                   ENDIF
018200970911     C*
018300970911     C                   ENDSR
018400970911     c*--------------------------------------------------------------------------------------------*
018500970911     C* f03ges - gestione tasto funzionale f03 ==> Fine
018600970911     c*--------------------------------------------------------------------------------------------*
018700970911     C     f03ges        BEGSR
018800970911     C*
018900970911     C                   MOVEL     'S'           $fine                          *fine programma
019000970911     C*
019100970911     C                   ENDSR
019200970911     c*--------------------------------------------------------------------------------------------*
019300970911     C* f06ges - gestione tasto funzionale f06 ==> Conferma
019400970911     c*--------------------------------------------------------------------------------------------*
019500970911     C     f06ges        BEGSR
019600970911     c*
019700970911     c* imposta i parametri di lancio
019800970911     c                   MOVEL     driv1c        p54dri                         *data riferimento
019900970911     c                   MOVEL     v1csrc        p54src                         *sorgenti
020000970911     c*
020100970911     c* lancia il programma
020200970911     c                   MOVEL     dspar54       kpjbu                          *parametri di lancio
020300971007     c                   MOVEL     'SI55'        kcoaz                          *codice azione
020400970911     c                   CALL      'BCH10'
020500970911     c                   PARM                    kpjba
020600970911     c                   MOVEL     'S'           $fine                          *fine programma
020700970911     c*
020800970911     C                   ENDSR
020900970911     c*--------------------------------------------------------------------------------------------*
021000970911     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
021100970911     c*--------------------------------------------------------------------------------------------*
021200970911     C     f12ges        BEGSR
021300970911     C*
021400970911     C                   MOVEL     'S'           $fine                          *fine programma
021500970911     C*
021600970911     C                   ENDSR
021700970911     c*--------------------------------------------------------------------------------------------*
021800970911     C* impvd0 - Impostazioni testata
021900970911     c*--------------------------------------------------------------------------------------------*
022000970911     C     impvd0        BEGSR
022100970911     C*
022200970911     C                   MOVEL     '1'           $tesvd                         *riemettere testata
022300970911     C*
022400970911     C                   ENDSR
022500970911     c*--------------------------------------------------------------------------------------------*
022600970911     C* *inzsr - operazioni iniziali
022700970911     c*--------------------------------------------------------------------------------------------*
022800970911     C     *inzsr        BEGSR
022900970911     c*
023000970911     c* ricevimento parametri
023100970911     c     *ENTRY        PLIST
023200970911     c                   PARM                    kpjba
023300970911     C*
023400970911     C* reperimento dati utente
023500970911     C                   CALL      'XFIL'
023600970911     C                   PARM                    dsfil
023700970911     C*
023800970911     C* reperimento dati utente
023900970911     c                   Z-ADD     dsficd        codut
024000970911     C                   CALL      'X§PARUT'
024100970911     C                   PARM                    ut§dse
024200970911     C                   MOVEL     rec80         cncr80
024300970911     C*
024400970911     C* reperisce la data corrente
024500970911     C                   TIME                    n14                            *ora (6)+ data (8)
024600970911     C                   MOVE      n14           n8                             *data (8) in g/m/aa
024700970911     C                   Z-ADD     n8            datcor                         *Data corrente
024800970911     C*
024900970911     C                   ENDSR
025000970911     O*--------------------------------------------------------------------------------------------*
025100970911** -msg-
025200970911TISI54R- Data errata                                                          1
025300971007TISI54R- 2                                                                    2
025400970911TISI54R- Immettere il codice della filiale elaboratore                        3
025500970911TISI54R- Immettere uno dei valori possibili                                   4
025600970911TISI54R- 5
025700970911TISI54R- 6
025800970911TISI54R- 7
025900970911TISI54R- 8
026000970911TISI54R- 9
026100970911TISI54R- 10
