000100970609     h*--------------------------------------------------------------------------------------------*
000200970610     h* Allineamento archivi per clienti - Richiesta parametri
000300970609     h*--------------------------------------------------------------------------------------------*
000400970609     h DECEDIT('0,') DATEDIT(*DMY.)
000500970609     f*--------------------------------------------------------------------------------------------*
000600970609     f* Data base
000700970609     f*--------------------------------------------------------------------------------------------*
000800970609     ftisi50d   CF   E             WORKSTN
000900970609     fazorg01l  IF   E           K DISK
001000970609     d*--------------------------------------------------------------------------------------------*
001100970609     d* Data structure
001200970609     d*--------------------------------------------------------------------------------------------*
001300970609     d*----------------
001400970609     d* schiere
001500970609     d*----------------
001600970609     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001700970609     D*----------------
001800970609     D* Variabili riferite al data base
001900970609     D*----------------
002000970609     D korfil          S                   LIKE(orgfil)                         *per azorg00f
002100970609     d*----------------
002200970609     d* Variabili di controllo
002300970609     d*----------------
002400970609     d $fine           S              1    INZ('N')                             *Gestione
002500970609     d $tesvd          S              1    INZ('1')                             *riemettere testata
002600970609     d $tipvid         S              1    INZ('1')                             *gestione video
002700970609     d $err            S              1    INZ('0')                             *errore generico
002800970609     d*----------------
002900970609     d* Variabili a programma
003000970609     d*----------------
003100970609     d wn8             S              8  0                                      *numerico 8
003200970609     d wn14            S             14  0                                      *numerico 14
003300970609     d wa1             S              1                                         *alfanumerico 1
003400970609     d datcor          S              8  0                                      *data corrente a/m/g
003500970609     d wda8            S              8  0                                      *data di 8 in g/m/a
003600970609     d da8w            S              8  0                                      *data di 8 in a/m/g
003700970609     D driv1c          S                   LIKE(v1cdri)                         *data video aa/m/g
003800970609     D wfil            S                   LIKE(v1cfil)                         *filiale
003900970609     D wdfil           S                   LIKE(v1dfil)                         *descrizione filiale
004000970609     d*----------------
004100970609     d* Ds
004200970609     d*----------------
004300020919     D TIBS34DS      E DS                                                       *Profili utente
004400020919     D DDATIUTE      E DS                                                       *Dati utente
004500020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004600971002     d wlbda8          DS                  INZ                                  *controlla data
004700971002     d  g08dat                        8  0
004800971002     d  g08inv                        8  0
004900971002     d  g08err                        1
005000971002     d  g08tgi                        5  0
005100971203     d  p24cod                        3                                         *TNSD24R-codice
005200971203     d  p24tip                        1                                         *TNSD24R-tipo
005300971203     d  p24des                       25                                         *TNSD24R-descrizione
005400971203     d  p24pra                        3                                         *TNSD24R-ditta
005500970609     d dspar50         DS                  INZ                                  *ds parametri lancio
005600970618     d  p50dri                        8                                          -data riferimento
005700970618     d  p50fil                        3                                          -filiale
005800970609     d  p50fmr                        1                                          -tracciato record
005900970609     d  p50sac                        1                                          -solo agg.cappario
006000010703     d  p50bos                        1                                          -cliente BAR o SDI
006100010918     d  p50azz                        1                                          -azzeramento linee
006200971216     D dslv55        E DS                  EXTNAME(fnlv55ds)                    *reper.terminal
006300970609     d kpjba         E DS                                                       *architettura
006400970609     c*--------------------------------------------------------------------------------------------*
006500970609     c* Main lines
006600970609     c*--------------------------------------------------------------------------------------------*
006700970609     C*
006800970609     C* impostazioni testata
006900970609     C                   EXSR      impvd0
007000970609     C*
007100970609     C* impostazioni prima videata
007200970609     C                   EXSR      impvd1
007300970609     C*
007400970609     C* ciclo fino a richiesta di fine elaborazione
007500970609do  1C                   DOU       $fine='S'
007600970609     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
007700970609     C                   ENDCS
007800970609e   1C                   ENDDO
007900971128     c*
008000971128     c* operazioni finali
008100971128     c                   EXSR      finsr
008200970609     C*
008300970609     C                   SETON                                        lr
008400971128     c*--------------------------------------------------------------------------------------------*
008500971128     C* finsr - operazioni finali
008600971128     c*--------------------------------------------------------------------------------------------*
008700971128     C     finsr         BEGSR
008800971128     C*
008900971128     C* chiude pgm chiamati
009000971128     C                   CLEAR                   dslv55
009100971128     C                   MOVEL     'C'           d55tla
009200971128     C                   CALL      'FNLV55R'
009300971128     C                   PARM                    dslv55
009400971128     C*
009500971128     C                   ENDSR
009600970609     c*--------------------------------------------------------------------------------------------*
009700970609     c* impvd1 - impostazioni prima videata
009800970609     c*--------------------------------------------------------------------------------------------*
009900970609     C     impvd1        BEGSR
010000970609     C*
010100970609     C* reimposta formato video e indicatori
010200970609     C                   Z-ADD     datcor        v1cdri                         data ultima versione
010300970609     C                   MOVEL     'V'           v1cfmr                         tracciato record
010400970609     C                   MOVEL     'S'           v1csac                         solo agg.cappario
010500010703     C                   MOVEL     'B'           v1cbos                         cliente BAR o SDI
010600010918     C                   MOVEL     'N'           v1cazz                         azzeramento linee
010700971201     C                   CLEAR                   dslv55
010800971201     C                   MOVEL     'P'           d55tpt
010900020919     C                   Z-ADD     DUTPOU        d55lin
011000971201     C                   Z-ADD     datcor        d55drf
011100971201     C                   CALL      'FNLV55R'
011200971201     C                   PARM                    dSlv55
011300971201if  3c                   IF        d55err <> *blanks
011400020919     c                   Z-ADD     DUTPOU        d55tfp
011500971201e   3c                   ENDIF
011600971201     c                   MOVEL     d55tfp        v1cfil                         filiale
011700971201     C*
011800970609     C                   EXSR      setvd1
011900970609     C                   MOVEL     '1'           $tipvid                        *gestione video
012000970609     C*
012100970609     C                   ENDSR
012200970609     c*--------------------------------------------------------------------------------------------*
012300970609     c* setvd1 - impostazioni indicatori prima videata
012400970609     c*--------------------------------------------------------------------------------------------*
012500970609     C     setvd1        BEGSR
012600970609     C*
012700970610     C                   SETOFF                                       202122     *errori
012800970609     C                   SETOFF                                       23
012900970609     C                   SETOFF                                       90         *generico di errore
013000970609     C                   MOVEL     *blanks       $msg                            *messaggio
013100970609     C*
013200970609     C                   ENDSR
013300970609     c*--------------------------------------------------------------------------------------------*
013400970609     C* gesvd1 - gestione prima videata
013500970609     c*--------------------------------------------------------------------------------------------*
013600970609     C     gesvd1        BEGSR
013700970609     C*
013800970609     C* emissione videata
013900970609if  1C                   IF        $tesvd='1'
014000970609     C                   WRITE     si50d00                                      *testata
014100970609     C                   MOVEL     '0'           $tesvd                         *no testata
014200970609e   1C                   ENDIF
014300970609     C                   EXFMT     si50d01                                      *video
014400970609     C                   EXSR      setvd1
014500970609     C* F03-Fine
014600970609if  1C                   IF        *inkc
014700970609     C                   EXSR      f03ges
014800970609     C                   GOTO      finvd1
014900970609e   1C                   ENDIF
015000970609     C* F12-Ritorno
015100970609if  1C                   IF        *inkl
015200970609     C                   EXSR      f12ges
015300970609     C                   GOTO      finvd1
015400970609e   1C                   ENDIF
015500970609     C* Controlla video
015600970609     C                   EXSR      ctrvd1
015700970609if  1C                   IF        *in90                                        *errori
015800970609     C                   GOTO      finvd1
015900970609e   1C                   ENDIF
016000970609     C* F06-Conferma
016100970609if  1C                   IF        *inkf
016200970609     C                   EXSR      f06ges
016300970609     C                   GOTO      finvd1
016400970609e   1C                   ENDIF
016500970609     C*
016600970609     C     finvd1        ENDSR
016700970609     c*--------------------------------------------------------------------------------------------*
016800970609     C* ctrvd1 - controlla prima videata
016900970609     c*--------------------------------------------------------------------------------------------*
017000970609     C     ctrvd1        BEGSR
017100970609     C*
017200970609     C* reimposta le variabili di lavoro
017300970609     C                   MOVEL     '0'           $err
017400970609     C*---
017500970609     C* data di riferimento
017600970609     C*---
017700970609     C                   Z-ADD     *zeros        driv1c
017800970609     C                   Z-ADD     v1cdri        wda8
017900970609     C                   Z-ADD     *zeros        da8w
018000970609     C                   EXSR      chkda8
018100970609     C                   Z-ADD     wda8          v1cdri
018200970609     C                   Z-ADD     da8w          driv1c
018300970609if  1C                   IF        $err='1'
018400970609     C                   SETON                                        2090
018500970609     C                   MOVEL     msg(1)        $msg
018600970609     C                   GOTO      finct1
018700970609e   1C                   ENDIF
018800970609     C*---
018900970609     C* filiale cliente
019000970609     C*---
019100970609     C                   MOVEL     v1cfil        wfil
019200970609     C                   MOVEL     *blanks       wdfil
019300971128     C                   EXSR      chktfp
019400970609     C                   MOVEL     wfil          v1cfil
019500970609     C                   MOVEL     wdfil         v1dfil
019600970609if  1C                   IF        $err='1'
019700970609     C                   SETON                                        2190
019800970609     C                   MOVEL     msg(2)        $msg
019900970609     C                   GOTO      finct1
020000970609e   1C                   ENDIF
020100970609     C*---
020200970609     C* tracciato archivio
020300970609     C*---
020400970609if  1c                   IF        v1cfmr<>'V' AND
020500970609     c                             v1cfmr<>'N' AND
020600970609     c                             v1cfmr<>'E'
020700970609     C                   SETON                                        2290
020800970609     C                   MOVEL     msg(3)        $msg
020900970609     C                   MOVEL     '1'           $err
021000970609     C                   GOTO      finct1
021100970609e   1C                   ENDIF
021200970609     C*---
021300970609     C* solo aggiornamento cappario
021400970609     C*---
021500970609if  1c                   IF        v1csac<>'S' AND
021600970609     c                             v1csac<>'N'
021700970610     C                   SETON                                        2390
021800970609     C                   MOVEL     msg(3)        $msg
021900970609     C                   MOVEL     '1'           $err
022000970609     C                   GOTO      finct1
022100970609e   1C                   ENDIF
022200970609     C*
022300970609     C     finct1        ENDSR
022400970609     c*--------------------------------------------------------------------------------------------*
022500970609     c* chkda8 - controlla la data (8)
022600970609     c*--------------------------------------------------------------------------------------------*
022700970609     C     chkda8        BEGSR
022800970609     C*
022900970609     C                   Z-ADD     wda8          g08dat
023000970609     C                   Z-ADD     *zeros        g08inv
023100970609     C                   MOVEL     '0'           g08err
023200970609     C                   CALL      'XSRDA8'
023300970609     C                   PARM                    wlbda8
023400970609     C                   Z-ADD     g08dat        wda8
023500971002     C                   Z-ADD     g08inv        da8w
023600970609IF  1C                   IF        g08err='1'
023700970609     C                   MOVEL     '1'           $err
023800970609E   1C                   ENDIF
023900970609     C*
024000970609     C                   ENDSR
024100970609     c*--------------------------------------------------------------------------------------------*
024200971128     c* chktfp - controlla il terminal pertenza
024300970609     c*--------------------------------------------------------------------------------------------*
024400971128     C     chktfp        BEGSR
024500970609     C*
024600970609     C* richiesto elenco
024700970610     C                   SETOFF                                           99
024800970610     C     '?'           SCAN      wfil                                   99
024900970610if  1C                   IF        *in99
025000971203     c                   CLEAR                   p24cod
025100971203     c                   CLEAR                   p24tip
025200971203     c                   CLEAR                   p24des
025300971203     c                   CLEAR                   p24pra
025400971203     C                   CALL      'TNSD24R'
025500971203     C                   PARM                    p24cod                         *codice
025600971203     C                   PARM                    p24tip                         *tipo
025700971203     C                   PARM                    p24des                         *filiale
025800971203     C                   PARM                    p24pra                         *ditta
025900971203     C                   MOVEL     p24cod        wfil
026000971203     C                   MOVEL     p24des        wdfil
026100970610     c                   MOVEL     '1'           $tesvd                         *riemettere testata
026200970609e   1C                   ENDIF
026300970609     C*
026400970609     C* solo numeri
026500970610     C                   SETOFF                                       99
026600970610     C                   TESTN                   wfil                 99
026700970610if  1c                   IF        NOT *in99                                    *non è un numero
026800970609     C                   MOVEL     '1'           $err
026900970609x   1c                   ELSE                                                   *è numero da testn
027000970609     c                   MOVE      wfil          wa1
027100970609     c                   IF        wa1<'0'                                      *trovata una lettera
027200970609     C                   MOVEL     '1'           $err
027300970609x   2c                   ELSE                                                   *numero
027400971128     c*
027500971128     c* reperisce il terminal partenza
027600971128     C                   CLEAR                   dslv55
027700971128     C                   MOVEL     'P'           d55tpt
027800971128     C                   MOVEL     wfil          d55lin
027900971128     C                   Z-ADD     datcor        d55drf
028000971128     C                   CALL      'FNLV55R'
028100971128     C                   PARM                    dSlv55
028200971128if  3c                   IF        d55err <> *blanks
028300971128     c                   MOVEL     wfil          d55tfp
028400971128e   3c                   ENDIF
028500970609     C*
028600970609     C* controlla esistenza codici
028700970609     C                   MOVEL     Wfil          korfil
028800970609     C     keyorg        chain     azorg01l                           99
028900970609if  3c                   IF        NOT *in99 AND                                *trovata
029000970609     c                             orgfva=*blanks AND                           *non annullata
029100970609     c                             orgfag<>'V' AND                              *no varie
029200971128     c                             orgfil=d55tfp                                *filiale terminal
029300970609     c                   MOVEL     orgdes        wdfil                          *descrizione
029400970609x   3c                   ELSE
029500970609     c                   MOVEL     '1'           $err
029600970609e   3c                   ENDIF
029700970609e   2c                   ENDIF
029800970609e   1c                   ENDIF
029900970609     C*
030000970609     C                   ENDSR
030100970609     c*--------------------------------------------------------------------------------------------*
030200970609     C* f03ges - gestione tasto funzionale f03 ==> Fine
030300970609     c*--------------------------------------------------------------------------------------------*
030400970609     C     f03ges        BEGSR
030500970609     C*
030600970609     C                   MOVEL     'S'           $fine                          *fine programma
030700970609     C*
030800970609     C                   ENDSR
030900970609     c*--------------------------------------------------------------------------------------------*
031000970609     C* f06ges - gestione tasto funzionale f06 ==> Conferma
031100970609     c*--------------------------------------------------------------------------------------------*
031200970609     C     f06ges        BEGSR
031300970609     c*
031400970609     c* imposta i parametri di lancio
031500970618     c                   MOVEL     driv1c        p50dri                         *data riferimento
031600970609     c                   MOVEL     v1cfil        p50fil                         *filiale cliente
031700970609     c                   MOVEL     v1cfmr        p50fmr                         *tracciato record
031800970609     c                   MOVEL     v1csac        p50sac                         *solo agg.cappario
031900010703     c                   MOVEL     v1cbos        p50bos                         *cliente BAR o SDI
032000010918     c                   MOVEL     v1cazz        p50azz                         *azzeramento linee
032100970609     c*
032200970609     c* lancia il programma
032300970609     c                   MOVEL     dspar50       kpjbu                          *parametri di lancio
032400970609     c                   MOVEL     'SI51'        kcoaz                          *codice azione
032500010924     c***                CALL      'TISI51C'
032600010924     c                   CALL      'BCH10'
032700970609     c                   PARM                    kpjba
032800970722     c                   MOVEL     'S'           $fine                          *fine programma
032900970609     c*
033000970609     C                   ENDSR
033100970609     c*--------------------------------------------------------------------------------------------*
033200970609     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
033300970609     c*--------------------------------------------------------------------------------------------*
033400970609     C     f12ges        BEGSR
033500970609     C*
033600970609     C                   MOVEL     'S'           $fine                          *fine programma
033700970609     C*
033800970609     C                   ENDSR
033900970609     c*--------------------------------------------------------------------------------------------*
034000970609     C* impvd0 - Impostazioni testata
034100970609     c*--------------------------------------------------------------------------------------------*
034200970609     C     impvd0        BEGSR
034300970609     C*
034400970609     C                   MOVEL     '1'           $tesvd                         *riemettere testata
034500970609     C*
034600970609     C                   ENDSR
034700020919     C*--------------------------------------------------------------------------------------------*
034800020919     C* REPERISCE I DATI UTENTE
034900020919     C*--------------------------------------------------------------------------------------------*
035000020919     C     REPDATIUTE    BEGSR
035100020919     C*
035200020919     C* INIZIALIZZA VARIABILI DI WRK
035300020919     C                   CLEAR                   TIBS34DS
035400020919     C                   CLEAR                   AZUTEDS
035500020919     C                   CLEAR                   DDATIUTE
035600020919     C*
035700020919     C     *DTAARA       DEFINE    §azute        azuteds
035800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
035900020919     C                   IN(E)     *DTAARA
036000020919if  1C                   IF        %Error
036100020919     c                   EVAL      I34Tla = 'L'
036200020919     C                   CALL      'TIBS34R'
036300020919     C                   PARM                    Tibs34Ds
036400020919     C                   IN        *DTAARA
036500020919e   1C                   ENDIF
036600020919     C*
036700020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
036800020919     C                   MOVEL(P)  RSUT          DSFIRS           20
036900020919     C*
037000020919     C                   ENDSR
037100970609     c*--------------------------------------------------------------------------------------------*
037200970609     C* *inzsr - operazioni iniziali
037300970609     c*--------------------------------------------------------------------------------------------*
037400970609     C     *inzsr        BEGSR
037500970609     c*
037600970609     c* ricevimento parametri
037700970609     c     *ENTRY        PLIST
037800970609     c                   PARM                    kpjba
037900970609     C*
038000970609     C* chiavi di lettura
038100970609     C     keyorg        klist                                                  *azorg00f
038200970609     C                   kfld                    korfil                         *filiale
038300970609     C*
038400970609     C* reperisce la data corrente
038500970609     C                   TIME                    wn14                           *ora (6)+ data (8)
038600970609     C                   MOVE      wn14          wn8                            *data (8) in g/m/aa
038700970610     C                   Z-ADD     wn8           datcor                         *Data corrente
038800971201     C*
038900971201     C* reperimento dati utente
039000020919     C                   EXSR      REPDATIUTE
039100970609     C*
039200970609     C                   ENDSR
039300970609     O*--------------------------------------------------------------------------------------------*
039400970609** -msg-
039500970609TISI50R- Data errata                                                          1
039600970609TISI50R- Filiale errata o inesistente                                         2
039700970609TISI50R- Immettere uno dei valori possibili                                   3
039800970609TISI50R- 4
039900970609TISI50R- 5
040000970609TISI50R- 6
040100970609TISI50R- 7
040200970609TISI50R- 8
040300970609TISI50R- 9
040400970609TISI50R- 10
