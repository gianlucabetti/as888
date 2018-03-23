000100000000     h*--------------------------------------------------------------------------------------------*
000200000000     h* Saldi letture pacchi civetta VanDerLande - Filtro                                          *
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* DATA BASE
000700000000     f*--------------------------------------------------------------------------------------------*
000800000000     ffims11d   CF   E             WORKSTN
000900000000     fazorg01l  IF   E           K DISK
001000000000     ftntbe01l  IF   E           K DISK
001100000000     d*--------------------------------------------------------------------------------------------*
001200000000     d* Data structure
001300000000     d*--------------------------------------------------------------------------------------------*
001400000000     d*---
001500000000     d* schiere
001600000000     d*---
001700000000     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001800000000     d*---
001900000000     d* Variabili riferite al data base
002000000000     d*---
002100000000     d krgfil          S                   LIKE(orgfil)                         *azorg01l
002200000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
002300000000     d kbeke1          S                   LIKE(tbeke1)
002400000000     d kbeke2          S                   LIKE(tbeke2)
002500000000     d kbelin          S                   LIKE(tbelin)
002600000000     d kbesif          S                   LIKE(tbesif)
002700000000     d dliv1c          S                   LIKE(v1cdli)                         *campi video
002800000000     d dlfv1c          S                   LIKE(v1cdlf)
002900000000     d*---
003000000000     d* Variabili di controllo
003100000000     d*---
003200000000     d $fine           S              1    INZ('N')                             *gestione
003300000000     d $tesvd          S              1    INZ('1')                             *riemettere testata
003400000000     d $tipvid         S              1    INZ('1')                             *gestione video
003500000000     d*---
003600000000     d* Variabili a programma
003700000000     d*---
003800000000     d datcor          S              8  0                                      *data corrente a/m/g
003900000105     d datcorpre       S              8  0                                      *d.corrente-1  a/m/g
004000000000     d corora          S              4  0                                      *ora  corrente h:m
004100000000     d n14             S             14  0                                      *numerico 14
004200000000     d n8              S              8  0                                      *numerico 8
004300000000     d n6              S              6  0                                      *numerico 6
004400000000     d n4              S              4  0                                      *numerico 4
004500000000     d n2              S              2  0                                      *numerico 2
004600000000     d werr            S              1    INZ('0')                             *esistenza versioni
004700000000     d wda8            S              8  0                                      *data di 8 in g/m/a
004800000000     d da8w            S              8  0                                      *data di 8 in a/m/g
004900000000     d wor4            S              4  0                                      *ora  di 4 in h:m
005000000000     d wfil            S              3  0                                      *filiale
005100000000     d wdfil           S             20                                         *descrizione filiale
005200000000     d*---
005300000000     d* Variabili temporali
005400000000     d*---
005500000000     D                 DS
005600000000     d wtempo                          Z   INZ(Z'1901-01-01-01.00.00.000000')
005700000000     d  wdata                          D   DATFMT(*ISO) OVERLAY(wtempo)
005800000000     d  wora                           T   TIMFMT(*ISO) OVERLAY(wtempo:12)
005900000000     d wtempoini                       Z   INZ(Z'1901-01-01-01.00.00.000000')
006000000000     d wtempofin                       Z   INZ(Z'1901-01-01-01.00.00.000000')
006100000000     d*---
006200000000     d* Ds
006300000000     d*---
006400000000     d wlbda8          DS                  INZ                                  *controlla data
006500000000     d  g08dat                        8  0
006600000000     d  g08inv                        8  0
006700000000     d  g08err                        1
006800000000     d  g08tgi                        5  0
006900000000     d dsms00        E DS                  EXTNAME(fims00ds)                    *ds di procedura
007000000000     d kpjba         E DS                                                       *architettura
007100020624     D* SCHIERE PER CARICAMENTO L1
007200020624     D L1              S              3  0 DIM(30)
007300020624     D* DS PER TRUL06R - CARICAMENTO £1
007400020624     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007500020624     D  LIN                    1     90  0
007600020624     D                                     DIM(30)
007700020624      *-----------------------------------------------------------
007800020624      * Ds di riferimento al file esterno AzUte00F
007900020624      *--------
008000020624     D AzuteDs       E DS                  ExtName(AzUte00F)
008100020624      *-----------------------------------------------------------
008200020624      * Ds per dati organigramma
008300020624      *--------
008400020624     D DDatiUte      E DS
008500020624      *-----------------------------------------------------------
008600020624      * Parametri per richiamo al pgm di controllo profilo utenti
008700020624      *--------
008800020624     D Tibs34Ds      E DS                  Inz
008900000000     c*--------------------------------------------------------------------------------------------*
009000000000     c* Main lines
009100000000     c*--------------------------------------------------------------------------------------------*
009200000000     C*
009300000000     C* impostazioni testata
009400000000     C                   EXSR      impvd0
009500000000     C*
009600000000     C* impostazioni prima videata
009700000000     C                   EXSR      impvd1
009800000000     C*
009900000000     C* ciclo fino a richiesta di fine elaborazione
010000000000do  1C                   DOU       $fine='S'
010100000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
010200000000     C                   ENDCS
010300000000e   1C                   ENDDO
010400000000     C*
010500000000     C                   SETON                                        lr
010600000000     c*--------------------------------------------------------------------------------------------*
010700000000     c* impvd1 - impostazioni prima videata
010800000000     c*--------------------------------------------------------------------------------------------*
010900000000     C     impvd1        BEGSR
011000000713     C*
011100000713     C* filiale -> filiale elaboratore
011200020726      * Propongo a vedeo se non sono in sede
011300020726     c                   IF        Simfel <> 0
011400020624     c                   Z-ADD     dutpou        v1cfil                         *filiale elaboratore
011500020726     c                   endif
011600000000     C*
011700000105     C* data inizio -> data corrente - 1 giorno
011800000105     C                   Z-ADD     datcorpre     g08inv
011900000105     C                   Z-ADD     *zeros        g08dat
012000000105     C                   MOVEL     '3'           g08err
012100000105     C                   CALL      'XSRDA8'
012200000105     C                   PARM                    wlbda8
012300000105     C                   Z-ADD     g08dat        v1cdli                         *data lettura inizio
012400000000     C                   Z-ADD     0000          v1coli                         *ora  lettura inizio
012500000105     c*
012600000105     c* data fine -> data corrente
012700000105     C                   Z-ADD     datcor        g08inv
012800000105     C                   Z-ADD     *zeros        g08dat
012900000105     C                   MOVEL     '3'           g08err
013000000105     C                   CALL      'XSRDA8'
013100000105     C                   PARM                    wlbda8
013200000105     C                   Z-ADD     g08dat        v1cdlf                         *data lettura fine
013300000105     C                   Z-ADD     corora        v1colf                         *ora  lettura fine
013400000105     C*
013500000105     C                   EXSR      setvd1
013600000105     C                   MOVEL     '1'           $tipvid                        *gestione video
013700000000     C*
013800000000     C                   ENDSR
013900000000     c*--------------------------------------------------------------------------------------------*
014000000000     c* setvd1 - impostazioni indicatori prima videata
014100000000     c*--------------------------------------------------------------------------------------------*
014200000000     C     setvd1        BEGSR
014300000000     C*
014400000000     C                   SETOFF                                       202122     *errori
014500000000     C                   SETOFF                                       2324
014600000000     C                   SETOFF                                       30
014700000000     C                   SETOFF                                       90         *generico di errore
014800000000     C                   MOVEL     *blanks       $msg                            *messaggio
014900000000     C*
015000000000     C                   ENDSR
015100000000     c*--------------------------------------------------------------------------------------------*
015200000000     C* gesvd1 - gestione prima videata
015300000000     c*--------------------------------------------------------------------------------------------*
015400000000     C     gesvd1        BEGSR
015500000000     C*
015600000000     C* emissione videata
015700000000if  1C                   IF        $tesvd='1'
015800000000     C                   WRITE     ms11d00                                      *testata
015900000000     C                   MOVEL     '0'           $tesvd
016000000000e   1C                   ENDIF
016100000000     C                   EXFMT     ms11d01                                      *video
016200000000     C                   EXSR      setvd1
016300000000     C* F03-Fine
016400000000if  1C                   IF        *inkc
016500000000     C                   EXSR      f03ges
016600000000     C                   GOTO      finvd1
016700000000e   1C                   ENDIF
016800000000     C* F12-Ritorno
016900000000if  1C                   IF        *inkl
017000000000     C                   EXSR      f12ges
017100000000     C                   GOTO      finvd1
017200000000e   1C                   ENDIF
017300000000     C* Enter-Controlla
017400000000     C                   EXSR      ctrvd1
017500000000if  1C                   IF        *in90                                        *errori
017600000000     C                   GOTO      finvd1
017700000000x   1c                   ELSE
017800000000     c                   EXSR      entges
017900000000e   1C                   ENDIF
018000000000     C*
018100000000     C     finvd1        ENDSR
018200000000     c*--------------------------------------------------------------------------------------------*
018300000000     C* ctrvd1 - controlla prima videata
018400000000     c*--------------------------------------------------------------------------------------------*
018500000000     C     ctrvd1        BEGSR
018600000000     C*---
018700000000     C* punto operativo
018800000000     C*---
018900000000     C                   Z-ADD     v1cfil        wfil
019000000000     C                   MOVEL     *blanks       wdfil
019100000000     C                   MOVEL     '0'           werr                           *no errore
019200000000     C                   EXSR      chkfil
019300000000     C                   MOVEL     wdfil         v1dfil
019400000713if  1C                   IF        werr='1'
019500000000     C                   SETON                                        3090
019600000000     C                   MOVEL     msg(4)        $msg
019700000000     C                   GOTO      finct1
019800000713e   1C                   ENDIF
019900000000     C*---
020000000000     C* data lettura iniziale
020100000000     C*---
020200000000     C                   Z-ADD     *zeros        dliv1c
020300000000     C                   Z-ADD     v1cdli        wda8
020400000000     C                   Z-ADD     *zeros        da8w
020500000000     C                   MOVEL     '0'           werr                           *no errore
020600000000     C                   EXSR      chkda8
020700000000     C                   Z-ADD     wda8          v1cdli
020800000000     C                   Z-ADD     da8w          dliv1c
020900000000if  1C                   IF        werr='1'
021000000000     C                   SETON                                        2090
021100000000     C                   MOVEL     msg(1)        $msg
021200000000     C                   GOTO      finct1
021300000000e   1C                   ENDIF
021400000000     C*---
021500000000     C* ora lettura iniziale
021600000000     C*---
021700000000     C                   Z-ADD     v1coli        wor4
021800000000     C                   MOVEL     '0'           werr                           *no errore
021900000000     C                   EXSR      chkor4
022000000000if  1C                   IF        werr='1'
022100000000     C                   SETON                                        2190
022200000000     C                   MOVEL     msg(2)        $msg
022300000000     C                   GOTO      finct1
022400000000e   1C                   ENDIF
022500000000     C*---
022600000000     C* data lettura finale
022700000000     C*---
022800000000     C                   Z-ADD     *zeros        dlfv1c
022900000000     C                   Z-ADD     v1cdlf        wda8
023000000000     C                   Z-ADD     *zeros        da8w
023100000000     C                   MOVEL     '0'           werr                           *no errore
023200000000     C                   EXSR      chkda8
023300000000     C                   Z-ADD     wda8          v1cdlf
023400000000     C                   Z-ADD     da8w          dlfv1c
023500000000if  1C                   IF        werr='1'
023600000000     C                   SETON                                        2290
023700000000     C                   MOVEL     msg(1)        $msg
023800000000     C                   GOTO      finct1
023900000000e   1C                   ENDIF
024000000000     C*---
024100000000     C* ora lettura finale
024200000000     C*---
024300000000     C                   Z-ADD     v1colf        wor4
024400000000     C                   MOVEL     '0'           werr                           *no errore
024500000000     C                   EXSR      chkor4
024600000000if  1C                   IF        werr='1'
024700000000     C                   SETON                                        2390
024800000000     C                   MOVEL     msg(2)        $msg
024900000000     C                   GOTO      finct1
025000000000e   1C                   ENDIF
025100000000     C*---
025200000000     C* periodo iniziale/finale
025300000000     C*---
025400000000     c                   Z-ADD     *zeros        n6
025500000000     c                   MOVEL     v1coli        n6
025600000000     c     *ISO          MOVE      dliv1c        wdata
025700000000     c     *ISO          MOVE      n6            wora
025800000000     c                   MOVEL     wtempo        wtempoini
025900000000     c                   Z-ADD     *zeros        n6
026000000000     c                   MOVEL     v1colf        n6
026100000000     c     *ISO          MOVE      dlfv1c        wdata
026200000000     c     *ISO          MOVE      n6            wora
026300000000     c                   MOVEL     wtempo        wtempofin
026400000000if  1C                   IF        wtempofin < wtempoini
026500000000     C                   SETON                                        202122
026600000000     C                   SETON                                        2390
026700000000     C                   MOVEL     msg(3)        $msg
026800000000     C                   GOTO      finct1
026900000000e   1C                   ENDIF
027000000000     C*
027100000000     C     finct1        ENDSR
027200000000     c*--------------------------------------------------------------------------------------------*
027300000000     c* chkfil - controlla la filiale
027400000000     c*--------------------------------------------------------------------------------------------*
027500000000     C     chkfil        BEGSR
027600000000     C*
027700000000     C                   Z-ADD     wfil          krgfil
027800000000     c     keyorg        CHAIN     azorg01l                           99
027900000000if  1c                   IF        *in99 OR                                     *inesistente O
028000000000     c                             orgfva <> *blanks OR                         *annullata   O
028100000000     c                             orgfag = 'V'                                 *varia
028200000000     C                   MOVEL     '1'           werr
028300000000x   1C                   ELSE
028400000000     C                   MOVEL     orgdes        wdfil                          *descrizione
028500000000e   1C                   ENDIF
028600020726      * Verifico se P.O è gestibile solo se non sono in sede
028700020726     C                   IF        werr = '0'  and Simfel <> 0
028800020624     C     wfil          lookup    L1                                     35
028900020624     C  N35              MOVEL     '1'           werr
029000020624     C                   ENDIF
029100020624     C*
029200000000     C*
029300000000     C                   ENDSR
029400000000     c*--------------------------------------------------------------------------------------------*
029500000000     c* chkda8 - controlla la data (8)
029600000000     c*--------------------------------------------------------------------------------------------*
029700000000     C     chkda8        BEGSR
029800000000     C*
029900000000     C                   Z-ADD     wda8          g08dat
030000000000     C                   Z-ADD     *zeros        g08inv
030100000000     C                   MOVEL     '0'           g08err
030200000000     C                   CALL      'XSRDA8'
030300000000     C                   PARM                    wlbda8
030400000000     C                   Z-ADD     g08dat        wda8
030500000000if  1C                   IF        g08err='1'
030600000000     C                   MOVEL     '1'           werr
030700000000x   1C                   ELSE
030800000000     C                   Z-ADD     g08inv        da8w
030900000000e   1C                   ENDIF
031000000000     C*
031100000000     C                   ENDSR
031200000000     c*--------------------------------------------------------------------------------------------*
031300000000     c* chkor4 - controlla l'ora (4)
031400000000     c*--------------------------------------------------------------------------------------------*
031500000000     C     chkor4        BEGSR
031600000000     C*
031700000000     c                   MOVEL     wor4          n2                             *ore    (0÷23)
031800000000if  1c                   IF        n2 < 0 OR n2 > 23
031900000000     C                   MOVEL     '1'           werr                           *errore
032000000000e   1C                   ENDIF
032100000000     c                   MOVE      wor4          n2                             *minuti (0÷59)
032200000000if  1c                   IF        n2 < 0 OR n2 > 59
032300000000     C                   MOVEL     '1'           werr                           *errore
032400000000e   1C                   ENDIF
032500000000     C*
032600000000     C                   ENDSR
032700000000     c*--------------------------------------------------------------------------------------------*
032800000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
032900000000     c*--------------------------------------------------------------------------------------------*
033000000000     C     f03ges        BEGSR
033100000000     C*
033200000000     C                   MOVEL     'S'           $fine                          *fine programma
033300000000     C*
033400000000     C                   ENDSR
033500000000     c*--------------------------------------------------------------------------------------------*
033600000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
033700000000     c*--------------------------------------------------------------------------------------------*
033800000000     C     f12ges        BEGSR
033900000000     C*
034000000000     C                   MOVEL     'S'           $fine                          *fine programma
034100000000     C*
034200000000     C                   ENDSR
034300000000     c*--------------------------------------------------------------------------------------------*
034400000000     C* entges - gestione tasto funzionale enter ==> Invio
034500000000     c*--------------------------------------------------------------------------------------------*
034600000000     C     entges        BEGSR
034700000000     c*
034800000000     c* imposta la ds di procedura
034900000000     c                   MOVEL     *blanks       d00op1                         *livello programma
035000000000     c                   MOVEL     '0'           d00f03                         *premuto F03
035100000000     c                   MOVEL     '0'           d00f12                         *premuto F12
035200000000     c                   MOVEL     '0'           d00err                         *errore
035300000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
035400000000     c                   Z-ADD     v1cfil        d00fil                         *filiale
035500000000     c                   Z-ADD     dliv1c        d00dli                         *data lettura inizio
035600000000     c                   Z-ADD     v1coli        d00oli                         *ora  lettura inizio
035700000000     c                   Z-ADD     dlfv1c        d00dlf                         *data lettura fine
035800000000     c                   Z-ADD     v1colf        d00olf                         *ora  lettura fine
035900020624     c                   Z-ADD     *zeros        d00civ                         *ora  lettura fine
036000000000     c                   CALL      'FIMS12R'
036100000000     c                   PARM                    kpjba
036200000000     c                   PARM                    dsms00
036300000000     C                   MOVEL     '1'           $tesvd                          *riemettere testata
036400000000     C*
036500000000     C* se ritorna premuto f3, chiude il programma
036600000000IF  1c                   IF        d00f03='1'                                    *premuto f03
036700000000     c                   MOVEL     'S'           $fine                           *fine pgm
036800000000X   1c                   ELSE                                                    *non premuto f03
036900000000     C                   MOVEL     d00msg        $msg                            *messaggio
037000000000e   1c                   ENDIF
037100000000     c*
037200000000     C                   ENDSR
037300000000     c*--------------------------------------------------------------------------------------------*
037400000000     C* impvd0 - Impostazioni testata
037500000000     c*--------------------------------------------------------------------------------------------*
037600000000     C     impvd0        BEGSR
037700000000     C*
037800000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
037900000000     C*
038000000000     C                   ENDSR
038100000000     c*--------------------------------------------------------------------------------------------*
038200000000     C* *inzsr - operazioni iniziali
038300000000     c*--------------------------------------------------------------------------------------------*
038400000000     C     *inzsr        BEGSR
038500000000     c*
038600000000     c* ricevimento parametri
038700000000     c     *ENTRY        PLIST
038800000000     c                   PARM                    kpjba
038900000000     c                   PARM                    dsms00
039000020624      *
039100020624      * Reperisco dati job
039200020624     C                   EXSR      DatiJob
039300000000     C*
039400000000     C* reperimento dati utente
039500000000     c     keyorg        KLIST                                                  *azorg01l
039600000000     c                   KFLD                    krgfil                          -filiale
039700000000     c     keytbe        KLIST                                                  *tntbe01l
039800000000     c                   KFLD                    kbecod                          -codice
039900000000     c                   KFLD                    kbeke1                          -chiave uno
040000000000     c                   KFLD                    kbeke2                          -chiave due
040100000000     c                   KFLD                    kbelin                          -lingua
040200000000     c                   KFLD                    kbesif                          -s.informativo
040300020624     C*
040400020624     C****  CARICO TABELLA FILIALI GESTITE £1  ****
040500020624     C                   CLEAR                   DSUL06
040600020624     C                   MOVE      '£1'          D06COD
040700020624     C                   MOVEL     SIMFEL        D06KEY
040800020624     C                   MOVEL     DSUL06        KPJBU
040900020624     C                   CALL      'TRUL06R'
041000020624     C                   PARM                    KPJBA
041100020624     C                   MOVEL     KPJBU         DSUL06
041200020624     C                   MOVEA     LIN           L1
041300000000     C*
041400000000     C* reperisce la data e ora corrente
041500000000     C                   TIME                    n14                            *ora (6)+ data (8)
041600000000     C                   MOVEL     n14           corora                         *ora  (4) in hh:mm
041700000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
041800000000     C                   Z-ADD     n8            g08dat
041900000000     C                   Z-ADD     *zeros        g08inv
042000000000     C                   MOVEL     '0'           g08err
042100000000     C                   CALL      'XSRDA8'
042200000000     C                   PARM                    wlbda8
042300000000     C                   Z-ADD     g08inv        datcor                         *data in aa/m/g
042400000105     C*
042500000105     C* calcola un giorno indietro alla data corrente
042600000105     c     *ISO          MOVE      datcor        wdata
042700000105     c                   SUBDUR    1:*Days       wtempo
042800000105     c     *ISO          MOVE      wdata         datcorpre
042900000105     C*
043000000000     C                   ENDSR
043100020624      *----------------------------------------------------------------
043200020624      *  Reperisco Dati del job (Utente/Operativi)
043300020624      *----------------------------------------------------------------
043400020624     C     DatiJob       BegSr
043500020624      *
043600020624     C     *dtaara       define    §azute        azuteds
043700020624     C     *dtaara       define    §datiute      ddatiute
043800020624      *
043900020624     C                   in(E)     *dtaara
044000020624     C                   IF        %Error or RSUT = *blanks
044100020624     C                   call      'TIBS34R'
044200020624     C                   parm                    Tibs34Ds
044300020624     C                   in        *dtaara
044400020624     C                   ENDIF
044500020624      *
044600020624     C                   endsr
044700000000     O*--------------------------------------------------------------------------------------------*
044800000000** -msg-
044900000000FIMS11R- Data errata                                                          1
045000000000FIMS11R- Ora errata                                                           2
045100000000FIMS11R- Il periodo finale deve essere maggiore/uguale di quello iniziale     3
045200000000FIMS11R- Filiale errata o non in gestione                                     4
045300000000FIMS11R- Codice errato o inesistente                                          5
045400000000FIMS11R- 6
045500000000FIMS11R- 7
045600000000FIMS11R- 8
045700000000FIMS11R- 9
045800000000FIMS11R- 10
