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
006400020624     d*dsfil         E DS                                                       *reperimento dati
006500000000     d wlbda8          DS                  INZ                                  *controlla data
006600000000     d  g08dat                        8  0
006700000000     d  g08inv                        8  0
006800000000     d  g08err                        1
006900000000     d  g08tgi                        5  0
007000000000     d dsms00        E DS                  EXTNAME(fims00ds)                    *ds di procedura
007100000000     d kpjba         E DS                                                       *architettura
007200020624     D* SCHIERE PER CARICAMENTO L1
007300020624     D L1              S              3  0 DIM(30)
007400020624     D* DS PER TRUL06R - CARICAMENTO £1
007500020624     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007600020624     D  LIN                    1     90  0
007700020624     D                                     DIM(30)
007800020624      *-----------------------------------------------------------
007900020624      * Ds di riferimento al file esterno AzUte00F
008000020624      *--------
008100020624     D AzuteDs       E DS                  ExtName(AzUte00F)
008200020624      *-----------------------------------------------------------
008300020624      * Ds per dati organigramma
008400020624      *--------
008500020624     D DDatiUte      E DS
008600020624      *-----------------------------------------------------------
008700020624      * Parametri per richiamo al pgm di controllo profilo utenti
008800020624      *--------
008900020624     D Tibs34Ds      E DS                  Inz
009000000000     c*--------------------------------------------------------------------------------------------*
009100000000     c* Main lines
009200000000     c*--------------------------------------------------------------------------------------------*
009300000000     C*
009400000000     C* impostazioni testata
009500000000     C                   EXSR      impvd0
009600000000     C*
009700000000     C* impostazioni prima videata
009800000000     C                   EXSR      impvd1
009900000000     C*
010000000000     C* ciclo fino a richiesta di fine elaborazione
010100000000do  1C                   DOU       $fine='S'
010200000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
010300000000     C                   ENDCS
010400000000e   1C                   ENDDO
010500000000     C*
010600000000     C                   SETON                                        lr
010700000000     c*--------------------------------------------------------------------------------------------*
010800000000     c* impvd1 - impostazioni prima videata
010900000000     c*--------------------------------------------------------------------------------------------*
011000000000     C     impvd1        BEGSR
011100000713     C*
011200000713     C* filiale -> filiale elaboratore
011300020624     c***                Z-ADD     dsfifi        v1cfil
011400020726      * Propongo a vedeo se non sono in sede
011500020726     c                   IF        Simfel <> 0
011600020624     c                   Z-ADD     dutpou        v1cfil                         *filiale elaboratore
011700020726     c                   endif
011800000000     C*
011900000105     C* data inizio -> data corrente - 1 giorno
012000000105     C                   Z-ADD     datcorpre     g08inv
012100000105     C                   Z-ADD     *zeros        g08dat
012200000105     C                   MOVEL     '3'           g08err
012300000105     C                   CALL      'XSRDA8'
012400000105     C                   PARM                    wlbda8
012500000105     C                   Z-ADD     g08dat        v1cdli                         *data lettura inizio
012600000000     C                   Z-ADD     0000          v1coli                         *ora  lettura inizio
012700000105     c*
012800000105     c* data fine -> data corrente
012900000105     C                   Z-ADD     datcor        g08inv
013000000105     C                   Z-ADD     *zeros        g08dat
013100000105     C                   MOVEL     '3'           g08err
013200000105     C                   CALL      'XSRDA8'
013300000105     C                   PARM                    wlbda8
013400000105     C                   Z-ADD     g08dat        v1cdlf                         *data lettura fine
013500000105     C                   Z-ADD     corora        v1colf                         *ora  lettura fine
013600000105     C*
013700000105     C                   EXSR      setvd1
013800000105     C                   MOVEL     '1'           $tipvid                        *gestione video
013900000000     C*
014000000000     C                   ENDSR
014100000000     c*--------------------------------------------------------------------------------------------*
014200000000     c* setvd1 - impostazioni indicatori prima videata
014300000000     c*--------------------------------------------------------------------------------------------*
014400000000     C     setvd1        BEGSR
014500000000     C*
014600000000     C                   SETOFF                                       202122     *errori
014700000000     C                   SETOFF                                       2324
014800000000     C                   SETOFF                                       30
014900000000     C                   SETOFF                                       90         *generico di errore
015000000000     C                   MOVEL     *blanks       $msg                            *messaggio
015100000000     C*
015200000000     C                   ENDSR
015300000000     c*--------------------------------------------------------------------------------------------*
015400000000     C* gesvd1 - gestione prima videata
015500000000     c*--------------------------------------------------------------------------------------------*
015600000000     C     gesvd1        BEGSR
015700000000     C*
015800000000     C* emissione videata
015900000000if  1C                   IF        $tesvd='1'
016000000000     C                   WRITE     ms11d00                                      *testata
016100000000     C                   MOVEL     '0'           $tesvd
016200000000e   1C                   ENDIF
016300000000     C                   EXFMT     ms11d01                                      *video
016400000000     C                   EXSR      setvd1
016500000000     C* F03-Fine
016600000000if  1C                   IF        *inkc
016700000000     C                   EXSR      f03ges
016800000000     C                   GOTO      finvd1
016900000000e   1C                   ENDIF
017000000000     C* F12-Ritorno
017100000000if  1C                   IF        *inkl
017200000000     C                   EXSR      f12ges
017300000000     C                   GOTO      finvd1
017400000000e   1C                   ENDIF
017500000000     C* Enter-Controlla
017600000000     C                   EXSR      ctrvd1
017700000000if  1C                   IF        *in90                                        *errori
017800000000     C                   GOTO      finvd1
017900000000x   1c                   ELSE
018000000000     c                   EXSR      entges
018100000000e   1C                   ENDIF
018200000000     C*
018300000000     C     finvd1        ENDSR
018400000000     c*--------------------------------------------------------------------------------------------*
018500000000     C* ctrvd1 - controlla prima videata
018600000000     c*--------------------------------------------------------------------------------------------*
018700000000     C     ctrvd1        BEGSR
018800000000     C*---
018900000000     C* punto operativo
019000000000     C*---
019100000000     C                   Z-ADD     v1cfil        wfil
019200000000     C                   MOVEL     *blanks       wdfil
019300000000     C                   MOVEL     '0'           werr                           *no errore
019400000000     C                   EXSR      chkfil
019500000000     C                   MOVEL     wdfil         v1dfil
019600000713if  1C                   IF        werr='1'
019700000000     C                   SETON                                        3090
019800000000     C                   MOVEL     msg(4)        $msg
019900000000     C                   GOTO      finct1
020000000713e   1C                   ENDIF
020100000000     C*---
020200000000     C* data lettura iniziale
020300000000     C*---
020400000000     C                   Z-ADD     *zeros        dliv1c
020500000000     C                   Z-ADD     v1cdli        wda8
020600000000     C                   Z-ADD     *zeros        da8w
020700000000     C                   MOVEL     '0'           werr                           *no errore
020800000000     C                   EXSR      chkda8
020900000000     C                   Z-ADD     wda8          v1cdli
021000000000     C                   Z-ADD     da8w          dliv1c
021100000000if  1C                   IF        werr='1'
021200000000     C                   SETON                                        2090
021300000000     C                   MOVEL     msg(1)        $msg
021400000000     C                   GOTO      finct1
021500000000e   1C                   ENDIF
021600000000     C*---
021700000000     C* ora lettura iniziale
021800000000     C*---
021900000000     C                   Z-ADD     v1coli        wor4
022000000000     C                   MOVEL     '0'           werr                           *no errore
022100000000     C                   EXSR      chkor4
022200000000if  1C                   IF        werr='1'
022300000000     C                   SETON                                        2190
022400000000     C                   MOVEL     msg(2)        $msg
022500000000     C                   GOTO      finct1
022600000000e   1C                   ENDIF
022700000000     C*---
022800000000     C* data lettura finale
022900000000     C*---
023000000000     C                   Z-ADD     *zeros        dlfv1c
023100000000     C                   Z-ADD     v1cdlf        wda8
023200000000     C                   Z-ADD     *zeros        da8w
023300000000     C                   MOVEL     '0'           werr                           *no errore
023400000000     C                   EXSR      chkda8
023500000000     C                   Z-ADD     wda8          v1cdlf
023600000000     C                   Z-ADD     da8w          dlfv1c
023700000000if  1C                   IF        werr='1'
023800000000     C                   SETON                                        2290
023900000000     C                   MOVEL     msg(1)        $msg
024000000000     C                   GOTO      finct1
024100000000e   1C                   ENDIF
024200000000     C*---
024300000000     C* ora lettura finale
024400000000     C*---
024500000000     C                   Z-ADD     v1colf        wor4
024600000000     C                   MOVEL     '0'           werr                           *no errore
024700000000     C                   EXSR      chkor4
024800000000if  1C                   IF        werr='1'
024900000000     C                   SETON                                        2390
025000000000     C                   MOVEL     msg(2)        $msg
025100000000     C                   GOTO      finct1
025200000000e   1C                   ENDIF
025300000000     C*---
025400000000     C* periodo iniziale/finale
025500000000     C*---
025600000000     c                   Z-ADD     *zeros        n6
025700000000     c                   MOVEL     v1coli        n6
025800000000     c     *ISO          MOVE      dliv1c        wdata
025900000000     c     *ISO          MOVE      n6            wora
026000000000     c                   MOVEL     wtempo        wtempoini
026100000000     c                   Z-ADD     *zeros        n6
026200000000     c                   MOVEL     v1colf        n6
026300000000     c     *ISO          MOVE      dlfv1c        wdata
026400000000     c     *ISO          MOVE      n6            wora
026500000000     c                   MOVEL     wtempo        wtempofin
026600000000if  1C                   IF        wtempofin < wtempoini
026700000000     C                   SETON                                        202122
026800000000     C                   SETON                                        2390
026900000000     C                   MOVEL     msg(3)        $msg
027000000000     C                   GOTO      finct1
027100000000e   1C                   ENDIF
027200000000     C*
027300000000     C     finct1        ENDSR
027400000000     c*--------------------------------------------------------------------------------------------*
027500000000     c* chkfil - controlla la filiale
027600000000     c*--------------------------------------------------------------------------------------------*
027700000000     C     chkfil        BEGSR
027800000000     C*
027900000000     C                   Z-ADD     wfil          krgfil
028000000000     c     keyorg        CHAIN     azorg01l                           99
028100000000if  1c                   IF        *in99 OR                                     *inesistente O
028200000000     c                             orgfva <> *blanks OR                         *annullata   O
028300000000     c                             orgfag = 'V'                                 *varia
028400000000     C                   MOVEL     '1'           werr
028500000000x   1C                   ELSE
028600000000     C                   MOVEL     orgdes        wdfil                          *descrizione
028700000000e   1C                   ENDIF
028800020726      * Verifico se P.O è gestibile solo se non sono in sede
028900020726     C                   IF        werr = '0'  and Simfel <> 0
029000020624     C     wfil          lookup    L1                                     35
029100020624     C  N35              MOVEL     '1'           werr
029200020624     C                   ENDIF
029300020624     C*
029400000000     C*
029500000000     C                   ENDSR
029600000000     c*--------------------------------------------------------------------------------------------*
029700000000     c* chkda8 - controlla la data (8)
029800000000     c*--------------------------------------------------------------------------------------------*
029900000000     C     chkda8        BEGSR
030000000000     C*
030100000000     C                   Z-ADD     wda8          g08dat
030200000000     C                   Z-ADD     *zeros        g08inv
030300000000     C                   MOVEL     '0'           g08err
030400000000     C                   CALL      'XSRDA8'
030500000000     C                   PARM                    wlbda8
030600000000     C                   Z-ADD     g08dat        wda8
030700000000if  1C                   IF        g08err='1'
030800000000     C                   MOVEL     '1'           werr
030900000000x   1C                   ELSE
031000000000     C                   Z-ADD     g08inv        da8w
031100000000e   1C                   ENDIF
031200000000     C*
031300000000     C                   ENDSR
031400000000     c*--------------------------------------------------------------------------------------------*
031500000000     c* chkor4 - controlla l'ora (4)
031600000000     c*--------------------------------------------------------------------------------------------*
031700000000     C     chkor4        BEGSR
031800000000     C*
031900000000     c                   MOVEL     wor4          n2                             *ore    (0÷23)
032000000000if  1c                   IF        n2 < 0 OR n2 > 23
032100000000     C                   MOVEL     '1'           werr                           *errore
032200000000e   1C                   ENDIF
032300000000     c                   MOVE      wor4          n2                             *minuti (0÷59)
032400000000if  1c                   IF        n2 < 0 OR n2 > 59
032500000000     C                   MOVEL     '1'           werr                           *errore
032600000000e   1C                   ENDIF
032700000000     C*
032800000000     C                   ENDSR
032900000000     c*--------------------------------------------------------------------------------------------*
033000000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
033100000000     c*--------------------------------------------------------------------------------------------*
033200000000     C     f03ges        BEGSR
033300000000     C*
033400000000     C                   MOVEL     'S'           $fine                          *fine programma
033500000000     C*
033600000000     C                   ENDSR
033700000000     c*--------------------------------------------------------------------------------------------*
033800000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
033900000000     c*--------------------------------------------------------------------------------------------*
034000000000     C     f12ges        BEGSR
034100000000     C*
034200000000     C                   MOVEL     'S'           $fine                          *fine programma
034300000000     C*
034400000000     C                   ENDSR
034500000000     c*--------------------------------------------------------------------------------------------*
034600000000     C* entges - gestione tasto funzionale enter ==> Invio
034700000000     c*--------------------------------------------------------------------------------------------*
034800000000     C     entges        BEGSR
034900000000     c*
035000000000     c* imposta la ds di procedura
035100000000     c                   MOVEL     *blanks       d00op1                         *livello programma
035200000000     c                   MOVEL     '0'           d00f03                         *premuto F03
035300000000     c                   MOVEL     '0'           d00f12                         *premuto F12
035400000000     c                   MOVEL     '0'           d00err                         *errore
035500000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
035600000000     c                   Z-ADD     v1cfil        d00fil                         *filiale
035700000000     c                   Z-ADD     dliv1c        d00dli                         *data lettura inizio
035800000000     c                   Z-ADD     v1coli        d00oli                         *ora  lettura inizio
035900000000     c                   Z-ADD     dlfv1c        d00dlf                         *data lettura fine
036000000000     c                   Z-ADD     v1colf        d00olf                         *ora  lettura fine
036100020624     c                   Z-ADD     *zeros        d00civ                         *ora  lettura fine
036200000000     c                   CALL      'FIMS12R'
036300000000     c                   PARM                    kpjba
036400000000     c                   PARM                    dsms00
036500000000     C                   MOVEL     '1'           $tesvd                          *riemettere testata
036600000000     C*
036700000000     C* se ritorna premuto f3, chiude il programma
036800000000IF  1c                   IF        d00f03='1'                                    *premuto f03
036900000000     c                   MOVEL     'S'           $fine                           *fine pgm
037000000000X   1c                   ELSE                                                    *non premuto f03
037100000000     C                   MOVEL     d00msg        $msg                            *messaggio
037200000000e   1c                   ENDIF
037300000000     c*
037400000000     C                   ENDSR
037500000000     c*--------------------------------------------------------------------------------------------*
037600000000     C* impvd0 - Impostazioni testata
037700000000     c*--------------------------------------------------------------------------------------------*
037800000000     C     impvd0        BEGSR
037900000000     C*
038000000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
038100000000     C*
038200000000     C                   ENDSR
038300000000     c*--------------------------------------------------------------------------------------------*
038400000000     C* *inzsr - operazioni iniziali
038500000000     c*--------------------------------------------------------------------------------------------*
038600000000     C     *inzsr        BEGSR
038700000000     c*
038800000000     c* ricevimento parametri
038900000000     c     *ENTRY        PLIST
039000000000     c                   PARM                    kpjba
039100000000     c                   PARM                    dsms00
039200020624      *
039300020624      * Reperisco dati job
039400020624     C                   EXSR      DatiJob
039500000000     C*
039600000000     C* reperimento dati utente
039700000000     c     keyorg        KLIST                                                  *azorg01l
039800000000     c                   KFLD                    krgfil                          -filiale
039900000000     c     keytbe        KLIST                                                  *tntbe01l
040000000000     c                   KFLD                    kbecod                          -codice
040100000000     c                   KFLD                    kbeke1                          -chiave uno
040200000000     c                   KFLD                    kbeke2                          -chiave due
040300000000     c                   KFLD                    kbelin                          -lingua
040400000000     c                   KFLD                    kbesif                          -s.informativo
040500020624     C*
040600020624     C****  CARICO TABELLA FILIALI GESTITE £1  ****
040700020624     C                   CLEAR                   DSUL06
040800020624     C                   MOVE      '£1'          D06COD
040900020624     C                   MOVEL     SIMFEL        D06KEY
041000020624     C                   MOVEL     DSUL06        KPJBU
041100020624     C                   CALL      'TRUL06R'
041200020624     C                   PARM                    KPJBA
041300020624     C                   MOVEL     KPJBU         DSUL06
041400020624     C                   MOVEA     LIN           L1
041500000000     C*
041600000000     C* reperimento dati utente
041700020624     C***                CALL      'XFIL'
041800020624     C***                PARM                    dsfil
041900000000     C*
042000000000     C* reperisce la data e ora corrente
042100000000     C                   TIME                    n14                            *ora (6)+ data (8)
042200000000     C                   MOVEL     n14           corora                         *ora  (4) in hh:mm
042300000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
042400000000     C                   Z-ADD     n8            g08dat
042500000000     C                   Z-ADD     *zeros        g08inv
042600000000     C                   MOVEL     '0'           g08err
042700000000     C                   CALL      'XSRDA8'
042800000000     C                   PARM                    wlbda8
042900000000     C                   Z-ADD     g08inv        datcor                         *data in aa/m/g
043000000105     C*
043100000105     C* calcola un giorno indietro alla data corrente
043200000105     c     *ISO          MOVE      datcor        wdata
043300000105     c                   SUBDUR    1:*Days       wtempo
043400000105     c     *ISO          MOVE      wdata         datcorpre
043500000105     C*
043600000000     C                   ENDSR
043700020624      *----------------------------------------------------------------
043800020624      *  Reperisco Dati del job (Utente/Operativi)
043900020624      *----------------------------------------------------------------
044000020624     C     DatiJob       BegSr
044100020624      *
044200020624     C     *dtaara       define    §azute        azuteds
044300020624     C     *dtaara       define    §datiute      ddatiute
044400020624      *
044500020624     C                   in(E)     *dtaara
044600020624     C                   IF        %Error or RSUT = *blanks
044700020624     C                   call      'TIBS34R'
044800020624     C                   parm                    Tibs34Ds
044900020624     C                   in        *dtaara
045000020624     C                   ENDIF
045100020624      *
045200020624     C                   endsr
045300000000     O*--------------------------------------------------------------------------------------------*
045400000000** -msg-
045500000000FIMS11R- Data errata                                                          1
045600000000FIMS11R- Ora errata                                                           2
045700000000FIMS11R- Il periodo finale deve essere maggiore/uguale di quello iniziale     3
045800000000FIMS11R- Filiale errata o non in gestione                                     4
045900000000FIMS11R- Codice errato o inesistente                                          5
046000000000FIMS11R- 6
046100000000FIMS11R- 7
046200000000FIMS11R- 8
046300000000FIMS11R- 9
046400000000FIMS11R- 10
