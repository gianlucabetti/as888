000100000000     h*--------------------------------------------------------------------------------------------*
000200101210     h* Saldi letture RPVP                 - Filtro                                          *
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* DATA BASE
000700000000     f*--------------------------------------------------------------------------------------------*
000800101210     ffims24d   CF   E             WORKSTN
000900000000     fazorg01l  IF   E           K DISK
001000000000     ftntbe01l  IF   E           K DISK
001100170320     ffnmsa02l  IF   E           K DISK    USROPN extfile(WFNMSA)
001200000000     d*--------------------------------------------------------------------------------------------*
001300000000     d* Data structure
001400000000     d*--------------------------------------------------------------------------------------------*
001500000000     d*---
001600000000     d* schiere
001700000000     d*---
001800000000     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001900000000     d*---
002000000000     d* Variabili riferite al data base
002100000000     d*---
002200000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
002300000000     d kbeke1          S                   LIKE(tbeke1)
002400000000     d kbeke2          S                   LIKE(tbeke2)
002500000000     d kbelin          S                   LIKE(tbelin)
002600000000     d kbesif          S                   LIKE(tbesif)
002700101213     d kapl            S                   LIKE(msaapl) inz('P')
002800000000     d*---
002900000000     d* Variabili di controllo
003000000000     d*---
003100000000     d $fine           S              1    INZ('N')                             *gestione
003200000000     d $tesvd          S              1    INZ('1')                             *riemettere testata
003300000000     d $tipvid         S              1    INZ('1')                             *gestione video
003400000000     d*---
003500000000     d* Variabili a programma
003600000000     d*---
003700101213     d w1cdtd          S              8  0                                      *data corrente a/m/g
003800101213     d w1cdta          S              8  0                                      *data corrente a/m/g
003900101213     d datcor          S              8  0                                      *data corrente a/m/g
004000000105     d datcorpre       S              8  0                                      *d.corrente-1  a/m/g
004100000000     d n14             S             14  0                                      *numerico 14
004200000000     d n8              S              8  0                                      *numerico 8
004300000000     d werr            S              1    INZ('0')                             *esistenza versioni
004400000000     d wda8            S              8  0                                      *data di 8 in g/m/a
004500000000     d da8w            S              8  0                                      *data di 8 in a/m/g
004600000000     d wfil            S              3  0                                      *filiale
004700000000     d wdfil           S             20                                         *descrizione filiale
004800000000     d*---
004900000000     d* Variabili temporali
005000000000     d*---
005100000000     D                 DS
005200101213     d  dataiso                        D   DATFMT(*ISO)
005300000000     d*---
005400000000     d* Ds
005500000000     d*---
005600000000     d wlbda8          DS                  INZ                                  *controlla data
005700000000     d  g08dat                        8  0
005800000000     d  g08inv                        8  0
005900000000     d  g08err                        1
006000000000     d  g08tgi                        5  0
006100000000     d kpjba         E DS                                                       *architettura
006200020624      *-----------------------------------------------------------
006300020624      * Ds di riferimento al file esterno AzUte00F
006400020624      *--------
006500020624     D AzuteDs       E DS                  ExtName(AzUte00F)
006600020624      *-----------------------------------------------------------
006700020624      * Ds per dati organigramma
006800020624      *--------
006900020624     D DDatiUte      E DS
007000020624      *-----------------------------------------------------------
007100020624      * Parametri per richiamo al pgm di controllo profilo utenti
007200020624      *--------
007300020624     D Tibs34Ds      E DS                  Inz
007400101213     d DVPO          e ds
007500101213     D DSBS02        E DS                  EXTNAME(TIBS02DS)
007600101213     d
007700101213      * - Parametri di elaborazione al batch
007800101213     d Para23          ds                                                       *DS parametri
007900101213     d  PARfil                 1      3  0                                      - filiale
008000101213     d  PARdfv                 4     11  0                                      - data foglio
008100101213     d  PARamb                12     12                                         - ambito spunte
008200101213     d  PARvol                13     19                                         - ambito spunte
008300101213     d  PARpes                20     24                                         - ambito spunte
008400101213      *
008500101214     d
008600170320     d Wfnmsa          s             21    inz('FILTRAGRU /FNMSA02L')
008700000000     c*--------------------------------------------------------------------------------------------*
008800000000     c* Main lines
008900000000     c*--------------------------------------------------------------------------------------------*
009000000000     C*
009100000000     C* impostazioni testata
009200101213     C                   MOVEL     '1'           $tesvd                         *riemettere testata
009300000000     C*
009400000000     C* impostazioni prima videata
009500000000     C                   EXSR      impvd1
009600000000     C*
009700000000     C* ciclo fino a richiesta di fine elaborazione
009800000000do  1C                   DOU       $fine='S'
009900000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
010000000000     C                   ENDCS
010100000000e   1C                   ENDDO
010200000000     C*
010300000000     C                   SETON                                        lr
010400000000     c*--------------------------------------------------------------------------------------------*
010500000000     c* impvd1 - impostazioni prima videata
010600000000     c*--------------------------------------------------------------------------------------------*
010700000000     C     impvd1        BEGSR
010800000713     C*
010900000713     C* filiale -> filiale elaboratore
011000101213      * Propongo a video se non sono in sede
011100020726     c                   IF        Simfel <> 0
011200020624     c                   Z-ADD     dutpou        v1cfil                         *filiale elaboratore
011300020726     c                   endif
011400000000     C*
011500101213     c                   clear                   v1cdtd
011600101213     c
011700101213     C* data foglio-> data corrente - 1 giorno
011800000105     C                   Z-ADD     datcorpre     g08inv
011900000105     C                   Z-ADD     *zeros        g08dat
012000000105     C                   MOVEL     '3'           g08err
012100000105     C                   CALL      'XSRDA8'
012200000105     C                   PARM                    wlbda8
012300101213     C                   Z-ADD     g08dat        v1cdta                         *data lettura inizio
012400000105     c*
012500000105     C                   EXSR      setvd1
012600000105     C                   MOVEL     '1'           $tipvid                        *gestione video
012700000000     C*
012800101213     c                   clear                   v1camb
012900101213     c
013000101213     c                   eval      v1cpes=§vporpvpp
013100101213     c                   eval      v1cvol=§vporpvpv
013200101213     c
013300101213     c                   eval      v1dfil='000=Tutte'
013400000000     C                   ENDSR
013500000000     c*--------------------------------------------------------------------------------------------*
013600000000     c* setvd1 - impostazioni indicatori prima videata
013700000000     c*--------------------------------------------------------------------------------------------*
013800000000     C     setvd1        BEGSR
013900000000     C*
014000000000     C                   SETOFF                                       202122     *errori
014100000000     C                   SETOFF                                       2324
014200000000     C                   SETOFF                                       30
014300101213     C                   SETOFF                                       9028       *generico di errore
014400101213     C                   MOVEL     *blanks       v1cmsg                          *messaggio
014500000000     C*
014600000000     C                   ENDSR
014700000000     c*--------------------------------------------------------------------------------------------*
014800000000     C* gesvd1 - gestione prima videata
014900000000     c*--------------------------------------------------------------------------------------------*
015000000000     C     gesvd1        BEGSR
015100000000     C*
015200000000     C* emissione videata
015300000000if  1C                   IF        $tesvd='1'
015400101213     C                   WRITE     ms24d00                                      *testata
015500000000     C                   MOVEL     '0'           $tesvd
015600000000e   1C                   ENDIF
015700101213c
015800101213     C                   EXFMT     ms24d01                                      *video
015900000000     C                   EXSR      setvd1
016000000000     C* F03-Fine
016100000000if  1C                   IF        *inkc
016200000000     C                   EXSR      f03ges
016300000000     C                   GOTO      finvd1
016400000000e   1C                   ENDIF
016500000000     C* F12-Ritorno
016600000000if  1C                   IF        *inkl
016700000000     C                   EXSR      f12ges
016800000000     C                   GOTO      finvd1
016900000000e   1C                   ENDIF
017000101213     C* Enter-F6 - Controlla
017100101213     C                   EXSR      ctrvd1
017200101213     c* Errore
017300000000if  1C                   IF        *in90                                        *errori
017400000000     C                   GOTO      finvd1
017500101213x   1c                   endif
017600101213
017700101213if  1C                   IF        *inkf
017800101213     C                   EXSR      f06ges
017900101213     C                   EXSR      f03ges
018000101213     c                   endif
018100101213     c
018200000000     C*
018300000000     C     finvd1        ENDSR
018400000000     c*--------------------------------------------------------------------------------------------*
018500000000     C* ctrvd1 - controlla prima videata
018600000000     c*--------------------------------------------------------------------------------------------*
018700000000     C     ctrvd1        BEGSR
018800000000     C*---
018900000000     C* punto operativo
019000000000     C*---
019100101213     c                   if        v1cfil>0
019200000000     C                   Z-ADD     v1cfil        wfil
019300000000     C                   MOVEL     *blanks       wdfil
019400000000     C                   MOVEL     '0'           werr                           *no errore
019500000000     C                   EXSR      chkfil
019600000000     C                   MOVEL     wdfil         v1dfil
019700000713if  1C                   IF        werr='1'
019800101213     C                   SETON                                        309028
019900101213     C                   MOVEL     msg(4)        v1cmsg
020000000000     C                   GOTO      finct1
020100000713e   1C                   ENDIF
020200101213     c
020300101213     c* controllo che abbia RPVP
020400170320     c     kmsa          chain     fnmsa02l
020500170320     c                   if        not %found(fnmsa02l)
020600101213     C                   SETON                                        309028
020700101213     C                   MOVEL     msg(6)        v1cmsg
020800101213     C                   GOTO      finct1
020900101213e   1C                   ENDIF
021000101213     c
021100101213     c                   else
021200101213     c                   eval      v1dfil='000=Tutte'
021300101213     c                   endif
021400101213
021500000000     C*---
021600101213     C* data foglio  dal
021700000000     C*---
021800101213     C                   Z-ADD     v1cdtd        wda8
021900000000     C                   Z-ADD     *zeros        da8w
022000000000     C                   MOVEL     '0'           werr                           *no errore
022100000000     C                   EXSR      chkda8
022200101213     C                   Z-ADD     wda8          v1cdtd
022300101213     C                   Z-ADD     da8w          w1cdtd
022400000000if  1C                   IF        werr='1'
022500101213     C                   SETON                                        209028
022600101213     C                   MOVEL     msg(1)        v1cmsg
022700000000     C                   GOTO      finct1
022800000000e   1C                   ENDIF
022900000000     C*---
023000101213     C* data foglio al
023100000000     C*---
023200101213     C                   Z-ADD     v1cdta        wda8
023300000000     C                   Z-ADD     *zeros        da8w
023400000000     C                   MOVEL     '0'           werr                           *no errore
023500000000     C                   EXSR      chkda8
023600101213     C                   Z-ADD     wda8          v1cdta
023700101213     C                   Z-ADD     da8w          w1cdta
023800101213     c
023900000000if  1C                   IF        werr='1'
024000101213     C                   SETON                                        229028
024100101213     C                   MOVEL     msg(1)        v1cmsg
024200000000     C                   GOTO      finct1
024300000000e   1C                   ENDIF
024400101213     c* Data  AL no > del giorno
024500101213     c                   if        w1cdta>datcor
024600101213     C                   SETON                                        229028
024700101213     C                   MOVEL     msg(7)        v1cmsg
024800101213     C                   GOTO      finct1
024900101213e   1C                   ENDIF
025000101213     c* Data  DAL no > data AL
025100101213     c                   if        w1cdtd>w1cdta
025200101213     C                   SETON                                        229028
025300101213     C                   MOVEL     msg(3)        v1cmsg
025400101213     C                   GOTO      finct1
025500101213e   1C                   ENDIF
025600101213     c
025700101213     c* Peso e volumi > 0
025800101213     c                   if        v1cpes=0
025900101213     C                   SETON                                        239028
026000101213     C                   MOVEL     msg(2)        v1cmsg
026100101213     C                   GOTO      finct1
026200101213e   1C                   ENDIF
026300101213     c                   if        v1cvol=0
026400101213     C                   SETON                                        249028
026500101213     C                   MOVEL     msg(5)        v1cmsg
026600101213     C                   GOTO      finct1
026700101213e   1C                   ENDIF
026800000000     C*
026900000000     C     finct1        ENDSR
027000000000     c*--------------------------------------------------------------------------------------------*
027100000000     c* chkfil - controlla la filiale
027200000000     c*--------------------------------------------------------------------------------------------*
027300000000     C     chkfil        BEGSR
027400000000     C*
027500101213     c     wfil          CHAIN     azorg01l                           99
027600000000if  1c                   IF        *in99 OR                                     *inesistente O
027700000000     c                             orgfva <> *blanks OR                         *annullata   O
027800000000     c                             orgfag = 'V'                                 *varia
027900000000     C                   MOVEL     '1'           werr
028000000000x   1C                   ELSE
028100000000     C                   MOVEL     orgdes        wdfil                          *descrizione
028200000000e   1C                   ENDIF
028300000000     C*
028400000000     C                   ENDSR
028500000000     c*--------------------------------------------------------------------------------------------*
028600000000     c* chkda8 - controlla la data (8)
028700000000     c*--------------------------------------------------------------------------------------------*
028800000000     C     chkda8        BEGSR
028900000000     C*
029000000000     C                   Z-ADD     wda8          g08dat
029100000000     C                   Z-ADD     *zeros        g08inv
029200000000     C                   MOVEL     '0'           g08err
029300000000     C                   CALL      'XSRDA8'
029400000000     C                   PARM                    wlbda8
029500000000     C                   Z-ADD     g08dat        wda8
029600000000if  1C                   IF        g08err='1'
029700000000     C                   MOVEL     '1'           werr
029800000000x   1C                   ELSE
029900000000     C                   Z-ADD     g08inv        da8w
030000000000e   1C                   ENDIF
030100000000     C*
030200000000     C                   ENDSR
030300000000     c*--------------------------------------------------------------------------------------------*
030400000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
030500000000     c*--------------------------------------------------------------------------------------------*
030600000000     C     f03ges        BEGSR
030700000000     C*
030800000000     C                   MOVEL     'S'           $fine                          *fine programma
030900000000     C*
031000000000     C                   ENDSR
031100000000     c*--------------------------------------------------------------------------------------------*
031200000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
031300000000     c*--------------------------------------------------------------------------------------------*
031400000000     C     f12ges        BEGSR
031500000000     C*
031600000000     C                   MOVEL     'S'           $fine                          *fine programma
031700000000     C*
031800000000     C                   ENDSR
031900000000     c*--------------------------------------------------------------------------------------------*
032000000000     C* entges - gestione tasto funzionale enter ==> Invio
032100000000     c*--------------------------------------------------------------------------------------------*
032200101213     C     F06ges        BEGSR
032300101213     c
032400101213     c* Se richiesta data DAL /AL una sottomissione per giorno
032500101213     c                   dow       w1cdtd<=w1cdta
032600101213     c*
032700000000     c* imposta la ds di procedura
032800101213     c                   clear                   para23
032900101213     c                   MOVEL     v1cfil        parfil
033000101213     c                   MOVEL     w1cdtd        pardfv
033100101213     c                   MOVEL     v1camb        paramb
033200101213     c                   MOVEL     v1cvol        parvol
033300101213     c                   MOVEL     v1cpes        parpes
033400101213     c                   movel     para23        kpjbu
033500101213     c                   movel     'MS23'        kcoaz
033600101213     c
033700101213     c                   CALL      'BCH10'
033800000000     c                   PARM                    kpjba
033900101213     c
034000101213     c     *iso          movel     w1cdtd        dataiso
034100101213     c                   adddur    1:*Days       dataiso
034200101213     c     *ISO          MOVE      dataiso       w1cdtd
034300101213     c
034400101213     c                   enddo
034500000000     c*
034600000000     C                   ENDSR
034700000000     c*--------------------------------------------------------------------------------------------*
034800000000     C* *inzsr - operazioni iniziali
034900000000     c*--------------------------------------------------------------------------------------------*
035000000000     C     *inzsr        BEGSR
035100000000     c*
035200000000     c* ricevimento parametri
035300000000     c     *ENTRY        PLIST
035400000000     c                   PARM                    kpjba
035500020624      *
035600020624      * Reperisco dati job
035700020624     C                   EXSR      DatiJob
035800000000     C*
035900000000     C* reperimento dati utente
036000101213     c     kMSA          KLIST                                                  *azorg01l
036100101213     c                   KFLD                    kapl                            -filiale
036200101213     c                   KFLD                    wfil                            -filiale
036300000000     c     keytbe        KLIST                                                  *tntbe01l
036400000000     c                   KFLD                    kbecod                          -codice
036500000000     c                   KFLD                    kbeke1                          -chiave uno
036600000000     c                   KFLD                    kbeke2                          -chiave due
036700000000     c                   KFLD                    kbelin                          -lingua
036800000000     c                   KFLD                    kbesif                          -s.informativo
036900000000     C*
037000000000     C* reperisce la data e ora corrente
037100000000     C                   TIME                    n14                            *ora (6)+ data (8)
037200000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
037300000000     C                   Z-ADD     n8            g08dat
037400000000     C                   Z-ADD     *zeros        g08inv
037500000000     C                   MOVEL     '0'           g08err
037600000000     C                   CALL      'XSRDA8'
037700000000     C                   PARM                    wlbda8
037800000000     C                   Z-ADD     g08inv        datcor                         *data in aa/m/g
037900000105     C*
038000000105     C* calcola un giorno indietro alla data corrente
038100101213     c     *ISO          MOVE      datcor        dataiso
038200101213     c                   SUBDUR    1:*Days       dataiso
038300101213     c     *ISO          MOVE      dataiso       datcorpre
038400000105     C*
038500101213     c* Carico tabella VPO
038600101213     C                   CLEAR                   dsbs02
038700101213     c                   clear                   dvpo
038800101213     C                   MOVEL     'C'           T02MOD
038900101213     C                   MOVEL     KNSIF         T02SIF
039000101213     C                   MOVEL     'VPO'         T02COD
039100101213     c                   movel(P)  'VPO'         t02ke1
039200101213     C                   CALL      'TIBS02R'
039300101213     C                   PARM                    KPJBA
039400101213     C                   PARM                    dsbs02
039500101213    2C                   IF        T02ERR = *BLANKS
039600101213     C                   MOVEL     T02uni        dvpo
039700101213    2C                   ENDIF
039800101213      *
039900170320     c                   open(e)   fnmsa02l
040000170320     c                   if        not %open(fnmsa02l)
040100101214     c                   eval      %subst(Wfnmsa:7:4)='GRPF'
040200170320     C                   OPEN      fnmsa02l
040300101214     c                   endif
040400101214     c
040500000000     C                   ENDSR
040600020624      *----------------------------------------------------------------
040700020624      *  Reperisco Dati del job (Utente/Operativi)
040800020624      *----------------------------------------------------------------
040900020624     C     DatiJob       BegSr
041000020624      *
041100020624     C     *dtaara       define    §azute        azuteds
041200020624     C     *dtaara       define    §datiute      ddatiute
041300020624      *
041400020624     C                   in(E)     *dtaara
041500020624     C                   IF        %Error or RSUT = *blanks
041600020624     C                   call      'TIBS34R'
041700020624     C                   parm                    Tibs34Ds
041800020624     C                   in        *dtaara
041900020624     C                   ENDIF
042000020624      *
042100020624     C                   endsr
042200000000     O*--------------------------------------------------------------------------------------------*
042300000000** -msg-
042400101213Data errata
042500101213Indicare il limite PESO bolla oltre cui conteggiare i colli partiti/arrivati
042600101213La data "AL"  deve essere maggiore/uguale alla data "DAL"
042700101213Filiale errata
042800101213Indicare il limite VOLUME bolla oltre cui conteggiare i colli partiti/arrivati
042900101213La filiale indicata non è provvista di impianto RPVP o non ha nessun dato
043000101213data "AL"  non può essere maggiore della data ccorrente
