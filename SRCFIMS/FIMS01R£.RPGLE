000100040316      *--------------------------------------------------------------------------------------------*
000200040316      * Letture VdL/CML - Filtro                                                                   *
000300040316      *--------------------------------------------------------------------------------------------*
000400040316     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500040316      *--------------------------------------------------------------------------------------------*
000600040316      * DATA BASE
000700040316      *--------------------------------------------------------------------------------------------*
000800000000     ffims01d   CF   E             WORKSTN
000900000000     fazorg01l  IF   E           K DISK
001000040609     ffnmsa02l  IF   E           K DISK
001100061116     fFNMSA12L  if   e           k disk    rename(FNMSA000:FNMSA100)
001200040316      *--------------------------------------------------------------------------------------------*
001300040316      * Data structure
001400040316      *--------------------------------------------------------------------------------------------*
001500000000     d*---
001600000000     d* schiere
001700000000     d*---
001800011003     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001900000000     d*---
002000000000     d* Variabili riferite al data base
002100000000     d*---
002200000000     d krgfil          S                   LIKE(orgfil)                         *azorg01l
002300000000     d dfvv1c          S                   LIKE(v1cdfv)                         *campi video
002400000000     d*---
002500000000     d* Variabili di controllo
002600000000     d*---
002700000000     d $fine           S              1    INZ('N')                             *gestione
002800000000     d $tesvd          S              1    INZ('1')                             *riemettere testata
002900000000     d $tipvid         S              1    INZ('1')                             *gestione video
003000040609     d $Riemiss        s              1    inz(*off)
003100000000     d*---
003200000000     d* Variabili a programma
003300000000     d*---
003400000000     d datcor          S              8  0                                      *data corrente a/m/g
003500000000     d cordat          S              8  0                                      *data corrente g/m/a
003600000000     d n8              S              8  0                                      *numerico 8
003700000000     d n14             S             14  0                                      *numerico 14
003800000000     d werr            S              1    INZ('0')                             *esistenza versioni
003900000000     d wda8            S              8  0                                      *data di 8 in g/m/a
004000000000     d da8w            S              8  0                                      *data di 8 in a/m/g
004100000000     d wfil            S              3  0                                      *filiale
004200000000     d wdfil           S             20                                         *descrizione filiale
004300000000     d*---
004400000000     d* Ds
004500000000     d*---
004600040609     d WLBda8          DS                  INZ                                  *controlla data
004700040609     d  G08dat                        8  0 inz
004800040609     d  G08inv                        8  0 inz
004900040609     d  G08err                        1    inz('0')
005000040609     d  G08tgi                        5  0 inz
005100040316      *
005200040316     d                 ds                  inz
005300040316     d wTime                   1      6  0 inz
005400040316     d  wHH                    1      2  0 inz
005500040316     d  wMM                    3      4  0 inz
005600040316     d  wSS                    5      6  0 inz
005700040609      *
005800040609     d                 ds                  inz
005900040609     d wTempo                          z   inz(z'1901-01-01-01.00.00.000000')
006000040609     d   wData                         d   datfmt(*iso) overlay(wtempo)
006100040609     d   wOra                          t   timfmt(*iso) overlay(wtempo:12)
006200040609     d wTempo1                         z   inz(z'1901-01-01-01.00.00.000000')
006300040609     d wTempo2                         z   inz(z'1901-01-01-01.00.00.000000')
006400040316      *
006500040609     d FIMS00ds      e ds                                                       *ds di procedura
006600000000     d kpjba         E DS                                                       *architettura
006700011003     d tibs02ds      E DS
006800020607     D* SCHIERE PER CARICAMENTO L1
006900020607     D L1              S              3  0 DIM(30)
007000020607     D* DS PER TRUL06R - CARICAMENTO £1
007100020607     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007200020607     D  LIN                    1     90  0
007300020607     D                                     DIM(30)
007400020607      *-----------------------------------------------------------
007500020607      * Ds di riferimento al file esterno AzUte00F
007600020607      *--------
007700020607     D AzuteDs       E DS                  ExtName(AzUte00F)
007800020607      *-----------------------------------------------------------
007900020607      * Ds per dati organigramma
008000020607      *--------
008100020607     D DDatiUte      E DS
008200020607      *-----------------------------------------------------------
008300020607      * Parametri per richiamo al pgm di controllo profilo utenti
008400020607      *--------
008500020607     D Tibs34Ds      E DS                  Inz
008600061002      *
008700061002     d Status         sds
008800061002     d   SDSpgm          *proc
008900040316      *--------------------------------------------------------------------------------------------*
009000040316      * Main lines
009100040316      *--------------------------------------------------------------------------------------------*
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
011000000000     C*
011100070201     c***                eval      *in33  = (%subst(KNMUS:1:3) = 'EDP'  or
011200070201     c***                                   %subst(KNMUS:1:5) = 'ISP99')
011300070201     c                   eval      *in33  = (%subst(KNMUS:1:3) = 'EDP')
011400061002      *
011500020612     c                   Z-ADD     dutpou        v1cfil                         *filiale elaboratore
011600011010      * Cerco applicazione principale dii filiale per default
011700011010     c     v1capl        ifne      *zeros
011800011010     C                   clear                   tibs02ds
011900011010     C                   movel     'C'           t02mod
012000011010     C                   movel     knsif         t02sif
012100011010     C                   movel     'MFP'         t02cod
012200011010     C                   movel     v1cfil        t02ke1
012300011010     C                   call      'TIBS02R'
012400011010     C                   parm                    KPJBA
012500011010     C                   parm                    TIBS02DS
012600011010     C                   if        t02err =  *blanks
012700011010     C                   movel     t02uni        v1capl
012800011010     C                   endif
012900011010     C                   endif
013000011010      *
013100000000     C                   Z-ADD     cordat        v1cdfv                         *data foglio
013200000000     C                   Z-ADD     5             v1cnpg                         *categoria foglio
013300000000     C                   Z-ADD     1             v1cmtd                         *mm tot dettaglio
013400000000     C                   Z-ADD     30            v1cmtr                         *mm tot riepilogo
013500000000     C                   EXSR      setvd1
013600000000     C                   MOVEL     '1'           $tipvid                        *gestione video
013700000000     C*
013800011018     C* Effettuo controlli per le decodifiche
013900011018     C                   EXSR      ctrvd1
014000000000     C                   ENDSR
014100000000     c*--------------------------------------------------------------------------------------------*
014200000000     c* setvd1 - impostazioni indicatori prima videata
014300000000     c*--------------------------------------------------------------------------------------------*
014400000000     C     setvd1        BEGSR
014500000000     C*
014600000000     C                   SETOFF                                       202122     *errori
014700040316     C                   SETOFF                                       232425     *errori
014800040609     C                   SETOFF                                       2627       *errori
014900040316     C                   SETOFF                                       3031
015000011003     C                   SETOFF                                       3590       *generico di errore
015100000000     C                   MOVEL     *blanks       $msg                            *messaggio
015200000000     C*
015300000000     C                   ENDSR
015400000000     c*--------------------------------------------------------------------------------------------*
015500000000     C* gesvd1 - gestione prima videata
015600000000     c*--------------------------------------------------------------------------------------------*
015700000000     C     gesvd1        BEGSR
015800000000     C*
015900000000     C* emissione videata
016000000000if  1C                   IF        $tesvd='1'
016100000000     C                   WRITE     ms01d00                                      *testata
016200000000     C                   MOVEL     '0'           $tesvd
016300000000e   1C                   ENDIF
016400000000     C                   EXFMT     ms01d01                                      *video
016500000000     C                   EXSR      setvd1
016600011003      *
016700011003      * Ricerca su tipo applicazione
016800011003     c     '?'           scan      V1Capl                                 35
016900011003     C     *in35         ifeq      *on
017000011003     C                   clear                   tibs02ds
017100011003     C                   movel     'R'           t02mod
017200011003     C                   movel     knsif         t02sif
017300011003     C                   movel     'MTP'         t02cod
017400011003     C                   call      'TIBS02R'
017500011003     C                   parm                    KPJBA
017600011003     C                   parm                    TIBS02DS
017700011003     C                   if        t02err =  *blanks
017800011003     C                   movel     t02ke1        V1Capl
017900011003     c                   endif
018000011003      *
018100011003     C                   endif
018200000000     C* F03-Fine
018300000000if  1C                   IF        *inkc
018400000000     C                   EXSR      f03ges
018500000000     C                   GOTO      finvd1
018600000000e   1C                   ENDIF
018700000000     C* F12-Ritorno
018800000000if  1C                   IF        *inkl
018900000000     C                   EXSR      f12ges
019000000000     C                   GOTO      finvd1
019100000000e   1C                   ENDIF
019200000000     C* Enter-Controlla
019300000000     C                   EXSR      ctrvd1
019400040609if  1C                   IF           *in90                                     *errori
019500040609     c                             or $Riemiss = *on
019600000000     C                   GOTO      finvd1
019700000000x   1c                   ELSE
019800000000     c                   EXSR      entges
019900000000e   1C                   ENDIF
020000000000     C*
020100000000     C     finvd1        ENDSR
020200000000     c*--------------------------------------------------------------------------------------------*
020300000000     C* ctrvd1 - controlla prima videata
020400000000     c*--------------------------------------------------------------------------------------------*
020500000000     C     ctrvd1        BEGSR
020600040609      *
020700040609     c                   reset                   $Riemiss
020800000000     C*---
020900000000     C* punto operativo
021000000000     C*---
021100020612     C                   Z-ADD     v1cfil        wfil
021200000000     C                   MOVEL     *blanks       wdfil
021300000000     C                   MOVEL     '0'           werr                           *no errore
021400000000     C                   EXSR      chkfil
021500000000     C                   MOVEL     wdfil         v1dfil
021600000713if  1C                   IF        werr='1'
021700000000     C                   SETON                                        3090
021800000000     C                   MOVEL     msg(4)        $msg
021900000000     C                   GOTO      finct1
022000000713e   1C                   ENDIF
022100011003     C*---
022200011003     C* tipo applicazione
022300011003     C*---
022400011003     c     v1capl        ifne      *blanks
022500011003     c                   clear                   v1dapl
022600011003     C                   clear                   tibs02ds
022700011003     C                   movel     'C'           t02mod
022800011003     C                   movel     knsif         t02sif
022900011003     C                   movel     'MTP'         t02cod
023000011003     C                   movel     V1capl        t02ke1
023100011003     C                   call      'TIBS02R'
023200011003     C                   parm                    KPJBA
023300011003     C                   parm                    TIBS02DS
023400011003     C                   if        t02err =  *blanks
023500011003     C                   movel     t02uni        v1dapl
023600011003     C                   else
023700011003     C                   eval      $msg = msg(6)
023800011003     c                   seton                                        3190
023900011003     c                   endif
024000011003     C                   else
024100011003     C                   eval      $msg = msg(5)
024200011003     c                   seton                                        3190
024300011003     c                   endif
024400000000     C*---
024500000000     C* data foglio
024600000000     C*---
024700000000     C                   Z-ADD     *zeros        dfvv1c
024800000000     C                   Z-ADD     v1cdfv        wda8
024900000000     C                   Z-ADD     *zeros        da8w
025000000000     C                   MOVEL     '0'           werr                           *no errore
025100000000     C                   EXSR      chkda8
025200000000     C                   Z-ADD     wda8          v1cdfv
025300000000     C                   Z-ADD     da8w          dfvv1c
025400000000if  1C                   IF        werr='1'
025500000000     C                   SETON                                        2090
025600000000     C                   MOVEL     msg(1)        $msg
025700000000     C                   GOTO      finct1
025800000000e   1C                   ENDIF
025900000000     C*---
026000000000     C* categoria foglio
026100000000     C*---
026200000000if  1C                   IF        v1cnpg <> 2 AND
026300040406     C                             v1cnpg <> 5
026400040406     C*****                        v1cnpg <> 5 AND
026500040406     C*****                        v1cnpg <> 6
026600000000     C                   SETON                                        2190
026700000000     C                   MOVEL     msg(2)        $msg
026800000000     C                   GOTO      finct1
026900000000e   1C                   ENDIF
027000000000     C*---
027100000000     C* minuti di dettaglio
027200000000     C*---
027300000000if  1c                   IF        v1cmtd <> 1  AND
027400000000     C                             v1cmtd <> 5  AND
027500000000     C                             v1cmtd <> 10 AND
027600000000     C                             v1cmtd <> 15 AND
027700000000     C                             v1cmtd <> 30 AND
027800000000     C                             v1cmtd <> 60
027900000000     C                   SETON                                        2290
028000000000     C                   MOVEL     msg(2)        $msg
028100000000     C                   GOTO      finct1
028200000000e   1C                   ENDIF
028300000000     C*---
028400000000     C* minuti di riepilogo
028500000000     C*---
028600000000if  1C                   IF        v1cmtr <> 30 AND
028700000000     C                             v1cmtr <> 60
028800000000     C                   SETON                                        2390
028900000000     C                   MOVEL     msg(2)        $msg
029000000000     C                   GOTO      finct1
029100000000e   1C                   ENDIF
029200000000     c*
029300000000     c* i mm di riepilogo devono essere maggiori/uguali dei mm di dettaglio
029400000000if  2c                   IF        v1cmtr < v1cmtd
029500000000     C                   SETON                                        2390
029600000000     C                   MOVEL     msg(3)        $msg
029700000000     C                   GOTO      finct1
029800000000e   2C                   ENDIF
029900040316      *---
030000040316      * orari
030100040316      *---
030200040316      * orario iniziale
030300040316     c                   eval      wTime = V1Coli * 100
030400040316     c                   exsr      ChkTime
030500040316if  1c                   if        wErr  = *on
030600040316     c                   seton                                        24  90
030700040316     c                   movel     Msg(7)        $Msg
030800040316     c                   goto      FinCt1
030900040316e   1c                   endif
031000040316      * orario finale
031100040316     c                   eval      wTime = V1Colf * 100
031200040316     c                   exsr      ChkTime
031300040316if  1c                   if        wErr  = *on
031400040316     c                   seton                                        25  90
031500040316     c                   movel     Msg(7)        $Msg
031600040316     c                   goto      FinCt1
031700040316e   1c                   endif
031800040609      * reperimento date (iniziale e/o finale)
031900040609if  1c                   if            (V1Coli <> *zeros
032000040609     c                              or  V1Colf <> *zeros)
032100040609     c                             and (V1Cdli =  *zeros
032200040609     c                              or  V1Cdlf =  *zeros)
032300040609     c                   exsr      RepDTL
032400040609e   1c                   endif
032500040609      * date
032600040609     c                   clear                   D00dli
032700040609     c                   clear                   D00dlf
032800040609if  1c                   if             V1Coli <> *zeros
032900040609     c                             or   V1Colf <> *zeros
033000040609      * - data iniziale
033100040609     c                   reset                   WLBda8
033200040609     c                   eval      G08dat = V1Cdli
033300040609     c                   call      'XSRDA8'
033400040609     c                   parm                    WLBda8
033500040609     c                   z-add     G08dat        V1Cdli
033600040609if  2c                   if        G08err = '1'
033700040609     c                   seton                                        26  90
033800040609     c                   movel     Msg(1)        $Msg
033900040609     c                   goto      finct1
034000040609x   2c                   else
034100040609     c                   eval      D00dli = G08inv
034200040609e   2c                   endif
034300040609      * - data finale
034400040609     c                   reset                   WLBda8
034500040609     c                   eval      G08dat = V1Cdlf
034600040609     c                   call      'XSRDA8'
034700040609     c                   parm                    WLBda8
034800040609     c                   z-add     G08dat        V1Cdlf
034900040609if  2c                   if        G08err = '1'
035000040609     c                   seton                                        27  90
035100040609     c                   movel     Msg(1)        $Msg
035200040609     c                   goto      finct1
035300040609x   2c                   else
035400040609     c                   eval      D00dlf = G08inv
035500040609e   2c                   endif
035600040609e   1c                   endif
035700040609      * range Date/Ore
035800040609if  1c                   if        D00dli > *zeros
035900040609     c     *iso          move      D00dli        wData
036000040609x   1c                   else
036100040609     c                   reset                   wData
036200040609e   1c                   endif
036300040609     c                   eval      wTime  = V1Coli * 100
036400040609     c     *iso          move      wTime         wOra
036500040609     c                   movel     wTempo        wTempo1                        *periodo INIZIALE
036600040609if  1c                   if        D00dli > *zeros
036700040609     c     *iso          move      D00dlf        wData
036800040609x   1c                   else
036900040609     c                   reset                   wData
037000040609e   1c                   endif
037100040609     c                   eval      wTime  = V1Colf * 100
037200040609     c     *iso          move      wTime         wOra
037300040609     c                   movel     wTempo        wTempo2                        *periodo FINALE
037400040609if  1c                   if        wTempo1 > wTempo2
037500040609     c                   seton                                        2425
037600040609     c                   seton                                        262790
037700040609     c                   movel     Msg(9)        $Msg
037800040609     c                   goto      FinCt1
037900040609e   1c                   endif
038000040609      *
038100000000     C*
038200000000     C     finct1        ENDSR
038300000000     c*--------------------------------------------------------------------------------------------*
038400000000     c* chkfil - controlla la filiale
038500000000     c*--------------------------------------------------------------------------------------------*
038600000000     C     chkfil        BEGSR
038700000000     C*
038800000000     C                   Z-ADD     wfil          krgfil
038900000000     c     keyorg        CHAIN     azorg01l                           99
039000000000if  1c                   IF        *in99 OR                                     *inesistente O
039100000000     c                             orgfva <> *blanks OR                         *annullata   O
039200000000     c                             orgfag = 'V'                                 *varia
039300000000     C                   MOVEL     '1'           werr
039400000000x   1C                   ELSE
039500000000     C                   MOVEL     orgdes        wdfil                          *descrizione
039600000000e   1C                   ENDIF
039700020607      * P.O gestibile
039800020612     C                   IF        werr = '0'
039900020607     C     wfil          lookup    L1                                     35
040000020612     C  N35              MOVEL     '1'           werr
040100020607     C                   ENDIF
040200000000     C*
040300000000     C                   ENDSR
040400000000     c*--------------------------------------------------------------------------------------------*
040500000000     c* chkda8 - controlla la data (8)
040600000000     c*--------------------------------------------------------------------------------------------*
040700000000     C     chkda8        BEGSR
040800000000     C*
040900000000     C                   Z-ADD     wda8          g08dat
041000000000     C                   Z-ADD     *zeros        g08inv
041100000000     C                   MOVEL     '0'           g08err
041200000000     C                   CALL      'XSRDA8'
041300000000     C                   PARM                    wlbda8
041400000000     C                   Z-ADD     g08dat        wda8
041500000000if  1C                   IF        g08err='1'
041600000000     C                   MOVEL     '1'           werr
041700000000x   1C                   ELSE
041800000000     C                   Z-ADD     g08inv        da8w
041900000000e   1C                   ENDIF
042000000000     C*
042100000000     C                   ENDSR
042200040316      *---------------------------------------------------------------*
042300040316      * ChkTime - controllo orario (hh:mm:ss)                         *
042400040316      *---------------------------------------------------------------*
042500040316     c     ChkTime       BEGSR
042600040316      *
042700040316     c                   eval      wErr = *off
042800040316      *
042900040610if  1c                   if            wHH < 00  or  wHH > 23
043000040316     c                             or  wMM < 00  or  wMM > 59
043100040316     c                             or  wSS < 00  or  wSS > 59
043200040316     c                   eval      wErr = *on
043300040316e   1c                   endif
043400040316      *
043500040316     c                   ENDSR
043600000000     c*--------------------------------------------------------------------------------------------*
043700000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
043800000000     c*--------------------------------------------------------------------------------------------*
043900000000     C     f03ges        BEGSR
044000000000     C*
044100000000     C                   MOVEL     'S'           $fine                          *fine programma
044200000000     C*
044300000000     C                   ENDSR
044400000000     c*--------------------------------------------------------------------------------------------*
044500000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
044600000000     c*--------------------------------------------------------------------------------------------*
044700000000     C     f12ges        BEGSR
044800000000     C*
044900000000     C                   MOVEL     'S'           $fine                          *fine programma
045000000000     C*
045100000000     C                   ENDSR
045200000000     c*--------------------------------------------------------------------------------------------*
045300000000     C* entges - gestione tasto funzionale enter ==> Invio
045400000000     c*--------------------------------------------------------------------------------------------*
045500000000     C     entges        BEGSR
045600000000     c*
045700000000     c* imposta la ds di procedura
045800000000     c                   MOVEL     *blanks       d00op1                         *livello programma
045900000000     c                   MOVEL     '0'           d00f03                         *premuto F03
046000000000     c                   MOVEL     '0'           d00f12                         *premuto F12
046100000000     c                   MOVEL     '0'           d00err                         *errore
046200000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
046300000000     c                   Z-ADD     v1cfil        d00fil                         *filiale
046400011003     c                   MOVEL     v1capl        d00apl                         *filiale
046500000000     c                   Z-ADD     dfvv1c        d00dfv                         *data foglio
046600000000     c                   Z-ADD     v1cnpg        d00npg                         *categoria foglio
046700000000     c                   Z-ADD     v1cmtd        d00mtd                         *mm totali dettaglio
046800000000     c                   Z-ADD     v1cmtr        d00mtr                         *mm totali riepilogo
046900040609     c*** già fatto:     z-add     dliV1C        D00dli
047000040316     c                   z-add     V1Coli        D00oli
047100040609     c*** già fatto:     z-add     dlfV1C        D00dlf
047200040316     c                   z-add     V1Colf        D00olf
047300061002     c                   call      'FIMS08R'
047400061002     c                   parm                    KPJBA
047500061002     c                   parm                    FIMS00ds
047600000000     C                   MOVEL     '1'           $tesvd                          *riemettere testata
047700000000     C*
047800000000     C* se ritorna premuto f3, chiude il programma
047900000000IF  1c                   IF        d00f03='1'                                    *premuto f03
048000000000     c                   MOVEL     'S'           $fine                           *fine pgm
048100000000X   1c                   ELSE                                                    *non premuto f03
048200000000     C                   MOVEL     d00msg        $msg                            *messaggio
048300000000e   1c                   ENDIF
048400000000     c*
048500000000     C                   ENDSR
048600000000     c*--------------------------------------------------------------------------------------------*
048700000000     C* impvd0 - Impostazioni testata
048800000000     c*--------------------------------------------------------------------------------------------*
048900000000     C     impvd0        BEGSR
049000000000     C*
049100061002     c                   movel     SDSpgm        V1Tpgm
049200000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
049300000000     C*
049400000000     C                   ENDSR
049500040609      *
049600040609      *---------------------------------------------------------------*
049700040609      * REPDTL - Reperimento range di date relative alla lettura      *
049800040609      *---------------------------------------------------------------*
049900040609     c     RepDTL        BEGSR
050000040609      *
050100040609     c                   reset                   $Riemiss
050200040609     c                   movel     V1Capl        MSAapl                         *tipo applic.
050300040609     c                   z-add     V1Cfil        MSAfil                         *p.o.
050400040609     c                   z-add     dfvV1C        MSAdfv                         *data foglio
050500040609     c                   z-add     V1Cnpg        MSAnpg                         *categoria foglio
050600040609      *
050700040609      * imposto la data dell'orario iniziale
050800061116      *
050900061116     c     k04msa02      setll     FNMSA100
051000061116     c     k04msa02      reade     FNMSA100
051100061116      *
051200061116if  1c                   if        %eof(FNMSA12L)
051300061116      *
051400061116     c                   seton                                        20  90
051500061116     c                   movel     Msg(8)        $Msg
051600061116     c                   goto      NoRepDTL
051700061116x   1c                   else
051800061116if  2c                   if        V1Cdli = *zeros
051900061116     c                   reset                   WLBda8
052000061116     c                   eval      G08inv = MSAdtl
052100061116     c                   eval      G08err = '3'
052200061116     c                   call      'XSRDA8'
052300061116     c                   parm                    WLBda8
052400061116     c                   z-add     G08dat        V1Cdli
052500061116     c                   eval      $Riemiss = *on
052600061116e   2c                   endif
052700061116      *
052800061116      * imposto la data dell'orario finale
052900040609     c     k04msa02      setgt     FNMSA000
053000040609     c     k04msa02      readpe    FNMSA000
053100040609if  2c                   if        NOT %eof(FNMSA02L)
053200040609if  3c                   if        V1Cdlf = *zeros
053300040609     c                   reset                   WLBda8
053400040609     c                   eval      G08inv = MSAdtl
053500040609     c                   eval      G08err = '3'
053600040609     c                   call      'XSRDA8'
053700040609     c                   parm                    WLBda8
053800040609     c                   z-add     G08dat        V1Cdlf
053900040609     c                   eval      $Riemiss = *on
054000040609e   3c                   endif
054100040609e   2c                   endif
054200040609      *
054300040609e   1c                   endif
054400040609      *
054500040609     c     NoRepDTL      ENDSR
054600040609      *
054700000000     c*--------------------------------------------------------------------------------------------*
054800000000     C* *inzsr - operazioni iniziali
054900000000     c*--------------------------------------------------------------------------------------------*
055000000000     C     *inzsr        BEGSR
055100000000     c*
055200000000     c* ricevimento parametri
055300000000     c     *ENTRY        PLIST
055400000000     c                   PARM                    kpjba
055500040609     c                   PARM                    FIMS00ds
055600040609      *
055700040609     c                   clear                   FIMS00ds
055800020607      *
055900020607      * Reperisco dati job
056000020607     C                   EXSR      DatiJob
056100000000     C*
056200000000     C* reperimento dati utente
056300000000     c     keyorg        KLIST                                                  *azorg01l
056400000000     c                   KFLD                    krgfil                          -filiale
056500061116      * file FNMSA02L / FNMSA12L
056600040609     c     K04MSA02      klist                                                  *FNMSA02L
056700040609     c                   kfld                    MSAapl                          -tipo appl.
056800040609     c                   kfld                    MSAfil                          -p.o.
056900040609     c                   kfld                    MSAdfv                          -data f.v.
057000040609     c                   kfld                    MSAnpg                          -categ. f.v.
057100000000     C*
057200020607     C*
057300020607     C****  CARICO TABELLA FILIALI GESTITE £1  ****
057400020607     C                   CLEAR                   DSUL06
057500020607     C                   MOVE      '£1'          D06COD
057600020607     C                   MOVEL     SIMFEL        D06KEY
057700020607     C                   MOVEL     DSUL06        KPJBU
057800020607     C                   CALL      'TRUL06R'
057900020607     C                   PARM                    KPJBA
058000020607     C                   MOVEL     KPJBU         DSUL06
058100020607     C                   MOVEA     LIN           L1
058200000000     C*
058300000000     C* reperisce la data corrente
058400000000     C                   TIME                    n14                            *ora (6)+ data (8)
058500000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
058600000000     C                   Z-ADD     n8            cordat                         *data in g/m/aa
058700000000     C                   Z-ADD     n8            g08dat
058800000000     C                   Z-ADD     *zeros        g08inv
058900000000     C                   MOVEL     '0'           g08err
059000000000     C                   CALL      'XSRDA8'
059100000000     C                   PARM                    wlbda8
059200000000     C                   Z-ADD     g08inv        datcor                         *data in aa/m/g
059300000000     C*
059400000000     C                   ENDSR
059500020607      *----------------------------------------------------------------
059600020607      *  Reperisco Dati del job (Utente/Operativi)
059700020607      *----------------------------------------------------------------
059800020607     C     DatiJob       BegSr
059900020607      *
060000020607     C     *dtaara       define    §azute        azuteds
060100020607     C     *dtaara       define    §datiute      ddatiute
060200020607      *
060300020607     C                   in(E)     *dtaara
060400020607     C                   IF        %Error or RSUT = *blanks
060500020607     C                   call      'TIBS34R'
060600020607     C                   parm                    Tibs34Ds
060700020607     C                   in        *dtaara
060800020607     C                   ENDIF
060900020607      *
061000020607     C                   endsr
061100000000** -msg-
061200000000FIMS01R- Data errata                                                          1
061300000000FIMS01R- Inserire uno dei valori possibili                                    2
061400000000FIMS01R- I mm di riepilogo devono essere maggiori/uguali dei mm di dettaglio  3
061500000000FIMS01R- Filiale errata o non in gestione                                     4
061600040316FIMS01R- Tipo applicazione obbligatoria                                       5
061700040316FIMS01R- Tipo applicazione errata                                             6
061800040316FIMS01R- Orario errato                                                        7
061900040609FIMS01R- Non reperiti dati per il F.V. indicato                               8
062000040609FIMS01R- Orario iniziale successivo all'orario finale                         9
062100040609FIMS01R- 10
