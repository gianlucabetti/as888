000100040316      *--------------------------------------------------------------------------------------------*
000200040316      * Letture VdL/CML - Filtro                                                                   *
000300040316      *--------------------------------------------------------------------------------------------*
000400040316     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500040316      *--------------------------------------------------------------------------------------------*
000600040316      * DATA BASE
000700040316      *--------------------------------------------------------------------------------------------*
000800000000     ffims01d   CF   E             WORKSTN
000900000000     fazorg01l  IF   E           K DISK
001000170201     fFNMSA02L  if   e           k disk
001100040316      *--------------------------------------------------------------------------------------------*
001200040316      * Data structure
001300040316      *--------------------------------------------------------------------------------------------*
001400000000     d*---
001500000000     d* schiere
001600000000     d*---
001700011003     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001800000000     d*---
001900000000     d* Variabili riferite al data base
002000000000     d*---
002100000000     d krgfil          S                   LIKE(orgfil)                         *azorg01l
002200000000     d dfvv1c          S                   LIKE(v1cdfv)                         *campi video
002300000000     d*---
002400000000     d* Variabili di controllo
002500000000     d*---
002600000000     d $fine           S              1    INZ('N')                             *gestione
002700000000     d $tesvd          S              1    INZ('1')                             *riemettere testata
002800000000     d $tipvid         S              1    INZ('1')                             *gestione video
002900040609     d $Riemiss        s              1    inz(*off)
003000000000     d*---
003100000000     d* Variabili a programma
003200000000     d*---
003300000000     d datcor          S              8  0                                      *data corrente a/m/g
003400000000     d cordat          S              8  0                                      *data corrente g/m/a
003500000000     d n8              S              8  0                                      *numerico 8
003600000000     d n14             S             14  0                                      *numerico 14
003700000000     d werr            S              1    INZ('0')                             *esistenza versioni
003800000000     d wda8            S              8  0                                      *data di 8 in g/m/a
003900000000     d da8w            S              8  0                                      *data di 8 in a/m/g
004000000000     d wfil            S              3  0                                      *filiale
004100000000     d wdfil           S             20                                         *descrizione filiale
004200000000     d*---
004300000000     d* Ds
004400000000     d*---
004500040609     d WLBda8          DS                  INZ                                  *controlla data
004600040609     d  G08dat                        8  0 inz
004700040609     d  G08inv                        8  0 inz
004800040609     d  G08err                        1    inz('0')
004900040609     d  G08tgi                        5  0 inz
005000040316      *
005100040316     d                 ds                  inz
005200040316     d wTime                   1      6  0 inz
005300040316     d  wHH                    1      2  0 inz
005400040316     d  wMM                    3      4  0 inz
005500040316     d  wSS                    5      6  0 inz
005600040609      *
005700040609     d                 ds                  inz
005800040609     d wTempo                          z   inz(z'1901-01-01-01.00.00.000000')
005900040609     d   wData                         d   datfmt(*iso) overlay(wtempo)
006000040609     d   wOra                          t   timfmt(*iso) overlay(wtempo:12)
006100040609     d wTempo1                         z   inz(z'1901-01-01-01.00.00.000000')
006200040609     d wTempo2                         z   inz(z'1901-01-01-01.00.00.000000')
006300040316      *
006400040609     d FIMS00ds      e ds                                                       *ds di procedura
006500000000     d kpjba         E DS                                                       *architettura
006600011003     d tibs02ds      E DS
006700020607     D* SCHIERE PER CARICAMENTO L1
006800020607     D L1              S              3  0 DIM(30)
006900020607     D* DS PER TRUL06R - CARICAMENTO £1
007000020607     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007100020607     D  LIN                    1     90  0
007200020607     D                                     DIM(30)
007300020607      *-----------------------------------------------------------
007400020607      * Ds di riferimento al file esterno AzUte00F
007500020607      *--------
007600020607     D AzuteDs       E DS                  ExtName(AzUte00F)
007700020607      *-----------------------------------------------------------
007800020607      * Ds per dati organigramma
007900020607      *--------
008000020607     D DDatiUte      E DS
008100020607      *-----------------------------------------------------------
008200020607      * Parametri per richiamo al pgm di controllo profilo utenti
008300020607      *--------
008400020607     D Tibs34Ds      E DS                  Inz
008500061002      *
008600061002     d Status         sds
008700061002     d   SDSpgm          *proc
008800040316      *--------------------------------------------------------------------------------------------*
008900040316      * Main lines
009000040316      *--------------------------------------------------------------------------------------------*
009100000000     C*
009200000000     C* impostazioni testata
009300000000     C                   EXSR      impvd0
009400000000     C*
009500000000     C* impostazioni prima videata
009600000000     C                   EXSR      impvd1
009700000000     C*
009800000000     C* ciclo fino a richiesta di fine elaborazione
009900000000do  1C                   DOU       $fine='S'
010000000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
010100000000     C                   ENDCS
010200000000e   1C                   ENDDO
010300000000     C*
010400000000     C                   SETON                                        lr
010500000000     c*--------------------------------------------------------------------------------------------*
010600000000     c* impvd1 - impostazioni prima videata
010700000000     c*--------------------------------------------------------------------------------------------*
010800000000     C     impvd1        BEGSR
010900000000     C*
011000070201     c***                eval      *in33  = (%subst(KNMUS:1:3) = 'EDP'  or
011100070201     c***                                   %subst(KNMUS:1:5) = 'ISP99')
011200070201     c                   eval      *in33  = (%subst(KNMUS:1:3) = 'EDP')
011300061002      *
011400020612     c                   Z-ADD     dutpou        v1cfil                         *filiale elaboratore
011500011010      * Cerco applicazione principale dii filiale per default
011600011010     c     v1capl        ifne      *zeros
011700011010     C                   clear                   tibs02ds
011800011010     C                   movel     'C'           t02mod
011900011010     C                   movel     knsif         t02sif
012000011010     C                   movel     'MFP'         t02cod
012100011010     C                   movel     v1cfil        t02ke1
012200011010     C                   call      'TIBS02R'
012300011010     C                   parm                    KPJBA
012400011010     C                   parm                    TIBS02DS
012500011010     C                   if        t02err =  *blanks
012600011010     C                   movel     t02uni        v1capl
012700011010     C                   endif
012800011010     C                   endif
012900011010      *
013000000000     C                   Z-ADD     cordat        v1cdfv                         *data foglio
013100000000     C                   Z-ADD     5             v1cnpg                         *categoria foglio
013200000000     C                   Z-ADD     1             v1cmtd                         *mm tot dettaglio
013300000000     C                   Z-ADD     30            v1cmtr                         *mm tot riepilogo
013400000000     C                   EXSR      setvd1
013500000000     C                   MOVEL     '1'           $tipvid                        *gestione video
013600000000     C*
013700011018     C* Effettuo controlli per le decodifiche
013800011018     C                   EXSR      ctrvd1
013900000000     C                   ENDSR
014000000000     c*--------------------------------------------------------------------------------------------*
014100000000     c* setvd1 - impostazioni indicatori prima videata
014200000000     c*--------------------------------------------------------------------------------------------*
014300000000     C     setvd1        BEGSR
014400000000     C*
014500000000     C                   SETOFF                                       202122     *errori
014600040316     C                   SETOFF                                       232425     *errori
014700040609     C                   SETOFF                                       2627       *errori
014800040316     C                   SETOFF                                       3031
014900011003     C                   SETOFF                                       3590       *generico di errore
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
016000000000     C                   WRITE     ms01d00                                      *testata
016100000000     C                   MOVEL     '0'           $tesvd
016200000000e   1C                   ENDIF
016300000000     C                   EXFMT     ms01d01                                      *video
016400000000     C                   EXSR      setvd1
016500011003      *
016600011003      * Ricerca su tipo applicazione
016700011003     c     '?'           scan      V1Capl                                 35
016800011003     C     *in35         ifeq      *on
016900011003     C                   clear                   tibs02ds
017000011003     C                   movel     'R'           t02mod
017100011003     C                   movel     knsif         t02sif
017200011003     C                   movel     'MTP'         t02cod
017300011003     C                   call      'TIBS02R'
017400011003     C                   parm                    KPJBA
017500011003     C                   parm                    TIBS02DS
017600011003     C                   if        t02err =  *blanks
017700011003     C                   movel     t02ke1        V1Capl
017800011003     c                   endif
017900011003      *
018000011003     C                   endif
018100000000     C* F03-Fine
018200000000if  1C                   IF        *inkc
018300000000     C                   EXSR      f03ges
018400000000     C                   GOTO      finvd1
018500000000e   1C                   ENDIF
018600000000     C* F12-Ritorno
018700000000if  1C                   IF        *inkl
018800000000     C                   EXSR      f12ges
018900000000     C                   GOTO      finvd1
019000000000e   1C                   ENDIF
019100000000     C* Enter-Controlla
019200000000     C                   EXSR      ctrvd1
019300040609if  1C                   IF           *in90                                     *errori
019400040609     c                             or $Riemiss = *on
019500000000     C                   GOTO      finvd1
019600000000x   1c                   ELSE
019700000000     c                   EXSR      entges
019800000000e   1C                   ENDIF
019900000000     C*
020000000000     C     finvd1        ENDSR
020100000000     c*--------------------------------------------------------------------------------------------*
020200000000     C* ctrvd1 - controlla prima videata
020300000000     c*--------------------------------------------------------------------------------------------*
020400000000     C     ctrvd1        BEGSR
020500040609      *
020600040609     c                   reset                   $Riemiss
020700000000     C*---
020800000000     C* punto operativo
020900000000     C*---
021000020612     C                   Z-ADD     v1cfil        wfil
021100000000     C                   MOVEL     *blanks       wdfil
021200000000     C                   MOVEL     '0'           werr                           *no errore
021300000000     C                   EXSR      chkfil
021400000000     C                   MOVEL     wdfil         v1dfil
021500000713if  1C                   IF        werr='1'
021600000000     C                   SETON                                        3090
021700000000     C                   MOVEL     msg(4)        $msg
021800000000     C                   GOTO      finct1
021900000713e   1C                   ENDIF
022000011003     C*---
022100011003     C* tipo applicazione
022200011003     C*---
022300011003     c     v1capl        ifne      *blanks
022400011003     c                   clear                   v1dapl
022500011003     C                   clear                   tibs02ds
022600011003     C                   movel     'C'           t02mod
022700011003     C                   movel     knsif         t02sif
022800011003     C                   movel     'MTP'         t02cod
022900011003     C                   movel     V1capl        t02ke1
023000011003     C                   call      'TIBS02R'
023100011003     C                   parm                    KPJBA
023200011003     C                   parm                    TIBS02DS
023300011003     C                   if        t02err =  *blanks
023400011003     C                   movel     t02uni        v1dapl
023500011003     C                   else
023600011003     C                   eval      $msg = msg(6)
023700011003     c                   seton                                        3190
023800011003     c                   endif
023900011003     C                   else
024000011003     C                   eval      $msg = msg(5)
024100011003     c                   seton                                        3190
024200011003     c                   endif
024300000000     C*---
024400000000     C* data foglio
024500000000     C*---
024600000000     C                   Z-ADD     *zeros        dfvv1c
024700000000     C                   Z-ADD     v1cdfv        wda8
024800000000     C                   Z-ADD     *zeros        da8w
024900000000     C                   MOVEL     '0'           werr                           *no errore
025000000000     C                   EXSR      chkda8
025100000000     C                   Z-ADD     wda8          v1cdfv
025200000000     C                   Z-ADD     da8w          dfvv1c
025300000000if  1C                   IF        werr='1'
025400000000     C                   SETON                                        2090
025500000000     C                   MOVEL     msg(1)        $msg
025600000000     C                   GOTO      finct1
025700000000e   1C                   ENDIF
025800000000     C*---
025900000000     C* categoria foglio
026000000000     C*---
026100000000if  1C                   IF        v1cnpg <> 2 AND
026200040406     C                             v1cnpg <> 5
026300040406     C*****                        v1cnpg <> 5 AND
026400040406     C*****                        v1cnpg <> 6
026500000000     C                   SETON                                        2190
026600000000     C                   MOVEL     msg(2)        $msg
026700000000     C                   GOTO      finct1
026800000000e   1C                   ENDIF
026900000000     C*---
027000000000     C* minuti di dettaglio
027100000000     C*---
027200000000if  1c                   IF        v1cmtd <> 1  AND
027300000000     C                             v1cmtd <> 5  AND
027400000000     C                             v1cmtd <> 10 AND
027500000000     C                             v1cmtd <> 15 AND
027600000000     C                             v1cmtd <> 30 AND
027700000000     C                             v1cmtd <> 60
027800000000     C                   SETON                                        2290
027900000000     C                   MOVEL     msg(2)        $msg
028000000000     C                   GOTO      finct1
028100000000e   1C                   ENDIF
028200000000     C*---
028300000000     C* minuti di riepilogo
028400000000     C*---
028500000000if  1C                   IF        v1cmtr <> 30 AND
028600000000     C                             v1cmtr <> 60
028700000000     C                   SETON                                        2390
028800000000     C                   MOVEL     msg(2)        $msg
028900000000     C                   GOTO      finct1
029000000000e   1C                   ENDIF
029100000000     c*
029200000000     c* i mm di riepilogo devono essere maggiori/uguali dei mm di dettaglio
029300000000if  2c                   IF        v1cmtr < v1cmtd
029400000000     C                   SETON                                        2390
029500000000     C                   MOVEL     msg(3)        $msg
029600000000     C                   GOTO      finct1
029700000000e   2C                   ENDIF
029800040316      *---
029900040316      * orari
030000040316      *---
030100040316      * orario iniziale
030200040316     c                   eval      wTime = V1Coli * 100
030300040316     c                   exsr      ChkTime
030400040316if  1c                   if        wErr  = *on
030500040316     c                   seton                                        24  90
030600040316     c                   movel     Msg(7)        $Msg
030700040316     c                   goto      FinCt1
030800040316e   1c                   endif
030900040316      * orario finale
031000040316     c                   eval      wTime = V1Colf * 100
031100040316     c                   exsr      ChkTime
031200040316if  1c                   if        wErr  = *on
031300040316     c                   seton                                        25  90
031400040316     c                   movel     Msg(7)        $Msg
031500040316     c                   goto      FinCt1
031600040316e   1c                   endif
031700040609      * reperimento date (iniziale e/o finale)
031800040609if  1c                   if            (V1Coli <> *zeros
031900040609     c                              or  V1Colf <> *zeros)
032000040609     c                             and (V1Cdli =  *zeros
032100040609     c                              or  V1Cdlf =  *zeros)
032200040609     c                   exsr      RepDTL
032300040609e   1c                   endif
032400040609      * date
032500040609     c                   clear                   D00dli
032600040609     c                   clear                   D00dlf
032700040609if  1c                   if             V1Coli <> *zeros
032800040609     c                             or   V1Colf <> *zeros
032900040609      * - data iniziale
033000040609     c                   reset                   WLBda8
033100040609     c                   eval      G08dat = V1Cdli
033200040609     c                   call      'XSRDA8'
033300040609     c                   parm                    WLBda8
033400040609     c                   z-add     G08dat        V1Cdli
033500040609if  2c                   if        G08err = '1'
033600040609     c                   seton                                        26  90
033700040609     c                   movel     Msg(1)        $Msg
033800040609     c                   goto      finct1
033900040609x   2c                   else
034000040609     c                   eval      D00dli = G08inv
034100040609e   2c                   endif
034200040609      * - data finale
034300040609     c                   reset                   WLBda8
034400040609     c                   eval      G08dat = V1Cdlf
034500040609     c                   call      'XSRDA8'
034600040609     c                   parm                    WLBda8
034700040609     c                   z-add     G08dat        V1Cdlf
034800040609if  2c                   if        G08err = '1'
034900040609     c                   seton                                        27  90
035000040609     c                   movel     Msg(1)        $Msg
035100040609     c                   goto      finct1
035200040609x   2c                   else
035300040609     c                   eval      D00dlf = G08inv
035400040609e   2c                   endif
035500040609e   1c                   endif
035600040609      * range Date/Ore
035700040609if  1c                   if        D00dli > *zeros
035800040609     c     *iso          move      D00dli        wData
035900040609x   1c                   else
036000040609     c                   reset                   wData
036100040609e   1c                   endif
036200040609     c                   eval      wTime  = V1Coli * 100
036300040609     c     *iso          move      wTime         wOra
036400040609     c                   movel     wTempo        wTempo1                        *periodo INIZIALE
036500040609if  1c                   if        D00dli > *zeros
036600040609     c     *iso          move      D00dlf        wData
036700040609x   1c                   else
036800040609     c                   reset                   wData
036900040609e   1c                   endif
037000040609     c                   eval      wTime  = V1Colf * 100
037100040609     c     *iso          move      wTime         wOra
037200040609     c                   movel     wTempo        wTempo2                        *periodo FINALE
037300040609if  1c                   if        wTempo1 > wTempo2
037400040609     c                   seton                                        2425
037500040609     c                   seton                                        262790
037600040609     c                   movel     Msg(9)        $Msg
037700040609     c                   goto      FinCt1
037800040609e   1c                   endif
037900040609      *
038000000000     C*
038100000000     C     finct1        ENDSR
038200000000     c*--------------------------------------------------------------------------------------------*
038300000000     c* chkfil - controlla la filiale
038400000000     c*--------------------------------------------------------------------------------------------*
038500000000     C     chkfil        BEGSR
038600000000     C*
038700000000     C                   Z-ADD     wfil          krgfil
038800000000     c     keyorg        CHAIN     azorg01l                           99
038900000000if  1c                   IF        *in99 OR                                     *inesistente O
039000000000     c                             orgfva <> *blanks OR                         *annullata   O
039100000000     c                             orgfag = 'V'                                 *varia
039200000000     C                   MOVEL     '1'           werr
039300000000x   1C                   ELSE
039400000000     C                   MOVEL     orgdes        wdfil                          *descrizione
039500000000e   1C                   ENDIF
039600020607      * P.O gestibile
039700020612     C                   IF        werr = '0'
039800020607     C     wfil          lookup    L1                                     35
039900020612     C  N35              MOVEL     '1'           werr
040000020607     C                   ENDIF
040100000000     C*
040200000000     C                   ENDSR
040300000000     c*--------------------------------------------------------------------------------------------*
040400000000     c* chkda8 - controlla la data (8)
040500000000     c*--------------------------------------------------------------------------------------------*
040600000000     C     chkda8        BEGSR
040700000000     C*
040800000000     C                   Z-ADD     wda8          g08dat
040900000000     C                   Z-ADD     *zeros        g08inv
041000000000     C                   MOVEL     '0'           g08err
041100000000     C                   CALL      'XSRDA8'
041200000000     C                   PARM                    wlbda8
041300000000     C                   Z-ADD     g08dat        wda8
041400000000if  1C                   IF        g08err='1'
041500000000     C                   MOVEL     '1'           werr
041600000000x   1C                   ELSE
041700000000     C                   Z-ADD     g08inv        da8w
041800000000e   1C                   ENDIF
041900000000     C*
042000000000     C                   ENDSR
042100040316      *---------------------------------------------------------------*
042200040316      * ChkTime - controllo orario (hh:mm:ss)                         *
042300040316      *---------------------------------------------------------------*
042400040316     c     ChkTime       BEGSR
042500040316      *
042600040316     c                   eval      wErr = *off
042700040316      *
042800040610if  1c                   if            wHH < 00  or  wHH > 23
042900040316     c                             or  wMM < 00  or  wMM > 59
043000040316     c                             or  wSS < 00  or  wSS > 59
043100040316     c                   eval      wErr = *on
043200040316e   1c                   endif
043300040316      *
043400040316     c                   ENDSR
043500000000     c*--------------------------------------------------------------------------------------------*
043600000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
043700000000     c*--------------------------------------------------------------------------------------------*
043800000000     C     f03ges        BEGSR
043900000000     C*
044000000000     C                   MOVEL     'S'           $fine                          *fine programma
044100000000     C*
044200000000     C                   ENDSR
044300000000     c*--------------------------------------------------------------------------------------------*
044400000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
044500000000     c*--------------------------------------------------------------------------------------------*
044600000000     C     f12ges        BEGSR
044700000000     C*
044800000000     C                   MOVEL     'S'           $fine                          *fine programma
044900000000     C*
045000000000     C                   ENDSR
045100000000     c*--------------------------------------------------------------------------------------------*
045200000000     C* entges - gestione tasto funzionale enter ==> Invio
045300000000     c*--------------------------------------------------------------------------------------------*
045400000000     C     entges        BEGSR
045500000000     c*
045600000000     c* imposta la ds di procedura
045700000000     c                   MOVEL     *blanks       d00op1                         *livello programma
045800000000     c                   MOVEL     '0'           d00f03                         *premuto F03
045900000000     c                   MOVEL     '0'           d00f12                         *premuto F12
046000000000     c                   MOVEL     '0'           d00err                         *errore
046100000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
046200000000     c                   Z-ADD     v1cfil        d00fil                         *filiale
046300011003     c                   MOVEL     v1capl        d00apl                         *filiale
046400000000     c                   Z-ADD     dfvv1c        d00dfv                         *data foglio
046500000000     c                   Z-ADD     v1cnpg        d00npg                         *categoria foglio
046600000000     c                   Z-ADD     v1cmtd        d00mtd                         *mm totali dettaglio
046700000000     c                   Z-ADD     v1cmtr        d00mtr                         *mm totali riepilogo
046800040609     c*** già fatto:     z-add     dliV1C        D00dli
046900040316     c                   z-add     V1Coli        D00oli
047000040609     c*** già fatto:     z-add     dlfV1C        D00dlf
047100040316     c                   z-add     V1Colf        D00olf
047200061002     c                   call      'FIMS08R'
047300061002     c                   parm                    KPJBA
047400061002     c                   parm                    FIMS00ds
047500000000     C                   MOVEL     '1'           $tesvd                          *riemettere testata
047600000000     C*
047700000000     C* se ritorna premuto f3, chiude il programma
047800000000IF  1c                   IF        d00f03='1'                                    *premuto f03
047900000000     c                   MOVEL     'S'           $fine                           *fine pgm
048000000000X   1c                   ELSE                                                    *non premuto f03
048100000000     C                   MOVEL     d00msg        $msg                            *messaggio
048200000000e   1c                   ENDIF
048300000000     c*
048400000000     C                   ENDSR
048500000000     c*--------------------------------------------------------------------------------------------*
048600000000     C* impvd0 - Impostazioni testata
048700000000     c*--------------------------------------------------------------------------------------------*
048800000000     C     impvd0        BEGSR
048900000000     C*
049000061002     c                   movel     SDSpgm        V1Tpgm
049100000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
049200000000     C*
049300000000     C                   ENDSR
049400040609      *
049500040609      *---------------------------------------------------------------*
049600040609      * REPDTL - Reperimento range di date relative alla lettura      *
049700040609      *---------------------------------------------------------------*
049800040609     c     RepDTL        BEGSR
049900040609      *
050000040609     c                   reset                   $Riemiss
050100040609     c                   movel     V1Capl        MSAapl                         *tipo applic.
050200040609     c                   z-add     V1Cfil        MSAfil                         *p.o.
050300040609     c                   z-add     dfvV1C        MSAdfv                         *data foglio
050400040609     c                   z-add     V1Cnpg        MSAnpg                         *categoria foglio
050500040609      *
050600040609      * imposto la data dell'orario iniziale
050700061116      *
050800170201     c     k04msa02      setll     FNMSA000
050900170201     c     k04msa02      reade     FNMSA000
051000061116      *
051100170201if  1c                   if        %eof(FNMSA02L)
051200061116      *
051300061116     c                   seton                                        20  90
051400061116     c                   movel     Msg(8)        $Msg
051500061116     c                   goto      NoRepDTL
051600061116x   1c                   else
051700061116if  2c                   if        V1Cdli = *zeros
051800061116     c                   reset                   WLBda8
051900061116     c                   eval      G08inv = MSAdtl
052000061116     c                   eval      G08err = '3'
052100061116     c                   call      'XSRDA8'
052200061116     c                   parm                    WLBda8
052300061116     c                   z-add     G08dat        V1Cdli
052400061116     c                   eval      $Riemiss = *on
052500061116e   2c                   endif
052600061116      *
052700061116      * imposto la data dell'orario finale
052800170201     c     k04msa02      setgt     FNMSA000
052900170201     c     k04msa02      readpe    FNMSA000
053000170201if  2c                   if        NOT %eof(FNMSA02L)
053100061116if  3c                   if        V1Cdlf = *zeros
053200061116     c                   reset                   WLBda8
053300061116     c                   eval      G08inv = MSAdtl
053400061116     c                   eval      G08err = '3'
053500061116     c                   call      'XSRDA8'
053600061116     c                   parm                    WLBda8
053700061116     c                   z-add     G08dat        V1Cdlf
053800061116     c                   eval      $Riemiss = *on
053900061116e   3c                   endif
054000061116e   2c                   endif
054100061116      *
054200061116e   1c                   endif
054300040609      *
054400040609     c     NoRepDTL      ENDSR
054500040609      *
054600000000     c*--------------------------------------------------------------------------------------------*
054700000000     C* *inzsr - operazioni iniziali
054800000000     c*--------------------------------------------------------------------------------------------*
054900000000     C     *inzsr        BEGSR
055000000000     c*
055100000000     c* ricevimento parametri
055200000000     c     *ENTRY        PLIST
055300000000     c                   PARM                    kpjba
055400040609     c                   PARM                    FIMS00ds
055500040609      *
055600040609     c                   clear                   FIMS00ds
055700020607      *
055800020607      * Reperisco dati job
055900020607     C                   EXSR      DatiJob
056000000000     C*
056100000000     C* reperimento dati utente
056200000000     c     keyorg        KLIST                                                  *azorg01l
056300000000     c                   KFLD                    krgfil                          -filiale
056400170201      * file FNMSA02L
056500170201     c     K04MSA02      klist                                                  *FNMSA02L
056600040609     c                   kfld                    MSAapl                          -tipo appl.
056700040609     c                   kfld                    MSAfil                          -p.o.
056800040609     c                   kfld                    MSAdfv                          -data f.v.
056900040609     c                   kfld                    MSAnpg                          -categ. f.v.
057000000000     C*
057100020607     C*
057200020607     C****  CARICO TABELLA FILIALI GESTITE £1  ****
057300020607     C                   CLEAR                   DSUL06
057400020607     C                   MOVE      '£1'          D06COD
057500020607     C                   MOVEL     SIMFEL        D06KEY
057600020607     C                   MOVEL     DSUL06        KPJBU
057700020607     C                   CALL      'TRUL06R'
057800020607     C                   PARM                    KPJBA
057900020607     C                   MOVEL     KPJBU         DSUL06
058000020607     C                   MOVEA     LIN           L1
058100000000     C*
058200000000     C* reperisce la data corrente
058300000000     C                   TIME                    n14                            *ora (6)+ data (8)
058400000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
058500000000     C                   Z-ADD     n8            cordat                         *data in g/m/aa
058600000000     C                   Z-ADD     n8            g08dat
058700000000     C                   Z-ADD     *zeros        g08inv
058800000000     C                   MOVEL     '0'           g08err
058900000000     C                   CALL      'XSRDA8'
059000000000     C                   PARM                    wlbda8
059100000000     C                   Z-ADD     g08inv        datcor                         *data in aa/m/g
059200000000     C*
059300000000     C                   ENDSR
059400020607      *----------------------------------------------------------------
059500020607      *  Reperisco Dati del job (Utente/Operativi)
059600020607      *----------------------------------------------------------------
059700020607     C     DatiJob       BegSr
059800020607      *
059900020607     C     *dtaara       define    §azute        azuteds
060000020607     C     *dtaara       define    §datiute      ddatiute
060100020607      *
060200020607     C                   in(E)     *dtaara
060300020607     C                   IF        %Error or RSUT = *blanks
060400020607     C                   call      'TIBS34R'
060500020607     C                   parm                    Tibs34Ds
060600020607     C                   in        *dtaara
060700020607     C                   ENDIF
060800020607      *
060900020607     C                   endsr
061000000000** -msg-
061100000000FIMS01R- Data errata                                                          1
061200000000FIMS01R- Inserire uno dei valori possibili                                    2
061300000000FIMS01R- I mm di riepilogo devono essere maggiori/uguali dei mm di dettaglio  3
061400000000FIMS01R- Filiale errata o non in gestione                                     4
061500040316FIMS01R- Tipo applicazione obbligatoria                                       5
061600040316FIMS01R- Tipo applicazione errata                                             6
061700040316FIMS01R- Orario errato                                                        7
061800040609FIMS01R- Non reperiti dati per il F.V. indicato                               8
061900040609FIMS01R- Orario iniziale successivo all'orario finale                         9
062000040609FIMS01R- 10
