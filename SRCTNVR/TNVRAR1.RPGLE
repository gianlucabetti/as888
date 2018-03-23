000100040406     H DECEDIT('0,') DATEDIT(*DMY.)
000200040406
000300040406     FCnAco00F  IF   E           K DISK
000400040406     FCnInd00F  IF   E           K DISK
000500040406     FFnAcr01L  IF   E           K DISK
000600040406     FAzORG01L  IF   E           K DISK
000700040406      *
000800040406     FPrtf198   O    F  198        PRINTER OFLIND(*INOF)
000900040406     FPrtf198X  O    F  198        PRINTER OFLIND(*INOG)
001000040406      *
001100040406      * Schiera per caricamento L6
001200040406     D L6              s              3  0 DIM(30)
001300040406      *
001400040406      * Ds per Trul06r - caricamento £1
001500040406     D Dsul06        e ds                  EXTNAME(trul06ds)
001600040406     D LIA                     1     90  0
001700040406     D                                     DIM(30)
001800040406      * Schiera per costanti di stampa
001900040406     D TPA             S             66    DIM(20) CTDATA PERRCD(1)
002000040406      *
002100040406      * Parametri di selezione
002200040406     D Param           ds
002300040406     D $fil                           3
002400040406     D $ArspcSN                       1
002500040406     D $CblSN                         1
002600040406     D $AcrSN                         1
002700040406     D $RagSN                         1
002800040406      *
002900040406     D tibs34ds      E DS                  inz
003000040406     D ddatiute      e ds
003100040406     D azuteds       e ds                  extname(AZUTE00F)
003200040406     D Kpjba         e ds
003300040406      *
003400040406     D $ACRCRO         S                   LIKE(ACRCRO)
003500040406     D $KSC            S                   LIKE(Acoksc)
003600040406     D x               s              2  0
003700040406     D $tot_Noaco      s              5  0
003800040406      *---------------------------------------------------------------*
003900040406      * INDICATORI UILIZZATI
004000040406      *---------------------------------------------------------------*
004100040406      * 05    - Per condizionare il salto dopo la stampa della testata
004200040406      * 06    - Per indicare ragione sociale diversa tra PdC e An.Ritiri
004300040406      * 21    - Letture, chain
004400040406      * 30,31 - Cicli di lettura
004500040406      *---------------------------------------------------------------*
004600040406      *
004700040406     C                   EXSR      INIZ
004800040406      *
004900040406      *  LETTURA ARCHIVIO PIANO DEI CONTI
005000040406      *
005100040406     C     kaco          setll     CnAco00f
005200040406     C     kaco          reade     CnAco00f                               30
005300040406     C
005400040406    1C                   DOW       *in30 = *off
005500040406      *
005600040406    2C                   IF        AcoFlg =' '
005700040406      *
005800040406      * Se richiesta esclusione clienti bloccati elaboro solo
005900040406      * i record con flag di blocco = ' '/'0'/'9'
006000040406    3C                   IF        $CblSN='I' or
006100040406     C                             ($CblSN='E' and AcoAbl=' ')   or
006200040406     C                             ($CblSN='E' and AcoAbl='0')   or
006300040406     C                             ($CblSN='E' and AcoAbl='9')   or
006400040406      * Se richiesto solo clienti bloccati elaboro solo
006500040406      * i record con flag di blocco <> ' '/'0'/'9'
006600040406     C                             ($CblSN='S' and AcoAbl <>' ' and
006700040406     C                               AcoAbl <> '0'and AcoAbl <> '9')
006800040406     c
006900040406     C                   EXSR      Ctr_Acr
007000040406      *
007100040406      * Stampo riga cliente da PdC,a meno che non ci siano selezioni attive
007200040406      * per cui devo escludere il record
007300040406    4C                   IF        ($AcrSN =' ' and $RagSN ='N') or
007400040406     C                             ($AcrSN = 'S' and $Esiste_Acr = *on ) or
007500040406     C                             ($AcrSN = 'N' and $Esiste_Acr = *off) or
007600040406     C                             ($RagSN = 'S' and $Uguale_Rag = *off)
007700040406     C                   EXSR      Prt_Dett1
007800040406      *
007900040406      * Aggancio anagrafica  ritiri
008000040406     C                   EXSR      Chn_Acr
008100040406      *
008200040406      *
008300040406    4C                   ENDIF
008400040406    3C                   ENDIF
008500040406    2C                   ENDIF
008600040406      *
008700040406     C     kaco          reade     CnAco00f                               30
008800040406     C                   ENDDO
008900040406      *
009000040406      * Stampo totale dei clienti senza ACR
009100040406      * Questo totale non viene stampato se è stata richiesta
009200040406      * la stampa dei clienti aventi ACR con ragione sociale
009300040406      * diversa da quella del PDC.
009400040406     C                   IF        $RagSN = 'N'
009500040406      *
009600040406      * testo salto pagina
009700040406     C   OF              DO
009800040406     C                   SETON                                        05
009900040406     C                   EXCEPT    TESTA
010000040406     C                   SETOFF                                       OF
010100040406     C                   ENDDO
010200040406      *
010300040406     C                   EXCEPT    TOT_NOAcr
010400040406     C                   ENDIF
010500040406      *
010600040406      * Fine stampa
010700040406      * testo salto pagina
010800040406     C   OF              DO
010900040406     C                   SETON                                        05
011000040406     C                   EXCEPT    TESTA
011100040406     C                   SETOFF                                       OF
011200040406     C                   ENDDO
011300040406     C                   EXCEPT    FINE
011400040406      *
011500040406      * SE RICHIESTA STAMPA ANAGRAFICA RITIRI SENZA PIANO DEI CONTI
011600040406      * Leggo il file FNACR01L per ogni P.O. della L6 e verifico
011700040406      * se esiste il suo relativo P.D.C.
011800040406      *
011900040406     c                   If        $ArspcSN = 'S'
012000040406     c                   z-add     *zeros        page
012100040406     c                   exsr      CTR_ACRACO
012200040406     c                   endif
012300040406      *
012400040406     C                   SETON                                        LR
012500040406      *-----------------------------------------------------------------*
012600040406      * Controllo esistenza Piano dei conti leggendo ACR    e
012700040406      *-----------------------------------------------------------------*
012800040406     C     Ctr_Acraco    BEGSR
012900040406      *
013000040406      * stampo la testata sempre
013100040406     c                   except    testa_bis
013200040406      *
013300040406     C                   clear                   $ACRCRO
013400040406     C     $ACRCRO       SETLL     FNACR01L
013500040406      *
013600040406    1C                   DO        *HIVAL
013700040406     C                   READ      FNACR01L
013800040406      *
013900040406    2C                   IF        %EOF(FNACR01L)
014000040406     C                   LEAVE
014100040406    2C                   ENDIF
014200040406      *
014300040406    2c                   if        acratb <> ' '
014400040406     c                   iter
014500040406    2c                   endif
014600040406      *
014700040406      * Verifico se esiste record relativo nel PDC
014800040406      *
014900040406     c                   eval      $ksc = acrcro /1000
015000040406     c     Kaco1         chain     Cnaco00f
015100040406    2c                   if        not %found(cnaco00f)
015200040406     c                   add       1             $tot_Noaco
015300040406      * stampo
015400040406     c                   exsr      ctr_prepar
015500040406    3C                   if        *inog
015600040406     C                   EXCEPT    TESTA_bis
015700040406     C                   SETOFF                                       OG
015800040406    3C                   END
015900040406     C                   EXCEPT    DETTA_bis
016000040406      *
016100040406    2c                   endif
016200040406      *
016300040406    1c                   enddo
016400040406      *
016500040406      * alla fine di tutto stampo i totali
016600040406      * testo salto pagina
016700040406     C                   if        *inog
016800040406     C                   EXCEPT    TESTA_bis
016900040406     C                   SETOFF                                       OG
017000040406     C                   END
017100040406     C                   EXCEPT    tot_noaco
017200040406     C                   EXCEPT    FINE_BIS
017300040406      *
017400040406     C                   ENDSR
017500040406      *-----------------------------------------------------------------*
017600040406      * Preparazione campi stampa FNACR
017700040406      *-----------------------------------------------------------------*
017800040406     C     Ctr_Prepar    BEGSR
017900040406      *
018000040406      * Pulizia campi
018100040406     C                   move      *zeros        $AcrRit           7 0
018200040406     C                   move      *zeros        $AcrOrd           3 0
018300040406     C                   move      *blanks       $AcrRag          30
018400040406     C                   move      *blanks       $AcrInr          35
018500040406     C                   move      *blanks       $Acrcar           9
018600040406     C                   move      *zeros        $Acrlor          35
018700040406     C                   move      *blanks       $Acrprr           2
018800040406     C                   move      *blanks       $Acrnar           3
018900040406     C                   move      *blanks       $AcrTac           1
019000040406     C                   move      *blanks       $TacDes           9
019100040406     C                   move      *blanks       $AcrTcr           1
019200040406     C                   move      *blanks       $TcrDes           7
019300040406     C                   move      *blanks       $Acrksc           7
019400040406      *
019500040406      *
019600040406      * Campi ACR
019700040406     C     AcrCro        div       1000          $AcrRit
019800040406     C                   move      AcrCro        $AcrOrd
019900040406     C                   movel     AcrRsr        $AcrRag
020000040406     C                   movel     AcrInr        $AcrInr
020100040406     C                   move      AcrCar        $AcrCar
020200040406     C                   movel     AcrLor        $AcrLor
020300040406     C                   movel     AcrPrr        $AcrPrr
020400040406     C                   movel     AcrNar        $AcrNar
020500040406     C                   move      AcrTac        $AcrTac           1
020600040406      *
020700040406      * Decodifica Tipo Anagrafica
020800040406     C                   IF        AcrTac = 'R'
020900040406     C                   movel     'RITIRO   '   $TacDes           9
021000040406     C                   ENDIF
021100040406     C                   IF        AcrTac = 'O'
021200040406     C                   movel     'ORDINANTE'   $TacDes
021300040406     C                   ENDIF
021400040406      *
021500040406      * Decodifica tipo ritiro
021600040406     C                   move      Acrtcr        $AcrTcr           1
021700040406     C                   SELECT
021800040406     C                   WHEN      AcrTcr = 'F'
021900040406     C                   move      'FISSO  '     $TcrDes           7
022000040406     C                   WHEN      AcrTcr = 'R'
022100040406     C                   move      'RICORR.'     $TcrDes
022200040406     C                   WHEN      AcrTcr = 'O'
022300040406     C                   move      'OCCAS. '     $TcrDes
022400040406     C                   WHEN      AcrTcr = 'M'
022500040406     C                   move      'MAI    '     $TcrDes
022600040406     C                   ENDSL
022700040406      * PdC
022800040406     C                   if        Acrksc <> *zeros
022900040406     C                   move      Acrksc        $Acrksc
023000040406     C                   else
023100040406     C                   move      *blanks       $Acrksc
023200040406     C                   endif
023300040406      *
023400040406     c                   endsr
023500040406      *-----------------------------------------------------------------*
023600040406      * Controllo esistenza An.Ritiri e controllo Rag.Sociale
023700040406      *-----------------------------------------------------------------*
023800040406     C     Ctr_Acr       BEGSR
023900040406      *
024000040406     C                   move      *off          $Esiste_Acr       1
024100040406     C                   move      *on           $Uguale_Rag       1
024200040406     C                   z-add     *zeros        KCro             10 0
024300040406     C     AcoKsc        mult      1000          KCro
024400040406     C     KCro          setll     fnacr01l
024500040406     C                   read      fnacr01l                               31
024600040406      *
024700040406   1aC                   IF        *in31 = *off
024800040406     C     AcrCro        div       1000          WAcoksc           7 0
024900040406   1-C                   ENDIF
025000040406      *
025100040406      * Leggo tutti i codici ritiro del cliente
025200040406   1aC                   DOW       WAcoKsc = Acoksc and *in31=*off
025300040406      *
025400040406   2aC                   IF        AcrAtb = *blanks
025500040406      *
025600040406      * Segnalo che esiste anagrafico ritiri
025700040406     C                   move      *on           $Esiste_Acr
025800040406      *
025900040406      * se non devo controllare la ragione sociale ho finito i controlli
026000040406      * ed esco dal ciclo
026100040406     C                   if        $RagSN ='N'
026200040406     C                   LEAVE
026300040406     C                   else
026400040406      *
026500040406      * controllo i due campi rag.sociale, appena ne trovo due diversi
026600040406      * lo segnalo ed esco dal ciclo.
026700040406     C                   if        AcrRsr <> AcoRag
026800040406     C                   move      *off          $Uguale_Rag       1
026900040406     C                   leave
027000040406     C                   endif
027100040406     C                   endif
027200040406      *
027300040406   2-C                   ENDIF
027400040406     C                   read      fnacr01l                               31
027500040406     C     AcrCro        div       1000          WAcoksc           7 0
027600040406   1-C                   ENDDO
027700040406      *
027800040406     C                   ENDSR
027900040406      *-----------------------------------------------------------------*
028000040406      * Stampo riga cliente
028100040406      *-----------------------------------------------------------------*
028200040406     C     Prt_Dett1     BEGSR
028300040406      *
028400040406      * Pulizia campi
028500040406     C                   move      *zeros        $ksc              7 0
028600040406     C                   move      *blanks       $rag             48
028700040406     C                   move      *blanks       $via             30
028800040406     C                   move      *blanks       $cit             30
028900040406     C                   move      *blanks       $cap              9
029000040406     C                   move      *blanks       $prv              2
029100040406     C                   move      *blanks       $sta              4
029200040406     C                   move      *blanks       $Abl              1
029300040406      *
029400040406      * Cliente piu'ragione sociale
029500040406     C                   z-add     AcoKsc        $Ksc
029600040406     C                   movel     AcoRag        $Rag
029700040406      *
029800040406      * Aggancio l'indirizzo
029900040406     C     kind          chain     CnInd00f                           21
030000040406     C                   IF        *in21 = *off and indatb = ' '
030100040406     C                   movel     IndVia        $Via
030200040406     C                   movel     IndCit        $Cit
030300040406     C                   move      IndCae        $Cap
030400040406     C                   movel     IndPrv        $Prv
030500040406     C                   movel     IndSta        $Sta
030600040406     C                   ENDIF
030700040406      *
030800040406      * Blocco
030900040406     C                   IF        AcoAbl <> '0' and acoabl <> '9'
031000040406     C                   movel     AcoAbl        $abl
031100040406     C                   ENDIF
031200040406      *
031300040406      * stampo dettaglio cliente
031400040406      * testo salto pagina
031500040406     C   OF              DO
031600040406     C                   SETOFF                                       05
031700040406     C                   EXCEPT    TESTA
031800040406     C                   SETOFF                                       OF
031900040406     C                   ENDDO
032000040406      *
032100040406     C                   EXCEPT    DETTA1
032200040406      *
032300040406     C                   ENDSR
032400040406      *-----------------------------------------------------------------*
032500040406      * Leggo anagrafico ritiri
032600040406      *-----------------------------------------------------------------*
032700040406     C     Chn_Acr       BEGSR
032800040406      *
032900040406      * Flag che segnala la ricerca del primo record su ACR del cliente
033000040406     C                   move      *on           $prima            1
033100040406     C                   z-add     *zeros        KCro             10 0
033200040406     C     AcoKsc        mult      1000          KCro
033300040406     C     KCro          setll     fnacr01l
033400040406     C                   read      fnacr01l                               31
033500040406      *
033600040406   1aC                   IF        *in31 = *off
033700040406     C     AcrCro        div       1000          WAcoksc           7 0
033800040406   1-C                   ENDIF
033900040406      *
034000040406   1aC                   DOW       WAcoKsc = Acoksc and *in31=*off
034100040406   2aC                   IF        AcrAtb = *blanks
034200040406      *
034300040406      * Stampo anagrafico rititi
034400040406     C                   EXSR      Prt_Dett2
034500040406   2-C                   ENDIF
034600040406     C                   read      fnacr01l                               31
034700040406     C     AcrCro        div       1000          WAcoksc           7 0
034800040406   1-C                   ENDDO
034900040406      * Se non ho stampato degli Acr del cliente incremento
035000040406      * il contatore
035100040406     C                   if        $prima = *on
035200040406     C                   add       1             $tot_NoAcr
035300040406     C                   endif
035400040406      *
035500040406     C                   ENDSR
035600040406      *-----------------------------------------------------------------*
035700040406      * Stampo dettaglio ACR
035800040406      *-----------------------------------------------------------------*
035900040406     C     Prt_Dett2     BEGSR
036000040406      *
036100040406      * testo salto pagina
036200040406     C   OF              DO
036300040406     C                   SETON                                        05
036400040406     C                   EXCEPT    TESTA
036500040406     C                   SETOFF                                       OF
036600040406     C                   ENDDO
036700040406      *
036800040406     C                   if        $prima = *on
036900040406     C                   EXCEPT    TESTA2
037000040406     C                   move      *off          $prima
037100040406     C                   endif
037200040406      *
037300040406     c                   exsr      ctr_prepar
037400040406      *
037500040406      * verifico se la ragione sociale PdC è diversa da quella ACR
037600040406     C                   if        AcrRsr <> AcoRag
037700040406     C                   seton                                        06
037800040406     C                   else
037900040406     C                   setoff                                       06
038000040406     C                   endif
038100040406      *
038200040406      *
038300040406      * stampo dettaglio ACR
038400040406      * testo salto pagina
038500040406     C   OF              DO
038600040406     C                   SETON                                        05
038700040406     C                   EXCEPT    TESTA
038800040406     C                   SETOFF                                       OF
038900040406     C                   ENDDO
039000040406     C                   EXCEPT    DETTA2
039100040406      *
039200040406     C                   ENDSR
039300040406      *-----------------------------------------------------------------*
039400040406      * SUBROUTINE INIZIALE
039500040406      *-----------------------------------------------------------------*
039600040406     C     INIZ          BEGSR
039700040406      *
039800040406     C     *ENTRY        PLIST
039900040406     C                   PARM                    kpjba
040000040406      *
040100040406     c     *dtaara       define    §azute        azuteds
040200040406     c     *dtaara       define    §datiute      ddatiute
040300040406     C                   in(E)     *dtaara
040400040406     C                   IF        %error  or RSUT = *blanks
040500040406     C                   CLEAR                   tibs34ds
040600040406     C                   CALL      'TIBS34R'
040700040406     C                   PARM                    tibs34ds
040800040406     C                   in        *dtaara
040900040406     c                   endif
041000040406      *
041100040406     C                   move      kpjbu         Param
041200040406      *
041300040406     C                   z-add     1             $kut              1 0
041400040406     C                   z-add     DutKci        $kcc              4 0
041500040406     C                   z-add     *zeros        $tot_NoAcr        5 0
041600040406     C                   move      046           OrgFil
041700040406     C     OrgFil        chain     AzOrg01l                           21
041800040406     C  n21              movel     OrgDes        $Fil_des         20
041900040406      *
042000040406     C                   TIME                    W0140            14 0
042100040406     C                   MOVE      W0140         UDATE8            8 0
042200040406      * Carico £6
042300040406     C                   clear                   Dsul06
042400040406     C                   move      '£6'          D06Cod
042500040406     C                   movel     046           D06Key
042600040406     C                   movel     Dsul06        kpjbu
042700040406     C                   call      'TRUL06R'
042800040406     C                   parm                    kpjba
042900040406     C                   movel     kpjbu         Dsul06
043000040406     C                   movea     LIA           L6
043100040406     C                   clear                   kpjbu
043200040406      *
043300040406      *
043400040406      * DEFINIZIONE CHIAVI E CAMPI :
043500040406     C     KAco          KList
043600040406     C                   Kfld                    $kut
043700040406     C                   Kfld                    $kcc
043800040406      *
043900040406     C     KAco1         KList
044000040406     C                   Kfld                    $kut
044100040406     C                   Kfld                    $kcc
044200040406     C                   Kfld                    $ksc
044300040406      *
044400040406     C     KInd          KList
044500040406     C                   Kfld                    $kut
044600040406     C                   Kfld                    $kcc
044700040406     C                   Kfld                    Acoksc
044800040406      *
044900040406     C     KACR          KList
045000040406     C                   Kfld                    $ACRCRO
045100040406      *
045200040406     C                   seton                                        OF
045300040406      *
045400040406     c                   eval      $cblsn = 'S'
045500040406     c                   eval      $ragsn = 'S'
045600040406     c                   eval      $acrsn = ' '
045700040406     c                   eval      $arspcsn = 'S'
045800040406     C                   ENDSR
045900040406      *-------------------------------------------------------------------------
046000040406     OPrtf198   E            TESTA             1
046100040406     O                       TPA(1)              66
046200040406     O                       TPA(2)             132
046300040406     O                       TPA(3)             198
046400040406     O          E            TESTA       1
046500040406     O                       TPA(4)              66
046600040406     O                       TPA(5)             132
046700040406     O                       TPA(6)             198
046800040406     O                       RSUT                22
046900040406     O                       UDATE8             170 '  /  /    '
047000040406     O                       PAGE          Z    192
047100040406     O          E            TESTA       1
047200040406     O                       TPA(7)              66
047300040406     O                       $Fil                24
047400040406     O                       $Fil_Des            45
047500040406     O                       TPA(8)             132
047600040406     O                       $CblSN              94
047700040406     O                       $AcrSN             125
047800040406     O                       TPA(9)             198
047900040406     O                       $RagSN             188
048000040406     O          E            TESTA       1
048100040406     O                       TPA(1)              66
048200040406     O                       TPA(2)             132
048300040406     O                       TPA(3)             198
048400040406     O          E   N05      TESTA       1
048500040406     O                       TPA(10)             66
048600040406     O                       TPA(11)            132
048700040406     O                       TPA(12)            198
048800040406     O          E    05      TESTA       1  1
048900040406     O                       TPA(10)             66
049000040406     O                       TPA(11)            132
049100040406     O                       TPA(12)            198
049200040406      *---------------------------------------------------------------
049300040406     O          E            DETTA1      2
049400040406     O                       $ksc              +  0
049500040406     O                       $Abl              +  3
049600040406     O                       $Rag              +  3
049700040406     O                       $via              +  1
049800040406     O                       $cap              +  1
049900040406     O                       $cit              +  1
050000040406     O                       $prv              +  1
050100040406     O                       $sta              +  5
050200040406      *---------------------------------------------------------------
050300040406     O          E            TESTA2      1
050400040406     O                                         +  3 'Anagrafica Ritiri'
050500040406      *---------------------------------------------------------------
050600040406     O          E            DETTA2      1
050700040406     O               06                        +  2 '*'
050800040406     O                       $AcrRit           +  1
050900040406     O                       $AcrOrd           +  1
051000040406     O                       $AcrRag           +  1
051100040406     O                       $AcrInr           +  1
051200040406     O                       $AcrCar           +  1
051300040406     O                       $AcrLor           +  1
051400040406     O                       $AcrPrr           +  1
051500040406     O                       $AcrNar           +  1
051600040406     O                                         +  1 'Tipo:'
051700040406     O                       $AcrTac           +  1
051800040406     O                                         +  0 '-'
051900040406     O                       $TacDes           +  0
052000040406     O                                         +  1 'Frequenza:'
052100040406     O                       $AcrTcr           +  1
052200040406     O                                         +  0 '-'
052300040406     O                       $TcrDes           +  0
052400040406     O                                         +  1 'Codice PdC:'
052500040406     O                       $AcrKsc           +  1
052600040406      *---------------------------------------------------------------
052700040406     O          E            FINE        2
052800040406     O                       TPA(13)            132
052900040406      *---------------------------------------------------------------
053000040406     O          E            TOT_NOACR   2
053100040406     O                       $Tot_NOAcr    2   +  3
053200040406     O                                         +  1 'Clienti senza Anagrafica -
053300040406     O                                              Ritiri'
053400040406     Oprtf198X  E            TESTA_BIS         1
053500040406     O                       TPA(1)
053600040406     O                       TPA(2)
053700040406     O                       TPA(3)
053800040406     O          E            TESTA_BIS   1
053900040406     O                       TPA(14)
054000040406     O                       TPA(15)
054100040406     O                       TPA(16)
054200040406     O                       RSUT                22
054300040406     O                       UDATE8             170 '  /  /    '
054400040406     O                       PAGE          Z    192
054500040406     O          E            TESTA_BIS   1
054600040406     O                       TPA(17)             66
054700040406     O                       $Fil                24
054800040406     O                       $Fil_Des            45
054900040406     O          E            TESTA_BIS   1
055000040406     O                       TPA(1)
055100040406     O                       TPA(2)
055200040406     O                       TPA(3)
055300040406     O          E            TESTA_BIS   1  1
055400040406     O                       TPA(18)
055500040406     O                       TPA(19)
055600040406     O                       TPA(20)
055700040406     O          E            DETTA_BIS   1
055800040406     O                       $AcrRit
055900040406     O                       $AcrOrd           +  1
056000040406     O                       $AcrRag           +  1
056100040406     O                       $AcrInr           +  1
056200040406     O                       $AcrCar           +  1
056300040406     O                       $AcrLor           +  1
056400040406     O                       $AcrPrr           +  1
056500040406     O                       $AcrNar           +  3
056600040406     O                                         +  3 'Tipo:'
056700040406     O                       $AcrTac           +  1
056800040406     O                                         +  0 '-'
056900040406     O                       $TacDes           +  0
057000040406     O                                         +  1 'Frequenza:'
057100040406     O                       $AcrTcr           +  1
057200040406     O                                         +  0 '-'
057300040406     O                       $TcrDes           +  0
057400040406     O                                         +  1 'Codice PdC:'
057500040406     O                       $AcrKsc           +  1
057600040406     O          E            TOT_NOACO   2
057700040406     O                       $Tot_NOAco    2   +  3
057800040406     O                                         +  1 'Anagrafiche Ritiri senza -
057900040406     O                                              Piano dei Conti'
058000040406      *---------------------------------------------------------------
058100040406     O          E            FINE_bis    2
058200040406     O                       TPA(13)            132
058300040406      *---------------------------------------------------------------
058400040406** TPA
058500040406* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    1
058600040406- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    2
058700040406- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *      3
058800040406* XXXXXXXXXXXXXXXXXXXX                    S T A M P A   P I A N O    4
058900040406  D E I   C O N T I   E   A N A G R A F I C A   R I T I R I          5
059000040406              FIOR36R                              PAG. XXXX  *      6
059100040406*   Punto Operativo: XXX OOOOOOOOOOOOOOOOOOOO               Selezi   7
059200040406oni:    -Clienti bloccati: X     -Clienti con An. Ritiri: X   -Cli   8
059300040406enti con Ragione Sociale diversa tra PdC e An. Ritiri: X      *      9
059400040406Cliente  Blc  Ragione sociale                                  Ind  10
059500040406irizzo                      C.A.P     Località                      11
059600040406  Prov.  Nazione                                                    12
059700040406              * F I N E   S  T  A  M  P  A *                        13
059800040406* XXXXXXXXXXXXXXXXXXXX                S T A M P A   A N A G R A F   14
059900040406I C A   R I T I R I   S E N Z A   P I A N O   D E I   C O N T I     15
060000040406              FIOR36R                              PAG. XXXX  *     16
060100040406*   Punto Operativo: XXX OOOOOOOOOOOOOOOOOOOO                       17
060200040406Cliente     Ragione Sociale                Indirizzo                18
060300040406             C.A.P     Località                           Prov. Na  19
060400040406z.                                                                  20
