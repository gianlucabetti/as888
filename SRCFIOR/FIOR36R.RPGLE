000100051003     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200030410      * FIOR36R *-----------------------------------------------------*
000300030428      *  - STAMPA PDC E ANAGRAFICA RITIRI                           *
000400020411      *---------------------------------------------------------------*
000500020604
000600030424     FCnAco00F  IF   E           K DISK
000700030424     FCnInd00F  IF   E           K DISK
000800030424     FFnAcr01L  IF   E           K DISK
000900030418     FAzORG01L  IF   E           K DISK
001000020604      *
001100030424     FPrtf198   O    F  198        PRINTER OFLIND(*INOF)
001200030508     FPrtf198X  O    F  198        PRINTER OFLIND(*INOG)
001300030424      *
001400030424      * Schiera per caricamento L6
001500030410     D L6              s              3  0 DIM(30)
001600030424      *
001700030424      * Ds per Trul06r - caricamento £1
001800030424     D Dsul06        e ds                  EXTNAME(trul06ds)
001900030424     D LIA                     1     90  0
002000030424     D                                     DIM(30)
002100030424      * Schiera per costanti di stampa
002200051003     D TPA             S             66    DIM(23) CTDATA PERRCD(1)
002300030410      *
002400030424      * Parametri di selezione
002500030417     D Param           ds
002600030507     D $fil                           3
002700030507     D $ArspcSN                       1
002800030507     D $CblSN                         1
002900030507     D $AcrSN                         1
003000030507     D $RagSN                         1
003100051003     d $tcrsn                         1
003200030424      *
003300030416     D tibs34ds      E DS                  inz
003400030416     D ddatiute      e ds
003500030416     D azuteds       e ds                  extname(AZUTE00F)
003600030424     D Kpjba         e ds
003700030507      *
003800030507     D $ACRCRO         S                   LIKE(ACRCRO)
003900030507     D $KSC            S                   LIKE(Acoksc)
004000030507     D x               s              2  0
004100030507     D $tot_Noaco      s              5  0
004200030416      *---------------------------------------------------------------*
004300030418      * INDICATORI UILIZZATI
004400030424      *---------------------------------------------------------------*
004500030418      * 05    - Per condizionare il salto dopo la stampa della testata
004600030428      * 06    - Per indicare ragione sociale diversa tra PdC e An.Ritiri
004700030418      * 21    - Letture, chain
004800030418      * 30,31 - Cicli di lettura
004900030418      *---------------------------------------------------------------*
005000030424      *
005100030404     C                   EXSR      INIZ
005200020604      *
005300030429      *  LETTURA ARCHIVIO PIANO DEI CONTI
005400030404      *
005500030410     C     kaco          setll     CnAco00f
005600030410     C     kaco          reade     CnAco00f                               30
005700030410     C
005800030410     C                   DOW       *in30 = *off
005900030410      *
006000030428     C                   IF        AcoFlg =' '
006100030428      *
006200030429      * Se richiesta esclusione clienti bloccati elaboro solo
006300030429      * i record con flag di blocco = ' '/'0'/'9'
006400031118     C                   IF        $CblSN='I' or
006500031118     C                             ($CblSN='E' and AcoAbl=' ')   or
006600031118      * Se richiesto solo clienti bloccati elaboro solo
006700031118      * i record con flag di blocco <> ' '/'0'/'9'
006800130315     C                             ($CblSN='S' and AcoAbl <>' ' )
006900030416      *
007000030410      * Controllo se il codice cliente appartiene alla £6
007100030416     C     AcoKsc        div       10000         $ksc03            3 0
007200030416     C     $Ksc03        lookup    L6                                     21
007300030416     C                   IF        *in21 = *on and $ksc03 <> *zeros
007400030418      *
007500030429      * Se richieste selezioni da elaborare in base all'An. Ritiri
007600030429      * eseguo subroutine per controlli
007700051003     c                   If        $acrsn <> *Blanks
007800030418      *
007900030418      * Controllo esistenza An.Ritiri e,se esiste,verifico uguaglianza
008000030418      * della Ragione sociale tra Acr e Aco
008100030418     C                   EXSR      Ctr_Acr
008200030418     C                   ENDIF
008300030416      *
008400030429      * Stampo riga cliente da PdC,a meno che non ci siano selezioni attive
008500030429      * per cui devo escludere il record
008600051003     c                   Select
008700051003      * stampa solo frequenza ritiro = mai
008800051003     c                   When      $tcrsn = 'S' and $mai = *On
008900051003     C                   EXSR      Prt_Dett1
009000051003     C                   EXSR      Chn_Acr
009100051003      * Escludi frequenza ritiro = mai
009200051003     c                   When      $tcrsn = 'E' and $mai = *On
009300051003     C                   EXSR      Prt_Dett1
009400051003     C                   EXSR      Chn_Acr
009500051003      * stampa ragioni sociali diverse
009600051003     c                   When      $ragsn = 'S' and $uguale_rag = *Off
009700030416     C                   EXSR      Prt_Dett1
009800030416     C                   EXSR      Chn_Acr
009900051003      * stampa solo se esiste anagrafico ritiri
010000051003     c                   When      $acrsn = 'S' and $esiste_acr = *On and
010100051003     c                             $tcrsn = 'I' and $ragsn = 'N'
010200051003     C                   EXSR      Prt_Dett1
010300051003     C                   EXSR      Chn_Acr
010400051003      * stampa solo se non esiste anagrafico ritiri
010500051003     c                   When      $acrsn = 'N' and $esiste_acr = *Off
010600051003     C                   EXSR      Prt_Dett1
010700051003      * stampa tutti
010800051003     c                   When      $acrsn = *Blanks
010900051003     C                   EXSR      Prt_Dett1
011000051003     C                   EXSR      Chn_Acr
011100030416      *
011200051003     c                   EndSl
011300030416      *
011400030428     C                   ENDIF
011500030410     C                   ENDIF
011600030410     C                   ENDIF
011700030417      *
011800030410     C     kaco          reade     CnAco00f                               30
011900030410     C                   ENDDO
012000030418      *
012100030424      * Stampo totale dei clienti senza ACR
012200030418      * Questo totale non viene stampato se è stata richiesta
012300030424      * la stampa dei clienti aventi ACR con ragione sociale
012400030424      * diversa da quella del PDC.
012500030418     C                   IF        $RagSN = 'N'
012600030424      *
012700030424      * testo salto pagina
012800030417     C   OF              DO
012900030417     C                   SETON                                        05
013000030417     C                   EXCEPT    TESTA
013100030417     C                   SETOFF                                       OF
013200030417     C                   ENDDO
013300030424      *
013400030418     C                   EXCEPT    TOT_NOAcr
013500030418     C                   ENDIF
013600030424      *
013700030429      * Fine stampa
013800030418      * testo salto pagina
013900030418     C   OF              DO
014000030418     C                   SETON                                        05
014100030418     C                   EXCEPT    TESTA
014200030418     C                   SETOFF                                       OF
014300030418     C                   ENDDO
014400030508     C                   EXCEPT    FINE
014500030507      *
014600030507      * SE RICHIESTA STAMPA ANAGRAFICA RITIRI SENZA PIANO DEI CONTI
014700030507      * Leggo il file FNACR01L per ogni P.O. della L6 e verifico
014800030507      * se esiste il suo relativo P.D.C.
014900020604      *
015000030507     c                   If        $ArspcSN = 'S'
015100030508     c                   z-add     *zeros        page
015200030507     c                   exsr      CTR_ACRACO
015300030507     c                   endif
015400030507      *
015500000000     C                   SETON                                        LR
015600030507      *-----------------------------------------------------------------*
015700030507      * Controllo esistenza Piano dei conti leggendo ACR    e
015800030507      *-----------------------------------------------------------------*
015900030507     C     Ctr_Acraco    BEGSR
016000030507      *
016100030507      * stampo la testata sempre
016200030507     c                   except    testa_bis
016300030507      *
016400030507      * per ogni P.O. della L6
016500030507      *
016600030507     c                   z-add     1             x
016700030507      *
016800030507     c     1             do        30            x
016900030507      *
017000030507     c                   IF        L6(X) = *zeros
017100030507     C                   ITER
017200030507     C                   ENDIF
017300030507      *
017400030507     C                   EVAL      $ACRCRO = (L6(X) * 10000000)
017500030507     C     $ACRCRO       SETLL     FNACR01L
017600030507      *
017700030507     C                   DO        *HIVAL
017800030507     C                   READ      FNACR01L
017900030507      *
018000030507     C                   IF        %EOF(FNACR01L)
018100030507     C                   LEAVE
018200030507     C                   ENDIF
018300030507      *
018400030507     c                   if        acratb <> ' '
018500030507     c                   iter
018600030507     c                   endif
018700030507      *
018800030507      * Verifico il P.O. di appartenenza con la  £6
018900030507      *
019000030507     c                   if        acrpoa <>  l6(x)
019100030507     c                   leave
019200030507     c                   endif
019300030507      *
019400030507      * Verifico se esiste record relativo nel PDC
019500030507      *
019600030507     c                   eval      $ksc = acrcro /1000
019700030507     c     Kaco1         chain     Cnaco00f
019800030507     c                   if        not %found(cnaco00f)
019900030507     c                   add       1             $tot_Noaco
020000030507      * stampo
020100030507     c                   exsr      ctr_prepar
020200030508     C                   if        *inog
020300030507     C                   EXCEPT    TESTA_bis
020400030508     C                   SETOFF                                       OG
020500030507     C                   END
020600030507     C                   EXCEPT    DETTA_bis
020700030507      *
020800030507     c                   endif
020900030507      *
021000030507     c                   enddo
021100030507      *
021200030507     c                   enddo
021300030507      * alla fine di tutto stampo i totali
021400030507      * testo salto pagina
021500030508     C                   if        *inog
021600030507     C                   EXCEPT    TESTA_bis
021700030508     C                   SETOFF                                       OG
021800030507     C                   END
021900030507     C                   EXCEPT    tot_noaco
022000030508     C                   EXCEPT    FINE_BIS
022100030507      *
022200030507     C                   ENDSR
022300030507      *-----------------------------------------------------------------*
022400030507      * Preparazione campi stampa FNACR
022500030507      *-----------------------------------------------------------------*
022600030507     C     Ctr_Prepar    BEGSR
022700030507      *
022800030507      * Pulizia campi
022900030507     C                   move      *zeros        $AcrRit           7 0
023000030507     C                   move      *zeros        $AcrOrd           3 0
023100030507     C                   move      *blanks       $AcrRag          30
023200030507     C                   move      *blanks       $AcrInr          35
023300030507     C                   move      *blanks       $Acrcar           9
023400030507     C                   move      *zeros        $Acrlor          35
023500030507     C                   move      *blanks       $Acrprr           2
023600030507     C                   move      *blanks       $Acrnar           3
023700030507     C                   move      *blanks       $AcrTac           1
023800030507     C                   move      *blanks       $TacDes           9
023900030507     C                   move      *blanks       $AcrTcr           1
024000030507     C                   move      *blanks       $TcrDes           7
024100030507     C                   move      *blanks       $Acrksc           7
024200030507      *
024300030507      *
024400030507      * Campi ACR
024500030507     C     AcrCro        div       1000          $AcrRit
024600030507     C                   move      AcrCro        $AcrOrd
024700030507     C                   movel     AcrRsr        $AcrRag
024800030507     C                   movel     AcrInr        $AcrInr
024900030507     C                   move      AcrCar        $AcrCar
025000030507     C                   movel     AcrLor        $AcrLor
025100030507     C                   movel     AcrPrr        $AcrPrr
025200030507     C                   movel     AcrNar        $AcrNar
025300030507     C                   move      AcrTac        $AcrTac           1
025400030507      *
025500030507      * Decodifica Tipo Anagrafica
025600030507     C                   IF        AcrTac = 'R'
025700030507     C                   movel     'RITIRO   '   $TacDes           9
025800030507     C                   ENDIF
025900030507     C                   IF        AcrTac = 'O'
026000030507     C                   movel     'ORDINANTE'   $TacDes
026100030507     C                   ENDIF
026200030507      *
026300030507      * Decodifica tipo ritiro
026400030507     C                   move      Acrtcr        $AcrTcr           1
026500030507     C                   SELECT
026600030507     C                   WHEN      AcrTcr = 'F'
026700030507     C                   move      'FISSO  '     $TcrDes           7
026800030507     C                   WHEN      AcrTcr = 'R'
026900030507     C                   move      'RICORR.'     $TcrDes
027000030507     C                   WHEN      AcrTcr = 'O'
027100081015     C                   move      'No Int.'     $TcrDes
027200030507     C                   WHEN      AcrTcr = 'M'
027300030507     C                   move      'MAI    '     $TcrDes
027400030507     C                   ENDSL
027500030508      * PdC
027600030508     C                   if        Acrksc <> *zeros
027700030508     C                   move      Acrksc        $Acrksc
027800030508     C                   else
027900030508     C                   move      *blanks       $Acrksc
028000030508     C                   endif
028100030507      *
028200030507     c                   endsr
028300030418      *-----------------------------------------------------------------*
028400030418      * Controllo esistenza An.Ritiri e controllo Rag.Sociale
028500030418      *-----------------------------------------------------------------*
028600030418     C     Ctr_Acr       BEGSR
028700030418      *
028800030418     C                   move      *off          $Esiste_Acr       1
028900030418     C                   move      *on           $Uguale_Rag       1
029000051003     c                   Move      *Off          $mai              1
029100030418     C                   z-add     *zeros        KCro             10 0
029200030418     C     AcoKsc        mult      1000          KCro
029300030418     C     KCro          setll     fnacr01l
029400030418     C                   read      fnacr01l                               31
029500030418      *
029600030418   1aC                   IF        *in31 = *off
029700030418     C     AcrCro        div       1000          WAcoksc           7 0
029800030418   1-C                   ENDIF
029900030418      *
030000030418      * Leggo tutti i codici ritiro del cliente
030100030418   1aC                   DOW       WAcoKsc = Acoksc and *in31=*off
030200030429      *
030300030418   2aC                   IF        AcrAtb = *blanks
030400030418      *
030500030418      * Segnalo che esiste anagrafico ritiri
030600030418     C                   move      *on           $Esiste_Acr
030700030418      *
030800030429      * se non devo controllare la ragione sociale ho finito i controlli
030900030429      * ed esco dal ciclo
031000051003      * e se non devo controllare la frequenza ritiro
031100051003     C                   if        $RagSN ='N' and $tcrsn = 'I'
031200030418     C                   LEAVE
031300051003     c                   endif
031400051003      * controllo la frequenza
031500051003     c                   If        $tcrsn = 'S' and acrtcr = 'M'
031600051003     c                   Eval      $mai = *On
031700051003      * se non devo controllare la ragione sociale esco dal ciclo
031800051003     c                   If        $ragsn = 'N'
031900051003     c                   Leave
032000051003     c                   EndIf
032100051003     c                   EndIf
032200051003     c                   If        $tcrsn = 'E' and acrtcr <> 'M'
032300051003     c                   Eval      $mai = *On
032400051003      * se non devo controllare la ragione sociale esco dal ciclo
032500051003     c                   If        $ragsn = 'N'
032600051003     c                   Leave
032700051003     c                   EndIf
032800051003     c                   EndIf
032900030418      *
033000030429      * controllo i due campi rag.sociale, appena ne trovo due diversi
033100030429      * lo segnalo ed esco dal ciclo.
033200051003      * se non devo controllare anche la frequenza
033300051003     C                   if        AcrRsr <> AcoRag and $ragsn = 'S'
033400030418     C                   move      *off          $Uguale_Rag       1
033500051003     c                   If        $tcrsn = 'I'
033600030418     C                   leave
033700051003     c                   EndIf
033800030418     C                   endif
033900030418      *
034000030418   2-C                   ENDIF
034100030418     C                   read      fnacr01l                               31
034200030418     C     AcrCro        div       1000          WAcoksc           7 0
034300030418   1-C                   ENDDO
034400030418      *
034500030418     C                   ENDSR
034600030416      *-----------------------------------------------------------------*
034700030416      * Stampo riga cliente
034800030416      *-----------------------------------------------------------------*
034900030416     C     Prt_Dett1     BEGSR
035000030416      *
035100030416      * Pulizia campi
035200030416     C                   move      *zeros        $ksc              7 0
035300030417     C                   move      *blanks       $rag             48
035400030417     C                   move      *blanks       $via             30
035500030417     C                   move      *blanks       $cit             30
035600030417     C                   move      *blanks       $cap              9
035700030416     C                   move      *blanks       $prv              2
035800030416     C                   move      *blanks       $sta              4
035900030416     C                   move      *blanks       $Abl              1
036000030416      *
036100030417      * Cliente piu'ragione sociale
036200030416     C                   z-add     AcoKsc        $Ksc
036300030416     C                   movel     AcoRag        $Rag
036400030417      *
036500030416      * Aggancio l'indirizzo
036600030418     C     kind          chain     CnInd00f                           21
036700030418     C                   IF        *in21 = *off and indatb = ' '
036800030416     C                   movel     IndVia        $Via
036900030416     C                   movel     IndCit        $Cit
037000030417     C                   move      IndCae        $Cap
037100030416     C                   movel     IndPrv        $Prv
037200030416     C                   movel     IndSta        $Sta
037300030416     C                   ENDIF
037400030416      *
037500030417      * Blocco
037600030416     C                   movel     AcoAbl        $abl
037700130321      * se blocco "7" automatico imposto "8"
037800130321     c                   IF        $abl = '7'
037900130321     c                   eval      $abl = '8'
038000130321     c                   ENDIF
038100030429      *
038200030429      * stampo dettaglio cliente
038300030416      * testo salto pagina
038400030417     C   OF              DO
038500030417     C                   SETOFF                                       05
038600030417     C                   EXCEPT    TESTA
038700030417     C                   SETOFF                                       OF
038800030417     C                   ENDDO
038900030417      *
039000030416     C                   EXCEPT    DETTA1
039100030416      *
039200030416     C                   ENDSR
039300030416      *-----------------------------------------------------------------*
039400030416      * Leggo anagrafico ritiri
039500030416      *-----------------------------------------------------------------*
039600030416     C     Chn_Acr       BEGSR
039700030416      *
039800030429      * Flag che segnala la ricerca del primo record su ACR del cliente
039900030416     C                   move      *on           $prima            1
040000030416     C                   z-add     *zeros        KCro             10 0
040100030416     C     AcoKsc        mult      1000          KCro
040200030416     C     KCro          setll     fnacr01l
040300030416     C                   read      fnacr01l                               31
040400030416      *
040500030416   1aC                   IF        *in31 = *off
040600030416     C     AcrCro        div       1000          WAcoksc           7 0
040700030418   1-C                   ENDIF
040800030416      *
040900030416   1aC                   DOW       WAcoKsc = Acoksc and *in31=*off
041000030416   2aC                   IF        AcrAtb = *blanks
041100030429      *
041200030416      * Stampo anagrafico rititi
041300030416     C                   EXSR      Prt_Dett2
041400030416   2-C                   ENDIF
041500030416     C                   read      fnacr01l                               31
041600030416     C     AcrCro        div       1000          WAcoksc           7 0
041700030416   1-C                   ENDDO
041800030418      * Se non ho stampato degli Acr del cliente incremento
041900030417      * il contatore
042000030417     C                   if        $prima = *on
042100030418     C                   add       1             $tot_NoAcr
042200030417     C                   endif
042300030416      *
042400030416     C                   ENDSR
042500030416      *-----------------------------------------------------------------*
042600030418      * Stampo dettaglio ACR
042700030416      *-----------------------------------------------------------------*
042800030416     C     Prt_Dett2     BEGSR
042900030429      *
043000030416      * testo salto pagina
043100030417     C   OF              DO
043200030417     C                   SETON                                        05
043300030417     C                   EXCEPT    TESTA
043400030417     C                   SETOFF                                       OF
043500030417     C                   ENDDO
043600030417      *
043700030416     C                   if        $prima = *on
043800030416     C                   EXCEPT    TESTA2
043900030416     C                   move      *off          $prima
044000030416     C                   endif
044100030507      *
044200030507     c                   exsr      ctr_prepar
044300030417      *
044400030429      * verifico se la ragione sociale PdC è diversa da quella ACR
044500030417     C                   if        AcrRsr <> AcoRag
044600030417     C                   seton                                        06
044700030417     C                   else
044800030417     C                   setoff                                       06
044900030417     C                   endif
045000030416      *
045100030429      *
045200030429      * stampo dettaglio ACR
045300030417      * testo salto pagina
045400030417     C   OF              DO
045500030417     C                   SETON                                        05
045600030417     C                   EXCEPT    TESTA
045700030417     C                   SETOFF                                       OF
045800030417     C                   ENDDO
045900030416     C                   EXCEPT    DETTA2
046000030416      *
046100030416     C                   ENDSR
046200030416      *-----------------------------------------------------------------*
046300030404      * SUBROUTINE INIZIALE
046400030404      *-----------------------------------------------------------------*
046500030404     C     INIZ          BEGSR
046600030404      *
046700030404     C     *ENTRY        PLIST
046800030410     C                   PARM                    kpjba
046900030416      *
047000030416     c     *dtaara       define    §azute        azuteds
047100030416     c     *dtaara       define    §datiute      ddatiute
047200030416     C                   in(E)     *dtaara
047300030416     C                   IF        %error  or RSUT = *blanks
047400030416     C                   CLEAR                   tibs34ds
047500030416     C                   CALL      'TIBS34R'
047600030416     C                   PARM                    tibs34ds
047700030416     C                   in        *dtaara
047800030416     c                   endif
047900030416      *
048000030417     C                   move      kpjbu         Param
048100030404      *
048200030410     C                   z-add     1             $kut              1 0
048300030417     C                   z-add     DutKci        $kcc              4 0
048400030418     C                   z-add     *zeros        $tot_NoAcr        5 0
048500030418     C                   move      $Fil          OrgFil
048600030418     C     OrgFil        chain     AzOrg01l                           21
048700030418     C  n21              movel     OrgDes        $Fil_des         20
048800030417      *
048900030417     C                   TIME                    W0140            14 0
049000030417     C                   MOVE      W0140         UDATE8            8 0
049100030410      * Carico £6
049200030410     C                   clear                   Dsul06
049300030410     C                   move      '£6'          D06Cod
049400030410     C                   movel     $Fil          D06Key
049500030410     C                   movel     Dsul06        kpjbu
049600030410     C                   call      'TRUL06R'
049700030410     C                   parm                    kpjba
049800030410     C                   movel     kpjbu         Dsul06
049900030410     C                   movea     LIA           L6
050000030410     C                   clear                   kpjbu
050100030410      *
050200030404      *
050300030404      * DEFINIZIONE CHIAVI E CAMPI :
050400030410     C     KAco          KList
050500030410     C                   Kfld                    $kut
050600030410     C                   Kfld                    $kcc
050700030417      *
050800030507     C     KAco1         KList
050900030507     C                   Kfld                    $kut
051000030507     C                   Kfld                    $kcc
051100030507     C                   Kfld                    $ksc
051200030507      *
051300030416     C     KInd          KList
051400030416     C                   Kfld                    $kut
051500030416     C                   Kfld                    $kcc
051600030416     C                   Kfld                    Acoksc
051700030404      *
051800030507     C     KACR          KList
051900030507     C                   Kfld                    $ACRCRO
052000030507      *
052100030416     C                   seton                                        OF
052200030404     C                   ENDSR
052300920508      *-------------------------------------------------------------------------
052400030417     OPrtf198   E            TESTA             1
052500030417     O                       TPA(1)              66
052600030417     O                       TPA(2)             132
052700030417     O                       TPA(3)             198
052800030417     O          E            TESTA       1
052900030417     O                       TPA(4)              66
053000030417     O                       TPA(5)             132
053100030417     O                       TPA(6)             198
053200030417     O                       RSUT                22
053300030417     O                       UDATE8             170 '  /  /    '
053400030417     O                       PAGE          Z    192
053500030417     O          E            TESTA       1
053600030418     O                       TPA(7)              66
053700030428     O                       $Fil                24
053800030428     O                       $Fil_Des            45
053900030418     O                       TPA(8)             132
054000030428     O                       $CblSN              94
054100030428     O                       $AcrSN             125
054200030418     O                       TPA(9)             198
054300030428     O                       $RagSN             188
054400051003     O          E            TESTA       1
054500051003     O                       TPA(21)             66
054600051003     O                       TPA(22)            132
054700051003     O                       $tcrsn             112
054800051003     O                       TPA(23)            198
054900030418     O          E            TESTA       1
055000030418     O                       TPA(1)              66
055100030418     O                       TPA(2)             132
055200030418     O                       TPA(3)             198
055300030417     O          E   N05      TESTA       1
055400030418     O                       TPA(10)             66
055500030418     O                       TPA(11)            132
055600030418     O                       TPA(12)            198
055700030417     O          E    05      TESTA       1  1
055800030418     O                       TPA(10)             66
055900030418     O                       TPA(11)            132
056000030418     O                       TPA(12)            198
056100030404      *---------------------------------------------------------------
056200030417     O          E            DETTA1      2
056300030417     O                       $ksc              +  0
056400030417     O                       $Abl              +  3
056500030417     O                       $Rag              +  3
056600030417     O                       $via              +  1
056700030417     O                       $cap              +  1
056800030417     O                       $cit              +  1
056900030416     O                       $prv              +  1
057000030417     O                       $sta              +  5
057100030416      *---------------------------------------------------------------
057200030416     O          E            TESTA2      1
057300030424     O                                         +  3 'Anagrafica Ritiri'
057400030416      *---------------------------------------------------------------
057500030417     O          E            DETTA2      1
057600030417     O               06                        +  2 '*'
057700030417     O                       $AcrRit           +  1
057800030416     O                       $AcrOrd           +  1
057900030416     O                       $AcrRag           +  1
058000030416     O                       $AcrInr           +  1
058100030416     O                       $AcrCar           +  1
058200030416     O                       $AcrLor           +  1
058300030416     O                       $AcrPrr           +  1
058400030416     O                       $AcrNar           +  1
058500030507     O                                         +  1 'Tipo:'
058600030416     O                       $AcrTac           +  1
058700030417     O                                         +  0 '-'
058800030417     O                       $TacDes           +  0
058900030417     O                                         +  1 'Frequenza:'
059000030417     O                       $AcrTcr           +  1
059100030417     O                                         +  0 '-'
059200030417     O                       $TcrDes           +  0
059300030428     O                                         +  1 'Codice PdC:'
059400030416     O                       $AcrKsc           +  1
059500030417      *---------------------------------------------------------------
059600030417     O          E            FINE        2
059700030418     O                       TPA(13)            132
059800030417      *---------------------------------------------------------------
059900030418     O          E            TOT_NOACR   2
060000030418     O                       $Tot_NOAcr    2   +  3
060100030417     O                                         +  1 'Clienti senza Anagrafica -
060200030424     O                                              Ritiri'
060300030508     Oprtf198X  E            TESTA_BIS         1
060400030507     O                       TPA(1)
060500030507     O                       TPA(2)
060600030507     O                       TPA(3)
060700030507     O          E            TESTA_BIS   1
060800030507     O                       TPA(14)
060900030507     O                       TPA(15)
061000030507     O                       TPA(16)
061100030507     O                       RSUT                22
061200030507     O                       UDATE8             170 '  /  /    '
061300030507     O                       PAGE          Z    192
061400030507     O          E            TESTA_BIS   1
061500030507     O                       TPA(17)             66
061600030507     O                       $Fil                24
061700030507     O                       $Fil_Des            45
061800030507     O          E            TESTA_BIS   1
061900030507     O                       TPA(1)
062000030507     O                       TPA(2)
062100030507     O                       TPA(3)
062200030507     O          E            TESTA_BIS   1  1
062300030507     O                       TPA(18)
062400030507     O                       TPA(19)
062500030507     O                       TPA(20)
062600030507     O          E            DETTA_BIS   1
062700030507     O                       $AcrRit
062800030507     O                       $AcrOrd           +  1
062900030508     O                       $AcrRag           +  1
063000030508     O                       $AcrInr           +  1
063100030507     O                       $AcrCar           +  1
063200030507     O                       $AcrLor           +  1
063300030508     O                       $AcrPrr           +  1
063400030508     O                       $AcrNar           +  3
063500030507     O                                         +  3 'Tipo:'
063600030507     O                       $AcrTac           +  1
063700030507     O                                         +  0 '-'
063800030507     O                       $TacDes           +  0
063900030507     O                                         +  1 'Frequenza:'
064000030507     O                       $AcrTcr           +  1
064100030507     O                                         +  0 '-'
064200030507     O                       $TcrDes           +  0
064300030508     O                                         +  1 'Codice PdC:'
064400030508     O                       $AcrKsc           +  1
064500030507     O          E            TOT_NOACO   2
064600030507     O                       $Tot_NOAco    2   +  3
064700030507     O                                         +  1 'Anagrafiche Ritiri senza -
064800030507     O                                              Piano dei Conti'
064900030508      *---------------------------------------------------------------
065000030508     O          E            FINE_bis    2
065100030508     O                       TPA(13)            132
065200030417      *---------------------------------------------------------------
065300030417** TPA
065400030417* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    1
065500030417- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    2
065600030417- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *      3
065700030417* XXXXXXXXXXXXXXXXXXXX                    S T A M P A   P I A N O    4
065800030428  D E I   C O N T I   E   A N A G R A F I C A   R I T I R I          5
065900030417              FIOR36R                              PAG. XXXX  *      6
066000070418*           Filiale: XXX OOOOOOOOOOOOOOOOOOOO               Selezi   7
066100031120oni:    -Clienti bloccati: X     -Clienti con An. Ritiri: X   -Cli   8
066200030428enti con Ragione Sociale diversa tra PdC e An. Ritiri: X      *      9
066300030428Cliente  Blc  Ragione sociale                                  Ind  10
066400030428irizzo                      C.A.P     Località                      11
066500030428  Prov.  Nazione                                                    12
066600030418              * F I N E   S  T  A  M  P  A *                        13
066700030507* XXXXXXXXXXXXXXXXXXXX                S T A M P A   A N A G R A F   14
066800030507I C A   R I T I R I   S E N Z A   P I A N O   D E I   C O N T I     15
066900030507              FIOR36R                              PAG. XXXX  *     16
067000070418*           Filiale: XXX OOOOOOOOOOOOOOOOOOOO                       17
067100030508Cliente     Ragione Sociale                Indirizzo                18
067200030508             C.A.P     Località                           Prov. Na  19
067300030508z.                                                                  20
067400051003*                                                                   21
067500051003        -Con tipo frequenza uguale a  M-MAI: X                      22
067600051003                                                              *     23
