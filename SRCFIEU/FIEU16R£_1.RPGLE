000100050418     H DECEDIT('0,') DATEDIT(*DMY/)
000200050418      ***************************************************************************
000300050418      *  CREA FILE PER TRASMISSIONE SCAN A DPD                                  *
000400050418      ***************************************************************************
000500050418      *   Codici aggiuntivi forzati
000600050418      *  ---------------------------
000700050418      * 11 - Indirizzo errato per lo scan 06 Reso
000800050418      * 44 - Pacchi piccoli
000900050418      *
001000050418      ***************************************************************************
001100060609     FtIDP303L  UF A E           K DISK
001200060609     FtIDP410W  O  A E             DISK    usropn
001300060609     FtIDP411W  IF   E           K DISK    usropn rename(tIDPW41:tIDP41W)
001400050418      *
001500050418     FFNARB01L  IF   E           K DISK
001600050418     FFNBLP01L  IF   E           K DISK
001700060727     FFNBLT01L  IF   E           K DISK
001800060213     Ffiar401L  IF   E           K DISK
001900160502     Ffiar501L  IF   E           K DISK
002000050418     FTABEL00F  IF   E           K DISK
002100050418     FTNTBE01L  IF   E           K DISK
002200050418     FAZORG01L  IF   E           K DISK
002300060613     ftivgd00f  if a E             DISK
002400101126      *
002500101126     Ftigcp01L  IF   E           K DISK
002600161125      *----------------------------------------------------*                                 SOLO IL
002700170407     D File_vers       S              1    inz('S')                             E=Entrambe T=TTEVENT
002800161125     D VGDTIP_$E       S              6    inz('APRE  ')
002900161125     D TESTA_TTE       S              2    inz('SI')
003000161125     d dati_TTE        S           2050    inz(' ')
003100161125      *----------------------------------------------------*
003200080903     D K2Z             S              3    DIM(999) inz                         CODICE NAZIONE
003300080903     D T2Z             S             89    DIM(999) inz                         CODICE NAZIONE
003400050418     D KEVD            S             15    DIM(30)  inz                         CODICE NAZIONE
003500050418     D EVD             S            256    DIM(30)  inz                         CODICE NAZIONE
003600161129     D hubDPD          S              3  0 DIM(99)                              Testata scan
003700050418     D POBAR           S              3  0 DIM(500)                             Testata scan
003800050418     D PODPD           S             25    DIM(500)                             Testata scan
003900050418     D ZCOD            S              2    DIM(3)                               P.O. COMODO
004000050418     D S3A             S              2    DIM(30)                              Tipo bolla
004100060727     D S3E             S              1    DIM(50)                              Tipo Anomalia
004200050418      *-------------------
004300161125     D HEAD            S             80    DIM(15) CTDATA PERRCD(1)
004400050418     D CMD             S             78    DIM(1) CTDATA PERRCD(1)
004500050418      *---------------------------------------------------------------------*
004600050418     D  MSGDS          S            100
004700050418     D  LENGH          S             15  5
004800050418      *---------------------------------------------------------------------*
004900161125     D Sender_Depot    S              4
005000161125     D Data_Evento     S              8
005100161130     D Ora__Evento     S              4
005200161130     D HMS__Evento     S              6
005300161125     D ServiceCODE     S              3
005400050418     D DIST            S              3    inz('001')
005500050418     D SSTOP           S              3    inz('001')
005600161125     D CAP_Dest        S              9
005700161128     D Delivery_Depot  S              4
005800161125     D CountryCodISO   S              3
005900161128     D Firma_Consegna  S             35
006000161128     D ContInfo1       S              2
006100161128     D ContInfo2       S            200
006200170410     D PodImage7       S              7
006300050418      *-------------------
006400060328     D wIMPORT         S              1A   inz
006500140331     D Peso_in_10gr    S              8S 0 inz
006600150323     D Peso_in_BLT     S              6S 0 inz
006700050418      *-------------------
006800050418     D XX              S              3  0 inz
006900050418     D YY              S              3  0 inz
007000050418     D WnewRec         S              7  0                                      *rec da trasmettere
007100050418     D W0080           S              8  0 inz
007200050418     D W008M           S              8  0 inz
007300050418     D W0140           S             14  0 inz
007400161128     D SCAN_CODE       S              2
007500050418     D W015A           S             15
007600161125     D ParcelNr        S             14
007700060727     D Wanomalia       S              1
007800050418     D Welabora        S              1
007900050418     D Wtestata        S              1
008000050418     D Wdataoggi       S              8
008100050418     D Wdata80         S              8  0 inz
008200050418     D Wdata_90        S              8
008300050418     D Wora            S              6
008400050418     D Wdata           S               D   DATFMT(*eur)
008500050418     D W0m90           S               D   DATFMT(*eur)
008600050418     D Kcod            S                   LIKE(TBLCOD)
008700060612     D KTRC            S                   LIKE(AR4TRC)
008800050418     D KDTR            S                   LIKE(DP3DTR)  INZ
008900050418     D KFTR            S                   LIKE(DP3FTR)  INZ
009000050418     D WPKC01          S                   LIKE(§DPPpkb)
009100050418     D WPKC02          S                   LIKE(§DPPpkb)
009200050418     D Kcode           S                   LIKE(TBECOD)
009300050418     D Kke1            S                   LIKE(TBEKE1)
009400060620     D S41len          S                   LIKE(w41len)
009500050418     D S41dep          S                   LIKE(w41dep)
009600050418     D S41typ          S                   LIKE(w41typ)
009700050418     D S41dat          S                   LIKE(w41dat)
009800050418     D S41tim          S                   LIKE(w41tim)
009900050418     D S41acc          S                   LIKE(w41acc)
010000050418     D S41tou          S                   LIKE(w41tou)
010100080403     D d41dat          S           2048
010200050418      *-------------------
010300160516     d non_flaggare_DP3...
010400160516     d                 S              1
010500160824      *-------------------
010600060621     D XTOEBCDDS     E DS
010700060619     D DS15          E DS
010800150826     D DSQT1         E DS                  INZ
010900050418     D DDPP          E DS
011000050418     D DS3A          E DS
011100060727     D DS3E          E DS
011200060609     D DSBL4i        E DS
011300050418     D DS2Z          E DS
011400050418     D DEVD          E DS
011500050418     D CNCR80        E DS
011600050418     D UT§DSE0F      E DS
011700050418     D OG143         E DS
011800151211     D dsDP3FLR      E DS
011900160502     D Dar5EMD       E DS
012000130704      *
012100050418     D DSFNBLP       E DS                  EXTNAME(FNBLP00F)
012200050418     D DSFNARB       E DS                  EXTNAME(FNARB00F)
012300130704      *
012400050418     D KPJBA         E DS
012500050418     D TRUL06DS      E DS
012600050418     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
012700050418     D tibs36ds      E DS
012800050418      *
012900060613     d trul47ds      e ds
013000130704
013100081015     d DSCAP           ds
013200081015     d  CAPinSK                1      9
013300081015     D SkCAP                   1      9    dim(9)
013400050418      *----------------------------------------------------*
013500050418     IFNARB000
013600050418     I              ARBAAS                      BLPAAS
013700050418     I              ARBLNA                      BLPLNA
013800050418     I              ARBPKB                      BLPPKB
013900150709     I              ARBPKC                      BLPPKC
014000050418     I              ARBCAD                      BLPCAD
014100161125     I              ARBLOD                      BLPLOD
014200050418     I              ARBNZD                      BLPNZD
014300050418     I              ARBCBO                      BLPCBO
014400150826     I              ARBNCL                      BLPNCL
014500150826     I              ARBNCP                      BLPNCP
014600161125     I              ARBRMA                      BLPRMA
014700161125     I              ARBRMN                      BLPRMN
014800060619      *----------------------------------------------------*
014900161125     c                   if        File_vers ='E'
015000170407     c                               or File_vers ='S'
015100050418      *    Prima Pulisce il file di lavoro
015200050418     C                   clear                   MSGDS
015300050418     C                   MOVEL     CMD(1)        MSGDS
015400050418     C                   Z-ADD     100           LENGH
015500050418     C                   CALL      'QCMDEXC'                            98
015600050418     C                   PARM                    MSGDS
015700050418     C                   PARM                    LENGH
015800060609     c                   open      tIDP410W
015900161125     c                   end
016000161125      *----------------------------------------------------*
016100060609     c     KDP3          setll     tIDP3000
016200060609     c     KDP3          readE     tIDP3000
016300060609     C                   DOW       NOT %eof(tIDP303L)
016400161129
016500050418      *   elaboro se data evento impostata
016600161129      *   IMPOSTA subito lo SCAN
016700050418     C                   IF        DP3dev > *zeros
016800161129     c                   movel     dp3evd        Scan_CODE
016900161129
017000161129      *   se lo SCAN è in tabella EVD
017100161129     c                   eval      w015A = SCAN_CODE
017200050418     c                   z-add     1             XX
017300050418     c     w015a         lookup    KEVD(xx)                               31
017400161129      ** quindi Considera se è uno SCAN
017500161129      ** oppure x gli ORM Italia su Italia (detti Italy Domestic) LNP e LNA Italia
017600161129     c                   IF        *IN31 = *on or Dp3CEV = 'ORM'
017700050418     C                   movel     EVD(XX)       devd
017800130705
017900050418     c                   IF        §evditr = 'S'  or  §evdetr = 'S'
018000080603     c                             or Dp3CEV = 'ORM'
018100050418
018200050418      *   Aggancio file Partenza o Arrivi per reperire dati
018300161129     c                   clear                   qualifica        10
018400050418     c                   clear                   Welabora
018500161125     c                   clear                   ParcelNr
018600161125     c                   clear                   ServiceCODE
018700060727     c                   clear                   Wanomalia
018800161128     c                   clear                   Delivery_Depot
018900050418     c                   clear                   dsFNBLP
019000050418     c                   clear                   dsFNARB
019100060609     c                   clear                   DSBL4i
019200150605     c                   clear                   Peso_in_BLT
019300161128     c                   clear                   Firma_consegna
019400050418      *
019500161129      * Se trovato con LNP    --> è una spedizione Import DPD
019600161129     C                   z-add     1             YY
019700161129     c     DP3lnp        lookup    hubDPD(YY)                             33
019800161129     c                   eval      wIMPORT  = 'N'
019900060328     c   33              eval      wIMPORT  = 'S'
020000050418      *
020100050418     C                   SELECT
020200050418      *
020300161129      ** se è un evento di import da trattare oppure
020400161129      ** è un evento x gli ORM Italia su Italia (detti Italy Domestic)
020500161129     c                   WHEN      (wIMPORT  = 'S' and §evditr = 'S')
020600080603     c                             or Dp3CEV = 'ORM'
020700130705      *
020800130705     c     Kspe          chain     FNBLP01L
020900130705     c                   if        %found(FNBLP01L)
021000050418     c                   eval      Welabora = 'S'
021100161129      *
021200161129      * Cerca la Firma di Consegna della spedizione
021300161129     c                   if        SCAN_code = '13'
021400161129     c                   move      '1'           ktrc
021500161129     c     Kar4          chain     fiar401l
021600161129     c                   if        %found(fiar401l) and ar4not <> *blank
021700170502     c     ';':' '       xlate     ar4not        Firma_Consegna
021800161129     c                   end
021900161129     c                   endif
022000161129      *
022100060727      * ? controllo su FNBLT se esiste un'anomalia (APERTO, BAGNATO....SCONDIZIONATO)
022200060727      * ? da segnalare a DPD onde non prendersi poi la responsabilità del DANNO.
022300061011      * ?  solo se non è stato dato già x consegnato
022400150323     c                   clear                   Peso_in_BLT
022500061011     c                   if        BLPdcm = 0
022600060727     c     Kspe          chain     FNBLT01L
022700150323     c                   if        %Found(FNBLT01L) and bltPUC > 0
022800150323      * ?  si deve memorizzare anche l'eventuale PESO da CML del parcel
022900150323      * ?   in decigrammi così elimina i centigrammi arrotondando.
023000150323     c     BLTpuc        mult(h)   100           Peso_in_BLT
023100150323     c                   end
023200150323      * ?   ci sono SPUNTE?
023300060727     c                   if        %Found(FNBLT01L) and bltCAN <> *blank
023400060727      **? controlla se presente fra le Spunte da segnalare a DPD
023500060727     c                   z-add     1             xx
023600060727     c     BLtcan        lookup    S3e(xx)                                31
023700060727     c   31              move      S3e(xx)       Wanomalia
023800060727     c                   endif
023900161128      **
024000061011     c                   endif
024100050418     c                   endif
024200050418      *
024300161129      * Se non trovato allora --> è una spedizione export DPD
024400060328     c                   WHEN      wIMPORT  <> 'S'    and  §evdetr = 'S'
024500161129     c                   eval      Welabora = 'S'
024600130704     c     Kspe          chain     FNARB01L
024700130704     c                   if        %found(FNARB01L)
024800060613     c                   endif
024900060619      *
025000050418     C                   ENDSL
025100080603      *-----------*
025200060619      * rek 'I'
025300060619     c                   move      'I'           ktrc
025400060621     c                   clear                   GEOID             1
025500060621     c                   clear                   GEOIDN            3
025600060619     c     Kar4          chain     fiar401l
025700161129      *
025800060619     c                   if        %found(fiar401l)
025900060619     c                   movel     AR4not        DSBL4i
026000161128     c                   movel     §b4iDA        Delivery_depot
026100161125     c                   movel     §b4isc        ServiceCODE
026200161129     c                   eval      ParcelNr = §b4ipn
026300060621      **   Convertitore
026400060621     c                   movel     §B4IBI        GEOID
026500130705     c                   exsr      CONVERTER
026600080603      * ? Se non trovato il tipo record ("I")
026700080603     c                   else
026800161129      **  Se è un evento x gli ORM Italia su Italia (detti Italy Domestic)
026900080603      * ? il Parcel è fittizio ed è stato staccato durante la generazione
027000080603      * ? degli SCAN x l'ORM. Quindi non esiste sul FIAR4 poichè si tratta
027100080603      * ? di una bolla Italia.
027200161128      **   Convertitore
027300080603     c                   if        Dp3CEV = 'ORM'
027400161129     c                   eval      ParcelNr = Dp3PRN
027500080603     c                   movel     '%'           GEOID
027600130705     c                   exsr      CONVERTER
027700080603      * ? Fine codifica ORM Italy Domestic.
027800080603     c                   endif
027900080603      *
028000080603     c                   endif
028100161129      *---------
028200161129      *  se il parcel non era presente su DP3  -->  AGGIORNA il DP3 con il PARCEL
028300161129     c                   if        dp3prn = *blank and ParcelNr <> *blank
028400161125     c                   eval        dp3prn = ParcelNr
028500160502     c                   endif
028600130705      *----------------------------------------------------------
028700130705      * ? Flagga il TIDP3 di Lettura .
028800060612      * Se ho trovato i file proseguo altrimenti aggiorno file eventi
028900160502     c                   IF        Welabora <> 'S'
029000050418     c                   eval      DP3ftr = 'T'
029100050418     C                   movel     99990101      DP3dtr
029200161129     C                   eval      DP3flr = 'NON ELABORARE - da EVD'
029300060609     c                   update    tidp3000
029400050418     C                   ELSE
029500050418      *
029600050418      *   Se bolla DPD x DPD non trasmetto dati e aggiorno File Eventi
029700161129     c     DP3lnp        lookup    hubDPD                                 31
029800161129     c   31BLPlna        lookup    hubDPD                                 31
029900161129     C                   IF        *in31
030000050418     c                   eval      DP3ftr = 'T'
030100050418     C                   movel     99990102      DP3dtr
030200161129     C                   eval      DP3flr = 'DA DPD A DPD - Errata'
030300060609     c                   update    tidp3000
030400050418     c                   ELSE
030500050418      *
030600050418      *   Se è una bolla recupero non trasmetto  dati
030700050418     c     BLPcbo        lookup    S3A                                    31
030800050418     C                   IF        *in31 = *on
030900050418     c                   eval      DP3ftr = 'T'
031000050418     C                   movel     99990103      DP3dtr
031100161129     C                   eval      DP3flr = 'BOLLA RECUPERO da NON trasm.'
031200060609     c                   update    tidp3000
031300050418     c                   ELSE
031400050418      *
031500161129      *   Se ho trovato i file elaboro -----*
031600050418     C                   exsr      elabora
031700161129      *                          *----------*
031800050418     C                   ENDIF
031900050418     C                   ENDIF
032000050418     C                   ENDIF
032100050418      *  ----------------------
032200050418     C                   ENDIF
032300050418     C                   ENDIF
032400050418      *
032500161129     C                   ENDIF
032600060609     c     KDP3          reade     tIDP3000
032700050418     c                   ENDDO
032800160502      * ____________________________________________________________
032900050418      * Scrivo il nuovo tracciato file
033000160502      *   dal workfile vengono riordinati e scritti sul TIVGD
033100160502      * ------------------------------------------------------------
033200161125     c                   if        File_vers ='E'
033300170407     c                              or File_vers ='S'
033400060609     c                   close     tidp410w
033500060609     c                   open      tidp411w
033600050418      *                *---------------------*
033700050418     c                   exsr      New_File
033800050418      *                *---------------------*
033900060609     C                   close     tIDP411W
034000050418      *
034100060613     C* Infine elimino il blocco elaborazione TIVGD
034200060613     C                   clear                   trul47ds
034300060613     C                   eval      d47opz  = 'F'
034400060613     C                   eval      d47tip  = '$3'
034500060613     C                   call      'TRUL47R'
034600060613     C                   parm                    trul47ds
034700161125     c                   end
034800060613     C*
034900161125     C* chiude il TIVGD se era stato aperto
035000161125     c                   if        VGDTIP_$E = *blank  AND
035100161125     c                             (File_vers ='E' or File_vers ='T')
035200161125     c                   eval      VGDTIP_$E = 'CHIUDE'
035300161125     c                   exsr      Wrk_TTEVENTS
035400161125     c                   end
035500161125     C*
035600050418     c                   EVAL      *INLR = *ON
035700050418      *----------------------------------------------------*
035800050418      *   ELABORO RECORD VALIDO                            *
035900050418      *----------------------------------------------------*
036000050418     C     ELABORA       BEGSR
036100060103      *
036200060103      * ?Riportato qui il controllo presente sul vecchio FIEU25R ora sostituito
036300150709      * ? dal giro del Manifest TRTC83R12.
036400060103      * ?Non devono essere inviati SCAN di tipo 05 - 06 - 10 con date superiori
036500060103      * ? alla data del giorno -> altrimenti creano problemi a DPD.
036600060103      * ? ******* Manda a FINE routine x evitare di inviare lo SCAN che manderà
036700060103      * ? ******* in un giro successivo.
036800161129     c                   If        Scan_CODE = '05' or Scan_CODE = '06' or
036900161129     c                             Scan_CODE = '10' or
037000161129     c                             (SCAN_CODE = '18' and
037100161129     c                               (dp3CEV='S25' or dp3CEV='E25'))
037200160518      * ? ** Adesso aggiungiamo l'ALERT **
037300160518      * xchè deve essere inviato con la stessa data del MIC o della consegna.
037400160518      *   ...quindi viene scritto nel TIDP300 con una data Futura in attesa del MIC.
037500160518      *   Se data FUTURA
037600161129      *    e non è x un ORM
037700060103     c                   If        DP3dev > wdata80
037800161129     c                               and
037900161129     c                             DP3CEV <> 'ORM'
038000060103     c                   goto      salta_scan
038100080603     c                   endif
038200060103     c                   end
038300161129      *****-------------------------------    QUA se NON ha saltato
038400050418      * P.O. scan
038500050418     c                   z-add     1             xx
038600161125     c                   clear                   Sender_Depot
038700050418     c     dp3fle        lookup    pobar(xx)                              31
038800050418     C   31              movel     POdpd(xx)     OG143
038900050418      *
039000050418     C                   SELECT
039100050418     c                   WHEN      *in31 = *off
039200130705      *
039300080611      *  solo x scan Fissi degli Orm Italy domestic (LOCALI Italia su Italia)
039400080611     c                   WHEN      DP3CEV = 'ORM'
039500161125     c                   move      §ogdp1        Sender_Depot
039600050418      *
039700060328     c                   WHEN      wIMPORT  ='S'    and  §evdige = 'H'
039800130705     c                                or
039900130705     c                             wIMPORT  <> 'S'  and  §evdege = 'H'
040000161125     c                   move      §oghb1        Sender_Depot
040100060328     c                   WHEN      wIMPORT  ='S'    and  §evdige = 'D'
040200130705     c                                or
040300130705     c                             wIMPORT  <> 'S'  and  §evdege = 'D'
040400161125     c                   move      §ogdp1        Sender_Depot
040500050418     c                   ENDSL
040600160519      *
040700160519      * Scan '03' in consegna segnalo consegna ISOLA
040800161129     c                   IF         SCAN_CODE ='18' and DP3cev = 'ISL'
040900161129     c                                or
041000161129     c                              SCAN_CODE = '03'  and  BLPfin = 'I'
041100160519     c                   movel     'ISLAND'      qualifica
041200161129     C                   ENDIF
041300050418      *
041400161125     c                   movel     dp3dev        Data_Evento
041500161125     c                   movel     dp3hev        Ora__Evento
041600161130     C                   move      *zeros        HMS__Evento
041700161130     c                   movel     dp3hev        HMS__Evento
041800050418      *
041900060619      *  Country Code ISO numerico x nuovi SCAN
042000161125     c                   clear                   CountryCodISO
042100060619     C                   z-add     1             TBLKUT
042200060619     C                   movel     '15'          TBLCOD
042300060619     C                   movel(p)  BLPnzd        TBLKEY
042400150826     c     KTABel        chain     tabel00f
042500060619     c                   if        %FOund(tabel00f)
042600060619     c                   movel     tbluni        ds15
042700161125     c                   move      §15CIE        CountryCodISO
042800060619     c                   end
042900050418      *
043000050418      * Codice Postale - Se diretta in GB forzo "01111"
043100050418      *                  Se trovo lettere non le scrivo
043200161125     c                   movel     blpCAD        CAP_Dest
043300081015      *
043400081015      * Se Gran Bretagna compatta il CAP togliendo i blank
043500081015     c                   if         BLPnzd = 'GB '
043600161125     c                   eval       CAPinSK = CAP_Dest
043700081015     c                   exsr      Compatta_CAP
043800161125     c                   eval       CAP_Dest = CAPinSK
043900081015     c                   endif
044000050418      *
044100081024      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
044200081024      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
044300161125     C                   if        CAP_Dest = 'EIRE'
044400161125     c                   eval      CAP_Dest = '0000001'
044500081024     c                   end
044600050418      *
044700130704      * se Export DPD  nella Route metto il Depot DPD da linea arrivo DPD
044800130704      *  Altrimenti quello sull'organigramma della nostra LNA
044900130704     c                   IF        wIMPORT  = 'S'
045000050418     c                   z-add     1             xx
045100050418     c     BLPlna        lookup    pobar(xx)                              31
045200050418     C   31              movel     POdpd(xx)     OG143
045300161128     c   31              move      §ogdp1        Delivery_Depot
045400130704     c                   END
045500050418      *
045600050418      * CODICI ANOMALIA
045700050418     c                   clear                   ZCOD
045800140331     c                   clear                   Peso_in_10gr
045900141217      *
046000141217      * il peso è con un decimale e deve essere passato intero con uno zero di arrotondamento
046100141217      * in step da 10gr.--> 3,6kg deve diventare 360 nella variabile richiesta da DPD lunga 8
046200141217     c     blpPKC        mult      100           Peso_in_10gr      8 0
046300150323      ***
046400150323      * ?   se non c'è il VDL (perchè non è stato ancora chiuso il foglio viaggio)
046500150323     c***  NON DEVE PIU' prendere il peso Fatturato - bensì deve prendere dal BLT il BLTPUC
046600150709      *  e comunque non deve mandare più il teorico.
046700150323     c                   if         Peso_in_10gr = 0
046800150323     c                               and
046900150323     c                              Peso_in_BLT > 0
047000150323     c                   move      Peso_in_BLT   Peso_in_10gr      8 0
047100150323     c                   end
047200150323      ***
047300150622      ** se proprio non c'è il peso in assoluto allora deve caricare il peso bollettato
047400150622     c                   if         Peso_in_10gr = 0
047500150623     c     blpPKB        mult      100           Peso_in_10gr
047600150622     c                   end
047700150826      ***
047800150826      *** ====================================
047900150826      **   solo x l'EXPORT quindi con ARB
048000150826      *** ====================================
048100150709      **   non inviamo informazioni di pesi FUORI MISURA (se può lo scoprirà DPD)
048200150709      **   quindi se si supera i 31,5 kg impostiamo fisso 31 kg.
048300150826     c                   if        wIMPORT <> 'S'
048400150826      ***
048500150826     c     *like         define    blppkb        wpes
048600150826     C     *LIKE         DEFINE    §qttpc        WTARA
048700150826      *
048800150826     C* PESO VDL - TARA
048900150826     C     §QTTPC        MULT      blpNCP        WTARA
049000150826     C                   Z-ADD     blpPKC        wpes
049100150826     c                   sub(h)    wtara         wpes
049200150826      *
049300150826      * Poi cerca il peso fra bollettato e CML
049400150826     C*  altrimenti se il bollettato è superiore passo il Bollettato.
049500150826     C     blpNCP        ifNE      blpNCL
049600150826     C     wpes          andLT     blpPKB
049700150826     C                   Z-ADD     blpPKB        wpes
049800150826     C                   ENDIF
049900150826      ***
050000150826     c     wpes          mult      100           Peso_in_10gr
050100150826      ***
050200150826     c                   if         Peso_in_10gr > 3150
050300150709     c                   eval       Peso_in_10gr = 3100
050400150709     c                   end
050500150623      ***
050600150826     c                   end
050700150826      *** ====================================
050800060727      * Anomalia (APERTO, BAGNATO.....SCONDIZIONATO) su IMPORT SCAN 10
050900161129     c                   IF        SCAN_CODE ='10' and wIMPORT ='S' and
051000061011     c                             Wanomalia <> *blank and blpdcm = 0
051100060727     c                   z-add     1             XX
051200060727     c     *blanks       lookup    Zcod(xx)                               31
051300060727      * Danneggiato: Damaged
051400060727     c   31              eval      ZCOD(XX) = '04'
051500060727     C                   ENDIF
051600050418      *
051700050418      * Scan '06' Reso forzo indirizzo errato
051800160830      * Scan '06' di DIROTTAMENTO impostiamo il "02" MISLOADED
051900161129     c                   IF        SCAN_CODE = '06'
052000050418     c                   z-add     1             XX
052100050418     c     *blanks       lookup    Zcod(xx)                               31
052200160830     c   31              if           DP3CEV =    'DIR'
052300160830     c                   move      '02'          ZCOD(XX)
052400160830     c                   else
052500160830     c                   move      '61'          ZCOD(XX)
052600160830     c                   end
052700101126      *
052800151214     c                   goto      al_momento_no
052900101126      * deve riportare l'ultimo motivo di giacenza assieme al reso
053000101126     c                   if        *in31
053100101126     c     Kgcp          chain     tigcp01l
053200101126     c                   if        %found(tigcp01l)
053300101126     c                   z-add     1             xx
053400101126     c     gcpCMC        lookup    K2z(xx)                                31
053500101126     c                   IF        *in31 = *on
053600101126     C                   movel     T2z(xx)       DS2Z
053700101126     c                   z-add     1             XX
053800151214     c     *blanks       lookup    zcod(xx)                               31
053900101126     c   31              eval      ZCOD(XX) = §2Zced
054000101126     c                   ELSE
054100101126     c                   eval      ZCOD(XX) = '11'
054200101126     c                   ENDIF
054300101126
054400101126     c                   endif
054500101126     c                   endif
054600151214     c     al_momento_no tag
054700161129
054800050418     C                   ENDIF
054900050418      * Eventi
055000161129     c                   If        SCAN_CODE <>'06'
055100050418     c                   IF        DP3cev <> *blanks
055200050418     c                   z-add     1             xx
055300050418     c     dp3cev        lookup    K2z(xx)                                31
055400050418     c                   IF        *in31 = *on
055500050418     C                   movel     T2z(xx)       DS2Z
055600050418     c                   z-add     1             XX
055700050418     c     *blanks       lookup    Zcod(xx)                               31
055800050418     c   31              eval      ZCOD(XX) = §2Zced
055900050418     c                   ENDIF
056000050418     c                   ENDIF
056100160830     c                   EndIF
056200050418      *
056300150709      *-------
056400150709      * Scan '03' in consegna segnalo FUORI MISURA
056500150709      *           se peso CML maggiore della tolleranza per fascia di peso
056600150709      * il 1/4/2014 :
056700150709      * DPD dice che sullo 03 non viene calcolato nel CLEARING e di mandarlo sullo 02
056800150709      *   in ogni caso lo inserisco anche sul passaggio alla HUB SCAN10 se già rilevato.
056900161129     c                   IF        SCAN_CODE= '03'  or
057000161129     c                             SCAN_CODE= '02'  or
057100161129     c                             SCAN_CODE= '10'
057200150709      * ma solo sull'IMPORT se supera i 31,5 kg.
057300150709     c                   IF         wIMPORT ='S' and BLPpkc > Wpkc01
057400150709      *  Not Conveyable (09)
057500151215      *    era una errata interpretazione lo 09 occorre inviare il 31 con mail di Jens
057600151215      *    Jens Opderbeck di oggi stesso
057700150709     c                   z-add     1             XX
057800150709     c     *blanks       lookup    Zcod(xx)                               31
057900151215     c   31              eval      ZCOD(XX) = '31'
058000150709     c                   end
058100150709      *--
058200150709     C                   ENDIF
058300140402      *-------
058400050418      * Colli sotto i 3 Kg
058500161125     c                   IF        ServiceCODE ='136'
058600050418     c                   z-add     1             XX
058700050418     c     *blanks       lookup    Zcod(xx)                               31
058800050418     c   31              eval      ZCOD(XX) = '44'
058900050418     C                   ENDIF
059000050418      *
059100050418      *
059200050418      * Se l'evento è in data di oggi e l'ora evento è maggiore dell'ora
059300050418      *  di elaborazione forzo questa come ora evento
059400161125     c                   IF        Data_Evento = WDATAoggi and
059500161125     c                             Ora__Evento > %subst(WORA:1:4)
059600161125     C                   movel     Wora          Ora__Evento
059700161130     C                   move      *zeros        HMS__Evento
059800161130     C                   movel     Wora          HMS__Evento
059900050418     c                   ENDIF
060000050418
060100050418      * Scrivo testata file se non ancora scritta
060200050418     c                   IF        Wtestata = *blanks
060300050418     C                   eval      Wtestata = 'S'
060400050418     c                   ENDIF
060500050418      * -----------
060600161125      *   Vecchio FILE SCANEW
060700161125     c                   if        File_vers ='E'
060800170407     c                               or File_vers ='S'
060900161125     c                   exsr      Wrk_ScaNEW
061000161125     c                   end
061100161125      *   Nuovo   FILE TTEVENTS
061200161125     c                   if        File_vers ='T' or
061300161125     c                             File_vers ='E'
061400161125     c                   exsr      Wrk_TTEVENTS
061500161125     c                   end
061600160516      *
061700160517      *  Se NON deve trasmettere lo SCAN (in questo caso lo SCAN 18 con mail/SMS)
061800160517      *   lo flagga
061900160516     c                   if        non_flaggare_DP3 <> *blank
062000160517     c                   eval      DP3ftr = 'T'
062100160517     C                   movel     99990104      DP3dtr
062200160517     C                   eval      DP3flr = 'NON TRASMESSO-MANCA MAIL/SMS'
062300160517     C                   ELSE
062400151211      *
062500151211     c                   exsr      Wrk_FILLER
062600050418      *
062700050418      * Aggiorno File eventi DPD
062800161125     C                   IF        Data_Evento > wdata_90
062900050418     c                   eval      DP3ftr = 'T'
063000050418     C                   else
063100050418     c                   eval      DP3ftr = 'S'
063200050418     C                   end
063300050418      *---
063400050418     C                   movel     wdataoggi     DP3dtr
063500160517     c                   end
063600160517      *---
063700160516     C     salta_scan    tag
063800160516      *
063900060609     c                   update    tidp3000
064000060103      *
064100050418     C                   ENDSR
064200050418      *----------------------------------------------------*
064300151211      *imposta il campo Filler con i dati dello SCAN inviato
064400050418      *----------------------------------------------------*
064500151211     C     Wrk_filler    Begsr
064600050418      *
064700151211     c                   clear                   dsDP3FLR
064800161125     c                   eval      DP3§PARCEL = ParcelNr
064900161125     c                   eval      DP3§SCODE  = ServiceCODE
065000161125     c                   eval      DP3§DATE   = Data_Evento
065100161125     c                   eval      DP3§TIME   = Ora__Evento
065200161128     c                   eval      DP3§SCAN   = Scan_CODE
065300151211     c                   eval      DP3§ADDC1  = zcod(1)
065400151211     c                   eval      DP3§ADDC2  = zcod(2)
065500151211     c                   eval      DP3§ADDC3  = zcod(3)
065600151211     c                   eval      DP3§PESO   = %editc(Peso_in_10gr:'X')
065700161125     c                   eval      DP3§DEPOT  = Sender_Depot
065800151214     c                   eval      DP3§TOUR   = '001'
065900161125     c                   eval      DP3§NAZISO = CountryCodISO
066000161125     c                   eval      DP3§CAP    = CAP_Dest
066100161128     c                   eval      DP3§ROUTE  = Delivery_Depot
066200151211     c                   eval      DP3§GEOIDN = GEOIDN
066300151211     c                   eval      DP3§DIST   = DIST
066400151211     c                   eval      DP3§STOP   = SSTOP
066500151211     c                   eval      DP3§QUALIF = qualifica
066600151211     c                   eval      DP3§TIPSCA = 'B'
066700151211     c                   if          DP3CEV ='ORM'
066800151211     c                   eval      DP3§TIPSCA = 'O'
066900151211     C                   end
067000151211     c                   eval      DP3flr = dsDP3FLR
067100151211      *
067200151211     C                   ENDSR
067300050418      *----------------------------------------------------*
067400050418      *   Imposta il nuovo file da trasmettere             *
067500050418      *----------------------------------------------------*
067600050418     C     New_File      Begsr
067700050418      *
067800060609     c     *loval        setll     tidp411w
067900060609     c                   read      tidp411w
068000050418      *
068100060609     c                   dow       not %eof(tidp411w)
068200060620     c                   eval      s41len = w41len
068300050418     c                   eval      s41dep = w41dep
068400050418     c                   eval      s41typ = w41typ
068500050418      *
068600050418     c                   exsr      new_testa
068700050418      *                           -----------
068800060609     c                   dow       not %eof(tidp411w) and
068900060620     c                             w41len = s41len    and
069000050418     c                             w41dep = s41dep    and
069100050418     c                             w41typ = s41typ
069200050418      *
069300050418     c                   exsr      new_detscan
069400050418      *                           -----------
069500050418      * da riportare sul record di coda (data e time)
069600050418     c                   eval      s41dat = w41dat
069700050418     c                   eval      s41tim = w41tim
069800050418     c                   eval      s41tou = w41tou
069900050418     c                   eval      s41acc = w41acc
070000050418      *
070100060609     c                   read      tidp411w
070200050418     C                   Enddo
070300050418      *
070400050418     c                   exsr      new_coda
070500050418      *                           -----------
070600050418     C                   Enddo
070700050418      *
070800050418     C                   Endsr
070900050418      *----------------------------------------------------*
071000050418      *   Nuova Testata gruppo SCAN x DEP/TYP              *
071100050418      *----------------------------------------------------*
071200050418     C     New_Testa     Begsr
071300050418      *
071400060612     c                   clear                   d41dat
071500060612     c                   eval      d41dat ='08' + %trim(w41typ)+ %trim(w41dat)+
071600060612     c                             %trim(w41tim) + '|D' + %trim(w41dep)
071700060612     c                              + '|T' + %trim(w41tou) + '|'
071800060613     C                   exsr      tivgd
071900050418      *
072000050418     C                   Endsr
072100050418      *----------------------------------------------------*
072200050418      *   Nuovo Dettaglio SCAN                             *
072300050418      *----------------------------------------------------*
072400050418     C     New_detscan   Begsr
072500050418      *
072600060613     c                   clear                   d41dat
072700060612     c                   eval      d41dat ='01' + %trim(w41typ)+ %trim(w41dat)+
072800060621     c                             %trim(w41tim) + '|' + %trim(w41fre)
072900060613     C                   exsr      tivgd
073000050418      *
073100050418     C                   Endsr
073200050418      *----------------------------------------------------*
073300050418      *   Nuova Coda gruppo SCAN x DEP/TYP                 *
073400050418      *----------------------------------------------------*
073500050418     C     New_Coda      Begsr
073600050418      *
073700050418     c                   clear                   d41dat
073800060612      *verifica se il parcel è nuovo o vecchio modo tramite la lunghezza
073900060620     c                   eval      d41dat ='09' + %trim(s41typ)+ %trim(s41dat)+
074000060620     c                             %trim(s41tim) + '|D' + %trim(s41dep)
074100060620     c                              + '|T' + %trim(s41tou) + '|'
074200060613     C                   exsr      tivgd
074300050418      *
074400050418     C                   Endsr
074500060613      *----------------------------------------------------*
074600060613      *   Scrittura tivgd file da trsmettere               *
074700060613      *----------------------------------------------------*
074800060613     C     tivgd         Begsr
074900060613      *
075000060613     c                   clear                   tivgd000
075100080403     c                   eval      vgddta = %TrimR(d41dat)
075200060613     c                   eval      vgdtip = '$3'
075300060613     c                   eval      vgdksu = '0DPD0OUT'
075400060613     c                   eval      vgdtsc = 'WW'
075500131114     c                   eval      vgdpgm = 'FIEU16R'
075600060613     c                   eval      vgddat = W0080
075700060613     C                   WRITE     tivgd000
075800060613      *
075900060613     C                   Endsr
076000050418      *----------------------------------------------------*
076100050418      *   DEFINIZIONE CHIAVI                               *
076200050418      *----------------------------------------------------*
076300050418     C     *INZSR        BEGSR
076400050418      *
076500050418     c     *ENTRY        PLIST
076600050418     c                   PARM                    KPJBA
076700050418      *
076800050418     C     KTBE          KLIST
076900050418     C                   KFLD                    KCODE
077000050418     C                   KFLD                    KKE1
077100050418      *
077200050418     C     KTAB          KLIST
077300050418     C                   KFLD                    CODUT
077400050418     C                   KFLD                    KCOD
077500060619      *
077600150826     C     KTABel        KLIST
077700060619     C                   KFLD                    TBLKUT
077800060619     C                   KFLD                    TBLCOD
077900060619     C                   KFLD                    TBLKEY
078000050418      *
078100050418     C     KDP3          KLIST
078200050418     C                   KFLD                    KDTR
078300050418     C                   KFLD                    KFTR
078400050418      *
078500050418     C     KSPE          KLIST
078600050418     C                   KFLD                    dp3AAS
078700050418     C                   KFLD                    dp3LNP
078800050418     C                   KFLD                    dp3NRS
078900050418     C                   KFLD                    dp3NSP
079000050418      *
079100050418     C     KAR4          KLIST
079200050418     C                   KFLD                    dp3AAS
079300050418     C                   KFLD                    dp3LNP
079400050418     C                   KFLD                    dp3NRS
079500050418     C                   KFLD                    dp3NSP
079600050418     C                   KFLD                    KTRC
079700160502      *
079800160502     C     KAR5          KLIST
079900160502     C                   KFLD                    dp3AAS
080000160502     C                   KFLD                    dp3LNP
080100160502     C                   KFLD                    dp3NRS
080200160502     C                   KFLD                    dp3NSP
080300160502     C                   KFLD                    TipoAr5           3
080400050418      *
080500101126     C     Kgcp          KLIST
080600101126     C                   KFLD                    dp3AAS
080700101126     C                   KFLD                    dp3LNP
080800101126     C                   KFLD                    dp3NRS
080900101126     C                   KFLD                    dp3NSP
081000101126     C                   KFLD                    TRC0              2 0
081100101126     C                   z-add     0             TRC0
081200050418      * Dati società
081300050418     C                   Z-ADD     1             CODUT
081400050418     C                   CALL      'X§PARUT'
081500050418     C                   PARM                    UT§DSE0F
081600050418     C                   MOVEL     REC80         CNCR80
081700050418      *
081800050418      *   EDPCEDxxx dove xxx è il Terminal
081900050418     C                   CLEAR                   Tibs36Ds
082000050418     c                   EVAL      I36ute = Knmus
082100050418     c                   EVAL      I36Tla = 'L'
082200050418     C                   CALL      'TIBS36R'
082300050418     C                   PARM                    Tibs36Ds
082400050418      *
082500050418      * LEggo AZORG per caricamento schiere P.O. DPD
082600050418     C                   clear                   XX
082700050418     C                   clear                   YY
082800050418     c     *loval        setll     azorg
082900050418     c                   read      azorg
083000050418      *
083100130910     C                   DOW       NOT %eof(Azorg01l)
083200130910      *
083300161129      * Decodifica P.O. BARTOLINI con codice DPD
083400050418     c                   If        ORGDE3 <> *blanks
083500050418     C                   add       1             xx
083600050418     C                   movel     ORGfil        POBAR(xx)
083700050418     C                   movel     ORGde3        PODPD(xx)
083800050418     C                   movel     ORGDE3        OG143
083900161129      * e se è un HUB DPD (190,191,192.....)
084000050418     c                   If        §ogNTW = 'DPD'
084100050418     C                   add       1             YY
084200161129     C                   movel     ORGfil        hubDPD(yy)
084300050418     c                   Endif
084400161129      *
084500050418     c                   Endif
084600050418      *
084700050418     c                   read      azorg
084800050418     c                   ENDDO
084900150826     C*
085000150826      *------------
085100150826     C** CARICAMENTO DATI VARI TASSAZIONE
085200150826     C                   Z-ADD     1             tblKUT
085300150826     C                   MOVEL     'QT'          tblcod
085400150826     C                   MOVEL(P)  '1'           tblKEY
085500150826     C                   CLEAR                   DSQT1
085600150826     C     KTABel        CHAIN     TABEL00F
085700150826     c                   if        %Found(TABEL00F)
085800150826     C                   MOVEL     TBLUNI        DSQT1
085900150826     c                   end
086000150826      *------------
086100050418      *
086200050418      * Carico Continuazione Tabella Causali Eventi "2Z"
086300050418     C                   clear                   XX
086400050418     C                   MOVE      '2Z'          KCOD
086500050418      *
086600050418     C     ktab          SETLL     TABEL00F
086700050418     C     ktab          READE     TABEL00F
086800130910     C                   DOW       NOT %eof(tabel00f)
086900050418     c                   movel     tbluni        ds2z
087000050418     c                   IF        §2zevd <> *blanks  and  §2zced <> *blanks
087100050418     C                   add       1             XX
087200050418     C                   movel     TBLKEY        K2Z(xx)
087300050418     C                   movel     TBLUNI        T2Z(xx)
087400050418     C                   ENDIF
087500050418     C     ktab          reade     TABEL00F
087600050418     C                   ENDDO
087700050418      *
087800050418      * carico schiera dei codici bolla di recupero
087900050418     C                   clear                   XX
088000050418     C                   move      '3A'          KCOD
088100050418     C     ktab          SETLL     TABEL00F
088200050418     C     ktab          READE     TABEL00F
088300130910     C                   DOW       NOT %eof(tabel00f)
088400050418     c                   movel     tbluni        ds3a
088500050418     c                   IF        §3arbl =  'R'
088600050418     C                   add       1             XX
088700050418     C                   movel     TBLKEY        s3a(xx)
088800050418     c                   ENDIF
088900050418     C     ktab          reade     TABEL00F
089000050418     C                   ENDDO
089100050418      *
089200060727      * carico schiera delle spunte Anoamlie (3E)
089300060727     C                   clear                   XX
089400060727     C                   move      '3E'          KCOD
089500060727     C     ktab          SETLL     TABEL00F
089600060727     C     ktab          READE     TABEL00F
089700130910     C                   DOW       NOT %eof(tabel00f)
089800060727     c                   movel     tbluni        ds3e
089900060727      * non deve caricare i rientri
090000170516      * e le segnalazioni
090100170516     c                   IF        §3eFTA <> 'R' and §3eFTA <> 'S'
090200060727     C                   add       1             XX
090300060727     C                   movel     TBLKEY        s3e(xx)
090400060727     c                   ENDIF
090500060727      *
090600060727     C     ktab          reade     TABEL00F
090700060727     C                   ENDDO
090800060727      *
090900050418      * Caricamento Tabella PESI DPD
091000050418     c                   eval      kcode = 'DPP'
091100050418     c                   eval      kke1 =  'PESO01'
091200050418     c     Ktbe          chain     tntbe000
091300050418      *
091400130910     c                   IF        %found(tntbe01l) and TBEatb = *BLANKS
091500050418     c                   movel     TBEuni        DDPP
091600050418     c                   movel     §DPPpkc       Wpkc01
091700050418     c                   ENDIF
091800050418      *
091900050418     c                   eval      kke1 =  'PESO02'
092000050418     c     Ktbe          chain     tntbe000
092100050418      *
092200130910     c                   IF        %found(tntbe01l) and TBEatb = *BLANKS
092300050418     c                   movel     TBEuni        DDPP
092400050418     c                   movel     §DPPpkc       Wpkc02
092500050418     c                   ENDIF
092600050418      *
092700050418      * Carico Tabella Eventi DPD
092800050418     C                   clear                   XX
092900050418     c     'EVD'         chain     tntbe000                           31
093000050418      *
093100050418     c                   DOW       *in31 = *off
093200050418      * Se il S.I. è indicato deve essere uguale al mio
093300050418     c                   IF        TBEatb = *BLANKS  and
093400050418     c                              (TBEsif = KNSIF or TBEsif = *blanks)
093500050418     c                   add       1             xx
093600050418     c                   movel     TBEke1        KEVD(XX)
093700050418     c                   movel     TBEuni        EVD(XX)
093800050418     c                   ENDIF
093900050418      *
094000050418     c     'EVD'         reade     tntbe000                               31
094100050418     c                   ENDDO
094200050418      *
094300050418      * DATA DEL GIORNO + ORA
094400050418     C                   TIME                    W0140
094500050418     C                   MOVE      W0140         W0080
094600050418     C     *eur          MOVEL     W0080         Wdata
094700050418     C     *iso          MOVEL     Wdata         W0080
094800050418     c                   MOVEL     W0080         Wdataoggi
094900050418     c                   MOVEL     W0080         Wdata80
095000050418     C                   MOVEL     W0140         WORA
095100170407      *
095200170407      *-----------------------------
095300170407      *  prima gli SCAN
095400170407     c                   eval      File_vers ='S'
095500170407      *
095600170407      *   dal 10/4/2017 anche i TTEVENTS con gli SCAN
095700170410     c                   if        wdata80 >= 20170410
095800170407     c                   eval      File_vers ='E'
095900170407     c                   end
096000170407      *
096100170412      *   dal 12/4/2017 solo i TTEVENTS
096200170412     c                   if        wdata80 >= 20170412
096300170407     c                   eval      File_vers ='T'
096400170407     c                   end
096500170407      *-----------------------------
096600170407      *
096700050418      * calcola il limite di 90 gg. oltre i quali la DpD non ha bisogno dei records
096800050418     C     Wdata         SUBDUR    90:*Days      W0m90
096900050418     c     *iso          MOVEL     W0m90         W008M
097000050418     c                   MOVEL     W008M         Wdata_90
097100161125      **
097200161125     c                   if        File_vers ='E'
097300170407     c                               or File_vers ='S'
097400060613      *  istruzioni apertura blocco scrittura TIVGD
097500060613     C                   clear                   trul47ds
097600060613     C                   eval      d47opz  = 'I'
097700060613     C                   eval      d47tip  = '$3'
097800060613     C                   eval      d47lck  = 'N'
097900060613     C                   eval      d47chkj = 'N'
098000131114     C                   eval      d47pgm  = 'FIEU16R'
098100060613     C                   call      'TRUL47R'
098200060613     C                   parm                    trul47ds
098300161125     c                   end
098400161125      **
098500050418     C     endinz        ENDSR
098600081015      **?------------------------------------------------------------------ */
098700081015      *   compatta il CAP eliminando dei blank all'interno es.GB
098800081015      **?------------------------------------------------------------------ */
098900081015     c     Compatta_CAP  begsr
099000081015      *
099100081015     c                   z-add     0             to                3 0
099200081015     c                   do        9             from              3 0
099300081015     c                   if        skCAP(from) = *blank
099400081015     c                   if          to = 0
099500081015     c                   z-add     from          to
099600081015     c                   end
099700081015     c                   else
099800081015     c                   if          to > 0
099900081015     c                   move      skCAP(from)   skCAP(to)
100000081015     c                   move      *blank        skCAP(from)
100100081015     c                   add       1             to
100200081015     c                   end
100300081015     c                   end
100400081015     c                   enddo
100500081015
100600081015     c                   endsr
100700130705      *-------------------------------------------------------------------------
100800130705     c     CONVERTER     begsr
100900130705
101000130705     C                   move (p)  GEOID         §xteAchr
101100130705     c                   call      'XTOEBCDR'
101200130705     c                   parm                    xtoEBCDds
101300130705     C                   IF        §XTEERR = *blanks
101400130705     C                   move      §xteAdec      GEOIDN
101500130705     C                   ENDIF
101600161125      *
101700161125     c                   endsr
101800161125      *----------------------------------------------------*
101900161125      *   SCANEW su TIVGD                                  *
102000161125      *----------------------------------------------------*
102100161125     C     Wrk_ScaNEW    Begsr
102200161125      *
102300161125      * Imposta sul wrkfile il tracciato dello SCAN
102400161125      *  con i campi fissi e in aggiunta nel campo note quelli variabili
102500161125      *verifica se il parcel è nuovo o vecchio modo tramite la lunghezza
102600161128     c                   movel     Scan_CODE     w41typ
102700161125     c                   movel     '001'         w41tou
102800161125     c                   movel     Data_Evento   w41dat
102900161125     c                   movel     Sender_Depot  w41dep
103000161125     c                   movel     *zeros        w41tim
103100161125     c                   movel     Ora__Evento   w41tim
103200161125     c                   movel     *zeros        w41acc
103300161125     c                   movel     ParcelNr      w41par
103400161125     c                   z-add     14            w41len
103500161125      *
103600161125     c                   exsr      crea_SCANew
103700161125      *
103800161125      * Dati aggiuntivi x tipo di scan solo se deocidificato
103900161125     c                   if        w41fre <> *blank
104000161125     C                   write     tidpw41
104100161125     c                   add       1             WNewRec
104200161125     C                   End
104300161125      *
104400161125     c                   clear                   tidpw41
104500161125      *
104600161125     C                   Endsr
104700161125      *----------------------------------------------------*
104800161125      *   Dati a tracciato libero NUOVO MODO               *
104900161125      *----------------------------------------------------*
105000161125     C     crea_SCANew   Begsr
105100161125      *
105200161125     c                   eval      non_flaggare_DP3 = ' '
105300161125      *
105400161125     c                   Select
105500161125      * SCAN 01/02/05/10
105600161125     c                   WHEN      w41typ = '01' or w41typ = '02'or
105700161125     c                             w41typ = '05' or w41typ = '10'
105800161125     c                   eval      w41fre = %trim(w41fre) +
105900161125     c                             'D' + %trim(Sender_Depot) +
106000161125     c                             '|T' + %trim('001') +
106100161125     c                             '|P' + %trim(ParcelNr) +
106200161125     c                             '|R' + %trim(CountryCodISO) + ','
106300161128     c                             + %trim(CAP_Dest)+ ','
106400161128     c                             + %trim(Delivery_Depot) +
106500161125     c                             '|S' + %trim(ServiceCODE)+
106600161125     c                             '|YGI' + %trim(GEOIDN) + '|'
106700161125     c                   if        zcod(1) <> *blank
106800161125     c                   eval      w41fre = %trim(w41fre)
106900161125     c                             + %trim('Z') + %trim(zcod(1)) + '|'
107000161125     c                   end
107100161125     c                   if        zcod(2) <> *blank
107200161125     c                   eval      w41fre = %trim(w41fre)
107300161125     c                             + %trim('Z') + %trim(zcod(2)) + '|'
107400161125     c                   end
107500161125     c                   if        zcod(3) <> *blank
107600161125     c                   eval      w41fre = %trim(w41fre)
107700161125     c                             + %trim('Z') + %trim(zcod(3)) + '|'
107800161125     c                   end
107900161125      *  il 1/4/2014 DPD dice che perchè entri nel clearing il peso deve essere sullo
108000161125      *   SCAN 02  allora mando add.code e peso anche su 02 e 10 se c'è.
108100161125      * Il pagamento dell'importo (Nuove regole Clearing) che non pagano con l'add.code
108200161125      *  il Peso deve essere espresso in decagrammi (quindi in step di 10gr di arrotondamento)
108300161125      * è il peso del CML con in più uno zero sulla destra.
108400161125      * dal 17/12/2014 il peso viene sempre inviato a DPD anche se non è Fuori Misura.
108500161125     c                   IF        Peso_in_10gr > 0
108600161125     c                   eval      w41fre = %trim(w41fre)
108700161125     c                             + %trim('YWG')
108800161125     c                             + %trim(%editc(Peso_in_10gr:'Z')) + '|'
108900161125     c                   end
109000161125      * SCAN 03
109100161125     c                   WHEN      w41typ = '03'
109200161125     c                   eval      w41fre = %trim(w41fre) +
109300161125     c                             'D' + %trim(Sender_Depot) +
109400161125     c                             '|T' + %trim('001') +
109500161125     c                             '|P' + %trim(ParcelNr) +
109600161125     c                             '|YDS' + %trim(DIST) +
109700161125     c                             '|YST' + %trim(SSTOP) +
109800161125     c                             '|YGI' + %trim(GEOIDN) + '|'
109900161125     c                   if        zcod(1) <> *blank
110000161125     c                   eval      w41fre = %trim(w41fre)
110100161125     c                             + %trim('Z') + %trim(zcod(1)) + '|'
110200161125     c                   end
110300161125     c                   if        zcod(2) <> *blank
110400161125     c                   eval      w41fre = %trim(w41fre)
110500161125     c                             + %trim('Z') + %trim(zcod(2)) + '|'
110600161125     c                   end
110700161125     c                   if        zcod(3) <> *blank
110800161125     c                   eval      w41fre = %trim(w41fre)
110900161125     c                             + %trim('Z') + %trim(zcod(3)) + '|'
111000161125     c                   end
111100161125      * segnalo con il peso il Fuori Misura  per poter avere poi sul Clearing
111200161125      * il pagamento dell'importo (Nuove regole Clearing) che non pagano con l'add.code
111300161125      *  il Peso deve essere espresso in decagrammi (quindi in step di 10gr di arrotondamento)
111400161125      * è il peso del CML con in più uno zero sulla destra.
111500161125      * dal 17/12/2014 il peso viene sempre inviato a DPD anche se non è Fuori Misura.
111600161125     c                   IF        Peso_in_10gr > 0
111700161125     c                   eval      w41fre = %trim(w41fre)
111800161125     c                             + %trim('YWG')
111900161125     c                             + %trim(%editc(Peso_in_10gr:'Z')) + '|'
112000161125     c                   end
112100161125      * SCAN 04/08
112200161125     c                   WHEN      w41typ = '04' or w41typ = '08'
112300161125     c                   eval      w41fre = %trim(w41fre) +
112400161125     c                             'D' + %trim(Sender_Depot) +
112500161125     c                             '|T' + %trim('001') +
112600161125     c                             '|P' + %trim(ParcelNr) +
112700161125     c                             '|YGI' + %trim(GEOIDN) + '|'
112800161125     c                   if        zcod(1) <> *blank
112900161125     c                   eval      w41fre = %trim(w41fre)
113000161125     c                             + %trim('Z') + %trim(zcod(1)) + '|'
113100161125     c                   end
113200161125     c                   if        zcod(2) <> *blank
113300161125     c                   eval      w41fre = %trim(w41fre)
113400161125     c                             + %trim('Z') + %trim(zcod(2)) + '|'
113500161125     c                   end
113600161125     c                   if        zcod(3) <> *blank
113700161125     c                   eval      w41fre = %trim(w41fre)
113800161125     c                             + %trim('Z') + %trim(zcod(3)) + '|'
113900161125     c                   end
114000161125      * SCAN 06
114100161125     c                   WHEN      w41typ = '06'
114200161125     c                   eval      w41fre = %trim(w41fre) +
114300161125     c                             'D' + %trim(Sender_Depot) +
114400161125     c                             '|T' + %trim('001') +
114500161125     c                             '|P' + %trim(ParcelNr) +
114600161125     c                             '|R' + %trim(CountryCodISO) + ','
114700161128     c                             + %trim(CAP_Dest)+ ','
114800161128     c                             + %trim(Delivery_Depot) +
114900161125     c                             '|S' + %trim(ServiceCODE)+
115000161125     c                             '|YGI' + %trim(GEOIDN) + '|'
115100161125     c                   if        zcod(1) <> *blank
115200161125     c                   eval      w41fre = %trim(w41fre)
115300161125     c                             + %trim('Z') + %trim(zcod(1)) + '|'
115400161125     c                   end
115500161125     c                   if        zcod(2) <> *blank
115600161125     c                   eval      w41fre = %trim(w41fre)
115700161125     c                             + %trim('Z') + %trim(zcod(2)) + '|'
115800161125     c                   end
115900161125     c                   if        zcod(3) <> *blank
116000161125     c                   eval      w41fre = %trim(w41fre)
116100161125     c                             + %trim('Z') + %trim(zcod(3)) + '|'
116200161125     c                   end
116300161125      * SCAN 07/09/13/14
116400161125     c                   WHEN      w41typ = '07' or w41typ = '09' or
116500161125     c                             w41typ = '13' or w41typ = '14'
116600161125     c                   eval      w41fre = %trim(w41fre) +
116700161125     c                             'D' + %trim(Sender_Depot) +
116800161125     c                             '|T' + %trim('001') +
116900161125     c                             '|P' + %trim(ParcelNr) +
117000161125     c                             '|YGI' + %trim(GEOIDN) + '|'
117100161125     c                   if        zcod(1) <> *blank
117200161125     c                   eval      w41fre = %trim(w41fre)
117300161125     c                             + %trim('Z') + %trim(zcod(1)) + '|'
117400161125     c                   end
117500161125     c                   if        zcod(2) <> *blank
117600161125     c                   eval      w41fre = %trim(w41fre)
117700161125     c                             + %trim('Z') + %trim(zcod(2)) + '|'
117800161125     c                   end
117900161125     c                   if        zcod(3) <> *blank
118000161125     c                   eval      w41fre = %trim(w41fre)
118100161125     c                             + %trim('Z') + %trim(zcod(3)) + '|'
118200161125     c                   end
118300161125      *
118400161125      * SCAN 18
118500161125     c                   WHEN      w41typ = '18'
118600161125      *
118700161125     c                   IF        wIMPORT ='S'
118800161125      *
118900161125      * inviato ALERT con mail o SMS
119000161125     c                   IF        (DP3CEV ='S25' or DP3CEV ='E25') or
119100161125     c                             (DP3CEV ='S44' or DP3CEV ='E44' or
119200161125     c                              DP3CEV ='X44')
119300161125      *
119400161125     c                   eval      w41fre = %trim(w41fre) +
119500161125     c                             'D' + %trim(Sender_Depot) +
119600161125     c                             '|T' + %trim('001') +
119700161125     c                             '|P' + %trim(ParcelNr) +
119800161125     c                             '|YAL01|YAD44:1:'
119900161125     c                   exsr      info_alert44
120000161125     c                   if            predict_invio = *blank
120100161125     c                   clear                   w41fre
120200161125     c                   eval      non_flaggare_DP3 = 'N'
120300161125     c                   end
120400161125      *
120500161125     c                   ENDIF
120600161125      *
120700161125      *
120800161125      * inviato ISOLA
120900161125     c                   IF         DP3CEV ='ISL'
121000161125     c                   eval      w41fre = %trim(w41fre) +
121100161125     c                             'D' + %trim(Sender_Depot) +
121200161125     c                             '|T' + %trim('001') +
121300161125     c                             '|P' + %trim(ParcelNr) +
121400161125     c                             '|YAL00|YADISLAND|YGI037|'
121500161125     c                   ENDIF
121600161125      *
121700161125     c                   ENDIF
121800161125      *
121900161125     c                   Endsl
122000161125      *
122100161125     C                   Endsr
122200161125      *----------------------------------------------------*
122300161125      *   Se inviato il PREDICT (Alert) SCAN 18(44)        *
122400161125      *----------------------------------------------------*
122500161125     C     Info_Alert44  Begsr
122600161125      *
122700161125      * aggancia FIAR500R per le decodifiche da passare
122800161125      *   per poi generare  il filler con lo scan 18
122900161125     c                   movel     'EMD'         TipoAr5
123000161125     c     Kar5          chain     fiar501l
123100161125     c                   IF        %found(fiar501l)
123200161125     c                   eval        dar5EMD = AR5UNI
123300161125      *
123400161125     c                   eval        w41fre = %trim(w41fre) +
123500161125     c                                        %trim(§AR5EML) + ':' +
123600161125     c                                        %trim(§AR5TEL) + ':'
123700161125      *  EMAIL/SMS
123800161125     c                   eval          predict_invio ='S'
123900161125     c                   move      '3'           tipopredict       1
124000161125     c                   if          §AR5EML<> *blank and §AR5TEL = *blank
124100161125     c                   move      '2'           tipopredict
124200161125     c                   elseif      §AR5EML = *blank and §AR5TEL<> *blank
124300161125     c                   move      '1'           tipopredict
124400161125     c                   elseif      §AR5EML = *blank and §AR5TEL = *blank
124500161125     c                   clear                   predict_invio     1
124600161125     c                   leavesr
124700161125     c                   end
124800161125      *
124900161125      *   durante il MIC  (DLO)
125000161125     c                   eval      w41fre = %trim(w41fre) + tipopredict +
125100161125     c                              ':2:DLO:'
125200161125      *   DATA
125300161125     c                   if          §AR5DTO<> *blank and §AR5DOS = *blank
125400161125     c                                  or
125500161125     c                               §AR5DTO > §AR5DOS
125600161125     c                   eval        w41fre = %trim(w41fre) +
125700161125     c                                 %subst(§AR5DTO:1:8) + ':' +
125800161125     c                                 %subst(§AR5DTO:9:4) + ':'
125900161125      *
126000161125     c                   elseif      §AR5DOS<> *blank and §AR5DTO = *blank
126100161125     c                                  or
126200161125     c                               §AR5DOS > §AR5DTO
126300161125     c                   eval        w41fre = %trim(w41fre) +
126400161125     c                                 %subst(§AR5DOS:1:8) + ':' +
126500161125     c                                 %subst(§AR5DOS:9:4) + ':'
126600161125     c                   end
126700161125      *    TIME
126800161125     c                   if        §AR5OPCD <> *blank
126900161125     c                                  and §AR5OPCA <> *blank
127000161125      *
127100161125     c                   eval        w41fre = %trim(w41fre) + '1:' +
127200161125     c                                      §AR5OPCD +':'+ §AR5OPCA +'::'
127300161125      *
127400161125     c                   elseif    §AR5OPCD = *blank
127500161125     c                                  and §AR5OPCA = *blank
127600161125      *
127700161125     c                   eval        w41fre = %trim(w41fre) + '0:1900:::'
127800161125      *
127900161125     C                   End
128000161125     c                   eval        w41fre = %trim(w41fre) + '|YGI037|'
128100161125      *------
128200161125     C                   END
128300161125      *
128400161125     C                   Endsr
128500161125      *----------------------------------------------------*
128600161125      *   TTEVENTS  TIVGD                                  *
128700161125      *----------------------------------------------------*
128800161125     C     Wrk_TTEVENTS  Begsr
128900161125      *
129000161125      **  TIVGD APRE e CHIUDE
129100161125     c                   if        VGDTIP_$E ='APRE  '
129200161125      **                                        ===========
129300161125     c                   clear                   VGDTIP_$E
129400161125      **                                        ===========
129500161125      *  istruzioni apertura blocco scrittura TIVGD
129600161125     C                   clear                   trul47ds
129700161125     C                   eval      d47opz  = 'I'
129800161125     C                   eval      d47tip  = '$E'
129900161125     C                   eval      d47lck  = 'N'
130000161125     C                   eval      d47chkj = 'N'
130100161125     C                   eval      d47pgm  = 'FIEU16R'
130200161125     C                   call      'TRUL47R'
130300161125     C                   parm                    trul47ds
130400161125      **
130500161125     c                   elseIF    VGDTIP_$E ='CHIUDE'
130600161125      **
130700161125     c                   exsr      FINE_TTEVENT                                 #FINE MSG
130800161125     C* Infine elimino il blocco elaborazione TIVGD
130900161125     C                   clear                   trul47ds
131000161125     C                   eval      d47opz  = 'F'
131100161125     C                   eval      d47tip  = '$E'
131200161125     C                   call      'TRUL47R'
131300161125     C                   parm                    trul47ds
131400161125      **
131500161125     c                   end
131600161125      **
131700161125      **  scrive il  TTEVENTS
131800161125      * dettaglio TIVGD                     ===========
131900161125     c                   IF        VGDTIP_$E <>'CHIUDE'
132000161125      ** Solo la 1°Volta                    ===========
132100161125     c                   if         TESTA_TTE ='SI'
132200161125     c                   clear                   TESTA_TTE                      Solo la 1°volta
132300161125     c                   exsr      TESTA_TTEVENT                                Testata MSG
132400161125     c                   end
132500161125      *
132600161125      *  riga di dettaglio status SCAN
132700161125      *                           ---------------
132800161125     c                   exsr      TTEVENTS_dati                                riga di TTEVENTS
132900161125      *                           ---------------
133000161125     c                   endIF
133100161125      *
133200161125     c                   endsr
133300130705      *-------------------------------------------------------------------------
133400161125     c     TESTA_TTEVENT BEGsr
133500161125      *-------------------------------------------------------------------------
133600161125      *  6 righe di testata
133700161125      *
133800170410      **   #FILE;GEODATA_STATUS_0230820_N00_D20161016T082554;
133900161125     c                   clear                   dati_TTE
134000161129     c                   EVAL      dati_TTE = %trim(HEAD(1)) +
134100161128     c                              wdataoggi + 'T' + wora + ';'
134200161125     c                   exsr      tivgd_$E
134300161125      *
134400161125      **   #ENCODING;ISO-8859-1;
134500161125     c                   clear                   dati_TTE
134600161125     c                   EVAL      dati_TTE = HEAD(2)
134700161125     c                   exsr      tivgd_$E
134800161125      *
134900170410      **   #VERSION;03.10;
135000161125     c                   clear                   dati_TTE
135100161125     c                   EVAL      dati_TTE = HEAD(3)
135200161125     c                   exsr      tivgd_$E
135300161125      *
135400161125      **   #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
135500161125     c                   clear                   dati_TTE
135600161125     c                   EVAL      dati_TTE = HEAD(4)
135700161125     c                   exsr      tivgd_$E
135800161125      *
135900161125      **   #DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;........
136000161125     c                   clear                   dati_TTE
136100161125     c                   EVAL      dati_TTE = %trim(HEAD(5)) +
136200161125     c                                        %trim(HEAD(6)) +
136300161125     c                                        %trim(HEAD(7)) +
136400161125     c                                        %trim(HEAD(8)) +
136500161125     c                                        %trim(HEAD(9)) +
136600161125     c                                        %trim(HEAD(10)) +
136700161125     c                                        %trim(HEAD(11)) +
136800161130     c                                        %trim(HEAD(12))
136900161125     c                   exsr      tivgd_$E
137000161125      *
137100170410      **   HEADER;03.10;STATUS;
137200161125     c                   clear                   dati_TTE
137300161125     c                   EVAL      dati_TTE = HEAD(14)
137400161125     c                   exsr      tivgd_$E
137500161125      *
137600161125     c                   endsr
137700161125      *-------------------------------------------------------------------------
137800161125     c     FINE_TTEVENT  BEGsr
137900161125      *-------------------------------------------------------------------------
138000161125     c                   clear                   dati_TTE
138100161125     c                   movel     '#END;'       dati_TTE
138200161125     c                   exsr      tivgd_$E
138300161125      *
138400161125     c                   endsr
138500161125      *-------------------------------------------------------------------------
138600161125     C     TTEVENTS_Dati Begsr
138700161125      *-------------------------------------------------------------------------
138800161128     c                   eval      non_flaggare_DP3 = ' '
138900161128     c                   eval        ContInfo1      = ' '
139000161128     c                   eval        ContInfo2      = ' '
139100170502     c     ';':' '       xlate     BLPLOD        BLPLOD
139200170502     c     ';':' '       xlate     BLPRMA        BLPRMA
139300161128      *
139400161128     c                   Select
139500161128      * SCAN 01/02/05/10
139600161129     c                   WHEN      Scan_code = '01' or Scan_code = '02'or
139700161129     c                             Scan_code = '05' or Scan_code = '10'
139800161128      * SCAN 03
139900161129     c                   WHEN      Scan_code = '03'
140000161128      * SCAN 06
140100161129     c                   WHEN      Scan_code = '06'
140200161128      * SCAN 04/08
140300161128      * SCAN 07/09/13/14
140400161129     c                   WHEN      Scan_code = '04' or Scan_code = '08' or
140500161129     c                             Scan_code = '13' or Scan_code = '14' or
140600161129     c                             Scan_code = '07' or Scan_code = '09'
140700161128      * SCAN 18
140800161129     c                   WHEN      Scan_code = '18'
140900161128      * solo x Import
141000161129     c                   IF        wIMPORT   ='S'
141100161128      * inviato ISOLA
141200161128     c                   IF         DP3CEV ='ISL'
141300161128     c                   eval        ContInfo1      = '00'
141400161128     c                   eval        ContInfo2      = 'ISLAND'
141500161128     c                   ENDIF
141600161128      *
141700161128      * inviato ALERT con mail o SMS
141800161128     c                   IF        (DP3CEV ='S25' or DP3CEV ='E25') or
141900161128     c                             (DP3CEV ='S44' or DP3CEV ='E44' or
142000161128     c                              DP3CEV ='X44')
142100161128     c                   Exsr      Alert_18_44
142200161128      * non c'era il Predict
142300161128     c                   if            predict_invio = *blank
142400161128     c                   eval      non_flaggare_DP3 = 'N'
142500161128     c                   clear                   ContInfo1
142600161128     c                   clear                   ContInfo2
142700161130      ** NON deve inviare lo SCAN 18 se mancano i dati
142800161130     c                   LEAVEsr
142900161130     c                   end
143000161128     c                   ENDIF
143100161128      *
143200161128     c                   END
143300161128      *
143400161128     c                   ENDSL
143500161128      *
143600161128      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
143700170410      *  particolarità sullo SCAN 13 vogliono il PODIMAGE7 con tutti zeri
143800170410      *  altrimenti è sempre vuoto.
143900170410      *
144000161128     c                   clear                   dati_TTE
144100170410     c                   clear                   podImage7
144200170410      * solo sulla consegna '0000000'
144300170410     c                   if        Scan_CODE = '13'
144400170410     c                   move      *all'0'       podImage7
144500170410     c                   end
144600161128      *
144700161125     c                   EVAL      dati_TTE = %trim(HEAD(15))  + ';' +            0 TTEVENTS;2
144800161125     c                                        %trim(ParcelNr)  + ';' +            1 PARCELNUMBER
144900170410     c                                            'DE06'       + ';' +            2 STATUSTYPE
145000161125     c                                                           ';' +            3 ORIGINPARCELNUMB
145100161125     c                                     %trim(ServiceCODE)  + ';' +            4 SERVICECODE
145200170410     c                                     %trim(CAP_Dest)     + ';' +            5 DZIPCODE
145300170410     c                                     %trim(CountryCodISO)+ ';' +            6 DCOUNTRYCODE
145400170410     c                                     %trim(Scan_CODE)    + ';' +            7 SCANCODE
145500170410     c                                     %trim(zcod(1))      + ';' +            8 REASONCODE
145600170410     c                                     %trim(zcod(2))      + ';' +            9 REASONCODE2
145700170410     c                                     %trim(zcod(3))      + ';' +           10 REASONCODE3
145800170410     c                                  '023' +  Sender_Depot  + ';' +           11 DEPOT
145900170410     c                                                           ';' +           12 AGENTLOCATCODE
146000170410     c                                                           ';' +           13 AGENTLOCATTYPE
146100170410     c                                                           ';' +           14 AGENTPARCELNUMBE
146200170410     c                                     %trim(BLPNZD)       + ';' +           15 COUNTRY
146300170410     c                                     %trim(BLPLOD)       + ';' +           16 CITY
146400170410     c                                     %trim(Data_Evento)        +           17 STATUSDATETIME
146500170410     c                                     %trim(HMS__Evento)  + ';' +           18
146600170410     c                                                           ';' +           19 TIMEZONE
146700161128     c                                            wdataoggi          +           20 MEMDATETIME
146800170410     c                                            wora         + ';' +           21
146900170517     c                              '023' +  Delivery_Depot    + ';' +           22 DDEPOT
147000170410     c                                     '001'               + ';' +           23 DELIVERYTOUR
147100170410     c                             %trim(%editc(Peso_in_10gr:'4')) + ';' +       24 MEASUREDWEIGHT
147200170410     c                                                           ';' +           25 DIMENSION
147300170410     c                                     %trim(BLPRMA)       + ';' +           26 PARCELREF
147400170410     c                                                           ';' +           27 PARCELREF2
147500170410     c                                   %trim(PodImage7)      + ';' +           28 PODIMAGEREF
147600170410     c                                   %trim(Firma_Consegna) + ';' +           23 RNAME
147700170410     c                                                           ';' +           38 TRDPNAME
147800170410     c                                         '001'           + ';' +           39 AREA
147900170410     c                                         '001'           + ';' +           30 STOP
148000170410     c                                     %trim(ContInfo1)    + ';' +           41 CONTAINERINFO1
148100170410     c                                     %trim(ContInfo2)    + ';' +           42 CONTAINERINFO2
148200170410     c                                                           ';' +           43 SHIPMENTREF
148300170410     c                                                           ';' +           44 SUPPLIERREF
148400170410     c                                                           ';' +           45 CONTAINERREF
148500170410     c                                                           ';' +           46 CONTAINERTYPE
148600170410     c                                                           ';' +           47 ACTORID
148700170410     c                                                           ';' +           48 OPERATIVERID
148800170410     c                                                           ';' +           49 PICKUPORDERNUMBE
148900170410     c                                                           ';' +           40 GPSLAT
149000170410     c                                                           ';' +           51 GPSLONG
149100170410     c                                                           ';' +           52 RETURNPARCELNUMB
149200170410     c                                                           ';'             53 COMPINFO1
149300161125      *
149400161125     c                   exsr      tivgd_$E
149500161125      *
149600161125      *
149700161125     c                   endsr
149800161125      *-------------------------------------------------------------------------
149900161125     C     tivgd_$E      Begsr
150000161125      *-------------------------------------------------------------------------
150100161125     c                   clear                   tivgd000
150200161125     c                   eval      vgddta = %TrimR(dati_TTE)
150300161125     c                   eval      vgdtip = '$E'
150400161125     c                   eval      vgdksu = '0DPD0OUT'
150500161125     c                   eval      vgdtsc = 'WW'
150600161125     c                   eval      vgdpgm = 'FIEU16R'
150700161125     c                   eval      vgddat = W0080
150800161125     C                   WRITE     tivgd000
150900161125      *
151000161125     C                   Endsr
151100161128      *-------------------------------------------------------------------------
151200161128      *   Se inviato il PREDICT (Alert) SCAN 18(44)        *
151300161128      *-------------------------------------------------------------------------
151400161128     C     Alert_18_44   Begsr
151500161128      *
151600161130     c                   clear                   predict_invio     1
151700161128     c                   eval        ContInfo1      = '01'
151800161130     c                   eval        ContInfo2      = '44:1'
151900161128      *
152000161128      * aggancia FIAR500R per le decodifiche da passare
152100161128      *   per poi generare  il filler con lo scan 18
152200161128     c                   movel     'EMD'         TipoAr5
152300161128     c     Kar5          chain     fiar501l
152400161128     c                   IF        %found(fiar501l)
152500161128     c                   eval        dar5EMD = AR5UNI
152600161128      *
152700161130     c                   eval        ContInfo2 = %trim(ContInfo2) + ':' +
152800161130     c                                        %trim(§AR5EML)      + ':' +
152900161130     c                                        %trim(§AR5TEL)      + ':'
153000161128      *  EMAIL/SMS
153100161128     c                   eval          predict_invio ='S'
153200161128     c                   move      '3'           tipopredict       1
153300161128     c                   if          §AR5EML<> *blank and §AR5TEL = *blank
153400161128     c                   move      '2'           tipopredict
153500161128     c                   elseif      §AR5EML = *blank and §AR5TEL<> *blank
153600161128     c                   move      '1'           tipopredict
153700161128     c                   elseif      §AR5EML = *blank and §AR5TEL = *blank
153800161128     c                   clear                   predict_invio     1
153900161128     c                   leavesr
154000161128     c                   end
154100161128      *
154200161128      *   durante il MIC  (DLO)
154300161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
154400161128     c                                    tipopredict +  ':2:DLO:'
154500161128      *   DATA
154600161128     c                   if          §AR5DTO<> *blank and §AR5DOS = *blank
154700161128     c                                  or
154800161128     c                               §AR5DTO > §AR5DOS
154900161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
155000161128     c                                 %subst(§AR5DTO:1:8) + ':' +
155100161128     c                                 %subst(§AR5DTO:9:4) + ':'
155200161128      *
155300161128     c                   elseif      §AR5DOS<> *blank and §AR5DTO = *blank
155400161128     c                                  or
155500161128     c                               §AR5DOS > §AR5DTO
155600161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
155700161128     c                                 %subst(§AR5DOS:1:8) + ':' +
155800161128     c                                 %subst(§AR5DOS:9:4) + ':'
155900161128     c                   end
156000161128      *    TIME
156100161128     c                   if        §AR5OPCD <> *blank
156200161128     c                                  and §AR5OPCA <> *blank
156300161128      *
156400161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
156500161128     c                               '1:' + §AR5OPCD +':'+ §AR5OPCA +'::'
156600161128      *
156700161128     c                   elseif    §AR5OPCD = *blank
156800161128     c                                  and §AR5OPCA = *blank
156900161128      *
157000161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
157100161128     c                                 '0:1900:::'
157200161128      *
157300161128     C                   End
157400161128      *------
157500161128     C                   END
157600161128      *
157700161128     C                   Endsr
157800161128      *-------------------------------------------------------------------------
157900161125** (HEAD)
158000170410#FILE;GEODATA_STATUS_0230820_N00_D                                                 01<
158100161125#ENCODING;ISO-8859-1;                                                              02
158200170410#VERSION;03.10;                                                                    03<
158300161125#DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;                                       04
158400161130#DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;         05|
158500170410SERVICECODE;DZIPCODE;DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;                 06||
158600161130REASONCODE3;DEPOT;AGENTLOCATIONCODE;AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;       07| |
158700161130COUNTRY;CITY;STATUSDATETIME;TIMEZONE;MEMDATETIME;DDEPOT;DELIVERYTOUR;              08|  |
158800170410MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;PODIMAGEREF;RNAME;TRDPNAME;          09|  |
158900161130AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;      10| |
159000161130CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;               11||
159100161130RETURNPARCELNUMBER;COMPINFO1;;                                                     12|
159200161130                                                                                   13
159300170410HEADER;03.10;STATUS;                                                               14<
159400161125TTEVENTS;2                                                                         15<----
159500161125** (CMD)
159600060609CLRPFM FILE(TIDP410W)
