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
001200050418      *
001300050418     FFNARB01L  IF   E           K DISK
001400050418     FFNBLP01L  IF   E           K DISK
001500060727     FFNBLT01L  IF   E           K DISK
001600060213     Ffiar401L  IF   E           K DISK
001700160502     Ffiar501L  IF   E           K DISK
001800050418     FTABEL00F  IF   E           K DISK
001900050418     FTNTBE01L  IF   E           K DISK
002000050418     FAZORG01L  IF   E           K DISK
002100060613     ftivgd00f  if a E             DISK
002200101126      *
002300101126     Ftigcp01L  IF   E           K DISK
002400171017      *
002500171017     Ffipnd01L  IF   E           K DISK
002600171023     FDPcdp12i  iF   E           K DISK
002700171023      ** ---------------------------------------------------------*
002800171023     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
002900171023      ** -------------------------------------------------------- *
003000171017     D File_vers       S              1    inz('T')                             dal 12/4/2017
003100161125     D VGDTIP_$E       S              6    inz('APRE  ')
003200161125     D TESTA_TTE       S              2    inz('SI')
003300161125     d dati_TTE        S           2050    inz(' ')
003400161125      *----------------------------------------------------*
003500080903     D K2Z             S              3    DIM(999) inz                         CODICE NAZIONE
003600080903     D T2Z             S             89    DIM(999) inz                         CODICE NAZIONE
003700050418     D KEVD            S             15    DIM(30)  inz                         CODICE NAZIONE
003800050418     D EVD             S            256    DIM(30)  inz                         CODICE NAZIONE
003900161129     D hubDPD          S              3  0 DIM(99)                              Testata scan
004000050418     D POBAR           S              3  0 DIM(500)                             Testata scan
004100050418     D PODPD           S             25    DIM(500)                             Testata scan
004200050418     D ZCOD            S              2    DIM(3)                               P.O. COMODO
004300050418     D S3A             S              2    DIM(30)                              Tipo bolla
004400060727     D S3E             S              1    DIM(50)                              Tipo Anomalia
004500050418      *-------------------
004600161125     D HEAD            S             80    DIM(15) CTDATA PERRCD(1)
004700050418      *---------------------------------------------------------------------*
004800050418     D  MSGDS          S            100
004900050418     D  LENGH          S             15  5
005000050418      *---------------------------------------------------------------------*
005100171020     d DPNDDP4       e ds
005200171020     d DPNDDP5       e ds
005300171020      **
005400171017     D Delivery_Depot  S              7
005500171017     D Sender_Depot    S              7
005600161125     D Data_Evento     S              8
005700161130     D Ora__Evento     S              4
005800161130     D HMS__Evento     S              6
005900161125     D ServiceCODE     S              3
006000050418     D DIST            S              3    inz('001')
006100050418     D SSTOP           S              3    inz('001')
006200161125     D CAP_Dest        S              9
006300161125     D CountryCodISO   S              3
006400161128     D Firma_Consegna  S             35
006500161128     D ContInfo1       S              2
006600161128     D ContInfo2       S            200
006700170410     D PodImage7       S              7
006800050418      *-------------------
006900060328     D wIMPORT         S              1A   inz
007000140331     D Peso_in_10gr    S              8S 0 inz
007100150323     D Peso_in_BLT     S              6S 0 inz
007200050418      *-------------------
007300050418     D XX              S              3  0 inz
007400050418     D YY              S              3  0 inz
007500050418     D WnewRec         S              7  0                                      *rec da trasmettere
007600050418     D W0080           S              8  0 inz
007700050418     D W008M           S              8  0 inz
007800050418     D W0140           S             14  0 inz
007900161128     D SCAN_CODE       S              2
008000050418     D W015A           S             15
008100161125     D ParcelNr        S             14
008200060727     D Wanomalia       S              1
008300050418     D Welabora        S              1
008400050418     D Wtestata        S              1
008500050418     D Wdataoggi       S              8
008600050418     D Wdata80         S              8  0 inz
008700050418     D Wdata_90        S              8
008800050418     D Wora            S              6
008900050418     D Wdata           S               D   DATFMT(*eur)
009000050418     D W0m90           S               D   DATFMT(*eur)
009100050418     D Kcod            S                   LIKE(TBLCOD)
009200060612     D KTRC            S                   LIKE(AR4TRC)
009300050418     D KDTR            S                   LIKE(DP3DTR)  INZ
009400050418     D KFTR            S                   LIKE(DP3FTR)  INZ
009500050418     D WPKC01          S                   LIKE(§DPPpkb)
009600050418     D WPKC02          S                   LIKE(§DPPpkb)
009700050418     D Kcode           S                   LIKE(TBECOD)
009800050418     D Kke1            S                   LIKE(TBEKE1)
009900080403     D d41dat          S           2048
010000050418      *-------------------
010100160516     d non_flaggare_DP3...
010200160516     d                 S              1
010300160824      *-------------------
010400171017     D DS3IDP        E DS
010500171017      *-------------------
010600060621     D XTOEBCDDS     E DS
010700060619     D DS15          E DS
010800150826     D DSQT1         E DS                  INZ
010900050418     D DDPP          E DS
011000050418     D DS3A          E DS
011100060727     D DS3E          E DS
011200050418     D DS2Z          E DS
011300050418     D DEVD          E DS
011400050418     D CNCR80        E DS
011500050418     D UT§DSE0F      E DS
011600050418     D OG143         E DS
011700151211     D dsDP3FLR      E DS
011800160502     D Dar5EMD       E DS
011900130704      *
012000050418     D DSFNBLP       E DS                  EXTNAME(FNBLP00F)
012100050418     D DSFNARB       E DS                  EXTNAME(FNARB00F)
012200130704      *
012300050418     D KPJBA         E DS
012400050418     D TRUL06DS      E DS
012500050418     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
012600050418     D tibs36ds      E DS
012700050418      *
012800060613     d trul47ds      e ds
012900130704
013000081015     d DSCAP           ds
013100081015     d  CAPinSK                1      9
013200081015     D SkCAP                   1      9    dim(9)
013300050418      *----------------------------------------------------*
013400050418     IFNARB000
013500050418     I              ARBAAS                      BLPAAS
013600050418     I              ARBLNA                      BLPLNA
013700050418     I              ARBPKB                      BLPPKB
013800150709     I              ARBPKC                      BLPPKC
013900050418     I              ARBCAD                      BLPCAD
014000161125     I              ARBLOD                      BLPLOD
014100050418     I              ARBNZD                      BLPNZD
014200050418     I              ARBCBO                      BLPCBO
014300150826     I              ARBNCL                      BLPNCL
014400150826     I              ARBNCP                      BLPNCP
014500161125     I              ARBRMA                      BLPRMA
014600161125     I              ARBRMN                      BLPRMN
014700060619      *----------------------------------------------------*
014800060609     c     KDP3          setll     tIDP3000
014900060609     c     KDP3          readE     tIDP3000
015000060609     C                   DOW       NOT %eof(tIDP303L)
015100161129
015200050418      *   elaboro se data evento impostata
015300161129      *   IMPOSTA subito lo SCAN
015400050418     C                   IF        DP3dev > *zeros
015500161129     c                   movel     dp3evd        Scan_CODE
015600161129
015700161129      *   se lo SCAN è in tabella EVD
015800161129     c                   eval      w015A = SCAN_CODE
015900050418     c                   z-add     1             XX
016000050418     c     w015a         lookup    KEVD(xx)                               31
016100161129      ** quindi Considera se è uno SCAN
016200161129      ** oppure x gli ORM Italia su Italia (detti Italy Domestic) LNP e LNA Italia
016300161129     c                   IF        *IN31 = *on or Dp3CEV = 'ORM'
016400050418     C                   movel     EVD(XX)       devd
016500130705
016600050418     c                   IF        §evditr = 'S'  or  §evdetr = 'S'
016700080603     c                             or Dp3CEV = 'ORM'
016800050418
016900050418      *   Aggancio file Partenza o Arrivi per reperire dati
017000161129     c                   clear                   qualifica        10
017100050418     c                   clear                   Welabora
017200161125     c                   clear                   ParcelNr
017300161125     c                   clear                   ServiceCODE
017400060727     c                   clear                   Wanomalia
017500161128     c                   clear                   Delivery_Depot
017600050418     c                   clear                   dsFNBLP
017700050418     c                   clear                   dsFNARB
017800150605     c                   clear                   Peso_in_BLT
017900161128     c                   clear                   Firma_consegna
018000050418      *
018100161129      * Se trovato con LNP    --> è una spedizione Import DPD
018200161129     C                   z-add     1             YY
018300161129     c     DP3lnp        lookup    hubDPD(YY)                             33
018400161129     c                   eval      wIMPORT  = 'N'
018500060328     c   33              eval      wIMPORT  = 'S'
018600050418      *
018700050418     C                   SELECT
018800050418      *
018900161129      ** se è un evento di import da trattare oppure
019000161129      ** è un evento x gli ORM Italia su Italia (detti Italy Domestic)
019100161129     c                   WHEN      (wIMPORT  = 'S' and §evditr = 'S')
019200080603     c                             or Dp3CEV = 'ORM'
019300130705      *
019400130705     c     Kspe          chain     FNBLP01L
019500130705     c                   if        %found(FNBLP01L)
019600050418     c                   eval      Welabora = 'S'
019700161129      *
019800161129      * Cerca la Firma di Consegna della spedizione
019900161129     c                   if        SCAN_code = '13'
020000161129     c                   move      '1'           ktrc
020100161129     c     Kar4          chain     fiar401l
020200161129     c                   if        %found(fiar401l) and ar4not <> *blank
020300170502     c     ';':' '       xlate     ar4not        Firma_Consegna
020400161129     c                   end
020500161129     c                   endif
020600161129      *
020700060727      * ? controllo su FNBLT se esiste un'anomalia (APERTO, BAGNATO....SCONDIZIONATO)
020800060727      * ? da segnalare a DPD onde non prendersi poi la responsabilità del DANNO.
020900061011      * ?  solo se non è stato dato già x consegnato
021000150323     c                   clear                   Peso_in_BLT
021100061011     c                   if        BLPdcm = 0
021200060727     c     Kspe          chain     FNBLT01L
021300150323     c                   if        %Found(FNBLT01L) and bltPUC > 0
021400150323      * ?  si deve memorizzare anche l'eventuale PESO da CML del parcel
021500150323      * ?   in decigrammi così elimina i centigrammi arrotondando.
021600150323     c     BLTpuc        mult(h)   100           Peso_in_BLT
021700150323     c                   end
021800150323      * ?   ci sono SPUNTE?
021900060727     c                   if        %Found(FNBLT01L) and bltCAN <> *blank
022000060727      **? controlla se presente fra le Spunte da segnalare a DPD
022100060727     c                   z-add     1             xx
022200060727     c     BLtcan        lookup    S3e(xx)                                31
022300060727     c   31              move      S3e(xx)       Wanomalia
022400060727     c                   endif
022500161128      **
022600061011     c                   endif
022700050418     c                   endif
022800050418      *
022900161129      * Se non trovato allora --> è una spedizione export DPD
023000060328     c                   WHEN      wIMPORT  <> 'S'    and  §evdetr = 'S'
023100161129     c                   eval      Welabora = 'S'
023200130704     c     Kspe          chain     FNARB01L
023300130704     c                   if        %found(FNARB01L)
023400060613     c                   endif
023500060619      *
023600050418     C                   ENDSL
023700080603      *-----------*
023800060619      * rek 'I'
023900171017     c     BOLLA_dp3     chain     fipnd01l
024000161129      *
024100171023      *  Deve impostare il DEPOT di CONSEGNA in funzione del verso della spedizione
024200171023      *   oppure se si tratta di un ORM ITALIA-ITALIA quindi non presente sul FIPND.
024300171017     c                   if        %found(fipnd01l)
024400171023      *  se IMPORT
024500171023     c                   if        pndDPE  = 'DI4'
024600171017     c                   movel     pndIDA7       Delivery_depot
024700171023      *  se EXPORT vecchia etichetta
024800171023     c                   elseif    pndDPE  = 'DP4'
024900171023     c                   eval      DPNDDP4 = pndETIU
025000171023      * trascodifico la B.U.
025100171023      * recupero  versione di cappario
025200171023     C                   clear                   ISID1DRI
025300171023     c                   z-add     wdata80       ISID1DRI
025400171023     C                   clear                   OSID1VER
025500171023     C                   clear                   OSID1VERD
025600171023     C                   clear                   OSID1DDE
025700171023     C                   clear                   OSID1DSC
025800171023     C                   clear                   OSID1BCID
025900171023     c                   call      'TISID1R'
026000171023     C                   PARM                    ISID1DRI
026100171023     C                   PARM                    OSID1VER
026200171023     C                   PARM                    OSID1VERD
026300171023     C                   PARM                    OSID1DDE
026400171023     C                   PARM                    OSID1DSC
026500171023     C                   PARM                    OSID1BCID
026600171023      * se TROVATA VERSIONE CAPPARIO
026700171023      * prova a decodificare il DEPOT con il nuovo cappario per reperire la BU
026800171023     c                   if        osid1ver > 0
026900171023     c                   move      *blank        CDPATB
027000171023     c                   z-add     OSID1VER      CDPVER
027100171023     c                   movel     §b4IDA        CDPDSTR
027200171023     c     Kcdp4         chain     dpcdp12i
027300171023      *   il Depot con la B.U.
027400171023     c                   if        %Found(dpcdp12i)
027500171023     c                   eval      Delivery_depot = CDPDPTN
027600171023     c                   end
027700171023     c                   end
027800171023      ** ------
027900171023      *  se EXPORT  nuova  etichetta
028000171023     c                   elseif    pndDPE  = 'DP5'
028100171023     c                   eval      DPNDDP5 = pndETIU
028200171023     c                   movel     §PNDBUCA      Delivery_depot
028300171023     c                   move      §pndIDAst     Delivery_depot
028400171023     c                   end
028500171023
028600171017     c                   movel     pndISC        ServiceCODE
028700171017     c                   eval      ParcelNr = pndIPN
028800171017
028900171023      *   Se non trovato il tipo record ("I")
029000080603     c                   else
029100171023      *
029200161129      **  Se è un evento x gli ORM Italia su Italia (detti Italy Domestic)
029300171023      *   il Parcel è fittizio ed è stato staccato durante la generazione
029400171023      *   degli SCAN x l'ORM. Quindi non esiste sul FIAR4 poichè si tratta
029500171023      *   di una bolla Italia.
029600161128      **   Convertitore
029700080603     c                   if        Dp3CEV = 'ORM'
029800161129     c                   eval      ParcelNr = Dp3PRN
029900171023      **
030000171023      *   Fine codifica ORM Italy Domestic.
030100171023      *    imposta il Depot di delivery direttamente dagli SCAN
030200171023     c                   z-add     1             xx
030300171023     c     blpLNA        lookup    pobar(xx)                              31
030400171023     C   31              movel     POdpd(xx)     OG143
030500171023     c   31              move      §ogdp1        Delivery_Depot
030600171023     c   31              movel     BU_brt        Delivery_Depot
030700080603     c                   endif
030800080603      *
030900080603     c                   endif
031000161129      *---------
031100161129      *  se il parcel non era presente su DP3  -->  AGGIORNA il DP3 con il PARCEL
031200161129     c                   if        dp3prn = *blank and ParcelNr <> *blank
031300161125     c                   eval        dp3prn = ParcelNr
031400160502     c                   endif
031500130705      *----------------------------------------------------------
031600171023      *   Flagga il TIDP3 di Lettura .
031700060612      * Se ho trovato i file proseguo altrimenti aggiorno file eventi
031800160502     c                   IF        Welabora <> 'S'
031900050418     c                   eval      DP3ftr = 'T'
032000050418     C                   movel     99990101      DP3dtr
032100161129     C                   eval      DP3flr = 'NON ELABORARE - da EVD'
032200060609     c                   update    tidp3000
032300050418     C                   ELSE
032400050418      *
032500050418      *   Se bolla DPD x DPD non trasmetto dati e aggiorno File Eventi
032600161129     c     DP3lnp        lookup    hubDPD                                 31
032700161129     c   31BLPlna        lookup    hubDPD                                 31
032800161129     C                   IF        *in31
032900050418     c                   eval      DP3ftr = 'T'
033000050418     C                   movel     99990102      DP3dtr
033100161129     C                   eval      DP3flr = 'DA DPD A DPD - Errata'
033200060609     c                   update    tidp3000
033300050418     c                   ELSE
033400050418      *
033500050418      *   Se è una bolla recupero non trasmetto  dati
033600050418     c     BLPcbo        lookup    S3A                                    31
033700050418     C                   IF        *in31 = *on
033800050418     c                   eval      DP3ftr = 'T'
033900050418     C                   movel     99990103      DP3dtr
034000161129     C                   eval      DP3flr = 'BOLLA RECUPERO da NON trasm.'
034100060609     c                   update    tidp3000
034200050418     c                   ELSE
034300050418      *
034400161129      *   Se ho trovato i file elaboro -----*
034500050418     C                   exsr      elabora
034600161129      *                          *----------*
034700050418     C                   ENDIF
034800050418     C                   ENDIF
034900050418     C                   ENDIF
035000050418      *  ----------------------
035100050418     C                   ENDIF
035200050418     C                   ENDIF
035300050418      *
035400161129     C                   ENDIF
035500060609     c     KDP3          reade     tIDP3000
035600050418     c                   ENDDO
035700160502      * ____________________________________________________________
035800161125     C* chiude il TIVGD se era stato aperto
035900161125     c                   if        VGDTIP_$E = *blank  AND
036000161125     c                             (File_vers ='E' or File_vers ='T')
036100161125     c                   eval      VGDTIP_$E = 'CHIUDE'
036200161125     c                   exsr      Wrk_TTEVENTS
036300161125     c                   end
036400161125     C*
036500050418     c                   EVAL      *INLR = *ON
036600050418      *----------------------------------------------------*
036700050418      *   ELABORO RECORD VALIDO                            *
036800050418      *----------------------------------------------------*
036900050418     C     ELABORA       BEGSR
037000060103      *
037100060103      * ?Riportato qui il controllo presente sul vecchio FIEU25R ora sostituito
037200150709      * ? dal giro del Manifest TRTC83R12.
037300060103      * ?Non devono essere inviati SCAN di tipo 05 - 06 - 10 con date superiori
037400060103      * ? alla data del giorno -> altrimenti creano problemi a DPD.
037500060103      * ? ******* Manda a FINE routine x evitare di inviare lo SCAN che manderà
037600060103      * ? ******* in un giro successivo.
037700161129     c                   If        Scan_CODE = '05' or Scan_CODE = '06' or
037800161129     c                             Scan_CODE = '10' or
037900161129     c                             (SCAN_CODE = '18' and
038000161129     c                               (dp3CEV='S25' or dp3CEV='E25'))
038100160518      * ? ** Adesso aggiungiamo l'ALERT **
038200160518      * xchè deve essere inviato con la stessa data del MIC o della consegna.
038300160518      *   ...quindi viene scritto nel TIDP300 con una data Futura in attesa del MIC.
038400160518      *   Se data FUTURA
038500161129      *    e non è x un ORM
038600060103     c                   If        DP3dev > wdata80
038700161129     c                               and
038800161129     c                             DP3CEV <> 'ORM'
038900060103     c                   goto      salta_scan
039000080603     c                   endif
039100060103     c                   end
039200161129      *****-------------------------------    QUA se NON ha saltato
039300050418      * P.O. scan
039400050418     c                   z-add     1             xx
039500161125     c                   clear                   Sender_Depot
039600050418     c     dp3fle        lookup    pobar(xx)                              31
039700050418     C   31              movel     POdpd(xx)     OG143
039800050418      *
039900050418     C                   SELECT
040000050418     c                   WHEN      *in31 = *off
040100130705      *
040200080611      *  solo x scan Fissi degli Orm Italy domestic (LOCALI Italia su Italia)
040300080611     c                   WHEN      DP3CEV = 'ORM'
040400161125     c                   move      §ogdp1        Sender_Depot
040500171017     c                   movel     BU_brt        Sender_Depot
040600050418      *
040700060328     c                   WHEN      wIMPORT  ='S'    and  §evdige = 'H'
040800130705     c                                or
040900130705     c                             wIMPORT  <> 'S'  and  §evdege = 'H'
041000161125     c                   move      §oghb1        Sender_Depot
041100171017     c                   movel     BU_brt        Sender_Depot
041200171017      *
041300060328     c                   WHEN      wIMPORT  ='S'    and  §evdige = 'D'
041400130705     c                                or
041500130705     c                             wIMPORT  <> 'S'  and  §evdege = 'D'
041600161125     c                   move      §ogdp1        Sender_Depot
041700171017     c                   movel     BU_brt        Sender_Depot
041800050418     c                   ENDSL
041900160519      *
042000160519      * Scan '03' in consegna segnalo consegna ISOLA
042100161129     c                   IF         SCAN_CODE ='18' and DP3cev = 'ISL'
042200161129     c                                or
042300161129     c                              SCAN_CODE = '03'  and  BLPfin = 'I'
042400160519     c                   movel     'ISLAND'      qualifica
042500161129     C                   ENDIF
042600050418      *
042700161125     c                   movel     dp3dev        Data_Evento
042800161125     c                   movel     dp3hev        Ora__Evento
042900161130     C                   move      *zeros        HMS__Evento
043000161130     c                   movel     dp3hev        HMS__Evento
043100050418      *
043200060619      *  Country Code ISO numerico x nuovi SCAN
043300161125     c                   clear                   CountryCodISO
043400060619     C                   z-add     1             TBLKUT
043500060619     C                   movel     '15'          TBLCOD
043600060619     C                   movel(p)  BLPnzd        TBLKEY
043700150826     c     KTABel        chain     tabel00f
043800060619     c                   if        %FOund(tabel00f)
043900060619     c                   movel     tbluni        ds15
044000161125     c                   move      §15CIE        CountryCodISO
044100060619     c                   end
044200050418      *
044300050418      * Codice Postale - Se diretta in GB forzo "01111"
044400050418      *                  Se trovo lettere non le scrivo
044500161125     c                   movel     blpCAD        CAP_Dest
044600081015      *
044700081015      * Se Gran Bretagna compatta il CAP togliendo i blank
044800081015     c                   if         BLPnzd = 'GB '
044900161125     c                   eval       CAPinSK = CAP_Dest
045000081015     c                   exsr      Compatta_CAP
045100161125     c                   eval       CAP_Dest = CAPinSK
045200081015     c                   endif
045300050418      *
045400081024      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
045500081024      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
045600161125     C                   if        CAP_Dest = 'EIRE'
045700161125     c                   eval      CAP_Dest = '0000001'
045800081024     c                   end
045900050418      *
046000130704      * se Export DPD  nella Route metto il Depot DPD da linea arrivo DPD
046100130704      *  Altrimenti quello sull'organigramma della nostra LNA
046200171023      **********  (serviva come paracadute ---- da eliminare in seguito)
046300171023     c**********         IF        wIMPORT  = 'S'
046400171023     c********           z-add     1             xx
046500171023     c**** BLPlna        lookup    pobar(xx)                              31
046600171023     C** 31              movel     POdpd(xx)     OG143
046700171023     c** 31              move      §ogdp1        Delivery_Depot
046800171023     c** 31              movel     BU_brt        Delivery_Depot
046900171023     c********           END
047000171023      **********
047100050418      * CODICI ANOMALIA
047200050418     c                   clear                   ZCOD
047300140331     c                   clear                   Peso_in_10gr
047400141217      *
047500141217      * il peso è con un decimale e deve essere passato intero con uno zero di arrotondamento
047600141217      * in step da 10gr.--> 3,6kg deve diventare 360 nella variabile richiesta da DPD lunga 8
047700141217     c     blpPKC        mult      100           Peso_in_10gr      8 0
047800150323      ***
047900150323      * ?   se non c'è il VDL (perchè non è stato ancora chiuso il foglio viaggio)
048000150323     c***  NON DEVE PIU' prendere il peso Fatturato - bensì deve prendere dal BLT il BLTPUC
048100150709      *  e comunque non deve mandare più il teorico.
048200150323     c                   if         Peso_in_10gr = 0
048300150323     c                               and
048400150323     c                              Peso_in_BLT > 0
048500150323     c                   move      Peso_in_BLT   Peso_in_10gr      8 0
048600150323     c                   end
048700150323      ***
048800150622      ** se proprio non c'è il peso in assoluto allora deve caricare il peso bollettato
048900150622     c                   if         Peso_in_10gr = 0
049000150623     c     blpPKB        mult      100           Peso_in_10gr
049100150622     c                   end
049200150826      ***
049300150826      *** ====================================
049400150826      **   solo x l'EXPORT quindi con ARB
049500150826      *** ====================================
049600150709      **   non inviamo informazioni di pesi FUORI MISURA (se può lo scoprirà DPD)
049700150709      **   quindi se si supera i 31,5 kg impostiamo fisso 31 kg.
049800150826     c                   if        wIMPORT <> 'S'
049900150826      ***
050000150826     c     *like         define    blppkb        wpes
050100150826     C     *LIKE         DEFINE    §qttpc        WTARA
050200150826      *
050300150826     C* PESO VDL - TARA
050400150826     C     §QTTPC        MULT      blpNCP        WTARA
050500150826     C                   Z-ADD     blpPKC        wpes
050600150826     c                   sub(h)    wtara         wpes
050700150826      *
050800150826      * Poi cerca il peso fra bollettato e CML
050900150826     C*  altrimenti se il bollettato è superiore passo il Bollettato.
051000150826     C     blpNCP        ifNE      blpNCL
051100150826     C     wpes          andLT     blpPKB
051200150826     C                   Z-ADD     blpPKB        wpes
051300150826     C                   ENDIF
051400150826      ***
051500150826     c     wpes          mult      100           Peso_in_10gr
051600150826      ***
051700150826     c                   if         Peso_in_10gr > 3150
051800150709     c                   eval       Peso_in_10gr = 3100
051900150709     c                   end
052000150623      ***
052100150826     c                   end
052200171024      ***
052300171024      *** Se si tratta di ORM non ha il Service code poichè non è su FIPND
052400171024      ***  essendo una spedizione ITALIA - ITALIA  quindi sulla base del peso
052500171024      ***   viene impostato il codice.
052600171024     c                   if        Dp3CEV = 'ORM'
052700171024     c                   clear                   peso_in_hg        7 0
052800171024      * tolgo l'ultima cifra che era decagrammi per comparare in etti
052900171024     c                   movel     Peso_in_10gr  peso_in_hg
053000171024      *** con il peso calcolare il service code
053100171024     c                   if        peso_in_hg  < hg_PPiccolo
053200171024     c                   movel     '136'         ServiceCODE
053300171024     c                   else
053400171024     c                   movel     '101'         ServiceCODE
053500171024     c                   end
053600171024     c                   end
053700171024      **
053800150826      *** ====================================
053900060727      * Anomalia (APERTO, BAGNATO.....SCONDIZIONATO) su IMPORT SCAN 10
054000161129     c                   IF        SCAN_CODE ='10' and wIMPORT ='S' and
054100061011     c                             Wanomalia <> *blank and blpdcm = 0
054200060727     c                   z-add     1             XX
054300060727     c     *blanks       lookup    Zcod(xx)                               31
054400060727      * Danneggiato: Damaged
054500060727     c   31              eval      ZCOD(XX) = '04'
054600060727     C                   ENDIF
054700050418      *
054800050418      * Scan '06' Reso forzo indirizzo errato
054900160830      * Scan '06' di DIROTTAMENTO impostiamo il "02" MISLOADED
055000161129     c                   IF        SCAN_CODE = '06'
055100050418     c                   z-add     1             XX
055200050418     c     *blanks       lookup    Zcod(xx)                               31
055300160830     c   31              if           DP3CEV =    'DIR'
055400160830     c                   move      '02'          ZCOD(XX)
055500160830     c                   else
055600160830     c                   move      '61'          ZCOD(XX)
055700160830     c                   end
055800101126      *
055900151214     c                   goto      al_momento_no
056000101126      * deve riportare l'ultimo motivo di giacenza assieme al reso
056100101126     c                   if        *in31
056200101126     c     Kgcp          chain     tigcp01l
056300101126     c                   if        %found(tigcp01l)
056400101126     c                   z-add     1             xx
056500101126     c     gcpCMC        lookup    K2z(xx)                                31
056600101126     c                   IF        *in31 = *on
056700101126     C                   movel     T2z(xx)       DS2Z
056800101126     c                   z-add     1             XX
056900151214     c     *blanks       lookup    zcod(xx)                               31
057000101126     c   31              eval      ZCOD(XX) = §2Zced
057100101126     c                   ELSE
057200101126     c                   eval      ZCOD(XX) = '11'
057300101126     c                   ENDIF
057400101126
057500101126     c                   endif
057600101126     c                   endif
057700151214     c     al_momento_no tag
057800161129
057900050418     C                   ENDIF
058000050418      * Eventi
058100161129     c                   If        SCAN_CODE <>'06'
058200050418     c                   IF        DP3cev <> *blanks
058300050418     c                   z-add     1             xx
058400050418     c     dp3cev        lookup    K2z(xx)                                31
058500050418     c                   IF        *in31 = *on
058600050418     C                   movel     T2z(xx)       DS2Z
058700050418     c                   z-add     1             XX
058800050418     c     *blanks       lookup    Zcod(xx)                               31
058900050418     c   31              eval      ZCOD(XX) = §2Zced
059000050418     c                   ENDIF
059100050418     c                   ENDIF
059200160830     c                   EndIF
059300050418      *
059400150709      *-------
059500150709      * Scan '03' in consegna segnalo FUORI MISURA
059600150709      *           se peso CML maggiore della tolleranza per fascia di peso
059700150709      * il 1/4/2014 :
059800150709      * DPD dice che sullo 03 non viene calcolato nel CLEARING e di mandarlo sullo 02
059900150709      *   in ogni caso lo inserisco anche sul passaggio alla HUB SCAN10 se già rilevato.
060000161129     c                   IF        SCAN_CODE= '03'  or
060100161129     c                             SCAN_CODE= '02'  or
060200161129     c                             SCAN_CODE= '10'
060300150709      * ma solo sull'IMPORT se supera i 31,5 kg.
060400150709     c                   IF         wIMPORT ='S' and BLPpkc > Wpkc01
060500150709      *  Not Conveyable (09)
060600151215      *    era una errata interpretazione lo 09 occorre inviare il 31 con mail di Jens
060700151215      *    Jens Opderbeck di oggi stesso
060800150709     c                   z-add     1             XX
060900150709     c     *blanks       lookup    Zcod(xx)                               31
061000151215     c   31              eval      ZCOD(XX) = '31'
061100150709     c                   end
061200150709      *--
061300150709     C                   ENDIF
061400140402      *-------
061500050418      * Colli sotto i 3 Kg
061600161125     c                   IF        ServiceCODE ='136'
061700050418     c                   z-add     1             XX
061800050418     c     *blanks       lookup    Zcod(xx)                               31
061900050418     c   31              eval      ZCOD(XX) = '44'
062000050418     C                   ENDIF
062100050418      *
062200050418      *
062300050418      * Se l'evento è in data di oggi e l'ora evento è maggiore dell'ora
062400050418      *  di elaborazione forzo questa come ora evento
062500161125     c                   IF        Data_Evento = WDATAoggi and
062600161125     c                             Ora__Evento > %subst(WORA:1:4)
062700161125     C                   movel     Wora          Ora__Evento
062800161130     C                   move      *zeros        HMS__Evento
062900161130     C                   movel     Wora          HMS__Evento
063000050418     c                   ENDIF
063100050418
063200050418      * Scrivo testata file se non ancora scritta
063300050418     c                   IF        Wtestata = *blanks
063400050418     C                   eval      Wtestata = 'S'
063500050418     c                   ENDIF
063600050418      * -----------
063700161125      *   Nuovo   FILE TTEVENTS
063800161125     c                   if        File_vers ='T' or
063900161125     c                             File_vers ='E'
064000161125     c                   exsr      Wrk_TTEVENTS
064100161125     c                   end
064200160516      *
064300160517      *  Se NON deve trasmettere lo SCAN (in questo caso lo SCAN 18 con mail/SMS)
064400160517      *   lo flagga
064500160516     c                   if        non_flaggare_DP3 <> *blank
064600160517     c                   eval      DP3ftr = 'T'
064700160517     C                   movel     99990104      DP3dtr
064800160517     C                   eval      DP3flr = 'NON TRASMESSO-MANCA MAIL/SMS'
064900160517     C                   ELSE
065000151211      *
065100151211     c                   exsr      Wrk_FILLER
065200050418      *
065300050418      * Aggiorno File eventi DPD
065400161125     C                   IF        Data_Evento > wdata_90
065500050418     c                   eval      DP3ftr = 'T'
065600050418     C                   else
065700050418     c                   eval      DP3ftr = 'S'
065800050418     C                   end
065900050418      *---
066000050418     C                   movel     wdataoggi     DP3dtr
066100160517     c                   end
066200160517      *---
066300160516     C     salta_scan    tag
066400160516      *
066500060609     c                   update    tidp3000
066600060103      *
066700050418     C                   ENDSR
066800050418      *----------------------------------------------------*
066900151211      *imposta il campo Filler con i dati dello SCAN inviato
067000050418      *----------------------------------------------------*
067100151211     C     Wrk_filler    Begsr
067200050418      *
067300151211     c                   clear                   dsDP3FLR
067400161125     c                   eval      DP3§PARCEL = ParcelNr
067500161125     c                   eval      DP3§SCODE  = ServiceCODE
067600161125     c                   eval      DP3§DATE   = Data_Evento
067700161125     c                   eval      DP3§TIME   = Ora__Evento
067800161128     c                   eval      DP3§SCAN   = Scan_CODE
067900151211     c                   eval      DP3§ADDC1  = zcod(1)
068000151211     c                   eval      DP3§ADDC2  = zcod(2)
068100151211     c                   eval      DP3§ADDC3  = zcod(3)
068200151211     c                   eval      DP3§PESO   = %editc(Peso_in_10gr:'X')
068300171017     c                   move      Sender_Depot  DP3§DEPOT
068400171027     c                   movel     Sender_Depot  DP3§BU
068500161125     c                   eval      DP3§NAZISO = CountryCodISO
068600161125     c                   eval      DP3§CAP    = CAP_Dest
068700171017     c                   move      Delivery_DepotDP3§ROUTE
068800171017     c****               eval      DP3§GEOIDN = GEOIDN
068900151211     c                   eval      DP3§DIST   = DIST
069000151211     c                   eval      DP3§STOP   = SSTOP
069100151211     c                   eval      DP3§QUALIF = qualifica
069200151211     c                   eval      DP3§TIPSCA = 'B'
069300151211     c                   if          DP3CEV ='ORM'
069400151211     c                   eval      DP3§TIPSCA = 'O'
069500151211     C                   end
069600151211     c                   eval      DP3flr = dsDP3FLR
069700151211      *
069800151211     C                   ENDSR
069900050418      *----------------------------------------------------*
070000050418      *   DEFINIZIONE CHIAVI                               *
070100050418      *----------------------------------------------------*
070200050418     C     *INZSR        BEGSR
070300050418      *
070400050418     c     *ENTRY        PLIST
070500050418     c                   PARM                    KPJBA
070600050418      *
070700050418     C     KTBE          KLIST
070800050418     C                   KFLD                    KCODE
070900050418     C                   KFLD                    KKE1
071000050418      *
071100050418     C     KTAB          KLIST
071200050418     C                   KFLD                    CODUT
071300050418     C                   KFLD                    KCOD
071400060619      *
071500150826     C     KTABel        KLIST
071600060619     C                   KFLD                    TBLKUT
071700060619     C                   KFLD                    TBLCOD
071800060619     C                   KFLD                    TBLKEY
071900050418      *
072000050418     C     KDP3          KLIST
072100050418     C                   KFLD                    KDTR
072200050418     C                   KFLD                    KFTR
072300050418      *
072400050418     C     KSPE          KLIST
072500050418     C                   KFLD                    dp3AAS
072600050418     C                   KFLD                    dp3LNP
072700050418     C                   KFLD                    dp3NRS
072800050418     C                   KFLD                    dp3NSP
072900050418      *
073000050418     C     KAR4          KLIST
073100050418     C                   KFLD                    dp3AAS
073200050418     C                   KFLD                    dp3LNP
073300050418     C                   KFLD                    dp3NRS
073400050418     C                   KFLD                    dp3NSP
073500050418     C                   KFLD                    KTRC
073600160502      *
073700160502     C     KAR5          KLIST
073800160502     C                   KFLD                    dp3AAS
073900160502     C                   KFLD                    dp3LNP
074000160502     C                   KFLD                    dp3NRS
074100160502     C                   KFLD                    dp3NSP
074200160502     C                   KFLD                    TipoAr5           3
074300050418      *
074400101126     C     Kgcp          KLIST
074500101126     C                   KFLD                    dp3AAS
074600101126     C                   KFLD                    dp3LNP
074700101126     C                   KFLD                    dp3NRS
074800101126     C                   KFLD                    dp3NSP
074900101126     C                   KFLD                    TRC0              2 0
075000101126     C                   z-add     0             TRC0
075100171017      *--
075200171017     C     BOLLA_dp3     KLIST
075300171017     C                   KFLD                    dp3AAS
075400171017     C                   KFLD                    dp3LNP
075500171017     C                   KFLD                    dp3NRS
075600171017     C                   KFLD                    dp3NSP
075700171023      *
075800171023     c     Kcdp4         klist
075900171023     c                   kfld                    CDPATB
076000171023     c                   kfld                    CDPVER
076100171023     c                   kfld                    CDPDSTR
076200050418      * Dati società
076300050418     C                   Z-ADD     1             CODUT
076400050418     C                   CALL      'X§PARUT'
076500050418     C                   PARM                    UT§DSE0F
076600050418     C                   MOVEL     REC80         CNCR80
076700050418      *
076800050418      *   EDPCEDxxx dove xxx è il Terminal
076900050418     C                   CLEAR                   Tibs36Ds
077000050418     c                   EVAL      I36ute = Knmus
077100050418     c                   EVAL      I36Tla = 'L'
077200050418     C                   CALL      'TIBS36R'
077300050418     C                   PARM                    Tibs36Ds
077400050418      *
077500050418      * LEggo AZORG per caricamento schiere P.O. DPD
077600050418     C                   clear                   XX
077700050418     C                   clear                   YY
077800050418     c     *loval        setll     azorg
077900050418     c                   read      azorg
078000050418      *
078100130910     C                   DOW       NOT %eof(Azorg01l)
078200130910      *
078300161129      * Decodifica P.O. BARTOLINI con codice DPD
078400050418     c                   If        ORGDE3 <> *blanks
078500050418     C                   add       1             xx
078600050418     C                   movel     ORGfil        POBAR(xx)
078700050418     C                   movel     ORGde3        PODPD(xx)
078800050418     C                   movel     ORGDE3        OG143
078900161129      * e se è un HUB DPD (190,191,192.....)
079000050418     c                   If        §ogNTW = 'DPD'
079100050418     C                   add       1             YY
079200161129     C                   movel     ORGfil        hubDPD(yy)
079300050418     c                   Endif
079400161129      *
079500050418     c                   Endif
079600050418      *
079700050418     c                   read      azorg
079800050418     c                   ENDDO
079900171017      *
080000171017      * -----------
080100171017      *  Cod.BRT -- B.U.
080200171017     C                   z-add     1             TBLKUT
080300171017     C                   movel     '3I'          TBLCOD
080400171017     C                   movel(p)  'DPD'         TBLKEY
080500171017     c     KTABel        chain     tabel00f
080600171017     c                   if        %Found(tabel00f)
080700171017     c                   movel     tbluni        ds3Idp
080800171017     c                   move      §3IBUCN       BU_brt            3 0
080900171017     c                   end
081000150826      *------------
081100150826     C** CARICAMENTO DATI VARI TASSAZIONE
081200150826     C                   Z-ADD     1             tblKUT
081300150826     C                   MOVEL     'QT'          tblcod
081400150826     C                   MOVEL(P)  '1'           tblKEY
081500150826     C                   CLEAR                   DSQT1
081600150826     C     KTABel        CHAIN     TABEL00F
081700150826     c                   if        %Found(TABEL00F)
081800150826     C                   MOVEL     TBLUNI        DSQT1
081900150826     c                   end
082000150826      *------------
082100050418      *
082200050418      * Carico Continuazione Tabella Causali Eventi "2Z"
082300050418     C                   clear                   XX
082400050418     C                   MOVE      '2Z'          KCOD
082500050418      *
082600050418     C     ktab          SETLL     TABEL00F
082700050418     C     ktab          READE     TABEL00F
082800130910     C                   DOW       NOT %eof(tabel00f)
082900050418     c                   movel     tbluni        ds2z
083000050418     c                   IF        §2zevd <> *blanks  and  §2zced <> *blanks
083100050418     C                   add       1             XX
083200050418     C                   movel     TBLKEY        K2Z(xx)
083300050418     C                   movel     TBLUNI        T2Z(xx)
083400050418     C                   ENDIF
083500050418     C     ktab          reade     TABEL00F
083600050418     C                   ENDDO
083700050418      *
083800050418      * carico schiera dei codici bolla di recupero
083900050418     C                   clear                   XX
084000050418     C                   move      '3A'          KCOD
084100050418     C     ktab          SETLL     TABEL00F
084200050418     C     ktab          READE     TABEL00F
084300130910     C                   DOW       NOT %eof(tabel00f)
084400050418     c                   movel     tbluni        ds3a
084500050418     c                   IF        §3arbl =  'R'
084600050418     C                   add       1             XX
084700050418     C                   movel     TBLKEY        s3a(xx)
084800050418     c                   ENDIF
084900050418     C     ktab          reade     TABEL00F
085000050418     C                   ENDDO
085100050418      *
085200060727      * carico schiera delle spunte Anoamlie (3E)
085300060727     C                   clear                   XX
085400060727     C                   move      '3E'          KCOD
085500060727     C     ktab          SETLL     TABEL00F
085600060727     C     ktab          READE     TABEL00F
085700130910     C                   DOW       NOT %eof(tabel00f)
085800060727     c                   movel     tbluni        ds3e
085900060727      * non deve caricare i rientri
086000170516      * e le segnalazioni
086100170516     c                   IF        §3eFTA <> 'R' and §3eFTA <> 'S'
086200060727     C                   add       1             XX
086300060727     C                   movel     TBLKEY        s3e(xx)
086400060727     c                   ENDIF
086500060727      *
086600060727     C     ktab          reade     TABEL00F
086700060727     C                   ENDDO
086800060727      *
086900050418      * Caricamento Tabella PESI DPD
087000050418     c                   eval      kcode = 'DPP'
087100050418     c                   eval      kke1 =  'PESO01'
087200050418     c     Ktbe          chain     tntbe000
087300050418      *
087400130910     c                   IF        %found(tntbe01l) and TBEatb = *BLANKS
087500050418     c                   movel     TBEuni        DDPP
087600050418     c                   movel     §DPPpkc       Wpkc01
087700050418     c                   ENDIF
087800050418      *
087900171024      *  Peso Piccolo
088000050418     c                   eval      kke1 =  'PESO02'
088100050418     c     Ktbe          chain     tntbe000
088200171024     c                   clear                   hg_PPiccolo       7 0
088300130910     c                   IF        %found(tntbe01l) and TBEatb = *BLANKS
088400050418     c                   movel     TBEuni        DDPP
088500050418     c                   movel     §DPPpkc       Wpkc02
088600171024     c                   move      §DPPpkb       hg_PPiccolo       7 0
088700050418     c                   ENDIF
088800050418      *
088900050418      * Carico Tabella Eventi DPD
089000050418     C                   clear                   XX
089100050418     c     'EVD'         chain     tntbe000                           31
089200050418      *
089300050418     c                   DOW       *in31 = *off
089400050418      * Se il S.I. è indicato deve essere uguale al mio
089500050418     c                   IF        TBEatb = *BLANKS  and
089600050418     c                              (TBEsif = KNSIF or TBEsif = *blanks)
089700050418     c                   add       1             xx
089800050418     c                   movel     TBEke1        KEVD(XX)
089900050418     c                   movel     TBEuni        EVD(XX)
090000050418     c                   ENDIF
090100050418      *
090200050418     c     'EVD'         reade     tntbe000                               31
090300050418     c                   ENDDO
090400050418      *
090500050418      * DATA DEL GIORNO + ORA
090600050418     C                   TIME                    W0140
090700050418     C                   MOVE      W0140         W0080
090800050418     C     *eur          MOVEL     W0080         Wdata
090900050418     C     *iso          MOVEL     Wdata         W0080
091000050418     c                   MOVEL     W0080         Wdataoggi
091100050418     c                   MOVEL     W0080         Wdata80
091200050418     C                   MOVEL     W0140         WORA
091300170407      *
091400170407      *-----------------------------
091500170412      *   dal 12/4/2017 solo i TTEVENTS
091600170407     c                   eval      File_vers ='T'
091700171017
091800170407      *-----------------------------
091900050418      * calcola il limite di 90 gg. oltre i quali la DpD non ha bisogno dei records
092000050418     C     Wdata         SUBDUR    90:*Days      W0m90
092100050418     c     *iso          MOVEL     W0m90         W008M
092200050418     c                   MOVEL     W008M         Wdata_90
092300161125      **
092400050418     C     endinz        ENDSR
092500081015      **?------------------------------------------------------------------ */
092600081015      *   compatta il CAP eliminando dei blank all'interno es.GB
092700081015      **?------------------------------------------------------------------ */
092800081015     c     Compatta_CAP  begsr
092900081015      *
093000081015     c                   z-add     0             to                3 0
093100081015     c                   do        9             from              3 0
093200081015     c                   if        skCAP(from) = *blank
093300081015     c                   if          to = 0
093400081015     c                   z-add     from          to
093500081015     c                   end
093600081015     c                   else
093700081015     c                   if          to > 0
093800081015     c                   move      skCAP(from)   skCAP(to)
093900081015     c                   move      *blank        skCAP(from)
094000081015     c                   add       1             to
094100081015     c                   end
094200081015     c                   end
094300081015     c                   enddo
094400081015
094500081015     c                   endsr
094600161125      *----------------------------------------------------*
094700161125      *   TTEVENTS  TIVGD                                  *
094800161125      *----------------------------------------------------*
094900161125     C     Wrk_TTEVENTS  Begsr
095000161125      *
095100161125      **  TIVGD APRE e CHIUDE
095200161125     c                   if        VGDTIP_$E ='APRE  '
095300161125      **                                        ===========
095400161125     c                   clear                   VGDTIP_$E
095500161125      **                                        ===========
095600161125      *  istruzioni apertura blocco scrittura TIVGD
095700161125     C                   clear                   trul47ds
095800161125     C                   eval      d47opz  = 'I'
095900161125     C                   eval      d47tip  = '$E'
096000161125     C                   eval      d47lck  = 'N'
096100161125     C                   eval      d47chkj = 'N'
096200161125     C                   eval      d47pgm  = 'FIEU16R'
096300161125     C                   call      'TRUL47R'
096400161125     C                   parm                    trul47ds
096500161125      **
096600161125     c                   elseIF    VGDTIP_$E ='CHIUDE'
096700161125      **
096800161125     c                   exsr      FINE_TTEVENT                                 #FINE MSG
096900161125     C* Infine elimino il blocco elaborazione TIVGD
097000161125     C                   clear                   trul47ds
097100161125     C                   eval      d47opz  = 'F'
097200161125     C                   eval      d47tip  = '$E'
097300161125     C                   call      'TRUL47R'
097400161125     C                   parm                    trul47ds
097500161125      **
097600161125     c                   end
097700161125      **
097800161125      **  scrive il  TTEVENTS
097900161125      * dettaglio TIVGD                     ===========
098000161125     c                   IF        VGDTIP_$E <>'CHIUDE'
098100161125      ** Solo la 1°Volta                    ===========
098200161125     c                   if         TESTA_TTE ='SI'
098300161125     c                   clear                   TESTA_TTE                      Solo la 1°volta
098400161125     c                   exsr      TESTA_TTEVENT                                Testata MSG
098500161125     c                   end
098600161125      *
098700161125      *  riga di dettaglio status SCAN
098800161125      *                           ---------------
098900161125     c                   exsr      TTEVENTS_dati                                riga di TTEVENTS
099000161125      *                           ---------------
099100161125     c                   endIF
099200161125      *
099300161125     c                   endsr
099400130705      *-------------------------------------------------------------------------
099500161125     c     TESTA_TTEVENT BEGsr
099600161125      *-------------------------------------------------------------------------
099700161125      *  6 righe di testata
099800161125      *
099900170410      **   #FILE;GEODATA_STATUS_0230820_N00_D20161016T082554;
100000161125     c                   clear                   dati_TTE
100100161129     c                   EVAL      dati_TTE = %trim(HEAD(1)) +
100200161128     c                              wdataoggi + 'T' + wora + ';'
100300161125     c                   exsr      tivgd_$E
100400161125      *
100500161125      **   #ENCODING;ISO-8859-1;
100600161125     c                   clear                   dati_TTE
100700161125     c                   EVAL      dati_TTE = HEAD(2)
100800161125     c                   exsr      tivgd_$E
100900161125      *
101000170410      **   #VERSION;03.10;
101100161125     c                   clear                   dati_TTE
101200161125     c                   EVAL      dati_TTE = HEAD(3)
101300161125     c                   exsr      tivgd_$E
101400161125      *
101500161125      **   #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
101600161125     c                   clear                   dati_TTE
101700161125     c                   EVAL      dati_TTE = HEAD(4)
101800161125     c                   exsr      tivgd_$E
101900161125      *
102000161125      **   #DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;........
102100161125     c                   clear                   dati_TTE
102200161125     c                   EVAL      dati_TTE = %trim(HEAD(5)) +
102300161125     c                                        %trim(HEAD(6)) +
102400161125     c                                        %trim(HEAD(7)) +
102500161125     c                                        %trim(HEAD(8)) +
102600161125     c                                        %trim(HEAD(9)) +
102700161125     c                                        %trim(HEAD(10)) +
102800161125     c                                        %trim(HEAD(11)) +
102900161130     c                                        %trim(HEAD(12))
103000161125     c                   exsr      tivgd_$E
103100161125      *
103200170410      **   HEADER;03.10;STATUS;
103300161125     c                   clear                   dati_TTE
103400161125     c                   EVAL      dati_TTE = HEAD(14)
103500161125     c                   exsr      tivgd_$E
103600161125      *
103700161125     c                   endsr
103800161125      *-------------------------------------------------------------------------
103900161125     c     FINE_TTEVENT  BEGsr
104000161125      *-------------------------------------------------------------------------
104100161125     c                   clear                   dati_TTE
104200161125     c                   movel     '#END;'       dati_TTE
104300161125     c                   exsr      tivgd_$E
104400161125      *
104500161125     c                   endsr
104600161125      *-------------------------------------------------------------------------
104700161125     C     TTEVENTS_Dati Begsr
104800161125      *-------------------------------------------------------------------------
104900161128     c                   eval      non_flaggare_DP3 = ' '
105000161128     c                   eval        ContInfo1      = ' '
105100161128     c                   eval        ContInfo2      = ' '
105200170502     c     ';':' '       xlate     BLPLOD        BLPLOD
105300170502     c     ';':' '       xlate     BLPRMA        BLPRMA
105400161128      *
105500161128     c                   Select
105600161128      * SCAN 01/02/05/10
105700161129     c                   WHEN      Scan_code = '01' or Scan_code = '02'or
105800161129     c                             Scan_code = '05' or Scan_code = '10'
105900161128      * SCAN 03
106000161129     c                   WHEN      Scan_code = '03'
106100161128      * SCAN 06
106200161129     c                   WHEN      Scan_code = '06'
106300161128      * SCAN 04/08
106400161128      * SCAN 07/09/13/14
106500161129     c                   WHEN      Scan_code = '04' or Scan_code = '08' or
106600161129     c                             Scan_code = '13' or Scan_code = '14' or
106700161129     c                             Scan_code = '07' or Scan_code = '09'
106800161128      * SCAN 18
106900161129     c                   WHEN      Scan_code = '18'
107000161128      * solo x Import
107100161129     c                   IF        wIMPORT   ='S'
107200161128      * inviato ISOLA
107300161128     c                   IF         DP3CEV ='ISL'
107400161128     c                   eval        ContInfo1      = '00'
107500161128     c                   eval        ContInfo2      = 'ISLAND'
107600161128     c                   ENDIF
107700161128      *
107800161128      * inviato ALERT con mail o SMS
107900161128     c                   IF        (DP3CEV ='S25' or DP3CEV ='E25') or
108000161128     c                             (DP3CEV ='S44' or DP3CEV ='E44' or
108100161128     c                              DP3CEV ='X44')
108200161128     c                   Exsr      Alert_18_44
108300161128      * non c'era il Predict
108400161128     c                   if            predict_invio = *blank
108500161128     c                   eval      non_flaggare_DP3 = 'N'
108600161128     c                   clear                   ContInfo1
108700161128     c                   clear                   ContInfo2
108800161130      ** NON deve inviare lo SCAN 18 se mancano i dati
108900161130     c                   LEAVEsr
109000161130     c                   end
109100161128     c                   ENDIF
109200161128      *
109300161128     c                   END
109400161128      *
109500161128     c                   ENDSL
109600161128      *
109700161128      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
109800170410      *  particolarità sullo SCAN 13 vogliono il PODIMAGE7 con tutti zeri
109900170410      *  altrimenti è sempre vuoto.
110000170410      *
110100161128     c                   clear                   dati_TTE
110200170410     c                   clear                   podImage7
110300170410      * solo sulla consegna '0000000'
110400170410     c                   if        Scan_CODE = '13'
110500170410     c                   move      *all'0'       podImage7
110600170410     c                   end
110700161128      *
110800161125     c                   EVAL      dati_TTE = %trim(HEAD(15))  + ';' +            0 TTEVENTS;2
110900161125     c                                        %trim(ParcelNr)  + ';' +            1 PARCELNUMBER
111000170410     c                                            'DE06'       + ';' +            2 STATUSTYPE
111100161125     c                                                           ';' +            3 ORIGINPARCELNUMB
111200161125     c                                     %trim(ServiceCODE)  + ';' +            4 SERVICECODE
111300170410     c                                     %trim(CAP_Dest)     + ';' +            5 DZIPCODE
111400170410     c                                     %trim(CountryCodISO)+ ';' +            6 DCOUNTRYCODE
111500170410     c                                     %trim(Scan_CODE)    + ';' +            7 SCANCODE
111600170410     c                                     %trim(zcod(1))      + ';' +            8 REASONCODE
111700170410     c                                     %trim(zcod(2))      + ';' +            9 REASONCODE2
111800170410     c                                     %trim(zcod(3))      + ';' +           10 REASONCODE3
111900171017     c                                           Sender_Depot  + ';' +           11 DEPOT
112000170410     c                                                           ';' +           12 AGENTLOCATCODE
112100170410     c                                                           ';' +           13 AGENTLOCATTYPE
112200170410     c                                                           ';' +           14 AGENTPARCELNUMBE
112300170410     c                                     %trim(BLPNZD)       + ';' +           15 COUNTRY
112400170410     c                                     %trim(BLPLOD)       + ';' +           16 CITY
112500170410     c                                     %trim(Data_Evento)        +           17 STATUSDATETIME
112600170410     c                                     %trim(HMS__Evento)  + ';' +           18
112700170410     c                                                           ';' +           19 TIMEZONE
112800161128     c                                            wdataoggi          +           20 MEMDATETIME
112900170410     c                                            wora         + ';' +           21
113000171017     c                                       Delivery_Depot    + ';' +           22 DDEPOT
113100170410     c                                     '001'               + ';' +           23 DELIVERYTOUR
113200170410     c                             %trim(%editc(Peso_in_10gr:'4')) + ';' +       24 MEASUREDWEIGHT
113300170410     c                                                           ';' +           25 DIMENSION
113400170410     c                                     %trim(BLPRMA)       + ';' +           26 PARCELREF
113500170410     c                                                           ';' +           27 PARCELREF2
113600170410     c                                   %trim(PodImage7)      + ';' +           28 PODIMAGEREF
113700170410     c                                   %trim(Firma_Consegna) + ';' +           23 RNAME
113800170410     c                                                           ';' +           38 TRDPNAME
113900170410     c                                         '001'           + ';' +           39 AREA
114000170410     c                                         '001'           + ';' +           30 STOP
114100170410     c                                     %trim(ContInfo1)    + ';' +           41 CONTAINERINFO1
114200170410     c                                     %trim(ContInfo2)    + ';' +           42 CONTAINERINFO2
114300170410     c                                                           ';' +           43 SHIPMENTREF
114400170410     c                                                           ';' +           44 SUPPLIERREF
114500170410     c                                                           ';' +           45 CONTAINERREF
114600170410     c                                                           ';' +           46 CONTAINERTYPE
114700170410     c                                                           ';' +           47 ACTORID
114800170410     c                                                           ';' +           48 OPERATIVERID
114900170410     c                                                           ';' +           49 PICKUPORDERNUMBE
115000170410     c                                                           ';' +           40 GPSLAT
115100170410     c                                                           ';' +           51 GPSLONG
115200170410     c                                                           ';' +           52 RETURNPARCELNUMB
115300170410     c                                                           ';'             53 COMPINFO1
115400161125      *
115500161125     c                   exsr      tivgd_$E
115600161125      *
115700161125      *
115800161125     c                   endsr
115900161125      *-------------------------------------------------------------------------
116000161125     C     tivgd_$E      Begsr
116100161125      *-------------------------------------------------------------------------
116200161125     c                   clear                   tivgd000
116300161125     c                   eval      vgddta = %TrimR(dati_TTE)
116400161125     c                   eval      vgdtip = '$E'
116500161125     c                   eval      vgdksu = '0DPD0OUT'
116600161125     c                   eval      vgdtsc = 'WW'
116700161125     c                   eval      vgdpgm = 'FIEU16R'
116800161125     c                   eval      vgddat = W0080
116900161125     C                   WRITE     tivgd000
117000161125      *
117100161125     C                   Endsr
117200161128      *-------------------------------------------------------------------------
117300161128      *   Se inviato il PREDICT (Alert) SCAN 18(44)        *
117400161128      *-------------------------------------------------------------------------
117500161128     C     Alert_18_44   Begsr
117600161128      *
117700161130     c                   clear                   predict_invio     1
117800161128     c                   eval        ContInfo1      = '01'
117900161130     c                   eval        ContInfo2      = '44:1'
118000161128      *
118100161128      * aggancia FIAR500R per le decodifiche da passare
118200161128      *   per poi generare  il filler con lo scan 18
118300161128     c                   movel     'EMD'         TipoAr5
118400161128     c     Kar5          chain     fiar501l
118500161128     c                   IF        %found(fiar501l)
118600161128     c                   eval        dar5EMD = AR5UNI
118700170719     C     ';':' '       XLATE     §AR5EML       §AR5EML                            *No puntoevirg
118800170719     C     ';':' '       XLATE     §AR5TEL       §AR5TEL                            *No puntoevirg
118900170719      *
119000161130     c                   eval        ContInfo2 = %trim(ContInfo2) + ':' +
119100161130     c                                        %trim(§AR5EML)      + ':' +
119200161130     c                                        %trim(§AR5TEL)      + ':'
119300161128      *  EMAIL/SMS
119400161128     c                   eval          predict_invio ='S'
119500161128     c                   move      '3'           tipopredict       1
119600161128     c                   if          §AR5EML<> *blank and §AR5TEL = *blank
119700161128     c                   move      '2'           tipopredict
119800161128     c                   elseif      §AR5EML = *blank and §AR5TEL<> *blank
119900161128     c                   move      '1'           tipopredict
120000161128     c                   elseif      §AR5EML = *blank and §AR5TEL = *blank
120100161128     c                   clear                   predict_invio     1
120200161128     c                   leavesr
120300161128     c                   end
120400161128      *
120500161128      *   durante il MIC  (DLO)
120600161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
120700161128     c                                    tipopredict +  ':2:DLO:'
120800161128      *   DATA
120900161128     c                   if          §AR5DTO<> *blank and §AR5DOS = *blank
121000161128     c                                  or
121100161128     c                               §AR5DTO > §AR5DOS
121200161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
121300161128     c                                 %subst(§AR5DTO:1:8) + ':' +
121400161128     c                                 %subst(§AR5DTO:9:4) + ':'
121500161128      *
121600161128     c                   elseif      §AR5DOS<> *blank and §AR5DTO = *blank
121700161128     c                                  or
121800161128     c                               §AR5DOS > §AR5DTO
121900161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
122000161128     c                                 %subst(§AR5DOS:1:8) + ':' +
122100161128     c                                 %subst(§AR5DOS:9:4) + ':'
122200161128     c                   end
122300161128      *    TIME
122400161128     c                   if        §AR5OPCD <> *blank
122500161128     c                                  and §AR5OPCA <> *blank
122600161128      *
122700161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
122800161128     c                               '1:' + §AR5OPCD +':'+ §AR5OPCA +'::'
122900161128      *
123000161128     c                   elseif    §AR5OPCD = *blank
123100161128     c                                  and §AR5OPCA = *blank
123200161128      *
123300161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
123400161128     c                                 '0:1900:::'
123500161128      *
123600161128     C                   End
123700161128      *------
123800161128     C                   END
123900161128      *
124000161128     C                   Endsr
124100161128      *-------------------------------------------------------------------------
124200161125** (HEAD)
124300170410#FILE;GEODATA_STATUS_0230820_N00_D                                                 01<
124400161125#ENCODING;ISO-8859-1;                                                              02
124500170410#VERSION;03.10;                                                                    03<
124600161125#DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;                                       04
124700161130#DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;         05|
124800170410SERVICECODE;DZIPCODE;DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;                 06||
124900161130REASONCODE3;DEPOT;AGENTLOCATIONCODE;AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;       07| |
125000161130COUNTRY;CITY;STATUSDATETIME;TIMEZONE;MEMDATETIME;DDEPOT;DELIVERYTOUR;              08|  |
125100170410MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;PODIMAGEREF;RNAME;TRDPNAME;          09|  |
125200161130AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;      10| |
125300161130CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;               11||
125400161130RETURNPARCELNUMBER;COMPINFO1;;                                                     12|
125500161130                                                                                   13
125600170410HEADER;03.10;STATUS;                                                               14<
125700161125TTEVENTS;2                                                                         15<----
