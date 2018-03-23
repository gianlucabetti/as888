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
014800171103     c                   clear                   conta_eventi      7 0
014900171103      **
015000060609     c     KDP3          setll     tIDP3000
015100060609     c     KDP3          readE     tIDP3000
015200060609     C                   DOW       NOT %eof(tIDP303L)
015300161129
015400050418      *   elaboro se data evento impostata
015500161129      *   IMPOSTA subito lo SCAN
015600050418     C                   IF        DP3dev > *zeros
015700161129     c                   movel     dp3evd        Scan_CODE
015800161129
015900161129      *   se lo SCAN è in tabella EVD
016000161129     c                   eval      w015A = SCAN_CODE
016100050418     c                   z-add     1             XX
016200050418     c     w015a         lookup    KEVD(xx)                               31
016300161129      ** quindi Considera se è uno SCAN
016400161129      ** oppure x gli ORM Italia su Italia (detti Italy Domestic) LNP e LNA Italia
016500161129     c                   IF        *IN31 = *on or Dp3CEV = 'ORM'
016600050418     C                   movel     EVD(XX)       devd
016700130705
016800050418     c                   IF        §evditr = 'S'  or  §evdetr = 'S'
016900080603     c                             or Dp3CEV = 'ORM'
017000050418
017100050418      *   Aggancio file Partenza o Arrivi per reperire dati
017200161129     c                   clear                   qualifica        10
017300050418     c                   clear                   Welabora
017400161125     c                   clear                   ParcelNr
017500161125     c                   clear                   ServiceCODE
017600060727     c                   clear                   Wanomalia
017700161128     c                   clear                   Delivery_Depot
017800050418     c                   clear                   dsFNBLP
017900050418     c                   clear                   dsFNARB
018000150605     c                   clear                   Peso_in_BLT
018100161128     c                   clear                   Firma_consegna
018200050418      *
018300161129      * Se trovato con LNP    --> è una spedizione Import DPD
018400161129     C                   z-add     1             YY
018500161129     c     DP3lnp        lookup    hubDPD(YY)                             33
018600161129     c                   eval      wIMPORT  = 'N'
018700060328     c   33              eval      wIMPORT  = 'S'
018800050418      *
018900050418     C                   SELECT
019000050418      *
019100161129      ** se è un evento di import da trattare oppure
019200161129      ** è un evento x gli ORM Italia su Italia (detti Italy Domestic)
019300161129     c                   WHEN      (wIMPORT  = 'S' and §evditr = 'S')
019400080603     c                             or Dp3CEV = 'ORM'
019500130705      *
019600130705     c     Kspe          chain     FNBLP01L
019700130705     c                   if        %found(FNBLP01L)
019800050418     c                   eval      Welabora = 'S'
019900161129      *
020000161129      * Cerca la Firma di Consegna della spedizione
020100161129     c                   if        SCAN_code = '13'
020200161129     c                   move      '1'           ktrc
020300161129     c     Kar4          chain     fiar401l
020400161129     c                   if        %found(fiar401l) and ar4not <> *blank
020500170502     c     ';':' '       xlate     ar4not        Firma_Consegna
020600161129     c                   end
020700161129     c                   endif
020800161129      *
020900060727      * ? controllo su FNBLT se esiste un'anomalia (APERTO, BAGNATO....SCONDIZIONATO)
021000060727      * ? da segnalare a DPD onde non prendersi poi la responsabilità del DANNO.
021100061011      * ?  solo se non è stato dato già x consegnato
021200150323     c                   clear                   Peso_in_BLT
021300061011     c                   if        BLPdcm = 0
021400060727     c     Kspe          chain     FNBLT01L
021500150323     c                   if        %Found(FNBLT01L) and bltPUC > 0
021600150323      * ?  si deve memorizzare anche l'eventuale PESO da CML del parcel
021700150323      * ?   in decigrammi così elimina i centigrammi arrotondando.
021800150323     c     BLTpuc        mult(h)   100           Peso_in_BLT
021900150323     c                   end
022000150323      * ?   ci sono SPUNTE?
022100060727     c                   if        %Found(FNBLT01L) and bltCAN <> *blank
022200060727      **? controlla se presente fra le Spunte da segnalare a DPD
022300060727     c                   z-add     1             xx
022400060727     c     BLtcan        lookup    S3e(xx)                                31
022500060727     c   31              move      S3e(xx)       Wanomalia
022600060727     c                   endif
022700161128      **
022800061011     c                   endif
022900050418     c                   endif
023000050418      *
023100161129      * Se non trovato allora --> è una spedizione export DPD
023200060328     c                   WHEN      wIMPORT  <> 'S'    and  §evdetr = 'S'
023300161129     c                   eval      Welabora = 'S'
023400130704     c     Kspe          chain     FNARB01L
023500130704     c                   if        %found(FNARB01L)
023600060613     c                   endif
023700060619      *
023800050418     C                   ENDSL
023900080603      *-----------*
024000060619      * rek 'I'
024100171017     c     BOLLA_dp3     chain     fipnd01l
024200161129      *
024300171023      *  Deve impostare il DEPOT di CONSEGNA in funzione del verso della spedizione
024400171023      *   oppure se si tratta di un ORM ITALIA-ITALIA quindi non presente sul FIPND.
024500171017     c                   if        %found(fipnd01l)
024600171023      *  se IMPORT
024700171023     c                   if        pndDPE  = 'DI4'
024800171017     c                   movel     pndIDA7       Delivery_depot
024900171023      *  se EXPORT vecchia etichetta
025000171023     c                   elseif    pndDPE  = 'DP4'
025100171023     c                   eval      DPNDDP4 = pndETIU
025200171023      * trascodifico la B.U.
025300171023      * recupero  versione di cappario
025400171023     C                   clear                   ISID1DRI
025500171023     c                   z-add     wdata80       ISID1DRI
025600171023     C                   clear                   OSID1VER
025700171023     C                   clear                   OSID1VERD
025800171023     C                   clear                   OSID1DDE
025900171023     C                   clear                   OSID1DSC
026000171023     C                   clear                   OSID1BCID
026100171023     c                   call      'TISID1R'
026200171023     C                   PARM                    ISID1DRI
026300171023     C                   PARM                    OSID1VER
026400171023     C                   PARM                    OSID1VERD
026500171023     C                   PARM                    OSID1DDE
026600171023     C                   PARM                    OSID1DSC
026700171023     C                   PARM                    OSID1BCID
026800171023      * se TROVATA VERSIONE CAPPARIO
026900171023      * prova a decodificare il DEPOT con il nuovo cappario per reperire la BU
027000171023     c                   if        osid1ver > 0
027100171023     c                   move      *blank        CDPATB
027200171023     c                   z-add     OSID1VER      CDPVER
027300171023     c                   movel     §b4IDA        CDPDSTR
027400171023     c     Kcdp4         chain     dpcdp12i
027500171023      *   il Depot con la B.U.
027600171023     c                   if        %Found(dpcdp12i)
027700171023     c                   eval      Delivery_depot = CDPDPTN
027800171023     c                   end
027900171023     c                   end
028000171023      ** ------
028100171023      *  se EXPORT  nuova  etichetta
028200171023     c                   elseif    pndDPE  = 'DP5'
028300171023     c                   eval      DPNDDP5 = pndETIU
028400171023     c                   movel     §PNDBUCA      Delivery_depot
028500171023     c                   move      §pndIDAst     Delivery_depot
028600171023     c                   end
028700171023
028800171017     c                   movel     pndISC        ServiceCODE
028900171017     c                   eval      ParcelNr = pndIPN
029000171017
029100171023      *   Se non trovato il tipo record ("I")
029200080603     c                   else
029300171023      *
029400161129      **  Se è un evento x gli ORM Italia su Italia (detti Italy Domestic)
029500171023      *   il Parcel è fittizio ed è stato staccato durante la generazione
029600171023      *   degli SCAN x l'ORM. Quindi non esiste sul FIAR4 poichè si tratta
029700171023      *   di una bolla Italia.
029800161128      **   Convertitore
029900080603     c                   if        Dp3CEV = 'ORM'
030000161129     c                   eval      ParcelNr = Dp3PRN
030100171023      **
030200171023      *   Fine codifica ORM Italy Domestic.
030300171023      *    imposta il Depot di delivery direttamente dagli SCAN
030400171023     c                   z-add     1             xx
030500171023     c     blpLNA        lookup    pobar(xx)                              31
030600171023     C   31              movel     POdpd(xx)     OG143
030700171023     c   31              move      §ogdp1        Delivery_Depot
030800171023     c   31              movel     BU_brt        Delivery_Depot
030900080603     c                   endif
031000080603      *
031100080603     c                   endif
031200161129      *---------
031300161129      *  se il parcel non era presente su DP3  -->  AGGIORNA il DP3 con il PARCEL
031400161129     c                   if        dp3prn = *blank and ParcelNr <> *blank
031500161125     c                   eval        dp3prn = ParcelNr
031600160502     c                   endif
031700130705      *----------------------------------------------------------
031800171023      *   Flagga il TIDP3 di Lettura .
031900060612      * Se ho trovato i file proseguo altrimenti aggiorno file eventi
032000160502     c                   IF        Welabora <> 'S'
032100050418     c                   eval      DP3ftr = 'T'
032200050418     C                   movel     99990101      DP3dtr
032300161129     C                   eval      DP3flr = 'NON ELABORARE - da EVD'
032400060609     c                   update    tidp3000
032500050418     C                   ELSE
032600050418      *
032700050418      *   Se bolla DPD x DPD non trasmetto dati e aggiorno File Eventi
032800161129     c     DP3lnp        lookup    hubDPD                                 31
032900161129     c   31BLPlna        lookup    hubDPD                                 31
033000161129     C                   IF        *in31
033100050418     c                   eval      DP3ftr = 'T'
033200050418     C                   movel     99990102      DP3dtr
033300161129     C                   eval      DP3flr = 'DA DPD A DPD - Errata'
033400060609     c                   update    tidp3000
033500050418     c                   ELSE
033600050418      *
033700050418      *   Se è una bolla recupero non trasmetto  dati
033800050418     c     BLPcbo        lookup    S3A                                    31
033900050418     C                   IF        *in31 = *on
034000050418     c                   eval      DP3ftr = 'T'
034100050418     C                   movel     99990103      DP3dtr
034200161129     C                   eval      DP3flr = 'BOLLA RECUPERO da NON trasm.'
034300060609     c                   update    tidp3000
034400050418     c                   ELSE
034500050418      *
034600171106      *   serve per mandare max 30000 SCAN e non di più altrimenti
034700171103      *    il server FTP non riesce ad elaborare il file creando problemi
034800171103      *   poichè non INVIA gli SCAN a DPD e le spedizioni non ci vengono
034900171103      *    pagate nel clearing.
035000171103      *
035100161129      *   Se ho trovato i file elaboro -----*
035200050418     C                   exsr      elabora
035300161129      *                          *----------*
035400171103      *
035500171103      *  incrementa il conteggio delle righe inviate
035600171103     c                   eval       conta_eventi = conta_eventi + 1
035700171103      *
035800171103      *  max.32500 righe per un limite sul server
035900171103      *   Le rimanenti verranno inviate al giro successivo.
036000171106     c                   if         conta_eventi = 30000
036100171103     c                   LEAVE
036200171103     c                   end
036300171103      *
036400050418     C                   ENDIF
036500050418     C                   ENDIF
036600050418     C                   ENDIF
036700050418      *  ----------------------
036800050418     C                   ENDIF
036900050418     C                   ENDIF
037000050418      *
037100161129     C                   ENDIF
037200060609     c     KDP3          reade     tIDP3000
037300050418     c                   ENDDO
037400160502      * ____________________________________________________________
037500161125     C* chiude il TIVGD se era stato aperto
037600161125     c                   if        VGDTIP_$E = *blank  AND
037700161125     c                             (File_vers ='E' or File_vers ='T')
037800161125     c                   eval      VGDTIP_$E = 'CHIUDE'
037900161125     c                   exsr      Wrk_TTEVENTS
038000161125     c                   end
038100161125     C*
038200050418     c                   EVAL      *INLR = *ON
038300050418      *----------------------------------------------------*
038400050418      *   ELABORO RECORD VALIDO                            *
038500050418      *----------------------------------------------------*
038600050418     C     ELABORA       BEGSR
038700060103      *
038800060103      * ?Riportato qui il controllo presente sul vecchio FIEU25R ora sostituito
038900150709      * ? dal giro del Manifest TRTC83R12.
039000060103      * ?Non devono essere inviati SCAN di tipo 05 - 06 - 10 con date superiori
039100060103      * ? alla data del giorno -> altrimenti creano problemi a DPD.
039200060103      * ? ******* Manda a FINE routine x evitare di inviare lo SCAN che manderà
039300060103      * ? ******* in un giro successivo.
039400161129     c                   If        Scan_CODE = '05' or Scan_CODE = '06' or
039500161129     c                             Scan_CODE = '10' or
039600161129     c                             (SCAN_CODE = '18' and
039700161129     c                               (dp3CEV='S25' or dp3CEV='E25'))
039800160518      * ? ** Adesso aggiungiamo l'ALERT **
039900160518      * xchè deve essere inviato con la stessa data del MIC o della consegna.
040000160518      *   ...quindi viene scritto nel TIDP300 con una data Futura in attesa del MIC.
040100160518      *   Se data FUTURA
040200161129      *    e non è x un ORM
040300060103     c                   If        DP3dev > wdata80
040400161129     c                               and
040500161129     c                             DP3CEV <> 'ORM'
040600060103     c                   goto      salta_scan
040700080603     c                   endif
040800060103     c                   end
040900161129      *****-------------------------------    QUA se NON ha saltato
041000050418      * P.O. scan
041100050418     c                   z-add     1             xx
041200161125     c                   clear                   Sender_Depot
041300050418     c     dp3fle        lookup    pobar(xx)                              31
041400050418     C   31              movel     POdpd(xx)     OG143
041500050418      *
041600050418     C                   SELECT
041700050418     c                   WHEN      *in31 = *off
041800130705      *
041900080611      *  solo x scan Fissi degli Orm Italy domestic (LOCALI Italia su Italia)
042000080611     c                   WHEN      DP3CEV = 'ORM'
042100161125     c                   move      §ogdp1        Sender_Depot
042200171017     c                   movel     BU_brt        Sender_Depot
042300050418      *
042400060328     c                   WHEN      wIMPORT  ='S'    and  §evdige = 'H'
042500130705     c                                or
042600130705     c                             wIMPORT  <> 'S'  and  §evdege = 'H'
042700161125     c                   move      §oghb1        Sender_Depot
042800171017     c                   movel     BU_brt        Sender_Depot
042900171017      *
043000060328     c                   WHEN      wIMPORT  ='S'    and  §evdige = 'D'
043100130705     c                                or
043200130705     c                             wIMPORT  <> 'S'  and  §evdege = 'D'
043300161125     c                   move      §ogdp1        Sender_Depot
043400171017     c                   movel     BU_brt        Sender_Depot
043500050418     c                   ENDSL
043600160519      *
043700160519      * Scan '03' in consegna segnalo consegna ISOLA
043800161129     c                   IF         SCAN_CODE ='18' and DP3cev = 'ISL'
043900161129     c                                or
044000161129     c                              SCAN_CODE = '03'  and  BLPfin = 'I'
044100160519     c                   movel     'ISLAND'      qualifica
044200161129     C                   ENDIF
044300050418      *
044400161125     c                   movel     dp3dev        Data_Evento
044500161125     c                   movel     dp3hev        Ora__Evento
044600161130     C                   move      *zeros        HMS__Evento
044700161130     c                   movel     dp3hev        HMS__Evento
044800050418      *
044900060619      *  Country Code ISO numerico x nuovi SCAN
045000161125     c                   clear                   CountryCodISO
045100060619     C                   z-add     1             TBLKUT
045200060619     C                   movel     '15'          TBLCOD
045300060619     C                   movel(p)  BLPnzd        TBLKEY
045400150826     c     KTABel        chain     tabel00f
045500060619     c                   if        %FOund(tabel00f)
045600060619     c                   movel     tbluni        ds15
045700161125     c                   move      §15CIE        CountryCodISO
045800060619     c                   end
045900050418      *
046000050418      * Codice Postale - Se diretta in GB forzo "01111"
046100050418      *                  Se trovo lettere non le scrivo
046200161125     c                   movel     blpCAD        CAP_Dest
046300081015      *
046400081015      * Se Gran Bretagna compatta il CAP togliendo i blank
046500081015     c                   if         BLPnzd = 'GB '
046600161125     c                   eval       CAPinSK = CAP_Dest
046700081015     c                   exsr      Compatta_CAP
046800161125     c                   eval       CAP_Dest = CAPinSK
046900081015     c                   endif
047000050418      *
047100081024      *   Controlla il CAP se scritto "EIRE" (Irlanda) deve essere sostituito
047200081024      *   con "0000001". (Richiesta di Villa da Meeting del 14/10/2008 con DPD.
047300161125     C                   if        CAP_Dest = 'EIRE'
047400161125     c                   eval      CAP_Dest = '0000001'
047500081024     c                   end
047600050418      *
047700130704      * se Export DPD  nella Route metto il Depot DPD da linea arrivo DPD
047800130704      *  Altrimenti quello sull'organigramma della nostra LNA
047900171023      **********  (serviva come paracadute ---- da eliminare in seguito)
048000171023     c**********         IF        wIMPORT  = 'S'
048100171023     c********           z-add     1             xx
048200171023     c**** BLPlna        lookup    pobar(xx)                              31
048300171023     C** 31              movel     POdpd(xx)     OG143
048400171023     c** 31              move      §ogdp1        Delivery_Depot
048500171023     c** 31              movel     BU_brt        Delivery_Depot
048600171023     c********           END
048700171023      **********
048800050418      * CODICI ANOMALIA
048900050418     c                   clear                   ZCOD
049000140331     c                   clear                   Peso_in_10gr
049100141217      *
049200141217      * il peso è con un decimale e deve essere passato intero con uno zero di arrotondamento
049300141217      * in step da 10gr.--> 3,6kg deve diventare 360 nella variabile richiesta da DPD lunga 8
049400141217     c     blpPKC        mult      100           Peso_in_10gr      8 0
049500150323      ***
049600150323      * ?   se non c'è il VDL (perchè non è stato ancora chiuso il foglio viaggio)
049700150323     c***  NON DEVE PIU' prendere il peso Fatturato - bensì deve prendere dal BLT il BLTPUC
049800150709      *  e comunque non deve mandare più il teorico.
049900150323     c                   if         Peso_in_10gr = 0
050000150323     c                               and
050100150323     c                              Peso_in_BLT > 0
050200150323     c                   move      Peso_in_BLT   Peso_in_10gr      8 0
050300150323     c                   end
050400150323      ***
050500150622      ** se proprio non c'è il peso in assoluto allora deve caricare il peso bollettato
050600150622     c                   if         Peso_in_10gr = 0
050700150623     c     blpPKB        mult      100           Peso_in_10gr
050800150622     c                   end
050900150826      ***
051000150826      *** ====================================
051100150826      **   solo x l'EXPORT quindi con ARB
051200150826      *** ====================================
051300150709      **   non inviamo informazioni di pesi FUORI MISURA (se può lo scoprirà DPD)
051400150709      **   quindi se si supera i 31,5 kg impostiamo fisso 31 kg.
051500150826     c                   if        wIMPORT <> 'S'
051600150826      ***
051700150826     c     *like         define    blppkb        wpes
051800150826     C     *LIKE         DEFINE    §qttpc        WTARA
051900150826      *
052000150826     C* PESO VDL - TARA
052100150826     C     §QTTPC        MULT      blpNCP        WTARA
052200150826     C                   Z-ADD     blpPKC        wpes
052300150826     c                   sub(h)    wtara         wpes
052400150826      *
052500150826      * Poi cerca il peso fra bollettato e CML
052600150826     C*  altrimenti se il bollettato è superiore passo il Bollettato.
052700150826     C     blpNCP        ifNE      blpNCL
052800150826     C     wpes          andLT     blpPKB
052900150826     C                   Z-ADD     blpPKB        wpes
053000150826     C                   ENDIF
053100150826      ***
053200150826     c     wpes          mult      100           Peso_in_10gr
053300150826      ***
053400150826     c                   if         Peso_in_10gr > 3150
053500150709     c                   eval       Peso_in_10gr = 3100
053600150709     c                   end
053700150623      ***
053800150826     c                   end
053900171024      ***
054000171024      *** Se si tratta di ORM non ha il Service code poichè non è su FIPND
054100171024      ***  essendo una spedizione ITALIA - ITALIA  quindi sulla base del peso
054200171024      ***   viene impostato il codice.
054300171024     c                   if        Dp3CEV = 'ORM'
054400171024     c                   clear                   peso_in_hg        7 0
054500171024      * tolgo l'ultima cifra che era decagrammi per comparare in etti
054600171024     c                   movel     Peso_in_10gr  peso_in_hg
054700171024      *** con il peso calcolare il service code
054800171024     c                   if        peso_in_hg  < hg_PPiccolo
054900171024     c                   movel     '136'         ServiceCODE
055000171024     c                   else
055100171024     c                   movel     '101'         ServiceCODE
055200171024     c                   end
055300171024     c                   end
055400171024      **
055500150826      *** ====================================
055600060727      * Anomalia (APERTO, BAGNATO.....SCONDIZIONATO) su IMPORT SCAN 10
055700161129     c                   IF        SCAN_CODE ='10' and wIMPORT ='S' and
055800061011     c                             Wanomalia <> *blank and blpdcm = 0
055900060727     c                   z-add     1             XX
056000060727     c     *blanks       lookup    Zcod(xx)                               31
056100060727      * Danneggiato: Damaged
056200060727     c   31              eval      ZCOD(XX) = '04'
056300060727     C                   ENDIF
056400050418      *
056500050418      * Scan '06' Reso forzo indirizzo errato
056600160830      * Scan '06' di DIROTTAMENTO impostiamo il "02" MISLOADED
056700161129     c                   IF        SCAN_CODE = '06'
056800050418     c                   z-add     1             XX
056900050418     c     *blanks       lookup    Zcod(xx)                               31
057000160830     c   31              if           DP3CEV =    'DIR'
057100160830     c                   move      '02'          ZCOD(XX)
057200160830     c                   else
057300160830     c                   move      '61'          ZCOD(XX)
057400160830     c                   end
057500101126      *
057600151214     c                   goto      al_momento_no
057700101126      * deve riportare l'ultimo motivo di giacenza assieme al reso
057800101126     c                   if        *in31
057900101126     c     Kgcp          chain     tigcp01l
058000101126     c                   if        %found(tigcp01l)
058100101126     c                   z-add     1             xx
058200101126     c     gcpCMC        lookup    K2z(xx)                                31
058300101126     c                   IF        *in31 = *on
058400101126     C                   movel     T2z(xx)       DS2Z
058500101126     c                   z-add     1             XX
058600151214     c     *blanks       lookup    zcod(xx)                               31
058700101126     c   31              eval      ZCOD(XX) = §2Zced
058800101126     c                   ELSE
058900101126     c                   eval      ZCOD(XX) = '11'
059000101126     c                   ENDIF
059100101126
059200101126     c                   endif
059300101126     c                   endif
059400151214     c     al_momento_no tag
059500161129
059600050418     C                   ENDIF
059700050418      * Eventi
059800161129     c                   If        SCAN_CODE <>'06'
059900050418     c                   IF        DP3cev <> *blanks
060000050418     c                   z-add     1             xx
060100050418     c     dp3cev        lookup    K2z(xx)                                31
060200050418     c                   IF        *in31 = *on
060300050418     C                   movel     T2z(xx)       DS2Z
060400050418     c                   z-add     1             XX
060500050418     c     *blanks       lookup    Zcod(xx)                               31
060600050418     c   31              eval      ZCOD(XX) = §2Zced
060700050418     c                   ENDIF
060800050418     c                   ENDIF
060900160830     c                   EndIF
061000050418      *
061100150709      *-------
061200150709      * Scan '03' in consegna segnalo FUORI MISURA
061300150709      *           se peso CML maggiore della tolleranza per fascia di peso
061400150709      * il 1/4/2014 :
061500150709      * DPD dice che sullo 03 non viene calcolato nel CLEARING e di mandarlo sullo 02
061600150709      *   in ogni caso lo inserisco anche sul passaggio alla HUB SCAN10 se già rilevato.
061700161129     c                   IF        SCAN_CODE= '03'  or
061800161129     c                             SCAN_CODE= '02'  or
061900161129     c                             SCAN_CODE= '10'
062000150709      * ma solo sull'IMPORT se supera i 31,5 kg.
062100150709     c                   IF         wIMPORT ='S' and BLPpkc > Wpkc01
062200150709      *  Not Conveyable (09)
062300151215      *    era una errata interpretazione lo 09 occorre inviare il 31 con mail di Jens
062400151215      *    Jens Opderbeck di oggi stesso
062500150709     c                   z-add     1             XX
062600150709     c     *blanks       lookup    Zcod(xx)                               31
062700151215     c   31              eval      ZCOD(XX) = '31'
062800150709     c                   end
062900150709      *--
063000150709     C                   ENDIF
063100140402      *-------
063200050418      * Colli sotto i 3 Kg
063300161125     c                   IF        ServiceCODE ='136'
063400050418     c                   z-add     1             XX
063500050418     c     *blanks       lookup    Zcod(xx)                               31
063600050418     c   31              eval      ZCOD(XX) = '44'
063700050418     C                   ENDIF
063800050418      *
063900050418      *
064000050418      * Se l'evento è in data di oggi e l'ora evento è maggiore dell'ora
064100050418      *  di elaborazione forzo questa come ora evento
064200161125     c                   IF        Data_Evento = WDATAoggi and
064300161125     c                             Ora__Evento > %subst(WORA:1:4)
064400161125     C                   movel     Wora          Ora__Evento
064500161130     C                   move      *zeros        HMS__Evento
064600161130     C                   movel     Wora          HMS__Evento
064700050418     c                   ENDIF
064800050418
064900050418      * Scrivo testata file se non ancora scritta
065000050418     c                   IF        Wtestata = *blanks
065100050418     C                   eval      Wtestata = 'S'
065200050418     c                   ENDIF
065300050418      * -----------
065400161125      *   Nuovo   FILE TTEVENTS
065500161125     c                   if        File_vers ='T' or
065600161125     c                             File_vers ='E'
065700161125     c                   exsr      Wrk_TTEVENTS
065800161125     c                   end
065900160516      *
066000160517      *  Se NON deve trasmettere lo SCAN (in questo caso lo SCAN 18 con mail/SMS)
066100160517      *   lo flagga
066200160516     c                   if        non_flaggare_DP3 <> *blank
066300160517     c                   eval      DP3ftr = 'T'
066400160517     C                   movel     99990104      DP3dtr
066500160517     C                   eval      DP3flr = 'NON TRASMESSO-MANCA MAIL/SMS'
066600160517     C                   ELSE
066700151211      *
066800151211     c                   exsr      Wrk_FILLER
066900050418      *
067000050418      * Aggiorno File eventi DPD
067100161125     C                   IF        Data_Evento > wdata_90
067200050418     c                   eval      DP3ftr = 'T'
067300050418     C                   else
067400050418     c                   eval      DP3ftr = 'S'
067500050418     C                   end
067600050418      *---
067700050418     C                   movel     wdataoggi     DP3dtr
067800160517     c                   end
067900160517      *---
068000160516     C     salta_scan    tag
068100160516      *
068200060609     c                   update    tidp3000
068300060103      *
068400050418     C                   ENDSR
068500050418      *----------------------------------------------------*
068600151211      *imposta il campo Filler con i dati dello SCAN inviato
068700050418      *----------------------------------------------------*
068800151211     C     Wrk_filler    Begsr
068900050418      *
069000151211     c                   clear                   dsDP3FLR
069100161125     c                   eval      DP3§PARCEL = ParcelNr
069200161125     c                   eval      DP3§SCODE  = ServiceCODE
069300161125     c                   eval      DP3§DATE   = Data_Evento
069400161125     c                   eval      DP3§TIME   = Ora__Evento
069500161128     c                   eval      DP3§SCAN   = Scan_CODE
069600151211     c                   eval      DP3§ADDC1  = zcod(1)
069700151211     c                   eval      DP3§ADDC2  = zcod(2)
069800151211     c                   eval      DP3§ADDC3  = zcod(3)
069900151211     c                   eval      DP3§PESO   = %editc(Peso_in_10gr:'X')
070000171017     c                   move      Sender_Depot  DP3§DEPOT
070100171027     c                   movel     Sender_Depot  DP3§BU
070200161125     c                   eval      DP3§NAZISO = CountryCodISO
070300161125     c                   eval      DP3§CAP    = CAP_Dest
070400171017     c                   move      Delivery_DepotDP3§ROUTE
070500171017     c****               eval      DP3§GEOIDN = GEOIDN
070600151211     c                   eval      DP3§DIST   = DIST
070700151211     c                   eval      DP3§STOP   = SSTOP
070800151211     c                   eval      DP3§QUALIF = qualifica
070900151211     c                   eval      DP3§TIPSCA = 'B'
071000151211     c                   if          DP3CEV ='ORM'
071100151211     c                   eval      DP3§TIPSCA = 'O'
071200151211     C                   end
071300151211     c                   eval      DP3flr = dsDP3FLR
071400151211      *
071500151211     C                   ENDSR
071600050418      *----------------------------------------------------*
071700050418      *   DEFINIZIONE CHIAVI                               *
071800050418      *----------------------------------------------------*
071900050418     C     *INZSR        BEGSR
072000050418      *
072100050418     c     *ENTRY        PLIST
072200050418     c                   PARM                    KPJBA
072300050418      *
072400050418     C     KTBE          KLIST
072500050418     C                   KFLD                    KCODE
072600050418     C                   KFLD                    KKE1
072700050418      *
072800050418     C     KTAB          KLIST
072900050418     C                   KFLD                    CODUT
073000050418     C                   KFLD                    KCOD
073100060619      *
073200150826     C     KTABel        KLIST
073300060619     C                   KFLD                    TBLKUT
073400060619     C                   KFLD                    TBLCOD
073500060619     C                   KFLD                    TBLKEY
073600050418      *
073700050418     C     KDP3          KLIST
073800050418     C                   KFLD                    KDTR
073900050418     C                   KFLD                    KFTR
074000050418      *
074100050418     C     KSPE          KLIST
074200050418     C                   KFLD                    dp3AAS
074300050418     C                   KFLD                    dp3LNP
074400050418     C                   KFLD                    dp3NRS
074500050418     C                   KFLD                    dp3NSP
074600050418      *
074700050418     C     KAR4          KLIST
074800050418     C                   KFLD                    dp3AAS
074900050418     C                   KFLD                    dp3LNP
075000050418     C                   KFLD                    dp3NRS
075100050418     C                   KFLD                    dp3NSP
075200050418     C                   KFLD                    KTRC
075300160502      *
075400160502     C     KAR5          KLIST
075500160502     C                   KFLD                    dp3AAS
075600160502     C                   KFLD                    dp3LNP
075700160502     C                   KFLD                    dp3NRS
075800160502     C                   KFLD                    dp3NSP
075900160502     C                   KFLD                    TipoAr5           3
076000050418      *
076100101126     C     Kgcp          KLIST
076200101126     C                   KFLD                    dp3AAS
076300101126     C                   KFLD                    dp3LNP
076400101126     C                   KFLD                    dp3NRS
076500101126     C                   KFLD                    dp3NSP
076600101126     C                   KFLD                    TRC0              2 0
076700101126     C                   z-add     0             TRC0
076800171017      *--
076900171017     C     BOLLA_dp3     KLIST
077000171017     C                   KFLD                    dp3AAS
077100171017     C                   KFLD                    dp3LNP
077200171017     C                   KFLD                    dp3NRS
077300171017     C                   KFLD                    dp3NSP
077400171023      *
077500171023     c     Kcdp4         klist
077600171023     c                   kfld                    CDPATB
077700171023     c                   kfld                    CDPVER
077800171023     c                   kfld                    CDPDSTR
077900050418      * Dati società
078000050418     C                   Z-ADD     1             CODUT
078100050418     C                   CALL      'X§PARUT'
078200050418     C                   PARM                    UT§DSE0F
078300050418     C                   MOVEL     REC80         CNCR80
078400050418      *
078500050418      *   EDPCEDxxx dove xxx è il Terminal
078600050418     C                   CLEAR                   Tibs36Ds
078700050418     c                   EVAL      I36ute = Knmus
078800050418     c                   EVAL      I36Tla = 'L'
078900050418     C                   CALL      'TIBS36R'
079000050418     C                   PARM                    Tibs36Ds
079100050418      *
079200050418      * LEggo AZORG per caricamento schiere P.O. DPD
079300050418     C                   clear                   XX
079400050418     C                   clear                   YY
079500050418     c     *loval        setll     azorg
079600050418     c                   read      azorg
079700050418      *
079800130910     C                   DOW       NOT %eof(Azorg01l)
079900130910      *
080000161129      * Decodifica P.O. BARTOLINI con codice DPD
080100050418     c                   If        ORGDE3 <> *blanks
080200050418     C                   add       1             xx
080300050418     C                   movel     ORGfil        POBAR(xx)
080400050418     C                   movel     ORGde3        PODPD(xx)
080500050418     C                   movel     ORGDE3        OG143
080600161129      * e se è un HUB DPD (190,191,192.....)
080700050418     c                   If        §ogNTW = 'DPD'
080800050418     C                   add       1             YY
080900161129     C                   movel     ORGfil        hubDPD(yy)
081000050418     c                   Endif
081100161129      *
081200050418     c                   Endif
081300050418      *
081400050418     c                   read      azorg
081500050418     c                   ENDDO
081600171017      *
081700171017      * -----------
081800171017      *  Cod.BRT -- B.U.
081900171017     C                   z-add     1             TBLKUT
082000171017     C                   movel     '3I'          TBLCOD
082100171017     C                   movel(p)  'DPD'         TBLKEY
082200171017     c     KTABel        chain     tabel00f
082300171017     c                   if        %Found(tabel00f)
082400171017     c                   movel     tbluni        ds3Idp
082500171017     c                   move      §3IBUCN       BU_brt            3 0
082600171017     c                   end
082700150826      *------------
082800150826     C** CARICAMENTO DATI VARI TASSAZIONE
082900150826     C                   Z-ADD     1             tblKUT
083000150826     C                   MOVEL     'QT'          tblcod
083100150826     C                   MOVEL(P)  '1'           tblKEY
083200150826     C                   CLEAR                   DSQT1
083300150826     C     KTABel        CHAIN     TABEL00F
083400150826     c                   if        %Found(TABEL00F)
083500150826     C                   MOVEL     TBLUNI        DSQT1
083600150826     c                   end
083700150826      *------------
083800050418      *
083900050418      * Carico Continuazione Tabella Causali Eventi "2Z"
084000050418     C                   clear                   XX
084100050418     C                   MOVE      '2Z'          KCOD
084200050418      *
084300050418     C     ktab          SETLL     TABEL00F
084400050418     C     ktab          READE     TABEL00F
084500130910     C                   DOW       NOT %eof(tabel00f)
084600050418     c                   movel     tbluni        ds2z
084700050418     c                   IF        §2zevd <> *blanks  and  §2zced <> *blanks
084800050418     C                   add       1             XX
084900050418     C                   movel     TBLKEY        K2Z(xx)
085000050418     C                   movel     TBLUNI        T2Z(xx)
085100050418     C                   ENDIF
085200050418     C     ktab          reade     TABEL00F
085300050418     C                   ENDDO
085400050418      *
085500050418      * carico schiera dei codici bolla di recupero
085600050418     C                   clear                   XX
085700050418     C                   move      '3A'          KCOD
085800050418     C     ktab          SETLL     TABEL00F
085900050418     C     ktab          READE     TABEL00F
086000130910     C                   DOW       NOT %eof(tabel00f)
086100050418     c                   movel     tbluni        ds3a
086200050418     c                   IF        §3arbl =  'R'
086300050418     C                   add       1             XX
086400050418     C                   movel     TBLKEY        s3a(xx)
086500050418     c                   ENDIF
086600050418     C     ktab          reade     TABEL00F
086700050418     C                   ENDDO
086800050418      *
086900060727      * carico schiera delle spunte Anoamlie (3E)
087000060727     C                   clear                   XX
087100060727     C                   move      '3E'          KCOD
087200060727     C     ktab          SETLL     TABEL00F
087300060727     C     ktab          READE     TABEL00F
087400130910     C                   DOW       NOT %eof(tabel00f)
087500060727     c                   movel     tbluni        ds3e
087600060727      * non deve caricare i rientri
087700170516      * e le segnalazioni
087800170516     c                   IF        §3eFTA <> 'R' and §3eFTA <> 'S'
087900060727     C                   add       1             XX
088000060727     C                   movel     TBLKEY        s3e(xx)
088100060727     c                   ENDIF
088200060727      *
088300060727     C     ktab          reade     TABEL00F
088400060727     C                   ENDDO
088500060727      *
088600050418      * Caricamento Tabella PESI DPD
088700050418     c                   eval      kcode = 'DPP'
088800050418     c                   eval      kke1 =  'PESO01'
088900050418     c     Ktbe          chain     tntbe000
089000050418      *
089100130910     c                   IF        %found(tntbe01l) and TBEatb = *BLANKS
089200050418     c                   movel     TBEuni        DDPP
089300050418     c                   movel     §DPPpkc       Wpkc01
089400050418     c                   ENDIF
089500050418      *
089600171024      *  Peso Piccolo
089700050418     c                   eval      kke1 =  'PESO02'
089800050418     c     Ktbe          chain     tntbe000
089900171024     c                   clear                   hg_PPiccolo       7 0
090000130910     c                   IF        %found(tntbe01l) and TBEatb = *BLANKS
090100050418     c                   movel     TBEuni        DDPP
090200050418     c                   movel     §DPPpkc       Wpkc02
090300171024     c                   move      §DPPpkb       hg_PPiccolo       7 0
090400050418     c                   ENDIF
090500050418      *
090600050418      * Carico Tabella Eventi DPD
090700050418     C                   clear                   XX
090800050418     c     'EVD'         chain     tntbe000                           31
090900050418      *
091000050418     c                   DOW       *in31 = *off
091100050418      * Se il S.I. è indicato deve essere uguale al mio
091200050418     c                   IF        TBEatb = *BLANKS  and
091300050418     c                              (TBEsif = KNSIF or TBEsif = *blanks)
091400050418     c                   add       1             xx
091500050418     c                   movel     TBEke1        KEVD(XX)
091600050418     c                   movel     TBEuni        EVD(XX)
091700050418     c                   ENDIF
091800050418      *
091900050418     c     'EVD'         reade     tntbe000                               31
092000050418     c                   ENDDO
092100050418      *
092200050418      * DATA DEL GIORNO + ORA
092300050418     C                   TIME                    W0140
092400050418     C                   MOVE      W0140         W0080
092500050418     C     *eur          MOVEL     W0080         Wdata
092600050418     C     *iso          MOVEL     Wdata         W0080
092700050418     c                   MOVEL     W0080         Wdataoggi
092800050418     c                   MOVEL     W0080         Wdata80
092900050418     C                   MOVEL     W0140         WORA
093000170407      *
093100170407      *-----------------------------
093200170412      *   dal 12/4/2017 solo i TTEVENTS
093300170407     c                   eval      File_vers ='T'
093400171017
093500170407      *-----------------------------
093600050418      * calcola il limite di 90 gg. oltre i quali la DpD non ha bisogno dei records
093700050418     C     Wdata         SUBDUR    90:*Days      W0m90
093800050418     c     *iso          MOVEL     W0m90         W008M
093900050418     c                   MOVEL     W008M         Wdata_90
094000161125      **
094100050418     C     endinz        ENDSR
094200081015      **?------------------------------------------------------------------ */
094300081015      *   compatta il CAP eliminando dei blank all'interno es.GB
094400081015      **?------------------------------------------------------------------ */
094500081015     c     Compatta_CAP  begsr
094600081015      *
094700081015     c                   z-add     0             to                3 0
094800081015     c                   do        9             from              3 0
094900081015     c                   if        skCAP(from) = *blank
095000081015     c                   if          to = 0
095100081015     c                   z-add     from          to
095200081015     c                   end
095300081015     c                   else
095400081015     c                   if          to > 0
095500081015     c                   move      skCAP(from)   skCAP(to)
095600081015     c                   move      *blank        skCAP(from)
095700081015     c                   add       1             to
095800081015     c                   end
095900081015     c                   end
096000081015     c                   enddo
096100081015
096200081015     c                   endsr
096300161125      *----------------------------------------------------*
096400161125      *   TTEVENTS  TIVGD                                  *
096500161125      *----------------------------------------------------*
096600161125     C     Wrk_TTEVENTS  Begsr
096700161125      *
096800161125      **  TIVGD APRE e CHIUDE
096900161125     c                   if        VGDTIP_$E ='APRE  '
097000161125      **                                        ===========
097100161125     c                   clear                   VGDTIP_$E
097200161125      **                                        ===========
097300161125      *  istruzioni apertura blocco scrittura TIVGD
097400161125     C                   clear                   trul47ds
097500161125     C                   eval      d47opz  = 'I'
097600161125     C                   eval      d47tip  = '$E'
097700161125     C                   eval      d47lck  = 'N'
097800161125     C                   eval      d47chkj = 'N'
097900161125     C                   eval      d47pgm  = 'FIEU16R'
098000161125     C                   call      'TRUL47R'
098100161125     C                   parm                    trul47ds
098200161125      **
098300161125     c                   elseIF    VGDTIP_$E ='CHIUDE'
098400161125      **
098500161125     c                   exsr      FINE_TTEVENT                                 #FINE MSG
098600161125     C* Infine elimino il blocco elaborazione TIVGD
098700161125     C                   clear                   trul47ds
098800161125     C                   eval      d47opz  = 'F'
098900161125     C                   eval      d47tip  = '$E'
099000161125     C                   call      'TRUL47R'
099100161125     C                   parm                    trul47ds
099200161125      **
099300161125     c                   end
099400161125      **
099500161125      **  scrive il  TTEVENTS
099600161125      * dettaglio TIVGD                     ===========
099700161125     c                   IF        VGDTIP_$E <>'CHIUDE'
099800161125      ** Solo la 1°Volta                    ===========
099900161125     c                   if         TESTA_TTE ='SI'
100000161125     c                   clear                   TESTA_TTE                      Solo la 1°volta
100100161125     c                   exsr      TESTA_TTEVENT                                Testata MSG
100200161125     c                   end
100300161125      *
100400161125      *  riga di dettaglio status SCAN
100500161125      *                           ---------------
100600161125     c                   exsr      TTEVENTS_dati                                riga di TTEVENTS
100700161125      *                           ---------------
100800161125     c                   endIF
100900161125      *
101000161125     c                   endsr
101100130705      *-------------------------------------------------------------------------
101200161125     c     TESTA_TTEVENT BEGsr
101300161125      *-------------------------------------------------------------------------
101400161125      *  6 righe di testata
101500161125      *
101600170410      **   #FILE;GEODATA_STATUS_0230820_N00_D20161016T082554;
101700161125     c                   clear                   dati_TTE
101800161129     c                   EVAL      dati_TTE = %trim(HEAD(1)) +
101900161128     c                              wdataoggi + 'T' + wora + ';'
102000161125     c                   exsr      tivgd_$E
102100161125      *
102200161125      **   #ENCODING;ISO-8859-1;
102300161125     c                   clear                   dati_TTE
102400161125     c                   EVAL      dati_TTE = HEAD(2)
102500161125     c                   exsr      tivgd_$E
102600161125      *
102700170410      **   #VERSION;03.10;
102800161125     c                   clear                   dati_TTE
102900161125     c                   EVAL      dati_TTE = HEAD(3)
103000161125     c                   exsr      tivgd_$E
103100161125      *
103200161125      **   #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
103300161125     c                   clear                   dati_TTE
103400161125     c                   EVAL      dati_TTE = HEAD(4)
103500161125     c                   exsr      tivgd_$E
103600161125      *
103700161125      **   #DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;........
103800161125     c                   clear                   dati_TTE
103900161125     c                   EVAL      dati_TTE = %trim(HEAD(5)) +
104000161125     c                                        %trim(HEAD(6)) +
104100161125     c                                        %trim(HEAD(7)) +
104200161125     c                                        %trim(HEAD(8)) +
104300161125     c                                        %trim(HEAD(9)) +
104400161125     c                                        %trim(HEAD(10)) +
104500161125     c                                        %trim(HEAD(11)) +
104600161130     c                                        %trim(HEAD(12))
104700161125     c                   exsr      tivgd_$E
104800161125      *
104900170410      **   HEADER;03.10;STATUS;
105000161125     c                   clear                   dati_TTE
105100161125     c                   EVAL      dati_TTE = HEAD(14)
105200161125     c                   exsr      tivgd_$E
105300161125      *
105400161125     c                   endsr
105500161125      *-------------------------------------------------------------------------
105600161125     c     FINE_TTEVENT  BEGsr
105700161125      *-------------------------------------------------------------------------
105800161125     c                   clear                   dati_TTE
105900161125     c                   movel     '#END;'       dati_TTE
106000161125     c                   exsr      tivgd_$E
106100161125      *
106200161125     c                   endsr
106300161125      *-------------------------------------------------------------------------
106400161125     C     TTEVENTS_Dati Begsr
106500161125      *-------------------------------------------------------------------------
106600161128     c                   eval      non_flaggare_DP3 = ' '
106700161128     c                   eval        ContInfo1      = ' '
106800161128     c                   eval        ContInfo2      = ' '
106900170502     c     ';':' '       xlate     BLPLOD        BLPLOD
107000170502     c     ';':' '       xlate     BLPRMA        BLPRMA
107100161128      *
107200161128     c                   Select
107300161128      * SCAN 01/02/05/10
107400161129     c                   WHEN      Scan_code = '01' or Scan_code = '02'or
107500161129     c                             Scan_code = '05' or Scan_code = '10'
107600161128      * SCAN 03
107700161129     c                   WHEN      Scan_code = '03'
107800161128      * SCAN 06
107900161129     c                   WHEN      Scan_code = '06'
108000161128      * SCAN 04/08
108100161128      * SCAN 07/09/13/14
108200161129     c                   WHEN      Scan_code = '04' or Scan_code = '08' or
108300161129     c                             Scan_code = '13' or Scan_code = '14' or
108400161129     c                             Scan_code = '07' or Scan_code = '09'
108500161128      * SCAN 18
108600161129     c                   WHEN      Scan_code = '18'
108700161128      * solo x Import
108800161129     c                   IF        wIMPORT   ='S'
108900161128      * inviato ISOLA
109000161128     c                   IF         DP3CEV ='ISL'
109100161128     c                   eval        ContInfo1      = '00'
109200161128     c                   eval        ContInfo2      = 'ISLAND'
109300161128     c                   ENDIF
109400161128      *
109500161128      * inviato ALERT con mail o SMS
109600161128     c                   IF        (DP3CEV ='S25' or DP3CEV ='E25') or
109700161128     c                             (DP3CEV ='S44' or DP3CEV ='E44' or
109800161128     c                              DP3CEV ='X44')
109900161128     c                   Exsr      Alert_18_44
110000161128      * non c'era il Predict
110100161128     c                   if            predict_invio = *blank
110200161128     c                   eval      non_flaggare_DP3 = 'N'
110300161128     c                   clear                   ContInfo1
110400161128     c                   clear                   ContInfo2
110500161130      ** NON deve inviare lo SCAN 18 se mancano i dati
110600161130     c                   LEAVEsr
110700161130     c                   end
110800161128     c                   ENDIF
110900161128      *
111000161128     c                   END
111100161128      *
111200161128     c                   ENDSL
111300161128      *
111400161128      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
111500170410      *  particolarità sullo SCAN 13 vogliono il PODIMAGE7 con tutti zeri
111600170410      *  altrimenti è sempre vuoto.
111700170410      *
111800161128     c                   clear                   dati_TTE
111900170410     c                   clear                   podImage7
112000170410      * solo sulla consegna '0000000'
112100170410     c                   if        Scan_CODE = '13'
112200170410     c                   move      *all'0'       podImage7
112300170410     c                   end
112400161128      *
112500161125     c                   EVAL      dati_TTE = %trim(HEAD(15))  + ';' +            0 TTEVENTS;2
112600161125     c                                        %trim(ParcelNr)  + ';' +            1 PARCELNUMBER
112700170410     c                                            'DE06'       + ';' +            2 STATUSTYPE
112800161125     c                                                           ';' +            3 ORIGINPARCELNUMB
112900161125     c                                     %trim(ServiceCODE)  + ';' +            4 SERVICECODE
113000170410     c                                     %trim(CAP_Dest)     + ';' +            5 DZIPCODE
113100170410     c                                     %trim(CountryCodISO)+ ';' +            6 DCOUNTRYCODE
113200170410     c                                     %trim(Scan_CODE)    + ';' +            7 SCANCODE
113300170410     c                                     %trim(zcod(1))      + ';' +            8 REASONCODE
113400170410     c                                     %trim(zcod(2))      + ';' +            9 REASONCODE2
113500170410     c                                     %trim(zcod(3))      + ';' +           10 REASONCODE3
113600171017     c                                           Sender_Depot  + ';' +           11 DEPOT
113700170410     c                                                           ';' +           12 AGENTLOCATCODE
113800170410     c                                                           ';' +           13 AGENTLOCATTYPE
113900170410     c                                                           ';' +           14 AGENTPARCELNUMBE
114000170410     c                                     %trim(BLPNZD)       + ';' +           15 COUNTRY
114100170410     c                                     %trim(BLPLOD)       + ';' +           16 CITY
114200170410     c                                     %trim(Data_Evento)        +           17 STATUSDATETIME
114300170410     c                                     %trim(HMS__Evento)  + ';' +           18
114400170410     c                                                           ';' +           19 TIMEZONE
114500161128     c                                            wdataoggi          +           20 MEMDATETIME
114600170410     c                                            wora         + ';' +           21
114700171017     c                                       Delivery_Depot    + ';' +           22 DDEPOT
114800170410     c                                     '001'               + ';' +           23 DELIVERYTOUR
114900170410     c                             %trim(%editc(Peso_in_10gr:'4')) + ';' +       24 MEASUREDWEIGHT
115000170410     c                                                           ';' +           25 DIMENSION
115100170410     c                                     %trim(BLPRMA)       + ';' +           26 PARCELREF
115200170410     c                                                           ';' +           27 PARCELREF2
115300170410     c                                   %trim(PodImage7)      + ';' +           28 PODIMAGEREF
115400170410     c                                   %trim(Firma_Consegna) + ';' +           23 RNAME
115500170410     c                                                           ';' +           38 TRDPNAME
115600170410     c                                         '001'           + ';' +           39 AREA
115700170410     c                                         '001'           + ';' +           30 STOP
115800170410     c                                     %trim(ContInfo1)    + ';' +           41 CONTAINERINFO1
115900170410     c                                     %trim(ContInfo2)    + ';' +           42 CONTAINERINFO2
116000170410     c                                                           ';' +           43 SHIPMENTREF
116100170410     c                                                           ';' +           44 SUPPLIERREF
116200170410     c                                                           ';' +           45 CONTAINERREF
116300170410     c                                                           ';' +           46 CONTAINERTYPE
116400170410     c                                                           ';' +           47 ACTORID
116500170410     c                                                           ';' +           48 OPERATIVERID
116600170410     c                                                           ';' +           49 PICKUPORDERNUMBE
116700170410     c                                                           ';' +           40 GPSLAT
116800170410     c                                                           ';' +           51 GPSLONG
116900170410     c                                                           ';' +           52 RETURNPARCELNUMB
117000170410     c                                                           ';'             53 COMPINFO1
117100161125      *
117200161125     c                   exsr      tivgd_$E
117300161125      *
117400161125      *
117500161125     c                   endsr
117600161125      *-------------------------------------------------------------------------
117700161125     C     tivgd_$E      Begsr
117800161125      *-------------------------------------------------------------------------
117900161125     c                   clear                   tivgd000
118000161125     c                   eval      vgddta = %TrimR(dati_TTE)
118100161125     c                   eval      vgdtip = '$E'
118200161125     c                   eval      vgdksu = '0DPD0OUT'
118300161125     c                   eval      vgdtsc = 'WW'
118400161125     c                   eval      vgdpgm = 'FIEU16R'
118500161125     c                   eval      vgddat = W0080
118600161125     C                   WRITE     tivgd000
118700161125      *
118800161125     C                   Endsr
118900161128      *-------------------------------------------------------------------------
119000161128      *   Se inviato il PREDICT (Alert) SCAN 18(44)        *
119100161128      *-------------------------------------------------------------------------
119200161128     C     Alert_18_44   Begsr
119300161128      *
119400161130     c                   clear                   predict_invio     1
119500161128     c                   eval        ContInfo1      = '01'
119600161130     c                   eval        ContInfo2      = '44:1'
119700161128      *
119800161128      * aggancia FIAR500R per le decodifiche da passare
119900161128      *   per poi generare  il filler con lo scan 18
120000161128     c                   movel     'EMD'         TipoAr5
120100161128     c     Kar5          chain     fiar501l
120200161128     c                   IF        %found(fiar501l)
120300161128     c                   eval        dar5EMD = AR5UNI
120400170719     C     ';':' '       XLATE     §AR5EML       §AR5EML                            *No puntoevirg
120500170719     C     ';':' '       XLATE     §AR5TEL       §AR5TEL                            *No puntoevirg
120600170719      *
120700161130     c                   eval        ContInfo2 = %trim(ContInfo2) + ':' +
120800161130     c                                        %trim(§AR5EML)      + ':' +
120900161130     c                                        %trim(§AR5TEL)      + ':'
121000161128      *  EMAIL/SMS
121100161128     c                   eval          predict_invio ='S'
121200161128     c                   move      '3'           tipopredict       1
121300161128     c                   if          §AR5EML<> *blank and §AR5TEL = *blank
121400161128     c                   move      '2'           tipopredict
121500161128     c                   elseif      §AR5EML = *blank and §AR5TEL<> *blank
121600161128     c                   move      '1'           tipopredict
121700161128     c                   elseif      §AR5EML = *blank and §AR5TEL = *blank
121800161128     c                   clear                   predict_invio     1
121900161128     c                   leavesr
122000161128     c                   end
122100161128      *
122200161128      *   durante il MIC  (DLO)
122300161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
122400161128     c                                    tipopredict +  ':2:DLO:'
122500161128      *   DATA
122600161128     c                   if          §AR5DTO<> *blank and §AR5DOS = *blank
122700161128     c                                  or
122800161128     c                               §AR5DTO > §AR5DOS
122900161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
123000161128     c                                 %subst(§AR5DTO:1:8) + ':' +
123100161128     c                                 %subst(§AR5DTO:9:4) + ':'
123200161128      *
123300161128     c                   elseif      §AR5DOS<> *blank and §AR5DTO = *blank
123400161128     c                                  or
123500161128     c                               §AR5DOS > §AR5DTO
123600161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
123700161128     c                                 %subst(§AR5DOS:1:8) + ':' +
123800161128     c                                 %subst(§AR5DOS:9:4) + ':'
123900161128     c                   end
124000161128      *    TIME
124100161128     c                   if        §AR5OPCD <> *blank
124200161128     c                                  and §AR5OPCA <> *blank
124300161128      *
124400161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
124500161128     c                               '1:' + §AR5OPCD +':'+ §AR5OPCA +'::'
124600161128      *
124700161128     c                   elseif    §AR5OPCD = *blank
124800161128     c                                  and §AR5OPCA = *blank
124900161128      *
125000161128     c                   eval        ContInfo2 = %trim(ContInfo2) +
125100161128     c                                 '0:1900:::'
125200161128      *
125300161128     C                   End
125400161128      *------
125500161128     C                   END
125600161128      *
125700161128     C                   Endsr
125800161128      *-------------------------------------------------------------------------
125900161125** (HEAD)
126000170410#FILE;GEODATA_STATUS_0230820_N00_D                                                 01<
126100161125#ENCODING;ISO-8859-1;                                                              02
126200170410#VERSION;03.10;                                                                    03<
126300161125#DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;                                       04
126400161130#DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;         05|
126500170410SERVICECODE;DZIPCODE;DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;                 06||
126600161130REASONCODE3;DEPOT;AGENTLOCATIONCODE;AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;       07| |
126700161130COUNTRY;CITY;STATUSDATETIME;TIMEZONE;MEMDATETIME;DDEPOT;DELIVERYTOUR;              08|  |
126800170410MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;PODIMAGEREF;RNAME;TRDPNAME;          09|  |
126900161130AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;      10| |
127000161130CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;               11||
127100161130RETURNPARCELNUMBER;COMPINFO1;;                                                     12|
127200161130                                                                                   13
127300170410HEADER;03.10;STATUS;                                                               14<
127400161125TTEVENTS;2                                                                         15<----
