000100170418     h DECEDIT('0.') DATEDIT(*YMD.)
000200051007     H* Fieu42R *----------------------------------------------------*
000300900418     H*                                                              *
000400080515     H* Scrittura SCANDATA  da aggiungere agli SCAN di delivery      *
000500050919     h* record in stato 10 DA PORTARE A 18 X MANCATI RITIRI          *
000600060811      **?___________________________________________________________ *
000700050919     Ffidpo04l  uf   E           k DISK    commit
000800080424     ftivgd00f  if a E             DISK    commit
000900080529     Ffnorm01l  if   E           k DISK
001000080529     Fazorg01l  if   E           k DISK
001100080529      **?___________________________________________________________ *
001200170418      *
001300170418     D HEAD            S             80    DIM(15) CTDATA PERRCD(1)
001400170418      *----------------------------------------------------*                                 SOLO IL
001500170418     D File_vers       S              1    inz('E')                             E=Entrambe T=TTEVENT
001600170418     D VGDTIP_$E       S              6    inz('APRE  ')
001700170418     D TESTA_TTE       S              2    inz('SI')
001800170418     d dati_TTE        S           2050    inz(' ')
001900170418     D Wdataoggi       S              8
002000170418     D Wora            S              6
002100170418     D ContInfo1       S              2
002200170418     D ContInfo2       S            200
002300170418      *----------------------------------------------------*                                 SOLO IL
002400080403     D** dpoRecord       S                   LIKE(vgddta)
002500080403     D dpoRecord       S           2048
002600060810     d trul47ds      e ds
002700060810      *
002800900418     D KPJBA         E DS
002900080529     D OG143         E DS
003000050705     D trul33ds      E DS
003100060810     D Lung_Parcel     S              3  0
003200060811     D sav_lung_par    S              3  0 inz(0)
003300060811     D Almeno_uno      S              1
003400050704
003500050705     D KPJBus          s                   like(kpjbu)
003600060811     D solo_inizio     s              1    inz(' ')
003700060920     D status          s                   like(dposts)
003800060811     D savDEP          S                   like(dpoDEP)
003900050920      **?__________________________________________________________________ */
004000170418      *    LETTURA del FIDPO con STATO 14 di Mancato Ritiro NON TRASMESSO
004100050920      **?__________________________________________________________________ */
004200060920     c                   z-add     14            status
004300170418      **?____________________________________ */
004400050927     c     status        setll     fidpo04l
004500050927     c     status        reade     fidpo04l
004600060811      **
004700060811     c                   dow       not %EoF(fidpo04l)
004800060920      * se già trasmesse  bypassa
004900060920     c                   if        dpodtr = 0
005000170418      **----
005100170418     c                   exsr      ELABORA
005200170418      *-----
005300050705     c                   move      *date         DPODTR
005400050919     c                   update    fidpo000                             99
005500060811     c                   endif
005600060811
005700060811     c     status        reade     fidpo04l
005800050705     c                   enddo
005900170418      **?____________________________________ */
006000060810
006100050919      * chiusura file da inviare solo se ho trovato almeno un record valido
006200170418     c                   if        File_vers ='S' or File_vers ='E'
006300060811     c                   exsr      Coda
006400170418     c                   end
006500170418      *--
006600060810     C* Infine elimino il blocco elaborazione TIVGD
006700060811     c                   if        almeno_uno = 'S' and solo_inizio = 'S'
006800170418      *
006900170418     c                   if        File_vers ='E' or File_vers ='S'
007000060810     C                   clear                   trul47ds
007100060810     C                   eval      d47opz  = 'F'
007200060810     C                   eval      d47tip  = '$3'
007300060810     C                   call      'TRUL47R'
007400060810     C                   parm                    trul47ds
007500170418     c                   end
007600170418      **
007700170418     C* chiude il TIVGD se era stato aperto
007800170418     c                   if        VGDTIP_$E = *blank  AND
007900170418     c                             (File_vers ='E' or File_vers ='T')
008000170418     c                   eval      VGDTIP_$E = 'CHIUDE'
008100170418     c                   exsr      Wrk_TTEVENTS
008200170418     c                   end
008300170418      *
008400060811     c                   endif
008500050704      *
008600170418     c                   commit
008700170418     C*
008800050705     c                   seton                                        lr
008900050920      **?__________________________________________________________________ */
009000170418     c     ELABORA       begsr
009100050920      **?__________________________________________________________________ */
009200170418      *--
009300170418      ** controlla lunghezza del Parcel
009400170418     c                   eval      Lung_Parcel = %Len(%trim(dpoNRP))
009500170418
009600170418      * operazioni da eseguire una sola volta reperimento numero di
009700170418      * trasmissione e intestazione del file da inviare
009800170418     c                   if        Lung_Parcel <> sav_lung_par  or
009900170418     c                             dpoDEP      <> savDEP
010000170418     c*-
010100170418     c                   if        File_vers ='S' or File_vers ='E'
010200170418     c                   exsr      Testata
010300170418     c                   end
010400170418     c*-
010500170418     c                   eval      sav_lung_par  = Lung_Parcel
010600170418     c                   eval      SavDEP        = dpoDEP
010700170418     c                   endif
010800170418      *
010900170418      *  Deve reperire il Collecting DEPOT che ha dato il Mancato Ritiro
011000170418      *   da impostare nello SCAN e lo decodifica dall'organigramma:
011100170418      *    mette di Default Verona
011200170418     c                   movel     '0837'        Coll_Depot        4
011300170418     c     korm          chain     fnorm01l
011400170418     c                   if        %found(fnorm01l)
011500170418      *  con la Filiale Ritirante
011600170418     c     ormPOR        chain     azorg01l
011700170418     c                   if        %found(azorg01l)
011800170418     c                   movel     orgDE3        Og143
011900170418     c                   if        §OGDP1 <> *blank
012000170418     c                   movel     §OGDP1        Coll_Depot
012100170418     c                   endif
012200170418     c                   endif
012300170418     c                   endif
012400170418      * dettaglio
012500170418     c                   move      'S'           almeno_uno
012600170418      *-- Vecchio SCAN
012700170418     c                   if        File_vers ='E'  or File_vers ='S'
012800170418     c                   exsr      Dettaglio
012900170418     c                   end
013000170418      *-- NUOVO TTEVENTS
013100170418     c                   if        File_vers ='E'  or File_vers ='T'
013200170418      *--
013300170418      *  istruzioni apertura blocco scrittura TIVGD
013400170418     c                   if        VGDTIP_$E ='APRE  '
013500170418      *       solo la prima riga di dettaglio
013600170418     c                   exsr      Wrk_TTEVENTS
013700170418     c                   end
013800170418      **----
013900170418      *  quindi scrive riga di dettaglio
014000170418     c                   exsr      Wrk_TTEVENTS
014100170418      *--
014200170418     c                   end
014300170418      *--
014400170418     c                   endsr
014500170418      **?__________________________________________________________________ */
014600170418     c     Testata       begsr
014700170418      **?__________________________________________________________________ */
014800170418      *  istruzioni apertura blocco scrittura TIVGD
014900170418      *    fatto solo all'inizio del programma
015000170418     c                   if        solo_inizio = *blank
015100170418     C                   clear                   trul47ds
015200170418     C                   eval      d47opz  = 'I'
015300170418     C                   eval      d47tip  = '$3'
015400170418     C                   eval      d47lck  = 'N'
015500170418     C                   eval      d47chkj = 'N'
015600170418     C                   eval      d47pgm  = 'FIEU42R'
015700170418     C                   call      'TRUL47R'
015800170418     C                   parm                    trul47ds
015900170418     c                   eval      solo_inizio = 'S'
016000170418     c                   end
016100170418
016200170418      * a cambio di tipo Parcel --> cambio di SCAN
016300170418      *   chiude con il record di Fine
016400170418     c                   if        lung_parcel <> sav_lung_par  or
016500170418     c                             dpoDEP      <> savDEP
016600170418     c                   exsr      Coda
016700170418     c                   end
016800170418
016900170418      *rekord testata scan
017000170418     c                   clear                   dpoRecord
017100170418
017200170418     c                   if        lung_parcel = 11
017300170418      *
017400170418     c                   eval      dporecord ='0818837001' +
017500170418     c                             udtymda + oratra + '000000' + '|'
017600170418     c                   else
017700170418     c                   eval      dporecord ='0818' +
017800170418     c                             udtymda + oratra +
017900170418     c                             '|D' + %trim(dpoDEP) + '|T001|'
018000170418     c                   end
018100170418      *
018200170418     C                   exsr      tivgd
018300170418      *
018400170418     c                   endsr
018500050920      **?__________________________________________________________________ */
018600060811     c     Dettaglio     begsr
018700050920      **?__rekord dettaglio scan __________________________________________ */
018800050920
018900050920      * prepara scan per invio
019000060811     c                   clear                   dporecord
019100060811
019200060811     c                   if        lung_parcel = 11
019300060811      *
019400051007     c                   eval      dporecord ='0118837001' +
019500061006     c                             udtymda + oratra + '000000' +
019600060811     c                             %trim(dpoNRP)+ '|Z' + %trim(dpoCDmr)+ '|'
019700060811     c                   else
019800061003     c                   eval      dporecord ='0118' +
019900061006     c                             udtymda + oratra +
020000080529     c                             '|D' + %trim(Coll_Depot) + '|T001' +
020100060811     c                             '|P' + %trim(dpoNRP) +
020200060915     c                             '|Z' + %trim(dpoCDmr)+
020300080529     c                             '|YAL01|YAD03' + %subst(dpoCDmr:2:2) + '|'
020400080527     c
020500060811     c                   end
020600060811      *
020700060810     C                   exsr      tivgd
020800060810
020900050920     c                   endsr
021000050920      **?__________________________________________________________________ */
021100060811     c     Coda          begsr
021200050920      **?-piede messaggio          ________________________________________ */
021300060811     c                   if        almeno_uno = 'S'
021400060811      *
021500060811     c                   clear                   dpoRecord
021600060811      *
021700060811     c                   if        sav_lung_par  = 11
021800060811      *
021900051007     c                   eval      dporecord ='0918837001' +
022000061006     c                             udtymda + oratra +'000000' + '|'
022100060811     c                   else
022200060811      *
022300060811     c                   eval      dporecord ='0918' +
022400061006     c                             udtymda + oratra +
022500061027     c                             '|D' + %trim(savDEP) + '|T001|'
022600060811     c                   end
022700050920      *
022800060810     C                   exsr      tivgd
022900060811
023000060811     c                   end
023100060810
023200050920     c                   endsr
023300050920      **?__________________________________________________________________ */
023400050512     c     *inzsr        begsr
023500050920      **?__________________________________________________________________ */
023600050705     c     *entry        plist
023700050705     C                   parm                    kpjba
023800060920      ***
023900080529     c     korm          klist
024000080529     c                   kfld                    DPOPOE
024100080529     c                   kfld                    DPONRS
024200080529     c                   kfld                    DPONOR
024300080529     c                   kfld                    DPONRV
024400080529
024500050705     C                   TIME                    ORADAT           14 0
024600050705     C                   MOVEL     ORADAT        ORATR             6 0
024700061006     C                   MOVEL     ORATR         ORATRa            6
024800050920     c                   move      *date         udtymd            8 0
024900061006     c                   move      udtymd        udtymda           8
025000170418     c                   move      udtymd        wdataoggi
025100170418     C                   MOVEL     oradat        WORA
025200170418      *-----------------------------
025300170418      *  prima gli SCAN
025400170418     c                   eval      File_vers ='S'
025500170418      *
025600170418      *   dal 18/4/2017 anche i TTEVENTS con gli SCAN
025700170418     c                   if        udtymd  >= 20170418
025800170418     c                   eval      File_vers ='E'
025900170418     c                   end
026000170418      *
026100170418      *   dal 18/4/2017 solo i TTEVENTS
026200170418     c                   if        udtymd  >= 20170420
026300170418     c                   eval      File_vers ='T'
026400170418     c                   end
026500170418      **
026600170418      *-----------------------------
026700050512     c                   endsr
026800060810      **?__________________________________________________________________ */
026900060810      *   Scrittura tivgd file da trsmettere               *
027000060810      **?__________________________________________________________________ */
027100060810     C     tivgd         Begsr
027200060810      *
027300170418     c                   if        File_vers ='E' or File_vers ='S'
027400170418      *
027500060810     c                   clear                   tivgd000
027600080403     c                   eval      vgddta = %TrimR(dpoRecord)
027700060810     c                   eval      vgdtip = '$3'
027800060810     c                   eval      vgdksu = '0DPD0OUT'
027900060810     c                   eval      vgdtsc = 'WW'
028000060810     c                   eval      vgdpgm = 'FIEU42R'
028100061023     c                   eval      vgddat = udtymd
028200060810      *
028300060810     C                   WRITE     tivgd000
028400170418      *
028500170418     c                   end
028600060810      *
028700060810     C                   Endsr
028800170418      **************************************************************************
028900170418      *-------------------------------------------------------------------------
029000170418     C     TTEVENTS_Dati Begsr
029100170418      *-------------------------------------------------------------------------
029200170418      *
029300170418     c                   eval        ContInfo1      = '01'
029400170418     c                   eval        ContInfo2      = '03' + %subst(dpoCDmr:2:2)
029500170418      *
029600170418     c                   clear                   dati_TTE
029700170418      *
029800170418     c                   EVAL      dati_TTE = %trim(HEAD(15))  + ';' +            0 TTEVENTS;2
029900170418     c                                        %trim(dpoNRP)    + ';' +            1 PARCELNUMBER
030000170418     c                                            'DE06'       + ';' +            2 STATUSTYPE
030100170418     c                                                           ';' +            3 ORIGINPARCELNUMB
030200170418     c                                                           ';' +            4 SERVICECODE
030300170418     c                                                           ';' +            5 DZIPCODE
030400170418     c                                                           ';' +            6 DCOUNTRYCODE
030500170418     c                                          '18'           + ';' +            7 SCANCODE
030600170418     c                                     %trim(dpoCDmr)      + ';' +            8 REASONCODE
030700170418     c                                                           ';' +            9 REASONCODE2
030800170418     c                                                           ';' +           10 REASONCODE3
030900170418     c                                  '023' +  Coll_Depot    + ';' +           11 DEPOT
031000170418     c                                                           ';' +           12 AGENTLOCATCODE
031100170418     c                                                           ';' +           13 AGENTLOCATTYPE
031200170418     c                                                           ';' +           14 AGENTPARCELNUMBE
031300170418     c                                                           ';' +           15 COUNTRY
031400170418     c                                                           ';' +           16 CITY
031500170418     c                                   udtymda                     +           17 STATUSDATETIME
031600170418     c                                   oratra                + ';' +           18
031700170418     c                                                           ';' +           19 TIMEZONE
031800170418     c                                            wdataoggi          +           20 MEMDATETIME
031900170418     c                                            wora         + ';' +           21
032000170418     c                                                           ';' +           22 DDEPOT
032100170418     c                                     '001'               + ';' +           23 DELIVERYTOUR
032200170418     c                                                           ';' +           24 MEASUREDWEIGHT
032300170418     c                                                           ';' +           25 DIMENSION
032400170418     c                                                           ';' +           26 PARCELREF
032500170418     c                                                           ';' +           27 PARCELREF2
032600170418     c                                                           ';' +           28 PODIMAGEREF
032700170418     c                                                           ';' +           23 RNAME
032800170418     c                                                           ';' +           38 TRDPNAME
032900170418     c                                                           ';' +           39 AREA
033000170418     c                                                           ';' +           30 STOP
033100170418     c                                     %trim(ContInfo1)    + ';' +           41 CONTAINERINFO1
033200170418     c                                     %trim(ContInfo2)    + ';' +           42 CONTAINERINFO2
033300170418     c                                                           ';' +           43 SHIPMENTREF
033400170418     c                                                           ';' +           44 SUPPLIERREF
033500170418     c                                                           ';' +           45 CONTAINERREF
033600170418     c                                                           ';' +           46 CONTAINERTYPE
033700170418     c                                                           ';' +           47 ACTORID
033800170418     c                                                           ';' +           48 OPERATIVERID
033900170418     c                                                           ';' +           49 PICKUPORDERNUMBE
034000170418     c                                                           ';' +           40 GPSLAT
034100170418     c                                                           ';' +           51 GPSLONG
034200170418     c                                                           ';' +           52 RETURNPARCELNUMB
034300170418     c                                                           ';'             53 COMPINFO1
034400170418      *
034500170418     c                   exsr      tivgd_$E
034600170418      *
034700170418     c                   endsr
034800170418      *-------------------------------------------------------------------------
034900170418     C     tivgd_$E      Begsr
035000170418      *-------------------------------------------------------------------------
035100170418     c                   clear                   tivgd000
035200170418     c                   eval      vgddta = %TrimR(dati_TTE)
035300170418     c                   eval      vgdtip = '$E'
035400170418     c                   eval      vgdksu = '0DPD0OUT'
035500170418     c                   eval      vgdtsc = 'WW'
035600170418     c                   eval      vgdpgm = 'FIEU42R'
035700170418     c                   eval      vgddat = udtymd
035800170418     C                   WRITE     tivgd000
035900170418      *
036000170418     C                   Endsr
036100170418      *-------------------------------------------------------------------------
036200170418      *   TTEVENTS  TIVGD       scrittura
036300170418      *-------------------------------------------------------------------------
036400170418     C     Wrk_TTEVENTS  Begsr
036500170418      *
036600170418      **  TIVGD APRE e CHIUDE
036700170418     c                   if        VGDTIP_$E ='APRE  '
036800170418      **                                        ===========
036900170418     c                   clear                   VGDTIP_$E
037000170418      **                                        ===========
037100170418      *  istruzioni apertura blocco scrittura TIVGD
037200170418     C                   clear                   trul47ds
037300170418     C                   eval      d47opz  = 'I'
037400170418     C                   eval      d47tip  = '$E'
037500170418     C                   eval      d47lck  = 'N'
037600170418     C                   eval      d47chkj = 'N'
037700170418     C                   eval      d47pgm  = 'FIEU42R'
037800170418     C                   call      'TRUL47R'
037900170418     C                   parm                    trul47ds
038000170418      **
038100170418     c                   elseIF    VGDTIP_$E ='CHIUDE'
038200170418      **
038300170418     c                   exsr      FINE_TTEVENT                                 #FINE MSG
038400170418     C* Infine elimino il blocco elaborazione TIVGD
038500170418     C                   clear                   trul47ds
038600170418     C                   eval      d47opz  = 'F'
038700170418     C                   eval      d47tip  = '$E'
038800170418     C                   call      'TRUL47R'
038900170418     C                   parm                    trul47ds
039000170418      **
039100170418     c                   end
039200170418      **
039300170418      **  scrive il  TTEVENTS
039400170418      * dettaglio TIVGD                     ===========
039500170418     c                   IF        VGDTIP_$E <>'CHIUDE'
039600170418      ** Solo la 1°Volta                    ===========
039700170418     c                   if         TESTA_TTE ='SI'
039800170418     c                   clear                   TESTA_TTE                      Solo la 1°volta
039900170418     c                   exsr      TESTA_TTEVENT                                Testata MSG
040000170418     c                   end
040100170418      *
040200170418      *  riga di dettaglio status SCAN
040300170418      *                           ---------------
040400170418     c                   exsr      TTEVENTS_dati                                riga di TTEVENTS
040500170418      *                           ---------------
040600170418     c                   endIF
040700170418      *
040800170418     c                   endsr
040900170418      *-------------------------------------------------------------------------
041000170418     c     TESTA_TTEVENT BEGsr
041100170418      *-------------------------------------------------------------------------
041200170418      *  6 righe di testata
041300170418      *
041400170418      **   #FILE;GEODATA_STATUS_0230820_N00_D20161016T082554;
041500170418     c                   clear                   dati_TTE
041600170418     c                   EVAL      dati_TTE = %trim(HEAD(1)) +
041700170418     c                              wdataoggi + 'T' + wora + ';'
041800170418     c                   exsr      tivgd_$E
041900170418      *
042000170418      **   #ENCODING;ISO-8859-1;
042100170418     c                   clear                   dati_TTE
042200170418     c                   EVAL      dati_TTE = HEAD(2)
042300170418     c                   exsr      tivgd_$E
042400170418      *
042500170418      **   #VERSION;03.10;
042600170418     c                   clear                   dati_TTE
042700170418     c                   EVAL      dati_TTE = HEAD(3)
042800170418     c                   exsr      tivgd_$E
042900170418      *
043000170418      **   #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
043100170418     c                   clear                   dati_TTE
043200170418     c                   EVAL      dati_TTE = HEAD(4)
043300170418     c                   exsr      tivgd_$E
043400170418      *
043500170418      **   #DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;........
043600170418     c                   clear                   dati_TTE
043700170418     c                   EVAL      dati_TTE = %trim(HEAD(5)) +
043800170418     c                                        %trim(HEAD(6)) +
043900170418     c                                        %trim(HEAD(7)) +
044000170418     c                                        %trim(HEAD(8)) +
044100170418     c                                        %trim(HEAD(9)) +
044200170418     c                                        %trim(HEAD(10)) +
044300170418     c                                        %trim(HEAD(11)) +
044400170418     c                                        %trim(HEAD(12))
044500170418     c                   exsr      tivgd_$E
044600170418      *
044700170418      **   HEADER;03.10;STATUS;
044800170418     c                   clear                   dati_TTE
044900170418     c                   EVAL      dati_TTE = HEAD(14)
045000170418     c                   exsr      tivgd_$E
045100170418      *
045200170418     c                   endsr
045300170418      *-------------------------------------------------------------------------
045400170418     c     FINE_TTEVENT  BEGsr
045500170418      *-------------------------------------------------------------------------
045600170418     c                   clear                   dati_TTE
045700170418     c                   movel     '#END;'       dati_TTE
045800170418     c                   exsr      tivgd_$E
045900170418      *
046000170418     c                   endsr
046100170418      *-------------------------------------------------------------------------
046200170418** (HEAD)
046300170418#FILE;GEODATA_STATUS_0230820_N00_D                                                 01<
046400170418#ENCODING;ISO-8859-1;                                                              02
046500170418#VERSION;03.10;                                                                    03<
046600170418#DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;                                       04
046700170418#DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;         05|
046800170418SERVICECODE;DZIPCODE;DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;                 06||
046900170418REASONCODE3;DEPOT;AGENTLOCATIONCODE;AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;       07| |
047000170418COUNTRY;CITY;STATUSDATETIME;TIMEZONE;MEMDATETIME;DDEPOT;DELIVERYTOUR;              08|  |
047100170418MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;PODIMAGEREF;RNAME;TRDPNAME;          09|  |
047200170418AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;      10| |
047300170418CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;               11||
047400170418RETURNPARCELNUMBER;COMPINFO1;;                                                     12|
047500170418                                                                                   13
047600170418HEADER;03.10;STATUS;                                                               14<
047700170418TTEVENTS;2                                                                         15<----
