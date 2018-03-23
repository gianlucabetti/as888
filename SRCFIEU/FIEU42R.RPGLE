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
007600170428      *
007700170428     c                   endif
007800170418      **
007900170428     c                   if        almeno_uno = 'S'
008000170418     C* chiude il TIVGD se era stato aperto
008100170418     c                   if        VGDTIP_$E = *blank  AND
008200170418     c                             (File_vers ='E' or File_vers ='T')
008300170418     c                   eval      VGDTIP_$E = 'CHIUDE'
008400170418     c                   exsr      Wrk_TTEVENTS
008500170418     c                   end
008600170418      *
008700060811     c                   endif
008800050704      *
008900170418     c                   commit
009000170418     C*
009100050705     c                   seton                                        lr
009200050920      **?__________________________________________________________________ */
009300170418     c     ELABORA       begsr
009400050920      **?__________________________________________________________________ */
009500170418      *--
009600170418      ** controlla lunghezza del Parcel
009700170418     c                   eval      Lung_Parcel = %Len(%trim(dpoNRP))
009800170418
009900170418      * operazioni da eseguire una sola volta reperimento numero di
010000170418      * trasmissione e intestazione del file da inviare
010100170418     c                   if        Lung_Parcel <> sav_lung_par  or
010200170418     c                             dpoDEP      <> savDEP
010300170418     c*-
010400170418     c                   if        File_vers ='S' or File_vers ='E'
010500170418     c                   exsr      Testata
010600170418     c                   end
010700170418     c*-
010800170418     c                   eval      sav_lung_par  = Lung_Parcel
010900170418     c                   eval      SavDEP        = dpoDEP
011000170418     c                   endif
011100170418      *
011200170418      *  Deve reperire il Collecting DEPOT che ha dato il Mancato Ritiro
011300170418      *   da impostare nello SCAN e lo decodifica dall'organigramma:
011400170418      *    mette di Default Verona
011500170418     c                   movel     '0837'        Coll_Depot        4
011600170418     c     korm          chain     fnorm01l
011700170418     c                   if        %found(fnorm01l)
011800170418      *  con la Filiale Ritirante
011900170418     c     ormPOR        chain     azorg01l
012000170418     c                   if        %found(azorg01l)
012100170418     c                   movel     orgDE3        Og143
012200170418     c                   if        §OGDP1 <> *blank
012300170418     c                   movel     §OGDP1        Coll_Depot
012400170418     c                   endif
012500170418     c                   endif
012600170418     c                   endif
012700170418      * dettaglio
012800170418     c                   move      'S'           almeno_uno
012900170418      *-- Vecchio SCAN
013000170418     c                   if        File_vers ='E'  or File_vers ='S'
013100170418     c                   exsr      Dettaglio
013200170418     c                   end
013300170418      *-- NUOVO TTEVENTS
013400170418     c                   if        File_vers ='E'  or File_vers ='T'
013500170418      *--
013600170418      *  istruzioni apertura blocco scrittura TIVGD
013700170418     c                   if        VGDTIP_$E ='APRE  '
013800170418      *       solo la prima riga di dettaglio
013900170418     c                   exsr      Wrk_TTEVENTS
014000170418     c                   end
014100170418      **----
014200170418      *  quindi scrive riga di dettaglio
014300170418     c                   exsr      Wrk_TTEVENTS
014400170418      *--
014500170418     c                   end
014600170418      *--
014700170418     c                   endsr
014800170418      **?__________________________________________________________________ */
014900170418     c     Testata       begsr
015000170418      **?__________________________________________________________________ */
015100170418      *  istruzioni apertura blocco scrittura TIVGD
015200170418      *    fatto solo all'inizio del programma
015300170418     c                   if        solo_inizio = *blank
015400170418     C                   clear                   trul47ds
015500170418     C                   eval      d47opz  = 'I'
015600170418     C                   eval      d47tip  = '$3'
015700170418     C                   eval      d47lck  = 'N'
015800170418     C                   eval      d47chkj = 'N'
015900170418     C                   eval      d47pgm  = 'FIEU42R'
016000170418     C                   call      'TRUL47R'
016100170418     C                   parm                    trul47ds
016200170418     c                   eval      solo_inizio = 'S'
016300170418     c                   end
016400170418
016500170418      * a cambio di tipo Parcel --> cambio di SCAN
016600170418      *   chiude con il record di Fine
016700170418     c                   if        lung_parcel <> sav_lung_par  or
016800170418     c                             dpoDEP      <> savDEP
016900170418     c                   exsr      Coda
017000170418     c                   end
017100170418
017200170418      *rekord testata scan
017300170418     c                   clear                   dpoRecord
017400170418
017500170418     c                   if        lung_parcel = 11
017600170418      *
017700170418     c                   eval      dporecord ='0818837001' +
017800170418     c                             udtymda + oratra + '000000' + '|'
017900170418     c                   else
018000170418     c                   eval      dporecord ='0818' +
018100170418     c                             udtymda + oratra +
018200170418     c                             '|D' + %trim(dpoDEP) + '|T001|'
018300170418     c                   end
018400170418      *
018500170418     C                   exsr      tivgd
018600170418      *
018700170418     c                   endsr
018800050920      **?__________________________________________________________________ */
018900060811     c     Dettaglio     begsr
019000050920      **?__rekord dettaglio scan __________________________________________ */
019100050920
019200050920      * prepara scan per invio
019300060811     c                   clear                   dporecord
019400060811
019500060811     c                   if        lung_parcel = 11
019600060811      *
019700051007     c                   eval      dporecord ='0118837001' +
019800061006     c                             udtymda + oratra + '000000' +
019900060811     c                             %trim(dpoNRP)+ '|Z' + %trim(dpoCDmr)+ '|'
020000060811     c                   else
020100061003     c                   eval      dporecord ='0118' +
020200061006     c                             udtymda + oratra +
020300080529     c                             '|D' + %trim(Coll_Depot) + '|T001' +
020400060811     c                             '|P' + %trim(dpoNRP) +
020500060915     c                             '|Z' + %trim(dpoCDmr)+
020600080529     c                             '|YAL01|YAD03' + %subst(dpoCDmr:2:2) + '|'
020700080527     c
020800060811     c                   end
020900060811      *
021000060810     C                   exsr      tivgd
021100060810
021200050920     c                   endsr
021300050920      **?__________________________________________________________________ */
021400060811     c     Coda          begsr
021500050920      **?-piede messaggio          ________________________________________ */
021600060811     c                   if        almeno_uno = 'S'
021700060811      *
021800060811     c                   clear                   dpoRecord
021900060811      *
022000060811     c                   if        sav_lung_par  = 11
022100060811      *
022200051007     c                   eval      dporecord ='0918837001' +
022300061006     c                             udtymda + oratra +'000000' + '|'
022400060811     c                   else
022500060811      *
022600060811     c                   eval      dporecord ='0918' +
022700061006     c                             udtymda + oratra +
022800061027     c                             '|D' + %trim(savDEP) + '|T001|'
022900060811     c                   end
023000050920      *
023100060810     C                   exsr      tivgd
023200060811
023300060811     c                   end
023400060810
023500050920     c                   endsr
023600050920      **?__________________________________________________________________ */
023700050512     c     *inzsr        begsr
023800050920      **?__________________________________________________________________ */
023900050705     c     *entry        plist
024000050705     C                   parm                    kpjba
024100060920      ***
024200080529     c     korm          klist
024300080529     c                   kfld                    DPOPOE
024400080529     c                   kfld                    DPONRS
024500080529     c                   kfld                    DPONOR
024600080529     c                   kfld                    DPONRV
024700080529
024800050705     C                   TIME                    ORADAT           14 0
024900050705     C                   MOVEL     ORADAT        ORATR             6 0
025000061006     C                   MOVEL     ORATR         ORATRa            6
025100050920     c                   move      *date         udtymd            8 0
025200061006     c                   move      udtymd        udtymda           8
025300170418     c                   move      udtymd        wdataoggi
025400170418     C                   MOVEL     oradat        WORA
025500170418      *-----------------------------
025600170418      *  prima gli SCAN
025700170418     c                   eval      File_vers ='S'
025800170418      *
025900170418      *   dal 18/4/2017 anche i TTEVENTS con gli SCAN
026000170418     c                   if        udtymd  >= 20170418
026100170418     c                   eval      File_vers ='E'
026200170418     c                   end
026300170418      *
026400170418      *   dal 18/4/2017 solo i TTEVENTS
026500170418     c                   if        udtymd  >= 20170420
026600170418     c                   eval      File_vers ='T'
026700170418     c                   end
026800170418      **
026900170418      *-----------------------------
027000050512     c                   endsr
027100060810      **?__________________________________________________________________ */
027200060810      *   Scrittura tivgd file da trsmettere               *
027300060810      **?__________________________________________________________________ */
027400060810     C     tivgd         Begsr
027500060810      *
027600170418     c                   if        File_vers ='E' or File_vers ='S'
027700170418      *
027800060810     c                   clear                   tivgd000
027900080403     c                   eval      vgddta = %TrimR(dpoRecord)
028000060810     c                   eval      vgdtip = '$3'
028100060810     c                   eval      vgdksu = '0DPD0OUT'
028200060810     c                   eval      vgdtsc = 'WW'
028300060810     c                   eval      vgdpgm = 'FIEU42R'
028400061023     c                   eval      vgddat = udtymd
028500060810      *
028600060810     C                   WRITE     tivgd000
028700170418      *
028800170418     c                   end
028900060810      *
029000060810     C                   Endsr
029100170418      **************************************************************************
029200170418      *-------------------------------------------------------------------------
029300170418     C     TTEVENTS_Dati Begsr
029400170418      *-------------------------------------------------------------------------
029500170418      *
029600170418     c                   eval        ContInfo1      = '01'
029700170418     c                   eval        ContInfo2      = '03' + %subst(dpoCDmr:2:2)
029800170418      *
029900170418     c                   clear                   dati_TTE
030000170418      *
030100170418     c                   EVAL      dati_TTE = %trim(HEAD(15))  + ';' +            0 TTEVENTS;2
030200170418     c                                        %trim(dpoNRP)    + ';' +            1 PARCELNUMBER
030300170418     c                                            'DE06'       + ';' +            2 STATUSTYPE
030400170418     c                                                           ';' +            3 ORIGINPARCELNUMB
030500170418     c                                                           ';' +            4 SERVICECODE
030600170418     c                                                           ';' +            5 DZIPCODE
030700170418     c                                                           ';' +            6 DCOUNTRYCODE
030800170418     c                                          '18'           + ';' +            7 SCANCODE
030900170418     c                                     %trim(dpoCDmr)      + ';' +            8 REASONCODE
031000170418     c                                                           ';' +            9 REASONCODE2
031100170418     c                                                           ';' +           10 REASONCODE3
031200170418     c                                  '023' +  Coll_Depot    + ';' +           11 DEPOT
031300170418     c                                                           ';' +           12 AGENTLOCATCODE
031400170418     c                                                           ';' +           13 AGENTLOCATTYPE
031500170418     c                                                           ';' +           14 AGENTPARCELNUMBE
031600170418     c                                                           ';' +           15 COUNTRY
031700170418     c                                                           ';' +           16 CITY
031800170418     c                                   udtymda                     +           17 STATUSDATETIME
031900170418     c                                   oratra                + ';' +           18
032000170418     c                                                           ';' +           19 TIMEZONE
032100170418     c                                            wdataoggi          +           20 MEMDATETIME
032200170418     c                                            wora         + ';' +           21
032300170418     c                                                           ';' +           22 DDEPOT
032400170418     c                                     '001'               + ';' +           23 DELIVERYTOUR
032500170418     c                                                           ';' +           24 MEASUREDWEIGHT
032600170418     c                                                           ';' +           25 DIMENSION
032700170418     c                                                           ';' +           26 PARCELREF
032800170418     c                                                           ';' +           27 PARCELREF2
032900170418     c                                                           ';' +           28 PODIMAGEREF
033000170418     c                                                           ';' +           23 RNAME
033100170418     c                                                           ';' +           38 TRDPNAME
033200170418     c                                                           ';' +           39 AREA
033300170418     c                                                           ';' +           30 STOP
033400170418     c                                     %trim(ContInfo1)    + ';' +           41 CONTAINERINFO1
033500170418     c                                     %trim(ContInfo2)    + ';' +           42 CONTAINERINFO2
033600170418     c                                                           ';' +           43 SHIPMENTREF
033700170418     c                                                           ';' +           44 SUPPLIERREF
033800170418     c                                                           ';' +           45 CONTAINERREF
033900170418     c                                                           ';' +           46 CONTAINERTYPE
034000170418     c                                                           ';' +           47 ACTORID
034100170418     c                                                           ';' +           48 OPERATIVERID
034200170418     c                                                           ';' +           49 PICKUPORDERNUMBE
034300170418     c                                                           ';' +           40 GPSLAT
034400170418     c                                                           ';' +           51 GPSLONG
034500170418     c                                                           ';' +           52 RETURNPARCELNUMB
034600170418     c                                                           ';'             53 COMPINFO1
034700170418      *
034800170418     c                   exsr      tivgd_$E
034900170418      *
035000170418     c                   endsr
035100170418      *-------------------------------------------------------------------------
035200170418     C     tivgd_$E      Begsr
035300170418      *-------------------------------------------------------------------------
035400170418     c                   clear                   tivgd000
035500170418     c                   eval      vgddta = %TrimR(dati_TTE)
035600170418     c                   eval      vgdtip = '$E'
035700170418     c                   eval      vgdksu = '0DPD0OUT'
035800170418     c                   eval      vgdtsc = 'WW'
035900170418     c                   eval      vgdpgm = 'FIEU42R'
036000170418     c                   eval      vgddat = udtymd
036100170418     C                   WRITE     tivgd000
036200170418      *
036300170418     C                   Endsr
036400170418      *-------------------------------------------------------------------------
036500170418      *   TTEVENTS  TIVGD       scrittura
036600170418      *-------------------------------------------------------------------------
036700170418     C     Wrk_TTEVENTS  Begsr
036800170418      *
036900170418      **  TIVGD APRE e CHIUDE
037000170418     c                   if        VGDTIP_$E ='APRE  '
037100170418      **                                        ===========
037200170418     c                   clear                   VGDTIP_$E
037300170418      **                                        ===========
037400170418      *  istruzioni apertura blocco scrittura TIVGD
037500170418     C                   clear                   trul47ds
037600170418     C                   eval      d47opz  = 'I'
037700170418     C                   eval      d47tip  = '$E'
037800170418     C                   eval      d47lck  = 'N'
037900170418     C                   eval      d47chkj = 'N'
038000170418     C                   eval      d47pgm  = 'FIEU42R'
038100170418     C                   call      'TRUL47R'
038200170418     C                   parm                    trul47ds
038300170418      **
038400170418     c                   elseIF    VGDTIP_$E ='CHIUDE'
038500170418      **
038600170418     c                   exsr      FINE_TTEVENT                                 #FINE MSG
038700170418     C* Infine elimino il blocco elaborazione TIVGD
038800170418     C                   clear                   trul47ds
038900170418     C                   eval      d47opz  = 'F'
039000170418     C                   eval      d47tip  = '$E'
039100170418     C                   call      'TRUL47R'
039200170418     C                   parm                    trul47ds
039300170418      **
039400170418     c                   end
039500170418      **
039600170418      **  scrive il  TTEVENTS
039700170418      * dettaglio TIVGD                     ===========
039800170418     c                   IF        VGDTIP_$E <>'CHIUDE'
039900170418      ** Solo la 1°Volta                    ===========
040000170418     c                   if         TESTA_TTE ='SI'
040100170418     c                   clear                   TESTA_TTE                      Solo la 1°volta
040200170418     c                   exsr      TESTA_TTEVENT                                Testata MSG
040300170418     c                   end
040400170418      *
040500170418      *  riga di dettaglio status SCAN
040600170418      *                           ---------------
040700170418     c                   exsr      TTEVENTS_dati                                riga di TTEVENTS
040800170418      *                           ---------------
040900170418     c                   endIF
041000170418      *
041100170418     c                   endsr
041200170418      *-------------------------------------------------------------------------
041300170418     c     TESTA_TTEVENT BEGsr
041400170418      *-------------------------------------------------------------------------
041500170418      *  6 righe di testata
041600170418      *
041700170418      **   #FILE;GEODATA_STATUS_0230820_N00_D20161016T082554;
041800170418     c                   clear                   dati_TTE
041900170418     c                   EVAL      dati_TTE = %trim(HEAD(1)) +
042000170418     c                              wdataoggi + 'T' + wora + ';'
042100170418     c                   exsr      tivgd_$E
042200170418      *
042300170418      **   #ENCODING;ISO-8859-1;
042400170418     c                   clear                   dati_TTE
042500170418     c                   EVAL      dati_TTE = HEAD(2)
042600170418     c                   exsr      tivgd_$E
042700170418      *
042800170418      **   #VERSION;03.10;
042900170418     c                   clear                   dati_TTE
043000170418     c                   EVAL      dati_TTE = HEAD(3)
043100170418     c                   exsr      tivgd_$E
043200170418      *
043300170418      **   #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
043400170418     c                   clear                   dati_TTE
043500170418     c                   EVAL      dati_TTE = HEAD(4)
043600170418     c                   exsr      tivgd_$E
043700170418      *
043800170418      **   #DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;........
043900170418     c                   clear                   dati_TTE
044000170418     c                   EVAL      dati_TTE = %trim(HEAD(5)) +
044100170418     c                                        %trim(HEAD(6)) +
044200170418     c                                        %trim(HEAD(7)) +
044300170418     c                                        %trim(HEAD(8)) +
044400170418     c                                        %trim(HEAD(9)) +
044500170418     c                                        %trim(HEAD(10)) +
044600170418     c                                        %trim(HEAD(11)) +
044700170418     c                                        %trim(HEAD(12))
044800170418     c                   exsr      tivgd_$E
044900170418      *
045000170418      **   HEADER;03.10;STATUS;
045100170418     c                   clear                   dati_TTE
045200170418     c                   EVAL      dati_TTE = HEAD(14)
045300170418     c                   exsr      tivgd_$E
045400170418      *
045500170418     c                   endsr
045600170418      *-------------------------------------------------------------------------
045700170418     c     FINE_TTEVENT  BEGsr
045800170418      *-------------------------------------------------------------------------
045900170418     c                   clear                   dati_TTE
046000170418     c                   movel     '#END;'       dati_TTE
046100170418     c                   exsr      tivgd_$E
046200170418      *
046300170418     c                   endsr
046400170418      *-------------------------------------------------------------------------
046500170418** (HEAD)
046600170418#FILE;GEODATA_STATUS_0230820_N00_D                                                 01<
046700170418#ENCODING;ISO-8859-1;                                                              02
046800170418#VERSION;03.10;                                                                    03<
046900170418#DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;                                       04
047000170418#DEF;GEODATA:TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;         05|
047100170418SERVICECODE;DZIPCODE;DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;                 06||
047200170418REASONCODE3;DEPOT;AGENTLOCATIONCODE;AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;       07| |
047300170418COUNTRY;CITY;STATUSDATETIME;TIMEZONE;MEMDATETIME;DDEPOT;DELIVERYTOUR;              08|  |
047400170418MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;PODIMAGEREF;RNAME;TRDPNAME;          09|  |
047500170418AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;      10| |
047600170418CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;               11||
047700170418RETURNPARCELNUMBER;COMPINFO1;;                                                     12|
047800170418                                                                                   13
047900170418HEADER;03.10;STATUS;                                                               14<
048000170418TTEVENTS;2                                                                         15<----
